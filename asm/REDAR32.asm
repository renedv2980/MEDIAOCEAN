*          DATA SET REDAR32    AT LEVEL 216 AS OF 05/09/13                      
*PHASE T80F32A                                                                  
*INCLUDE REGENPBY                                                               
         TITLE 'T80F30 - REDAR30 - DARE DRAFT REPORT'                           
***********************************************************************         
*                                                                     *         
*  REDAR30 (T80F30) --- DARE DRAFT REPORT                             *         
*                                                                     *         
* ------------------------------------------------------------------- *         
* UPDATE HISTORY:                                                     *         
*                                                                     *         
* 03MAY13 KWA BUMP UP LISTAREA FOR REGENDB                            *         
* 17FEB12 SKU SKIP LOCAL MARKET CHECK FOR FOXLOC                      *         
* 02SEP09 KUI FIX TOTAL SPOT/$ DISPLAY FOR ESPERANTO XML ORDERS       *         
* 05AUG04 SKU FIX ALPHANUMERIC ESTIMATE PRINT BUG                     *         
* 22JAN03 SKU SUPPORT ALPHANUMERIC ESTIMATE                           *         
* 26JUN02 HQ  EXCLUDES CONTRACT BUYLINE IF CALLED BUY 10 MODULE       *         
* 26FEB02 SKU SUPPORT LONGER COMMENTS                                 *         
* 04JAN02 SKU CHANGE APPROVE TO OPEN                                  *         
* 03DEC01 SKU ADD TRADE INDICATORS                                    *         
* 22MAY01 SKU DEMO OVERRIDE BUG FIX                                   *         
* 18JUL00 SKU ADD REQUESTOR FIELD                                     *         
* 28JUN00 SKU SKIP USER DEFINED DEMOS                                 *         
* 06AUG98 SKU DISPLAY AGENCY DEMO CATEGORY                            *         
* 04JUN98 SKU UPDATE CALL TO REGENDAB                                 *         
* 24FEB98 SKU TIME STAMP CORRECTION                                   *         
* 23JAN98 SKU FIX REJECT PRINTING BUG                                 *         
* 23OCT97 SKU MAKE PROGRAM RE-ENTRANT                                 *         
* 13MAR97 SKU SUPPORT VARIOUS/BRAND ORDERS                            *         
* 07OCT96 SKU LOW POWER STATION                                       *         
* 17JUL96 SKU SHOW PRODUCT CODES                                      *         
* 20FEB96 SKU MAKE WORKSHEETS GENERIC (NO REFERENCES TO 'DARE')       *         
* 13FEB96 SKU REMOVE REJECT DATE/TIME FOR KATZ/EDI ORDERS             *         
* 19JUL95 SKU SPECIFY MOD CODE 'A' FOR NEW DARE BUY LINES             *         
* 13JUL95 SKU RECALL SUPPORT                                          *         
* 21JUN95 SKU UNLINK ORDERS NEED TO READ STATION RECORD FOR MKT NAME  *         
* 11MAY95 SKU PRINT HIATUS DATES                                      *         
* 04APR94 SKU COMPARE AGENCY AGAINST CONTRACT BUYS FOR MOD CHANGES    *         
* 26JAN94 SKU SPECIFY IF DAILY BUY                                    *         
* 12JUN94 SKU INITIAL RELEASE                                         *         
***********************************************************************         
T80F32   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKLQ,*T80F32*,R7,RR=R3                                         
         LR    R5,RC                                                            
         USING MYAREAD,R5          R5 = A(OVERLAY STORAGE AREA)                 
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
                                                                                
         MVC   PRTSTAT,4(R1)       PRINT STATUS                                 
         MVC   PRTSTAT2,5(R1)       PRINT STATUS                                
         XC    CMTFLAG,CMTFLAG                                                  
         B     PR                                                               
                                                                                
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* PRINT A REPORT                                                                
* DARE HEADER RECORD WILL BE READ INTO IO1                                      
***********************************************************************         
PR       DS    0H                                                               
         TM    PRTSTAT,PRTCLOSE                                                 
         BZ    PR03                                                             
         MVI   SPMODE,X'FF'                                                     
         BAS   RE,PRINT            CLOSE PRINT QUEUE                            
         B     PRX                                                              
*                                                                               
PR03     TM    PRTSTAT,PRTNEWPG    NEXT REPORT, SKIP PQ OPEN                    
         BO    PR10                                                             
*                                                                               
         OI    GENSTAT3,NOCLRSPK                                                
         MVC   REMUSER,=C'DAO'     DARE AGENCY ORDER                            
         TM    PRTSTAT,PRTDAB      LABEL AS DAB IF FROM BUY LIST SCR            
         BZ    *+10                                                             
         MVC   REMUSER,=C'DAB'                                                  
*                                                                               
         OC    REQINIT,REQINIT     CHECK IF OVERRIDE REPORT NAME                
         BZ    *+10                                                             
         MVC   REMUSER,REQINIT                                                  
*                                                                               
         LA    RF,SPOOLKEY                                                      
         USING PQPLD,RF                                                         
         XC    SPOOLKEY,SPOOLKEY                                                
         MVC   PLDESC,=C'WORKSHEET  '                                           
         MVI   PLCLASS,C' '                                                     
         OI    SPOOLIND,SPUINIT    PERMITS SETTING OF CLASS                     
         MVC   PLSUBID,=C'DAO'     DARE AGENCY ORDER                            
         TM    PRTSTAT,PRTDAB      LABEL AS DAB IF FROM BUY LIST SCR            
         BZ    *+10                                                             
         MVC   PLSUBID,=C'DAB'                                                  
*                                                                               
         OC    REQINIT,REQINIT     CHECK IF OVERRIDE REPORT NAME                
         BZ    *+10                                                             
         MVC   PLSUBID,REQINIT                                                  
         DROP  RF                                                               
*                                                                               
PR05     DS    0H                                                               
         LA    RE,SPLKEYAD                                                      
         ST    RE,SPOOLQLK         SET EXTENDED KEY ADDRESS                     
         USING PQPLD,RE                                                         
         XC    0(133,RE),0(RE)                                                  
         MVI   QLEXTRA,X'FF'                                                    
         MVC   QLRETNL,=H'6'                                                    
         MVC   QLRETND,=H'2'                                                    
         MVC   QLRETNL,=H'36'                                                   
         MVI   QLSYS,C'R'          FORM/COPIES COLUMN                           
         MVC   QLPRG,=C'CO'                                                     
         DROP  RE                                                               
                                                                                
         GOTO1 OPENPQ                                                           
                                                                                
PR10     DS    0H                                                               
         L     R1,=A(HEDSPECS)                                                  
         A     R1,RELO                                                          
         ST    R1,SPECS                                                         
         L     R1,=A(HOOK)                                                      
         A     R1,RELO                                                          
         ST    R1,HEADHOOK                                                      
                                                                                
         OC    SELECTKY,SELECTKY   *** SHOULD NEVER HAPPEN !                    
         BZ    PRX                                                              
         MVC   KEY(L'SELECTKY),SELECTKY                                         
         MVI   RDUPDATE,C'N'                                                    
         BAS   RE,MODKEY                                                        
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
                                                                                
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         MVC   PRBUYKEY,RDARKEY    INIT BUY KEY                                 
                                                                                
         NI    MISCFLAG,X'FF'-MSFTRADE                                          
         CLI   RDARCORT,C'T'       FLAG IF TRADE ORDER                          
         BNE   *+8                                                              
         OI    MISCFLAG,MSFTRADE                                                
*                                                                               
         XC    SVDARFL2,SVDARFL2                                                
         MVI   ELCODE,X'0F'                                                     
         BRAS  RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   SVDARFL2,RDARFLG2-RDARFLEM(R6)                                   
         L     R6,AIO                                                           
                                                                                
* DEFAULT IS ORIGINAL OR RESENT                                                 
* CHECK FOR OPEN/REJECT IF SET                                                  
                                                                                
         MVC   ORDSTAT,=C'ORIGINAL'                                             
         OC    RDARRNUM,RDARRNUM                                                
         BZ    PR12                                                             
         MVC   ORDSTAT,=C'REVISION'                                             
                                                                                
PR12     DS    0H                                                               
         TM    RDARMISC,X'20'                                                   
         BZ    PR15                                                             
         MVC   ORDSTAT,=C'NOTDARE '                                             
         CLC   =C'PV',AGENCY                                                    
         BE    PR13                                                             
         CLC   =C'P9',AGENCY                                                    
         BNE   PR40                                                             
PR13     DS    0H                                                               
         MVC   ORDSTAT,=C'NOTPOEM '                                             
         B     PR40                                                             
                                                                                
PR15     DS    0H                                                               
         TM    RDARMISC,X'80'                                                   
         BZ    PR18                                                             
         MVC   ORDSTAT,=C'RESENT  '                                             
         B     PR20                                                             
                                                                                
PR18     DS    0H                                                               
         TM    RDARMISC,X'10'                                                   
         BZ    PR20                                                             
         MVC   ORDSTAT,=C'VARNEW  '                                             
                                                                                
PR20     DS    0H                                                               
         CLI   RDARBSTS,C'C'                                                    
         BNE   PR25                                                             
         MVC   ORDSTAT,=C'RECALLED'                                             
         B     PR40                                                             
                                                                                
PR25     DS    0H                                                               
         CLI   RDARBSTS,C'R'                                                    
         BNE   PR30                                                             
         MVC   ORDSTAT,=C'REJECTED'                                             
         B     PR40                                                             
                                                                                
PR30     DS    0H                                                               
         CLI   RDARBSTS,C'A'                                                    
         BNE   PR40                                                             
         MVC   ORDSTAT,=C'OPENED  '                                             
                                                                                
PR40     DS    0H                                                               
         XC    SVCONNUM,SVCONNUM                                                
         OC    RDARREP#,RDARREP#   NOT LINKED TO A CONTRACT                     
         BZ    PR55                                                             
                                                                                
PR50     DS    0H                                                               
         MVC   SVCONNUM,CCONKNUM   SAVE OFF CON NUM, SO WE CAN RESTORE          
         ZAP   WORK+20(5),=P'0'                                                 
         MVO   WORK+20(5),RDARREP#                                              
         LA    R2,FAKEHDR          SETUP FAKE FIELD HEADER                      
         EDIT  (P5,WORK+20),(8,8(R2)),ALIGN=LEFT TO CALL VCON WITH              
         STC   R0,5(R2)            SET LENGTH OF FIELD                          
         MVI   0(R2),16            FIELD HEADER LENGTH                          
         MVI   4(R2),X'08'         SET VALID NUMERIC                            
         GOTO1 VALICON,DMCB,(R2)                                                
         DROP  R6                                                               
                                                                                
PR55     DS    0H                                                               
         TM    PRTSTAT,PRTNEWPG                                                 
         BZ    PR60                                                             
         MVI   FORCEHED,C'Y'                                                    
                                                                                
PR60     DS    0H                                                               
         GOTOR PRPAGE1                                                          
*&&DO                                                                           
         BAS   RE,PRTCON           PRINT CONTRACT INFORMATION                   
*                                                                               
         MVC   KEY(L'SELECTKY),SELECTKY                                         
         MVI   RDUPDATE,C'N'                                                    
         BAS   RE,MODKEY                                                        
         GOTO1 HIGH                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         GOTO1 =A(PRTAGY),RR=RELO  PRINT AGENCY INFORMATION                     
*&&                                                                             
*                                                                               
* IF VARIOUS, NEED TO SAY SO                                                    
*                                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
*                                                                               
         TM    RDARMISC,X'10'                                                   
         BZ    PR65                                                             
         BAS   RE,VARIOUS                                                       
*                                                                               
* IF BRAND, NEED TO SAY SO                                                      
*                                                                               
PR65     DS    0H                                                               
         TM    RDARMISC,X'08'                                                   
         BZ    PR67                                                             
         BAS   RE,BRAND                                                         
*                                                                               
* CHECK IF ANY STANDARD OR ORDER OR REJECTION COMMENTS PRESENT                  
* FIRST CHECK IF ANY REJECTION COMMENTS                                         
*                                                                               
PR67     DS    0H                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BNE   *+8                                                              
         OI    CMTFLAG,CMTREJ                                                   
*                                                                               
* NOW LOOK FOR STANDARD COMMENTS                                                
*                                                                               
         MVC   KEY(L'PRBUYKEY),PRBUYKEY                                         
         LA    R6,KEY                                                           
         USING RDARKEY,R6                                                       
         MVI   RDARKRT,X'20'       TYPE STANDARD COMMENT                        
                                                                                
         MVI   RDUPDATE,C'N'                                                    
         BAS   RE,MODKEY                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
         BNE   *+8                 IF STANDARD COMMENT                          
         OI    CMTFLAG,CMTSTAND    SET FLAG                                     
                                                                                
* NOW LOOK FOR ORDER COMMENTS                                                   
         MVI   RDARKRT,X'30'       TYPE ORDER COMMENT                           
         DROP  R6                                                               
                                                                                
         MVI   RDUPDATE,C'N'                                                    
         BAS   RE,MODKEY                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
         BNE   *+8                 IF ORDER COMMENT                             
         OI    CMTFLAG,CMTORD      SET FLAG                                     
                                                                                
         GOTOR PRTNOTE                                                          
                                                                                
PR70     DS    0H                                                               
         MVI   BUYNUM,0                                                         
                                                                                
         LA    R6,PRBUYKEY                                                      
         USING RDARKEY,R6                                                       
         MVI   RDARKRT,X'40'       TYPE BUY                                     
         MVC   RDARKSEQ,BUYNUM     CHECK IF THERE ARE ANY BUYS                  
         DROP  R6                                                               
                                                                                
         MVC   KEY(L'PRBUYKEY),PRBUYKEY                                         
         MVI   RDUPDATE,C'N'                                                    
         BAS   RE,MODKEY                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(RDARKSEQ-RDARKEY),KEYSAVE                                    
         BNE   PR180               IF NO BUYS SKIP                              
         MVC   PRBUYKEY,KEY                                                     
                                                                                
PR80     DS    0H                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         OC    RDARREP#,RDARREP#                                                
         BZ    PR90                                                             
         DROP  R6                                                               
                                                                                
         LA    R6,KEY              SEE IF LINKED CONTRACT HAS ANY BUYS          
         USING RCONKEY,R6                                                       
         XC    KEY,KEY                                                          
         MVI   RCONPTYP,X'8C'                                                   
         MVC   RCONPREP,AGENCY                                                  
         MVC   RCONPCON,CCONNUM                                                 
         DROP  R6                                                               
                                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'RCONKEY),KEYSAVE                                           
         BNE   PR90                                                             
         OI    PRTSTAT,PRTKBUY     CONTRACT HAS BUYS                            
                                                                                
         BAS   RE,SORTBUYS         BUILD AGENCY/CONTRACT BUY SORT TABLE         
         LA    R2,SORTAREA                                                      
                                                                                
PR90     DS    0H                                                               
         BAS   RE,PRINT                                                         
         LA    R6,P                                                             
         TM    PRTSTAT,PRTKBUY     SHIFT OVER FOR COMPARE-ORDER                 
         BZ    *+8                                                              
         LA    R6,10(R6)                                                        
         MVC   0(BUYTITLQ,R6),BUYTITLE                                          
         BAS   RE,PRINT                                                         
         MVI   P,C'-'                                                           
         MVC   P+1(109),P                                                       
         BAS   RE,PRINT                                                         
*                                                                               
PR100    DS    0H                                                               
         MVC   DMCB2(4),ACOMFACS   SET UP ROUTINE ADDRESS BLOCK                 
         MVC   DMCB2+4(4),VOUTDAY                                               
         MVC   DMCB2+8(4),UNTIME                                                
         MVC   DMCB2+12(4),DEMOCON                                              
                                                                                
         XC    BYTE,BYTE           BYTE IS THE PARAMETER BYTE TO                
*                                  REGENDB                                      
         LR    RE,RA               CHECK TO SEE IF LOCAL SIGN ON                
         AHI   RE,DARPROFS-CONHEADH                                             
         USING SVDSECT,RE                                                       
         TM    SVPGPBIT+CNTRPEAB,CNTRPEAA                                       
         BZ    PR100A                                                           
         DROP  RE                                                               
*                                                                               
         CLC   =C'CB',AGENCY                                                    
         BE    PR100A                                                           
         CLC   =C'FF',AGENCY                                                    
         BE    PR100A                                                           
         CLI   SIGNONID+4,C'L'                                                  
         BNE   PR100A                                                           
         OI    BYTE,X'10'          YES, DON'T SHOW DEMOS                        
*                                                                               
PR100A   DS    0H                                                               
         OC    RTKODATE,RTKODATE   NEED TO CHOP & PRINT?                        
         BZ    PR103               YES                                          
*                                                                               
         MVC   WORK(4),VREDARTK                                                 
         MVC   WORK+4(4),PERVERT                                                
         MVC   WORK+8(3),RTKODATE                                               
*                                                                               
         OI    BYTE,X'C0'                                                       
*                                                                               
         TM    PRTSTAT,PRTDAB      PRINTING DAB REPORT                          
         BO    *+8                 YES                                          
         OI    BYTE,X'04'          NO, DON'T SHOW BUY CHANGE COMMT              
         TM    PRTSTAT2,PRTMGIND                                                
         BZ    *+8                                                              
         OI    BYTE,X'20'           PRINT MAKE GOOD INDICATOR                   
*                                                                               
         GOTO1 VREGENDB,DMCB,(BYTE,PRBUYKEY),(35,LISTAREA),            X        
               (BUYNUM,AIO3),DMCB2,WORK                                         
         MVC   BUYNUM,DMCB+9                                                    
         CLI   DMCB+8,0            DELETED VIA TKO?                             
         BNE   PR104               NO                                           
         B     PR170                                                            
*                                                                               
PR103    DS    0H                                                               
         OI    BYTE,X'80'                                                       
         TM    PRTSTAT2,PRTMGIND                                                
         BZ    *+8                                                              
         OI    BYTE,X'20'          PRINT MAKE GOOD INDICATOR                    
         TM    PRTSTAT,PRTDAB      PRINTING DAB REPORT                          
         BO    *+8                 YES                                          
         OI    BYTE,X'04'          NO, DON'T SHOW BUY CHANGE COMMT              
*                                                                               
         GOTO1 VREGENDB,DMCB,(BYTE,PRBUYKEY),(35,LISTAREA),            X        
               (BUYNUM,AIO3),DMCB2                                              
                                                                                
         MVC   BUYNUM,DMCB+9                                                    
*                                                                               
PR104    DS    0H                                                               
         NI    PRTSTAT,X'FF'-PRTDAILY                                           
         ZIC   R3,DMCB+10          CHECK IF DAILY                               
         LTR   R3,R3                                                            
         BZ    PR105                                                            
         OI    PRTSTAT,PRTDAILY                                                 
                                                                                
PR105    DS    0H                                                               
         MVC   AGYBUYLN,DMCB+8                                                  
         ZIC   R3,DMCB+8                                                        
*                                                                               
         CHI   R3,35               REACHED MAX SUPPORTED LINES?                 
         JNH   *+8                                                              
         LHI   R3,35                                                            
*                                                                               
         LTR   R3,R3                                                            
         BZ    PR170               NO BUYLINES FOUND                            
                                                                                
         LA    R6,H5                                                            
         TM    PRTSTAT,PRTKBUY     SHIFT OVER FOR COMPARE-ORDER                 
         BZ    *+8                                                              
         LA    R6,10(R6)                                                        
         MVC   0(BUYTITLQ,R6),BUYTITLE   BUY HEADING                            
         MVI   H6,C'-'                                                          
         MVC   H6+1(109),H6                                                     
                                                                                
         TM    PRTSTAT,PRTKBUY     FOR COMPARE-ORDERS ONLY                      
         BZ    PRT108                                                           
         TM    PRTSTAT,PRTDAB      COME FROM BUYLIST SCR?                       
         BO    PRT108              YES,DO NOT PRINT MOD CODE AT ALL             
         BAS   RE,GETMODCD         CHECK AGENCY BUYS AGAINST K BUYS             
*                                  AND SHOW MOD CODES                           
PRT108   DS    0H                                                               
         LA    R4,LISTAREA                                                      
                                                                                
PR110    DS    0H                                                               
         LA    R6,P                                                             
         TM    PRTSTAT,PRTKBUY     FOR COMPARE-ORDERS ONLY                      
         BZ    PR115                                                            
         TM    PRTSTAT,PRTDAILY    SHOW IF DAILY                                
         BZ    PR113                                                            
         MVC   0(9,R6),=C'* DAILY *'                                            
                                                                                
PR113    DS    0H                                                               
         LA    R6,10(R6)                                                        
                                                                                
PR115    DS    0H                                                               
         MVC   0(L'LISTAREA,R6),0(R4)                                           
         LA    R4,L'LISTAREA(R4)                                                
                                                                                
         CLC   =C'P=',9(R4)        CHECK FOR PROGRAM NAME                       
         BNE   PR120                                                            
         MVC   81(L'RDARBYPN,R6),11(R4)                                         
         LA    R4,L'LISTAREA(R4)                                                
*        NI    PRTSTAT,X'FF'-PRTDAILY ALL DONE WITH DAILY                       
                                                                                
PR120    DS    0H                                                               
         BAS   RE,PRINT                                                         
         BCT   R3,PR110                                                         
                                                                                
         TM    PRTSTAT,PRTKBUY     FOR COMPARE-ORDERS ONLY                      
         BZ    PR170                                                            
                                                                                
PR130    DS    0H                                                               
         LA    RF,PRBUYKEY                                                      
         USING RDARKEY,RF                                                       
         CLC   RDARKSEQ,0(R2)                                                   
         BNE   PR170               MATCHING K BUY TO AGENCY BUY?                
         DROP  RF                                                               
                                                                                
         MVC   KEY+28(4),2(R2)     YES                                          
         MVC   AIO,AIO3            PUT BUY RECORD IN IO3                        
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
                                                                                
         LA    R6,KEY              PUT CONTRACT RECORD IN IO2                   
         USING RCONKEY,R6                                                       
         XC    KEY,KEY                                                          
         MVI   RCONPTYP,X'8C'                                                   
         MVC   RCONPREP,AGENCY                                                  
         MVC   RCONPCON,CCONNUM                                                 
         DROP  R6                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'RCONKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
                                                                                
         MVC   DMCB2(4),VOUTDAY    SET UP ROUTINE ADDRESS BLOCK                 
         MVC   DMCB2+4(4),UNTIME                                                
         MVC   DMCB2+8(4),DATCON                                                
         MVC   DMCB2+12(4),ADDAY                                                
         MVC   DMCB2+16(4),REPFACS                                              
*                                                                               
         GOTO1 =V(REGENPBY),DMCB,AIO3,(32,LISTAREA),DMCB2,AIO2,,       X        
               ACOMFACS,RR=RELO                                                 
*                                                                               
         ZIC   R3,DMCB+4                                                        
         LTR   R3,R3                                                            
         BZ    PR160               NO BUYLINES FOUND                            
         TM    PRTSTAT,PRTDAB      COME FROM BUYLIST SCR, DO NOT                
         BO    PR160                PRINT CONTRACT BUYLINE                      
         LA    R4,LISTAREA                                                      
                                                                                
PR140    DS    0H                                                               
         LA    R6,P                                                             
         TM    PRTSTAT,PRTKBUY     FOR COMPARE-ORDERS ONLY                      
         BZ    PR150                                                            
         MVC   P(9),=C'** CON **'                                               
         LA    R6,10(R6)                                                        
                                                                                
PR150    DS    0H                  DON'T PRINT CONTRACT MOD CODES               
         MVC   2(L'LISTAREA,R6),2(R4)                                           
         BAS   RE,PRINT                                                         
         LA    R4,L'LISTAREA(R4)                                                
         BCT   R3,PR140                                                         
         BAS   RE,PRINT            SKIP A LINE BETWEEN BUYS                     
                                                                                
PR160    DS    0H                  SEE IF MORE THAN ONE CONTRACT BUY            
         LA    R2,L'SORTAREA(R2)   FROM THIS AGENCY BUY                         
         B     PR130                                                            
                                                                                
PR170    DS    0H                  GET NEXT AGENCY BUY                          
         CLI   BUYNUM,0            NO MORE?                                     
         BE    PR180                                                            
                                                                                
         MVC   KEY(L'PRBUYKEY),PRBUYKEY                                         
                                                                                
PR170D   USING RDARKEY,KEY                                                      
         MVC   PR170D.RDARKSEQ,BUYNUM                                           
         DROP  PR170D                                                           
                                                                                
         MVI   RDUPDATE,C'N'                                                    
         BAS   RE,MODKEY                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(RDARKSRT-RDARKEY),KEYSAVE                                    
         BNE   PR180                                                            
* * * *                                                                         
         CLC   PRBUYKEY,KEY        ALREADY PROCESSED?                           
         JE    PR180                                                            
* * * *                                                                         
         MVC   PRBUYKEY,KEY                                                     
         B     PR100                                                            
                                                                                
PR180    DS    0H                  PRINT TOTALS                                 
         XC    KEY,KEY                                                          
         MVC   KEY(RDARKRT-RDARKEY),PRBUYKEY                                    
         MVI   KEY+RDARKRT-RDARKEY,X'50' TRAILER RECORD                         
         MVI   RDUPDATE,C'N'                                                    
         BAS   RE,MODKEY                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    PR183                                                            
*                                                                               
* VARIOUS ORDER MIGHT NOT HAVE TRAILER RECORD:                                  
*                                                                               
         LA    R4,P                                                             
         TM    PRTSTAT,PRTKBUY     FOR COMPARE-ORDERS, SHIFT TO RIGHT           
         BZ    *+8                                                              
         LA    R4,10(R4)                                                        
*                                                                               
         TM    MISCFLAG,MSFTRADE                                                
         BZ    *+10                                                             
         MVC   48(5,R4),=C'TRADE'                                               
*                                                                               
         MVC   54(11,R4),=C'TOTAL$ 0.00'                                        
         BAS   RE,PRINT                                                         
                                                                                
         LA    R4,P                                                             
         TM    PRTSTAT,PRTKBUY     FOR COMPARE-ORDERS, SHIFT TO RIGHT           
         BZ    *+8                                                              
         LA    R4,10(R4)                                                        
*                                                                               
         TM    MISCFLAG,MSFTRADE                                                
         BZ    *+10                                                             
         MVC   48(5,R4),=C'TRADE'                                               
*                                                                               
         MVC   54(14,R4),=C'TOTAL SPOTS: 0'                                     
*                                                                               
         BAS   RE,PRINT                                                         
         B     PR185                                                            
                                                                                
PR183    DS    0H                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
                                                                                
         L     R6,AIO                                                           
         USING RDARELE9,R6                                                      
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,PRINT                                                         
                                                                                
         LA    R4,P                                                             
         TM    PRTSTAT,PRTKBUY     FOR COMPARE-ORDERS, SHIFT TO RIGHT           
         BZ    *+8                                                              
         LA    R4,10(R4)                                                        
*                                                                               
         TM    MISCFLAG,MSFTRADE                                                
         BZ    *+10                                                             
         MVC   48(5,R4),=C'TRADE'                                               
*                                                                               
         MVC   54(6,R4),=C'TOTAL$'                                              
         EDIT  (P6,RDARTDOL),(14,61(R4)),2,COMMAS=YES                           
*                                                                               
* ESPERANTO XML ORDERS DO NOT PROVIDE TOTALS. USE CALCULATED TOTALS             
* FROM REDAR04                                                                  
*                                                                               
         TM    SVDARFL2,SF2XML                                                  
         BZ    PR184                                                            
         OC    AGORDTOT,AGORDTOT                                                
         BZ    PR184                                                            
         EDIT  AGORDTOT,(14,61(R4)),2,COMMAS=YES                                
*                                                                               
PR184    DS    0H                                                               
         BAS   RE,PRINT                                                         
                                                                                
         LA    R4,P                                                             
         TM    PRTSTAT,PRTKBUY     FOR COMPARE-ORDERS, SHIFT TO RIGHT           
         BZ    *+8                                                              
         LA    R4,10(R4)                                                        
*                                                                               
         TM    MISCFLAG,MSFTRADE                                                
         BZ    *+10                                                             
         MVC   48(5,R4),=C'TRADE'                                               
*                                                                               
         MVC   54(12,R4),=C'TOTAL SPOTS:'                                       
         EDIT  RDARTSPT,(5,70(R4))                                              
*                                                                               
* ESPERANTO XML ORDERS DO NOT PROVIDE TOTALS. USE CALCULATED TOTALS             
* FROM REDAR04                                                                  
*                                                                               
         TM    SVDARFL2,SF2XML                                                  
         BZ    PR184A                                                           
         OC    AGSPTTOT,AGSPTTOT                                                
         BZ    PR184A                                                           
         EDIT  AGSPTTOT,(5,70(R4))                                              
*                                                                               
PR184A   DS    0H                                                               
         BAS   RE,PRINT                                                         
         DROP  R6                                                               
                                                                                
PR185    DS    0H                                                               
         XC    H5(L'HEAD5),H5      BUY PRINTING DONE, CLEAR BUY HEADING         
         XC    H6(L'HEAD6),H6                                                   
                                                                                
* PRINT STANDARD COMMENTS, IF ANY                                               
         TM    CMTFLAG,CMTSTAND+CMTORD+CMTREJ                                   
         BZ    PR220                                                            
                                                                                
         BAS   RE,PRINT                                                         
         MVI   P,C'*'                                                           
         MVC   P+1(109),P                                                       
         MVC   P+3(19),=C' START OF COMMENTS '                                  
         BAS   RE,PRINT                                                         
                                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(RDARKRT-RDARKEY),PRBUYKEY                                    
         LA    R6,KEY                                                           
         USING RDARKEY,R6                                                       
         MVI   RDARKRT,X'20'       TYPE STANDARD COMMENT                        
         DROP  R6                                                               
                                                                                
         MVI   RDUPDATE,C'N'                                                    
         BAS   RE,MODKEY                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
         BNE   PR190               IF STANDARD COMMENT                          
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         BAS   RE,PRTSTCMT                                                      
                                                                                
* PRINT ORDER COMMENTS, IF ANY                                                  
PR190    DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(RDARKRT-RDARKEY),PRBUYKEY                                    
         LA    R6,KEY                                                           
         USING RDARKEY,R6                                                       
         MVI   RDARKRT,X'30'       TYPE ORDER COMMENT                           
         DROP  R6                                                               
                                                                                
         MVI   RDUPDATE,C'N'                                                    
         BAS   RE,MODKEY                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
         BNE   PR200               IF ORDER COMMENT                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         BAS   RE,PRTORCMT                                                      
                                                                                
PR200    DS    0H                  PRINT REJECTION CMTS IF REQUESTED            
         XC    KEY,KEY                                                          
         MVC   KEY(RDARKRT-RDARKEY),PRBUYKEY                                    
         LA    R6,KEY                                                           
         USING RDARKEY,R6                                                       
         MVI   RDARKRT,X'10'       REJECT CMT IS IN THE HEADER                  
         DROP  R6                                                               
                                                                                
         MVI   RDUPDATE,C'N'                                                    
         BAS   RE,MODKEY                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
         BNE   PR210                                                            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BNE   PR210               IF REJECT COMMENT                            
         BAS   RE,PRTREJCT                                                      
                                                                                
PR210    DS    0H                                                               
         BAS   RE,PRINT                                                         
         MVI   P,C'*'                                                           
         MVC   P+1(109),P                                                       
         MVC   P+3(17),=C' END OF COMMENTS '                                    
         BAS   RE,PRINT                                                         
                                                                                
PR220    DS    0H                  PRINT HIATUS, IF ANY                         
         XC    KEY,KEY                                                          
         MVC   KEY(RDARKRT-RDARKEY),PRBUYKEY                                    
         LA    R6,KEY                                                           
         USING RDARKEY,R6                                                       
         MVI   RDARKRT,X'15'                                                    
         DROP  R6                                                               
                                                                                
         MVI   RDUPDATE,C'N'                                                    
         BAS   RE,MODKEY                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
         BNE   PR225                                                            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         BAS   RE,HIATUS                                                        
                                                                                
* IF WE CAME FROM THE SELECT OR REJECT SCREEN, WE CAN CLOSE THE PQ              
*    BEFORE EXITING                                                             
PR225    DS    0H                                                               
         TM    PRTSTAT,PRTONE                                                   
         BZ    PR240                                                            
                                                                                
PR230    DS    0H                                                               
         MVI   SPMODE,X'FF'                                                     
         BAS   RE,PRINT            CLOSE PRINT QUEUE                            
                                                                                
PR240    DS    0H                  RESTORE GLOBAL CONTRACT VALUES               
         OC    SVCONNUM,SVCONNUM     BEFORE EXITING                             
         BZ    PRX                                                              
         ZAP   WORK+20(5),=P'0'                                                 
         MVO   WORK+20(5),SVCONNUM                                              
         XC    FAKEHDR,FAKEHDR                                                  
         LA    R2,FAKEHDR          SETUP FAKE FIELD HEADER                      
         EDIT  (P5,WORK+20),(8,8(R2)),ALIGN=LEFT TO CALL VCON WITH              
         STC   R0,5(R2)            SET LENGTH OF FIELD                          
         MVI   0(R2),16            FIELD HEADER LENGTH                          
         MVI   4(R2),X'08'         SET VALID NUMERIC                            
         GOTO1 VALICON,DMCB,(R2)                                                
                                                                                
PRX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* MODIFY KEY ACCORDING TO THE RECORD WE ARE LOOKING AT                          
***********************************************************************         
MODKEY   DS    0H                                                               
         MVC   KEY(2),SELECTKY                                                  
         TM    PRTSTAT,PRTSHDOW                                                 
         BZ    *+14                                                             
         MVC   KEY(2),=XL2'4101'                                                
         B     MODKX                                                            
         TM    PRTSTAT,PRTCFCON                                                 
         BZ    MODKX                                                            
         MVC   KEY(2),=XL2'5100'                                                
MODKX    DS    0H                                                               
         BR    RE                                                               
***********************************************************************         
* COMPARE AGENCY BUY AGAINST THE CONTRACT BUY(S) TO REPORT ANY                  
* MODIFICATION TO THE AGENCY BUY                                                
***********************************************************************         
GETMODCD NTR1                                                                   
         LA    R4,LISTAREA         AGENCY BUY OUTPUT AREA                       
         MVC   MODCODE,SPACES                                                   
                                                                                
         LA    RF,PRBUYKEY                                                      
         USING RDARKEY,RF                                                       
         CLC   RDARKSEQ,0(R2)                                                   
         BE    GETMD08             MATCHING K BUY TO AGENCY BUY?                
         DROP  RF                                                               
                                                                                
         MVI   CURRMOD,C'A'        NO, MUST BE A NEW AGENCY BUYLINE             
         BAS   RE,UPDMODCD         CHANGES FOUND, UPDATE MOD CODE               
         B     GETMDX                                                           
                                                                                
GETMD05  LA    RF,PRBUYKEY                                                      
         USING RDARKEY,RF                                                       
         CLC   RDARKSEQ,0(R2)                                                   
         BNE   GETMDX              MATCHING K BUY TO AGENCY BUY?                
         DROP  RF                                                               
                                                                                
GETMD08  DS    0H                                                               
         MVC   KEY+28(4),2(R2)     YES                                          
         MVC   AIO,AIO3            PUT BUY RECORD IN IO3                        
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
                                                                                
         LA    R6,KEY              PUT CONTRACT RECORD IN IO2                   
         USING RCONKEY,R6                                                       
         XC    KEY,KEY                                                          
         MVI   RCONPTYP,X'8C'                                                   
         MVC   RCONPREP,AGENCY                                                  
         MVC   RCONPCON,CCONNUM                                                 
         DROP  R6                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'RCONKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
                                                                                
         LR    R3,R5                                                            
         AHI   R3,LISTARE2-MYAREAD                                              
                                                                                
         MVC   DMCB2(4),VOUTDAY    SET UP ROUTINE ADDRESS BLOCK                 
         MVC   DMCB2+4(4),UNTIME                                                
         MVC   DMCB2+8(4),DATCON                                                
         MVC   DMCB2+12(4),ADDAY                                                
         MVC   DMCB2+16(4),REPFACS                                              
*                                                                               
         GOTO1 =V(REGENPBY),DMCB,AIO3,(32,(R3)),DMCB2,AIO2,,           X        
               ACOMFACS,RR=RELO                                                 
         ZIC   R3,DMCB+4                                                        
         LTR   R3,R3                                                            
         BZ    GETMDX              NO BUYLINES FOUND                            
                                                                                
         LR    R6,R5               CONTRACT BUY OUTPUT AREA                     
         AHI   R6,LISTARE2-MYAREAD                                              
         USING LBUYD,R6                                                         
         OC    0(L'LISTAREA,R4),SPACES                                          
         OC    0(L'LISTAREA,R6),SPACES                                          
                                                                                
GETMD10  DS    0H                                                               
         CLC   LBUYDAYS,SPACES     SKIP IF EITHER ARE SPACES                    
         BE    GETMD20                                                          
         CLC   LBUYDAYS-LBUYD(L'LBUYDAYS,R4),SPACES                             
         BE    GETMD20                                                          
         CLC   LBUYDAYS,LBUYDAYS-LBUYD(R4)                                      
         BE    *+12                COMPARE DAYS                                 
         MVI   CURRMOD,C'D'                                                     
         BAS   RE,UPDMODCD         CHANGES FOUND, UPDATE MOD CODE               
                                                                                
GETMD20  DS    0H                                                               
         CLC   LBUYTIME,SPACES     SKIP IF EITHER ARE SPACES                    
         BE    GETMD30                                                          
         CLC   LBUYTIME-LBUYD(L'LBUYTIME,R4),SPACES                             
         BE    GETMD30                                                          
         CLC   LBUYTIME,LBUYTIME-LBUYD(R4)                                      
         BE    *+12                COMPARE TIME                                 
         MVI   CURRMOD,C'T'                                                     
         BAS   RE,UPDMODCD         CHANGES FOUND, UPDATE MOD CODE               
                                                                                
GETMD30  DS    0H                                                               
         CLC   LBUYLENM(4),SPACES  SKIP IF EITHER ARE SPACES                    
         BE    GETMD40                                                          
         CLC   LBUYLENM-LBUYD(4,R4),SPACES                                      
         BE    GETMD40                                                          
         CLC   LBUYLENM(4),LBUYLENM-LBUYD(R4)                                   
         BE    *+12                COMPARE LENGTH                               
         MVI   CURRMOD,C'L'                                                     
         BAS   RE,UPDMODCD         CHANGES FOUND, UPDATE MOD CODE               
                                                                                
GETMD40  DS    0H                                                               
         CLC   LBUYDATE,SPACES     SKIP IF EITHER ARE SPACES                    
         BE    GETMD50                                                          
         CLC   LBUYDATE-LBUYD(L'LBUYDATE,R4),SPACES                             
         BE    GETMD50                                                          
         CLC   LBUYDATE,LBUYDATE-LBUYD(R4)                                      
         BE    *+12                COMPARE EFFECTIVE DATES                      
         MVI   CURRMOD,C'E'                                                     
         BAS   RE,UPDMODCD         CHANGES FOUND, UPDATE MOD CODE               
                                                                                
GETMD50  DS    0H                                                               
         CLC   LBUYNPW,SPACES      SKIP IF EITHER ARE SPACES                    
         BE    GETMD60                                                          
         CLC   LBUYNPW-LBUYD(L'LBUYNPW,R4),SPACES                               
         BE    GETMD60                                                          
         CLC   LBUYNPW,LBUYNPW-LBUYD(R4)                                        
         BE    *+12                COMPARE NPW                                  
         MVI   CURRMOD,C'S'                                                     
         BAS   RE,UPDMODCD         CHANGES FOUND, UPDATE MOD CODE               
                                                                                
GETMD60  DS    0H                                                               
         CLC   LBUYRATE,SPACES     SKIP IF EITHER ARE SPACES                    
         BE    GETMD70                                                          
         CLC   LBUYRATE-LBUYD(L'LBUYRATE,R4),SPACES                             
         BE    GETMD70                                                          
         CLC   LBUYRATE,LBUYRATE-LBUYD(R4)                                      
         BE    *+12                COMPARE RATES                                
         MVI   CURRMOD,C'R'                                                     
         BAS   RE,UPDMODCD         CHANGES FOUND, UPDATE MOD CODE               
                                                                                
GETMD70  DS    0H                                                               
         LA    R4,L'LISTAREA(R4)                                                
         LA    R6,L'LISTAREA(R6)                                                
         OC    0(L'LISTAREA,R4),SPACES                                          
         OC    0(L'LISTAREA,R6),SPACES                                          
         BCT   R3,GETMD85                                                       
         B     GETMD130                                                         
                                                                                
GETMD85  DS    0H                                                               
         CLC   LBUYSPT,SPACES      MORE ORBITS?                                 
         BE    GETMD90                                                          
         BCT   R3,GETMD10                                                       
         B     GETMD130                                                         
                                                                                
GETMD90  DS    0H                  COMPARE PROGRAM                              
         CLC   =C'P=',LBUYPROG-LBUYD(R4) COMMENTS ONLY                          
         BNE   GETMD130                                                         
         CLC   LBUYPROG+3(L'LBUYPROG),LBUYPROG-LBUYD+2(R4)                      
         BE    GETMD130                                                         
         MVI   CURRMOD,C'Z'                                                     
         BAS   RE,UPDMODCD         CHANGES FOUND, UPDATE MOD CODE               
         B     GETMD130                                                         
                                                                                
* SKIP COMMENTS CHECK                                                           
*GETMD100 DS    0H                  AFTER PROGRAM, CHECK ONLY COMMENTS          
*         CLC   LBUYCMMT,LBUYCMMT-LBUYD(R4)                                     
*         BE    GETMD120                                                        
*         MVI   CURRMOD,C'Z'                                                    
*         BAS   RE,UPDMODCD         CHANGES FOUND, UPDATE MOD CODE              
*                                                                               
*GETMD120 DS    0H                                                              
*         LA    R4,L'LISTAREA(R4)                                               
*         LA    R6,L'LISTAREA(R6)                                               
*         OC    0(L'LISTAREA,R4),SPACES                                         
*         OC    0(L'LISTAREA,R6),SPACES                                         
*         BCT   R3,GETMD100                                                     
                                                                                
GETMD130 DS    0H                  SEE IF MORE THAN ONE CONTRACT BUY            
         LA    R2,L'SORTAREA(R2)   FROM THIS AGENCY BUY                         
         B     GETMD05                                                          
                                                                                
GETMDX   DS    0H                                                               
         CLI   LISTAREA+1,C'C'     SKIP FOR SOFT DELETE                         
         BE    *+10                                                             
         MVC   LISTAREA(L'MODCODE),MODCODE                                      
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* UDPATE CURRENT AGENCY BUY MODIFICATION CODES                                  
***********************************************************************         
UPDMODCD NTR1                                                                   
         CLI   MODCODE,C'*'        IF ALREADY MULTIPLE CHANGES, EXIT            
         BE    UPDMDX                                                           
         CLC   CURRMOD,MODCODE     SEE IF CODE EXISTED ALREADY                  
         BE    UPDMDX              IF YES, EXIT                                 
         CLC   CURRMOD,MODCODE+1                                                
         BE    UPDMDX                                                           
         CLI   MODCODE,C' '        ROOM TO RECORD 2 CHANGES                     
         BNE   UPDMD10                                                          
         MVC   MODCODE(1),CURRMOD                                               
         B     UPDMDX                                                           
                                                                                
UPDMD10  DS    0H                                                               
         CLI   MODCODE+1,C' '                                                   
         BNE   UPDMD20                                                          
         MVC   MODCODE+1(1),CURRMOD                                             
         B     UPDMDX                                                           
                                                                                
UPDMD20  DS    0H                  MORE THAN 2 CHANGES DETECTED                 
         MVC   MODCODE,=C'* '                                                   
                                                                                
UPDMDX   DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* SORT CONTRACT BUYS BY AGENCY BUY NUMBER AND CONTRACT BUY NUMBER               
* USES IO3 AS TEMPORARY IO AREA                                                 
***********************************************************************         
SORTBUYS NTR1                                                                   
         SR    R3,R3                                                            
         LA    R4,SORTAREA                                                      
         XCEF  (R4),1530                                                        
                                                                                
         XC    KEY,KEY             CONSTRUCT CONTRACT BUY KEY                   
         LA    R6,KEY                                                           
         USING RBUYKEY,R6                                                       
         MVI   RBUYKTYP,X'0B'                                                   
         MVC   RBUYKREP,AGENCY                                                  
         PACK  WORK(1),CCONNUM+3(1) REVERSE THE COMPLIMENT                      
         PACK  WORK+1(1),CCONNUM+2(1)                                           
         PACK  WORK+2(1),CCONNUM+1(1)                                           
         PACK  WORK+3(1),CCONNUM(1)                                             
         MVC   RBUYKCON,WORK                                                    
         DROP  R6                                                               
                                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(22),KEYSAVE                                                  
         BNE   SORTBX                                                           
                                                                                
         MVC   AIO,AIO3                                                         
         L     R6,AIO                                                           
         USING RBUYREC,R6                                                       
                                                                                
SORTB10  DS    0H                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVC   0(1,R4),RBUYAGBL                                                 
         MVC   1(1,R4),RBUYKLIN                                                 
         MVC   2(4,R4),KEY+28                                                   
         LA    R4,L'SORTAREA(R4)                                                
         LA    R3,1(R3)            NUMBER OF SORT RECORDS                       
                                                                                
         OC    RBUYAGBL,RBUYAGBL   IF NO REFERENCE TO AGENCY BUY#               
         BNZ   *+8                 PRINT ALL THESE CONTRACT BUYS AT THE         
         MVI   0(R4),X'FF'         END (SHOULDN'T HAPPEN, BUT J.I.C.)           
                                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         CLC   KEY(22),KEYSAVE                                                  
         BE    SORTB10                                                          
         DROP  R6                                                               
                                                                                
         GOTO1 XSORT,DMCB,SORTAREA,(R3),6,2,0                                   
                                                                                
SORTBX   DS    0H                                                               
         MVC   AIO,AIO1                                                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PRINT PAGE1 "HEADINGS"                                                        
*   CONTRACT DETAILS ARE SET                                                    
*   DARE RECORD IS IN AIO                                                       
***********************************************************************         
PRPAGE1  NTR1                                                                   
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         OI    MISCFLAG,MSUNLINK                                                
         OC    RDARREP#,RDARREP#                                                
         BZ    *+8                                                              
         NI    MISCFLAG,FF-MSUNLINK                                             
*                                                                               
         LA    R2,H5                                                            
         USING PRHEAD,R2                                                        
*                                                                               
         MVC   PRALBL(17),=C'---AGY--- ORDER#:'                                 
         ZAP   WORK+20(5),=P'0'                                                 
         MVO   WORK+20(5),RDARKORD                                              
         EDIT  (P5,WORK+20),(8,PRACODE),ALIGN=LEFT,ZERO=NOBLANK,FILL=0          
*                                                                               
         MVC   PRRLBL(20),=C'---REP--- CONTRACT#:'                              
*                                                                               
         TM    MISCFLAG,MSUNLINK                                                
         BO    PPGE05                                                           
         ZAP   WORK+20(5),=P'0'                                                 
         MVO   WORK+20(5),CCONKNUM                                              
         EDIT  (P5,WORK+20),(8,PRRCODE),ALIGN=LEFT                              
*                                                                               
*----------------*                                                              
* REP AGENCY                                                                    
*----------------*                                                              
PPGE05   DS    0H                                                               
         LA    R2,L'H7(R2)                                                      
*                                                                               
         MVC   PRALBL(7),=C'AGENCY:'                                            
         MVC   PRACODE(L'RDARKAGY),RDARKAGY                                     
         OC    RDARKAOF,RDARKAOF                                                
         BZ    PPGE08                                                           
*                                                                               
         LA    RE,PRACODE+L'PRACODE-1                                           
         CLI   0(RE),X'40'                                                      
         BH    *+8                                                              
         BCT   RE,*-8                                                           
*                                                                               
         MVI   1(RE),C'-'                                                       
         MVC   2(L'RDARKAOF,RE),RDARKAOF    AGENCY OFFICE                       
*                                                                               
PPGE08   DS    0H                                                               
         MVC   PRANAME,RDARAGNM    AGENCY AGENCY NAME                           
         MVC   PRRLBL(7),=C'AGENCY:'                                            
*                                                                               
         TM    MISCFLAG,MSUNLINK                                                
         BO    PPGE20                                                           
         MVC   PRRCODE(L'CCONKAGY),CCONKAGY                                     
*                                                                               
         CLC   CCONKAOF,SPACES     OFFICE?                                      
         BE    PPGE20              NO                                           
         LA    RE,PRRCODE+L'PRRCODE-1                                           
         CLI   0(RE),X'40'                                                      
         BH    *+8                                                              
         BCT   RE,*-8                                                           
*                                                                               
         MVI   1(RE),C'-'                                                       
         MVC   2(L'CCONKAOF,RE),CCONKAOF    AGENCY OFFICE                       
*                                                                               
         MVC   PRRNAME,EAGYNAM1                                                 
*                                                                               
*****                                                                           
** ADVERTISER INFO                                                              
*****                                                                           
PPGE20   DS    0H                                                               
         LA    R2,L'H7(R2)                                                      
         MVC   PRALBL(11),=C'ADVERTISER:'                                       
         MVC   PRACODE(L'RDARCLI),RDARCLI                                       
         MVC   PRANAME(L'RDARCLNM),RDARCLNM                                     
*                                                                               
         MVC   PRRLBL(11),=C'ADVERTISER:'                                       
*                                                                               
         TM    MISCFLAG,MSUNLINK                                                
         BO    PPGE30                                                           
         MVC   PRRCODE(L'CCONKADV),CCONKADV                                     
         MVC   PRRNAME,EADVNAME                                                 
*                                                                               
*                                                                               
PPGE30   DS    0H                                                               
         LA    R2,L'H7(R2)                                                      
***                                                                             
* PRODUCT                                                                       
***                                                                             
         L     R6,AIO                                                           
         USING RDARCLEM,R6                                                      
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
         MVC   PRALBL(8),=C'PRODUCT:'                                           
         MVC   PRACODE(L'RDARPRD1),RDARPRD1                                     
         MVC   PRANAME,RDARPRN1                                                 
         MVC   PRRLBL(8),=C'PRODUCT:'                                           
         TM    MISCFLAG,MSUNLINK                                                
         BO    PPGE40                                                           
         MVC   PRRCODE(L'CCONPRD),CCONPRD                                       
         MVC   PRRNAME,EPRDNAME                                                 
                                                                                
* SECOND PRODUCT                                                                
PPGE40   DS    0H                                                               
         LA    R2,L'H7(R2)                                                      
*                                                                               
         MVC   PRALBL(12),=C'2ND PRODUCT:'                                      
         MVC   PRACODE(L'RDARPRD2),RDARPRD2                                     
         MVC   PRANAME,RDARPRN2                                                 
*        MVC   PRRLBL,=C'PRODUCT'                                               
*        MVC   PRRCODE,CCONPRD                                                  
*        MVC   PRRNAME,EPRDNAME                                                 
*                                                                               
         DROP  R6                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
*                                                                               
         LA    R2,L'H7(R2)                                                      
* ESTIMATE #                                                                    
         MVC   PRALBL(11),=C'ESTIMATE #:'                                       
         EDIT  RDAREST#,(4,PRACODE),ALIGN=RIGHT,FILL=0                          
         DROP  R6                                                               
*                                                                               
* USE ALPHANUMERIC ESTIMATE IF PRESENT                                          
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'11'                                                     
         BRAS  RE,GETEL                                                         
         BNE   PPGE50                                                           
         USING RDAREL2M,R6                                                      
         OC    RDAR2EST,RDAR2EST                                                
         BZ    PPGE50                                                           
         MVC   PRACODE,SPACES                                                   
         MVC   PRACODE(6),RDAR2EST                                              
         DROP  R6                                                               
*                                                                               
PPGE50   DS    0H                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
*                                                                               
         MVC   PRRLBL(11),=C'ESTIMATE #:'                                       
*                                                                               
         TM    MISCFLAG,MSUNLINK                                                
         BO    PPGE60                                                           
         MVC   PRRCODE(L'CCONIEST),CCONIEST                                     
         CLC   CCONIEST,SPACES                                                  
         BH    *+10                                                             
         MVC   PRRCODE(L'CCONXEST),CCONXEST                                     
*                                                                               
PPGE60   DS    0H                                                               
         LA    R2,L'H7(R2)                                                      
* FLIGHT DATE                                                                   
         MVC   PRALBL(12),=C'FLIGHT DATE:'                                      
         GOTO1 DATCON,DMCB,(2,RDARESST),(5,PRACODE)                             
         MVI   PRACODE+8,C'-'                                                   
         GOTO1 DATCON,DMCB,(2,RDARESEN),(5,PRACODE+9)                           
         MVC   PRRLBL(12),=C'FLIGHT DATE:'                                      
         TM    MISCFLAG,MSUNLINK                                                
         BO    PPGE80                                                           
         MVC   PRRCODE(L'ECONDATE),ECONDATE                                     
* LEN                                                                           
         MVC   PRRNAME+L'PRRNAME+1(4),=C'LEN:'                                  
         LA    R6,MYWORK           PUT OUT LENGTHS AND CLASS                    
         MVC   MYWORK,SPACES                                                    
         LA    R4,CSARLEN                                                       
         LA    R3,6                                                             
                                                                                
PPGE70   OC    0(2,R4),0(R4)                                                    
         BZ    PPGE75              NO LENGTH IN THIS FIELD                      
         CH    R3,=H'6'                                                         
         BE    *+12                FIRST TIME                                   
         MVI   0(R6),C','                                                       
         LA    R6,1(R6)                                                         
                                                                                
         ZIC   RE,1(R4)            LENGTH                                       
         MH    RE,=H'10'           LEFT ONE POSITION                            
         ZIC   RF,0(R4)            CLASS                                        
         AR    RE,RF                                                            
         EDIT  (RE),(5,0(R6)),1,ALIGN=LEFT                                      
         SH    R0,=H'2'                                                         
         AR    R6,R0               LENGTH OF OUTPUT                             
         CLC   0(2,R6),=C'.0'                                                   
         BE    *+8                                                              
         LA    R6,2(R6)                                                         
         MVC   0(2,R6),SPACES                                                   
                                                                                
PPGE75   LA    R4,2(R4)                                                         
         BCT   R3,PPGE70                                                        
         MVC   PRRNAME+L'PRRNAME+6(3),MYWORK                                    
*                                                                               
PPGE80   DS    0H                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
*                                                                               
         LA    R2,L'H7(R2)                                                      
         MVC   PRALBL(8),=C'STATION:'                                           
         MVC   PRACODE(4),RDARKSTA AGENCY STATION                               
         CLI   RDARKSTA+4,C' '                                                  
         BE    PPGE125                                                          
         MVC   PRACODE+4(3),=C'- M'                                             
         MVC   PRACODE+5(1),RDARKSTA+4                                          
         CLI   RDARKSTA+4,C'T'                                                  
         BNE   *+8                                                              
         MVI   PRACODE+6,C'V'                                                   
         CLI   RDARKSTA+3,C' '                                                  
         BNE   PPGE125                                                          
         MVC   PRACODE+3(3),PRACODE+4                                           
         MVI   PRACODE+6,C' '                                                   
                                                                                
PPGE125  DS    0H                                                               
         LR    R3,R2               SAVE OFF R2                                  
         OC    RDARREP#,RDARREP#   NOT LINKED TO CONTRACT                       
         BNZ   PPGE128             NEED TO READ STA REC FOR MARKET NAME         
         XC    EMKTNAME,EMKTNAME                                                
         XC    FAKEHDR,FAKEHDR                                                  
         LA    RE,FAKEHDR          SETUP FAKE FIELD HEADER                      
         MVI   0(RE),14            FIELD HEADER LENGTH                          
         MVI   5(RE),6             SET FIELD LENGTH                             
         MVC   8(6,RE),PRACODE                                                  
         LR    R2,RE                                                            
         GOTO1 VALISTA             GET MARKET NAME                              
         BNZ   PPGE128                                                          
         MVC   EMKTNAME,WORK+10                                                 
                                                                                
PPGE128  DS    0H                                                               
         LR    R2,R3               RESTORE R2                                   
         MVC   PRANAME,EMKTNAME                                                 
*                                                                               
         MVC   PRRLBL(8),=C'STATION:'                                           
*                                                                               
         TM    MISCFLAG,MSUNLINK                                                
         BO    PPGE150                                                          
*                                                                               
         MVC   PRRCODE(4),CCONKSTA                                              
         MVC   PRRCODE+4(3),=C'-TV'                                             
         CLI   CCONKSTA+4,C' '                                                  
         BE    PPGE135                                                          
         CLI   CCONKSTA+4,C'L'                                                  
         BNE   PPGE130                                                          
         MVC   PRRCODE+4(3),=C'-L '                                             
         B     PPGE135                                                          
PPGE130  DS    0H                                                               
         MVC   PRRCODE+4(3),=C'- M'                                             
         MVC   PRRCODE+5(1),CCONKSTA+4                                          
                                                                                
PPGE135  DS    0H                                                               
         CLI   CCONKSTA+3,C' '                                                  
         BNE   PPGE140                                                          
         MVC   PRRCODE+3(3),PRRCODE+4                                           
         MVI   PRRCODE+6,C' '                                                   
PPGE140  DS    0H                                                               
         MVC   PRRNAME,EMKTNAME                                                 
*                                                                               
PPGE150  DS    0H                                                               
         LA    R2,L'H7(R2)                                                      
         MVC   PRALBL(6),=C'BUYER:'                                             
         MVC   PRACODE(L'RDARBUYR),RDARBUYR                                     
         MVC   PRRLBL(20),=C'REP OFF/SALESPERSON:'                              
*                                                                               
         TM    MISCFLAG,MSUNLINK                                                
         BO    PPGE180                                                          
*                                                                               
         MVC   PRRCODE(L'CCONKOFF),CCONKOFF                                     
         MVC   PRRCODE+5(L'CCONSAL),CCONSAL                                     
         MVC   PRRNAME,ESALNAME                                                 
*                                                                               
PPGE180  DS    0H                                                               
         LA    R2,L'H7(R2)                                                      
         MVC   PRACODE(26),=C'(   )    -     (EX       )'                       
         MVC   PRACODE+1(3),RDARBTEL                                            
         MVC   PRACODE+6(3),RDARBTEL+3                                          
         MVC   PRACODE+10(4),RDARBTEL+6                                         
         MVC   PRACODE+19(6),RDARBXTN EXTENSION                                 
         MVC   PRRLBL(8),=C'DAYPART:'                                           
* DAYPART                                                                       
         TM    MISCFLAG,MSUNLINK                                                
         BO    PPGE260X                                                         
*                                                                               
         LA    R1,WORK             DAYPARTS                                     
         XC    WORK,WORK                                                        
         XC    BYTE,BYTE           0=1 CHAR DPT CODE                            
         LA    R4,CSARDPT                                                       
         LA    R3,6                                                             
                                                                                
PPGE220  LA    R6,DPTABLE                                                       
         OC    0(1,R4),0(R4)                                                    
         BZ    PPGE250             NO DPT                                       
*                                                                               
         CLI   1(R4),X'C1'                                                      
         BL    PPGE230             NO CHAR IN CPP PART OF FIELD                 
         CLI   1(R4),X'E9'                                                      
         BH    PPGE230             NO CHAR IN CPP PART OF FIELD                 
*                                  3 CHAR DPT CODE                              
         MVI   BYTE,1                                                           
         B     PPGE240                                                          
*                                                                               
PPGE230  CLC   3(1,R6),0(R4)                                                    
         BE    PPGE240             ONE CHAR CODE IN RSARDPT                     
         CLI   0(R6),X'FF'                                                      
         BE    PPGE260                                                          
         LA    R6,L'DPTABLE(R6)                                                 
         B     PPGE230                                                          
*                                                                               
PPGE240  CH    R3,=H'6'                                                         
         BE    *+12                                                             
         MVI   0(R1),C','                                                       
         LA    R1,1(R1)                                                         
                                                                                
         CLI   BYTE,0                                                           
         BNE   *+14                DPT IS THREE CHAR CODE                       
         MVC   0(3,R1),0(R6)       DPT 1 CHAR MOVE FROM TABLE                   
         B     *+10                                                             
         MVC   0(3,R1),0(R4)       MOVE DPT 3 CHAR FROM RSARDPT                 
         LA    R1,3(R1)                                                         
                                                                                
PPGE250  LA    R4,3(R4)            NEXT DPT FIELD                               
         BCT   R3,PPGE220                                                       
*                                                                               
PPGE260  DS    0H                                                               
         MVC   PRRCODE,WORK                                                     
         L     R6,AIO                                                           
PPGE260X DS    0H                                                               
         BAS   RE,PRINT                                                         
*                                                                               
*-----------------------------*                                                 
* REP AND AGENCY ORDER TOTALS                                                   
*-----------------------------*                                                 
         GOTOR CALTOT                                                           
*                                                                               
         LA    R2,P                                                             
         CLI   RDARCORT,C'T'                                                    
         BE    PRTOT05                                                          
         MVC   PRALBL(24),=C'TOTAL SPOTS AND DOLLARS:'                          
         EDIT  AGSPTTOT,(4,PRALBL+25),ZERO=NOBLANK                              
         EDIT  AGORDTOT,(16,PRANAME),2,ZERO=NOBLANK,COMMAS=YES,        X        
               ALIGN=LEFT,FLOAT=$                                               
         B     PRTOT08                                                          
*                                                                               
PRTOT05  DS    0H                                                               
*                                                                               
         MVC   PRALBL(30),=C'TRADE TOTAL SPOTS AND DOLLARS:'                    
         EDIT  AGSPTTOT,(4,PRALBL+31),ZERO=NOBLANK                              
         EDIT  AGORDTOT,(16,PRANAME+7),2,ZERO=NOBLANK,COMMAS=YES,      X        
               FLOAT=$,ALIGN=LEFT                                               
         LA    RE,PRRLBL-6                                                      
         MVC   0(5,RE),=C'TRADE'                                                
*                                                                               
PRTOT08  DS    0H                                                               
*                                                                               
         MVC   PRRLBL(24),=C'TOTAL SPOTS AND DOLLARS:'                          
         TM    MISCFLAG,MSUNLINK                                                
         BO    PPGE300                                                          
         EDIT  RPSPTTOT,(4,PRRLBL+25),ZERO=NOBLANK                              
         EDIT  RPORDTOT,(16,PRRNAME),2,ZERO=NOBLANK,COMMAS=YES,FLOAT=$,X        
               ALIGN=LEFT                                                       
*                                                                               
         MVC   P2+71(12),=C'DIFFERENCE:'                                        
         ZICM  RE,AGSPTTOT,2                                                    
         ZICM  RF,RPSPTTOT,2                                                    
         SR    RE,RF                                                            
         BP    PRTOT10                                                          
         EDIT  (RE),(5,P2+82),FLOAT=-,ZERO=NOBLANK                              
         B     PRTOT20                                                          
*                                                                               
PRTOT10  DS    0H                                                               
         EDIT  (RE),(5,P2+82),FLOAT=+                                           
*                                                                               
PRTOT20  DS    0H                                                               
         MVI   P2+88,C'+'                                                       
         ZICM  RE,AGORDTOT,4                                                    
         ZICM  RF,RPORDTOT,4                                                    
         SR    RE,RF                                                            
         BP    PRTOT30                                                          
         MVI   P2+88,C'-'                                                       
         LTR   RE,RE                                                            
         BNZ   PRTOT30                                                          
         MVI   P2+88,C' '                                                       
*                                                                               
PRTOT30  DS    0H                                                               
         EDIT  (RE),(16,P2+89),2,ALIGN=LEFT,ZERO=NOBLANK,COMMAS=YES,   X        
               FLOAT=$                                                          
*                                                                               
PPGE300  DS    0H                                                               
         BAS   RE,PRINT                                                         
         BAS   RE,PRINT                                                         
*&&DO                                                                           
         TM    CMTFLAG,CMTSTAND+CMTORD+CMTREJ                                   
         BNZ   PPGE350                                                          
         TM    MISCFLAG,MSFTRADE                                                
         BO    PPGE350                                                          
         TM    SVDARFL2,SF2XML                                                  
         BO    PPGE350                                                          
         B     PPGE600                                                          
*                                                                               
PPGE350  DS    0H                                                               
         LA    R2,P                                                             
         MVC   0(CMTFLW2Q,R2),CMTFLW2                                           
         LA    R2,P+CMTFLW2Q+1                                                  
*                                                                               
         TM    SVDARFL2,SF2XML                                                  
         BO    PPGE400                                                          
         LA    R4,0                                                             
         B     PPGE450                                                          
*                                                                               
PPGE400  DS    0H                                                               
         MVC   0(CMTFLWQ,R2),CMTFLW                                             
         LA    R2,CMTFLWQ+1(R2)                                                 
         LA    R4,3                                                             
*                                                                               
PPGE450  DS    0H                                                               
         TM    CMTFLAG,CMTSTAND+CMTORD+CMTREJ                                   
         BNZ   PPGE500                                                          
         TM    MISCFLAG,MSFTRADE                                                
         BZ    PPGE550                                                          
*                                                                               
PPGE500  DS    0H                                                               
         LA    R2,0(R4,R2)                                                      
         MVC   0(CMTFLW3Q,R2),CMTFLW3                                           
         LA    R2,CMTFLW3Q+1(R2)                                                
PPGE550  DS    0H                                                               
         MVC   0(CMTFLW2Q,R2),CMTFLW2                                           
PPGE580  DS    0H                                                               
         BAS   RE,PRINT                                                         
*&&                                                                             
PPGE600  DS    0H                                                               
*        GOTOR PRWKHD              PRINT WEEK HEADING                           
*        GOTOR PRFILTER            PRINT FILTER, IF NOT DEFAULT                 
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
FF       EQU   X'FF'                                                            
***********************************************************************         
* PRINT VARIOUS COMMENT                                                         
***********************************************************************         
VARIOUS  NTR1                                                                   
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         BAS   RE,PRINT                                                         
         MVC   P(73),=107C'*'                                                   
         BAS   RE,PRINT                                                         
         MVC   P(73),=C'* THIS VARIOUS ORDER WILL BE REPLACED BY BRAND X        
               ORDERS UPON CONFIRMATION *'                                      
         BAS   RE,PRINT                                                         
         MVC   P(73),=107C'*'                                                   
         BAS   RE,PRINT                                                         
         BAS   RE,PRINT                                                         
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT BRAND COMMENT                                                           
***********************************************************************         
BRAND    NTR1                                                                   
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         BAS   RE,PRINT                                                         
         MVC   P(107),=107C'*'                                                  
         BAS   RE,PRINT                                                         
         MVC   P(107),=C'* THIS ORDER IS ONE OF THE BRANDS BOUGHT ON THX        
               E VARIOUS DARE AGENCY ORDER #         , CONTRACT #      X        
                   *'                                                           
*                                                                               
         MVC   KEY(L'RDARKEY),SVAGYVKY                                          
         MVI   RDUPDATE,C'N'                                                    
         BAS   RE,MODKEY                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
         BNE   BRANDX                                                           
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         GOTO1 HEXOUT,DMCB,RDARKORD,P+76,L'RDARKORD,=C'TOG'                     
         GOTO1 HEXOUT,DMCB,RDARREP#,P+97,L'RDARREP#,=C'TOG'                     
*                                                                               
         BAS   RE,PRINT                                                         
         MVC   P(107),=107C'*'                                                  
         BAS   RE,PRINT                                                         
         BAS   RE,PRINT                                                         
*                                                                               
BRANDX   DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT STANDARD COMMENTS                                                       
***********************************************************************         
PRTSTCMT NTR1                                                                   
         BAS   RE,PRINT                                                         
         MVC   P+4(24),=C'AGENCY STANDARD COMMENT:'                             
         BAS   RE,PRINT                                                         
                                                                                
PRTST10  DS    0H                                                               
         L     R6,AIO                                                           
         USING RDARSCEL,R6                                                      
         MVI   ELCODE,2                                                         
         BRAS  RE,GETEL                                                         
         BNE   PRTSTX                                                           
                                                                                
PRTST20  DS    0H                                                               
         ZIC   R1,RDARSCLN                                                      
         CLI   RDARSCLN,3                                                       
         BL    *+8                                                              
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+7(0),RDARSCCM                                                  
         BAS   RE,PRINT                                                         
         BRAS  RE,NEXTEL                                                        
         BE    PRTST20                                                          
         DROP  R6                                                               
                                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
                                                                                
         CLC   KEY(RDARKSEQ-RDARKEY),KEYSAVE                                    
         BNE   PRTSTX                                                           
                                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         B     PRTST10                                                          
                                                                                
PRTSTX   DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
***********************************************************************         
* PRINT ORDER COMMENTS                                                          
***********************************************************************         
PRTORCMT NTR1                                                                   
         BAS   RE,PRINT                                                         
         MVC   P+4(21),=C'AGENCY ORDER COMMENT:'                                
         BAS   RE,PRINT                                                         
*                                                                               
         TM    MISCFLAG,MSFTRADE                                                
         BZ    PRTOR05                                                          
         MVC   P+7(15),=C'* TRADE ORDER *'                                      
         BAS   RE,PRINT                                                         
*                                                                               
PRTOR05  DS    0H                                                               
         L     R6,AIO                                                           
         USING RDAROREL,R6                                                      
         MVI   ELCODE,2                                                         
         BRAS  RE,GETEL                                                         
         BNE   PRTORX                                                           
                                                                                
PRTOR10  DS    0H                                                               
         ZIC   R1,RDARORLN                                                      
         CLI   RDARORLN,3                                                       
         BL    *+8                                                              
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+7(0),RDARORCM                                                  
         BAS   RE,PRINT                                                         
         BRAS  RE,NEXTEL                                                        
         BE    PRTOR10                                                          
                                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
                                                                                
         CLC   KEY(RDARKSEQ-RDARKEY),KEYSAVE                                    
         BNE   PRTORX                                                           
                                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         B     PRTOR05                                                          
                                                                                
PRTORX   DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* PRINT REJECTION COMMENTS                                                      
***********************************************************************         
PRTREJCT NTR1                                                                   
         BAS   RE,PRINT                                                         
         MVC   P+4(22),=C'REP REJECTION COMMENT:'                               
         BAS   RE,PRINT                                                         
                                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BNE   PRTREJX                                                          
         USING RDARRCEM,R6                                                      
                                                                                
PRTREJ10 DS    0H                                                               
         CLI   RDARRCEN,2                                                       
         BNH   PRTREJ20                                                         
         ZIC   R1,RDARRCEN                                                      
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+7(0),RDARRCCM                                                  
                                                                                
PRTREJ20 DS    0H                                                               
         BAS   RE,PRINT                                                         
         BRAS  RE,NEXTEL                                                        
         BE    PRTREJ10                                                         
                                                                                
PRTREJX  DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT HIATUS INFO                                                             
***********************************************************************         
HIATUS   NTR1                                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL                                                         
         BNE   HIATUSX                                                          
         USING RDARHIE2,R6                                                      
                                                                                
         CLI   RDARHIL2,2          SKIP IF NO DATES                             
         BNH   HIATUSX                                                          
                                                                                
         BAS   RE,PRINT                                                         
         MVC   P(22),=C'AGENCY HIATUS DATE(S):'                                 
         BAS   RE,PRINT                                                         
                                                                                
         ZIC   R2,RDARHIL2                                                      
         SH    R2,=H'2'            SUBTRACT OVERHEAD AND                        
         SRL   R2,1                DIVIDE BY 2 TO GET NUMBER OF ENTRIES         
                                                                                
         LA    R6,RDARHIDT                                                      
         DROP  R6                                                               
                                                                                
         LA    R4,P                                                             
                                                                                
* IF WEEKLY, WILL TRY TO COLLASP DATES. IE AUG24-3W                             
                                                                                
HIATUS03 DS    0H                                                               
         LA    R3,1                NUMBER OF CONSECUTIVE WEEKS                  
         GOTO1 DATCON,DMCB,(2,0(R6)),(4,0(R4))                                  
         LA    R4,5(R4)                                                         
                                                                                
         TM    PRTSTAT,PRTDAILY    PRINT ALL IF DAILY                           
         BO    HIATUS10                                                         
                                                                                
         CH    R2,=H'1'                                                         
         BNH   HIATUS08                                                         
                                                                                
HIATUS05 DS    0H                                                               
         GOTO1 DATCON,DMCB,(2,0(R6)),(0,MYWORK)                                 
         GOTO1 DATCON,DMCB,(2,2(R6)),(0,MYWORK+6)                               
         GOTO1 ADDAY,DMCB,MYWORK,WORK,7                                         
         CLC   WORK(6),MYWORK+6    IF NEXT DATE IS EXACTLY ONE WEEK             
         BNE   HIATUS08            AWAY, KEEP LOOKING                           
                                                                                
         MVC   MYWORK(6),MYWORK+6                                               
         LA    R3,1(R3)                                                         
         LA    R6,2(R6)                                                         
         BCTR  R2,0                                                             
         CH    R2,=H'1'                                                         
         BH    HIATUS05                                                         
         SR    R2,R2                                                            
                                                                                
HIATUS08 DS    0H                                                               
         MVI   0(R4),C'-'                                                       
         EDIT  (R3),(2,1(R4)),ALIGN=LEFT                                        
         AR    R4,R0                                                            
         MVI   1(R4),C'W'                                                       
         LA    R4,2(R4)                                                         
                                                                                
HIATUS10 DS    0H                                                               
         LTR   R2,R2                                                            
         BZ    HIATUS80                                                         
         BCTR  R2,0                                                             
         LTR   R2,R2                                                            
         BZ    HIATUS80                                                         
                                                                                
         LA    R4,1(R4)                                                         
         LA    R6,2(R6)                                                         
         LA    RF,P+110                                                         
         CR    R4,RF                                                            
         BL    HIATUS03                                                         
         BAS   RE,PRINT                                                         
         LA    R4,P                                                             
         B     HIATUS03                                                         
                                                                                
HIATUS80 DS    0H                                                               
         BAS   RE,PRINT                                                         
                                                                                
HIATUSX  DS    0H                                                               
         B     EXIT                                                             
*                                                                               
* CONSTANTS                                                                     
*                                                                               
BUYTITLE DC    C'MC LIN DAYS         TIMES       LEN EFF. DATES'                
         DC    C'    NW         NPW       RATE SPT  PROGRAM NAME'               
BUYTITLQ EQU   *-BUYTITLE                                                       
*                                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT A LINE                                                                  
***********************************************************************         
PRINT    NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
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
*                                                                               
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
         EJECT                                                                  
***********************************************************************         
* PRINT AGENCY ORDER INFORMATION                                                
***********************************************************************         
PRTAGY   NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
                                                                                
         MVC   P+43(5),=C'AGY #'                                                
         ZAP   WORK(5),=P'0'                                                    
         MVO   WORK(5),RDARKORD                                                 
         EDIT  (P5,WORK),(8,P+49),ALIGN=LEFT                                    
         GOTO1 =A(PRINT2),RR=RELO                                               
                                                                                
* AGENCY                                                                        
         MVC   P(3),=C'AGY'                                                     
         MVC   P+6(L'RDARKAGY),RDARKAGY                                         
         OC    RDARKAOF,RDARKAOF                                                
         BZ    PRTA05                                                           
         LA    RE,P+6                                                           
         MVI   P+9,C' '                                                         
         CLI   0(RE),C' '                                                       
         BE    *+12                                                             
         LA    RE,1(RE)                                                         
         B     *-12                                                             
         MVI   0(RE),C'-'                                                       
         MVC   1(2,RE),RDARKAOF     AGENCY OFFICE                               
                                                                                
PRTA05   DS    0H                                                               
         MVC   P+15(34),RDARAGNM    AGENCY NAME                                 
                                                                                
PRTA10   DS    0H                                                               
         MVC   P+50(5),=C'BUYER'                                                
         MVC   P+58(24),RDARBUYR    FROM BUYER                                  
         GOTO1 =A(PRINT2),RR=RELO                                               
                                                                                
* PHONE                                                                         
         MVC   P+50(6),=C'PHONE#'                                               
         MVC   P+58(26),=C'(   )    -     (EX       )'                          
         MVC   P+59(3),RDARBTEL                                                 
         MVC   P+64(3),RDARBTEL+3                                               
         MVC   P+68(4),RDARBTEL+6                                               
         MVC   P+77(6),RDARBXTN EXTENSION                                       
         DROP  R6                                                               
                                                                                
* ADVERTISER                                                                    
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL                                                         
         USING RDARCLEM,R6                                                      
         MVC   P(3),=C'ADV'                                                     
         MVC   P+6(6),RDARCLI                                                   
         MVC   P+15(34),RDARCLNM                                                
         DROP  R6                                                               
         GOTO1 =A(PRINT2),RR=RELO                                               
                                                                                
* STATION                                                                       
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         MVC   P(3),=C'STA'                                                     
         MVC   P+6(4),RDARKSTA                                                  
         MVC   P+10(3),=C'- M'                                                  
         MVC   P+11(1),RDARKSTA+4                                               
         CLI   RDARKSTA+4,C'T'                                                  
         BNE   PRTA30                                                           
         MVI   P+12,C'V'                                                        
                                                                                
PRTA30   DS    0H                  UNLINKED ORDERS NEED TO READ STATION         
         OC    RDARREP#,RDARREP#   RECORD FOR MARKET NAME                       
         BNZ   PRTA50                                                           
         XC    EMKTNAME,EMKTNAME                                                
         XC    FAKEHDR,FAKEHDR                                                  
         LA    R2,FAKEHDR          SETUP FAKE FIELD HEADER                      
         MVI   0(R2),14            FIELD HEADER LENGTH                          
         MVI   5(R2),6             SET FIELD LENGTH                             
         MVC   8(6,R2),P+6                                                      
         GOTO1 VALISTA             GET MARKET NAME                              
         BNZ   PRTA40                                                           
         MVC   EMKTNAME,WORK+10                                                 
                                                                                
PRTA40   DS    0H                  RESET IOAREA                                 
         MVC   AIO,AIO1                                                         
                                                                                
PRTA50   DS    0H                                                               
         MVC   P+15(20),EMKTNAME                                                
         DROP  R6                                                               
                                                                                
* PRD 1                                                                         
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL                                                         
         USING RDARCLEM,R6                                                      
         MVC   P+50(4),=C'PRD1'                                                 
         MVC   P+58(4),RDARPRD1                                                 
         MVC   P+63(34),RDARPRN1                                                
         GOTO1 =A(PRINT2),RR=RELO                                               
         DROP  R6                                                               
                                                                                
* EST START-END DATES                                                           
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         MVC   P(5),=C'DATES'                                                   
         LA    R2,P+6                                                           
         GOTO1 DATCON,DMCB,(2,RDARESST),(5,P+6)                                 
         GOTO1 DATCON,DMCB,(2,RDARESST),(3,WORK)                                
         CLC   RTKODATE,WORK       TAKEOVER DATE CHOPPING??                     
         BL    PRTA60                                                           
         GOTO1 DATCON,DMCB,(3,RTKODATE),(5,P+6)                                 
*                                                                               
PRTA60   DS    0H                                                               
         MVI   P+14,C'-'                                                        
         GOTO1 DATCON,DMCB,(2,RDARESEN),(5,P+15)                                
                                                                                
* PRD2                                                                          
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL                                                         
         USING RDARCLEM,R6                                                      
         MVC   P+50(4),=C'PRD2'                                                 
         MVC   P+58(4),RDARPRD2                                                 
         MVC   P+63(34),RDARPRN2                                                
         DROP  R6                                                               
*                                                                               
         L     R6,AIO              EDI ORDERS USE PRODUCT 2 FOR SPECIAL         
         USING RDARREC,R6          STORAGE                                      
         CLC   =C'$EDI$',RDARAGAD  DON'T PRINT IT                               
         BNE   PRTA70                                                           
         XC    P+58(34),P+58                                                    
         DROP  R6                                                               
*                                                                               
PRTA70   DS    0H                                                               
         GOTO1 =A(PRINT2),RR=RELO                                               
*                                                                               
* ESTIMATE NUMBER                                                               
         MVC   P(4),=C'EST#'                                                    
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         EDIT  RDAREST#,(8,P+6),ALIGN=LEFT                                      
         DROP  R6                                                               
*                                                                               
* USE ALPHANUMERIC ESTIMATE IF PRESENT                                          
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'11'                                                     
         BRAS  RE,GETEL                                                         
         BNE   PRTA75                                                           
         USING RDAREL2M,R6                                                      
         OC    RDAR2EST,RDAR2EST                                                
         BZ    PRTA75                                                           
         MVC   P+6(8),SPACES                                                    
         MVC   P+6(6),RDAR2EST                                                  
         DROP  R6                                                               
*                                                                               
* DEMO                                                                          
PRTA75   DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL                                                         
         USING RDARCLEM,R6                                                      
         MVC   P+50(5),=C'DEMOS'                                                
         MVC   P+58(8),RDARTDEM                                                 
         DROP  R6                                                               
*                                                                               
* DEMO CATEGORIES                                                               
*                                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         CLC   =C'$EDI$',RDARAGAD                                               
         BE    PRTA130             SKIP IF EDI                                  
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,X'03'                                                     
         BRAS  RE,GETEL                                                         
         BNE   PRTA130                                                          
         USING RDARDMEL,R6                                                      
         CLC   RDARDEM1(16),SPACES                                              
         BE    PRTA130                                                          
*                                                                               
         LA    R3,0                                                             
         LA    R6,RDARDEM1                                                      
         LA    R4,MYWORK                                                        
         XC    MYWORK(30),MYWORK                                                
         DROP  R6                                                               
*                                                                               
PRTA80   DS    0H                                                               
         AHI   R3,1                                                             
         CLI   0(R6),C'('          USER DEFINED DEMO?                           
         BE    PRTA85              SKIP IT                                      
         MVC   1(1,R4),0(R6)                                                    
         CLI   1(R4),C'T'          FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   1(R4),C'I'                                                       
         PACK  DUB(8),1(3,R6)                                                   
         CVB   RF,DUB                                                           
         STC   RF,2(R4)                                                         
*                                                                               
PRTA85   DS    0H                                                               
         LA    R6,L'RDARDEM1(R6)                                                
         LA    R4,3(R4)                                                         
         CLC   0(4,R6),SPACES                                                   
         BE    PRTA90                                                           
         CHI   R3,4                                                             
         BL    PRTA80                                                           
*                                                                               
PRTA90   DS    0H                                                               
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
         LA    R2,P+58                                                          
         MVC   P+58(32),SPACES                                                  
         LA    R4,WORK                                                          
*                                                                               
         CHI   R3,1                                                             
         BNH   PRTA120                                                          
         AHI   R3,-1                                                            
*                                                                               
PRTA100  DS    0H                                                               
         MVC   0(7,R2),0(R4)                                                    
         AHI   R4,7                                                             
*                                                                               
PRTA110  DS    0H                                                               
         AHI   R2,1                                                             
         CLI   0(R2),C' '                                                       
         BNE   PRTA110                                                          
*                                                                               
         MVI   0(R2),C','                                                       
         AHI   R2,1                                                             
         BCT   R3,PRTA100                                                       
PRTA120  MVC   0(7,R2),0(R4)                                                    
*                                                                               
PRTA130  DS    0H                                                               
         GOTO1 =A(PRINT2),RR=RELO                                               
         GOTO1 =A(PRINT2),RR=RELO                                               
                                                                                
* RECEIVED DATE AND TIME                                                        
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         MVC   P(9),=C'RECEIVED:'                                               
         GOTO1 DATCON,DMCB,(2,RDARDATE),(5,P+10)                                
         EDIT  RDARTIME,(4,MYWORK)                                              
         MVC   P+19(2),MYWORK   FORMAT TO HH:MM                                 
         MVI   P+21,C':'                                                        
         MVC   P+22(2),MYWORK+2                                                 
                                                                                
         MVC   P+33(7),=C'OPENED:'                                              
         OC    RDARREP#,RDARREP#   IF UNLINKED, SKIP                            
         BZ    PRTA140                                                          
         OC    CDARAPDT,CDARAPDT                                                
         BZ    PRTA140                                                          
* OPENED DATE                                                                   
         GOTO1 DATCON,DMCB,(2,CDARAPDT),(5,P+43)                                
* OPENED TIME HH:MM                                                             
         GOTO1 HEXOUT,DMCB,CDARAPTM,P+52,1,=C'TOG'                              
         MVI   P+54,C':'                                                        
         GOTO1 HEXOUT,DMCB,CDARAPTM+1,P+55,1,=C'TOG'                            
                                                                                
* CHECK IF RECALLED                                                             
PRTA140  DS    0H                                                               
         CLC   =C'$EDI$',RDARAGAD  SKIP RECALL/REJECT DATE/TIME FOR EDI         
         BE    PRTA170                                                          
         MVC   P+66(9),=C'RECALLED:'                                            
         CLI   RDARBSTS,C'C'                                                    
         BNE   PRTA150                                                          
         L     R6,AIO                                                           
         MVI   ELCODE,X'40'        RECALL TIME                                  
         BRAS  RE,GETEL                                                         
         BNE   PRTA170                                                          
         USING RDARRKEM,R6                                                      
* RECALLED DATE                                                                 
         GOTO1 DATCON,DMCB,(2,RDARRKDT),(5,P+76)                                
* RECALLED TIME HH:MM                                                           
         GOTO1 HEXOUT,DMCB,RDARRKTM,P+85,1,=C'TOG'                              
         MVI   P+87,C':'                                                        
         GOTO1 HEXOUT,DMCB,RDARRKTM+1,P+88,1,=C'TOG'                            
         B     PRTA170                                                          
                                                                                
PRTA150  DS    0H                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         MVC   P+66(9),=C'REJECTED:'                                            
         OC    RDARREP#,RDARREP#   IF UNLINKED, REJECT TIME IN HEADER           
         BZ    PRTA160                                                          
         OC    CDARRJDT,CDARRJDT                                                
         BZ    PRTA170                                                          
* REJECTED DATE                                                                 
         GOTO1 DATCON,DMCB,(2,CDARRJDT),(5,P+76)                                
* REJECTED TIME HH:MM                                                           
         GOTO1 HEXOUT,DMCB,CDARRJTM,P+85,1,=C'TOG'                              
         MVI   P+87,C':'                                                        
         GOTO1 HEXOUT,DMCB,CDARRJTM+1,P+88,1,=C'TOG'                            
         B     PRTA170                                                          
         DROP  R6                                                               
                                                                                
PRTA160  DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'20'        REJECT TIME                                  
         BRAS  RE,GETEL                                                         
         BNE   PRTA170                                                          
         USING RDARRTEM,R6                                                      
* REJECTED DATE                                                                 
         GOTO1 DATCON,DMCB,(2,RDARRTDT),(5,P+76)                                
* REJECTED TIME HH:MM                                                           
         GOTO1 HEXOUT,DMCB,RDARRTTM,P+85,1,=C'TOG'                              
         MVI   P+87,C':'                                                        
         GOTO1 HEXOUT,DMCB,RDARRTTM+1,P+88,1,=C'TOG'                            
         DROP  R6                                                               
                                                                                
PRTA170  DS    0H                                                               
         GOTO1 =A(PRINT2),RR=RELO                                               
                                                                                
         LA    R3,6                CLEAR HEADING, WANT CONTRACT INFO            
         LA    R4,H5               ON FIRST PAGE ONLY                           
                                                                                
PRTA180  XC    0(132,R4),0(R4)                                                  
         LA    R4,132(R4)                                                       
         BCT   R3,PRTA180                                                       
         B     EXIT2                                                            
         EJECT                                                                  
***********************************************************************         
* PRINT A LINE                                                                  
***********************************************************************         
PRINT2   NTR1  BASE=*,LABEL=*                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT2                                                            
         EJECT                                                                  
***********************************************************************         
* REPORT CONTRACT SPECS                                                         
***********************************************************************         
HEDSPECS DS    0H                                                               
         SPROG 0                                                                
         PSPEC H1,1,AGYNAME                                                     
         PSPEC H1,120,PAGE                                                      
         PSPEC H2,91,REQUESTOR                                                  
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
***********************************************************************         
* REPORT HEADHOOK ROUTINE                                                       
***********************************************************************         
HOOK     NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
*                                                                               
         MVC   H1+45(32),=C'****** AGENCY ORDER REPORT ******'                  
*                                                                               
         MVC   H2+54(14),=C'ORIGINAL ORDER'                                     
         TM    SVDARFL2,SF2XML                                                  
         BZ    *+10                                                             
         MVC   H2+51(18),=C'XML ORIGINAL ORDER'                                 
*                                                                               
         TM    PRTSTAT,PRTDAB                                                   
         BZ    HK010                                                            
         MVC   H1+47(28),=C'****** BUY LIST REPORT ******'                      
         CLI   RDARRNUM,0                                                       
         BE    HK010                                                            
         MVC   H2+51(17),=C'AGENCY REVISION #'                                  
         EDIT  RDARRNUM,(3,H2+69),ALIGN=LEFT,ZERO=NOBLANK,FILL=0                
*                                                                               
HK010    DS    0H                                                               
         TM    MISCFLAG,MSFTRADE                                                
         BZ    HK020                                                            
*                                                                               
         LA    R2,H2+77                                                         
         CLI   0(R2),X'40'                                                      
         BH    *+8                                                              
         BCT   R2,*-8                                                           
*                                                                               
         MVC   2(13,R2),=C'- TRADE ORDER'                                       
*                                                                               
HK020    DS    0H                                                               
*                                                                               
         BAS   RE,GETPRTDT         GET PRINT DATE/TIME                          
                                                                                
         MVC   H1+90(8),PRTDATE                                                 
         MVC   H1+99(2),=C'AT'                                                  
         UNPK  DUB,PRTTIME                                                      
         MVC   H1+102(2),DUB+2     TIME                                         
         MVI   H1+104,C'.'                                                      
         MVC   H1+105(2),DUB+4                                                  
                                                                                
         MVC   H2(13),=C'ORDER STATUS:'                                         
         MVC   H2+14(8),ORDSTAT                                                 
*                                                                               
         MVC   H3+50(9),=C'RECEIVED:'                                           
         GOTO1 DATCON,DMCB,(2,RDARDATE),(5,H3+60)                               
         EDIT  RDARTIME,(4,WORK)                                                
         MVC   H3+70(2),WORK     FORMAT TO HH:MM                                
         MVI   H3+72,C':'                                                       
         MVC   H3+73(2),WORK+2                                                  
*                                                                               
         MVC   H3+90(8),RDARKSTA   PRINT STATION/MKT NAME                       
         MVC   H3+94(3),=C'- M'                                                 
         MVC   H3+95(1),RDARKSTA+4                                              
         CLI   RDARKSTA+4,C'T'                                                  
         BNE   *+8                                                              
         MVI   H3+96,C'V'                                                       
         MVI   H3+97,C'/'                                                       
*                                                                               
         OC    RDARREP#,RDARREP#   NOT LINKED TO CONTRACT                       
         BNZ   HK040               NEED TO READ STA REC FOR MARKET NAME         
         XC    EMKTNAME,EMKTNAME                                                
         XC    FAKEHDR,FAKEHDR                                                  
         LA    R2,FAKEHDR          SETUP FAKE FIELD HEADER                      
         MVI   0(R2),14            FIELD HEADER LENGTH                          
         MVI   5(R2),6             SET FIELD LENGTH                             
         MVC   8(6,R2),H3+90                                                    
         GOTO1 VALISTA             GET MARKET NAME                              
         BNZ   HK040                                                            
         MVC   EMKTNAME,WORK+10                                                 
                                                                                
HK040    EQU   *                                                                
         MVC   H3+98(L'EMKTNAME),EMKTNAME                                       
*&&DO                                                                           
         BRAS  RE,PRTSUB           GET SUBREP NAME INTO WORK                    
         MVC   H4+90(L'RDARKREP),RDARKREP                                       
         MVI   H4+92,C'/'                                                       
         MVC   H4+93(L'RREPNAME),WORK                                           
*&&                                                                             
HK050    EQU   *                                                                
*&&DO                                                                           
         MVC   H5+53(16),=C'* AGENCY ORDER *'                                   
*                                                                               
         TM    MISCFLAG,MSFTRADE                                                
         BZ    *+10                                                             
         MVC   H5+50(22),=C'* TRADE AGENCY ORDER *'                             
*&&                                                                             
*                                                                               
HOOKX    DS    0H                                                               
EXIT2    XIT1                                                                   
*&&DO                                                                           
***********************************************************************         
* REPORT HEADHOOK ROUTINE                                                       
***********************************************************************         
HOOK     NTR1  BASE=*,LABEL=*                                                   
         BAS   RE,GETPRTDT         GET PRINT DATE/TIME                          
                                                                                
         MVC   H1+78(8),PRTDATE                                                 
         MVC   H1+87(2),=C'AT'                                                  
         UNPK  DUB,PRTTIME                                                      
         MVC   H1+90(2),DUB+2     TIME                                          
         MVI   H1+92,C'.'                                                       
         MVC   H1+93(2),DUB+4                                                   
                                                                                
         MVC   H2(13),=C'ORDER STATUS:'                                         
         MVC   H2+14(8),ORDSTAT                                                 
                                                                                
         MVC   H3+41(16),=C'* AGENCY ORDER *'                                   
*                                                                               
         TM    MISCFLAG,MSFTRADE                                                
         BZ    *+10                                                             
         MVC   H3+38(22),=C'* TRADE AGENCY ORDER *'                             
*                                                                               
HOOKX    DS    0H                                                               
EXIT2    XIT1                                                                   
*&&                                                                             
***********************************************************************         
* GET CURRENT DATE/TIME                                                         
***********************************************************************         
GETPRTDT NTR1                                                                   
* TODAY'S DATE                                                                  
         GOTO1 DATCON,DMCB,(5,0),(8,PRTDATE)                                    
                                                                                
* CURRENT TIME                                                                  
         THMS  DDSTIME=YES                         GET CURRENT TIME             
         ST    R1,DUB                                                           
         ZAP   PRTTIME,DUB(4)                                                   
         ST    R0,DUB              ADDJUST FOR DDS TIME                         
         AP    PRTTIME,DUB(4)                                                   
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* PRTOTAL: PRINT TOTAL                                                          
**********************************************************************          
CALTOT   NTR1  BASE=*,WORK=(R4,DLWORKQ),LABEL=*                                 
         USING DLWORKD,R4                                                       
         MVC   DLSVKEY,KEY                                                      
         MVC   DLSVIO,AIO                                                       
*                                                                               
         LA    RE,DLIO                                                          
         ST    RE,AIO                                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(RDARKRT-RDARKEY),PRBUYKEY                                    
         MVI   KEY+RDARKRT-RDARKEY,X'50' TRAILER RECORD                         
         MVI   RDUPDATE,C'N'                                                    
*                                                                               
         MVC   KEY(2),SELECTKY                                                  
         TM    PRTSTAT,PRTSHDOW                                                 
         BZ    *+14                                                             
         MVC   KEY(2),=XL2'4101'                                                
         B     *+18                                                             
         TM    PRTSTAT,PRTCFCON                                                 
         BZ    *+10                                                             
         MVC   KEY(2),=XL2'5100'                                                
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   CTOTX                                                            
*                                                                               
CTOT100  DS    0H                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING RDARELE9,R6                                                      
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB+L'DUB-L'RDARTDOL(L'RDARTDOL),RDARTDOL                        
         CVB   RF,DUB                                                           
         STCM  RF,15,AGORDTOT                                                   
         MVC   AGSPTTOT,RDARTSPT+1                                              
*                                                                               
* ESPERANTO XML ORDERS DO NOT PROVIDE TOTALS. USE CALCULATED TOTALS             
* FROM REDAR04                                                                  
*                                                                               
         TM    SVDARFL2,SF2XML                                                  
         BZ    CTOT105                                                          
         OC    AGORDTOT,AGORDTOT                                                
         BNZ   CTOT105                                                          
         OC    AGSPTTOT,AGSPTTOT                                                
         BNZ   CTOT105                                                          
         MVC   AGORDTOT,GTOTAL$                                                 
         MVC   AGSPTTOT,GSPT#+2                                                 
         DROP  R6                                                               
*                                                                               
CTOT105  DS    0H                                                               
         XC    RPSPTTOT,RPSPTTOT   CLEAR TOTALS FOR PRINTING                    
         XC    RPORDTOT,RPORDTOT                                                
*                                                                               
         PACK  COMPCON+0(1),CCONNUM+3(1)                                        
         PACK  COMPCON+1(1),CCONNUM+2(1)                                        
         PACK  COMPCON+2(1),CCONNUM+1(1)                                        
         PACK  COMPCON+3(1),CCONNUM+0(1)                                        
*                                                                               
         LA    R6,KEY                                                           
         USING RBUYKEY,R6                                                       
         XC    KEY,KEY                                                          
         MVC   RBUYKTYP(2),=X'0B00' AGENCY BUY SHADOW RECORD TYPE               
         MVC   RBUYKREP,SELECTKY+RDARKREP-RDARKEY                               
         MVC   RBUYKCON,COMPCON    INSERT CONTRACT #, 9/COMP/REV                
         DROP  R6                                                               
*                                                                               
CTOT110  DS    0H                                                               
         NI    DMINBTS,X'FF'-X'08' DON'T PASS DELETES                           
         GOTO1 HIGH                                                             
*                                                                               
CTOT120  DS    0H                                                               
         CLC   KEY(22),KEYSAVE                                                  
         BNE   CTOT200             AGENCY CANCELLED ALL BUYS                    
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING RBUYREC,R6          CHECK AND SKIP                               
         CLI   RBUYCHGI,C'C'       CANCELLED BUT NOT DELETED                    
         BE    CTOT140                                                          
*                                                                               
         ZICM  RE,RPSPTTOT,2                                                    
         ZICM  RF,RBUYTSPT,2                                                    
         AR    RE,RF                                                            
         STCM  RE,3,RPSPTTOT                                                    
*                                                                               
         ZICM  RE,RPORDTOT,4                                                    
         ZICM  RF,RBUYTCOS,4                                                    
         AR    RE,RF                                                            
         STCM  RE,15,RPORDTOT                                                   
*                                                                               
CTOT140  DS    0H                                                               
         NI    DMINBTS,X'FF'-X'08' DON'T PASS DELETES                           
         GOTO1 SEQ                                                              
         B     CTOT120                                                          
CTOT200  DS    0H                                                               
CTOTX    DS    0H                                                               
         MVC   KEY,DLSVKEY                                                      
         MVC   AIO,DLSVIO                                                       
         GOTO1 HIGH                                                             
         XIT1                                                                   
***********************************************************************         
* PRINT XML AND COMMENT NOTE                                                    
***********************************************************************         
PRTNOTE  NTR1  BASE=*,LABEL=*                                                   
         TM    CMTFLAG,CMTSTAND+CMTORD+CMTREJ                                   
         BNZ   PNOT350                                                          
         TM    MISCFLAG,MSFTRADE                                                
         BO    PNOT350                                                          
         TM    SVDARFL2,SF2XML                                                  
         BO    PNOT350                                                          
         B     PNOTX                                                            
*                                                                               
PNOT350  DS    0H                                                               
         LA    R2,P                                                             
         MVC   0(CMTFLW2Q,R2),CMTFLW2        ***                                
         LA    R2,P+CMTFLW2Q+1                                                  
*                                                                               
         TM    SVDARFL2,SF2XML                                                  
         BO    PNOT400                                                          
         LA    R4,0                                                             
         B     PNOT450                                                          
*                                                                               
PNOT400  DS    0H                                                               
         MVC   0(CMTFLWQ,R2),CMTFLW          THIS IS AN XML ORDER               
         LA    R2,CMTFLWQ+1(R2)                                                 
         LA    R4,3                                                             
*                                                                               
PNOT450  DS    0H                                                               
         TM    CMTFLAG,CMTSTAND+CMTORD+CMTREJ                                   
         BNZ   PNOT500                                                          
         TM    MISCFLAG,MSFTRADE                                                
         BZ    PNOT550                                                          
*                                                                               
PNOT500  DS    0H                                                               
         LA    R2,0(R4,R2)                                                      
         MVC   0(CMTFLW3Q,R2),CMTFLW3                                           
         LA    R2,CMTFLW3Q+1(R2)                                                
PNOT550  DS    0H                                                               
         MVC   0(CMTFLW2Q,R2),CMTFLW2                                           
PNOT580  DS    0H                                                               
         BRAS  RE,PRINT2                                                        
PNOTX    DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
CMTFLW   EQU   *                                                                
         DC    C'THIS IS AN XML ORDER'                                          
CMTFLWQ  EQU   *-CMTFLW                                                         
*                                                                               
CMTFLW2  EQU   *                                                                
         DC    C'***'                                                           
CMTFLW2Q EQU   *-CMTFLW2                                                        
*                                                                               
CMTFLW3  EQU   *                                                                
         DC    C'COMMENTS FOLLOW ORDER'                                         
CMTFLW3Q EQU   *-CMTFLW3                                                        
PRHEAD   DSECT                                                                  
PRALBL   DS    CL20                                                             
         DS    CL1                                                              
PRACODE  DS    CL9                                                              
         DS    CL1                                                              
PRANAME  DS    CL20                                                             
         DS    CL14                                                             
*                                                                               
PRRLBL   DS    CL20                                                             
         DS    CL1                                                              
PRRCODE  DS    CL9                                                              
         DS    CL1                                                              
PRRNAME  DS    CL20                                                             
PRHEADQ  EQU   *-PRHEAD                                                         
***********************************************************************         
*                                                                               
* APPLICATION STORAGE AREA                                                      
*                                                                               
***********************************************************************         
MYAREAD  DSECT                                                                  
RELO     DS    A                                                                
DMCB2    DS    6F                                                               
MYWORK   DS    CL64                                                             
SVCONNUM DS    XL4                 SAVED CONTRACT NUMBER                        
PRBUYKEY DS    CL32                AGENCY BUY KEY                               
BUYNUM   DS    X                                                                
SCMTPAGE DS    X                   STANDARD COMMENT PAGE(RECORD) NUMBER         
*                                                                               
PRTSTAT  DS    X                   STATUS FLAG FOR PRINTING                     
PRTONE   EQU   X'80'               ONLY ONE REPORT WILL BE PRINTED              
*                                  SINCE WE CAME FROM SELECT OR REJECT          
PRTNEWPG EQU   X'40'               EJECT PAGE FOR NEXT REPORT                   
PRTCLOSE EQU   X'20'               WE'RE ALL DONE, CLOSE PQ AND EXIT            
PRTDAILY EQU   X'10'               BUY IS DAILY                                 
PRTKBUY  EQU   X'08'               CONTRACT BUYS FOUND, PRINT CONTRACT          
*                                   BUYS SIDE-BY-SIDE WITH AGENCY'S             
PRTCFCON EQU   X'04'               PRINT MAKE GOOD INDICATOR                    
*                                                                               
PRTSHDOW EQU   X'02'               PRINT THE SHADOW REC                         
*                                                                               
PRTDAB   EQU   X'01'               PRINTING DAB REPORT, DO NOT                  
*                                  PRINT CONTRACT BUYLINE                       
PRTSTAT2 DS    X                   STATUS FLAG FOR PRINTING 2                   
PRTMGIND EQU   X'80'               PRINT MAKE GOOD INDICATOR                    
*                                  SINCE WE CAME FROM SELECT OR REJECT          
CMTFLAG  DS    X                   COMMENT PRINT FLAG                           
CMTSTAND EQU   X'80'               THERE ARE STANDARD COMMENTS                  
CMTORD   EQU   X'40'               THERE ARE ORDER COMMENTS                     
CMTREJ   EQU   X'20'               THERE ARE REJECTION COMMENTS                 
*                                                                               
MISCFLAG DS    X                                                                
MSFTRADE EQU   X'80'               ORDER IS TRADE, SHOW INDICATORS              
MSUNLINK EQU   X'40'               ORDER IS UNLINK                              
*                                                                               
SVDARFL2 DS    X                                                                
SF2XML   EQU   X'80'               XML ORDER                                    
*                                                                               
SPLKEYAD DS    CL133               EXTENDED SPOOLKEY AREA                       
FAKEHDR  DS    CL16                A FAKE HEADER FOR VCON                       
PSTATION DS    CL5                 STATION                                      
PHDLN    DS    XL4                 CONTRACT NUMBER                              
PAGYORD  DS    XL4                 AGENCY ORDER NUMBER                          
PMKTNAME DS    CL20                MARKET NAME                                  
ORDSTAT  DS    CL8                 ORDER STATUS                                 
PRTDATE  DS    CL8                 PRINT DATE                                   
PRTTIME  DS    F                   PRINT TIME                                   
MODCODE  DS    CL2                 AGENCY BUY MODIFICATION CODE                 
CURRMOD  DS    C                   CURRENT MOD CODE                             
AGYBUYLN DS    X                   # OF AGY BUY DISP LINES IN LISTAREA          
*                                                                               
*                                                                               
RPORDTOT DS    XL4                 REP CONTRACT ORDER TOTAL                     
AGORDTOT DS    XL4                 AGENCY ORDER TOTAL                           
RPSPTTOT DS    XL2                 REP CONTRACT SPOT TOTAL                      
AGSPTTOT DS    XL2                 AGENCY SPOT TOTAL                            
COMPCON  DS    XL4                 CONTRACT #(9/REV/COMP)                       
*                                                                               
SORTAREA DS    255CL6              CONTRACT BUYS SORT AREA                      
*                                  BYTE 1 = AGENCY BUY NUMBER                   
*                                  BYTE 2 = CONTRACT BUY NUMBER                 
*                                  BYTE 3-6 = CONTRACT BUY D/A                  
*                                                                               
LISTAREA DS    35CL81              FOR AGENCY                                   
LISTARE2 DS    35CL81              FOR CONTRACT                                 
*                                                                               
WORKLQ   EQU   *-MYAREAD                                                        
*        PRINT OFF                                                              
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
       ++INCLUDE RECNTPROF                                                      
       ++INCLUDE REDARFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE REDARF0D                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE REDARTWA                                                       
       ++INCLUDE REDARWORKD                                                     
       ++INCLUDE REDARDSECT                                                     
       ++INCLUDE REGENPBYD                                                      
*        PRINT ON                                                               
*                                                                               
DLWORKD  DSECT                                                                  
DLIO     DS    XL2000                                                           
DLSVIO   DS    XL4                                                              
DLSVKEY  DS    XL32                                                             
DLWORKQ  EQU   *-DLWORKD                                                        
*                                                                               
* BUY LIST LINE                                                                 
*                                                                               
LBUYD    DSECT                                                                  
LBUYMC   DS    CL2                                                              
         DS    CL1                                                              
LBUYLINE DS    CL3                                                              
LBUYTYPE DS    CL1                                                              
LBUYDAYS DS    CL12                                                             
         DS    CL1                                                              
LBUYTIME DS    CL11                                                             
LBUYLENM DS    CL1                                                              
LBUYLEN  DS    CL3                                                              
         DS    CL1                                                              
LBUYDATE DS    CL12                                                             
         DS    CL1                                                              
LBUYNW   DS    CL3                                                              
         DS    CL9                                                              
LBUYNPW  DS    CL3                                                              
         DS    CL1                                                              
LBUYRATE DS    CL10                                                             
LBUYSPT  DS    CL4                                                              
         DS    CL2                                                              
LBUYLENQ EQU   *-LBUYMC                                                         
         ORG   LBUYDAYS+2                                                       
LBUYPROG DS    CL34                                                             
         ORG   LBUYDAYS                                                         
LBUYCMMT DS    CL71                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'216REDAR32   05/09/13'                                      
         END                                                                    
