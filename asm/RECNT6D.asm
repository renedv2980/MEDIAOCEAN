*          DATA SET RECNT6D    AT LEVEL 020 AS OF 11/14/03                      
*PHASE T8026DA,+0                                                               
         TITLE 'T8026D - BIAS COPY DOWN (W) FORMAT'                             
*                                                                               
***********************************************************************         
*                                                                     *         
*        RECNT6D (T8026D) --- BIAS COPY DOWN (W) FORMAT               *         
*                                                                     *         
* ------------------------------------------------------------------- *         
* REFER TO RECNTHIST FOR PAST HISTORY                                 *         
*                                                                     *         
* 10JAN01 RHV SPORTS BUYS                                             *         
* 21OCT96 RHV PRINT K ORD CMT AFTER BUYLINES                          *         
* 08OCT96 SKU LOW POWER STATION                                       *         
* 07MAY96 RHV CONTYPE RECORD CONTROLLED WORKSHEET FORMATTING          *         
* 09APR96 RHV SUPPORT 34 BYTE AGY ADDRESS FIELDS FOR ALL USERS        *         
* 02APR96 RHV NEW DISPLAY OF LAST VER# & MOD#                         *         
* 06FEB96 RHV SUPPRESS PRINTING OF EI FIELDS IF EMPTY                 *         
* 01MAR96 RHV SUPPORT PETRY 34 BYTE AGY ADDR FIELDS                   *         
* 06JAN96 SKU PROFILE 24 FOR TYPE D AND PROFILE 20 FOR TYPE N/X       *         
*             PRINT PTP NAME/PHONE OVER SALESPERSON'S                 *         
* 12DEC95 SKU 2K CONTRACT SUPPORT                                     *         
*                                                                     *         
***********************************************************************         
*                                                                               
T8026D   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T8026D,R9                                                      
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     R7,4(R1)                                                         
         USING MYD,R7                                                           
         ST    RB,MYRB                                                          
         ST    R9,MYR9                                                          
         L     RA,VTWA                                                          
         USING TWAD,RA                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         LA    RF,HOOK                                                          
         ST    RF,HEADHOOK                                                      
         LA    RF,HEDSPECS                                                      
         ST    RF,SPECS                                                         
         SPACE 2                                                                
         CLI   8(R1),1             ARE WE SETTING ADDRESSES ONLY                
         BE    EXXMOD                                                           
         SPACE 2                                                                
         CLI   FORMAT,C'W'         BIAS COPY DOWN FORMAT                        
         BE    FMTW                                                             
         DC    H'0'                                                             
         EJECT                                                                  
*           ROUTINE TO FORMAT BUY LINE TO BIAS COPY DOWN SPECIFICATIONS         
         SPACE 1                                                                
FMTW     DS    0H                                                               
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         TM    TWAWSFLG,X'40'      K ORD COMMENT PRINTED YET?                   
         BO    W1G                 YES - SKIP                                   
         OI    TWAWSFLG,X'40'      NO - IT IS NOW                               
*                                                                               
         L     R4,ASVRCOC          REP CONTRACT ORDER COMMENT                   
         LA    R5,10               FOR BCT                                      
         SPACE 1                                                                
         OC    0(60,R4),0(R4)      IS THERE A COMMENT                           
         BZ    W1D                                                              
         MVC   P+6(19),=C'*REP ORDER COMMENT*'                                  
*                                                                               
W1B      OC    0(60,R4),0(R4)      IS THERE A COMMENT                           
         BZ    W1C                                                              
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         OI    TWAWSFLG,X'20'      SINGLE SPACE THIS SECTION                    
         DROP  RF                                                               
         GOTO1 PSTCMT,DMCB,(3,0(R4))  PRINT ORD CMTS                            
*                                                                               
         LA    R4,60(R4)           POINT TO NEXT PART OF COMMENT                
         BCT   R5,W1B                                                           
*                                                                               
W1C      BAS   RE,GOSPOOL                                                       
         SPACE 1                                                                
*                                                                               
W1D      LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         L     R4,ASVSCOC          STA CONTRACT ORDER COMMENT                   
         LA    R5,10               FOR BCT                                      
         OC    0(60,R4),0(R4)      IS THERE A COMMENT                           
         BZ    W1G                                                              
         MVC   P+6(23),=C'*STATION ORDER COMMENT*'                              
*                                                                               
W1E      OC    0(60,R4),0(R4)      IS THERE A COMMENT                           
         BZ    W1F                                                              
*                                                                               
         OI    TWAWSFLG,X'20'      SINGLE SPACE THIS SECTION                    
         DROP  RF                                                               
         MVC   P+30(60),0(R4)                                                   
         OC    P+30(60),SPACES                                                  
         BAS   RE,GOSPOOL                                                       
*                                                                               
         LA    R4,60(R4)           POINT TO NEXT PART OF COMMENT                
         BCT   R5,W1E                                                           
*                                                                               
W1F      BAS   RE,GOSPOOL                                                       
*                                                                               
W1G      DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         NI    TWAWSFLG,X'FF'-X'20'                                             
         XC    SAVEP,SAVEP                                                      
         L     R4,AWORK4           POINT TO OUTPUT                              
         USING PD,R4                                                            
         LA    R3,P                POINT TO PRINT LINE                          
         USING PWD,R3                                                           
         ZIC   R5,BUYLIN                                                        
         LR    R2,R5               SET UP ALLOWLIN                              
         CLI   SVSTAOP2,C'Y'       TRIPLE SPACING                               
         BE    W10                                                              
         SLL   R2,1                DOUBLE SPACING                               
         B     *+8                                                              
W10      MH    R2,=H'3'                                                         
         TM    TWAWSFLG,X'02'      PRINT BUYLIN HEADER ALSO?                    
         BZ    *+8                 NO                                           
         AH    R2,=H'4'            YES - ALLOW ANOTHER 4 LINES                  
         STC   R2,ALLOWLIN                                                      
         DROP  RF                                                               
W20      DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         TM    TWAWSFLG,X'02'      DO WE NEED TO PRINT BUYLINE HEADER?          
         BZ    W25                 NO - SKIP                                    
         CLC   CONACT,=C'CONF'     FOR CONFIRM, NEVER PRINT HEADER              
         BE    W25                                                              
         DROP  RF                                                               
         LA    R6,P                PUT HEADER ON P                              
         BAS   RE,BLH              GENERATE BUYLINE HEADER                      
         BAS   RE,GOSPOOL          AND PRINT IT                                 
W25      CLC   =C'WEEKLY RATE',PDAT+1                                           
         BNE   *+14                                                             
         MVC   PWDAY+2(66),PDAY+5  'WEEKLY RATE FOR PLAN XXX IS'                
         B     W500                                                             
         SPACE 1                                                                
         OC    PDAY,PDAY           IF NOTHING THERE, IT'S MORE DATES            
         BZ    W60                                                              
         SPACE 1                                                                
         CLI   PDAY,X'FF'         SPORTS BUY DESCRIPTIVE?                       
         BNE   W28                                                              
         MVC   PWDAY(L'PSDESC),PSDESC   SPORTS DESCRIPTIVE                      
         B     W500                                                             
W28      DS    0H                                                               
         OC    PDAY(5),PDAY       IF NOTHING HERE, IT'S A COMMENT               
         BNZ   W60                                                              
         CLC   =C'ROC',PDAY+6      REP ORDER COMMENT                            
         BNE   W30                                                              
         MVC   PW+3(13),=C'*REP ORD CMT*'                                       
         B     W40                                                              
W30      CLC   =C'SOC',PDAY+6      STATION ORDER COMMENT                        
         BNE   W50                                                              
         MVC   PW+3(13),=C'*STA ORD CMT*'                                       
W40      MVC   18(60,R3),PDAY+10                                                
         B     W500                                                             
         SPACE 1                                                                
*    IT'S A BUY COMMENT, BUT THEY DON'T WANT TO PRINT THE LABEL                 
W50      DS    0H                                                               
         CLC   =C'P=',PDAY+6                                                    
         BNE   W55                                                              
         MVC   18(12,R3),=C'PROGRAMMING='                                       
         MVC   30(64,R3),PDAY+8                                                 
         B     W500                                                             
*                                                                               
W55      MVC   PWDAY+6(66),PDAY+6                                               
         B     W500                                                             
         SPACE 1                                                                
W60      MVC   P,SAVEP                                                          
         OC    PCHG,PCHG                                                        
         BZ    W70                 COPY DOWN                                    
         MVC   PWCHG,PCHG          REVISION CODE                                
*                                                                               
W70      OC    PLIN,PLIN                                                        
         BZ    W80                 COPY DOWN                                    
         MVC   PWLIN,PLIN          LINE NUMBER                                  
*                                                                               
W80      DS    0H                                                               
         L     RF,AIO2             BUYREC                                       
         USING RBUYREC,RF                                                       
         TM    RBUYSFG,X'40'       SPORTS BUY?                                  
         BZ    W85                 NO                                           
         DROP  RF                                                               
         MVC   PWDAY(L'PSTYPE),PSTYPE   SPORTS TYPE                             
         B     W180                                                             
*                                                                               
W85      OC    PDAY(3),PDAY                                                     
         BZ    W120                COPY DOWN                                    
         MVC   PWDAY,SPACES                                                     
         LA    R6,DAYTAB           CONVERT SINGLE DAYS TO W FORMAT              
W90      CLC   0(3,R6),PDAY                                                     
         BE    W100                                                             
         CLI   0(R6),X'FF'                                                      
         BE    W110                NOT A SINGLE DAY                             
         LA    R6,5(R6)                                                         
         B     W90                                                              
W100     MVC   PWDAY(2),3(R6)                                                   
         B     W120                                                             
W110     MVC   PWDAY,PDAY          SO USE EXPRESSION AS GIVEN                   
         SPACE 1                                                                
*  CONVERT TO MILITARY TIME                                                     
W120     OC    PTIM,PTIM                                                        
         BZ    W170                COPY DOWN                                    
*         FIGURE OUT THE LENGTH OF THE TIME EXPRESSION FROM REGENPBY            
         LA    R6,11               11 IS THE MAXIMUM LENGTH                     
         LA    R1,PTIM+10          START AT END AND WORK BACKWARDS              
W130     CLI   0(R1),C'A'          AM                                           
         BE    W140                                                             
         CLI   0(R1),C'P'          PM                                           
         BE    W140                                                             
         CLI   0(R1),C'N'          NOON                                         
         BE    W140                                                             
         CLI   0(R1),C'M'          MIDNIGHT                                     
         BE    W140                                                             
         BCTR  R1,R0                                                            
         BCT   R6,W130                                                          
         DC    H'0'                SHOULD NEVER GET TO HERE                     
         SPACE 1                                                                
W140     GOTO1 TIMVAL,DMCB,((R6),PTIM),FULL                                     
         CLI   DMCB,X'FF'                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         EDIT  (2,FULL),(4,PWTIM)                                               
         EDIT  (2,FULL+2),(4,PWTIM+6)                                           
         SPACE 1                                                                
         LA    R1,21(R6,R4)                                                     
         CLC   0(2,R1),=C'CC'      IF END TIME IS TO CONCLUSION                 
         BNE   *+10                                                             
         MVC   PWTIM+6(2),=C'CC'   PRINT CC                                     
         SPACE 1                                                                
         LA    R1,PWTIM            SHOW 1201A-1259A AS 2401-2459                
         LA    R6,2                                                             
W150     CLC   0(2,R1),SPACES                                                   
         BNE   W160                                                             
         MVC   0(2,R1),=C'24'                                                   
         CLC   2(1,R1),SPACES                                                   
         BNE   W160                                                             
         MVI   2(R1),C'0'                                                       
W160     LA    R1,6(R1)            POINT TO END TIME                            
         CLC   0(4,R1),SPACES      IF NO END TIME                               
         BE    *+8                 DON'T BOTHER LOOPING BACK                    
         BCT   R6,W150                                                          
         SPACE 1                                                                
W170     OC    PLEN,PLEN                                                        
         BZ    W180                COPY DOWN                                    
         MVC   PWLEN,PLEN          LENGTH                                       
         SPACE 1                                                                
W180     OC    PDAT,PDAT           IF NO DATES, COPY DOWN BOTH                  
         BZ    W200                                                             
         XC    PWDAT(12),PWDAT     IF 1 OR 2 DATES, DON'T COPY EITHER           
         GOTO1 DATVAL,DMCB,(1,PDAT),WORK      DATES                             
         OC    DMCB(4),DMCB                                                     
         BZ    W190                                                             
         SPACE 1                                                                
         MVC   PWDAT(2),WORK+2                                                  
         MVI   PWDAT+2,C'/'                                                     
         MVC   PWDAT+3(2),WORK+4                                                
         SPACE 1                                                                
W190     GOTO1 DATVAL,DMCB,(1,PDAT+6),WORK                                      
         OC    DMCB(4),DMCB                                                     
         BZ    W200                                                             
         SPACE 1                                                                
         MVC   PWDAT+6(2),WORK+2                                                
         MVI   PWDAT+8,C'/'                                                     
         MVC   PWDAT+9(2),WORK+4                                                
         SPACE 1                                                                
W200     MVI   PWDAT+11,0          CLEAR IN CASE THERE IS NONE                  
         CLI   PDAT+11,C'A'                                                     
         BNE   W210                                                             
         MVI   PWDAT+11,C'A'       ALTERNATE WEEKS INDICATOR                    
         MVC   PWDAT-1(1),PDAT-1        * FOR ALTERNATE WEEKS                   
         B     W220                NEVER COPY DOWN CLASS IF ALT WEEKS           
         SPACE 1                                                                
W210     OC    PCLS,PCLS                                                        
         BZ    *+10                                                             
W220     MVC   PWCLS,PCLS          CLASS                                        
         OC    PSEC,PSEC                                                        
         BZ    *+10                                                             
         MVC   PWSEC,PSEC          SECTION                                      
         OC    PPLN,PPLN                                                        
         BZ    *+10                                                             
         MVC   PWPLN,PPLN          PLAN                                         
         OC    PNPW,PNPW                                                        
         BZ    *+10                                                             
         MVC   PWNPW,PNPW          NUMBER PER WEEK                              
         OC    PRAT,PRAT                                                        
         BZ    *+10                                                             
         MVC   PWRAT,PRAT          RATE                                         
         CLI   PDAT+11,C'A'        NEVER COPY TOT SPOTS IF ALT WEEKS            
         BE    W230                                                             
         OC    PTOT,PTOT                                                        
         BZ    *+10                                                             
W230     MVC   PWTSPOT,PTOT        TOTAL SPOTS                                  
         SPACE 1                                                                
W500     OC    P,SPACES                                                         
         MVC   SAVEP,P                                                          
         BAS   RE,GOSPOOL                                                       
         MVI   RCSUBPRG,2          FOR CONTINUATION PAGES                       
*                                  ONLY INCLUDE A FEW HEADLINES                 
         LA    R4,L'PRTLN(R4)                                                   
         BCT   R5,W20                                                           
         SPACE 1                                                                
         B     EXXMOD                                                           
         EJECT                                                                  
*              ROUTINE TO HANDLE SPOOL INTERFACE                                
         SPACE 1                                                                
GOSPOOL  NTR1                                                                   
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         MVI   SPACING,2                                                        
         CLI   SVSTAOP2,C'Y'       TRIPLE SPACING                               
         BNE   *+8                                                              
         MVI   SPACING,3                                                        
         TM    TWAWSFLG,X'20'                                                   
         BZ    GOSPL2                                                           
         MVI   SPACING,1                                                        
GOSPL2   GOTO1 SPOOL,DMCB,(R8)                                                  
         DROP  RF                                                               
         CLI   MYHEDSW,C'Y'        DID WE JUST PRINT HEADLINES                  
         BNE   GOSPLX                                                           
         LA    R4,H15                                                           
         ZIC   R3,XTRHED                                                        
GOSPL5   MVC   P,0(R4)             PRINT THE EXTRA HEADLINES                    
         MVI   SPACING,1           NO DOUBLE SPACE BTWN HEADS                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R4,132(R4)                                                       
         BCT   R3,GOSPL5                                                        
         SPACE 1                                                                
         GOTO1 SPOOL,DMCB,(R8)     PRINT BLANK LINE FOR SPACING                 
         SPACE 1                                                                
         MVC   P,SVPRNT            NOW PRINT THE LINE OF DATA                   
         MVI   SPACING,2                                                        
         CLI   SVSTAOP2,C'Y'       TRIPLE SPACING                               
         BNE   *+8                                                              
         MVI   SPACING,3                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
GOSPLX   MVI   MYHEDSW,C'N'                                                     
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PURPOSE:                                                                      
*     PRINT STORED COMMENTS IF ANY, ELSE IT WILL PRINT FREE                     
*     FORM COMMENTS                                                             
*                                                                               
* INPUT: PARAMETER 1: BYTE 1    = MODE                                          
*                     BYTE 2-4  = A(COMMENT CODE)                               
*                                                                               
* OUTPUT: NONE                                                                  
***********************************************************************         
PSTCMT   NTR1                                                                   
         L     R3,0(R1)                                                         
*                                                                               
         MVC   WORK2X(L'KEY),KEY     SAVE OFF KEY FOR RESTORE                   
         LA    R2,IOAREA                                                        
         GOTO1 VREGENSC,DMCB,(3,0(R3)),(R2),DATAMGR,RCONREC,GETTXT              
         BNZ   PSTCMTX             COMMENT NOT FOUND, PRINT NOTHING             
         CLI   0(R2),X'FF'         IF X'FF', PRINT NOTHING                      
         BE    PSTCMTX                                                          
         CLI   0(R2),0             IF NULL, PRINT FREE FORM COMMENT             
         BE    PSTCMT20                                                         
*                                                                               
PSTCMT10 ZIC   R4,0(R2)            GET LENGTH OF COMMENT                        
         BCTR  R4,0                                                             
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P+30(0),1(R2)                                                    
         BAS   RE,GOSPOOL                                                       
*                                                                               
         ZIC   R4,0(R2)            BUMP TO NEXT COMMENT ENTRY                   
         AR    R2,R4                                                            
         CLI   0(R2),X'FF'         IF X'FF', DONE                               
         BE    PSTCMTX                                                          
*                                                                               
         LR    R4,RC               BOUNDARY CHECK FOR R2                        
         A     R4,=AL4(IOAREA-GENOLD+1001)                                      
         CR    R4,R2                                                            
         BH    PSTCMT10                                                         
         B     PSTCMTX                                                          
*                                                                               
PSTCMT20 DS    0H                  PRINT FREE FORM COMMENTS                     
         MVC   P+30(60),0(R3)                                                   
         OC    P+30(60),SPACES                                                  
*                                                                               
         BAS   RE,GOSPOOL                                                       
*                                                                               
PSTCMTX  DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         TM    TWAWSFLG,X'04'           NEED TO RESTORE SEQ LOOP?               
         BZ    XIT                                                              
         DROP  RF                                                               
         MVC   KEY,WORK2X               RESTORE BYREC SEQ LOOP                  
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RBUYKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO GENERATE HEADLINES (HEADHOOK)                         
         SPACE 2                                                                
HOOK     NTR1  BASE=MYRB                                                        
         L     R9,MYR9                                                          
         CLI   FORMAT,C'W'         BIAS COPY DOWN FORMAT                        
         BE    HD10                                                             
         DC    H'0'                                                             
         SPACE 1                                                                
HD10     DS    0H                                                               
         CLI   RCSUBPRG,1                                                       
         BNE   *+8                                                              
         MVI   RCSUBPRG,2          FOR CONTINUATION PAGES                       
*                                  ONLY INCLUDE A FEW HEADLINES                 
         CLC   =C'MGS',CONACT                                                   
         BNE   HD15                                                             
         MVC   H1+38(24),=C'MAKEGOOD OFFER WORKSHEET'                           
*                                                                               
HD15     DS    0H                                                               
         MVC   H1+5(8),CONCNUM                                                  
         CLC   CONACT,=C'LAST'     FOR ACTION LAST                              
         BNE   *+14                IDENTIFY AS A DUPLICATE COPY                 
         MVC   H1+78(9),=C'DUPLICATE'                                           
         B     HD20                                                             
*                                                                               
         CLC   =C'RSND',CONACT     ACTION RESEND                                
         BNE   *+14                IDENTIFY AS A DUPLICATE COPY                 
         MVC   H1+81(6),=C'RESENT'                                              
         B     HD20                                                             
*                                                                               
         MVC   H1+79(8),=C'FROM REP'                                            
         CLI   TWAACCS,C'$'        STATION                                      
         BNE   *+10                                                             
         MVC   H1+75(12),=C'FROM STATION'                                       
HD20     GOTO1 DATCON,DMCB,(5,0),(8,H1+90)                                      
         MVC   H1+100(2),=C'AT'                                                 
         UNPK  DUB,SENDTIME        TIME                                         
         MVC   H1+103(2),DUB+1                                                  
         MVI   H1+105,C'.'                                                      
         MVC   H1+106(2),DUB+3                                                  
         SPACE 1                                                                
*                                                                               
*   CALL ROUTINE TO DISPLAY MOD & VERSION NUMBER                                
         GOTO1 VGENDMV,DMCB,RCONREC,H30+8,GENOLD                                
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         CLC   PAGE,=H'1'          FIRST PAGE?                                  
         BE    HD60                                                             
         MVC   H2(36),H30+8        ON CONTINUATION PAGES, PUT                   
*                                  VERSION & CONF. STATUS ON H2                 
         NI    SPOOLIND,X'7F'      TURN OFF 'SKIP LINE' INDICATOR               
         TM    TWAWSFLG,X'80'           SHOULD WE PRINT BUYLINE HEADER?         
         BZ    XIT                                                              
         MVI   H3,0                                                             
         LA    R6,H4                                                            
         BAS   RE,BLH                                                           
         B     XIT                                                              
         SPACE 1                                                                
HD60     MVC   H5+8(36),H30+8    ON PAGE 1, PUT VERS/CONF STATUS ON H5          
         MVI   MYHEDSW,C'Y'        MORE THAN 14 HEADLINES                       
         MVC   SVPRNT,P            SAVE PRINT LINE                              
         MVC   P,SPACES            BLANK OUT PRINT LINE                         
         MVI   XTRHED,11           MAXIMUM NUMBER OF EXTRA HEADLINES            
         OI    SPOOLIND,X'80'  DON'T SKIP LINES AFTER 1ST SET OF HEADS          
         SPACE 1                                                                
         ZIC   R0,XTRHED           CLEAR OUT EXTRA HEADLINES                    
         LA    R1,H15                                                           
HD70     MVC   0(132,R1),SPACES                                                 
         LA    R1,132(R1)                                                       
         BCT   R0,HD70                                                          
         SPACE 1                                                                
         OC    SVTRAF,SVTRAF       TRAFFIC NUMBER                               
         BZ    *+10                                                             
         MVC   H3+61(10),SVTRAF                                                 
         MVC   H3+80(5),ACTSTAT    STATION                                      
         CLI   ACTSTAT+4,C' '                                                   
         BNE   *+10                                                             
         MVC   H3+84(3),=C'-TV'                                                 
         CLI   ACTSTAT+4,C'L'                                                   
         BNE   *+10                                                             
         MVC   H3+84(3),=C'-L '                                                 
         SPACE 1                                                                
         MVC   H5+69(10),SVAGYC    AGENCY CODE                                  
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         TM    TWAFMTFL,X'08'      CARE OF AGENCY?                              
         BZ    HD074               NO                                           
*                                                                               
         LA    RE,H5+80                                                         
         MVC   0(L'RCONKADV,RE),RCONKADV                                        
         LA    RE,L'RCONKADV+1(RE)                                              
         MVC   0(4,RE),=C'C/O '                                                 
         LA    RE,4(RE)                                                         
         MVC   0(L'TWAAGNM1,RE),TWAAGNM1  AGENCY NAME FROM SCREEN               
         B     HD076                                                            
*                                                                               
HD074    DS    0H                                                               
         MVC   H5+80(L'TWAAGNM2),TWAAGNM2  AGENCY NAME FROM SCREEN              
*                                                                               
HD076    DS    0H                                                               
         DROP  RF                                                               
         SPACE 1                                                                
         TM    SVCONF,X'80'        IF UNCONFIRMED                               
         BZ    HD80                                                             
         CLI   SVVER,1             AND NOT VERSION 1                            
         BE    HD80                                                             
         TM    SVSTAT,X'04'        AND PRINT CHANGES ONLY                       
         BZ    HD80                                                             
         MVC   H7+16(12),=C'CHANGES ONLY'  THEN SAY SO IN HEADLINE              
         SPACE 1                                                                
HD80     MVC   H7+69(10),SVADVC    ADVERTISER CODE                              
         MVC   H9+80(20),CONPRD                                                 
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   H7+80(20),TWAADVNM  ADVERTISER NAME                              
         CLC   SVCONPRD,SPACES                                                  
         BE    *+10                                                             
         MVC   H9+80(20),TWAPRDNM                                               
         MVC   H11+80(20),TWASALNM                                              
         MVC   H19+89(20),TWABUYER                                              
         DROP  RF                                                               
         SPACE 1                                                                
*                                                                               
         OC    SVSASST,SVSASST                                                  
         BZ    HD90                                                             
         MVI   H11+100,C'/'                                                     
         MVC   H11+101(9),SVSASST  SALES ASSISTANT                              
HD90     MVI   H13+85,C'-'                                                      
         MVC   H13+87(20),CONOFFN                                               
         MVC   H21+94(3),=C'ARB'                                                
         CLI   SVSRC,C'A'                                                       
         BE    HD100                                                            
         MVC   H21+94(3),=C'NSI'                                                
         CLI   SVSRC,C'N'                                                       
         BE    HD100                                                            
         MVC   H21+94(3),=C'SRC'                                                
         CLI   SVSRC,C'S'                                                       
         BE    HD100                                                            
         XC    H21+94(3),H21+94    NO RATING FOUND                              
         SPACE 2                                                                
*                                                                               
* CONFLICT CODES                                                                
*                                                                               
HD100    DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'13'                                                     
         BAS   RE,GETEL                                                         
         BNE   HD103                                                            
         USING RCONCCEL,R6                                                      
         MVC   H9+69(3),RCONCCPR   PRODUCT CODE                                 
         MVC   H17+69(3),RCONCCAD  ADVERTISER CODE                              
         OC    RCONCCAR,RCONCCAR                                                
         BZ    *+10                                                             
         MVC   H17+89(1),RCONCCAR  AREA                                         
         MVC   H21+69(1),RCONCCOT  ORDER TYPE                                   
         DROP  R6                                                               
*                                                                               
* GET EOP RECORD FOR OFFCIE AND SALESPERSON                                     
*                                                                               
HD103    DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   H11+71(6),TWAEOPSL                                               
         MVC   H13+69(6),TWAEOPOF                                               
         DROP  RF                                                               
*                                                                               
*                                                                               
* DISPLAY EASI CODES (IF EXIST)                                                 
HD105    DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'A2'                                                     
         BAS   RE,GETEL                                                         
         BNE   HD110                                                            
         USING RCONIEL,R6                                                       
         MVC   H23+55(13),=C'EI CODES  ADV'                                     
         MVC   H23+75(03),=C'PRD'                                               
         MVC   H23+85(03),=C'PRD'                                               
         MVC   H23+95(03),=C'EST'                                               
         MVC   H23+69(4),RCONIADV                                               
         MVC   H23+79(4),RCONIPRD                                               
         MVC   H23+89(4),RCONIPR2                                               
         MVC   H23+99(10),RCONXEST                                              
         OC    H23+99(10),MYSPACES                                              
         CLC   H23+99(10),MYSPACES                                              
         BNE   *+10                                                             
         MVC   H23+99(4),RCONIEST                                               
         DROP  R6                                                               
HD110    DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   H20+11(34),TWAAGAD1                                              
         MVC   H21+11(36),TWAAGAD2                                              
         MVC   H22+11(36),TWAAGAD3                                              
         DROP  RF                                                               
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
*                                                                               
         LA    R1,H23+2                                                         
         OC    TWAAGYPH,TWAAGYPH                                                
         BZ    HD140                                                            
         MVC   0(7,R1),=C'PHONE #'                                              
         MVC   9(3,R1),TWAAGYPH   AGENCY PHONE NUMBER                           
         MVI   12(R1),C'-'                                                      
         MVC   13(3,R1),TWAAGYPH+3                                              
         MVI   16(R1),C'-'                                                      
         MVC   17(4,R1),TWAAGYPH+6                                              
         LA    R1,27(R1)                                                        
*                                                                               
HD140    DS    0H                                                               
         OC    TWAAFAX,TWAAFAX                                                  
         BZ    HD150                                                            
         MVC   0(5,R1),=C'FAX #'                                                
         MVC   6(3,R1),TWAAFAX    AGENCY FAX NUMBER                             
         MVI   9(R1),C'-'                                                       
         MVC   10(3,R1),TWAAFAX+3                                               
         MVI   13(R1),C'-'                                                      
         MVC   14(4,R1),TWAAFAX+6                                               
*                                                                               
HD150    DS    0H                                                               
         MVC   H25+55(18),=C'SALESPERSON PHONE#'                                
         MVC   H25+74(L'TWASALTL),TWASALTL                                      
         OC    TWASALFX,TWASALFX   SALESPERSON FAX NUMBER                       
         BZ    HD160                                                            
         MVC   H26+55(16),=C'SALESPERSON FAX#'                                  
         MVC   H26+74(L'TWASALFX),TWASALFX                                      
         B     HD170                                                            
*                                                                               
HD160    DS    0H                  -OR-                                         
         OC    TWAOFFFX,TWAOFFFX   OFFICE FAX NUMBER                            
         BZ    HD170                                                            
         MVC   H25+55(12),=C'OFFICE FAX# '                                      
         MVC   H25+67(3),TWAOFFFX                                               
         MVI   H25+70,C'-'                                                      
         MVC   H25+71(3),TWAOFFFX+3                                             
         MVI   H25+74,C'-'                                                      
         MVC   H25+75(4),TWAOFFFX+6                                             
         DROP  RF                                                               
*                                                                               
HD170    DS    0H                                                               
         GOTO1 DATCON,DMCB,(3,SVCONDTE),(5,H24+13)                              
         GOTO1 DATCON,DMCB,(3,SVCONDTE+3),(5,H24+34)                            
         EDIT  (1,SVWKS),(2,H24+49),ALIGN=LEFT                                  
         SPACE 1                                                                
* DEAL WITH HEADLINES THAT DON'T FIT IN HEDSPECS                                
         SPACE 1                                                                
         MVC   H15+2(51),DASH                                                   
         MVC   H17+2(14),=C'PARTIAL LINE/S '                                    
         MVC   H17+17(36),DASH                                                  
         MVC   H19+2(51),DASH                                                   
         MVC   H20+2(8),=C'AGENCY - '                                           
         MVC   H21+2(7),=C'ADDRESS'                                             
*        MVC   H22+2(7),=C'PHONE #'                                             
*        MVC   H23+2(7),=C'FAX   #'                                             
         MVC   H24+2(11),=C'START DATE-'                                        
         MVC   H24+25(9),=C'END DATE-'                                          
         MVC   H24+46(3),=C'WK-'                                                
         SPACE 1                                                                
         MVC   H15+55(10),=C'8/BILL WK.'                                        
         MVC   H16+55(23),DASH                                                  
         MVC   H17+55(11),=C'33/CFLCT CD'                                       
         MVC   H18+55(23),DASH                                                  
         MVC   H19+55(12),=C'11/RATE CARD'                                      
         MVC   H20+55(23),DASH                                                  
         MVC   H21+55(13),=C'14/ORDER TYPE'                                     
         MVC   H22+55(23),DASH                                                  
         MVC   H24+55(48),DASH                                                  
         SPACE 1                                                                
         MVC   H15+80(12),=C'10/BILL CODE'                                      
         MVC   H16+80(30),DASH                                                  
         MVC   H17+80(10),=C'9/AREA - 1'                                        
         MVC   H18+80(30),DASH                                                  
         MVC   H19+80(9),=C'16/BYR - '                                          
         MVC   H20+80(30),DASH                                                  
         MVC   H21+80(13),=C'17/RTG SVC - '                                     
         MVC   H22+80(30),DASH                                                  
         B     XIT                                                              
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
BLH      NTR1                                                                   
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         NI    TWAWSFLG,X'FF'-X'02'  TURN OFF FLAG FOR FIRST TIME HEADR         
         DROP  RF                                                               
         MVC   68(4,R6),=C'PLAN'                                                
         MVC   84(4,R6),=C'RATE'                                                
         MVC   101(3,R6),=C'STS'                                                
         LA    R6,132(R6)                                                       
         MVC   0(34,R6),=C'BIAS REP                 BEGIN END'                  
         MVC   41(39,R6),=C'BEGIN  END   CLA  SEC      PRICE SPOTS/'            
         MVC   84(26,R6),=C'SPOT    CONFLICT CD. TOTAL'                         
         LA    R6,132(R6)                                                       
         MVC   0(35,R6),=C'LN#  LN# CD DAYS         TIME  TIME'                 
         MVC   36(36,R6),=C'LEN  DATE   DATE  13/  14/ PLAN  16/'               
         MVC   74(36,R6),=C'WEEK      18/     PROD ADV 23/ SPOTS'               
         LA    R6,132(R6)                                                       
         MVC   0(35,R6),=C'---- --- -- -----------  ----------'                 
         MVC   36(37,R6),=C'---  -----------  ---  --- ---- -----'              
         MVC   74(36,R6),=C'----- ----------- -------- --- -----'               
BLHX     B     XIT                                                              
*        CONSTANTS, LITERAL POOL, ETC.                                          
*                                                                               
SVKEY    DS    CL27                                                             
DASH     DC    51C'-'                                                           
         SPACE 1                                                                
DAYTAB   DS    0CL5                                                             
         DC    CL3'MON',CL2'M '                                                 
         DC    CL3'TUE',CL2'T '                                                 
         DC    CL3'WED',CL2'W '                                                 
         DC    CL3'THU',CL2'TH'                                                 
         DC    CL3'FRI',CL2'F '                                                 
         DC    CL3'SAT',CL2'SA'                                                 
         DC    CL3'SUN',CL2'SU'                                                 
         DC    X'FF'                                                            
         SPACE 1                                                                
SAVEP    DS    CL132                                                            
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*          HEADLINE SPECS FOR CONTRACTS                                         
         SPACE 2                                                                
HEDSPECS DS    0D                                                               
         SPACE 1                                                                
         SPROG 1          ***BIAS COPY DOWN - PAGE 1 ONLY***                    
         PSPEC H3,1,C'NEW'                                                      
         PSPEC H5,1,C'ADD'                                                      
         PSPEC H7,1,C'DELETE'                                                   
         PSPEC H9,3,C'ORDER '                                                   
         PSPEC H9,9,C'---------------------------------------------'            
         PSPEC H11,3,C'------------------------------------------------X        
               ---'                                                             
         PSPEC H13,3,C'LINE/S '                                                 
         PSPEC H13,10,C'--------------------------------------------'           
         SPACE 1                                                                
         PSPEC H3,56,C'BIAS#'                                                   
         PSPEC H4,56,C'-----------------------'                                 
         PSPEC H5,56,C'1/AGENCY#'                                               
         PSPEC H6,56,C'------------------------'                                
         PSPEC H7,56,C'2/ADVERTISER#'                                           
         PSPEC H8,56,C'------------------------'                                
         PSPEC H9,56,C'3/PRODUCT#'                                              
         PSPEC H10,56,C'-----------------------'                                
         PSPEC H11,56,C'10/EOS   4/SAL#'                                        
         PSPEC H12,56,C'-----------------------'                                
         PSPEC H13,56,C'5/REP#'                                                 
         PSPEC H14,56,C'-----------------------'                                
         PSPEC H3,81,C'DATE'                                                    
         PSPEC H4,81,C'------------------------------'                          
         PSPEC H6,81,C'------------------------------'                          
         PSPEC H8,81,C'------------------------------'                          
         PSPEC H10,81,C'------------------------------'                         
         PSPEC H12,81,C'------------------------------'                         
         PSPEC H14,81,C'------------------------------'                         
         SPACE 1                                                                
         SPROG 1,2,3      ***BIAS COPY DOWN - ALL PAGES***                      
         PSPEC H1,1,C'ACE #'                                                    
         PSPEC H1,43,C'ORDER WORKSHEET'                                         
         PSPEC H2,103,PAGE                                                      
         DC    X'00'                                                            
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
       ++INCLUDE RECNTFMTD                                                      
       ++INCLUDE REGENPBYD                                                      
*    FORMAT FOR BIAS COPY DOWN WORKSHEET BUYLINES                               
         SPACE 1                                                                
PWD      DSECT                                                                  
PW       DS    0CL110                                                           
         DS    CL5                                                              
PWLIN    DS    CL3                 DDS LINE NUMBER                              
         DS    CL1                                                              
PWCHG    DS    CL2                 CHANGE CODE                                  
         DS    CL1                                                              
PWDAY    DS    CL12                DAY                                          
         DS    CL1                                                              
PWTIM    DS    CL10                TIME                                         
         DS    CL1                                                              
PWLEN    DS    CL3                 LENGTH                                       
         DS    CL2                                                              
PWDAT    DS    CL12                DATE                                         
         DS    CL1                                                              
PWCLS    DS    CL3                 CLASS                                        
         DS    CL2                                                              
PWSEC    DS    CL3                 SECTION                                      
         DS    CL2                                                              
PWPLN    DS    CL3                 PLAN                                         
         DS    CL1                                                              
PWPRAT   DS    CL5                 PLAN PRICE                                   
         DS    CL2                                                              
PWNPW    DS    CL3                 NUMBER PER WEEK                              
         DS    CL2                                                              
PWRAT    DS    CL10                RATE                                         
         DS    CL16                                                             
PWTSPOT  DS    CL3                 TOTAL SPOTS                                  
         DS    CL1                                                              
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020RECNT6D   11/14/03'                                      
         END                                                                    
