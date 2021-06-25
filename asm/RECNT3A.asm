*          DATA SET RECNT3A    AT LEVEL 005 AS OF 10/08/15                      
*PHASE T8023AA                                                                  
*&&      SET   LR=Y                                                             
*INCLUDE OUTDAY                                                                 
         TITLE 'T8023A - MAKEGOOD OFFER EDIT'                                   
*                                                                               
*******************************************************************             
*                                                                 *             
*     RECNT3A (T8023A) --- MAKEGOOD OFFER EDIT                    *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* 08OCT15 KWA BYAPSS AM VALIDATION                                              
* 11DEC13 SKU ADDITIONAL FIX TO TAKEOVER OFFERS TO START AT DA    *             
* 27AUG13 SKU SAVE OFF FIRST AIR DATE FOR RIS OFFER LISTING       *             
* 04JAN13 SKU DISALLOW RNA FOR KATZ TV (AM/CQ/SZ/TV)              *             
* 28Sep12 KWA Remove X66 Buy elem in MGP66ELM and MGM66ELM logic  *             
* 13Sep12 KWA Set flag to process MQ message when MGO is added    *             
* 20Aug12 KWA Update CON rec for makegood offers count (add/del)  *             
* 18NOV11 SKU PUT BACK X'66' RESERVE SPOT ELEMENT BUT ONLY NEED   *             
*             THE ONES WITH SPOTS MISSED                          *             
* 15MAR11 SMY DELETE ALL X'66' MAKEGOOD ELEMENTS FROM BUY BEFORE  *             
*             PUTREC - SEE DUMP10 IN DUMPTSAR                     *             
* 14SEP09 SKU START TAKEOVER CONTRACTS AT OFFER CODE DA INSTEAD   *             
* 07MAY09 SKU START TAKEOVER CONTRACTS AT OFFER CODE BA           *             
* 21FEB07 HQ  FIX LENGTH DISPLAY WHEN IT IS SET TO MINUTE         *             
* 27OCT06 SKU ACCEPT M/D/Y INPUT FROM USER TO ACCOMODATE          *             
*             CONTRACT THAT SPAN ACROSS A WHOLE BROADCAST CALENDAR*             
*             YEAR                                                *             
* 13JUN06 SKU ALLOW ORBIT OFFERS                                  *             
* 05JAN04 HQ  FIX CONDITION CODE                                  *             
* 14NOV03 SKU ALLOW RNA FOR EAGLE TV                              *             
*             DISALLOW MAKEGOOD TO TARGET CANCELLED BUYS          *             
* 22OCT03 HQ  DEMO CAT FIX                                                      
* 10OCT03 MLN CHANGE LEN OF PGM/CMT FIELDS ON SCREEN AND RECORDS  *             
* 29SEP03 HQ  ERROR ON BLANK DAY/TIME FIELD                       *             
* 19MAY03 SKU SELF APPLY                                          *             
* 07JUL02 HQ  DEMO CAT AND VALUE DISP,INPUT ON MKG SCREEN         *             
* 20FEB03 HQ  EXTRA X'66' ELEMENT CREATED IN MGM BUG FIX          *             
*         HQ  MGX BUG FIX                                         *             
* 12AUG02 SKU MAKEGOOD LINE COUNTER BUG FIX                       *             
*             DISALLOW RNA FOR SZ, CQ AND AM                      *             
* 18JUL02 SKU CANNOT MISS 0 SPOTS FOR MGM                         *             
* 07FEB02 SKU REMOVE 5 MINUTE RULE                                *             
* 08JAN02 SKU SUPPORT REPLACEMENT OFFER AND LINE CORRECTION       *             
* 04OCT01 SKU FIX MISSING X'05' ELEMENT                           *             
* 27AUG01 SKU FIX LATE RUN DAY GENERATION (HAS BEEN COMMENTED OUT *             
*             TEMPORARILY)                                        *             
*             FIX DELETE BUG                                      *             
* 21FEB01 SKU SPORTS BUY LOCKOUT FOR PREEMPT AND LATE RUNS        *             
* 02FEB01 RHV SPORTS BUY LOCKOUT                                  *             
*         SKU SKIP SENDING MKGCAN TO AGENCY IF DARE INFO NOT FOUND*             
* 18JAN01 HWO IGNORE AGENCY OFFICE WHEN RETRIEVING X'51' DARE RECORD            
* 30AUG00 SKU FIX BONUS DATE/DAY VALIDATION IN LATE RUN W/BONUS   *             
* 04AUG00 SKU ALLOW MGX OF OFFERS WITH MISSING TARGET BUY         *             
* 02JUL99 SKU INITIAL ENTRY                                       *             
*******************************************************************             
T8023A   CSECT                                                                  
         PRINT GEN                                                              
         NMOD1 MYWORKX-MYWORKD,T8023A,R9,CLEAR=YES,RR=R3                        
         LR    R8,RC                                                            
         USING MYWORKD,R8          LOCAL WORK AREA                              
         L     RC,0(R1)            WORK                                         
         L     R1,4(R1)            DISP/EDIT                                    
         MVC   ACTION,0(R1)                                                     
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
TD       USING TSARD,TSARBLK                                                    
*                                                                               
         ST    R3,RELO                                                          
         OI    CONCACTH+1,X'01'    SET MODIFIED ALWAYS                          
*                                                                               
         BAS   RE,CKPFKEY                                                       
         BZ    MAIN10                                                           
         LA    R2,CONCACTH                                                      
         LA    R3,227              INVALID PFKEY                                
         B     ERROR                                                            
*                                                                               
MAIN10   DS    0H                                                               
         CLI   PFKEY,2                                                          
         BNE   MAIN13              APPLY GROUP?                                 
*                                                                               
         LR    R4,RA                                                            
         AHI   R4,TWAWORKQ                                                      
         USING TWAWORK,R4                                                       
         LA    R2,CONCACTH                                                      
         LA    R3,227              INVALID PFKEY                                
         OC    TWAMKGD2,TWAMKGD2                                                
         BZ    ERROR                                                            
         OC    TWAMKGDH,TWAMKGDH                                                
         BZ    ERROR                                                            
         MVC   TWAMKGD2,TWAMKGDH   FOR SOME REASON, RECNT37 EXPECTS             
*        DROP  R4                  THE HEADER D/A TO BE IN TWAMKGD2             
*                                                                               
         XC    MYWRK,MYWRK                                                      
         MVI   MYWRK+8,C'A'        FAKE APPLY FIELD                             
         LA    R2,MYWRK                                                         
         GOTO1 VLOAD,DMCB,(X'37',0),(R2)                                        
*                                                                               
         MVC   MGHSTAT(7),=C'APPLIED'                                           
         XC    MGHRSWP,MGHRSWP                                                  
         GOTO1 DATCON,DMCB,(3,TODAY),(5,MGHUPDT)                                
         OI    MGHSTATH+6,X'80'    XMIT                                         
         OI    MGHRSWPH+6,X'80'                                                 
         OI    MGHUPDTH+6,X'80'                                                 
*                                                                               
         OI    TWACONFG,TW_MGOPQ   Makegood offer is applied                    
         OI    TWACONFG,TW_MGOMQ   Makegood offer is updated, do MQ msg         
         GOTO1 =A(UPDTCON),RR=RELO                                              
*                                                                               
         LA    R3,132              GROUP APPLIED                                
         LA    R2,CONCACTH                                                      
         B     INFEXIT                                                          
*                                                                               
MAIN13   DS    0H                                                               
         CLC   =C'MGO',CONACT      USER INVOKED MAKEGOOD MENU                   
         BNE   MAIN15                                                           
         GOTO1 =A(MENU),RR=RELO                                                 
         B     MAINX                                                            
*                                                                               
MAIN15   DS    0H                                                               
         BAS   RE,WHATAMI          DETERMINE OFFER TYPE                         
*                                                                               
         CLC   =C'DISP',ACTION                                                  
         BE    MAIN50                                                           
         CLC   =C'EDIT',ACTION                                                  
         BE    *+6                                                              
         DC    H'0'                SHOULDN'T BE HERE                            
*                                                                               
         TM    FLAGS,FACTADDQ                                                   
         BO    MAIN20                                                           
         CLC   =C'APPLIED',MGHSTAT DON'T VALIDATE APPLIED OFFER                 
         BE    MAINX                                                            
*                                                                               
MAIN20   DS    0H                                                               
         GOTO1 =A(DITTO),RR=RELO                                                
*                                                                               
         GOTO1 =A(VALIMKG),RR=RELO                                              
*                                                                               
         CLC   =C'MGX',CONACT                                                   
         BNE   MAIN30                                                           
*                                                                               
         LA    R3,123              MAKEGOOD DELETED                             
         OI    CONCACTH+6,X'40'+X'80'                                           
         B     INFEXIT                                                          
*                                                                               
MAIN30   DS    0H                                                               
         BAS   RE,PAGING                                                        
*                                                                               
         BAS   RE,DMKGHDR          RE-DISPLAY OFFER HEADER INFO                 
*                                                                               
         BAS   RE,DMKGOFF          RE-DISPLAY MISSED/OFFER LINES                
*                                                                               
         CLC   =C'MGC',CONACT                                                   
         BNE   MAIN40                                                           
*                                                                               
         OI    CONCACTH+6,X'40'+X'80'                                           
*                                                                               
         LA    R3,181              MAKEGOOD CHANGED                             
         CLI   FMGTYPE,FMGPOFFQ                                                 
         BNE   INFSUBX                                                          
         LA    R3,188              PREEMPT/CREDIT CHANGED                       
         B     INFEXIT                                                          
*                                                                               
MAIN40   DS    0H                                                               
         MVC   CONCACT(3),=C'MGC'                                               
         OI    CONCACTH+6,X'40'+X'80'                                           
*                                                                               
         LA    R3,182              MAKEGOOD ADDED                               
         CLI   FMGTYPE,FMGPOFFQ                                                 
         BNE   INFSUBX                                                          
         LA    R3,189              PREEMPT/CREDIT ADDED                         
         B     INFEXIT                                                          
*                                                                               
MAIN50   DS    0H                                                               
         BAS   RE,LOADSCRN                                                      
*                                                                               
MAIN55   DS    0H                                                               
         GOTOR CKSTPROF                                                         
         BNE   MAIN57                                                           
         GOTOR DISDEMO                                                          
*                                                                               
MAIN57   DS    0H                                                               
         BAS   RE,DISPCON                                                       
*                                                                               
         BAS   RE,PAGING                                                        
*                                                                               
         CLC   =C'MGL',CONACT                                                   
         BE    MAIN70                                                           
*                                                                               
MAIN60   DS    0H                                                               
         MVC   CONCACT(3),CONACT                                                
         OI    CONCACTH+6,X'80'    XMIT                                         
         OI    MGHMOPTH+6,X'40'                                                 
         CLI   FMGTYPE,FMGMOFFQ                                                 
         BE    MAINX                                                            
         NI    MGHMOPTH+6,X'FF'-X'40'                                           
         OI    MGHGCMTH+6,X'40'                                                 
         B     MAINX                                                            
*                                                                               
MAIN70   DS    0H                                                               
         BAS   RE,DMKGHDR          DISPLAY OFFER HEADER INFO                    
*                                                                               
         BAS   RE,DMKGOFF          DISPLAY MISSED/OFFER LINES                   
*                                                                               
         MVC   CONCACT(3),=C'MGC'                                               
         MVC   CONACT,=C'MGC '                                                  
         MVC   LASTCACT,CONACT                                                  
         OI    CONCACTH+6,X'40'+X'80'                                           
*        OI    CONCNUMH+4,X'20'    INVOKE HOTKEY IN BASE                        
*                                                                               
         LA    R3,180              MAKEGOOD OFFER DISPLAYED...                  
         CLI   FMGTYPE,FMGPOFFQ                                                 
         BNE   INFSUBX                                                          
         LA    R3,187              PREEMPT/CREDIT DISPLAYED                     
         B     INFEXIT                                                          
*                                                                               
MAINX    DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
* OFFER TYPE AWARE MESSAGE                                                      
*                                                                               
INFSUBX  DS    0H                                                               
         LA    R1,OFFRTYPE+1                                                    
         ST    R1,DMCB+12                                                       
         MVC   DMCB+12(1),OFFRTYPE                                              
*                                                                               
         GOTO1 GETTXT,DMCB,(R3),0,(C'I',0),,0,0                                 
         B     EXXMOD                                                           
INFEXIT  DS    0H                                                               
         GOTO1 GETTXT,DMCB,(R3),0,(C'I',0),0,0,0                                
         B     EXXMOD                                                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* DISPLAY MAKEGOOD MENU AND ALLOW USER TO PICK OFFER TYPE TO ADD                
*                                                                               
MENU     NTR1  BASE=*,LABEL=*                                                   
         LR    R4,RA                                                            
         AHI   R4,TWAWORKQ                                                      
         USING TWAWORK,R4                                                       
*                                                                               
         CLI   TWASCRN,X'C6'       SCREEN ALREADY LOADED?                       
         BE    MENU10                                                           
*                                                                               
         LA    R3,CONLAST          A(LOAD POINT FOR MENU SCREEN)                
         MVC   DMCB+4(3),=X'D90802' PROGRAM NUMBER                              
         MVI   DMCB+7,X'C6'        SCREEN NUMBER                                
*                                                                               
         GOTO1 CALLOV,DMCB,(R3)    LOAD SCREEN # C6 (MENU SCREEN)               
         CLI   DMCB+4,X'FF'        SCREEN FOUND?                                
         BNE   *+6                 YES                                          
         DC    H'0'                NO  -  UMP IT OUT                            
         MVI   TWASCRN,X'C6'       LOAD NEW SCREEN NUMBER                       
*                                                                               
         CLC   =C'SZ',REPALPHA                                                  
         BE    MENU5                                                            
         CLC   =C'AM',REPALPHA                                                  
         BE    MENU5                                                            
         CLC   =C'CQ',REPALPHA                                                  
         BE    MENU5                                                            
         CLC   =C'TV',REPALPHA                                                  
         BNE   MENU8                                                            
*                                                                               
MENU5    DS    0H                                                               
         LA    R2,MENSEL7H                                                      
         OI    1(R2),X'20'         NOT ALLOWED FOR SZ, AM, CQ AND TV            
         OI    6(R2),X'80'                                                      
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         OI    1(R2),X'0C'                                                      
         OI    6(R2),X'80'                                                      
*                                                                               
MENU8    DS    0H                                                               
         MVC   MENLAST+1(2),=X'0101' TRANSMIT SCREEN                            
*                                                                               
         LA    R3,858                                                           
         LA    R2,MENSEL1H                                                      
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONXEL,R6                                                       
         TM    RCONCONF,X'40'+X'20'                                             
         BNZ   ERROR                                                            
         OI    MENBUYH+1,X'20'     NOT ALLOWED FOR UNCONFIRMED                  
         OI    MENSEL1H+1,X'20'    NOT ALLOWED FOR UNCONFIRMED                  
         OI    MENSEL2H+1,X'20'    NOT ALLOWED FOR UNCONFIRMED                  
         OI    MENSEL3H+1,X'20'    NOT ALLOWED FOR UNCONFIRMED                  
         OI    MENSEL4H+1,X'20'    NOT ALLOWED FOR UNCONFIRMED                  
         OI    MENSEL5H+1,X'20'    NOT ALLOWED FOR UNCONFIRMED                  
         OI    MENSEL6H+1,X'20'    NOT ALLOWED FOR UNCONFIRMED                  
*        LA    R2,MENSEL7H                                                      
         LA    R3,479              TEMPORARY UNTIL RNA IS RELEASED              
         LA    R2,CONBACTH                                                      
         B     ERROR                                                            
*                                                                               
* CHECK MENU SELECTION                                                          
*                                                                               
MENU10   DS    0H                                                               
         CLC   =C'SZ',REPALPHA                                                  
         BE    MENU15                                                           
         CLC   =C'AM',REPALPHA                                                  
         BE    MENU15                                                           
         CLC   =C'CQ',REPALPHA                                                  
         BE    MENU15                                                           
         CLC   =C'TV',REPALPHA                                                  
         BNE   MENU18                                                           
*                                                                               
MENU15   DS    0H                                                               
         LA    R2,MENSEL7H                                                      
         OI    1(R2),X'20'         NOT ALLOWED FOR SZ, AM ,CQ AND TV            
         OI    6(R2),X'80'                                                      
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         OI    1(R2),X'0C'                                                      
         OI    6(R2),X'80'                                                      
*                                                                               
MENU18   DS    0H                                                               
         LA    R2,MENBUYH          SINGLE TARGET BONUS DESIRED?                 
         GOTO1 VPACK                                                            
         LTR   R0,R0                                                            
         BZ    MENU20                                                           
         LA    R3,90               BUY NOT FOUND                                
         CHI   R0,255                                                           
         BH    ERROR                                                            
*                                                                               
         XC    CONBNUM,CONBNUM     SET TARGET NUMBER                            
         ZIC   R1,MENBUYH+5                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CONBNUM(0),MENBUY                                                
         MVC   CONBNUMH+5(1),MENBUYH+5                                          
         NI    CONBNUMH+4,X'FF'-X'20' MODIFIED                                  
         OI    CONBNUMH+4,X'08'    SET NUMERIC                                  
         OI    CONBNUMH+6,X'80'    XMIT                                         
         B     MENUX                                                            
*                                                                               
MENU20   DS    0H                                                               
         CLI   MENSEL1H+5,0                                                     
         BE    MENU30                                                           
         LA    R2,MENBUYH                                                       
         LA    R3,857              ENTER TARGET BUY NUMBER                      
         B     ERROR                                                            
*                                                                               
MENU30   DS    0H                                                               
         LA    R2,MENSEL2H                                                      
*                                                                               
MENU40   CLI   5(R2),0                                                          
         BNE   MENU50                                                           
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         LA    R1,MENSEL7H                                                      
         CR    R2,R1                                                            
         BNH   MENU40                                                           
         LA    R2,MENSEL1H                                                      
         LA    R3,858                                                           
         B     ERROR                                                            
*                                                                               
MENU50   DS    0H                                                               
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         XC    CONBNUM,CONBNUM                                                  
         XC    CONCACT,CONCACT                                                  
         MVC   CONCACT(3),8(R2)                                                 
         MVI   CONCACTH+5,3                                                     
         NI    CONCACTH+4,X'FF'-X'20' MODIFIED                                  
         OI    CONCACTH+6,X'80'    XMIT                                         
*                                                                               
MENUX    DS    0H                                                               
         B     EXXMOD                                                           
         DROP  R4                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* DETERMINE WHAT TYPE OFFER THIS IS AND WHAT ACTION IS INVOKED                  
*                                                                               
WHATAMI  NTR1  BASE=*,LABEL=*                                                   
         LR    R4,RA                                                            
         AHI   R4,TWAWORKQ                                                      
         USING TWAWORK,R4                                                       
*                                                                               
         XCEFL MGDTREC,1000                                                     
*                                                                               
         CLC   =C'MGL',CONACT                                                   
         BE    *+14                                                             
         CLC   =C'MGC',CONACT                                                   
         BNE   WHAT010                                                          
         OI    FLAGS,FACTCHAQ      ACTION IS MAKEGOOD CHANGE                    
         B     WHAT050                                                          
*                                                                               
WHAT010  DS    0H                                                               
         CLC   =C'MGX',CONACT                                                   
         BNE   WHAT020                                                          
         OI    FLAGS,FACTMGXQ      ACTION IS MAKEGOOD DELETE                    
         B     WHAT050                                                          
*                                                                               
WHAT020  DS    0H                                                               
         OI    FLAGS,FACTADDQ      ACTION IS MAKEGOOD ADD                       
         XC    TWAMKGDH,TWAMKGDH   CLEAR D/A AND START FRESH                    
         XC    TWAMKGD2,TWAMKGD2                                                
         B     WHAT100                                                          
*                                                                               
WHAT050  DS    0H                                                               
         OC    TWAMKGD2,TWAMKGD2   ADDING?                                      
         BZ    WHAT100                                                          
*                                                                               
         CLI   TWASCRN,X'CC'       BUG FIX, A STRICTER CHECK HERE               
         BE    WHAT070             TO AVOID DUMPS                               
         CLI   TWASCRN,X'C0'                                                    
         BL    WHAT105             MKG SCREENS ARE C0,C1,C2,C3,C4,C5,C6         
         CLI   TWASCRN,X'C5'                       CC                           
         BH    WHAT105                                                          
*                                                                               
WHAT070  DS    0H                                                               
         XC    KEY,KEY             RECORD SELECTED: LOAD D/A                    
         MVC   KEY+28(4),TWAMKGD2                                               
         GOTO1 VGETREC,DMCB,RMKGREC                                             
*                                  SAVE IT OFF FOR LATER REFERENCE              
         GOTO1 VMOVEREC,DMCB,RMKGREC,MGDTREC                                    
*                                                                               
         MVC   FMGTYPE,RMKGRTS     DON'T CARE ABOUT THE OTHER BITS              
         NI    FMGTYPE,FMGPOFFQ+FMGBOFFQ+FMLROFFQ+FMLBOFFQ+FRNAOFFQ             
         TM    FLAGS,FACTADDQ                                                   
         BZ    WHAT110             USER WANTS TO ADD NEW OFFER TO THE           
*                                  GROUP ON SCREEN                              
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(27),RMKGREC     INSERT KEY OF SELECTED REC                   
         XC    KEY+21(6),KEY+21    CLEAR LOW ORDER OF KEY                       
         GOTO1 VHIGH               READ KEY                                     
         CLC   KEY(27),KEYSAVE     GROUP COMMENT RECORD FOUND?                  
         BE    *+6                 NO                                           
         DC    H'0'                                                             
         GOTO1 VGETREC,DMCB,RMKGREC                                             
         TM    RMKGSCST,RMKGSAPQ   APPLIED?                                     
         BO    WHAT100                                                          
*                                                                               
WHAT100  DS    0H                                                               
         MVI   FMGTYPE,FMGBOFFQ    BONUS                                        
         CLC   =C'MGB',CONACT                                                   
         BE    WHAT110                                                          
         MVI   FMGTYPE,FMGPOFFQ    PREEMPT/CREDIT                               
         CLC   =C'MGP',CONACT                                                   
         BE    WHAT110                                                          
         MVI   FMGTYPE,FMGMOFFQ    REGULAR MAKEGOOD OFFER                       
         CLC   =C'MGM',CONACT                                                   
         BE    WHAT110                                                          
         MVI   FMGTYPE,FMLROFFQ    LATE RUN                                     
         CLC   =C'MLR',CONACT                                                   
         BE    WHAT110                                                          
         MVI   FMGTYPE,FMLBOFFQ    LATE RUN WITH BONUS                          
         CLC   =C'MLB',CONACT                                                   
         BE    WHAT110                                                          
         MVI   FMGTYPE,FRNAOFFQ    REPLACEMENT NA                               
         CLC   =C'RNA',CONACT                                                   
         BE    WHAT110                                                          
WHAT105  DS    0H                                                               
         LA    R3,12               INVALID ACTION                               
         LA    R2,CONCACTH                                                      
         B     ERROR                                                            
*                                                                               
WHAT110  DS    0H                                                               
         XC    OFFRTYPE,OFFRTYPE                                                
         MVI   OFFRTYPE,5                                                       
         MVC   OFFRTYPE+1(5),=C'Bonus'                                          
         CLI   FMGTYPE,FMGBOFFQ    BONUS                                        
         BE    WHATX                                                            
         MVI   OFFRTYPE,7                                                       
         MVC   OFFRTYPE+1(7),=C'Preempt'                                        
         CLI   FMGTYPE,FMGPOFFQ    PREEMPT/CREDIT                               
         BE    WHATX                                                            
         MVI   OFFRTYPE,8                                                       
         MVC   OFFRTYPE+1(8),=C'Makegood'                                       
         CLI   FMGTYPE,FMGMOFFQ    REGULAR MAKEGOOD OFFER                       
         BE    WHATX                                                            
         MVI   OFFRTYPE,8                                                       
         MVC   OFFRTYPE+1(8),=C'Late run'                                       
         CLI   FMGTYPE,FMLROFFQ    LATE RUN                                     
         BE    WHATX                                                            
         MVI   OFFRTYPE,11                                                      
         MVC   OFFRTYPE+1(11),=C'Replacement'                                   
         CLI   FMGTYPE,FRNAOFFQ    REPLACEMENT OFFER                            
         BE    WHATX                                                            
         MVI   OFFRTYPE,19                                                      
         MVC   OFFRTYPE+1(19),=C'Late run with bonus'                           
         CLI   FMGTYPE,FMLBOFFQ    LATE RUN WITH BONUS                          
         BE    WHATX                                                            
         DC    H'0'                                                             
*                                                                               
WHATX    DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* LOAD APPROPRIATE SUB-SCREEN (IE: MG, PREEMPT, BONUS, LATE-RUNS, ETC)          
* IF NOT ALREADY LOADED                                                         
*                                                                               
LOADSCRN NTR1  BASE=*,LABEL=*                                                   
         MVI   SUBSCRN,X'C3'       BONUS                                        
         MVC   MGHGCMT(11),=C'BONUS OFFER'                                      
         LA    R2,MGBPFLNH                                                      
*        LA    R3,MGBLAST                                                       
         CLI   FMGTYPE,FMGBOFFQ                                                 
         BE    LDSC10                                                           
         MVI   SUBSCRN,X'C2'       PREEMPT/CREDIT                               
         MVC   MGHGCMT(14),=C'PREEMPT/CREDIT'                                   
         LA    R2,MGPPFLNH                                                      
*        LA    R3,MGPLAST                                                       
         CLI   FMGTYPE,FMGPOFFQ                                                 
         BE    LDSC10                                                           
         MVI   SUBSCRN,X'C4'       LATE RUN                                     
         MVC   MGHGCMT(14),=C'LATE RUN OFFER'                                   
         LA    R2,MLRPFLNH                                                      
*        LA    R3,MLRLAST                                                       
         CLI   FMGTYPE,FMLROFFQ                                                 
         BE    LDSC10                                                           
         MVI   SUBSCRN,X'C5'       LATE RUN WITH BONUS                          
         MVC   MGHGCMT(25),=C'LATE RUN WITH BONUS OFFER'                        
         LA    R2,MLBPFLNH                                                      
*        LA    R3,MLBLAST                                                       
         CLI   FMGTYPE,FMLBOFFQ                                                 
         BE    LDSC10                                                           
         MVI   SUBSCRN,X'CC'       REPLACEMENT OFFER                            
         MVC   MGHGCMT(30),=C'REPLACEMENT OFFER FOR NA SPOTS'                   
         LA    R2,RNAPFLNH                                                      
*        LA    R3,RNALAST                                                       
         CLI   FMGTYPE,FRNAOFFQ                                                 
         BE    LDSC10                                                           
         MVI   SUBSCRN,X'C1'       DEFAULT GENERIC MAKEGOOD OFFER               
         MVC   MGHGCMT(30),=C'MANY-FOR-MANY MAKEGOOD OFFER  '                   
         LA    R2,MGSPFLNH                                                      
*        LA    R3,MGSLAST                                                       
*                                                                               
LDSC10   DS    0H                                                               
         TM    FLAGS,FACTADDQ      AUTO COMMENT ONLY FOR ADDING                 
         BNZ   *+10                                                             
         XC    MGHGCMT,MGHGCMT                                                  
*                                                                               
         LR    R4,RA                                                            
         AHI   R4,TWAWORKQ                                                      
         USING TWAWORK,R4                                                       
*                                                                               
         CLC   TWASCRN,SUBSCRN     SCREEN ALREADY LOADED?                       
         BE    LDSCX                                                            
*                                                                               
         MVI   TWASTRT#,1          INITIALIZE TO REC 1 ON 1ST PAGE              
*                                                                               
         LA    R3,MGHTAGH          A(LOAD POINT FOR MKGOOD SCREEN)              
         MVC   DMCB+4(3),=X'D90802' PROGRAM NUMBER                              
         MVC   DMCB+7(1),SUBSCRN   SCREEN NUMBER                                
*                                                                               
         GOTO1 CALLOV,DMCB,(R3)    LOAD SCREEN # C1 (MAKEGOOD SCREEN)           
         CLI   DMCB+4,X'FF'        SCREEN FOUND?                                
         BNE   *+6                 YES                                          
         DC    H'0'                NO  -  UMP IT OUT                            
         MVC   TWASCRN,SUBSCRN     LOAD NEW SCREEN NUMBER                       
*        MVC   1(2,R3),=X'0101'    SET TO RE-TRANSMIT SCREEN                    
*                                                                               
         OI    MGHMOPQH+1,X'0C'                                                 
         OI    MGHMOPTH+1,X'0C'+X'20'                                           
         CLI   SUBSCRN,X'C1'       ONLY GENERIC MAKEGOOD OFFER                  
         BE    LDSC12                                                           
***      CLI   SUBSCRN,X'CC'       AND RNA SCREEN                               
***      BE    LDSC12              HAS ALL OR CHOICE                            
         B     LDSC15                                                           
LDSC12   DS    0H                                                               
         NI    MGHMOPQH+1,X'FF'-X'0C'                                           
         NI    MGHMOPTH+1,X'FF'-X'0C'-X'20'                                     
*                                                                               
LDSC15   DS    0H                                                               
         MVC   8(9,R2),=C'Pf2 Apply'                                            
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'                                                     
         BRAS  RE,GETEL                                                         
         BNE   LDSCX               YES - NOT A DARE ORDER                       
         USING RCONDREL,R6                                                      
*        TM    RCONDRFG,X'80'+X'40'+X'01'                                       
*        BZ    LDSCX                                                            
         OC    RCONDRLK,RCONDRLK                                                
         BZ    LDSCX                                                            
         CLI   RCONDRLN,RCONDL2Q                                                
         BL    LDSC20                                                           
         TM    RCONDRF2,X'08'      ORDER WAS REMOVED??                          
         BO    LDSCX               YES, NOT A DARE ORDER ANYMORE                
         DROP  R6                                                               
*                                                                               
LDSC20   DS    0H                                                               
         MVC   8(9,R2),=C'Pf2 Dare '                                            
*                                                                               
LDSCX    DS    0H                                                               
         B     EXXMOD                                                           
         DROP  R4                                                               
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
CKPFKEY  NTR1  BASE=*,LABEL=*                                                   
         L     R1,AFACILS                                                       
         L     R4,0(R1)                                                         
         USING TIOBD,R4                                                         
         MVC   DUB(2),TIOBCURD                                                  
         ZIC   R0,TIOBAID                                                       
         CHI   R0,12                                                            
         BNH   *+8                                                              
         AHI   R0,-12                                                           
         STC   R0,PFKEY            SAVE OFF KEY HIT                             
         DROP  R4                                                               
*                                                                               
         CLI   PFKEY,0             NO PFKEY PRESSED                             
         BE    CKPFYES                                                          
         CLI   PFKEY,2             APPLY GROUP                                  
         BE    CKPFYES                                                          
         CLI   PFKEY,5             PAGE UP                                      
         BE    CKPF100                                                          
         CLI   PFKEY,6             PAGE DOWN                                    
         BE    CKPF100                                                          
*                                                                               
         B     CKPFNO                                                           
*                                                                               
CKPF100  DS    0H                  PAGING UP/DOWN                               
         LR    R4,RA                                                            
         AHI   R4,TWAWORKQ                                                      
         USING TWAWORK,R4                                                       
         OC    TWAMKGDH,TWAMKGDH   CANNOT PAGE UP/DOWN IF ADDING                
         BZ    CKPFNO              NEW OFFER                                    
         DROP  R4                                                               
*                                                                               
         MVC   CONACT,=C'MGL '     FAKE REDISPLAY                               
         MVC   ACTION,=C'DISP'                                                  
*                                                                               
CKPFYES  SR    RC,RC                                                            
CKPFNO   LTR   RC,RC                                                            
CKPFXIT  XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* DISPLAY ABBREVIATED CONTRACT HEADER INFO                                      
*                                                                               
DISPCON  NTR1  BASE=*,LABEL=*                                                   
         LR    R4,RA                                                            
         AHI   R4,TWAWORKQ                                                      
         USING TWAWORK,R4                                                       
         GOTO1 (RFSTAOUT,VREPFACS),DMCB,RCONKSTA,MGHSTA                         
         MVC   WORK(4),ACOMFACS                                                 
         MVC   WORK+4(2),REPALPHA                                               
         GOTO1 (RFKFLT,VREPFACS),DMCB,(X'80',RCONREC),MGHDTES,0,WORK            
         XC    MYWRK,MYWRK                                                      
         GOTO1 (RFAGYOUT,VREPFACS),DMCB,RCONKAGY,MGHAGY,MYWRK,WORK              
         MVC   MGHAGYN,MYWRK                                                    
         MVC   MGHADV,RCONKADV                                                  
         XC    MYWRK,MYWRK                                                      
         GOTO1 (RFAGYOUT,VREPFACS),DMCB,RCONKADV,MYWRK,0,WORK                   
         MVC   MGHADVN,MYWRK                                                    
         MVC   MGHSAL,RCONSAL                                                   
*                                                                               
         CLC   RCONPRD,MYSPACES    PRODUCT CODE?                                
         BE    DISCON40                                                         
*                                                                               
         MVC   MGHPRD(2),=C'C='                                                 
         MVC   MGHPRD+2(3),RCONPRD                                              
         MVI   MGHPRD+5,0                                                       
         MVC   MGHPRD+6(14),TWAPRDNM                                            
         B     DISCON60                                                         
*                                                                               
*              FIND PRODUCT ELEMENT                                             
*                                                                               
DISCON40 DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'05'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DISCON60                                                         
         MVC   MGHPRD,2(R6)        PRODUCT EXPANSION                            
*                                                                               
* DISPLAY ESTIMATE NUMBER                                                       
*                                                                               
DISCON60 DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'A2'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DISCONX             NO EI CODES                                  
         USING RCONIEL,R6                                                       
*                                                                               
         MVC   MGHEST,RCONXEST     DEFAULT USING EXPANDED EST #                 
*                                                                               
         OC    MGHEST,MYSPACES     IF NO EXPANDED ESTIMATE NUMBER FOUND         
         CLC   MGHEST,MYSPACES     USE OLD FORMAT                               
         BNE   DISCONX                                                          
         MVC   MGHEST(L'RCONIEST),RCONIEST                                      
         DROP  R6                                                               
*                                                                               
DISCONX DS     0H                                                               
         B     EXXMOD                                                           
         DROP  R4                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* CHECK IF USER PRESSED PF5/6 FOR PAGING UP OR DOWN AND SET SCREEN              
* AND OFFER RECORDS ACCORDINGLY                                                 
*                                                                               
PAGING   NTR1  BASE=*,LABEL=*                                                   
         LR    R4,RA                                                            
         AHI   R4,TWAWORKQ                                                      
         USING TWAWORK,R4                                                       
*                                                                               
         CLI   FMGTYPE,FMLBOFFQ    LATE RUN BONUS                               
         BNE   PAGE03              PF5/6 NOT SUPPORTED AT THIS TIME             
         MVI   MAXLN,MLBMAXLQ      MAX LINES ON SCREEN                          
         MVI   MAXREC#,MLBMAXRQ                                                 
         MVI   TWANEXT#,MLBMAXLQ+1                                              
         B     PAGEX                                                            
*                                                                               
PAGE03   DS    0H                                                               
         LA    R2,MGSOFL1H                                                      
         LA    R5,MGSOFL2H-MGSOFL1H                                             
         MVI   MAXLN,MGMMAXLQ                                                   
         MVI   MAXREC#,MGMMAXRQ                                                 
         CLI   FMGTYPE,FMGMOFFQ    REGULAR MAKEGOOD                             
         BE    PAGE05                                                           
*                                                                               
         LA    R2,MGPOFL1H                                                      
         LA    R5,MGPOFL2H-MGPOFL1H                                             
         MVI   MAXLN,MGPMAXLQ                                                   
         MVI   MAXREC#,MGPMAXRQ                                                 
         CLI   FMGTYPE,FMGPOFFQ    PREEMPT/CREDIT                               
         BE    PAGE05                                                           
*                                                                               
         LA    R2,MGBOFL1H                                                      
         LA    R5,MGBOFL2H-MGBOFL1H                                             
         MVI   MAXLN,MGBMAXLQ      MAX LINES ON SCREEN                          
         MVI   MAXREC#,MGBMAXRQ                                                 
         CLI   FMGTYPE,FMGBOFFQ    BONUS                                        
         BE    PAGE05                                                           
*                                                                               
         LA    R2,MLROFL1H                                                      
         LA    R5,MLROFL2H-MLROFL1H                                             
         MVI   MAXLN,MLRMAXLQ      MAX LINES ON SCREEN                          
         MVI   MAXREC#,MLRMAXRQ                                                 
         CLI   FMGTYPE,FMLROFFQ    LATE RUN                                     
         BE    PAGE05                                                           
*                                                                               
         LA    R2,RNAOFL1H                                                      
         LA    R5,RNAOFL2H-RNAOFL1H                                             
         MVI   MAXLN,RNAMAXLQ      MAX LINES ON SCREEN                          
         MVI   MAXREC#,RNAMAXRQ                                                 
         CLI   FMGTYPE,FRNAOFFQ    REPLACEMENT OFFER                            
         BE    PAGE05                                                           
*                                                                               
         DC    H'0'                                                             
*                                                                               
PAGE05   DS    0H                                                               
         ZIC   R6,MAXLN                                                         
*                                                                               
         CLI   PFKEY,5             PAGE UP                                      
         BE    PAGE08                                                           
         CLI   PFKEY,6             PAGE DOWN                                    
         BE    PAGE10                                                           
         B     PAGE20                                                           
*                                                                               
PAGE08   DS    0H                  PF5 PAGE UP                                  
         ZIC   R3,0(R2)            PUT CURSOR ON FIRST INPUT FIELD              
         LA    R3,0(R2,R3)                                                      
         OI    6(R3),X'40'                                                      
*                                                                               
         CLI   TWASTRT#,1          CHECK LOWER LIMIT                            
         BE    PAGEX                                                            
         ZIC   R0,TWASTRT#                                                      
         ZIC   R1,MAXLN                                                         
         SR    R0,R1                                                            
         STC   R0,TWASTRT#                                                      
         B     PAGE15                                                           
*                                                                               
PAGE10   DS    0H                  PF6 PAGE DOWN                                
         ZIC   R3,0(R2)            PUT CURSOR ON FIRST INPUT FIELD              
         LA    R3,0(R2,R3)                                                      
         OI    6(R3),X'40'                                                      
*                                                                               
         ZIC   R0,TWALAST#         DON'T PAGE DOWN IF THERE IS ROOM             
         ZIC   R1,TWANEXT#         AT BOTTOM OF PAGE                            
         AHI   R1,-1                                                            
         CR    R0,R1                                                            
         BL    PAGEX                                                            
*                                                                               
         CLC   TWANEXT#,MAXREC#    CHECK UPPER LIMIT                            
         BNL   PAGEX                                                            
         MVC   TWASTRT#,TWANEXT#                                                
*                                                                               
PAGE15   DS    0H                                                               
         TWAXC (R2)                CLEAR WHEN WE PAGE UP OR DOWN                
*                                                                               
PAGE20   DS    0H                                                               
         ZIC   R3,TWASTRT#                                                      
*                                                                               
PAGE30   DS    0H                                                               
         MVI   8(R2),C' '          CLEAR CHOICE COLUMN                          
         EDIT  (R3),(2,9(R2)),ALIGN=LEFT                                        
         OI    6(R2),X'80'         XMIT                                         
         AHI   R3,1                                                             
         AR    R2,R5                                                            
         BCT   R6,PAGE30                                                        
         STC   R3,TWANEXT#                                                      
*                                                                               
PAGEX    DS    0H                                                               
         B     EXXMOD                                                           
         DROP  R4                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* DISPLAY MAKEGOOD HEADER INFO                                                  
*                                                                               
DMKGHDR  NTR1  BASE=*,LABEL=*                                                   
         TWAXC MGHMGRPH                                                         
*                                                                               
         LR    R4,RA                                                            
         AHI   R4,TWAWORKQ                                                      
         USING TWAWORK,R4                                                       
*                                                                               
         XC    TWAMKGDH,TWAMKGDH                                                
*                                                                               
         OC    TWAMKGD2,TWAMKGD2                                                
         BZ    DMGHX               MUST BE ADDING                               
         XC    KEY,KEY             RECORD SELECTED: LOAD D/A                    
         MVC   KEY+28(4),TWAMKGD2                                               
         GOTO1 VGETREC,DMCB,RMKGREC                                             
*                                                                               
* OFFER NUMBER                                                                  
*                                                                               
*        EDIT  RMKGKLIN,(2,MGHMOFF),ALIGN=LEFT                                  
*                                                                               
* ALL OR CHOICE                                                                 
*                                                                               
         TM    MGHMOPTH+1,X'0C'    FIELD IS HIDDEN, DON'T BOTHER                
         BO    DMGH03                                                           
         MVI   MGHMOPT,C'A'                                                     
         TM    RMKGKRTY,X'10'                                                   
         BZ    *+8                                                              
         MVI   MGHMOPT,C'C'                                                     
         OI    MGHMOPTH+6,X'80'    XMIT                                         
*                                                                               
* MISSED LINE COMMENT                                                           
*                                                                               
DMGH03   DS    0H                                                               
*&&DO                                                                           
         LA    R6,RMKGREC                                                       
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DMGH05                                                           
         USING RMKGCDEL,R6                                                      
         CLI   RMKGCDLN,11                                                      
         BNH   DMGH05                                                           
         ZIC   RF,RMKGCDLN         GET LENGTH OF ELEMENT                        
         SHI   RF,11               MINUS 2 FOR CONTROL, 1 FOR EX                
         EX    RF,*+8              MOVE COMMENT BY LENGTH                       
         B     *+10                                                             
         MVC   MGHMLCM(0),RMKGCDDS MOVE COMMENT BY LENGTH                       
         DROP  R6                                                               
*&&                                                                             
*                                                                               
DMGH05   DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(27),RMKGREC     INSERT KEY OF SELECTED REC                   
         XC    KEY+21(6),KEY+21    CLEAR LOW ORDER OF KEY                       
         GOTO1 VHIGH               READ KEY                                     
         CLC   KEY(27),KEYSAVE     GROUP COMMENT RECORD FOUND?                  
         BNE   DMGHX               NO                                           
*                                                                               
* DDS SHOW D/A                                                                  
*                                                                               
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BNE   DMGH10                                                           
         GOTO1 HEXOUT,DMCB,KEY+28,MGHDKAD,4,=C'TOG'                             
         OI    MGHDKADH+6,X'80'    XMIT                                         
*                                                                               
DMGH10   DS    0H                                                               
         MVC   TWAMKGDH,KEY+28     SAVE OFF HEADER D/A                          
*                                                                               
         GOTO1 VGETREC,DMCB,RMKGREC                                             
*                                  DON'T MOVE THE RECORD TO RMKGREC:            
* DISPLAY GROUP CODE                                                            
*                                                                               
         MVC   MGHMGRP,RMKGKGRP                                                 
         OI    MGHMGRPH+6,X'80'    XMIT                                         
         MVC   TWAMKGDT,RMKGKGRP                                                
*                                                                               
* DISPLAY STATUS                                                                
*                                                                               
DMGH20   DS    0H                                                               
         XC    MGHSTAT,MGHSTAT                                                  
         MVC   MGHSTAT(3),=C'NEW'                                               
* APPLIED ?                                                                     
         TM    RMKGSCST,RMKGSAPQ                                                
         BZ    DMGH25                                                           
         MVC   MGHSTAT(7),=C'APPLIED'                                           
         TM    RMKGSFG3,RMGF3SAQ                                                
         BZ    DMGH40                                                           
         TM    RMKGSFG3,RMGF3ARQ                                                
         BO    DMGH40                                                           
         MVC   MGHSTAT(7),=C'SELFAPP'                                           
         CLC   RMKGAPMN,RCONMOD                                                 
         BE    DMGH40                                                           
         MVI   MGHSTAT+7,C'*'                                                   
*        BAS   RE,PROTSCRN         PROTECT ENTIRE SCREEN                        
         B     DMGH40                                                           
* RECALLED ?                                                                    
DMGH25   DS    0H                                                               
         TM    RMKGSCST,RMKGSRCQ                                                
         BZ    *+14                                                             
         MVC   MGHSTAT(8),=C'RECALLED'                                          
         B     DMGH40                                                           
* REJECTED ?                                                                    
         TM    RMKGSCST,RMKGSRJQ                                                
         BZ    DMGH30                                                           
         MVC   MGHSTAT(8),=C'REJECTED'                                          
         TM    RMKGSFG1,RMGF1MCN   SET IF DARE AND CANCELLED TO AGY             
         BZ    DMGH40                                                           
         MVC   MGHSTAT(8),=C'DARECAND'                                          
         B     DMGH40                                                           
* REVISED ?                                                                     
DMGH30   DS    0H                                                               
         TM    RMKGSCST,RMKGSRVQ                                                
         BZ    DMGH40                                                           
         MVC   MGHSTAT(7),=C'REVISED'                                           
*                                                                               
DMGH40   DS    0H                                                               
         OI    MGHSTATH+6,X'80'    XMIT                                         
*&&DO                                                                           
         MVC   MGHCRBY,=C'REP'                                                  
         TM    RMKGSCST,RMKGSCRQ                                                
         BO    *+10                                                             
         MVC   MGHCRBY,=C'STA'                                                  
                                                                                
         TM    RMKGSCST,RMKGSPAQ                                                
         BZ    DMGH60                                                           
         MVI   MGHMPAD,C'Y'                                                     
         MVI   MGHMAOK,0                                                        
         B     DMGH70                                                           
                                                                                
DMGH60   DS    0H                                                               
         MVI   MGHMPAD,0                                                        
         MVI   MGHMAOK,C'Y'                                                     
*                                                                               
* EXPIRATION DATE                                                               
*                                                                               
DMGH70   DS    0H                                                               
         GOTO1 DATCON,DMCB,(2,RMKGSEXD),(5,MGHEXPD)                             
*                                                                               
* WIP STATUS                                                                    
*                                                                               
         XC    MGHRSWP,MGHRSWP                                                  
         MVC   MGHUPBY,=C'Rep'                                                  
         TM    RMKGSFG2,RMGF2RPQ                                                
         BO    DMGH80                                                           
         MVC   MGHUPBY,=C'Sta'                                                  
*&&                                                                             
*                                                                               
DMGH80   DS    0H                                                               
         TM    RMKGSFG2,RMGF2WPQ                                                
         BZ    DMGH90                                                           
*                                                                               
         TM    RCONMODR+1,X'40'    DON'T SHOW WIP FOR GRAPHNET                  
         BO    DMGH90                                                           
*                                                                               
         TM    RMKGSCST,RMKGSAPQ   DON'T SHOW WIP IF APPLIED                    
         BO    DMGH90                                                           
         MVC   MGHRSWP(3),=C'WIP'                                               
         OI    MGHRSWPH+6,X'80'    XMIT                                         
*                                                                               
* LAST ACTIVITY DATE/TIME                                                       
*                                                                               
DMGH90   DS    0H                                                               
         GOTO1 DATCON,DMCB,(2,RMKGSLAD),(5,MGHUPDT)                             
         OI    MGHUPDTH+6,X'80'    XMIT                                         
*        GOTO1 HEXOUT,DMCB,RMKGSLAT,MGHUPTM+1,2,=C'TOG'                         
*        MVC   MGHUPTM(2),MGHUPTM+1                                             
*        MVI   MGHUPTM+2,C':'                                                   
*                                                                               
* CREATION DATE/TIME                                                            
*                                                                               
         GOTO1 DATCON,DMCB,(2,RMKGSCRD),(5,MGHCRDT)                             
         OI    MGHCRDTH+6,X'80'    XMIT                                         
*                                                                               
* GROUP COMMENT                                                                 
*                                                                               
         LA    R2,MGHGCMTH                                                      
         LA    R6,RMKGREC                                                       
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DMGHX                                                            
         USING RMKGGCEM,R6                                                      
DMGH100  CLI   RMKGGCLN,3                                                       
         BNH   DMGH110                                                          
         ZIC   RF,RMKGGCLN         GET LENGTH OF ELEMENT                        
         SHI   RF,3                MINUS 2 FOR CONTROL, 1 FOR EX                
         EX    RF,*+8              MOVE COMMENT BY LENGTH                       
         B     *+10                                                             
         MVC   8(0,R2),RMKGGCCM    MOVE COMMENT BY LENGTH                       
         OI    6(R2),X'80'         XMIT                                         
DMGH110  BRAS  RE,NEXTEL                                                        
         BNE   DMGHX                                                            
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         B     DMGH100                                                          
         DROP  R6                                                               
*                                                                               
DMGHX    DS    0H                                                               
         B     EXXMOD                                                           
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
* FOR APPLIED OFFERS, PROTECT ENTIRE SCREEN TO PREVENT USERS FROM               
* MAKING CHANGES                                                                
*                                                                               
PROTSCRN NTR1                                                                   
         LA    R2,MGHMGRPH                                                      
*                                                                               
PROTS10  DS    0H                                                               
         OI    1(R2),X'20'         SET PROTECTED                                
         ZIC   RE,0(R2)                                                         
         LTR   RE,RE                                                            
         BZ    PROTSX                                                           
         AR    R2,RE                                                            
         B     PROTS10                                                          
*                                                                               
PROTSX   DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* PROT DEMO INPUT FIELD IF THERE IS NO DEMO CATE                                
*                                                                               
PROTDEM  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,MGHTAGH       TURN DEM INPUT TO PROTECTION                    
         USING FLDHDRD,R2                                                       
PROTD10  DS    0H                                                               
         TM    FLDATB,X'02'                                                     
         BZ    NEXT             NO EXTENTION                                    
         LR    R4,R2                                                            
         ZIC   R3,0(R4)                                                         
         AR    R4,R3                                                            
         AHI   R4,-8            GO TO THE EXTENTION                             
         CLI   0(R4),255        DEM FIELD ALL HAS ID = 255                      
         BNE   NEXT                                                             
         OI    FLDATB,X'20'     MARK PROTECTED                                  
         OI    FLDOIND,X'80'    MARK RETRANSMIT                                 
NEXT     DS    0H                                                               
         ZIC   R3,FLDLEN                                                        
         LTR   R3,R3                                                            
         BZ    PROTDEMX                                                         
         AR    R2,R3                                                            
         B     PROTD10                                                          
PROTDEMX DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* DISPLAY MISSED/OFFER LINE(S)                                                  
*                                                                               
DMKGOFF  NTR1  BASE=*,LABEL=*                                                   
         LR    R4,RA                                                            
         AHI   R4,TWAWORKQ                                                      
         USING TWAWORK,R4                                                       
*                                                                               
         OC    TWAMKGD2,TWAMKGD2   INCASE U=N TESTING                           
         BZ    DOFFX                                                            
*                                                                               
         MVC   KEY+28(4),TWAMKGD2                                               
         GOTO1 VGETREC,DMCB,RMKGREC                                             
*                                                                               
         LA    R2,MGSMD1H                                                       
         CLI   FMGTYPE,FMGMOFFQ    DO ONLY FOR REGULAR MAKEGOOD OFFERS          
         BE    DOFF10                                                           
         LA    R2,RNAMD1H                                                       
         CLI   FMGTYPE,FRNAOFFQ    AND REPLACEMENT NA OFFERS                    
         BNE   DOFF20                                                           
*                                                                               
DOFF10   DS    0H                                                               
         GOTO1 DMKGMSS,DMCB,(R2)   DISPLAY MISSED LINE(S) INFO                  
*                                                                               
DOFF20   DS    0H                                                               
*                                                                               
         MVI   LINECTR,1                                                        
*                                                                               
         LA    R2,MGPOFL1H                                                      
         MVI   MAXLN,MGPMAXLQ      MAX LINES ON SCREEN                          
         CLI   FMGTYPE,FMGPOFFQ    PREEMPT/CREDIT                               
         BE    DOFF25                                                           
*                                                                               
         LA    R2,MGBOFL1H                                                      
         MVI   MAXLN,MGBMAXLQ      MAX LINES ON SCREEN                          
         CLI   FMGTYPE,FMGBOFFQ    BONUS                                        
         BE    DOFF25                                                           
*                                                                               
         LA    R2,MLROFL1H                                                      
         MVI   MAXLN,MLRMAXLQ      MAX LINES ON SCREEN                          
         CLI   FMGTYPE,FMLROFFQ    LATE RUN                                     
         BE    DOFF25                                                           
*                                                                               
         LA    R2,MLBMFL1H                                                      
         MVI   MAXLN,MLBMAXLQ      MAX LINES ON SCREEN                          
         CLI   FMGTYPE,FMLBOFFQ    LATE RUN WITH BONUS                          
         BE    DOFF25                                                           
*                                                                               
         LA    R2,RNAOFL1H                                                      
         MVI   MAXLN,RNAMAXLQ      MAX LINES ON SCREEN                          
         CLI   FMGTYPE,FRNAOFFQ    REPLACEMENT NA                               
         BE    DOFF25                                                           
*                                                                               
         LA    R2,MGSOFL1H                                                      
         MVI   MAXLN,MGMMAXLQ      MAX LINES ON SCREEN                          
*                                                                               
DOFF25   DS    0H                                                               
         MVC   KEY(27),RMKGKEY                                                  
         GOTO1 VHIGH                                                            
         B     DOFF35                                                           
*                                                                               
DOFF30   DS    0H                                                               
         GOTO1 VSEQ                READ DETAIL RECORD                           
*                                                                               
DOFF35   DS    0H                                                               
         CLC   KEY(RMKGKPLN-RMKGKEY),KEYSAVE                                    
         BNE   DOFFX                                                            
         CLC   KEY+RMKGKLIN-RMKGKEY(1),RMKGKLIN                                 
         BNE   DOFF30                                                           
*                                                                               
         MVI   STARTNUM,0          START AT RECORD                              
         MVN   STARTNUM,KEY+RMKGKRTY-RMKGKEY                                    
         CLC   STARTNUM,TWASTRT#                                                
         BL    DOFF30                                                           
*                                                                               
DOFF40   DS    0H                                                               
         GOTO1 VGETREC,DMCB,RMKGREC                                             
*                                                                               
         MVI   8(R2),C' '                                                       
         LA    R6,RMKGREC                                                       
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DOFF45                                                           
         USING RMKGSTEL,R6                                                      
         TM    RMKGSTCH,X'01'                                                   
         BZ    DOFF45                                                           
         MVI   8(R2),C'*'                                                       
         DROP  R6                                                               
*                                                                               
DOFF45   DS    0H                                                               
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         LR    R3,R2                                                            
*                                                                               
         MVI   TWALAST#,0          HIGHEST DETAIL RECORD # ON SCREEN            
         MVN   TWALAST#,RMKGKRTY                                                
*                                                                               
* DISPLAY EFFECTIVE DATES                                                       
*                                                                               
*        LR    R2,R3                                                            
*                                                                               
         TM    RMKGRTS,X'02'       FOR REPLACEMENT NA OFFER, CHECK IF           
         BZ    DOFF48              ORDER IS CREDIT FOR NA                       
         LA    R6,RMKGREC                                                       
         MVI   ELCODE,X'03'                                                     
         BRAS  RE,GETEL                                                         
         BE    DOFF80                                                           
         MVC   RNAODTS(4),=C'NONE'                                              
         B     DOFFX                                                            
*                                                                               
DOFF48   DS    0H                                                               
         TM    RMKGRTS,X'10'       FOR PREEMPT/CREDIT, WE WANT TO SHOW          
         BZ    DOFF50              MISSED DATE(S) INSTEAD                       
         GOTO1 DMKGMSS,DMCB,(R2)                                                
         AHI   R2,MGPODEMH-MGPODTSH                                             
         GOTO1 =A(DISDEMV),RR=YES                                               
         AHI   R2,MGPOCMTH-MGPODEMH                                             
         B     DOFF140             BUMP TO COMMENT FIELD                        
*                                                                               
DOFF50   DS    0H                                                               
         TM    RMKGRTS,X'08'       FOR LATE RUN, WE WANT TO                     
         BZ    DOFF60              SHOW MISSED DATE(S) INSTEAD                  
         GOTO1 DMKGMSS,DMCB,(R2)                                                
         AHI   R2,MLRORATH-MLRODTSH                                             
         EDIT  (4,RMKGCOS),(9,8(R2)),2,ALIGN=LEFT,FLOAT=-                       
         AHI   R2,MLROTMSH-MLRORATH                                             
         GOTO1 ACTUTIME,DMCB,(R2)                                               
         AHI   R2,MLRODEMH-MLROTMSH                                             
         GOTO1 =A(DISDEMV),RR=YES                                               
         AHI   R2,MLROPGNH-MLRODEMH                                             
         B     DOFF131             BUMP TO PROGRAM FIELD                        
*                                                                               
DOFF60   DS    0H                                                               
         TM    RMKGRTS,X'04'       FOR LATE RUN W/BONUS, WE WANT TO             
         BZ    DOFF80              SHOW MISSED DATE(S) INSTEAD                  
         TM    RMKGRTS,X'20'       BONUS PORTION?                               
         BO    DOFF70                                                           
         GOTO1 DMKGMSS,DMCB,(R2)                                                
         AHI   R2,MLBMRATH-MLBMDTSH                                             
         EDIT  (4,RMKGCOS),(9,8(R2)),2,ALIGN=LEFT,FLOAT=-                       
         AHI   R2,MLBMTMSH-MLBMRATH                                             
         GOTO1 ACTUTIME,DMCB,(R2)                                               
         AHI   R2,MLBMDEMH-MLBMTMSH                                             
         GOTO1 =A(DISDEMV),RR=YES                                               
         AHI   R2,MLBOPGNH-MLBMDEMH                                             
         B     DOFF131             BUMP TO PROGRAM FIELD                        
*                                                                               
DOFF70   DS    0H                  LATE RUN BONUS                               
         LA    RE,MLBBDTSH         STARTS AT BOTTOM HALF OF SCREEN              
         CR    R2,RE                                                            
         BH    DOFF80                                                           
         LR    R2,RE                                                            
*                                                                               
DOFF80   DS    0H                                                               
         GOTO1 DEFFDAT,DMCB,(R2)                                                
*                                                                               
* NUMBER OF SPOT(S) OFFERED                                                     
*                                                                               
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         EDIT  RMKGNW,(3,8(R2)),ALIGN=LEFT,ZERO=NOBLANK                         
*                                                                               
* RATE (SKIP FOR PREEMPT AND LATERUN)                                           
*                                                                               
         TM    FMGTYPE,FMGPOFFQ+FMLROFFQ                                        
         BNZ   DOFF90                                                           
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         EDIT  (4,RMKGCOS),(9,8(R2)),2,ALIGN=LEFT,FLOAT=-                       
*                                                                               
* LENGTH                                                                        
*                                                                               
DOFF90   DS    0H                                                               
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         MVC   HALF,RMKGDUR                                                     
         NI    HALF,X'7F'                                                       
         EDIT  (2,HALF),(3,8(R2)),ALIGN=LEFT                                    
*                                                                               
         OI    6(R2),X'80'         TURN ON TRANSMIT BIT                         
         STC   R0,5(R2)            NEW INPUT LENGTH                             
         LA    RE,8(R2)                                                         
         AR    RE,R0                                                            
         TM    RMKGDUR,X'80'       MINUTES?                                     
         BZ    DOFF100                                                          
         MVI   0(RE),C'M'                                                       
*                                                                               
         ZIC   RF,5(R2)            ADJUST FOR NEW LENGTH                        
         AHI   RF,1                                                             
         STC   RF,5(R2)                                                         
*                                                                               
* DISPLAY DAY/TIME                                                              
*                                                                               
DOFF100  DS    0H                                                               
         TM    RMKGRTS,X'10'       SKIP DAY/TIME FOR PREEMPT                    
         BO    DOFF130                                                          
         TM    RMKGRTS,X'20'       BONUS                                        
         BO    DOFF110                                                          
*        TM    RMKGRTS,X'08'+X'04' LATE RUN ONLY DISPLAY ACTUAL TIME            
*        BNZ   DOFF120                                                          
*                                                                               
DOFF110  DS    0H                                                               
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         GOTO1 DDAYTIM,DMCB,(R2)                                                
         ZIC   RE,0(R2)            BUMP PASS TIME FIELD                         
         AR    R2,RE                                                            
*                                                                               
* DISPLAY ACTUAL TIME FOR LATE RUN OFFERS                                       
*                                                                               
*OFF120  DS    0H                                                               
*        GOTO1 ACTUTIME,DMCB,(R2)                                               
*                                                                               
*                                                                               
* DISPLAY PROGRAM NAME FOR REPLACEMENT NA                                       
* UPDATE: PROGRAM NAME FOR ALL SCREEN BUT PREEMPT/CREDIT                        
DOFF130  DS    0H                                                               
         CLI   FMGTYPE,FMGPOFFQ                                                 
         BE    DOFF135                                                          
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
*                                                                               
DOFF131  LA    R6,RMKGREC          FIND PROGRAM NAME                            
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL                                                         
         BNE   DOFF135             NOT FOUND                                    
         CLI   1(R6),3                                                          
         BL    DOFF140                                                          
         ZIC   RF,1(R6)            DERIVE COMMENT LENGTH                        
         SHI   RF,3                                                             
*MN                                                                             
         CLI   1(R6),20                                                         
         BL    *+8                                                              
         LA    RF,16               NEW PGM FIELD MAX (FROM 34 TO 17)            
*MN                                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),2(R6)                                                    
*                                                                               
*                                                                               
* DISPLAY DETAIL COMMENT                                                        
*                                                                               
DOFF135  DS    0H                                                               
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
*                                                                               
DOFF140  DS    0H                                                               
         LA    R6,RMKGREC          FIND DETAIL COMMENT                          
         MVI   ELCODE,X'11'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DOFF150             NOT FOUND                                    
         CLI   1(R6),3                                                          
         BL    DOFF150                                                          
         ZIC   RF,1(R6)            DERIVE COMMENT LENGTH                        
         SHI   RF,2                                                             
*                                                                               
         ZIC   RE,0(R2)            IF COMMENT LONGER                            
         SHI   RE,8                THAN COMMENT FIELD ON SCREEN                 
         CR    RF,RE               CHOP OFF COMMENT                             
         BNH   *+6                                                              
         LR    RF,RE                                                            
*                                                                               
         SHI   RF,1                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),2(R6)                                                    
*                                                                               
DOFF150  DS    0H                  DEMO VALUE                                   
         TM    RMKGRTS,X'10'       PREEMPT                                      
         BO    DOFF160             SKIP TO NEXT LINE                            
         TM    RMKGRTS,X'08'       FOR LATE RUN, SKIP TO NEXT LINE              
         BO    DOFF160             SINCE DEMO IS ON THE PREVIOUS LINE           
         TM    RMKGRTS,X'04'       NOT LATE RUN/BONUS,PROCESS                   
         BZ    NOTLB                                                            
         TM    RMKGRTS,X'20'       LATE RUN POTION OF L/B?                      
         BZ    DOFF160             YES, SKIP TO NEXT LINE                       
*                                  SINCE DEMO IS ON THE PREVIOUS LINE           
NOTLB    DS    0H                                                               
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
*                                                                               
         GOTO1 =A(DISDEMV),RR=YES                                               
DOFF160  DS    0H                                                               
         CLC   LINECTR,MAXLN                                                    
         BNL   DOFFX                                                            
*                                                                               
         ZIC   RE,LINECTR                                                       
         AHI   RE,1                                                             
         STC   RE,LINECTR                                                       
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
*                                                                               
         TM    FMGTYPE,FMLBOFFQ    IF LATE RUN WITH BONUS                       
         BZ    DOFF30                                                           
         TM    RMKGRTS,X'20'+X'04' PROCESS BONUS PORTION?                       
         BNO   DOFF30                                                           
         CLI   LINECTR,4           LATE RUN BONUS STARTS AT LINE 4              
         BH    DOFF30                                                           
*        LA    R2,MLBBFL1H                                                      
         MVI   LINECTR,5                                                        
         B     DOFF30                                                           
*                                                                               
DOFFX    DS    0H                                                               
         B     EXXMOD                                                           
         DROP  R4                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* DISPLAY MISSED DATES                                                          
*                                                                               
DMKGMSS  NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)                                                         
*                                                                               
         LR    R4,RA                                                            
         AHI   R4,TWAWORKQ                                                      
         USING TWAWORK,R4                                                       
*                                                                               
         LA    R6,RMKGREC                                                       
         MVI   ELCODE,X'05'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DMSSX                                                            
         USING RMKGMGEL,R6                                                      
*                                                                               
DMSS110  DS    0H                                                               
         OI    4(R2),X'20'         SET PREVIOUSLY VALIDATED                     
         GOTO1 DATCON,DMCB,(3,RMKGMGD1),(4,8(R2))                               
         TM    RMKGMGFG,X'80'      ALTERNATING WEEKS?                           
         BZ    *+8                                                              
         MVI   13(R2),C'A'                                                      
         OC    RMKGMGD2,RMKGMGD2                                                
         BZ    DMSS120                                                          
         MVI   13(R2),C'-'                                                      
         GOTO1 DATCON,DMCB,(3,RMKGMGD2),(4,14(R2))                              
         TM    RMKGMGFG,X'80'      ALTERNATING WEEKS?                           
         BZ    *+8                                                              
         MVI   19(R2),C'A'                                                      
*                                                                               
DMSS120  DS    0H                                                               
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
*                                                                               
         OI    4(R2),X'20'         SET PREVIOUSLY VALIDATED                     
         EDIT  RMKGMGSP,(3,8(R2)),ALIGN=LEFT                                    
*                                                                               
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
*                                                                               
         TM    FMGTYPE,FMLROFFQ+FMLBOFFQ                                        
         BZ    DMSS130                                                          
         OI    4(R2),X'20'         SET PREVIOUSLY VALIDATED                     
         ZIC   R1,0(R2)            BUMP PASS RATE FIELD                         
         AR    R2,R1                                                            
*                                                                               
DMSS130  DS    0H                                                               
         OI    4(R2),X'20'         SET PREVIOUSLY VALIDATED                     
         EDIT  RMKGMGLI,(3,8(R2)),ALIGN=LEFT                                    
*                                                                               
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BE    DMSS110                                                          
         DROP  R6                                                               
*                                                                               
DMSSX    EQU   *                                                                
         B     EXXMOD                                                           
         DROP  R4                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* DISPLAY EFFECTIVE DATE(S)                                                     
*                                                                               
DEFFDAT  NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)                                                         
*                                                                               
         LA    R6,RMKGREC                                                       
         MVI   ELCODE,X'03'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R7,WORK2            OUTPUT                                       
         MVI   WORK2,C' '                                                       
*                                                                               
         MVC   WORK2+1(L'WORK2-1),WORK2                                         
         LA    R5,WORK2+60         OUTPUT END                                   
*                                                                               
         LA    R4,WORK2+120                                                     
DEFF10   LA    R3,WORK+20                                                       
*                                  DISPLAY DATES                                
         GOTO1 DATCON,DMCB,(3,2(R6)),(4,(R3))                                   
*                                  START DATE                                   
         CLC   2(3,R6),5(R6)                                                    
         BNE   *+12                                                             
         LA    R3,5(R3)                                                         
         B     DEFF20                                                           
*                                                                               
         MVI   5(R3),C'-'                                                       
*                                                                               
         GOTO1 (RF),(R1),(3,5(R6)),(4,6(R3))                                    
*                                  END DATE                                     
         LA    R3,11(R3)                                                        
*                                                                               
DEFF20   TM    8(R6),X'40'         ALT WEEKS?                                   
         BZ    *+12                                                             
         MVI   0(R3),C'A'                                                       
         LA    R3,1(R3)                                                         
*                                                                               
DEFF30   LA    RE,WORK+20                                                       
         SR    R3,RE               GET ELEM DISPLAY LEN                         
         LR    RF,R7                                                            
         AR    RF,R3               OUTPUT PTR                                   
*                                  ROOM IN FIRST LINE?                          
         CR    RF,R5               WORK2+60                                     
         BNH   DEFF40              NO  - START AT SECOND LINE                   
*                                                                               
         LA    R5,500(R5)          ELIM. FIRST TEST                             
         LA    R7,WORK2+60         START 2D LINE                                
         CLI   WORK2+60,C'*'                                                    
         BNE   DEFF50                                                           
         LA    R7,1(R7)                                                         
         B     DEFF50                                                           
*                                                                               
DEFF40   CR    RF,R4               WORK2+120                                    
         BH    DEFF60              DOESN'T FIT                                  
*                                                                               
DEFF50   BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),WORK+20                                                  
         MVC   WORK+20(20),MYSPACES                                             
         LA    R7,1(R3,R7)         OUTPUT PTR                                   
         BRAS  RE,NEXTEL                                                        
         BNE   DEFF110                                                          
*                                                                               
         MVI   0(R7),C'*'                                                       
         LA    R7,1(R7)                                                         
         B     DEFF10                                                           
*                                  DATES DON'T FIT  - TRY TO                    
*                                     COMPRESS  - DISPLAY WEEKS AS              
*                                     END DATE                                  
DEFF60   LA    R6,RMKGREC                                                       
         MVI   ELCODE,X'03'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   WORK2,C' '                                                       
         MVC   WORK2+1(L'WORK2-1),WORK2                                         
         LA    R3,WORK2                                                         
*                                                                               
DEFF70   GOTO1 DATCON,(R1),(3,2(R6)),(4,(R3))                                   
         LA    R7,5                                                             
         CLI   3(R3),C'0'          DAY =JAN01?                                  
         BNE   DEFF80                                                           
         MVC   3(1,R3),4(R3)       COMPRESS TO JAN1                             
         MVI   4(R3),C' '                                                       
         BCTR  R7,R0                                                            
DEFF80   AR    R3,R7                                                            
*                                                                               
         CLI   10(R6),1            1 WEEK?                                      
         BE    DEFF90                                                           
         MVI   0(R3),C'-'                                                       
         LA    R3,1(R3)                                                         
*                                  DISPLAY WEEKS                                
         EDIT  (1,10(R6)),(2,(R3)),ALIGN=LEFT                                   
         CLI   10(R6),9                                                         
         BH    *+16                                                             
         MVI   1(R3),C'W'                                                       
         LA    R3,2(R3)                                                         
         B     *+12                                                             
         MVI   2(R3),C'W'                                                       
         LA    R3,3(R3)                                                         
         TM    8(R6),X'40'         ALT WEEKS?                                   
         BZ    *+12                                                             
         MVI   0(R3),C'A'                                                       
         LA    R3,1(R3)                                                         
*                                                                               
DEFF90   EQU   *                                                                
*                                  GET NEXT ELEM                                
DEFF100  DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   DEFF110                                                          
         MVI   0(R3),C'*'                                                       
         LA    R3,1(R3)                                                         
         B     DEFF70                                                           
DEFF110  EQU   *                                                                
         MVC   8(24,R2),WORK2      INSERT DATES ONTO SCREEN                     
         OI    6(R2),X'80'         SET TRANSMIT BIT                             
*                                                                               
DEFFX    DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* DISPLAY DAY/TIME                                                              
*                                                                               
DDAYTIM  NTR1  BASE=*,LABEL=*                                                   
         L     R3,0(R1)            DAY FIELD HEADER                             
         ZIC   R5,0(R3)                                                         
         AR    R5,R3               TIME FIELD HEADER                            
*                                                                               
         AHI   R3,8                BUMP TO DAY FIELD                            
         AHI   R5,8                BUMP TO TIME FIELD                           
         LA    R7,L'MGSODYS-1(R3)  DAY FIELD END-1                              
         LA    R4,L'MGSOTMS-1(R5)  TIME FIELD END-1                             
*                                                                               
         LA    R6,RMKGREC                                                       
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DDT10    DS    0H                                                               
         MVC   WORK,MYSPACES                                                    
*                                  DISPLAY DAY-TIME FIELDS                      
         GOTO1 =V(OUTDAY),DMCB,3(R6),2(R6),WORK,RR=RELO                         
         LA    RE,WORK                                                          
         SR    R2,R2                                                            
         CLI   0(RE),C' '                                                       
         BE    *+16                                                             
         LA    R2,1(R2)                                                         
         LA    RE,1(RE)                                                         
         B     *-16                                                             
*                                  ADD DAY LENGTH FIELD                         
         LA    RE,0(R2,R3)                                                      
         CR    RE,R7               ROOM LEFT?                                   
         BH    DDTX                                                             
         STC   R2,BYTE                                                          
         BCTR  R2,R0                                                            
         EX    R2,*+8              MOVE DAY                                     
         B     *+10                                                             
         MVC   0(0,R3),WORK                                                     
*                                                                               
*                                  TIME                                         
         MVI   WORK,C' '                                                        
         MVC   WORK+1(L'WORK-1),WORK                                            
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A11'                                           
         GOTO1 CALLOV,DMCB,0       GET ADDRESS OF UNTIME                        
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),4(R6),WORK                                             
         LA    RE,WORK             GET TIME LEN                                 
         SR    R2,R2                                                            
         CLI   0(RE),C' '                                                       
         BE    *+16                                                             
         LA    R2,1(R2)                                                         
         LA    RE,1(RE)                                                         
         B     *-16                                                             
         LA    RE,0(R2,R5)                                                      
         CR    RE,R4               ROOM LEFT?                                   
         BH    DDTX                                                             
         STC   R2,BYTE2                                                         
         BCTR  R2,R0                                                            
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),WORK                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   DDTX                                                             
         ZIC   R2,BYTE             DAY LENGTH                                   
         LA    R3,0(R2,R3)         NEXT DAY                                     
         ZIC   R2,BYTE2            TIME LARGER                                  
         LA    R5,0(R2,R5)         NEXT TIME                                    
         MVI   0(R3),C'*'          FIELD SEPARATOR                              
         MVI   0(R5),C'*'                                                       
         LA    R3,1(R3)                                                         
         LA    R5,1(R5)                                                         
         B     DDT10                                                            
*                                                                               
DDTX     DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* DISPLAY ACTUAL AIR TIME                                                       
*                                                                               
ACTUTIME NTR1  BASE=*,LABEL=*                                                   
         L     R5,0(R1)            TIME FIELD HEADER                            
*                                                                               
         LR    R4,R5               TIME FIELD END-1                             
         ZIC   R1,0(R5)                                                         
         AR    R4,R1                                                            
         AHI   R4,-1                                                            
         AHI   R5,8                BUMP TO TIME FIELD                           
*                                                                               
         LA    R6,RMKGREC                                                       
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
ACT10    DS    0H                                                               
         MVC   WORK,MYSPACES                                                    
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A11'                                           
         GOTO1 CALLOV,DMCB,0       GET ADDRESS OF UNTIME                        
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),4(R6),WORK                                             
         LA    RE,WORK             GET TIME LEN                                 
         SR    R2,R2                                                            
         CLI   0(RE),C' '                                                       
         BE    *+16                                                             
         LA    R2,1(R2)                                                         
         LA    RE,1(RE)                                                         
         B     *-16                                                             
         LA    RE,0(R2,R5)                                                      
         CR    RE,R4               ROOM LEFT?                                   
         BH    ACTX                                                             
         STC   R2,BYTE2                                                         
         BCTR  R2,R0                                                            
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),WORK                                                     
         B     ACTX                                                             
*&&DO                                                                           
         BRAS  RE,NEXTEL                                                        
         BNE   ACTX                                                             
         ZIC   R2,BYTE2            TIME LARGER                                  
         AR    R5,R2               NEXT TIME                                    
         MVI   0(R5),C'*'          FIELD SEPARATOR                              
         LA    R5,1(R5)                                                         
         B     ACT10                                                            
*&&                                                                             
*                                                                               
ACTX     DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
         ORG   CONCNUMX+8                                                       
       ++INCLUDE RECNTC0D                                                       
         ORG   MGHTAGH                                                          
       ++INCLUDE RECNTC1D                                                       
         ORG   MGHTAGH                                                          
       ++INCLUDE RECNTC2D                                                       
         ORG   MGHTAGH                                                          
       ++INCLUDE RECNTC3D                                                       
         ORG   MGHTAGH                                                          
       ++INCLUDE RECNTC4D                                                       
         ORG   MGHTAGH                                                          
       ++INCLUDE RECNTC5D                                                       
         ORG   MGHTAGH                                                          
       ++INCLUDE RECNTCCD                                                       
         ORG   CONLAST                                                          
       ++INCLUDE RECNTC6D                                                       
       ++INCLUDE DDTSARD                                                        
         PRINT OFF                                                              
EDICTD   DSECT                                                                  
       ++INCLUDE EDIDDSHD                                                       
       ++INCLUDE EDILINKD                                                       
       ++INCLUDE SPDARMKGDD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE REGENDAR                                                       
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDCOMFACSD                                                     
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
       ++INCLUDE FLDHDRD                                                        
         EJECT                                                                  
IMWORKD  DSECT                                                                  
IMSVIO   DS    A                                                                
IMSVKEY  DS    XL27                                                             
IMIO     DS    XL2000                                                           
IMWORKQ  EQU   *-IMWORKD                                                        
*                                                                               
* LOCAL STORAGE, EQUATES, ETC                                                   
*                                                                               
MYWORKD  DSECT                                                                  
RELO     DS    A                                                                
PERVAL   DS    V                                                                
VTSAR    DS    V                                                                
ACTION   DS    F                                                                
PFKEY    DS    X                                                                
*                                                                               
TEMPFLG  DS    C                                                                
*                                                                               
FLAGS    DS    X                                                                
* X'80' : DETAIL RECORD SELECTED                                                
* X'40' : MAKEGOOD HEADER RECORD EXISTS                                         
* X'20' : ADD RECORD TO TSAR BUFFER                                             
*                                                                               
* X'10' : USER IS ADDING NEW OFFER: MGM/MGP/MGB/MLR/MLB                         
FACTADDQ EQU   X'10'                                                            
*                                                                               
* X'08' : USER IS CHANGING OFFER: MGC                                           
FACTCHAQ EQU   X'08'                                                            
*                                                                               
* X'04' : USER IS DELETING OFFER: MGX                                           
FACTMGXQ EQU   X'04'                                                            
*                                                                               
* X'02' : PROCESSING BONUS PORTION OF LATE RUN WITH BONUS                       
FACTBONQ EQU   X'02'                                                            
*                                                                               
* X'01' : USER IS DELETING AT LEAST ONE LINE ON SCREEN                          
FACTDELQ EQU   X'01'                                                            
*                                                                               
FMGTYPE  DS    X                   MUST BE *ONE* OF THE FOLLOWING:              
FMGMOFFQ EQU   X'00'               MGM OFFER TYPE                               
FMGBOFFQ EQU   X'20'               MGB OFFER TYPE                               
FMGPOFFQ EQU   X'10'               MGP OFFER TYPE                               
FMLROFFQ EQU   X'08'               MLR OFFER TYPE                               
FMLBOFFQ EQU   X'04'               MLB OFFER TYPE                               
FRNAOFFQ EQU   X'02'               RNA OFFER TYPE                               
*                                                                               
OFFRTYPE DS    0CL21                                                            
         DS    X                   LENGTH                                       
         DS    CL19                OFFER TYPE                                   
         DS    X                   TERMINATING NULL                             
*                                                                               
MAXLN DS       X                   MAX DISPLAY LINES FOR OFFER TYPE             
MGMMAXLQ EQU   4                                                                
MGPMAXLQ EQU   7                                                                
MGBMAXLQ EQU   7                                                                
MLRMAXLQ EQU   7                                                                
MLBMAXLQ EQU   6                                                                
RNAMAXLQ EQU   4                                                                
*                                                                               
MAXREC#  DS    X                   MAX DETAIL OFFER # FOR OFFER TYPE            
MGMMAXRQ EQU   12                                                               
MGPMAXRQ EQU   14                                                               
MGBMAXRQ EQU   14                                                               
MLRMAXRQ EQU   14                                                               
MLBMAXRQ EQU   6                                                                
RNAMAXRQ EQU   12                                                               
*                                                                               
SUBSCRN  DS    X                                                                
CREATED  DS    X                   OFFER CREATOR, REP=BLANK, STA=$              
BYREP    EQU   0                                                                
BYSTA    EQU   C'$'                                                             
TARGETBY DS    X                                                                
LINECTR  DS    X                                                                
STARTNUM DS    X                                                                
OFFREC#  DS    X                                                                
OFFERNUM DS    X                                                                
LRDAY    DS    X                   LATE RUN DAY                                 
DAYBITS  DS    X                   TARGET LATE RUN DAYS IN BITS                 
GSTRDATE DS    XL3                 (YMD) MISSED BUY START DATE FOR GRID         
BUYGRID  DS    XL53                BUYGRID FOR TARGET MISSED BUY                
BSTRDATE DS    XL3                 START DATE USED IN GRID CALCULATIONS         
BMSS#SPT DS    X                                                                
LASTMG#  DS    X                   LAST DETAIL OFFER RECORD NUMBER              
MGDATE   DS    XL3                 USE IF RANGE SPECIFIED                       
FIRSTAIR DS    XL3                 FIRST AIR DATE OF OFFERS                     
MYWRK    DS    XL27                                                             
SVKEY    DS    XL27                                                             
SVTSARKY DS    XL27                                                             
TSARBLK  DS    CL256                                                            
ELEM     DS    XL256                                                            
EL05BLK  DS    XL(15*EL05BLQ)      X'05' MISSED ELEMENT BLOCK                   
EL05BLQ  EQU   RMKGMGLQ+3          THE EXTRA 3 BYTES IS THE FLD OFFSET          
*                                  SO WE KNOW WHERE TO PUT CURSOR IN            
*                                  CASE OF ERROR                                
MGDTREC  DS    XL1000              FIRST MAKEGOOD DETAIL RECORD                 
TSARREC  DS    XL1002              LENGTH + RECORD                              
MKGHDR   DS    XL1002              MAKEGODD HEADER RECORD                       
MYWORKX  EQU   *                                                                
         EJECT                                                                  
T8023A   CSECT                                                                  
*                                                                               
* FOR #SP/RATE/LEN/TIME FIELDS, IF BLANK                                        
* PROPAGATE INPUTS FROM LINE ABOVE                                              
*                                                                               
DITTO    NTR1  BASE=*,LABEL=*                                                   
         TM    FLAGS,FACTADDQ+FACTCHAQ                                          
         BZ    DITTOX              DO ONLY FOR ADD OR CHANGE                    
*                                                                               
         XC    OFFRDISP,OFFRDISP                                                
*                                                                               
         LA    R3,MGMTAB                                                        
         CLI   FMGTYPE,FMGMOFFQ    MANY FOR MANY                                
         BE    DITTO10                                                          
*                                                                               
         LA    R3,RNATAB                                                        
         CLI   FMGTYPE,FRNAOFFQ    REPLACEMENT NA                               
         BE    DITTO10                                                          
*                                                                               
         LA    R3,MGPTAB                                                        
         CLI   FMGTYPE,FMGPOFFQ    PREEMPT/CREDIT                               
         BE    DITTO10                                                          
*                                                                               
         LA    R3,MGBTAB                                                        
         CLI   FMGTYPE,FMGBOFFQ    BONUS                                        
         BE    DITTO10                                                          
*                                                                               
         LA    R3,MLRTAB                                                        
         CLI   FMGTYPE,FMLROFFQ    LATE RUN                                     
         BE    DITTO10                                                          
*                                                                               
         LA    R3,MLBTAB                                                        
         CLI   FMGTYPE,FMLBOFFQ    LATE RUN WITH BONUS                          
         BE    DITTO10                                                          
*                                                                               
         B     DITTOX                                                           
*                                                                               
DITTO10  DS    0H                                                               
         MVC   OFFRDISP+2(2),0(R3) OFFSET TO NEXT LINE                          
         AHI   R3,2                                                             
         ZICM  R2,0(R3),2          POINT R2 TO STARTING LINE TO PROCESS         
         AR    R2,RA                                                            
         AHI   R3,2                                                             
         ZICM  R4,0(R3),2          NUMBER OF LINES TO PROCESS                   
         AHI   R3,2                                                             
         LR    R6,R3                                                            
         B     DITTO25                                                          
*                                                                               
DITTO20  DS    0H                                                               
         LR    R3,R6                                                            
*                                                                               
DITTO25  DS    0H                                                               
         CLI   5(R2),0             MUST SPECIFY DATE                            
         BE    DITTO40                                                          
*                                                                               
DITTO30  ZICM  R5,0(R3),2          FIELD OFFSET FROM BEGINNING OF LINE          
         AR    R2,R5                                                            
         CLI   5(R2),0                                                          
         BNE   *+8                                                              
         BAS   RE,COPYFLD                                                       
         SR    R2,R5               POINT R2 BACK TO START OF LINE               
         AHI   R3,2                                                             
         CLI   0(R3),X'FF'                                                      
         BNE   DITTO30                                                          
*                                                                               
DITTO40  DS    0H                                                               
         L     RE,OFFRDISP                                                      
         AR    R2,RE                                                            
         BCT   R4,DITTO20                                                       
*                                                                               
         CLI   FMGTYPE,FMLBOFFQ    LATE RUN WITH BONUS                          
         BNE   DITTOX                                                           
         LA    RE,MLBBTAB          PROCESS BONUS PORTION OF SCREEN              
         CR    R3,RE                                                            
         BH    DITTOX              WE DID IT ALREADY, EXIT                      
         LR    R3,RE                                                            
         B     DITTO10                                                          
*                                                                               
DITTOX   DS    0H                                                               
         B     EXXMOD                                                           
*                                                                               
MGMTAB   DS    0CL2                                                             
         DC    AL2(MGSODT2H-MGSODTSH)      LINE OFFSET                          
         DC    AL2(MGSODT2H-TWAD)          STARTING LINE TO PROCESS             
         DC    AL2(3)                      NUMBER OF LINES TO PROCESS           
         DC    AL2(MGSOSPTH-MGSODTSH)                                           
         DC    AL2(MGSORATH-MGSODTSH)                                           
         DC    AL2(MGSOLENH-MGSODTSH)                                           
         DC    AL2(MGSODYSH-MGSODTSH)                                           
         DC    AL2(MGSOTMSH-MGSODTSH)                                           
         DC    X'FF'                                                            
*                                                                               
RNATAB   DS    0CL2                                                             
         DC    AL2(RNAODT2H-RNAODTSH)      LINE OFFSET                          
         DC    AL2(RNAODT2H-TWAD)          STARTING LINE TO PROCESS             
         DC    AL2(3)                      NUMBER OF LINES TO PROCESS           
         DC    AL2(RNAOSPTH-RNAODTSH)                                           
         DC    AL2(RNAORATH-RNAODTSH)                                           
         DC    AL2(RNAOLENH-RNAODTSH)                                           
         DC    AL2(RNAODYSH-RNAODTSH)                                           
         DC    AL2(RNAOTMSH-RNAODTSH)                                           
         DC    X'FF'                                                            
*                                                                               
MGPTAB   DS    0CL2                                                             
         DC    AL2(MGPODT2H-MGPODTSH)      LINE OFFSET                          
         DC    AL2(MGPODT2H-TWAD)          STARTING LINE TO PROCESS             
         DC    AL2(6)                      NUMBER OF LINES TO PROCESS           
         DC    AL2(MGPOSPTH-MGPODTSH)                                           
         DC    AL2(MGPOLN#H-MGPODTSH)                                           
         DC    X'FF'                                                            
*                                                                               
MGBTAB   DS    0CL2                                                             
         DC    AL2(MGBODT2H-MGBODTSH)      LINE OFFSET                          
         DC    AL2(MGBODT2H-TWAD)          STARTING LINE TO PROCESS             
         DC    AL2(6)                      NUMBER OF LINES TO PROCESS           
         DC    AL2(MGBOSPTH-MGBODTSH)                                           
         DC    AL2(MGBORATH-MGBODTSH)                                           
         DC    AL2(MGBOLENH-MGBODTSH)                                           
         DC    AL2(MGBODYSH-MGBODTSH)                                           
         DC    AL2(MGBOTMSH-MGBODTSH)                                           
         DC    X'FF'                                                            
*                                                                               
MLRTAB   DS    0CL2                                                             
         DC    AL2(MLRODT2H-MLRODTSH)      LINE OFFSET                          
         DC    AL2(MLRODT2H-TWAD)          STARTING LINE TO PROCESS             
         DC    AL2(6)                      NUMBER OF LINES TO PROCESS           
         DC    AL2(MLROSPTH-MLRODTSH)                                           
         DC    AL2(MLRORATH-MLRODTSH)                                           
         DC    AL2(MLROLN#H-MLRODTSH)                                           
         DC    AL2(MLROTMSH-MLRODTSH)                                           
         DC    X'FF'                                                            
*                                                                               
MLBTAB   DS    0CL2                                                             
         DC    AL2(MLBMDT2H-MLBMDTSH)      LINE OFFSET                          
         DC    AL2(MLBMDT2H-TWAD)          STARTING LINE TO PROCESS             
         DC    AL2(2)                      NUMBER OF LINES TO PROCESS           
         DC    AL2(MLBMSPTH-MLBMDTSH)                                           
         DC    AL2(MLBMRATH-MLBMDTSH)                                           
         DC    AL2(MLBMLN#H-MLBMDTSH)                                           
         DC    AL2(MLBMTMSH-MLBMDTSH)                                           
         DC    X'FF'                                                            
*                                                                               
MLBBTAB  DS    0CL2                                                             
         DC    AL2(MLBBDT2H-MLBBDTSH)      LINE OFFSET                          
         DC    AL2(MLBBDT2H-TWAD)          STARTING LINE TO PROCESS             
         DC    AL2(2)                      NUMBER OF LINES TO PROCESS           
         DC    AL2(MLBBSPTH-MLBBDTSH)                                           
         DC    AL2(MLBBRATH-MLBBDTSH)                                           
         DC    AL2(MLBBLENH-MLBBDTSH)                                           
         DC    AL2(MLBBDYSH-MLBBDTSH)                                           
         DC    AL2(MLBBTMSH-MLBBDTSH)                                           
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
* R2 IS POINTING TO CURRENT FIELD. THIS ROUTINE WILL COPY                       
* FROM SAME FIELD FROM PREVIOUS LINE IF CURRENT FIELD IS BLANK                  
* OFFRDISP HAS OFFSET TO PREVIOUS LINE                                          
*                                                                               
COPYFLD  NTR1                                                                   
         LR    R3,R2                                                            
         L     RE,OFFRDISP         BACKUP TO PREVIOUS LINE                      
         SR    R3,RE                                                            
         ZIC   R1,5(R3)                                                         
         LTR   R1,R1                                                            
         BZ    COPYFLDX                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),8(R3)                                                    
*                                                                               
         MVC   4(1,R2),4(R3)       COPY INPUT INDICATORS                        
         MVC   5(1,R2),5(R3)       COPY FIELD LENGTH                            
*                                                                               
         OI    6(R2),X'80'         XMIT                                         
*                                                                               
COPYFLDX DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
VALIMKG  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 =A(CHECKACT),RR=RELO                                             
*                                                                               
* INITIALIZE TSAR BUFFER                                                        
*                                                                               
         GOTO1 =A(INITTSAR),RR=RELO                                             
*                                                                               
         TM    FLAGS,FACTMGXQ      ACTION IS MAKEGOOD DELETE                    
         BZ    VMKG50                                                           
*                                                                               
         GOTO1 =A(DELMGMF),RR=RELO                                              
*                                                                               
         GOTO1 =A(MKGCAN),RR=RELO                                               
*                                                                               
         GOTO1 =A(UPDTCON),RR=RELO                                              
*                                                                               
         B     VMKGX                                                            
*                                                                               
VMKG50   DS    0H                                                               
         CLI   FMGTYPE,FMGPOFFQ    PREEMPT/CREDIT                               
         BE    VALMGP                                                           
         CLI   FMGTYPE,FMGBOFFQ    BONUS                                        
         BE    VALMGB                                                           
         CLI   FMGTYPE,FMLROFFQ    LATE RUN                                     
         BE    VALMLR                                                           
         CLI   FMGTYPE,FMLBOFFQ    LATE RUN WITH BONUS                          
         BE    VALMLB                                                           
         CLI   FMGTYPE,FRNAOFFQ    REPLACEMENT FOR NA                           
         BE    VALRNA                                                           
         B     VALMGM              DEFAULT IS REGULAR MAKEGOOD OFFER            
*                                                                               
**********************************************************************          
* REGULAR MAKEGOOD OFFERS                                                       
* BONUS OFFERS                                                                  
* REPLACEMENT FOR NA                                                            
**********************************************************************          
VALMGM   DS    0H                                                               
VALMGB   DS    0H                                                               
VALRNA   DS    0H                                                               
*                                                                               
* FILL TSAR BUFFER WITH OFFER RECORDS, ORIGINAL TARGET BUYS AND NEW             
* TARGET BUYS                                                                   
*                                                                               
         GOTO1 =A(MGMGR),RR=RELO                                                
*                                                                               
* VALIDATE MAKEGOOD HEADER INFO                                                 
*                                                                               
         GOTO1 =A(MGMVHDR),RR=RELO                                              
*                                                                               
* VALIDATE OFFER LINES AND UPDATE TARGET OFFERS IN TSAR BUFFER                  
*                                                                               
         NI    FLAGS,X'FF'-FACTDELQ                                             
         GOTO1 =A(MGMVOFF),RR=RELO                                              
         TM    FLAGS,FACTDELQ      AT LEAST ONE LINE WAS DELETED, NEED          
         BZ    VALMG10             TO RENUMBER THE DETAIL OFFER RECS            
         GOTO1 =A(RENUMBER),RR=RELO                                             
*                                                                               
VALMG10  DS    0H                                                               
         CLI   FMGTYPE,FMGBOFFQ                                                 
         BE    VMKG100                                                          
*                                                                               
* VALIDATE MISS DATES AND UPDATE TARGET BUYS IN TSAR BUFFER                     
*                                                                               
         GOTO1 =A(MGMVMISS),RR=RELO                                             
*                                                                               
         GOTO1 =A(MGMUPOFF),RR=RELO                                             
*                                                                               
* UPDATE OFFER LINES WITH X'05' MISSED DATES FROM TARGET BUYS                   
* ALSO ADD X'04' MG= COMMENT ELEMENT                                            
*                                                                               
         CLI   FMGTYPE,FRNAOFFQ    REPLACEMENT NA?                              
         BNE   VALMG20                                                          
         CLC   =C'NONE',RNAODTS    CREDIT FOR NA KEYWORD                        
         BE    VALMGP20                                                         
*                                                                               
* UPDATE TARGET BUY RECORDS WITH X'66' REFERENECE ELEMENTS TO OFFERS            
*                                                                               
VALMG20  DS    0H                                                               
         GOTO1 =A(MGM66ELM),RR=RELO                                             
         B     VMKG100                                                          
         EJECT                                                                  
**********************************************************************          
* PREEMPT/CREDIT MAKEGOOD OFFERS                                                
**********************************************************************          
VALMGP   DS    0H                                                               
*                                                                               
* FILL TSAR BUFFER WITH OFFER RECORDS, ORIGINAL TARGET BUYS AND NEW             
* TARGET BUYS                                                                   
*                                                                               
         GOTO1 =A(MGPGR),RR=RELO                                                
*                                                                               
* VALIDATE MAKEGOOD HEADER INFO                                                 
*                                                                               
         GOTO1 =A(MGMVHDR),RR=RELO                                              
*                                                                               
* VALIDATE MISS DATES, UPDATE TARGET BUYS IN TSAR BUFFER                        
*                                                                               
         GOTO1 =A(MGPVMISS),RR=RELO                                             
*                                                                               
* ADD/UPDATE TARGET OFFERS IN TSAR BUFFER                                       
*                                                                               
         NI    FLAGS,X'FF'-FACTDELQ                                             
         GOTO1 =A(MGPVOFF),RR=RELO                                              
         TM    FLAGS,FACTDELQ      AT LEAST ONE LINE WAS DELETED, NEED          
         BZ    VALMGP10            TO RENUMBER THE DETAIL OFFER RECS            
         GOTO1 =A(RENUMBER),RR=RELO                                             
*                                                                               
VALMGP10 DS    0H                                                               
*                                                                               
* UPDATE OFFER LINES WITH X'05' MISSED DATES FROM TARGET BUYS                   
*                                                                               
         GOTO1 =A(MGPUPOFF),RR=RELO                                             
*                                                                               
* UPDATE TARGET BUY RECORDS WITH X'66' REFERENECE ELEMENTS TO OFFERS            
*                                                                               
VALMGP20 DS    0H                                                               
         GOTO1 =A(MGP66ELM),RR=RELO                                             
         B     VMKG100                                                          
         EJECT                                                                  
**********************************************************************          
* LATE RUN OFFERS                                                               
**********************************************************************          
VALMLR   DS    0H                                                               
*                                                                               
* FILL TSAR BUFFER WITH OFFER RECORDS, ORIGINAL TARGET BUYS AND NEW             
* TARGET BUYS                                                                   
*                                                                               
         GOTO1 =A(MGPGR),RR=RELO                                                
*                                                                               
* VALIDATE MAKEGOOD HEADER INFO                                                 
*                                                                               
         GOTO1 =A(MGMVHDR),RR=RELO                                              
*                                                                               
* VALIDATE MISS DATES, UPDATE TARGET BUYS IN TSAR BUFFER                        
*                                                                               
         GOTO1 =A(MGPVMISS),RR=RELO                                             
*                                                                               
* ADD/UPDATE TARGET OFFERS IN TSAR BUFFER                                       
*                                                                               
         NI    FLAGS,X'FF'-FACTDELQ                                             
         GOTO1 =A(MGMVOFF),RR=RELO                                              
         TM    FLAGS,FACTDELQ      AT LEAST ONE LINE WAS DELETED, NEED          
         BZ    VALMLR10            TO RENUMBER THE DETAIL OFFER RECS            
         GOTO1 =A(RENUMBER),RR=RELO                                             
*                                                                               
VALMLR10 DS    0H                                                               
*                                                                               
* UPDATE OFFER LINES WITH X'05' MISSED DATES FROM TARGET BUYS                   
*                                                                               
         GOTO1 =A(MGPUPOFF),RR=RELO                                             
*                                                                               
* UPDATE TARGET BUY RECORDS WITH X'66' REFERENECE ELEMENTS TO OFFERS            
*                                                                               
         GOTO1 =A(MGP66ELM),RR=RELO                                             
         B     VMKG100                                                          
         EJECT                                                                  
**********************************************************************          
* MAKEGOOD LATE RUN WITH BONUS OFFERS                                           
**********************************************************************          
VALMLB   DS    0H                                                               
*                                                                               
* FILL TSAR BUFFER WITH ORIGINAL AND NEW TARGET BUY RECORDS                     
*                                                                               
         GOTO1 =A(MGPGR),RR=RELO                                                
*                                                                               
* VALIDATE MAKEGOOD HEADER INFO                                                 
*                                                                               
         GOTO1 =A(MGMVHDR),RR=RELO                                              
*                                                                               
* VALIDATE MISS DATES, UPDATE TARGET BUYS IN TSAR BUFFER                        
*                                                                               
         GOTO1 =A(MGPVMISS),RR=RELO                                             
*                                                                               
* ADD/UPDATE TARGET OFFERS IN TSAR BUFFER                                       
*                                                                               
* LATE RUN VALIDATION                                                           
*                                                                               
         LR    R4,RA                                                            
         AHI   R4,TWAWORKQ                                                      
         USING TWAWORK,R4                                                       
         MVI   TWASTRT#,1          LATE RUN ALWAYS STARTS AT LINE 1             
         DROP  R4                                                               
*                                                                               
         NI    FLAGS,X'FF'-FACTBONQ                                             
         NI    FLAGS,X'FF'-FACTDELQ                                             
         GOTO1 =A(MGMVOFF),RR=RELO                                              
         TM    FLAGS,FACTDELQ      AT LEAST ONE LINE WAS DELETED, NEED          
         BZ    VALMLB10            TO RENUMBER THE DETAIL OFFER RECS            
         GOTO1 =A(RENUMBER),RR=RELO                                             
*                                                                               
* LATE RUN BONUS VALIDATION                                                     
*                                                                               
VALMLB10 DS    0H                                                               
         LR    R4,RA                                                            
         AHI   R4,TWAWORKQ                                                      
         USING TWAWORK,R4                                                       
         MVI   TWASTRT#,4          LATE RUN BONUS STARTS AT LINE 4              
         DROP  R4                                                               
*                                                                               
         OI    FLAGS,FACTBONQ      LATE RUN BONUS PROCESSING                    
         NI    FLAGS,X'FF'-FACTDELQ                                             
         GOTO1 =A(MGMVOFF),RR=RELO                                              
         TM    FLAGS,FACTDELQ      AT LEAST ONE LINE WAS DELETED, NEED          
         BZ    VALMLB20            TO RENUMBER THE DETAIL OFFER RECS            
         GOTO1 =A(RENUMBER),RR=RELO                                             
*                                                                               
VALMLB20 DS    0H                                                               
         LR    R4,RA                                                            
         AHI   R4,TWAWORKQ                                                      
         USING TWAWORK,R4                                                       
         MVI   TWASTRT#,1          RESET TO START OF FIRST RECORD               
         DROP  R4                                                               
*                                                                               
         NI    FLAGS,X'FF'-FACTBONQ                                             
*                                                                               
* UPDATE OFFER LINES WITH X'05' MISSED DATES FROM TARGET BUYS                   
* ALSO ADD X'04' MG= COMMENT ELEMENT                                            
*                                                                               
         GOTO1 =A(MGPUPOFF),RR=RELO                                             
*                                                                               
* UPDATE TARGET BUY RECORDS WITH X'66' REFERENECE ELEMENTS TO OFFERS            
*                                                                               
         GOTO1 =A(MGP66ELM),RR=RELO                                             
*                                                                               
* FLAG RECORDS ALL OR CHOICE                                                    
*                                                                               
VMKG100  DS    0H                                                               
         GOTO1 =A(MRKCHOYS),RR=RELO                                             
*                                                                               
* DUMP RECORDS FROM TSAR TO FILE                                                
*                                                                               
         GOTO1 =A(DUMPTSAR),RR=RELO                                             
*                                                                               
* UPDATE CONTRACT WITH MAKEGOOD WORK IN PROGRESS                                
*                                                                               
         GOTO1 =A(UPDTCON),RR=RELO                                              
*                                                                               
VMKGX    DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* CHECK IF ACTION VALID                                                         
*                                                                               
CHECKACT NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    FLAGS,FACTADDQ                                                   
         BZ    CACT0010            FOR ACTION ADD                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'                                                     
         BRAS  RE,GETEL                                                         
         BNE   CACT0010                                                         
         USING RCONDREL,R6                                                      
         TM    RCONDRF2,X'80'+X'10'                                             
         BZ    CACT0010                                                         
         LA    R2,CONCACTH                                                      
         LA    R3,692              INVALID FOR DARE CORPORATE ORDERS            
         B     ERROR                                                            
         DROP  R6                                                               
*                                                                               
CACT0010 DS    0H                                                               
         TM    FLAGS,FACTCHAQ+FACTMGXQ                                          
         BZ    CACTX               FOR ACTION CHANGE/DELETE ONLY                
*                                                                               
CACT0020 DS    0H                                                               
         LR    R4,RA                                                            
         AHI   R4,TWAWORKQ                                                      
         USING TWAWORK,R4                                                       
         LA    R2,CONCACTH                                                      
         LA    R3,576              NO D/A, REC NOT FOUND                        
         OC    TWAMKGDH,TWAMKGDH                                                
         BZ    ERROR                                                            
         MVC   KEY+28(4),TWAMKGDH                                               
         DROP  R4                                                               
*                                                                               
         GOTO1 VGETREC,DMCB,RMKGREC                                             
*                                                                               
* FOR DARE MG, ONLY ACTION LEFT IS MGX IF MAKEGOOD OFFER HAS BEEN               
* CANCELLED TO AGENCY                                                           
*                                                                               
         TM    FLAGS,FACTCHAQ                                                   
         BZ    CACT0030            FOR ACTION CHANGE ONLY                       
*                                                                               
         LA    R3,582              MG IS CANCELLED, MUST DO MGX                 
         TM    RMKGSFG1,RMGF1MCN   DARE MAKEGOOD WAS CANCELLED                  
         BO    ERROR                                                            
*                                                                               
* FOR DARE MG, OFFERER CANNOT REVISE A MAKEGOOD                                 
* THAT HAS BEEN SENT/RESENT TO THE AGENCY BY THE REP                            
*                                                                               
CACT0030 DS    0H                                                               
         LA    R3,618              MG IS SENT, MUST RECALL                      
         TM    RMKGSFG1,RMGF1MSN+RMGF1MCR+RMGF1MAR                              
         BNZ   ERROR                                                            
*                                                                               
         CLI   TWAACCS,C'$'        ARE I A REP?                                 
         BE    CACT0050                                                         
*                                                                               
* YES, CHECK IF ORDER IS IN-PROGRESS                                            
*                                                                               
         LA    R3,628              WIP ON STATION SIDE                          
         TM    RMKGSFG2,RMGF2STQ+RMGF2WPQ                                       
         BO    ERROR               ON THE STATION SIDE                          
         B     CACT0060                                                         
*                                                                               
* CHECK IF ORDER IS IN-PROGRESS                                                 
*                                                                               
CACT0050 DS    0H                  I MUST BE A STATION                          
         LA    R3,629              WIP ON REP SIDE                              
         TM    RMKGSFG2,RMGF2RPQ+RMGF2WPQ                                       
         BO    ERROR               ON THE REP SIDE                              
*                                                                               
* PROCESS FOR ACTION MGX                                                        
*                                                                               
CACT0060 DS    0H                                                               
         TM    FLAGS,FACTMGXQ                                                   
         BZ    CACTX               FOR ACTION DELETE ONLY                       
*                                                                               
         LA    R3,497              ONLY OFFERER CAN DELETE                      
         CLI   TWAACCS,C'$'        ARE I A REP?                                 
         BE    CACT0070                                                         
         TM    RMKGSCST,RMKGSCRQ   CREATOR REP OR STATION?                      
         BZ    ERROR                                                            
         B     CACT0080                                                         
*                                                                               
CACT0070 DS    0H                                                               
         TM    RMKGSCST,RMKGSCRQ   CREATOR REP OR STATION?                      
         BO    ERROR                                                            
*                                                                               
CACT0080 DS    0H                                                               
*        LA    R3,646              MUST WAIT 5 MINUTE SINCE                     
*        BAS   RE,CHKTIME          PREVIOUS ACTION TO AGENCY, IF ANY            
*        BNE   ERROR                                                            
*                                                                               
CACTX    DS    0H                                                               
         B     EXXMOD                                                           
*&&DO                                                                           
***********************************************************************         
* CHECK TO MAKE SURE 5 MINUTES HAVE ELAPSED BEFORE NEXT ACTION                  
* THIS WOULD ENSURE THAT EDICT HAD A CHANCE TO PROCESS OUTSTANDING              
* ENTRIES                                                                       
***********************************************************************         
CHKTIME  NTR1                                                                   
         LA    R6,RMKGREC                                                       
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL                                                         
         BNE   CHKTMYES                                                         
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
         BZ    CHKTMYES                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(2,WORK+5)                                     
*                                                                               
         CLC   WORK(2),WORK+5      WAS LAST ACTION TODAY?                       
         BNE   CHKTMYES                                                         
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
         BNL   CHKTMNO                                                          
*                                                                               
CHKTMYES SR    RC,RC                                                            
CHKTMNO  LTR   RC,RC                                                            
         B     EXXMOD                                                           
*&&                                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* INITIALIZE TSAR BUFFER                                                        
*                                                                               
INITTSAR NTR1  BASE=*,LABEL=*                                                   
         L     RE,ACOMFACS         GET ADDRESS OF PERVAL                        
         USING COMFACSD,RE                                                      
         MVC   PERVAL,CPERVAL                                                   
         DROP  RE                                                               
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'D9000A5D',0                                      
         CLI   DMCB+4,X'FF'        ERROR?                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VTSAR,0(R1)                                                      
*                                                                               
         XC    TSARBLK,TSARBLK                                                  
         MVC   TD.TSACOM,ACOMFACS  A(COMFACS)                                   
         MVI   TD.TSPAGL,2         USE TEMPSTR PAGE 2                           
         MVI   TD.TSPAGN,3         USE 3 PAGES                                  
         MVI   TD.TSKEYL,27        KEY LENGTH                                   
         MVI   TD.TSRECI,TSRVAR    SET VARIABLE LENGTH RECORDS                  
         MVC   TD.TSRECL,=Y(1000)  MAX RECORD LENGTH                            
         OI    TD.TSINDS,TSIXTTWA  14K RECORDS                                  
         MVI   TD.TSACTN,TSAINI    SET INITIALIZE TSAR                          
         LA    RF,TSARREC                                                       
         ST    RF,TD.TSAREC                                                     
*                                                                               
         GOTO1 =A(GOTSAR),RR=RELO                                               
         TM    TD.TSERRS,TSEINIF                                                
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     EXXMOD                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
GOTSAR   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLC   TSARREC(2),=AL2(1000)                                            
         BL    GOTSARX                                                          
         LA    R2,CONCACTH                                                      
         LA    R3,339              RECORD FULL - CHANGE NOT PROCESSED           
         B     ERROR                                                            
*                                                                               
GOTSARX  DS 0H                                                                  
         GOTO1 VTSAR,TSARBLK                                                    
         B     EXXMOD                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   MRKCHOYS:  MARK SELECTED RECORD AS INDICATED BY CHOICE FIELD                
*                                                                               
MRKCHOYS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XCEFL TSARREC,1002                                                     
         MVC   TSARREC+2(RMKGKRTY-RMKGKEY),MGDTREC                              
         MVI   TD.TSACTN,TSARDH    LOOP THRU ALL DETAIL RECORDS                 
*                                                                               
         NI    FLAGS,X'FF'-X'80'   NO DETAIL OFFER SELECTED YET                 
*                                                                               
MCHOYS10 DS    0H                                                               
         GOTO1 =A(GOTSAR),RR=RELO                                               
         TM    TD.TSERRS,TSEEOF                                                 
         BNZ   MCHOYS60                                                         
*                                                                               
         GOTO1 VMOVEREC,DMCB,TSARREC+2,RMKGREC                                  
*                                                                               
         MVI   LINECTR,0                                                        
         MVN   LINECTR,RMKGKRTY                                                 
*                                                                               
MCHOYS30 DS    0H                                                               
         LA    R6,RMKGREC                                                       
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         USING RMKGSTEL,R6                                                      
         TM    RMKGSTCH,X'01'      RECORD PREVIOUSLY SELECTED?                  
         BO    MCHOYS40                                                         
         ZIC   RF,CHOYSCTR         NO, IS THIS REC CURRENTLY SELECTED?          
         CLC   CHOYSCTR,LINECTR                                                 
         BNE   MCHOYS50                                                         
         OI    RMKGSTCH,X'01'      YES, MARK RECORD AND WRITE IT BACK           
         OI    FLAGS,X'80'         RECORD SELECTED                              
         B     MCHOYS45                                                         
*                                                                               
MCHOYS40 DS    0H                  RECORD PREVIOUSLY SELECTED?                  
         CLC   CHOYSCTR,LINECTR    YES, IS THIS REC CURRENTLY SELECTED?         
         BE    MCHOYS50            YES, SKIP WRITE BACK                         
         NI    RMKGSTCH,X'FF'-X'01' NO, REMOVE MARK AND WRITE BACK              
*                                                                               
MCHOYS45 DS    0H                                                               
         GOTO1 VMOVEREC,DMCB,RMKGREC,TSARREC+2                                  
         SR    R1,R1                                                            
         ICM   R1,3,RMKGLEN        ADD LENGTH FIELD                             
         AHI   R1,2                                                             
         STCM  R1,3,TSARREC                                                     
*                                                                               
         MVI   TD.TSACTN,TSAPUT    PUT RECORD BACK BY NUMBER                    
         GOTO1 =A(GOTSAR),RR=RELO                                               
         TM    TD.TSERRS,TSEEOF+TSERNF                                          
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
MCHOYS50 DS    0H                                                               
         XCEFL TSARREC,1002                                                     
         MVI   TD.TSACTN,TSANXT                                                 
         B     MCHOYS10                                                         
*                                                                               
MCHOYS60 DS    0H                                                               
         TM    MGHMOPTH+1,X'0C'    FIELD IS HIDDEN, DON'T BOTHER                
         BO    MCHOYSX                                                          
         CLI   MGHMOPT,C'A'        OFFERING ALL/CHOICE?                         
         BE    MCHOYSX                                                          
         CLI   MGHMOPT,C'C'                                                     
         BE    MCHOYS70                                                         
         LA    R3,CHOYSERR         ELSE MUST SELECT VALID OFFER LINE            
         TM    FLAGS,X'80'                                                      
         BZ    ERROR                                                            
*                                                                               
* KEY CHANGED, OFFER IS DEFAULT AS ALL BUT USER WANTS CHOICE                    
* NEED TO CHANGE ALL KEYS TO CHOICE                                             
*                                                                               
MCHOYS70 DS    0H                                                               
         XCEFL TSARREC,1002                                                     
         MVC   TSARREC+2(RMKGKRTY-RMKGKEY),MGDTREC                              
         MVI   TD.TSACTN,TSARDH    LOOP THRU ALL DETAIL RECORDS                 
         GOTO1 =A(GOTSAR),RR=RELO                                               
         TM    TD.TSERRS,TSEEOF                                                 
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
MCHOYS80 DS    0H                                                               
MGRECD   USING RMKGREC,TSARREC+2   SKIP CHOICE RECORDS                          
         TM    MGRECD.RMKGKRTY,X'10'                                            
         BZ    MCHOYS90            ALL DONE                                     
         DROP  MGRECD                                                           
*                                                                               
         XCEFL TSARREC,1002                                                     
         MVI   TD.TSACTN,TSANXT                                                 
         GOTO1 =A(GOTSAR),RR=RELO                                               
         TM    TD.TSERRS,TSEEOF+TSERNF                                          
         BZ    MCHOYS80                                                         
         B     MCHOYSX                                                          
*                                                                               
MCHOYS90 DS    0H                                                               
         GOTO1 VMOVEREC,DMCB,TSARREC+2,RMKGREC                                  
         OI    RMKGKRTY,X'10'                                                   
*                                                                               
         MVI   TD.TSACTN,TSADEL    DELETE 'ALL' OFFER                           
         GOTO1 =A(GOTSAR),RR=RELO                                               
         TM    TD.TSERRS,TSEEOF+TSERNF                                          
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 VMOVEREC,DMCB,RMKGREC,TSARREC+2                                  
         SR    R1,R1               ADD RECORD BACK AS CHOICE                    
         ICM   R1,3,RMKGLEN        ADD LENGTH FIELD                             
         AHI   R1,2                                                             
         STCM  R1,3,TSARREC                                                     
*                                                                               
         MVI   TD.TSACTN,TSAADD    ADD RECORD                                   
         GOTO1 =A(GOTSAR),RR=RELO                                               
         TM    TD.TSERRS,TSEEOF+TSEDUP                                          
         BZ    MCHOYS70                                                         
         DC    H'0'                                                             
*                                                                               
MCHOYSX  DS    0H                                                               
         XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* FILL TSAR BUFFER WITH TARGET AND OFFER RECORDS:                               
*                                                                               
* - MAKEGOOD OFFER RECORDS (REMOVE ALL X'05' ELEMENTS IN DETAIL RECS)           
* - TARGET BUY RECORDS BEFORE CHANGES/DELETIONS                                 
* - NEW TARGET BUY RECORDS                                                      
*   (REMOVE CORRESPONDING X'66' ELEMENTS IN BOTH CURRENT AND NEW TARGET         
*   BUYS)                                                                       
MGMGR    NTR1  BASE=*,LABEL=*                                                   
         LR    R4,RA                                                            
         AHI   R4,TWAWORKQ                                                      
         USING TWAWORK,R4                                                       
*                                                                               
         TM    FLAGS,FACTADDQ      ADD OR CHA?                                  
         BO    MGMGR10                                                          
*        LA    R2,MGHMOFFH                                                      
*        GOTO1 VPACK                                                            
*        STC   R0,OFFERNUM         MAKEGOOD OFFER NUMBER                        
         MVI   OFFERNUM,1                                                       
*                                                                               
         LA    R2,MGSOFL1H         POINT TO DETAIL RECORD NUMBER                
         CLI   FMGTYPE,FMGMOFFQ                                                 
         BE    MGMGR20                                                          
         LA    R2,RNAOFL1H         POINT TO DETAIL RECORD NUMBER                
         CLI   FMGTYPE,FRNAOFFQ                                                 
         BE    MGMGR20                                                          
         LA    R2,MGBOFL1H                                                      
         B     MGMGR20                                                          
*                                                                               
* READ MAKEGOOD OFFER RECORDS TO TSAR BUFFER                                    
*                                                                               
* NOTE: NEW MAKEGOOD OFFER SETS RMKGKMLN TO ZERO. THIS IS NO LONGER             
* NEEDED AS WE MOVE TO MULTI-MISSED LINE MAKEGOOD OFFERS. WE WILL BE            
* USING THE X'05' MISSED ELEMENT TO REFERENCE MISSED BUYS INSTEAD.              
* OLD MAKEGOOD OFFERS WILL BE CONVERTED AS THEY ARE PASSED THROUGH THIS         
* MODULE.                                                                       
*                                                                               
MGMGR10  DS    0H                                                               
         OC    TWAMKGDH,TWAMKGDH   BRAND NEW OFFER WILL NOT HAVE                
         BZ    MGMGR120            HEADER RECORD ON FILE                        
*                                                                               
MGMGR20  DS    0H                                                               
         MVC   KEY+28(4),TWAMKGDH                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,RMKGREC                                             
*                                                                               
         MVI   CREATED,BYREP                                                    
         TM    RMKGSCST,RMKGSCRQ                                                
         BO    *+8                                                              
         MVI   CREATED,BYSTA                                                    
*                                                                               
         MVC   KEY(27),RMKGKEY                                                  
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    MGMGR40                                                          
         DC    H'0'                                                             
*                                                                               
MGMGR30  DS    0H                                                               
**************************                                                      
* TEMP BONUS CODING                                                             
*                                                                               
         MVI   TEMPFLG,0                                                        
         OC    RMKGKPLN,RMKGKPLN                                                
         BNZ   MGMGR30A                                                         
         CLI   RMKGKLIN,0                                                       
         BE    MGMGR30A                                                         
         MVI   TEMPFLG,C'Y'        YES, OLD STYLE MAKEGOOD IN USE               
MGMGR30A DS    0H                                                               
**************************                                                      
*                                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,RMKGREC                                             
*                                                                               
         TM    FLAGS,FACTMGXQ      SKIP IF MGX                                  
         BO    MGMGR40                                                          
*                                                                               
         NI    RMKGKRTY,X'FF'-X'10' WE WILL SET ALL/CHOICE LATER                
*                                                                               
*                                  REMOVE X'04' MG= ELEMENT                     
         GOTO1 VDELELEM,DMCB,(X'04',RMKGREC)                                    
*                                  REMOVE MAKEGOOD REFERENCE ELEMENT            
         GOTO1 VDELELEM,DMCB,(X'05',RMKGREC)                                    
*                                                                               
MGMGR40  DS    0H                                                               
         MVI   TD.TSACTN,TSAADD    ADD RECORD TO TSAR BUFFER                    
         MVI   RMKGKMLN,0                                                       
         GOTO1 VMOVEREC,DMCB,RMKGREC,TSARREC+2                                  
         SR    R1,R1                                                            
         ICM   R1,3,RMKGLEN        ADD LENGTH FIELD                             
         AHI   R1,2                                                             
         STCM  R1,3,TSARREC                                                     
*                                                                               
         GOTO1 =A(GOTSAR),RR=RELO                                               
         TM    TD.TSERRS,TSEEOF+TSEDUP                                          
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    FLAGS,FACTADDQ      ADD OR CHA?                                  
         BO    MGMGR120                                                         
*                                                                               
MGMGR50  DS    0H                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VSEQ                                                             
         CLC   KEY(RMKGKPLN-RMKGKEY),KEYSAVE                                    
         BNE   MGMGR60                                                          
         CLC   KEY+RMKGKLIN-RMKGKEY(1),OFFERNUM                                 
         BE    MGMGR30                                                          
         B     MGMGR50                                                          
*                                                                               
* READ ORIGINAL TARGET BUYS TO TSAR BUFFER                                      
*                                                                               
MGMGR60  DS    0H                                                               
         CLI   FMGTYPE,FMGBOFFQ    NO TARGET FOR BONUS                          
         BE    MGMGRX                                                           
         LA    R6,MGDTREC                                                       
         MVI   ELCODE,X'05'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RMKGMGEL,R6                                                      
*                                                                               
MGMGR70  DS    0H                                                               
         XC    KEY,KEY                                                          
BUYD     USING RBUYKEY,KEY                                                      
         MVI   BUYD.RBUYKTYP,X'0B'                                              
         MVC   BUYD.RBUYKREP,REPALPHA                                           
         MVC   BUYD.RBUYKCON,TWACNUM                                            
         MVC   BUYD.RBUYKPLN,=3X'FF'                                            
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
MGMGR80  DS    0H                                                               
         CLC   KEY(RBUYKMLN-RBUYKEY),KEYSAVE                                    
         BE    MGMGR85                                                          
*                                                                               
* FOR ACTION MGX, IT IS POSSIBLE THAT THE TARGET BUY IS MISSING                 
* FROM DARE REVISIONS                                                           
*                                                                               
         TM    FLAGS,FACTMGXQ                                                   
         BO    MGMGR110                                                         
         DC    H'0'                                                             
*                                                                               
MGMGR85  DS    0H                                                               
         CLC   BUYD.RBUYKLIN,RMKGMGLI                                           
         BE    MGMGR90                                                          
         GOTO1 VSEQ                                                             
         B     MGMGR80                                                          
         DROP  BUYD                                                             
*                                                                               
MGMGR90  DS    0H                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,RBUYREC                                             
*                                                                               
         TM    FLAGS,FACTADDQ      ADD OR CHA?                                  
         BO    MGMGR100                                                         
         GOTO1 =A(DELEL66),RR=RELO DELETE X'66' ELEM FOR THIS GRP/OFFER         
*                                                                               
MGMGR100 DS    0H                                                               
         MVI   TD.TSACTN,TSAADD    ADD RECORD TO TSAR BUFFER                    
         GOTO1 VMOVEREC,DMCB,RBUYREC,TSARREC+2                                  
         SR    R1,R1                                                            
         ICM   R1,3,RBUYLEN        ADD LENGTH FIELD                             
         AHI   R1,2                                                             
         STCM  R1,3,TSARREC                                                     
*                                                                               
         GOTO1 =A(GOTSAR),RR=RELO                                               
         TM    TD.TSERRS,TSEEOF+TSEDUP                                          
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
MGMGR110 DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   MGMGR120                                                         
         CLC   RBUYKLIN,RMKGMGLI-RMKGMGEL(R6)                                   
         BE    MGMGR110            SKIP IF SAME TARGET                          
         B     MGMGR70                                                          
*                                                                               
* READ ANY NEW TARGET BUYS TO TSAR BUFFER                                       
*                                                                               
MGMGR120 DS    0H                                                               
         TM    FLAGS,FACTMGXQ      MGX, NO NEW TARGETS TO RETREIVE              
         BO    MGMGRX                                                           
         CLI   FMGTYPE,FMGBOFFQ    NO TARGET FOR BONUS                          
         BE    MGMGRX                                                           
         LA    R3,2                INVALID INPUT                                
         LA    R2,MGSLN1H                                                       
*                                                                               
MGMGR130 DS    0H                                                               
         CLI   5(R2),0                                                          
         BE    MGMGR160                                                         
         GOTO1 VPACK                                                            
         LTR   R0,R0                                                            
         BZ    ERROR                                                            
         STC   R0,LINECTR                                                       
*                                                                               
         XC    KEY,KEY                                                          
BUYD     USING RBUYKEY,KEY                                                      
         MVI   BUYD.RBUYKTYP,X'0B'                                              
         MVC   BUYD.RBUYKREP,REPALPHA                                           
         MVC   BUYD.RBUYKCON,TWACNUM                                            
         MVC   BUYD.RBUYKPLN,=3X'FF'                                            
         DROP  R4                                                               
*                                                                               
         GOTO1 VHIGH                                                            
         LA    R3,90               BUY NOT FOUND                                
*                                                                               
MGMGR140 DS    0H                                                               
         CLC   KEY(RBUYKMLN-RBUYKEY),KEYSAVE                                    
         BNE   ERROR                                                            
         CLC   BUYD.RBUYKLIN,LINECTR                                            
         BE    MGMGR150                                                         
         GOTO1 VSEQ                                                             
         B     MGMGR140                                                         
         DROP  BUYD                                                             
*                                                                               
MGMGR150 DS    0H                                                               
         XCEFL TSARREC,1002                                                     
         MVC   TSARREC+2(27),KEY                                                
         MVI   TD.TSACTN,TSARDH    CHECK IF RECORD ALREADY IN BUFFER            
         GOTO1 =A(GOTSAR),RR=RELO                                               
         TM    TD.TSERRS,TSEEOF+TSERNF                                          
         BZ    MGMGR160                                                         
*                                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,RBUYREC                                             
*                                                                               
         TM    FLAGS,FACTADDQ      ADD OR CHA?                                  
         BO    MGMGR155                                                         
         GOTO1 =A(DELEL66),RR=RELO DELETE X'66' ELEM FOR THIS GRP/OFFER         
*                                                                               
MGMGR155 DS    0H                                                               
         MVI   TD.TSACTN,TSAADD    ADD RECORD TO TSAR BUFFER                    
         GOTO1 VMOVEREC,DMCB,RBUYREC,TSARREC+2                                  
         SR    R1,R1                                                            
         ICM   R1,3,RBUYLEN        ADD LENGTH FIELD                             
         AHI   R1,2                                                             
         STCM  R1,3,TSARREC                                                     
*                                                                               
         GOTO1 =A(GOTSAR),RR=RELO                                               
         TM    TD.TSERRS,TSEEOF+TSEDUP                                          
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
MGMGR160 DS    0H                                                               
         LA    R1,MGSLNLH                                                       
*                                                                               
         CR    R2,R1                                                            
         BNL   MGMGRX                                                           
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     MGMGR130                                                         
*                                                                               
MGMGRX   DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* PREEMPT, LATE RUN AND LATE RUN WITH BONUS MAKEGOOD OFFER PROCESSING:          
*                                                                               
* FILL TSAR BUFFER WITH:                                                        
*                                                                               
* - ALL DETAIL OFFER RECORDS FOR THIS OFFER                                     
* - TARGET BUY RECORDS FROM EACH OF THE OFFER RECORDS                           
* - NEW TARGET BUY RECORDS ON SCREEN (IF ANY)                                   
*                                                                               
* REMOVES CORRESPONDING X'66' ELEMENTS IN BUYS SHOWN ON SCREEN!!                
*                                                                               
MGPGR    NTR1  BASE=*,LABEL=*                                                   
         LR    R4,RA                                                            
         AHI   R4,TWAWORKQ                                                      
         USING TWAWORK,R4                                                       
*                                                                               
         TM    FLAGS,FACTADDQ      ADD NEW OFFER?                               
         BO    MGPGR20                                                          
*        LA    R2,MGHMOFFH                                                      
*        GOTO1 VPACK                                                            
*        STC   R0,OFFERNUM         MAKEGOOD OFFER NUMBER                        
         MVI   OFFERNUM,1                                                       
         B     MGPGR30                                                          
*                                                                               
* READ MAKEGOOD OFFER RECORDS TO TSAR BUFFER                                    
*                                                                               
* NOTE: NEW MAKEGOOD OFFER SETS RMKGKMLN TO ZERO. THIS IS NO LONGER             
* NEEDED AS WE MOVE TO MULTI-MISSED LINE MAKEGOOD OFFERS. WE WILL BE            
* USING THE X'05' MISSED ELEMENT TO REFERENCE MISSED BUYS INSTEAD.              
* OLD MAKEGOOD OFFERS WILL BE CONVERTED AS THEY ARE PASSED THROUGH THIS         
* MODULE.                                                                       
*                                                                               
MGPGR20  DS    0H                                                               
         OC    TWAMKGDH,TWAMKGDH   BRAND NEW OFFER WILL NOT HAVE                
         BZ    MGPGR110            HEADER RECORD ON FILE                        
*                                                                               
MGPGR30  DS    0H                                                               
         MVC   KEY+28(4),TWAMKGDH                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,RMKGREC                                             
         MVC   KEY(27),RMKGREC                                                  
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VHIGH               HIGH FOR SEQ LATER                           
         CLC   KEY(27),KEYSAVE                                                  
         BE    MGPGR45                                                          
         DC    H'0'                                                             
*                                                                               
MGPGR40  DS    0H                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,RMKGREC                                             
*                                                                               
         MVC   SVKEY(27),RMKGREC                                                
*                                                                               
         TM    FLAGS,FACTMGXQ      SKIP IF MGX                                  
         BO    *+8                                                              
         NI    RMKGKRTY,X'FF'-X'10' WE WILL SET ALL/CHOICE LATER                
*                                                                               
         LA    R6,RMKGREC                                                       
*                                                                               
         TM    RMKGRTS,X'20'       LATE RUN BONUS                               
         BO    MGPGR43                                                          
*                                                                               
         MVI   ELCODE,X'05'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RMKGMGEL,R6                                                      
         MVC   TARGETBY,RMKGMGLI   SAVE TARGET BUY NUMBER                       
         DROP  R6                                                               
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   *+6                                                              
         DC    H'0'                SHOULD ONLY BE ONE!                          
*                                                                               
MGPGR43  DS    0H                                                               
         TM    FLAGS,FACTMGXQ      SKIP IF MGX                                  
         BO    MGPGR45                                                          
         MVI   OFFREC#,0                                                        
         MVN   OFFREC#,RMKGKRTY                                                 
         CLC   OFFREC#,TWASTRT#    CLEAR ONLY IF SHOWN ON SCREEN                
         BL    MGPGR45                                                          
         CLC   OFFREC#,TWANEXT#                                                 
         BNL   MGPGR45                                                          
*                                  REMOVE X'04' CR= ELEMENT                     
         GOTO1 VDELELEM,DMCB,(X'04',RMKGREC)                                    
*                                  REMOVE MAKEGOOD REFERENCE ELEMENT            
         GOTO1 VDELELEM,DMCB,(X'05',RMKGREC)                                    
*                                                                               
MGPGR45  DS    0H                                                               
         MVI   TD.TSACTN,TSAADD    ADD RECORD TO TSAR BUFFER                    
         MVI   RMKGKMLN,0                                                       
         GOTO1 VMOVEREC,DMCB,RMKGREC,TSARREC+2                                  
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,RMKGLEN        ADD LENGTH FIELD                             
         AHI   R1,2                                                             
         STCM  R1,3,TSARREC                                                     
*                                                                               
         GOTO1 =A(GOTSAR),RR=RELO                                               
         TM    TD.TSERRS,TSEEOF+TSEDUP                                          
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    FLAGS,FACTADDQ      ADD NEW OFFER?                               
         BO    MGPGR110                                                         
*                                                                               
* READ ORIGINAL TARGET BUYS TO TSAR BUFFER                                      
*                                                                               
         LA    R6,TSARREC+2                                                     
         USING RMKGREC,R6                                                       
         OC    RMKGKPLN(6),RMKGKPLN                                             
         BZ    MGPGR100            SKIP FOR HEADER RECORD                       
         DROP  R6                                                               
*                                                                               
         XC    KEY,KEY                                                          
BUYD     USING RBUYKEY,KEY                                                      
         MVI   BUYD.RBUYKTYP,X'0B'                                              
         MVC   BUYD.RBUYKREP,REPALPHA                                           
         MVC   BUYD.RBUYKCON,TWACNUM                                            
         MVC   BUYD.RBUYKPLN,=3X'FF'                                            
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
MGPGR60  DS    0H                                                               
         CLC   KEY(RBUYKMLN-RBUYKEY),KEYSAVE                                    
         BE    MGPGR65                                                          
*                                                                               
* FOR ACTION MGX, IT IS POSSIBLE THAT THE TARGET BUY IS MISSING                 
* FROM DARE REVISIONS                                                           
*                                                                               
         TM    FLAGS,FACTMGXQ                                                   
         BO    MGPGR95                                                          
         DC    H'0'                                                             
*                                                                               
MGPGR65  DS    0H                                                               
         CLC   BUYD.RBUYKLIN,TARGETBY                                           
         BE    MGPGR70                                                          
         GOTO1 VSEQ                                                             
         B     MGPGR60                                                          
         DROP  BUYD                                                             
*                                                                               
* CHECK IF RECORD ALREADY IN TSAR BUFFER                                        
*                                                                               
MGPGR70  DS    0H                                                               
         XCEFL TSARREC,1002                                                     
         MVC   TSARREC+2(27),KEY                                                
         MVI   TD.TSACTN,TSARDH                                                 
         GOTO1 =A(GOTSAR),RR=RELO                                               
         TM    TD.TSERRS,TSEEOF+TSERNF                                          
         BNZ   MGPGR75                                                          
         GOTO1 VMOVEREC,DMCB,TSARREC+2,RBUYREC                                  
         MVI   TD.TSACTN,TSAWRT    WRITE RECORD TO TSAR BUFFER                  
         B     MGPGR80                                                          
*                                                                               
MGPGR75  DS    0H                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,RBUYREC                                             
*                                                                               
         MVI   TD.TSACTN,TSAADD    ADD RECORD TO TSAR BUFFER                    
*                                                                               
MGPGR80  DS    0H                                                               
         TM    FLAGS,FACTADDQ      ADD NEW OFFER?                               
         BO    MGPGR90                                                          
         TM    FLAGS,FACTMGXQ      DEL OFFER?                                   
         BO    MGPGR85                                                          
         CLC   OFFREC#,TWASTRT#    ELSE, DO ONLY IF SHOWN ON SCREEN             
         BL    MGPGR90                                                          
         CLC   OFFREC#,TWANEXT#                                                 
         BNL   MGPGR90                                                          
*                                                                               
MGPGR85  DS    0H                                                               
         GOTO1 =A(DELEL66),RR=RELO DELETE X'66' ELEM FOR THIS GRP/OFFER         
*                                                                               
MGPGR90  DS    0H                  ADD/WRITE TARGET BUY TO TSAR BUFFER          
         GOTO1 VMOVEREC,DMCB,RBUYREC,TSARREC+2                                  
         SR    R1,R1                                                            
         ICM   R1,3,RBUYLEN        ADD LENGTH FIELD                             
         AHI   R1,2                                                             
         STCM  R1,3,TSARREC                                                     
*                                                                               
         GOTO1 =A(GOTSAR),RR=RELO                                               
         TM    TD.TSERRS,TSEEOF                                                 
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
MGPGR95  DS    0H                                                               
         MVC   KEY(27),SVKEY       RESTORE OFFER RECORD                         
         GOTO1 VHIGH                                                            
*                                                                               
MGPGR100 DS    0H                  AND GET NEXT OFFER RECORD TO PROCESS         
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VSEQ                                                             
         CLC   KEY(RMKGKPLN-RMKGKEY),KEYSAVE                                    
         BNE   MGPGR110                                                         
         CLC   KEY+RMKGKLIN-RMKGKEY(1),OFFERNUM                                 
         BE    MGPGR40                                                          
         B     MGPGR100                                                         
*                                                                               
* READ ANY NEW TARGET BUYS TO TSAR BUFFER                                       
*                                                                               
MGPGR110 DS    0H                                                               
         TM    FLAGS,FACTMGXQ      SKIP IF MGX                                  
         BO    MGPGRX                                                           
         LA    R2,MGPOLN#H         PREEMPT                                      
         CLI   FMGTYPE,FMGPOFFQ                                                 
         BE    MGPGR115                                                         
         LA    R2,MLROLN#H         LATE RUN                                     
         CLI   FMGTYPE,FMLROFFQ                                                 
         BE    MGPGR115                                                         
         LA    R2,MLBMLN#H         LATE RUN WITH BONUS                          
         CLI   FMGTYPE,FMLBOFFQ                                                 
         BE    MGPGR115                                                         
         DC    H'0'                                                             
*                                                                               
MGPGR115 DS    0H                                                               
         LA    R3,2                                                             
*                                                                               
MGPGR120 DS    0H                                                               
         CLI   5(R2),0                                                          
         BE    MGPGR160                                                         
         GOTO1 VPACK                                                            
         LTR   R0,R0                                                            
         BZ    ERROR                                                            
         STC   R0,TARGETBY                                                      
*                                                                               
         XC    KEY,KEY                                                          
BUYD     USING RBUYKEY,KEY                                                      
         MVI   BUYD.RBUYKTYP,X'0B'                                              
         MVC   BUYD.RBUYKREP,REPALPHA                                           
         MVC   BUYD.RBUYKCON,TWACNUM                                            
         MVC   BUYD.RBUYKPLN,=3X'FF'                                            
         DROP  R4                                                               
*                                                                               
         GOTO1 VHIGH                                                            
         LA    R3,90               BUY NOT FOUND                                
*                                                                               
MGPGR130 DS    0H                                                               
         CLC   KEY(RBUYKMLN-RBUYKEY),KEYSAVE                                    
         BNE   ERROR                                                            
         CLC   BUYD.RBUYKLIN,TARGETBY                                           
         BE    MGPGR140                                                         
         GOTO1 VSEQ                                                             
         B     MGPGR130                                                         
         DROP  BUYD                                                             
*                                                                               
MGPGR140 DS    0H                                                               
         XCEFL TSARREC,1002                                                     
         MVC   TSARREC+2(27),KEY                                                
         MVI   TD.TSACTN,TSARDH    CHECK IF RECORD ALREADY IN BUFFER            
         GOTO1 =A(GOTSAR),RR=RELO                                               
         TM    TD.TSERRS,TSEEOF+TSERNF                                          
         BZ    MGPGR160                                                         
*                                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,RBUYREC                                             
*                                                                               
MGPGR150 DS    0H                                                               
         MVI   TD.TSACTN,TSAADD    ADD RECORD TO TSAR BUFFER                    
         GOTO1 VMOVEREC,DMCB,RBUYREC,TSARREC+2                                  
         SR    R1,R1                                                            
         ICM   R1,3,RBUYLEN        ADD LENGTH FIELD                             
         AHI   R1,2                                                             
         STCM  R1,3,TSARREC                                                     
*                                                                               
         GOTO1 =A(GOTSAR),RR=RELO                                               
         TM    TD.TSERRS,TSEEOF+TSEDUP                                          
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
MGPGR160 DS    0H                                                               
         LA    R1,MLROLN7H                                                      
         LA    RE,MLROLN2H-MLROLN#H                                             
         CLI   FMGTYPE,FMLROFFQ    LATE RUN?                                    
         BE    MGPGR170                                                         
*                                                                               
         LA    R1,MLBMLN3H                                                      
         LA    RE,MLBMLN2H-MLBMLN#H                                             
         CLI   FMGTYPE,FMLBOFFQ    LATE RUN WITH BONUS?                         
         BE    MGPGR170                                                         
*                                                                               
         LA    R1,MGPOLNLH         DEFAULT PREEMPT                              
         LA    RE,MGPOLN2H-MGPOLN#H                                             
*                                                                               
MGPGR170 DS    0H                                                               
         CR    R2,R1                                                            
         BNL   MGPGRX                                                           
         AR    R2,RE                                                            
         B     MGPGR120                                                         
*                                                                               
MGPGRX   DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DELETE APPROPRIATE X'66' ELEMENTS FROM BUY RECORD                             
* X'66' ELEMENT MUST MATCH CURRENT GROUP AND OFFER                              
* FOR PREEMPT AND LATE RUN, DELETE AT THE OFFER RECORD LEVEL                    
***********************************************************************         
DELEL66  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ZIC   R4,ELCODE           PRESERVE CALLER'S NEXTEL                     
*                                                                               
D6610    DS    0H                                                               
MGD      USING RMKGREC,MGDTREC                                                  
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'66'                                                     
         BRAS  RE,GETEL                                                         
         BNE   D66X                                                             
*                                                                               
D6620    DS    0H                                                               
         USING RBMGMSEL,R6                                                      
         CLC   MGD.RMKGKGRP,RBMGMSGD   CURRENT GROUP                            
         BNE   D6630                                                            
         CLC   MGD.RMKGKLIN,RBMGMSR#+1 CURRENT OFFER                            
         BNE   D6630                                                            
         DROP  MGD                                                              
*                                                                               
         TM    FMGTYPE,FMGPOFFQ+FMLROFFQ+FMLBOFFQ                               
         BZ    D6625                                                            
*                                                                               
MGKEYD   USING RMKGKEY,SVKEY                                                    
         CLC   MGKEYD.RMKGKRTY,RBMGMMUL CURRENT OFFER RECORD #                  
         BNE   D6630                                                            
         DROP  MGKEYD                                                           
*                                                                               
D6625    DS    0H                                                               
         GOTO1 VRECUP,DMCB,(2,RBUYREC),(R6),(R6)                                
         B     D6610                                                            
*                                                                               
D6630    DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         BE    D6620                                                            
*                                                                               
D66X     DS    0H                                                               
         STC   R4,ELCODE                                                        
         B     EXXMOD                                                           
         DROP  R6                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* VALIDATE MAKEGOOD HEADER INFO                                                 
*                                                                               
MGMVHDR  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
CHOYSERR EQU   464                 ERROR IN CHOICE FIELD                        
ALLCHOYS EQU   465                 OPTIONS CONFLICT                             
*                                                                               
         NI    FLAGS,X'FF'-X'40'   CLEAR HEADER RECORD INDICATOR                
         MVI   CHOYSCTR,0          CLEAR WORKFIELD                              
         TM    MGHMOPTH+1,X'0C'    FIELD IS HIDDEN, DON'T BOTHER                
         BO    BUED0005                                                         
         LA    R3,1                MISSING INPUT                                
         LA    R2,MGHMOPTH         A(OPTION FIELD)                              
         CLI   5(R2),0             ANYTHING IN FIELD?                           
         BE    ERROR               NO  -                                        
*                                                                               
* ACTION IS MG ADD                                                              
*                                                                               
BUED0005 DS    0H                                                               
         LA    R3,ALLCHOYS                                                      
         TM    FLAGS,FACTADDQ      FOR ADDING NEW OFFERS                        
         BZ    BUED0110            SPECIFY IF ALL OR CHOICE                     
*                                                                               
         LR    R4,RA                                                            
         AHI   R4,TWAWORKQ                                                      
         USING TWAWORK,R4                                                       
*                                                                               
         XCEFL RMKGREC,1000                                                     
         MVI   RMKGKEY,X'11'                                                    
         MVC   RMKGKREP,REPALPHA                                                
         MVC   RMKGKOFF,RCONKOFF   INSERT OFFICE                                
         MVC   RMKGKSTA,RCONKSTA   INSERT STATION+MEDIA                         
         MVC   RMKGKCON,TWACNUM    CONTRACT NUMBER                              
*        MVC   RMKGKGRP,MGHMGRP    GROUP CODE                                   
*                                                                               
*        OC    RMKGKGRP,RMKGKGRP                                                
*        BNZ   BUED0090                                                         
         MVC   KEY(27),RMKGKEY                                                  
         OI    DMINBTS,X'08'                                                    
         GOTO1 VHIGH                                                            
*                                                                               
BUED0010 DS    0H                                                               
         CLC   KEY(RMKGKGRP-RMKGKEY),KEYSAVE                                    
         BNE   BUED0020                                                         
         MVC   RMKGKGRP,KEY+RMKGKGRP-RMKGKEY                                    
         OI    DMINBTS,X'08'                                                    
         GOTO1 VSEQ                                                             
         B     BUED0010                                                         
*                                                                               
BUED0020 DS    0H                                                               
         XC    TWAMKGDH,TWAMKGDH   CLEAR NEW OFFER HEADER D/A                   
         DROP  R4                                                               
*                                                                               
         OC    RMKGKGRP,RMKGKGRP                                                
         BNZ   BUED0030                                                         
         MVC   RMKGKGRP,=C'AA'                                                  
*                                                                               
* IF TAKEOVER, AVOID CONFLICT WITH PREVIOUS REP'S OFFER CODES BY                
* STARTING OFFER CODE AT DA INSTEAD OF AA                                       
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1C'        TAKEOVER?                                    
         BRAS  RE,GETEL                                                         
         BNE   BUED0080                                                         
         MVC   RMKGKGRP,=C'DA'     FOR BRAND NEW OFFERS                         
         B     BUED0080                                                         
*                                                                               
* IF TAKEOVER, AND OFFER NOT ALREADY STARTED AT DA, FORCE IT TO START           
* AT GROUP CODE DA                                                              
*                                                                               
BUED0030 DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1C'        TAKEOVER?                                    
         BRAS  RE,GETEL                                                         
         BNE   BUED0035                                                         
         CLC   RMKGKGRP,=C'DA'                                                  
         BNL   BUED0035                                                         
         MVC   RMKGKGRP,=C'DA'                                                  
*                                                                               
BUED0035 DS    0H                                                               
         CLI   RMKGKGRP+1,C'I'                                                  
         BNE   *+12                                                             
         MVI   RMKGKGRP+1,C'J'                                                  
         B     BUED0080                                                         
*                                                                               
         CLI   RMKGKGRP+1,C'R'                                                  
         BNE   *+12                                                             
         MVI   RMKGKGRP+1,C'S'                                                  
         B     BUED0080                                                         
*                                                                               
         CLI   RMKGKGRP+1,C'Z'                                                  
         BNE   BUED0040                                                         
         MVI   RMKGKGRP+1,C'A'                                                  
         ZIC   RE,RMKGKGRP         ADVANCE FIRST LETTER                         
         AHI   RE,1                GOOD UNTIL LETTER J!                         
         STC   RE,RMKGKGRP                                                      
         B     BUED0080                                                         
*                                                                               
BUED0040 DS    0H                                                               
         ZIC   RE,RMKGKGRP+1       ADVANCE SECOND LETTER                        
         AHI   RE,1                                                             
         STC   RE,RMKGKGRP+1                                                    
*                                                                               
BUED0080 DS    0H                  ADDING TO NEW GROUP                          
*        MVI   MGHMOFF,C'1'        DEFAULT OFFER ONE                            
*        MVI   MGHMOFFH+5,1                                                     
*        OI    MGHMOFFH+4,X'08'    NUMERIC                                      
         B     BUED0100                                                         
*                                                                               
BUED0090 DS    0H                  ADDING TO EXISTING GROUP                     
*        BAS   RE,NEXTOFF          GET NEXT AVAILIABLE OFFER NUMBER             
*                                                                               
BUED0100 DS    0H                                                               
         MVC   TSARREC+2(27),RMKGKEY                                            
         MVC   MGHMGRP,RMKGKGRP                                                 
         OI    MGHMGRPH+6,X'80'    XMIT                                         
         TM    MGHMOPTH+1,X'0C'    FIELD IS HIDDEN, DON'T BOTHER                
         BO    BUED0171                                                         
         CLI   MGHMOPT,C'A'        YES - IS OPTION 'CHOICE'?                    
         BE    BUED0171            NO  - OPTIONS CONFLICT                       
         LA    R3,856                                                           
         CLI   FMGTYPE,FMLBOFFQ                                                 
         BE    ERROR                                                            
         CLI   MGHMOPT,C'C'        YES - IS OPTION 'CHOICE'?                    
         BE    BUED0171            NO  - OPTIONS CONFLICT                       
         LA    R3,ALLCHOYS                                                      
         B     ERROR                                                            
*                                                                               
* ACTION IS MGC/CHANGE                                                          
*                                                                               
BUED0110 DS    0H                                                               
         TM    MGHMOPTH+1,X'0C'    FIELD IS HIDDEN, DON'T BOTHER                
         BO    BUED0170                                                         
         CLI   CREATED,BYSTA       CREATOR REP OR STATION?                      
         BE    BUED0120                                                         
*                                                                               
* CREATOR WAS REP, CHECK IF USER STATION OR REP                                 
*                                                                               
         CLI   TWAACCS,C'$'                                                     
         BE    BUED0140                                                         
         B     BUED0130                                                         
*                                                                               
* CREATOR WAS STATION, CHECK IF USER STATION OR REP                             
*                                                                               
BUED0120 DS    0H                                                               
         CLI   TWAACCS,C'$'                                                     
         BNE   BUED0140                                                         
*                                                                               
* CURRENT USER IS THE CREATOR OF THIS MAKEGOOD OFFER                            
* SO CHECK FOR (A)LL OR (C)HOICE ONLY                                           
*                                                                               
BUED0130 DS    0H                                                               
         CLI   MGHMOPT,C'A'        YES - IS OPTION 'CHOICE'?                    
         BE    BUED0170            NO  - OPTIONS CONFLICT                       
         LA    R3,856                                                           
         CLI   FMGTYPE,FMLBOFFQ                                                 
         BE    ERROR                                                            
         CLI   MGHMOPT,C'C'        YES - IS OPTION 'CHOICE'?                    
         BE    BUED0170            NO  - OPTIONS CONFLICT                       
         LA    R3,ALLCHOYS                                                      
         B     ERROR                                                            
*                                                                               
* CURRENT USER DID NOT CREATE THIS MAKEGOOD OFFER                               
* SO IF (A)LL, DO NOTHING, ELSE IF (C)HOICE, SPECIFY MKG LINE DESIRED           
*                                                                               
BUED0140 DS    0H                                                               
         CLI   MGHMOPT,C'A'                                                     
         BE    BUED0170                                                         
*        CLI   MGHMOPT,C'1'                                                     
*        BL    ERROR                                                            
*        CLI   MGHMOPT,C'4'                                                     
*        BH    ERROR                                                            
                                                                                
         GOTO1 VPACK                                                            
         LTR   R0,R0                                                            
         BZ    ERROR                                                            
         STC   R0,CHOYSCTR                                                      
         CLI   CHOYSCTR,15                                                      
         BH    ERROR                                                            
*                                                                               
BUED0170 EQU   *                                                                
*        BAS   RE,MRKCHOYS                                                      
*                                                                               
         XCEFL TSARREC,1002                                                     
         MVC   TSARREC+2(RMKGKPLN-RMKGKEY),MGDTREC                              
*                                                                               
BUED0171 EQU   *                                                                
         MVI   TD.TSACTN,TSARDH    RETRIEVE MAKEGOOD HEADER RECORD              
         GOTO1 =A(GOTSAR),RR=RELO                                               
*                                                                               
         TM    FLAGS,FACTADDQ                                                   
         BZ    BUED0174                                                         
*                                                                               
         TM    TD.TSERRS,TSEEOF+TSERNF                                          
         BZ    BUED017A                                                         
         B     BUED0175                                                         
*                                                                               
BUED0174 EQU   *                                                                
         TM    TD.TSERRS,TSEEOF+TSERNF                                          
         BZ    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
BUED017A EQU   *                                                                
         GOTO1 VMOVEREC,DMCB,TSARREC+2,RMKGREC                                  
         OI    FLAGS,X'40'         HEADER RECORD EXISTS                         
*                                                                               
* DELETE AND REBUILD X'01' ELEMENT                                              
*                                                                               
         XC    ELEM,ELEM                                                        
         MVC   ELEM(RMKGSELQ),RMKGELEM                                          
         GOTO1 VDELELEM,DMCB,(X'01',RMKGREC)                                    
         B     BUED0178                                                         
*                                                                               
* VALIDATE PLEASE ADVISE OR ASSUME OK SELECTIONS                                
*                                                                               
BUED0175 EQU   *                                                                
         XC    ELEM,ELEM                                                        
MG01D    USING RMKGSDEM,ELEM                                                    
         MVI   MG01D.RMKGSDEM,X'01'                                             
         MVI   MG01D.RMKGSELN,RMKGSELQ   SET INITIAL LENGTH                     
*                                                                               
BUED0178 EQU   *                                                                
*&&DO                                                                           
         LA    R3,495              SELECTION ERROR                              
         LA    R2,MGHMPADH                                                      
         CLI   MGHMPAD,C'Y'        PLEASE ADVISE                                
         BE    BUED0180                                                         
         CLI   MGHMAOK,C'Y'        OR ASSUME OK                                 
         BNE   ERROR                                                            
         NI    MG01D.RMKGSCST,X'FF'-RMKGSPAQ                                    
         B     BUED0190                                                         
*                                                                               
BUED0180 DS    0H                                                               
         CLI   MGHMAOK,C'Y'                                                     
         BE    ERROR                                                            
         OI    MG01D.RMKGSCST,RMKGSPAQ                                          
*                                                                               
* VALIDATE EXPIRATION DATE                                                      
*                                                                               
BUED0190 DS    0H                                                               
         LA    R2,MGHEXPDH                                                      
         CLI   5(R2),0                                                          
         BE    BUED0200                                                         
         LA    R3,496                                                           
         GOTO1 DATVAL,DMCB,MGHEXPD,WORK                                         
         OC    DMCB(4),DMCB        ERROR?                                       
         BZ    ERROR                                                            
         GOTO1 DATCON,DMCB,WORK,(2,MG01D.RMKGSEXD)                              
*&&                                                                             
*                                                                               
BUED0200 DS    0H                                                               
         TM    FLAGS,FACTADDQ                                                   
         BZ    GCOM0050                                                         
*                                                                               
* IF MGM/ADD, SAVE REP OR STATION CREATOR/ORIGINATOR                            
*                                                                               
* STATION WIP                                                                   
*                                                                               
         MVI   MG01D.RMKGSFG2,RMGF2STQ+RMGF2WPQ                                 
         CLI   TWAACCS,C'$'        STATION SIDE?                                
         BE    GCOM0090            YES                                          
         OI    MG01D.RMKGSCST,RMKGSCRQ   NO, REP                                
*                                                                               
* REP WIP                                                                       
*                                                                               
         MVI   MG01D.RMKGSFG2,RMGF2RPQ+RMGF2WPQ                                 
         B     GCOM0090                                                         
                                                                                
GCOM0050 EQU   *                   FOR ACTION MGC/CHANGE                        
         CLI   TWAACCS,C'$'                                                     
         BE    GCOM0070                                                         
*                                                                               
* IF FIRST TO CHANGE AFTER MGS                                                  
*                                                                               
         TM    MG01D.RMKGSFG2,RMGF2RPQ+RMGF2WPQ                                 
         BO    GCOM0080                                                         
         OI    MG01D.RMKGSCST,RMKGSRVQ   CHANGE STATUS TO REVISED               
         NI    MG01D.RMKGSFG2,X'FF'-RMGF2STQ                                    
*                                                                               
* REP WIP                                                                       
*                                                                               
         OI    MG01D.RMKGSFG2,RMGF2RPQ+RMGF2WPQ                                 
         B     GCOM0080                                                         
*                                                                               
* IF FIRST TO CHANGE AFTER MGS                                                  
*                                                                               
GCOM0070 EQU   *                                                                
         TM    MG01D.RMKGSFG2,RMGF2STQ+RMGF2WPQ                                 
         BO    GCOM0080                                                         
         OI    MG01D.RMKGSCST,RMKGSRVQ   CHANGE STATUS TO REVISED               
         NI    MG01D.RMKGSFG2,X'FF'-RMGF2RPQ                                    
*                                                                               
* STATION WIP                                                                   
*                                                                               
         OI    MG01D.RMKGSFG2,RMGF2STQ+RMGF2WPQ                                 
*                                                                               
GCOM0080 EQU   *                                                                
         TM    MG01D.RMKGSCST,RMKGSRCQ+RMKGSRJQ                                 
         BZ    GCOM0090            OFFER WAS RECALLED/REJECTED                  
         NI    MG01D.RMKGSCST,X'FF'-RMKGSRCQ-RMKGSRJQ                           
         OI    MG01D.RMKGSCST,RMKGSRVQ   CHANGE STATUS TO REVISED               
*                                                                               
GCOM0090 EQU   *                                                                
         MVC   MG01D.RMKGSMOD,RCONMOD CURRENT CONTRACT MOD NUMBER               
*                                                                               
GCOM0110 EQU   *                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'                                                     
         BRAS  RE,GETEL                                                         
         BNE   GCOM0120            YES - NOT A DARE ORDER                       
         USING RCONDREL,R6                                                      
*        TM    RCONDRFG,X'80'+X'40'+X'01'                                       
*        BZ    GCOM0120                                                         
         OC    RCONDRLK,RCONDRLK                                                
         BZ    GCOM0120                                                         
         CLI   RCONDRLN,RCONDL2Q                                                
         BL    GCOM0115                                                         
         TM    RCONDRF2,X'08'      ORDER WAS REMOVED??                          
         BO    GCOM0120            YES, NOT A DARE ORDER ANYMORE                
*                                                                               
GCOM0115 EQU   *                                                                
         MVC   MG01D.RMKGDARN,RCONDRLK GET AGENCY DARE ORDER NUMBER             
         DROP  R6                                                               
*                                                                               
GCOM0120 EQU   *                                                                
*&&DO                                                                           
         OI    MG01D.RMKGSCST,RMKGSPAQ   DEFAULT:PLEASE ADVISE                  
         CLI   MGHMPAD,C'Y'                                                     
         BE    *+8                                                              
         NI    MG01D.RMKGSCST,X'FF'-RMKGSPAQ                                    
*&&                                                                             
*                                                                               
* CREATION DATE                                                                 
*                                                                               
         TM    FLAGS,FACTADDQ      FOR ACTION MGM/ADD                           
         BZ    GCOM0130                                                         
         GOTO1 DATCON,DMCB,(5,0),(2,MG01D.RMKGSCRD)                             
         GOTO1 GETTIME,DMCB,MG01D.RMKGSCRT                                      
*                                                                               
* EXPIRATION DATE                                                               
*                                                                               
GCOM0130 DS    0H                                                               
*&&DO                                                                           
         LA    R2,MGHEXPDH                                                      
         CLI   5(R2),0                                                          
         BE    GCOM0140                                                         
         GOTO1 DATVAL,DMCB,MGHEXPD,WORK                                         
         OC    DMCB(4),DMCB        ERROR?                                       
         BZ    GCOM0140            SKIP IT                                      
         GOTO1 DATCON,DMCB,WORK,(2,MG01D.RMKGSEXD)                              
*&&                                                                             
*                                                                               
* LAST ACTIVITY DATE                                                            
*                                                                               
GCOM0140 DS    0H                                                               
         GOTO1 DATCON,DMCB,(5,0),(2,MG01D.RMKGSLAD)                             
         GOTO1 GETTIME,DMCB,MG01D.RMKGSLAT                                      
         GOTO1 VADDELEM,DMCB,RMKGREC,ELEM                                       
         DROP  MG01D                                                            
*                                                                               
* ADD OR DELETE/REPLACE EXISTING SWITCH/PASSIVE KEY/SP-TEAM ELEMENT             
*                                                                               
         GOTOR OLD0AELT            FIND OLD X'0A' ELEMENT                       
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'0A',RMKGREC),0,0                
         XC    WORK2,WORK2         CLEAR ELEMENT                                
         MVI   WORK2,X'0A'         INSERT ELEMENT CODE                          
         MVI   WORK2+1,RMKGXLNQ    INSERT ELEMENT LENGTH                        
WK2      USING RMKGXEL,WORK2                                                    
         MVC   WK2.RMKGXSAL,RCONSAL                                             
*                                  INSERT SALESPERSON CODE                      
         MVC   WK2.RMKGXOFF,RCONKOFF                                            
*                                  INSERT OFFICE CODE                           
         MVC   WK2.RMKGXTEM,RCONTEM                                             
*                                  INSERT TEAM CODE                             
         MVC   WK2.RMKGXSTA,RCONKSTA                                            
*                                  INSERT STATION CODE                          
         MVC   WK2.RMKGXADV,RCONKADV                                            
*                                  INSERT ADVERTISER CODE                       
         MVC   WK2.RMKGXAGY,RCONKAGY                                            
*                                  INSERT AGENCY CODE                           
         MVC   WK2.RMKGXAOF,RCONKAOF                                            
*                                  INSERT AGENCY OFFICE CODE                    
         MVC   WK2.RMKGXGRP,RCONKGRP                                            
*                                  INSERT GROUP/SUBGROUP                        
         MVC   WK2.RMKGXFLT,RCONDATE                                            
*                                  INSERT FLIGHT DATES                          
         LA    R6,RCONREC          CHECK FOR USE OF REVISED FLIGHT              
         MVI   ELCODE,X'1E'                                                     
         BRAS  RE,GETEL                                                         
         BNE   GCOM0170                                                         
         USING RCONRFEL,R6                                                      
         OC    RCONRFLT,RCONRFLT                                                
         BZ    GCOM0170                                                         
         MVC   WK2.RMKGXFLT,RCONRFLT  USE REVISED FLIGHT                        
         DROP  R6                                                               
*                                                                               
GCOM0170 DS    0H                                                               
         LA    RF,RCONELEM         FIND DEVELOPMENT ELT                         
GCOM0180 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BE    GCOM0200            YES - NO DEV ELT PRESENT                     
         CLI   0(RF),X'18'         DEVELOPMENT ELT?                             
         BE    GCOM0190            YES -                                        
         ZIC   RE,1(RF)            NO  - BUMP TO NEXT ELEMENT                   
         AR    RF,RE                                                            
         B     GCOM0180            GO BACK FOR NEXT                             
GCOM0190 EQU   *                                                                
         MVC   WK2.RMKGXDSP,RCONDVSP-RCONDVEL(RF)                               
*                                  INSERT DEV S/P                               
         MVC   WK2.RMKGXDCT,RCONDVCT-RCONDVEL(RF)                               
*                                  INSERT DEV CON TYPE                          
GCOM0200 EQU   *                                                                
         GOTO1 VADDELEM,DMCB,RMKGREC,WORK2                                      
         DROP  WK2                                                              
*                                                                               
*                                                                               
* DELETE EXISTING GROUP COMMENT                                                 
*                                                                               
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'10',RMKGREC),0,0                
*                                                                               
         LA    R6,RMKGELEM                                                      
GCOM0210 CLI   0(R6),X'10'                                                      
         BH    GCOM0220                                                         
         CLI   0(R6),0                                                          
         BE    GCOM0220                                                         
         ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     GCOM0210                                                         
*                                                                               
GCOM0220 EQU   *                                                                
         LA    R3,3                UP TO 3 LINES OF GROUP COMMENTS              
         LA    R2,MGHGCMTH         SET A(GROUP COMMENT FIELD)                   
GCOM0225 CLI   5(R2),0             ANYTHING IN FIELD?                           
         BE    GCOM0230            SKIP IF NO COMMENT                           
         XC    WORK2,WORK2                                                      
WRK2D    USING RMKGGCEM,WORK2                                                   
         MVI   WRK2D.RMKGGCCD,X'10'                                             
         ZIC   RF,5(R2)            GET LENGTH OF FIELD                          
         AHI   RF,2                                                             
         STC   RF,WRK2D.RMKGGCLN   INSERT LENGTH INTO ELEMENT                   
         AHI   RF,-3               SUBTRACT 2 FOR CONTROL, 1 FOR EX             
         EX    RF,*+8              MOVE COMMENT BY LENGTH                       
         B     *+10                                                             
         MVC   WRK2D.RMKGGCCM(0),8(R2)                                          
         DROP  WRK2D                                                            
*                                                                               
         GOTO1 VRECUP,DMCB,(2,RMKGREC),WORK2,(R6)                               
         ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
*                                                                               
GCOM0230 DS    0H                                                               
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BCT   R3,GCOM0225                                                      
*                                                                               
         MVI   TD.TSACTN,TSAADD    ADD RECORD TO TSAR BUFFER                    
         TM    FLAGS,X'40'         HEADER REC EXISTS IN TSAR BUFFER?            
         BZ    *+8                                                              
         MVI   TD.TSACTN,TSAPUT    PUT RECORD TO TSAR BUFFER                    
         GOTO1 VMOVEREC,DMCB,RMKGREC,TSARREC+2                                  
         SR    R1,R1                                                            
         ICM   R1,3,RMKGLEN        ADD LENGTH FIELD                             
         AHI   R1,2                                                             
         STCM  R1,3,TSARREC                                                     
*                                                                               
         GOTO1 =A(GOTSAR),RR=RELO                                               
         TM    TD.TSERRS,TSEEOF+TSEDUP                                          
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VHDRX    DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
* DISPLAYS NEXT OFFER NUMBER ON SCREEN                                          
*                                                                               
NEXTOFF  NTR1                                                                   
*                                                                               
         XR    R3,R3                                                            
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(RMKGKPLN-RMKGKEY),MGDTREC                                    
MGKEYD   USING RMKGKEY,KEY                                                      
         GOTO1 VHIGH                                                            
         CLC   KEY(RMKGKPLN-RMKGKEY),KEYSAVE                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
NOFF10   DS    0H                                                               
         ZIC   R3,MGKEYD.RMKGKLIN                                               
         GOTO1 VSEQ                                                             
         CLC   KEY(RMKGKPLN-RMKGKEY),KEYSAVE                                    
         BE    NOFF10                                                           
*                                                                               
NOFF20   DS    0H                                                               
         AHI   R3,1                OKAY, BUMP TO NEXT AVAILIABLE NUMBER         
*                                                                               
*        EDIT  (R3),(2,MGHMOFF),ALIGN=LEFT                                      
*        STC   R0,MGHMOFFH+5                                                    
*        OI    MGHMOFFH+4,X'08'    NUMERIC                                      
*                                                                               
NOFFX    DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
* GET CURRENT TIME                                                              
* P1 HAS A(2-BYTE) HHMM DESTINATION                                             
*                                                                               
GETTIME  NTR1                                                                   
         L     R4,0(R1)                                                         
*                                                                               
         TIME  DEC                                                              
*                                  RETRIEVE TIME:  RETURNED IN RO               
*                                     AS HH:MM:SS:TT                            
         STCM  R0,4,1(R4)          STORE MINUTES IN SAVE AREA                   
         STCM  R0,2,2(R4)          STORE SECONDS IN SAVE AREA                   
         SRL   R0,24               SHIFT HOURS TO LOW-ORDER                     
         STC   R0,WORK                                                          
         GOTO1 HEXOUT,DMCB,WORK,WORK+1,1,=C'TOG'                                
         PACK  DUB,WORK+1(2)                                                    
         CVB   R2,DUB                                                           
         LA    R2,DDSTMADJ(R2)     ADD ADJUSTMENT FOR DDS TIME                  
         EDIT  (R2),(2,WORK+17),FILL=0,ZERO=NOBLANK                             
         GOTO1 HEXIN,DMCB,WORK+17,(R4),2,0                                      
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
*   OLD0AELT:  FIND EXISTING 0A ELT, SAVE S/P, TEAM CODES FOR                   
*        DELETING OLD KEY                                                       
*                                                                               
OLD0AELT NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*         READ MAKEGOOD HEADER                                                  
*                                                                               
         XC    DUB,DUB             USE DUB FOR INTERMEDIATE                     
*                                                                               
         TM    FLAGS,FACTADDQ      IF NOT ADDING, READ HEADER                   
         BZ    OLD0A05             THAT'S ON FILE                               
         LA    R6,RMKGREC                                                       
         B     OLD0A10                                                          
*                                                                               
OLD0A05  DS    0H                                                               
         LR    R4,RA                                                            
         AHI   R4,TWAWORKQ                                                      
         USING TWAWORK,R4                                                       
         OC    TWAMKGDH,TWAMKGDH                                                
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   KEY+28(4),TWAMKGDH                                               
         GOTO1 VGETREC,DMCB,MKGHDR                                              
         DROP  R4                                                               
*                                                                               
         LA    R6,MKGHDR           POINT TO MAKEGOOD HEADER                     
*                                                                               
OLD0A10  DS    0H                                                               
*                                                                               
         LA    R1,RMKGELEM-RMKGREC(R6)                                          
*                                                                               
*        SAVE FIELDS FOR DELETING PASSIVES                                      
*                                                                               
         USING RMKGSDEM,R1         ESTABLISH GROUP STATUS ELEMENT               
*                                                                               
         MVC   SAVDAT,RMKGFOFD     SAVE FIRST OFFERED DATE                      
         MVC   SAVWIP,RMKGSFG2     WIP STATUS                                   
         MVC   SAVSTT,RMKGSCST     OFFER STATUS                                 
         NI    SAVSTT,X'FF'-RMKGSPAQ KILL OLD STATUS                            
         TM    RMKGSFG3,RMGF3SAQ   IF SELF APPLIED                              
         BNO   *+8                                                              
         OI    SAVSTT,RMKGSLFQ        SET INDICATOR                             
*                                                                               
         MVC   SAVDST,RMKGSFG1     DARE  STATUS                                 
*                                                                               
         DROP  R1                                                               
*                                                                               
OLDA0020 EQU   *                                                                
         CLI   0(R1),0             END OF RECORD?                               
         BE    OLDA0100            YES - FINISHED - NO ELT                      
         CLI   0(R1),X'0A'         0A ELT?                                      
         BE    OLDA0040            YES - PROCESS IT                             
         ZIC   RF,1(R1)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R1,RF                                                            
         B     OLDA0020            GO BACK FOR NEXT                             
OLDA0040 EQU   *                                                                
         USING RMKGXEL,R1                                                       
         MVC   DUB(3),RMKGXSAL     SAVE S/P CODE                                
         MVC   DUB+3(2),RMKGXTEM   SAVE TEAM CODE                               
*                                                                               
*        SAVE FIELDS FOR PASSIVES                                               
*                                                                               
         MVC   SAVSAL,RMKGXSAL     SALESPERSON                                  
         MVC   SAVTEM,RMKGXTEM     TEAM                                         
         MVC   SAVADV,RMKGXADV     ADVERTISER                                   
         MVC   SAVDSL,RMKGXDSP     DEVELOPMENTAL SALESPERSON                    
*                                                                               
         DROP  R1                                                               
*                                                                               
OLDA0100 EQU   *                                                                
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* - VALIDATE MAKEGOOD AND LATE RUN OFFERS                                       
* - X'04' MG= AND X'05' ELEMENTS WILL BE ADDED LATER                            
*                                                                               
MGMVOFF NTR1   BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,MGSMD1H                                                       
         CLI   FMGTYPE,FMGMOFFQ    CHECK IF RNA                                 
         BE    VOFF05                                                           
         LA    R2,RNAMD1H                                                       
         CLI   FMGTYPE,FRNAOFFQ    OR REGULAR MG OFFER                          
         BNE   VOFF10                                                           
*                                                                               
VOFF05   DS    0H                                                               
         LA    R3,1                MISSING                                      
         CLI   5(R2),0             MAKE SURE USER ENTER A DATE IN               
         BE    ERROR               MISSED DATE FIELD                            
*                                                                               
VOFF10   DS    0H                                                               
         LR    R4,RA                                                            
         AHI   R4,TWAWORKQ                                                      
         USING TWAWORK,R4                                                       
         MVC   LINECTR,TWASTRT#                                                 
         DROP  R4                                                               
*                                                                               
*        TM    FLAGS,FACTBONQ      PROCESSING LATE RUN BONUS?                   
*        BZ    VOFF20                                                           
*                                                                               
VOFF20   DS    0H                                                               
         XC    OFFRDISP,OFFRDISP                                                
         NI    FLAGS,X'FF'-X'20'   DEFAULT PUT TO TSAR BUFFER                   
*                                                                               
* IF CHANGE, RETRIEVE OFFER RECORD FROM TSAR BUFFER                             
*                                                                               
VOFF30   DS    0H                                                               
         LA    R2,MGSODTSH                                                      
         CLI   FMGTYPE,FMGMOFFQ    REGULAR?                                     
         BE    VOFF40                                                           
*                                                                               
         LA    R2,RNAODTSH                                                      
         CLI   FMGTYPE,FRNAOFFQ    REPLACEMENT NA?                              
         BE    VOFF40                                                           
*                                                                               
         LA    R2,MGBODTSH                                                      
         CLI   FMGTYPE,FMGBOFFQ    BONUS?                                       
         BE    VOFF40                                                           
*                                                                               
         LA    R2,MLRODTSH                                                      
         CLI   FMGTYPE,FMLROFFQ    LATE RUN OFFER?                              
         BE    VOFF40                                                           
*                                                                               
         CLI   FMGTYPE,FMLBOFFQ    LATE RUN W/ BONUS OFFER?                     
         BE    *+6                                                              
         DC    H'0'                INVALID OFFER TYPE                           
         LA    R2,MLBMDTSH                                                      
         TM    FLAGS,FACTBONQ      PROCESSING BONUS PORTION?                    
         BZ    *+8                                                              
         LA    R2,MLBBDTSH                                                      
*                                                                               
VOFF40   DS    0H                                                               
         A     R2,OFFRDISP                                                      
         CLI   5(R2),0             CHECK DATE FIELD                             
         BNE   VOFF50                                                           
         LA    R3,SDTERR                                                        
         OC    OFFRDISP,OFFRDISP                                                
         BZ    ERROR                                                            
         B     VOFFX                                                            
*                                                                               
VOFF50   DS    0H                                                               
         XCEFL RMKGREC,1000                                                     
*                                                                               
         LR    R4,RA                                                            
         AHI   R4,TWAWORKQ                                                      
         USING TWAWORK,R4                                                       
*                                                                               
         XC    KEY,KEY                                                          
         MVI   RMKGKTYP,X'11'                                                   
         MVC   RMKGKREP,REPALPHA                                                
         MVC   RMKGKOFF,RCONKOFF   INSERT OFFICE                                
         MVC   RMKGKSTA,RCONKSTA   INSERT STATION+MEDIA                         
         MVC   RMKGKCON,TWACNUM    CONTRACT NUMBER                              
         MVC   RMKGKGRP,MGHMGRP    GROUP CODE                                   
         MVC   RMKGKPLN,=3X'FF'    SKIP GROUP RECORD                            
         MVI   RMKGKLIN,1                                                       
****************                                                                
* TEMPORARY CODING TO FIX BONUS KEY BUG                                         
*                                                                               
         CLI   FMGTYPE,FMGBOFFQ    BONUS                                        
         BNE   TEMP10                                                           
         TM    FLAGS,FACTCHAQ                                                   
         BZ    TEMP10                                                           
         CLI   TEMPFLG,C'Y'                                                     
         BNE   TEMP10                                                           
         MVC   RMKGKPLN,=3X'0'     OLD STYLE BONUS HAS X'00' HERE               
*                                                                               
TEMP10   DS    0H                                                               
****************                                                                
*                                                                               
         TM    FLAGS,FACTADDQ                                                   
         BZ    VOFF60                                                           
         OI    FLAGS,X'20'         ADD RECORD TO TSAR BUFFER                    
*                                                                               
VOFF60   DS    0H                                                               
         MVC   RMKGKRTY,LINECTR    LINE NUMBER                                  
         ZIC   RE,LINECTR                                                       
         AHI   RE,1                                                             
         STC   RE,LINECTR                                                       
*        CLI   MGHMOPT,C'C'        OFFERING ALL OR CHOICE?                      
*        BNE   VOFF90                                                           
*        OI    RMKGKRTY,X'10'                                                   
         DROP  R4                                                               
*                                                                               
VOFF90   DS    0H                                                               
         MVC   KEY(27),RMKGKEY                                                  
         XC    ELEM,ELEM                                                        
*                                                                               
         LA    R2,MGSODTSH         A(1ST LINE OF MG OFFER)                      
         CLI   FMGTYPE,FMGMOFFQ    REGULAR?                                     
         BE    VOFF100                                                          
*                                                                               
         LA    R2,RNAODTSH                                                      
         CLI   FMGTYPE,FRNAOFFQ    REPLACMENT NA?                               
         BE    VOFF100                                                          
*                                                                               
         LA    R2,MGBODTSH                                                      
         CLI   FMGTYPE,FMGBOFFQ    BONUS?                                       
         BE    VOFF100                                                          
*                                                                               
         LA    R2,MLRODTSH         A(1ST LINE OF LR OFFER)                      
         CLI   FMGTYPE,FMLROFFQ                                                 
         BE    VOFF100                                                          
*                                                                               
         CLI   FMGTYPE,FMLBOFFQ                                                 
         BE    *+6                                                              
         DC    H'0'                INVALID OFFER TYPE                           
         LA    R2,MLBMDTSH         A(1ST LINE OF LR W/B OFFER)                  
         TM    FLAGS,FACTBONQ      PROCESSING BONUS PORTION?                    
         BZ    *+8                                                              
         LA    R2,MLBBDTSH                                                      
VOFF100  DS    0H                                                               
*                                                                               
* MAKEGOOD OFFER ADD                                                            
*                                                                               
         TM    FLAGS,FACTADDQ                                                   
         BZ    VOFF110                                                          
         MVC   RMKGLEN,=H'34'                                                   
         MVC   ELEM(2),=X'012B'                                                 
MG01D    USING RMKGELEM,ELEM                                                    
         MVC   MG01D.RMKGCREA,TODAY                                             
         MVC   MG01D.RMKGKMOD,RCONMOD                                           
         MVC   MG01D.RMKGCHGI,=C'A '                                            
*                                                                               
         CLI   FMGTYPE,FRNAOFFQ    REPLACEMENT NA?                              
         BNE   *+8                                                              
         OI    MG01D.RMKGRTS,X'02' MARK AS REPLACEMENT NA                       
*                                                                               
         CLI   FMGTYPE,FMGBOFFQ    BONUS?                                       
         BNE   *+8                                                              
         OI    MG01D.RMKGRTS,X'20' MARK AS BONUS                                
*                                                                               
         CLI   FMGTYPE,FMLROFFQ                                                 
         BNE   *+8                                                              
         OI    MG01D.RMKGRTS,X'08' MARK AS LATE RUN                             
*                                                                               
         CLI   FMGTYPE,FMLBOFFQ                                                 
         BNE   *+8                                                              
         OI    MG01D.RMKGRTS,X'04' MARK AS LATE RUN W/BONUS                     
*                                                                               
         TM    FLAGS,FACTBONQ                                                   
         BZ    *+8                                                              
         OI    MG01D.RMKGRTS,X'20' MARK AS BONUS OF LATE RUN W/BONUS            
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL                                                         
         BNE   VOFF160                                                          
         USING RCONSEND,R6                                                      
         MVC   MG01D.RMKGVER,RCONSRV                                            
         B     VOFF160                                                          
         DROP  MG01D,R6                                                         
*                                                                               
* MAKEGOOD OFFER CHANGE                                                         
*                                                                               
VOFF110  DS    0H                                                               
         XCEFL TSARREC,1002                                                     
         MVC   TSARREC+2(27),RMKGKEY                                            
         MVI   TD.TSACTN,TSARDH    RETRIEVE FIRST DETAIL OFFER RECORD           
         GOTO1 =A(GOTSAR),RR=RELO                                               
*                                                                               
         TM    TD.TSERRS,TSEEOF+TSERNF                                          
         BZ    VOFF150                                                          
         ZIC   R3,RMKGKRTY         RESET TO LAST RECORD NUMBER                  
         AHI   R3,-1                                                            
         STC   R3,RMKGKRTY         WILL BUMP TO NEW NUMBER BELOW                
         B     VOFF130                                                          
*                                                                               
VOFF120  DS    0H                                                               
*        XCEFL TSARREC,1002                                                     
         MVI   TD.TSACTN,TSANXT                                                 
         GOTO1 =A(GOTSAR),RR=RELO                                               
         TM    TD.TSERRS,TSEEOF+TSERNF                                          
         BNZ   VOFF125                                                          
*                                                                               
         TM    FMGTYPE,FMLBOFFQ    FOR LATE RUN W/BONUS PROCESSING:             
         BZ    VOFF140                                                          
         TM    FLAGS,FACTBONQ      SKIP LATE RUN BONUS IF WE ARE                
         BO    VOFF140             PROCESSING LATE RUN PORTION                  
MGRECD   USING RMKGREC,TSARREC+2                                                
         TM    MGRECD.RMKGRTS,X'20'                                             
         BZ    VOFF140                                                          
         DROP  MGRECD                                                           
*                                                                               
VOFF125  DS    0H                                                               
         LA    R2,MGSODTSH                                                      
         CLI   FMGTYPE,FMGMOFFQ    REGULAR?                                     
         BE    VOFF130                                                          
*                                                                               
         LA    R2,RNAODTSH                                                      
         CLI   FMGTYPE,FRNAOFFQ    REPLACEMENT NA?                              
         BE    VOFF130                                                          
*                                                                               
         LA    R2,MGBODTSH                                                      
         CLI   FMGTYPE,FMGBOFFQ    BONUS?                                       
         BE    VOFF130                                                          
*                                                                               
         LA    R2,MLRODTSH                                                      
         CLI   FMGTYPE,FMLROFFQ    LATE RUN?                                    
         BE    VOFF130                                                          
*                                                                               
         CLI   FMGTYPE,FMLBOFFQ    LATE RUN WITH BONUS?                         
         BE    *+6                                                              
         DC    H'0'                INVALID OFFER TYPE                           
         LA    R2,MLBMDTSH                                                      
         TM    FLAGS,FACTBONQ      PROCESSING BONUS PORTION?                    
         BZ    *+8                                                              
         LA    R2,MLBBDTSH                                                      
*                                                                               
VOFF130  DS    0H                                                               
         A     R2,OFFRDISP                                                      
         CLI   5(R2),0                                                          
         BE    VOFFX               WE ARE DONE                                  
*                                                                               
* USER PUT IN A NEW OFFER LINE. ADD RECORD TO TSAR BUFFER                       
*                                                                               
         OI    FLAGS,X'20'         CHANGE MODE TO TSAR ADD                      
         ZIC   R3,RMKGKRTY                                                      
         AHI   R3,1                                                             
         XCEFL RMKGREC,1000                                                     
         MVC   RMKGREC(27),MGDTREC                                              
         STC   R3,RMKGKRTY         SET NEXT RECORD NUMBER                       
*                                                                               
* GET NEXT OFFER LINE NUMBER                                                    
*                                                                               
         MVC   RMKGLEN,=H'34'                                                   
         XC    ELEM,ELEM                                                        
         MVC   ELEM(2),=X'012B'                                                 
MG01D    USING RMKGELEM,ELEM                                                    
         MVC   MG01D.RMKGCREA,TODAY                                             
         MVC   MG01D.RMKGKMOD,RCONMOD                                           
         MVC   MG01D.RMKGCHGI,=C'A '                                            
*                                                                               
         CLI   FMGTYPE,FMGBOFFQ    BONUS?                                       
         BNE   *+8                                                              
         OI    MG01D.RMKGRTS,X'20' MARK AS BONUS                                
*                                                                               
         CLI   FMGTYPE,FMLROFFQ                                                 
         BNE   *+8                                                              
         OI    MG01D.RMKGRTS,X'08' MARK AS LATE RUN                             
*                                                                               
         CLI   FMGTYPE,FMLBOFFQ                                                 
         BNE   *+8                                                              
         OI    MG01D.RMKGRTS,X'04' MARK AS LATE RUN W/BONUS                     
*                                                                               
         TM    FLAGS,FACTBONQ                                                   
         BZ    *+8                                                              
         OI    MG01D.RMKGRTS,X'20' MARK AS BONUS OF LATE RUN W/BONUS            
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL                                                         
         BNE   VOFF160                                                          
         USING RCONSEND,R6                                                      
         MVC   MG01D.RMKGVER,RCONSRV                                            
         B     VOFF160                                                          
         DROP  MG01D,R6                                                         
*                                                                               
VOFF140  DS    0H                                                               
         CLC   TSARREC+2(RMKGKMLN-RMKGKEY),KEY                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VOFF150  DS    0H                                                               
         GOTO1 VMOVEREC,DMCB,TSARREC+2,RMKGREC                                  
*                                                                               
         TM    FMGTYPE,FMLBOFFQ    FOR LATE RUN W/BONUS PROCESSING:             
         BZ    VOFF158                                                          
         TM    FLAGS,FACTBONQ      START AT BONUS RECORD FOR                    
         BZ    VOFF158             LATE RUN BONUS PROCESSING                    
         TM    RMKGRTS,X'20'                                                    
         BZ    VOFF120                                                          
*                                                                               
VOFF158  DS    0H                                                               
         LA    R6,RMKGREC                                                       
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         BE    VOFF170                                                          
*                                                                               
VOFF160  DS    0H                                                               
         GOTO1 VADDELEM,DMCB,RMKGREC,ELEM                                       
*                                                                               
VOFF170  DS    0H                                                               
         GOTO1 =A(SETCHOYS),RR=RELO                                             
*        GOTO1 =A(MISSCMNT),RR=RELO                                             
*                                                                               
         GOTO1 =A(CHECKDEL),RR=RELO                                             
         BZ    VOFF215             USER DELETING DETAIL LINE?                   
*                                                                               
         TM    FLAGS,FACTBONQ      PROCESSING BONUS PORTION                     
         BO    VOFF190             OF LATE RUN W/ BONUS?                        
         CLI   FMGTYPE,FMLROFFQ    LATE RUN USES TARGET                         
         BE    VOFF180             RATE, LENGTH AND DAY/TIME                    
         CLI   FMGTYPE,FMLBOFFQ                                                 
         BNE   VOFF190                                                          
VOFF180  DS    0H                                                               
         GOTO1 =A(LREDIT),RR=RELO                                               
         GOTO1 =A(DTSPEDIT),RR=RELO                                             
         GOTO1 =A(LRDAYGEN),RR=RELO                                             
         GOTO1 =A(RATEDIT),RR=RELO                                              
         B     VOFF200                                                          
*                                                                               
VOFF190  DS    0H                                                               
         CLI   FMGTYPE,FRNAOFFQ    REPLACEMENT NA?                              
         BNE   VOFF195                                                          
         CLC   =C'NONE',RNAODTS    CREDIT FOR NA KEYWORD                        
         BE    VOFF215                                                          
*                                                                               
VOFF195  DS    0H                                                               
         GOTO1 =A(DYTMEDIT),RR=RELO                                             
         GOTO1 =A(DTSPEDIT),RR=RELO                                             
         GOTO1 =A(RATEDIT),RR=RELO                                              
         GOTO1 =A(LENEDIT),RR=RELO                                              
*                                                                               
VOFF200  DS    0H                                                               
         BAS   RE,BLDRTSEL         CHECK REP TO SPOT TRANSFER                   
*                                  LIMITATIONS                                  
*                                                                               
         LA    R2,MGSOPGNH         SET A(PROGRAM NAME)                          
         CLI   FMGTYPE,FMGMOFFQ    REGULAR?                                     
         BE    VOFF205                                                          
                                                                                
         LA    R2,RNAOPGNH         SET A(PROGRAM NAME)                          
         CLI   FMGTYPE,FRNAOFFQ    REPLACEMENT NA?                              
         BE    VOFF205                                                          
*                                                                               
         LA    R2,MGBOPGNH         SET A(PROGRAM NAME)                          
         CLI   FMGTYPE,FMGBOFFQ    BONUS?                                       
         BE    VOFF205                                                          
*                                                                               
         LA    R2,MLROPGNH         SET A(PROGRAM NAME)                          
         CLI   FMGTYPE,FMLROFFQ    LATE RUN?                                    
         BE    VOFF205                                                          
                                                                                
         CLI   FMGTYPE,FMLBOFFQ    LATE RUN W/BONUS?                            
         BE    *+6                                                              
         DC    H'0'                INVALID OFFER TYPE!                          
         LA    R2,MLBOPGNH         SET A(PROGRAM NAME)                          
         TM    FLAGS,FACTBONQ      PROCESSING BONUS PORTION                     
         BZ    *+8                 OF LATE RUN W/ BONUS?                        
         LA    R2,MLBBPN1H         SET A(PROGRAM NAME)                          
*                                                                               
VOFF205  DS    0H                                                               
         A     R2,OFFRDISP         OFFSET TO SPECIFIC LINE                      
         GOTOX =A(PRNMDTCM),X'21',RR=RELO                                       
*                                                                               
VOFF210  DS    0H                                                               
         LA    R2,MGSOCMTH         SET A(DETAIL COMMENT)                        
         CLI   FMGTYPE,FMGMOFFQ    REGULAR?                                     
         BE    VOFF213                                                          
*                                                                               
         LA    R2,RNAOCMTH         SET A(DETAIL COMMENT)                        
         CLI   FMGTYPE,FRNAOFFQ    REPLACEMENT NA?                              
         BE    VOFF213                                                          
*                                                                               
         LA    R2,MGBOCMTH                                                      
         CLI   FMGTYPE,FMGBOFFQ    BONUS?                                       
         BE    VOFF213                                                          
*                                                                               
         LA    R2,MLROCMTH         SET A(DETAIL COMMENT)                        
         CLI   FMGTYPE,FMLROFFQ    LATE RUN?                                    
         BE    VOFF213                                                          
*                                                                               
         CLI   FMGTYPE,FMLBOFFQ    LATE RUN W/BONUS?                            
         BE    *+6                                                              
         DC    H'0'                INVALID OFFER TYPE!                          
         LA    R2,MLBMCMTH         SET A(DETAIL COMMENT)                        
         TM    FLAGS,FACTBONQ      PROCESSING BONUS PORTION                     
         BZ    *+8                 OF LATE RUN W/ BONUS?                        
         LA    R2,MLBBCMTH         SET A(DETAIL COMMENT)                        
VOFF213  DS    0H                                                               
         A     R2,OFFRDISP         OFFSET TO SPECIFIC LINE                      
*                                                                               
         GOTOX =A(PRNMDTCM),X'11',RR=RELO                                       
*                                                                               
* READ DEMO VALUE INPUT                                                         
         DS    0H                                                               
         LA    R2,MGSODEMH         SET A(DEMO INPUT    )                        
         CLI   FMGTYPE,FMGMOFFQ    REGULAR?                                     
         BE    VOFF214                                                          
*                                                                               
         LA    R2,RNAODEMH         SET A(DEMO INPUT    )                        
         CLI   FMGTYPE,FRNAOFFQ    REPLACEMENT NA?                              
         BE    VOFF214                                                          
*                                                                               
         LA    R2,MGBODEMH                                                      
         CLI   FMGTYPE,FMGBOFFQ    BONUS?                                       
         BE    VOFF214                                                          
*                                                                               
         LA    R2,MLRODEMH         SET A(DEMO INPUT    )                        
         CLI   FMGTYPE,FMLROFFQ    LATE RUN?                                    
         BE    VOFF214                                                          
*                                                                               
         CLI   FMGTYPE,FMLBOFFQ    LATE RUN W/BONUS?                            
         BE    *+6                                                              
         DC    H'0'                INVALID OFFER TYPE!                          
         LA    R2,MLBMDEMH         SET A(DEMO INPUT    )                        
         TM    FLAGS,FACTBONQ      PROCESSING BONUS PORTION                     
         BZ    *+8                 OF LATE RUN W/ BONUS?                        
         LA    R2,MLBBDEMH         SET A(DEMO INPUT    )                        
VOFF214  DS    0H                                                               
         A     R2,OFFRDISP         OFFSET TO SPECIFIC LINE                      
*                                                                               
         GOTOR CKSTPROF            SEE IF WE SHOULD PROCESS DEMO                
         BNE   VOFF215                                                          
         GOTOR GETDEMV,DMCB,X'0E'                                               
*                                                                               
VOFF215  DS    0H                                                               
         BAS   RE,SVFSTAIR         SAVE OFF FIRST AIR DATE                      
*                                                                               
         MVI   TD.TSACTN,TSAADD    ADD RECORD                                   
         TM    FLAGS,X'20'         ADD TO TSAR BUFFER?                          
         BO    VOFF220                                                          
         MVI   TD.TSACTN,TSAPUT    PUT RECORD BACK BY NUMBER                    
*        NI    RMKGCNTL,X'FF'-X'80' SET ACTIVE                                  
*                                                                               
VOFF220  DS    0H                                                               
         GOTO1 VMOVEREC,DMCB,RMKGREC,TSARREC+2                                  
         SR    R1,R1                                                            
         ICM   R1,3,RMKGLEN        ADD LENGTH FIELD                             
         AHI   R1,2                                                             
         STCM  R1,3,TSARREC                                                     
*                                                                               
         GOTO1 =A(GOTSAR),RR=RELO                                               
         TM    TD.TSERRS,TSEEOF+TSEDUP                                          
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* FOR ADD, SAVE OFF FIRST DETAIL OFFER RECORD                                   
*                                                                               
         TM    FLAGS,FACTADDQ                                                   
         BZ    VOFF230                                                          
*                                                                               
         LR    R4,RA                                                            
         AHI   R4,TWAWORKQ                                                      
         USING TWAWORK,R4                                                       
         CLI   TWASTRT#,1                                                       
         BNE   VOFF230                                                          
         DROP  R4                                                               
*                                                                               
         GOTO1 VMOVEREC,DMCB,RMKGREC,MGDTREC                                    
*                                                                               
VOFF230  DS    0H                                                               
         CLI   FMGTYPE,FRNAOFFQ    REPLACEMENT NA?                              
         BNE   VOFF235                                                          
         CLC   =C'NONE',RNAODTS    IF CREDIT FOR NA, EXIT HERE                  
         BE    VOFFX                                                            
*                                                                               
VOFF235  DS    0H                                                               
         LA    R2,MGSODTSH                                                      
         LA    R3,MGSODT4H         COMPARE TO LAST DATE FIELD                   
         LA    RE,MGSODT2H-MGSODTSH                                             
         CLI   FMGTYPE,FMGMOFFQ    REGULAR?                                     
         BE    VOFF240                                                          
*                                                                               
         LA    R2,RNAODTSH                                                      
         LA    R3,RNAODT4H         COMPARE TO LAST DATE FIELD                   
         LA    RE,RNAODT2H-RNAODTSH                                             
         CLI   FMGTYPE,FRNAOFFQ    REPLACEMENT NA?                              
         BE    VOFF240                                                          
*                                                                               
         LA    R2,MGBODTSH                                                      
         LA    R3,MGBODT7H         COMPARE TO LAST DATE FIELD                   
         LA    RE,MGBODT2H-MGBODTSH                                             
         CLI   FMGTYPE,FMGBOFFQ    BONUS?                                       
         BE    VOFF240                                                          
*                                                                               
         LA    R2,MLRODTSH                                                      
         LA    R3,MLRODT7H         COMPARE TO LAST DATE FIELD                   
         LA    RE,MLRODT2H-MLRODTSH                                             
         CLI   FMGTYPE,FMLROFFQ    LATE RUN?                                    
         BE    VOFF240                                                          
*                                                                               
         CLI   FMGTYPE,FMLBOFFQ                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,MLBMDTSH                                                      
         LA    R3,MLBMDT3H         COMPARE TO LAST DATE FIELD                   
         LA    RE,MLBMDT2H-MLBMDTSH                                             
         TM    FLAGS,FACTBONQ      PROCESSING BONUS PORTION                     
         BZ    VOFF240             OF LATE RUN W/ BONUS?                        
         LA    R2,MLBBDTSH                                                      
         LA    R3,MLBBDT3H                                                      
         LA    RE,MLBBDT2H-MLBBDTSH                                             
*                                                                               
VOFF240  DS    0H                                                               
         L     RF,OFFRDISP         BUMP TO NEXT SCREENLINE                      
         AR    RF,RE                                                            
         ST    RF,OFFRDISP         STORE IT BACK                                
*                                                                               
         AR    R2,RF               ADD DISPLACEMENT TO A(1ST LINE)              
         CR    R2,R3               END OF SCREEN REACHED?                       
         BH    VOFFX               YES - LAST LINE DONE - WRAP IT UP            
         TM    FLAGS,FACTADDQ                                                   
         BO    VOFF30                                                           
         B     VOFF120                                                          
*                                                                               
VOFFX    DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
* SAVE OFF FIRST OFFERED AIR DATE                                               
*                                                                               
SVFSTAIR NTR1                                                                   
         LA    R6,RMKGREC                                                       
         MVI   ELCODE,X'03'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RMKGDTEL,R6                                                      
         OC    FIRSTAIR,FIRSTAIR                                                
         BZ    SVFA10                                                           
         CLC   FIRSTAIR,RMKGDTST                                                
         BNH   SVFAX                                                            
*                                                                               
SVFA10   DS    0H                                                               
         MVC   FIRSTAIR,RMKGDTST                                                
         DROP  R6                                                               
*                                                                               
SVFAX    DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
* CHECK IF REP TO SPOT TRASNFER CANDIDATE AND INSERT APPROPRIATE                
* RTS ELEMENT                                                                   
*                                                                               
BLDRTSEL NTR1                                                                   
         LR    R4,RA                                                            
         AHI   R4,TWAWORKQ                                                      
         USING TWAWORK,R4                                                       
*                                                                               
         LA    R3,265                                                           
*                                                                               
         CLI   TWARTS,C'0'         ANY OPTIONAL RTS CONTRACT TYPE?              
         BE    BRTS10              NO  -                                        
         CLC   RCONTYPE,TWARTS     YES - CONTYPE = OPTIONAL TYPE?               
         BE    BRTS20              YES - TREAT AS RTS BUY                       
*                                  NOT OPTIONAL TYPE:  STANDARD TYPE?           
BRTS10   DS    0H                                                               
         CLI   RCONTYPE,C'X'                                                    
         BE    BRTS20                                                           
         CLI   RCONTYPE,C'N'                                                    
         BNE   CKRTSX                                                           
*                                                                               
BRTS20   DS    0H                                                               
         LH    R1,RMKGTSPT                                                      
         LTR   R1,R1               IF NEGATIVE SKIP CHECK                       
         BM    BRTS30                                                           
         CHI   R1,169              MAX OF 169 SPOTS                             
         BNH   BRTS30                                                           
         LA    R3,266                                                           
         B     ERROR                                                            
*                                                                               
BRTS30   DS    0H                                                               
         LA    R6,RMKGREC                                                       
         MVI   ELCODE,X'08'        X'08' ELEMENT MUST BE UNIQUE                 
         BRAS  RE,GETEL                                                         
         BE    CKRTSX                                                           
*                                                                               
         CLI   TWASPES,0                                                        
         BNE   BRTS40                                                           
                                                                                
* PROFILE TO ALLOW SPOTPAK INTERFACE DATA                                       
         TM    PROFILES+CNTSPOTB,CNTSPOTA                                       
         BZ    CKRTSX              IF OFF, SKIP SPOTPAK INTERFACE ADD           
         B     ERROR                                                            
                                                                                
BRTS40   DS    0H                                                               
         XC    ELEM,ELEM                                                        
ELM      USING RBUYSPEL,ELEM                                                    
         MVC   ELM.RBUYSPCD(2),=X'0830'                                         
         MVC   ELM.RBUYSPAG,TWASPAG    SPOTPAK AGENCY POWER CODE                
         MVC   ELM.RBUYSPMD,TWASPMD    SPOTPAK MEDIA CODE                       
         MVC   ELM.RBUYSPCL,TWASPCL    SPOTPAK CLIENT CODE                      
         MVC   ELM.RBUYSPPD,TWASPPD    SPOTPAK PRODUCT CODE                     
         MVC   ELM.RBUYSPES,TWASPES    SPOTPAK ESTIMATE NUMBER                  
         MVC   ELM.RBUYSPPP,TWASPPP    SPOTPAK PIGGY PRODUCT CODE               
         MVC   ELM.RBUYSPP1,TWASPP1    SPOTPAK PRODUCT 1 SPLIT                  
         MVC   ELM.RBUYSPP2,TWASPP2    SPOTPAK PRODUCT 2 SPLIT                  
         MVC   ELM.RBUYSPST,RCONKSTA   STATION CALL LETTERS                     
         MVC   ELM.RBUYSADV,RCONKADV   REPPAK ADVERTISER CODE                   
         MVC   ELM.RBUYSPRD,RCONPRD    REPPAK PRODUCT CODE                      
         DROP  ELM                                                              
*                                                                               
         GOTO1 VADDELEM,DMCB,RMKGREC,ELEM    ADD SPOTPAK INTERFACE ELEM         
*                                                                               
CKRTSX   DS    0H                                                               
         B     EXXMOD                                                           
         DROP  R4                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* - VALIDATE OFFERS                                                             
* - X'04' CR= AND X'05' ELEMENTS WILL BE ADDED LATER                            
*                                                                               
MGPVOFF  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*        LA    R3,1                MISSING                                      
*        LA    R2,MGPODTSH         MAKE SURE USER ENTER A DATE IN               
*        CLI   5(R2),0             MISSED DATE FIELD                            
*        BE    ERROR                                                            
*                                                                               
         LR    R4,RA                                                            
         AHI   R4,TWAWORKQ                                                      
         USING TWAWORK,R4                                                       
         MVC   LINECTR,TWASTRT#                                                 
         DROP  R4                                                               
*                                                                               
         XC    OFFRDISP,OFFRDISP                                                
         NI    FLAGS,X'FF'-X'20'   DEFAULT PUT TO TSAR BUFFER                   
*                                                                               
* IF CHANGE, RETRIEVE OFFER RECORD FROM TSAR BUFFER                             
*                                                                               
VMGPOF03 DS    0H                                                               
         LA    R2,MGPODTSH         NO MORE OFFER LINE TO VALIDATE?              
         A     R2,OFFRDISP                                                      
         CLI   5(R2),0             CHECK DATE FIELD                             
         BE    VMGPOF04                                                         
*                                                                               
         TM    FLAGS,FACTADDQ      SKIP FIELDS THAT USER                        
         BZ    VMGPOF05            WANTS TO DELETE DURING ADD                   
         CLC   =C'DEL',8(R2)                                                    
         BE    VMGPOF95                                                         
         B     VMGPOF05                                                         
*                                                                               
VMGPOF04 DS    0H                                                               
         LA    R3,SDTERR                                                        
         OC    OFFRDISP,OFFRDISP                                                
         BZ    ERROR                                                            
         B     VMGPOFX                                                          
*                                                                               
VMGPOF05 DS    0H                                                               
         XCEFL RMKGREC,1000                                                     
*                                                                               
         LR    R4,RA                                                            
         AHI   R4,TWAWORKQ                                                      
         USING TWAWORK,R4                                                       
*                                                                               
         XC    KEY,KEY                                                          
         MVI   RMKGKTYP,X'11'                                                   
         MVC   RMKGKREP,REPALPHA                                                
         MVC   RMKGKOFF,RCONKOFF   INSERT OFFICE                                
         MVC   RMKGKSTA,RCONKSTA   INSERT STATION+MEDIA                         
         MVC   RMKGKCON,TWACNUM    CONTRACT NUMBER                              
         MVC   RMKGKGRP,MGHMGRP    GROUP CODE                                   
         MVC   RMKGKPLN,=3X'FF'    SKIP GROUP RECORD                            
*        LA    R2,MGHMOFFH                                                      
*        GOTO1 VPACK                                                            
*        STC   R0,RMKGKLIN         MAKEGOOD OFFER NUMBER                        
         MVI   RMKGKLIN,1                                                       
*                                                                               
         TM    FLAGS,FACTADDQ                                                   
         BZ    VMGPOF20                                                         
         OI    FLAGS,X'20'         ADD RECORD TO TSAR BUFFER                    
*        CLI   OFFERNUM,0                                                       
*        BNE   VMGPOF20            NEXT OFFERNUM ALREADY CALCULATED?            
*        MVC   KEY(27),RMKGKEY                                                  
*        GOTO1 VHIGH                                                            
*                                                                               
VMGPOF08 DS    0H                  FIND HIGHEST OFFER NUMBER                    
*        CLC   KEY(RMKGKMLN-RMKGKEY),KEYSAVE                                    
*        BNE   VMGPOF10                                                         
*        CLC   RMKGKLIN,KEY+RMKGKLIN-RMKGKEY                                    
*        BH    *+10                                                             
*        MVC   RMKGKLIN,KEY+RMKGKLIN-RMKGKEY                                    
*        GOTO1 VSEQ                                                             
*        B     VMGPOF08                                                         
*                                                                               
VMGPOF10 DS    0H                  WILL USE NEXT HIGHEST OFFER NUMBER           
*        ZIC   RE,RMKGKLIN                                                      
*        AHI   RE,1                                                             
*        STC   RE,RMKGKLIN                                                      
*        EDIT  RMKGKLIN,(2,MGHMOFF),ALIGN=LEFT                                  
*        STC   R0,MGHMOFFH+5                                                    
*        OI    MGHMOFFH+4,X'08'    SET NUMERIC                                  
*        MVC   OFFERNUM,RMKGKLIN                                                
*                                                                               
VMGPOF20 DS    0H                                                               
         MVC   RMKGKRTY,LINECTR    LINE NUMBER                                  
         ZIC   RE,LINECTR                                                       
         AHI   RE,1                                                             
         STC   RE,LINECTR                                                       
*        CLI   MGHMOPT,C'C'        OFFERING ALL OR CHOICE?                      
*        BNE   VMGPOF30                                                         
*        OI    RMKGKRTY,X'10'                                                   
         DROP  R4                                                               
*                                                                               
VMGPOF30 DS    0H                                                               
         MVC   KEY(27),RMKGKEY                                                  
         XC    ELEM,ELEM                                                        
         LA    R2,MGPODTSH         A(1ST LINE OF MG OFFER)                      
*                                                                               
* MAKEGOOD OFFER ADD                                                            
*                                                                               
         TM    FLAGS,FACTADDQ                                                   
         BZ    VMGPOF40                                                         
         MVC   RMKGLEN,=H'34'                                                   
         MVC   ELEM(2),=X'012B'                                                 
MG01D    USING RMKGELEM,ELEM                                                    
         MVC   MG01D.RMKGCREA,TODAY                                             
         MVC   MG01D.RMKGKMOD,RCONMOD                                           
         MVC   MG01D.RMKGCHGI,=C'A '                                            
*                                                                               
         OI    MG01D.RMKGRTS,X'10' MARK PREEMPT/CREDIT                          
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL                                                         
         BNE   VMGPOF80                                                         
         USING RCONSEND,R6                                                      
         MVC   MG01D.RMKGVER,RCONSRV                                            
         B     VMGPOF80                                                         
         DROP  MG01D,R6                                                         
*                                                                               
* MAKEGOOD OFFER CHANGE                                                         
*                                                                               
VMGPOF40 DS    0H                                                               
         XCEFL TSARREC,1002                                                     
         MVC   TSARREC+2(27),RMKGKEY                                            
         MVI   TD.TSACTN,TSARDH    RETRIEVE FIRST DETAIL OFFER RECORD           
         GOTO1 =A(GOTSAR),RR=RELO                                               
         TM    TD.TSERRS,TSEEOF+TSERNF                                          
         BZ    VMGPOF70                                                         
         ZIC   R3,RMKGKRTY         RESET TO LAST RECORD NUMBER                  
         AHI   R3,-1                                                            
         STC   R3,RMKGKRTY         WILL BUMP TO NEW NUMBER BELOW                
         B     VMGPOF55                                                         
*                                                                               
VMGPOF50 DS    0H                                                               
*        XCEFL TSARREC,1002                                                     
         MVI   TD.TSACTN,TSANXT                                                 
         GOTO1 =A(GOTSAR),RR=RELO                                               
         TM    TD.TSERRS,TSEEOF+TSERNF                                          
         BZ    VMGPOF60                                                         
*                                                                               
VMGPOF55 DS    0H                                                               
         LA    R2,MGPODTSH                                                      
         A     R2,OFFRDISP                                                      
         CLI   5(R2),0                                                          
         BE    VMGPOFX             WE ARE DONE                                  
*                                                                               
* USER PUT IN A NEW OFFER LINE. ADD RECORD TO TSAR BUFFER                       
*                                                                               
         OI    FLAGS,X'20'         CHANGE MODE TO ADD                           
         ZIC   R3,RMKGKRTY                                                      
         AHI   R3,1                                                             
         XCEFL RMKGREC,1000                                                     
         MVC   RMKGREC(27),MGDTREC                                              
         STC   R3,RMKGKRTY         SET NEXT RECORD IN OFFER                     
*                                                                               
         MVC   RMKGLEN,=H'34'                                                   
         XC    ELEM,ELEM                                                        
         MVC   ELEM(2),=X'012B'                                                 
MG01D    USING RMKGELEM,ELEM                                                    
         MVC   MG01D.RMKGCREA,TODAY                                             
         MVC   MG01D.RMKGKMOD,RCONMOD                                           
         MVC   MG01D.RMKGCHGI,=C'A '                                            
*                                                                               
         OI    MG01D.RMKGRTS,X'10' MARK PREEMPT/CREDIT                          
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL                                                         
         BNE   VMGPOF80                                                         
         USING RCONSEND,R6                                                      
         MVC   MG01D.RMKGVER,RCONSRV                                            
         B     VMGPOF80                                                         
         DROP  MG01D,R6                                                         
*                                                                               
VMGPOF60 DS    0H                                                               
         CLC   TSARREC+2(RMKGKMLN-RMKGKEY),KEY                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VMGPOF70 DS    0H                                                               
         GOTO1 VMOVEREC,DMCB,TSARREC+2,RMKGREC                                  
         LA    R6,RMKGREC                                                       
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   R1,1(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),0(R6)                                                    
         GOTO1 VDELELEM,DMCB,(X'01',RMKGREC)                                    
*                                                                               
VMGPOF80 DS    0H                                                               
         GOTO1 VADDELEM,DMCB,RMKGREC,ELEM                                       
*                                                                               
         GOTO1 =A(SETCHOYS),RR=RELO                                             
*        GOTO1 =A(MISSCMNT),RR=RELO                                             
*                                                                               
         GOTO1 =A(CHECKDEL),RR=RELO                                             
         BZ    VMGPOF85            USER DELETING DETAIL LINE?                   
*                                                                               
         LA    R2,MGPOCMTH         SET A(DETAIL COMMENT)                        
         A     R2,OFFRDISP         OFFSET TO SPECIFIC LINE                      
         GOTOX =A(PRNMDTCM),X'11',RR=RELO                                       
*                                                                               
*                                                                               
         LA    R3,90               SET UP BUY LINE# FOR GETDEMV                 
         LA    R2,MGPOLN#H         SET A(BUY LINE#)                             
         A     R2,OFFRDISP         OFFSET TO SPECIFIC LINE                      
         CLI   5(R2),0                                                          
         BE    ERROR                                                            
         GOTO1 VPACK                                                            
         LTR   R0,R0                                                            
         BZ    ERROR                                                            
         STC   R0,TARGETBY                                                      
*                                                                               
         LA    R2,MGPODEMH         SET A(DEMO VALUE)                            
         A     R2,OFFRDISP         OFFSET TO SPECIFIC LINE                      
         GOTOR CKSTPROF                                                         
         BNE   VMGPOF85                                                         
         GOTOR GETDEMV             GET DEMO VALUE FROM INPUT                    
*                                                                               
*                                                                               
VMGPOF85 DS    0H                                                               
         MVI   TD.TSACTN,TSAADD    ADD RECORD                                   
         TM    FLAGS,X'20'         ADD TO TSAR BUFFER?                          
         BO    VMGPOF90                                                         
         MVI   TD.TSACTN,TSAPUT    PUT RECORD BACK BY NUMBER                    
*        NI    RMKGCNTL,X'FF'-X'80' SET ACTIVE                                  
*                                                                               
VMGPOF90 DS    0H                                                               
         GOTO1 VMOVEREC,DMCB,RMKGREC,TSARREC+2                                  
         SR    R1,R1                                                            
         ICM   R1,3,RMKGLEN        ADD LENGTH FIELD                             
         AHI   R1,2                                                             
         STCM  R1,3,TSARREC                                                     
*                                                                               
         GOTO1 =A(GOTSAR),RR=RELO                                               
         TM    TD.TSERRS,TSEEOF+TSEDUP                                          
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* FOR ADD, SAVE OFF FIRST DETAIL OFFER RECORD                                   
*                                                                               
         TM    FLAGS,FACTADDQ                                                   
         BZ    VMGPOF95                                                         
*                                                                               
         LR    R4,RA                                                            
         AHI   R4,TWAWORKQ                                                      
         USING TWAWORK,R4                                                       
         CLI   TWASTRT#,1                                                       
         BNE   VMGPOF95                                                         
         DROP  R4                                                               
*                                                                               
         GOTO1 VMOVEREC,DMCB,RMKGREC,MGDTREC                                    
*                                                                               
VMGPOF95 DS    0H                                                               
         LA    R2,MGPODTSH                                                      
         L     RF,OFFRDISP         BUMP TO NEXT SCREENLINE                      
         AHI   RF,MGPODT2H-MGPODTSH                                             
         ST    RF,OFFRDISP         STORE IT BACK                                
         LA    RE,MGPODT7H         COMPARE TO LAST DATE FIELD                   
*                                                                               
         AR    R2,RF               ADD DISPLACEMENT TO A(1ST LINE)              
         CR    R2,RE               END OF SCREEN REACHED?                       
         BH    VMGPOFX             YES - LAST LINE DONE - WRAP IT UP            
         TM    FLAGS,FACTADDQ                                                   
         BO    VMGPOF03                                                         
         B     VMGPOF50                                                         
*                                                                               
VMGPOFX  DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   SETCHOYS:  FIND THE X'20' ELEMENT IN THE RECORD, AND TURN ON                
*        THE CHOICE BIT.                                                        
*                                                                               
SETCHOYS NTR1  BASE=*,LABEL=*                                                   
         XC    ELEM,ELEM                                                        
         LA    R6,RMKGREC                                                       
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL                                                         
         BNE   SETC10                                                           
         ZIC   R1,1(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),0(R6)                                                    
         GOTO1 VDELELEM,DMCB,(X'20',RMKGREC)                                    
         B     SETC20                                                           
*                                                                               
SETC10   DS    0H                                                               
MGSTD    USING RMKGSTEL,ELEM                                                    
         MVI   MGSTD.RMKGSTCD,X'20'                                             
         MVI   MGSTD.RMKGSTLN,10                                                
*                                                                               
* SET APPROPRIATE CHOICE FLAGS                                                  
*                                                                               
SETC20   DS    0H                                                               
*                                                                               
*                                                                               
         GOTO1 VADDELEM,DMCB,RMKGREC,ELEM                                       
*                                                                               
         B     EXXMOD                                                           
         DROP  MGSTD                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
* VALIDATE MISSED LINE COMMENT                                                  
*                                                                               
*&&DO                                                                           
MISSCMNT NTR1  BASE=*,LABEL=*                                                   
         LA    R2,MGHMLCMH         SET A(MISSED LINE COMMENT)                   
*                                                                               
* PROCESS FOR MISSED LINE COMMENT                                               
*                                                                               
         TM    4(R2),X'20'         PREVALID SET ON COMMENT?                     
         BNZ   MISCX                                                            
         XC    ELEM,ELEM                                                        
*                                                                               
         LA    R6,RMKGREC                                                       
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BNE   MISC20                                                           
*                                  SAVE PREVIOUS DATE/TIME STAMP                
         USING RMKGCDEL,R6                                                      
         MVC   ELEM(RMKGCDDS-RMKGCDEL),RMKGCDEL                                 
         MVI   ELEM+1,RMKGCDDS-RMKGCDEL                                         
         DROP  R6                                                               
*                                                                               
MISC10   DS    0H                                                               
         GOTO1 VDELELEM,DMCB,(X'10',RMKGREC)                                    
*                                  DROP PREVIOUS MISSED LINE COMMENT            
         B     MISC50                                                           
*                                                                               
MISC20   DS    0H                                                               
         LA    R6,ELEM                                                          
         USING RMKGCDEL,R6                                                      
         MVI   RMKGCDCD,X'10'      INSERT ELEMENT CODE                          
         MVI   RMKGCDLN,RMKGCDDS-RMKGCDEL                                       
         GOTO1 DATCON,DMCB,(5,DUB),(2,RMKGCDDT)                                 
*                                  INSERT DATE CREATED                          
         MVC   RMKGCDDU,RMKGCDDT                                                
*                                  LAST UPDATE = DATE CREATED                   
         TIME  DEC                 GET THE TIME CREATED                         
*                                  CURRENT TIME IN R0: HH:MM:SS:TT              
         STCM  R0,4,RMKGCDTM+1                                                  
*                                  STORE MM IN AREA                             
         SRL   R0,24               SAVE ONLY HOURS                              
         STCM  R0,1,DUB            STORE HH                                     
         GOTO1 HEXOUT,DMCB,DUB,FULL,1,=C'TOG'                                   
*                                  CONVERT TO EBCDIC VALUE                      
         PACK  DUB(8),FULL(2)      PACK EBCDIC VALUE                            
         CVB   RF,DUB              CONVERT IT TO BINARY                         
         LA    RF,DDSTMADJ(RF)     ADD 8 HR ADJUSTMENT                          
         EDIT  (RF),(2,FULL),FILL=0                                             
*                                  MAKE HH EBCDIC                               
         GOTO1 HEXIN,DMCB,FULL,RMKGCDTM,2                                       
*                                  STRIP ZONES FROM EBCDIC                      
         MVC   RMKGCDTU,RMKGCDTM                                                
*                                  TIME LAST UPDATED = TIME CREATED             
         DROP  R6                                                               
*                                                                               
MISC50   DS    0H                                                               
         CLI   5(R2),0             ANY INPUT?                                   
         BE    MISC60              NO                                           
         ZIC   RF,5(R2)            GET LENGTH OF INPUT                          
         BCTR  RF,0                -1 FOR EX                                    
         EX    RF,*+8              MOVE COMMENT BY LENGTH                       
         B     *+10                                                             
         MVC   ELEM+10(0),8(R2)                                                 
         LA    RF,11(RF)           ADD BACK EX + 10 FOR CONTROL                 
         STC   RF,ELEM+1           INSERT LENGTH                                
*                                                                               
MISC60   DS    0H                                                               
         GOTO1 VADDELEM,DMCB,RMKGREC,ELEM                                       
*                                                                               
MISCX    DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*&&                                                                             
*                                                                               
* CHECK IF USER WANTS TO DELETE THIS DETAIL LINE                                
*                                                                               
CHECKDEL NTR1  BASE=*,LABEL=*                                                   
         TM    FLAGS,FACTADDQ      NOT VALID IF ADDING NEW OFFER                
         BO    CDELNO                                                           
*                                                                               
         LA    R2,MGSODTSH                                                      
         CLI   FMGTYPE,FMGMOFFQ    REGULAR?                                     
         BE    CDEL10                                                           
*                                                                               
         LA    R2,RNAODTSH                                                      
         CLI   FMGTYPE,FRNAOFFQ    REPLACEMENT NA?                              
         BE    CDEL10                                                           
*                                                                               
         LA    R2,MGBODTSH                                                      
         CLI   FMGTYPE,FMGBOFFQ    BONUS?                                       
         BE    CDEL10                                                           
*                                                                               
         LA    R2,MGPODTSH                                                      
         CLI   FMGTYPE,FMGPOFFQ    BONUS?                                       
         BE    CDEL10                                                           
*                                                                               
         LA    R2,MLRODTSH                                                      
         CLI   FMGTYPE,FMLROFFQ    LATE RUN?                                    
         BE    CDEL10                                                           
*                                                                               
         LA    R2,MLBMDTSH                                                      
         CLI   FMGTYPE,FMLBOFFQ    LATE RUN W/BONUS?                            
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    FLAGS,FACTBONQ      PROCESSING BONUS PORTION                     
         BZ    *+8                 OF LATE RUN W/ BONUS?                        
         LA    R2,MLBBDTSH                                                      
*                                                                               
CDEL10   DS    0H                                                               
         A     R2,OFFRDISP                                                      
*                                                                               
         CLC   =C'DEL',8(R2)                                                    
         BNE   CDELNO                                                           
         OI    RMKGCNTL,X'80'      MARK DELETE                                  
         OI    FLAGS,FACTDELQ                                                   
*                                                                               
CDELYES  SR    RC,RC                                                            
CDELNO   LTR   RC,RC                                                            
CDELX    B     EXXMOD                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* USER IS DELETING AT LEAST ONE LINE, NEED TO RESEQUENCE THE DETAIL             
* RECORDS IN TSAR BEFORE WE PROCEED                                             
*                                                                               
RENUMBER NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   OFFREC#,1                                                        
*                                                                               
         TM    FMGTYPE,FMLBOFFQ                                                 
         BZ    RENUM10             LATE RUN BONUS OFFER RECORDS                 
         TM    FLAGS,FACTBONQ      STARTS AT RECORD NUMBER 4                    
         BZ    RENUM10                                                          
         MVI   OFFREC#,4                                                        
*                                                                               
RENUM10  DS    0H                                                               
         XCEFL TSARREC,1002                                                     
         MVI   TSARREC+2,X'11'                                                  
         MVI   TD.TSACTN,TSARDH    RETRIEVE MAKEGOOD HEADER RECORD              
         GOTO1 =A(GOTSAR),RR=RELO                                               
         TM    TD.TSERRS,TSEEOF                                                 
         BZ    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
RENUM20  DS    0H                                                               
MGRECD   USING RMKGREC,TSARREC+2                                                
         MVI   TD.TSACTN,TSANXT    GET NEXT MAKEGOOD DETAIL RECORD              
         GOTO1 =A(GOTSAR),RR=RELO                                               
         TM    TD.TSERRS,TSEEOF                                                 
         BNZ   RENUM50                                                          
*                                                                               
* SPECIAL CHECK FOR LATE RUN WITH BONUS                                         
*                                                                               
RENUM23  DS    0H                                                               
         TM    FMGTYPE,FMLBOFFQ                                                 
         BZ    RENUM30             LATE RUN BONUS OFFER RECORDS                 
         TM    FLAGS,FACTBONQ      STARTS AT RECORD NUMBER 4                    
         BZ    RENUM25                                                          
         TM    MGRECD.RMKGRTS,X'20'                                             
         BZ    RENUM20             SKIP AHEAD TO BONUS RECORD                   
         B     RENUM30                                                          
*                                                                               
RENUM25  DS    0H                  LATE RUN STOPS AT BONUS RECORD               
         TM    MGRECD.RMKGRTS,X'20'                                             
         BO    RENUM50                                                          
*                                                                               
RENUM30  DS    0H                                                               
         TM    MGRECD.RMKGCNTL,X'80' USER DELETED?                              
         BZ    RENUM40                                                          
*                                                                               
         MVI   TD.TSACTN,TSADEL    DELETE TSAR RECORD                           
         GOTO1 =A(GOTSAR),RR=RELO                                               
         TM    TD.TSERRS,TSEEOF+TSERNF                                          
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   TD.TSACTN,TSAGET    NEXT RECORD WILL NOW HAVE THE                
         GOTO1 =A(GOTSAR),RR=RELO  CURRENT DELETED RECORD'S INDEX               
         TM    TD.TSERRS,TSEEOF    UNLESS WE ARE AT THE EOF                     
         BNZ   RENUM50                                                          
         B     RENUM23                                                          
*                                                                               
RENUM40  DS    0H                                                               
         MVI   TD.TSACTN,TSADEL    DELETE TSAR RECORD                           
         GOTO1 =A(GOTSAR),RR=RELO                                               
         TM    TD.TSERRS,TSEEOF+TSERNF                                          
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   MGRECD.RMKGKRTY,OFFREC#                                          
*                                                                               
         MVI   TD.TSACTN,TSAADD    ADD RECORD BACK WITH SEQUENCED               
         GOTO1 =A(GOTSAR),RR=RELO  DETAIL REOCRD NUMBER                         
         TM    TD.TSERRS,TSEEOF+TSEDUP                                          
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ZIC   RE,OFFREC#                                                       
         AHI   RE,1                                                             
         STC   RE,OFFREC#                                                       
*                                                                               
         B     RENUM20                                                          
*                                                                               
RENUM50  DS    0H                                                               
         LA    R2,CONCACTH                                                      
         LA    R3,854              USE MGX TO DELETE ENTIRE OFFER               
         CLI   OFFREC#,1                                                        
         BE    ERROR                                                            
*                                                                               
         TM    FMGTYPE,FMLBOFFQ                                                 
         BZ    RENUMX              LATE RUN BONUS OFFER RECORDS                 
         TM    FLAGS,FACTBONQ      STARTS AT RECORD NUMBER 4                    
         BZ    RENUMX                                                           
         LA    R2,MLBBDTSH                                                      
         LA    R3,855              MUST HAVE AT LEAST ONE BONUS LINE            
         CLI   OFFREC#,4                                                        
         BE    ERROR                                                            
*                                                                               
RENUMX   DS    0H                                                               
         B     EXXMOD                                                           
         DROP  MGRECD                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* FOR LATE RUN OFFERS, COPY LENGTH, RATE, AND DAY FROM TARGET BUY               
*                                                                               
LREDIT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,MLROLN#H         FIND TARGET BUY IN TSAR                      
         CLI   FMGTYPE,FMLBOFFQ                                                 
         BNE   *+8                                                              
         LA    R2,MLBMLN#H                                                      
*                                                                               
         A     R2,OFFRDISP                                                      
*                                                                               
         GOTO1 VPACK                                                            
         LTR   R0,R0                                                            
         BNZ   LRED05                                                           
         OC    OFFRDISP,OFFRDISP                                                
         BZ    ERROR                                                            
         B     LREDX               NO INPUT, EXIT                               
*                                                                               
LRED05   DS    0H                                                               
         STC   R0,TARGETBY                                                      
*                                                                               
         MVC   SVKEY,TSARREC+2     SAVE FOR RESEQUENCING LATER                  
*                                                                               
         XCEFL TSARREC,1002                                                     
         MVI   TSARREC+2,X'0B'                                                  
         MVI   TD.TSACTN,TSARDH    RETRIEVE FIRST BUY                           
         GOTO1 =A(GOTSAR),RR=RELO                                               
         TM    TD.TSERRS,TSEEOF                                                 
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
LRED10   DS    0H                                                               
         LA    R6,TSARREC+2                                                     
TBUYD    USING RBUYREC,R6                                                       
         CLC   TBUYD.RBUYKLIN,TARGETBY                                          
         BE    LRED20                                                           
*                                                                               
         XCEFL TSARREC,1002                                                     
         MVI   TD.TSACTN,TSANXT                                                 
         GOTO1 =A(GOTSAR),RR=RELO                                               
         TM    TD.TSERRS,TSEEOF                                                 
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLI   TSARREC+2,X'0B'                                                  
         BE    LRED10                                                           
         DC    H'0'                                                             
*                                                                               
LRED20   DS    0H                                                               
         MVC   RMKGNW,TBUYD.RBUYNW     SPOTS/WEEK                               
         MVC   RMKGCOS,TBUYD.RBUYCOS   RATE                                     
         MVC   RMKGSTED,TBUYD.RBUYSTED START END DATES                          
         MVC   RMKGDUR,TBUYD.RBUYDUR   LENGTH                                   
*                                                                               
         MVI   ELCODE,X'02'        DAY/TIME ELEMENTS                            
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* REFRESH DAY/TIME ELEMENT IN MAKEGOOD OFFER FROM TARGET BUY                    
*                                                                               
         GOTO1 VDELELEM,DMCB,(X'02',RMKGREC)                                    
*                                                                               
         LA    R2,MLROTMSH                                                      
         CLI   FMGTYPE,FMLBOFFQ                                                 
         BNE   *+8                                                              
         LA    R2,MLBMTMSH                                                      
         A     R2,OFFRDISP                                                      
*                                                                               
LRED30   DS    0H                                                               
         BAS   RE,TIMEEDIT                                                      
*                                                                               
         GOTO1 VADDELEM,DMCB,RMKGREC,(R6)                                       
         BRAS  RE,NEXTEL                                                        
         BE    LRED30                                                           
*                                                                               
         MVC   TSARREC+2(27),SVKEY REESTABLISH SEQ ORDER IN TSAR                
         MVI   TD.TSACTN,TSARDH                                                 
         GOTO1 =A(GOTSAR),RR=RELO                                               
*                                                                               
LREDX    DS    0H                                                               
         B     EXXMOD                                                           
         DROP  TBUYD                                                            
         EJECT                                                                  
*                                                                               
*                                                                               
* FOR LATE RUN:                                                                 
* VALIDATE TIME FIELD AND INSERT ACTUAL TIME IN X'02' DAY/TIME ELEMENT          
*                                                                               
* R2 IS POINTING TO ACTUAL TIME FIELD TO BE VALIDATED                           
* R6 IS POINTING TO X'02' DAY/TIME ELEMENT                                      
*                                                                               
TIMEEDIT NTR1                                                                   
         USING RMKGDYEL,R6                                                      
*                                                                               
         LA    R3,TIMERR                                                        
         CLI   5(R2),0             ANY VALUE IN FIELD?                          
         BE    ERROR               NO  - ERROR                                  
*                                                                               
         LA    RE,8(R2)                                                         
         ST    RE,DMCB                                                          
         MVC   DMCB(1),5(R2)                                                    
*        GOTO1 =V(RETIMVAL),DMCB,,RMKGDYT1,RR=RELO                              
         GOTOX (RFTIMVAL,VREPFACS),DMCB,,RMKGDYT1                               
         CLI   DMCB,X'FF'          TIME INVALID?                                
         BE    ERROR                                                            
*                                                                               
         OC    RMKGDYT2,RMKGDYT2   IS THERE AN END TIME?                        
         BZ    TEDITX              NO                                           
*                                                                               
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
*                                                                               
         B     TEDITX              BYPASS AM VALIDATION                         
*                                                                               
         TM    TWATIME,X'80'       CHECK IF B'CAST DAY SET TO USE               
         BZ    TEDIT10             6AM - 559AM                                  
         DROP  RF                                                               
*                                                                               
         CLC   RMKGDYT1,=H'0600'   START TIME LT 6AM?                           
         BNL   TEDITX              NO                                           
         CLC   RMKGDYT1,=H'0600'   END TIME GT = 6AM?                           
         BL    TEDITX              NO                                           
         LA    R3,344              END TIME MUST BE W/IN B'CAST DAY             
         B     ERROR                                                            
*                                                                               
TEDIT10  EQU    *                                                               
         CLC   RMKGDYT1,=H'0500'   START TIME LT 5AM?                           
         BNL   TEDITX              NO                                           
         CLC   RMKGDYT2,=H'0500'   END TIME GT = 5AM?                           
         BL    TEDITX              NO                                           
         LA    R3,344              END TIME MUST BE W/IN B'CAST DAY             
         B     ERROR                                                            
*                                  ADD DAY-TIME ELEMENT TO MKGREC               
TEDITX   DS    0H                                                               
         B     EXXMOD                                                           
         DROP  R6                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* FOR LATE RUN OFFERS, FIGURE OUT WHAT THE LATE RUN DAY IS AND INSERT           
* INTO RECORD                                                                   
*                                                                               
LRDAYGEN NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   LRDAY,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ZIC   R1,LRDAY            START AND END DAY SHOULD BE THE              
         SLL   R1,4                SAME FOR LATE RUN                            
         STC   R1,RMKGSTED                                                      
         MVN   RMKGSTED,LRDAY                                                   
*                                                                               
         LA    R6,RMKGREC                                                       
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RMKGDYEL,R6                                                      
*                                                                               
LRDG10   DS    0H                                                               
         MVC   RMKGDYIN,RMKGSTED                                                
*                                                                               
         ZIC   RF,RMKGDYIN                                                      
         SRL   RF,4                                                             
         ZIC   RE,=X'80'                                                        
         SRL   RE,1                                                             
         BCT   RF,*-4                                                           
*                                                                               
         LA    R3,MG2ERR                                                        
         MVC   DAYBITS,RMKGDAYS    DAYS BITS OF TARGET BUY                      
         STC   RE,RMKGDAYS         DAY BIT OF LATE RUN                          
         OC    RMKGDAYS,DAYBITS    MAKE SURE LATE RUN DAY FALLS WITHIN          
         CLC   RMKGDAYS,DAYBITS    TARGET BUYS' DAYS                            
*&&LR                                                                           
         BE    LRDG20                                                           
         BRAS  RE,NEXTEL                                                        
         BE    LRDG10                                                           
         B     ERROR                                                            
*&&                                                                             
*                                                                               
         BNE   ERROR                                                            
*                                                                               
LRDG20   DS    0H                                                               
         STC   RE,RMKGDAYS         RESTORE LATE RUN DAY                         
*&&LR                                                                           
* REMOVE ALL OTHER DAY/TIME ELEMENTS BUT THE MATCHING ONE                       
* SINCE LATE RUN CAN ONLY BE SPECIFIED BY ONE DAY/TIME ELEMENT                  
         XC    ELEM,ELEM                                                        
         ZIC   R1,1(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),0(R6)                                                    
*                                                                               
         GOTO1 VDELELEM,DMCB,(X'02',RMKGREC)                                    
*                                                                               
         GOTO1 VADDELEM,DMCB,RMKGREC,ELEM                                       
*&&                                                                             
*                                                                               
LRDGX    DS    0H                                                               
         B     EXXMOD                                                           
         DROP  R6                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* VALIDATE DATES AND SPOTS                                                      
*                                                                               
DTSPEDIT NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,MGSODTSH                                                      
         CLI   FMGTYPE,FMGMOFFQ    REGULAR?                                     
         BE    DATD0030                                                         
*                                                                               
         LA    R2,RNAODTSH                                                      
         CLI   FMGTYPE,FRNAOFFQ    REPLACEMENT NA?                              
         BE    DATD0030                                                         
*                                                                               
         LA    R2,MGBODTSH                                                      
         CLI   FMGTYPE,FMGBOFFQ    BONUS?                                       
         BE    DATD0030                                                         
*                                                                               
         LA    R2,MLRODTSH                                                      
         CLI   FMGTYPE,FMLROFFQ    LATE RUN?                                    
         BE    DATD0030                                                         
*                                                                               
         LA    R2,MLBMDTSH                                                      
         CLI   FMGTYPE,FMLBOFFQ    LATE RUN W/BONUS?                            
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    FLAGS,FACTBONQ      PROCESSING BONUS PORTION                     
         BZ    *+8                 OF LATE RUN W/ BONUS?                        
         LA    R2,MLBBDTSH                                                      
*                                                                               
DATD0030 EQU   *                                                                
         A     R2,OFFRDISP                                                      
*                                                                               
         XC    MGTOTSPT,MGTOTSPT   CLEAR TOTAL SPOTS FOR LINE                   
         XC    MGTOTWKS,MGTOTWKS   CLEAR TOTAL WEEKS FOR LINE                   
*                                                                               
*                                  DELETE ALL PREV DATE ELTS (CHANGE)           
         GOTO1 VDELELEM,DMCB,(3,RMKGREC)                                        
*                                  SET UP 2 DATE I/P FLDS IN WORK3              
*                                     PRETEND 1 FIELD                           
*                                                                               
         MVC   WORK3(L'MGSODTS+8),0(R2)                                         
         ZIC   RE,5(R2)            INSERT LENGTH OF FIELD                       
*                                  LEN OF DATE FIELD                            
         LA    R4,WORK3+8(RE)      END OF DATE FIELD                            
*                                                                               
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),REPALPHA                                                
         GOTO1 (RFKFLT,VREPFACS),DMCB,RCONREC,WORK,0,DUB                        
*                                                                               
         MVC   WORK2(2),=X'030B'   DATE ELEM CODE + LEN                         
*                                                                               
*                                  SET EARLIEST START DATE/LATEST               
*                                     END DATE IN DAY ELTS                      
         SR    R5,R5                                                            
         ZIC   R4,RMKGSTED                                                      
         SRDL  R4,4                                                             
         SRL   R5,28                                                            
         STM   R4,R5,DMCB+16                                                    
*                                                                               
         XC    WORK+24(6),WORK+24                                               
         LA    R7,WORK3+7                                                       
         ST    R7,DMCB+12                                                       
DATD0040 EQU   *                   EDIT START DATE                              
         MVC   DMCB(4),DMCB+12                                                  
         LA    R3,SDTERR                                                        
         GOTO1 =A(SCAN),DMCB,,WORK3,RR=RELO SCAN FOR NEXT DATE FIELD            
*                                                                               
         CLI   DMCB,0              ANY DATE                                     
         BNE   DATD0060                                                         
*                                  NO DATE                                      
         OC    WORK+24(6),WORK+24  NO DATES GIVEN?                              
         BZ    ERROR                                                            
         B     DATDX                                                            
DATD0060 EQU   *                                                                
         L     R5,DMCB             FIELD ADDR                                   
         MVC   DMCB+12(4),DMCB     SAVE LEN + ADDR FOR SCAN                     
         CLC   0(2,R5),=C'S-'      CONTRACT START INDICATOR                     
         BNE   DATD0080                                                         
*                                                                               
*                                  GET DATE IN FIRST WEEK OF CONTRACT           
         GOTO1 GETDAY,DMCB,WORK,FULL                                            
*                                                                               
         CLC   FULL(3),MYSPACES    VALID CONTRACT START DATE?                   
         BNE   *+6                                                              
         DC    H'0'                CONTRACT ERROR                               
*                                                                               
         ZIC   RE,DMCB             DAY OF WEEK                                  
         L     R4,DMCB+16          BUY START DAY                                
         SR    R4,RE                                                            
         BNM   *+8                 BNM                                          
         A     R4,=F'7'            NEXT WEEK                                    
         GOTO1 ADDAY,DMCB,WORK,WORK+12,(R4)                                     
*                                  GET FIRST DATE                               
         LA    R5,2(R5)            NEXT FIELD                                   
*                                                                               
         B     DATD0220                                                         
DATD0080 EQU   *                                                                
*                                                                               
*                                  EDIT START DATE                              
*                                                                               
         GOTO1 DATVAL,DMCB,(1,(R5)),WORK+12                                     
*                                                                               
         L     R7,DMCB             FIELD LENGTH                                 
         LTR   R7,R7                                                            
         BZ    ERROR                                                            
*                                                                               
         LA    R5,1(R7,R5)         NEXT FIELD                                   
         MVC   WORK+12(2),WORK     CONTRACT START YEAR                          
         CLC   WORK+14(4),WORK+2   BUY MMDD V CONTRACT MMDD                     
         BNL   DATD0120                                                         
DATD0100 EQU   *                   BUY MMDD LOW                                 
         CLC   WORK(2),WORK+6      CONTRACT START AND END YEARS SAME?           
         BE    ERROR                                                            
         MVC   WORK+12(2),WORK+6   USE CONTRACT END YEAR                        
DATD0120 EQU   *                                                                
         GOTO1 GETDAY,DMCB,WORK+12,FULL                                         
*                                  VALIDATE START DATE                          
*                                                                               
         CLC   FULL(3),MYSPACES                                                 
         BE    ERROR                                                            
*                                                                               
* FOR LATE RUNS, DON'T VALIDATE THE DAY SINCE IT MAY NOT MATCH THE              
* TARGET'S. WE'LL FIGURE OUT THE ACTUAL DAY LATER ON                            
*                                                                               
         MVC   LRDAY,DMCB                                                       
*                                                                               
         TM    FLAGS,FACTBONQ      PROCESSING BONUS PORTION                     
         BO    DATD0130            OF LATE RUN W/ BONUS?                        
         CLI   FMGTYPE,FMLROFFQ    LATE RUN USES TARGET                         
         BE    DATD0220            RATE, LENGTH AND DAY/TIME                    
         CLI   FMGTYPE,FMLBOFFQ                                                 
         BE    DATD0220                                                         
*                                                                               
DATD0130 EQU   *                                                                
         LA    R3,SDYERR                                                        
         ZIC   R4,DMCB             START DAY                                    
         C     R4,DMCB+16          SAME AS 1ST DAY?                             
         BE    DATD0220                                                         
         CLC   WORK+12(2),WORK+6   BUY YEAR SAME AS END YEAR?                   
         BNE   DATD0100            FOR CONTRACTS MORE THAN 1 CALENDAR           
*                                     YEAR                                      
         B     ERROR                                                            
*                                                                               
DATD0220 EQU   *                                                                
         XC    WORK2+8(2),WORK2+8                                               
         CLC   WORK+12(6),WORK+6   BUY START VS CONTRACT END                    
         BNH   *+12                                                             
         LA    R3,SDTERR                                                        
         B     ERROR                                                            
         CLC   WORK+12(6),WORK+24  BUY STRT DATE VS LAST ELT END DATE           
         BH    *+12                                                             
         LA    R3,SDTERR                                                        
         B     ERROR                                                            
*                                                                               
*              EDIT END DATE                                                    
*              END DATE                                                         
*                                                                               
         TM    FLAGS,FACTBONQ      PROCESSING BONUS PORTION                     
         BO    DATD0230            OF LATE RUN W/ BONUS?                        
         CLI   FMGTYPE,FMLROFFQ    LATE RUN DOES NOT HAS END DATE               
         BE    DATD0225            SKIP VALIDATION                              
         CLI   FMGTYPE,FMLBOFFQ                                                 
         BNE   DATD0230                                                         
*                                                                               
* LATE RUN END DATE IS SAME AS START DATE SINCE ONLY ONE DATE IS                
* EXPECTED ANYWAY                                                               
*                                                                               
DATD0225 EQU   *                                                                
         LA    R3,869                                                           
         CLI   0(R5),0             END DATE NOT EXPECTED                        
         BNE   ERROR                                                            
         MVC   WORK+18(6),WORK+12                                               
         B     DATD0460                                                         
*                                                                               
DATD0230 EQU   *                                                                
         CLI   0(R5),C'E'          -E INDICATOR?                                
         BNE   DATD0240                                                         
*                                  GET END DATE FROM CONTRACT END DATE          
         GOTO1 GETDAY,DMCB,WORK+6,FULL                                          
         CLC   FULL(3),MYSPACES    ERROR?                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         ZIC   R4,DMCB             CONTRACT END DAY                             
*                                                                               
         S     R4,DMCB+20          BUY END DAY                                  
         BNM   *+8                 BNM                                          
         A     R4,=F'7'            PREVIOUS WEEK                                
         LNR   R4,R4                                                            
*                                  BACK UP CONTRACT END DATE TO                 
*                                     LAST BUY DATE                             
         GOTO1 ADDAY,DMCB,WORK+6,WORK+18,(R4)                                   
*                                                                               
         LA    R5,1(R5)                                                         
         B     DATD0440                                                         
DATD0240 EQU   *                   EDIT END DATE                                
         CLI   0(R5),C'*'          NO END DATE?                                 
         BE    DATD0280                                                         
         LR    R6,R5                                                            
         BCTR  R6,R0                                                            
         CLI   0(R6),C'*'                                                       
         BE    DATD0260                                                         
***      CLI   0(R6),C'('                                                       
***      BE    DATD0260                                                         
         CLI   0(R6),0                                                          
         BE    DATD0260                                                         
         CLI   0(R6),C' '                                                       
         BNE   DATD0300                                                         
DATD0260 EQU   *                   NO END DATE GIVEN                            
         LR    R5,R6                                                            
DATD0280 EQU   *                   NO END DATE GIVEN                            
         LM    R6,R7,DMCB+16       START AND END DAYS                           
         CR    R6,R7               END IN NEXT WEEK?                            
         BNH   *+8                                                              
         LA    R7,7(R7)                                                         
         SR    R7,R6                                                            
         GOTO1 ADDAY,DMCB,WORK+12,WORK+18,(R7)                                  
         B     DATD0460                                                         
DATD0300 EQU   *                                                                
         GOTO1 DATVAL,DMCB,(1,(R5)),WORK+18 END DATE                            
*                                                                               
         L     RE,DMCB             LENGTH                                       
         LTR   RE,RE                                                            
         BNZ   DATD0400                                                         
*                                                                               
*                                  CHECK FOR END WEEKS OPTION                   
         LA    R4,1                                                             
         CLI   1(R5),C'W'          WEEKS IND?                                   
         BE    DATD0320                                                         
         LA    R4,2                                                             
         CLI   2(R5),C'W'                                                       
         BE    *+12                                                             
         LA    R3,EDTERR                                                        
         B     ERROR                                                            
DATD0320 EQU   *                   W HAS BEEN ENTERED - PACK WEEKS              
         LR    R7,R5                                                            
         LR    R3,R5                                                            
         LA    R5,1(R4,R5)         END OF FIELD                                 
         LR    R6,R4                                                            
DATD0340 EQU   *                                                                
         CLI   0(R7),X'F0'         NUMERIC?                                     
         BNL   *+12                                                             
         LA    R3,EDTERR                                                        
         B     ERROR                                                            
         CLI   0(R7),X'F9'                                                      
         BNH   *+12                                                             
         LA    R3,EDTERR                                                        
         B     ERROR                                                            
         LA    R7,1(R7)                                                         
         BCT   R4,DATD0340                                                      
*                                  NUMERIC WEEKS ENTERED                        
         BCTR  R6,R0                                                            
         EX    R6,*+8                                                           
         B     *+10                PACK WEEKS                                   
         PACK  DUB,0(0,R3)                                                      
         CVB   R4,DUB                                                           
         LTR   R4,R4                                                            
         BNZ   *+12                                                             
         LA    R3,EDTERR                                                        
         B     ERROR                                                            
         MVC   WORK+30(6),WORK+12  START DATE                                   
         MVC   WORK+18(6),WORK+12  START TO END                                 
         OI    WORK2+8,X'80'       EVERY WEEK                                   
         LA    R3,7                                                             
         BCTR  R4,R0               NUMBER OF WEEKS                              
         LTR   R4,R4                                                            
         BZ    DATD0380                                                         
*                                                                               
*                                  TEST FOR ALTERNATE WEEKS                     
         CLI   0(R5),C'A'                                                       
         BNE   DATD0360                                                         
         OI    WORK2+8,X'40'                                                    
         NI    WORK2+8,X'7F'                                                    
         LA    R5,1(R5)                                                         
         LA    R3,14                                                            
DATD0360 EQU   *                   GET NEXT WEEK                                
         GOTO1 ADDAY,DMCB,WORK+30,WORK+18,(R3)                                  
         MVC   WORK+30(6),WORK+18                                               
         BCT   R4,DATD0360         GET NUMBER OF WEEKS-1                        
*                                                                               
DATD0380 EQU   *                   GET DAY SPAN FOR WEEK                        
         L     R6,DMCB+20          END DAY OF WEEK                              
         C     R6,DMCB+16          END V START DAY                              
         BNL   *+8                                                              
         LA    R6,7(R6)            OUT OF WEEK ROTATOR                          
         S     R6,DMCB+16          GET DAY SPAN                                 
         GOTO1 ADDAY,(R1),WORK+30,WORK+18,(R6)                                  
         B     DATD0480                                                         
*                                                                               
DATD0400 EQU   *                   END DATE IS VALID MONTH-DAY                  
         MVC   WORK+18(2),WORK+6   CONTRACT END YEAR                            
         CLC   WORK+20(4),WORK+8   BUY END MMDD VS CONTRACT END MMDD            
         BNH   *+10                                                             
         MVC   WORK+18(2),WORK     MOVE CONTRACT START YEAR                     
*                                                                               
         LA    R5,0(RE,R5)         FIELD END                                    
*                                                                               
*                                  VALIDATE END DATE                            
         GOTO1 GETDAY,DMCB,WORK+18,FULL                                         
         CLC   FULL(3),MYSPACES                                                 
         BNE   *+12                                                             
         LA    R3,EDTERR                                                        
         B     ERROR                                                            
*                                                                               
         ZIC   R4,DMCB             DAY OF WEEK                                  
*                                                                               
         C     R4,DMCB+20          SAME DAY AS END DAY?                         
         BE    DATD0440                                                         
*                                  FLIGHT BUY NEED NOT HAVE PROPER              
*                                     END DATE - FIND WK, ADJUST DATE           
         L     R7,DMCB+20          END DAY                                      
*                                  GET CONTRACT END DAY                         
         GOTO1 GETDAY,DMCB,WORK+6,FULL                                          
         CLC   FULL(3),MYSPACES                                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         ZIC   R6,DMCB                                                          
         LR    R3,R7                                                            
*                                                                               
         SR    R7,R4               DAY - DATE DAY                               
*                                                                               
DATD0420 EQU   *                                                                
         GOTO1 ADDAY,(R1),WORK+18,DUB,(R7)                                      
         MVC   WORK+18(6),DUB                                                   
*                                                                               
         CLC   WORK+18(6),WORK+6   DATE VS CONTRACT END                         
         BNH   *+12                                                             
         LA    R3,EDYERR                                                        
         B     ERROR               JUST IN CASE                                 
DATD0440 EQU   *                                                                
         CLI   0(R5),C'A'          ALTERNATE WEEKS?                             
         BNE   DATD0460                                                         
         LA    R5,1(R5)                                                         
         OI    WORK2+8,X'40'                                                    
         B     *+8                                                              
DATD0460 EQU   *                                                                
         OI    WORK2+8,X'80'       EVERY WEEK                                   
DATD0480 EQU   *                                                                
         LA    R3,EDTERR                                                        
         CLC   WORK+18(6),WORK+6   BUY END V K END DATE                         
         BH    ERROR                                                            
*                                                                               
         CLC   WORK+18(6),WORK+12  BY END V BUY START                           
         BL    ERROR                                                            
*                                                                               
         MVC   WORK+24(6),WORK+18  SAVE BUY END DATE FOR                        
*                                     CONSECUTIVE TEST                          
         MVI   WORK2+9,1           NUMBER OF SPOTS GIVEN                        
*                                     DEFAULTS TO 1                             
         BAS   RE,NPWEDIT          VALIDATE NUMBER PER WEEK FIELD               
         BZ    DATD0520            VALID                                        
         LA    R3,NPWERR           INCORRECT:  PUT OUT ERROR MESSAGE            
         LA    R2,MGSOSPTH         SET CURSOR ADDRESS FOR ERROR                 
*                                                                               
         CLI   FMGTYPE,FMGBOFFQ    BONUS?                                       
         BNE   *+8                                                              
         LA    R2,MGBOSPTH                                                      
*                                                                               
         CLI   FMGTYPE,FMLROFFQ    LATE RUN?                                    
         BNE   *+8                                                              
         LA    R2,MLROSPTH                                                      
*                                                                               
         CLI   FMGTYPE,FMLBOFFQ    LATE RUN W/BONUS?                            
         BNE   DATD0500                                                         
         LA    R2,MLBMSPTH                                                      
         TM    FLAGS,FACTBONQ      PROCESSING BONUS PORTION                     
         BZ    *+8                 OF LATE RUN W/ BONUS?                        
         LA    R2,MLBBSPTH                                                      
*                                                                               
DATD0500 EQU   *                                                                
         A     R2,OFFRDISP         DISPLACE TO LINE                             
*                                                                               
         B     ERROR                                                            
DATD0520 EQU   *                   NOW GET TOTAL WEEKS                          
         SR    R7,R7               CTR                                          
         MVC   WORK+30(6),WORK+12  START                                        
*                                                                               
         LA    R3,7                                                             
         TM    WORK2+8,X'40'       ALT?                                         
         BZ    *+8                                                              
         LA    R3,14                                                            
DATD0540 EQU   *                                                                
         LA    R7,1(R7)                                                         
         GOTO1 ADDAY,DMCB,WORK+30,WORK+36,(R3)                                  
         MVC   WORK+30(6),WORK+36                                               
*                                                                               
         CLC   WORK+30(6),WORK+18  PAST END?                                    
         BNH   DATD0540                                                         
*                                                                               
         STC   R7,WORK2+10                                                      
*                                  CONVERT DATES FOR MKGREC                     
         GOTO1 DATCON,DMCB,WORK+12,(3,WORK2+2)                                  
*                                  START DATE                                   
         GOTO1 (RF),(R1),WORK+18,(3,WORK2+5)                                    
*                                  END DATE                                     
         MVC   DMCB2(24),DMCB      SAVE SINCE TRASHED BY HELLO...               
*                                                                               
*        KEEP A WEEK COUNT, BECAUSE WEEKS CAN'T OVERLAP                         
*                                                                               
         L     RF,MGTOTWKS         KEEP WEEK COUNT                              
         ZIC   RE,WORK2+10         LOAD NUMBER WEEKS FROM X'03'                 
         AR    RF,RE                                                            
         ST    RF,MGTOTWKS         RESTORE TOTAL WEEKS                          
*                                                                               
*        CALCULATE NUMBER OF SPOTS/X'03' ELEMENT, ACCUMULATE                    
*                                                                               
         SR    RE,RE                                                            
         ZIC   RF,WORK2+9          LOAD NUMBER SPOTS FROM X'03'                 
         ZIC   R1,WORK2+10         LOAD NUMBER WEEKS FROM X'03'                 
         MR    RE,R1               # SPOTS X # WEEKS = TOTL SPOTS               
         L     RE,MGTOTSPT         LOAD SPOT COUNTER                            
         AR    RF,RE               ACCUMULATE                                   
         ST    RF,MGTOTSPT         RESTORE TOTAL SPOTS                          
*                                                                               
         GOTO1 VADDELEM,DMCB,RMKGREC,WORK2                                      
*                                  ADD DATE ELT TO MKGREC                       
         MVC   DMCB(24),DMCB2      RESTORE                                      
         B     DATD0040            GO BACK FOR NEXT                             
*                                                                               
DATDX    DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
*        SCAN ROUTINE FOR SUBFIELDS DENOTED BY ASTERISKS                        
*          P1 = A(LAST FIELD) BYTE 0=FIELD LENGTH (0=NONE)                      
*          P2 = A(FIELD HEADER) BYTE 0=* ON RETURN IF STOP CHAR FOUND.          
*          AN ASTERISK DELIMITS SUB-FIELDS                                      
*                                                                               
* NOTE: FOR DARE ONLY, ORBITS ARE NOT SUPPORTED AT THIS TIME                    
*                                                                               
SCAN     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R7,4(R1)       FIELD HEADER                                      
         L     R3,0(R1)       LAST FIELD                                        
*                                                                               
         ZIC   R4,0(R1)       LENGTH OF PREVIOUS FIELD                          
         LA    R3,1(R4,R3)    NEW SCAN PLUS DELIMITER                           
         ST    R3,0(R1)       LAST FIELD                                        
         SR    R5,R5          LENGTH COUNTER                                    
         IC    R4,5(R7)       TOTAL LENGTH OF INPUT                             
         LA    R7,8(R4,R7)    TOTAL FIELD END                                   
         MVI   4(R1),0        ELIM. STOP INDICATOR                              
FIELD25  CR    R3,R7          END OF INPUT?                                     
         BL    FIELD100                                                         
FIELD50  STC   R5,0(R1)       NEW FIELD LENGTH                                  
         B     EXXMOD                                                           
FIELD100 CLI   0(R3),C'*'     SUB-FIELD INDICATOR                               
         BNE   FIELD110                                                         
         MVI   4(R1),C'*'                                                       
*                                                                               
         LA    RF,MGSODTSH                                                      
         CLI   FMGTYPE,FMGMOFFQ    REGULAR?                                     
         BE    FIELD105                                                         
*                                                                               
         LA    RF,RNAODTSH                                                      
         CLI   FMGTYPE,FRNAOFFQ    REPLACEMENT NA?                              
         BE    FIELD105                                                         
*                                                                               
         LA    RF,MGBODTSH                                                      
         CLI   FMGTYPE,FMGBOFFQ    BONUS?                                       
         BE    FIELD105                                                         
*                                                                               
         LA    RF,MLRODTSH                                                      
         CLI   FMGTYPE,FMLROFFQ    LATE RUN?                                    
         BE    FIELD105                                                         
*                                                                               
         LA    RF,MLBMDTSH                                                      
         CLI   FMGTYPE,FMLBOFFQ    LATE RUN W/BONUS?                            
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    FLAGS,FACTBONQ      PROCESSING BONUS PORTION                     
         BZ    *+8                 OF LATE RUN W/ BONUS?                        
         LA    RF,MLBBDTSH                                                      
*                                                                               
FIELD105 DS    0H                                                               
         A     RF,OFFRDISP                                                      
*        CR    RF,R2                                                            
*        BE    FIELD50                                                          
*                                                                               
         B     FIELD50                                                          
*&&DO                                                                           
*                                                                               
         LA    R6,RCONREC          FOR DARE ORDERS, ORBITS ARE NOT              
         MVI   ELCODE,X'1D'        SUPPORTED                                    
         BRAS  RE,GETEL                                                         
         BNE   FIELD50                                                          
         USING RCONDREL,R6                                                      
         OC    RCONDRLK,RCONDRLK                                                
         BZ    FIELD50                                                          
         LA    R3,585                                                           
         B     ERROR                                                            
         DROP  R6                                                               
*&&                                                                             
*                                                                               
FIELD110 DS    0H                                                               
         LA    R5,1(R5)       LENGTH                                            
         LA    R3,1(R3)                                                         
         B     FIELD25                                                          
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
*   NPWEDIT:  VALIDATION OF NPW FIELD, INSERTION INTO APPROPRIATE               
*        AREAS.                                                                 
NPWEDIT  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,MGSOSPTH                                                      
         CLI   FMGTYPE,FMGBOFFQ    BONUS?                                       
         BNE   *+8                                                              
         LA    R2,MGBOSPTH                                                      
*                                                                               
         CLI   FMGTYPE,FMLROFFQ    LATE RUN?                                    
         BNE   *+8                                                              
         LA    R2,MLROSPTH                                                      
*                                                                               
         CLI   FMGTYPE,FMLBOFFQ    LATE RUN W/BONUS?                            
         BNE   NPWE0030                                                         
         LA    R2,MLBMSPTH                                                      
         TM    FLAGS,FACTBONQ      PROCESSING BONUS PORTION                     
         BZ    *+8                 OF LATE RUN W/ BONUS?                        
         LA    R2,MLBBSPTH                                                      
*                                                                               
NPWE0030 EQU   *                                                                
         A     R2,OFFRDISP                                                      
*                                                                               
         ZIC   RE,5(R2)            GET FIELD INPUT LENGTH                       
         SR    R1,R1                                                            
         LA    R2,8(R2)            BUMP TO DATA FIELD                           
         LR    RF,R2               SAVE A(DATA FIELD)                           
NPWE0040 EQU   *                   CHECK NUMBER PER WEEK                        
         CLI   0(R2),X'F0'         CHECK VALID NUMERIC                          
         BL    NPWE0080            ERROR EXIT                                   
         CLI   0(R2),X'F9'                                                      
         BH    NPWE0080            ERROR EXIT                                   
*                                                                               
         LA    R2,1(R2)            BUMP TO NEXT FIELD                           
         LA    R1,1(R1)                                                         
         BCT   RE,NPWE0040         GO BACK AND CHECK NEXT FIELD                 
*                                                                               
         BCTR  R1,00               SUBTRACT 1 FOR EX STATEMENT                  
         EX    R1,*+8              PACK THE FIELD                               
         B     *+10                                                             
         PACK  DUB,0(0,RF)                                                      
         CVB   R1,DUB              CONVERT IT TO BINARY                         
         LTR   R1,R1               MUST BE NON-ZERO                             
         BZ    NPWE0080                                                         
         CH    R1,=H'255'                                                       
         BH    NPWE0080            ERROR: CAN'T HAPPEN - ONLY                   
*                                     ALLOWING 2 POSITIONS IN FIELD             
         STC   R1,WORK2+9          NPW IN EFF DATE ELT                          
         STC   R1,RMKGNW           NPW IN DESCRIPTION ELEMENT                   
         SR    R0,R0               SET CC = ZERO                                
         B     NPWE0120                                                         
NPWE0080 EQU   *                                                                
         LTR   RB,RB               SET CC NOT = ZERO:  ERROR                    
NPWE0120 EQU   *                                                                
         B     EXXMOD                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* EDIT RATES                                                                    
*                                                                               
RATEDIT  NTR1  BASE=*,LABEL=*                                                   
         TM    FLAGS,FACTBONQ      PROCESSING BONUS PORTION                     
         BZ    REDE0005            OF LATE RUN W/ BONUS?                        
         LA    R2,MLBBRATH                                                      
         B     REDE0010                                                         
*                                                                               
REDE0005 EQU   *                   EDIT RATE                                    
         LA    R2,MGSORATH                                                      
         CLI   FMGTYPE,FMGMOFFQ    REGULAR?                                     
         BE    REDE0010                                                         
*                                                                               
         LA    R2,RNAORATH                                                      
         CLI   FMGTYPE,FRNAOFFQ    REPLACEMENT NA?                              
         BE    REDE0010                                                         
*                                                                               
         LA    R2,MGBORATH                                                      
         CLI   FMGTYPE,FMGBOFFQ    BONUS?                                       
         BE    REDE0010                                                         
*                                                                               
         LA    R2,MLRORATH                                                      
         CLI   FMGTYPE,FMLROFFQ    LATE RUN                                     
         BE    REDE0010                                                         
*                                                                               
         LA    R2,MLBMRATH                                                      
         CLI   FMGTYPE,FMLBOFFQ    LATE RUN W/BONUS                             
         BE    REDE0010                                                         
         DC    H'0'                                                             
*                                                                               
REDE0010 EQU   *                   EDIT RATE                                    
         A     R2,OFFRDISP         OFFSET TO SPECIFIC LINE                      
*                                  RATE EDIT                                    
         LA    R3,RATERR                                                        
*                                                                               
         XC    RMKGCOMB,RMKGCOMB   DEFAULT BUY TO NON-COMBO                     
         XC    RMKGCOS,RMKGCOS                                                  
         CLI   5(R2),0             ANYTHING IN RATE FIELD?                      
         BNE   REDE0020            YES                                          
         TM    RMKGRTS,X'20'       OPTIONAL FOR BONUS LINE                      
         BO    REDE0040                                                         
*        L     RF,AORIGBUY         NO  - TAKE RATE FROM 'ORIG BUY'              
*        MVC   RMKGCOS,RMKGCOS-RMKGREC(RF)                                      
*        B     REDE0040                                                         
         B     ERROR                                                            
REDE0020 EQU   *                   EDIT RATE                                    
         XC    DMCB+4(3),DMCB+4                                                 
         MVC   DMCB+7(1),5(R2)     FIELD LENGTH                                 
         GOTO1 CASHVAL,DMCB,8(R2)                                               
*                                                                               
         CLI   DMCB,X'FF'          ERROR?                                       
         BE    ERROR                                                            
*                                                                               
         MVC   RMKGCOS,DMCB+4                                                   
*                                                                               
REDE0040 EQU   *                                                                
*                                                                               
*   CALCULATE TOTAL SPOTS, WEEKS, $$ FOR THIS BUYLINE                           
*                                                                               
         SR    RE,RE                                                            
         L     RF,MGTOTSPT         TOTAL SPOTS                                  
         ZICM  R1,RMKGCOS,4        INSERT COST                                  
         MR    RE,R1               TOTL SPOTS X COST = TOTAL $$                 
         STCM  RF,15,RMKGTCOS      INSERT TOTAL COST                            
         MVC   RMKGTSPT,MGTOTSPT+2                                              
*                                  INSERT TOTAL SPOTS (2 CHARS ONLY)            
         MVC   RMKGTWKS,MGTOTWKS+3                                              
*                                  INSERT TOTAL WEEKS (1 CHAR ONLY)             
         XC    MGTOTSPT,MGTOTSPT                                                
         XC    MGTOTWKS,MGTOTWKS                                                
*                                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                  EDIT LENGTH                                  
LENEDIT  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,MGSOLENH                                                      
         CLI   FMGTYPE,FMGBOFFQ    BONUS?                                       
         BNE   *+8                                                              
         LA    R2,MGBOLENH                                                      
         TM    FLAGS,FACTBONQ      PROCESSING BONUS PORTION                     
         BZ    *+8                 OF LATE RUN W/ BONUS?                        
         LA    R2,MLBBLENH                                                      
         A     R2,OFFRDISP         OFFSET TO SPECIFIC LINE                      
*                                  RATE EDIT                                    
         LA    RE,MGSODYSH                                                      
         CLI   FMGTYPE,FMGBOFFQ    BONUS?                                       
         BNE   *+8                                                              
         LA    RE,MGBODYSH                                                      
         TM    FLAGS,FACTBONQ      PROCESSING BONUS PORTION                     
         BZ    *+8                 OF LATE RUN W/ BONUS?                        
         LA    RE,MLBBDYSH                                                      
         A     RE,OFFRDISP                                                      
*                                  RATE EDIT                                    
         LA    R3,LENERR                                                        
         CLI   5(R2),0             ANYTHING IN FIELD?                           
         BNE   LENE0010            YES                                          
*                                                                               
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         TM    TWAMKGFG,X'08'                                                   
         BO    ERROR               BONUS NEEDS LENGTH INPUT                     
         DROP  RF                                                               
*                                                                               
*        L     RF,AORIGBUY         NO  - TAKE LENGTH FROM ORIG BUY              
*        MVC   RMKGDUR,RMKGDUR-RMKGREC(RF)                                      
*                                  INSERT INTO NEW RECORD                       
*        B     LENEX                                                            
         B     ERROR                                                            
LENE0010 EQU   *                                                                
         GOTO1 VPACK               LENGTH                                       
         LTR   R0,R0                                                            
         BZ    LENE0040                                                         
* VALID SECONDS                                                                 
         CLI   RCONTYPE,C'X'                                                    
         BE    *+12                                                             
         CLI   RCONTYPE,C'N'                                                    
         BNE   LENE0020                                                         
         CH    R0,=H'120'                                                       
         BNH   LENE0020                                                         
         LA    R3,268              BUY MUST BE L.T. 120 SECS FOR XFER           
         B     ERROR                                                            
LENE0020 EQU   *                                                                
         STH   R0,HALF                                                          
         MVC   RMKGDUR,HALF                                                     
         B     LENEX                                                            
* TEST FOR MINUTES                                                              
LENE0040 EQU   *                                                                
         LA    R4,4                                                             
         LA    R5,MGSOLEN                                                       
         CLI   FMGTYPE,FMGBOFFQ    BONUS?                                       
         BNE   *+8                                                              
         LA    R5,MGBOLEN                                                       
         TM    FLAGS,FACTBONQ      PROCESSING BONUS PORTION                     
         BZ    *+8                 OF LATE RUN W/ BONUS?                        
         LA    R5,MLBBLEN                                                       
         A     R5,OFFRDISP         OFFSET TO SPECIFIC LINE                      
LENE0060 EQU   *                                                                
         CLI   0(R5),C'M'          MINUTES?                                     
         BE    LENE0080                                                         
         CLI   0(R5),X'F0'                                                      
         BL    ERROR                                                            
         CLI   0(R5),X'F9'                                                      
         BH    ERROR                                                            
         LA    R5,1(R5)                                                         
         BCT   R4,LENE0060                                                      
         B     ERROR                                                            
*                                  PACK MINUTES (MINUTES NOT ALLOWED            
*                                     FOR SPOTPAK TRANSFER)                     
LENE0080 EQU   *                                                                
*                                                                               
         CLI   RCONTYPE,C'X'                                                    
         BE    *+12                                                             
         CLI   RCONTYPE,C'N'                                                    
         BNE   LENE0100                                                         
         LA    R3,267              MUST BE SECONDS FOR SPOTPAK XFER             
         B     ERROR                                                            
*                                                                               
LENE0100 EQU   *                                                                
         LA    R2,MGSOLEN          PREPARE TO PACK LEN                          
         CLI   FMGTYPE,FMGBOFFQ    BONUS?                                       
         BNE   *+8                                                              
         LA    R2,MGBOLEN                                                       
         TM    FLAGS,FACTBONQ      PROCESSING BONUS PORTION                     
         BZ    *+8                 OF LATE RUN W/ BONUS?                        
         LA    R2,MLBBLEN                                                       
         A     R2,OFFRDISP         OFFSET TO SPECIFIC LINE                      
         LA    R6,4                                                             
         SR    R6,R4                                                            
         BNP   ERROR                                                            
         BCTR  R6,R0                                                            
         EX    R6,*+8                                                           
         B     LENE0120                                                         
         PACK  DUB,0(0,R2)         PACK LENGTH BY LENGTH                        
LENE0120 EQU   *                                                                
         CVB   R0,DUB                                                           
         STH   R0,HALF                                                          
         MVC   RMKGDUR,HALF                                                     
         OI    RMKGDUR,X'80'       MINUTES IND                                  
*                                                                               
*        LA    R3,638              NOT SURE HOW LONG MINUTES SHOULD BE          
*        CLC   HALF,=H'60'                                                      
*        BH    ERROR                                                            
*                                                                               
LENEX    DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* EDIT DAYS AND TIMES                                                           
*                                                                               
DYTMEDIT NTR1  BASE=*,LABEL=*                                                   
         LA    R2,MGSODYSH                                                      
         CLI   FMGTYPE,FMGMOFFQ    REGULAR?                                     
         BE    DAED0010                                                         
*                                                                               
         LA    R2,RNAODYSH                                                      
         CLI   FMGTYPE,FRNAOFFQ    REPLACMENT NA?                               
         BE    DAED0010                                                         
*                                                                               
         LA    R2,MGBODYSH                                                      
         CLI   FMGTYPE,FMGBOFFQ    BONUS?                                       
         BE    DAED0010                                                         
*                                                                               
         LA    R2,MLBBDYSH                                                      
         TM    FLAGS,FACTBONQ      PROCESSING BONUS PORTION                     
         BO    *+6                 OF LATE RUN W/ BONUS?                        
         DC    H'0'                                                             
*                                                                               
DAED0010 DS    0H                                                               
         A     R2,OFFRDISP         OFFSET TO SPECIFIC LINE                      
*                                  A(NTH LINE, 1ST FIELD)                       
*                                 DELETE ALL DAY-TIME ELEMENTS                  
         GOTO1 VDELELEM,DMCB,(2,RMKGREC)                                        
*                                                                               
         MVC   WORK2(2),=X'0209'   ELEM CODE + LENGTH                           
         MVI   WORK2+8,1           WEIGHTING FACTOR                             
*                                                                               
*                                  PREPARE TO EDIT STRING OF                    
*                                     DAY-TIME FIELDS IN PARALLEL               
         LA    R4,MGSODYS-1                                                     
         LA    R5,MGSODYSH                                                      
         LA    R6,MGSOTMS-1                                                     
         LA    R7,MGSOTMSH                                                      
*                                                                               
         CLI   FMGTYPE,FMGBOFFQ    BONUS?                                       
         BNE   DAED0020                                                         
         LA    R4,MGBODYS-1                                                     
         LA    R5,MGBODYSH                                                      
         LA    R6,MGBOTMS-1                                                     
         LA    R7,MGBOTMSH                                                      
         B     DAED0030                                                         
*                                                                               
DAED0020 DS    0H                                                               
         TM    FLAGS,FACTBONQ      PROCESSING BONUS PORTION                     
         BZ    DAED0030            OF LATE RUN W/ BONUS?                        
         LA    R4,MLBBDYS-1                                                     
         LA    R5,MLBBDYSH                                                      
         LA    R6,MLBBTMS-1                                                     
         LA    R7,MLBBTMSH                                                      
*                                                                               
DAED0030 DS    0H                                                               
         A     R4,OFFRDISP         OFFSET TO SPECIFIC LINE                      
         A     R5,OFFRDISP         OFFSET TO SPECIFIC LINE                      
         A     R6,OFFRDISP         OFFSET TO SPECIFIC LINE                      
         A     R7,OFFRDISP         OFFSET TO SPECIFIC LINE                      
*                                                                               
         STM   R4,R7,DMCB+8        PARAMETERS FOR SCAN                          
         SR    R6,R6                                                            
         SR    R3,R3               START DAY FOR ALL 02 ELEMENTS                
         SR    R7,R7               END DAY                                      
         GOTO1 CHKBLANK,DMCB,(RC)                                               
*                                  CHECK DAY/TIMES FOR BLANK, AND               
*                                     PRIME FIELDS IF NECESSARY.                
         CLI   DAYTMFLG,0          BOTH FIELDS ENTERED?                         
         BE    DAED0120            YES - EDIT BOTH IN SYNCH                     
         CLI   DAYTMFLG,2          SEED X'02'S SERIALLY? (NO TIMES)             
         BE    DAED0060            YES                                          
         CLI   DAYTMFLG,3          SEED X'02'S SERIALLY? (NO DAYS)              
         BE    DAED0060            YES                                          
         CLI   DAYTMFLG,1          NEITHER FIELD ENTERED?                       
         BNE   DAED0040            NO  - CONTINUE FLAG CHECK                    
*        BAS   RE,COPYX02S         YES - COPY ORIG X'02S' INTO NEW              
*        L     RF,AORIGBUY         SET A(ORIGINAL RECORD)                       
*        MVC   RMKGSTED,RMKGSTED-RMKGREC(RF)                                    
*                                  INSERT START-END DAY FOR ALL X'02'           
*                                     ELTS FROM ORIGINAL RECORD                 
*        B     DAEDX               SKIP FURTHER CHECKS ON FIELDS                
         LA    R3,DAYERR                                                        
         B     ERROR                                                            
DAED0040 EQU   *                   ***>>>ONLY ERRORS REMAIN<<<***               
         LA    R2,MGSODYSH                                                      
         CLI   FMGTYPE,FMGBOFFQ    BONUS?                                       
         BNE   *+8                                                              
         LA    R2,MGBODYSH                                                      
         TM    FLAGS,FACTBONQ      PROCESSING BONUS PORTION                     
         BZ    *+8                 OF LATE RUN W/ BONUS?                        
         LA    R2,MLBBDYSH                                                      
         A     R2,OFFRDISP         OFFSET TO SPECIFIC LINE                      
*                                                                               
         LA    R3,DAYERR                                                        
         CLI   DAYTMFLG,9          DAY  FIELD IN ERROR?                         
         BE    ERROR               YES                                          
*                                                                               
         LA    R2,MGSOTMSH         NO  - TIME FIELD IN ERROR                    
         CLI   FMGTYPE,FMGBOFFQ    BONUS?                                       
         BNE   *+8                                                              
         LA    R2,MGBOTMSH                                                      
         TM    FLAGS,FACTBONQ      PROCESSING BONUS PORTION                     
         BZ    *+8                 OF LATE RUN W/ BONUS?                        
         LA    R2,MLBBTMSH                                                      
         A     R2,OFFRDISP         OFFSET TO SPECIFIC LINE                      
*                                                                               
         LA    R3,TIMERR                                                        
         B     ERROR                                                            
DAED0060 EQU   *                                                                
         CLI   DAYTMFLG,3          SEEDING OF X'02' NEEDED?                     
         BNE   DAED0080            NO  -                                        
*                                  YES - NO DAYS ENTERED                        
*        L     RF,AORIGBUY         SET A(ORIGINAL RECORD)                       
*        MVC   RMKGSTED,RMKGSTED-RMKGREC(RF)                                    
*                                  INSERT START-END DAY FOR ALL X'02'           
*                                     ELTS FROM ORIGINAL RECORD                 
*        B     DAED0100                                                         
         LA    R3,DAYERR                                                        
         B     ERROR                                                            
DAED0080 EQU   *                                                                
         CLI   DAYTMFLG,2          SEEDING OF X'02' NEEDED?                     
         BNE   DAED0120            NO                                           
*                                  YES - NO TIMES ENTERED                       
DAED0100 EQU   *                                                                
         L     RF,ADYTMELT         SET A(X'02' ELEMENT)                         
         MVC   WORK2(9),0(RF)      MOVE X'02' ELT FROM ORIGINAL                 
*                                     RECORD TO WORK2                           
         LA    RF,9(RF)            BUMP TO NEXT ELEMENT                         
         ST    RF,ADYTMELT         SAVE ADDRESS                                 
DAED0120 EQU   *                                                                
         LA    R2,MGSODYSH                                                      
         CLI   FMGTYPE,FMGBOFFQ    BONUS?                                       
         BNE   *+8                                                              
         LA    R2,MGBODYSH                                                      
         TM    FLAGS,FACTBONQ      PROCESSING BONUS PORTION                     
         BZ    *+8                 OF LATE RUN W/ BONUS?                        
         LA    R2,MLBBDYSH                                                      
         A     R2,OFFRDISP         OFFSET TO SPECIFIC LINE                      
*                                                                               
         CLI   5(R2),0             ANY DATA IN FIELD?                           
         BE    DAED0200            NO  - DON'T CHECK DAY FIELD                  
         GOTO1 =A(SCAN),DMCB+8,RR=RELO  SCAN DAY FIELD TO GET LENGTH            
         CLI   DAYTMFLG,2          SEEDING OF X'02' DONE? (NO TIMES)            
         BE    DAED0140            YES - DON'T CLEAR THE FIELD                  
         CLI   DAYTMFLG,3          NO  - SEEDING OF X'02'? (NO DAYS)            
         BE    DAED0140            YES - DON'T CLEAR THE FIELD                  
         XC    WORK2+2(6),WORK2+2                                               
DAED0140 EQU   *                                                                
         CLI   DMCB+8,0            NO DAY ENTRY?                                
         BNE   DAED0160                                                         
*                                  NO DAY LENGTH                                
         LTR   R6,R6               ANY DAYS?                                    
         BNZ   *+12                                                             
         LA    R3,DAYERR                                                        
         B     ERROR                                                            
         CLI   DMCB+20,C'*'        ANOTHER TIME ENTRY?                          
         BNE   *+12                                                             
         LA    R3,DAYERR                                                        
         B     ERROR                                                            
*                                  GET START/END DAYS FOR                       
*                                     ALL X'02' ELEMENTS                        
         SLL   R3,4                START DAY                                    
         CH    R7,=H'8'            END DAY IN NEXT WEEK?                        
         BL    *+8                                                              
         SH    R7,=H'7'                                                         
         OR    R3,R7                                                            
         STC   R3,RMKGSTED                                                      
*                                                                               
         B     DAEDX               EDIT NEXT FIELD                              
DAED0160 EQU   *                                                                
         MVC   DMCB2(4),DMCB+8     DAY FIELD ADDR + LEN                         
*                                                                               
*                                  EDIT DAY FIELD                               
*        GOTO1 =V(REDAYVAL),DMCB2,,WORK2+3,WORK2+2,RR=YES                       
         GOTOX (RFDAYVAL,VREPFACS),DMCB2,,WORK2+3,WORK2+2                       
*                                                                               
         CLI   WORK2+3,0           VALID DAY?                                   
         BNE   *+12                                                             
         LA    R3,DAYERR                                                        
         B     ERROR                                                            
*                                                                               
         LA    R6,1(R6)            COUNTER                                      
*                                                                               
*                                 GET FIRST START/LAST END DAY                  
         SR    R4,R4               START                                        
         SR    R5,R5               END                                          
         IC    R4,WORK2+2          START-END                                    
         SRDL  R4,4                                                             
         SRL   R5,28               END                                          
         LTR   R3,R3               FIRST 02 ELEMENT?                            
         BNZ   *+6                                                              
         LR    R3,R4               FIRST START DAY IS KEY                       
         CR    R4,R5               START V END                                  
         BNH   *+8                                                              
         LA    R5,7(R5)            NEXT WEEK                                    
         CR    R3,R4               CHECK AGAINST 1ST START DAY                  
         BNH   *+8                                                              
         LA    R5,7(R5)            NEXT WEEK                                    
         CH    R5,=H'8'            END DAY                                      
         BL    DAED0180                                                         
*                                  MORE THAN 7 DAYS COVERED?                    
         LA    R1,7(R3)                                                         
         CR    R1,R5                                                            
         BH    *+12                                                             
         LA    R3,DAYERR                                                        
         B     ERROR               MORE THAN 7 DAYS                             
DAED0180 EQU   *                                                                
         CR    R5,R7               END                                          
         BNH   *+6                                                              
         LR    R7,R5               NEW HIGH END                                 
*                                                                               
DAED0200 EQU   *                                                                
*                                  EDIT TIME FIELD                              
         LA    R2,MGSOTMSH                                                      
         CLI   FMGTYPE,FMGBOFFQ    BONUS?                                       
         BNE   *+8                                                              
         LA    R2,MGBOTMSH                                                      
         TM    FLAGS,FACTBONQ      PROCESSING BONUS PORTION                     
         BZ    *+8                 OF LATE RUN W/ BONUS?                        
         LA    R2,MLBBTMSH                                                      
         A     R2,OFFRDISP         OFFSET TO SPECIFIC LINE                      
*                                                                               
         CLI   5(R2),0             ANY VALUE IN FIELD?                          
         BE    DAED0280            NO  - DON'T CHECK THE FIELD                  
* SCAN NEXT TIME FIELD FOR LENGTH                                               
         GOTO1 =A(SCAN),DMCB+16,RR=RELO                                         
*                                                                               
         CLI   DMCB+16,0           NO TIME ENTRY?                               
         BNE   *+12                                                             
         LA    R3,TIMERR                                                        
         B     ERROR                                                            
*                                                                               
         ZIC   R4,DMCB+16          ALLOW INPUT OF 'VARIOUS'                     
         L     R5,DMCB+16                                                       
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R5),=C'VARIOUS'                                              
         BNE   DAED0220                                                         
         CLI   DMCB+16,3           MUST INPUT AT LEAST 3 CHARACTERS             
         BNL   *+12                                                             
         LA    R3,TIMERR                                                        
         B     ERROR                                                            
         MVC   WORK2+4,=C'VARY'                                                 
         B     DAED0280                                                         
DAED0220 EQU   *                                                                
         EX    R4,*+8              ALLOW INPUT OF 'NONE'                        
         B     *+10                                                             
         CLC   0(0,R5),=C'NONE'                                                 
         BNE   DAED0240            OR GO TO RETIMVAL                            
         CLI   DMCB+16,3           MUST INPUT AT LEAST 3 CHARACTERS             
         BNL   *+12                                                             
         LA    R3,TIMERR                                                        
         B     ERROR                                                            
         MVC   WORK2+4,=C'NONE'                                                 
         B     DAED0280                                                         
DAED0240 EQU   *                                                                
         MVC   DMCB(4),DMCB+16     TIME LEN + ADDR                              
*        GOTO1 =V(RETIMVAL),DMCB,,WORK2+4,RR=YES  EDIT TIME                     
         GOTOX (RFTIMVAL,VREPFACS),DMCB,,WORK2+4                                
         CLI   DMCB,X'FF'          TIME INVALID?                                
         BNE   *+12                                                             
         LA    R3,TIMERR                                                        
         B     ERROR                                                            
*                                                                               
         CLC   =C'CC',WORK2+6      CC NOT SUPPORTED IF DARE                     
         BNE   DAED0250                                                         
*                                                                               
         LA    R6,RCONREC          FOR DARE ORDERS, ORBITS ARE NOT              
         MVI   ELCODE,X'1D'        SUPPORTED                                    
         BRAS  RE,GETEL                                                         
         BNE   DAED0250                                                         
         USING RCONDREL,R6                                                      
         OC    RCONDRLK,RCONDRLK                                                
         BZ    DAED0250                                                         
         LA    R3,623                                                           
         B     ERROR                                                            
         DROP  R6                                                               
*                                                                               
DAED0250 EQU   *                                                                
         OC    WORK2+6(2),WORK2+6  IS THERE AN END TIME?                        
         BZ    DAED0280            NO                                           
*                                                                               
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
*                                                                               
         B     DAED0280            BYPASS AM VALIDATION                         
*                                                                               
         TM    TWATIME,X'80'       CHECK IF B'CAST DAY SET TO USE               
         BZ    DAED0260            6AM - 559AM                                  
         DROP  RF                                                               
*                                                                               
         CLC   WORK2+4(2),=H'0600' START TIME LT 6AM?                           
         BNL   DAED0280            NO                                           
         CLC   WORK2+6(2),=H'0600' END TIME GT = 6AM?                           
         BL    DAED0280            NO                                           
         LA    R3,344              END TIME MUST BE W/IN B'CAST DAY             
         B     ERROR                                                            
*                                                                               
DAED0260 EQU   *                                                                
         CLC   WORK2+4(2),=H'0500' START TIME LT 5AM?                           
         BNL   DAED0280            NO                                           
         CLC   WORK2+6(2),=H'0500' END TIME GT = 5AM?                           
         BL    DAED0280            NO                                           
         LA    R3,344              END TIME MUST BE W/IN B'CAST DAY             
         B     ERROR                                                            
*                                  ADD DAY-TIME ELEMENT TO MKGREC               
DAED0280 EQU   *                                                                
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         CLI   TWAECON,C'B'        CHECK IF ELECTRONIC CONTRACT                 
         BNE   DAED0300            PARTICULARLY, IF BIAS                        
         DROP  RF                                                               
                                                                                
         BAS   RE,VCROSS           VALIDATE CROSS DAY                           
                                                                                
DAED0300 EQU   *                                                                
         MVC   DMCB2(24),DMCB      SAVE SINCE TRASHED BY HELLO...               
         GOTO1 VADDELEM,DMCB,RMKGREC,WORK2                                      
         MVC   DMCB(24),DMCB2      RESTORE                                      
*                                                                               
         CLI   DAYTMFLG,0          BOTH FIELDS ENTERED?                         
         BE    DAED0120            YES - DON'T BOTHER WITH SEEDING              
         ZIC   RF,DAYTMCTR         NO  - CHECK COUNTER FOR ANOTHER              
*                                     SET OF FIELDS                             
         BCTR  RF,0                DECREMENT BY 1                               
         STC   RF,DAYTMCTR         SAVE COUNTER BACK                            
         LTR   RF,RF               ANYTHING LEFT?                               
         BNZ   DAED0060            NO  - EDIT NEXT DAY/TM FLD COMBO             
*                                  GET START/END DAYS FOR                       
*                                     ALL X'02' ELEMENTS                        
         CLI   DAYTMFLG,3          DAYS NOT ENTERED?                            
         BE    DAED0320            YES - SET RMKGSTED FROM ORIG REC             
         SLL   R3,4                START DAY                                    
         CH    R7,=H'8'            END DAY IN NEXT WEEK?                        
         BL    *+8                                                              
         SH    R7,=H'7'                                                         
         OR    R3,R7                                                            
         STC   R3,RMKGSTED                                                      
         B     DAEDX                                                            
*                                                                               
DAED0320 EQU   *                                                                
*        L     RF,AORIGBUY                                                      
*        MVC   RMKGSTED,RMKGSTED-RMKGREC(RF)                                    
*                                  USE ORIG FIELD TO SET                        
         LA    R3,DAYERR                                                        
         B     ERROR                                                            
*                                                                               
DAEDX    DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   CHKBLANK:  IF EITHER DAY OR TIME FIELD IS EMPTY, DEFAULT IS                 
*        FROM TARGET BUY, FOR 1ST LINE, OR PRECEDING OFFER LINE.                
*        MUST CHECK TO ENSURE THAT SAME NUMBER OF DAYS AND TIMES                
*        ARE PRESENT.                                                           
*                                                                               
CHKBLANK NTR1                                                                   
         MVI   DAYTMCTR,0          CLEAR COUNTER                                
         MVI   DAYTMFLG,0          CLEAR FLAG                                   
*                                                                               
         LA    R2,MGSODYSH         DAY FIELD                                    
         LA    R3,MGSOTMSH         DAY FIELD                                    
         CLI   FMGTYPE,FMGBOFFQ    BONUS?                                       
         BNE   CHKB0050                                                         
         LA    R2,MGBODYSH         DAY FIELD                                    
         LA    R3,MGBOTMSH         DAY FIELD                                    
         B     CHKB0100                                                         
*                                                                               
CHKB0050 EQU   *                                                                
         TM    FLAGS,FACTBONQ      PROCESSING BONUS PORTION                     
         BZ    CHKB0100            OF LATE RUN W/ BONUS?                        
         LA    R2,MLBBDYSH         DAY FIELD                                    
         LA    R3,MLBBTMSH         DAY FIELD                                    
*                                                                               
CHKB0100 EQU   *                                                                
         A     R2,OFFRDISP         DISPLACEMENT TO SPECIFIC LINE                
         A     R3,OFFRDISP         DISPLACEMENT TO SPECIFIC LINE                
         CLI   5(R2),0             ANY DATA IN DAY FIELD?                       
         BE    CHKB0200            NO  - CHECK TIMES                            
         CLI   5(R3),0             YES - ANY DATA IN TIME FIELD?                
         BE    CHKB0400            NO  -                                        
         B     CHKB0900            YES - DATA IN BOTH FIELDS                    
*                                     NORMAL VALIDATION                         
CHKB0200 EQU   *                   NO DATA IN DAY FIELD                         
         CLI   5(R3),0             ANY DATA IN TIME FIELD?                      
         BNE   CHKB0240            YES - TIMES ENTERED                          
         MVI   DAYTMFLG,1          NO  - SET FLAG TO 1 TO SIGNAL                
*                                     THAT X'02' ELTS MUST BE COPIED            
         B     CHKB0900            EXIT                                         
CHKB0240 EQU   *                                                                
***>>    BAS   RE,COUNT02S         COUNT X'02'S IN ORIG REC                     
***>>    GOTO1 SCANFLD,DMCB,(R3)   COUNT NUMBER ENTRIES IN TIME                 
         MVI   DAYTMFLG,9          SET 'ERROR IN TIME FIELD' FLAG               
***>>    CLC   WORK(1),DAYTMCTR    # X'02'S = ENTRIES IN TIME?                  
***>>    BNE   CHKB0900            NO  - ERROR                                  
         B     CHKB0900                                                         
***>>    MVI   DAYTMFLG,3          YES - MUST SEED X'02'S FROM                  
*                                     ORIGINAL ORDER                            
***>>    B     CHKB0900            EXIT                                         
CHKB0400 EQU   *                                                                
***>>    BAS   RE,COUNT02S         COUNT X'02'S IN ORIG REC                     
***>>    GOTO1 SCANFLD,DMCB,(R2)   COUNT NUMBER ENTRIES IN DAY                  
         MVI   DAYTMFLG,8          SET 'ERROR IN DAY  FIELD' FLAG               
***>>    CLC   WORK(1),DAYTMCTR    # X'02'S = ENTRIES IN DAY?                   
***>>    BNE   CHKB0900            NO  - ERROR                                  
         B     CHKB0900                                                         
***>>    MVI   DAYTMFLG,2          YES - MUST SEED X'02'S FROM                  
*                                     ORIGINAL ORDER                            
***>>    B     CHKB0900            EXIT                                         
CHKB0900 EQU   *                                                                
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
COUNT02S NTR1                                                                   
         XC    ADYTMELT,ADYTMELT   CLEAR A(1ST DAY/TIME ELEMENT)                
         L     R2,AORIGBUY         A(ORIGINAL/PRECEDING RECORD)                 
         LA    R2,34(R2)           SET A(X'01' DESCRIPTOR ELEMENT)              
         SR    RF,RF               CLEAR COUNTER                                
CONS0020 EQU   *                                                                
         CLI   0(R2),0             END OF RECORD?                               
         BE    CONS0080            YES - FINISHED                               
         CLI   0(R2),X'02'         DAY/TIME ELEMENT?                            
         BNE   CONS0040            NO                                           
         OC    ADYTMELT,ADYTMELT   1ST DAY/TIME ELEMENT?                        
         BNZ   CONS0030            NO                                           
         ST    R2,ADYTMELT         YES - SAVE ITS ADDR                          
CONS0030 EQU   *                                                                
         LA    RF,1(RF)            BUMP COUNTER                                 
CONS0040 EQU   *                                                                
         ZIC   RE,1(R2)            BUMP TO NEXT ELEMENT                         
         AR    R2,RE                                                            
         B     CONS0020            GO BACK FOR NEXT                             
CONS0080 EQU   *                                                                
         STC   RF,DAYTMCTR         SAVE COUNT OF X'02' ELTS                     
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
*   SCANFLD:   P1 CONTAINS A(SCREEN FIELD HEADER).  A COUNT                     
*        MUST BE MADE TO DETERMINE WHETHER SAME NUMBER OF                       
*        FIELDS ARE BEING ENTERED AS EXIST WITHIN THE RECORD FROM               
*        WHICH X'02' ELEMENTS ARE BEING SEEDED.                                 
*        FIELDS ARE SEPARATED BY '*'                                            
*                                                                               
SCANFLD  NTR1                                                                   
         L     R2,0(R1)            SET A(FIELD HEADER)                          
         ZIC   R3,5(R2)            SET L(INPUT)                                 
         LA    RF,1                INITIALIZE CNTR: ALWAYS 1 ENTRY              
         LA    R2,8(R2)            SET A(DATA WITHIN FIELD)                     
SCFL0020 EQU   *                                                                
         CLI   0(R2),C'*'          IS FIELD A SEPARATOR?                        
         BNE   SCFL0040            NO                                           
         LA    RF,1(RF)            YES - COUNT IT                               
SCFL0040 EQU   *                                                                
         LA    R2,1(R2)            BUMP TO NEXT FIELD                           
         BCT   R3,SCFL0020         GO BACK AND CHECK NEXT FIELD                 
         STC   RF,WORK             PASS BACK RESULT                             
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
*   COPYX02S:  NEITHER DAY NOR TIME INFORMATION WAS ENTERED FOR                 
*        THIS MAKEGOOD BUYLINE.  THE X'02'S FROM THE ORIGINAL                   
*        BUY LINE MUST BE COPIED OVER TO THE NEW OFFER LINE.                    
*                                                                               
COPYX02S NTR1                                                                   
         L     R2,AORIGBUY         SET A(ORIGINAL BUYLINE)                      
         LA    R2,34(R2)           SET TO DESCRIPTOR ELEMENT                    
CPYX0020 EQU   *                                                                
         CLI   0(R2),0             END OF RECORD?                               
         BE    CPYX0080            YES - FINISHED                               
         CLI   0(R2),X'02'         DAY/TIME ELEMENT?                            
         BNE   CPYX0040            NO                                           
         GOTO1 VADDELEM,DMCB,RMKGREC,(R2)                                       
*                                  YES - ADD ELT FROM ORIGINAL REC              
CPYX0040 EQU   *                                                                
         ZIC   RF,1(R2)                                                         
         AR    R2,RF               BUMP TO NEXT ELEMENT                         
         B     CPYX0020            GO BACK FOR NEXT                             
CPYX0080 EQU   *                                                                
         B     EXXMOD                                                           
         EJECT                                                                  
********************************************************************            
* FOR BIAS ELECTRONIC CONTRACT, VALIDATE CROSS DAY.  CROSS DAY MUST             
*   HAVE AT LEAST ONE DAY OPEN                                                  
*   THIS ELEMENT WILL BE ADDED HERE, BUT THE CONTRACT RECORD WILL               
*     NOT BE UPDATED.  UPON FINALLY ADDING THE RECORDS, THE                     
*     CROSS-DAY VALIDATION WILL HAVE TO BE ACCOMPLISHED.                        
* ***   SERIOUS NOTE   ***                                                      
********************************************************************            
VCROSS   NTR1                                                                   
         ZIC   RF,WORK2+2          START, END DAYS                              
         SRL   RF,4                WANT START DAY ONLY                          
         ZIC   RE,=X'80'           SHIFT BITS TO CORRESPONDING DAY              
         SRL   RE,1                POSITION: MON=X'40', TUE=X'20', ETC.         
         BCT   RF,*-4                                                           
         ZIC   RF,WORK2+3                                                       
         XR    RE,RF               EXCLUSIVE-OR THE START DAY (NULL IT)         
         STC   RE,WORK                                                          
                                                                                
         LA    R6,RCONREC          NOW CHECK IF BIAS ELEMENT EXISTS             
         MVI   ELCODE,X'13'        IN CONTRACT                                  
         BRAS  RE,GETEL                                                         
         BNE   VCROSS30            ELEMENT HASN'T BEEN ADDED YET                
         USING RCONCCEL,R6                                                      
                                                                                
         LA    R3,399              CANNOT CROSS CROSS DAY DEFAULT               
         MVC   WORK+1(1),WORK      MAKE A COPY OF BUYLINE'S DAYS                
         NC    WORK+1(1),RCONCCDF  CHECK WITH CROSS DAY DEFAULT                 
         BZ    VCROSS10                                                         
         LA    R2,MGSODYSH                                                      
         CLI   FMGTYPE,FMGBOFFQ                                                 
         BNE   *+8                                                              
         LA    R2,MGBODYSH                                                      
         TM    FLAGS,FACTBONQ      PROCESSING BONUS PORTION                     
         BZ    *+8                 OF LATE RUN W/ BONUS?                        
         LA    R2,MLBBDYSH                                                      
         A     R2,OFFRDISP         DISPLACEMENT TO SPECIFIC LINE                
         B     ERROR               CANNOT CROSS CROSS DAY DEFAULT               
                                                                                
VCROSS10 DS    0H                                                               
         OC    WORK(1),RCONCCCD    COMBINED WITH CROSS DAY                      
         LA    R3,398              CD MUST HAVE AT LEAST 1 OPEN DAY             
         TM    WORK,X'7F'          ERROR IF ALL ON                              
         BNO   VCROSS20                                                         
         LA    R2,MGSODYSH                                                      
         CLI   FMGTYPE,FMGBOFFQ                                                 
         BNE   *+8                                                              
         LA    R2,MGBODYSH                                                      
         TM    FLAGS,FACTBONQ      PROCESSING BONUS PORTION                     
         BZ    *+8                 OF LATE RUN W/ BONUS?                        
         LA    R2,MLBBDYSH                                                      
         A     R2,OFFRDISP         DISPLACEMENT TO SPECIFIC LINE                
         B     ERROR                                                            
                                                                                
VCROSS20 DS    0H                                                               
         MVC   RCONCCCD,WORK       UPDATE NEW CROSS DAY                         
****>>>  OI    BYTE4,X'02'         CONTRACT UPDATED-DO PUTREC                   
         B     VCROSSX                                                          
         DROP  R6                                                               
                                                                                
VCROSS30 DS    0H                  ADD EC BIAS ELEMENT TO CONTRACT REC          
         ZIC   RF,WORK                                                          
         XC    WORK,WORK                                                        
         LA    R6,WORK                                                          
         USING RCONCCEL,R6                                                      
         MVI   RCONCCCO,X'13'                                                   
         MVI   RCONCCLN,RCONCCL2                                                
         STC   RF,RCONCCCD         DAYS IN CROSS DAY FOR THIS BUY               
         MVI   RCONCCOT,C'5'       DEFAULT ORDER TYPE                           
         MVC   DMCB2(24),DMCB      SAVE SINCE TRASHED BY HELLO...               
         GOTO1 VADDELEM,DMCB,RCONREC,WORK                                       
         MVC   DMCB(24),DMCB2      RESTORE                                      
****>>>  OI    BYTE4,X'02'         CONTRACT UPDATED-DO PUTREC                   
         DROP  R6                                                               
                                                                                
VCROSSX  DS    0H                                                               
         B     EXXMOD                                                           
*                                                                               
*                                                                               
*   PROCESS PROGRAM NAME/DETAIL COMMENT, IF ANY                                 
*   R2 IS POINTING TO FIELD TO BE VALIDATED                                     
*   HI ORDER BYTE IS THE ELEMENT CODE                                           
PRNMDTCM NTR1  BASE=*,LABEL=*                                                   
         STC   R1,BYTE                                                          
         XC    ELTBILD,ELTBILD     CLEAR ELEMENT BUILD AREA                     
         MVC   ELTBILD(1),BYTE     YES - SET ELEMENT TYPE CODE                  
*                                                                               
DCMT0030 EQU   *                                                                
         GOTO1 VDELELEM,DMCB,(BYTE,RMKGREC)                                     
         CLI   5(R2),0                                                          
         BNE   DCMT0080                                                         
*                                                                               
         B     DCMTX               DISABLE AUTO PROGRAM NAME POPULATION         
*                                  PER CLIENT                                   
         CLI   BYTE,X'21'          NOTHING IN INPUT FIELD                       
         BNE   DCMTX               IF THIS IS ABOUT PROGRAM NAME                
*                                                                               
         LR    R5,R2               R2->PROGRAM NAME FIELD                       
         LA    R2,MLROLN#H         SET A(BUY LINE#)                             
         TM    FMGTYPE,FMLROFFQ    IF LATE RUN, GET PROG NAME FROM BUY          
         BO    DMCT0040                                                         
         CLI   FMGTYPE,FMLBOFFQ    LATE RUN WITH BONUS?                         
         BNE   DCMTX                                                            
         LA    R2,MLBMLN#H         SET A(BUY LINE#)                             
         TM    FLAGS,FACTBONQ      PROCESSING BONUS PORTION?                    
         BO    DCMTX                                                            
*                                                                               
DMCT0040 DS    0H                                                               
         LA    R3,90               SET UP BUY LINE# FOR GETDEMV                 
         A     R2,OFFRDISP         OFFSET TO SPECIFIC LINE                      
         CLI   5(R2),0             R2->BUYLINE #                                
         BE    ERROR                                                            
         GOTO1 VPACK                                                            
         LTR   R0,R0                                                            
         BZ    ERROR                                                            
         STC   R0,TARGETBY                                                      
         LR    R2,R5                RESTORE R2, R2->PROGRAM NAME FIELD          
*                                                                               
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ          GET TARGET BUY LINE#                        
         USING TWAWORK,RF                                                       
*                                                                               
         MVC   WORK(L'KEY),KEY      BACK UP KEY                                 
         XC    KEY,KEY                                                          
K        USING RBUYKEY,KEY                                                      
         MVI   K.RBUYKTYP,X'0B'                                                 
         MVC   K.RBUYKREP,REPALPHA                                              
         MVC   K.RBUYKCON,TWACNUM                                               
         MVC   K.RBUYKPLN,=3X'FF'                                               
*                                                                               
         GOTO1 VHIGH                                                            
DMCT0050 DS    0H                                                               
         CLC   KEY(RBUYKMLN-RBUYKEY),KEYSAVE                                    
         BE    DMCT0070                                                         
         DC    H'0'                                                             
*                                                                               
DMCT0060 DS    0H                                                               
         CLC   K.RBUYKLIN,TARGETBY                                              
         BE    DMCT0070                                                         
         GOTO1 VSEQ                                                             
         B     DMCT0050                                                         
         DROP  K                                                                
         DROP  RF                                                               
*                                                                               
DMCT0070 DS    0H                                                               
         GOTO1 VGETREC,DMCB,IOAREA  DO NOT READ INTO RBUYREC,WILL SCREW         
         MVC   KEY,WORK             UP TSAR                                     
         GOTO1 VHIGH                RESTORE READ SEQUENCE                       
*                                                                               
         LA    R6,IOAREA                                                        
         USING RBUYREC,R6                                                       
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'21'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DCMTX                                                            
*                                                                               
         ZIC   R5,1(R6)                                                         
         SHI   R5,3                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   ELTBILD+2(0),2(R6)                                               
         AHI   R5,3                ADD FOR CONTROL, EX                          
*MN                                                                             
         CHI   R5,19                                                            
         BNH   *+8                                                              
         LA    R5,19                                                            
*MN                                                                             
         STC   R5,ELTBILD+1        INSERT LENGTH INTO ELEMENT                   
         B     DCMT0100                                                         
*                                                                               
DCMT0080 DS    0H                                                               
         ZIC   R1,5(R2)            GET LENGTH OF INPUT                          
         BCTR  R1,0                SUBTRACT 1 FOR EX                            
         EX    R1,DCMT0040         MOVE COMMENT BY LENGTH                       
         AHI   R1,3                ADD FOR CONTROL, EX                          
         STC   R1,ELTBILD+1        INSERT LENGTH INTO ELEMENT                   
DCMT0100 DS    0H                                                               
         DROP  R6                                                               
         GOTO1 VADDELEM,DMCB,RMKGREC,ELTBILD                                    
*                                  ADD ELEMENT TO RECORD                        
DCMTX    DS    0H                                                               
         B     EXXMOD                                                           
*                                                                               
DCMT0040 EQU   *                                                                
         MVC   ELTBILD+2(0),8(R2)  MOVE COMMENT BY LENGTH                       
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* - VALIDATE MISSED SPOTS                                                       
*                                                                               
MGMVMISS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*                                                                               
* BUILD A BLOCK OF X'05' MISSED ELEMENTS FROM SCREEN INPUTS                     
*                                                                               
         XC    EL05BLK,EL05BLK                                                  
         SR    R7,R7                                                            
         LA    R5,EL05BLK                                                       
MGRD     USING RMKGMGEL,R5                                                      
*                                                                               
         LA    R2,MGSMD1H                                                       
*                                                                               
VMISS10  DS    0H                                                               
         CLI   5(R2),0                                                          
         BNE   VMISS20                                                          
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     VMISS50                                                          
*                                                                               
VMISS20  DS    0H                                                               
         MVI   MGRD.RMKGMGCD,X'05'                                              
         MVI   MGRD.RMKGMGLN,10                                                 
         LR    RE,R2                                                            
         SR    RE,RA                                                            
         STCM  RE,7,RMKGMGLQ(R5)                                                
*                                                                               
* MISSED DATES                                                                  
*                                                                               
         CLC   =C'ALL',8(R2)                                                    
         BNE   VMISS25                                                          
         GOTO1 =A(MISSALL),RR=RELO                                              
*                                                                               
         XC    BYTE,BYTE           CLEAR BYTE FOR / COUNTER                     
VMISS25  DS    0H                                                               
         LA    R3,47               MISSED DATE INVALID                          
         LA    R4,8(R2)                                                         
*********************************************************                       
* CHECKS TO SEE IF THE INPUT IS IN M/D/Y FORM                                   
* IF SO, THE USER SPECIFY THE YEAR EXPLICITLY                                   
* THEREFORE, USE THE YEAR THE USER INPUT INSTEAD OF TRYING TO FIGURE            
* IT OUT                                                                        
*********************************************************                       
         ZIC   RE,0(R2)                                                         
         AR    RE,R2               START OF NEXT SCREEN FIELD                   
         LR    RF,R4               START OF THIS INPUT CHUNK                    
         SR    R1,R1               CLEAR / COUNTER                              
CEDI0404 CR    RF,RE                                                            
         BNL   CEDI0406            REACH THE END OF THIS FIELD                  
         CLI   0(RF),C'*'          * SEPARATED                                  
         BE    CEDI0406                                                         
         CLI   0(RF),C'-'          - SEPARATED                                  
         BE    CEDI0406                                                         
         CLI   0(RF),C'/'          COUNT /                                      
         BNE   CEDI0405                                                         
         LA    R1,1(R1)            INCREMENT COUNTER                            
CEDI0405 LA    RF,1(RF)            NEXT CHARACTER                               
         B     CEDI0404            LOOP                                         
*                                                                               
CEDI0406 DS    0H                                                               
         STC   R1,BYTE             SAVE COUNTER                                 
         CLI   BYTE,2              2 = M/D/Y, 1 = M/D                           
         BL    CEDI0408                                                         
         GOTO1 DATVAL,DMCB,(0,8(R2)),DUB     DATVAL OPTION 0 FOR M/D/Y          
         B     CEDI0409                                                         
*                                                                               
CEDI0408 GOTO1 DATVAL,DMCB,(1,8(R2)),DUB     DATVAL OPTION 1 FOR M/D            
CEDI0409 OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
         L     R4,DMCB                                                          
         LA    R4,8(R2,R4)                                                      
         GOTO1 DATCON,DMCB,DUB,(3,MGRD.RMKGMGD1)                                
         CLI   BYTE,2              USER SPECIFIED M/D/Y                         
         BE    VMISS35                                                          
         MVC   MGRD.RMKGMGD1(1),RCONDATE CONTRACT START YEAR                    
         CLC   MGRD.RMKGMGD1+1(2),RCONDATE+1                                    
         BNL   VMISS35             DETERMINE YEAR TO USE                        
         CLC   MGRD.RMKGMGD1(1),RCONDATE+3                                      
         BE    ERROR               USE CONTRACT END YEAR                        
         MVC   MGRD.RMKGMGD1(1),RCONDATE+3                                      
*                                                                               
VMISS35  DS    0H                                                               
         CLI   0(R4),C'-'                                                       
         BNE   VMISS40                                                          
         AHI   R4,1                                                             
         XC    BYTE,BYTE           CLEAR BYTE FOR / COUNTER                     
*********************************************************                       
* CHECKS TO SEE IF THE INPUT IS IN M/D/Y FORM                                   
* IF SO, THE USER SPECIFY THE YEAR EXPLICITLY                                   
* THEREFORE, USE THE YEAR THE USER INPUT INSTEAD OF TRYING TO FIGURE            
* IT OUT                                                                        
*********************************************************                       
         ZIC   RE,0(R2)                                                         
         AR    RE,R2               START OF NEXT SCREEN FIELD                   
         LR    RF,R4               START OF THIS INPUT CHUNK                    
         SR    R1,R1               CLEAR / COUNTER                              
CEDI0414 CR    RF,RE                                                            
         BNL   CEDI0416            REACH THE END OF THIS FIELD                  
         CLI   0(RF),C'*'          * SEPARATED                                  
         BE    CEDI0416                                                         
         CLI   0(RF),C'-'          - SEPARATED                                  
         BE    CEDI0416                                                         
         CLI   0(RF),C'/'          COUNT /                                      
         BNE   *+8                                                              
         LA    R1,1(R1)            INCREMENT COUNTER                            
         LA    RF,1(RF)            NEXT CHARACTER                               
         B     CEDI0414            LOOP                                         
*                                                                               
CEDI0416 DS    0H                                                               
         STC   R1,BYTE             SAVE COUNTER                                 
         CLI   BYTE,2              2 = M/D/Y, 1 = M/D                           
         BL    CEDI0418                                                         
         GOTO1 DATVAL,DMCB,(0,(R4)),DUB     DATVAL OPTION 0 FOR M/D/Y           
         B     CEDI0419                                                         
*                                                                               
CEDI0418 GOTO1 DATVAL,DMCB,(1,(R4)),DUB     DATVAL OPTION 1 FOR M/D             
CEDI0419 OC    DMCB(4),DMCB                                                     
         BZ    VMISS36                                                          
         L     RE,DMCB                                                          
         AR    R4,RE                                                            
         GOTO1 DATCON,DMCB,DUB,(3,MGRD.RMKGMGD2)                                
         CLI   BYTE,2              USER SPECIFIED M/D/Y                         
         BE    VMISS38                                                          
         MVC   MGRD.RMKGMGD2(1),RCONDATE CONTRACT START YEAR                    
         CLC   MGRD.RMKGMGD2+1(2),RCONDATE+1                                    
         BNL   VMISS38             DETERMINE YEAR TO USE                        
         CLC   MGRD.RMKGMGD2(1),RCONDATE+3                                      
         BE    ERROR               USE CONTRACT END YEAR                        
         MVC   MGRD.RMKGMGD2(1),RCONDATE+3                                      
         B     VMISS38                                                          
*                                                                               
VMISS36  DS    0H                                                               
         CLI   0(R4),C'E'          FORMAT IS MMMDD-E                            
         BNE   VMISS36B                                                         
*                                                                               
         LR    R3,R2               SKIP TO TARGET BUY FIELD                     
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         GOTO1 VPACK                                                            
         BNZ   *+12                                                             
         LA    R3,2                                                             
         B     ERROR                                                            
         CHI   R0,255                                                           
         BNH   *+12                                                             
         LA    R3,2                                                             
         B     ERROR                                                            
         STC   R0,MGRD.RMKGMGLI                                                 
         LR    R2,R3                                                            
*                                                                               
         GOTO1 =A(GETBUYTR),DMCB,MGRD.RMKGMGLI,RR=RELO                          
*                                                                               
         LA    R6,TSARREC+2                                                     
         MVI   ELCODE,X'03'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VMISS36A LR    R3,R6               GET TARGET BUY END DATE                      
         BAS   RE,NEXTEL                                                        
         BE    VMISS36A                                                         
*                                                                               
         LR    R6,R3                                                            
         USING RBUYDTEL,R6                                                      
         MVC   MGRD.RMKGMGD2,RBUYDTED                                           
         DROP  R6                                                               
*                                                                               
         B     VMISS38                                                          
*                                                                               
VMISS36B EQU   *                   FORMAT IS MMMDD-MMMDD                        
         LA    R1,1                                                             
         CLI   1(R4),C'W'          WEEKS IND?                                   
         BE    VMISS36C                                                         
         LA    R1,2                                                             
         CLI   2(R4),C'W'                                                       
         BE    VMISS36C                                                         
         B     VMISS40                                                          
*                                                                               
VMISS36C DS    0H                                                               
         LR    RE,R1                                                            
         LA    RF,0(R4)                                                         
*                                                                               
VMISS36D CLI   0(RF),C'0'          NUMERIC?                                     
         BL    ERROR                                                            
         CLI   0(RF),C'9'                                                       
         BH    ERROR                                                            
         AHI   RF,1                                                             
         BCT   RE,VMISS36D                                                      
*                                  NUMERIC WEEKS ENTERED                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                PACK WEEKS                                   
         PACK  DUB,0(0,R4)                                                      
         CVB   R3,DUB                                                           
         LTR   R3,R3                                                            
         BZ    ERROR                                                            
         SHI   R3,1                                                             
         MHI   R3,7                                                             
*                                                                               
         GOTO1 DATCON,DMCB,(3,MGRD.RMKGMGD1),WORK+20                            
         GOTO1 ADDAY,DMCB,WORK+20,WORK+20,(R3)                                  
         GOTO1 DATCON,DMCB,WORK+20,(3,MGRD.RMKGMGD2)                            
*                                                                               
* FIGURE OUT CORRECT END DATE FOR RANGE                                         
*                                                                               
         LR    R3,R2               SKIP TO TARGET BUY FIELD                     
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         GOTO1 VPACK                                                            
         LTR   R0,R0                                                            
         BNZ   *+12                                                             
         LA    R3,2                                                             
         B     ERROR                                                            
         CHI   R0,255                                                           
         BNH   *+12                                                             
         LA    R3,2                                                             
         B     ERROR                                                            
         STC   R0,MGRD.RMKGMGLI                                                 
         LR    R2,R3                                                            
*                                                                               
         GOTO1 =A(GETBUYTR),DMCB,MGRD.RMKGMGLI,RR=RELO                          
*                                                                               
         LA    R6,TSARREC+2                                                     
         USING RBUYREC,R6                                                       
*                                                                               
         SR    R1,R1                                                            
         ZIC   R0,RBUYSTED                                                      
         SRDL  R0,4                                                             
         SRL   R1,28                                                            
         SR    R1,R0                                                            
         LR    R3,R1                                                            
         DROP  R6                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(3,MGRD.RMKGMGD2),WORK                               
         GOTO1 ADDAY,DMCB,WORK,WORK,(R3)                                        
         GOTO1 DATCON,DMCB,WORK,(3,MGRD.RMKGMGD2)                               
*                                                                               
VMISS38  DS    0H                                                               
         CLI   0(R4),C'A'          ALTERNATE WEEKS?                             
         BNE   VMISS40                                                          
         OI    MGRD.RMKGMGFG,X'80'                                              
*                                                                               
* MISSED NUMBER OF SPOTS                                                        
*                                                                               
VMISS40  DS    0H                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         LA    R3,2                                                             
         GOTO1 VPACK                                                            
         CHI   R0,255                                                           
         BH    ERROR                                                            
         STC   R0,MGRD.RMKGMGSP                                                 
*                                                                               
         CLI   FMGTYPE,FMGMOFFQ                                                 
         BE    VMISS41                                                          
         CLI   FMGTYPE,FRNAOFFQ                                                 
         BNE   VMISS43                                                          
*                                                                               
VMISS41  DS    0H                                                               
*        CLC   =C'NONE',RNAODTS    CREDIT FOR NA KEYWORD                        
*        BNE   VMISS43                                                          
         LA    R3,808              MUST SPECIFY NUMBER OF SPOTS                 
         CLI   MGRD.RMKGMGSP,0     # OF SPOTS CANNOT BE ZERO                    
         BE    ERROR                                                            
*                                                                               
* MISSED LINE NUMBER                                                            
*                                                                               
VMISS43  DS    0H                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         LA    R3,2                                                             
         GOTO1 VPACK                                                            
         LTR   R0,R0                                                            
         BZ    ERROR                                                            
         CHI   R0,255                                                           
         BH    ERROR                                                            
         STC   R0,MGRD.RMKGMGLI                                                 
*                                                                               
* CHECK IF EXPANDED BUY SCREEN IN USE. IF NOT, CANNOT ALLOW USER TO             
* TARGET DIFFERENT BUY LINES.                                                   
*                                                                               
*&&DO                                                                           
         TM    PROFILES+CNTXBUYB,CNTXBUYA                                       
         BO    VMISS45                                                          
         LTR   R7,R7                                                            
         BZ    VMISS45                                                          
         LA    R3,864              CANNOT TARGET DIFFERENT BUY LINE             
         CLC   MGRD.RMKGMGLI,EL05BLK+(RMKGMGLI-RMKGMGEL)                        
         BNE   ERROR                                                            
*&&                                                                             
*                                                                               
VMISS45  DS    0H                                                               
         AHI   R7,1                NUMBER OF RECORDS FOR XSORT                  
         AHI   R5,EL05BLQ                                                       
*                                                                               
VMISS48  DS    0H                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
VMISS50  DS    0H                                                               
         LA    R0,MGSMDLH                                                       
         CR    R2,R0                                                            
         BNH   VMISS10                                                          
*                                                                               
* SORT ELEMENTS SO LINE NUMBER AND DATES WILL BE IN SEQUENCE                    
*                                                                               
VMISS60  DS    0H                  SORT ONLY FOR REGULAR MAKEGOOD OFFER         
         GOTO1 XSORT,DMCB,EL05BLK,(R7),EL05BLQ,RMKGMGLQ,0                       
*                                                                               
         LA    R5,EL05BLK                                                       
         LA    R7,1                                                             
*                                                                               
VMISS65  DS    0H                                                               
         LR    R4,RA                                                            
         AHI   R4,TWAWORKQ                                                      
         USING TWAWORK,R4                                                       
         XC    KEY,KEY                                                          
BUYD     USING RBUYKEY,KEY                                                      
         MVI   BUYD.RBUYKTYP,X'0B'                                              
         MVC   BUYD.RBUYKREP,REPALPHA                                           
         MVC   BUYD.RBUYKCON,TWACNUM                                            
         MVC   BUYD.RBUYKPLN,=3X'FF'                                            
         DROP  BUYD,R4                                                          
*                                                                               
         MVC   RBUYKEY(27),KEY                                                  
*                                                                               
         XCEFL TSARREC,1002                                                     
         MVC   TSARREC+2(27),RBUYKEY                                            
         MVI   TD.TSACTN,TSARDH    FIND FIRST BUY RECORD IN TSAR                
*                                                                               
VMISS70  DS    0H                  NOW READ NEXT UNTIL WE FIND TARGET           
         GOTO1 =A(GOTSAR),RR=RELO  MISSED BUY                                   
         TM    TD.TSERRS,TSEEOF                                                 
         BZ    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VMOVEREC,DMCB,TSARREC+2,RBUYREC                                  
         CLC   RBUYREC(RBUYKMLN-RBUYKEY),KEY                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    RBUYSFG,X'40'       TARGET SPORTS BUY?                           
         BZ    VMISS75                                                          
         LA    R3,880              NOT ALLOWED                                  
         LA    R2,CONCACTH                                                      
         B     ERROR                                                            
*                                                                               
VMISS75  DS    0H                                                               
         CLI   RBUYCHGI,C'C'       CANCELLED BUY?                               
         BNE   VMISS80                                                          
         LA    R3,983              NOT ALLOWED                                  
         LA    R2,CONCACTH                                                      
         B     ERROR                                                            
*                                                                               
VMISS80  DS    0H                                                               
         CLC   RBUYKLIN,MGRD.RMKGMGLI                                           
         BE    VMISS90                                                          
         XCEFL TSARREC,1002                                                     
         MVI   TD.TSACTN,TSANXT    READ NEXT                                    
         B     VMISS70                                                          
*                                                                               
* CHECK IF MISSED DATE/#SPOTS VALID                                             
*                                                                               
VMISS90  DS    0H                                                               
         GOTO1 =A(BLDGRID),RR=RELO                                              
*                                                                               
VMISS100 DS    0H                                                               
         GOTO1 =A(CHKMISS),RR=RELO                                              
         BNZ   ERROR                                                            
*                                                                               
         AHI   R5,EL05BLQ                                                       
         CLI   0(R5),0                                                          
         BE    VMISSX                                                           
         AHI   R7,1                                                             
         CHI   R7,15               MAX 15 MISSED DATES                          
         BH    VMISSX                                                           
         CLC   RBUYKLIN,MGRD.RMKGMGLI                                           
         BE    VMISS100                                                         
         B     VMISS65                                                          
*                                                                               
VMISSX   DS    0H                                                               
         B     EXXMOD                                                           
         DROP  MGRD                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* - SPECIAL KEYWORD TO SPECIFY ALL DATES ON TARGET LINE ARE NA                  
*                                                                               
MISSALL  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R3,915              NO OTHER INPUTS ALLOWED FOR 'ALL'            
         LR    R4,R2                                                            
         ZIC   R0,0(R4)                                                         
         AR    R4,R0                                                            
         ZIC   R0,0(R4)                                                         
         AR    R4,R0                                                            
*                                                                               
MSALL03  DS    0H                                                               
         ZIC   R0,0(R4)                                                         
         AR    R4,R0                                                            
         CLI   5(R4),0                                                          
         BE    MSALL04                                                          
         LR    R2,R4                                                            
         B     ERROR                                                            
MSALL04  DS    0H                                                               
         LA    R1,RNALNLH                                                       
         CR    R4,R1                                                            
         BNH   MSALL03                                                          
*                                                                               
* MISSED LINE NUMBER                                                            
*                                                                               
MSALL05  DS    0H                                                               
         LR    R4,R2                                                            
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         LA    R3,913                                                           
         CLI   5(R2),0                                                          
         BNE   ERROR                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         LA    R3,489                                                           
         GOTO1 VPACK                                                            
         LTR   R0,R0                                                            
         BZ    ERROR                                                            
         CHI   R0,255                                                           
         BH    ERROR                                                            
         STC   R0,TARGETBY                                                      
*                                                                               
         LR    R7,RA                                                            
         AHI   R7,TWAWORKQ                                                      
         USING TWAWORK,R7                                                       
*                                                                               
         XC    KEY,KEY                                                          
BUYD     USING RBUYKEY,KEY                                                      
         MVI   BUYD.RBUYKTYP,X'0B'                                              
         MVC   BUYD.RBUYKREP,REPALPHA                                           
         MVC   BUYD.RBUYKCON,TWACNUM                                            
         MVC   BUYD.RBUYKPLN,=3X'FF'                                            
         DROP  BUYD,R7                                                          
*                                                                               
         MVC   RBUYKEY(27),KEY                                                  
*                                                                               
         XCEFL TSARREC,1002                                                     
         MVC   TSARREC+2(27),RBUYKEY                                            
         MVI   TD.TSACTN,TSARDH    FIND FIRST BUY RECORD IN TSAR                
*                                                                               
MSALL10  DS    0H                  NOW READ NEXT UNTIL WE FIND TARGET           
         GOTO1 =A(GOTSAR),RR=RELO  MISSED BUY                                   
         TM    TD.TSERRS,TSEEOF                                                 
         BZ    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VMOVEREC,DMCB,TSARREC+2,RBUYREC                                  
         CLC   RBUYREC(RBUYKMLN-RBUYKEY),KEY                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    RBUYSFG,X'40'       TARGET SPORTS BUY?                           
         BZ    *+12                                                             
         LA    R3,880              NOT ALLOWED                                  
         B     ERROR                                                            
*                                                                               
         CLI   RBUYCHGI,C'C'       CANCELLED BUY?                               
         BNE   *+12                                                             
         LA    R3,983              NOT ALLOWED                                  
         B     ERROR                                                            
*                                                                               
         CLC   RBUYKLIN,TARGETBY                                                
         BE    MSALL20                                                          
         XCEFL TSARREC,1002                                                     
         MVI   TD.TSACTN,TSANXT    READ NEXT                                    
         B     MSALL10                                                          
*                                                                               
MSALL20  DS    0H                                                               
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'03'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RBUYDTEL,R6                                                      
*                                                                               
         LR    R2,R4                                                            
*                                                                               
MSALL30  DS    0H                                                               
         LA    R3,916                                                           
         TM    RBUYDTIN,X'40'      ALTERNATE WEEK                               
         BO    ERROR               NOT SUPPORTED AT THIS POINT                  
*                                                                               
         MVI   5(R2),5                                                          
         OI    6(R2),X'80'         XMIT                                         
         GOTO1 DATCON,DMCB,(3,RBUYDTST),(4,8(R2))                               
*                                                                               
         CLC   RBUYDTST,RBUYDTED                                                
         BE    MSALL40                                                          
         MVI   5(R2),11                                                         
         MVI   13(R2),C'-'                                                      
         GOTO1 DATCON,DMCB,(3,RBUYDTED),(4,14(R2))                              
*                                                                               
*        TM    RBUYDTIN,X'40'      ALTERNATE WEEK                               
*        BZ    MSALL40                                                          
*        MVI   19(R2),C'A'                                                      
*        MVI   5(R2),12                                                         
*                                                                               
MSALL40  DS    0H                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         EDIT  RBUYDTNW,(3,8(R2)),ALIGN=LEFT                                    
         STC   R0,5(R2)                                                         
         OI    6(R2),X'80'         XMIT                                         
         OI    4(R2),X'08'         VALID NUMERIC                                
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         EDIT  TARGETBY,(3,8(R2)),ALIGN=LEFT                                    
         STC   R0,5(R2)                                                         
         OI    6(R2),X'80'         XMIT                                         
         OI    4(R2),X'08'         VALID NUMERIC                                
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BRAS  RE,NEXTEL                                                        
         BNE   EXXMOD                                                           
         LA    R3,914                                                           
         LA    R1,RNALNLH                                                       
         CR    R2,R1                                                            
         BNH   MSALL30                                                          
         LA    R2,RNAMD1H                                                       
         B     ERROR                                                            
         DROP  R6                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* - VALIDATE MISSED SPOTS FOR PREEMPTION                                        
* - VALIDATE MISSED SPOTS FOR LATE RUN (AND LR W/BONUS)                         
*                                                                               
MGPVMISS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R3,1                MISSING                                      
         LA    R2,MGPODTSH         MAKE SURE USER ENTER A DATE IN               
         CLI   FMGTYPE,FMGPOFFQ    MISSED DATE FIELD                            
         BE    VMGP05                                                           
         LA    R2,MLRODTSH                                                      
         CLI   FMGTYPE,FMLROFFQ                                                 
         BE    VMGP05                                                           
         LA    R2,MLBMDTSH                                                      
         CLI   FMGTYPE,FMLBOFFQ                                                 
         BE    VMGP05                                                           
         DC    H'0'                                                             
*                                                                               
VMGP05   DS    0H                                                               
         CLI   5(R2),0                                                          
         BE    ERROR                                                            
*                                                                               
*        LR    R4,RA                                                            
*        AHI   R4,TWAWORKQ                                                      
*        USING TWAWORK,R4                                                       
*                                                                               
* BUILD A BLOCK OF X'05' MISSED ELEMENTS FROM SCREEN INPUTS                     
*                                                                               
         XC    OFFRDISP,OFFRDISP                                                
         XC    EL05BLK,EL05BLK                                                  
         LA    R5,EL05BLK                                                       
MGRD     USING RMKGMGEL,R5                                                      
*                                                                               
VMGP10   DS    0H                                                               
         CLI   FMGTYPE,FMLROFFQ    LATE RUN?                                    
         BNE   VMGP20                                                           
         LA    R2,MLRODTSH                                                      
         A     R2,OFFRDISP                                                      
         L     RF,OFFRDISP                                                      
         AHI   RF,MLRODT2H-MLRODTSH                                             
         ST    RF,OFFRDISP                                                      
         LA    R0,MLRODT7H                                                      
         CR    R2,R0                                                            
         BH    VMGP70                                                           
         CLI   5(R2),0                                                          
         BNE   VMGP40                                                           
         B     VMGP10                                                           
*                                                                               
VMGP20   DS    0H                                                               
         CLI   FMGTYPE,FMLBOFFQ    LATE RUN W/ BONUS ?                          
         BNE   VMGP30                                                           
         LA    R2,MLBMDTSH                                                      
         A     R2,OFFRDISP                                                      
         L     RF,OFFRDISP                                                      
         AHI   RF,MLBMDT2H-MLBMDTSH                                             
         ST    RF,OFFRDISP                                                      
         LA    R0,MLBMDT3H                                                      
         CR    R2,R0                                                            
         BH    VMGP70                                                           
         CLI   5(R2),0                                                          
         BNE   VMGP40                                                           
         B     VMGP20                                                           
*                                                                               
VMGP30   DS    0H                                                               
         LA    R2,MGPODTSH         DEFAULT PREEMPT                              
         A     R2,OFFRDISP                                                      
         L     RF,OFFRDISP                                                      
         AHI   RF,MGPODT2H-MGPODTSH                                             
         ST    RF,OFFRDISP                                                      
         LA    R0,MGPODT7H                                                      
         CR    R2,R0                                                            
         BH    VMGP70                                                           
         CLI   5(R2),0                                                          
         BNE   VMGP40                                                           
*                                                                               
* USER ELECTED TO BLANK OUT THE DATES FIELD INSTEAD OF TYPING DELETE            
*                                                                               
         MVC   8(6,R2),=C'DELETE'                                               
         B     VMGP30                                                           
*                                                                               
VMGP40   DS    0H                                                               
         CLC   =C'DEL',8(R2)                                                    
         BE    VMGP10              SKIP IF USER WANTS TO DELETE LINE            
         MVI   MGRD.RMKGMGCD,X'05'                                              
         MVI   MGRD.RMKGMGLN,10                                                 
         LR    RE,R2                                                            
         SR    RE,RA                                                            
         STCM  RE,7,RMKGMGLQ(R5)                                                
*                                                                               
* MISSED DATES                                                                  
*                                                                               
         LA    R3,47               MISSED DATE INVALID                          
         LA    R4,8(R2)                                                         
         XC    BYTE,BYTE           CLEAR / COUNTER                              
*********************************************************                       
* CHECKS TO SEE IF THE INPUT IS IN M/D/Y FORM                                   
* IF SO, THE USER SPECIFY THE YEAR EXPLICITLY                                   
* THEREFORE, USE THE YEAR THE USER INPUT INSTEAD OF TRYING TO FIGURE            
* IT OUT                                                                        
*********************************************************                       
         ZIC   RE,0(R2)                                                         
         AR    RE,R2               START OF NEXT SCREEN FIELD                   
         LR    RF,R4               START OF THIS INPUT CHUNK                    
         SR    R1,R1               CLEAR / COUNTER                              
CEDI0424 CR    RF,RE                                                            
         BNL   CEDI0426            REACH THE END OF THIS FIELD                  
         CLI   0(RF),C'*'          * SEPARATED                                  
         BE    CEDI0426                                                         
         CLI   0(RF),C'-'          - SEPARATED                                  
         BE    CEDI0426                                                         
         CLI   0(RF),C'/'          COUNT /                                      
         BNE   *+8                                                              
         LA    R1,1(R1)            INCREMENT COUNTER                            
         LA    RF,1(RF)            NEXT CHARACTER                               
         B     CEDI0424            LOOP                                         
*                                                                               
CEDI0426 DS    0H                                                               
         STC   R1,BYTE             SAVE COUNTER                                 
         CLI   BYTE,2              2 = M/D/Y, 1 = M/D                           
         BL    CEDI0428                                                         
         GOTO1 DATVAL,DMCB,(0,8(R2)),DUB     DATVAL OPTION 0 FOR M/D/Y          
         B     CEDI0429                                                         
*                                                                               
CEDI0428 GOTO1 DATVAL,DMCB,(1,8(R2)),DUB     DATVAL OPTION 1 FOR M/D            
CEDI0429 OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
         A     R4,DMCB                                                          
         GOTO1 DATCON,DMCB,DUB,(3,MGRD.RMKGMGD1)                                
         CLI   BYTE,2              USER SPECIFIED M/D/Y                         
         BE    VMGP55                                                           
         MVC   MGRD.RMKGMGD1(1),RCONDATE CONTRACT START YEAR                    
         CLC   MGRD.RMKGMGD1+1(2),RCONDATE+1                                    
         BNL   VMGP55              DETERMINE YEAR TO USE                        
         CLC   MGRD.RMKGMGD1(1),RCONDATE+3                                      
         BNE   VMGP50              USE CONTRACT END YEAR                        
         B     ERROR               USE CONTRACT END YEAR                        
*                                                                               
VMGP50   DS    0H                                                               
         MVC   MGRD.RMKGMGD1(1),RCONDATE+3                                      
*                                                                               
* ONLY PREEMPT OFFERS SUPPORT END DATE RANGE FOR NOW                            
*                                                                               
VMGP55   DS    0H                                                               
         CLI   FMGTYPE,FMGPOFFQ    MISSED DATE FIELD                            
         BNE   VMGP60                                                           
         CLI   0(R4),C'-'          END DATE OR RANGE IS SPECIFIED               
         BNE   VMGP60                                                           
         AHI   R4,1                                                             
         XC    BYTE,BYTE           CLEAR / COUNTER                              
*********************************************************                       
* CHECKS TO SEE IF THE INPUT IS IN M/D/Y FORM                                   
* IF SO, THE USER SPECIFY THE YEAR EXPLICITLY                                   
* THEREFORE, USE THE YEAR THE USER INPUT INSTEAD OF TRYING TO FIGURE            
* IT OUT                                                                        
*********************************************************                       
         ZIC   RE,0(R2)                                                         
         AR    RE,R2               START OF NEXT SCREEN FIELD                   
         LR    RF,R4               START OF THIS INPUT CHUNK                    
         SR    R1,R1               CLEAR / COUNTER                              
CEDI0434 CR    RF,RE                                                            
         BNL   CEDI0436            REACH THE END OF THIS FIELD                  
         CLI   0(RF),C'/'          COUNT /                                      
         BNE   *+8                                                              
         LA    R1,1(R1)            INCREMENT COUNTER                            
         LA    RF,1(RF)            NEXT CHARACTER                               
         B     CEDI0434            LOOP                                         
*                                                                               
CEDI0436 DS    0H                                                               
         STC   R1,BYTE             SAVE COUNTER                                 
         CLI   BYTE,2              2 = M/D/Y, 1 = M/D                           
         BL    CEDI0438                                                         
         GOTO1 DATVAL,DMCB,(0,(R4)),DUB      DATVAL OPTION 0 FOR M/D/Y          
         B     CEDI0439                                                         
*                                                                               
CEDI0438 GOTO1 DATVAL,DMCB,(1,(R4)),DUB      DATVAL OPTION 1 FOR M/D            
CEDI0439 OC    DMCB(4),DMCB                                                     
         BZ    CEDI0460            RANGE IS SPECIFIED INSTEAD                   
         A     R4,DMCB                                                          
*                                                                               
         GOTO1 DATCON,DMCB,DUB,(3,MGRD.RMKGMGD2)                                
         CLI   BYTE,2              USER SPECIFIED M/D/Y                         
         BE    VMGP60                                                           
*                                                                               
         MVC   MGRD.RMKGMGD2(1),RCONDATE CONTRACT START YEAR                    
         CLC   MGRD.RMKGMGD2+1(2),RCONDATE+1                                    
*                                  DETERMINE YEAR TO USE                        
         BNL   VMGP60                                                           
         CLC   MGRD.RMKGMGD2(1),RCONDATE+3                                      
         BE    ERROR                                                            
         MVC   MGRD.RMKGMGD2(1),RCONDATE+3                                      
*                                  USE CONTRACT END YEAR                        
         B     VMGP60                                                           
*                                                                               
CEDI0460 EQU   *                                                                
         CLI   0(R4),C'E'          FORMAT IS MMMDD-E                            
         BNE   CEDI0462                                                         
*                                                                               
         LR    R7,R2               SKIP TO TARGET BUY FIELD                     
         ZIC   R0,0(R7)                                                         
         AR    R7,R0                                                            
         ZIC   R0,0(R7)                                                         
         AR    R7,R0                                                            
         LA    R3,2                                                             
         GOTO1 VPACK                                                            
         CHI   R0,255                                                           
         BH    ERROR                                                            
         STC   R0,MGRD.RMKGMGLI                                                 
*                                                                               
         GOTO1 =A(GETBUYTR),DMCB,MGRD.RMKGMGLI,RR=RELO                          
*                                                                               
         LA    R6,TSARREC+2                                                     
         MVI   ELCODE,X'03'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CEDI0461 LR    R2,R6               GET TARGET BUY END DATE                      
         BAS   RE,NEXTEL                                                        
         BE    CEDI0461                                                         
*                                                                               
         LR    R6,R2                                                            
         USING RBUYDTEL,R6                                                      
         MVC   MGRD.RMKGMGD2,RBUYDTED                                           
         DROP  R6                                                               
*                                                                               
         B     VMGP60                                                           
*                                                                               
CEDI0462 EQU   *                   FORMAT IS MMMDD-MMMDD                        
         LA    R1,1                                                             
         CLI   1(R4),C'W'          WEEKS IND?                                   
         BE    CEDI0463                                                         
         LA    R1,2                                                             
         CLI   2(R4),C'W'                                                       
         BE    CEDI0463                                                         
         B     VMGP60                                                           
*                                                                               
CEDI0463 DS    0H                                                               
         LR    RE,R1                                                            
         LA    R7,0(R4)                                                         
*                                                                               
CEDI0465 CLI   0(R7),C'0'          NUMERIC?                                     
         BL    ERROR                                                            
         CLI   0(R7),C'9'                                                       
         BH    ERROR                                                            
         AHI   R7,1                                                             
         BCT   RE,CEDI0465                                                      
*                                  NUMERIC WEEKS ENTERED                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                PACK WEEKS                                   
         PACK  DUB,0(0,R4)                                                      
         CVB   R7,DUB                                                           
         LTR   R7,R7                                                            
         BZ    ERROR                                                            
         MHI   R7,7                                                             
*                                                                               
         GOTO1 DATCON,DMCB,(3,MGRD.RMKGMGD1),WORK+20                            
         GOTO1 ADDAY,DMCB,WORK+20,WORK+20,(R7)                                  
         GOTO1 DATCON,DMCB,WORK+20,(3,MGRD.RMKGMGD2)                            
**********                                                                      
*                                                                               
* MISSED NUMBER OF SPOTS                                                        
*                                                                               
VMGP60   DS    0H                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         LA    R3,2                                                             
         GOTO1 VPACK                                                            
         LTR   R0,R0                                                            
         BZ    ERROR                                                            
         CHI   R0,255                                                           
         BH    ERROR                                                            
         STC   R0,MGRD.RMKGMGSP                                                 
*                                                                               
* SKIP RATE FIELD FOR LATE RUN OFFER                                            
*                                                                               
         TM    FMGTYPE,FMLROFFQ+FMLBOFFQ                                        
         BZ    VMGP65                                                           
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
* MISSED LINE NUMBER                                                            
*                                                                               
VMGP65   DS    0H                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         LA    R3,2                                                             
         GOTO1 VPACK                                                            
         LTR   R0,R0                                                            
         BZ    ERROR                                                            
         CHI   R0,255                                                           
         BH    ERROR                                                            
         STC   R0,MGRD.RMKGMGLI                                                 
*                                                                               
         AHI   R5,EL05BLQ                                                       
         B     VMGP10                                                           
*                                                                               
VMGP70   DS    0H                                                               
         CLI   EL05BLK,0           DID USER DELETE ALL THE MISSE LINES          
         BNE   VMGP75                                                           
         LA    R3,862              CANNOT DELETE ONLY TARGET                    
         LA    R2,MGPODTSH                                                      
         CLI   FMGTYPE,FMGPOFFQ                                                 
         BE    ERROR                                                            
         LA    R2,MLRODTSH                                                      
         CLI   FMGTYPE,FMLROFFQ                                                 
         BE    ERROR                                                            
         LA    R2,MLBMDTSH                                                      
         CLI   FMGTYPE,FMLBOFFQ                                                 
         BE    ERROR                                                            
         LA    R2,CONCACTH                                                      
         B     ERROR                                                            
*                                                                               
VMGP75   DS    0H                                                               
         LA    R5,EL05BLK                                                       
         LA    R7,1                                                             
*                                                                               
VMGP80   DS    0H                                                               
*        GOTO1 =A(GETBUYTR),DMCB,MGRD.RMKGMGLI,RR=RELO                          
                                                                                
*                                                                               
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
*                                                                               
         XC    KEY,KEY                                                          
BUYD     USING RBUYKEY,KEY                                                      
         MVI   BUYD.RBUYKTYP,X'0B'                                              
         MVC   BUYD.RBUYKREP,REPALPHA                                           
         MVC   BUYD.RBUYKCON,TWACNUM                                            
         MVC   BUYD.RBUYKPLN,=3X'FF'                                            
         DROP  BUYD,RF                                                          
*                                                                               
         MVC   RBUYKEY(27),KEY                                                  
*                                                                               
         XCEFL TSARREC,1002                                                     
         MVC   TSARREC+2(27),RBUYKEY                                            
         MVI   TD.TSACTN,TSARDH    FIND FIRST BUY RECORD IN TSAR                
*                                                                               
VMGP90   DS    0H                  NOW READ NEXT UNTIL WE FIND TARGET           
         GOTO1 =A(GOTSAR),RR=RELO  MISSED BUY                                   
         TM    TD.TSERRS,TSEEOF                                                 
         BZ    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VMOVEREC,DMCB,TSARREC+2,RBUYREC                                  
         CLC   RBUYREC(RBUYKMLN-RBUYKEY),KEY                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VMGP100  DS    0H                                                               
         CLC   RBUYKLIN,MGRD.RMKGMGLI                                           
         BE    VMGP110                                                          
         XCEFL TSARREC,1002                                                     
         MVI   TD.TSACTN,TSANXT    READ NEXT                                    
         B     VMGP90                                                           
*                                                                               
* CHECK IF MISSED DATE/#SPOTS VALID                                             
*                                                                               
VMGP110  DS    0H                                                               
         LA    R6,RBUYREC                                                       
         USING RBUYREC,R6                                                       
         TM    RBUYSFG,X'40'       TARGET SPORTS BUY?                           
         BZ    VMGP113                                                          
         LA    R3,880              NOT ALLOWED                                  
         B     VMGP114                                                          
*                                                                               
VMGP113  DS    0H                                                               
         CLI   RBUYCHGI,C'C'                                                    
         BNE   VMGP115                                                          
         LA    R3,983              NOT ALLOWED                                  
*                                                                               
VMGP114  DS    0H                                                               
         ZICM  R2,RMKGMGLQ(R5),3                                                
         LTR   R2,R2                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,RA                                                            
         B     ERROR                                                            
         DROP  R6                                                               
*                                                                               
VMGP115  DS    0H                                                               
         GOTO1 =A(BLDGRID),RR=RELO                                              
*                                                                               
VMGP120  DS    0H                                                               
         GOTO1 =A(CHKMISS),RR=RELO                                              
         BNZ   ERROR                                                            
*                                                                               
         AHI   R5,EL05BLQ                                                       
         CLI   0(R5),0                                                          
         BE    VMGPX                                                            
         AHI   R7,1                                                             
         CHI   R7,15               MAX 15 MISSED DATES                          
         BH    VMGPX                                                            
         CLC   RBUYKLIN,MGRD.RMKGMGLI                                           
         BE    VMGP120                                                          
         B     VMGP80              NOT IN SORTED ORDER, READ HIGH AGAIN         
*                                                                               
VMGPX    DS    0H                                                               
         B     EXXMOD                                                           
         DROP  MGRD                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* GET BUY RECORD FROM TSAR BUFFER                                               
* P1 = BUY NUMBER                                                               
* RETURNS BUY RECORD IN TSARREC                                                 
*                                                                               
GETBUYTR NTR1  BASE=*,LABEL=*                                                   
         L     R1,0(R1)                                                         
         ZIC   R3,0(R1)                                                         
*                                                                               
         LR    R4,RA                                                            
         AHI   R4,TWAWORKQ                                                      
         USING TWAWORK,R4                                                       
*                                                                               
         XCEFL TSARREC,1002                                                     
BUYD     USING RBUYKEY,TSARREC+2                                                
         MVI   BUYD.RBUYKTYP,X'0B'                                              
         MVC   BUYD.RBUYKREP,REPALPHA                                           
         MVC   BUYD.RBUYKCON,TWACNUM                                            
         MVC   BUYD.RBUYKPLN,=3X'FF'                                            
*                                                                               
         MVI   TD.TSACTN,TSARDH    FIND FIRST BUY RECORD IN TSAR                
*                                                                               
GBUY10   DS    0H                  NOW READ NEXT UNTIL WE FIND TARGET           
         GOTO1 =A(GOTSAR),RR=RELO  MISSED BUY                                   
         TM    TD.TSERRS,TSEEOF                                                 
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLI   BUYD.RBUYKEY,X'0B'                                               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ZIC   RE,BUYD.RBUYKLIN                                                 
         CR    RE,R3                                                            
         BE    GBUYX                                                            
         XCEFL TSARREC,1002                                                     
         MVI   TD.TSACTN,TSANXT    READ NEXT                                    
         B     GBUY10                                                           
*                                                                               
GBUYX    DS    0H                                                               
         B     EXXMOD                                                           
         DROP  BUYD,R4                                                          
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* BUILD GRID OF AVALIABLE SPOTS TO BE MISSED                                    
* POPULATE GRID BY SEEDING WITH X'03' ELEMENT                                   
*                                                                               
BLDGRID  NTR1  BASE=*,LABEL=*                                                   
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'03'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RBUYDTEL,R6                                                      
*                                                                               
         MVC   GSTRDATE,RBUYDTST   GET BUY START DATE                           
         XC    BUYGRID,BUYGRID                                                  
         LA    R4,BUYGRID          POINT TO START OF GRID                       
         B     BGRID20             SKIP OFFSET FOR FIRST DATES                  
*                                                                               
BGRID10  DS    0H                                                               
         LA    R4,BUYGRID          RESET TO START OF GRID                       
         XC    ELEM,ELEM                                                        
         XC    WORK2,WORK2                                                      
         GOTO1 DATCON,DMCB,(3,GSTRDATE),(5,ELEM)                                
         GOTO1 DATCON,DMCB,(3,RBUYDTST),(5,ELEM+9)                              
         MVI   ELEM+8,C'-'                                                      
         GOTO1 PERVAL,DMCB,(17,ELEM),WORK2                                      
         CLI   DMCB+4,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
WKD      USING PERVALD,WORK2                                                    
         ZICM  R1,WKD.PVALNWKS,2   # WEEKS ROUNDED UP                           
         BCTR  R1,0                ADJUST FOR ROUNDING                          
         DROP  WKD                                                              
*                                                                               
         AR    R4,R1               BUMP TO STARTING CELL FOR THIS DATE          
*                                                                               
BGRID20  DS    0H                                                               
         ZIC   R1,RBUYDTWK         NUMBER OF WEEKS                              
         LTR   R1,R1                                                            
         BZ    BGRID40                                                          
*                                                                               
BGRID30  DS    0H                                                               
         MVC   0(1,R4),RBUYDTNW                                                 
         AHI   R4,1                                                             
         TM    RBUYDTIN,X'40'      WEEKLY OR EVERY OTHER??                      
         BZ    *+8                                                              
         AHI   R4,1                                                             
         BCTR  R1,0                                                             
         LTR   R1,R1                                                            
         BNZ   BGRID30                                                          
*                                                                               
BGRID40  DS    0H                  INCASE OF MULTIPLE EFFECTIVE DATES           
         BRAS  RE,NEXTEL                                                        
         BE    BGRID10                                                          
         DROP  R6                                                               
*                                                                               
* REMOVE RESERVED SPOTS FROM GRID                                               
*                                                                               
         LA    R6,RBUYREC          CHECK OLD STYLE MAKEGOOD                     
         MVI   ELCODE,X'06'                                                     
         BRAS  RE,GETEL                                                         
         BNE   BGRID60                                                          
         USING RBUYMSEL,R6                                                      
BGRID50  MVC   BSTRDATE,RBUYMSDT                                                
         MVC   BMSS#SPT,RBUYMSSP                                                
         DROP  R6                                                               
*                                                                               
         GOTO1 =A(REMSPTS),RR=RELO                                              
         BZ    *+6                                                              
         DC    H'0'                SHOULD NOT HAPPEN!                           
         BRAS  RE,NEXTEL                                                        
         BE    BGRID50                                                          
*                                                                               
BGRID60  DS    0H                  CHECK CREDIT                                 
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'07'                                                     
         BRAS  RE,GETEL                                                         
         BNE   BGRID80                                                          
         USING RBUYCREL,R6                                                      
BGRID70  MVC   BSTRDATE,RBUYCRDT                                                
         MVC   BMSS#SPT,RBUYCRSP                                                
         DROP  R6                                                               
*                                                                               
         GOTO1 =A(REMSPTS),RR=RELO                                              
         BZ    *+6                                                              
         DC    H'0'                SHOULD NOT HAPPEN!                           
         BRAS  RE,NEXTEL                                                        
         BE    BGRID70                                                          
*                                                                               
BGRID80  DS    0H                  CHECK MAKEGOOD OFFERS                        
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'66'                                                     
         BRAS  RE,GETEL                                                         
         BNE   BGRIDX                                                           
         USING RBMGMSEL,R6                                                      
*                                                                               
BGRID90  DS    0H                                                               
         GOTOR CLEANUP             THIS 66 POINTS TO A BOGUS RECORD             
         BNE   BGRID100            SO DO NO BUILD GRID                          
*                                  AFTER CLEANUP, R6 POINT TO NEXT ELM          
         MVC   BSTRDATE,RBMGMSDT                                                
         MVC   BMSS#SPT,RBMGMSSP                                                
*                                                                               
         GOTO1 =A(REMSPTS),RR=RELO                                              
         BNZ   BGRID95             POINT R6 TO NEXT ELEMENT                     
         ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     BGRID100                                                         
*                                                                               
BGRID95  DS    0H                                                               
*                                                                               
* SITUATION:                                                                    
* MAKEGOOD OFFER MISSED SPOTS GREATER THAN AVAILABLE MISSED SPOTS.              
* THIS CAN ONLY HAPPEN IF THE DARE REVISION FROM THE AGENCY REMOVED             
* SPOTS FROM THE TARGET MISSED LINE. WE NEED TO TELL THE USER TO                
* ADJUST THE MAKEGOOD OFFER FOR THIS LINE ACCORDINGLY.                          
*                                                                               
         LA    R3,190                                                           
         OI    CONCACTH+6,X'40'+X'80'                                           
         XC    MYWRK,MYWRK                                                      
         MVI   MYWRK,3                                                          
         MVC   MYWRK+1(2),RBMGMSGD                                              
         MVI   MYWRK+3,4                                                        
         DROP  R6                                                               
*                                                                               
         EDIT  RBUYKLIN,(3,MYWRK+4),ALIGN=LEFT                                  
         LA    R1,MYWRK                                                         
         ST    R1,DMCB+16                                                       
         GOTO1 GETTXT,DMCB,(R3),0,(C'I',0),0,,0                                 
         L     RD,BASERD           INSTANT UNWIND                               
         B     EXXMOD                                                           
*                                                                               
BGRID100 DS    0H                                                               
         CLI   0(R6),X'66'                                                      
         BE    BGRID90                                                          
*                                                                               
BGRIDX   DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* CLEANUP 66 ELEMENT IF THERE IS NO CORRESPONDING MAKE GOOD RECORD              
* EXIT NO = 66 POINTS TO BOGUS RECORD AND HAS BEEN CLEAN UP                     
*                                                                               
CLEANUP  NTR1  BASE=*,WORK=(R4,500),LABEL=*                                     
         MVC   MYWRK,2(R6)                                                      
         USING IMWORKD,R4                                                       
         MVC   IMSVKEY,KEY                                                      
*                                                                               
         XC    KEY,KEY                                                          
MKGD     USING RMKGKEY,KEY                                                      
         MVI   MKGD.RMKGKTYP,X'11'                                              
         MVC   MKGD.RMKGKREP,REPALPHA                                           
         MVC   MKGD.RMKGKOFF,RCONKOFF   INSERT OFFICE                           
         MVC   MKGD.RMKGKSTA,RCONKSTA   INSERT STATION+MEDIA                    
         MVC   MKGD.RMKGKCON,RBUYKCON   CONTRACT NUMBER                         
         MVC   MKGD.RMKGKGRP,RBMGMSGD-RBMGMSEL(R6)                              
         DROP  MKGD                                                             
         GOTO1 VHIGH                                                            
         CLC   KEYSAVE(21),KEY          GROUP EXIST?                            
         JNE   CLUP0100                 No, do clean up                         
         GOTO1 VGETREC,DMCB,IOAREA                                              
         LA    RE,IOAREA                                                        
         USING RMKGREC,RE                                                       
         TM    RMKGSCST,RMKGSAPQ        Applied?                                
         JZ    CLUPYES                  No, no need to clean up                 
         DROP  RE                                                               
*                                                                               
CLUP0100 DS    0H                                                               
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'66',RBUYREC),          >        
               (10,MYWRK),0                                                     
         MVC   KEY(27),RBUYREC                                                  
         GOTO1 VHIGH                                                            
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,IMIO                                                
         GOTO1 VMOVEREC,DMCB,RBUYREC,IMIO                                       
         GOTO1 VPUTREC,DMCB,IMIO                                                
*                                                                               
         XCEFL TSARREC,1002                                                     
         MVC   TSARREC+2(27),KEY                                                
         MVI   TD.TSACTN,TSARDH    RETRIEVE FIRST BUY RECORD                    
         GOTO1 =A(GOTSAR),RR=RELO                                               
         TM    TD.TSERRS,TSEEOF                                                 
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLI   TSARREC+2,X'0B'                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   TD.TSACTN,TSAPUT    PUT RECORD BACK                              
         GOTO1 VMOVEREC,DMCB,RBUYREC,TSARREC+2                                  
         SR    R1,R1                                                            
         ICM   R1,3,RBUYLEN        ADD LENGTH FIELD                             
         AHI   R1,2                                                             
         STCM  R1,3,TSARREC                                                     
*                                                                               
         GOTO1 =A(GOTSAR),RR=RELO                                               
         TM    TD.TSERRS,TSEEOF                                                 
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XCEFL TSARREC,1002        RE-READ TSAR RECORD                          
         MVC   TSARREC+2(27),KEY                                                
         MVI   TD.TSACTN,TSARDH    RETRIEVE FIRST BUY RECORD                    
         GOTO1 =A(GOTSAR),RR=RELO                                               
         TM    TD.TSERRS,TSEEOF                                                 
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLI   TSARREC+2,X'0B'                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    R5,RB                                                            
         B     CLUPNO                                                           
*                                                                               
CLUPYES  SR    R5,R5                                                            
CLUPNO   LTR   R5,R5                                                            
         MVC   KEY,IMSVKEY                                                      
CLEANUPX DS    0H                                                               
         B     EXXMOD                                                           
         DROP  R4                                                               
*                                                                               
* CHECK IF MISSED DATE/SPOTS VALID                                              
* REMOVE RESERVED SPOTS IN X'06', X'07' AND X'66' ELEMENTS                      
*                                                                               
* R5 IS POINTING TO CURRENT MISSED ELEMENT IN EL05BLK                           
* IF ERROR, R2=A(FIELD), R3=ERROR#                                              
CHKMISS  NTR1  BASE=*,LABEL=*                                                   
MGRD     USING RMKGMGEL,R5                                                      
*                                                                               
         LA    R3,47               MISSED DATE INVALID FOR MISSED LINE          
         ZICM  R2,RMKGMGLQ(R5),3                                                
         LTR   R2,R2                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,RA                                                            
*                                                                               
         MVC   MGDATE,MGRD.RMKGMGD1                                             
*                                                                               
CHKM10   DS    0H                                                               
         GOTO1 DATCON,DMCB,(3,MGDATE),(0,WORK)                                  
         GOTO1 GETDAY,DMCB,WORK,FULL                                            
         ZIC   RF,DMCB                                                          
         ZIC   RE,=X'80'                                                        
         SRL   RE,1                                                             
         BCT   RF,*-4                                                           
         STC   RE,DAYBITS                                                       
*                                                                               
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RBUYDYEL,R6                                                      
*                                                                               
CHKM20   DS    0H                                                               
         MVC   MYWRK(1),RBUYDAYS                                                
         OC    MYWRK(1),DAYBITS                                                 
         CLC   RBUYDAYS,MYWRK                                                   
         BE    CHKM25                                                           
         BAS   RE,NEXTEL                                                        
         BE    CHKM20                                                           
         B     CHKMNO                                                           
         DROP  R6                                                               
*                                                                               
CHKM25   DS    0H                                                               
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'03'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RBUYDTEL,R6                                                      
*                                                                               
CHKM30   DS    0H                                                               
         CLC   MGDATE,RBUYDTST                                                  
         BL    CHKMNO                                                           
*                                                                               
CHKM40   DS    0H                                                               
         CLC   MGDATE,RBUYDTED                                                  
         BNH   CHKM50                                                           
         BRAS  RE,NEXTEL                                                        
         BE    CHKM30                                                           
         B     CHKMNO                                                           
         DROP  R6                                                               
*                                                                               
CHKM50   DS    0H                                                               
         MVC   BSTRDATE,MGDATE                                                  
         MVC   BMSS#SPT,MGRD.RMKGMGSP                                           
         GOTO1 =A(REMSPTS),RR=RELO                                              
         BZ    CHKM60                                                           
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         LA    R3,457              TOTAL SPOTS EXCEEDS SPOTS PER WEEK           
         B     CHKMNO                                                           
*                                                                               
CHKM60   DS    0H                                                               
         OC    MGRD.RMKGMGD2,MGRD.RMKGMGD2                                      
         BZ    CHKMYES                                                          
*                                                                               
         LA    R3,7                                                             
         GOTO1 DATCON,DMCB,(3,MGDATE),WORK                                      
         GOTO1 ADDAY,DMCB,WORK,WORK,(R3)                                        
         GOTO1 DATCON,DMCB,WORK,(3,MGDATE)                                      
         CLC   MGDATE,MGRD.RMKGMGD2                                             
         BNH   CHKM10                                                           
*                                                                               
CHKMYES  SR    RC,RC                                                            
CHKMNO   LTR   RC,RC                                                            
CHKMXIT  XIT1  REGS=(R2,R3)                                                     
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* REMOVE SPOTS FROM GRID                                                        
*                                                                               
REMSPTS  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,BUYGRID          RESET TO START OF GRID                       
         XC    ELEM,ELEM                                                        
         XC    WORK2,WORK2                                                      
         GOTO1 DATCON,DMCB,(3,GSTRDATE),(5,ELEM)                                
         GOTO1 DATCON,DMCB,(3,BSTRDATE),(5,ELEM+9)                              
         MVI   ELEM+8,C'-'                                                      
         GOTO1 PERVAL,DMCB,(17,ELEM),WORK2                                      
         CLI   DMCB+4,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
WKD      USING PERVALD,WORK2                                                    
         ZICM  R1,WKD.PVALNWKS,2   # WEEKS ROUNDED UP                           
         BCTR  R1,0                ADJUST FOR ROUNDING                          
         DROP  WKD                                                              
*                                                                               
         AR    R2,R1               BUMP TO STARTING CELL FOR THIS DATE          
*                                                                               
         CLC   BMSS#SPT,0(R2)      SPTS MUST BE AVAILABLE TO BE REMOVED         
         BH    RSPTSNO                                                          
         ZIC   RF,BMSS#SPT                                                      
         ZIC   RE,0(R2)                                                         
         SR    RE,RF                                                            
         STC   RE,0(R2)                                                         
*                                                                               
RSPTSYES SR    RC,RC                                                            
RSPTSNO  LTR   RC,RC                                                            
         B     EXXMOD                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* UPDATE OFFER RECORDS WITH X'05' MISSED ELEMENTS FROM TARGET BUYS              
* ALSO ADD X'04' MG= COMMENT ELEMENT                                            
*                                                                               
MGMUPOFF NTR1  BASE=*,LABEL=*                                                   
         LR    R4,RA                                                            
         AHI   R4,TWAWORKQ                                                      
         USING TWAWORK,R4                                                       
*                                                                               
         XC    KEY,KEY                                                          
MKGD     USING RMKGKEY,KEY                                                      
         MVI   MKGD.RMKGKTYP,X'11'                                              
         MVC   MKGD.RMKGKREP,REPALPHA                                           
         MVC   MKGD.RMKGKOFF,RCONKOFF   INSERT OFFICE                           
         MVC   MKGD.RMKGKSTA,RCONKSTA   INSERT STATION+MEDIA                    
         MVC   MKGD.RMKGKCON,TWACNUM    CONTRACT NUMBER                         
         MVC   MKGD.RMKGKGRP,MGHMGRP    GROUP CODE                              
         MVC   MKGD.RMKGKPLN,=3X'FF'    SKIP GROUP RECORD                       
*        LA    R2,MGHMOFFH                                                      
*        GOTO1 VPACK                                                            
*        STC   R0,MKGD.RMKGKLIN         MAKEGOOD OFFER NUMBER                   
         MVI   MKGD.RMKGKLIN,1                                                  
         DROP  MKGD,R4                                                          
*                                                                               
         XCEFL TSARREC,1002                                                     
         MVC   TSARREC+2(27),KEY                                                
         MVI   TD.TSACTN,TSARDH    RETRIEVE FIRST DETAIL OFFER RECORD           
         GOTO1 =A(GOTSAR),RR=RELO                                               
         TM    TD.TSERRS,TSEEOF                                                 
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLC   TSARREC+2(RMKGKMLN-RMKGKEY),KEY                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
MGMUOF10 DS    0H                                                               
         GOTO1 VMOVEREC,DMCB,TSARREC+2,RMKGREC                                  
*                                                                               
         LA    R5,EL05BLK                                                       
         LA    R4,15                                                            
*                                                                               
MGMUOF20 DS    0H                                                               
         CLI   0(R5),0                                                          
         BE    MGMUOF30                                                         
*                                                                               
* SET MISSED SPOTS TO ZERO AFTER FIRST DETAIL OFFER RECORD                      
*                                                                               
         XC    ELEM,ELEM                                                        
         MVC   ELEM(RMKGMGLQ),0(R5)                                             
         MVI   LINECTR,0                                                        
         MVN   LINECTR,RMKGKRTY                                                 
         CLI   LINECTR,1                                                        
         BE    *+8                                                              
         MVI   ELEM+RBUYMGSP-RBUYMGEL,0                                         
*                                                                               
MGMUOF25 DS    0H                                                               
         GOTO1 VADDELEM,DMCB,RMKGREC,ELEM                                       
         AHI   R5,EL05BLQ                                                       
         BCT   R4,MGMUOF20                                                      
*                                                                               
MGMUOF30 DS    0H                                                               
         BAS   RE,BUILD04S                                                      
*                                                                               
         GOTO1 VMOVEREC,DMCB,RMKGREC,TSARREC+2                                  
         SR    R1,R1                                                            
         ICM   R1,3,RMKGLEN        ADD LENGTH FIELD                             
         AHI   R1,2                                                             
         STCM  R1,3,TSARREC                                                     
         MVI   TD.TSACTN,TSAWRT    WRITE RECORD BACK                            
         GOTO1 =A(GOTSAR),RR=RELO                                               
         TM    TD.TSERRS,TSEEOF                                                 
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
MGMUOF40 DS    0H                                                               
         XCEFL TSARREC,1002                                                     
         MVI   TD.TSACTN,TSANXT                                                 
         GOTO1 =A(GOTSAR),RR=RELO                                               
         TM    TD.TSERRS,TSEEOF                                                 
         BNZ   MGMUOFX                                                          
         CLC   TSARREC+2(RMKGKMLN-RMKGKEY),KEY                                  
         BE    MGMUOF10                                                         
         DC    H'0'                                                             
*                                                                               
MGMUOFX  DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
* FOR PREEMPT/CREDIT AND LATE RUN OFFERS:                                       
* UPDATE OFFER RECORDS WITH X'05' MISSED ELEMENTS FROM TARGET BUYS              
*                                                                               
MGPUPOFF NTR1  BASE=*,LABEL=*                                                   
         LR    R4,RA                                                            
         AHI   R4,TWAWORKQ                                                      
         USING TWAWORK,R4                                                       
*                                                                               
         XC    KEY,KEY                                                          
MKGD     USING RMKGKEY,KEY                                                      
         MVI   MKGD.RMKGKTYP,X'11'                                              
         MVC   MKGD.RMKGKREP,REPALPHA                                           
         MVC   MKGD.RMKGKOFF,RCONKOFF   INSERT OFFICE                           
         MVC   MKGD.RMKGKSTA,RCONKSTA   INSERT STATION+MEDIA                    
         MVC   MKGD.RMKGKCON,TWACNUM    CONTRACT NUMBER                         
         MVC   MKGD.RMKGKGRP,MGHMGRP    GROUP CODE                              
         MVC   MKGD.RMKGKPLN,=3X'FF'    SKIP GROUP RECORD                       
*        LA    R2,MGHMOFFH                                                      
*        GOTO1 VPACK                                                            
*        STC   R0,MKGD.RMKGKLIN         MAKEGOOD OFFER NUMBER                   
         MVI   MKGD.RMKGKLIN,1                                                  
         MVC   MKGD.RMKGKRTY,TWASTRT#   SET 'START AT' LINE NUMBER              
*        CLI   MGHMOPT,C'C'             OFFERING CHOICE?                        
*        BNE   *+8                                                              
*        OI    MKGD.RMKGKRTY,X'10'                                              
         DROP  MKGD                                                             
*                                                                               
         XCEFL TSARREC,1002                                                     
         MVC   TSARREC+2(27),KEY                                                
         MVI   TD.TSACTN,TSARDH    RETRIEVE FIRST DETAIL OFFER                  
         GOTO1 =A(GOTSAR),RR=RELO                                               
         TM    TD.TSERRS,TSEEOF+TSERNF                                          
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R5,EL05BLK                                                       
*                                                                               
MGPUOF10 DS    0H                                                               
         GOTO1 VMOVEREC,DMCB,TSARREC+2,RMKGREC                                  
*                                                                               
         TM    RMKGRTS,X'20'       LATE RUN BONUS LINES NEED NOT                
         BO    MGPUOFX             TO SPECIFY TARGET                            
*                                                                               
         CLI   0(R5),0                                                          
         BE    MGPUOFX                                                          
         GOTO1 VADDELEM,DMCB,RMKGREC,(R5)                                       
         AHI   R5,EL05BLQ                                                       
*                                                                               
         TM    RMKGRTS,X'08'+X'04' ADD MG= COMMENT FOR LATE RUN                 
         BZ    MGPUOF20                                                         
         TM    RMKGRTS,X'20'       BUT NOT FOR BONUS PORTION                    
         BO    MGPUOF20                                                         
         BAS   RE,BUILD04S                                                      
*                                                                               
MGPUOF20 DS    0H                                                               
         GOTO1 VMOVEREC,DMCB,RMKGREC,TSARREC+2                                  
         SR    R1,R1                                                            
         ICM   R1,3,RMKGLEN        ADD LENGTH FIELD                             
         AHI   R1,2                                                             
         STCM  R1,3,TSARREC                                                     
         MVI   TD.TSACTN,TSAWRT    WRITE RECORD BACK                            
         GOTO1 =A(GOTSAR),RR=RELO                                               
         TM    TD.TSERRS,TSEEOF                                                 
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XCEFL TSARREC,1002                                                     
         MVI   TD.TSACTN,TSANXT                                                 
         GOTO1 =A(GOTSAR),RR=RELO                                               
         TM    TD.TSERRS,TSEEOF                                                 
         BNZ   MGPUOFX                                                          
         CLC   TSARREC+2(RMKGKMLN-RMKGKEY),KEY                                  
         BE    MGPUOF10                                                         
         DC    H'0'                                                             
*                                                                               
MGPUOFX  DS    0H                                                               
         B     EXXMOD                                                           
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
* BUILD X'04' MG= ELEMENTS                                                      
*                                                                               
BUILD04S NTR1  BASE=*,LABEL=*                                                   
         XC    ELEM,ELEM                                                        
MG04D    USING RMKGCMEL,ELEM                                                    
         MVI   MG04D.RMKGCMCD,X'04'                                             
         MVC   MG04D.RMKGCMNT(3),=C'MG='                                        
         LA    R6,RMKGREC                                                       
         MVI   ELCODE,X'05'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RMKGMGEL,R6                                                      
*                                                                               
         LA    R7,MG04D.RMKGCMNT+3                                              
         GOTO1 DATCON,DMCB,(X'83',RMKGMGD1),(4,(R7))                            
         ZIC   RF,DMCB+4           GET L(DATE OUTPUT)                           
         AR    R7,RF               ADD TO PTR TO ELTBILD                        
*                                                                               
         MVI   TARGETBY,0                                                       
*                                                                               
BILX0030 EQU   *                                                                
         CLC   TARGETBY,RMKGMGLI   MORE DATES FOR SAME TARGET BUY?              
         BE    BILX0040                                                         
         MVI   0(R7),C'-'          INSERT SEPARATOR                             
         AHI   R7,1                BUMP TO NEXT POSITION                        
         EDIT  (1,RMKGMGLI),(3,(R7)),ALIGN=LEFT                                 
*                                  INSERT MISSED LINE NUMBER                    
         AR    R7,R0               ADD SIGNIFICANT DIGITS TO ADDRESS            
         MVC   TARGETBY,RMKGMGLI                                                
*                                                                               
BILX0040 EQU   *                                                                
         CLI   RMKGMGSP,1          1 SIGNIFICANT SPOT MISSED?                   
         BE    BILX0100            YES                                          
         MVI   0(R7),C'('          NO  - INSERT SPOT # OVERRIDE                 
         AHI   R7,1                BUMP TO NEXT POSITION                        
         CLI   RMKGMGSP,0          ANY SPOTS HERE?                              
         BNE   BILX0060            YES - INSERT IT INTO LINE                    
         MVI   0(R7),C'0'          NO  - INSERT A ZERO                          
         LA    R0,1                SET LENGTH OF DATA TO 1                      
         B     BILX0080                                                         
BILX0060 EQU   *                                                                
         EDIT  (1,RMKGMGSP),(3,(R7)),ALIGN=LEFT                                 
*                                  INSERT # MISSED SPOTS                        
*                                     CONTINUE TO USE ELTBILD+32 AS             
*                                        WORK AREA FOR EDIT                     
BILX0080 EQU   *                                                                
         AR    R7,R0               ADD SIGNIFICANT DIGITS TO ADDRESS            
         MVI   0(R7),C')'          CLOSE FRAME                                  
         AHI   R7,1                                                             
*                                                                               
BILX0100 EQU   *                                                                
         BRAS  RE,NEXTEL                                                        
         BNE   BILX0200                                                         
         MVI   0(R7),C','          MORE DATES                                   
         AHI   R7,1                                                             
         GOTO1 DATCON,DMCB,(X'83',RMKGMGD1),(4,(R7))                            
         ZIC   RF,DMCB+4           GET L(DATE OUTPUT)                           
         AR    R7,RF               ADD TO PTR TO ELTBILD                        
         B     BILX0030                                                         
*                                                                               
BILX0200 EQU   *                                                                
         LA    RE,ELEM                                                          
         SR    R7,RE                                                            
         STC   R7,MG04D.RMKGCMLN                                                
         CLI   MG04D.RMKGCMLN,62                                                
         BNH   *+8                                                              
         MVI   MG04D.RMKGCMLN,62   MAX LENGTH                                   
         DROP  R6,MG04D                                                         
*                                                                               
         GOTO1 VADDELEM,DMCB,RMKGREC,ELEM                                       
*                                                                               
B04SX    DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* FOR REGULAR MAKEGOOD OFFER:                                                   
* LOOP THRU ALL BUYS IN TSAR BUFFER                                             
* ADD X'66' REFERENCE ELEMENTS TO TARGET BUYS                                   
*                                                                               
MGM66ELM NTR1  BASE=*,LABEL=*                                                   
         GOTO1 =A(GETLAST#),RR=RELO                                             
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'0B'                                                        
*                                                                               
         XCEFL TSARREC,1002                                                     
         MVC   TSARREC+2(27),KEY                                                
         MVI   TD.TSACTN,TSARDH    RETRIEVE FIRST BUY RECORD                    
         GOTO1 =A(GOTSAR),RR=RELO                                               
         TM    TD.TSERRS,TSEEOF                                                 
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLI   TSARREC+2,X'0B'                                                  
         BE    MGMEL20                                                          
         DC    H'0'                                                             
*                                                                               
MGMEL10  DS    0H                                                               
         GOTO1 =A(GOTSAR),RR=RELO                                               
         TM    TD.TSERRS,TSEEOF                                                 
         BNZ   MGMELX                                                           
         CLI   TSARREC+2,X'0B'                                                  
         BNE   MGMELX                                                           
*                                                                               
MGMEL20  DS    0H                                                               
         GOTO1 VMOVEREC,DMCB,TSARREC+2,RBUYREC                                  
*                                                                               
* FOR REGULAR OFFER:                                                            
* NUMBER OF X'66' ELEMENTS ADDED IS THE NUMBER OF MISSED X'05'S FOR             
* TARGET BUY TIMES THE NUMBER OF OFFER LINES                                    
* (*** THIS IS NO LONGER TRUE. WILL NOT ADD X'66' OFFER LINE HOLDERS)           
*                                                                               
* THIS IS FURTHER MULTIPLIED BY THE NUMBER OF WEEKS IF A RANGE OF DATES         
* ARE SPECIFIED                                                                 
*                                                                               
         MVI   LINECTR,1                                                        
*                                                                               
MGMEL30  DS    0H                                                               
         LA    R4,15                                                            
         LA    R5,EL05BLK                                                       
EL05D    USING RMKGMGEL,R5                                                      
MGD      USING RMKGREC,MGDTREC                                                  
*                                                                               
MGMEL40  DS    0H                  OFFER FOR THIS BUY?                          
         CLC   RBUYKLIN,EL05D.RMKGMGLI                                          
         BNE   MGMEL55                                                          
*                                                                               
         MVC   MGDATE,EL05D.RMKGMGD1                                            
*                                                                               
* CONSTRUCT X'66' REFERENCE ELEMENT                                             
*                                                                               
MGMEL45  DS    0H                                                               
         XC    ELEM,ELEM                                                        
MG66D    USING RBMGMSEL,ELEM                                                    
         MVI   MG66D.RBMGMSCD,X'66'                                             
         MVI   MG66D.RBMGMSLN,12                                                
         MVC   MG66D.RBMGMSGD,MGD.RMKGKGRP                                      
         MVC   MG66D.RBMGMSR#,RBUYKLIN                                          
         MVC   MG66D.RBMGMSR#+1(1),MGD.RMKGKLIN                                 
         MVC   MG66D.RBMGMMUL,LINECTR                                           
         CLI   MGHMOPT,C'C'        OFFERING ALL OR CHOICE?                      
         BNE   *+8                                                              
         OI    MG66D.RBMGMMUL,X'10'                                             
         MVC   MG66D.RBMGMSDT,MGDATE                                            
         MVC   MG66D.RBMGMSLI,MGD.RMKGKLIN                                      
*                                                                               
         CLI   LINECTR,1           MISSED SPOTS ON FIRST OFFER ONLY             
         BH    *+10                                                             
         MVC   MG66D.RBMGMSSP,EL05D.RMKGMGSP                                    
*                                                                               
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),(0,RBUYREC),(0,ELEM),0             
*        TM    DMCB+12,X'05'       REC TOO LONG                                 
*        BZ    MGMEL50                                                          
         CLC   RBUYLEN,=AL2(972)                                                
         BNH   MGMEL50                                                          
         LA    R2,CONCACTH                                                      
         LA    R3,339              RECORD FULL - CHANGE NOT PROCESSED           
         B     ERROR                                                            
*                                                                               
MGMEL50  DS    0H                                                               
         OC    EL05D.RMKGMGD2,EL05D.RMKGMGD2                                    
         BZ    MGMEL55                                                          
         LA    R3,7                                                             
         GOTO1 DATCON,DMCB,(3,MGDATE),WORK                                      
         GOTO1 ADDAY,DMCB,WORK,WORK,(R3)                                        
         GOTO1 DATCON,DMCB,WORK,(3,MGDATE)                                      
         CLC   MGDATE,EL05D.RMKGMGD2                                            
         BNH   MGMEL45                                                          
         DROP  MGD,EL05D                                                        
*                                                                               
MGMEL55  DS    0H                                                               
         AHI   R5,EL05BLQ                                                       
         BCT   R4,MGMEL40                                                       
*                                                                               
         BRAS  RE,FIX66ELM                                                      
*                                                                               
* NO LONGER ADD 0 RESERVED SPOT PLACE HOLDERS                                   
*                                                                               
*&&DO                                                                           
         CLC   LINECTR,LASTMG#     LOOP THRU ALL OFFERS                         
         BNL   MGMEL60                                                          
         ZIC   RE,LINECTR                                                       
         AHI   RE,1                                                             
         STC   RE,LINECTR                                                       
         B     MGMEL30                                                          
*&&                                                                             
*                                                                               
MGMEL60  DS    0H                                                               
         MVI   TD.TSACTN,TSAPUT    PUT RECORD BACK                              
         GOTO1 VMOVEREC,DMCB,RBUYREC,TSARREC+2                                  
         SR    R1,R1                                                            
         ICM   R1,3,RBUYLEN        ADD LENGTH FIELD                             
         AHI   R1,2                                                             
         STCM  R1,3,TSARREC                                                     
*                                                                               
         GOTO1 =A(GOTSAR),RR=RELO                                               
         TM    TD.TSERRS,TSEEOF                                                 
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XCEFL TSARREC,1002                                                     
         MVI   TD.TSACTN,TSANXT                                                 
         B     MGMEL10                                                          
*                                                                               
MGMELX   DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* FOR PREEMPT AND LATE RUN OFFERS:                                              
* LOOP THRU X'05' ELEMENT BLOCK                                                 
* ADD X'66' REFERENCE ELEMENTS TO TARGET BUYS                                   
* X'05' ELEMENT BLOCK EL05BLK IS *NOT* SORTED                                   
*                                                                               
MGP66ELM NTR1  BASE=*,LABEL=*                                                   
         GOTO1 =A(GETLAST#),RR=RELO                                             
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'0B'                                                        
*                                                                               
         XCEFL TSARREC,1002                                                     
         MVC   TSARREC+2(27),KEY                                                
         MVI   TD.TSACTN,TSARDH    RETRIEVE FIRST BUY RECORD                    
         GOTO1 =A(GOTSAR),RR=RELO                                               
         TM    TD.TSERRS,TSEEOF                                                 
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLI   TSARREC+2,X'0B'                                                  
         BE    MGPEL20                                                          
         DC    H'0'                                                             
*                                                                               
MGPEL10  DS    0H                                                               
         GOTO1 =A(GOTSAR),RR=RELO                                               
         TM    TD.TSERRS,TSEEOF                                                 
         BNZ   MGPELX                                                           
         CLI   TSARREC+2,X'0B'                                                  
         BNE   MGPELX                                                           
*                                                                               
MGPEL20  DS    0H                                                               
         GOTO1 VMOVEREC,DMCB,TSARREC+2,RBUYREC                                  
*                                                                               
* FOR PREEMPT AND LATE RUN:                                                     
* NUMBER OF X'66' ELEMENTS ADDED IS ONE OFFER PER TARGET BUY                    
*                                                                               
         LR    R4,RA                                                            
         AHI   R4,TWAWORKQ                                                      
         USING TWAWORK,R4                                                       
         MVC   LINECTR,TWASTRT#                                                 
         DROP  R4                                                               
*                                                                               
         LA    R4,15               MAX NUMBER OF OFFER RECORDS                  
         LA    R5,EL05BLK                                                       
EL05D    USING RMKGMGEL,R5                                                      
MGD      USING RMKGREC,MGDTREC                                                  
*                                                                               
MGPEL40  DS    0H                  OFFER FOR THIS BUY?                          
         CLC   RBUYKLIN,EL05D.RMKGMGLI                                          
         BNE   MGPEL50                                                          
*                                                                               
* CONSTRUCT X'66' REFERENCE ELEMENT                                             
*                                                                               
         XC    ELEM,ELEM                                                        
MG66D    USING RBMGMSEL,ELEM                                                    
         MVI   MG66D.RBMGMSCD,X'66'                                             
         MVI   MG66D.RBMGMSLN,12                                                
         MVC   MG66D.RBMGMSGD,MGD.RMKGKGRP                                      
         MVC   MG66D.RBMGMSR#,RBUYKLIN                                          
         MVC   MG66D.RBMGMSR#+1(1),MGD.RMKGKLIN                                 
         MVC   MG66D.RBMGMMUL,LINECTR                                           
         CLI   MGHMOPT,C'C'        OFFERING ALL OR CHOICE?                      
         BNE   *+8                                                              
         OI    MG66D.RBMGMMUL,X'10'                                             
         MVC   MG66D.RBMGMSDT,EL05D.RMKGMGD1                                    
         MVC   MG66D.RBMGMSLI,MGD.RMKGKLIN                                      
         MVC   MG66D.RBMGMSSP,EL05D.RMKGMGSP                                    
         DROP  MGD,EL05D                                                        
*                                                                               
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),(0,RBUYREC),(0,ELEM),0             
*        TM    DMCB+12,X'05'       REC TOO LONG                                 
*        BZ    MGPEL50                                                          
         CLC   RBUYLEN,=AL2(972)                                                
         BNH   MGPEL50                                                          
         LA    R2,CONCACTH                                                      
         LA    R3,339              RECORD FULL - CHANGE NOT PROCESSED           
         B     ERROR                                                            
*                                                                               
MGPEL50  DS    0H                                                               
         AHI   R5,EL05BLQ                                                       
         ZIC   RE,LINECTR                                                       
         AHI   RE,1                                                             
         STC   RE,LINECTR                                                       
*                                                                               
MGPEL55  DS    0H                                                               
         BCT   R4,MGPEL40                                                       
*                                                                               
         BRAS  RE,FIX66ELM                                                      
*                                                                               
* RESEQUENCE ALL X'66' ELEMENTS FOR THIS GROUP                                  
*                                                                               
*        GOTO1 =A(RESEQ66),RR=RELO                                              
*                                                                               
*                                                                               
* ONLY ONE TARGET PER OFFER FOR PREEMPT OR LATE RUN                             
*                                                                               
MGPEL80  DS    0H                                                               
         MVI   TD.TSACTN,TSAPUT    PUT RECORD BACK                              
         GOTO1 VMOVEREC,DMCB,RBUYREC,TSARREC+2                                  
         SR    R1,R1                                                            
         ICM   R1,3,RBUYLEN        ADD LENGTH FIELD                             
         AHI   R1,2                                                             
         STCM  R1,3,TSARREC                                                     
*                                                                               
         GOTO1 =A(GOTSAR),RR=RELO                                               
         TM    TD.TSERRS,TSEEOF                                                 
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XCEFL TSARREC,1002                                                     
         MVI   TD.TSACTN,TSANXT                                                 
         B     MGPEL10                                                          
*                                                                               
MGPELX   DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* RESEQUENCE ALL X'66' ELEMENTS FOR THIS GROUP                                  
* WILL ONLY WORK FOR MGP, MLR, AND MLB                                          
*                                                                               
RESEQ66  NTR1  BASE=*,LABEL=*                                                   
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'66'                                                     
         BRAS  RE,GETEL                                                         
         BNE   RESQX                                                            
         LA    R5,1                                                             
         USING RBMGMSEL,R6                                                      
MGD      USING RMKGREC,MGDTREC                                                  
*                                                                               
RESQ10   DS    0H                                                               
         CLC   RBMGMSGD,MGD.RMKGKGRP                                            
         BNE   RESQ20                                                           
         STC   R5,RBMGMMUL                                                      
         AHI   R5,1                                                             
*                                                                               
RESQ20   DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         BE    RESQ10                                                           
*                                                                               
RESQX    DS    0H                                                               
         B     EXXMOD                                                           
         DROP  R6,MGD                                                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* Remove X'66' in buy record if not removed earlier                             
*                                                                               
FIX66ELM NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R4,15               Maximum number of offers                     
         LA    R5,EL05BLK          Point to table of saved offer elems          
         LA    R6,RBUYREC                                                       
         MVI   BYTE3,0             Number of offer saved in table count         
*                                                                               
FX66_10  CLC   RBUYKLIN-RBUYKEY(L'RBUYKLIN,R6),RMKGMGLI-RMKGMGEL(R5)            
         JE    FX66_X                                                           
         CLI   RMKGMGLI-RMKGMGEL(R5),0                                          
         JE    FX66_12                                                          
         LLC   RE,BYTE3                                                         
         AHI   RE,1                                                             
         STC   RE,BYTE3                                                         
FX66_12  AHI   R5,EL05BLQ          Point to next entry in table                 
         JCT   R4,FX66_10                                                       
*                                                                               
         LA    R6,RBUYREC                                                       
         LA    R6,RBUYELEM-RBUYREC(R6)                                          
FX66_20  CLI   0(R6),0             End of record?                               
         JE    FX66_X                                                           
         CLI   0(R6),X'66'         Makegood buy missed element code?            
         JE    FX66_30                                                          
         LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         J     FX66_20                                                          
*                                                                               
         USING RBMGMSEL,R6                                                      
FX66_30  LLC   RE,RBMGMMUL         Offer line number to be fixed                
         CHI   RE,7                Offer lines need to be fixed?                
         JNH   FX66_X                                                           
         LLC   RF,BYTE3            Number of offer saved in table               
         SR    RE,RF                                                            
         CHI   RE,0                                                             
         JNH   FX66_X                                                           
         LLC   RF,BYTE3            Number of offer saved in table               
         BCTR  RF,0                                                             
         AR    RE,RF                                                            
         STC   RE,RBMGMMUL         Acutal line number                           
*                                                                               
FX66_X   XIT1                                                                   
*                                                                               
         LTORG                                                                  
         DROP  RB,R6                                                            
         EJECT                                                                  
*                                                                               
* GET HIGHEST RECORD # FOR THIS OFFER                                           
*                                                                               
GETLAST# NTR1  BASE=*,LABEL=*                                                   
         MVI   LASTMG#,0                                                        
         LA    R6,TSARREC+2                                                     
         USING RMKGREC,R6                                                       
         XC    KEY,KEY                                                          
         MVI   KEY,X'11'                                                        
*                                  GET HIGHEST RECORD # FOR THIS OFFER          
         MVC   TSARREC+2(27),KEY                                                
         MVI   TD.TSACTN,TSARDH                                                 
GET#5    GOTO1 =A(GOTSAR),RR=RELO                                               
         TM    TD.TSERRS,TSEEOF                                                 
         BO    GET#X                                                            
         MVC   LASTMG#,RMKGKRTY                                                 
         MVI   TD.TSACTN,TSANXT                                                 
         B     GET#5                                                            
*                                                                               
GET#X    DS    0H                                                               
         CLI   LASTMG#,0                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         B     EXXMOD                                                           
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* DELETE MAKEGOOD OFFER AND REMOVE ALL REFERENCED ELEMENTS IN TARGET            
* BUYS                                                                          
*                                                                               
DELMGMF  NTR1  BASE=*,LABEL=*                                                   
         TM    FMGTYPE,FMGPOFFQ+FMLROFFQ+FMLBOFFQ                               
         BNZ   DELMG10                                                          
*                                                                               
* FILL TSAR BUFFER WITH OFFER RECORDS AND ORIGINAL TARGET BUYS.                 
* REGULAR OFFER HAS ALL OF THE TARGET BUYS REFERENCE IN THE FIRST               
* DETAIL RECORD                                                                 
*                                                                               
         GOTO1 =A(MGMGR),RR=RELO                                                
         B     DELMG20                                                          
*                                                                               
* FILL TSAR BUFFER WITH OFFER RECORDS AND ORIGINAL TARGET BUYS.                 
* PREEMPT, LATE RUN AND LATE RUN WITH BONUS POTENTIALLY HAVE                    
* DIFFERENT TARGETS IN EACH DETAIL RECORD                                       
*                                                                               
DELMG10  DS    0H                                                               
         GOTO1 =A(MGPGR),RR=RELO                                                
*                                                                               
* DUMP RECORDS FROM TSAR TO FILE                                                
*                                                                               
DELMG20  DS    0H                                                               
*                                                                               
* NEED TO SAVE OFF INFO IN X'0A' FOR PASSIVE KEY CLEAN UP LATER                 
*                                                                               
         GOTOR OLD0AELT            FIND OLD X'0A' ELEMENT                       
*                                                                               
         GOTO1 =A(DUMPTSAR),RR=RELO                                             
*                                                                               
* CHECK IF ALL OFFERS DELETED FOR THIS HEADER. IF SO, DELETE HEADER             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(RMKGKPLN-RMKGKEY),MGDTREC                                    
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VSEQ                                                             
         CLC   KEY(RMKGKPLN-RMKGKEY),KEYSAVE                                    
         BE    DELMGX                                                           
*                                                                               
* DELETE HEADER RECORD                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(RMKGKPLN-RMKGKEY),MGDTREC                                    
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,RMKGREC                                             
         OI    RMKGCNTL,X'80'                                                   
         GOTO1 VPUTREC,DMCB,RMKGREC                                             
         OI    KEY+27,X'80'                                                     
         GOTO1 VWRITE                                                           
*                                                                               
DELMGX   LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         OI    TWACONFG,TW_MGOXQ   Makegood offer is deleted                    
         OI    TWACONFG,TW_MGOMQ   Makegood offer is updated, do MQ msg         
         DROP  RF                                                               
*                                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
* DUMP RECORDS FROM TSAR TO FILE                                                
*                                                                               
DUMPTSAR NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   FMGTYPE,FMGBOFFQ    IF BONUS, NO BUYS IN TSAR BUFFER             
         BE    DUMP20                                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'0B'                                                        
*                                                                               
         XCEFL TSARREC,1002                                                     
         MVC   TSARREC+2(27),KEY                                                
         MVI   TD.TSACTN,TSARDH    RETRIEVE FIRST BUY TO WRITE BACK             
         GOTO1 =A(GOTSAR),RR=RELO                                               
         TM    TD.TSERRS,TSEEOF                                                 
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLI   TSARREC+2,X'0B'     START WITH BUY                               
         BNE   DUMP20              OK IF NO TARGETS BUYS SINCE REVISION         
*                                  CAN DELETE THEM                              
DUMP10   DS    0H                                                               
         MVC   KEY(27),TSARREC+2                                                
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VHIGH               READ KEY                                     
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,RBUYREC                                             
         GOTO1 VMOVEREC,DMCB,TSARREC+2,RBUYREC                                  
*                                                                               
* COMMENTING OUT AS WE WILL PUT BACK THE X'66' BUT ONLY ONES WITH               
* MISSED SPOTS. IN THE PAST, WE WERE CREATING ONES WITH 0 MISSED AS             
* PLACE HOLDER WHICH CAUSED RECORD OVERFLOW ISSUES (NOV18/11 SKU)               
*                                                                               
*                                                                               
*&&DO                                                                           
*  DELETE ALL X'66' MAKEGOOD BUY MISSED ELEMENTS BEFORE PUTREC - THEY           
*  ARE ONLY RETAINED UNTIL NOW TO PREVENT OVERLY LARGE MAKEGOOD INPUT           
*                                                                               
DE6620   DS    0H                                                               
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'66'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DE66X               NO MORE X'66' ELEMEENTS                      
*                                                                               
         GOTO1 VRECUP,DMCB,(2,RBUYREC),(R6),(R6)                                
         B     DE6620                                                           
*&&                                                                             
*                                                                               
DE66X    DS    0H                                                               
*                                                                               
         GOTO1 VPUTREC,DMCB,RBUYREC                                             
*                                                                               
         XCEFL TSARREC,1002                                                     
         MVI   TD.TSACTN,TSANXT                                                 
         GOTO1 =A(GOTSAR),RR=RELO                                               
         TM    TD.TSERRS,TSEEOF                                                 
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLI   TSARREC+2,X'0B'                                                  
         BE    DUMP10                                                           
*                                                                               
* NOW PROCESS MAKEGOOD OFFERS                                                   
* - WRITE BACK HEADER RECORD                                                    
* - DELETED ALL DETAIL RECORD WITHIN THIS OFFER                                 
* - IF MGC, WRITE BACK ACTIVE RECORDS FROM TSAR TO FILE                         
*                                                                               
DUMP20   DS    0H                                                               
         LR    R4,RA                                                            
         AHI   R4,TWAWORKQ                                                      
         USING TWAWORK,R4                                                       
*                                                                               
         XC    KEY,KEY                                                          
MKGD     USING RMKGKEY,KEY                                                      
         MVI   MKGD.RMKGKTYP,X'11'                                              
         MVC   MKGD.RMKGKREP,REPALPHA                                           
         MVC   MKGD.RMKGKOFF,RCONKOFF   INSERT OFFICE                           
         MVC   MKGD.RMKGKSTA,RCONKSTA   INSERT STATION+MEDIA                    
         MVC   MKGD.RMKGKCON,TWACNUM    CONTRACT NUMBER                         
         MVC   MKGD.RMKGKGRP,MGHMGRP    GROUP CODE                              
*                                                                               
         XCEFL TSARREC,1002                                                     
         MVC   TSARREC+2(27),KEY                                                
         MVI   TD.TSACTN,TSARDH    RETRIEVE MAKEGOOD HEADER RECORD              
         GOTO1 =A(GOTSAR),RR=RELO                                               
         TM    TD.TSERRS,TSEEOF+TSERNF                                          
         BZ    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
* SAVE OFF FIRST OFFERED AIR DATE FOR RIS LISTING                               
*                                                                               
MGRECD   USING RMKGREC,TSARREC+2                                                
         TM    FLAGS,FACTMGXQ      SKIP IF DELETING OFFER                       
         BO    DUMP30                                                           
         GOTO1 DATCON,DMCB,(3,FIRSTAIR),(2,MGRECD.RMKGFOFD)                     
         DROP  MGRECD                                                           
*                                                                               
DUMP30   DS    0H                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    DUMP40                                                           
*                                                                               
         TM    FLAGS,FACTADDQ      MUST BE ADDING NEW OFFER                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 VMOVEREC,DMCB,TSARREC+2,RMKGREC                                  
         GOTO1 VADDREC,DMCB,RMKGREC                                             
         MVC   TWAMKGDH,KEY        SAVE DISK ADDR OF HEADER                     
         XC    TWAMKGD2,TWAMKGD2                                                
*                                                                               
         LA    R3,RMKGREC                                                       
         BAS   RE,PASSADDR                                                      
*                                                                               
         OI    TWACONFG,TW_MGOAQ   Makegood offer is added                      
         OI    TWACONFG,TW_MGOMQ   Makegood offer is updated, do MQ msg         
*                                                                               
         B     DUMP70                                                           
*                                                                               
DUMP40   DS    0H                                                               
         OI    TWACONFG,TW_MGOMQ   Makegood offer is updated, do MQ msg         
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,RMKGREC                                             
         GOTO1 VMOVEREC,DMCB,TSARREC+2,RMKGREC                                  
         GOTO1 VPUTREC,DMCB,RMKGREC                                             
         MVC   TWAMKGDH,KEY+28     SAVE DISK ADDR OF HEADER                     
*                                                                               
         XC    TWAMKGD2,TWAMKGD2                                                
*                                                                               
         LA    R3,RMKGREC                                                       
         BAS   RE,PASSREWR                                                      
*                                                                               
* DELETE ALL DETAIL RECORDS FOR THIS OFFER                                      
*                                                                               
         TM    FLAGS,FACTCHAQ+FACTMGXQ                                          
         BZ    DUMP70                                                           
         MVC   KEY(27),MGDTREC     FIRST DETAIL RECORD                          
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    DUMP60                                                           
         DC    H'0'                                                             
*                                                                               
DUMP50   DS    0H                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VSEQ                                                             
         CLC   KEY(RMKGKRTY-RMKGKEY),KEYSAVE                                    
         BNE   DUMP70                                                           
*                                                                               
DUMP60   DS    0H                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,RMKGREC                                             
         OI    RMKGCNTL,X'80'                                                   
         GOTO1 VPUTREC,DMCB,RMKGREC                                             
         OI    KEY+27,X'80'                                                     
         GOTO1 VWRITE                                                           
         B     DUMP50                                                           
*                                                                               
* DUMP DETAIL RECORDS FROM TSAR TO FILE                                         
*                                                                               
DUMP70   DS    0H                                                               
         TM    FLAGS,FACTMGXQ                                                   
         BNZ   DUMPX                                                            
         XCEFL TSARREC,1002                                                     
         MVI   TD.TSACTN,TSANXT                                                 
         GOTO1 =A(GOTSAR),RR=RELO                                               
         TM    TD.TSERRS,TSEEOF                                                 
         BNZ   DUMPX                                                            
         CLI   TSARREC+2,X'11'                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY(27),TSARREC+2                                                
         MVI   UPDATE,C'Y'                                                      
         OI    DMINBTS,X'08'       READ FOR DELETED                             
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    DUMP80                                                           
         GOTO1 VMOVEREC,DMCB,TSARREC+2,RMKGREC                                  
         GOTO1 VADDREC,DMCB,RMKGREC                                             
         OC    TWAMKGD2,TWAMKGD2   SAVE OFF D/A OF FIRST DETAIL RECORD          
         BNZ   DUMP70                                                           
         MVC   TWAMKGD2,KEY                                                     
         B     DUMP70                                                           
*                                                                               
DUMP80   DS    0H                                                               
         MVI   UPDATE,C'Y'                                                      
         OI    DMINBTS,X'08'       READ FOR DELETED                             
         GOTO1 VGETREC,DMCB,RMKGREC                                             
         GOTO1 VMOVEREC,DMCB,TSARREC+2,RMKGREC                                  
         GOTO1 VPUTREC,DMCB,RMKGREC                                             
*                                                                               
         NI    KEY+27,X'FF'-X'80'                                               
         GOTO1 VWRITE                                                           
*                                                                               
         OC    TWAMKGD2,TWAMKGD2                                                
         BNZ   DUMP70                                                           
         MVC   TWAMKGD2,KEY+28     SAVE DISK ADDR OF 1ST DETAIL OFFER           
         B     DUMP70                                                           
*                                                                               
DUMPX    DS    0H                                                               
         B     EXXMOD                                                           
         DROP  MKGD,R4                                                          
         EJECT                                                                  
*                                                                               
*   PASSREWR:  RETRIEVE AND REWRITE PASSIVE KEYS                                
*                                                                               
PASSREWR NTR1                                                                   
*                                                                               
*   DELETE OLD PASSIVE POINTERS                                                 
*                                                                               
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,X'A0'           INSERT FIRST PASSIVE                         
         MVI   KEY+1,X'11'         INSERT ID FOR MG PASSIVE 1                   
         MVC   KEY+16(11),10(R3)   **ALL MOVES FROM REC IN STORAGE**            
*                                  MOVE STA/CON#/GROUP TO NEW POSITIONS         
         MVC   KEY+9(2),6(R3)      MOVE REP CODE TO NEW POSITION                
         MVC   KEY+11(3),SAVSAL    INSERT ORIGINAL S/P  INTO KEY                
         MVC   KEY+11(2),SAVTEM    INSERT ORIGINAL TEAM INTO KEY                
PAWR0010 EQU   *                                                                
         MVI   UPDATE,C'Y'         SET FOR UPDATE                               
         GOTO1 VHIGH               READ FOR KEY                                 
         CLC   KEYSAVE(27),KEY     KEY FOUND?                                   
         BE    PAWR0020            YES -                                        
         MVC   KEY,KEYSAVE         NO  - RESET ORIGINAL KEY                     
         B     PAWR0040            PROCESS NEXT KEY                             
PAWR0020 EQU   *                                                                
         MVI   DMOUTBTS,0          NO ERROR TESTS                               
         OI    KEY+27,X'80'        TURN ON 'DELETE' BIT                         
         GOTO1 VWRITE              REWRITE KEY AS DELETED                       
         BAS   RE,CHECK                                                         
PAWR0040 EQU   *                                                                
         CLI   KEY,X'A1'           SECOND KEY ALREADY DONE?                     
         BE    PAWR0100            YES                                          
         MVI   KEY,X'A1'           NO  - SET TO NEXT PASSIVE KEY                
         MVC   KEY+11(2),SAVTEM    INSERT TEAM INTO KEY HIGH                    
         MVC   KEY+13(3),SAVSAL    INSERT S/P INTO KEY LOW                      
         B     PAWR0010            GO BACK AND DO NEXT KEY                      
*                                                                               
PAWR0100 DS    0H                                                               
*                                                                               
*        DELETE A0 PASSIVES                                                     
*                                                                               
PAWR0101 DS    0H                                                               
*                                                                               
         XC    KEY,KEY             CLEAR KEY                                    
         LA    R5,KEY                                                           
*                                                                               
RMGSPKEY USING RMGSPTYP,R5         ESTABLISH PASSIVE                            
*                                                                               
         USING RMKGKEY,R3          ESTABLISH MAKEGOOD HEADER KEY                
*                                                                               
         MVC   RMGSPKEY.RMGSPTYP,=X'A001' SET PASSIVE ID                        
         MVC   RMGSPKEY.RMGSPREP,RMKGKREP REPCODE                               
         MVC   RMGSPKEY.RMGSPSTA,RMKGKSTA STATION                               
         PACK  RMGSPKEY.RMGSPCON(1),RMKGKCON+3(1) CONTRACT                      
         PACK  RMGSPKEY.RMGSPCON+1(1),RMKGKCON+2(1) REVERSE DIGITS              
         PACK  RMGSPKEY.RMGSPCON+2(1),RMKGKCON+1(1) TO GET 9'S                  
         PACK  RMGSPKEY.RMGSPCON+3(1),RMKGKCON(1)   COMPLEMENT                  
         MVC   RMGSPKEY.RMGSPGRP,RMKGKGRP MAKEGOOD GROUP                        
*                                                                               
         MVC   27(1,R5),29(R2)     STATUS                                       
*                                                                               
         MVC   RMGSPKEY.RMGSPSAL,SAVSAL   INSERT S/P INTO KEY                   
         MVC   RMGSPKEY.RMGSPADV,SAVADV   ADVERTISER CODE                       
         MVC   RMGSPKEY.RMGSPDAT,SAVDAT   FIRST OFFERED DATE                    
         MVC   RMGSPKEY.RMGSPWIP,SAVWIP   SET WIP STATUS                        
         MVC   RMGSPKEY.RMGSPSTT,SAVSTT   OFFER STATUS                          
         MVC   RMGSPKEY.RMGSPDST,SAVDST   DARE  STATUS                          
*                                                                               
*        DELETE ANY PASSIVE ALREADY ON FILE                                     
*                                                                               
         MVI   UPDATE,C'Y'         SET FOR UPDATE                               
         GOTO1 VHIGH               READ FOR KEY                                 
*                                                                               
         CLC   KEYSAVE(27),KEY     KEY FOUND?                                   
         BE    PAWR0520            YES -                                        
*                                                                               
         MVC   KEY,KEYSAVE         NO  - RESET ORIGINAL KEY                     
         B     PAWR0540            PROCESS NEXT KEY                             
*                                                                               
PAWR0520 EQU   *                                                                
*                                                                               
         MVI   DMOUTBTS,0          NO ERROR TESTS                               
         OI    KEY+27,X'80'        TURN ON 'DELETE' BIT                         
         GOTO1 VWRITE              REWRITE KEY AS DELETED                       
         BAS   RE,CHECK                                                         
*                                                                               
PAWR0540 EQU   *                                                                
*                                                                               
         OC    SAVDSL,SAVDSL       SKIP IF NO DEV SALESPER                      
         BZ    PAWR0600                                                         
*                                                                               
         MVI   1(R5),X'02'         SET NEXT MG PASSIVE                          
         MVC   RMGSPKEY.RMGSPSAL,SAVDSL INSERT DEV SALESPER                     
*                                                                               
*        DELETE ANY PASSIVE ALREADY ON FILE                                     
*                                                                               
         MVI   UPDATE,C'Y'         SET FOR UPDATE                               
         GOTO1 VHIGH               READ FOR KEY                                 
*                                                                               
         CLC   KEYSAVE(27),KEY     KEY FOUND?                                   
         BE    PAWR0560            YES -                                        
*                                                                               
         MVC   KEY,KEYSAVE         NO  - RESET ORIGINAL KEY                     
         B     PAWR0580            PROCESS NEXT KEY                             
*                                                                               
PAWR0560 EQU   *                                                                
*                                                                               
         MVI   DMOUTBTS,0          NO ERROR TESTS                               
         OI    KEY+27,X'80'        TURN ON 'DELETE' BIT                         
         GOTO1 VWRITE              REWRITE KEY AS DELETED                       
         BAS   RE,CHECK                                                         
*                                                                               
PAWR0580 EQU   *                                                                
*                                                                               
PAWR0600 DS    0H                                                               
         B     PASA0010                                                         
*                                                                               
*   PASSADDR:  ADD PASSIVES FOR GROUP COMMENT RECORDS                           
*                                                                               
PASSADDR NTR1                                                                   
*                                                                               
*   RESTRUCTURE THE GROUP COMMENT CODE TO PASSIVE LAYOUT                        
*                                                                               
PASA0010 EQU   *                                                                
         TM    FLAGS,FACTMGXQ      IS ACTION MAKEGOOD DELETE? (MGX)             
         BO    PASAX               DON'T ADD PASSIVE KEYS                       
         XC    KEY,KEY             CLEAR NEW KEY                                
         MVI   KEY,X'A0'           INSERT FIRST PASSIVE                         
         MVI   KEY+1,X'11'         INSERT ID FOR MG PASSIVE 1                   
         MVC   KEY+16(11),10(R3)   **ALL MOVES FROM REC IN STORAGE**            
*                                  MOVE STA/CON#/GROUP TO NEW POSITIONS         
         MVC   KEY+9(2),6(R3)      MOVE REP CODE TO NEW POSITION                
         LA    RF,RMKGELEM-RMKGREC(R3)                                          
*                                  SET A(DESCRIPTOR ELT OF MKG RECORD)          
PASA0020 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BE    PASA0120            YES - EXIT ROUTINE: NO PASSIVES              
         CLI   0(RF),X'0A'         SWITCH/PASSIVE ELT OF RECORD?                
         BE    PASA0040            YES - PROCESS                                
         ZIC   RE,1(RF)            NO  - BUMP TO NEXT ELT                       
         AR    RF,RE                                                            
         B     PASA0020            GO BACK FOR NEXT                             
PASA0040 EQU   *                                                                
         MVC   KEY+11(3),RMKGXSAL-RMKGXEL(RF)                                   
*                                  INSERT SALESPERSON CODE                      
         MVC   KEY+14(2),RMKGXTEM-RMKGXEL(RF)                                   
*                                  INSERT S/P TEAM    CODE                      
PASA0060 EQU   *                                                                
         OI    DMINBTS,X'08'       PASS DELETES                                 
         MVI   DMOUTBTS,0          NO ERROR TESTS                               
         MVI   UPDATE,C'Y'         SET FOR UPDATE                               
         GOTO1 VHIGH                                                            
*                                                                               
         LR    R4,RA                                                            
         AHI   R4,TWAWORKQ                                                      
         USING TWAWORK,R4                                                       
         MVC   KEY+28(4),TWAMKGDH  INSERT D/A                                   
         DROP  R4                                                               
*                                                                               
         CLC   KEYSAVE(27),KEY     KEY ALREADY ON FILE?                         
         BNE   PASA0080            NO                                           
         NI    KEY+27,X'7F'        TURN OFF DELETE BIT                          
         GOTO1 VWRITE              YES - WRITE THE KEY                          
         B     PASA0100            GO PROCESS NEXT KEY (IF ANY)                 
PASA0080 EQU   *                                                                
         MVC   KEY(27),KEYSAVE     RESTORE KEY FOR 27 CHARS                     
         GOTO1 VADD                ADD NEW KEY                                  
PASA0100 EQU   *                                                                
         CLI   KEY,X'A1'           SECOND KEY PROCESSED?                        
         BE    PASA0120            YES - BOTH KEYS DONE                         
         MVI   KEY,X'A1'           NO  - SET SECOND KEY TYPE                    
         MVC   WORK(3),KEY+11      SAVE S/P  COMPONENT OF KEY                   
         MVC   KEY+11(2),KEY+14    SLIDE TEAM UP IN KEY                         
         MVC   KEY+13(3),WORK INSERT S/P BACK INTO KEY                          
         B     PASA0060            GO BACK AND PROCESS                          
PASA0120 EQU   *                                                                
*                                                                               
*        ADD MAKEGOOD A001, A002 PASSIVE KEYS                                   
*                                                                               
         XC    KEY,KEY             CLEAR NEW KEY                                
         LA    R5,KEY                                                           
*                                                                               
RMGSPKEY USING RMGSPTYP,R5         ESTABLISH PASSIVE                            
*                                                                               
         USING RMKGKEY,R3          ESTABLISH MAKEGOOD HEADER KEY                
*                                                                               
         MVC   RMGSPKEY.RMGSPTYP,=X'A001' SET PASSIVE ID                        
         MVC   RMGSPKEY.RMGSPREP,RMKGKREP REPCODE                               
         MVC   RMGSPKEY.RMGSPSTA,RMKGKSTA STATION                               
         PACK  RMGSPKEY.RMGSPCON(1),RMKGKCON+3(1) CONTRACT                      
         PACK  RMGSPKEY.RMGSPCON+1(1),RMKGKCON+2(1) REVERSE DIGITS              
         PACK  RMGSPKEY.RMGSPCON+2(1),RMKGKCON+1(1) TO GET 9'S                  
         PACK  RMGSPKEY.RMGSPCON+3(1),RMKGKCON(1)   COMPLEMENT                  
         MVC   RMGSPKEY.RMGSPGRP,RMKGKGRP MAKEGOOD GROUP                        
*                                                                               
         LA    RF,RMKGELEM-RMKGREC(R3)                                          
*                                  SET A(DESCRIPTOR ELT OF MKG RECORD)          
         USING RMKGSDEM,RF         ESTABLISH GROUP STATUS ELEMENT               
*                                                                               
         MVC   RMGSPKEY.RMGSPDAT,RMKGFOFD RMGSPKEY.RMGSPE 1ST OFFERED           
         MVC   RMGSPKEY.RMGSPWIP,RMKGSFG2 WIP STATUS                            
         MVC   RMGSPKEY.RMGSPSTT,RMKGSCST OFFER STATUS                          
         NI    RMGSPKEY.RMGSPSTT,X'FF'-RMKGSPAQ KILL OLD STATUS                 
         TM    RMKGSFG3,RMGF3SAQ   IF SELF APPLIED                              
         BNO   *+8                                                              
         OI    RMGSPKEY.RMGSPSTT,RMKGSLFQ SET INDICATOR                         
*                                                                               
         MVC   RMGSPKEY.RMGSPDST,RMKGSFG1 DARE STATUS                           
*                                                                               
         DROP  RF                                                               
*                                                                               
PASA0220 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BE    PASAX               YES - EXIT ROUTINE: NO PASSIVES              
         CLI   0(RF),X'0A'         SWITCH/PASSIVE ELT OF RECORD?                
         BE    PASA0240            YES - PROCESS                                
         ZIC   RE,1(RF)            NO  - BUMP TO NEXT ELT                       
         AR    RF,RE                                                            
         B     PASA0220            GO BACK FOR NEXT                             
PASA0240 EQU   *                                                                
         USING RMKGXEL,RF                                                       
         MVC   RMGSPKEY.RMGSPSAL,RMKGXSAL SALESPERSON                           
         MVC   RMGSPKEY.RMGSPADV,RMKGXADV ADVERTISER                            
         MVC   SAVDSL,RMKGXDSP     SAVE DEVELOPMENTAL SALESPERSON               
*                                                                               
         DROP  RF                                                               
*                                                                               
PASA0260 EQU   *                                                                
         OI    DMINBTS,X'08'       PASS DELETES                                 
         MVI   DMOUTBTS,0          NO ERROR TESTS                               
         MVI   UPDATE,C'Y'         SET FOR UPDATE                               
         GOTO1 VHIGH                                                            
*                                                                               
         LR    R4,RA                                                            
         AHI   R4,TWAWORKQ                                                      
         USING TWAWORK,R4                                                       
         MVC   KEY+28(4),TWAMKGDH  INSERT D/A                                   
         DROP  R4                                                               
*                                                                               
         CLC   KEYSAVE(27),KEY     KEY ALREADY ON FILE?                         
         BNE   PASA0280            NO                                           
         NI    KEY+27,X'7F'        TURN OFF DELETE BIT                          
         GOTO1 VWRITE              YES - WRITE THE KEY                          
         B     PASA0300            GO PROCESS NEXT KEY (IF ANY)                 
PASA0280 EQU   *                                                                
         MVC   KEY(27),KEYSAVE     RESTORE KEY FOR 27 CHARS                     
         NI    KEY+27,X'7F'        TURN OFF DELETE BIT                          
         GOTO1 VADD                ADD NEW KEY                                  
PASA0300 EQU   *                                                                
         CLI   KEY+1,X'02'         SECOND KEY PROCESSED?                        
         BE    PASA0320            YES - BOTH KEYS DONE                         
         MVI   KEY+1,X'02'         NO  - SET SECOND KEY TYPE                    
         MVC   RMGSPKEY.RMGSPSAL,SAVDSL   USE DEV SALESPERSON                   
         OC    SAVDSL,SAVDSL       SKIP IF NO DEV SAL                           
         BZ    PASA0320                                                         
         B     PASA0260            GO BACK AND PROCESS                          
PASA0320 EQU   *                                                                
PASAX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
CHECK    EQU   *                                                                
         TM    DMCB+8,X'FD'                                                     
         BCR   8,RE                                                             
         DC    H'0'                                                             
RMGSPKEY USING RMGSPTYP,R5         ESTABLISH PASSIVE                            
*                                                                               
         DROP  R3,RMGSPKEY                                                      
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CANCEL MAKEGOOD                                                               
***********************************************************************         
MKGCAN   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         OC    TWAMKGDH,TWAMKGDH                                                
         BZ    MKGCANX                                                          
         MVC   KEY+28(4),TWAMKGDH                                               
         DROP  RF                                                               
*                                                                               
         OI    DMINBTS,X'08'       PASS BACK DELETED                            
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
* SEND CANCELLATION TO DARE AGENCY IF NOT ALREADY MARKED 'CANCELLED'            
*                                                                               
         LA    R6,IOAREA                                                        
         USING RMKGREC,R6                                                       
         OC    RMKGSFG1,RMKGSFG1                                                
         BZ    MKGCANX                                                          
         TM    RMKGSFG1,RMGF1MCN                                                
         BO    MKGCANX                                                          
         DROP  R6                                                               
*                                                                               
         BAS   RE,GETDAREH         RETRIEVE DARE HEADER RECORD INTO IO4         
         BNZ   MKGCANX                                                          
*                                                                               
         L     R7,ASPULAR                                                       
         USING SPOOLD,R7                                                        
*                                                                               
         BAS   RE,MYPQOPEN                                                      
*                                                                               
         MVC   P+4(5),=C'*HDR*'                                                 
         MVC   P+9(14),=C'EDICT=*DDSDARR'                                       
         MVI   P+34,C'W'      WIDE REPORT - 132 CHARS                           
*                                  SEND SPECIAL PRINT LINE                      
         BAS   RE,PRINT                                                         
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
         BAS   RE,PRINT                                                         
*                                                                               
         L     R6,AIO4                                                          
         USING RDARREC,R6                                                       
                                                                                
         LA    R4,P                                                             
         USING MOFRCAND,R4                                                      
                                                                                
* RECORD                                                                        
         MVC   MOCNTID,=C'MKGCAN'                                               
                                                                                
* ORDER NUMBER                                                                  
         ZAP   WORK2(5),=P'0'                                                   
         MVO   WORK2(5),RDARKORD                                                
         EDIT  (P5,WORK2),(8,MOCNORDR),FILL=0                                   
                                                                                
* ID OF SENDER                                                                  
         MVC   MOCNFRID,RDARRCVR                                                
                                                                                
* ID OF RECEIVER                                                                
         MVC   MOCNTOID,RDARSNDR                                                
                                                                                
* ROUTING CODE                                                                  
         MVC   MOCNROUT,RDARKAGY                                                
                                                                                
* CURRENT DATE                                                                  
         GOTO1 DATCON,DMCB,(5,0),(X'20',MOCNDATE)                               
                                                                                
* CURRENT TIME                                                                  
*                                                                               
         THMS  DDSTIME=YES                                                      
         ST    R0,DUB              ACTUAL TIME ADJUSTMENT                       
         ST    R1,DUB+4            DDS TIME                                     
         AP    DUB(4),DUB+4(4)                                                  
         ICM   R1,15,DUB                                                        
         SRL   R1,12               SHIFT OFF SECONDS AND SIGN                   
         STCM  R1,3,WORK                                                        
         GOTO1 HEXOUT,DMCB,WORK,MOCNTIME,2,0                                    
*                                                                               
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
         ZAP   WORK2(5),=P'0'                                                   
         MVO   WORK2(5),RCONKCON                                                
         EDIT  (P5,WORK2),(8,MOCNRPCN),FILL=0                                   
                                                                                
* AGENCY 'RETURN TO SENDER' DATA                                                
         MVC   MOCNRTNS,RDARRTS                                                 
         DROP  R6                                                               
                                                                                
         LA    R6,IOAREA                                                        
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
                                                                                
* OFFER NO MORE TO FOLLOW                                                       
         MVI   MOCNNEWO,C'N'                                                    
                                                                                
         BAS   RE,PRINT                                                         
                                                                                
         MVI   SPMODE,X'FF'        CLOSE PQ                                     
         GOTO1 SPOOL,PARAS,ASPULAR                                              
                                                                                
MKGCANX  DS    0H                                                               
         B     EXXMOD                                                           
         DROP  R4,R6                                                            
         EJECT                                                                  
*********************************************************************           
* PRINT A LINE                                                                  
*********************************************************************           
PRINT    NTR1                                                                   
         GOTO1 SPOOL,PARAS,ASPULAR                                              
         MVI   LINE,2              FORCE EVERYTHING TO PAGE ONE                 
         B     EXXMOD                                                           
         EJECT                                                                  
**********************************************************************          
* OPEN THE PRINT QUEUE                                                          
**********************************************************************          
MYPQOPEN NTR1                                                                   
*                                                                               
* INITIALIZE                                                                    
*                                                                               
         L     R1,AFACILS                                                       
         LM    R2,R4,8(R1)                                                      
         ST    R3,ATIA                                                          
         MVC   SCANNER(16),24(R4)                                               
         LA    R2,BOOKVAL          FIND ADDRESSES OF CORE RESIDENT              
         SR    R3,R3               MODULES WITH PHONY CALLOV READS.             
         LA    R4,17                                                            
*                                                                               
INIT2    DS    0H                                                               
         CH    R3,=H'9'                                                         
         BE    INIT2A                                                           
         CH    R3,=H'10'                                                        
         BE    INIT2A                                                           
         CH    R3,=H'11'                                                        
         BE    INIT2A                                                           
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A00'                                           
         STC   R3,DMCB+7                                                        
         GOTO1 CALLOV,DMCB                                                      
         MVC   0(4,R2),DMCB                                                     
         LA    R2,4(R2)                                                         
INIT2A   LA    R3,1(R3)                                                         
         BCT   R4,INIT2                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(10,RCDATE)                                    
*                                                                               
         MVC   SPOOLDM,DATAMGR                                                  
         MVC   SPOOLBUF,ATIA                                                    
         XC    VPRINT,VPRINT                                                    
         MVC   RCDATCON,DATCON                                                  
         MVC   RCCOMFAC,ACOMFACS                                                
*                                                                               
* OPEN PRINT QUEUE                                                              
*                                                                               
         XC    SPOOLKEY,SPOOLKEY                                                
         OI    SPOOLIND,SPUINIT ALLOW USER VALUES-CLASS,LPP,COPIES              
*                                                                               
         LA    RF,SPOOLKEY                                                      
         USING PQPLD,RF                                                         
         MVC   PLSUBID,=C'DMG'                                                  
         MVC   PLDESC(9),=C'DARE MKGD'                                          
         MVI   PLCLASS,C'G'                                                     
         DROP  RF                                                               
*                                                                               
VPQ20    DS    0H                                                               
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ         4K                                           
         USING TWAWORK,RF                                                       
         LA    RE,TWASPKEY                                                      
         DROP  RF                                                               
*                                                                               
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
*                                                                               
         GOTO1 SPOOL,PARAS,ASPULAR                                              
         MVC   SPOOLRPN,SPOOLKEY+19                                             
         B     EXXMOD                                                           
         EJECT                                                                  
**********************************************************************          
* RETRIEVE DARE HEADER RECORD X'51' INTO IO4 WHILE IGNORING AGENCY              
* OFFICE                                                                        
**********************************************************************          
GETDAREH NTR1                                                                   
         XC    KEY,KEY                                                          
KEYD     USING RAGY2KEY,KEY                                                     
         MVI   KEYD.RAGK2TYP,RAGK2TYQ                                           
         MVC   KEYD.RAGK2AGY,RCONKAGY                                           
         MVC   KEYD.RAGK2AOF,RCONKAOF                                           
         MVC   KEYD.RAGK2REP,RCONKREP                                           
         DROP  KEYD                                                             
*                                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RAGY2KEY),KEYSAVE                                          
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE!                               
         GOTO1 VGETREC,DMCB,AIO2                                                
*                                                                               
         LA    R5,KEY                                                           
         USING RDARKEY,R5                                                       
         XC    KEY,KEY                                                          
         MVI   RDARKTYP,X'51'                                                   
         MVC   RDARKREP,REPALPHA                                                
         MVC   RDARKSTA(5),RCONKSTA                                             
         CLI   RDARKSTA+4,C' '                                                  
         BNE   *+8                                                              
         MVI   RDARKSTA+4,C'T'     MUST SPECIFY IF TV                           
         OC    RDARKSTA,MYSPACES                                                
*                                                                               
         L     R6,AIO2                                                          
         USING RAGY2REC,R6                                                      
         OC    RAGY2DAR,RAGY2DAR   NULL EQUIVALENCY CODE?                       
         BZ    GETDAR30            YES -- MISSING AGENCY                        
         MVC   RDARKAGY,RAGY2DAR   EQUIVALENCY CODE                             
         DROP  R6                                                               
                                                                                
         LA    R6,RCONREC                                                       
         USING RCONDREL,R6                                                      
         MVI   ELCODE,X'1D'        GET DARE ELEMENT                             
         BRAS  RE,GETEL                                                         
         BNE   GETDNO                                                           
         XC    MYWRK,MYWRK                                                      
         MVC   MYWRK(4),RCONDRLK     SAVE ORDER NUMBER                          
         DROP  R6                                                               
                                                                                
         L     R6,AIO2                                                          
         USING RAGY2REC,R6                                                      
         LA    R4,RAGY2DAR         THERE ARE MAX 4 AGENCY ASSIGNMENT            
         LA    R3,3                COMBINATIONS WE NEED TO CHECK                
         B     GETDAR10                                                         
*                                                                               
** PREP KEY FOR SKIP READING : SKIP TO NEXT AGY OFFICE IF AGENCY OFFICE         
** DIDN'T CHANGE                                                                
*                                                                               
PRVKEY   USING RDARKEY,KEYSAVE                                                  
GETDAR08 CLC   RDARKAOF,PRVKEY.RDARKAOF  DID AGENCY OFFICE CHANGE?              
         DROP  PRVKEY                                                           
         BNE   GETDAR09               YES -- DON'T INCREMENT                    
         XR    R0,R0                                                            
         ICM   R0,3,RDARKAOF                                                    
         AHI   R0,1                   INCREMENT AGENCY OFFICE FIELD             
         STCM  R0,3,RDARKAOF                                                    
GETDAR09 XC    RDARKORD(7),RDARKORD  CLEAR FIELDS AFTER AGENCY OFFICE           
*                                                                               
GETDAR10 DS    0H                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(RDARKAOF-RDARKEY),KEYSAVE  SAME KEY?                         
         BNE   GETDAR11                                                         
         XC    RDARKORD(7),RDARKORD  CLEAR FIELDS AFTER AGENCY OFFICE           
         MVC   RDARKORD,MYWRK        MOVE IN ORDER # FOR RDHI                   
*                                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(RDARKAOF-RDARKEY),KEYSAVE  SAME KEY?                         
         BNE   GETDAR11                                                         
         CLC   RDARKORD,MYWRK        SAME ORDER NUMBER?                         
         BNE   GETDAR08                                                         
         CLI   RDARKRT,X'10'         AGENCY HEADER?                             
         BE    GETDAR20                                                         
         B     GETDAR30                                                         
*                                                                               
GETDAR11 CLI   RAGY2FXL,RAGY2FLQ   ONLY NEWER AGENCY RECORDS                    
         BNL   *+6                 HAS MULTI-DARE AGENCY ASSIGNMENTS            
         DC    H'0'                                                             
         MVC   KEY,KEYSAVE                                                      
*                                                                               
GETDAR12 LA    R4,5(R4)                                                         
         OC    0(3,R4),0(R4)         NULL EQUIVALENCY CODE?                     
         BZ    GETDAR30              YES                                        
         CLC   RDARKAGY,0(R4)        SAME EQUIVALENCY CODE?                     
         BNE   *+12                                                             
         BCT   R3,GETDAR12                                                      
         B     GETDAR30                                                         
*                                                                               
         MVC   RDARKAGY(5),0(R4)   EQUIVALENCY CODE                             
         XC    RDARKAOF(9),RDARKAOF   CLEAR FIEILDS AFTER AGENCY CODE           
         BCT   R3,GETDAR10                                                      
         B     GETDAR30                                                         
         DROP  R5,R6                                                            
*                                                                               
GETDAR20 DS    0H                                                               
         GOTO1 VGETREC,DMCB,AIO4   USE IO4                                      
         B     GETDYES                                                          
*                                                                               
GETDAR30 DS    0H                  CHECK IF TAKEOVER                            
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1C'                                                     
         BRAS  RE,GETEL                                                         
         BNE   GETDNO                                                           
         USING RCONTKEL,R6                                                      
*                                                                               
         L     R5,AIO4                                                          
         USING RDARREC,R5                                                       
*                                                                               
         MVC   RDARKSTA(5),RCONKSTA                                             
         CLI   RDARKSTA+4,C' '                                                  
         BNE   *+8                                                              
         MVI   RDARKSTA+4,C'T'                                                  
         MVC   RDARKAGY,RCONTKAR   AGENCY ROUTING (FOR JDS)                     
         MVC   RDARSNDR,RCONTKRC   SENDER ID                                    
         MVC   RDARRTS,RCONTKRT    AGENCY RETURN TO SENDER INFO                 
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONDREL,R6                                                      
         MVC   RDARKORD,RCONDRLK                                                
         DROP  R6                                                               
*                                                                               
* GET RECEIVER ID                                                               
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,C'I'                                                         
         MVC   KEY+23(2),TWAUSRID                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',KEY,AIO2                      
*                                                                               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO2                                                          
         CLC   KEY(25),0(R6)                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,28(R6)                                                        
GETDAR40 CLI   0(R6),X'02'         GET REP SIGN-ON ID                           
         BE    GETDAR50                                                         
         BNH   *+6                                                              
         DC    H'0'                                                             
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   GETDAR40                                                         
         DC    H'0'                                                             
*                                                                               
GETDAR50 DS    0H                                                               
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RDARRCVR(0),2(R6)                                                
         OC    RDARRCVR,=20C' '                                                 
         DROP  R5                                                               
*                                                                               
GETDYES  SR    RC,RC                                                            
GETDNO   LTR   RC,RC                                                            
GETDARX  B     EXXMOD                                                           
         B     EXXMOD                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* UPDATE CONTRACT WITH MAKEGOOD WORK IN PROGRESS                                
* UPDATE X'21' MAKEGOOD OFFER ELEMENT                                           
*                                                                               
UPDTCON  NTR1  BASE=*,LABEL=*                                                   
         LR    R4,RA                                                            
         AHI   R4,TWAWORKQ                                                      
         USING TWAWORK,R4                                                       
         MVC   KEY+28(4),TWAKADDR                                               
         MVI   UPDATE,C'Y'                                                      
*                                                                               
         GOTO1 VGETREC,DMCB,RCONREC                                             
*                                                                               
         TM    TWACONFG,TW_MGOXQ   Makegood offer is deleted?                   
         JNZ   UPCON20                                                          
         TM    TWACONFG,TW_MGOPQ   Makegood offer is applied?                   
         JNZ   UPCON20                                                          
*                                                                               
* UPDATE MAKEGOOD OFFER WORK-IN-PROGRESS                                        
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'21'        MG OFFER ELEMENT PRESENT?                    
         BRAS  RE,GETEL                                                         
         BNE   UPCON10                                                          
         USING RCONMGEL,R6                                                      
         OI    RCONMGFG,X'80'      SET WIP (WORK-IN-PROGRESS)                   
         B     UPCON20                                                          
         DROP  R6                                                               
*                                                                               
UPCON10  DS    0H                                                               
         LA    R6,ELEM             NO, ADD ONE                                  
         XC    ELEM,ELEM                                                        
         USING RCONMGEL,R6                                                      
         MVI   RCONMGCD,X'21'                                                   
         MVI   RCONMGLN,RCONXMGQ                                                
         MVI   RCONMGFG,X'80'                                                   
         DROP  R6                                                               
*                                                                               
         GOTO1 VADDELEM,DMCB,RCONREC,ELEM                                       
*                                                                               
UPCON20  LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1E'        Random flag elem present?                    
         BRAS  RE,GETEL                                                         
         JNE   UPCON30                                                          
         USING RCONRFEL,R6                                                      
         TM    TWACONFG,TW_MGOAQ   Makegood offer is added?                     
         JZ    UPCON20H                                                         
         LLC   RE,RCONR#MO                                                      
         AHI   RE,1                Bump up count by 1                           
         STC   RE,RCONR#MO                                                      
         CHI   RE,0                                                             
         JH    *+8                                                              
         MVI   RCONR#MO,0          make sure count is not negative              
         NI    TWACONFG,X'FF'-TW_MGOAQ                                          
         J     UPCON30                                                          
UPCON20H TM    TWACONFG,TW_MGOXQ   Makegood offer is deleted?                   
         JNZ   *+12                                                             
         TM    TWACONFG,TW_MGOPQ   Makegood offer is applied?                   
         JZ    UPCON30                                                          
         LLC   RE,RCONR#MO                                                      
         SHI   RE,1                Bump down count by 1                         
         STC   RE,RCONR#MO                                                      
         CHI   RE,0                                                             
         JH    *+8                                                              
         MVI   RCONR#MO,0          Make sure count is not negative              
         NI    TWACONFG,X'FF'-(TW_MGOXQ+TW_MGOPQ)                               
         DROP  R6                                                               
*                                                                               
UPCON30  DS    0H                                                               
         GOTO1 VPUTREC,DMCB,RCONREC                                             
*                                                                               
UPCONX   DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB,R4                                                            
*                                                                               
* CHECK STATION PROFILES TO SEE IF WE NEED TO DISPLAY DEMO OR NOT               
*                                                                               
CKSTPROF NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   TWAACCS,C'$'                                                     
         BNE   CKSTPRY                                                          
*                                                                               
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
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
         DROP  RF                                                               
*                                                                               
CKSTPRY  SR    RC,RC                                                            
CKSTPRN  LTR   RC,RC                                                            
CKSTPRX  DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
*                                                                               
* DISPLAY PRIMARY DEMO OF THE CONTRACT ON THE MAKE GOOD SCREEN                  
*                                                                               
DISDEMO  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,MGPDEM1H         PREEMPT?                                     
         CLI   FMGTYPE,FMGPOFFQ                                                 
         BE    DISDEMOX            YES, DO NOT DISPLAY DEMO CAT                 
*                                                                               
         LA    R2,MGSDEM1H                                                      
         CLI   FMGTYPE,FMGMOFFQ    REGULAR?                                     
         BE    DISD0010                                                         
*                                                                               
         LA    R2,RNADEM1H                                                      
         CLI   FMGTYPE,FRNAOFFQ    REPLACEMENT NA?                              
         BE    DISD0010                                                         
*                                                                               
         LA    R2,MGBDEM1H                                                      
         CLI   FMGTYPE,FMGBOFFQ    BONUS?                                       
         BE    DISD0010                                                         
*                                                                               
         LA    R2,MLRDEM1H                                                      
         CLI   FMGTYPE,FMLROFFQ    LATE RUN?                                    
         BE    DISD0010                                                         
*                                                                               
         CLI   FMGTYPE,FMLBOFFQ    LATE RUN WITH BONUS?                         
         BE    *+6                                                              
         DC    H'0'                INVALID OFFER TYPE                           
         LA    R2,MLBDEM1H                                                      
         TM    FLAGS,FACTBONQ      PROCESSING BONUS PORTION?                    
         BZ    *+8                                                              
         LA    R2,MLBDEM2H                                                      
*                                                                               
DISD0010 DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'DD'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DISD0050                                                         
*        MVC   8(7,R2),=C'NO DEMO'                                              
*        GOTO1 =A(PROTDEM),RR=YES  NO DEMOS, PROTECT FIELD                      
*        B     DISDEMOX                                                         
*                                                                               
DISD0030 DS    0H                                                               
         CLI   1(R6),18            OLD DEMO FORMAT?                             
         BE    DISD0050            IGNORE IT                                    
         XC    WORK2(30),WORK2                                                  
         USING RCONDDEL,R6                                                      
         MVC   WORK2(3),RCONDDCT                                                
         CLC   WORK2(3),MYSPACES                                                
         BNE   DISD0180                                                         
         DROP  R6                                                               
*                                                                               
DISD0050 DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'12'         PENDING                                     
         BRAS  RE,GETEL                                                         
         BNE   DISD0100                                                         
         USING RSARXEL,R6                                                       
         MVC   WORK2(3),RSARXDEM   DEFAULT                                      
         LA    R3,RSARXDEM                                                      
         LA    R4,8                                                             
*                                                                               
DISD0060 DS    0H                                                               
         CLI   0(R4),C'('          USER DEFINED?  SKIP                          
         BE    DISD0065                                                         
         TM    0(R3),X'40'         PRIMARY?                                     
         BO    DISD0080                                                         
DISD0065 DS    0H                                                               
         AHI   R3,3                                                             
         BCT   R4,DISD0060                                                      
         OC    WORK2(3),WORK2      NO DEMO                                      
         BZ    DISD0100                                                         
         B     DISD0180            IF NOT FOUND, USE DEFAULT                    
DISD0080 DS    0H                                                               
         MVC   WORK2(3),0(R3)      PRIMARY FOUND                                
         B     DISD0180                                                         
*                                                                               
DISD0100 DS    0H                                                               
         MVC   8(7,R2),=C'NO DEMO'                                              
         GOTO1 =A(PROTDEM),RR=YES  NO DEMOS, PROTECT FIELD                      
         B     DISDEMOX                                                         
*                                                                               
DISD0180 DS    0H                                                               
*                                                                               
         CLI   WORK2,C'('           USER DEFINED DEMOS                          
         BE    DISD0185                                                         
         CLI   WORK2+1,C'T'         FUDGE FOR DEMOCON                           
         BNE   *+8                                                              
         MVI   WORK2+1,C'I'                                                     
         B     DISD0190                                                         
DISD0185 MVC   8(3,R2),WORK2        USER DEFINED, JUST MOVE TO SCR              
         B     DISD0500                                                         
*                                                                               
DISD0190 DS    0H                                                               
         L     R4,AIO3                                                          
         USING DBLOCKD,R4                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
*                                                                               
* THIS IS REALLY A CALL TO DEMOCON (NOT DEMCON)                                 
         GOTO1 DEMCON,DMCB,WORK2,(2,8(R2)),(0,DBLOCKD)                          
         DROP  R4                                                               
*                                                                               
DISD0500 DS    0H                                                               
         CLI   FMGTYPE,FMLBOFFQ    LATE RUN WITH BONUS?                         
         BNE   DISDEMOX                                                         
         MVC   MLBDEM2,MLBDEM1                                                  
*                                                                               
DISDEMOX DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
*******************************************************************             
*GETDEMV  GETS DEMO VALUE FROM INPUT, R2 POINT TO INPUT FIELD                   
*******************************************************************             
GETDEMV  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 VDELELEM,DMCB,(X'0E',RMKGREC)                                    
         XC    ELTBILD,ELTBILD     CLEAR ELEMENT BUILD AREA                     
MKGDEM   USING RMKGDMEL,ELTBILD                                                 
         MVI   MKGDEM.RMKGDMCD,RMKGDMCQ SET ELEMENT TYPE CODE                   
         MVI   MKGDEM.RMKGDMLN,L'RMKGDMCV+2                                     
         MVC   MKGDEM.RMKGDM2M,=F'-1'                                           
*                                                                               
         TM    FMGTYPE,FMLROFFQ+FMGPOFFQ                                        
         BNZ   GETD0020             FOR LATE RUN/PREEMPT                        
         TM    RMKGRTS,X'04'        /LATE RUN POTION OF L/B                     
         BZ    GETD0040             THERE IS SPECIFIC DEMO FOR BYLINE           
         TM    RMKGRTS,X'20'                                                    
         BO    GETD0040                                                         
*                                                                               
GETD0020 DS    0H                                                               
         GOTOR PICKDEM                                                          
         BNE   GETD0040                 NO 0D, NO 0E, LOOK AT CONTRT            
         USING RBUYRDCV,R6                                                      
         MVC   MKGDEM.RMKGDMCT,RBUYRDCT                                         
         MVC   MKGDEM.RMKGDMDM,RBUYRDDM DEFAULT VALUE FROM BUY LINE             
         DROP  R6                                                               
         B     GETD0160                                                         
*                                                                               
GETD0040 DS    0H                                                               
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'DD'                                                     
         BRAS  RE,GETEL                                                         
         BNE   GETD0050                                                         
*                                                                               
GETD0030 DS    0H                                                               
         CLI   1(R6),18            OLD DEMO FORMAT?                             
         BE    GETD0050            IGNORE IT                                    
         XC    WORK2(30),WORK2                                                  
         USING RCONDDEL,R6                                                      
         MVC   WORK2(3),RCONDDCT                                                
         CLC   WORK2(3),MYSPACES                                                
         BNE   GETD0140                                                         
         DROP  R6                                                               
*                                                                               
GETD0050 DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'12'        PENDING                                      
         BRAS  RE,GETEL                                                         
         BNE   GETDEMVX            NO, GET OUT                                  
*                                                                               
         USING RSARXEL,R6                                                       
         MVC   WORK2(3),RSARXDEM   DEFAULT                                      
         LA    R3,RSARXDEM                                                      
         LA    R4,8                                                             
*                                                                               
GETD0060 DS    0H                                                               
         TM    0(R3),X'40'         PRIMARY?                                     
         BO    GETD0080                                                         
         AHI   R3,3                                                             
         BCT   R4,GETD0060                                                      
         OC    WORK2(3),WORK2      NO DEMO?                                     
         BZ    GETDEMVX            EXIT                                         
         B     GETD0140            IF NOT FOUND, USE DEFAULT                    
*                                                                               
GETD0080 DS    0H                                                               
         MVC   WORK2(3),0(R3)      PRIMARY FOUND                                
*                                                                               
GETD0140 DS    0H                                                               
         MVC   MKGDEM.RMKGDMCT,WORK2    SAVE DEMO CAT                           
         MVC   MKGDEM.RMKGDMDM,=F'-1'   VALUE DEFAULT TO -1                     
*                                                                               
GETD0160 EQU   *                                                                
         CLI   5(R2),0             ANYTHING IN FIELD?                           
         BE    GETDEMVX            NO  - GET DEMO VAL FROM TARGET BUY           
*                                                                               
GETD0180 EQU   *                   GET INPUT FROM SCREEN                        
         ZIC   R5,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(X'81',8(R2)),(X'80',(R5))                          
         CLI   DMCB,X'FF'                                                       
         BE    ERRINVL                                                          
*                                                                               
         L     RF,DMCB+8                                                        
         SRL   RF,4                                                             
         STCM  RF,15,MKGDEM.RMKGDMDM                                            
         B     GETD0200             ADD ELEMENT TO RECORD                       
*                                                                               
GETD0200 EQU   *                                                                
         GOTO1 VADDELEM,DMCB,RMKGREC,ELTBILD                                    
         OI    6(R2),X'80'          RETRANSMIT                                  
         DROP  MKGDEM                                                           
*        DROP  R6                                                               
*                                                                               
GETDEMVX DS    0H                                                               
         B     EXXMOD                                                           
*                                                                               
ERRINVL  DS    0H                                                               
         LA    R3,INVINP                                                        
         B     ERROR                                                            
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
************************************************************                    
* DETERMINE WHICH DEMO TO USE IN 0E AND 0D                                      
* IF THE LOGIC IS CHANGED, THEN SHOULD CHANGE THE SAME ROUNTINE                 
* IN MODULE RECNT29                                                             
************************************************************                    
PICKDEM  NTR1  BASE=*,LABEL=*                                                   
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ          GET TARGET BUY LINE#                        
         USING TWAWORK,RF                                                       
*                                                                               
         MVC   WORK(L'KEY),KEY      BACK UP KEY                                 
         XC    KEY,KEY                                                          
K        USING RBUYKEY,KEY                                                      
         MVI   K.RBUYKTYP,X'0B'                                                 
         MVC   K.RBUYKREP,REPALPHA                                              
         MVC   K.RBUYKCON,TWACNUM                                               
         MVC   K.RBUYKPLN,=3X'FF'                                               
         GOTO1 VHIGH                                                            
*                                                                               
PICK0005 DS    0H                                                               
         CLC   KEY(RBUYKMLN-RBUYKEY),KEYSAVE                                    
         BE    PICK0010                                                         
         DC    H'0'                                                             
*                                                                               
PICK0010 DS    0H                                                               
         CLC   K.RBUYKLIN,TARGETBY                                              
         BE    PICK0050                                                         
         GOTO1 VSEQ                                                             
         B     PICK0005                                                         
         DROP  K                                                                
         DROP  RF                                                               
*                                                                               
PICK0050 DS    0H                                                               
         GOTO1 VGETREC,DMCB,IOAREA  DO NOT READ INTO RBUYREC,WILL SCREW         
         MVC   KEY,WORK             UP TSAR                                     
         GOTO1 VHIGH                RESTORE READ SEQUENCE                       
*                                                                               
         LA    R6,IOAREA                                                        
         USING RBUYREC,R6                                                       
         MVI   ELCODE,X'0E'                                                     
         BRAS  RE,GETEL                                                         
         BNE   NO0EELT                                                          
*                                                                               
HAS0EELT DS    0H                                                               
         LR    R7,R6                                                            
         LA    R7,2(R7)            R7->FIRST DEMO IN 0E ELT                     
*                                                                               
         GOTO1 =A(LOOPDEM),RR=Y                                                 
         BE    *+10                                                             
         LR    R6,R7               NO PRIMARY DEM, USE THE FIRST                
         B     PICKDYES                                                         
*                                                                               
         USING RBUYRDCV,R6                                                      
         LR    R5,R6               R5->PRIMARY IN 0E                            
         CLC   RBUYRDDM,=F'-1'                                                  
         BNE   PICKDYES            PRIMARY <> -1, USE IT                        
         DROP  R6                                                               
*                                                                               
*  BELOW DEALS WITH SITUATION WHEN PRIMARY DEMO VALUE IN 0E = -1                
         LA    R6,IOAREA           PRIMARY IN 0E = -1,LOOK FOR 0D               
         MVI   ELCODE,X'0D'                                                     
         BRAS  RE,GETEL                                                         
         BE    PICK0100                                                         
*                                                                               
         CLI   1(R6),18            OLD DEMO FORMAT                              
         BE    PICK0070                                                         
         CLI   1(R6),34                                                         
         BNE   PICK0100            IGNORE THEM                                  
PICK0070 DS    0H                                                               
         LR    R6,R5               NO 0D, USE THE PRIMARY IN 0E                 
         B     PICKDYES                                                         
*                                                                               
PICK0100 DS    0H                                                               
         GOTO1 =A(LOOPDEM),RR=Y                                                 
         BE    *+10                                                             
         LR    R6,R5               0D HAS NO PRIMARY, USE PRIMARY IN 0E         
         B     PICKDYES                                                         
*                                                                               
         USING RBUYDMCV,R6                                                      
         CLC   RBUYDMDM,=F'-1'     IF 0D'S PRIMARY HAS A VALUE OF -1            
         BNE   PICKDYES            OTHERWISE USE PRIMARY IN 0D                  
         LR    R6,R5               THEN USE THE PRIMARY IN 0E                   
         B     PICKDYES                                                         
         DROP  R6                                                               
*                                                                               
*                                                                               
NO0EELT  DS    0H                                                               
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'0D'                                                     
         BRAS  RE,GETEL            NO DEM CAT                                   
         BNE   PICKDNO                                                          
         CLI   1(R6),18            OLD DEMO FORMAT                              
         BE    PICKDNO                                                          
         CLI   1(R6),34            IGNORE THEM                                  
         BE    PICKDNO                                                          
*                                                                               
         LR    R8,R6               THERE IS A 0D ELT                            
         LA    R8,2(R8)            R8->FIRST DEMO IN 0D ELT                     
*                                                                               
         GOTO1 =A(LOOPDEM),RR=Y                                                 
         BE    PICKDYES                                                         
         LR    R6,R8               USE THE FIRST DEMO IN 0D                     
PICKDYES SR    RC,RC                                                            
PICKDNO  LTR   RC,RC               SET CONDITION CODE                           
PICKDEMX XIT1  REGS=(R6)                                                        
*                                                                               
***************************************                                         
* LOOP UNTIL WE FIND A PRIMARY DEMO                                             
***************************************                                         
LOOPDEM  NTR1                       R6->ELT                                     
         ZIC   R5,1(R6)                                                         
         AR    R5,R6                <BOUNDRY>                                   
         LA    R6,2(R6)                                                         
LOOPD10  DS    0H                                                               
         CLI   0(R6),C'('           USER DEFINED DEMO?                          
         BE    LOOPD30              SKIP                                        
         TM    0(R6),X'40'          PRIMARY DEM?                                
         BO    LOOPDYES                                                         
LOOPD30  DS    0H                                                               
         LA    R6,L'RBUYDMCV(R6)                                                
         CR    R6,R5                                                            
         BNL   LOOPDNO                                                          
         B     LOOPD10                                                          
LOOPDYES SR    RC,RC                                                            
LOOPDNO  LTR   RC,RC                                                            
LOOPDEMX XIT1  REGS=(R6)                                                        
*                                                                               
*                                                                               
DISDEMV  NTR1  BASE=*,LABEL=*                                                   
         LA    R6,RMKGREC                                                       
         MVI   ELCODE,X'0E'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DISDEMVX                                                         
*                                                                               
         USING RMKGDMEL,R6                                                      
         CLC   RMKGDMDM,=F'-1'                                                  
         BE    DISDEMVX                                                         
         ZAP   WORK(5),=P'0'                                                    
         MVO   WORK(5),RMKGDMDM                                                 
         EDIT  (P5,WORK),(8,8(R2)),1,ALIGN=LEFT                                 
         DROP  R6                                                               
DISDEMVX DS    0H                                                               
         B     EXXMOD                                                           
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005RECNT3A   10/08/15'                                      
         END                                                                    
