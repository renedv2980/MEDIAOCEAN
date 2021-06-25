*          DATA SET PRREQ04    AT LEVEL 049 AS OF 06/09/20                      
*PHASE T41204A                                                                  
*INCLUDE SRCHCALL                                                               
         TITLE 'CHANGE LOG'                                                     
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* ------------------------------------------------------------------- *         
* USER    JIRA       DATE                  CHANGE LOG                 *         
* ---- ----------  -------- ----------------------------------------- *         
* KWAN SPEC-42338  02/28/20 ALLOW VENDOR LOCK REPORTS                 *         
* SMUR SPEC-17729  04/13/18 NEW MEDIA D FOR DIGITAL AUDIO (18.3)      *         
*                                                                     *         
* ------------------------------------------------------------------- *         
*                                                                               
* BPLA 07/15     CHANGES FOR NEW MEDIA CODES                                    
*                                                                               
* BPLA 05/15     ONEVTBL ENTRY FOR PAI (144) AT&T                               
*                                                                               
* BPLA 04/14     ONEVTBL ENTRY FOR PPZ (143) PFIZER                             
*                                                                               
* BPLA 02/14     CHANGE FOR MEDIA L IN ONEVTBL                                  
*                                                                               
* BPLA 10/13     ONEVTBL ENTRIES FOR P48 (048)                                  
*                                                                               
* BPLA 01/13     ONEVAL ENTRY FOR PIN (141)                                     
*                ONEVAL ENTRY FOR PCH (142)                                     
*                                                                               
* BPLA 03/12     ONEVAL ENTRY FOR PWB (140)                                     
*                                                                               
* BPLA 02/11     ONEVAL ENTRY FOR PJW - OPT4                                    
*                                                                               
* BPLA 01/11     ONEVAL ENTRY FOR P92 - OPT4                                    
*                                                                               
* BPLA 09/09     ONEVAL ENTRY FOR PTD (138) - QOPT6                             
*                                                                               
* BPLA 02/09     ONEVAL ENTRY FOR PLO (137) - QOPT4                             
*                                                                               
* BPLA 09/08     ONEVAL ENTRY FOR P10 - QOPT5                                   
*                                                                               
* BPLA 06/08     ONEVAL ENTRY FOR P98                                           
*                                                                               
* BPLA 12/07     CHK MOS VS ESTIMATE DATES FOR BILLING REQS                     
*                ALSO MOVED 4 VALIDATION ROUTINES TO ONEVAL                     
*                                                                               
* BOBY 01/07     PLANNED COSTS OPTION - ONEVAL ENTRY FOR P49                    
*                                                                               
* BPLA 12/06     MORE ONEVAL ENTRIES FOR EB (105) AN EX (107)                   
*                                                                               
* BPLA 08/06     ONEVAL ENTRY FOR EX (107)                                      
*                                                                               
* BPLA 10/05     ONEVAL ENTRY FOR PH (136)                                      
*                                                                               
* BPLA 09/05     ONEVAL ENTRY FOR SE (135)                                      
*                                                                               
* BPLA 08/05     NO 52/EC FOR SOON BILLING                                      
*                                                                               
* SMYE 06/05     ADD NEW ENTRY IN ONEVTBL FOR PLT (REQNUM 134)                  
*                                                                               
* BPLA 03/05     NEW OPTIONS FOR SN REPORT                                      
*                                                                               
* BOBY 01/05     ADD &BDATE AS VALID RFP KEYWORD                                
*                                                                               
* SMYE 09/02     IN CONVAL CHECK FOR EXISTENCE OF AT LEAST ONE MATCHING         
*                  CONTRACT RECORD IN "ALL EDITIONS" REQUEST                    
*                                                                               
* KWAN 04/09/01 NEW FIELDS FOR CLT/PRD/PUB GRP RECORD PURGES                    
*                                                                               
* SMYE 12/00     ADD NEW ENTRIES IN ONEVTBL FOR PCM (REQNUM 94)                 
*                                                                               
* KWAN 12/00     P02 - NOW ALLOWS PURGE OPT "A" (EVEN IF LIVE RUNS)             
*                                                                               
* KWAN 12/00     B1 & D1, MANUAL AMT, LIMIT GROSS & FEE TO 21 MILLIONS          
*                                                                               
* KWAN 12/00     P02 FIX, DISALLOW "A" IN QOPT2                                 
*                                                                               
* KWAN 11/00     FOR B1 AND D1 IN MANUAL AMT, LIMIT GROSS AND FEE               
*                TO NO MORE THAN 9,999,999.99                                   
*                                                                               
* KWAN 07/00     ADD NEW ENTRY IN ONEVTBL FOR PA8                               
*                                                                               
* KWAN 05/00     FIX P02'S OPT4=Y PROBLEM                                       
*                                                                               
* KWAN 03/00     ALLOW "G" IN PURGE OPTION IN P01 AND P02                       
*                                                                               
* KWAN 03/00     BUG FIX IN LSTVAL FOR P01 AND P02                              
*                                                                               
* KWAN 10/99     P41 PRINT OPTION 3 ALLOWS "I" FOR INACTIVE                     
*                                                                               
* KWAN 10/99     ADD CODES FOR RT02 (RECORD PURGES DDS ONLY)                    
*                                                                               
* KWAN 09/99     ADD CODES FOR RT01 (RECORD PURGES)                             
*                                                                               
* SMYE 11/98     NEW ENTRY IN ONEVTBL FOR WEB SITE (I) ON P48                   
*                                                                               
* BPLA  9/98     CHANGE PUBLISHER VALIDATION TO ALWAYS PUT                      
*                "P=ALL" IN REQUEST CARD IF ALL IS ENTERED                      
*                CHANGE OF MEDIA "I" IN SORTVAL                                 
*                CLEAR NAME IN PUBVAL AND REPVAL                                
*                                                                               
* SMYE 8/98      NEW ENTRY IN ONEVTBL FOR TEST RUN (Y/N) FOR PCL (106)          
*                                                                               
* SMYE 5/98      NEW ENTRIES IN ONEVTBL FOR P14 (O & A FOR LEVEL OPT)           
*                ALLOW ALL IN PUBLISHER FOR P19 (PUBVAL)                        
*                                                                               
* SMYE 4/98      NEW ENTRIES IN ONEVTBL FOR "TEST RUN" (Y/N) AND                
*                "RERUN" (R/ ) AND ADDED                                        
*                INVCVALS TO VALIDATE START INVOICE # AND                       
*                INVCVALX TO VALIDATE END INVOICE #                             
*                     * ALL ABOVE FOR 105 (PPEB) *                              
*                                                                               
* SMYE 4/98      NEW ENTRIES IN ONEVTBL FOR GROUP ASSIGNS ON P48                
*                (ADDED G AND U TO TYPE OF LIST - REQ CARD COL 62)              
*                                                                               
* BPLA 3/98      IN OVEVTBL CHANGE COLUMN FOR 219 (PPAU) LIKE P19               
*                                                                               
* SMYE 12/97     IN ONEVTBL CHANGED COLUMN NUM IN REQUEST CARD FROM             
*                61 TO 142 FOR DETAIL OPTION ON P19 AND ADDED ENTRIES           
*                TO VALIDATE SFH ENTRIES ON P18, P19, P52 AND PEC               
*                                                                               
* SMYE 6/97      IN CONVAL ACCEPT H AND L FOR HIGH AND LAST                     
*                ALSO DISPLAY CONTRACT NUMBER SELECTED                          
*                                                                               
* SMYE 2/97      IN CONVAL ACCEPT "HIGH" TO LOOK FOR HIGHEST CONTRACT           
*                NUMBER                                                         
*                                                                               
* SMYE 1/97      IN CONVAL ACCEPT "LAST" TO LOOK FOR "LATEST" CONTRACT          
*                START DATE                                                     
*                                                                               
* SMYE 7/96      NEW ENTRY IN ONEVTBL FOR SCHEME FILTER ON P48                  
*                                                                               
* SMYE 4/96      NEW PUBLISHER REP VALIDATION RTN (PUBVAL)                      
*                                                                               
* BPLA 1/96      NEW ENTRY IN ONEVTBL FOR PUB PAY CONTROLS ON P48               
*                                                                               
* BPLA 11/95     NEW ENTRIES IN ONEVTBL FOR LANGUAGE FILTER                     
*                ON P46 AND P48                                                 
*                                                                               
* BPLA 10/95     NEW ENTRY IN ONEVTBL FOR PPM                                   
*                                                                               
* BPLA 9/95      NEW ENTRY IN ONEVTBL FOR NEW P12 AND PAC                       
*                "$ ON SCHEDULE" OPTION (QOPT2 COL 63)                          
*                                                                               
* BPLA 8/95      NEW ENTRY IN ONEVAL FOR NEW P43 TYPE OF LIST OPTION            
*                                                                               
* BPLA 5/9/95    NEW ENTRY IN ONEVAL FOR NEW S2 ADCODE RECAP OPTION             
*                                                                               
* BPLA 2/22/95   NEW ENTRIES IN ONEVAL FOR Z5 REPORT                            
*                                                                               
* BPLA 2/7/95    IN EC52VAL CHECK FOR INPUT                                     
*                ALSO IN BILLVAL RFP CHECK  - SET FIND TO X'04'                 
*                NOT 'X'40' IF 'STARTM' IS ENTERED                              
*                                                                               
* BPLA 11/94     NEW ENTRIES IN ONEVAL TABLE FOR TS REPORT                      
*                                                                               
* BPLA 10/94     FAX OPTION NOW IN COL 53 (WAS 61)                              
*                                                                               
* BPLA 8/94      FAX OPTION FOR CONTRACTS (P12)                                 
*                                                                               
* BPLA 8/94      RFP FIX IN DTEVAL                                              
*                                                                               
* BPLA 6/6/94    ADD BDATE (BILLING INVOICE DATE) TO RFP                        
*                VALIDATIONS                                                    
* BPLA 11/18/93  NEW ENTRIES IN SORTVTBL FOR REQ 14                             
*                                                                               
* BPLA 7/29/93   RFP CHANGE FOR BILLVAL AND DTEVAL                              
*                                                                               
* BPLA 7/22/93   NEW ENTRY IN ONEVTBL COL 65 FOR PRA                            
*                                                                               
* BPLA 2/25/93   FOR AR REPORT DON'T CHECK IF CONTRACT IS ON FILE               
*                                                                               
* BPLA 1/29/93   ENTRIES TO ONEVTBL AND SORTVTBL FOR AR (217)                   
*                                                                               
* BPLA 10/28/92  "S" ADDED TO 77 DATE TYPE (ONEVTBL)                            
*                                                                               
* BPLA 4/28/92   NET OPTION FOR P49 ADDED TO ONEVTBL                            
*                                                                               
* BPLA 12/3/91   NEW OPTIONS FOR P48 AND P77 ADDED TO ONEVTBL                   
*                                                                               
* BPLA 9/12/91   REP NAME SEARCHING ADDED                                       
*                                                                               
* BPLA 8/26/91   ADD VALUES H FOR DETAIL AND H + I FOR SUMMARY                  
*                IN COVRVAL (PP52 $ COLUMN OVERRIDE)                            
*                                                                               
* BPLA 5/23/91   ADD ENTRIES TO ONEVTBL FOR NEW OPTION ON P48                   
*                                                                               
* BPLA 5/22/91   ADD VALUE "F" TO MARKET SORT FOR P77                           
*                                                                               
* BPLA 2/26/91   ADD LOGIC FOR RD - REBATE DRAFT (RT123)                        
*                                                                               
* ROSA 2/11/91   ADD MARKET SORT OPTION TO P77                     L02          
* BPLA 2/7/91    ONETAB ENTRIES FOR P49                                         
*                                                                               
* BPLA 9/26/90   REVERSAL INVOICE NUMBER NOW CHARACTERS                         
*                WAS BINARY FOLLOWING DATE                                      
*                CHANGE IN NEWBILL (IN AMTVAL)                                  
*                                                                               
* BPLA 7/16/90   MANUAL AMT FOR NEW BILLING NOW ON CARD 2 COL 27                
*                ALSO INVOICE DATE AND DUE DAYS MOVED FROM RPUB+1               
*                TO RPAY                                                        
*                                                                               
* ROSA 1/17/90  ADD NEW LINE FOR P10 REQUEST                      L01           
         TITLE 'PRREQ04  NEW REQUESTS  VALIDATE FIELDS PART-2'                  
         PRINT NOGEN                                                            
T41204   CSECT                                                                  
         NMOD1 000,T41204,RR=R9                                                 
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         L     R9,0(R1)                                                         
         USING REQTEMP,R9                    R9=A(W/S)                          
         L     R3,ASAVE                                                         
         USING TWAD,R3                    R3=A(TWA)                             
         LA    R8,T41204+4095                R8=2ND BASE REGISTER               
         LA    R8,1(R8)                                                         
         USING T41204+4096,R8                                                   
         EJECT                                                                  
         L     R1,FLDHADR                    R1=A(FLD HDR TO BE VALED)          
         SR    RF,RF                                                            
         IC    RF,ROUTNUM                    RF=ROUTINE NUM REQD                
         SLL   RF,2                                                             
         L     RF,ROUTADRT(RF)                                                  
         A     RF,RELO                       RF=A(ROUTINE REQD)                 
         BASR  RE,RF                         PASS CONTROL TO ROUTINE            
         SPACE 2                                                                
EXIT     XMOD1 1                                                                
         EJECT                                                                  
*        VALIDATE SORT MENU AND SET FIND FORMAT BITS                            
*        X'04' NN & DEFINED FOR REQUEST                                         
*                                                                               
SORTVAL  NTR1                                                                   
         GOTO1 AINITV                        SET R4=A(HDR) & R5=L'DATA          
         CLI   FIND,1                                                           
         BNE   SORTVX                                                           
         CLI   IFLDH+5,2                                                        
         BNE   SORTINV                                                          
         CLI   REQMED,C'*'         NO SORT FOR 'ALL MEDIAS'                     
         BE    SORTINV                                                          
         MVC   DUB(2),=C'00'                                                    
         MVZ   DUB(2),IFLD                                                      
         CLC   DUB(2),=C'00'                                                    
         BNE   SORTINV                       SORT MENU NOT 2 CHRS NUM           
         LA    R6,SORTVTBL                                                      
         SR    R7,R7                                                            
SORTV1   CLI   0(R6),0                       SEARCH TABLE FOR REQ NUM           
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,R6),REQNUM                                                   
         BE    SORTV2                                                           
         IC    R7,1(R6)                                                         
         AR    R6,R7                                                            
         B     SORTV1                                                           
SORTV2   IC    R7,1(R6)                                                         
         SRL   R7,1                          SEARCH REQ ENTRY FOR CODE          
         BCTR  R7,R0                         R7=NUM OF CODES FOR REQ            
SORTV3   LA    R6,2(R6)                                                         
         CLC   0(2,R6),IFLD                                                     
         BE    SORTV4                                                           
         BCT   R7,SORTV3                                                        
         B     SORTINV                                                          
SORTV4   OI    FIND,X'04'          SORT MENU VALID =NN                          
         CLC   IFLD(2),=C'02'                                                   
         BE    SORTVX              02 LEAVES RSORT BLANK                        
         LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(2,R7),IFLD                                                     
*                                                                               
SORTV5   CLI   REQMED,C'N'                                                      
         BNE   SORTV5A                                                          
         CLC   0(2,R7),=C'05'            05 NOT FOR NEWS                        
         BE    SORTINV                                                          
         CLC   0(2,R7),=C'06'      06 NOT FOR NEWS                              
         BE    SORTINV                                                          
         B     SORTVX                                                           
*                                                                               
SORTV5A  CLI   REQMED,C'M'                                                      
         BE    SORTV5B                                                          
         CLI   REQMED,C'S'                                                      
         BE    SORTV5B                                                          
         CLI   REQMED,C'I'          TREAT INTERACTIVE AS MAGAZINE               
         BE    SORTV5B                                                          
         CLI   REQMED,C'T'                                                      
         BE    SORTV5B                                                          
         B     SORTVX                                                           
*                                                                               
SORTV5B  CLC   0(2,R7),=C'07'         07 - NEWS OR OUTDOOR ONLY                 
         BE    SORTINV                                                          
         B     SORTVX                                                           
*                                                                               
SORTINV  MVC   FERN,=AL2(FLDINV)                                                
SORTVX   DS    0H                                                               
*                                                                               
*                                                                               
*                                                                               
EXITVAL  XIT1                                                                   
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*        THIS TABLE HAS V/L ENTRIES OF FORMAT                                   
*        XL1   REQ NUM (=X'00' TABLE END)                                       
*        XL1   ENTRY LEN                                                        
*        NCL2  N POSSIBLE VALUES OF SORT MENU                                   
*                                                                               
*          SORT MENU 02 SET TO BLANKS IN REQ RECORD                             
*                                                                               
SORTVTBL DS    0CL1                                                             
         DC    AL1(06,04),C'05'           REQ 06                                
         DC    AL1(12,06),C'0507'                                               
         DC    AL1(14,10),C'05070809'                                           
         DC    AL1(18,06),C'0507'                                               
         DC    AL1(19,06),C'0507'                                               
         DC    AL1(27,10),C'06070809'     REQ 27                                
         DC    AL1(28,10),C'06070809'     REQ 28                                
         DC    AL1(36,06),C'0607'         REQ 36                                
         DC    AL1(37,10),C'06070809'     REQ 37                                
         DC    AL1(46,08),C'020307'                                             
         DC    AL1(48,10),C'02030407'                                           
         DC    AL1(52,10),C'03040507'     REQ 52                                
         DC    AL1(60,06),C'0507'                                               
         DC    AL1(113,06),C'0507'                                              
         DC    AL1(118,06),C'0507'        REQ EC FOR BSNY BACKER                
         DC    AL1(121,10),C'06070809'     REQ 27                               
         DC    AL1(130,06),C'0507'        REQ TS                                
         DC    AL1(212,06),C'0507'         AC                                   
         DC    AL1(217,06),C'0507'         AR                                   
         DC    AL1(219,06),C'0507'         AU                                   
         DC    H'0'                                                             
         EJECT                                                                  
*        VALIDATE REPORT AND SET FIND FORMAT BITS                               
*        X'04' XX                                                               
*                                                                               
REPRTVAL NTR1                                                                   
         GOTO1 AINITV                        SET R4=A(HDR) & R5=L'DATA          
         CLI   FIND,1                                                           
         BNE   REPRTVX                                                          
         CLI   IFLDH+5,3                                                        
         BH    REPRTINV                                                         
         CLI   IFLDH+5,2                                                        
         BL    REPRTINV                                                         
         BH    REPRTVO                                                          
         MVC   TEMP+1(2),IFLD                                                   
         MVI   TEMP,C' '           INSERT LEADING SPACE                         
         MVC   IFLD(3),TEMP                                                     
REPRTVO  LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(3,R7),IFLD                                                     
         OI    FIND,X'04'                    REPRT VALID =XX                    
         B     REPRTVX                                                          
REPRTINV MVC   FERN,=AL2(FLDINV)                                                
REPRTVX  B     EXITVAL                                                          
         EJECT                                                                  
*                                                                               
*        GENERAL DATE ROUTINE                                                   
*                                                                               
*         X'04' =YYMM     X'08' = YYMMDD           X'02' = ALL REQ 31           
*         X'10' =NO                                                             
DTEVAL   NTR1                                                                   
*                                                                               
         GOTO1 AINITV                                                           
         CLI   IFLDH,1                                                          
         BL    DTEVX                                                            
*                                                                               
DTV3     CLI   IFLDH,1                                                          
         BH    DTEV3      DATE=ALL                                              
         CLI   IFLDH+5,2                                                        
         BNE   DTEV                                                             
         CLC   IFLD(2),=C'NO'                                                   
         BNE   DTEVE                                                            
         OI    FIND,X'10'                                                       
         MVC   TEMP(4),=C'NO  '                                                 
         B     DTEVO                                                            
*                                                                               
DTEV     GOTO1 DATVAL,PLIST,(0,IFLD),TEMP                                       
         OC    PLIST(4),PLIST                                                   
         BE    DTEV1                                                            
         OI    FIND,X'08'       DATE = YYMMDD                                   
         B     DTEVO                                                            
*                                                                               
DTEV1    GOTO1 (RF),(R1),(2,IFLD)                                               
         OC    PLIST(4),PLIST                                                   
         BE    DTEVE                                                            
         OI    FIND,X'04'      DATE = YYMM                                      
         B     DTEVO                                                            
*                                                                               
DTEV3    MVC   TEMP(6),=C'999999'    CONVERT ALL TO 999999                      
         B     DTEVO                                                            
*                                                                               
DTEVE    MVC   FERN,=AL2(DTEINV)  INVALID DATE                                  
*                                                                               
         TM    RFPSTAT,RFPINUSE          SEE IF $RFP IN USE                     
         BZ    DTEVX                                                            
*                                                                               
*              $RFP - VALIDATE SYMBOLIC EQUATE                                  
*                                                                               
         MVI   FIND,X'01'                                                       
         XC    QRFPBLK(QRFPBLKQ),QRFPBLK                                        
         MVI   QRFPMODE,QRFPSYMB                                                
         MVC   QRFPWORK,IFLD                                                    
         OC    QRFPWORK,SPACES                                                  
         MVC   FERN,=AL2(ISYMBEQU)      INVALID SYMBOLIC EQUATE                 
         GOTO1 ARFP,DMCB                                                        
         CLI   QRFPMODE,QRFPOK                                                  
         BNE   DTEVX                                                            
*                                                                               
         CLC   QRFPDICT,=Y(PP#RFPBD)    BDATE                                   
         BNE   *+16                                                             
         OI    FIND,X'08'                                                       
         LA    RE,6                    LENGTH                                   
         B     DTERFP                                                           
*                                                                               
         CLC   QRFPDICT,=Y(PP#ABDAT)    BDATE                                   
         BNE   *+16                                                             
         OI    FIND,X'08'                                                       
         LA    RE,6                    LENGTH                                   
         B     DTERFP                                                           
*                                                                               
         CLC   QRFPDICT,=Y(PP#RFPSM)    START DATE (YYMM) + NO END              
         BNE   *+16                                                             
         OI    FIND,X'04'                                                       
         LA    RE,4                    LENGTH                                   
         B     DTERFP                                                           
*                                                                               
         B     DTEVX                                                            
*                                                                               
DTERFP   LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(L'QRFPESC,R7),QRFPESC                                          
         STC   RE,3(R7)                                                         
         MVC   FERN,=AL2(FF)                                                    
*                                                                               
         B     DTEVX                                                            
*                                                                               
DTEVO    LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         TM    FIND,X'14'                                                       
         BZ    *+14                                                             
         MVC   0(4,R7),TEMP                                                     
         B     *+10                                                             
         MVC   0(6,R7),TEMP                                                     
DTEVX    B     EXITVAL                                                          
         EJECT                                                                  
*                                                                               
*        VALIDATE REP CODE AND SET FIND BITS                                    
*        X'02' = ALL                                                            
*        X'04' = NNNN                                                           
*                                                                               
REPVAL   NTR1                                                                   
*                                                                               
*        NAME SEARCH CALL                                                       
*                                                                               
         L     R4,FLDHADR                                                       
         SR    R4,R3               GET DISPLACEMENT INTO TWA OF PUB             
         LA    R5,TEMP                                                          
         USING DSPARM,R5                                                        
         XC    DSPARM(DSPARML),DSPARM                                           
         MVC   DSMEDCOD,RMED                                                    
*                                                                               
         GOTO1 =V(SRCHCALL),PLIST,(3,(R4)),(X'80',(R3)),ACOMFACS,      X        
               ('DSPARML',TEMP),(1,=CL8'REP'),0,RR=RELO                         
         DROP  R5                                                               
*                                                                               
         GOTO1 AINITV              RETURNS R4(FLD ADDR), R5(LEN)                
*                                                                               
         CLC   RNUM,=C'01'         REQUEST 01?                                  
         BE    REPV0FA                                                          
         CLC   RNUM,=C'02'         REQUEST 02?                                  
         BNE   REPV001                                                          
REPV0FA  LR    R2,R3                                                            
         USING T412FFD,R2                                                       
         XC    BVRHDR,BVRHDR                                                    
*                                                                               
         CLI   IFLDH+5,0           ANY INPUTS?                                  
         BE    REPVX                                                            
         CLI   RO1,C'R'            RECORD TYPE MUST BE REPS                     
         BE    REPV001                                                          
         MVC   BVRHDR(L'RN01ERR2),RN01ERR2                                      
         MVC   FERN,=AL2(FE)                                                    
         B     REPVX                                                            
         DROP  R2                                                               
*                                                                               
REPV001  DS    0H                                                               
*                                                                               
         CLI   FIND,1                                                           
         BNE   REPVO               ALL OR MISSING                               
*                                                                               
REPVAL1  TM    4(R4),X'08' NUMERIC                                              
         BO    REPVAL1A                                                         
         MVC   FERN,=AL2(NUMINV)  NOT NUMERIC                                   
         B     REPVO                                                            
*                                                                               
REPVAL1A CHI   R5,4                                                             
         BNH   REPVAL1C                                                         
         MVC   FERN,=AL2(FLDINV)    INVALID                                     
         B     REPVO                                                            
*                                                                               
REPVAL1C BCTR  R5,R0                                                            
         EX    R5,REPPACK                                                       
         OI    DUB+7,X'0F'                                                      
         UNPK  RPAY(4),DUB+5(3)                                                 
         MVC   KRT1+4(4),RPAY                                                   
         MVI   KRT1+3,X'11'                                                     
         GOTO1 AREAD,PLIST,C'PRT1'                                              
         CLC   FERN,=AL2(FE)                                                    
         BL    REPVO                                                            
         BH    REPVAL1E                                                         
         MVC   FERN,=AL2(MISREC)                                                
         B     REPVO                                                            
REPVAL1E MVC   NAME(20),PRTREC+35                                               
         MVC   IFLD(4),RPAY                                                     
         OI    FIND,X'04'       REP=NNNN                                        
         B     REPVO                                                            
*                                                                               
REPPACK  PACK  DUB,IFLD(0)                                                      
*                                                                               
REPVO    FOUT  (R6),NAME                                                        
*                                                                               
         CLC   RNUM,=C'01'         REQUEST 01?                                  
         BE    REPVO40                                                          
         CLC   RNUM,=C'02'         REQUEST 02?                                  
         BNE   REPVO50                                                          
REPVO40  MVI   RPUB+1,C'R'                                                      
         MVC   RPUB+2(4),IFLD                                                   
         MVC   RPAY(4),=C'    '    CLR IT, JUST IN CASE                         
         B     REPVX                                                            
*                                                                               
REPVO50  MVC   RPAY(4),IFLD                                                     
*                                                                               
REPVX    B     EXITVAL                                                          
*                                                                               
*                                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
*        VALIDATE ONE CHARACTER    X'04'=X                                      
*                                                                               
ONEVAL   NTR1                      ONE CHR INPUT - FIND BITS                    
         GOTO1 AINITV              04 = X                                       
*                                                                               
         CLI   IFLDH+5,1                                                        
         BH    ONEVE                                                            
         LH    R7,COLNUM                     FIND REQNUM/COLNUM ENTRY           
         LA    R7,1(R7)                                                         
         STC   R7,FULL+2                                                        
         MVC   FULL(2),REQNUM                                                   
         LA    R7,ONEVTBL                                                       
         SR    R5,R5                                                            
ONEV1    IC    R5,0(R7)                                                         
         LTR   R5,R5                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   1(3,R7),FULL                                                     
         BE    ONEV2                                                            
         AR    R7,R5                                                            
         B     ONEV1                                                            
ONEV2    LR    RA,R7                         SAVE ENTRY ADDR                    
         CLI   IFLDH,0                       SET DEFAULT IF NO INPUT            
         BNE   ONEV3                                                            
         CLI   4(R7),C'*'                                                       
         BNE   *+8                                                              
         LA    R7,1(R7)                                                         
         MVC   IFLD,4(R7)                                                       
         B     ONEVO                                                            
ONEV3    AHI   R5,-4                         SEARCH VALUE LIST(S)               
ONEV4    CLI   4(R7),C'*'                                                       
         BNE   ONEV5                                                            
         CLI   DDS,1                         END OF STD LIST                    
         BNE   ONEVE                         ERROR NOT DDS TERMINAL             
         B     ONEV6                                                            
*                                                                               
ONEV5    CLC   4(1,R7),IFLD                                                     
         BE    ONEV7                                                            
         CLI   4(R7),254           MEANS ACCEPT ANY ALPHA/NUMERIC               
         BNE   ONEV6                                                            
         CLI   IFLD,C'A'                                                        
         BL    ONEVE                                                            
         MVC   TEMP(1),IFLD                                                     
         NI    TEMP,X'0F'                                                       
         CLI   TEMP,X'09'                                                       
         BH    ONEVE                                                            
         B     ONEV7               ACCEPT INPUT                                 
*                                                                               
ONEV6    LA    R7,1(R7)                                                         
         BCT   R5,ONEV4                                                         
         B     ONEVE                                                            
ONEV7    OI    FIND,X'04'                    CHR = X                            
         B     ONEVO                                                            
ONEVE    MVC   FERN,=AL2(ONEINV)             INVALID CHR                        
*                                                                               
ONEVO    DS    0H                                                               
         SR    R5,R5                                                            
         IC    R5,0(RA)                      R5=L'TBL ENTRY                     
         AR    RA,R5                         RA=A(NEXT TBL ENTRY)               
         CLI   1(RA),X'FF'                   SYNONYM ENTRY                      
         BNE   *+12                          NO                                 
         AR    R7,R5                         YES REPLACE INPUT                  
         MVC   IFLD(1),4(R7)                                                    
         LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(1,R7),IFLD                                                     
*                                                                               
ONEVX    B     EXITVAL                                                          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         EJECT                                                                  
*              VALIDATE AMOUNT AND SET FIND BITS                                
*                                                                               
*        X'04'=NNNNNNNN.NN                                                      
*        X'08'= FEE=NNNNNNNN.NN                                                 
*        X'10'= PCT=NNN.NN                                                      
*        X'20'= R=YYMMDD,I=NNNN                                                 
*        X'40'= ONLY PREV  OR  NO PREV                                          
*              OR TIME OR INTEGRATION OR PREV OR OLD OR PKG=NNN                 
*                                                                               
AMTVAL   NTR1                                                                   
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BNE   AMTVX                                                            
         CLC   RNUM,=C'07'         IS IT NEW BILLING                            
         BE    NEWBILL                                                          
         CLC   RNUM,=C'B1'         IS IT NEW BILLING                            
         BE    NEWBILL                                                          
         CLC   RNUM,=C'D1'         IS IT NEW BILLING                            
         BE    NEWBILL                                                          
         CLC   RNUM,=C'E1'         IS IT NEW BILLING                            
         BE    NEWBILL                                                          
*                                                                               
         CLC   RNUM,=C'R1'         IS IT NEW BILLING                            
         BE    NEWBILL                                                          
         CLC   RNUM,=C'RD'         IS IT NEW BILLING REBATE DRAFT               
         BE    NEWBILL                                                          
*                                                                               
         BNE   OAMTVAL              NO/ GOTO OLD PRINT BILLING                  
         SPACE 2                                                                
*                                   YES/ NEW BILLING                            
NEWBILL  MVC   TEMP(12),=12C' '                                                 
         GOTO1 AINITV                                                           
         CLI   FIND,X'01'                                                       
         BNE   AMTV0               AMT ALL OR MISSING                           
         CLC   IFLD(4),=C'FEE='                                                 
         BE    AMTV10                                                           
         CLC   IFLD(4),=C'PCT='                                                 
         BE    AMTV20                                                           
         CLC   IFLD(2),=C'R='                                                   
         BE    AMTV30                                                           
         CLC   IFLD(12),=CL12'ONLY PREV'                                        
         BE    AMTV3                                                            
         CLC   IFLD(12),=CL12'NO PREV'                                          
         BE    AMTV3                                                            
         CLC   IFLD(12),=CL12'PREV'                                             
         BE    AMTV3                                                            
         CLC   IFLD(12),=CL12'OLD'                                              
         BE    AMTV3                                                            
         CLC   IFLD(12),=CL12'NON-RETAIL'                                       
         BE    AMTV3                                                            
         B     AMTV4                                                            
         SPACE                                                                  
*                                                                               
AMTV3    LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(9,R7),IFLD                                                     
         OI    FIND,X'40'                                                       
         B     AMTVX                                                            
*                                                                               
AMTV4    DS    0H                                                               
         CLC   RNUM,=C'07'         PRINT UNBILLING ?                            
         BE    AMTV40              YES, SLIGHTLY DIFFERENT FORMAT               
*                                                                               
         CLI   RMED,C'*'           NO MANUAL AMOUNT FOR MEDIA *                 
         BE    AMTVE                                                            
*                                                                               
         MVC   TEMP(2),=C'G='      SET TO GROSS = (= DROPS IN AMTOK)            
         MVI   TEMP+14,X'04'       VALIDATION BIT                               
         LA    R4,IFLD                                                          
AMTV5    GOTO1 CASHVAL,PLIST,0(R4),(R5)                                         
         CLI   PLIST,0                                                          
         BNE   AMTVE                                                            
         L     R7,PLIST+4                                                       
         CVD   R7,DUB                                                           
         UNPK  TEMP+2(10),DUB                                                   
         OI    TEMP+11,X'F0'                                                    
         TM    PLIST+4,X'80'       TEST NEGATIVE                                
         BZ    AMTV8                                                            
         MVI   TEMP+2,C'-'                                                      
         CP    DUB,=P'-2100000000'                                              
         BL    AMTVE               CAN'T HANDLE MINUS 21 MILLIONW               
*                                                                               
AMTV8    DS    0H                                                               
         CLI   TEMP,C'G'           GROSS?                                       
         BE    *+12                                                             
         CLI   TEMP,C'F'           FEE?                                         
         BNE   *+14                                                             
         CP    DUB,=P'2100000000'                                               
         BH    AMTVE               CAN'T HANDLE 21 MILLIONS                     
*                                                                               
         OC    FIND,TEMP+14                                                     
         B     AMTOK                                                            
*                                                                               
AMTV10   SH    R5,=H'4'                                                         
         MVC   TEMP(2),=C'F='       (= DROPS IN AMTOK)                          
         MVI   TEMP+14,X'08'                                                    
         LA    R4,IFLD+4                                                        
         B     AMTV5                                                            
*                                                                               
AMTV20   SH    R5,=H'4'                                                         
         GOTO1 CASHVAL,PLIST,IFLD+4,(R5)                                        
         CLI   PLIST,0                                                          
         BNE   AMTVE                                                            
         L     R7,PLIST+4                                                       
         CVD   R7,DUB                                                           
         UNPK  TEMP+2(5),DUB                                                    
         OI    TEMP+6,X'F0'                                                     
         CP    DUB,=P'0'           CAN'T BE NEGATIVE                            
         BNH   AMTVE                                                            
         CP    DUB,=P'10000'                                                    
         BH    AMTVE                                                            
         MVC   TEMP(2),=C'P='         (= DROPS IN AMTOK)                        
         MVC   TEMP+7(4),=4C' '                                                 
         OI    FIND,X'10'                                                       
         B     AMTOK                                                            
*                                                                               
AMTV30   DS    0H                                                               
         MVI   ROUTSUB,1           EXTRACT DATE                                 
         GOTO1 AINITV                                                           
         GOTO1 DATVAL,PLIST,(0,IFLD+2),TEMP+12                                  
         OC    PLIST(4),PLIST                                                   
         BE    AMTVDE              DATE ERROR                                   
         MVI   TEMP,C'R'                                                        
         MVC   TEMP+1(6),TEMP+12                                                
         MVI   ROUTSUB,2           GET INV NUMBER                               
         GOTO1 AINITV                                                           
         CLI   IFLDH,1                                                          
         BNE   AMTVE                                                            
         CLC   IFLD(2),=C'I='                                                   
         BNE   AMTVE                                                            
         CLI   IFLDH+5,6                                                        
         BNE   AMTVE                                                            
         LA    R6,4                                                             
         LA    R5,IFLD+2                                                        
AMTV35   CLI   0(R5),C'0'                                                       
         BL    AMTVE                                                            
         CLI   0(R5),C'9'                                                       
         BH    AMTVE                                                            
         LA    R5,1(R5)                                                         
         BCT   R6,AMTV35                                                        
         MVC   TEMP+7(4),IFLD+2                                                 
         OI    FIND,X'20'                                                       
         LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(11,R7),TEMP      SET R123456NNNN IN REQ CARD                   
         B     AMTVX                                                            
*                                                                               
AMTV40   DS    0H                  FOR PRINT UNBILLIG USE PL8'S                 
         LA    R4,IFLD                                                          
         GOTO1 CASHVAL,PLIST,(X'80',0(R4)),(R5)                                 
         CLI   PLIST,0                                                          
         BNE   AMTVE                                                            
         MVC   DUB,PLIST+4                                                      
         UNPK  TEMP(12),DUB                                                     
         OI    TEMP+11,X'F0'                                                    
         CP    PLIST+4(8),=P'0'      TEST NEGATIVE                              
         BNL   AMTV45                                                           
         MVI   TEMP,C'-'                                                        
         CP    DUB,=P'-99999999999'                                             
         BL    AMTVE               CAN'T HANDLE MINUS 1 BILLION                 
AMTV45   OI    FIND,X'04'                                                       
         LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(12,R7),TEMP                                                    
         B     AMTV0                                                            
*                                                                               
*                                                                               
AMTVE    MVC   FERN,=AL2(FLDINV)                                                
         B     AMTVX                                                            
AMTVDE   MVC   FERN,=AL2(SEDINV)                                                
         B     AMTVX                                                            
*                                                                               
AMTV0    B     AMTVX              NO INPUT                                      
         SPACE                                                                  
AMTOK    DS    0H                                                               
         LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(1,R7),TEMP        MOVE IN P/G                                  
         L     R6,PLIST+4          LOAD IN BINARY AMT                           
         STCM  R6,15,1(R7)                                                      
         CLC   RPRO,=C'   '        PRD OR EST CAN'T BE ALL                      
         BE    AMTER1              OR BLANK                                     
         CLC   RPRO,=C'ALL'                                                     
         BE    AMTER1                                                           
         CLC   REST,=C'   '                                                     
         BE    AMTER2                                                           
         CLC   REST,=C'ALL'                                                     
         BE    AMTER2                                                           
         B     AMTVX                                                            
*                                                                               
*                                                                               
OAMTVAL  XC    RPAY,RPAY        * OLD PRINT VALIDATION                          
         SR    R6,R6                                                            
         IC    R6,IFLDH+5                                                       
         GOTO1 CASHVAL,PLIST,(2,IFLD),(R6)                                      
         CLI   PLIST,X'FF'                                                      
         BNE   OAMTV1                                                           
         MVC   FERN,=AL2(FLDINV)                                                
         B     AMTVX                                                            
*                                                                               
OAMTV1   MVC   RPAY,PLIST+4                                                     
         CLC   RPRO,=C'   '                                                     
         BE    AMTER1                                                           
         CLC   RPRO,=C'ALL'                                                     
         BE    AMTER1                                                           
         CLC   REST,=C'   '                                                     
         BE    AMTER2                                                           
         CLC   REST,=C'ALL'                                                     
         BE    AMTER2                                                           
         OI    FIND,X'04'                                                       
         B     AMTVX                                                            
*                                                                               
AMTER1   MVC   FERN,=AL2(PDNALL)                                                
         B     AMTVX                                                            
AMTER2   MVC   FERN,=AL2(ESNALL)                                                
         B     AMTVX                                                            
*                                                                               
AMTVX    B     EXITVAL                                                          
         EJECT                                                                  
*                                                                               
*        VALIDATE MANUAL CD AND SET FIND BIT                                    
*        X'04'=$AMT                                                             
*        BINARY VALUE SET IN RO4(4)                                             
*                                                                               
MCDVAL   NTR1                                                                   
         GOTO1 AINITV                                                           
         XC    RO4(4),RO4                                                       
         CLI   FIND,1                                                           
         BNE   MCDVX                                                            
         CLC   IFLD(10),=C'NON-RETAIL'                                          
         BNE   MCDVA                                                            
*                                                                               
         CLC   RNUM,=C'B1'         NON-RETAIL ONLY FOR B1                       
         BE    MCB1D1                                                           
         CLC   RNUM,=C'D1'         DITTO D1                                     
         BNE   MCDVB                                                            
MCB1D1   CLI   RCARD2+26,C'R'      AND ONLY IF RCARD2+26=R                      
         BNE   MCDVB                                                            
         MVC   RO4(4),IFLD           IF YES/MOVE NON- TO RO4                    
         OI    FIND,X'04'                                                       
         B     MCDVX                                                            
*                                                                               
MCDVA    SR    R6,R6                                                            
         IC    R6,IFLDH+5                                                       
         GOTO1 CASHVAL,PLIST,(2,IFLD),(R6)                                      
         CLI   PLIST,X'FF'                                                      
         BNE   MCDV1                                                            
MCDVB    MVC   FERN,=AL2(FLDINV)                                                
         B     MCDVX                                                            
*                                                                               
MCDV1    CLC   RNUM,=C'B1'                                                      
         BE    MCDV1A                                                           
         CLC   RNUM,=C'D1'                                                      
         BNE   MCDV5                                                            
MCDV1A   CLI   RCARD2+26,C' '      B1/D1 HAS SPACES                             
         BE    MCDER1              IF YES/ERROR - CAN'T USE MANUAL CD           
         B     MCDV7               WITHOUT MANUAL                               
*                                                                               
MCDV5    OC    RPAY,RPAY        SEE IF GROSS INPUT                              
         BZ    MCDER1                                                           
MCDV7    CLC   RPRO,=C'   '                                                     
         BE    MCDER2                                                           
         CLC   RPRO,=C'ALL'                                                     
         BE    MCDER2                                                           
         CLC   REST,=C'   '                                                     
         BE    MCDER3                                                           
         CLC   REST,=C'ALL'                                                     
         BE    MCDER3                                                           
         MVC   RO4(4),PLIST+4                                                   
         OI    FIND,X'04'                                                       
         B     MCDVX                                                            
*                                                                               
MCDER1   MVC   FERN,=AL2(NOGROSS)                                               
         B     MCDVX                                                            
MCDER2   MVC   FERN,=AL2(PDNALL)                                                
         B     MCDVX                                                            
MCDER3   MVC   FERN,=AL2(ESNALL)                                                
         B     MCDVX                                                            
*                                                                               
MCDVX    B     EXITVAL                                                          
         EJECT                                                                  
*                                                                               
*        VALIDATE COMMENT NUMBER AND SET FIND BIT                               
*        X'04'=XXXXXX                                                           
*                                                                               
COMVAL   NTR1                                                                   
*                                                                               
         GOTO1 AINITV                                                           
*                                                                               
         CLC   RNUM,=C'01'         REQUEST 01?                                  
         BE    COMV0FA                                                          
         CLC   RNUM,=C'02'         REQUEST 02?                                  
         BNE   COMV001                                                          
COMV0FA  LR    R2,R3                                                            
         USING T412FFD,R2                                                       
         XC    BVRHDR,BVRHDR                                                    
*                                                                               
         CLI   IFLDH+5,0           ANY INPUTS?                                  
         BE    COMVX                                                            
         CLI   RO1,C'C'            RECORD TYPE MUST BE COMMENTS                 
         BE    COMV001                                                          
         MVC   BVRHDR(L'RN01ERR3),RN01ERR3                                      
         MVC   FERN,=AL2(FE)                                                    
         B     COMVX                                                            
         DROP  R2                                                               
*                                                                               
COMV001  DS    0H                                                               
*                                                                               
         CLI   FIND,1                                                           
         BNE   COMVX                                                            
         MVC   COMWORK,=10C' '      MUST INITIALIZE COMWORK                     
         BCTR  R5,R0                                                            
         EX    R5,COMMOVE                                                       
         LA    R5,1(R5)       RESTORE LENGTH                                    
         CLI   COMWORK+5,C' '                                                   
         BNE   COMVB                                                            
         MVC   TEMP(5),COMWORK                                                  
         LA    R6,COMWORK                                                       
         SR    R7,R7                                                            
         LA    R7,6                                                             
         SR    R7,R5                                                            
         AR    R6,R7                                                            
         BCTR  R7,R0                                                            
         BCTR  R5,R0                                                            
         EX    R5,MOVECOM                                                       
         EX    R7,MOVESPE                                                       
         B     COMVB                                                            
*                                                                               
MOVECOM  MVC   0(0,R6),TEMP        EXECUTED                                     
MOVESPE  MVC   COMWORK(0),=C'     '  EXECUTED                                   
*                                                                               
*                                                                               
COMVB    XC    KRT1+10(14),KRT1+10                                              
         MVC   KRT1+4(6),COMWORK                                                
         MVI   KRT1+3,X'40'                                                     
         GOTO1 AREAD,PLIST,C'PRT1'                                              
         CLC   FERN,=AL2(FE)                                                    
         BL    COMVX               DISK ERROR                                   
         BH    COMVC               FOUND                                        
         MVC   FERN,=AL2(MISREC)   NOT FOUND                                    
         B     COMVX                                                            
*                                                                               
COMMOVE  MVC   COMWORK(0),IFLD       EXECUTED                                   
*                                                                               
*                                                                               
COMVC    DS    0H                                                               
         LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         CLI   0(R7),C' '          SEE IF DATA ALREADY THERE                    
         BNE   COMVERR                                                          
         MVC   0(6,R7),COMWORK                                                  
         OI    FIND,X'04'          VALID XXXXXX                                 
*                                                                               
         CLC   RNUM,=C'01'         REQUEST 01?                                  
         BE    COMVC4                                                           
         CLC   RNUM,=C'02'         REQUEST 02?                                  
         BNE   *+8                                                              
COMVC4   MVI   RPUB+1,C'C'         FOR COMMENTS                                 
*                                                                               
         CLC   RNUM,=C'L1'         SPECIAL FOR PPL1                             
         BE    COMVC5                                                           
         CLC   RNUM,=C'LB'         SPECIAL FOR PPLB                             
         BNE   COMVX                                                            
COMVC5   MVC   RPUB+1(3),=C'SC='   PRREQ00 SETS COLNUM TO 30                    
         B     COMVX                                                            
*                                                                               
COMVERR  MVC   FERN,=AL2(FLDINV)                                                
COMVX    B     EXITVAL                                                          
*                                                                               
COMWORK  DS    CL10                                                             
         EJECT                                                                  
*                                                                               
*        VALIDATE CHANGE CONTROL DATE AND SET FIND BIT                          
*        X'04'=BILL       OUTPUT IS X'000000'                                   
*                SPECIAL TO ACCOMPANY BILLING FOR PP52 +  PPEC                  
*        X'08'=YYMMDD     OUTPUT TO 3 BYTE BINARY FIELD                         
*                                                                               
CDATVAL  NTR1                                                                   
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BNE   CDATVX                                                           
*****                                                                           
         LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)       SET R7 TO PROPER COLUMN                        
**                                                                              
         CLC   RNUM,=C'52'                                                      
         BE    CDATV10                                                          
         CLC   RNUM,=C'EC'                                                      
         BNE   CDATV30                                                          
CDATV10  CLC   IFLD(4),=C'BILL'                                                 
         BNE   CDATV30                                                          
         XC    0(3,R7),0(R7)                                                    
         OI    FIND,X'04'                                                       
         B     CDATVX                                                           
*****                                                                           
CDATV30  GOTO1 DATVAL,PLIST,IFLD,CDATWK                                         
         CLI   3(R1),0                                                          
         BE    CDATINV             INVALID                                      
         GOTO1 DATCON,PLIST,CDATWK,(3,0(R7))                                    
         OI    FIND,X'08'          VALID YYMMDD                                 
         B     CDATVX                                                           
*                                                                               
CDATINV  MVC   FERN,=AL2(SEDINV)   DATE INVALID                                 
CDATVX   B     EXITVAL                                                          
*                                                                               
CDATWK   DS    CL10                WORK AREA                                    
         EJECT                                                                  
*                                                                               
*        VALIDATE CONTRACT NUMBER AND SET FIND BITS                             
*             X'08'=C'000'                                                      
*        X'04'=NNN                                                              
*        X'02'=ALL          OUTPUT TO REST FOR REQ 12                           
*                                                                               
CONVAL   NTR1                                                                   
         GOTO1 AINITV                                                           
         CLI   IFLDH+5,1           1-POS'N ENTRY ?                              
         BNE   CVAL1               NO                                           
         CLI   IFLD,C'H'           H FOR "HIGH" ?                               
         BE    CVAL2               YES                                          
         CLI   IFLD,C'L'           L FOR "LAST" ?                               
         BE    CVAL2               YES                                          
         B     CVALX               FIELD NOT H OR L                             
CVAL1    CLI   IFLDH+5,4                                                        
         BL    CVALX               FIELD NOT "HIGH" OR "LAST"                   
         CLC   IFLD(4),=C'HIGH'    LOOK FOR HIGHEST CONTRACT NUMBER ?           
         BE    CVAL2               YES                                          
         CLC   IFLD(4),=C'LAST'  LOOK FOR CON WITH HIGHEST START DATE ?         
         BNE   CONINV              NO - CONTRACT FIELD INVALID                  
CVAL2    DS    0H                                                               
         OC    KRT1+4(3),KRT1+4    IS CLIENT SPECIFIED ?                        
         BZ    CONINV              NO - HIGH AND LAST ARE INVALID               
         OC    KUB1+1(6),KUB1+1    IS PUB SPECIFIED ?                           
         BZ    CONINV              NO - HIGH AND LAST ARE INVALID               
*                                                                               
         CLC   RNUM,=C'AR'         SEE IF AOR CONTRACT REPORT                   
         BE    CONINV              YES - HIGH AND LAST ARE INVALID              
         CLI   RPUB+10,C'Z'        SEE IF DOING ALL EDITIONS                    
         BE    CONINV              YES - HIGH AND LAST ARE INVALID              
*                                                                               
         XC    SVCONDT,SVCONDT     CLEAR SEARCH CONTRACT START DATE             
         XC    SVLCON,SVLCON       CLEAR SEARCH CONTRACT NUMBER                 
         XC    KEYSAVE,KEYSAVE                                                  
         XC    KRT1+13(12),KRT1+13    CLEAR KEY FOLLOWING PUB                   
         MVI   KRT1+3,X'10'        CONTRACT CODE                                
         MVC   KRT1+7(6),KUB1+1    PUB NUMBER                                   
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'PRTDIR',KRT1,KEYSAVE                  
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     CVAL4B                                                           
*                                                                               
CVAL4    GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'PRTDIR',KEYSAVE,KEYSAVE               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CVAL4B   CLC   KRT1(13),KEYSAVE    CHECK THRU PUB                               
         BNE   CVAL4T              GO SEE IF ANYTHING FOUND                     
         MVC   FULL,KEYSAVE+27     SAVE REC ADDRESS                             
*****    CLC   IFLD(4),=C'HIGH'    SEE IF LOOKING FOR HIGHEST CONTRACT          
         CLI   IFLD,C'H'           SEE IF LOOKING FOR HIGHEST CONTRACT          
         BNE   CVAL4D              NO - READ THE RECORD FOR DATE                
         TM    PROSAVE,X'04'       WAS PRODUCT SPECIFIED ?                      
         BO    CVAL4D               YES - READ THE RECORD FOR PRD CODE          
         MVC   SVLCON,KEYSAVE+13    NO - SAVE NUMBER AND                        
         B     CVAL4                     KEEP SEARCHING                         
*                                                                               
CVAL4D   DS    0H                  READ THE RECORD                              
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'PRTFIL',FULL,PRTREC,DMWORK            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   PRTREC+33,X'10'     CONTRACT DESCRIPTION ELEMENT ?               
         BE    *+6                 YES                                          
         DC    H'0'                                                             
*                                                                               
*****    CLC   IFLD(4),=C'HIGH'    LOOKING FOR HIGHEST CONTRACT NUM ?           
         CLI   IFLD,C'H'           LOOKING FOR HIGHEST CONTRACT NUM ?           
         BE    CVAL4G              YES - CHECK FOR PRODUCT                      
*                               LOOKING FOR HIGHEST START DATE                  
         CLC   SVCONDT,PRTREC+35   EST DATE IN RECORD HIGH ?                    
         BH    CVAL4               NO - KEEP SEARCHING                          
*                                  YES - DATE HIGH                              
CVAL4G   TM    PROSAVE,X'04'       WAS PRODUCT SPECIFIED ?                      
         BNO   CVAL4K              NO                                           
         OC    PRTREC+50(3),SPACES                                              
         CLC   PRTREC+50(3),RPRO   IS PRD THE ONE SPECIFIED ?                   
         BNE   CVAL4               NO - KEEP SEARCHING                          
CVAL4K   MVC   SVCONDT,PRTREC+35   YES - SAVE DATE                              
         MVC   SVLCON,PRTREC+13          AND CONTRACT NUMBER                    
         B     CVAL4               KEEP SEARCHING                               
*                                                                               
CVAL4T   DS    0H                  LAST CONTRACT READ                           
         OC    SVLCON,SVLCON       SEE IF ANY FOUND                             
         BNZ   CVAL4TX             YES                                          
         MVC   FERN,=AL2(MISREC)   NOT ON FILE                                  
         B     CONVO                                                            
*                                                                               
CVAL4TX  OI    FIND,X'04'          CONTRACT VALID NNN                           
         LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         EDIT  (B2,SVLCON),(3,0(R7)),0,ALIGN=LEFT,FILL=0,ZERO=NOBLANK           
         XC    8(4,R4),8(R4)       CLEAR LAST OR HIGH                           
         FOUT  (R4),0(R7),3        DISPLAY CONTRACT NUMBER                      
         B     CONVX               DONE WITH "HIGH" AND "LAST"                  
*                                                                               
*                                                                               
CVALX    DS    0H                  CONTRACT NOT "LAST" OR "HIGH"                
         CLI   FIND,1                                                           
         BNE   CONVO                                                            
         GOTO1 ARJN                                                             
         CLC   TEMP+2(3),=C'000'                                                
         BNE   CONV1                                                            
         MVC   FERN,=AL2(FF)                                                    
         OI    FIND,X'08'        CONTRACT=000                                   
         MVC   IFLD(3),TEMP+2                                                   
         B     CONVO                                                            
*                                                                               
CONV1    CLC   FERN,=AL2(FF)                                                    
         BL    CONINV                                                           
         MVC   IFLD(3),TEMP+2                                                   
         MVI   KRT1+3,X'10'                                                     
         MVC   KRT1+7(6),KUB1+1       PUB NUMBER                                
         MVC   KRT1+13(2),TEMP      BINARY CONTRACT NUMBER                      
         OC    KRT1+4(3),KRT1+4       CLIENT NOT SPECIFIED                      
         BZ    CONINV                                                           
         OC    KRT1+7(6),KRT1+7      PUB NOT SPECIFIED                          
         BZ    CONINV                                                           
*                                                                               
         CLC   RNUM,=C'AR'      SEE IF AOR CONTRACT REPORT                      
         BE    CONV4            CAN'T READ CONTRACT                             
*                                                                               
         CLI   RPUB+10,C'Z'      SEE IF DOING ALL EDITIONS                      
*NOP*    BE    CONV4             BYPASS FILE READ                               
         BNE   CONV2             GO DO FILE READ                                
*    DOING ALL EDITIONS - MUST FIND AT LEAST ONE CONTRACT                       
         XC    KEYSAVE,KEYSAVE                                                  
         XC    KRT1+11(14),KRT1+11    CLEAR KEY FOLLOWING PUB                   
         MVI   KRT1+3,X'10'        CONTRACT CODE                                
         MVC   KRT1+7(4),KUB1+1    PUB NUMBER (BASE ONLY)                       
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'PRTDIR',KRT1,KEYSAVE                  
         CLI   DMCB+8,0                                                         
         BE    CONV1T                                                           
         DC    H'0'                                                             
*                                                                               
CONV1G   GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'PRTDIR',KEYSAVE,KEYSAVE               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CONV1T   CLC   KRT1(11),KEYSAVE    CHECK THRU PUB BASE                          
         BNE   CONV2E              NOTHING FOUND - ERROR                        
         CLC   KEYSAVE+13(2),TEMP  MATCHING BINARY CONTRACT NUMBER ?            
         BE    CONV4               OK                                           
         B     CONV1G              TRY NEXT CONTRACT RECORD                     
*                                                                               
CONV2    GOTO1 AREAD,PLIST,C'PRT1'                                              
         CLC   FERN,=AL2(FE)                                                    
         BL    CONVO                                                            
         BH    CONV4                                                            
CONV2E   MVC   FERN,=AL2(MISREC) NOT ON FILE                                    
         B     CONVO                                                            
CONV4    OI    FIND,X'04'        CONTRACT VALID NNN                             
*****                                                                           
         CLC   RNUM,=C'16'                                                      
         BNE   CONVO                                                            
         LA    R6,PRTREC+33                                                     
CONV5    CLI   0(R6),X'00'                                                      
         BE    CONVO                                                            
         CLI   0(R6),X'85'                                                      
         BE    CONV8                                                            
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     CONV5                                                            
CONV8    MVI   RFREQ,C'Y'                                                       
*****                                                                           
         B     CONVO                                                            
*                                                                               
CONINV   MVC   FERN,=AL2(FLDINV)                                                
CONVO    LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(3,R7),IFLD                                                     
*                                                                               
CONVX    B     EXITVAL                                                          
         EJECT                                                                  
*                                                                               
*        VALIDATE DAYS DUE AND SET FIND BIT                                     
*        X'04'=NN OR 00                                                         
*                                                                               
DAYDVAL  NTR1                                                                   
DAYD1    GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BNE   DAYDVX              NO INPUT                                     
*                                                                               
DAYD5    GOTO1 ARJN                                                             
         CLC   FERN,=AL2(FLDINV)   ZERO                                         
         BNE   DAYDV2                                                           
         MVC   IFLD(2),=C'00'      ZERO IS OK                                   
         MVC   FERN,=AL2(FF)                                                    
         B     DAYDV4                                                           
*                                                                               
DAYDV2   CLC   FERN,=AL2(FF)                                                    
         BL    DAYDINV                                                          
         MVC   IFLD(2),TEMP+3                                                   
DAYDV4   OI    FIND,X'04'       DAYS DUE = NN                                   
         B     DAYDVO                                                           
*                                                                               
DAYDINV  MVC   FERN,=AL2(FLDINV)                                                
         B     DAYDVX                                                           
*                                                                               
DAYDVO   LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(2,R7),IFLD                                                     
*                                                                               
DAYDVX   B     EXITVAL                                                          
         EJECT                                                                  
*                                                                               
*          VALIDATE BILLING PERIOD                                              
*        X'04'=YYMM       MONTH OF SERVICE                                      
*        X'08'=YYMMDD,YYMMDD                                                    
*                                                                               
BILLVAL  NTR1                                                                   
         MVI   ROUTSUB,1                                                        
         GOTO1 AINITV                                                           
         CLI   5(R4),0                                                          
         BE    BILLEXT         NO INPUT                                         
         CLI   IFLDH,1                                                          
         BNE   BILLVE                                                           
         GOTO1 DATVAL,PLIST,(0,IFLD),RSTRD                                      
         OC    PLIST(4),PLIST                                                   
         BE    *+12                                                             
         OI    FIND,X'08'          YYMMDD                                       
         B     BENDVAL                                                          
         GOTO1 (RF),(R1),(2,IFLD)                                               
         OC    PLIST(4),PLIST                                                   
         BE    BILLVE                                                           
         OI    FIND,X'04'          YYMM                                         
*                                                                               
BENDVAL  MVI   ROUTSUB,2                                                        
         GOTO1 AINITV                                                           
         TM    FIND,X'04'                                                       
         BNO   BEND1                                                            
         CLI   IFLDH,0                                                          
         BNE   BILLVE     IF START WAS YYMM THEN NO END ALLOWED                 
         MVC   RSTRD+4(2),=C'01'       SET DAY TO 01                            
         B     BILLVO                                                           
*                                                                               
BEND1    CLI   IFLDH,1                                                          
         BNE   BILLVE     END REQUIRED IF START WAS YYMMDD                      
         GOTO1 DATVAL,PLIST,(0,IFLD),RENDD                                      
         OC    PLIST(4),PLIST                                                   
         BE    BILLVE     MUST BE YYMMDD                                        
         B     BEND3                                                            
*                                                                               
BEND3    CLC   RSTRD,RENDD                                                      
         BNH   BEND4                                                            
         MVC   FERN,=AL2(SEDSGE)                                                
         B     BILLEXT                                                          
*                                                                               
BEND4    CLC   RSTRD(4),RENDD       MUST BE SAME YYMM                           
         BE    BILLVO                                                           
         MVC   FERN,=AL2(SEDBIG)   INVALID DATE SPREAD                          
         B     BILLEXT                                                          
*                                                                               
BILLVE   MVC    FERN,=AL2(SEDINV)    INVALID DATE FORMAT                        
*                                                                               
         TM    RFPSTAT,RFPINUSE          SEE IF $RFP IN USE                     
         BZ    BILLEXT                                                          
*                                                                               
*              $RFP - VALIDATE SYMBOLIC EQUATE                                  
*                                                                               
         MVI   FIND,X'01'                                                       
         XC    QRFPBLK(QRFPBLKQ),QRFPBLK                                        
         MVI   QRFPMODE,QRFPSYMB                                                
         MVC   QRFPWORK,IFLD                                                    
         OC    QRFPWORK,SPACES                                                  
         MVC   FERN,=AL2(ISYMBEQU)      INVALID SYMBOLIC EQUATE                 
         GOTO1 ARFP,DMCB                                                        
         CLI   QRFPMODE,QRFPOK                                                  
         BNE   BILLEXT                                                          
*                                                                               
         CLC   QRFPDICT,=Y(PP#RFPSM)    START DATE (YYMM) + NO END              
         BNE   *+12                                                             
         OI    FIND,X'04'                                                       
         B     START400                                                         
*                                                                               
         CLC   QRFPDICT,=Y(PP#RFPRD)    START-END (YYMMDD-YYMMDD)               
         BNE   *+12                                                             
         OI    FIND,X'08'                                                       
         B     START400                                                         
*                                                                               
         B     BILLEXT                                                          
*                                                                               
START400 MVC   RSTRD,SPACES             STORE ESCAPE SEQ IN REQCARD             
         MVC   RSTRD(L'QRFPESC),QRFPESC                                         
         MVI   RSTRD+3,12               MUST ALTER LENGTH TO 12                 
         MVC   FERN,=AL2(FF)                                                    
         B     BILLEXT                                                          
*                                                                               
BILLVO   DS    0H                  NOW CHECK DATE VS. ESTIMATE                  
         OC    ESTDATES,ESTDATES   DO I HAVE ESTIMATE DATES?                    
         BZ    BILLEXT                                                          
         GOTO1 DATCON,PLIST,(0,ESTDATES),(3,WORK)                               
         GOTO1 DATCON,PLIST,(0,ESTDATES+6),(3,WORK+3)                           
         GOTO1 DATCON,PLIST,(0,RSTRD),(3,WORK+6)                                
*                                                                               
         CLC   RSTRD(4),ESTDATES   FIRST SEE IF AFTER START                     
         BH    BILLVO10                                                         
         BE    BILLEXT             IF EQUAL THEN O.K.                           
*                                                                               
         ZIC   R0,WORK+1                                                        
         SH    R0,=H'6'            BACK-UP 6 MONTHS                             
         CH    R0,=H'0'                                                         
         BNH   BILLVO2                                                          
         STC   R0,WORK+1                                                        
         B     BILLVO4                                                          
*                                                                               
BILLVO2  AH    R0,=H'12'           BUMP 12 MONTHS                               
         STC   R0,WORK+1                                                        
         ZIC   R0,WORK             AND DECREMENT YEAR                           
         SH    R0,=H'1'                                                         
         STC   R0,WORK                                                          
*                                                                               
BILLVO4  CLC   WORK+6(2),WORK                                                   
         BL    BILLVPE                                                          
         B     BILLEXT                                                          
*                                                                               
BILLVO10 CLC   RSTRD(4),ESTDATES+6  CHECK VS. EST END                           
         BNH   BILLEXT              IF NOT AFTER - O.K.                         
*                                 ADVANCE END 3 MONTHS                          
         ZIC   R0,WORK+4                                                        
         AH    R0,=H'3'                                                         
         CH    R0,=H'12'          OVER 12                                       
         BH    BILLVO12                                                         
         STC   R0,WORK+4                                                        
         B     BILLVO14                                                         
*                                                                               
BILLVO12 SH    R0,=H'12'           SUBTRACT 12 MONTHS                           
         STC   R0,WORK+4                                                        
         ZIC   R0,WORK+3           AND ADVANCE YEAR                             
         AH    R0,=H'1'                                                         
         STC   R0,WORK+3                                                        
*                                                                               
BILLVO14 CLC   WORK+6(2),WORK+3                                                 
         BNH   BILLEXT                                                          
*                                                                               
BILLVPE  MVC   FERN,=AL2(146)       MOS NOT WITHIN -6 OR +3 MONTHS              
         B     BILLEXT                                                          
                                                                                
BILLEXT  B     EXITVAL                                                          
         EJECT                                                                  
*                                                                               
*        VALIDATE LIST CODE AND SET FIND BITS                                   
*        X'02'=ALL                                                              
*        X'04'=XXX                                                              
*                                                                               
LSTVAL   NTR1                                                                   
*                                                                               
         GOTO1 AINITV                                                           
*                                                                               
         CLC   RNUM,=C'01'         REQUEST 01?                                  
         BE    LSTV0FA                                                          
         CLC   RNUM,=C'02'         REQUEST 02?                                  
         BNE   LSTV001                                                          
LSTV0FA  LR    R2,R3                                                            
         USING T412FFD,R2                                                       
         XC    BVRHDR,BVRHDR                                                    
*                                                                               
         CLI   IFLDH+5,0           ANY INPUTS?                                  
         BE    LSTVX                                                            
         CLI   RO1,C'L'            RECORD TYPE MUST BE PUB LIST                 
         BNE   LSTV0AC                                                          
         CLC   RCLI(3),=C'   '     CLIENT PRESENT?                              
         BE    LSTV0AD                                                          
*                                                                               
         CLC   RCLI(3),=C'ZZZ'     CLIENT ZZZ?                                  
         BE    LSTV0AB                                                          
         CLC   RCLI(3),=C'ALL'     CLIENT ALL?                                  
         BNE   LSTV001                                                          
LSTV0AB  MVC   KRT2+4(3),=C'ZZZ'   CLIENT ZZZ AND ALL ARE SAME                  
         B     LSTV001                                                          
*                                                                               
LSTV0AC  MVC   BVRHDR(L'RN01ERR4),RN01ERR4                                      
         B     LSTV0AF                                                          
LSTV0AD  MVC   BVRHDR(L'RN01ERR5),RN01ERR5                                      
         MVI   ROUTNUM,X'02'                                                    
         BAS   RE,PCURSOR                                                       
*                                                                               
LSTV0AF  MVC   FERN,=AL2(FE)                                                    
         B     LSTVX                                                            
         DROP  R2                                                               
*                                                                               
LSTV001  DS    0H                                                               
*                                                                               
         CLI   FIND,1                                                           
         BNE   LSTVO               LIST ALL OR MISSING                          
         MVC   KRT2+7(3),IFLD                                                   
         MVI   KRT2+10,X'01'       TO READ FIRST REC                            
         OC    KRT2+4(3),KRT2+4    WAS CLT INPUT                                
         BZ    LSTINV              NO - SPECIFIC LIST INVALID                   
         MVI   KRT2+3,X'17'                                                     
         GOTO1 AREAD,PLIST,C'PRT2'                                              
         CLC   FERN,=AL2(FE)                                                    
         BL    LSTVO                                                            
         BH    *+14                                                             
         MVC   FERN,=AL2(MISREC)   REC NOT FOUND                                
         B     LSTVO                                                            
*                                                                               
         OI    FIND,X'04'          LST=XXX                                      
         MVC   NAME(20),PRTREC+40                                               
         B     LSTVO                                                            
*                                                                               
LSTINV   MVC   FERN,=AL2(FLDINV)                                                
LSTVO    LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(3,R7),IFLD                                                     
*                                                                               
         CLC   RNUM,=C'01'         REQUEST 01?                                  
         BE    LSTVO60                                                          
         CLC   RNUM,=C'02'         REQUEST 02?                                  
         BNE   *+8                                                              
LSTVO60  MVI   RPUB+1,C'L'         FOR PUB LIST                                 
*                                                                               
         FOUT  (R6),NAME                                                        
LSTVX    B     EXITVAL                                                          
         EJECT                                                                  
*                                                                               
*        VALIDATE FILTER AND SET FIND BITS                                      
*        X'04'=1-6X                                                             
*                                                                               
FLTRVAL  NTR1                                                                   
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BNE   FLTRVX                                                           
         ZIC   R1,IFLDH+5                                                       
         LA    R4,IFLD                                                          
         LA    R5,6                                                             
         MVC   TEMP+2(6),=6C' '                                                 
         MVI   TEMP,0                                                           
         LA    R6,TEMP+2                                                        
*                                                                               
FLTRV3   CLI   0(R4),C'-'          NEGATIVE FILTER                              
         BNE   FLTRV5                                                           
         MVI   TEMP,1                                                           
         LA    R4,1(R4)                                                         
         BCTR  R1,0                                                             
FLTRV5   MVC   0(1,R6),0(R4)                                                    
         CLI   0(R6),C'*'                                                       
         BNE   FLTRV7                                                           
         CLI   TEMP,0                                                           
         BNE   FLTRVE                                                           
         B     FLTRV10                                                          
*                                                                               
FLTRV7   CLI   0(R6),C'A'                                                       
         BL    FLTRVE                                                           
         CLI   0(R6),C'9'                                                       
         BH    FLTRVE                                                           
         CLI   TEMP,1                                                           
         BNE   *+8                                                              
         NI    0(R6),X'BF'         NEGATIVE FILTER                              
*                                                                               
FLTRV10  LA    R6,1(R6)                                                         
         MVI   TEMP,0                                                           
         LA    R4,1(R4)                                                         
         BCTR  R1,0                                                             
         LTR   R1,R1               CHK FOR END OF INPUT                         
         BZ    FLTRVO                                                           
         BCT   R5,FLTRV3                                                        
         LTR   R1,R1               CHK FOR MORE INPUT                           
         BNZ   FLTRVE              INVALID                                      
*                                                                               
FLTRVO   LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(6,R7),TEMP+2                                                   
         OI    FIND,X'04'                                                       
         B     FLTRVX                                                           
*                                                                               
FLTRVE   MVC   FERN,=AL2(FLDINV)                                                
FLTRVX   B     EXITVAL                                                          
         EJECT                                                                  
COVRVAL  NTR1                     COST COLUMNS OVERRIDE FOR PP52                
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BNE   COVRVX                                                           
         CLI   IFLDH+5,2                                                        
         BNE   COVRVE                                                           
         LA    R4,IFLD                                                          
         LA    R6,TEMP                                                          
*                                                                               
COVRV5   MVC   0(1,R6),0(R4)                                                    
         CLI   0(R6),C'*'                                                       
         BNE   COVRV7                                                           
         B     COVRV10                                                          
*                                                                               
COVRV7   CLI   0(R6),C'9'      DETAIL COL 1-9 OR B-I                            
         BH    COVRVE                                                           
         CLI   0(R6),C'1'                                                       
         BNL   COVRV10                                                          
         CLI   0(R6),C'I'                                                       
         BH    COVRVE                                                           
         CLI   0(R6),C'B'                                                       
         BL    COVRVE                                                           
*                                                                               
COVRV10  LA    R6,1(R6)                                                         
         LA    R4,1(R4)                                                         
COVRV15  MVC   0(1,R6),0(R4)                                                    
         CLI   0(R6),C'*'                                                       
         BNE   COVRV17                                                          
         B     COVRVO              OK                                           
*                                                                               
COVRV17  CLI   0(R6),C'7'      SUMMARY COL 1-7 OR C-J                           
         BH    COVRVE                                                           
         CLI   0(R6),C'1'                                                       
         BNL   COVRVO              OK                                           
         CLI   0(R6),C'J'                                                       
         BH    COVRVE                                                           
         CLI   0(R6),C'C'                                                       
         BL    COVRVE                                                           
*                                                                               
*                                                                               
COVRVO   LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(2,R7),TEMP                                                     
         OI    FIND,X'04'                                                       
         B     COVRVX                                                           
*                                                                               
COVRVE   MVC   FERN,=AL2(FLDINV)                                                
COVRVX   B     EXITVAL                                                          
         EJECT                                                                  
         SPACE                                                                  
*                                                                               
*        VALIDATE USERP REC NUMBER                                              
*        X'04'=XXXX                                                             
*                                                                               
         PRINT NOGEN                                                            
USERPVAL NTR1                                                                   
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BNE   USRPW               NUM=ALL OR MISSING                           
         XC    KRT2,KRT2                                                        
         MVC   KRT2(2),RAGY                                                     
         MVC   KRT2+2(1),RMED                                                   
         CLI   RMED,C'*'                   ADD 01/06/88                         
         BNE   *+8                         ADD 01/06/88                         
         MVI   KRT2+2,C'M'      NEEDED TO READ MAG USERP                        
         CLI   RMED,C'C'                   ADD 03/31/89                         
         BNE   *+8                         ADD 03/31/89                         
         MVI   KRT2+2,C'M'      NEEDED TO READ MAG USERP                        
**                              00 PHASE NOW CHECKS KEY+2 FOR *                 
**                              TO SKIP RECORD READ                             
         MVI   KRT2+3,X'30'                                                     
         CLC   IFLD(2),=C'W='                                                   
         BNE   USRP5                                                            
         MVC   KRT2+4(4),IFLD+2                                                 
         B     USRP6                                                            
USRP5    MVC   KRT2+4(4),IFLD                                                   
USRP6    GOTO1 AREAD,PLIST,C'PRT2'                                              
         CLC   FERN,=AL2(FE)                                                    
         BL    USRPW               DISK ERROR                                   
         BH    *+14                                                             
         MVC   FERN,=AL2(MISREC)                                                
         B     USRPW                                                            
         OI    FIND,X'40'          JOB=XXXX                                     
         LA    R6,PRTREC+33                                                     
USRP10   CLI   0(R6),X'15'                                                      
         BE    USRP20                                                           
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),X'00'          IF NO X'15' ELEMENT FOUND CANNOT            
         BE    USRPW                BE WIDE SINCE WIDE WAS ADDED AFTER          
         B     USRP10               THESE STATUS ELEMENTS                       
*                                                                               
USRP20   CLC   3(2,R6),=X'0084'                                                 
         BNH   USRPW                                                            
         CLC   IFLD(2),=C'W='                                                   
         BE    USRPW                                                            
         LR    R2,R3                                                            
         USING T412FFD,R2                                                       
         XC    BVRHDR,BVRHDR                                                    
         MVC   BVRHDR(43),=C'** ERROR ** - REPORT REQUIRES WIDE PRINTINX        
               G'                                                               
         MVC   FERN,=AL2(FE)                                                    
*                                                                               
USRPW    LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         CLC   IFLD(2),=C'W='                                                   
         BNE   USRPW5                                                           
         MVC   0(4,R7),IFLD+2                                                   
         B     USRPX                                                            
USRPW5   MVC   0(4,R7),IFLD                                                     
USRPX    B     EXITVAL                                                          
         EJECT                                                                  
*                                                                               
*        VALIDATE REQUEST FOR 52/EC TO ACCOMPANY B1 BILLING                     
*        X'04' = 52, EC, OR B (=BOTH)                                           
*                                                                               
*****                                                                           
EC52VAL  NTR1                                                                   
         MVC   FERN,=AL2(FF)                                                    
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BNE   EC52X                                                            
*                                                                               
         CLC   =C'SOON',BVROUT      DISALLOW FOR SOON BILLING                   
         BE    EC52V                                                            
*                                                                               
         CLC   IFLD(2),=C'52'                                                   
         BNE   EC52C                                                            
         NI    RNAME+10,X'BF'       COL 79                                      
         OI    FIND,X'04'                                                       
         B     EC52X                                                            
EC52C    CLC   IFLD(2),=C'EC'                                                   
         BNE   EC52G                                                            
         NI    RNAME+09,X'BF'       COL 78                                      
         OI    FIND,X'04'                                                       
         B     EC52X                                                            
*                                                                               
EC52G    CLC   IFLD(2),=C'B '       BOTH 52 AND EC                              
         BNE   EC52V                                                            
         NI    RNAME+09,X'BF'       COL 78                                      
         NI    RNAME+10,X'BF'       COL 79                                      
         OI    FIND,X'04'                                                       
         B     EC52X                                                            
*                                                                               
EC52V    MVC   FERN,=AL2(FLDINV)                                                
EC52X    B     EXITVAL                                                          
*****                                                                           
         EJECT                                                                  
*                                                                               
*        VALIDATE PUBLISHER REP CODE AND SET FIND BITS                          
*        X'02' = ALL                                                            
*        X'04' = NNNN                                                           
*                                                                               
PPUBVAL  NTR1                                                                   
*                                                                               
**       NAME SEARCH CALL                                                       
**                                                                              
         L     R4,FLDHADR                                                       
         SR    R4,R3               GET DISPLACEMENT INTO TWA OF PUB             
         LA    R5,TEMP                                                          
         USING DSPARM,R5                                                        
         XC    DSPARM(DSPARML),DSPARM                                           
         MVC   DSMEDCOD,RMED                                                    
*                                                                               
         GOTO1 =V(SRCHCALL),PLIST,(3,(R4)),(X'80',(R3)),ACOMFACS,      X        
               ('DSPARML',TEMP),(1,=CL8'REP'),0,RR=RELO                         
         DROP  R5                                                               
*                                                                               
         GOTO1 AINITV                                                           
*                     RETURNS FIELD ADDR IN R4 AND LENGTH IN R5                 
         CLC   =C'ALL',IFLD                                                     
         BNE   PUBVAL0                                                          
         CHI   R5,3                                                             
         BNE   PUBVALE                                                          
         CLI   RPUB+3,C' '         RPUB BLANK?                                  
         BE    PUBVALM             YES                                          
PUBVALE  MVC   FERN,=AL2(FLDINV)   INVALID                                      
         B     PUBVX                                                            
*                                                                               
PUBVALM  DS    0H                                                               
         CLC   RNUM(2),=C'46'      FOR 46 AND 48 LEAVE NAME BLANK               
         BE    PUBVALMX                                                         
         CLC   RNUM(2),=C'48'                                                   
         BE    PUBVALMX                                                         
         MVC   NAME(20),=C'ALL PUBLISHER PUBS  '                                
*                                                                               
PUBVALMX OI    FIND,X'02'          ALL PUBLISHERS                               
         B     PUBVO                                                            
*                                                                               
PUBVAL0  FOUT  (R6),NAME                                                        
         CLI   FIND,1                                                           
         BNE   PUBVX               MISSING                                      
*                                                                               
PUBVAL1  TM    4(R4),X'08' NUMERIC                                              
         BO    PUBVAL1A                                                         
         MVC   FERN,=AL2(NUMINV)  NOT NUMERIC                                   
         B     PUBVX                                                            
*                                                                               
PUBVAL1A CHI   R5,4                                                             
         BNH   PUBVAL1B                                                         
         MVC   FERN,=AL2(FLDINV)    INVALID                                     
         B     PUBVX                                                            
*                                                                               
PUBVAL1B CLI   RPUB+3,C' '         RPUB BLANK?                                  
         BE    PUBVAL1C            YES                                          
         MVC   FERN,=AL2(FLDINV)    INVALID                                     
         B     PUBVX                                                            
*                                                                               
PUBVAL1C BCTR  R5,R0                                                            
         EX    R5,PUBPACK                                                       
         OI    DUB+7,X'0F'                                                      
         UNPK  RPUB(4),DUB+5(3)                                                 
         MVC   KRT1+4(4),RPUB                                                   
         MVI   KRT1+3,X'11'                                                     
         GOTO1 AREAD,PLIST,C'PRT1'                                              
         CLC   FERN,=AL2(FE)                                                    
         BL    PUBVX                                                            
         BH    PUBVAL1D                                                         
         MVC   FERN,=AL2(MISREC)                                                
         B     PUBVX                                                            
*                                                                               
PUBVAL1D LA    R7,PRTREC                                                        
         USING REPHDRD,R7                                                       
         TM    PREPSTAT,X'01'      PUBLISHER REP?                               
         BNZ   PUBVAL1E            YES                                          
         MVC   FERN,=AL2(FLDINV)    INVALID                                     
         B     PUBVX                                                            
         DROP  R7                                                               
*                                                                               
PUBVAL1E MVC   NAME(20),PRTREC+35                                               
         MVC   IFLD(4),RPUB                                                     
         OI    FIND,X'04'       REP=NNNN                                        
         B     PUBVO                                                            
*                                                                               
PUBPACK  PACK  DUB,IFLD(0)                                                      
*                                                                               
PUBVO    FOUT  (R6),NAME                                                        
         MVI   RPUB,C' '                                                        
         MVC   RPUB+1(2),=C'P='                                                 
         MVC   RPUB+3(4),IFLD                                                   
PUBVX    B     EXITVAL                                                          
         EJECT                                                                  
*                                                                               
*        VALIDATE START INVOICE # AND SET FIND BITS                             
*        X'04' = NNNN                                                           
*                                                                               
INVCVALS NTR1                                                                   
         GOTO1 AINITV                                                           
*                     RETURNS FIELD ADDR IN R4 AND LENGTH IN R5                 
         CLI   FIND,1                                                           
         BL    INVCS90            NO INPUT IS OK                                
*                                                                               
         TM    4(R4),X'08' NUMERIC                                              
         BO    INVCS10                                                          
         MVC   FERN,=AL2(NUMINV)  NOT NUMERIC                                   
         B     INVCS90                                                          
*                                                                               
INVCS10  CHI   R5,4                                                             
         BNH   INVCS20                                                          
         MVC   FERN,=AL2(FLDINV)    INVALID                                     
         B     INVCS90                                                          
*                                                                               
INVCS20  BCTR  R5,R0                                                            
         EX    R5,INVCSPAK                                                      
         OI    DUB+7,X'0F'                                                      
         UNPK  RPAY(4),DUB+5(3)                                                 
         MVC   IFLD(4),RPAY                                                     
         OI    FIND,X'04'          START INVOICE # = NNNN                       
         B     INVCS90                                                          
*                                                                               
INVCSPAK PACK  DUB,IFLD(0)                                                      
*                                                                               
INVCS90  B     EXITVAL                                                          
         EJECT                                                                  
*                                                                               
*        VALIDATE END INVOICE # AND SET FIND BITS                               
*        X'04' = NNNN                                                           
*                                                                               
INVCVALX NTR1                                                                   
         GOTO1 AINITV                                                           
*                     RETURNS FIELD ADDR IN R4 AND LENGTH IN R5                 
         CLI   FIND,1                                                           
         BL    INVCX90            NO INPUT IS OK                                
*                                                                               
         TM    4(R4),X'08' NUMERIC                                              
         BO    INVCX10                                                          
         MVC   FERN,=AL2(NUMINV)  NOT NUMERIC                                   
         B     INVCX90                                                          
*                                                                               
INVCX10  CHI   R5,4                                                             
         BNH   INVCX20                                                          
         MVC   FERN,=AL2(FLDINV)    INVALID                                     
         B     INVCX90                                                          
*                                                                               
INVCX20  BCTR  R5,R0                                                            
         EX    R5,INVCXPAK                                                      
         OI    DUB+7,X'0F'                                                      
         UNPK  RBILL(4),DUB+5(3)                                                
         MVC   IFLD(4),RBILL                                                    
         OI    FIND,X'04'          END INVOICE # = NNNN                         
         B     INVCX90                                                          
*                                                                               
INVCXPAK PACK  DUB,IFLD(0)                                                      
*                                                                               
INVCX90  B     EXITVAL                                                          
*                                                                               
*                                                                               
***********************************************************************         
*                                                                               
KEYSAVE  DS    CL32                                                             
DMWORK   DS    12D                                                              
WORK     DS    CL17                                                             
SVCONDT  DS    CL3                 CONTRACT DATE (BINARY YMD)                   
SVLCON   DS    H                   CONTRACT NUMBER (BINARY)                     
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*        THIS TABLE CONTAINS THE ADDRESSES OF THE VALIDATION ROUTINES           
*        CONTAINED IN THIS PHASE. INDEXED BY ROUTNUM                            
*                                                                               
ROUTADRT DC    F'0'                   00                                        
         DC    A(SORTVAL)             01 - SORT MENU                            
         DC    A(REPRTVAL)            02 - REPRT                                
         DC    A(DTEVAL)              03 - DATE                                 
         DC    A(REPVAL)              04 - REP                                  
         DC    A(ONEVAL)              05 - ONE CHARACTER                        
         DC    A(AMTVAL)              06 - MANUAL AMOUNT                        
         DC    A(MCDVAL)              07 - MANUAL C/D                           
         DC    A(COMVAL)              08 - COMMENT                              
         DC    A(CDATVAL)             09 - CHANGE CONTROL DATE                  
         DC    A(CONVAL)              10 - CONTRACT NUMBER                      
         DC    A(DAYDVAL)             11 - DAYS TILL DUE                        
         DC    A(BILLVAL)             12 - BILLING PERIOD                       
         DC    A(LSTVAL)              13 - LIST CODE                            
         DC    A(FLTRVAL)             14 - FILTER                               
         DC    A(COVRVAL)             15 - $ COLUMNS OVERRIDE                   
         DC    A(USERPVAL)            16 - USERP REC VALIDATION                 
*                                                                               
         DC    A(EC52VAL)             17 - 52/EC REQUESTED                      
         DC    A(PPUBVAL)             18 - PUBLISHER REP                        
         DC    A(INVCVALS)            19 - START INVOICE #                      
         DC    A(INVCVALX)            20 - END INVOICE #                        
*                                                                               
*                                                                               
DDSFMT   EQU   X'04'                                                            
MISFLD   EQU   1        FLD INPUT IS MISSING                                    
ONEINV   EQU   2                                                                
FLDINV   EQU   2                                                                
NUMINV   EQU   3        NOT NUMERIC                                             
MISREC   EQU   53       RECORD NOT FOUND                                        
DTEINV   EQU   20                                                               
PDNALL   EQU   166                                                              
ESNALL   EQU   167                                                              
NOGROSS  EQU   168                                                              
SEDSGE   EQU   80                                                               
SEDBIG   EQU   69                                                               
SEDINV   EQU   20                                                               
*                                                                               
* OTHER DESCRIPTIVE ERROR MSGS (RN=REQUEST NUMBER)                              
*                                                                               
RN01ERR2 DC    C'** ERROR ** - RECORD TYPE MUST BE "R"'                         
RN01ERR3 DC    C'** ERROR ** - RECORD TYPE MUST BE "C"'                         
RN01ERR4 DC    C'** ERROR ** - RECORD TYPE MUST BE "L"'                         
RN01ERR5 DC    C'** ERROR ** - CLIENT CODE FIELD IS REQUIRED'                   
*                                                                               
*                                                                               
*                                                                               
PCURSOR  DS    0H                  POSITION CURSOR TO TARGET FIELD              
*                                                                               
         LA    R0,24               SEARCH REQ MAP TABLE                         
         LA    R1,LREQMAP                                                       
CHKREQ1  CLI   0(R1),127                                                        
         BE    CHKREQ2                                                          
         CLC   ROUTNUM,0(R1)                                                    
         BE    CHKREQ3                                                          
         LA    R1,3(R1)                                                         
         BCT   R0,CHKREQ1                                                       
CHKREQ2  LA    R1,LREQMAP          NOT IN TBL POSN TO 1ST FLD                   
CHKREQ3  MVC   HALF,1(R1)                                                       
         ST    RE,FULL                                                          
         LR    RE,R3                                                            
         AH    RE,HALF                                                          
CHKREQ4  ST    RE,FADR             POSN CURSOR TO ROUTNUM FLD                   
         L     RE,FULL                                                          
         BR    RE                                                               
*                                                                               
*                                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
*        EACH ENTRY IN THE ONE CHR TABLE HAS THE FORMAT                         
*        XL1   ENTRY LEN                                                        
*        XL2   REQUEST NUM / REQUEST SUB                                        
*        XL1   COLUMN NUM IN REQUEST CARD                                       
*        XLN   VALUE LIST(S) - 1ST LIST TERMINATES WITH * - ANY EXTRA           
*              VALUES FOR DDS TERMS FOLLOW * AND FORM 2ND LIST.                 
*        AN ENTRY CAN BE FOLLOWED BY A SYNONYM ENTRY (REQUEST NUM=255)          
*        OF THE SAME LENGTH GIVING THE VALUES FOR THE REQ REC.                  
*                                                                               
*           ** NOTE ** ENTRY IN VALUES OF AL1(254) MEANS ACCEPT ANY             
*              ALPHA/NUMERIC                                                    
*                                                                               
*  NOTE: DON'T INSERT NEW ENTRY BEFORE ENTRY (REQUEST NUM=255)                  
*                                                                               
ONEVTBL  DS    0C                                                               
         DC    AL1(13,01,0,62),C'HPRCJLDEF'                                     
         DC    AL1(07,01,0,63),C'AGP'                                           
         DC    AL1(13,02,0,62),C'HPRCJLDEF'                                     
         DC    AL1(07,02,0,63),C'AGP'                                           
         DC    AL1(07,02,0,65),C' NY'                                           
         DC    AL1(06,02,0,66),C' Y'                                            
         DC    AL1(14,02,0,67),C' CJPEBRULA'                                    
         DC    AL1(07,02,0,68),C'*NY'                  DDS ONLY                 
         DC    AL1(07,255,0,0),C'  T'                                           
         DC    AL1(06,04,0,57),C' Y'                                            
         DC    AL1(06,255,0,0),C' P'                                            
         DC    AL1(06,06,0,57),C' Y'                                            
         DC    AL1(06,255,0,0),C' P'                                            
         DC    AL1(06,10,0,65),C'N',AL1(254)  DEFAULT IS N ACCEPT A/NUM         
         DC    AL1(06,10,0,66),C' S'        BILLING TYPE (S=SOON ONLY)          
         DC    AL1(09,10,0,67),C' AXCN'                                         
         DC    AL1(07,11,0,62),C' NY'                                           
         DC    AL1(06,12,0,53),C'NY'                                            
         DC    AL1(07,07,0,62),C'BMR'                                           
         DC    AL1(08,12,0,62),C' CSB'                                          
         DC    AL1(07,12,0,63),C' NX'                                           
         DC    AL1(06,12,0,64),C' R'                                            
         DC    AL1(06,12,0,65),C'NY'                                            
         DC    AL1(06,255,0,0),C' G'                                            
         DC    AL1(07,12,0,66),C' FC'                                           
         DC    AL1(09,12,0,67),C' 12AB'                                         
         DC    AL1(07,12,0,68),C' IL'                                           
* NO-OP  DC    AL1(07,12,0,68),C' PD'                                           
         DC    AL1(06,14,0,62),C'NY'                                            
         DC    AL1(06,14,0,63),C' Y'                                            
         DC    AL1(10,14,0,64),C' HLBOA'                                        
         DC    AL1(06,14,0,65),C' Y'                                            
         DC    AL1(06,16,0,63),C' Y'                                            
         DC    AL1(08,16,0,64),C'NYHL'                                          
         DC    AL1(06,18,0,62),C'NY'                                            
         DC    AL1(06,18,0,63),C' Y'                                            
         DC    AL1(08,18,0,64),C' HLB'                                          
         DC    AL1(06,18,0,65),C' Y'                                            
         DC    AL1(06,18,0,66),C'NY'                                            
         DC    AL1(06,18,0,142),C' H'          "SFH" OPTION                     
         DC    AL1(07,19,0,59),C' PD'                                           
         DC    AL1(06,19,0,60),C'NY'                                            
*NOP*    DC    AL1(06,19,0,61),C'NY'                                            
         DC    AL1(06,19,0,62),C'NY'                                            
         DC    AL1(06,19,0,63),C' Y'                                            
         DC    AL1(08,19,0,64),C' HLB'                                          
         DC    AL1(06,19,0,65),C' Y'                                            
         DC    AL1(06,19,0,66),C'NY'                                            
         DC    AL1(07,19,0,142),C' NY'                                          
         DC    AL1(06,19,0,143),C' H'          "SFH" OPTION                     
         DC    AL1(07,27,0,50),C' BP'                                           
         DC    AL1(07,27,0,62),C' PU'                                           
         DC    AL1(06,27,0,63),C'NY'                                            
         DC    AL1(07,27,0,64),C' CN'                                           
         DC    AL1(09,27,0,65),C' 12AB'                                         
         DC    AL1(06,27,0,66),C' Y'                                            
         DC    AL1(07,27,0,67),C' NY'                                           
         DC    AL1(07,28,0,50),C' BP'                                           
         DC    AL1(07,28,0,62),C' PU'                                           
         DC    AL1(06,28,0,63),C'NY'                                            
         DC    AL1(07,28,0,64),C' CN'                                           
         DC    AL1(07,36,0,50),C' BP'                                           
         DC    AL1(07,36,0,62),C' PU'                                           
         DC    AL1(07,36,0,64),C' CN'                                           
         DC    AL1(07,36,0,65),C' CP'                                           
         DC    AL1(06,36,0,66),C'YN'                                            
         DC    AL1(06,36,0,67),C' M'                                            
         DC    AL1(07,37,0,50),C' BP'                                           
         DC    AL1(07,37,0,62),C' PU'                                           
         DC    AL1(06,37,0,63),C'YN'                                            
         DC    AL1(07,37,0,64),C' CN'                                           
         DC    AL1(09,37,0,65),C' 12AB'                                         
         DC    AL1(06,37,0,66),C' Y'                                            
         DC    AL1(07,37,0,67),C' NY'                                           
         DC    AL1(07,41,0,62),C' YN'                                           
         DC    AL1(07,41,0,63),C' YN'                                           
         DC    AL1(08,41,0,64),C' YNI'                                          
         DC    AL1(07,41,0,65),C' YN'                                           
         DC    AL1(08,43,0,62),C' PXL'                                          
         DC    AL1(07,46,0,66),C' EF'                                           
*NOP*    DC    AL1(27,48,0,62),C'ABCDKLNPRSVWXZ1234$MYGU'                       
         DC    AL1(27,48,0,62),C'ABCDKLNPRSVWXZ1234$MYGI'  NOTE: U TO I         
         DC    AL1(06,48,0,64),C'NY'                                            
         DC    AL1(10,48,0,65),C' YBCLU'                                        
         DC    AL1(07,48,0,66),C' EF'                                           
         DC    AL1(06,48,0,68),C' ',AL1(254)  DEFAULT ' '-ACCEPT A/NUM          
         DC    AL1(06,49,0,58),C'NY'                                            
         DC    AL1(06,49,0,59),C' P'                                            
         DC    AL1(06,49,0,60),C' T'                                            
         DC    AL1(08,49,0,62),C' PSB'                                          
         DC    AL1(06,49,0,65),C' C'                                            
         DC    AL1(10,52,0,50),C' BP1CS'                                        
         DC    AL1(06,52,0,62),C'NY'                                            
         DC    AL1(06,255,0,0),C' S'                                            
         DC    AL1(07,52,0,63),C' BP'                                           
         DC    AL1(08,52,0,64),C' SPR'                                          
         DC    AL1(06,52,0,65),C' D'                                            
         DC    AL1(07,52,0,66),C' CT'                                           
         DC    AL1(06,52,0,142),C' H'          "SFH" OPTION                     
         DC    AL1(07,54,0,50),C' BP'                                           
         DC    AL1(06,54,0,62),C'GN'                                            
         DC    AL1(09,60,0,50),C' BPCS'                                         
         DC    AL1(11,60,0,62),C' PMDREB'                                       
         DC    AL1(07,60,0,63),C' SC'                                           
         DC    AL1(07,60,0,64),C' WD'                                           
         DC    AL1(06,60,0,65),C' S'                                            
         DC    AL1(10,60,0,66),C' XPJAB'                                        
         DC    AL1(06,66,0,62),C'NY'                                            
         DC    AL1(06,66,0,63),C'NY'                                            
         DC    AL1(06,66,0,64),C' S'                                            
         DC    AL1(13,66,0,65),C' SD123ABC'                                     
         DC    AL1(06,66,0,67),C'AF'                                            
         DC    AL1(09,66,0,68),C' 12AB'                                         
         DC    AL1(06,72,0,63),C' Y'                                            
         DC    AL1(12,77,0,50),C' DBPCIMS'                                      
         DC    AL1(08,77,0,62),C' ANO'                                          
         DC    AL1(07,77,0,63),C' $S'                                           
         DC    AL1(07,77,0,64),C' AN'                                           
         DC    AL1(08,77,0,65),C' N12'                                          
         DC    AL1(07,77,0,66),C' NY'                                           
         DC    AL1(08,77,0,67),C' NYS'                                          
         DC    AL1(08,77,0,142),C' NYF' MARKET OPTION              L02          
         DC    AL1(08,79,0,62),C'YN12'                                          
         DC    AL1(06,79,0,63),C'NY'                                            
         DC    AL1(07,79,0,64),C' AM'                                           
         DC    AL1(06,79,0,67),C'YN'                                            
         DC    AL1(06,81,0,62),C' D'                                            
         DC    AL1(10,91,0,62),C' N12*3'                                        
         DC    AL1(07,91,0,63),C' NY'                                           
         DC    AL1(07,91,0,64),C' NY'                  QOPT3 (TEMP USE)         
         DC    AL1(09,91,0,65),C' MN*P'                                         
         DC    AL1(07,91,0,66),C' NY'                                           
         DC    AL1(07,91,0,67),C' 57'                                           
         DC    AL1(08,92,0,65),C' N*Y'         Y ONLY FOR DDS                   
         DC    AL1(07,92,0,50),C' BP'                                           
         DC    AL1(15,94,0,62),C'BDILMNOSTVW'                                   
         DC    AL1(06,94,0,64),C'NY'                                            
         DC    AL1(06,94,0,65),C'NY'                                            
         DC    AL1(06,94,0,66),C'NY'                                            
         DC    AL1(06,94,0,68),C'NY'                                            
         DC    AL1(06,98,0,68),C'NY'                                            
         DC    AL1(06,255,0,0),C' D'           ALTERS Y TO D                    
         DC    AL1(06,100,0,62),C'NY'                                           
         DC    AL1(06,100,0,63),C'NY'                                           
         DC    AL1(07,100,0,64),C'NYM'                                          
         DC    AL1(06,104,0,62),C' B'                                           
         DC    AL1(10,105,0,63),C'NYT*OW'      EB   O+W DDS ONLY                
         DC    AL1(06,105,0,64),C' R'          EB                               
         DC    AL1(06,106,0,66),C'NY'                                           
         DC    AL1(10,107,0,63),C'NYT*OW'      EX   O+W DDS ONLY                
         DC    AL1(06,107,0,64),C' R'          EX                               
         DC    AL1(07,109,0,63),C'NYD'                                          
         DC    AL1(06,109,0,64),C'NY'                                           
         DC    AL1(06,109,0,65),C'NY'                                           
         DC    AL1(06,109,0,66),C'NY'                                           
         DC    AL1(12,109,0,50),C' BPCSMIA'    I=BILLED,A=PAID                  
         DC    AL1(08,110,0,63),C'4567'                                         
         DC    AL1(07,110,0,64),C' CN'                                          
         DC    AL1(07,110,0,62),C' CN'                                          
         DC    AL1(06,112,0,67),C'NY'                                           
         DC    AL1(11,113,0,65),C' GN12CT'                                      
         DC    AL1(06,113,0,66),C'NY'                                           
         DC    AL1(09,113,0,50),C' BPCS'                                        
         DC    AL1(07,113,0,64),C' WD'                                          
         DC    AL1(08,115,0,63),C'4567'          D1 LIKE B1                     
         DC    AL1(07,115,0,64),C' CN'                                          
         DC    AL1(07,115,0,62),C' CN'                                          
         DC    AL1(08,116,0,63),C'4567'          E1 LIKE B1                     
         DC    AL1(07,116,0,64),C' CN'                                          
         DC    AL1(07,116,0,62),C' CN'                                          
         DC    AL1(07,117,0,50),C' BP'                                          
         DC    AL1(06,117,0,62),C'GN'                                           
         DC    AL1(10,118,0,50),C' BP1CS'                                       
         DC    AL1(06,118,0,62),C'NY'                                           
         DC    AL1(06,255,0,0),C' S'                                            
         DC    AL1(07,118,0,63),C' BP'                                          
         DC    AL1(08,118,0,64),C' SPR'                                         
         DC    AL1(06,118,0,65),C' D'                                           
         DC    AL1(09,118,0,66),C' YT$B'                                        
         DC    AL1(06,118,0,142),C' H'          "SFH" OPTION                    
         DC    AL1(08,119,0,63),C'4567'          R1 LIKE B1                     
         DC    AL1(07,119,0,64),C' CN'                                          
         DC    AL1(07,119,0,62),C' CN'                                          
         DC    AL1(07,120,0,62),C' YN'                                          
         DC    AL1(07,120,0,63),C' YN'                                          
         DC    AL1(07,120,0,64),C' YN'                                          
         DC    AL1(06,120,0,65),C' C'                                           
         DC    AL1(08,121,0,50),C' BPI'                                         
         DC    AL1(06,121,0,63),C'NY'                                           
         DC    AL1(09,121,0,65),C' 12AB'                                        
         DC    AL1(06,121,0,66),C' Y'                                           
         DC    AL1(07,121,0,67),C' NY'                                          
         DC    AL1(09,121,0,68),C' RLFS'                                        
         DC    AL1(08,123,0,63),C'4567'          RD LIKE B1                     
         DC    AL1(07,123,0,64),C' CN'                                          
         DC    AL1(07,123,0,62),C' CN'                                          
         DC    AL1(07,124,0,64),C' RI'                                          
         DC    AL1(12,129,0,50),C' BPCSMIA'    I=BILLED,A=PAID                  
         DC    AL1(07,129,0,63),C'NYD'                                          
         DC    AL1(06,129,0,64),C'NY'                                           
         DC    AL1(06,129,0,65),C'NY'                                           
         DC    AL1(06,129,0,66),C'NY'                                           
         DC    AL1(07,130,0,50),C' BP'                                          
         DC    AL1(11,130,0,62),C' ANMRIX'   (M,R,I ONLY FOR WESTERN)           
         DC    AL1(06,130,0,63),C'NY'                                           
         DC    AL1(07,130,0,64),C' PU'                                          
         DC    AL1(07,130,0,65),C' BU'                                          
         DC    AL1(06,131,0,65),C'NY'        PRODUCE FILE - GT                  
         DC    AL1(06,132,0,62),C'IE'        DATA TYPE SN                       
         DC    AL1(06,132,0,67),C'NY'        TEST RUN SN                        
*******  DC    AL1(06,133,0,67),C'NY'        TEST RUN CI - NO-OPED              
         DC    AL1(06,134,0,65),C'NY'       PRODUCE TAPE  LT                    
         DC    AL1(06,135,0,67),C'NY'        TEST RUN SE                        
         DC    AL1(06,136,0,67),C'NY'        TEST RUN PH                        
         DC    AL1(06,137,0,65),C'NY'       PRODUCE TAPE  OR                    
         DC    AL1(06,138,0,67),C'NY'        TEST RUN TD                        
         DC    AL1(06,139,0,65),C'NY'       PRODUCE TAPE  JW                    
         DC    AL1(06,140,0,65),C'NY'       PRODUCE FILE - WB                   
         DC    AL1(06,141,0,65),C'NY'       PRODUCE FILE - IN                   
         DC    AL1(06,142,0,67),C'NY'       PRODUCE FILE - CH                   
         DC    AL1(06,143,0,67),C'NY'       PRODUCE FILE - PZ                   
         DC    AL1(06,144,0,67),C'NY'       PRODUCE FILE - AI                   
         DC    AL1(06,145,0,65),C'N',AL1(254) DEFLT N ACCEPT A/NUM -VL          
         DC    AL1(08,212,0,62),C' CSB'                                         
         DC    AL1(07,212,0,63),C' NX'                                          
         DC    AL1(06,212,0,64),C' R'                                           
         DC    AL1(06,212,0,65),C'NY'                                           
         DC    AL1(06,255,0,0),C' G'                                            
         DC    AL1(07,212,0,66),C' FC'                                          
         DC    AL1(09,212,0,67),C' 12AB'                                        
         DC    AL1(07,212,0,68),C' IL'                                          
* NO-OP  DC    AL1(07,212,0,68),C' PD'                                          
         DC    AL1(07,217,0,61),C'ALR'                                          
         DC    AL1(06,217,0,62),C'NY'                                           
         DC    AL1(06,217,0,63),C' Y'                                           
         DC    AL1(10,217,0,64),C' NYHLB'                                       
         DC    AL1(06,217,0,65),C' Y'                                           
         DC    AL1(07,219,0,59),C' PD'                                          
         DC    AL1(06,219,0,60),C'NY'                                           
         DC    AL1(06,219,0,62),C'NY'                                           
         DC    AL1(06,219,0,63),C' Y'                                           
         DC    AL1(08,219,0,64),C' HLB'                                         
         DC    AL1(06,219,0,65),C' Y'                                           
         DC    AL1(06,219,0,66),C'NY'                                           
         DC    AL1(07,219,0,142),C' NY'                                         
         DC    AL1(06,219,0,143),C' H'         "SFH" OPTION                     
         DC    AL1(07,228,0,62),C' NY'                                          
         DC    AL1(08,228,0,63),C' *NY'     DDS ONLY                            
         DC    AL1(07,228,0,64),C' NY'                                          
         DC    AL1(07,228,0,65),C' NY'                                          
         DC    AL1(09,228,0,66),C' NY*P'      (P IS DDS ONLY)                   
         DC    AL1(09,228,0,67),C' NY*P'     (P IS DDS ONLY)                    
         DC    AL1(06,229,0,65),C'NY'                                           
         DC    AL1(0)                                                           
         DC    X'0000'                                                          
         DS    0H                                                               
         EJECT                                                                  
       ++INCLUDE PRREQSAVE                                                      
       ++INCLUDE PRREQTEMP                                                      
         SPACE 2                                                                
       ++INCLUDE FLDIND                                                         
       ++INCLUDE PRREQFFD                                                       
*                                                                               
*INCLUDE PPSRCHPARM                                                             
       ++INCLUDE PPSRCHPARM                                                     
         EJECT                                                                  
       ++INCLUDE PPDDEQUS                                                       
         EJECT                                                                  
REPHDRD  DSECT                                                                  
       ++INCLUDE PREPREC                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'049PRREQ04   06/09/20'                                      
         END                                                                    
