*          DATA SET RESFM3BS   AT LEVEL 061 AS OF 09/11/02                      
*PHASE T8183BA,*                                                                
*&&      SET   TT=Y                                                             
*INCLUDE REPIO                                                                  
*INCLUDE LOADER                                                                 
*INCLUDE UNBOOK                                                                 
         TITLE 'T8183B - RESFM3B - ACTIVITY REPORT'                             
*                                                                               
**********************************************************************          
*                                                                    *          
*        RESFM3B (T8183B) --- ACTIVITY REPORT                        *          
*                                                                    *          
*  DEC30/96 (PXZ) - INCREASE TSAR TABLE FROM 12000 TO 15000          *          
*                                                                    *          
*  FEB26/97 (JRD) - RESTRUCTURING & NEW FEATURES                     *          
*                                                                    *          
*  JUL18/97 (JRD) - SKIP UNCODED CONTRACT TYPES WHEN FILTERING       *          
*                                                                    *          
*  MAR13/98 (PXZ) - BUG CATCHING (TSAR TBL INCREASED /RESET )        *          
*                                                                    *          
*  MAR19/98 (JRD) - LARGE MARKET BUDGETS                             *          
*  MAR30/98         FIX INCOMPLETES                                  *          
*                                                                    *          
*  OCT06/99 (BU ) - FIX OBSCURE LOSS BUG                             *          
*                                                                    *          
*  NOV08/99 (BU ) - MISSING AGY/ADV CODES:  KATZ CONVERSION          *          
*                                                                    *          
* ------------------------------------------------------------------ *          
**********************************************************************          
                                                                                
T8183B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**183B**,R7,RA,RR=RE                                           
         ST    RE,RELO                                                          
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         TM    TOTALS,TTLQONLY                                                  
         BO    MAIN02                                                           
*                                                                               
         LA    R1,HOOK             DEFAULT HOOK/SPECS                           
         ST    R1,HEADHOOK                                                      
         L     R1,=A(REGSPECS)                                                  
         A     R1,RELO                                                          
         ST    R1,SPECS                                                         
         B     MAIN04                                                           
*                                                                               
MAIN02   DS    0H                  RECAP HOOK/SPECS                             
         LA    R1,HOOK             JUST SKIPS MIDLINE                           
         ST    R1,HEADHOOK                                                      
         L     R1,=A(RECSPECS)                                                  
         A     R1,RELO                                                          
         ST    R1,SPECS                                                         
*                                                                               
MAIN04   DS    0H                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PREP                                                             
*                                                                               
         B    EXIT                                                              
****************************************************************                
NUMSTAS  EQU   12                                                               
NUMLVLS  EQU   5                                                                
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              PROCESS REPORT                                  *                
****************************************************************                
****************************************************************                
         SPACE 1                                                                
PREP     DS    0H                                                               
         LA    R5,REPIOTBL                                                      
         USING REPIOD,R5                                                        
         XC    MYCOUNT,MYCOUNT     FOR TESTING                                  
         MVI   RIPSKIP,C'Y'        DON'T NEED PREVIOUS $                        
*                                                                               
         GOTO1 =A(INIT),DMCB,SORTREC,RR=RELO                                    
*                                                                               
         BAS   RE,READRECS         READ CONTRACTS-PUT TO TSAR                   
*                                                                               
         BAS   RE,PRNTRECS         GET FROM TSAR-PRINT REPORT                   
*                                                                               
         BAS   RE,CLOSEUP          CLOSE IT UP                                  
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
******************************************************************              
* CLOSE UP SHOP                                                  *              
******************************************************************              
******************************************************************              
CLOSEUP  NTR1                                                                   
         L     R1,TSARBUFF                       RETURN MEMORY                  
         L     R2,TSARBUFL                                                      
         FREEMAIN RC,LV=(2),A=(1)                                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
******************************************************************              
* READ CONTRACT RECORDS                                          *              
******************************************************************              
******************************************************************              
READRECS NTR1                                                                   
                                                                                
GOREPIO  GOTO1 =V(REPIO),DMCB,REPIOTBL,0                                        
         LA    R5,REPIOTBL                                                      
         USING REPIOD,R5                                                        
         TM    RIPSTAT,RIPRQERR             ANY REQUIRED DATA MISSING           
         BZ    *+6                                                              
         DC    H'0'                SHOULD NEVER GET HERE                        
         XC    RIPFULL,RIPFULL     IGNORE MAX IO COUNT FOR THIS REPRT           
         TM    RIPSTAT,RIPENDF     EOF                                          
         BO    RDRX                                                             
         TM    RIPSTAT,RIPMAXIO    RESTART ON MAX IO?                           
         BZ    *+12                                                             
         NI    RIPSTAT,X'FF'-RIPMAXIO                                           
         B     GOREPIO                                                          
*                                                                               
* CONTRACT FILTERING                                                            
*                                                                               
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
*                                                                               
**       MVC   P+01(05),=C'READ:'                                               
**       MVC   P+07(27),RCONREC                                                 
**       MVC   P+36(27),KEY                                                     
**       BAS   RE,SPLAT                                                         
*                                                                               
                                                                                
         CLC   =C'ACC-',RCONBUYR   SKIP ACC CONTRACTS                           
         BE    GOREPIO                                                          
                                                                                
         L     R1,=A(OFFLIST)      REGION FILTERING ?                           
         A     R1,RELO                                                          
         CLI   0(R1),0                                                          
         BE    RDR2A               NO                                           
RDR2     CLC   RCONKOFF,0(R1)      YES / IS OFFICE VALID FOR REGION?            
         BE    RDR2A                                                            
         LA    R1,2(R1)            BUMP TO NEXT OFFICE IN LIST                  
         CLI   0(R1),0             EOF                                          
         BE    GOREPIO                                                          
         B     RDR2                                                             
                                                                                
RDR2A    TM    RCONMODR,X'08'      WAS/IS TAKEOVER                              
         BO    GOREPIO             EXCLUDE WAS/IS TAKEOVER                      
                                                                                
         CLI   CONTYPES,0          CONTRACT TYPE FILTERING ?                    
         BE    RDR3                NO                                           
                                                                                
         CLI   RCONTYPE,0          UNCODED?                                     
         BE    GOREPIO             YES - SKIP PER RRG/NRGON                     
                                                                                
         LA    RE,CONTYPES                                                      
         LA    RF,255(RE)                                                       
RDR2B    DS    0H                                                               
         CLI   0(RE),0             TYPE NOT FOUND                               
         BE    RDR2C                                                            
         CLC   RCONTYPE,0(RE)                                                   
         BE    RDR2D               TYPE FOUND                                   
         LA    RE,1(RE)                                                         
         CR    RE,RF                                                            
         BL    RDR2B                                                            
*                                                                               
RDR2C    TM    REPORT,RPTQNCT      NEGATIVE FILTER?                             
         BNO   GOREPIO             NO                                           
         B     RDR3                                                             
*                                                                               
RDR2D    TM    REPORT,RPTQNCT      NEGATIVE FILTER?                             
         BO    GOREPIO             YES                                          
*                                                                               
RDR3     MVI   ELCODE,X'12'        EXCLUDE FORECAST CONTRACTS                   
         BAS   RE,GETEL                                                         
         BNE   RDR5                                                             
         USING RSARXCO,R6                                                       
         TM    RSARXFLG,X'08'      IF ITS FORECAST                              
         BO    GOREPIO             GO GET NEXT CONTRACT                         
                                                                                
RDR5     L     R6,AIO                                                           
         USING RCONREC,R6                                                       
                                                                                
RDR10    EQU   *                                                                
* CHECK CONTRACT REC TO ASSIGN REPORT STATUS                                    
*                                                                               
* 1 = PENDING    = NO BUCKETS AND NO SPL ELEMENT                                
* 2 = INCOMPLETE = BUCKETS BUT NO SPL OR SPL FOR 1ST STATION ONLY               
* 3 = COMPLETED  = BUCKETS AND SPL FOR ALL STATIONS                             
* 4 = LOSSES     = 0$ FOR 1ST STATION, SPL FOR OTHER STATIONS                   
*                                                                               
         MVI   BYTE,0              USE BYTE AS CONDITION INDICATOR              
         L     R6,AIO                                                           
         MVI   ELCODE,X'03'        IS THERE BUCKETS ELEMENT                     
         BAS   RE,GETEL                                                         
         BNE   *+8                 NO                                           
         OI    BYTE,X'01'          YES/MARK X'01'=BUCKET ELEMENT                
         L     R6,AIO                                                           
         MVI   ELCODE,X'06'        IS THERE SPL ELEMENT                         
         BAS   RE,GETEL                                                         
         BNE   *+8                                                              
         OI    BYTE,X'02'          X'02'=SPL ELEMENT                            
         B     RDR12                                                            
                                                                                
* CHECK BYTE STATUE                                                             
RDR12    EQU   *                                                                
         CLI   BYTE,0              ..BYTE=0 = NO SPL/NO BUCKETS                 
         BNE   RDR12B                                                           
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
         TM    RCONMODR,X'10'      IF BUY LINE ADDED                            
         BO    RDR80               IT'S INCOMPLETE                              
         B     RDR20               ELSE PENDING                                 
                                                                                
RDR12B   CLI   BYTE,X'01'          BUCKETS ONLY?                                
         BE    RDR80               YES- SO ITS INCOMPLETE                       
*                                                                               
                                                                                
         L     R6,AIO              BUCKETS AND SPL                              
         MVI   ELCODE,X'06'        CHK IF SPL FOR ALL STATIONS                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONSPEL,R6                                                      
         ZIC   R1,RCONSPNU         NUMBER OF MINI ELEMENTS                      
         LA    R2,RCONSPST         IS THERE SPL FOR 1ST STATION?                
         OC    5(4,R2),5(R2)                                                    
         BZ    *+8                                                              
         OI    BYTE,4              BYTE =X'04' = SPL FOR 1ST STATION            
         LA    R2,9(R2)            BUMP TO NEXT STATION/AMOUNT                  
         B     RDR13B              AND FOR OTHER STATIONS ?                     
RDR13    OC    5(4,R2),5(R2)       SPL AMOUNT?                                  
         BZ    *+8                 NO                                           
         OI    BYTE,8              BYTE =X'08' = SPL FOR OTHERS                 
         LA    R2,9(R2)            BUMP TO NEXT STATION/AMOUNT                  
RDR13B   BCT   R1,RDR13                                                         
         TM    BYTE,X'0F'          BUCKETS AND SPL FOR 1ST AND OTHERS ?         
         BNO   RDR14                                                            
         MVI   BYTE,3              BUCKETS AND SPL FOR ALL - COMPLETED          
         B     RDR40                                                            
                                                                                
* CHECK IF INCOMPLETE OR LOSSES                                                 
RDR14    EQU   *                                                                
         TM    BYTE,X'04'          IS THERE SPL FOR 1ST STATION ONLY            
*****    BO    RDR80               YES/INCOMPLETE ****                          
         BNO   RDR14B                                                           
         MVI   BYTE,3              CHANGED PER PATRICE - COMPLETED              
         B     RDR40                                                            
                                                                                
RDR14B   TM    BYTE,X'08'          IS IT SPL FOR OTHERS ONLY?                   
         BO    RDR15               YES/LOSSES                                   
         MVI   BYTE,3              ,,SPL BUT NO $ FOR ANYONE                    
         B     RDR40               ,,COMPLETED                                  
RDR15    MVI   BYTE,4                 SET LOSSES ID                             
         B     RDR40                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
* - FILL IN PENDING RECORD X'01'                                                
RDR20    EQU   *                                                                
         TM    REPORT,RPTQPND      DO PENDING ?                                 
         BNO   GOREPIO                                                          
                                                                                
         BAS   R5,CHKXCTYP         EXCLUDE CON TYPES                            
         BE    GOREPIO                                                          
                                                                                
         L     R6,AIO              YES - PENDING MUST HAVE SAR                  
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         BNE   GOREPIO                                                          
                                                                                
         LA    RE,SRTREC          CLEAR RECORD AREA                             
         LA    RF,SRTLENE                                                       
         XCEF                                                                   
                                                                                
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
                                                                                
         MVI   PNDKID,X'01'        PENDING ID                                   
         BAS   RE,FILLCMN          FILL SORTREC WITH COMMON DATA                
         MVC   PNDKCON,PNDCON      SET CONTRACT NUMBER TO KEY                   
                                                                                
* - NEED BOP COMMENT                                                            
         MVI   PNDCOMMT,C'N'                                                    
         L     R6,AIO                                                           
         USING RCONBCEL,R6                                                      
         MVI   ELCODE,X'11'        BOP COMMENT ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   RDR30                                                            
         MVC   PNDCOMMT(3),=C'BOP'             BOP COMMENT                      
         LA    R5,REPIOTBL                                                      
         USING REPIOD,R5                                                        
         MVC   PNDCOMMT+3(27),RIPKEY                                            
         B     RDR30                                                            
         DROP  R5                                                               
                                                                                
* - PENDING DONE - PASS REC TO TSAR                                             
RDR30    EQU   *                                                                
         B     RDR100                                                           
                                                                                
         EJECT                                                                  
* COMPLETED BUSINESS / LOSSES                                                   
RDR40    EQU   *                                                                
         CLI   BYTE,3              3=COMPLETED                                  
         BNE   RDR42                                                            
         TM    REPORT,RPTQCPL                                                   
         BNO   GOREPIO                                                          
*                                                                               
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
         CLI   RCONTYPE,C'X'       X AND N ONLY INCLUDED IF IN TABLE            
         BE    *+12                                                             
         CLI   RCONTYPE,C'N'                                                    
         BNE   RDR44                                                            
         BAS   R5,CHKICTYP                                                      
         BE    RDR44                                                            
         DROP  R6                                                               
*                                                                               
RDR42    CLI   BYTE,4              4=LOSSES                                     
         BNE   GOREPIO                                                          
         TM    REPORT,RPTQLOS                                                   
         BNO   GOREPIO                                                          
         BAS   R5,CHKXCTYP                                                      
         BE    GOREPIO             EXCLUDE IF IN TABLE                          
                                                                                
* - NEED SPL ELEMENT TO CHECK IF SPL DATE IS WITHIN THIS WEEK                   
* - THIS WEEK = FROM RUN DATE TO PREVIOUS MONDAY                                
RDR44    L     R6,AIO                                                           
         USING RCONSPEL,R6                                                      
         MVI   ELCODE,X'06'        SPL COMMENT ELEMENT                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                SHOULD NEVER GET HERE                        
                                                                                
         MVC   WORK(3),BTODAY                                                   
         MVC   WORK+3(3),MONDAYDT                                               
         CLI   ACTSTR,0           OVERRIDE DEFAULT ACTV DATES ?                 
         BE    RDR45                                                            
         MVC   WORK(3),ACTEND     YES                                           
         MVC   WORK+3(3),ACTSTR                                                 
                                                                                
RDR45    CLC   RCONSPDT,WORK       SPL DATE MUST BE WITHIN THESE DATES          
         BH    GOREPIO             HIGH/SKIP                                    
         CLC   RCONSPDT,WORK+3                                                  
         BL    GOREPIO             LOW/SKIP                                     
                                                                                
         LA    RE,SRTREC          CLEAR RECORD AREA                             
         LA    RF,SRTLENE                                                       
         XCEF                                                                   
                                                                                
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
                                                                                
         MVC   PNDKID,BYTE         COMPLETED = X'03'/ LOSSES=X'04'              
         BAS   RE,FILLCMN          FILL SORTREC WITH COMMON DATA                
         MVC   PNDKCON,PNDCON      SET CONTRACT NUMBER TO KEY                   
*                                                                               
* - NEED SPL COMMENT                                                            
         MVI   PNDCOMMT,C'N'                                                    
         L     R6,AIO                                                           
         USING RCONSMEL,R6                                                      
         MVI   ELCODE,X'07'        SPL COMMENT ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   RDR50                                                            
         MVC   PNDCOMMT(3),=C'SPL'             SPL COMMENT                      
         LA    R5,REPIOTBL                                                      
         USING REPIOD,R5                                                        
         MVC   PNDCOMMT+3(27),RIPKEY           DISK ADDRESS                     
         DROP  R5                                                               
         B     RDR50                                                            
                                                                                
RDR50    EQU   *                                                                
* GET CONTRACT $ INTO FULL                                                      
* IF IT'S LOSS, $ FROM X'08' ELEM - IF IT'S COMPLETE, $ FROM BUCKETS            
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
*                                                                               
         CLI   PNDKID,4            IS IT LOSS?                                  
         BNE   RDR51                                                            
         MVI   ELCODE,8            YES/GET X'08' ELEM FOR BUDGET $              
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONACEL,R6                                                      
         MVC   FULL,RCONAC$$       TOTAL BUDGET $                               
         B     RDR54B                                                           
*                                                                               
         USING RCONREC,R6                                                       
RDR51    LA    R4,RCONELEM         COMPLETED- GET $ FROM BUCKETS                
         XC    FULL,FULL                                                        
RDR52    SR    R6,R6                                                            
         IC    R6,1(R4)                                                         
         AR    R4,R6                                                            
         CLI   0(R4),3                                                          
         BE    RDR54                                                            
         CLI   0(R4),0                                                          
         BE    RDR54B                                                           
         B     RDR52                                                            
*                                                                               
         USING RCONBKEL,R4                                                      
RDR54    SR    R1,R1                                                            
         MVC   DUB(4),RCONBKAM                                                  
         L     R1,DUB                                                           
         A     R1,FULL                                                          
         ST    R1,FULL                                                          
         B     RDR52                                                            
*                                                                               
RDR54B   EQU   *                                                                
* - FULL HAS $ AMOUNT                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'06'        GET SPL STATION DATA                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONSPEL,R6                                                      
         ZIC   R3,RCONSPNU         NUMBER OF STATIONS                           
         LTR   R3,R3                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         C     R3,=F'12'                                                        
         BNH   *+8                                                              
         LA    R3,12               12 MAX NUMBER OF STATIONS                    
         LA    R5,12                                                            
         LA    R2,RCONSPST         POINT R2 TO STATION/AMOUNT FIELD             
         LA    R6,COMSCALL         POINT R6 TO SORT REC STATION                 
         MVI   BYTE,0                                                           
         CLI   PNDKID,4            IF IT'S A LOSS                               
         BE    RDR54C              TOTAL $ AMT SET FROM 08 ELEM                 
         GOTO1 =A(CALCAMT),RR=RELO                                              
RDR54C   DS    0H                                                               
         BAS   RE,COMPSTAT         SET STATION+COMPETITIVES TO SRTREC           
RDR55    DS    0H                                                               
         CLC   0(5,R6),0(R2)      MATCH STATION CALL LETTERS ?                  
         BE    RDR60                                                            
         LA    R6,COMLENE(R6)      BUMP SORTREC                                 
         BCT   R5,RDR55                                                         
         B     RDR67               COMPETITIVE STATION NOT IN                   
*                                     X'06' COMPETITIVE ELEMENT                 
*   NOTE:  IT IS NOT NECESSARY THAT THE COMPETITIVE STATION                     
*        IN THE STATION RECORD'S X'02' ELEMENTS EXIST IN THE                    
*        X'06' OF THE CONTRACT RECORD.  THIS SITUATION ARISES                   
*        WHEN THE SPL FOR A STATION'S ORDER IS ENTERED, AND,                    
*        SOMETIME LATER, A NEW COMPETITIVE STATION IS ADDED TO                  
*        THE STATION RECORD'S X'02' ELEMENTS.                                   
*        THIS CONDITION SHOULD BE TREATED AS IF THE ABSENT INFO                 
*        HAS A VALUE OF ZERO.                                                   
*                                                                               
***      DC    H'0'                SHOULD ALWAYS BE A MATCH !                   
*        MVC   P+2(42),=C'*** COMPETITIVE STATION ERROR - CONTRACT #'           
*        MVC   P+44(8),PNDCON                                                   
*        BAS   RE,SPLAT                                                         
*        LA    R5,REPIOTBL                                                      
*        USING REPIOD,R5                                                        
*        OI    RIPSTAT,RIPRDHI                                                  
*        B     GOREPIO                                                          
*        DROP  R5                                                               
                                                                                
RDR60    DS    0H                                                               
         ICM   R4,15,5(R2)         PERCENT SHARE TO R4                          
         LTR   R4,R4               ..IF 0%                                      
         BZ    RDR65               ..SKIP $ CALC                                
         MVC   8(4,R6),5(R2)       SET SHARE TO SORT REC                        
                                                                                
         L     R1,FULL             FULL HAS TOTAL $ AMOUNT                      
         CVD   R1,DUB                                                           
         ZAP   WORK(16),DUB         PACKED $ INTO WORK                          
                                                                                
         CVD   R4,DUB               PACKED % INTO DUB                           
                                                                                
         MP    WORK(16),DUB+4(4)    % ONLY USES LAST 4 OF DUB                   
         AP    WORK(16),=P'5000'      ROUND UP                                  
         DP    WORK(16),=P'10000'                                               
         ZAP   12(L'COMSAMT,R6),WORK(13)                                        
                                                                                
RDR65    LA    R2,9(R2)            BUMP R2 TO NEXT STATION & % SHARE            
RDR67    EQU   *                                                                
         LA    R6,COMSCALL         RESET TO START OF SORT AREA                  
         LA    R5,12               RESET R3 TO MAX NO OF STATIONS               
         BCT   R3,RDR55                                                         
         B     RDR70                                                            
                                                                                
RDR70    EQU   *                                                                
         B     RDR100              PASS REC TO TSAR                             
         EJECT                                                                  
**********************************************************************          
* INCOMPLETE                                                                    
**********************************************************************          
RDR80    DS    0H                                                               
         TM    REPORT,RPTQINC                                                   
         BNO   GOREPIO                                                          
                                                                                
         BAS   R5,CHKXCTYP         EXCLUDE IF IN TABLE                          
         BE    GOREPIO                                                          
                                                                                
         LA    RE,SRTREC          CLEAR RECORD AREA                             
         LA    RF,SRTLENE                                                       
         XCEF                                                                   
                                                                                
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
                                                                                
         MVI   PNDKID,X'02'        INCOMPLETE                                   
         BAS   RE,FILLCMN          FILL SORTREC WITH COMMON DATA                
         MVC   PNDKCON,PNDCON      SET CONTRACT NUMBER TO KEY                   
*                                                                               
* - NEED SPL COMMENT                                                            
         MVI   PNDCOMMT,C'N'                                                    
         L     R6,AIO                                                           
         MVI   ELCODE,X'07'        SPL COMMENT ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   RDR81                                                            
         MVC   PNDCOMMT(3),=C'SPL'             SPL COMMENT                      
         LA    R5,REPIOTBL                                                      
         USING REPIOD,R5                                                        
         MVC   PNDCOMMT+3(27),RIPKEY           DISK ADDRESS                     
         DROP  R5                                                               
                                                                                
* GET CONTRACT $ INTO FULL                                                      
RDR81    L     R6,AIO                                                           
         USING RCONREC,R6                                                       
*                                                                               
         LA    R4,RCONELEM                                                      
         XC    FULL,FULL                                                        
RDR82    SR    R6,R6                                                            
         IC    R6,1(R4)                                                         
         AR    R4,R6                                                            
         CLI   0(R4),3                                                          
         BE    RDR84                                                            
         CLI   0(R4),0                                                          
         BE    RDR86                                                            
         B     RDR82                                                            
*                                                                               
         USING RCONBKEL,R4                                                      
RDR84    SR    R1,R1                                                            
         MVC   DUB(4),RCONBKAM                                                  
         L     R1,DUB                                                           
         A     R1,FULL                                                          
         ST    R1,FULL                                                          
         B     RDR82                                                            
         DROP  R4                                                               
*                                                                               
RDR86    EQU   *                                                                
* - FULL HAS $ AMOUNT - GET RID OF PENNIES                                      
         L     R1,FULL                                                          
         SR    R0,R0                                                            
         D     R0,=F'100'          GO PENNIES                                   
         ST    R1,FULL                                                          
*                                                                               
         LA    R2,COMSCALL         SORT REC STATION CALL LETTERS                
         BAS   RE,GETSTATS         GET COMPETITIVE STATIONS/AFFIL               
         MVI   ELCODE,0            CLEAR ELCODE                                 
                                                                                
RDR87    MVC   0(5,R2),WORK       STATION CALL LETTERS                          
         MVC   5(3,R2),WORK+5     AFFILIATION                                   
         L     R1,FULL                                                          
         CVD   R1,DUB                                                           
         ZAP   12(L'COMSAMT,R2),DUB                                             
         XC    FULL,FULL           ALL $ GO TO 1ST STATION                      
         LA    R2,COMLENE(R2)      BUMP SORTREC                                 
         CLI   ELCODE,2            ARE WE IN ELEMENT ALREADY                    
         BE    RDR88               YES                                          
*                                                                               
         L     R6,AIO2             NO-POINT TO STATION REC                      
         MVI   ELCODE,X'02'                                                     
         USING RSTAMKEL,R6                                                      
         BAS   RE,GETEL                                                         
         BNE   RDR90                                                            
         B     *+8                                                              
RDR88    BAS   RE,NEXTEL                                                        
         BNE   RDR90                                                            
         MVC   WORK(5),RSTAMKST                                                 
         MVC   WORK+5(3),RSTAMKAF                                               
         B     RDR87                                                            
         DROP  R6                                                               
*                                                                               
RDR90    EQU   *                                                                
         B     RDR100              PASS REC TO TSAR                             
         EJECT                                                                  
**********************************************************************          
* ADD RECORD TO TSAROFF                                                         
**********************************************************************          
RDR100   DS    0H                                                               
         LA    R2,TSAREA                                                        
         USING TSARD,R2                                                         
         LA    RE,MYTSREC                                                       
         LA    RF,SRTLENE                                                       
         XCEF                                                                   
         LA    RF,MYTSREC                                                       
         LA    R1,SRTLENE                                                       
         LA    RE,SRTREC                                                        
         MOVE  ((RF),(R1)),(RE)                                                 
                                                                                
         MVI   TSOFFACT,TSAADD                                                  
         LA    RE,MYTSREC                                                       
         ST    RE,TSAREC                                                        
*                                                                               
         L     R1,MYCOUNT                                                       
         LA    R1,1(R1)                                                         
         ST    R1,MYCOUNT                                                       
*                                                                               
         GOTO1 ATSAROFF,(R2)                                                    
*                                                                               
**       MVC   P+01(05),=C'TSAR:'                                               
**       MVC   P+06(1),TSERRS                                                   
**       MVC   P+07(4),MYCOUNT                                                  
**       MVI   P+11,C'/'                                                        
**       MVC   P+12(50),SORTREC                                                 
**       MVI   P+62,C'/'                                                        
**       MVC   P+63(64),TSAREA                                                  
**       BAS   RE,SPLAT                                                         
*                                                                               
         CLI   TSERRS,0                                                         
         BE    RDR0120                                                          
         TM    TSERRS,X'20'        DUPLICATE KEY?                               
         BO    RDR0120             YES - SKIP IT                                
         DC    H'0'                                                             
RDR0120  EQU   *                                                                
         LA    R5,REPIOTBL                                                      
         USING REPIOD,R5                                                        
         OI    RIPSTAT,RIPRDHI                                                  
         B     GOREPIO             GET NEXT RECORD                              
*                                                                               
RDRX     B     EXIT                                                             
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* EXCLUDE ANY CONTYPES THAT MATCH                                               
***********************************************************************         
CHKXCTYP DS    0H                                                               
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
         CLI   RCONTYPE,X'40'      IS THERE A CONTRACT TYPE ?                   
         BNH   XCT20               NO/ SO NO MATCH                              
         CLI   RCONTYPE,C'X'        ALWAYS EXCLUDE X AND N                      
         BER   R5                                                               
         CLI   RCONTYPE,C'N'                                                    
         BER   R5                                                               
CHKICTYP L     R6,AIO            COMPLETED ENTER HERE INCLUDE MATCHES           
         LA    R1,CONTYPEX                                                      
         LA    RE,50                                                            
XCT10    CLC   RCONTYPE,0(R1)                                                   
         BER   R5                                                               
         LA    R1,1(R1)                                                         
         BCT   RE,XCT10                                                         
XCT20    LTR   R6,R6                                                            
         BR    R5                                                               
         DROP  R6                                                               
**********************************************************************          
* READ STATION RECORD AND RETURNS STATION/AFFILIATION IN WORK                   
**********************************************************************          
GETSTATS NTR1                                                                   
         L     R6,AIO              NO/ GET CONTRACT STATION REC                 
         USING RCONREC,R6                                                       
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING RSTAREC,R5                                                       
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,RCONKREP                                                
         MVC   RSTAKSTA,RCONKSTA   STATION CALL LETTERS                         
         GOTO1 HIGH                                                             
         CLC   KEY(26),KEYSAVE     NOT CHECKING LAST POSITION(A,F,ETC)          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO2                                                         
         L     R5,AIO                                                           
         GOTO1 GETREC                                                           
         MVC   WORK(5),RSTAKSTA          STATION                                
         MVC   WORK+5(3),RSTAAFFL          STATION AFFILIATION                  
         MVC   AIO,AIO1                                                         
         L     R6,AIO                                                           
         B     EXIT                                                             
         DROP  R5,R6                                                            
         EJECT                                                                  
**********************************************************************          
*  FILL IN FIELDS COMMON TO ALL REPORT TYPES                                    
**********************************************************************          
FILLCMN  NTR1                                                                   
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
                                                                                
         UNPK  PNDCON(9),RCONKCON(5)  CONTRACT NUMBER (PWO)                     
         MVI   PNDCON+8,0             CLEAR SIGN BYTE                           
* GET RID OF LEADING ZEROS                                                      
         XC    WORK,WORK                                                        
         LA    R1,8                                                             
         LA    RE,PNDCON                                                        
FIL02    CLI   0(RE),C'0'                                                       
         BNE   FIL03                                                            
         LA    RE,1(RE)                                                         
         BCT   R1,FIL02                                                         
         DC    H'0'                CONTRACT # CAN'T BE ALL ZEROS                
FIL03    BCTR  R1,0                                                             
         EX    R1,*+4                                                           
**JRD    B     *+10                                                             
         MVC   WORK(0),0(RE)                                                    
         MVC   PNDCON,WORK                                                      
*                                                                               
         TM    REPORT,RPTQSTA         STATION SORT ?                            
         BO    FIL05                                                            
* SALESPERSON SORT                                                              
         MVC   PNDKOFF,RCONKOFF       OFFICE       * (DATA FOR KEY)             
         MVC   PNDKSTAT,RCONKSTA      STATION      *                            
         MVC   PNDKTEAM,RCONTEM       TEAM         *                            
         MVC   PNDKSLS,RCONSAL        SALESMAN     *                            
         BAS   RE,GETSLSNM                                                      
         MVC   PNDSLSNM,WORK          SALESMAN NAME *                           
         B     FIL09                                                            
* STATION SORT                                                                  
FIL05    MVC   PNDKOFF2,RCONKOFF      OFFICE      *                             
         MVC   PNDKSTA,RCONKSTA       STATION     *                             
         MVC   PNDKSLS2,RCONSAL        SALESMAN     *                           
         BAS   RE,GETSLSNM            SALSPERSON NAME                           
         MVC   PNDSLSNM(3),RCONSAL          SALESMAN ID                         
         MVC   PNDSLSNM+4(16),WORK          SALESMAN NAME                       
         B     FIL09                                                            
*                                                                               
FIL09    DS    0H                                                               
         BAS   RE,GETMRKT                                                       
         MVC   PNDMRKT,WORK        MARKET NAME *                                
         MVC   PNDWEEKS,RCONWKS    WEEKS IN FLIGHT                              
         MVC   PNDCNTY,RCONTYPE    CONTRACT TYPE                                
*                                                                               
         TM    REPORT,RPTQPRO      COUNT PROPOSALS?                             
         BZ    FIL09A                                                           
         GOTO1 =A(COUNTPRO),RR=RELO                                             
         MVC   PNDPROS,BYTE        NUMBER OF PROPOSALS                          
FIL09A   DS    0H                                                               
*                                                                               
         CLI   PNDKID,1               ,,IF ITS PENDING                          
         BNE   FIL10                  ,,NO                                      
*                                     ,,YES - NEED DAYS TO START                
         GOTO1 DATCON,DMCB,(3,RCONDATE),(0,DUB)                                 
         GOTO1 DATCON,DMCB,(5,BTODAY),(0,WORK)                                  
         GOTO1 PERVERT,DMCB,DUB,WORK                                            
         SR    R3,R3                                                            
         LH    R3,DMCB+8                                                        
         STCM  (R3),15,PNDKDST                                                  
                                                                                
* PNDCREAT GETS DIFFERENT DATES ACCORING TO REPORT TYPE                         
FIL10    MVC   PNDCREAT,RCONHDRD      HEADER CREATION DATE                      
         XC    PNDACTIV,PNDACTIV                                                
*                                                                               
         CLI   PNDKID,1               IS IT PENDING                             
         BNE   FIL11                  NOS                                       
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'12'           GET SAR ELEMENT                           
         USING RSARXEL,R6                                                       
         BAS   RE,GETEL                                                         
         BNE   *+18                   NOT FOUND                                 
         CLI   1(R6),RSARXLTH                                                   
         BNE   *+10                   OLD ELEMENT                               
         MVC   PNDACTIV,RSARXLAD      LAST PENDING ACTIVITY DATE                
         B     FIL18                                                            
*                                                                               
         USING RCONREC,R6                                                       
FIL11    CLI   PNDKID,2               IS IT INCOMPLETE                          
         BNE   FIL15                  NO                                        
*                                                                               
         MVC   PNDACTIV,RCONCREA      HEADER OR 1ST BUYLINE DATE                
*                                                                               
         XC    HALF,HALF                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,3               BUCKET ELEMENT                            
         USING RCONBKEL,R6                                                      
         BAS   RE,GETEL                                                         
         BNE   FIL18                                                            
         B     *+14                                                             
FIL13    CLC   HALF,RCONBKWK          OLDER BUCKET?                             
         BNH   FIL14                  NO                                        
         GOTO1 DATCON,DMCB,(2,RCONBKWK),(3,PNDACTIV)                            
         MVC   HALF,RCONBKWK                                                    
FIL14    BAS   RE,NEXTEL                                                        
         BE    FIL13                                                            
         B     FIL18                                                            
*                                                                               
         USING RCONREC,R6                                                       
FIL15    CLI   PNDKID,3               IS IT COMPLETED                           
         BE    *+14                   YES                                       
         CLI   PNDKID,4               IS IT LOSSES                              
         BE    *+6                    YES                                       
         DC    H'0'                                                             
*                                                                               
         MVI   ELCODE,6               THEN NEED SPL ELEMENT                     
         BAS   RE,GETEL                                                         
         BNE   FIL16                                                            
         USING RCONSPEL,R6                                                      
         MVC   PNDACTIV,RCONSPDT      SPL ENTRY DATE                            
         DROP  R6                                                               
*                                                                               
FIL16    L     R6,AIO                                                           
         MVI   ELCODE,8               BUT TRY FOR TRUE ACTIVITY DATE            
         BAS   RE,GETEL                                                         
         BNE   FIL18                                                            
         USING RCONACEL,R6                                                      
         MVC   PNDACTIV,RCONACTA      GOT IT                                    
         B     FIL18                                                            
         DROP  R6                                                               
*                                                                               
FIL18    L     R6,AIO              RESET R6                                     
         USING RCONREC,R6                                                       
*                                                                               
FIL20    MVC   PNDDADV,RCONKADV        ADVERTISER                               
         BAS   RE,GETADVR                                                       
         MVC   PNDADVN,WORK        ADVERTISER NAME                              
         BAS   RE,FILLPROD         RETURNS PROD IN WORK                         
         MVC   PNDPROD,WORK                                                     
         BAS   RE,FILLAGY          RETURNS AGENCY NAME IN WORK                  
         MVC   PNDDAGY,WORK                                                     
         MVC   PNDBUYER,RCONBUYR      BUYER                                     
         MVC   PNDFLITE,RCONDATE      START-END(YMD)                            
         MVC   PNDKFLDT,RCONDATE      START-END(YMD) FOR SORT                   
                                                                                
FIL30    L     R6,AIO               LOOK FOR SAR ELEMENT                        
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         BNE   FIL40                                                            
         USING RSAREL,R6                                                        
         MVC   WORK(3),RSARDEM     GET PRIMARY DEMO                             
         LA    RE,RSARDEM                                                       
         LA    RF,8                                                             
FIL33    TM    0(RE),X'40'         IS IT MARKED AS PRIMARY                      
         BO    FIL35               YES                                          
         LA    RE,3(RE)            NO/BUMP TO NEXT                              
         MVC   WORK(3),0(RE)                                                    
         BCT   RF,FIL33                                                         
         MVC   WORK(3),RSARDEM     NO 'P' DEMO FOUND/USE 1ST DEMO               
FIL35    BAS   RE,GETDEMNM         RETURNS DEMO NAME IN WORK                    
         MVC   PNDDEMO,WORK                                                     
         B     FIL40                                                            
                                                                                
* NEED SAR ELEMENT FOR SERVICE/BOOK/SECONDS/DAYPARTS/BUDGET                     
FIL40    EQU   *                                                                
         XC    PNDDAYP,PNDDAYP        WILL THIS TAKE CARE OF GARBAGE?           
         XC    PNDCPP,PNDCPP                                                    
         XC    PNDBOOK,PNDBOOK                                                  
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         BNE   FIL60                                                            
         USING RSAREL,R6                                                        
         MVC   PNDSVC,RSARSRC         SERVICE                                   
         MVC   PNDLEN,RSARRFRM        SECONDS (6 AT 2 BYTES)                    
         MVC   PNDDOLS,RSARBGT        BUDGET                                    
         MVC   PNDBOOK+2(3),RSARBKS   BOOK    (PASS ONLY 1ST BOOK)              
*                                                                               
         CLI   1(R6),RSARXLTH         EXPANDED ELEMENT?                         
         BNE   FIL42                  NO                                        
*                                                                               
         CLI   RSARXFL2-RSARXEL(R6),0                                           
         BE    *+16                                                             
         MVC   PNDDOLS(2),=C'MB'                                                
         MVC   PNDDOLS+2,RSARXFL2-RSARXEL(R6)                                   
*                                                                               
         MVC   PNDSHAR,RSARXSHG-RSARXEL(R6)                                     
*                                                                               
         TM    RSARXFLG-RSARXEL(R6),X'04'                                       
         BO    FIL50                  YES                                       
*                                                                               
FIL42    LA    R2,PNDDAYP             DAYPARTS (UP TO 6)                        
         LA    RE,PNDCPP                                                        
         LA    R3,6                                                             
         LA    R4,RSARDPT                                                       
FIL43    CLI   0(R4),X'40'                                                      
         BNH   FIL44                                                            
         MVC   0(1,R2),0(R4)                                                    
         MVC   2(2,RE),1(R4)                                                    
         LA    R2,1(R2)                                                         
         LA    RE,4(RE)                                                         
         LA    R4,3(R4)                                                         
         BCT   R3,FIL43                                                         
*                                                                               
FIL44    EQU   *                                                                
         B     FIL60                                                            
*                                                                               
FIL50    EQU   *                   DIGEST EXPANSION ELEMENTS                    
         LR    R4,R6                                                            
         USING RSARXEL,R4                                                       
         CLC   RSARXBKS(2),=C'DR'     FOR DIRECT RESPONSE                       
         BE    FIL51                                                            
         CLC   RSARXBKS(2),=C'PP'     FOR PAID PROGRAMING                       
         BE    FIL51                                                            
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,RCPRBKEQ        GET PROPOSAL EXP BOOK ELEMENT             
         USING RCPRBKEL,R6                                                      
         BAS   RE,GETEL                                                         
         BE    *+14                                                             
FIL51    MVC   PNDBOOK+2(3),RSARXBKS  BOOK    (PASS ONLY 1ST BOOK)              
         B     FIL52                                                            
         MVC   PNDBOOK,RCPRBKBK       BOOK    (PASS ONLY 1ST BOOK)              
         DROP  R6,R4                                                            
*                                                                               
FIL52    EQU   *                      READ DAYPARTS & CPP                       
         L     R6,AIO                                                           
         MVI   ELCODE,RCPRDPEQ        GET PROPOSAL EXP BOOK ELEMENT             
         USING RCPRDPEL,R6                                                      
         BAS   RE,GETEL                                                         
         BNE   FIL42                  BAILOUT TO SAR ELEMENT                    
*                                                                               
         SR    R0,R0                                                            
         ZIC   R1,RCPRDPLN                                                      
         SH    R1,=AL2(RCPRDPOX)                                                
         SR    RE,RE                                                            
         LA    RE,L'RCPRDPDP                                                    
         DR    R0,RE                                                            
         LTR   R1,R1                                                            
         BZ    FIL60                                                            
*                                                                               
         LA    RE,RCPRDPDP                                                      
         LA    R2,PNDDAYP             DAYPARTS (UP TO 8)                        
         LA    R3,PNDCPP                                                        
         LA    R0,L'PNDDAYP(R2)                                                 
*                                                                               
FIL54    EQU   *                                                                
         CR    R2,R0                                                            
         BNL   FIL55                                                            
         MVC   0(1,R2),0(RE)          MOVE DAYPART                              
         MVC   0(4,R3),1(RE)           & CPP                                    
         LA    R2,1(R2)                                                         
         LA    R3,4(R3)                                                         
         LA    RE,L'RCPRDPDP(RE)                                                
         BCT   R1,FIL54                                                         
*                                                                               
FIL55    EQU   *                                                                
*                                                                               
FIL60    EQU   *                                                                
*                                                                               
FILX     B     EXIT                                                             
         EJECT                                                                  
*********************************************************************           
* READ STATION REC TO GET COMPETITIVE STATIONS/AFFILIATION                      
* SET TO SORTREC                                                                
* R6 POINTS TO SORTREC AREA                                                     
*********************************************************************           
COMPSTAT NTR1                                                                   
         LR    R3,R6               SET R3 TO SORTREC AREA                       
         L     R6,AIO              GET STATION RECORD                           
         USING RCONREC,R6                                                       
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING RSTAREC,R5                                                       
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,RCONKREP                                                
         MVC   RSTAKSTA,RCONKSTA   STATION CALL LETTERS                         
         GOTO1 HIGH                                                             
         CLC   KEY(26),KEYSAVE     NOT CHECKING LAST POSITION(A,F,ETC)          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO2                                                         
         L     R5,AIO                                                           
         GOTO1 GETREC                                                           
         MVC   0(5,R3),RCONKSTA          STATION                                
         MVC   5(3,R3),RSTAAFFL          STATION AFFILIATION                    
                                                                                
* NOW GET COMPETITIVES                                                          
         LA    R2,11               MAX NUMBER OF COMPETITIVES                   
*                                                                               
*   NOTE:  '11' PERMITS 12 COMPETITIVE STATIONS, DUE TO THE                     
*        WAY THIS CODING IS SET UP.  THE FIRST COMPETITIVE IS                   
*        INSERTED OUTSIDE THE LOOP.                                             
*        IF COMPETITIVE STATION # IS INCREASED, THIS VALUE SHOULD               
*        BE MAX - 1.   BILL UHR.  JAN 99.                                       
*                                                                               
         L     R6,AIO2             POINT TO STATION REC                         
         MVI   ELCODE,X'02'        COMPETITIVE STATION ELEMENT                  
         BAS   RE,GETEL                                                         
         BNE   COMPX                                                            
COMP10   LA    R3,COMLENE(R3)            BUMP SORTREC                           
         USING RSTAMKEL,R6                                                      
         MVC   0(5,R3),RSTAMKST    COMPETITIVE STATION                          
         MVC   5(3,R3),RSTAMKAF    AFFILIATION                                  
         BAS   RE,NEXTEL                                                        
         BNE   COMPX                                                            
         BCT   R2,COMP10                                                        
*                                                                               
COMPX    DS    0H                                                               
         MVC   AIO,AIO1                                                         
         L     R6,AIO                                                           
         B     EXIT                                                             
         DROP  R5,R6                                                            
         EJECT                                                                  
*********************************************************************           
* - GET RECS FROM TSAROFF                                                       
*********************************************************************           
PRNTRECS NTR1                                                                   
         XC    PREVIOUS,PREVIOUS                                                
         LA    R2,NUMLVLS                                                       
PRT02    DS    0H                                                               
         GOTO1 =A(CLEARLVL),(R2),RR=RELO                                        
         BCT   R2,PRT02                                                         
         GOTO1 =A(CLEARLVL),0,RR=RELO                                           
*                                                                               
         LA    R5,TSAREA                                                        
         USING TSARD,R5                                                         
         LA    RE,MYTSREC          CLEAR I/O AREA                               
         LA    RF,SRTLENE                                                       
         XCEF                                                                   
         MVI   TSOFFACT,TSAGET     READ BY NUMBER                               
         LA    R0,1                                                             
PRT20    STH   R0,TSRNUM                                                        
         LA    R0,MYTSREC                                                       
         ST    R0,TSAREC                                                        
         GOTO1 ATSAROFF,(R5)                                                    
         TM    TSERRS,TSEEOF                  END OF FILE?                      
         BNO   PRT25                          NO - CONTINUE                     
         OC    PREVIOUS,PREVIOUS              ANY DATA PASSED?                  
         BZ    PRTX                           NO  - EXIT                        
                                                                                
         MVI   LVLIND,X'FF'                   PRINT ANY TOTALS LEFT             
         GOTO1 =A(PRTTOTS),RR=RELO                                              
         B     EXIT                                                             
                                                                                
PRT25    LA    RF,SRTREC                MOVE RECORD TO SRTREC AREA              
         LA    RE,MYTSREC                                                       
         LA    R1,SRTLENE                                                       
         MOVE  ((RF),(R1)),(RE)                                                 
                                                                                
         MVC   RCSUBPRG,PNDKID         SET SUBPROG (REC ID = SUBPROG)           
*                                                                               
         OC    PREVIOUS,PREVIOUS                                                
         BNZ   PRT30                                                            
         MVI   FORCEHED,C'Y'                                                    
         BAS   RE,SETTHIS              SET FIELDS FOR HEADS                     
         BAS   RE,SETMIDS                                                       
         B     PRT35                                                            
*                                                                               
PRT30    TM    REPORT,RPTQSTA               IS IT STATION SORT REPORT?          
         BO    PRT31                        YES                                 
                                                                                
*                                                                               
* SALESMAN SORT - TOTAL CHECKS                                                  
*                                                                               
         CLC   PREVIOUS(PNDSOFBK),SRTREC       OFFICE CHANGE?                   
         BE    PRT30A                          NO                               
         MVI   LVLIND,4                                                         
         GOTO1 =A(PRTTOTS),RR=RELO                                              
         BNE   PRT32A              NO PAGE BREAK                                
         B     PRT32                                                            
*                                                                               
PRT30A   DS    0H                                                               
         CLC   PREVIOUS(PNDSTMBK),SRTREC       TEAM CHANGE?                     
         BE    PRT30B                          NO                               
         MVI   LVLIND,3                                                         
         GOTO1 =A(PRTTOTS),RR=RELO                                              
         BNE   PRT32A              NO PAGE BREAK                                
         B     PRT32                                                            
*                                                                               
PRT30B   DS    0H                                                               
         CLC   PREVIOUS(PNDSLSBK),SRTREC       SALESPERSON CHANGE?              
         BE    PRT30C                          NO                               
         MVI   LVLIND,2                                                         
         GOTO1 =A(PRTTOTS),RR=RELO                                              
         BNE   PRT32A              NO PAGE BREAK                                
         B     PRT32                                                            
*                                                                               
PRT30C   DS    0H                                                               
         CLC   PREVIOUS(PNDSSTBK),SRTREC       STATION CHANGE                   
         BE    PRT30D                          NO                               
         MVI   LVLIND,1                                                         
         GOTO1 =A(PRTTOTS),RR=RELO                                              
         B     PRT32A              NO PAGE BREAK                                
*                                                                               
PRT30D   DS    0H                                                               
         B     PRT33                                                            
*                                                                               
* STATION SORT - TOTAL CHECK                                                    
*                                                                               
PRT31    DS    0H                                                               
         CLC   PREVIOUS(PNDSTABK),SRTREC       STATION BREAK ?                  
         BE    PRT31A                          NO                               
         MVI   LVLIND,3                                                         
         GOTO1 =A(PRTTOTS),RR=RELO                                              
         BNE   PRT32A              NO PAGE BREAK                                
         B     PRT32                                                            
                                                                                
PRT31A   DS    0H                                                               
         CLC   PREVIOUS(PNDOFFBK),SRTREC       OFFICE BREAK ?                   
         BE    PRT31B                          NO                               
         MVI   LVLIND,2                                                         
         GOTO1 =A(PRTTOTS),RR=RELO                                              
         BNE   PRT32A              NO PAGE BREAK                                
         B     PRT32                                                            
                                                                                
PRT31B   DS    0H                                                               
         CLC   PREVIOUS(PNDSLBK2),SRTREC       SALES BREAK ?                    
         BE    PRT31C                          NO                               
         MVI   LVLIND,1                                                         
         GOTO1 =A(PRTTOTS),RR=RELO                                              
         BNE   PRT32A              NO PAGE BREAK                                
         B     PRT32                                                            
                                                                                
PRT31C   DS    0H                                                               
         B     PRT33                                                            
*                                                                               
PRT32    DS    0H                              FORCE PAGE BREAK                 
         MVI   FORCEHED,C'Y'                                                    
PRT32A   DS    0H                                                               
         MVI   MIDSV,0                                                          
         BAS   RE,SETTHIS                                                       
         BAS   RE,SETMIDS                                                       
         B     PRT35                                                            
*                                                                               
PRT33    CLC   PREVIOUS+PNDKIDID(1),SRTREC+PNDKIDID     ID CHANGE?              
         BE    PRT35                                    NO                      
*                                                                               
         MVI   MIDSV,0                                                          
         BAS   RE,SETTHIS                           SET HEADS FIELDS            
         BAS   RE,SETMIDS                           SET NEW MIDLINES            
         EJECT                                                                  
*                                                                               
PRT35    EQU   *                                                                
         MVC   PREVIOUS,SRTREC     MOVE REC TO PREVIOUS REC AREA                
         BAS   RE,SETTHIS          SETS THISFIELDS FOR HEAD LINES               
*                                                                               
         TM    TOTALS,TTLQONLY     RECAP?                                       
         BNO   PRT35AX             NO                                           
*                                                                               
         BAS   RE,ROLLEM           ROLL OVER TOTALS (SETS LINETOT)              
         CLI   PNDKID,X'01'        IS THIS PENDING RECORD?                      
         BE    PRT351              YES                                          
*                                                                               
         LA    R4,12               MAX 12 STATIONS                              
         MVC   STACOL1,SPACES                                                   
         MVC   STACOL2,SPACES                                                   
         LA    R2,STACOL1+(PCOMCALL-PENDINGD)                                   
         USING PCOMCALL,R2                                                      
         LA    R3,COMSCALL                                                      
         USING COMSCALL,R3                                                      
PRT35A2  DS    0H                                                               
         OC    0(1,R3),0(R3)       ANY STATION?                                 
         BZ    PRT35A4             NO                                           
         MVC   PCOMCALL(4),COMSCALL                                             
         MVI   PCOMAFF,C'('                                                     
         MVC   PCOMAFF+1(3),COMSAFF                                             
         MVI   PCOMAFF+4,C')'                                                   
PRT35A4  LA    R3,COMLENE(R3)                                                   
         LA    R2,10(R2)                                                        
         BCT   R4,PRT35A2                                                       
*                                                                               
         MVC   0(3,R2),=C'MKT'                                                  
         MVC   132(5,R2),=C'TOTAL'                                              
*                                                                               
         B     PRT351                                                           
         DROP  R2,R3                                                            
*                                                                               
PRT35AX  DS    0H                                                               
         BAS   RE,SPLAT            PRINT A 2ND BLANK LINE                       
*                                                                               
         LA    R2,P                                                             
         USING PENDINGD,R2                                                      
         MVC   PPDCON,PNDCON       CONTRACT NUMBER                              
         MVC   PPDADV,PNDADVN      ADVERTISER NAME                              
                                                                                
* FLIGHT START-END DATES                                                        
         GOTO1 DATCON,DMCB,(3,PNDFLITE),(5,PPDFLITE)                            
         MVI   PPDFLITE+8,C'-'                                                  
         LA    R3,PPDFLITE+9                                                    
         GOTO1 DATCON,DMCB,(3,PNDFLITE+3),(5,0(R3))                             
* BUDGET                                                                        
         CLC   =C'MB',PNDDOLS      MARKET BUDGET ?                              
         BNE   PRT36                                                            
         MVC   PPDBUDGT(5),=C'BONUS'                                            
         TM    PNDDOLS+2,X'80'                                                  
         BO    PRT37D                                                           
         MVC   PPDBUDGT(5),=C'TRADE'                                            
         TM    PNDDOLS+2,X'40'                                                  
         BO    PRT37D                                                           
         MVC   PPDBUDGT(5),=C'DR   '                                            
         TM    PNDDOLS+2,X'20'                                                  
         BO    PRT37D                                                           
         MVC   PPDBUDGT(5),=C'ORDER'                                            
         TM    PNDDOLS+2,X'10'                                                  
         BO    PRT37D                                                           
         MVC   PPDBUDGT(5),=C'ORDER'                                            
         TM    PNDDOLS+2,X'10'                                                  
         BO    PRT37D                                                           
         MVC   PPDBUDGT(7),=C'GEN AVL'                                          
         TM    PNDDOLS+2,X'04'                                                  
         BO    PRT37D                                                           
         MVC   PPDBUDGT(7),=C'PP     '                                          
         TM    PNDDOLS+2,X'08'                                                  
         BO    PRT37D                                                           
                                                                                
PRT36    DS    0H                                                               
         EDIT  (B4,PNDDOLS),(11,PPDBUDGT),ALIGN=LEFT,ZERO=NOBLANK               
PRT37D   DS    0H                                                               
         TM    REPORT,RPTQPRO                                                   
         BNO   PRT37F                                                           
         MVC   PPDPROLB,=C'# PRO:'                                              
         EDIT  (B1,PNDPROS),(3,PPDPROS),ALIGN=LEFT,ZERO=NOBLANK                 
PRT37F   DS    0H                                                               
*                                                                               
* END OF 1ST PRINT LINE                                                         
                                                                                
         CLI   PNDKID,1            IF ITS PENDING ?                             
         BNE   PRT40               NO                                           
         ICM   R3,15,PNDKDST       YES/GET DAYS TO START                        
         LTR   R3,R3                                                            
         BM    PRT38                                                            
         EDIT  (R3),(8,PPDAYS),FLOAT=+                                          
         B     PRT40                                                            
PRT38    EDIT  (R3),(8,PPDAYS),FLOAT=-                                          
         B     PRT40                                                            
*                                                                               
PRT40    MVC   PPDPROD,PNDPROD     PRODUCT                                      
         MVC   PPDDEMO,PNDDEMO     DEMO                                         
         CLC   =C'MB',PNDDOLS                                                   
         BE    PRT41                                                            
         EDIT  (B1,PNDSHAR),(4,PPDSHAR),ZERO=NOBLANK,TRAIL=C'%',       +        
               ALIGN=LEFT                                                       
PRT41    EQU   *                                                                
* WEEKS IN FLIGHT                                                               
         TM    TOTALS,TTLQWKS                                                   
         BNO   PRT41A                                                           
*                                                                               
         MVC   PPDWKSLB,=C'# WEEKS:'                                            
         EDIT  (B1,PNDWEEKS),(3,PPDWEEKS),ALIGN=LEFT                            
*                                                                               
* END OF 2ND LINE                                                               
*                                                                               
PRT41A   EQU   *                                                                
         GOTO1 DATCON,DMCB,(3,PNDCREAT),(5,PPDCREAT)   CREATION DATE            
         GOTO1 DATCON,DMCB,(3,PNDACTIV),(5,PPDACTIV)   ACTIVITY DATE            
         MVC   PPDAGY,PNDDAGY                          AGENCY                   
         MVC   PPDSVCBK(1),PNDSVC                      SERVICE                  
         CLC   PNDBOOK+2(2),=C'PP'                                              
         BE    *+14                                                             
         CLC   PNDBOOK+2(2),=C'DR'                                              
         BNE   *+14                                                             
         MVC   PPDSVCBK+2(2),PNDBOOK+2                                          
         B     PRT48                                                            
         CLI   PNDBOOK,0                                                        
         BE    *+14                                                             
         MVC   PPDSVCBK+2(5),PNDBOOK                                            
         B     PRT44                                                            
*                                                                               
         XC    WORK,WORK                                                        
         MVI   WORK,28                                                          
         XC    ELEM,ELEM                                                        
         MVC   ELEM(2),=X'0B07'      PUT OUT SOURCE                             
         MVC   ELEM+2(1),PNDBOOK+1                                              
         GOTOX =V(UNBOOK),DMCB,(1,PNDBOOK+2),WORK,(C'L',ELEM),         X        
               (C'+',=CL6' ')                                                   
*                                                                               
         ZIC   RE,WORK                                                          
         LA    RE,WORK(RE)                                                      
         TM    WORK+1,X'02'       EXT FIELD HDR?                                
         BNE   *+8                                                              
         SH    RE,=H'8'                                                         
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
*                                                                               
         CLI   0(RE),C')'                                                       
         BNE   PRT42                                                            
         BCTR  RE,0                                                             
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         CLI   0(RE),C'('                                                       
         BNE   *+10                                                             
         BCTR  RE,0                                                             
         B     PRT42                                                            
*                                                                               
         LA    RE,1(RE)                                                         
         MVI   0(RE),C')'                                                       
*                                                                               
PRT42    LA    RF,WORK+8                                                        
         SR    RE,RF                                                            
         EX    RE,*+4                                                           
**JRD    B     *+10                                                             
         MVC   PPDSVCBK+2(0),WORK+8                                             
*                                                                               
PRT44    EQU   *                                                                
         CLC   =C'MB',PNDDOLS                                                   
         BE    PRT48                                                            
         MVI   PPDSTAB,C'0'                                                     
         OC    PNDDOLS,PNDDOLS                                                  
         BZ    PRT48                                                            
         OC    PNDSHAR,PNDSHAR                                                  
         BZ    PRT48                                                            
*                                                                               
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,15,PNDDOLS                                                    
         XC    HALF,HALF                                                        
         MVC   HALF+1,PNDSHAR                                                   
         MH    RF,HALF                                                          
         D     RE,=F'100'                                                       
         EDIT  (RF),(11,PPDSTAB),ALIGN=LEFT,ZERO=NOBLANK                        
*                                                                               
PRT48    EQU   *                                                                
* CONTRACT TYPE                                                                 
*&&TT*&& MVC   PPDCTYLB,=C'CONTYPE:'                                            
*&&TT*&& MVC   PPDCNTY,PNDCNTY                                                  
* END OF 3D PRINT LINE                                                          
*                                                                               
         TM    REPORT,RPTQSTA      ,,IF STATION SORT REPORT                     
         BNO   *+10                                                             
         MVC   PPD4P(20),THISLSNM  ,,SALESMAN NAME                              
         MVC   PPDBUYER,PNDBUYER   BUYER                                        
                                                                                
* MAXIMUM OF 6 LENGTHS                                                          
         LA    R4,PPDLEN                                                        
         LA    R3,PNDLEN                                                        
         LA    R6,6                                                             
PRT50    MVI   BYTE,0                                                           
         TM    1(R3),X'80'         IS IT MINUTES?                               
         BZ    PRT52              NO                                            
         NI    1(R3),X'FF'-X'80'  YES                                           
         MVI   BYTE,C'M'                                                        
PRT52    EDIT  (B2,0(R3)),(3,0(R4)),ALIGN=LEFT                                  
         CLI   BYTE,0              ,,IF MINUTES                                 
         BE    *+10                                                             
         MVC   2(1,R4),BYTE        ,,SAY SO                                     
         LA    R3,2(R3)                                                         
         OC    0(2,R3),0(R3)                                                    
         BZ    PRT55                                                            
         LA    R4,3(R4)                                                         
         BCT   R6,PRT50                                                         
* END OF 4TH PRINT LINE                                                         
PRT55    EQU   *                                                                
*                                                                               
* CHECK REPORT TYPE                                                             
         MVI   ALLOWLIN,6                                                       
         CLI   PNDKID,X'01'        IS THIS PENDING REPORT                       
         BE    *+8                 YES                                          
         MVI   ALLOWLIN,10                                                      
*                                                                               
         CLI   PNDCOMMT,C'N'       ARE THERE COMMENTS?                          
         BE    PRT58               NO                                           
         ZIC   RE,ALLOWLIN                                                      
         LA    RE,1(RE)                                                         
         STC   RE,ALLOWLIN                                                      
PRT58    BAS   RE,SPLAT            PRINT                                        
*                                                                               
PRT60    EQU   *                                                                
         LA    RF,PNDDAYP                                                       
         LA    R3,PNDCPP                                                        
         LA    R4,PPDDPT                                                        
         LA    R6,PPDCPP                                                        
*                                                                               
PRT62    CLI   0(RF),C' '                                                       
         BNH   PRT65                                                            
         MVC   0(1,R4),0(RF)                                                    
         EDIT  (B4,0(R3)),(10,0(R6)),2,ZERO=NOBLANK                             
         LA    RF,1(RF)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,PPDDPTNX-PPDDPT(R4)                                           
         LA    R6,PPDCPPNX-PPDCPP(R6)                                           
         LA    R0,PNDDAYP+L'PNDDAYP                                             
         CR    RF,R0                                                            
         BNL   PRT65                                                            
         LA    R0,PPDDPTEN                                                      
         CR    R4,R0                                                            
         BL    PRT62                                                            
*                                                                               
PRT65    CLC   P,SPACES                                                         
         BNH   PRT70                                                            
         BAS   RE,SPLAT            PRINT                                        
*                                                                               
PRT70    EQU   *                                                                
         BAS   RE,ROLLEM           ROLL OVER TOTALS (SETS LINETOT)              
*                                                                               
* END OF COMMON REPORT PRINT LINES                                              
         CLI   PNDKID,X'01'        IS THIS PENDING REPORT                       
         BE    PRT300              YES                                          
*                                                                               
* COMPLETED / INCOMPLETE/ LOSS REPORTS                                          
PRT250   EQU   *                                                                
         LA    R4,12               MAX 12 STATIONS                              
         LA    R2,PCOMCALL                                                      
         USING PCOMCALL,R2                                                      
         LA    R3,COMSCALL                                                      
         USING COMSCALL,R3                                                      
PRT260   MVC   PCOMCALL(4),COMSCALL                                             
         MVI   PCOMAFF,C'('                                                     
         MVC   PCOMAFF+1(3),COMSAFF                                             
         MVI   PCOMAFF+4,C')'                                                   
         CLI   COMSAMT+(L'COMSAMT-1),0    INITIALIZED?                          
         BNE   *+10                                                             
         ZAP   COMSAMT,=P'0'                                                    
         EDIT  COMSAMT,(10,PCOMAMT),ALIGN=LEFT,ZERO=NOBLANK                     
         CLC   COMSSHR,=X'00000000'            DON'T PRINT 0 %                  
         BE    PRT264                                                           
         TM    REPORT,RPTQRND                                                   
         BZ    PRT262                                                           
*                                                                               
         ICM   R1,15,COMSSHR                                                    
         CVD   R1,DUB                                                           
         ZAP   WORK+20(8),DUB                                                   
         SRP   WORK+20(8),64-2,5                                                
         EDIT  (P8,WORK+20),(10,PCOMSHR),ALIGN=LEFT,TRAIL=C'%'                  
         B     PRT264                                                           
*                                                                               
PRT262   DS    0H                                                               
         EDIT  (B4,COMSSHR),(10,PCOMSHR),ALIGN=LEFT,2,TRAIL=C'%'                
PRT264   LA    R3,COMLENE(R3)                                                   
         LA    R2,10(R2)                                                        
         OC    0(1,R3),0(R3)       ANY MORE STATIONS                            
         BZ    PRT265                                                           
         BCT   R4,PRT260                                                        
*                                                                               
         DROP  R2,R3                                                            
*                                                                               
PRT265   CLI   PNDKID,3            IF COMPLETED                                 
         BE    *+12                                                             
         CLI   PNDKID,4            OR LOSS                                      
         BNE   PRT267                                                           
         LTR   R4,R4               DID WE PRINT ALL 12 MKTS ?                   
         BNZ   *+12                                                             
         LA    R2,10(R2)           YES/BUMP TO LAST PRINT POSITION              
         B     PRT266                                                           
         BCTR  R4,0                NO - BUMP TO LAST PRINT POSITION             
         MH    R4,=H'10'                10=LENGTH OF EACH PRINT POST            
         AR    R2,R4                                                            
PRT266   MVC   0(3,R2),=C'MKT'                                                  
         MVC   132(5,R2),=C'TOTAL'                                              
         LA    R2,132(R2)                                                       
         LA    R2,132(R2)                                                       
         EDIT  (P6,LINETOT),(10,0(R2)),ALIGN=LEFT,ZERO=NOBLANK                  
         ZAP   LINETOT,=P'0'       CLEAR LINETOT                                
                                                                                
         CLC   P3,SPACES           IF NO 3D P LINE                              
         BNE   PRT267                                                           
         MVC   P3,P4               MOVE UP 4TH                                  
         XC    P4,P4                                                            
*                                                                               
PRT267   DS    0H                                                               
         MVC   STACOL1,P           KEEP THESE AROUND                            
         MVC   STACOL2,P2                                                       
         BAS   RE,SPLAT                                                         
         B     PRT300                                                           
*                                                                               
         EJECT                                                                  
*                                                                               
PRT300   DS    0H                                                               
* - ARE THERE COMMENTS TO PRINT ?                                               
* PNDCOMMT HAS SPL/BOP + DISK ADDRESS                                           
*                                                                               
         CLI   PNDCOMMT,C'N'                                                    
         BE    PRT350              NO                                           
*                                                                               
         LA    R4,2                2 COMMENT ELEM PER PRINT LINE                
         XC    WORK2,WORK2         WORK AREA TO BUILD COMMENTS                  
         LA    R3,WORK2                                                         
         MVC   KEY,PNDCOMMT+3    MOVE IN KEY                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         CLI   PNDCOMMT,C'B'                                                    
         BNE   PRT320                                                           
         MVI   ELCODE,X'11'        BOP COMMENT ELEMENT                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                SHOULD NEVER GET HERE                        
         USING RCONBCEL,R6                                                      
PRT305   ZIC   R1,RCONBCLN         LENGTH OF ELEMENT                            
         S     R1,=F'3'                                                         
         LTR   R1,R1                                                            
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+4                                                           
**JRD    B     *+10                                                             
         MVC   0(0,R3),RCONBCOM    MOVE COMMENT TO WORK2                        
         AR    R3,R1               BUMP TO NEXT FREE AREA IN WORK2              
         LA    R3,2(R3)                                                         
         BAS   RE,NEXTEL           ANY MORE COMMENT ELEMENTS?                   
         BNE   PRT310                                                           
         BCT   R4,PRT305                                                        
                                                                                
PRT310   BAS   RE,CHOPNPRT         CHOP AND PRINT LINES                         
         LTR   R4,R4               .IF R4 NOT = 0                               
         BNZ   PRT350              .NO MORE COMMENT ELEM                        
         XC    WORK2,WORK2         YES/DEAL WITH MORE ELEMS HERE                
         LA    R3,WORK2                                                         
         LA    R4,2                2 COMMENT ELEMS PER PRINT LINE               
         B     PRT305                                                           
*                                                                               
PRT320   L     R6,AIO              SPL COMMENT ELEMENT                          
         USING RCONREC,R6                                                       
         MVI   ELCODE,X'07'                                                     
         BAS   RE,GETEL                                                         
         BNE   PRT350                                                           
         USING RCONSMEL,R6                                                      
PRT325   ZIC   R1,RCONSMLN         LENGTH OF ELEMENT                            
         S     R1,=F'3'                                                         
         LTR   R1,R1                                                            
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+4                                                           
**JRD    B     *+10                                                             
         MVC   0(0,R3),RCONSMNT                                                 
         AR    R3,R1               BUMP TO NEXT FREE AREA IN WORK2              
         LA    R3,2(R3)                                                         
         BAS   RE,NEXTEL                                                        
         BNE   PRT330                                                           
         BCT   R4,PRT325                                                        
PRT330   BAS   RE,CHOPNPRT         CHOP AND PRINT LINES                         
         LTR   R4,R4               R4 NOT = 0                                   
         BNZ   PRT350              NO MORE COMMENT ELEMENTS                     
         XC    WORK2,WORK2         YES/DEAL WITH MORE ELEMS HERE                
         LA    R3,WORK2                                                         
         LA    R4,2                                                             
         B     PRT325                                                           
                                                                                
*                                                                               
PRT350   DS    0H                                                               
         BAS   RE,SPLAT            PRINT BLANK LINE                             
PRT351   DS    0H                                                               
         LH    R0,TSRNUM           BUMP TSAR ID NUMBER                          
         A     R0,=F'1'                                                         
         STH   R0,TSRNUM                                                        
         B     PRT20               GET NEXT TSAR REC                            
PRTX     B     EXIT                                                             
*                                                                               
         DROP R5                                                                
         EJECT                                                                  
**********************************************************************          
* CHOP AND PRINT                                                                
**********************************************************************          
CHOPNPRT NTR1                                                                   
         XC    DMCB(16),DMCB                                                    
         LA    R3,WORK2                                                         
         ST    R3,DMCB             A(INPUT)                                     
         MVI   DMCB,131            LENGTH OF INPUT                              
         LA    R1,P                                                             
         ST    R1,DMCB+4           A(OUTPUT)                                    
         MVI   DMCB+4,132          L'OUTPUT                                     
         MVI   DMCB+11,1            MAX NO OF LINES                             
         MVI   DMCB+8,132          PRINT LINES ARE 132 APART                    
         GOTO1 CHOPPER,DMCB                                                     
         BAS   RE,SPLAT            PRINT                                        
         B     EXIT                                                             
**********************************************************************          
* ROLL OVER TOTALS                                                              
**********************************************************************          
ROLLEM   NTR1                                                                   
         GOTO1 =A(CLEARLVL),0,RR=RELO                                           
         ZAP   LINETOT,=P'0'                                                    
         AP    CMPTCHDR,=P'1'      USE COMPLETED FOR ALL COMMON TOTS            
         ZIC   RE,PNDWEEKS                                                      
         CVD   RE,DUB                                                           
         AP    CMPTCWKS,DUB                                                     
*                                                                               
         CLI   PNDKID,1            PENDING LINE?                                
         BNE   ROLL10              NO                                           
         CLC   =C'MB',PNDDOLS                                                   
         BE    ROLL100                                                          
*                                                                               
         ICM   RE,15,PNDDOLS                                                    
         CVD   RE,DUB                                                           
         AP    CMPTCMKB,DUB                                                     
*                                                                               
         OC    PNDSHAR,PNDSHAR                                                  
         BZ    ROLL100                                                          
*                                                                               
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,15,PNDDOLS                                                    
         XC    HALF,HALF                                                        
         MVC   HALF+1,PNDSHAR                                                   
         MH    RF,HALF                                                          
         D     RE,=F'100'                                                       
         CVD   RF,DUB                                                           
         AP    CMPTCSTB,DUB                                                     
*                                                                               
         B     ROLL100                                                          
*                                                                               
ROLL10   DS    0H                  INCOMPLETE/COMPLETE/LOSS                     
         LA    RE,NUMSTAS          MAX NUM OF $ FIELDS                          
         LA    R2,COMSAMT          STATION $FIELD                               
         LA    R3,CSTATOTS         PACKED TOTAL FIELDS                          
ROLL20   DS    0H                                                               
         CLI   L'COMSAMT-1(R2),0                                                
         BNE   *+10                                                             
         ZAP   0(L'COMSAMT,R2),=P'0'                                            
         AP    0(6,R3),0(L'COMSAMT,R2)                                          
         AP    LINETOT,0(L'COMSAMT,R2)                                          
ROLL30   LA    R2,COMLENE(R2)      BUMP STATION $ FIELD                         
         LA    R3,6(R3)            BUMP TOTAL SALES FIELD                       
         BCT   RE,ROLL20                                                        
*                                                                               
         CLI   COMSAMT+(L'COMSAMT-1),0    INITIALIZED?                          
         BNE   *+10                                                             
         ZAP   COMSAMT,=P'0'                                                    
         AP    CMPTCSTB,COMSAMT                                                 
*                                                                               
         AP    CMPTCMKB,LINETOT                                                 
*                                                                               
ROLL100  DS    0H                  ADD TOTALS TO EACH LEVEL                     
         LA    R2,CMPTOTS+CMPTOTLN                                              
L        USING CMPTOTS,R2                                                       
         LA    R3,NUMLVLS                                                       
*                                                                               
ROLL102  CLI   PNDKID,1            PENDING?                                     
         BNE   ROLL104             NO                                           
         AP    L.CMPTPMKB,CMPTCMKB                                              
         AP    L.CMPTPHDR,CMPTCHDR                                              
         AP    L.CMPTPSTB,CMPTCSTB                                              
         AP    L.CMPTPWKS,CMPTCWKS                                              
         B     ROLL120                                                          
*                                                                               
ROLL104  CLI   PNDKID,2            INCOMPLETE?                                  
         BNE   ROLL106             NO                                           
*                                                                               
         LA    R4,L.ISTATOTS                                                    
         AP    L.CMPTIHDR,CMPTCHDR                                              
         AP    L.CMPTISTB,CMPTCSTB                                              
         AP    L.CMPTIWKS,CMPTCWKS                                              
         B     ROLL110                                                          
*                                                                               
ROLL106  CLI   PNDKID,3            COMPLETE?                                    
         BNE   ROLL108             NO                                           
*                                                                               
         LA    R4,L.CSTATOTS                                                    
         AP    L.CMPTCMKB,CMPTCMKB                                              
         AP    L.CMPTCHDR,CMPTCHDR                                              
         AP    L.CMPTCSTB,CMPTCSTB                                              
         AP    L.CMPTCWKS,CMPTCWKS                                              
         B     ROLL110                                                          
*                                                                               
ROLL108  CLI   PNDKID,4            LOSS?                                        
         BE    *+6                 YES                                          
         DC    H'0'                                                             
*                                                                               
         LA    R4,L.LSTATOTS                                                    
         AP    L.CMPTLMKB,CMPTCMKB                                              
         AP    L.CMPTLHDR,CMPTCHDR                                              
         AP    L.CMPTLWKS,CMPTCWKS                                              
         B     ROLL110                                                          
*                                                                               
ROLL110  DS    0H                                                               
         LA    R1,NUMSTAS                                                       
         LA    R5,CSTATOTS                                                      
*                                                                               
ROLL112  DS    0H                                                               
         AP    0(L'CSTATOTS,R4),0(L'CSTATOTS,R5)                                
         LA    R4,L'CSTATOTS(R4)                                                
         LA    R5,L'CSTATOTS(R5)                                                
         BCT   R1,ROLL112                                                       
*                                                                               
ROLL120  DS    0H                                                               
         LA    R2,CMPTOTLN(R2)                                                  
         BCT   R3,ROLL102                                                       
*                                                                               
         B     EXIT                                                             
         DROP  L                                                                
         EJECT                                                                  
**********************************************************************          
* GETS PRODUCT NAME FROM ELEMENT 5 OR PRODUCT RECORD                            
* AND RETURNS IT IN WORK                                                        
**********************************************************************          
FILLPROD NTR1                                                                   
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
         CLI   RCONPRD,X'40'       ,,IF PROD NOT = BLANK                        
         BNH   FILL10                                                           
         MVC   WORK(3),RCONPRD     ,,PASS PROD CODE IN WORK                     
         BAS   RE,GETPROD               RETURNS PRODUCT NAME IN WORK            
         B     FILL25                                                           
*                                                                               
FILL10   MVI   ELCODE,5            ,,ELSE PROD NAME ON ELEM 5                   
         BAS   RE,GETEL                                                         
         USING RCONEXEL,R6                                                      
         MVC   WORK(20),RCONEXPR                                                
*                                                                               
FILL25   EQU   *                                                                
         B     EXIT                                                             
         DROP R6                                                                
*****************************************************************               
* READS PRODUCT RECORD                                                          
* EXPECTS PROD CODE IN WORK - CONTRACT RECORD IN AIO                            
* RETURNS PROD NAME IN WORK                                                     
*****************************************************************               
GETPROD  NTR1                                                                   
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING RPRDREC,R5                                                       
         MVI   RPRDKTYP,9                                                       
         MVC   RPRDKADV,RCONKADV                                                
         MVC   RPRDKPRD,WORK                                                    
         MVC   RPRDKREP,RCONKREP                                                
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO2                                                         
         L     R5,AIO                                                           
         GOTO1 GETREC                                                           
         MVC   WORK(20),RPRDNAME                                                
         MVC   AIO,AIO1                                                         
         L     R6,AIO                                                           
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
*********************************************************************           
* RETURN AGENCY NAME - EXPECTS CONTRACT REC IN AIO                              
*********************************************************************           
FILLAGY  NTR1                                                                   
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING RAGYKEY,R5                                                       
         MVI   RAGYKTYP,X'0A'                                                   
         MVC   RAGYKAGY,RCONKAGY                                                
         MVC   RAGYKAOF,RCONKAOF                                                
         MVC   RAGYKREP,RCONKREP                                                
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    FAGY0020                                                         
         MVC   WORK(20),=C'**CODE NOT ON FILE**'                                
         B     EXIT                                                             
FAGY0020 EQU   *                                                                
         MVC   AIO,AIO2                                                         
         L     R5,AIO                                                           
         GOTO1 GETREC                                                           
         MVC   WORK(20),RAGYNAM1                                                
         MVC   AIO,AIO1                                                         
         L     R6,AIO                                                           
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
*********************************************************************           
* HEADLINE HOOK                                                                 
*********************************************************************           
HOOK     NTR1                                                                   
         LA    R5,REPIOTBL                                                      
         USING REPIOD,R5                                                        
*                                                                               
         TM    REPORT,RPTQSTA      IS IT STATION SORT?                          
         BO    HO10                NO                                           
*                                                                               
*   SALESPERSON SORT HEADLINES                                                  
*                                                                               
         TM    TOTALS,TTLQONLY     RECAP REPORT?                                
         BO    HO02                YES - NEED TO DO DYNAMIC HEADLINES           
*                                                                               
         MVC   H4+1(6),=C'OFFICE'                                               
         MVC   H4+14(2),THISOFF                                                 
         MVC   H5+1(4),=C'TEAM'                                                 
         MVC   H5+14(2),THISTEAM                                                
         MVC   H6+1(11),=C'SALESPERSON'                                         
         MVC   H6+14(24),THISLSNM                                               
         B     HO40                                                             
*                                                                               
HO02     DS    0H                                                               
         LA    R2,H4                                                            
         TM    TOTALS,SALQTEAM                                                  
         BNO   HO03                                                             
*                                                                               
         MVC   1(6,R2),=C'OFFICE'                                               
         MVC   14(2,R2),THISOFF                                                 
         LA    R2,132(R2)                                                       
*                                                                               
HO03     DS    0H                                                               
         TM    TOTALS,SALQSAL                                                   
         BNO   HO04                                                             
*                                                                               
         MVC   1(4,R2),=C'TEAM'                                                 
         MVC   15(2,R2),THISTEAM                                                
         LA    R2,132(R2)                                                       
*                                                                               
HO04     DS    0H                                                               
         TM    TOTALS,SALQSTA                                                   
         BNO   HO05                                                             
*                                                                               
         MVC   1(11,R2),=C'SALESPERSON'                                         
         MVC   14(24,R2),THISLSNM                                               
         LA    R2,132(R2)                                                       
*                                                                               
HO05     DS    0H                                                               
         OC    RIPSTA,RIPSTA       STATION FILTER?                              
         BNZ   HO06                YES                                          
*                                                                               
         MVC   1(12,R2),=C'ALL STATIONS'                                        
         LA    R2,132(R2)                                                       
*                                                                               
HO06     DS    0H                                                               
         B     HO20                RECAP NEEDS "FILTERS" LINE                   
*                                                                               
*   STATION SORT HEADLINES                                                      
*                                                                               
HO10     DS    0H                                                               
         TM    TOTALS,TTLQONLY     RECAP REPORT?                                
         BO    HO12                YES - NEED TO DO DYNAMIC HEADLINES           
*                                                                               
         MVC   H6+1(6),=C'OFFICE'                                               
         MVC   H6+14(2),THISOFF                                                 
         MVC   H5+1(7),=C'STATION'                                              
         MVC   H5+14(5),THISSTA                                                 
         B     HO40                                                             
*                                                                               
HO12     DS    0H                                                               
         LA    R2,H4                                                            
         TM    TOTALS,STAQOFF                                                   
         BNO   HO13                                                             
*                                                                               
         MVC   1(7,R2),=C'STATION'                                              
         MVC   14(5,R2),THISSTA                                                 
         LA    R2,132(R2)                                                       
*                                                                               
HO13     DS    0H                                                               
         TM    TOTALS,STAQSAL                                                   
         BNO   HO14                                                             
*                                                                               
         MVC   1(6,R2),=C'OFFICE'                                               
         MVC   14(2,R2),THISOFF                                                 
         LA    R2,132(R2)                                                       
*                                                                               
HO14     DS    0H                                                               
         OC    RIPOFF,RIPOFF       STATION FILTER?                              
         BNZ   HO15                YES                                          
         TM    TOTALS,STAQSAL      SHOWN OFFICE ALREADY?                        
         BO    HO15                YES                                          
*                                                                               
         MVC   1(11,R2),=C'ALL OFFICES'                                         
         LA    R2,132(R2)                                                       
*                                                                               
HO15     DS    0H                                                               
         B     HO20                RECAP NEEDS "FILTERS" LINE                   
*                                                                               
HO20     DS    0H                  DO COMMON RECAP HEADLINES                    
         LA    R5,REPIOTBL                                                      
         USING REPIOD,R5                                                        
         LA    RE,H7               WHERE TO PUT THE LINES?                      
         CR    R2,RE                                                            
         BH    *+8                                                              
         LA    R2,H7                                                            
         LA    R3,10(R2)           SKIP FOR "FILTERS: "                         
*                                                                               
         OC    FLTREG,FLTREG       REGION?                                      
         BZ    HO22                NO                                           
*                                                                               
         MVC   0(4,R3),=C'REG='                                                 
         MVC   4(L'FLTREG,R3),FLTREG                                            
         LA    R3,4+L'FLTREG-1(R3)                                              
         BAS   RE,ADDCOMMA                                                      
*                                                                               
HO22     DS    0H                                                               
         OC    RIPOFF,RIPOFF       OFFICE?                                      
         BZ    HO23                NO                                           
*                                                                               
         MVC   0(4,R3),=C'OFF='                                                 
         MVC   4(L'RIPOFF,R3),RIPOFF                                            
         LA    R3,4+L'RIPOFF-1(R3)                                              
         BAS   RE,ADDCOMMA                                                      
*                                                                               
HO23     DS    0H                                                               
         OC    RIPGRP,RIPGRP       GROUP?                                       
         BZ    HO24                NO                                           
*                                                                               
         MVC   0(4,R3),=C'GRP='                                                 
         MVC   4(L'RIPGRP,R3),RIPGRP                                            
         LA    R3,4+L'RIPGRP-1(R3)                                              
         BAS   RE,ADDCOMMA                                                      
*                                                                               
HO24     DS    0H                                                               
         OC    RIPSTA,RIPSTA       STATION?                                     
         BZ    HO25                NO                                           
*                                                                               
         MVC   0(4,R3),=C'STA='                                                 
         MVC   4(L'RIPSTA,R3),RIPSTA                                            
         LA    R3,4+L'RIPSTA-1(R3)                                              
         BAS   RE,ADDCOMMA                                                      
*                                                                               
HO25     DS    0H                                                               
         OC    RIPSAL,RIPSAL       SALESPERSON?                                 
         BZ    HO26                NO                                           
*                                                                               
         MVC   0(4,R3),=C'SAL='                                                 
         MVC   4(L'RIPSAL,R3),RIPSAL                                            
         LA    R3,4+L'RIPSAL-1(R3)                                              
         BAS   RE,ADDCOMMA                                                      
*                                                                               
HO26     DS    0H                                                               
         OC    RIPTEAM,RIPTEAM     TEAM?                                        
         BZ    HO27                NO                                           
*                                                                               
         MVC   0(09,R3),=C'DIV/TEAM='                                           
         MVC   9(L'RIPTEAM,R3),RIPTEAM                                          
         LA    R3,9+L'RIPTEAM-1(R3)                                             
         BAS   RE,ADDCOMMA                                                      
*                                                                               
HO27     DS    0H                                                               
         OC    RIPADV,RIPADV       ADVERTISER?                                  
         BZ    HO28                NO                                           
*                                                                               
         MVC   0(4,R3),=C'ADV='                                                 
         MVC   4(L'RIPADV,R3),RIPADV                                            
         LA    R3,4+L'RIPADV-1(R3)                                              
         BAS   RE,ADDCOMMA                                                      
*                                                                               
HO28     DS    0H                                                               
         OC    RIPAGY,RIPAGY       AGENCY?                                      
         BZ    HO29                NO                                           
*                                                                               
         MVC   0(4,R3),=C'AGY='                                                 
         MVC   4(L'RIPAGY,R3),RIPAGY                                            
         LA    R3,4+L'RIPAGY-1(R3)                                              
         OC    RIPAGOFF,RIPAGOFF                                                
         BZ    *+14                                                             
         MVC   1(L'RIPAGOFF,R3),RIPAGOFF                                        
         LA    R3,RIPAGOFF(R3)                                                  
         BAS   RE,ADDCOMMA                                                      
*                                                                               
HO29     DS    0H                                                               
         OC    RIPCAT,RIPCAT       CATEGORY?                                    
         BZ    HO30                NO                                           
*                                                                               
         MVC   0(4,R3),=C'CAT='                                                 
         MVC   4(L'RIPCAT,R3),RIPCAT                                            
         LA    R3,4+L'RIPCAT-1(R3)                                              
         BAS   RE,ADDCOMMA                                                      
*                                                                               
HO30     DS    0H                                                               
         CLC   10(20,R2),SPACES                                                 
         BNH   HO40                                                             
         MVC   0(10,R2),=CL10'FILTERS:'                                         
         BCTR  R3,0                                                             
         MVI   0(R3),C' '                                                       
*                                                                               
HO40     DS    0H                                                               
         MVC   H5+48(7),=C'PERIOD:'                                             
         GOTO1 DATCON,DMCB,(2,RIPDATS),(5,H5+55)                                
         MVI   H5+63,C'-'                                                       
         GOTO1 DATCON,DMCB,(2,RIPDATE),(5,H5+64)                                
         MVC   H6+46(9),=C'ACTIVITY:'                                           
         MVC   WORK(3),MONDAYDT                                                 
         MVC   WORK+3(3),BTODAY                                                 
         CLI   ACTSTR,0                                                         
         BE    HO42                                                             
         MVC   WORK(3),ACTSTR                                                   
         MVC   WORK+3(3),ACTEND                                                 
HO42     GOTO1 DATCON,DMCB,(3,WORK),(5,H6+55)                                   
         MVI   H6+63,C'-'                                                       
         GOTO1 DATCON,DMCB,(3,WORK+3),(5,H6+64)                                 
*                                                                               
         TM    TOTALS,TTLQONLY                                                  
         BO    HOXX                NO MIDLINE ON RECAP                          
         CLI   MIDSV,0                                                          
         BE    HOXX                NO MIDLINE TO SHOW                           
         MVC   H14+30(80),MIDSV                                                 
HOXX     B     EXIT                                                             
         DROP  R5                                                               
         SPACE 3                                                                
ADDCOMMA DS    0H                                                               
         CLI   0(R3),C' '                                                       
         BH    *+10                                                             
         BCTR  R3,0                                                             
         B     *-10                                                             
         MVI   1(R3),C','                                                       
         LA    R3,2(R3)                                                         
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
**********************************************************************          
* EXPECTS DEMO CODE IN WORK                                                     
* RETURNS DEMO NAME IN WORK                                                     
**********************************************************************          
GETDEMNM NTR1                                                                   
         GOTO1 =A(XGETDEM),DMCB,SORTREC,RR=RELO                                 
         B     EXIT                                                             
         EJECT                                                                  
*********************************************************************           
* - PRINT ROUTINE                                                               
*********************************************************************           
SPLAT    NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
EXIT     XIT1                                                                   
**********************************************************************          
*  SETS THISFIELDS FOR HEADLINES                                                
**********************************************************************          
SETTHIS  NTR1                                                                   
         MVC   THISMRKT,PNDMRKT                                                 
*                                                                               
         MVC   THISOFF,PNDKOFF         OFFICE FOR HEADLINES                     
         CLI   THISOFF,X'40'       IF BLANK                                     
         BH    *+10                                                             
         MVC   THISOFF,PNDKOFF2    MUST BE STATION SORT                         
*                                                                               
         MVC   THISTEAM,PNDKTEAM       TEAM FOR HEADLINES                       
         MVC   THISLSNM(3),PNDKSLS     SALESPERSON CODE/NAME FOR HEADS          
         MVC   THISLSNM+4(20),PNDSLSNM                                          
         TM    REPORT,RPTQSTA          IF STATION SORT                          
         BNO   SH10                                                             
         XC    THISLSNM,THISLSNM                                                
         MVC   THISLSNM(20),PNDSLSNM  CODE/NAME TRUNCATED                       
*                                                                               
SH10     MVC   THISSTA,PNDKSTAT                                                 
         CLI   THISSTA,X'40'       IF BLANK                                     
         BH    *+10                                                             
         MVC   THISSTA,PNDKSTA     MUST BE STATION SORT                         
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
*  SETS MIDLINES                                                                
**********************************************************************          
SETMIDS  NTR1                                                                   
         TM    TOTALS,TTLQONLY                                                  
         BO    EXIT                                                             
*                                                                               
         MVI   ALLOWLIN,12                                                      
                                                                                
         BAS   RE,SPLAT              A COUPLE OF BLANK LINES                    
         BAS   RE,SPLAT                                                         
                                                                                
         MVI   P+1,C'*'                                                         
         MVC   P+2(50),P+1                                                      
                                                                                
         TM    REPORT,RPTQSTA                                                   
         BO    SM10                                                             
         MVC   P+50(13),=C'*****STATION:'                                       
         MVC   P+63(5),PNDKSTAT                                                 
         MVI   P+69,C'-'                                                        
         MVC   P+71(20),PNDMRKT                                                 
         LA    RE,P+90                                                          
         CLI   0(RE),C' '                                                       
         BH    *+10                                                             
         BCTR  RE,0                                                             
         B     *-10                                                             
         LA    RE,1(RE)                                                         
         LR    R0,RE                                                            
         LA    R2,P+64                                                          
         SR    R0,R2                                                            
         STC   R0,BYTE             LENGTH OF MOVE                               
         MVI   0(RE),C'*'                                                       
         LA    R2,P+129                                                         
         SR    R2,RE                                                            
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   1(0,RE),0(RE)                                                    
         LA    R2,P2+57                                                         
         B     SM20                                                             
                                                                                
SM10     MVC   P+46(17),=C'*****SALESPERSON:'                                   
         MVC   P+63(20),PNDSLSNM                                                
         MVI   P+83,C'*'                                                        
         MVC   P+84(44),P+83                                                    
         LA    R2,P2+62                                                         
                                                                                
SM20     DS    0H                                                               
         ZIC   R1,SRTREC+PNDKIDID                                               
         BCTR  R1,0                                                             
         MH    R1,=H'10'                                                        
         LA    RE,MIDTABLE                                                      
         AR    RE,R1                                                            
         MVC   0(10,R2),0(RE)                                                   
                                                                                
* - SAVE CURRENT MIDLINE DATA TO PRINT AT TOP OF PAGE IF CURRENT                
*   MIDLINE GOES MORE THAN ONE PAGE                                             
         XC    MIDSV,MIDSV                                                      
         MVI   MIDSV,C'*'                                                       
         MVC   MIDSV+1(19),MIDSV                                                
         MVI   MIDSV+20,C'('                                                    
         MVC   MIDSV+21(10),0(RE)                                               
         LA    R1,MIDSV                                                         
         CLI   0(R1),X'40'                                                      
         BNH   *+12                                                             
         LA    R1,1(R1)                                                         
         B     *-12                                                             
         MVI   0(R1),C':'                                                       
         SR    R2,R2                                                            
         ZIC   R2,BYTE                                                          
         TM    REPORT,RPTQSTA      IS IT STATION SORT                           
         BNO   SM25                                                             
         LA    R2,19                                                            
SM25     EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R1),P+63                                                     
         AR    R1,R2                                                            
         MVI   2(R1),C')'                                                       
         MVI   3(R1),C'*'                                                       
         LA    RE,MIDSV+L'MIDSV-5                                               
         SR    RE,R1                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   4(19,R1),3(R1)                                                   
                                                                                
         BAS   RE,SPLAT                                                         
SMX      B     EXIT                                                             
                                                                                
*                                                                               
MIDTABLE DS    0CL10                                                            
         DC    CL10'PENDING'                                                    
         DC    CL10'INCOMPLETE'                                                 
         DC    CL10'COMPLETED'                                                  
         DC    CL10'LOSSES'                                                     
**********************************************************************          
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
**********************************************************************          
* GET ADVERTISER                                                                
**********************************************************************          
GETADVR  NTR1                                                                   
         XC    WORK(5),WORK                                                     
         XC    KEY,KEY                                                          
         MVI   KEY,8                                                            
         MVC   KEY+21(4),RCONKADV                                               
         MVC   KEY+25(2),RCONKREP                                               
GETADV2  GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BNE   GETADV3                                                          
         CLC   RCONKREP,KEY+25                                                  
         BE    GETADV4                                                          
         CLC   =C'ZZ',KEY+25                                                    
         BE    GETADV4                                                          
         MVC   KEY+25(2),=C'ZZ'                                                 
         B     GETADV2                                                          
GETADV3  MVC   WORK(12),=C'ADV UNKNOWN '                                        
         B     GETADVX                                                          
*                                                                               
GETADV4  EQU   *                                                                
         USING RADVREC,R4                                                       
         MVC   AIO,AIO2                                                         
         L     R4,AIO                                                           
         GOTO1 GETREC                                                           
         MVC   WORK(20),RADVNAME                                                
         MVC   AIO,AIO1                                                         
         L     R6,AIO                                                           
         B     GETADVX                                                          
         DROP  R4                                                               
GETADVX  EQU   *                                                                
         B     EXIT                                                             
**********************************************************************          
* GET SALESPERSON NAME                                                          
**********************************************************************          
GETSLSNM NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R1,KEY                                                           
         USING RSALREC,R1                                                       
         MVI   RSALKTYP,X'06'                                                   
         MVC   RSALKREP,RCONKREP                                                
         MVC   RSALKSAL,RCONSAL                                                 
         DROP  R1                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   GTNOSAL                                                          
         SPACE 1                                                                
GTDISSAL EQU   *                                                                
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         L     R1,AIO                                                           
         USING RSALREC,R1                                                       
         MVC   WORK(20),RSALNAME                                                
         B     GTSALX                                                           
         DROP  R1                                                               
                                                                                
GTNOSAL  MVC   WORK(20),=CL20'NOT FOUND'                                        
         B     GTSALX                                                           
                                                                                
GTSALX   EQU   *                                                                
         MVC   AIO,AIO1                                                         
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* GET STATION MARKET                                                            
**********************************************************************          
GETMRKT  NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R1,KEY                                                           
         USING RSTAREC,R1                                                       
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,RCONKREP                                                
         MVC   RSTAKSTA,RCONKSTA                                                
         DROP  R1                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   GTNOSTA                                                          
         SPACE 1                                                                
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         L     R1,AIO                                                           
         USING RSTAREC,R1                                                       
         MVC   WORK(20),RSTAMKT                                                 
         B     GTSTAX                                                           
         DROP  R1                                                               
GTNOSTA  MVC   WORK(20),=CL20'UNKNOWN MARKET'                                   
         B     GTSTAX                                                           
GTSTAX   EQU   *                                                                
         MVC   AIO,AIO1                                                         
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
** SORTREC ***********************************************************          
**********************************************************************          
SORTREC  DS    0CL750              USE THIS LABEL TO ESTABLISH                  
*                                  ADDRESSABILITY FOR XRTNS                     
SRTREC   DS    0CL750                                                           
*                                                                               
SRTKEY   DS    CL100                                                            
SRTDATA  DS    CL650                                                            
SRTEND   EQU   *                                                                
SRTLENE  EQU   *-SRTREC                                                         
*                                                                               
* COMMON SORT KEY ****************************************************          
         ORG   SRTKEY                                                           
PNDKEY   DS    0CL(L'SRTKEY)                                                    
         DS    CL1                 SPARE                                        
PNDKOFF  DS    CL2                 OFFICE                                       
PNDSOFBK EQU   *-PNDKEY            OFFICE                                       
PNDKTEAM DS    CL2                 DIVISION/TEAM                                
PNDSTMBK EQU   *-PNDKEY            TEAM BREAK                                   
PNDKSLS  DS    CL3                 SALESPERSON                                  
PNDSLSBK EQU   *-PNDKEY            SALESPERSON BREAK                            
*                                                                               
PNDKSTAT DS    CL5                 STATION                                      
PNDSSTBK EQU   *-PNDKEY            STATION BREAK                                
*                                                                               
         DS    CL5                 SPARE                                        
         ORG   PNDKSTAT                                                         
PNDKSTA  DS    CL5                 STATION SORT ONLY - STATION                  
PNDSTABK EQU   *-PNDKEY                                STAT BREAK               
PNDKOFF2 DS    CL2                 STATION SORT ONLY - OFFICE                   
PNDOFFBK EQU   *-PNDKEY                                OFF BREAK                
PNDKSLS2 DS    CL3                 STATION SORT ONLY - SLS                      
PNDSLBK2 EQU   *-PNDKEY                                SLS BREAK                
*                                                                               
PNDKID   DS    CL1                 X'01'/02/03/04  REPORT TYPE                  
PNDKIDID EQU   PNDKID-PNDKEY                                                    
PNDKFLDT DS    CL6                 START-END YMD                                
PNDKDST  DS    CL4                 DAYS TO START                                
         DS    CL3                 SPARE                                        
*                                                                               
PNDKCON  DS    CL8                 CONTRACT NUMBER - FOR UNIQUENESS             
         DS    XL(L'PNDKEY-(*-PNDKEY))                                          
*                                                                               
* COMMON DATA     ****************************************************          
         ORG   SRTDATA                                                          
PNDDATA  DS    0CL(L'SRTDATA)                                                   
PNDCON   DS    CL8                 CONTRACT NUMBER                              
PNDSLSNM DS    CL20                SALESPERSON NAME                             
PNDMRKT  DS    CL20                MARKET NAME                                  
PNDCREAT DS    CL3                 CREATION DATE YMD                            
PNDACTIV DS    CL3                 PENDING ACTIVITY DATE YMD                    
PNDDADV  DS    CL4                 ADVERTISER                                   
PNDADVN  DS    CL20                ADVERTISER NAME                              
PNDPROD  DS    CL20                PRODUCT                                      
PNDDAGY  DS    CL20                AGENCY                                       
PNDBUYER DS    CL20                BUYER                                        
PNDFLITE DS    CL6                 FLIGHT DATE START/END (YMD)                  
PNDDEMO  DS    CL11                DEMO                                         
PNDSVC   DS    CL1                 SERVICE                                      
PNDBOOK  DS    CL5                 BOOK                                         
PNDLEN   DS    CL12                SECONDS 6 AT 2 BYTES                         
PNDWEEKS DS    XL1                 WEEKS IN FLIGHT                              
PNDPROS  DS    XL1                 NUMBER OF PROPOSALS                          
PNDDOLS  DS    CL4                 BUDGET                                       
PNDCOMMT DS    CL65                COMMENT                                      
PNDSHAR  DS    XL1                 STATION SHARE GOAL                           
PNDDAYP  DS    CL8                 DAYPARTS                                     
PNDCNTY  DS    CL8                 CONTRACT TYPE                                
PNDCPP   DS    XL(8*4)             CPPS                                         
PNDECOM EQU    *                   END OF COMMON DATA                           
*                                                                               
* COMPLETED & LOSS DATA **********************************************          
         ORG   PNDECOM                                                          
COMSCALL DS    CL5                 COMPLETED CALL LETTERS                       
COMSAFF  DS    CL3                 COMPLETED STATION AFFILIATION                
COMSSHR  DS    CL4                 COMPLETED STATION % SHARE                    
COMSAMT  DS    PL16                COMPLETED STATION $ SHARE                    
COMLENE  EQU   *-COMSCALL                                                       
         DS    (NUMSTAS-1)XL(COMLENE)                                           
         DS    XL(L'PNDDATA-(*-PNDDATA))                                        
*                                                                               
** END OF SORTREC ****************************************************          
*                                                                               
**********************************************************************          
** TOTAL AREAS                                                                  
**********************************************************************          
CMPTOTS  EQU   *                                                                
CMPTWKS  DS    PL6                 TOTAL # OF WEEKS                             
CMPTPMKB DS    PL8                 PENDING MARKET BUDGET                        
CMPTPSTB DS    PL8                 PENDING STATION BUDGET TOTAL                 
CMPTPHDR DS    PL6                 PENDING HEADERS                              
CMPTPWKS DS    PL8                 PENDING # OF WEEKS                           
CMPTISTB DS    PL8                 INCOMPLETE STATION BUDGET TOTAL              
CMPTIHDR DS    PL6                 INCOMPLETE HEADERS                           
CMPTIWKS DS    PL8                 INCOMPLETE # OF WEEKS                        
CMPTCMKB DS    PL8                 COMPLETE MARKET BUDGET                       
CMPTCSTB DS    PL8                 COMPLETE STATION BUDGET TOTAL                
CMPTCHDR DS    PL6                 COMPLETE HEADERS                             
CMPTCWKS DS    PL8                 COMPLETE # OF WEEKS                          
CMPTLMKB DS    PL8                 LOSS MARKET BUDGET                           
CMPTLHDR DS    PL6                 LOSS HEADERS                                 
CMPTLWKS DS    PL8                 LOSS # OF WEEKS                              
*                                                                               
ISTATOTS DS    (NUMSTAS)PL6        INCOMPLETE STATION/COMP TOTALS               
CSTATOTS DS    (NUMSTAS)PL6        COMPLETED STATION/COMP TOTALS                
LSTATOTS DS    (NUMSTAS)PL6        LOSS STATION/COMP TOTALS                     
CMPTOTLN EQU   *-CMPTOTS                                                        
         DS    (NUMLVLS)XL(CMPTOTLN)                                            
CMPTOTEN EQU   *                                                                
*                                                                               
LINETOT  DS    PL6                                                              
STACOL1  DS    CL132              STATION/AFFILIATE HEADINGS                    
STACOL2  DS    CL132              STATION/AFFILIATE HEADINGS                    
*                                                                               
** END OF TOTALS *****************************************************          
*                                                                               
* -  CONSTANTS FOR TSAROFF                                                      
TSARBUFF DS    A                   ADDRESS OF GETMAIN BUFF                      
TSARBUFL DC    A(SRTLENE*15000)                                                 
*                                                                               
RELO     DS    A                                                                
MYCOUNT  DS    F                   TESTING                                      
*                                                                               
LVLIND   DS    X                                                                
TSAREA   DS    XL64                                                             
*                                                                               
MYTSREC  DS    0D                                                               
*                                                                               
MYTSKEY  DS    CL(L'SRTKEY)                                                     
MYTSKEYL EQU   *-MYTSKEY                                                        
MYTSDATA DS    CL(SRTEND-SRTDATA)                                               
MYTSRECL EQU   *-MYTSREC                                                        
*                                                                               
OFFLIST  DS    CL100               VALID OFFICES FOR REGION (2 X 50)            
         DC    X'00'                                                            
                                                                                
*********************************************************************           
* REPORT HEADLINE SPECS                                                         
*********************************************************************           
REGSPECS DS    0H                                                               
         SPROG 1,2,3,4                                                          
         PSPEC H1,89,AGYNAME                                                    
         PSPEC H2,89,RUN                                                        
         PSPEC H1,47,C'SALES ACTIVITY DETAIL REPORT'                            
         PSPEC H2,47,C'----- -------- ------ ------'                            
         PSPEC H3,89,REPORT                                                     
         PSPEC H3,89,REQUESTOR                                                  
         PSPEC H4,89,PAGE                                                       
         PSPEC H8,2,C'DAYS TO START        ADV                  FLIGHT X        
                             MKT BUDGET                  '                      
         PSPEC H9,2,C'CONTRACT             PROD                 DEMOS  X        
                             SHARE GOAL                  '                      
         PSPEC H10,2,C'CREATION DATE        AGY                  SVC/BOX        
               OK             STA. BUDGET                 '                     
         PSPEC H11,2,C'ACTIVITY DATE        BUYER                LEN   X        
                                                          '                     
         PSPEC H12,2,C'DPT                                             X        
                                                          '                     
         PSPEC H13,2,C'CPP                                             X        
                                                          '                     
         PSPEC H14,2,C'------------------------------------------------X        
               --------------------------------------------------------X        
               --------------------------'                                      
         DC    X'00'                                                            
         EJECT                                                                  
*********************************************************************           
* RECAP REPORT HEADLINE SPECS                                                   
*********************************************************************           
RECSPECS DS    0H                                                               
         SPROG 1,2,3,4                                                          
         PSPEC H1,89,AGYNAME                                                    
         PSPEC H2,89,RUN                                                        
         PSPEC H1,47,C'SALES ACTIVITY RECAP REPORT'                             
         PSPEC H2,47,C'----- -------- ----- ------'                             
         PSPEC H3,89,REPORT                                                     
         PSPEC H3,89,REQUESTOR                                                  
         PSPEC H4,89,PAGE                                                       
         PSPEC H9,2,C'-------------------------------------------------X        
               --------------------------------------------------------X        
               -------------------------'                                       
         DC    X'00'                                                            
         EJECT                                                                  
* OVERFLOW ROUTINES                                                             
         CSECT                                                                  
**********************************************************************          
* REPIOTBL INITIALIZED IN RESFM3A                                               
**********************************************************************          
         DS    0D                                                               
INIT     NTR1  BASE=*,LABEL=*                                                   
         LA    R5,REPIOTBL         FINISH INITIALIZING REPIO                    
         USING REPIOD,R5                                                        
         MVI   RIPSKIP,C'Y'        DON'T NEED PREVIOUS $                        
*                                  SO SET REPIO FLAG ACCORDINGLY                
*                                                                               
* GET DATE OF MONDAY PREVIOUS TO TODAY'S DATE                                   
         GOTO1 DATCON,DMCB,(3,BTODAY),(0,WORK)    TODAY'S DATE-YYMMDD           
         GOTO1 GETDAY,DMCB,WORK,WORK+6            RETURNS DAY OF WEEK           
         CLI   DMCB+4,X'40'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         ZIC   R2,DMCB           R2 =TODAY=1-7(DAY OF WEEK)                     
         BCTR  R2,0              NO OF DAYS DIFF BETWEEN TODAY-MONDAY           
         MH    R2,=H'-1'           MAKE NUM OF DAYS DIFF NEGATIVE               
         PRINT GEN                                                              
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R2)                                      
         PRINT NOGEN                                                            
         GOTO1 DATCON,DMCB,(0,WORK+6),(3,MONDAYDT)                              
*                                                                               
                                                                                
* - INITIALIZE TSAROFF                                                          
         PRINT GEN                                                              
         GOTO1 =V(LOADER),DMCB,=CL8'T00A7D',0    TSAROFF                        
         PRINT NOGEN                                                            
         MVC   ATSAROFF,DMCB+4                                                  
         OC    ATSAROFF,ATSAROFF                                                
         BNO   *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R0,TSARBUFL                                                      
         GETMAIN RU,LV=(0),LOC=(ANY,ANY)                                        
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R1,TSARBUFF                                                      
*                                                                               
         XC    TSAREA,TSAREA                                                    
         LA    R2,TSAREA                                                        
         USING TSARD,R2                                                         
*                                                                               
         MVI   TSOFFACT,TSAINI                                                  
         MVC   TSABUF,TSARBUFF                                                  
         MVC   TSAREC,TSARBUFL                                                  
         LA    R0,MYTSKEYL                                                      
         STC   R0,TSKEYL                                                        
         LA    R0,MYTSRECL                                                      
         STH   R0,TSRECL                                                        
         GOTO1 ATSAROFF,(R2)                                                    
*                                                                               
***      MVC   WORK(4),TSARBUFF                                                 
***      GOTO1 =V(PRNTBL),DMCB,=C'GTM',WORK,C'DUMP',4,=C'1D'                    
*                                                                               
         CLI   TSERRS,0                                                         
         BE    INIT6                                                            
         DC    H'0'                                                             
*                                                                               
INIT6    DS    0H                                                               
         L     R2,=A(OFFLIST)                                                   
         A     R2,RELO             GET OFFICE LIST                              
         XC    0(L'OFFLIST,R2),0(R2)                                            
         CLI   FLTREG,X'40'        ARE WE FILTERING ON REGION                   
         BNH   INIT10                                                           
         LA    R3,50                                                            
         LA    R6,KEY                                                           
         USING ROFFREC,R6                                                       
         XC    KEY,KEY                                                          
         MVI   ROFFKTYP,X'04'                                                   
         MVC   ROFFKREP,AGENCY                                                  
         GOTO1 HIGH                                                             
         B     INIT8                                                            
INIT7    GOTO1 SEQ                                                              
                                                                                
INIT8    CLC   KEY(25),KEYSAVE                                                  
         BNE   INIT10              THAT'S ALL                                   
         L     R6,AIO                                                           
         GOTO1 GETREC                                                           
         CLC   FLTREG,ROFFREG      REGIONS MATCH                                
         BNE   INIT7               NO                                           
         MVC   0(2,R2),ROFFKOFF    YES/SAVE OFFICE IN LIST                      
         LA    R2,2(R2)            BUMP LIST                                    
         BCT   R3,INIT7                                                         
         DC    H'0'                MAXED OUT                                    
                                                                                
INIT10   DS    0H                                                               
         XC    CONTYPEX,CONTYPEX                                                
         LA    R4,50               50=MAX NUMBER OF CON TYPES                   
         MVC   AIO,AIO2                                                         
         LA    R2,CONTYPEX         LOAD CON TYPE TABLE                          
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RCTYKEY,R6                                                       
         MVI   RCTYKTYP,RCTYKTYQ                                                
         MVC   RCTYKREP,RIPREP                                                  
         GOTO1 HIGH                                                             
INIT12   CLC   KEY(26),KEYSAVE                                                  
         BNE   INIT19                                                           
         L     R6,AIO                                                           
         GOTO1 GETREC                                                           
         LA    R6,RCTYELEM                                                      
INIT13   CLI   0(R6),X'10'                                                      
         BE    INIT14                                                           
         ZIC   R1,1(R6)                                                         
         LTR   R1,R1                                                            
         BZ    INIT18                                                           
         AR    R6,R1                                                            
         B     INIT13                                                           
         USING RCTYFEL,R6                                                       
INIT14   TM    RCTYFPRC,X'20'      CONTRACT TYPE BIT ON                         
         BNO   INIT18                                                           
         MVC   0(1,R2),KEY+26      SET CONTRACT TYPE                            
         LA    R2,1(R2)                                                         
INIT17   BCT   R4,INIT18                                                        
         DC    H'0'                NEED TO EXPAND TABLE                         
INIT18   GOTO1 SEQ                                                              
         B     INIT12                                                           
*                                                                               
INIT19   MVC   AIO,AIO1                                                         
*                                                                               
INIT20   DS    0H                                                               
*                                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* - CALCULATE TOTAL $ AMOUNT FROM TOTAL $ BUCKETS AND ANY ONE STA %             
* FULL HAS TOTAL BUCKET $                                                       
* 5(R2) POINTS TO 1ST STATION %                                                 
* (TOTAL BUCKET $) / (1ST STATION %) = TOTAL $ AMT                              
* RETURNS TOTAL $ AMT IN FULL                                                   
* R3 HAS MAX NUMBER OF STATIONS                                                 
***********************************************************************         
CALCAMT  NTR1  LABEL=*,BASE=*                                                   
         L     R1,FULL                                                          
         CVD   R1,DUB                                                           
         ZAP   WORK(12),DUB        $                                            
*        MP    WORK(12),=P'100'                                                 
         MP    WORK(12),=P'1000'                                                
                                                                                
CLC10    ICM   R1,15,5(R2)         %                                            
         LTR   R1,R1                                                            
         BNZ   CLC12                                                            
         LA    R2,9(R2)            BUMP TO NXT STA /  % MINI ELELENT            
         BCT   R3,CLC10                                                         
         B     CLC14               NO % - SPL ELEM BUT NO %                     
CLC12    CVD   R1,DUB                                                           
         DP    WORK(12),DUB+4(4)                                                
         SRP   WORK(8),64-1,0      ROUND UP                                     
         ZAP   DUB,WORK(8)                                                      
         CVB   R1,DUB                                                           
CLC14    ST    R1,FULL                                                          
CLCX     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* EXPECTS DEMO CODE IN WORK                                                     
* RETURNS DEMO NAME IN WORK                                                     
***********************************************************************         
         DS    0D                                                               
XGETDEM  NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
         NI    WORK,X'FF'-X'40'    CLEAR PRIMARY DEMO FLAGX'40'                 
         XC    WORK+10(15),WORK+10       CLEAR OUTPUT AREA                      
                                                                                
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'R'                                                    
         CLI   RCONKSTA+4,C'A'                                                  
         BE    *+16                                                             
         CLI   RCONKSTA+4,C'F'                                                  
         BE    *+8                                                              
         MVI   DBSELMED,C'T'                                                    
                                                                                
         L     RF,CALLOV                GET DEMOCON                             
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(4),=X'D9000AE0'                                           
         LA    R1,DMCB                                                          
         BASR  RE,RF                                                            
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(0,WORK),(6,WORK+10),(0,DBLOCK),0                      
         CLI   DMCB+4,0                                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   WORK(11),WORK+10                                                 
         B     EXIT                                                             
         DROP  R6                                                               
         SPACE 3                                                                
DBLOK    DS    0H                                                               
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* COUNT THE NUMBER FO PROPSALS                                                  
**********************************************************************          
         DS    0D                                                               
COUNTPRO NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
         SR    R2,R2                                                            
         XC    KEY,KEY                                                          
K        USING RPROKEY,KEY                                                      
         ZAP   WORK+10(5),=P'0'                                                 
         MVO   WORK+10(5),RCONKCON                                              
         ZAP   WORK(5),=P'99999999'                                             
         SP    WORK(5),WORK+10(5)                                               
         MVO   WORK+10(5),WORK(5)                                               
*                                                                               
         MVI   K.RPROKTYP,RPROKTYQ                                              
         MVI   K.RPROKSTY,RPROKSBQ                                              
         MVC   K.RPROKRCD,RCONKREP                                              
         MVC   K.RPROKCON,WORK+10                                               
*                                                                               
         MVC   KEYSAVE,KEY                                                      
CPRO10   DS    0H                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(RPROKPRO-RPROKEY),KEYSAVE                                    
         BNE   CPROX                                                            
         LA    R2,1(R2)                                                         
         ZIC   RE,K.RPROKPRO                                                    
         LA    RE,1(RE)                                                         
         CLM   RE,1,=X'00'                                                      
         BE    CPROX                                                            
         STC   RE,K.RPROKPRO                                                    
         B     CPRO10                                                           
*                                                                               
CPROX    DS    0H                                                               
         STC   R2,BYTE                                                          
         B     EXIT                                                             
         DROP  K                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* PRINT TOTALS                                                                  
**********************************************************************          
PRTTOTS  NTR1  BASE=*,LABEL=*                                                   
         MVC   P,SPACES            CLEAR IN CASE OF RECAP                       
         MVC   P2,SPACES                                                        
         MVC   P3,SPACES                                                        
         MVC   P4,SPACES                                                        
*                                                                               
         MVI   BYTE,0              FOR FLAGS                                    
*                                   - X'80' DO COMPETITIVE TOTALS               
*                                   - X'40' FORCE PAGEBREAK ON EXIT             
*                                   - X'20' COMPETITIVE TOTALS PRINTED          
*                                   - X'10' MIDLINES WAITING                    
*                                                                               
         TM    TOTALS,TTLQONLY     DO TOTAL MIDLINE?                            
         BO    PTOT010             NO                                           
*                                                                               
         OI    BYTE,X'40'+X'10'    FORCE PAGE BREAK                             
*                                                                               
         MVI   MIDSV,0                                                          
         MVI   ALLOWLIN,12                                                      
         BAS   RE,SPLAT                                                         
         BAS   RE,SPLAT                                                         
         MVI   P,C'*'                                                           
         MVC   P+1(61),P                                                        
         MVC   P+63(6),=C'TOTALS'                                               
         MVC   P+70(L'P-71),P                                                   
         MVI   P,C' '                                                           
*                                                                               
         MVC   MIDSV,SPACES                                                     
         MVI   MIDSV,C'*'                                                       
         MVC   MIDSV+1(35),MIDSV                                                
         MVC   MIDSV+37(6),=C'TOTALS'                                           
         MVC   MIDSV+44(L'MIDSV-44),MIDSV                                       
*                                                                               
PTOT010  LA    R5,REPIOTBL                                                      
         USING REPIOD,R5                                                        
***********                                                                     
* LEVEL 1 *                                                                     
***********                                                                     
         CLI   LVLIND,1            PRINT LEVEL 1 TOTALS?                        
         BL    PTOTX               NO - SHOULD NEVER REALLY TAKE THIS           
         TM    TOTALS,TTLQLVL1     PRINTING THIS LEVEL?                         
         BZ    PTOT1L30            NO                                           
*                                                                               
         TM    BYTE,X'10'          MIDLINES?                                    
         BNO   *+12                                                             
         NI    BYTE,X'FF'-X'10'                                                 
         BAS   RE,SPLAT                                                         
*                                                                               
         LA    R3,P                                                             
         MVC   0(4,R3),=CL04'*** '                                              
         LA    R3,04(R3)                                                        
*                                                                               
         TM    REPORT,RPTQSTA      STATION SORT ?                               
         BO    PTOT1L10            YES                                          
*                                                                               
         OI    BYTE,X'80'          DO COMPETITV BLOCK                           
         MVC   0(09,R3),=CL09'STATION: '                                        
         LA    R3,09(R3)                                                        
*                                                                               
         MVC   0(L'THISSTA,R3),THISSTA                                          
         MVI   L'THISSTA+1(R3),C'-'                                             
         MVC   L'THISSTA+3(L'THISMRKT,R3),THISMRKT                              
         B     PTOT1L20                                                         
*                                                                               
PTOT1L10 DS    0H                                                               
         OI    BYTE,X'80'          DO COMPETITV BLOCK                           
         MVC   0(13,R3),=CL13'SALESPERSON: '                                    
         LA    R3,13(R3)                                                        
*                                                                               
         MVC   0(L'THISLSNM-8,R3),THISLSNM+4                                    
*                                                                               
PTOT1L20 DS    0H                                                               
         TM    BYTE,X'80'                                                       
         BZ    PTOT1L22                                                         
*                                                                               
         GOTO1 =A(DOLVLTOT),DMCB,1,1,RR=RELO                                    
         NI    BYTE,X'FF'-X'80'                                                 
*                                                                               
PTOT1L22 DS    0H                                                               
         GOTO1 =A(DOLVLTOT),DMCB,1,0,RR=RELO                                    
         GOTO1 =A(CLEARLVL),1,RR=RELO                                           
         BAS   RE,SPLAT                                                         
*                                                                               
PTOT1L30 DS    0H                                                               
***********                                                                     
* LEVEL 2 *                                                                     
***********                                                                     
PTOT2L   DS    0H                                                               
         CLI   LVLIND,2            PRINT LEVEL 2 TOTALS?                        
         BL    PTOTX               NO                                           
         TM    TOTALS,TTLQLVL2     PRINTING THIS LEVEL?                         
         BZ    PTOT2L30            NO                                           
*                                                                               
         TM    BYTE,X'10'          MIDLINES?                                    
         BNO   *+12                                                             
         NI    BYTE,X'FF'-X'10'                                                 
         BAS   RE,SPLAT                                                         
*                                                                               
         LA    R3,P                                                             
         TM    TOTALS,TTLQLVL1     PRINTING PREVIOUS LEVELS?                    
         BZ    PTOT2L0             NOT PRINTINT PREVIOUS LEVELS                 
*                                                                               
         BAS   RE,SPLAT            SOME BLANK LINES                             
         BAS   RE,SPLAT                                                         
*                                                                               
         OI    BYTE,X'40'                                                       
PTOT2L0  DS    0H                                                               
         MVC   0(4,R3),=CL04'*** '                                              
         LA    R3,04(R3)                                                        
*                                                                               
         TM    REPORT,RPTQSTA      STATION SORT ?                               
         BO    PTOT2L10            YES                                          
*                                                                               
         TM    TOTALS,TTLQONLY     RECAP?                                       
         BO    PTOT2L02            YES                                          
         OC    RIPSTA,RIPSTA       FILTER BY STATION?                           
         BZ    PTOT2L02            NO                                           
         OI    BYTE,X'80'          DO COMPETITV BLOCK                           
*                                                                               
PTOT2L02 DS    0H                                                               
         MVC   0(13,R3),=CL13'SALESPERSON: '                                    
         LA    R3,13(R3)                                                        
*                                                                               
         MVC   0(L'THISLSNM-4,R3),THISLSNM+4                                    
         B     PTOT2L20                                                         
*                                                                               
PTOT2L10 DS    0H                                                               
         OI    BYTE,X'80'                                                       
         MVC   0(08,R3),=CL08'OFFICE: '                                         
         LA    R3,08(R3)                                                        
*                                                                               
         MVC   0(L'THISOFF,R3),THISOFF                                          
*                                                                               
PTOT2L20 DS    0H                                                               
         TM    BYTE,X'80'                                                       
         BZ    PTOT2L22                                                         
*                                                                               
         GOTO1 =A(DOLVLTOT),DMCB,2,1,RR=RELO                                    
         NI    BYTE,X'FF'-X'80'                                                 
*                                                                               
PTOT2L22 DS    0H                                                               
         GOTO1 =A(DOLVLTOT),DMCB,2,0,RR=RELO                                    
         GOTO1 =A(CLEARLVL),2,RR=RELO                                           
         BAS   RE,SPLAT                                                         
*                                                                               
PTOT2L30 DS    0H                                                               
***********                                                                     
* LEVEL 3 *                                                                     
***********                                                                     
PTOT3L   DS    0H                                                               
         CLI   LVLIND,3            PRINT LEVEL 3 TOTALS?                        
         BL    PTOTX               NO                                           
         TM    TOTALS,TTLQLVL3     PRINTING THIS LEVEL?                         
         BZ    PTOT3L30            NO                                           
*                                                                               
         TM    BYTE,X'10'          MIDLINES?                                    
         BNO   *+12                                                             
         NI    BYTE,X'FF'-X'10'                                                 
         BAS   RE,SPLAT                                                         
*                                                                               
         LA    R3,P                                                             
         TM    TOTALS,TTLQLVL1+TTLQLVL2                                         
         BZ    PTOT3L0             NOT PRINTINT PREVIOUS LEVELS                 
*                                                                               
         BAS   RE,SPLAT            SOME BLANK LINES                             
         BAS   RE,SPLAT                                                         
*                                                                               
         OI    BYTE,X'40'                                                       
PTOT3L0  DS    0H                                                               
         MVC   0(4,R3),=CL04'*** '                                              
         LA    R3,04(R3)                                                        
*                                                                               
         TM    REPORT,RPTQSTA      STATION SORT ?                               
         BO    PTOT3L10            YES                                          
*                                                                               
         TM    TOTALS,TTLQALL      ALL TOTALS?                                  
         BNO   PTOT3L02            NO - PRINT ALL REQUESTED TOTALS              
*                                                                               
         OC    RIPSAL,RIPSAL       FILTER ON SAL                                
         BNZ   PTOT3L30            YES - SKIP                                   
*                                                                               
PTOT3L02 DS    0H                                                               
         TM    TOTALS,TTLQONLY     RECAP?                                       
         BO    PTOT3L04            YES                                          
         OC    RIPSTA,RIPSTA       FILTER BY STATION?                           
         BZ    PTOT3L04            NO                                           
         OI    BYTE,X'80'          DO COMPETITV BLOCK                           
*                                                                               
PTOT3L04 DS    0H                                                               
         MVC   0(06,R3),=CL06'TEAM: '                                           
         LA    R3,06(R3)                                                        
*                                                                               
         MVC   0(L'THISTEAM,R3),THISTEAM                                        
         B     PTOT3L20                                                         
*                                                                               
PTOT3L10 DS    0H                                                               
         OI    BYTE,X'80'                                                       
         MVC   0(09,R3),=CL09'STATIO?: '                                        
         LA    R3,09(R3)                                                        
*                                                                               
         MVC   0(L'THISSTA,R3),THISSTA                                          
         MVI   L'THISSTA+1(R3),C'-'                                             
         MVC   L'THISSTA+3(L'THISMRKT,R3),THISMRKT                              
*                                                                               
PTOT3L20 DS    0H                                                               
         TM    BYTE,X'80'                                                       
         BZ    PTOT3L22                                                         
*                                                                               
         GOTO1 =A(DOLVLTOT),DMCB,3,1,RR=RELO                                    
         NI    BYTE,X'FF'-X'80'                                                 
*                                                                               
PTOT3L22 DS    0H                                                               
         GOTO1 =A(DOLVLTOT),DMCB,3,0,RR=RELO                                    
         GOTO1 =A(CLEARLVL),3,RR=RELO                                           
         BAS   RE,SPLAT                                                         
*                                                                               
PTOT3L30 DS    0H                                                               
***********                                                                     
* LEVEL 4 *                                                                     
***********                                                                     
PTOT4L   CLI   LVLIND,4            PRINT LEVEL 4 TOTALS?                        
         BL    PTOTX               NO                                           
         TM    TOTALS,TTLQLVL4     PRINTING THIS LEVEL?                         
         BZ    PTOT4L30            NO                                           
         TM    REPORT,RPTQSTA      STATION SORT ?                               
         BO    PTOT4L30            YES - NO TOTALS AT THIS LEVEL                
*                                                                               
         TM    BYTE,X'10'          MIDLINES?                                    
         BNO   *+12                                                             
         NI    BYTE,X'FF'-X'10'                                                 
         BAS   RE,SPLAT                                                         
*                                                                               
         LA    R3,P                                                             
         TM    TOTALS,TTLQLVL1+TTLQLVL2+TTLQLVL3                                
         BZ    PTOT4L0             NOT PRINTINT PREVIOUS LEVELS                 
*                                                                               
         BAS   RE,SPLAT            SOME BLANK LINES                             
         BAS   RE,SPLAT                                                         
*                                                                               
         OI    BYTE,X'40'                                                       
PTOT4L0  DS    0H                                                               
         MVC   0(4,R3),=CL04'*** '                                              
         LA    R3,04(R3)                                                        
*                                                                               
         TM    TOTALS,TTLQALL      ALL TOTALS?                                  
         BNO   PTOT4L02            NO - PRINT ALL REQUESTED TOTALS              
*                                                                               
         OC    RIPSAL,RIPSAL       FILTER ON SAL                                
         BNZ   PTOT4L30            YES - SKIP                                   
         OC    RIPTEAM,RIPTEAM     FILTER ON TEAM?                              
         BNZ   PTOT4L30            YES - SKIP                                   
*                                                                               
PTOT4L02 DS    0H                                                               
         TM    TOTALS,TTLQONLY     RECAP?                                       
         BO    PTOT4L04            YES                                          
         OC    RIPSTA,RIPSTA       FILTER BY STATION?                           
         BZ    PTOT4L04            NO                                           
         OI    BYTE,X'80'          DO COMPETITV BLOCK                           
*                                                                               
PTOT4L04 DS    0H                                                               
         MVC   0(08,R3),=CL20'OFFICE: '                                         
         LA    R3,08(R3)                                                        
*                                                                               
         MVC   0(L'THISOFF,R3),THISOFF                                          
*                                                                               
PTOT4L20 DS    0H                                                               
         TM    BYTE,X'80'                                                       
         BZ    PTOT4L22                                                         
*                                                                               
         GOTO1 =A(DOLVLTOT),DMCB,4,1,RR=RELO                                    
         NI    BYTE,X'FF'-X'80'                                                 
*                                                                               
PTOT4L22 DS    0H                                                               
         GOTO1 =A(DOLVLTOT),DMCB,4,0,RR=RELO                                    
         GOTO1 =A(CLEARLVL),4,RR=RELO                                           
         BAS   RE,SPLAT                                                         
*                                                                               
PTOT4L30 DS    0H                                                               
***********                                                                     
* LEVEL 5 *                                                                     
***********                                                                     
PTOT5L   CLI   LVLIND,5            PRINT LEVEL 5 TOTALS?                        
         BL    PTOTX               NO                                           
         TM    TOTALS,TTLQLVLC     PRINTING THIS LEVEL?                         
         BZ    PTOT5L30            NO                                           
*                                                                               
         TM    BYTE,X'10'          MIDLINES?                                    
         BNO   *+12                                                             
         NI    BYTE,X'FF'-X'10'                                                 
         BAS   RE,SPLAT                                                         
*                                                                               
         LA    R3,P                                                             
         OC    RIPOFF,RIPOFF       FILTER ON OFFICE                             
         BNZ   PTOT5L30            YES - SKIP                                   
         OC    RIPSAL,RIPSAL       FILTER ON SAL                                
         BNZ   PTOT5L30            YES - SKIP                                   
         OC    RIPTEAM,RIPTEAM     FILTER ON TEAM?                              
         BNZ   PTOT5L30            YES - SKIP                                   
*                                                                               
         TM    REPORT,RPTQSTA      STATION SORT ?                               
         BNO   PTOT5L10            NO                                           
*                                                                               
         OC    RIPSTA,RIPSTA       FILTER BY STATION?                           
         BNZ   PTOT5L30            YES - NO COMPANY TOTALS                      
*                                                                               
PTOT5L10 DS    0H                                                               
         BAS   RE,SPLAT            SOME BLANK LINES                             
         BAS   RE,SPLAT                                                         
*                                                                               
         MVC   0(4,R3),=CL04'*** '                                              
         LA    R3,04(R3)                                                        
         MVC   0(12,R3),=CL12'TOTALS FOR:'                                      
         LA    R3,13(R3)                                                        
         MVC   0(L'USERNAME,R3),USERNAME                                        
*                                                                               
PTOT5L20 DS    0H                                                               
         TM    BYTE,X'80'                                                       
         BZ    PTOT5L22                                                         
*                                                                               
         GOTO1 =A(DOLVLTOT),DMCB,5,1,RR=RELO                                    
         NI    BYTE,X'FF'-X'80'                                                 
*                                                                               
PTOT5L22 DS    0H                                                               
         GOTO1 =A(DOLVLTOT),DMCB,5,0,RR=RELO                                    
         GOTO1 =A(CLEARLVL),5,RR=RELO                                           
         BAS   RE,SPLAT                                                         
*                                                                               
PTOT5L30 DS    0H                                                               
*                                                                               
PTOTX    DS    0H                                                               
         MVC   P,SPACES                                                         
         MVI   ALLOWLIN,0                                                       
*                                                                               
         TM    BYTE,X'40'          PAGE BREAK?                                  
         BNO   NO                  NO                                           
YES      CR    RB,RB                                                            
         B     EXIT                                                             
NO       LTR   RB,RB                                                            
         B     EXIT                                                             
         DROP  R5                                                               
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* DO TOTALS AT LEVEL INDICATED                                                  
**********************************************************************          
DOLVLTOT NTR1  BASE=*,LABEL=*                                                   
         LR    R2,R1                                                            
         ZIC   R3,3(R2)                                                         
         MH    R3,=Y(CMPTOTLN)                                                  
         LA    R3,CMPTOTS(R3)                                                   
X        USING CMPTOTS,R3                                                       
*                                                                               
         CLI   7(R2),1             COMPETITIVE TOTALS?                          
         BNE   DOLVL100            NO                                           
*************************                                                       
* COMETITVE TOTAL BLOCK *                                                       
*************************                                                       
         ZAP   CMPTCHDR,X.CMPTCHDR                                              
         AP    CMPTCHDR,X.CMPTLHDR                                              
         CP    CMPTCHDR,=P'0'                                                   
         BE    DOLVLX              NOTHING WORTH DOING                          
*                                                                               
         OI    BYTE,X'20'         PRINTED COMPETITIVE TOTALS                    
*                                                                               
         MVI   ALLOWLIN,6                                                       
         MVC   P2(20),=CL20'COMPETITIVE TOTALS'                                 
         MVC   P3,STACOL1                                                       
         MVC   P4,STACOL2                                                       
         BAS   RE,SPLAT                                                         
*                                                                               
         LA    R4,P+(PCOMCALL-PENDINGD)                                         
         LA    R5,X.CSTATOTS                                                    
         LA    R6,X.LSTATOTS                                                    
         LA    R2,STACOL1+(PCOMCALL-PENDINGD)                                   
         ZAP   CMPTCMKB,X.CMPTCMKB                                              
         AP    CMPTCMKB,X.CMPTLMKB                                              
         LA    RF,NUMSTAS                                                       
         XC    WORK,WORK                                                        
DOLVL002 DS    0H                                                               
         ZAP   CSTATOTS,0(L'CSTATOTS,R5)                                        
         AP    CSTATOTS,0(L'LSTATOTS,R6)                                        
*                                                                               
         CLC   0(10,R2),SPACES     ANY STATION HEADING?                         
         BNH   DOLVL04             NO                                           
*                                                                               
         EDIT  CSTATOTS,(09,0(R4)),ZERO=NOBLANK,ALIGN=LEFT                      
         ZAP   WORK+20(16),CSTATOTS                                             
         MP    WORK+20(16),=P'10000'                                            
         CP    CMPTCMKB,=P'0'                                                   
         BE    *+14                                                             
         DP    WORK+20(16),CMPTCMKB                                             
         B     *+10                                                             
         ZAP   WORK+20(8),=P'0'                                                 
         TM    REPORT,RPTQRND                                                   
         BZ    DOLVL03                                                          
*                                                                               
         SRP   WORK+20(8),64-2,5                                                
         EDIT  (P8,WORK+20),(09,132(R4)),TRAIL=C'%',ALIGN=LEFT                  
         B     DOLVL04                                                          
*                                                                               
DOLVL03  DS    0H                                                               
         EDIT  (P8,WORK+20),(09,132(R4)),2,TRAIL=C'%',ALIGN=LEFT                
*                                                                               
DOLVL04  DS    0H                                                               
         LA    R5,L'CSTATOTS(R5)                                                
         LA    R6,L'LSTATOTS(R6)                                                
         LA    R4,10(R4)                                                        
         LA    R2,10(R2)                                                        
         BCT   RF,DOLVL002                                                      
*                                                                               
         EDIT  CMPTCMKB,(09,0(R4)),ZERO=NOBLANK,ALIGN=LEFT                      
         BAS   RE,SPLAT                                                         
*                                                                               
         B     DOLVLX                                                           
***************                                                                 
* TOTAL BLOCK *                                                                 
***************                                                                 
DOLVL100 DS    0H                                                               
         MVI   ALLOWLIN,8                                                       
         TM    BYTE,X'20'          NEED HEADING?                                
         BNO   *+10                NO                                           
         MVC   P2(20),=CL20'SECTION TOTALS'                                     
*                                                                               
         BAS   RE,SPLAT                                                         
         NI    BYTE,X'FF'-X'20'                                                 
*                                                                               
         LA    R4,P+20                                                          
         TM    REPORT,RPTQPND      PENDING SECTION?                             
         BNO   *+14                                                             
         MVC   0(10,R4),=CL10'PENDING'                                          
         LA    R4,22(R4)                                                        
*                                                                               
         TM    REPORT,RPTQINC      INCOMPLETE SECTION?                          
         BNO   *+14                                                             
         MVC   0(10,R4),=CL10'INCOMPLETE'                                       
         LA    R4,22(R4)                                                        
*                                                                               
         TM    REPORT,RPTQCPL      COMPLETE SECTION?                            
         BNO   *+14                                                             
         MVC   0(10,R4),=CL10'COMPLETE'                                         
         LA    R4,22(R4)                                                        
*                                                                               
         TM    REPORT,RPTQLOS      LOSS SECTION?                                
         BNO   *+14                                                             
         MVC   0(10,R4),=CL10'LOSS'                                             
         LA    R4,22(R4)                                                        
*                                                                               
         TM    REPORT,RPTQCPL+RPTQLOS    COMPLETES AND LOSSES?                  
         BNO   *+14                                                             
         MVC   0(10,R4),=CL10'COMP+LOSS'                                        
         LA    R4,22(R4)                                                        
*                                                                               
         BAS   RE,SPLAT                                                         
*                                                                               
         LA    R4,P                                                             
         MVC   000(15,R4),=CL15'# OF HEADERS'                                   
         MVC   132(15,R4),=CL15'STATION DOLLARS'                                
         MVC   264(15,R4),=CL15'SHARE'                                          
         MVC   396(15,R4),=CL15'MARKET DOLLARS'                                 
         LA    R4,P+20                                                          
*                                                                               
         TM    REPORT,RPTQPND      PENDING SECTION?                             
         BNO   DOLVL110                                                         
*                                                                               
         EDIT  X.CMPTPHDR,(10,0(R4)),ZERO=NOBLANK,ALIGN=LEFT                    
         EDIT  X.CMPTPSTB,(10,132(R4)),ZERO=NOBLANK,ALIGN=LEFT                  
         ZAP   WORK+20(16),X.CMPTPSTB                                           
         MP    WORK+20(16),=P'10000'                                            
         CP    X.CMPTPMKB,=P'0'                                                 
         BE    *+14                                                             
         DP    WORK+20(16),X.CMPTPMKB                                           
         B     *+10                                                             
         ZAP   WORK+20(8),=P'0'                                                 
         TM    REPORT,RPTQRND                                                   
         BZ    DOLVL106                                                         
*                                                                               
         SRP   WORK+20(8),64-2,5                                                
         EDIT  (P8,WORK+20),(10,264(R4)),TRAIL=C'%',ALIGN=LEFT                  
         B     DOLVL108                                                         
*                                                                               
DOLVL106 DS    0H                                                               
         EDIT  (P8,WORK+20),(10,264(R4)),2,TRAIL=C'%',ALIGN=LEFT                
*                                                                               
DOLVL108 DS    0H                                                               
         EDIT  X.CMPTPMKB,(10,396(R4)),ZERO=NOBLANK,ALIGN=LEFT                  
         LA    R4,22(R4)                                                        
DOLVL110 DS    0H                                                               
*                                                                               
         TM    REPORT,RPTQINC      INCOMPLETE SECTION?                          
         BNO   DOLVL120                                                         
*                                                                               
         EDIT  X.CMPTIHDR,(10,0(R4)),ZERO=NOBLANK,ALIGN=LEFT                    
         EDIT  X.CMPTISTB,(10,132(R4)),ZERO=NOBLANK,ALIGN=LEFT                  
         LA    R4,22(R4)                                                        
DOLVL120 DS    0H                                                               
*                                                                               
         TM    REPORT,RPTQCPL      COMPLETE SECTION?                            
         BNO   DOLVL130                                                         
*                                                                               
         EDIT  X.CMPTCHDR,(10,0(R4)),ZERO=NOBLANK,ALIGN=LEFT                    
         EDIT  X.CMPTCSTB,(10,132(R4)),ZERO=NOBLANK,ALIGN=LEFT                  
         ZAP   WORK+20(16),X.CMPTCSTB                                           
         MP    WORK+20(16),=P'10000'                                            
         CP    X.CMPTCMKB,=P'0'                                                 
         BE    *+14                                                             
         DP    WORK+20(16),X.CMPTCMKB                                           
         B     *+10                                                             
         ZAP   WORK+20(8),=P'0'                                                 
         TM    REPORT,RPTQRND                                                   
         BZ    DOLVL126                                                         
*                                                                               
         SRP   WORK+20(8),64-2,5                                                
         EDIT  (P8,WORK+20),(10,264(R4)),TRAIL=C'%',ALIGN=LEFT                  
         B     DOLVL128                                                         
*                                                                               
DOLVL126 DS    0H                                                               
         EDIT  (P8,WORK+20),(10,264(R4)),2,TRAIL=C'%',ALIGN=LEFT                
*                                                                               
DOLVL128 DS    0H                                                               
         EDIT  X.CMPTCMKB,(10,396(R4)),ZERO=NOBLANK,ALIGN=LEFT                  
         LA    R4,22(R4)                                                        
DOLVL130 DS    0H                                                               
*                                                                               
         TM    REPORT,RPTQLOS      LOSS SECTION?                                
         BNO   DOLVL140                                                         
*                                                                               
         EDIT  X.CMPTLHDR,(10,0(R4)),ZERO=NOBLANK,ALIGN=LEFT                    
         EDIT  X.CMPTLMKB,(10,396(R4)),ZERO=NOBLANK,ALIGN=LEFT                  
         LA    R4,22(R4)                                                        
DOLVL140 DS    0H                                                               
*                                                                               
         TM    REPORT,RPTQLOS+RPTQCPL    COMPLETES AND LOSSES?                  
         BNO   DOLVL150                                                         
*                                                                               
         ZAP   CMPTCHDR,X.CMPTCHDR                                              
         AP    CMPTCHDR,X.CMPTLHDR                                              
         EDIT  CMPTCHDR,(10,0(R4)),ZERO=NOBLANK,ALIGN=LEFT                      
         EDIT  X.CMPTCSTB,(10,132(R4)),ZERO=NOBLANK,ALIGN=LEFT                  
         ZAP   CMPTCMKB,X.CMPTCMKB                                              
         AP    CMPTCMKB,X.CMPTLMKB                                              
         ZAP   WORK+20(16),X.CMPTCSTB                                           
         MP    WORK+20(16),=P'10000'                                            
         CP    CMPTCMKB,=P'0'                                                   
         BE    *+14                                                             
         DP    WORK+20(16),CMPTCMKB                                             
         B     *+10                                                             
         ZAP   WORK+20(8),=P'0'                                                 
         TM    REPORT,RPTQRND                                                   
         BZ    DOLVL146                                                         
*                                                                               
         SRP   WORK+20(8),64-2,5                                                
         EDIT  (P8,WORK+20),(10,264(R4)),TRAIL=C'%',ALIGN=LEFT                  
         B     DOLVL148                                                         
*                                                                               
DOLVL146 DS    0H                                                               
         EDIT  (P8,WORK+20),(10,264(R4)),2,TRAIL=C'%',ALIGN=LEFT                
*                                                                               
DOLVL148 DS    0H                                                               
         EDIT  CMPTCMKB,(10,396(R4)),ZERO=NOBLANK,ALIGN=LEFT                    
         LA    R4,22(R4)                                                        
DOLVL150 DS    0H                                                               
*                                                                               
         BAS   RE,SPLAT                                                         
         TM    TOTALS,TTLQWKS      DO WEEKLY TOTALS?                            
         BZ    DOLVL210                                                         
*                                                                               
         LA    R4,P                                                             
         MVC   000(15,R4),=CL15'# OF WEEKS'                                     
         MVC   132(15,R4),=CL15'DOLLARS/WEEK'                                   
         LA    R4,P+20                                                          
*                                                                               
         TM    REPORT,RPTQPND      PENDING SECTION?                             
         BNO   DOLVL160                                                         
*                                                                               
         EDIT  X.CMPTPWKS,(10,0(R4)),ALIGN=LEFT,ZERO=NOBLANK                    
         CP    X.CMPTPSTB,=P'0'                                                 
         BE    DOLVL159                                                         
         CP    X.CMPTPWKS,=P'0'                                                 
         BE    DOLVL159                                                         
         ZAP   WORK+20(16),X.CMPTPSTB                                           
         MP    WORK+20(16),=P'100'                                              
         DP    WORK+20(16),X.CMPTPWKS                                           
         SRP   WORK+20(8),64-2,5                                                
         EDIT  (P8,WORK+20),(10,132(R4)),ALIGN=LEFT                             
DOLVL159 DS    0H                                                               
         LA    R4,22(R4)                                                        
DOLVL160 DS    0H                                                               
*                                                                               
         TM    REPORT,RPTQINC      INCOMPLETE SECTION?                          
         BNO   DOLVL170                                                         
*                                                                               
         EDIT  X.CMPTIWKS,(10,0(R4)),ALIGN=LEFT,ZERO=NOBLANK                    
         CP    X.CMPTISTB,=P'0'                                                 
         BE    DOLVL169                                                         
         CP    X.CMPTIWKS,=P'0'                                                 
         BE    DOLVL169                                                         
         ZAP   WORK+20(16),X.CMPTISTB                                           
         MP    WORK+20(16),=P'100'                                              
         DP    WORK+20(16),X.CMPTIWKS                                           
         SRP   WORK+20(8),64-2,5                                                
         EDIT  (P8,WORK+20),(10,132(R4)),ALIGN=LEFT                             
DOLVL169 DS    0H                                                               
         LA    R4,22(R4)                                                        
DOLVL170 DS    0H                                                               
*                                                                               
         TM    REPORT,RPTQCPL      COMPLETE SECTION?                            
         BNO   DOLVL180                                                         
*                                                                               
         EDIT  X.CMPTCWKS,(10,0(R4)),ALIGN=LEFT,ZERO=NOBLANK                    
         CP    X.CMPTCSTB,=P'0'                                                 
         BE    DOLVL179                                                         
         CP    X.CMPTCWKS,=P'0'                                                 
         BE    DOLVL179                                                         
         ZAP   WORK+20(16),X.CMPTCSTB                                           
         MP    WORK+20(16),=P'100'                                              
         DP    WORK+20(16),X.CMPTCWKS                                           
         SRP   WORK+20(8),64-2,5                                                
         EDIT  (P8,WORK+20),(10,132(R4)),ALIGN=LEFT                             
DOLVL179 DS    0H                                                               
         LA    R4,22(R4)                                                        
DOLVL180 DS    0H                                                               
*                                                                               
         TM    REPORT,RPTQLOS      LOSS SECTION?                                
         BNO   DOLVL190                                                         
*                                                                               
         EDIT  X.CMPTLWKS,(10,0(R4)),ALIGN=LEFT,ZERO=NOBLANK                    
         LA    R4,22(R4)                                                        
DOLVL190 DS    0H                                                               
*                                                                               
         TM    REPORT,RPTQLOS+RPTQCPL    COMPLETES AND LOSSES?                  
         BNO   DOLVL200                                                         
*                                                                               
         ZAP   CMPTCWKS,X.CMPTCWKS                                              
         AP    CMPTCWKS,X.CMPTLWKS                                              
         EDIT  CMPTCWKS,(10,0(R4)),ALIGN=LEFT,ZERO=NOBLANK                      
         CP    X.CMPTCSTB,=P'0'                                                 
         BE    DOLVL199                                                         
         CP    CMPTCWKS,=P'0'                                                   
         BE    DOLVL199                                                         
         ZAP   WORK+20(16),X.CMPTCSTB                                           
         MP    WORK+20(16),=P'100'                                              
         DP    WORK+20(16),CMPTCWKS                                             
         SRP   WORK+20(8),64-2,5                                                
         EDIT  (P8,WORK+20),(10,132(R4)),ALIGN=LEFT                             
DOLVL199 DS    0H                                                               
         LA    R4,22(R4)                                                        
DOLVL200 DS    0H                                                               
*                                                                               
         BAS   RE,SPLAT                                                         
*                                                                               
DOLVL210 DS    0H                                                               
*                                                                               
DOLVLX   DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
*  CLEAR TOTAL AREA                                                             
*    R1 CONTAINS THE LEVEL TO CLEAR                                             
**********************************************************************          
CLEARLVL NTR1  BASE=*,LABEL=*                                                   
         MH    R1,=Y(CMPTOTLN)                                                  
         LA    R1,CMPTOTS(R1)                                                   
         USING CMPTOTS,R1                                                       
         ZAP   CMPTPMKB,=P'0'                                                   
         ZAP   CMPTPSTB,=P'0'                                                   
         ZAP   CMPTPHDR,=P'0'                                                   
         ZAP   CMPTPWKS,=P'0'                                                   
*                                                                               
         ZAP   CMPTISTB,=P'0'                                                   
         ZAP   CMPTIHDR,=P'0'                                                   
         ZAP   CMPTIWKS,=P'0'                                                   
*                                                                               
         ZAP   CMPTCMKB,=P'0'                                                   
         ZAP   CMPTCSTB,=P'0'                                                   
         ZAP   CMPTCHDR,=P'0'                                                   
         ZAP   CMPTCWKS,=P'0'                                                   
*                                                                               
         ZAP   CMPTLMKB,=P'0'                                                   
         ZAP   CMPTLHDR,=P'0'                                                   
         ZAP   CMPTLWKS,=P'0'                                                   
*                                                                               
         LA    R2,CSTATOTS                                                      
         LA    R3,LSTATOTS                                                      
         LA    R4,ISTATOTS                                                      
         DROP  R1                                                               
         LA    R1,NUMSTAS                                                       
CLRLVL2  DS    0H                                                               
         ZAP   0(L'CSTATOTS,R2),=P'0'                                           
         ZAP   0(L'LSTATOTS,R3),=P'0'                                           
         ZAP   0(L'ISTATOTS,R4),=P'0'                                           
         LA    R2,L'CSTATOTS(R2)                                                
         LA    R3,L'LSTATOTS(R3)                                                
         LA    R4,L'ISTATOTS(R4)                                                
         BCT   R1,CLRLVL2                                                       
*                                                                               
         B       EXIT                                                           
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
       ++INCLUDE REPIOBLK                                                       
         EJECT                                                                  
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE REGENCON                                                       
         EJECT                                                                  
       ++INCLUDE REGENPRD                                                       
         EJECT                                                                  
       ++INCLUDE REGENPRO                                                       
         EJECT                                                                  
       ++INCLUDE REGENAGY                                                       
         EJECT                                                                  
       ++INCLUDE REGENADV                                                       
         EJECT                                                                  
       ++INCLUDE REGENSAL                                                       
         EJECT                                                                  
       ++INCLUDE REGENSTA                                                       
         EJECT                                                                  
       ++INCLUDE REGENOFF                                                       
         EJECT                                                                  
       ++INCLUDE REGENCTY                                                       
         EJECT                                                                  
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
       ++INCLUDE DDWIDED                                                        
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE RESFMFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMEFD                                                       
         EJECT                                                                  
       ++INCLUDE RESFMWORKD                                                     
         SPACE 3                                                                
         ORG   SYSSPARE                                                         
         PRINT ON                                                               
       ++INCLUDE RESFM3AWRK                                                     
         ORG   MYWORKND                                                         
ATSAROFF DS    F                                                                
*                                                                               
THISOFF  DS    CL2                 OFFICE FOR HEADLINE PRINTING                 
THISTEAM DS    CL2                 TEAM FO3 HEADLINE PRINTING                   
THISLSNM DS    CL24                CODE/NAME FOR HEADLINES                      
THISSTA  DS    CL5                 STATION FOR HEADLINES                        
THISMRKT DS    CL20                MARKET NAME                                  
MONDAYDT DS    CL3                 PREVIOUS MONDAY'S DATE                       
MIDSV    DS    CL80                                                             
CONTYPEX DS    CL50                CONTRACT TYPES BASED ON REGENCTY             
         EJECT                                                                  
*                                                                               
* PRINT LINE DSECTS FOR REPORTS                                                 
*                                                                               
PENDINGD DSECT                                                                  
PPD1ST   DS    CL1                                                              
PPDAYS   DS    CL8                 DAYS TO START                                
         DS    CL12                SPARE                                        
         DS    CL1                                                              
PPDADV   DS    CL20                ADVERTISER                                   
         DS    CL1                                                              
PPDFLITE DS    CL17                FLITE START-END DATE                         
         DS    CL3                 SPARE                                        
         DS    CL1                                                              
PPDBUDGT DS    CL11                DOLLARS                                      
         DS    CL9                 SPARE                                        
         DS    CL1                                                              
PPDPROLB DS    CL6                 '# PRO:'                                     
         DS    CL1                                                              
PPDPROS  DS    CL3                 PROPOSALS                                    
         DS    CL1                                                              
         DS    CL35                SPARE                                        
         DS    CL1                                                              
*                                                                               
* START OF 2ND PRINT LINE                                                       
         DS    CL1                                                              
PPDCON   DS    CL8                 CONTRACT NUMBER                              
         DS    CL12                SPARE                                        
         DS    CL1                                                              
PPDPROD  DS    CL20                PRODUCT                                      
         DS    CL1                                                              
PPDDEMO  DS    CL11                DEMOS                                        
         DS    CL9                 SPARE                                        
         DS    CL1                                                              
PPDSHAR  DS    CL6                 SHARE GOAL                                   
         DS    CL14                SPARE                                        
         DS    CL1                                                              
PPDWKSLB DS    CL8                 C'# WEEKS:'                                  
         DS    CL1                                                              
PPDWEEKS DS    CL3                 WEEKS                                        
         DS    CL1                                                              
         DS    CL33                SPARE                                        
         DS    CL1                 END OF 3D PRINT LINE                         
* START OF 3D LINE                                                              
         DS    CL1                                                              
PPDCREAT DS    CL8                 CREATION DATE                                
         DS    CL12                SPARE                                        
         DS    CL1                                                              
PPDAGY   DS    CL20                AGENCY                                       
         DS    CL1                                                              
PPDSVCBK DS    CL7                 SERVICE/BOOK                                 
         DS    CL13                SPARE                                        
         DS    CL1                                                              
PPDSTAB  DS    CL20                STATION BUDGET                               
         DS    CL1                                                              
PPDCTYLB DS    CL8                 C'CONTYPE:'                                  
         DS    CL1                                                              
PPDCNTY  DS    CL1                 CONTRACT TYPE                                
         DS    CL1                                                              
         DS    CL35                SPARE                                        
         DS    CL1                 END OF 3D PRINT LINE                         
*                                                                               
* START OF 4TH PRINT LINE                                                       
         DS    CL1                                                              
PPDACTIV DS    CL8                 ACTIVITY DATE                                
         DS    CL12                SPARE                                        
         DS    CL1                                                              
PPDBUYER DS    CL20                BUYER                                        
         DS    CL1                                                              
PPDLEN   DS    CL18                SECONDS (6 LENGTHS = NN,NN, ETC)             
         DS    CL2                 SPARE                                        
         DS    CL1                                                              
PPD4P    DS    0CL20               SPARE                                        
         DS    CL1                                                              
         DS    CL46                SPARE                                        
         DS    CL1                 END OF 3D PRINT LINE                         
*                                                                               
         ORG   PPD1ST                                                           
         DS    CL10                                                             
PPDDPT   DS    CL1                 DPT LINE                                     
         DS    CL11                                                             
PPDDPTNX DS    CL1                                                              
         DS    CL11                                                             
         DS    CL1                                                              
         DS    CL11                                                             
         DS    CL1                                                              
         DS    CL11                                                             
         DS    CL1                                                              
         DS    CL11                                                             
         DS    CL1                                                              
         DS    CL11                                                             
         DS    CL1                                                              
         DS    CL11                                                             
         DS    CL1                                                              
PPDDPTEN EQU   *                   END OF PRINTABLE DPT'S                       
         DS    CL37                SPARE                                        
*                                                                               
         DS    CL1                                                              
PPDCPP   DS    CL10                CPP LINE                                     
         DS    CL2                                                              
PPDCPPNX DS    CL10                                                             
         DS    CL2                                                              
         DS    CL10                                                             
         DS    CL2                                                              
         DS    CL10                                                             
         DS    CL2                                                              
         DS    CL10                                                             
         DS    CL2                                                              
         DS    CL10                                                             
         DS    CL2                                                              
         DS    CL10                                                             
         DS    CL2                                                              
         DS    CL10                                                             
PPDCPPEN EQU   *                   END OF PRINTABLE CPP'S                       
         DS    CL37                SPARE                                        
*                                                                               
         DS    CL132                                                            
*                                                                               
         DS    CL132                                                            
*                                                                               
         ORG   PPD1ST                                                           
* THESE EXTENDED PRINT LINES FOR COMPLETED REPORT ONLY                          
         DS    CL1                                                              
PCOMCALL DS    CL10                                                             
         DS    CL1                                                              
         DS    CL120                                                            
*                                                                               
         DS    CL1                                                              
PCOMAFF  DS    CL10                                                             
         DS    CL1                                                              
         DS    CL120                                                            
*                                                                               
         DS    CL1                                                              
PCOMAMT  DS    CL10                                                             
         DS    CL1                                                              
         DS    CL120                                                            
*                                                                               
         DS    CL1                                                              
PCOMSHR  DS    CL10                                                             
         DS    CL1                                                              
         DS    CL120                                                            
* END OF COMPETED REPORT PRINT LINES                                            
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'061RESFM3BS  09/11/02'                                      
         END                                                                    
