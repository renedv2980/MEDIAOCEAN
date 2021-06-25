*          DATA SET SPSMF19S   AT LEVEL 005 AS OF 05/30/96                      
*PHASE T21719A                                                                  
T21719   TITLE 'SPSFM19 - CONTRACTOR TRANSACTION MAINTENANCE OVERLAY'           
***********************************************************************         
*                                                                     *         
*  TITLE:        SPSFM19 -- CONTRACTOR TRANSACTION                    *         
*                                                                     *         
*  COMMENTS:     VALID RECORD ACTIONS: MAINT, LIST, USAGE             *         
*                USES GLOBBER TO CALL ACC CTAGS & CALLED BY SPO BUY   *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (T21700), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREENS SPSFM80 (LIST)                               *         
*                        SPSFM86 (MAINT)                              *         
*                        SPSFMD9 (USAGE)                              *         
*                                                                     *         
*  OUTPUTS:      NONE                                                 *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK, GETEL2 REGISTER                          *         
*                R4 -- WORK                                           *         
*                R5 -- THIRD BASE                                               
*                R6 -- GETEL REGISTER                                 *         
*                R7 -- SECOND BASE                                    *         
*                R8 -- SPOOLD                                         *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- FIRST BASE                                     *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
***********************************************************************         
***********************************************************************         
*                           UPDATE LOG                                *         
***********************************************************************         
* DATE    LEV WHO  DESCRIPTION                                        *         
* ------- --- ---- -------------------------------------------------- *         
* 21NOV95 01  SPRI ORIGINAL ENTRY                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
T21719   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T21719*,R7,R5,RR=RE                                           
                                                                                
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         ST    RE,RELO             RELOCATION FACTOR                            
                                                                                
         BAS   RE,SETUP                                                         
         BE    XIT                                                              
                                                                                
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
                                                                                
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BNE   MAIN10                                                           
         CLI   ACTEQU,20           ACTION USAGE                                 
         BE    DU                  DISPLAY USAGE                                
         B     VR                                                               
                                                                                
MAIN10   CLI   ACTEQU,20           ACTION USAGE                                 
         BE    XIT                                                              
                                                                                
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
                                                                                
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
                                                                                
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
                                                                                
*        CLI   MODE,REPORT         PRINT RECORDS                                
*        BE    PR                                                               
                                                                                
*        CLI   MODE,RECPUT         PUT RECORD                                   
*        BE    PR                                                               
                                                                                
*        CLI   MODE,XRECPUT        AFTER PUT RECORD                             
*        BE    XP                                                               
                                                                                
NEXTMODE DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                         VALIDATE KEY ROUTINE                        *         
***********************************************************************         
VK       DS    0H                                                               
                                                                                
* VALIDATE MEDIA                                                                
         LA    R2,CTMMEDH          MEDIA                                        
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED?                        
         BO    VK10                                                             
         GOTO1 VALIMED                                                          
         OI    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         OI    RECFLAG1,KEYCHNGD   KEY CHANGED                                  
                                                                                
* BUILD KEY                                                                     
VK10     CLI   ACTEQU,ACTLIST      LIST?                                        
         BE    VKX                                                              
                                                                                
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTARECD,R4                                                       
         MVI   CTAKTYP,CTAKTYPQ    RECORD TYPE                                  
         MVI   CTAKSUB,CTAKSUBQ    RECORD SUBTYPE                               
         MVC   CTAKAGMD,BAGYMD     AGENCY/MEDIA                                 
                                                                                
         CLI   ACTEQU,20           ACTION = USAGE                               
         BNE   *+12                                                             
         LA    R2,CTUCNUMH         CONTRACT NUMBER                              
         B     *+8                                                              
                                                                                
* VALIDATE CONTRACT                                                             
         LA    R2,CTMCNUMH         CONTRACT NUMBER                              
                                                                                
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BO    VK20                                                             
         OI    4(R2),X'20'         PREVIOUSLY VALIDATED                         
                                                                                
         MVI   RECFLAG1,0                                                       
         OI    RECFLAG1,KEYCHNGD   KEY CHANGED                                  
                                                                                
VK20     TM    RECFLAG1,NEWCNTRT                                                
         BNO   VK30                                                             
         MVC   CTAKCNUM,CNUMCOMP                                                
         B     VKX                                                              
                                                                                
VK30     ZICM  R1,5(R2),1          LENGTH OF CONTRACT NUMBER                    
         BZ    MISSINGN                                                         
                                                                                
         BCTR  R1,0                                                             
         TM    4(R2),X'08'         VALID NUMERIC?                               
         BNO   VK40                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  WORK2,8(0,R2)       CONTRACT NUMBER                              
                                                                                
         ZAP   WORK1,=P'999999'                                                 
         SP    WORK1,WORK2         FIND 9'S COMPLEMENT OF CONTRACT NO.          
         SRP   WORK1,1,0           SHIFT LEFT 1 DIGIT                           
         MVC   CTAKCNUM,WORK1      MOVE PWOS CONTRACT NUMBER (COMPLI.)          
                                                                                
         SRP   WORK2,1,0           SHIFT LEFT 1 DIGIT (UNCOMPLIMENTED)          
         MVC   CNUMPWOS,WORK2      SAVE CONTRACT NUMBER                         
                                                                                
****     CLI   ACTEQU,20           ACTION = USAGE                               
****     BE    DU                  DISPLAY USAGE                                
         B     VKX                 ACTION = MAINT, SO DISPLAY RECORD            
                                                                                
VK40     CLI   ACTEQU,20           ACTION = USAGE                               
         BE    NOTNUMGN            MUST BE NUMERIC                              
         EX    R1,*+8                                                           
         BE    VK45                                                             
         CLC   8(0,R2),=C'NEW  '   ADDING NEW CONTRACT?                         
         EX    R1,*+8                                                           
         BNE   INVALDGN            INVALID                                      
         CLC   8(0,R2),=C'BF   '   ADDING 95 CONTRACT?                          
         MVI   YR,5                                                             
         B     VK47                                                             
                                                                                
VK45     DS    0H                                                               
         GOTO1 DATCON,DMCB,(5,0),(6,DATE) GET CURRENT YEAR                      
         MVI   DATE+4,C'0'                                                      
         GOTO1 HEXIN,DMCB,DATE+4,YR,2     CONVERT LAST DIGIT OF YR ONLY         
         OC    12(4,R1),12(R1)            CHECK RETURN CODE                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
                                                                                
VK47     DS    0H                                                               
         OI    RECFLAG1,NEWCNTRT          NEW CONTRACT & DON'T VAL REC          
         ZAP   WORK1,=P'99'                                                     
         ZAP   WORK2,=P'9'                                                      
         MVO   WORK2,YR            CONVERT TO PACKED                            
         SP    WORK1,WORK2         FIND 9'S COMPLIMENT OF YEAR                  
         SRP   WORK1,1,0           SHIFT LEFT 1 DIGIT                           
         MVC   CTAKYR,WORK1+2      CONVERT TO PACKED W/O SIGN                   
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(CTAKNUM-CTAKEY),KEYSAVE   SAME TYPE, A/M, & YEAR?            
         BE    VK50                                                             
                                                                                
         XC    KEY,KEY             FIRST CONTRACT OF YEAR                       
         MVC   KEY(CTAKNUM-CTAKEY),KEYSAVE   RESTORE KEY                        
         MVC   CTAKNUM,=X'9999'    GIVE CONTRACT NO. 0001                       
         B     VK60                                                             
                                                                                
VK50     OC    CTAKNUM,CTAKNUM     CHECK CONTRACT NO. RETURNED                  
         BNZ   *+6                                                              
         DC    H'0'                RAN OUT OF CONTRACT NOS.*********            
                                                                                
VK60     ZAP   WORK1,=P'999999'                                                 
         MVO   WORK1,CTAKCNUM      CONVERT TO PACKED                            
         SP    WORK1,=P'1'         ADD 1 FOR NEXT CONTRACT NO.                  
         MVC   WORK2,WORK1                                                      
         SRP   WORK1,1,0           SHIFT LEFT 1 DIGIT                           
         MVC   CTAKCNUM,WORK1      CONVERT TO PACKED W/O SIGN                   
         MVC   CNUMCOMP,WORK1      COMPLIMENT PWOS CONTRACT NUMBER              
                                                                                
         ZAP   WORK1,=P'999999'                                                 
         SP    WORK1,WORK2                                                      
         SRP   WORK1,1,0           SHIFT LEFT 1 DIGIT                           
         MVC   CNUMPWOS,WORK1      PWOS CONTRACT NUMBER                         
         DROP  R4                                                               
                                                                                
VKX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                             DISPLAY KEY                             *         
***********************************************************************         
DK       DS    0H                                                               
         L     R4,AIO                                                           
         USING CTARECD,R4                                                       
         MVC   CTMMED,QMED         MEDIA                                        
         OI    CTMMEDH+6,X'80'                                                  
                                                                                
         ZAP   WORK1,=P'999999'                                                 
         ZAP   WORK2,=P'0'                                                      
         MVO   WORK2,CTAKCNUM              CONVERT TO PACKED                    
         SP    WORK1,WORK2                                                      
         EDIT  (P4,WORK1),(5,CTMCNUM)      CONTRACT NUMBER                      
                                                                                
                                                                                
         OI    CTMCNUMH+6,X'80'    TRANSMIT CONTRACT #                          
         MVI   CTMCNUMH+5,5        PUT LENGTH OF 5 IN                           
         OI    CTMCNUMH+4,X'08'    VALID NUMERIC                                
         OI    CTMCNUMH+4,X'20'    PREVIOUSLY VALIDATED                         
         MVC   CNUM,CTMCNUM                                                     
         DROP  R4                                                               
         B     XIT                                                              
                                                                                
         EJECT                                                                  
***********************************************************************         
*                           VALIDATE RECORD                           *         
***********************************************************************         
VR       DS    0H                                                               
                                                                                
         CLI   TWALACT,ACTSEL      ACTION SELECT/CHANGE?                        
         BNE   *+8                                                              
         NI    RECFLAG1,X'FF'-KEYCHNGD                                          
                                                                                
         XC    SUM,SUM             INITIALIZE SUM OF PART. AMOUNTS              
                                                                                
         TM    RECFLAG1,NEWCNTRT   NEW CONTRACT BEING ADDED                     
         BNO   *+14                                                             
         MVC   KEYSAVE(L'CTAKEY),KEY                                            
         B     VR10                                                             
                                                                                
         LA    R2,CTMCNUMH         CONTRACT #                                   
         GOTO1 HIGH                                                             
         MVC   ERRNUM,=AL2(SE#CNTFD)    CONTRACT NOT FOUND                      
         CLC   KEY(L'CTAKEY),KEYSAVE                                            
         BNE   SPERREX                                                          
                                                                                
         MVC   SAVEKEY,KEY                                                      
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
                                                                                
         CLC   MYLSTACT,ACTEQU     LAST ACTION WASN'T MAINT?                    
         BNE   *+12                SO DON'T CHECK IF KEY CHANGED                
         TM    RECFLAG1,KEYCHNGD   KEY CHANGED?                                 
         BNO   VR10                                                             
         NI    RECFLAG1,X'FF'-KEYCHNGD                                          
         GOTO1 =A(CS),DMCB,(RC),RR=RELO     CLEAR SCREEN                        
         B     DR                  DISPLAY RECORD                               
                                                                                
VR10     TM    RECFLAG1,NEWCNTRT                                                
         BNO   VR20                                                             
         MVI   RECFLAG3,0                                                       
         OI    RECFLAG3,GCICHNGD                                                
         MVC   AIO,AIO1            CLEAR IO1                                    
         L     R0,AIO                                                           
         L     R1,=A(LIOS)                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         BAS   RE,UNVAL            TURN OFF PREV. VAL BITS                      
                                                                                
***************************   CONTRACTOR  **************************            
VR20     L     R3,AIO                                                           
         USING CTAKEY,R3                                                        
         MVC   CTAKEY,KEYSAVE                                                   
         MVC   CTARAGYA,TWAAGY      AGENCY                                      
         DROP  R3                                                               
                                                                                
         LA    R2,CTMCNTRH                                                      
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BO    VR40                                                             
                                                                                
         GOTO1 ANY                                                              
         MVC   CONTR,WORK                                                       
         TM    RECFLAG3,ACCRECRD   ACC CONTRACT RECORD EXISTS?                  
         BNO   VR30                NO, SO O.K. TO CHANGE GCI                    
         CLC   CONTR,DISPDCTR      COMPARE W/ CONTRACTOR DISPLAYED              
         BE    VR40                                                             
                                                                                
         MVC   8(L'CONTR,R2),CONTR YES, CHANGE CONTRACTOR BACK                  
         OI    6(R2),X'80'                                                      
         OI    4(R2),X'20'                                                      
         MVC   ERRNUM,=AL2(SE#CNTCH)    CANNOT CHANGE THIS FIELD                
         B     SPERREX                                                          
                                                                                
VR30     OI    RECFLAG3,RECCHNGD   CONTRACT RECORD CHANGED                      
         GOTO1 =A(CNTR),DMCB,(RC),RR=RELO   CHECK IF CONTRACTOR EXISTS          
         OI    4(R2),X'20'                                                      
                                                                                
***************************   GCI   ********************************            
                                                                                
VR40     LA    R2,CTMGCIH                                                       
                                                                                
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED?                        
         BO    VR50                                                             
         ZICM  RE,5(R2)            LENGTH OF FIELD                              
         BZ    MISSINGN            MISSING INPUT FIELD                          
                                                                                
         ST    RE,DMCB+4                                                        
         GOTO1 CASHVAL,DMCB,(2,CTMGCI)                                          
         CLI   0(R1),0             CHECK RETURN CODE                            
         BNE   INVALDGN            INVALID INPUT                                
                                                                                
         MVC   ERRNUM,=AL2(SE#ZRINV)    ZERO IS NOT VALID                       
         ZICM  RE,4(R1),4                                                       
         BZ    SPCURSER            AMOUNT GIVEN WAS 0?                          
         BM    INVALDGN            NEGATIVE NUMBER                              
         ST    RE,GCI              SAVE GCI                                     
                                                                                
         C     RE,DISPDGCI         CHECK IF GCI CHANGED                         
         BE    VR50                                                             
         OI    RECFLAG2,GCICHNGD   GCI WAS CHANGED                              
         OI    RECFLAG3,RECCHNGD   CONTRACT RECORD CHANGED                      
         TM    RECFLAG3,ACCRECRD   ACC CONTRACT RECORD EXISTS?                  
         BNO   VR50                NO, SO O.K. TO CHANGE GCI                    
                                                                                
         EDIT  (4,DISPDGCI),(9,CTMGCI),2  YES, CHANGE GCI BACK                  
         MVC   GCI,DISPDGCI                                                     
         OI    6(R2),X'80'                                                      
         OI    4(R2),X'20'                                                      
         NI    RECFLAG2,X'FF'-GCICHNGD   GCI WAS CHANGED                        
         MVC   ERRNUM,=AL2(SE#USCTG)     MUST USE CTAGS TO CHANGE GCI           
         B     SPERREX                                                          
                                                                                
*************************  PARTICIPANTS  ***************************            
                                                                                
VR50     TM    RECFLAG2,GCICHNGD   DID GCI CHANGE?                              
         BO    VR60                                                             
         TM    CTMPARFH+4,X'20'    PREVIOUSLY VALIDATED?                        
         BNO   VR60                                                             
         TM    CTMPARLH+4,X'20'    PREVIOUSLY VALIDATED?                        
         BNO   VR60                                                             
         OI    RECFLAG1,SAMEPART   PARTICIPANTS DID NOT CHANGE                  
         B     VR260                                                            
                                                                                
VR60     OI    RECFLAG3,RECCHNGD   CONTRACT RECORD CHANGED                      
         LA    R1,TABLE                                                         
         ST    R1,NEXTPART                                                      
                                                                                
         LA    R2,CTMPARFH         FIRST PARTICIPANTS LINE                      
         CLI   5(R2),0             LENGTH OF INPUT                              
         BE    MISSINGN            MISSING INPUT FIELD                          
                                                                                
VR70     LA    R3,ELEM                                                          
         USING CTPARD,R3           PARTICIPANTS ELEMENT                         
                                                                                
         NI    RECFLAG1,X'FF'-DOLLRAMT-PRCNTAMT-NOAMOUNT                        
                                                                                
VR80     LA    R1,CTMPARLH         LAST PARTICIPANTS LINE                       
         CR    R2,R1               PAST THE LAST PARTICIPANTS LINE?             
         BH    VR140                                                            
         CLI   5(R2),0             LENGTH OF INPUT                              
         BE    VR140                                                            
                                                                                
         LA    R0,MYBLOCK                                                       
         LA    R1,L'MYBLOCK                                                     
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         GOTO1 SCANNER,DMCB,(R2),(9,MYBLOCK)                                    
         ZICM  R4,4(R1),1          NUMBER OF PARTICIPANTS ENTERED               
         BZ    INVALDGN            INVALID INPUT                                
                                                                                
         MVI   FIELDERR,1          COUNTS WHICH FIELD THE ERROR IS IN           
         LA    R6,MYBLOCK                                                       
                                                                                
VR90     CLI   0(R6),0             LENGTH OF FIELD = 0?                         
         BE    VR130                                                            
                                                                                
         TM    RECFLAG1,NEWCNTRT                                                
         BO    *+10                                                             
         L     R1,NEXTPART                                                      
         LR    R3,R1               PUT ELEMENT IN TABLE INSTEAD OF IO           
                                                                                
         XC    0(CTPARLNQ,R3),0(R3)                                             
         MVI   CTPAREL,CTPARELQ    ELEMENT CODE                                 
         MVI   CTPARLEN,CTPARLNQ   LENGTH                                       
                                                                                
* VALIDATE THE STATION W/ VALISTA                                               
                                                                                
         ST    R2,SAVEADR          SAVE REAL FIELD HEADER ADDRESS               
         LA    R2,FAKEHDR                                                       
         XC    FAKEFLD,FAKEFLD                                                  
         XC    FAKEHDR,FAKEHDR                                                  
                                                                                
         ZICM  R1,0(R6),1          LENGTH OF FIRST FIELD                        
         BNZ   *+6                                                              
         DC    H'0'                SHOULD BE OUT OF LOOP                        
                                                                                
         STC   R1,5(R2)            STORE INPUT LENGTH IN FIELD HEADER           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FAKEFLD(0),12(R6)   STORE STATION                                
                                                                                
         MVI   USEIONUM,2                                                       
         OI    TRNSTAT,NOVALERR                                                 
         GOTO1 VALISTA             VALIDATE STATION                             
         MVI   USEIONUM,0          RESET                                        
         L     R2,SAVEADR          RESTORE REAL FIELD HEADER                    
         NI    TRNSTAT,X'FF'-NOVALERR                                           
         MVC   ERRNUM,=AL2(SE#INVST)    INVALID STATION CODE                    
         TM    TRNSTAT,BASERR                                                   
         BO    SPCURSER                                                         
                                                                                
         MVC   AIO,AIO1            RESTORE AIO1 ADDRESS                         
         MVC   CTPARSTA,BSTA       STORE BINARY STATION                         
         MVC   CTPARMKT,BMKT       BINARY MARKET                                
                                                                                
         ZICM  RE,1(R6)            LENGTH OF SECOND PART OF FIELD               
         BZ    VR100               NO AMOUNT GIVEN                              
         ST    RE,DMCB+4                                                        
         GOTO1 CASHVAL,DMCB,(2,22(R6))                                          
         CLI   0(R1),0             CHECK RETURN CODE                            
         BNE   INVALDCR            INVALD INPUT                                 
                                                                                
         MVC   ERRNUM,=AL2(SE#ZRINV)    ZERO IS NOT VALID                       
         ZICM  RE,4(R1),4                                                       
         BZ    SPCURSER            AMOUNT GIVEN WAS 0?                          
         BM    INVALDCR            NEGATIVE NUMBER                              
                                                                                
         CLI   22(R6),C'$'         DOLLAR SIGN IN FRONT OF NUMBER?              
         BE    VR110                                                            
                                                                                
* PERCENT AMOUNT                                                                
         TM    RECFLAG1,DOLLRAMT+NOAMOUNT                                       
         BNZ   INVALDCR                                                         
                                                                                
         MVC   ERRNUM,=AL2(SE#SMPER)   SUM OF % NOT 100                         
         ICM   R0,15,4(R1)                                                      
         C     R0,=F'10000'        SUM SHOULD BE 100%                           
         BNH   *+12                                                             
         LA    R2,CTMPARFH                                                      
         B     SPERREX                                                          
                                                                                
         STCM  R0,15,CTPARPCT      PERCENT GIVEN                                
         L     R0,SUM                                                           
         A     R0,4(R1)                                                         
         ST    R0,SUM                                                           
                                                                                
         L     RF,GCI                                                           
         ICM   R1,15,CTPARPCT                                                   
         MR    RE,R1               GCI X PARICIPANT %                           
         D     RE,=F'10000'        GET RID OF FOUR DECIMAL PLACES               
         STCM  RF,15,CTPARAMT                                                   
                                                                                
         OI    RECFLAG1,PRCNTAMT                                                
         B     VR120                                                            
                                                                                
* NO AMOUNTS                                                                    
VR100    TM    RECFLAG1,PRCNTAMT+DOLLRAMT                                       
         BNZ   INVALDCR            INVALID INPUT                                
         OI    RECFLAG1,NOAMOUNT                                                
         B     VR120                                                            
                                                                                
* DOLLAR AMOUNT                                                                 
VR110    TM    RECFLAG1,PRCNTAMT+NOAMOUNT                                       
         BNZ   INVALDCR            INVALID INPUT                                
                                                                                
         MVC   CTPARAMT,4(R1)                                                   
         L     R0,SUM              ADD TO SUM                                   
         A     R0,4(R1)                                                         
         ST    R0,SUM                                                           
         OI    RECFLAG1,DOLLRAMT                                                
                                                                                
VR120    TM    RECFLAG1,NEWCNTRT   NEW CONTRACT BEING ADDED                     
         BNO   VR130                                                            
         GOTO1 ADDELEM             ADD PARTICIPANTS ELEMENT                     
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
VR130    LA    R6,32(R6)           NEXT PARTICIPANT                             
                                                                                
         L     R1,NEXTPART                                                      
         LA    R1,CTPARLNQ(R1)     BUILD NEXT ELEMENT                           
         ST    R1,NEXTPART                                                      
                                                                                
         ZIC   R1,FIELDERR         INCREMENT FIELD #                            
         LA    R1,1(R1)                                                         
         STC   R1,FIELDERR                                                      
         BCT   R4,VR90                                                          
                                                                                
         ZIC   R1,0(R2)            NEXT PARTICIPANT LINE                        
         AR    R2,R1                                                            
         B     VR80                                                             
                                                                                
         DROP  R3                                                               
                                                                                
VR140    MVC   ERRNUM,=AL2(SE#SMPER)   SUM OF % NOT 100                         
         LA    R2,CTMPARFH                                                      
         L     R0,SUM                                                           
         TM    RECFLAG1,PRCNTAMT                                                
         BNO   VR150                                                            
         C     R0,=F'10000'        SUM SHOULD BE 100%                           
         BNE   SPERREX                                                          
         B     VR160                                                            
                                                                                
VR150    DS    0H                                                               
         MVC   ERRNUM,=AL2(SE#SMGCI)                                            
         TM    RECFLAG1,DOLLRAMT   PARTIPANT IN DOLLARS?                        
         BNO   VR160                                                            
         C     R0,GCI              CHECK IF SUM = GCI                           
         BNE   SPERREX             SUM DOES NOT EQUAL GCI                       
                                                                                
VR160    TM    RECFLAG1,NEWCNTRT   NEW CONTRACT?                                
         BO    VR260                                                            
                                                                                
         L     R1,NEXTPART                                                      
         MVI   0(R1),X'FF'         MARK END OF TABLE                            
                                                                                
* COMPARE OLD PARTICIPANTS WITH NEW PARTICIPANTS                                
         MVC   AIO,AIO1                                                         
         L     R6,AIO                                                           
         MVI   ELCODE,CTPARELQ     PARTICIPANT ELEMENT                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
OLD      USING CTPARD,R6           COPY OF OLD PARTIC. ELEM'S IN IO1            
         OI    OLD.CTPARFLG,X'40'  MARK ALL ELEMENTS W/ X'40'                   
         BAS   RE,NEXTEL                                                        
         BE    *-8                                                              
                                                                                
         LA    R3,TABLE                                                         
         MVI   FIELDERR,0                                                       
NEW      USING CTPARD,R3                                                        
                                                                                
VR170    CLI   0(R3),X'FF'         END OF TABLE?                                
         BE    VR210                                                            
                                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,CTPARELQ     PARTICIPANT ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   VR200                                                            
                                                                                
         ZIC   R1,FIELDERR                                                      
         LA    R1,1(R1)                                                         
         STC   R1,FIELDERR                                                      
VR180    CLC   NEW.CTPARMKT(5),OLD.CTPARMKT                                     
         BNE   VR200                                                            
                                                                                
* CHECK FOR DUPLICATE STATIONS                                                  
         MVC   ERRNUM,=AL2(SE#DUPLC)                                            
         TM    OLD.CTPARFLG,X'40'                                               
         BNO   SPCURSER                                                         
                                                                                
* MOVE A COPY OF PARTICIPANT INFO INTO ELEMENT THAT EXISTS                      
         CLC   OLD.CTPARAMT,NEW.CTPARAMT     AMOUNT DIDN'T CHANGE?              
         BE    VR190                                                            
                                                                                
         TM    RECFLAG1,DOLLRAMT+PRCNTAMT                                       
         BZ    VR190                                                            
         MVC   BINSTA,OLD.CTPARSTA    GET USAGE FOR THE STATION                 
         BAS   RE,CUSAGE                                                        
         ICM   R1,15,NEW.CTPARAMT                                               
         C     R1,USAGE                                                         
         BL    VR230                                                            
                                                                                
VR190    MVC   OLD.CTPARFLG(CTPARLNQ-2),NEW.CTPARFLG                            
         LA    R3,CTPARLNQ(R3)                                                  
         B     VR170                                                            
                                                                                
VR200    BAS   RE,NEXTEL                                                        
         BE    VR180                                                            
                                                                                
         XC    ELEM,ELEM                                                        
         MVC   ELEM(CTPARLNQ),0(R3)                                             
         GOTO1 ADDELEM             ADD NEW PARTICIPANTS ELEMENT                 
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,CTPARLNQ(R3)     NEXT TABLE ENTRY                             
         B     VR170                                                            
                                                                                
VR210    L     R6,AIO                                                           
         MVI   ELCODE,CTPARELQ     PARTICIPANT ELEMENT                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
VR220    TM    OLD.CTPARFLG,X'40'  CHECK IF PARTICIPANTS NEED TO BE DEL         
         BNO   VR250                                                            
         MVC   BINSTA,OLD.CTPARSTA GET USAGE FOR THE STATION                    
         BAS   RE,CUSAGE                                                        
         OC    USAGE,USAGE         ANY USAGE?                                   
         BZ    VR240                                                            
                                                                                
* TRYING TO DELETE A PARTICIPANT WITH USAGE                                     
VR230    MVI   FIELDERR,0                                                       
         MVC   STADEL,OLD.CTPARSTA         STATION TRYING TO DELETE             
         OI    RECFLAG2,DELETERR+STANOFND                                       
         XC    KEY,KEY                                                          
         MVC   KEY(L'CTAKEY),SAVEKEY                                            
         GOTO1 HIGH                                                             
         CLC   KEY(L'CTAKEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         XC    CTMPARF,CTMPARF                                                  
         XC    CTMPARL,CTMPARL                                                  
         NI    RECFLAG2,X'FF'-GCICHNGD                                          
         NI    RECFLAG1,X'FF'-DOLLRAMT-PRCNTAMT-NOAMOUNT-KEYCHNGD               
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE IN AIO2                      
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         B     DR140               JUST PRINT OLD PARTICIPANTS                  
                                                                                
VR240    MVI   OLD.CTPAREL,X'FF'   MARK FOR DELETION                            
                                                                                
VR250    BAS   RE,NEXTEL                                                        
         BE    VR220                                                            
                                                                                
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
         DROP  NEW,OLD                                                          
                                                                                
***************************  CONTRACTOR  ***************************            
VR260    MVI   ELCODE,CTDSCELQ     DESCRIPTION ELEMENT                          
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BE    VR270                                                            
                                                                                
         LA    R6,ELEM             BUILD ELEMENT                                
         USING CTDSCEL,R6          DESCRIPTION ELEMENT                          
         XC    ELEM,ELEM                                                        
         MVI   CTDSCEL,CTDSCELQ    ELEMENT CODE                                 
         MVI   CTDSCLEN,CTDSCLNQ   LENGTH                                       
                                                                                
VR270    MVC   CTDSCNTR,CONTR                                                   
*******************  CONTRACT START & END DATE  ********************            
                                                                                
VR280    TM    CTMCONSH+4,X'20'    PREVIOUSLY VALIDATED?                        
         BNO   *+12                                                             
         TM    CTMCONEH+4,X'20'                                                 
         BO    VR300                                                            
                                                                                
         NI    RECFLAG2,X'FF'-NOSTRTDT     NO START DATE                        
         OI    RECFLAG3,RECCHNGD   CONTRACT RECORD CHANGED                      
         LA    R4,PERIOD                                                        
         LA    R2,CTMCONSH         START DATE                                   
         ZICM  R1,5(R2)            INPUT DATA LENGTH                            
         BNZ   *+12                                                             
         OI    RECFLAG2,NOSTRTDT   NO START DATE ENTERED                        
         B     VR290                                                            
                                                                                
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R4),CTMCONS                                                  
         LA    R4,1(R1,R4)         ADDRESS TO PLACE NEXT DATE                   
         MVI   0(R4),C'-'          HYPHEN BETWEEN DATES                         
         LA    R4,1(R4)                                                         
         LA    R3,2(R1)            LENGTH OF DATE                               
                                                                                
VR290    LA    R2,CTMCONEH         END DATE                                     
         ZICM  R1,5(R2)            INPUT DATA LENGTH                            
         BNZ   VR295                                                            
         TM    RECFLAG2,NOSTRTDT   NO START DATE ENTERED                        
         BNO   MISSINGN                                                         
         XC    CTDSCST(L'CTDSCST+L'CTDSCEND),CTDSCST                            
         B     VR300                                                            
                                                                                
VR295    LA    R2,CTMCONSH         START DATE                                   
         TM    RECFLAG2,NOSTRTDT   NO START DATE ENTERED                        
         BO    MISSINGN                                                         
                                                                                
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R4),CTMCONE                                                  
         LA    R4,1(R1,R4)         ADDRESS OF END OF DATE                       
         LA    R3,1(R1,R3)         LENGTH OF DATE                               
                                                                                
         LA    RE,PERIOD                                                        
         ST    RE,DMCB                                                          
         STC   R3,DMCB                                                          
                                                                                
         GOTO1 PERVAL,DMCB,,MYBLOCK                                             
         MVC   ERRNUM,=AL2(SE#STINV)                                            
         LA    R2,CTMCONSH                                                      
         CLI   4(R1),PVRCINV1      START DATE VALID?                            
         BE    SPERREX                                                          
         MVC   ERRNUM,=AL2(SE#ENINV)                                            
         LA    R2,CTMCONEH                                                      
         CLI   4(R1),PVRCINV2      END DATE VALID?                              
         BE    SPERREX                                                          
                                                                                
         LA    R3,MYBLOCK                                                       
         USING PERVALD,R3                                                       
         MVC   CTDSCST,PVALBSTA    CONTRACT START DATE                          
         MVC   CTDSCEND,PVALBEND   CONTRACT END DATE                            
         DROP  R3                                                               
                                                                                
***************************   NCI   ********************************            
                                                                                
VR300    LA    R2,CTMCOMMH                                                      
         TM    RECFLAG2,GCICHNGD   GCI WAS CHANGED                              
         BO    *+12                                                             
                                                                                
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BO    VR360                                                            
                                                                                
         MVC   CTDSCGCI,GCI        STORE GCI IN RECORD                          
         OI    RECFLAG3,RECCHNGD   CONTRACT RECORD CHANGED                      
         CLI   5(R2),0             ANY INPUT?                                   
         BE    VR310               N IS THE DEFAULT VALUE                       
                                                                                
         CLI   8(R2),C'N'          NOT COMMISSIONABLE                           
         BNE   VR320                                                            
VR310    XC    CTDSCCOM,CTDSCCOM                                                
         MVC   CTDSCNCI,CTDSCGCI   NCI & GCI ARE THE SAME                       
         B     VR350                                                            
                                                                                
VR320    CLI   8(R2),C'Y'          COMMISSIONABLE AT 85%?                       
         BNE   VR330                                                            
         MVC   CTDSCCOM,=F'8500'   85%                                          
         B     VR340               GO CALCULATE THE NCI                         
                                                                                
VR330    ZIC   RE,5(R2)            LENGTH OF FIELD                              
         ST    RE,DMCB+4                                                        
         GOTO1 CASHVAL,DMCB,(2,CTMCOMM)                                         
         CLI   0(R1),0             CHECK RETURN CODE                            
         BNE   INVALDGN            INVALID INPUT                                
                                                                                
         MVC   ERRNUM,=AL2(SE#ZRINV)    ZERO IS NOT VALID                       
         OC    4(4,R1),4(R1)       AMOUNT GIVEN WAS 0?                          
         BZ    SPERREX                                                          
                                                                                
         MVC   CTDSCCOM,4(R1)      STORE IN RECORD                              
         CLC   CTDSCCOM,=F'10000'                                               
         BE    VR310               100% = N                                     
         BH    INVALDGN            CAN'T BE HIGHER THAN 100%                    
                                                                                
VR340    ICM   RF,15,CTDSCCOM      CALCULATE THE NCI                            
         ICM   R1,15,CTDSCGCI                                                   
         MR    RE,R1                                                            
         D     RE,=F'10000'        GET RID OF FOUR DECIMAL PLACES               
         STCM  RF,15,CTDSCNCI                                                   
                                                                                
VR350    TM    RECFLAG3,ACCRECRD   ACC CONTRACT RECORD EXISTS?                  
         BNO   VR360                                                            
         ICM   R1,15,CTDSCCOM      CHECK IF COMMISSION RATE CHANGED             
         C     R1,DISPDCOM         COMPARE W/ COMMISION DISPLAYED               
         BE    VR360                                                            
                                                                                
         OI    RECFLAG3,COMCHGER   COMMISSION CHANGE ERROR                      
         MVC   CTDSCCOM,DISPDCOM                                                
         B     DR40                GO AND DISPLAY THE ORIGINAL COMMISS          
                                                                                
**********************  TRADE COMMITMENT  **************************            
                                                                                
VR360    LA    R2,CTMTRCMH                                                      
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BO    VR380                                                            
                                                                                
         OI    RECFLAG3,RECCHNGD   CONTRACT RECORD CHANGED                      
         ZICM  RE,5(R2)            LENGTH OF FIELD                              
         BZ    VR380                                                            
                                                                                
         ST    RE,DMCB+4                                                        
         GOTO1 CASHVAL,DMCB,(2,CTMTRCM)                                         
         CLI   0(R1),0             CHECK RETURN CODE                            
         BNE   INVALDGN            INVALID INPUT                                
                                                                                
         ICM   R0,15,4(R1)                                                      
         C     R0,=F'10000'        PERCENT CAN'T BE GREATER THAN 100%           
         BH    INVALDCR            INVALID INPUT                                
                                                                                
         STCM  R0,15,CTDSCTRD      STORE IN RECORD                              
                                                                                
**********************  CONTRACT TYPE  *****************************            
                                                                                
VR380    TM    RECFLAG1,SAMEPART   PARTICIPANTS DID NOT CHANGE                  
         BO    VR390                                                            
         NI    CTDSCTYP,X'FF'-X'04'-X'80'                                       
         TM    RECFLAG1,DOLLRAMT   DOLLARS?                                     
         BNO   *+8                                                              
         OI    CTDSCTYP,X'80'                                                   
                                                                                
         TM    RECFLAG1,PRCNTAMT   PERCENT?                                     
         BNO   *+8                                                              
         OI    CTDSCTYP,X'04'                                                   
                                                                                
VR390    LA    R2,CTMCTYPH         CONTRACT TYPE                                
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BO    VR400                                                            
         OI    RECFLAG3,RECCHNGD   CONTRACT RECORD CHANGED                      
         NI    CTDSCTYP,X'FF'-X'40'-X'20'                                       
                                                                                
         ZICM  R1,5(R2),1          INPUT LENGTH                                 
         BZ    VR400               NO INPUT                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=C'BB '     BB?                                          
         BNE   *+12                                                             
         OI    CTDSCTYP,X'40'                                                   
         B     VR400                                                            
                                                                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=C'INV'     INV?                                         
         BNE   INVALDGN            INVALID INPUT                                
         OI    CTDSCTYP,X'20'                                                   
                                                                                
VR400    LA    R2,CTMSENDH         SEND CONTRACT?                               
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BO    VR410                                                            
         OI    RECFLAG3,RECCHNGD   CONTRACT RECORD CHANGED                      
         CLI   5(R2),0             ANY INPUT?                                   
         BE    VR410                                                            
         CLI   8(R2),C'N'                                                       
         BNE   *+12                                                             
         OI    CTDSCTYP,X'10'                                                   
         B     VR410                                                            
         CLI   8(R2),C'Y'                                                       
         BNE   INVALDGN            INVALID INPUT                                
         NI    CTDSCTYP,X'FF'-X'10'                                             
                                                                                
*******************  ANTICIPATED USAGE  ****************************            
                                                                                
VR410    LA    R2,CTMCLT1H         FIRST                                        
         LA    R3,CTMCLT2H         LAST                                         
         ST    R6,SAVEADR          SAVE ADDRESS OF START OF ELEMENT             
                                                                                
VR420    CR    R2,R3                                                            
         BH    VR470                                                            
                                                                                
*************************  CLIENT  *********************************            
         CLI   5(R2),0             ANY INPUT?                                   
         BNE   *+12                                                             
         OI    RECFLAG2,NOCLIENT                                                
         B     VR440                                                            
                                                                                
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BNO   VR430                                                            
                                                                                
         ZIC   R1,0(R2)            POINT TO PRODUCT FIELD                       
         AR    R1,R2                                                            
         ZIC   R0,0(R2)                                                         
         AR    R1,R0                                                            
         TM    4(R1),X'20'         PRODUCT PREVIOUSLY VALIDATED?                
         BNO   VR430                                                            
         LR    R2,R1                                                            
         B     VR450                                                            
                                                                                
VR430    OI    RECFLAG3,RECCHNGD   CONTRACT RECORD CHANGED                      
         MVI   USEIONUM,2                                                       
         GOTO1 VALICLT             VALIDATE CLIENT                              
         MVI   USEIONUM,0          RESET                                        
         L     R4,AIO                                                           
         MVC   ERRNUM,=AL2(SE#INVCL) INVALID CLIENT CODE                        
         USING CLTHDRD,R4                                                       
         DROP  R4                                                               
                                                                                
         MVC   AIO,AIO1            RESTORE PRIMARY IO AREA ADDRESS              
         MVC   CTDSCLT1,BCLT       STORE BINARY CLIENT                          
                                                                                
VR440    ZIC   R1,0(R2)            POINT TO PRODUCT FIELD                       
         AR    R2,R1                                                            
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
                                                                                
*************************  PRODUCT  ********************************            
                                                                                
         CLI   5(R2),0             ANY INPUT?                                   
         BE    VR450                                                            
                                                                                
         TM    RECFLAG3,CLTCHNGD   CLIENT CHANGED                               
         BO    *+12                                                             
                                                                                
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BO    VR450                                                            
                                                                                
         NI    RECFLAG3,X'FF'-CLTCHNGD   CLIENT CHANGED                         
         OI    RECFLAG3,RECCHNGD   CONTRACT RECORD CHANGED                      
         TM    RECFLAG2,NOCLIENT   NO CLIENT CODE ENTERED                       
         BO    INVALDGN                                                         
                                                                                
         MVI   USEIONUM,2          USE IO2 FOR VALIPRD                          
         GOTO1 VALIPRD             VALIDATE PRODUCT                             
         MVI   USEIONUM,0          CLEAR                                        
         MVC   AIO,AIO1            USE IO1 AGAIN                                
         MVC   CTDSPRD1,BPRD       STORE BINARY PRODUCT                         
                                                                                
VR450    NI    RECFLAG2,X'FF'-NOCLIENT                                          
         ZIC   R1,0(R2)            POINT TO PERIOD FIELD                        
         AR    R2,R1                                                            
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
                                                                                
*************************  PERIOD   ********************************            
                                                                                
         CLI   5(R2),0             ANY INPUT?                                   
         BE    VR460                                                            
                                                                                
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BO    VR460                                                            
                                                                                
         OI    RECFLAG3,RECCHNGD   CONTRACT RECORD CHANGED                      
         MVC   PERIOD,8(R2)        PERIOD                                       
                                                                                
         LA    R1,PERIOD                                                        
         ST    R1,DMCB                                                          
         ZIC   R1,5(R2)            LENGTH OF PERIOD ENTERED                     
         STC   R1,DMCB                                                          
                                                                                
         GOTO1 PERVAL,DMCB,,MYBLOCK                                             
         MVC   ERRNUM,=AL2(SE#STINV) START DATE INVALID                         
         CLI   4(R1),PVRCINV1      START DATE VALID?                            
         BE    SPERREX                                                          
         MVC   ERRNUM,=AL2(SE#ENINV) END DATE INVALID                           
         CLI   4(R1),PVRCINV2      END DATE VALID?                              
         BE    SPERREX                                                          
                                                                                
         LA    R4,MYBLOCK                                                       
         USING PERVALD,R4                                                       
         MVC   CTDSPER1,PVALBSTA   PERIOD                                       
         DROP  R4                                                               
                                                                                
VR460    ZIC   R1,0(R2)            POINT TO CLIENT FIELD                        
         AR    R2,R1                                                            
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
                                                                                
         LA    R6,L'CTDSCUS1(R6)   CHANGES USING FOR DESCR. ELEMENT             
         B     VR420                                                            
                                                                                
**********************  RDR TRADER NAME  ***************************            
                                                                                
VR470    L     R6,SAVEADR          RESTORE R6 BEFORE ADDING MORE FIELDS         
         LA    R2,CTMRDRTH                                                      
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BO    VR480                                                            
         OI    RECFLAG3,RECCHNGD   CONTRACT RECORD CHANGED                      
         MVC   CTDSCRDR,8(R2)                                                   
         OC    CTDSCRDR,SPACES     PAD W/ SPACES                                
                                                                                
**********************  MEDIA BUYER NAME  **************************            
                                                                                
VR480    LA    R2,CTMMEDBH                                                      
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BO    VR490                                                            
         OI    RECFLAG3,RECCHNGD   CONTRACT RECORD CHANGED                      
         MVC   CTDSCBYR,8(R2)                                                   
         OC    CTDSCBYR,SPACES                                                  
                                                                                
********************************************************************            
                                                                                
VR490    MVC   KEY(L'CTAKEY),SAVEKEY                                            
         TM    RECFLAG1,NEWCNTRT   NEW CONTRACT?                                
         BNO   VR500               NO, DO A PUTREC & ADD ELEMENT                
                                                                                
         MVC   AIO,AIO1                                                         
         GOTO1 ADDELEM             ADD DESCRIPTION ELEMENT                      
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R6                                                               
                                                                                
         GOTO1 ADDREC              YES, DO AN ADDREC (IO1)                      
                                                                                
         XC    KEY,KEY             ADD '0DFD' KEY                               
         LA    R6,KEY                                                           
         USING CTAKEY,R6                                                        
         MVI   CTCPTYP,CTCPTYPQ    SET TYPE                                     
         MVI   CTCPSUB,CTCPSUBQ    SET SUBTYPE                                  
         MVC   CTCPAGMD,BAGYMD     AGENCY MEDIA                                 
         MVC   CTCPCNTR,CONTR      SET CONTRACTOR                               
         MVC   CTCPCNUM,CNUMPWOS   SET CONTRACT NUMBER                          
         MVI   CTCPCNUM+L'CTCPCNUM,0                                            
         MVC   CTAKDA,DMDSKADD     PUT DISK ADDRESS IN KEY                      
         GOTO1 ADD                 ADD PASSIVE KEY                              
         DROP  R6                                                               
                                                                                
         B     VR510                                                            
                                                                                
VR500    TM    RECFLAG3,RECCHNGD   CONTRACT RECORD CHANGED                      
         BNO   VR510                                                            
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'CTAKEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE IN AIO2                      
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
                                                                                
         MVC   AIO,AIO1                                                         
         LA    R6,KEY                                                           
         USING CTAKEY,R6                                                        
         NI    CTAKCNTL,X'FF'-CTAKCBAL   CONTRACT NOT OUT OF BALANCE            
         L     R6,AIO                                                           
         NI    CTARCNTL,X'FF'-CTARCBAL   CONTRACT NOT OUT OF BALANCE            
         DROP  R6                                                               
                                                                                
         GOTO1 PUTREC                                                           
         GOTO1 WRITE                                                            
                                                                                
**********************   PASSIVE POINTERS   ***************************         
VR510    NI    RECFLAG1,X'FF'-DOLLRAMT-PRCNTAMT-NOAMOUNT-KEYCHNGD               
         NI    RECFLAG2,X'FF'-DELP-ADDP                                         
         MVI   IOOPT,C'Y'                                                       
                                                                                
         TM    RECFLAG1,SAMEPART   PARTICIPANTS DID NOT CHANGE                  
         BNO   *+12                                                             
         NI    RECFLAG1,X'FF'-SAMEPART                                          
         B     DR                                                               
                                                                                
         L     R6,AIO1                                                          
NEW      USING CTPARD,R6                                                        
                                                                                
         L     R3,AIO2                                                          
OLD      USING CTPARD,R3                                                        
         MVI   ELCODE,CTPARELQ     PARTICIPANTS ELEMENT                         
                                                                                
         BAS   RE,GETEL                                                         
         BE    *+12                                                             
         OI    RECFLAG2,DELP       NO MORE NEW PASSIVE POINTERS                 
         B     *+10                                                             
         MVC   STAB,NEW.CTPARSTA                                                
                                                                                
         TM    RECFLAG1,NEWCNTRT   NEW CONTRACT?                                
         BO    *+12                NO OLD CONTRACT TO GET ELEMENT FROM          
         BAS   RE,GETEL2                                                        
         BE    *+8                                                              
         OI    RECFLAG2,ADDP       ADD NEW PASSIVE POINTERS                     
                                                                                
VR520    TM    RECFLAG2,DELP+ADDP  DONE ADDING AND DELETING?                    
         BO    VR550                                                            
         TM    RECFLAG2,ADDP       ADD THE REST OF NEW PASSIVE                  
         BO    VR540                                                            
         TM    RECFLAG2,DELP       DELETE THE REST OF OLD PASSIVE               
         BO    VR530                                                            
                                                                                
         CLC   NEW.CTPARMKT(5),OLD.CTPARMKT                                     
         BH    VR530               DELETE OLD PASSIVE KEY                       
         BL    VR540               ADD NEW KEY                                  
         BAS   RE,NXTLOLD          GET NEXT OLD PASSIVE KEY                     
         BAS   RE,NXTLNEW          GET NEXT NEW PASSIVE KEY                     
         B     VR520                                                            
                                                                                
VR530    GOTO1 =A(DELPOLD),DMCB,(RC),RR=RELO    DELETE PASSIVE KEY              
         BAS   RE,NXTLOLD          GET NEXT OLD PASSIVE KEY                     
         B     VR520                                                            
                                                                                
VR540    GOTO1 =A(ADDPNEW),DMCB,(RC),RR=RELO    ADD NEW PASSIVE KEY             
         BAS   RE,NXTLNEW          GET NEXT NEW PASSIVE KEY                     
         B     VR520                                                            
                                                                                
VR550    NI    RECFLAG2,X'FF'-GCICHNGD                                          
         MVC   KEY(L'CXRKEY),CNTRKEY   READ THE CONTRACTOR RECORD               
         GOTO1 HIGH                                                             
         CLC   KEY(L'CXRKEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE IN AIO3                      
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
                                                                                
         LA    R6,KEY                                                           
         USING CXRRECD,R6                                                       
         TM    CXRKCNTL,CXRKCCNT   ALREADY ON?                                  
         BO    VRX                                                              
         OI    CXRKCNTL,CXRKCCNT   SO WE KNOW A CONTRACT RECORD EXISTS          
         L     R6,AIO                                                           
         OI    CXRRCNTL,CXRRCCNT                                                
         DROP  R6                                                               
                                                                                
         GOTO1 PUTREC                                                           
         GOTO1 WRITE                                                            
                                                                                
VRX      MVC   AIO,AIO1                                                         
         B     DR                                                               
                                                                                
         EJECT                                                                  
***********************************************************************         
*                       NEXTEL W/ IO2 (NEW ELEMENTS)                            
***********************************************************************         
NXTLNEW  LR    R0,RE                                                            
         BAS   RE,NEXTEL                                                        
         BE    *+12                                                             
         OI    RECFLAG2,DELP                                                    
         LR    RE,R0                                                            
         BR    RE                                                               
                                                                                
         CLC   NEW.CTPARSTA,STAB   CHECK FOR DUPLICATE                          
         BE    ABEND                                                            
         MVC   STAB,NEW.CTPARSTA                                                
         LR    RE,R0                                                            
         BR    RE                                                               
                                                                                
***********************************************************************         
*                       NEXTEL W/ IO1 (OLD ELEMENTS)                            
***********************************************************************         
NXTLOLD  LR    R0,RE                                                            
         BAS   RE,NEXTEL2                                                       
         BE    *+8                                                              
         OI    RECFLAG2,ADDP                                                    
         LR    RE,R0                                                            
         BR    RE                                                               
         DROP  NEW,OLD                                                          
                                                                                
***********************************************************************         
*                            DISPLAY RECORD                           *         
***********************************************************************         
DR       DS    0H                                                               
         MVC   MYLSTACT,ACTEQU                                                  
         OI    RECFLAG1,DSPLYREC   DISPLAY RECORD MODE                          
         GOTO1 =A(CS),DMCB,(RC),RR=RELO     CLEAR SCREEN                        
                                                                                
         MVC   AIO,AIO1                                                         
                                                                                
         MVI   RECFLAG3,0          CLEAR FLAGS                                  
                                                                                
         L     R6,AIO              A(FIRST ELEMENT)                             
         USING CTAKEY,R6                                                        
         TM    CTARCNTL,CTARCACC   ACC CONTRACT RECORD EXISTS?                  
         BNO   *+8                                                              
         OI    RECFLAG3,ACCRECRD                                                
                                                                                
         TM    CTARCNTL,CTARCBAL   CONTRACT OUT OF BALANCE?                     
         BNO   *+8                                                              
         OI    RECFLAG3,CTRNTBAL                                                
                                                                                
         DROP  R6                                                               
                                                                                
         USING CTDSCEL,R6                                                       
         MVI   ELCODE,CTDSCELQ     DESCRIPTION ELEMENT                          
         BAS   RE,GETEL            REQUIRED                                     
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   CTMCNTR,CTDSCNTR    CONTRACTOR (CODE)                            
         MVC   DISPDCTR,CTDSCNTR   SAVE DISPLAYED CONTRACTOR (CODE)             
         OI    CTMCNTRH+6,X'80'                                                 
         OI    CTMCNTRH+4,X'20'    PREVIOUSLY  VALIDATED                        
                                                                                
* GO GET CONTRACTOR NAME                                                        
                                                                                
         MVC   CONTR,CTDSCNTR      CONTRACTOR                                   
         GOTO1 =A(CNTR),DMCB,(RC),RR=RELO   CHECK IF CONTRACTOR EXISTS          
                                                                                
DR10     OC    CTDSCST,CTDSCST                                                  
         BZ    DR20                                                             
         GOTO1 DATCON,DMCB,(3,CTDSCST),(5,CTMCONS)                              
         OI    CTMCONSH+6,X'80'    CONTRACT START DATE                          
         OI    CTMCONSH+4,X'20'                                                 
                                                                                
         GOTO1 DATCON,DMCB,(3,CTDSCEND),(5,CTMCONE)                             
         OI    CTMCONEH+6,X'80'    CONTRACT END DATE                            
         OI    CTMCONEH+4,X'20'                                                 
                                                                                
DR20     MVC   DISPDGCI,CTDSCGCI   SAVE GCI DISPLAYED                           
         MVC   GCI,CTDSCGCI        SAVE GCI DISPLAYED                           
         OC    CTDSCGCI,CTDSCGCI                                                
         BZ    DR30                                                             
         EDIT  (4,CTDSCGCI),(9,CTMGCI),2        GCI                             
         OI    CTMGCIH+6,X'80'                                                  
         OI    CTMGCIH+4,X'20'                                                  
                                                                                
DR30     OC    CTDSCNCI,CTDSCNCI                                                
         BZ    DR40                                                             
         EDIT  (4,CTDSCNCI),(9,CTMNCI),2        NCI                             
         OI    CTMNCIH+6,X'80'                                                  
                                                                                
DR40     MVC   DISPDCOM,CTDSCCOM   SAVE COMMISSION DISPLAYED                    
         OI    CTMCOMMH+6,X'80'                                                 
         OI    CTMCOMMH+4,X'20'                                                 
         XC    CTMCOMM,CTMCOMM                                                  
         OC    CTDSCCOM,CTDSCCOM   COMMISSIONABLE?                              
         BNZ   DR50                                                             
         MVI   CTMCOMM,C'N'                                                     
         B     DR60                                                             
                                                                                
DR50     CLC   CTDSCCOM,=F'8500'   85%?                                         
         BNE   *+12                                                             
         MVI   CTMCOMM,C'Y'        COMMISSIONABLE?                              
         B     DR60                                                             
                                                                                
         EDIT  (4,CTDSCCOM),(5,CTMCOMM),2       COMMISSABLE %                   
DR60     TM    RECFLAG3,COMCHGER                                                
         BNO   DR70                                                             
         NI    RECFLAG3,X'FF'-COMCHGER          COMMISSION CHANGE ERROR         
         LA    R2,CTMCOMMH                                                      
         MVC   ERRNUM,=AL2(SE#USCTG)    MUST USE CTAGS TO CHANGE                
         B     SPCURSER                                                         
                                                                                
DR70     OI    CTMCTYPH+6,X'80'                                                 
         OI    CTMCTYPH+4,X'20'                                                 
                                                                                
         TM    CTDSCTYP,X'40'      CONTRACT TYPE IS BB?                         
         BNO   *+10                                                             
         MVC   CTMCTYP(2),=C'BB'                                                
                                                                                
         TM    CTDSCTYP,X'20'      CONTRACT TYPE IS INV?                        
         BNO   *+10                                                             
         MVC   CTMCTYP(3),=C'INV'                                               
                                                                                
DR80     OI    CTMTRCMH+4,X'20'                                                 
         OC    CTDSCTRD,CTDSCTRD                                                
         BZ    DR90                                                             
         EDIT  (4,CTDSCTRD),(6,CTMTRCM),2       TRADE COMMITMENT                
         OI    CTMTRCMH+6,X'80'                                                 
                                                                                
DR90     OI    CTMCLT1H+4,X'20'                                                 
         OI    CTMPRD1H+4,X'20'                                                 
         OC    CTDSCLT1,CTDSCLT1                                                
         BZ    DR100                                                            
         GOTO1 CLUNPK,DMCB,CTDSCLT1,CTMCLT1     PRINT CLIENT2                   
         OI    CTMCLT1H+6,X'80'                                                 
                                                                                
         CLI   CTDSPRD1,0                                                       
         BE    DR100                                                            
         MVC   CLT,CTDSCLT1        CLIENT CODE                                  
         MVC   PRDB,CTDSPRD1       PRODUCT CODE                                 
         BAS   RE,PRDT                                                          
         MVC   CTMPRD1,PRD         PRODUCT1                                     
         OI    CTMPRD1H+6,X'80'                                                 
                                                                                
DR100    OI    CTMPER1H+4,X'20'                                                 
         OC    CTDSPER1,CTDSPER1                                                
         BZ    DR110                                                            
         LA    RE,CTDSPER1         PERIOD 1                                     
         ST    RE,DMCB                                                          
         MVI   DMCB,3                                                           
         OI    DMCB,X'20'                                                       
         GOTO1 DATCON,DMCB,,(5,CTMPER1),CTDSPER1+3                              
         OI    CTMPER1H+6,X'80'                                                 
                                                                                
DR110    OI    CTMCLT2H+4,X'20'                                                 
         OI    CTMPRD2H+4,X'20'                                                 
         OC    CTDSCLT2,CTDSCLT2                                                
         BZ    DR120                                                            
         GOTO1 CLUNPK,DMCB,CTDSCLT2,CTMCLT2     PRINT CLIENT2                   
         OI    CTMCLT2H+6,X'80'                                                 
                                                                                
         CLI   CTDSPRD2,0                                                       
         BE    DR120                                                            
         MVC   CLT,CTDSCLT2        CLIENT CODE                                  
         MVC   PRDB,CTDSPRD2       PRODUCT CODE                                 
         BAS   RE,PRDT                                                          
         MVC   CTMPRD2,PRD         PRODUCT2                                     
         OI    CTMPRD2H+6,X'80'                                                 
                                                                                
DR120    OC    CTDSPER2,CTDSPER2                                                
         BZ    DR130                                                            
         LA    RE,CTDSPER2         PERIOD 2                                     
         ST    RE,DMCB                                                          
         MVI   DMCB,3                                                           
         OI    DMCB,X'20'                                                       
         GOTO1 DATCON,DMCB,,(5,CTMPER2),CTDSPER2+3                              
         OI    CTMPER2H+6,X'80'                                                 
         OI    CTMPER2H+4,X'20'                                                 
                                                                                
DR130    MVC   CTMRDRT,CTDSCRDR    RDR TRADER                                   
         OI    CTMRDRTH+6,X'80'                                                 
         OI    CTMRDRTH+4,X'20'                                                 
                                                                                
         MVC   CTMMEDB,CTDSCBYR    MEDIA BUYER NAME                             
         OI    CTMMEDBH+6,X'80'                                                 
         OI    CTMMEDBH+4,X'20'                                                 
                                                                                
         MVI   CTMSEND,C'N'        DON'T SEND CONTRACT?                         
         TM    CTDSCTYP,X'10'                                                   
         BO    *+8                                                              
         MVI   CTMSEND,C'Y'                                                     
         OI    CTMSENDH+6,X'80'                                                 
         OI    CTMSENDH+4,X'20'                                                 
                                                                                
         TM    RECFLAG2,DELETERR                                                
         BNO   DR150                                                            
                                                                                
DR140    L     R6,AIO              A(FIRST ELEMENT)                             
         USING CTDSCEL,R6                                                       
         MVI   ELCODE,CTDSCELQ     DESCRIPTION ELEMENT                          
         BAS   RE,GETEL            REQUIRED                                     
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
DR150    TM    CTDSCTYP,X'80'      PARTICIPANTS IN DOLLARS?                     
         BNO   *+8                                                              
         OI    RECFLAG1,DOLLRAMT                                                
                                                                                
         TM    CTDSCTYP,X'04'+X'80' PARTICIPANTS HAVE NO AMOUNTS                
         BNZ   DR160                BY NOT BEING IN DOLLARS OR PERCENT          
         OI    RECFLAG1,NOAMOUNT                                                
                                                                                
DR160    L     R6,AIO              A(FIRST ELEMENT)                             
         USING CTPARD,R6                                                        
         MVI   ELCODE,CTPARELQ     PARTICIPANTS ELEMENT                         
         BAS   RE,GETEL                                                         
         BNE   DR280                                                            
                                                                                
         XC    MKTSTAB,MKTSTAB                                                  
         LA    R4,MYBLOCK                                                       
         MVC   MYBLOCK(L'CTMPARF+L'CTMPARL),SPACES                              
                                                                                
DR170    DS    0H                                                               
         ZIC   R1,FIELDERR                                                      
         LA    R1,1(R1)                                                         
         STC   R1,FIELDERR                                                      
         TM    RECFLAG2,STANOFND                                                
         BNO   DR180                                                            
                                                                                
         CLC   STADEL,CTPARSTA                                                  
         BNE   *+8                                                              
         NI    RECFLAG2,X'FF'-STANOFND                                          
                                                                                
DR180    MVC   STAB,CTPARSTA                                                    
         GOTO1 MSUNPK,DMCB,MKTSTAB,MKT,(R4)                                     
         LA    R4,4(R4)            LONGEST POSSIBLE LENGTH IS 5                 
         CLI   0(R4),C' '          CHECK FOR TRAILING SPACES                    
         BH    *+8                                                              
         BCT   R4,*-8                                                           
                                                                                
         CLI   FIELDERR,1                                                       
         BNE   DR190                                                            
         LR    R1,R4                                                            
         LA    R0,MYBLOCK                                                       
         SR    R1,R0                                                            
         STC   R1,STALEN                                                        
                                                                                
DR190    MVI   1(R4),C'='                                                       
         LA    R4,2(R4)                                                         
                                                                                
         TM    RECFLAG1,NOAMOUNT   NO AMOUNT                                    
         BNO   *+10                                                             
         BCTR  R4,0                ERASE '='                                    
         B     DR220                                                            
                                                                                
         TM    RECFLAG1,DOLLRAMT                                                
         BNO   DR200                                                            
         EDIT  (4,CTPARAMT),(9,(R4)),2,FLOAT=$,ALIGN=LEFT                       
         B     DR210                                                            
DR200    EDIT  (4,CTPARPCT),(9,(R4)),2,ALIGN=LEFT                               
DR210    AR    R4,R0                                                            
         LR    R1,R4               CHECK IF DECIMALS ARE 0'S                    
         SH    R1,=H'3'                                                         
         CLC   1(2,R1),=C'00'                                                   
         BNE   DR220                                                            
         LR    R4,R1                                                            
DR220    MVC   0(3,R4),=C',  '     WRITE OVER 2 DECIMAL PLACES                  
                                                                                
         LA    R4,1(R4)                                                         
                                                                                
         BAS   RE,NEXTEL                                                        
         BE    DR170                                                            
                                                                                
         TM    RECFLAG2,DELETERR                                                
         BNO   *+14                                                             
         OI    RECFLAG2,STANOFND                                                
         MVC   FIELD,FIELDERR                                                   
                                                                                
         LA    R2,CTMPARFH         FIRST PARTICIPANTS LINE                      
         LA    R1,CTMPARLH         LAST PARTICIPANTS LINE                       
         ST    R1,LASTPART                                                      
         LA    R4,MYBLOCK                                                       
         LA    R3,L'CTMPARF(R4)                                                 
                                                                                
DR230    L     R1,LASTPART                                                      
         CR    R2,R1                                                            
         BH    DR280                                                            
                                                                                
         CLI   0(R3),C','          LOOK FOR LAST COMMA                          
         BE    *+8                                                              
         BCT   R3,*-8                                                           
                                                                                
         LR    R1,R3                                                            
         SR    R1,R4               LENGTH TO PRINT                              
         STC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),0(R4)                                                    
                                                                                
         OI    6(R2),X'80'         TRANSMIT                                     
         OI    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         TM    RECFLAG2,STANOFND                                                
         BNO   DR260                                                            
                                                                                
         ZIC   RE,5(R2)                                                         
         MVC   FIELDERR,FIELD                                                   
         LA    R1,8(R2)                                                         
         BCTR  R1,0                                                             
                                                                                
DR240    ZIC   RF,FIELD                                                         
         BCTR  RF,0                                                             
         STC   RF,FIELD                                                         
                                                                                
         CLI   FIELD,0                                                          
         BNE   DR250                                                            
         ST    R2,SAVEADR                                                       
         NI    RECFLAG2,X'FF'-STANOFND                                          
         B     DR260                                                            
                                                                                
DR250    LA    R1,1(R1)                                                         
         CLI   0(R1),C','                                                       
         BE    *+12                                                             
         BCT   RE,DR250                                                         
         B     DR260                                                            
         BCT   RE,DR240                                                         
                                                                                
DR260    ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
                                                                                
         LA    R4,1(R3)            POINT TO FRONT OF NEXT LINE                  
         LA    R3,L'CTMPARF(R3)                                                 
         CLI   0(R4),C' '          CHECK IF THERE'S MORE TO PRINT               
         BNE   DR230                                                            
                                                                                
DR270    L     R1,LASTPART         MARK THE REST OF THE LINES                   
         CR    R2,R1                                                            
         BH    DR280                                                            
         OI    4(R2),X'20'         MARK PREVIOUSLY VALIDATED                    
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         B     DR270                                                            
         DROP  R6                                                               
                                                                                
DR280    L     R6,AIO              A(FIRST ELEMENT)                             
         USING ACTVD,R6                                                         
         MVI   ELCODE,X'F1'        ACTIVITY ELEMENT                             
         BAS   RE,GETEL            REQUIRED                                     
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTO1 DATCON,DMCB,(3,ACTVADDT),(5,CTMCADD)                             
         OI    CTMCADDH+6,X'80'    DATE CONTRACT ADDED                          
                                                                                
         GOTO1 DATCON,DMCB,(3,ACTVCHDT),(5,CTMCCHG)                             
         OI    CTMCCHGH+6,X'80'    DATE CONTRACT LAST CHANGED                   
                                                                                
         XC    CTMPF,CTMPF         CLEAR PFKEYS LINE                            
         CLI   ACTEQU,ACTSEL       SELECT                                       
         BE    DR290                                                            
                                                                                
         MVC   CTMPF(L'MNTPFLN),MNTPFLN    MAINT PFKEY LINE                     
                                                                                
         L     R1,ATWA             CALLSTCK & CALLSP ARE HERE BECAUSE           
         AH    R1,=Y(TWAENDLQ-2)     SPSFM HAS NO ROOM IN ITS SAVE AREA         
         CLI   1(R1),1             CHECK IF USED PFKEY TO GET HERE              
         BNE   DR290                                                            
                                                                                
         MVC   CTMPF+L'MNTPFLN+2(L'MNTPFRTN),MNTPFRTN                           
         OI    CTMPFH+6,X'80'                                                   
         OI    CTMPFH+1,X'08'      HIGHLIGHT                                    
                                                                                
DR290    OI    CTMPFH+6,X'80'                                                   
         NI    RECFLAG1,X'FF'-DSPLYREC-DOLLRAMT-PRCNTAMT-NOAMOUNT               
         TM    RECFLAG2,DELETERR                                                
         BNO   DR300                                                            
         NI    RECFLAG2,X'FF'-DELETERR                                          
         L     R2,SAVEADR                                                       
         MVC   ERRNUM,=AL2(SE#CNCHP)    CHANGE PART. AMOUNT BELOW USAGE         
         B     SPCURSER                                                         
                                                                                
DR300    TM    RECFLAG3,CTRNTBAL                                                
         BNO   DR310                                                            
         NI    RECFLAG3,X'FF'-CTRNTBAL                                          
         MVC   ERRNUM,=AL2(SE#SMGCI)                                            
         LA    R2,CTMPARFH                                                      
         B     SPERREX             SUM DOES NOT EQUAL GCI                       
                                                                                
DR310    TM    RECFLAG3,DUPSTATN                                                
         BNO   DRX                                                              
         NI    RECFLAG3,X'FF'-DUPSTATN                                          
         MVC   ERRNUM,=AL2(SE#DUPLC)                                            
         LA    R2,CTMPARFH                                                      
         B     SPCURSER            SUM DOES NOT EQUAL GCI                       
                                                                                
DRX      TM    RECFLAG1,NEWCNTRT                                                
         BNO   XIT                                                              
         NI    RECFLAG1,X'FF'-NEWCNTRT                                          
         B     DK                                                               
         EJECT                                                                  
***********************************************************************         
*                            LIST RECORD                              *         
***********************************************************************         
LR       DS    0H                                                               
         LA    R4,KEY                                                           
         USING CTARECD,R4                                                       
                                                                                
         XC    MKT,MKT                                                          
                                                                                
         CLI   XFRCALL,C'Y'        CALLED WITH GLOBBER (RTN FROM CTAGS)         
         BE    LR10                                                             
                                                                                
         TM    TRNSTAT,RETURNED    RETURNED FROM USAGE SCREEN                   
         BO    LR10                                                             
                                                                                
         OC    KEY(CTAEL-CTAPASS),KEY  FIRST TIME THROUGH?                      
         BZ    LR20                                                             
                                                                                
         CLI   MYLSTACT,ACTLIST                                                 
         BNE   *+14                                                             
         MVC   KEY(13),LASTKEY     LAST KEY ON LIST SCREEN                      
         B     LR60                                                             
                                                                                
LR10     MVC   KEY(13),FIRSTKEY    RESTORE KEY                                  
         B     LR60                                                             
                                                                                
LR20     XC    MKTSTAB,MKTSTAB                                                  
         XC    CNUMPWOS,CNUMPWOS                                                
         LA    R2,CTLMKSTH                                                      
         CLI   5(R2),0             START FOR MKT OR STATION                     
         BE    LR40                                                             
                                                                                
* START AT MKT(STA)                                                             
         TM    4(R2),X'08'         VALID NUMERIC?                               
         BNO   LR30                                                             
                                                                                
         ZIC   R1,5(R2)            LENGTH OF MARKET NUMBER                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)         MARKET NUMBER                                
         CVB   R3,DUB                                                           
         STCM  R3,3,MKTB                                                        
                                                                                
         B     LR40                                                             
                                                                                
LR30     CLI   12(R2),C'='         CHECK IF '=' BECAUSE PF'ED FROM              
         BNE   *+8                 MAINT SCREEN                                 
         MVI   12(R2),C' '                                                      
         MVI   USEIONUM,2                                                       
         GOTO1 VALISTA             VALIDATE STATION                             
         MVI   USEIONUM,0                                                       
         MVC   STAB,BSTA           BINARY STATION                               
         MVC   MKTB,BMKT           BINARY STATION                               
                                                                                
LR40     XC    CNUMP,CNUMP                                                      
         LA    R2,CTLCNUMH                                                      
         ZICM  R1,5(R2),1          LENGTH OF CONTRACT NUMBER                    
         BZ    LR50                                                             
         TM    4(R2),X'08'         VALID NUMERIC?                               
         BNO   NOTNUMGN            MUST BE NUMERIC                              
                                                                                
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  CNUMP,8(0,R2)       CONTRACT NUMBER                              
                                                                                
* BUILD KEY                                                                     
LR50     XC    KEY,KEY                                                          
         MVI   CTAPTYP,CTAPTYPQ    PASSIVE KEY                                  
         MVI   CTAPSUB,CTAPSUBQ    PASSIVE KEY                                  
         MVC   CTAPAGMD,BAGYMD     MEDIA                                        
         MVC   CTAPMKT(5),MKTSTAB  BINARY MARKET STATION                        
                                                                                
*        SAVE RECORD TYPE                                                       
LR60     MVC   SAVEKEY(L'CTAPTYP+L'CTAPSUB+L'CTAPAGMD),KEY                      
                                                                                
         MVC   FIRSTKEY(13),KEY    FIRST KEY ON LIST SCREEN                     
         GOTO1 HIGH                                                             
                                                                                
*        SAME RECORD TYPE?                                                      
LR70     CLC   KEY(L'CTAPTYP+L'CTAPSUB+L'CTAPAGMD),SAVEKEY                      
         BNE   LRX                                                              
                                                                                
         MVC   LASTKEY(13),KEY     LAST KEY ON LIST SCREEN                      
         XC    LISTAR,LISTAR                                                    
                                                                                
         XC    WORK1,WORK1                                                      
         OI    WORK1+3,X'0F'                                                    
         MVO   WORK1,CTAPCNUM      CONVERT TO PACKED                            
                                                                                
         CLC   CNUMP,WORK1                                                      
         BH    LR160                                                            
                                                                                
         EDIT  (P4,WORK1),(5,LCNUM)      CONTRACT NUMBER                        
                                                                                
         GOTO1 MSUNPK,DMCB,CTAPMKT,LMKT,LSTA      MARKET & STATION              
         CLC   MKT,LMKT                                                         
         BNE   *+14                                                             
         XC    LMKT,LMKT           DON'T PRINT MKT # AGAIN                      
         B     LR80                                                             
         MVC   MKT,LMKT            SAVE PREVIOUS MARKET #                       
                                                                                
* PRINT MARKET NAME                                                             
         MVC   SAVEKEY,KEY         SAVE DIRECTORY POINTER                       
         LA    R2,FAKEHDR                                                       
         XC    FAKEFLD,FAKEFLD                                                  
         XC    FAKEHDR,FAKEHDR                                                  
         MVI   4(R2),X'08'         SET VALID NUMERIC                            
         MVI   5(R2),4             STORE INPUT LENGTH IN FIELD HEADER           
         MVC   FAKEFLD(4),MKT      STORE MARKET                                 
                                                                                
         MVI   USEIONUM,2          USE IO2 FOR VALISTA                          
         GOTO1 VALIMKT             VALIDATE MARKET                              
         MVI   USEIONUM,0                                                       
         MVC   LMKTNAME,MKTNM      MARKET NAME                                  
         MVC   KEY(18),SAVEKEY     RESTORE KEY                                  
                                                                                
LR80     GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R6                                                               
                                                                                
         L     R6,AIO                                                           
         USING CTDSCEL,R6          DESCRIPTION ELEMENT                          
         MVI   ELCODE,CTDSCELQ     ELEMENT CODE                                 
                                                                                
         BAS   RE,GETEL            REQUIRED                                     
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         TM    CTDSCTYP,X'04'+X'80' PARTICIPANTS HAVE NO AMOUNTS                
         BNZ   *+8                  BY NOT BEING IN DOLLARS OR PERCENT          
         OI    RECFLAG1,NOAMOUNT                                                
                                                                                
         TM    CTDSCTYP,X'80'      DOLLARS?                                     
         BNO   *+8                                                              
         OI    RECFLAG1,DOLLRAMT                                                
                                                                                
         MVC   LCNTR,CTDSCNTR      CONTRACTOR CODE                              
         MVC   GCI,CTDSCGCI                                                     
         DROP  R6                                                               
                                                                                
         L     R6,AIO                                                           
         USING CTPARD,R6           PARTICIPANT ELEMENT                          
         MVI   ELCODE,CTPARELQ     ELEMENT CODE                                 
                                                                                
         BAS   RE,GETEL            REQUIRED                                     
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
LR90     CLC   CTAPMKT(5),CTPARMKT  FIND PARTICIPANT ELEMENT FOR                
         BE    LR100                W/ SAME MARKET & STATION                    
         BAS   RE,NEXTEL                                                        
         BE    LR90                                                             
         DC    H'0'                                                             
                                                                                
LR100    TM    RECFLAG1,NOAMOUNT                                                
         BNO   LR110                                                            
         XC    BINSTA,BINSTA                                                    
         BAS   RE,CUSAGE                                                        
         L     RF,GCI                                                           
         B     LR130                                                            
                                                                                
LR110    DS    0H                                                               
         MVC   BINSTA,CTAPSTA                                                   
         BAS   RE,CUSAGE                                                        
         ICM   RF,15,CTPARAMT      PARTICIPANT AMOUNT                           
                                                                                
LR130    SR    RE,RE                                                            
         D     RE,=F'100'          LOOSE 2 DECIMAL PLACES                       
         LR    R3,RF                                                            
                                                                                
         EDIT  (R3),(8,LGCI)       GCI                                          
         SR    RE,RE                                                            
         ICM   RF,15,USAGE                                                      
         D     RE,=F'100'                                                       
         SR    R3,RF               BALANCE = GCI - USAGE                        
                                                                                
         TM    RECFLAG1,NOAMOUNT                                                
         BNO   LR140                                                            
         LTR   R3,R3                                                            
         BM    LR140                                                            
         EDIT  (R3),(9,LBALANCE),TRAIL=C'*',ZERO=NOBLANK                        
         B     LR150                                                            
LR140    EDIT  (R3),(9,LBALANCE),MINUS=YES,ZERO=NOBLANK                         
                                                                                
LR150    MVC   MYLSTACT,ACTEQU                                                  
         NI    RECFLAG1,X'FF'-DOLLRAMT-NOAMOUNT                                 
         NI    RECFLAG2,X'FF'-INACTIVE                                          
         GOTO1 LISTMON             PRINT LINE                                   
                                                                                
LR160    GOTO1 SEQ                 NEXT CONTRACTOR RECORD                       
         LA    R4,KEY              POINT R4 BACK TO KEY                         
         B     LR70                                                             
         DROP  R4,R6                                                            
                                                                                
LRX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                          DISPLAY USAGE                              *         
***********************************************************************         
* DISPLAY THE BALANCE, GCI AND USAGES FOR ALL PARTICIPANTS                      
DU       DS    0H                                                               
         TM    RECFLAG1,KEYCHNGD   KEY CHANGED                                  
         BO    DU03                                                             
         CLI   MYLSTACT,20                                                      
         BNE   DU03                                                             
         TM    USGFLAG1,CONTUSGE                                                
         BNO   DU05                                                             
         LA    R3,BUFF                                                          
         LR    R6,R3                                                            
         A     R3,SAVEDSP1                                                      
         A     R6,SAVEDSP2                                                      
****     MVC   AIO,AIO1                                                         
****     MVI   ELCODE,CTAUSELQ     ELEMENT CODE                                 
         B     DU25                                                             
                                                                                
DU03     XC    CTUBALN,CTUBALN     CLEAR BALANCE                                
         OI    6(R2),X'80'         TRANSMIT                                     
                                                                                
DU05     MVI   USGFLAG1,FRSPAREL   FIRST PARTICIPANT ELEMENT                    
         L     R1,ATWA             CALLSTCK & CALLSP ARE HERE BECAUSE           
         AH    R1,=Y(TWAENDLQ-2)     SPSFM HAS NO ROOM IN ITS SAVE AREA         
         CLI   1(R1),1             CHECK IF USED PFKEY TO GET HERE              
         BE    DU10                                                             
                                                                                
         CLI   XFRCALL,C'Y'        CALLED WITH GLOBBER?                         
         BNE   DU20                                                             
DU10     MVC   CTUPF(11),=C'PF12=RETURN'                                        
         OI    CTUPFH+6,X'80'                                                   
         OI    CTUPFH+1,X'08'      HIGHLIGHT                                    
                                                                                
DU20     GOTO1 HIGH                                                             
         MVC   ERRNUM,=AL2(SE#CNTFD)  CONTRACT NOT FOUND                        
         CLC   KEY(L'CTAKEY),KEYSAVE                                            
         BNE   SPERREX                                                          
                                                                                
         GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),=C'SPTFILE',KEY+14,BUFF,    X        
               DMWORK                                                           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    R6,BUFF                                                          
         USING CTDSCEL,R6          DESCRIPTION ELEMENT                          
         MVI   ELCODE,CTDSCELQ     ELEMENT CODE                                 
                                                                                
         BAS   RE,GETEL            REQUIRED                                     
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   CTUCNTR,CTDSCNTR    CONTRACTOR (CODE)                            
         OI    CTUCNTRH+6,X'80'                                                 
                                                                                
         MVC   GCI,CTDSCGCI                                                     
         MVC   BALANCE,CTDSCGCI                                                 
         EDIT  (4,CTDSCGCI),(10,CTUGCI),2                                       
         OI    CTUGCIH+6,X'80'                                                  
         DROP  R6                                                               
                                                                                
         XC    MKTSTAB,MKTSTAB                                                  
                                                                                
DU25     LA    R2,CTULINEH                                                      
         LA    R1,CTUPFH                                                        
         ST    R1,SAVEADR                                                       
                                                                                
* CLEAR THE REST OF THE FIELDS ON SCREEN                                        
DU30     L     R1,SAVEADR                                                       
         CR    R1,R2               END OF FIELDS                                
         BNH   DU40                                                             
         XC    8(L'CTULINE,R2),8(R2)  CLEAR FIELD                               
         OI    6(R2),X'80'         TRANSMIT                                     
                                                                                
         ZIC   R1,0(R2)            GO TO NEXT FIELD                             
         AR    R2,R1                                                            
         B     DU30                                                             
                                                                                
DU40     LA    R2,CTULINEH                                                      
         USING USAGED,R2                                                        
         TM    USGFLAG1,CONTUSGE                                                
         BO    DU100                                                            
         XC    STAB,STAB           LOOK FOR USAGE ELEMENTS W/O A MKTSTA         
         MVC   STATN,=C'BF   '      (BALANCE BROUGHT FORWARD)                   
         B     DU80                                                             
                                                                                
DU45     LA    R6,BUFF                                                          
         USING CTPARD,R6           PARTICIPANT ELEMENT                          
         MVI   ELCODE,CTPARELQ     ELEMENT CODE                                 
         BAS   RE,GETEL                                                         
         BNE   DU150                                                            
         B     DU70                                                             
                                                                                
DU50     NI    USGFLAG1,X'FF'-NOMRUSGE                                          
         CLC   =C'BF',STATN                                                     
         BNE   *+12                                                             
         TM    USGFLAG1,BFUELFND   BF USAGE ELEMENT FOUND?                      
         BNO   DU45                                                             
         MVC   USTATN,STATN        STATION                                      
         MVC   UCLT(15),=C'*** TOTALS  ***'                                     
         SR    RE,RE                                                            
         L     RF,TORDERED         TOTAL ORDERED                                
         D     RE,=F'100'          LOOSE TWO DECIMAL PLACES                     
         ST    RF,WORK1                                                         
         EDIT  (B4,WORK1),(7,UORDERED),ZERO=NOBLANK       TOTAL ORDERED         
         MVI   UORDERED+L'UORDERED,C'*'                                         
                                                                                
         SR    RE,RE                                                            
         ZICM  RF,TALLOCAT,4       TOTAL ALLOCATED                              
         BZ    DU60                                                             
         D     RE,=F'100'          LOOSE TWO DECIMAL PLACES                     
         ST    RF,WORK1                                                         
         EDIT  (B4,WORK1),(7,UALLOCAT)                  TOTAL ALLOCATED         
         MVI   UALLOCAT+L'UALLOCAT,C'*'                                         
                                                                                
         SR    RE,RE                                                            
         L     RF,TORDERED         TOTAL ORDERED                                
         D     RE,=F'100'          LOOSE TWO DECIMAL PLACES                     
                                                                                
         L     R1,WORK1                                                         
         SR    R1,RF               ALLLOCATED - ORDERED                         
         ST    R1,WORK1            TOTAL BALANCE                                
         EDIT  (B4,WORK1),(8,UBALANCE),ZERO=NOBLANK,MINUS=YES                   
         ZICM  R1,WORK1,4                                                       
         BM    *+8                                                              
         MVI   UBALANCE+L'UBALANCE-1,C'*'                                       
                                                                                
DU60     SR    RE,RE                                                            
         L     RF,TPAID            TOTAL PAID                                   
         D     RE,=F'100'          LOOSE TWO DECIMAL PLACES                     
         ST    RF,WORK1                                                         
         EDIT  (B4,WORK1),(7,UPAID),ZERO=NOBLANK    TOTAL PAID                  
         MVI   UPAID+L'UPAID,C'*'                                               
         OI    6(R2),X'80'                                                      
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
                                                                                
         CLC   =C'BF',STATN                                                     
         BE    DU45                                                             
         LR    R6,R3                                                            
         MVI   ELCODE,CTPARELQ     PARTICIPANT ELEMENT                          
         USING CTPARD,R6                                                        
         BAS   RE,NEXTEL                                                        
         BNE   DU150                                                            
                                                                                
DU70     MVC   STAB,CTPARSTA                                                    
         MVC   TALLOCAT,CTPARAMT                                                
         GOTO1 MSUNPK,DMCB,MKTSTAB,MKT,STATN                                    
         DROP  R6                                                               
                                                                                
         LR    R3,R6               SAVE ADDRESS OF ELEMENT                      
         CLC   =C'BF',STATN        DON'T TEST IF BF                             
         BE    DU80                                                             
         TM    USGFLAG1,BFUELFND   BF USAGE ELEMENT FOUND?                      
         BO    DU80                                                             
         TM    USGFLAG1,FRSPAREL   FIRST PARTICIPANT ELEMENT?                   
         BNO   DU80                                                             
         NI    USGFLAG1,X'FF'-FRSPAREL                                          
         BAS   RE,NEXTEL           CHECK IF ONLY ONE PARTICIPANT                
         BE    *+10                                                             
         MVC   TALLOCAT,GCI        YES, TOTAL ALLOCATED IS THE GCI              
                                                                                
DU80     XC    TPAID,TPAID                                                      
         XC    TORDERED,TORDERED                                                
                                                                                
         LA    R6,BUFF                                                          
         MVI   ELCODE,CTAUSELQ     ELEMENT CODE                                 
         USING CTAUSELD,R6         USAGE ELEMENT                                
         BAS   RE,GETEL                                                         
         B     *+8                                                              
DU90     BAS   RE,NEXTEL                                                        
         BE    *+12                                                             
         OI    USGFLAG1,NOMRUSGE                                                
         B     DU95                                                             
         CLC   STAB,CTAUSSTA       CHECK THE STATION                            
         BNE   DU90                                                             
                                                                                
DU95     L     R1,SAVEADR                                                       
         CR    R2,R1               CHECK IF AT END OF SCREEN                    
         BL    DU100                                                            
         OI    USGFLAG1,CONTUSGE                                                
         LA    R1,BUFF                                                          
         SR    R3,R1                                                            
         ST    R3,SAVEDSP1                                                      
         SR    R6,R1                                                            
         ST    R6,SAVEDSP2                                                      
         B     DUX                 NOT ENOUGH ROOM TO DISP ALL STATIONS         
                                                                                
DU100    TM    USGFLAG1,NOMRUSGE                                                
         BO    DU50                                                             
         MVC   USTATN,STATN                                                     
         CLC   =C'BF',STATN                                                     
         BNE   *+12                                                             
         OI    USGFLAG1,BFUELFND   BF USAGE ELEMENT FOUND                       
         B     DU130                                                            
                                                                                
         GOTO1 CLUNPK,DMCB,CTAUSCLT,UCLT        PRINT CLIENT                    
                                                                                
         MVC   CLT,CTAUSCLT        CLIENT CODE                                  
         MVC   PRDB,CTAUSPRD       PRODUCT CODE                                 
         BAS   RE,PRDT                                                          
         MVC   UPRD,PRD            PRODUCT1                                     
                                                                                
         EDIT  (B1,CTAUSEST),(3,UEST),FILL=0    ESTIMATE                        
                                                                                
DU130    DS    0H                                                               
         ICM   R1,15,CTAUSOGR      USAGE(GROSS ORDERED DOLLARS)                 
         L     RE,BALANCE                                                       
         SR    RE,R1                                                            
         ST    RE,BALANCE          BALANCE = GCI - USAGE                        
         A     R1,TORDERED         ADD TO SUM                                   
         ST    R1,TORDERED                                                      
         CLC   =C'BF',STATN                                                     
         BE    DU140                                                            
                                                                                
         SR    RE,RE                                                            
         ICM   RF,15,CTAUSOGR      GROSS PAID DOLLARS                           
         D     RE,=F'100'          LOOSE TWO DECIMAL PLACES                     
         ST    RF,WORK1                                                         
         EDIT  (B4,WORK1),(7,UORDERED),ZERO=NOBLANK    GROSS ORDERED            
                                                                                
DU140    DS    0H                                                               
         ICM   R1,15,CTAUSPGR      GROSS PAID DOLLARS                           
         A     R1,TPAID                                                         
         ST    R1,TPAID                                                         
         CLC   =C'BF',STATN                                                     
         BE    DU90                                                             
                                                                                
         SR    RE,RE                                                            
         ICM   RF,15,CTAUSPGR      GROSS PAID DOLLARS                           
         D     RE,=F'100'          LOOSE TWO DECIMAL PLACES                     
         ST    RF,WORK1                                                         
         EDIT  (B4,WORK1),(7,UPAID),ZERO=NOBLANK       GROSS PAID               
                                                                                
         OI    6(R2),X'80'         DISPLAY STATION                              
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         B     DU90                                                             
                                                                                
DU150    DS    0H                                                               
         EDIT  (B4,BALANCE),(11,CTUBALN),2,MINUS=YES  DISPLAY BALANCE           
         OI    CTUBALNH+6,X'80'                                                 
         NI    USGFLAG1,X'FF'-CONTUSGE                                          
                                                                                
         DROP  R2,R6                                                            
                                                                                
DUX      MVI   MYLSTACT,20                                                      
         NI    RECFLAG1,X'FF'-KEYCHNGD                                          
         B     XIT                                                              
                                                                                
         EJECT                                                                  
***********************************************************************         
*                       TRANSFER TO ACC CTAG                          *         
***********************************************************************         
XFERAC   DS    0H                                                               
         XC    ELEM,ELEM           MEDIA                                        
         MVC   ELEM(1),CTMMED                                                   
         GOTO1 VGLOBBER,DMCB,=C'PUTD',ELEM,1,GLVSPMD                            
                                                                                
         XC    ELEM,ELEM           CONTRACT NUMBER                              
         MVC   ELEM(L'CTMCNUM),CTMCNUM                                          
                                                                                
         CLI   ACTEQU,ACTLIST      ACTION LIST?                                 
         BNE   XFERAC10                                                         
         OI    GENSTAT2,RETEQSEL                                                
         LH    R2,CURDISP          DISPLACEMENT OF SELECT FIELD                 
         AR    R2,RA               ADD THE ADDRESS OF THE TWA                   
         ZIC   R1,0(R2)            BUMP TO LIST LINE                            
         LA    R2,8(R2,R1)                                                      
         USING LMKT,R2                                                          
         MVC   ELEM(L'LCNUM),LCNUM                                              
         DROP  R2                                                               
                                                                                
XFERAC10 GOTO1 VGLOBBER,DMCB,=C'PUTD',ELEM,L'CTMCNUM,GLVSPCON                   
                                                                                
         XC    ELEM,ELEM           CONTRACT NUMBER                              
         MVC   ELEM(3),=C'CTA'                                                  
         GOTO1 VGLOBBER,DMCB,=C'PUTD',ELEM,3,GLVXREC                            
                                                                                
         XC    ELEM,ELEM           CONTRACT NUMBER                              
         MVC   ELEM(3),=C'CHA'                                                  
         GOTO1 VGLOBBER,DMCB,=C'PUTD',ELEM,3,GLVXACT                            
                                                                                
         XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'SPO'                                                 
         MVC   GLVXFRPR,=C'SFM'                                                 
         MVC   GLVXTOSY,=C'ACC'                                                 
         MVC   GLVXTOPR,=C'CTA'                                                 
         OI    GLVXFLG1,GLV1GOTO+GLV1SEPS                                       
         DROP  R1                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',ELEM,14,GLVXCTL                           
         B     YES                                                              
                                                                                
***********************************************************************         
*                       SETUP                                         *         
***********************************************************************         
SETUP    NTR1                                                                   
         L     R2,ATWA             MUST SET INDICTOR TO XMIT ALL FIELDS         
         LA    R2,64(R2)               OR SCREEN WILL BE MESSED UP              
         CLI   0(R2),0                                                          
         BE    *+16                FIND END OF TWA                              
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     *-16                                                             
         MVC   1(2,R2),=X'0101'    SET INDICATOR TO XMIT ALL FIELDS             
                                                                                
         OI    CONSERVH+1,X'01'    INPUT THIS TIME                              
         OI    CONSERVH+6,X'80'    TRANSMIT                                     
         OI    GENSTAT1,NOSETEFH   SO WE CAN PASS KEYS WITH PFKEYS              
         OI    GENSTAT4,NODELLST   DO NOT ALLOW DELETES FROM LIST               
                                                                                
* TEST IF THIS IS A TRANSFER OF CONTROL FROM ANOTHER PROGRAM                    
         L     RF,SYSPARMS                                                      
         L     RF,16(RF)                                                        
         ICM   RF,15,CGLOBBER-COMFACSD(RF)                                      
         BZ    SETUP20                                                          
         ST    RF,VGLOBBER                                                      
                                                                                
         CLI   ACTEQU,20           CAN'T XFER FROM USAGE SCREEN TO ACC          
         BE    SETUP10                                                          
                                                                                
         CLI   PFKEY,4             TRANSFER TO THE ACC PROGRAM?                 
         BE    XFERAC                                                           
                                                                                
SETUP10  BAS   RE,CHKGLB                                                        
         BE    YES                                                              
                                                                                
SETUP20  CLI   ACTEQU,16                        ACTION MAINT                    
         BNE   SETUP40                                                          
                                                                                
* CLEAR PF12=RETURN PORTION SINCE NO LONGER VALID                               
         CLI   PFKEY,4             VALID PFKEYS ARE ONLY 4 - 6                  
         BL    SETUP30                                                          
         CLI   PFKEY,6                                                          
         BH    SETUP30                                                          
         MVC   MPF06A+1(1),STALEN                                               
         XC    CTMPF+L'MNTPFLN+2(L'MNTPFRTN),CTMPF+L'MNTPFLN+2                  
                                                                                
SETUP30  GOTO1 INITPFKY,DMCB,MPFTABLE           MAINT PF TABLE                  
         B     SETUPX                                                           
                                                                                
SETUP40  CLI   ACTEQU,ACTLIST      ACTION LIST?                                 
         BNE   SETUP50                                                          
                                                                                
         GOTO1 INITPFKY,DMCB,LPFTABLE                                           
         B     SETUPX                                                           
                                                                                
SETUP50  CLI   ACTEQU,20           ACTION USAGE                                 
         BNE   SETUPX                                                           
                                                                                
         GOTO1 INITPFKY,DMCB,UPFTABLE                                           
                                                                                
SETUPX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*   TURN OFF PREVIOUSLY VALIDATED BITS                                *         
***********************************************************************         
UNVAL    NTR1                                                                   
         LA    R2,CTMREVNH         FIRST HEADER AFTER KEY FIELDS                
         LA    R3,CTMLAST          LAST FIELD                                   
                                                                                
UNVAL10  CR    R2,R3                                                            
         BNL   UNVALX                                                           
                                                                                
         TM    1(R2),X'20'         PROTECTED FIELD?                             
         BO    UNVAL20                                                          
         NI    4(R2),X'FF'-X'20'   PREVIOUSLY VALIDATED?                        
         OI    6(R2),X'80'         TRANSMIT                                     
                                                                                
UNVAL20  ZIC   R1,0(R2)            FIELD LENGTH                                 
         AR    R2,R1                                                            
         B     UNVAL10                                                          
                                                                                
UNVALX   B     XIT                                                              
***********************************************************************         
*                       VALIDATE PRODUCT                              *         
***********************************************************************         
* CONVERT BINARY PRODUCT CODE TO NAME OF PRODUCT                                
PRDT     NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CKEY,R4                                                          
         MVI   CKEYTYPE,0          RECORD TYPE                                  
         MVC   CKEYAM,BAGYMD       MEDIA                                        
         MVC   CKEYCLT,CLT         CLIENT                                       
         DROP  R4                                                               
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'CKEY),KEYSAVE     SAME CLIENT?                             
         BE    PRDT10                                                           
         DC    H'0'                                                             
                                                                                
PRDT10   MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING CKEY,R6                                                          
         B     *+8                                                              
                                                                                
PRDT20   LA    R6,4(R6)                                                         
         CLI   CLIST+3,0                                                        
         BNE   *+6                                                              
         DC    H'0'                PRODUCT CODE DOESN'T EXIST                   
         CLC   CLIST+3,PRDB                                                     
         BNE   PRDT20                                                           
                                                                                
         MVC   PRD,CLIST           PRODUCT NAME                                 
         DROP  R6                                                               
                                                                                
PRDTX    MVC   AIO,AIO1            RESTORE I/O AREA ADDRESS                     
         B     XIT                                                              
                                                                                
         EJECT                                                                  
***********************************************************************         
*                       CALCULATE USAGE                               *         
*        AIO SHOULD HAVE THE ADDRESS OF THE RECORD                    *         
*        BINSTA MAY HAVE A SPECIFIC STATION THAT USAGE IS ONLY NEEDED *         
*        USAGE WILL CONTAIN THE USAGE FOR STATIONS REQUESTED          *         
***********************************************************************         
* CALCULATE USAGE FROM USAGE ELEMENTS                                           
CUSAGE   NTR1                                                                   
                                                                                
         SR    R3,R3               INITIALIZE USAGE                             
         L     R6,AIO                                                           
         USING CTAUSELD,R6         USAGE ELEMENT                                
         MVI   ELCODE,CTAUSELQ     ELEMENT CODE                                 
                                                                                
         BAS   RE,GETEL                                                         
         BNE   CUSAGEX             NO USAGE ELEMENTS                            
                                                                                
CUSAGE10 OC    BINSTA,BINSTA       SPECIFIC STATION ONLY?                       
         BZ    *+14                                                             
         CLC   CTAUSSTA,BINSTA     IS IT THAT SAME STATION?                     
         BNE   CUSAGE20                                                         
                                                                                
         ICM   R1,15,CTAUSOGR      GROSS ORDERED DOLLARS                        
         AR    R3,R1                                                            
                                                                                
CUSAGE20 BAS   RE,NEXTEL                                                        
         BE    CUSAGE10                                                         
                                                                                
CUSAGEX  ST    R3,USAGE                                                         
         MVI   ELCODE,CTPARELQ     RESTORE PARTICIPANT ELEMENT CODE             
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       CHECK TRANFER W/ GLOBBER                      *         
***********************************************************************         
CHKGLB   NTR1                                                                   
         CLI   XFRCALL,C'Y'        CALLED WITH GLOBBER                          
         BNE   NO                                                               
         TM    PRGSTAT,GLOBELEM    ALREADY GOT ELEMS                            
         BO    CHKGLBX                                                          
         OI    PRGSTAT,GLOBELEM    SET READ ELEMS                               
                                                                                
         L     R1,ASFMPARM                                                      
         USING SFMPARMD,R1                                                      
         MVC   GLFRSYS,SFMXFRSY                                                 
         MVC   GLFRPRG,SFMXFRPR                                                 
         DROP  R1                                                               
                                                                                
         GOTO1 VGLOBBER,DMCB,=C'GETF',CTMMEDH,1,GLVSPMD                         
         CLI   8(R1),0                                                          
         BNE   CHKGLBX                                                          
                                                                                
         CLI   ACTEQU,ACTLIST                                                   
         BE    CHKGLB10                                                         
         GOTO1 VGLOBBER,DMCB,=C'GETF',CTMCNUMH,5,GLVSPCON                       
         CLI   8(R1),0                                                          
         BNE   CHKGLBX                                                          
                                                                                
CHKGLB10 CLC   GLFRSYS,=C'ACC'     FROM THE ACC SYSTEM                          
         BE    NO                                                               
                                                                                
CHKGLBX  CLI   GLFRSYS,C'S'        FROM THE SPOT SYSTEM                         
         BNE   NO                                                               
         CLI   PFKEY,12                                                         
         BNE   NO                                                               
         MVC   CONSERV,SPACES                                                   
         MVC   CONSERV(3),=C'=SW'                                               
         MVI   CONSERVH+5,3                                                     
         OI    CONSERVH+6,X'80'                                                 
         OI    CONSERVH+1,X'01'    INPUT THIS TIME                              
         B     YES                                                              
                                                                                
         EJECT                                                                  
***********************  ERROR MESSAGES  ******************************         
MISSINGN MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         B     GNERREX                                                          
                                                                                
INVALDGN MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     GNERREX                                                          
                                                                                
NOTNUMGN MVI   ERROR,NOTNUM        NOT VALID NUMERIC DATA                       
         B     GNERREX                                                          
                                                                                
INVALDCR MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     GNCURSER                                                         
                                                                                
PLSENTR  OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVI   GTMSGNO1,2          ERROR MESSAGE NUMBER                         
         MVC   GTMTYP,GTMINF       ERROR TYPE                                   
         MVI   GTMSYS,X'FF'        GENERAL                                      
         DROP  RF                                                               
         B     GNERREX                                                          
SPERREX  OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM      ERROR MESSAGE NUMBER                         
         MVC   GTMTYP,GTMERR       ERROR TYPE                                   
         MVI   GTMSYS,2            SPOT SYSTEM                                  
         DROP  RF                                                               
GNERREX  GOTO1 ERREX                                                            
SPCURSER OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM      ERROR MESSAGE NUMBER                         
         MVC   GTMTYP,GTMERR       ERROR TYPE                                   
         MVI   GTMSYS,2            SPOT SYSTEM                                  
         DROP  RF                                                               
GNCURSER GOTO1 CURSERR                                                          
ABEND    MVC   CONHEAD(15),=C'DUPLICATE ENTRY'                                  
         B     *+10                                                             
ABEND2   MVC   CONHEAD(29),=C'CANNOT DELETE STATION IN USE'                     
         OI    CONHEADH+6,X'80'    TRANSMIT                                     
         OI    CTMPARFH+6,X'40'+X'80'  POSITION CURSOR & TRANSMIT               
         DC    H'0',C'$ABEND'                                                   
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
                                                                                
RELO     DS    F                   RELOCATION FACTOR                            
                                                                                
         SPACE 3                                                                
         LTORG                                                                  
MNTPFLN  DC    CL30'PF4=CTAGS  PF5=USAGE  PF6=LIST'                             
MNTPFRTN DC    CL11'PF12=RETURN'                                                
                                                                                
         GETEL R6,DATADISP,ELCODE                                               
         GETEL2 R3,DATADISP,ELCODE                                              
                                                                                
***********************************************************************         
*                  MAINT SCREEN PF TABLE                              *         
***********************************************************************         
MPFTABLE DS    0C                                                               
* PF04 = CTAGS CHANGE                                                           
* PF05 = USAGE DISPLAY                                                          
* PF06 = LIST                                                                   
* PF12 = RETURN CALLER                                                          
                                                                                
*        USAGE DISPLAY                                                          
         DC    AL1(MPF05X-*,05,PFTCPROG,(MPF05X-MPF05)/KEYLNQ,0)                
         DC    CL3'U  '            USAGE                                        
         DC    CL8'CTA'            RECORD: CTA                                  
         DC    CL8'USAGE'          ACTION: USAGE                                
MPF05    DC    AL1(KEYTYTWA,L'CTMMED-1),AL2(CTMMED-T217FFD)                     
         DC    AL1(KEYTYTWA,L'CTMCNUM-1),AL2(CTMCNUM-T217FFD)                   
MPF05X   EQU   *                                                                
                                                                                
*        LIST                                                                   
         DC    AL1(MPF06X-*,06,PFTCPROG,(MPF06X-MPF06)/KEYLNQ,0)                
         DC    CL3' ',CL8' ',CL8'LIST'                                          
MPF06    DC    AL1(KEYTYTWA,L'CTMMED-1),AL2(CTMMED-T217FFD)                     
MPF06A   DC    AL1(KEYTYTWA,L'STATN-1),AL2(CTMPARF-T217FFD)                     
         DC    AL1(KEYTYTWA,L'CTMCNUM-1),AL2(CTMCNUM-T217FFD)                   
MPF06X   EQU   *                                                                
                                                                                
*        RETURN CALLER                                                          
         DC    AL1(MPF12X-*,12,PFTRPROG,0,0)                                    
         DC    CL3' ',CL8' ',CL8' '                                             
MPF12X   EQU   *                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*                   LIST SCREEN PF TABLE                              *         
***********************************************************************         
LPFTABLE DS    0C                                                               
* PF03 = CTA MAINT                                                              
* PF04 = CTAGS CHANGE                                                           
* PF05 = USAGE DISPLAY                                                          
                                                                                
*        MAINT DISPLAY                                                          
         DC    AL1(LPF03X-*,03,PFTCPROG,(LPF03X-LPF03)/KEYLNQ,0)                
         DC    CL3'S  '            SELECT                                       
         DC    CL8'CTA'            RECORD                                       
         DC    CL8'MAINT'          ACTION                                       
LPF03    DC    AL1(KEYTYTWA,L'CTLMED-1),AL2(CTLMED-T217FFD)                     
         DC    AL1(KEYTYCUR,L'LCNUM-1),AL2(LCNUM-LMKT)                          
LPF03X   EQU   *                                                                
                                                                                
*        USAGE DISPLAY                                                          
         DC    AL1(LPF05X-*,05,PFTCPROG,(LPF05X-LPF05)/KEYLNQ,0)                
         DC    CL3'U  '            USAGE                                        
         DC    CL8'CTA'            RECORD                                       
         DC    CL8'USAGE'          ACTION                                       
LPF05    DC    AL1(KEYTYTWA,L'CTLMED-1),AL2(CTLMED-T217FFD)                     
         DC    AL1(KEYTYCUR,L'LCNUM-1),AL2(LCNUM-LMKT)                          
LPF05X   EQU   *                                                                
                                                                                
*        RETURN CALLER                                                          
         DC    AL1(LPF12X-*,12,PFTRPROG,0,0)                                    
         DC    CL3' ',CL8' ',CL8' '                                             
LPF12X   EQU   *                                                                
         DC    X'FF'                                                            
                                                                                
         EJECT                                                                  
***********************************************************************         
*                  USAGE SCREEN PF TABLE                              *         
***********************************************************************         
UPFTABLE DS    0C                                                               
* PF12 = RETURN CALLER                                                          
                                                                                
*        RETURN CALLER                                                          
         DC    AL1(UPF12X-*,12,PFTRPROG,0,0)                                    
         DC    CL3' ',CL8' ',CL8' '                                             
UPF12X   EQU   *                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*                           CLEAR SCREEN                              *         
***********************************************************************         
CS       DS    0H                                                               
         NMOD1 0,***CS***                                                       
         L     RC,0(R1)                                                         
                                                                                
         L     R9,ASYSD                                                         
         L     RA,ATWA                                                          
                                                                                
         XC    DISPDCTR,DISPDCTR                                                
         XC    DISPDGCI,DISPDGCI                                                
         XC    DISPDCOM,DISPDCOM                                                
         LA    R2,CTMREVNH         FIRST HEADER AFTER KEY FIELDS                
         LA    R3,CTMLAST          LAST FIELD                                   
                                                                                
CS10     CR    R2,R3                                                            
         BNL   CS30                                                             
                                                                                
         ZIC   R4,0(R2)            FIELD LENGTH                                 
         TM    1(R2),X'20'         PROTECTED FIELD?                             
         BO    CS20                                                             
                                                                                
         LR    R1,R4               FIELD LENGTH                                 
         SH    R1,=H'8'            SUBTRACT LENGTH OF HEADER                    
         TM    1(R2),X'02'         EXTENSION PRESENT?                           
         BNO   *+8                                                              
         SH    R1,=H'8'            SUBTRACT LENGTH OF EXTENSION                 
                                                                                
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)       CLEAR FIELD                                  
         OI    6(R2),X'80'         TRANSMIT                                     
                                                                                
CS20     AR    R2,R4                                                            
         B     CS10                                                             
                                                                                
CS30     DS    0H                  CLEAR PROTECTED FIELDS                       
         XC    CTMREVN,CTMREVN     REVISION NO.                                 
         OI    CTMREVNH+6,X'80'    TRANSMIT                                     
         XC    CTMCONT,CTMCONT     CONTRACTOR NAME                              
         OI    CTMCONTH+6,X'80'    TRANSMIT                                     
         XC    CTMNCI,CTMNCI       NCI                                          
         OI    CTMNCIH+6,X'80'     TRANSMIT                                     
         XC    CTMCADD,CTMCADD     DATE CONTRACT ADDED                          
         OI    CTMCADDH+6,X'80'    TRANSMIT                                     
         XC    CTMCCHG,CTMCCHG     DATE LAST CHANGED                            
         OI    CTMCCHGH+6,X'80'    TRANSMIT                                     
                                                                                
CSX      B     XIT                                                              
                                                                                
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE CONTRACTOR                           *         
***********************************************************************         
* GET CONTRACTOR NAME FROM CONTRACTOR RECORDS                                   
CNTR     DS    0H                                                               
         NMOD1 0,**CNTR**                                                       
         L     RC,0(R1)                                                         
                                                                                
         L     R9,ASYSD                                                         
         L     RA,ATWA                                                          
                                                                                
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CXRKEY,R4                                                        
         MVI   CXRKTYP,CXRKTYPQ    RECORD TYPE                                  
         MVI   CXRKSUB,CXRKSUBQ    SUB TYPE                                     
         MVC   CXRKAGMD,BAGYMD     MEDIA                                        
         MVC   CXRKCTA,CONTR       CONTRACTOR                                   
         DROP  R4                                                               
                                                                                
         LA    R2,CTMCNTRH                                                      
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
         CLC   KEY(L'CXRKEY),KEYSAVE     SAME CONTRACTOR?                       
         BE    CNTR10                                                           
                                                                                
         TM    RECFLAG1,DSPLYREC   DISPLAYING  EXISTING RECORD?                 
         BNO   *+6                                                              
         DC    H'0'                                                             
         B     INVALDGN                                                         
                                                                                
CNTR10   MVC   CNTRKEY,KEY                                                      
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING CXRKEY,R6                                                        
         MVC   CTMCONT,CXRCTCON    CONTRACTOR NAME                              
         OI    CTMCONTH+6,X'80'    TRANSMIT                                     
         DROP  R6                                                               
                                                                                
CNTRX    MVC   AIO,AIO1            RESTORE PRIMARY I/O ADDRESS                  
         B     XIT                                                              
                                                                                
         EJECT                                                                  
***********************************************************************         
*                       ADD PASSIVE POINTER                                     
***********************************************************************         
ADDPNEW  DS    0H                                                               
         NMOD1 0,*ADDPNEW                                                       
         L     RC,0(R1)                                                         
                                                                                
         L     R9,ASYSD                                                         
         L     RA,ATWA                                                          
                                                                                
NEW      USING CTPARD,R6                                                        
OLD      USING CTPARD,R3                                                        
                                                                                
         MVI   RDUPDATE,C'Y'                  READ FOR UPDATE                   
         OI    DMINBTS,X'08'                  READ FOR DELETES                  
                                                                                
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTAPASS,R4                                                       
         MVI   CTAPTYP,CTAPTYPQ               PASSIVE KEY                       
         MVI   CTAPSUB,CTAPSUBQ                                                 
         MVC   CTAPAGMD,BAGYMD                AGENCY/MEDIA                      
         MVC   CTAPMKT(5),NEW.CTPARMKT        BINARY MARKET AND STATION         
         MVC   CTAPCNUM,CNUMPWOS              CONTRACT #                        
                                                                                
         GOTO1 HIGH                           CHECK IF EXISTS                   
         NI    DMINBTS,X'FF'-X'08'            DON'T READ FOR DELETES            
         CLC   KEY(L'CTAPASS),KEYSAVE                                           
         BE    ADDP10                                                           
                                                                                
         MVC   KEY(L'CTAPASS),KEYSAVE                                           
         MVI   KEY+L'CTAPASS,0                STATUS BIT = 0                    
         MVC   KEY+L'CTAPASS+1(4),DMDSKADD    PUT DISK ADDRESS IN KEY           
         GOTO1 ADD                            ADD PASSIVE KEY                   
         B     XIT                                                              
                                                                                
ADDP10   NI    KEY+L'CTAPASS,X'FF'-X'80'                                        
         GOTO1 WRITE                          UNDELETE PASSIVE KEY              
         B     XIT                                                              
                                                                                
         EJECT                                                                  
***********************************************************************         
*                       DELETE PASSIVE POINTER                                  
***********************************************************************         
DELPOLD  DS    0H                                                               
         NMOD1 0,*DELPOLD                                                       
         L     RC,0(R1)                                                         
                                                                                
         L     R9,ASYSD                                                         
         L     RA,ATWA                                                          
                                                                                
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTAPASS,R4                                                       
         MVI   CTAPTYP,CTAPTYPQ               PASSIVE KEY                       
         MVI   CTAPSUB,CTAPSUBQ                                                 
         MVC   CTAPMKT(5),OLD.CTPARMKT   MARKET AND STATION                     
         MVC   CTAPAGMD,BAGYMD           AGENCY/MEDIA                           
         MVC   CTAPCNUM,CNUMPWOS         CONTRACT #                             
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'CTAPASS),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         OI    KEY+L'CTAPASS,X'80' MARK PASS KEY DELETED                        
         GOTO1 WRITE                                                            
                                                                                
         DROP  R4,NEW,OLD                                                       
         B     XIT                                                              
                                                                                
         EJECT                                                                  
                                                                                
*---------------------------- OTHER DSECTS ---------------------------*         
                                                                                
         PRINT OFF                                                              
       ++INCLUDE SPSFMWORKD                                                     
       ++INCLUDE SPSFMFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFM80D          LIST SCREEN                                  
         SPACE 2                                                                
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFM86D          MAINTENANCE SCREEN                           
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMD9D          USAGE SCREEN                                 
         EJECT                                                                  
                                                                                
* DDGENTWA                                                                      
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FATIOB                                                                        
* FAGETTXTD                                                                     
* DDCOMFACS                                                                     
* DDGLOBEQUS                                                                    
* DDPERVALD                                                                     
* DDCOREQUS                                                                     
* SPDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE SPDDEQUS                                                       
       ++INCLUDE SPMSGEQUS                                                      
       ++INCLUDE DDGLVXCTLD                                                     
         PRINT ON                                                               
       ++INCLUDE SPGENCTA          CONTRACT TRANSACTION RECORD DSECT            
       ++INCLUDE SPGENCTR          CONTRACTOR RECORD DSCECT                     
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT          CLIENT RECORDS                               
       ++INCLUDE DDACTIVD          ACTIVITY ELEMENT DSECT                       
*---------------------------- SPGEN DSECTS ---------------------------*         
                                                                                
                                                                                
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
CNTRKEY  DS    XL18                CONTRACTOR KEY                               
SAVEKEY  DS    XL18                                                             
FIRSTKEY DS    XL13                FIRST KEY ON LIST SCREEN                     
LASTKEY  DS    XL13                LAST KEY ON LIST SCREEN                      
VGLOBBER DS    V                   ADDRESS OF GLOBBER                           
WORK1    DS    F                   WORK AREA 1                                  
WORK2    DS    F                   WORK AREA 2                                  
ERRNUM   DS    XL2                 ERROR MESSAGE NUMBER                         
                                                                                
USGFLAG1 DS    X                                                                
FRSPAREL EQU   X'80'               FIRST PARTICIPANT ELEMENT                    
BFUELFND EQU   X'40'               BF USAGE ELEMENT FOUND                       
CONTUSGE EQU   X'20'               CONTINUE DISPLAYING USAGE                    
NOMRUSGE EQU   X'10'               NO MORE USAGE ELEMENTS                       
                                                                                
RECFLAG1 DS    X                                                                
KEYCHNGD EQU   X'80'               THE KEY CHANGED                              
NEWCNTRT EQU   X'40'               NEW CONTRACT BEING ADDED                     
DSPLYREC EQU   X'20'               DISPLAY RECORD MODE                          
PRCNTAMT EQU   X'10'               PARTICIPANTS HAVE % AMOUNTS                  
DOLLRAMT EQU   X'08'               PARTICIPANTS HAVE $ AMOUNTS                  
NOAMOUNT EQU   X'04'               PARTICIPANTS HAVE NO AMOUNT                  
SAMEPART EQU   X'02'               PARTICIPANTS DIDN'T CHANGE                   
                                                                                
RECFLAG2 DS X                                                                   
INACTIVE EQU   X'80'               INACTIVE PARTICIPANT                         
DELP     EQU   X'40'               DELETE PASSIVE POINTERS                      
ADDP     EQU   X'20'               ADD PASSIVE POINTERS                         
NOCLIENT EQU   X'10'               NO CLIENT ENTERED                            
NOSTRTDT EQU   X'08'               NO CONTRACT START DATE ENTERED               
DELETERR EQU   X'04'               PARTICIPANTS DELETE WITH USAGE ERROR         
STANOFND EQU   X'02'               DIDN'T FIND DELETED STATION                  
GCICHNGD EQU   X'01'               GCI WAS CHANGED                              
                                                                                
RECFLAG3 DS    X                                                                
ACCRECRD EQU   X'80'               ACC RECORD HAS BEEN ADDED                    
CTRNTBAL EQU   X'40'               CONTRACT OUT OF BALANCE                      
RECCHNGD EQU   X'20'               CONTRACT RECORD WAS CHANGED                  
CLTCHNGD EQU   X'10'               CLIENT CHANGED                               
COMCHGER EQU   X'08'               COMMISSION CHANGE ERROR                      
DUPSTATN EQU   X'04'               DUPLICATE STATION                            
                                                                                
PRGSTAT  DS    X                                                                
GLOBELEM EQU   X'80'               ALREADY GOT ELEMS                            
                                                                                
GLFRSYS  DS    CL3                 GLOBBER CALLED FROM SYSTEM                   
GLFRPRG  DS    CL3                 GLOBBER CALLED FROM PROGRAM                  
                                                                                
STALEN   DS    X                   STATION LENGTH                               
MYLSTACT DS    X                   LAST ACTION                                  
PRD      DS    CL3                 PRODUCT NAME                                 
PRDB     DS    X                   PRODUCT CODE                                 
MKTSTAB  DS    0XL5                BINARY MARKET AND STATION                    
MKTB     DS    XL2                                                              
STAB     DS    XL3                                                              
BINSTA   DS    XL3                 STATION REQUESTING USAGE FOR                 
STADEL   DS    XL3                 STATION DELETED                              
MKT      DS    CL4                 MARKET                                       
STATN    DS    CL5                 STATION NAME                                 
CLT      DS    XL2                 CLIENT CODE                                  
CONTR    DS    CL6                 CONTRACTOR (CODE)                            
DISPDCTR DS    CL6                 DISPLAYED CONTRACTOR (CODE)                  
CNUM     DS    CL5                 CONTRACT NUMBER                              
CNUMPWOS DS    XL3                 CONTRACT # 9'S COMPL PACKED W/O SIGN         
CNUMCOMP DS    XL3                 CONTRACT # UNCOMPLI. PACKED W/O SIGN         
CNUMP    DS    XL4                 PACKED                                       
EDAMOUNT DS    CL10                EDITED AMOUNT W/ DECIMALS                    
DATE     DS    CL6                 DATE                                         
YR       DS    X                   LAST DIGIT OF YEAR                           
LENGTH   DS    X                                                                
FIELD    DS    X                   COUNTER FOR # IN FIELD                       
FAKEHDR  DS    XL8                 FAKE FIELD HEADER                            
FAKEFLD  DS    XL80                FAKE FIELD                                   
NEXTPART DS    F                   NEXT PARTICIPANT IN TABLE                    
SAVEADR  DS    F                   SAVE AN ADDRESS                              
SAVEDSP1 DS    F                   SAVE DISPLACEMENT                            
SAVEDSP2 DS    F                   SAVE DISPLACEMENT                            
LASTPART DS    F                   ADDRESS OF LAST PARTICIPANT                  
SUM      DS    F                   SUM OF PARTICIPANT AMOUNTS                   
USAGE    DS    F                   USAGE                                        
DISPDCOM DS    F                   DISPLAYED COMMISSION                         
GCI      DS    F                   GCI                                          
TALLOCAT DS    F                   TOTAL ALLOCATED FOR A STATION                
TORDERED DS    F                   TOTAL ORDER FOR A STATION                    
TPAID    DS    F                   TOTAL PAID FOR A STATION                     
DISPDGCI DS    F                   DISPLAYED GCI                                
BALANCE  DS    F                   BALANCE FOR USAGE SCREEN                     
PERIOD   DS    XL17                PERIOD OF 2 DATES                            
TABLE    DS    24XL(CTPARLNQ)      TABLE FOR PARTICIPANT ELEMENTS               
MYBLOCK  DS    XL300                                                            
*                                                                               
MYSSPREL EQU   *-SYSSPARE                                                       
         DS    0CL(1024-MYSSPREL)  CHECK AGAINST AVAIL SYSSPARE AMT             
USAGED   DSECT                                                                  
UFLDHDR  DS    CL8                 FIELD HEADER                                 
USTATN   DS    CL5                 STATION                                      
         DS    CL5                                                              
UCLT     DS    CL3                 CLIENT                                       
         DS    CL3                                                              
UPRD     DS    CL3                 PRODUCT                                      
         DS    CL3                                                              
UEST     DS    CL3                 ESTIMATE                                     
         DS    CL6                                                              
UALLOCAT DS    CL7                 ALLOCATED TO STATION                         
         DS    CL4                                                              
UORDERED DS    CL7                 GROSS DOLLARS ORDERED                        
         DS    CL4                                                              
UBALANCE DS    CL8                 BALANCE FOR THE STATION                      
         DS    CL3                                                              
UPAID    DS    CL7                 GROSS DOLLARS PAID                           
USAGELNQ EQU   *-USTATN            LENGTH OF ONE LINE ON USAGE SCREEN           
                                                                                
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LMKT     DS    CL4                 MARKET #                                     
         DS    CL2                                                              
LMKTNAME DS    CL24                MARKET NAME                                  
         DS    CL1                                                              
LSTA     DS    CL5                 STATION                                      
         DS    CL4                                                              
LCNTR    DS    CL6                 CONTRACTOR (CODE)                            
         DS    CL2                                                              
LCNUM    DS    CL5                 CONTRACT NO.                                 
         DS    CL2                                                              
LGCI     DS    CL8                 GCI                                          
         DS    CL2                                                              
LBALANCE DS    CL9                 BALANCE                                      
LISTLNQ  EQU   *-LMKT              LENGTH OF ONE LINE ON LIST SCREEN            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005SPSMF19S  05/30/96'                                      
         END                                                                    
