*          DATA SET PRSFM1F    AT LEVEL 039 AS OF 02/17/10                      
*PHASE T41C1FA                                                                  
*INCLUDE PPBROWSE                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*               T41C1F - PRD/EST BILL MAINT                                     
*                                                                               
* BPLA 02/17/10 FIX STORING OF 'E' FORMULAS                                     
*                                                                               
* KWAN 09/13/06 FIX COMMISSION FORMULA VALIDATION (TYPO)                        
*                                                                               
* KWAN 06/06/05 BROWSE FUNCTION                                                 
*                                                                               
* KWAN 02/04/05 NEED TO GENERATE AN AUTO P41 T/A REPORT                         
*                                                                               
* KWAN 11/09/04 CORRECT ESTIMATE NAME DISPLAY                                   
*                                                                               
* KWAN 01/10/03 CONVERT PRD/EST BILL FROM FIL TO SFM                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*  CALLED FROM  GENCON VIA T41C00 (SFM CONTROLLER)                    *         
*                                                                     *         
*  COMMENTS     SUPPORTS CHG, DISP                                    *         
*                                                                     *         
*  INPUTS       SCREEN T41CA2 (PRD/EST BILL MAINT SCR)                *         
*                                                                     *         
*  OUTPUTS                                                            *         
*                                                                     *         
*  REGISTERS    R0 -- WORK                                            *         
*               R1 -- WORK                                            *         
*               R2 -- WORK                                            *         
*               R3 -- WORK                                            *         
*               R4 -- WORK                                            *         
*               R5 -- WORK                                            *         
*               R6 -- WORK                                            *         
*               R7 -- WORK                                            *         
*               R8 -- SPOOLD                                          *         
*               R9 -- SYSD                                            *         
*               RA -- TWA                                             *         
*               RB -- FIRST BASE                                      *         
*               RC -- GEND                                            *         
*               RD -- SYSTEM                                          *         
*               RE -- SYSTEM                                          *         
*               RF -- SYSTEM                                          *         
*                                                                     *         
*  I/O AREAS                                                          *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'T41C1F - PRD/EST BILL MAINT'                                    
*                                                                               
T41C1F   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41C1F,RR=R3                                                   
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         ST    R3,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         BRAS  RE,INITIALZ         INITIALIZE WORKING STORAGES                  
         CLI   MODE,NEWSCR         SCR JUST BEING LOADED?                       
         JE    EXIT                YES, DISPLAY INIT'D SCR                      
*                                                                               
* FOLLOWING ACTIONS ARE INVALID (SHOULD BE CHECKED IN PRSFM00)                  
*                                                                               
         CLI   ACTNUM,ACTDEL       ACTION DELETE?                               
         JE    RECACERR                                                         
         CLI   ACTNUM,ACTREST      ACTION RESTORE?                              
         JE    RECACERR                                                         
         CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         JE    RECACERR                                                         
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         JE    RECACERR                                                         
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         JE    RECACERR                                                         
*                                                                               
         CLI   PFAID,0             PFKEY IS PRESSED?                            
         BE    *+12                NO                                           
         BRAS  RE,CKPFKEYS                                                      
         JNE   PFKEYERR            INVALID PFKEY IS PRESSED                     
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,XRECPUT        AFTER PUT                                    
         BE    PPTRS                                                            
*                                                                               
SETCCEQ  CR    RB,RB               EQUAL                                        
         J     *+6                                                              
SETCCNEQ LTR   RB,RB               NOT EQUAL                                    
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VK       DS    0H                  VALIDATE KEY ROUTINE                         
         LA    R2,BSCMEDH          POINT TO MEDIA FLD                           
         GOTO1 VALIMED                                                          
         MVC   BSCMEDN,MEDNM                                                    
         OI    BSCMEDNH+6,X'80'                                                 
*                                                                               
         XC    SVKEY,SVKEY                                                      
         LA    R6,SVKEY                                                         
         USING PRESBILK,R6                                                      
         MVC   PRES_AGY,AGENCY                                                  
         MVC   PRES_MED,QMED                                                    
         CLI   RECNUM,56           PRD BILL?                                    
         BNE   *+8                                                              
         MVI   PRES_RTY,X'06'      REC TYPE IS PRD                              
         CLI   RECNUM,58           EST BILL?                                    
         BNE   *+8                                                              
         MVI   PRES_RTY,X'07'      REC TYPE IS EST                              
         CLI   PRES_RTY,0          REC TYPE IS DEFINED?                         
         BNE   *+6                                                              
         DC    H'0'                SOMETHING WRONG WITH REC TYPE                
*                                                                               
         LA    R2,BSCCLTH          POINT TO CLT FLD                             
         CLI   5(R2),0                                                          
         JE    MSSNGERR                                                         
*                                                                               
         CLI   8(R2),C'='          BROWSE?                                      
         BNE   VK24                                                             
                                                                                
         GOTO1 =V(PPBROWSE),DMCB,ACOMFACS,SYSRD,(R2),                  +        
               0,(QMED,C' CLT'),0,RR=RELO                                       
         DC    H'0'                                                             
*                                                                               
VK24     CLI   5(R2),3             MAX 3 CHARS                                  
         JH    INVFDERR                                                         
         CLC   =C'ALL',8(R2)       ALL CLT?                                     
         JE    INVFDERR            ALL IS NOT ALLOWED AS CLT CODE               
*                                                                               
         GOTO1 VALICLT                                                          
*                                                                               
         MVC   PRES_CLT,QCLT                                                    
         MVC   BSCCLTN,CLTNM                                                    
         OI    BSCCLTNH+6,X'80'                                                 
*                                                                               
         LA    R2,BSCPRDH          POINT TO PRD FLD                             
         CLI   5(R2),0                                                          
         JE    MSSNGERR                                                         
*                                                                               
         CLI   8(R2),C'='          BROWSE?                                      
         BNE   VK64                                                             
*                                                                               
         GOTO1 =V(PPBROWSE),DMCB,ACOMFACS,SYSRD,(R2),                  +        
               (0,QCLT),(QMED,C' PRD'),0,RR=RELO                                
         DC    H'0'                                                             
*                                                                               
VK64     CLI   5(R2),3             MAX 3 CHARS                                  
         JH    INVFDERR                                                         
         CLC   =C'ALL',8(R2)       ALL PRD?                                     
         JE    INVFDERR            ALL IS NOT ALLOWED AS PRD CODE               
*                                                                               
         GOTO1 VALIPRD                                                          
*                                                                               
         MVC   PRES_PRD,QPRD                                                    
         MVC   BSCPRDN,PRDNM                                                    
         OI    BSCPRDNH+6,X'80'                                                 
*                                                                               
         CLI   RECNUM,58           EST BILLING PROFILE?                         
         BNE   VKX                                                              
         LA    R2,BSCESTH          POINT TO EST FLD                             
         CLI   5(R2),0                                                          
         JE    MSSNGERR                                                         
*                                                                               
         CLI   8(R2),C'='          BROWSE?                                      
         BNE   VK74                                                             
*                                                                               
         GOTO1 =V(PPBROWSE),DMCB,ACOMFACS,SYSRD,(R2),                  +        
               (0,QCLT),(QMED,C' EST'),0,RR=RELO                                
         DC    H'0'                                                             
*                                                                               
VK74     TM    4(R2),X'08'         VALID NUMBERIC?                              
         JZ    NTNUMERR                                                         
         CLI   5(R2),3             MAX 3 NUMBERS                                
         JH    INVFDERR                                                         
*                                                                               
         GOTO1 VALIEST                                                          
*                                                                               
         MVC   PRES_EST,BEST                                                    
         MVC   BSCESTN,ESTNM                                                    
         OI    BSCESTNH+6,X'80'                                                 
*                                                                               
VKX      MVC   KEY,SVKEY                                                        
         MVC   AIO,AIO1            RECORD WILL BE READ INTO AIO1                
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VR       DS    0H                  VALIDATE RECORD                              
         LA    R0,WKSVSTRT                                                      
         LHI   R1,WKSVLNQ          LENGTH OF WORK STORAGE TO BE SAVED           
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               SAME AS XCEF, ONLY SHORTER                   
*                                                                               
         L     R6,AIO                                                           
         CLI   3(R6),X'06'         PRD RECORD CODE?                             
         BNE   *+12                                                             
         MVI   ELCODE,X'06'        FIRST PRD ELEM CODE                          
         B     VR10                                                             
         CLI   3(R6),X'07'         EST RECORD CODE?                             
         BNE   *+12                                                             
         MVI   ELCODE,X'07'        FIRST EST ELEM CODE                          
         B     VR10                                                             
         DC    H'0'                INVALID RECORD CODE!                         
*                                                                               
VR10     BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                FIRST ELEM MUST BE THERE!                    
*                                                                               
         LR    RE,R6                                                            
         USING PPRDELEM,RE                                                      
         CLI   0(RE),X'06'         PRD ELEM?                                    
         BNE   VR16                                                             
         LA    R6,PPRDBILP         POINT TO PRD BILLING PROFILE                 
         B     VR20                                                             
         USING PESTELEM,RE                                                      
VR16     CLI   0(RE),X'07'         EST ELEM?                                    
         BNE   VR18                                                             
         MVC   WKESRTYP,PESTRTYP   SAVE EST RATE TYPE                           
         LA    R6,PESTBILP         POINT TO EST BILLING PROFILE                 
         B     VR20                                                             
         DROP  RE                                                               
*                                                                               
VR18     DC    H'0'                SOMETHING WRONG WITH ELEM IN REC             
*                                                                               
         USING BILPROF,R6          PRD/EST BILLING PROFILE DSECT                
VR20     MVC   WKCOLPRB,BILDETS    SAVE "OLD" COLUNM TO PRINT ON BILL           
         XC    BILPROF,BILPROF     INIT BILL PROF                               
*                                                                               
         LA    R2,BSCBASAH         POINT TO FORMULA INPUT                       
         CLI   BSCBASAH+5,0        ANY BASE FORMULA INPUT?                      
         BNE   VR24H                                                            
         CLI   BSCBASBH+5,0        ANY BASE B ("OF") INPUT?                     
         BNE   VR24H                                                            
         CLI   BSCADJH+5,0         ANY PECENTAGE ADJUSTMENT INPUT?              
         BE    VR34                NO, NO NEED TO EDIT FORMULA                  
         CLI   BSCADATH+5,0        ANY EFFECTIVE DATE INPUT?                    
         BE    VR34                NO, NO NEED TO EDIT FORMULA                  
*                                                                               
VR24H    CLI   SVCPROF+10,C'0'     NO ADD/CHG TO PRD/EST BILL FORMULA?          
         JE    NACBFERR                                                         
         LA    R3,BILBASA          VALIDATED VALUE WILL BE RETURNED             
         BRAS  RE,VR_EBASE                                                      
         CLI   BILBASA,X'FF'       INVALID BASE A VALUE RETURNED?               
         JE    INVFDERR                                                         
         TM    BILBASA,X'10'       COMMISSION ONLY?                             
         BNO   *+12                                                             
         NI    BILBASA,X'FF'-X'10' SET OFF X'10' BIT                            
         MVI   BILCMSW,C'C'                                                     
         TM    BILBASA,X'20'       NOT FOR BILLING?                             
         BNO   *+12                                                             
         NI    BILBASA,X'FF'-X'20' SET OFF X'20' BIT                            
         MVI   BILNBSW,C'Y'                                                     
*                                                                               
         LA    R2,BSCBASBH         POINT TO BASE B                              
         LA    R3,BILBASB          VALIDATED VALUE WILL BE RETURNED             
         BRAS  RE,VR_EBASE                                                      
         CLI   BILBASB,X'FF'       INVALID BASE B VALUE RETURNED?               
         JE    INVFDERR                                                         
         TM    BILBASB,X'10'       COMMISSION ONY                               
         JO    INVFDERR            INVALID ON BILBASB                           
         TM    BILBASB,X'20'       NOT FOR BILLING                              
         JO    INVFDERR            INVALID ON BILBASB                           
*                                                                               
         LA    R2,BSCADJH          PERCENTAGE ADJUSTMENT                        
         CLI   5(R2),0                                                          
         JE    MSSNGERR            FLD IS REQUIRED                              
         SR    R0,R0                                                            
         IC    R0,5(R2)                                                         
         BCTR  R0,R0                                                            
         GOTO1 CASHVAL,DMCB,(4,BSCADJ+1),(R0)                                   
         CLI   DMCB,X'FF'                                                       
         JE    ADJFDERR            ADJUSTMENT FLD IS INVALID                    
         CLI   DMCB+4,0                                                         
         JNE   ADJFDERR            TOO BIG FOR 3 BYTES                          
         L     R0,DMCB+4                                                        
         CLI   BSCADJ,C'+'                                                      
         BE    VR28H                                                            
         CLI   BSCADJ,C'-'                                                      
         JNE   ADJFDERR            FIRST CHAR IS NOT "+" OR "-"                 
         LCR   R0,R0                                                            
*                                                                               
VR28H    STCM  R0,7,BILADJ                                                      
*                                                                               
         CLI   RECNUM,58           EST BILL?                                    
         BNE   VR32                                                             
         CLI   WKESRTYP,C'C'       'C' RATE ESTIMATE?                           
         BNE   VR32                                                             
         CLI   BILCMSW,C'C'        COMMISSION ONLY?                             
         BE    VR32                                                             
         LA    R2,BSCBASAH                                                      
         J     INVFDERR            INDICATOR MUST BE COMMISSION ONLY            
*                                                                               
VR32     LA    R2,BSCADATH         ADJUSTMENT DATE                              
         CLI   5(R2),0                                                          
         BE    VR34                                                             
         GOTO1 DATVAL,DMCB,(2,BSCADAT),WORK                                     
         OC    DMCB(4),DMCB                                                     
         JZ    INVDTERR                                                         
         GOTO1 DATCON,DMCB,(0,WORK),(3,BILADAT)                                 
*                                                                               
* NOTE: BOTH OR NEITHER "DISPLAY AS" AND "PCT OF" FLDS MUST BE ENTERED          
*                                                                               
VR34     LA    R2,BSCPADJH         POINT TO "DISPLAY AS" ADJ                    
         CLI   5(R2),0                                                          
         BE    VR36                                                             
         OC    BILADJ(3),BILADJ    FORMULA CONTAINS ADJUSTMENT?                 
         JZ    ADJFDERR            ADJ MUST PRESENT FOR "DISPLAY AS"            
         CLC   BSCPADJ(1),BSCADJ   SIGN IS SAME AS PERCENTAGE ADJ FLD?          
         JNE   ADJFDERR                                                         
         TM    BILBASB,X'08'       REAL ADJUSTMENT IS AC?                       
         JZ    ADJFDERR                                                         
*                                                                               
         SR    R0,R0                                                            
         IC    R0,5(R2)                                                         
         BCTR  R0,R0                                                            
         GOTO1 CASHVAL,DMCB,(4,BSCPADJ+1),(R0)                                  
         CLI   DMCB,X'FF'                                                       
         JE    ADJFDERR            ADJUSTMENT IS INVALID                        
         CLI   DMCB+4,0                                                         
         JNE   ADJFDERR            TOO BIG FOR 3 BYTES                          
         L     R0,DMCB+4                                                        
         LTR   R0,R0                                                            
         JZ    ADJFDERR            CANNOT BE 0                                  
         CLI   BSCPADJ,C'+'                                                     
         BE    VR34H                                                            
         CLI   BSCPADJ,C'-'                                                     
         JNE   ADJFDERR            FIRST CHAR IS NOT "+" OR "-"                 
         LCR   R0,R0                                                            
*                                                                               
VR34H    STCM  R0,7,BILPADJ                                                     
*                                                                               
VR36     LA    R2,BSCPBASH         "PCT OF" BASE FLD                            
         CLI   5(R2),0                                                          
         BE    VR36P                                                            
         OC    BILADJ,BILADJ       FORMULA HAS ADJUSTMENT?                      
         JZ    INVFDERR                                                         
         LA    R3,BILPBASB         VALIDATED VALUE WILL BE RETURNED             
         BRAS  RE,VR_EBASE                                                      
         CLI   BILPBASB,X'06'      IS IT G,N,G-CD OR N-CD?                      
         JH    INVFDERR                                                         
         CLI   BSCPADJH+5,0        "DISPLAY AS" FLD ENTERED?                    
         BNE   VR38                                                             
         LA    R2,BSCPADJH         PLACE CURSOR ON "DISPLAY AS" FLD             
         J     MSSNGERR                                                         
*                                                                               
VR36P    CLI   BSCPADJH+5,0        "DISPLAY AS" FLD ENTERED?                    
         JNE   MSSNGERR                                                         
*                                                                               
VR38     LA    R2,BSCDETSH         COLUMNS TO PRINT ON BILL                     
         CLI   5(R2),0                                                          
         BE    VR40                                                             
         CLC   WKCOLPRB,8(R2)      INPUT IS SAME AS SAVED VALUE?                
         BNE   *+14                                                             
         MVC   BILDETS,WKCOLPRB    NO NEED TO VALIDATE "OLD" DATA               
         B     VR40                                                             
         LA    RE,VR_COLBP         POINT TO TABLE OF VALID VALES                
VR38H    CLI   0(RE),X'FF'         END OF TABLE?                                
         JE    INVFDERR                                                         
         CLC   0(1,RE),8(R2)       INPUT IS DEFINED IN TABLE?                   
         BE    VR38M                                                            
         LA    RE,1(RE)            NEXT ENTRY IN TABLE                          
         B     VR38H                                                            
VR38M    MVC   BILDETS,0(RE)                                                    
*                                                                               
VR40     LA    R2,BSCCOM1H         COMMENT CODES                                
         LA    R3,3                THREE SETS TO BE PROCESSED                   
         LA    R4,BILCMNTS                                                      
         MVC   SVWORK(L'KEY),KEY                                                
         XC    KEY+03(L'KEY-03),KEY+03                                          
VR40D    MVI   KEY+03,X'40'                                                     
         MVC   KEY+04(06),SPACES                                                
         CLI   5(R2),0             COMMENT CODE IS ENTERED?                     
         BE    VR42                                                             
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         LCR   RF,R1                                                            
         AHI   RF,6                                                             
         LA    RF,KEY+04(RF)                                                    
         BCTR  R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),8(R2)       COM CODE IN KEY IS RIGHT JUSTIFIED           
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         JNE   COMNFERR            COMMENT CODE NOT FOUND                       
         MVC   01(06,R4),KEY+04    GET VALIDATED COMMENT CODE                   
*                                                                               
VR42     BRAS  RE,BUMPFLD          BUMP TO CONTROL FLD                          
         CLI   5(R2),0             ANY INPUTS ON CONTROL FLD?                   
         BNE   *+18                                                             
         OC    1(6,R4),1(R4)       COMMENT CODE PRESENT?                        
         JNZ   MSSNGERR            YES, BUT CONTROL CODE IS MISSING             
         B     VR44                NO COM CODE & CONTROL, CK NEXT FLD           
         OC    1(6,R4),1(R4)       COMMENT CODE PRESENT?                        
         JZ    INVFDERR            CONTROL HAS INPUT, BUT NO COM CODE           
*                                                                               
         LA    R1,8(R2)            CK FOR DUPLICATE ENTRIES                     
         CLC   0(1,R1),1(R1)       1ST AND 3RD CHARS SAME?                      
         BNE   *+12                                                             
         CLI   0(R1),C' '          NOT A SPACE?                                 
         JH    DUPEDERR            DUPLICATE ERROR                              
         CLC   0(1,R1),2(R1)       1ST AND 2ND CHARS SAME?                      
         BNE   *+12                                                             
         CLI   0(R1),C' '          NOT A SPACE?                                 
         JH    DUPEDERR            DUPLICATE ERROR                              
         CLC   1(1,R1),2(R1)       2ND AND 3RD CHARS SAME?                      
         BNE   *+12                                                             
         CLI   1(R1),C' '          NOT A SPACE?                                 
         JH    DUPEDERR            DUPLICATE ERROR                              
*                                                                               
         LA    R1,8(R2)            POINT TO CONTROL FLD'S DATA                  
         LA    RF,3                THREE INPUT CHARS TO BE PROCESSED            
VR42H    CLI   0(R1),C' '                                                       
         BNH   VR42K                                                            
         CLI   0(R1),C'R'          R=REGULAR?                                   
         BNE   *+12                                                             
         OI    0(R4),X'80'                                                      
         B     VR42K                                                            
         CLI   0(R1),C'C'          C=CASH DISCOUNT?                             
         BNE   *+12                                                             
         OI    0(R4),X'40'                                                      
         B     VR42K                                                            
         CLI   0(R1),C'A'          A=ADJUSTMENT?                                
         BNE   *+12                                                             
         OI    0(R4),X'20'                                                      
         B     VR42K                                                            
         J     INVFDERR            INPUT CHAR IS INVALID                        
VR42K    LA    R1,1(R1)            POINT TO NEXT INPUT CHAR                     
         BCT   RF,VR42H                                                         
*                                                                               
VR44     BRAS  RE,BUMPFLD          BUMP TO BILL TYPES FLD                       
         CLI   5(R2),0             ANY INPUTS ON BILL TYPES FLD?                
         BE    VR44T                                                            
         OC    1(6,R4),1(R4)       COMMENT CODE PRESENT?                        
         BNZ   VR44D                                                            
         CLC   8(3,R2),=C'ALL'     NO COM CODE AND BILL TYPE IS "ALL"?          
         JNE   INVFDERR                                                         
         XC    8(4,R2),8(R2)       CLEAR BILL TYPE IF ALL                       
         OI    6(R2),X'80'         TRANSMIT                                     
         B     VR44T                                                            
VR44D    CLC   8(3,R2),=C'ALL'                                                  
         BE    VR44T                                                            
         LA    R1,8(R2)            CK FOR DUPLICATE ENTRIES                     
         CLC   0(1,R1),1(R1)       1ST AND 2ND CHARS SAME?                      
         BNE   *+12                                                             
         CLI   0(R1),C' '          NOT A SPACE?                                 
         JH    DUPEDERR            DUPLICATE ERROR                              
         CLC   0(1,R1),2(R1)       1ST AND 3RD CHARS SAME?                      
         BNE   *+12                                                             
         CLI   0(R1),C' '          NOT A SPACE?                                 
         JH    DUPEDERR            DUPLICATE ERROR                              
         CLC   0(1,R1),3(R1)       1ST AND 4TH CHARS SAME?                      
         BNE   *+12                                                             
         CLI   0(R1),C' '          NOT A SPACE?                                 
         JH    DUPEDERR            DUPLICATE ERROR                              
         CLC   1(1,R1),2(R1)       2ND AND 3RD CHARS SAME?                      
         BNE   *+12                                                             
         CLI   1(R1),C' '          NOT A SPACE?                                 
         JH    DUPEDERR            DUPLICATE ERROR                              
         CLC   1(1,R1),3(R1)       2ND AND 4TH CHARS SAME?                      
         BNE   *+12                                                             
         CLI   1(R1),C' '          NOT A SPACE?                                 
         JH    DUPEDERR            DUPLICATE ERROR                              
         CLC   2(1,R1),3(R1)       3RD AND 4TH CHARS SAME?                      
         BNE   *+12                                                             
         CLI   2(R1),C' '          NOT A SPACE?                                 
         JH    DUPEDERR            DUPLICATE ERROR                              
*                                                                               
* SET LOWER 4 BITS ON, WILL VALIDATE BITS ONE BY ONE                            
*                                                                               
         LA    RF,4                                                             
         OI    0(R4),X'0F'                                                      
*                                                                               
VR44H    CLI   0(R1),C' '                                                       
         BNH   VR44M                                                            
         CLI   0(R1),C'4'                                                       
         BNE   *+12                                                             
         XI    0(R4),X'01'                                                      
         B     VR44M                                                            
         CLI   0(R1),C'5'                                                       
         BNE   *+12                                                             
         XI    0(R4),X'02'                                                      
         B     VR44M                                                            
         CLI   0(R1),C'6'                                                       
         BNE   *+12                                                             
         XI    0(R4),X'04'                                                      
         B     VR44M                                                            
         CLI   0(R1),C'7'                                                       
         BNE   *+12                                                             
         XI    0(R4),X'08'                                                      
         B     VR44M                                                            
         J     INVFDERR                                                         
VR44M    LA    R1,1(R1)                                                         
         BCT   RF,VR44H                                                         
*                                                                               
VR44T    BRAS  RE,BUMPFLD          BUMP TO NEXT SET OF COMMENT CODE FLD         
         LA    R4,7(R4)                                                         
         BCT   R3,VR40D                                                         
*                                                                               
         DROP  R6                                                               
*                                                                               
VRX      DS    0H                                                               
         J     DR                  RECORD VALIDATED, REDISPLAY IT               
*                                                                               
* R2=FLD, R3=BASE CODE, MUST BE SET BEFORE CALLING THIS ROUTINE                 
*                                                                               
VR_EBASE DS    0H                  EDIT BASE                                    
         ST    RE,FULL                                                          
         OC    8(5,R2),SPACES                                                   
         LA    RE,BASLST           POINT TO BEGINNING OF TABLE                  
         LA    RF,BASLSTN          NUMBER OF ENTRY IN TABLE                     
VR_EB30  CLC   8(5,R2),1(RE)                                                    
         BE    VR_EB40                                                          
         LA    RE,6(RE)                                                         
         BCT   RF,VR_EB30                                                       
         MVI   0(R3),X'FF'         RETURN ERROR                                 
         B     *+10                                                             
VR_EB40  MVC   0(1,R3),0(RE)       VALIDATED BASE FORMULA CODE                  
         L     RE,FULL                                                          
         BR    RE                                                               
*                                                                               
* "USE PRD/EST BILLING PROFILE" IN B2 PROFILE MUST SET TO "Y"                   
* IN ORDER FOR VALUES IN TABLE TO BE USED AS OVERRIDES                          
*                                                                               
VR_COLBP DS    0H                  COLUMNS TO PRINT ON BILL                     
         DC    C'1'                G,N=STRIP-OFF COL                            
         DC    C'2'                N,G=STRIP-OFF COL                            
         DC    C'3'                G,N,NOTHING ON STRIP-OFF COL                 
         DC    C'4'                G,CD,G-CD,N=STRIP-OFF COL                    
         DC    C'5'                N,CD,N-CD,G=STRIP-OFF COL                    
         DC    C'6'                G,N,CD,NOTHING ON STRIP-OFF COL              
         DC    C'7'                G,N,G-CD,N=STRIP-OFF COL                     
         DC    C'8'                G,N,N-CD,G=STRIP-OFF COL                     
         DC    C'9'                G,CD,N-CD,N=STRIP-OFF COL                    
         DC    C'A'                G,G-CD,N-CD,N=STRIP-OFF COL                  
         DC    C'B'                G,CD,G-CD,G=STRIP-OFF COL                    
         DC    C'C'                N,CD,N-CD,N=STRIP-OFF COL                    
         DC    C'D'                G,CD,N=STRIP-OFF COL                         
         DC    C'E'                N,CD,G=STRIP-OFF COL                         
         DC    X'FF'               END OF TABLE                                 
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DR       DS    0H                  DISPLAY RECORD                               
         L     R6,AIO                                                           
         CLI   3(R6),X'06'         PRD RECORD CODE?                             
         BNE   *+12                                                             
         MVI   ELCODE,X'06'        FIRST PRD ELEM CODE                          
         B     DR10                                                             
         CLI   3(R6),X'07'         EST RECORD CODE?                             
         BNE   *+12                                                             
         MVI   ELCODE,X'07'        FIRST EST ELEM CODE                          
         B     DR10                                                             
         DC    H'0'                INVALID RECORD CODE!                         
*                                                                               
DR10     BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                FIRST ELEM MUST BE THERE!                    
*                                                                               
         LR    RE,R6                                                            
         USING PPRDELEM,RE                                                      
         CLI   0(R6),X'06'         PRD ELEM?                                    
         BNE   *+12                                                             
         LA    R6,PPRDBILP         POINT TO PRD BILLING PROFILE                 
         B     DR20                                                             
         USING PESTELEM,RE                                                      
         CLI   0(R6),X'07'         EST ELEM?                                    
         BNE   *+12                                                             
         LA    R6,PESTBILP         POINT TO EST BILLING PROFILE                 
         B     DR20                                                             
         DROP  RE                                                               
*                                                                               
         DC    H'0'                SOMETHING WRONG WITH ELEM IN REC             
*                                                                               
         USING BILPROF,R6          PRD/EST BILLING PROFILE DSECT                
DR20     MVC   WORK(1),BILBASA                                                  
         CLI   BILCMSW,C'C'        COMMISSION ONLY                              
         BNE   *+8                                                              
         OI    WORK,X'10'                                                       
         CLI   BILNBSW,C'Y'        NOT FOR BILLING                              
         BNE   *+8                                                              
         OI    WORK,X'20'                                                       
         LA    R2,BSCBASAH         BASE A                                       
         LA    R3,WORK                                                          
         BRAS  RE,DR_FBCOD                                                      
*                                                                               
         LA    R2,BSCBASBH         BASE B                                       
         LA    R3,BILBASB                                                       
         BRAS  RE,DR_FBCOD                                                      
*                                                                               
         XC    BSCADJ,BSCADJ       PERCENTAGE ADJUSTMENT                        
         OC    BILADJ,BILADJ                                                    
         BNZ   DR24H                                                            
         CLI   BILBASA,0           FORMULA PRESENT?                             
         BE    DR24U               NO                                           
         MVC   BSCADJ(2),=C'+0'                                                 
         B     DR24U                                                            
DR24H    EDIT  (B3,BILADJ),(8,BSCADJ+1),4,ALIGN=LEFT                            
         OI    BSCADJ+6,C'0'                                                    
         OI    BSCADJ+7,C'0'                                                    
         OI    BSCADJ+8,C'0'                                                    
         LA    RF,BSCADJ+8                                                      
         CLI   0(RF),C'0'                                                       
         BH    *+20                                                             
         BL    *+12                                                             
         MVI   0(RF),C' '                                                       
         BCT   RF,*-16                                                          
         MVI   0(RF),C' '                                                       
         MVI   BSCADJ,C'+'                                                      
         TM    BILADJ,X'80'                                                     
         BZ    *+8                                                              
         MVI   BSCADJ,C'-'                                                      
DR24U    OI    BSCADJH+6,X'80'                                                  
*                                                                               
         XC    BSCPADJ,BSCPADJ     "IF AC, DISPLAY AS" FLD                      
         OC    BILPADJ,BILPADJ                                                  
         BNZ   DR26H                                                            
         CLI   BILPBASB,0          FORMULA OVERRIDE PRESENT?                    
         BE    DR26U               NO                                           
         MVC   BSCPADJ(2),=C'+0'                                                
         B     DR26U                                                            
*                                                                               
DR26H    EDIT  (B3,BILPADJ),(8,BSCPADJ+1),4,ALIGN=LEFT                          
         OI    BSCPADJ+6,C'0'                                                   
         OI    BSCPADJ+7,C'0'                                                   
         OI    BSCPADJ+8,C'0'                                                   
         LA    RF,BSCPADJ+8                                                     
         CLI   0(RF),C'0'                                                       
         BH    *+20                                                             
         BL    *+12                                                             
         MVI   0(RF),C' '                                                       
         BCT   RF,*-16                                                          
         MVI   0(RF),C' '                                                       
*                                                                               
         MVI   BSCPADJ,C'+'                                                     
         TM    BILPADJ,X'80'                                                    
         BZ    *+8                                                              
         MVI   BSCPADJ,C'-'                                                     
DR26U    OI    BSCPADJH+6,X'80'                                                 
*                                                                               
         LA    R2,BSCPBASH         "PCT. OF" FLD                                
         LA    R3,BILPBASB                                                      
         BRAS  RE,DR_FBCOD                                                      
*                                                                               
         XC    BSCADAT,BSCADAT     ADJUSTMENT EFF DATE                          
         OC    BILADAT,BILADAT                                                  
         BZ    DR28U                                                            
         GOTO1 DATCON,DMCB,(3,BILADAT),(9,BSCADAT)                              
DR28U    OI    BSCADATH+6,X'80'                                                 
*                                                                               
         MVC   BSCDETS,BILDETS     DETAILS ON BILL                              
         OI    BSCDETSH+6,X'80'                                                 
*                                                                               
         LA    R4,BILCMNTS                                                      
         LHI   R3,3                THREE COMMENT FLDS TO BE PROCESSED           
         LA    R2,BSCCOM1H                                                      
*                                                                               
DR30E    OC    1(6,R4),1(R4)       COMMENT NUMBER PRESENT?                      
         BNZ   DR30G                                                            
         XC    8(6,R2),8(R2)       CLR COMMENT NUMBER                           
         OI    6(R2),X'80'                                                      
         BRAS  RE,BUMPFLD          BUMP TO CONTROL FLD                          
         XC    8(3,R2),8(R2)       CLR CONTROL FLD                              
         OI    6(R2),X'80'                                                      
         BRAS  RE,BUMPFLD          BUMP TO BILL TYPE FLD                        
         XC    8(4,R2),8(R2)       CLR BILL TYPE FLD                            
         B     DR30T                                                            
*                                                                               
DR30G    MVC   8(6,R2),1(R4)       COMMENT NUM                                  
DR30J    CLI   8(R2),C' '          NEED TO LEFT JUSTIFY?                        
         BH    DR30M                                                            
         MVC   8(5,R2),8+1(R2)     OVERLAPPING MOVE                             
         MVI   8+5(R2),C' '                                                     
         B     DR30J                                                            
DR30M    OI    6(R2),X'80'         DISPLAY LEFT JUSTIFIED COMMENT NUM           
         BRAS  RE,BUMPFLD          BUMP TO CONTROL FLD                          
         XC    8(3,R2),8(R2)                                                    
         LA    RE,8(R2)                                                         
         TM    0(R4),X'80'         REGULAR?                                     
         BZ    *+12                                                             
         MVI   0(RE),C'R'          R=REGULAR                                    
         LA    RE,1(RE)                                                         
         TM    0(R4),X'40'         CASH DISCOUNT?                               
         BZ    *+12                                                             
         MVI   0(RE),C'C'          C=CD                                         
         LA    RE,1(RE)                                                         
         TM    0(R4),X'20'         ADJUSTMENT?                                  
         BZ    *+12                                                             
         MVI   0(RE),C'A'          A=ADJ                                        
         LA    RE,1(RE)                                                         
         OI    6(R2),X'80'         DISPLAY CONTROL FLD                          
*                                                                               
         BRAS  RE,BUMPFLD          BUMP TO BILL TYPE FLD                        
         XC    8(4,R2),8(R2)                                                    
         TM    0(R4),X'EE'                                                      
         BZ    DR30T               NO COMMENT AND NO BILL TYPE                  
         TM    0(R4),X'0F'                                                      
         BZ    DR30P                                                            
         LA    RE,8(R2)                                                         
         TM    0(R4),X'01'                                                      
         BO    *+12                                                             
         MVI   0(RE),C'4'                                                       
         LA    RE,1(RE)                                                         
         TM    0(R4),X'02'                                                      
         BO    *+12                                                             
         MVI   0(RE),C'5'                                                       
         LA    RE,1(RE)                                                         
         TM    0(R4),X'04'                                                      
         BO    *+12                                                             
         MVI   0(RE),C'6'                                                       
         LA    RE,1(RE)                                                         
         TM    0(R4),X'08'                                                      
         BO    *+12                                                             
         MVI   0(RE),C'7'                                                       
         LA    RE,1(RE)                                                         
         B     *+10                                                             
DR30P    MVC   8(3,R2),=C'ALL'                                                  
DR30T    OI    6(R2),X'80'                                                      
         BRAS  RE,BUMPFLD          BUMP TO NEXT COMMENT FLD                     
         LA    R4,7(R4)            POINT TO NEXT SET IN BILL PROF               
         BCT   R3,DR30E            SHOULD LOOP THREE TIMES                      
*                                                                               
DR45     DS    0H                  FOR FUTURE FLDS                              
*                                                                               
DRX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* R2=FLD, R3=BASE CODE, MUST BE SET BEFORE CALLING THIS ROUTINE                 
*                                                                               
DR_FBCOD DS    0H                  EXPAND BASE CODE                             
         ST    RE,FULL                                                          
         LA    RE,BASLST           POINT TO BEGINNING OF TABLE                  
         LA    RF,BASLSTN          NUMBER OF ENTRY IN TABLE                     
DR_FB20  CLC   0(1,R3),0(RE)                                                    
         BE    DR_FB30                                                          
         LA    RE,6(RE)            NEXT ENTRY IN TABLE                          
         BCT   RF,DR_FB20                                                       
         MVC   8(5,R2),SPACES                                                   
         B     DR_FB40                                                          
DR_FB30  MVC   8(5,R2),1(RE)                                                    
DR_FB40  OI    6(R2),X'80'                                                      
         L     RE,FULL             RETURN ADDRESS                               
         BR    RE                                                               
*                                                                               
BASLST   DS    0C                                                               
         DC    X'01',C'G    '                                                   
         DC    X'02',C'N    '                                                   
         DC    X'05',C'G-CD '                                                   
         DC    X'06',C'N-CD '                                                   
         DC    X'08',C'AC   '                                                   
         DC    X'11',C'CG   '      PRECEED BASE WITH 'C' FOR COMM               
         DC    X'12',C'CN   '                                                   
         DC    X'15',C'CG-CD'                                                   
         DC    X'16',C'CN-CD'                                                   
         DC    X'18',C'CAC  '                                                   
         DC    X'21',C'EG   '      PRECEED WITH E IF NOT FOR BILLING            
         DC    X'22',C'EN   '      ('EST' ONLY)                                 
         DC    X'25',C'EG-CD'                                                   
         DC    X'26',C'EN-CD'                                                   
         DC    X'28',C'EAC  '                                                   
BASLSTN  EQU   (*-BASLST)/6        NUMBER IN LIST                               
*                                                                               
BUMPFLD  SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         TM    1(R2),X'20'         PROTECTED?  (BYPASS PROTECTED)               
         BZR   RE                                                               
         J     BUMPFLD                                                          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DK       DS    0H                  DISPLAY KEY                                  
         L     R6,AIO                                                           
         CLI   3(R6),X'06'         PRD RECORD CODE?                             
         BE    DK10                                                             
         CLI   3(R6),X'07'         EST RECORD CODE?                             
         BE    DK10                                                             
         DC    H'0'                                                             
         USING PRESBILK,R6                                                      
DK10     MVC   BSCMED,PRES_MED                                                  
         MVC   BSCCLT,PRES_CLT                                                  
         MVC   BSCPRD,PRES_PRD                                                  
         CLI   RECNUM,58           EST BILLING PROFILE?                         
         BNE   DK20                                                             
         EDIT  (B2,PRES_EST),(3,BSCEST),0,ALIGN=RIGHT,                 +        
               ZERO=NOBLANK,FILL=0                                              
         DROP  R6                                                               
*                                                                               
DK20     MVI   USEIONUM,2          USE AIO2 FOR MED/CLT/PRD/EST RECS            
*                                                                               
         OI    BSCMEDH+6,X'80'     TRANSMIT MEDIA CODE                          
         XC    BSCMEDN,BSCMEDN                                                  
         OI    BSCMEDNH+6,X'80'    CLEARED MEDIA NAME                           
         MVI   BSCMEDH+5,1         INPUT LENGTH                                 
         LA    R2,BSCMEDH                                                       
         GOTO1 VALIMED                                                          
         MVC   BSCMEDN,MEDNM                                                    
         OI    BSCMEDNH+6,X'80'    TRANSMIT MEDIA NAME                          
*                                                                               
         OI    BSCCLTH+6,X'80'     TRANSMIT CLIENT CODE                         
         XC    BSCCLTN,BSCCLTN                                                  
         OI    BSCCLTNH+6,X'80'    CLEARED CLIENT NAME                          
         MVI   BSCCLTH+5,3         INPUT LENGTH                                 
         LA    R2,BSCCLTH                                                       
         GOTO1 VALICLT                                                          
         MVC   BSCCLTN,CLTNM                                                    
         OI    BSCCLTNH+6,X'80'    TRANSMIT CLIENT NAME                         
*                                                                               
         OI    BSCPRDH+6,X'80'     TRANSMIT PRD CODE                            
         XC    BSCPRDN,BSCPRDN                                                  
         OI    BSCPRDNH+6,X'80'    CLEARED PRD NAME                             
         MVI   BSCPRDH+5,3         INPUT LENGTH                                 
         LA    R2,BSCPRDH                                                       
         GOTO1 VALIPRD                                                          
         MVC   BSCPRDN,PRDNM                                                    
         OI    BSCPRDNH+6,X'80'    TRANSMIT PRD NAME                            
*                                                                               
         CLI   RECNUM,58           EST BILLING PROFILE?                         
         BNE   DKX                                                              
         OI    BSCESTH+6,X'80'     TRANSMIT EST CODE                            
         XC    BSCESTN,BSCESTN                                                  
         OI    BSCESTNH+6,X'80'    CLEARED EST NAME                             
         MVI   BSCESTH+5,3         INPUT LENGTH                                 
         LA    R2,BSCESTH                                                       
         GOTO1 VALIEST                                                          
         MVC   BSCESTN,ESTNM                                                    
         OI    BSCESTNH+6,X'80'    TRANSMIT EST NAME                            
*                                                                               
DKX      DS    0H                                                               
         MVC   AIO,AIO1            AIO1 HAS REC TO BE DISPLAYED                 
         MVI   USEIONUM,1          RESET TO AIO1                                
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PPTRS    DS    0H                  REC IS JUST CHANGED                          
*                                                                               
         BRAS  RE,PUTREQRC         TO REQUEST AUTO T/A REPORTS                  
         BE    PPTRS_X                                                          
         LHI   R2,65               CANNOT GENERATE REQ FOR T/A REPORT           
         BRAS  RE,GET_ITXT                                                      
         LA    R2,CONACTH                                                       
         J     TRAPERR2            ERROR OR COMPLETION MSG IS SET               
*                                                                               
PPTRS_X  B     EXIT                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
TRAPERR2 GOTO1 ERREX2                                                           
*                                                                               
MSSNGERR MVI   ERROR,001                                                        
         J     TRAPERR                                                          
*                                                                               
INVFDERR MVI   ERROR,002                                                        
         J     TRAPERR                                                          
*                                                                               
NTNUMERR MVI   ERROR,003           NOT VALID NUMERIC DATA                       
         J     TRAPERR                                                          
*                                                                               
RECACERR MVI   ERROR,012           INVALID RECORD ACTION ERROR                  
         LA    R2,CONACTH          POINT TO ACTION FLD                          
         J     TRAPERR                                                          
*                                                                               
RECNFERR MVI   ERROR,053           RECORD NOT FOUND                             
         J     TRAPERR                                                          
*                                                                               
INVCLERR MVI   ERROR,062           INVALID CLIENT                               
         J     TRAPERR                                                          
*                                                                               
INVDTERR MVI   ERROR,068           INVALID DATE FORMAT                          
         J     TRAPERR                                                          
*                                                                               
CLTRQERR MVI   ERROR,085           SPECIFIC CLT REQUIRED (SECURITY)             
         J     TRAPERR                                                          
*                                                                               
PFKEYERR MVI   ERROR,088           INVALID PFKEY                                
         LA    R2,CONACTH          POINT TO ACTION FLD                          
         J     TRAPERR                                                          
*                                                                               
CLTACERR MVI   ERROR,089           CLIENT LIMIT ACCESS ERROR                    
         J     TRAPERR                                                          
*                                                                               
MAXLNERR MVI   ERROR,090           MAXIMUM RECORD SIZE EXCEEDED                 
         J     TRAPERR                                                          
*                                                                               
NACBFERR MVI   ERROR,114           CLT PROF DISALLOW ADD/CHG TO BF              
         J     TRAPERR                                                          
*                                                                               
ADJFDERR MVI   ERROR,115           INVALID ADJUSTMENT FLD                       
         J     TRAPERR                                                          
*                                                                               
COMNFERR MVI   ERROR,165           COMMENT NOT FOUND                            
         J     TRAPERR                                                          
*                                                                               
DUPEDERR MVI   ERROR,179           DUPLICATE ENTRIES ERROR MSG                  
         J     TRAPERR                                                          
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
INITIALZ NTR1  BASE=*,LABEL=*      INITIALIZE WORKING STORAGES                  
*                                                                               
         CLI   TRANSSW,C'Y'        TRANSFERRED INTO PROGRAM?                    
         BNE   INITI50                                                          
         CLI   PFAID,0             PF KEY PRESSED?                              
         BE    INITI50             NO                                           
*                                                                               
         OC    KEY(25),KEY         HAVE KEY?                                    
         BZ    INITI50                                                          
         LA    R3,KEY                                                           
         CLI   3(R3),X'06'         PRD REC CODE?                                
         BE    INITI20                                                          
         CLI   3(R3),X'07'         EST REC CODE?                                
         BNE   INITI50                                                          
         USING PRESBILK,R3                                                      
INITI20  LA    R2,BSCMEDH          MEDIA FLD ON MAINT SCR                       
         CLI   ACTNUM,ACTLIST      LIST?                                        
         BNE   *+8                                                              
         LA    R2,LSTMEDH          POINT TO MEDIA FLD ON LIST SCR               
         MVC   8(1,R2),PRES_MED                                                 
         MVI   5(R2),1             INPUT LENGTH                                 
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
         LA    R2,BSCCLTH          CLT FLD ON MAINT SCR                         
         CLI   ACTNUM,ACTLIST      LIST?                                        
         BNE   *+8                                                              
         LA    R2,LSTCLTH          POINT TO CLT FLD ON LIST SCR                 
         MVC   8(3,R2),PRES_CLT                                                 
         MVI   5(R2),3             INPUT LENGTH                                 
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
         LA    R2,BSCPRDH          PRD FLD ON MAINT SCR                         
         CLI   ACTNUM,ACTLIST      LIST?                                        
         BNE   *+8                                                              
         LA    R2,LSTPRDH          POINT TO PRD FLD ON LIST SCR                 
         MVC   8(3,R2),PRES_PRD                                                 
         MVI   5(R2),3             INPUT LENGTH                                 
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
         LA    R2,BSCESTH          EST FLD ON MAINT SCR                         
         CLI   ACTNUM,ACTLIST      LIST?                                        
         BNE   *+8                                                              
         LA    R2,LSTESTH          POINT TO PRD FLD ON LIST SCR                 
         EDIT  (B2,PRES_EST),(3,8(R2)),0,ALIGN=RIGHT,                  +        
               ZERO=NOBLANK,FILL=0                                              
         MVI   5(R2),3             INPUT LENGTH                                 
         OI    6(R2),X'80'         TRANSMIT                                     
         DROP  R3                                                               
*                                                                               
INITI50  MVI   ACTELOPT,C'N'       NO ACTIVITY ELEM WILL BE ADDED               
         MVC   BSCESHD,=C'Estimate'                                             
         OI    BSCESHDH+6,X'80'                                                 
         NI    BSCESTH+1,X'FF'-X'20'                                            
         OI    BSCESTH+6,X'80'                                                  
         XC    BSCBOTL,BSCBOTL                                                  
         MVC   BSCBOTL(07),=C'Pf2=Est'                                          
         MVC   BSCBOTL+07+02(L'PFPRTXT1),PFPRTXT1                               
         MVC   BSCBOTL+07+02+L'PFPRTXT1+02(L'PFPRTXT2),PFPRTXT2                 
         OI    BSCBOTLH+6,X'80'    PUT Pf2=Est NOTATION ON BOTTOM LINE          
*                                                                               
         CLI   RECNUM,56           PRD BILLING PROFILE?                         
         JNE   EXIT                                                             
         XC    BSCESHD,BSCESHD     CLR EST FLD HEADER (TITLE)                   
         OI    BSCESHDH+6,X'80'                                                 
         XC    BSCEST,BSCEST       CLR EST INPUT FLD                            
         OI    BSCESTH+1,X'20'     PROTECT INPUT FLD                            
         OI    BSCESTH+6,X'80'                                                  
         XC    BSCESTN,BSCESTN     CLR EST NAME FLD                             
         OI    BSCESTNH+6,X'80'                                                 
         XC    BSCBOTL,BSCBOTL                                                  
         MVC   BSCBOTL(L'PFPRTXT1),PFPRTXT1                                     
         MVC   BSCBOTL+L'PFPRTXT1+02(L'PFPRTXT2),PFPRTXT2                       
         OI    BSCBOTLH+6,X'80'    CLR Pf2=Prd NOTATION ON BOTTOM LINE          
*                                                                               
         J     EXIT                                                             
*                                                                               
PFPRTXT1 DC    C'Pf3=Clt  PF4=Prd  Pf5=CltList'                                 
PFPRTXT2 DC    C'Pf6=PrdList  Pf7=EstList'                                      
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKPFKEYS NTR1  BASE=*,LABEL=*      CKING FOR PK KEYS                            
*                                                                               
         CLI   RECNUM,58           EST BILLING PROFILE?                         
         BNE   *+12                                                             
         CLI   PFAID,2             PF2, EST MAINT?                              
         BE    CKPFK10             VALID ONLY ON EST BILLING MAINT SCR          
*                                                                               
         CLI   PFAID,3             PF3, CLT MAINT?                              
         BE    CKPFK10                                                          
         CLI   PFAID,4             PF3, CLT MAINT?                              
         BE    CKPFK10                                                          
         CLI   PFAID,5             PF5, CLT LIST?                               
         BE    CKPFK10                                                          
         CLI   PFAID,6             PF6, PRD LIST?                               
         BE    CKPFK10                                                          
         CLI   PFAID,7             PF7, EST LIST?                               
         BE    CKPFK10                                                          
*                                                                               
         J     SETCCNEQ            VALID PFKEY IS NOT ENTERED                   
*                                                                               
CKPFK10  XC    WORK,WORK           ESTABLISH AS XCTL ELEMENT                    
         LA    R1,WORK                                                          
         USING GLVXFRSY,R1                                                      
*                                                                               
         MVC   GLVXFRSY,=C'PRI'    SET FROM SYSTEM                              
         MVC   GLVXFRPR,=C'SFM'    SET FROM PROGRAM                             
         MVC   GLVXTOSY,=C'PRI'    SET TO   SYSTEM                              
         MVC   GLVXTOPR,=C'SFM'    SET TO   PROGRAM                             
         OI    GLVXFLG1,GLV1RETN                                                
         OI    GLVXFLG1,GLV1RETG                                                
*                                                                               
* SEND XCTL ELM                                                                 
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',WORK,14,GLVXCTL                           
*                                                                               
         MVC   DUB,SPACES          PREPARE IT FOR RECORD FLD                    
*                                                                               
         CLI   PFAID,2             RECORD IS EST (FOR MAINT)?                   
         BNE   CKPFK13                                                          
CKPFK12H MVC   DUB,=C'ESTIMATE'                                                 
         B     CKPFK25                                                          
*                                                                               
CKPFK13  CLI   PFAID,3             RECORD IS CLT (FOR MAINT)?                   
         BNE   CKPFK14                                                          
CKPFK13H MVC   DUB,=C'CLIENT  '                                                 
         B     CKPFK25                                                          
*                                                                               
CKPFK14  CLI   PFAID,4             RECORD IS PRD (FOR MAINT)?                   
         BNE   CKPFK15                                                          
CKPFK14H MVC   DUB,=C'PRODUCT '                                                 
         B     CKPFK25                                                          
*                                                                               
CKPFK15  CLI   PFAID,5             RECORD IS CLT (FOR LIST)?                    
         BNE   CKPFK16                                                          
         B     CKPFK13H                                                         
*                                                                               
CKPFK16  CLI   PFAID,6             RECORD IS PRD (FOR LIST)?                    
         BNE   CKPFK17                                                          
         B     CKPFK14H                                                         
*                                                                               
CKPFK17  CLI   PFAID,7             RECORD IS EST (FOR LIST)?                    
         BNE   CKPFK18                                                          
         B     CKPFK12H                                                         
*                                                                               
CKPFK18  DS    0H                  FOR FUTURE PFKEYS                            
*                                                                               
         J     SETCCNEQ            NO OTHER PFKEY DEFINED YET                   
*                                                                               
* SET RECORD FLD                                                                
*                                                                               
CKPFK25  GOTO1 VGLOBBER,DMCB,=C'PUTD',DUB,8,GLVXREC                             
*                                                                               
         MVC   DUB,SPACES          PREPARE IT FOR ACTION FLD                    
*                                                                               
         CLI   PFAID,2             EST MAINT?                                   
         BE    CKPFK28H                                                         
         CLI   PFAID,3             CLT MAINT?                                   
         BE    CKPFK28H                                                         
         CLI   PFAID,4             PRD MAINT?                                   
         BE    CKPFK28H                                                         
*                                                                               
         B     CKPFK30             CK OTHER PFKEYS FOR LIST                     
*                                                                               
CKPFK28H MVC   DUB,=C'CHANGE  '                                                 
         CLI   ACTNUM,ACTCHA       CHANGE ACTION?                               
         BE    CKPFK40                                                          
         CLI   THISLSEL,C'C'       SEL CODE IS CHANGE ON LIST?                  
         BE    CKPFK40                                                          
         CLI   MODE,VALREC         MODE IS VALREC?                              
         BE    CKPFK40                                                          
         CLI   MODE,RECPUT         MODE IS PUTREC? (STILL CHG)                  
         BE    CKPFK40                                                          
         CLI   MODE,XRECPUT        MODE IS XPUTREC?                             
         BE    CKPFK40                                                          
         MVC   DUB,=C'DISPLAY '                                                 
         B     CKPFK40                                                          
*                                                                               
CKPFK30  CLI   PFAID,5             CLT LIST?                                    
         BNE   CKPFK31                                                          
CKPFK30H MVC   DUB,=C'LIST    '                                                 
         B     CKPFK40                                                          
*                                                                               
CKPFK31  CLI   PFAID,6             PRD LIST?                                    
         BNE   CKPFK32                                                          
         B     CKPFK30H                                                         
*                                                                               
CKPFK32  CLI   PFAID,7             EST LIST?                                    
         BNE   CKPFK33                                                          
         B     CKPFK30H                                                         
*                                                                               
CKPFK33  DS    0H                  FOR FUTURE PFKEYS                            
*                                                                               
         J     SETCCNEQ            NO OTHER PFKEY DEFINED YET                   
*                                                                               
* SET ACTION FLD                                                                
*                                                                               
CKPFK40  GOTO1 VGLOBBER,DMCB,=C'PUTD',DUB,8,GLVXACT                             
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTF',BSCMEDH,,GLVPRKEY   KEY                   
         GOTO1 VGLOBBER,DMCB,=C'PUTF',BSCMEDH,,GLVPRMD    MEDIA                 
         GOTO1 VGLOBBER,DMCB,=C'PUTF',BSCCLTH,,GLVPRCLT   CLIENT                
         GOTO1 VGLOBBER,DMCB,=C'PUTF',BSCPRDH,,GLVPRPRD   PRODUCT               
*                                                                               
         CLI   RECNUM,58           EST BILLING PROFILE?                         
         JNE   SETCCEQ                                                          
         GOTO1 VGLOBBER,DMCB,=C'PUTF',BSCESTH,,GLVPREST   ESTIMATE              
         J     SETCCEQ                                                          
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PUTREQRC NTR1  BASE=*,LABEL=*      PUT A REQUEST CARD FOR T/A REPORT            
*                                                                               
         XC    QCTL,QCTL                                                        
         MVC   QAREA,SPACES                                                     
         MVC   QAREA+00(2),=C'41'                                               
         MVC   QAREA+02(2),AGENCY                                               
         MVC   QAREA+04(1),QMED                                                 
         MVC   QAREA+05(3),QCLT                                                 
         MVC   QAREA+11(3),QPRD                                                 
*                                                                               
         CLI   RECNUM,58           EST BILL?                                    
         BNE   PUTRQ10                                                          
         SR    RE,RE                                                            
         ICM   RE,3,BEST                                                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QAREA+20(3),DUB                                                  
*                                                                               
PUTRQ10  MVC   QAREA+68(7),=C'AUTOREQ'                                          
*                                                                               
         MVI   QCTL+10,41                                                       
         MVI   QCTL+14,106                                                      
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'PREQUEST',QCTL,QCTL                    
*                                                                               
         TM    DMCB+8,X'FF'                                                     
         JZ    SETCCEQ                                                          
         J     SETCCNEQ                                                         
*                                                                               
GET_ITXT ST    RE,SAVERE                                                        
         XC    FULL,FULL                                                        
         MVI   FULL+1,25           SYSTEM                                       
         L     R3,FULL                                                          
         GOTOR GETTXT,DMCB+12,(R2),0,(C'I',DMCB),0,0,(R3)                       
         OI    CONHEADH+6,X'80'                                                 
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
*                                                                               
       ++INCLUDE PRSFMFFD                                                       
         EJECT                                                                  
*                                                                               
         PRINT ON                                                               
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMA2D          PRD/EST BILL MAINT SCR                       
         EJECT                                                                  
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMA4D          EST LIST SCR                                 
*                                                                               
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE PRSFMWORKD                                                     
         ORG   SYSSPARE            WORKING AREA                                 
SVWORK   DS    XL64                GENERAL WORKING STORAGE                      
*                                                                               
WKSVSTRT DS    0H                  START OF VALUES TO BE SAVED                  
WKESRTYP DS    CL(L'PESTRTYP)      EST RATE TYPE                                
WKCOLPRB DS    CL(L'BILDETS)       COLUMN TO PRINT ON BILL                      
WKSVLNQ  EQU   *-WKSVSTRT                                                       
*                                                                               
SAVERE   DS    F                   FOR SAVING RETURN ADDRESSES                  
QCTL     DS    CL26                                                             
QAREA    DS    CL80                                                             
*                                                                               
PRESBILK DSECT                     PRD/EST BILL KEY                             
PRES_AGY DS    CL2                                                              
PRES_MED DS    C                                                                
PRES_RTY DS    X                   RECORD TYPE                                  
PRES_CLT DS    CL3                                                              
PRES_PRD DS    CL3                                                              
PRES_EST DS    CL2                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE PPRDREC           PRD RECORD                                   
         EJECT                                                                  
*                                                                               
       ++INCLUDE PESTREC           EST RECORD                                   
         EJECT                                                                  
*                                                                               
       ++INCLUDE PBILPROF          PRD/EST BILLING PROFILE                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDGLVXCTLD        GLOBBER TRANSFER CONTROLS                    
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDGLOBEQUS        DSECT FOR GLOBBER                            
         EJECT                                                                  
*                                                                               
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
*                                                                               
       ++INCLUDE CTGENRFP                                                       
         EJECT                                                                  
*                                                                               
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'039PRSFM1F   02/17/10'                                      
         END                                                                    
