*          DATA SET PRSFM2C    AT LEVEL 022 AS OF 02/25/20                      
*PHASE T41C2CC                                                                  
*INCLUDE PPBROWSE                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*               T41C2C - ESTIMAT2 (ES2) MAINT/LIST                              
*                                                                               
* RKEJ 07/23/19 NEW ALLOCATION EXTENSION FIELD   SPEC-37333                     
*                                                                               
* KWAN 06/60/05 BROWSE FUNCTION                                                 
*                                                                               
* KWAN 02/04/05 NEED TO GENERATE AN AUTO P41 T/A REPORT                         
*                                                                               
* KWAN 06/03/04 FIX SFH ERROR MSG FOR ACTION CHANGE                             
*                                                                               
* KWAN 04/11/03 CONVERT ESTIMATE FROM FIL TO SFM                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*  CALLED FROM  GENCON VIA T41C00 (SFM CONTROLLER)                    *         
*                                                                     *         
*  COMMENTS     SUPPORTS CHG, DISP                                    *         
*                                                                     *         
*  INPUTS       SCREEN T41CA5 (ESTIMAT2 MAINTENANCE)                  *         
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
         TITLE 'T41C2C - ESTIMAT2 (ES2) MAINT/LIST'                             
*                                                                               
T41C2C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41C2C,RR=R3                                                   
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
         LA    R2,ES2MEDH          POINT TO MEDIA FLD                           
         GOTO1 VALIMED                                                          
*                                                                               
         MVC   ES2MEDN,MEDNM                                                    
         OI    ES2MEDNH+6,X'80'    DISPLAY MEDIA NAME                           
*                                                                               
         XC    SVKEY,SVKEY                                                      
         LA    R6,SVKEY                                                         
         USING PESTKEY,R6                                                       
*                                                                               
         MVC   PESTKAGY,AGENCY                                                  
         MVC   PESTKMED,QMED                                                    
         MVI   PESTKRCD,X'07'      RECORD CODE FOR EST                          
*                                                                               
         LA    R2,ES2CLTH          POINT TO CLT FLD                             
         CLI   5(R2),0                                                          
         JE    MSSNGERR                                                         
*                                                                               
         CLI   8(R2),C'='          BROWSE?                                      
         BNE   VK24                                                             
*                                                                               
         GOTO1 =V(PPBROWSE),DMCB,ACOMFACS,SYSRD,(R2),                  +        
               0,(QMED,C' CLT'),0,RR=RELO                                       
         DC    H'0'                                                             
*                                                                               
VK24     CLI   5(R2),3             MAX 3 CHARS                                  
         JH    INVFDERR                                                         
         CLC   =C'ALL',8(R2)       ALL CLIENT?                                  
         JE    INVFDERR            ALL IS NOT ALLOWED AS CLIENT CODE            
*                                                                               
         GOTO1 VALICLT                                                          
*                                                                               
         MVC   PESTKCLT,QCLT                                                    
         MVC   ES2CLTN,CLTNM                                                    
         OI    ES2CLTNH+6,X'80'    DISPLAY CLT NAME                             
*                                                                               
         LA    R2,ES2PRDH          POINT TO PRD FLD                             
         CLI   5(R2),0                                                          
         JE    MSSNGERR                                                         
*                                                                               
         CLI   8(R2),C'='          BROWSE?                                      
         BNE   VK34                                                             
                                                                                
         GOTO1 =V(PPBROWSE),DMCB,ACOMFACS,SYSRD,(R2),                  +        
               (0,QCLT),(QMED,C' PRD'),0,RR=RELO                                
         DC    H'0'                                                             
*                                                                               
VK34     CLI   5(R2),3             MAX 3 CHARS                                  
         JH    INVFDERR                                                         
         CLC   =C'ALL',8(R2)       ALL PRD?                                     
         JE    INVFDERR            ALL IS NOT ALLOWED AS PRD CODE               
*                                                                               
         GOTO1 VALIPRD                                                          
*                                                                               
         MVC   PESTKPRD,QPRD                                                    
         MVC   ES2PRDN,PRDNM                                                    
         OI    ES2PRDNH+6,X'80'    DISPLAY PRD NAME                             
*                                                                               
         LA    R2,ES2ESTH          POINT TO EST FLD                             
         CLI   5(R2),0                                                          
         JE    MSSNGERR                                                         
*                                                                               
         CLI   8(R2),C'='          BROWSE?                                      
         BNE   VK54                                                             
                                                                                
         GOTO1 =V(PPBROWSE),DMCB,ACOMFACS,SYSRD,(R2),                  +        
               (0,QCLT),(QMED,C' EST'),0,RR=RELO                                
         DC    H'0'                                                             
*                                                                               
VK54     CLI   5(R2),3             MAX 3 CHARS                                  
         JH    INVFDERR                                                         
         TM    4(R2),X'08'         VALID NUMBERIC?                              
         JZ    NTNUMERR                                                         
*                                                                               
         GOTO1 VALIEST                                                          
*                                                                               
         MVC   PESTKEST,BEST       BINARY EST                                   
         MVC   ES2ESTN,ESTNM                                                    
         OI    ES2ESTNH+6,X'80'    DISPLAY PRD NAME                             
*                                                                               
VKX      MVC   KEY,SVKEY                                                        
         MVC   AIO,AIO1            RECORD WILL BE READ INTO AIO1                
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VR       DS    0H                  VALIDATE DATA FOR PROCTER $ GAMBLE           
         L     RF,ACOMFACS                                                      
         L     RF,CGETFACT-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(2,0)                                                  
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         MVC   LINID,FALINE        LINE ID                                      
         MVC   LINADDR,FAADDR      LINE ADDRESS                                 
         DROP  R1                                                               
         MVI   MASTEMNL,0          INIT MASTER TERMINAL SWITCH                  
         LA    RF,MASTTAB          TABLE OF MASTER TERMINAL IDS                 
VR06D    CLI   0(RF),X'FF'         END OF TABLE?                                
         BE    VR10                                                             
         CLC   LINID(8),0(RF)      FOUND IN TABLE?                              
         BE    *+12                                                             
         LA    RF,8(RF)            NEXT ENTRY IN TABLE                          
         B     VR06D                                                            
         MVI   MASTEMNL,C'Y'       SET MASTER TERMINAL SWITCH                   
*                                                                               
VR10     L     R6,AIO                                                           
         MVI   ELCODE,X'07'                                                     
         MVI   ALLO1FLG,NO         ALLOCATIONS NOT AVAILABLE ON EST             
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                FIRST ELEM MUST BE THERE!                    
*                                                                               
         USING PESTELEM,R6                                                      
*                                                                               
         OC    PESTZZZ,PESTZZZ     ALLOCATION FIELD PRESENT ?                   
         BZ    *+8                                                              
         MVI   ALLO1FLG,YES        ALLOCATIONS AVAILABLE ON EST                 
*                                                                               
         MVC   WKEPROF,PESTPROF    SAVE "OLD" EST PROFILE VALUES                
         MVC   WKESTTST,PESTTEST   EST TEST STATUS BYTE                         
*                                                                               
* INITIALIZE ENTIRE PROFILE TO C'0'                                             
*                                                                               
         MVI   PESTPROF,C'0'                                                    
         MVC   PESTPROF+1(L'PESTPROF-1),PESTPROF                                
*                                                                               
         LA    R2,ES2BDCOH         BILLABLE DATE CALC OVVERRIDE                 
         CLI   5(R2),0                                                          
         BE    VR22                                                             
         CLC   8(1,R2),WKEPROF+01  SAME INPUT AS IN RECORD?                     
         BE    VR20M                                                            
         CLI   8(R2),C'0'          USE AGENCY BILLABLE DATE FORMULA?            
         BE    VR20M                                                            
         CLI   8(R2),C'1'          OVERRIDE BILL. DATE FORMULA?                 
         BE    VR20M                                                            
*                                                                               
         J     INVFDERR            OTHER OPTIONS ARE INVALID FOR NOW            
*                                                                               
VR20M    MVC   PESTPROF+01(01),8(R2)                                            
*                                                                               
VR22     LA    R2,ES2BDBDH         BILLABLE DATE -> BASE DATE                   
         CLI   5(R2),0                                                          
         BE    VR24                                                             
         CLC   8(1,R2),WKEPROF+02  SAME INPUT AS IN RECORD?                     
         BE    VR22M                                                            
         CLI   8(R2),C'0'          INSERTION DATE?                              
         BE    VR22M                                                            
         CLI   8(R2),C'1'          PAYABLE DATE?                                
         BE    VR22M                                                            
         CLI   8(R2),C'2'          ON-SALE DATE?                                
         BE    VR22M                                                            
         CLI   8(R2),C'3'          CLOSING DATE?                                
         BE    VR22M                                                            
*                                                                               
         J     INVFDERR            OTHER OPTIONS ARE INVALID FOR NOW            
*                                                                               
VR22M    MVC   PESTPROF+02(01),8(R2)                                            
*                                                                               
VR24     LA    R2,ES2BDDAH         BILLABLE DATE -> DATE ADJUSTMENT             
         CLI   5(R2),0                                                          
         BE    VR26                                                             
         CLC   8(1,R2),WKEPROF+03  SAME INPUT AS IN RECORD?                     
         BE    VR24M                                                            
         CLI   8(R2),C'0'          NO ADJUSTMENT TO BASE DATE?                  
         BE    VR24M                                                            
         CLI   8(R2),C'1'          1 MONTH BEFORE BASE DATE?                    
         BE    VR24M                                                            
         CLI   8(R2),C'2'          2 MONTHS BEFORE BASE DATE?                   
         BE    VR24M                                                            
         CLI   8(R2),C'3'          3 MONTHS BEFORE BASE DATE?                   
         BE    VR24M                                                            
         CLI   8(R2),C'A'          1 MONTH AFTER BASE DATE?                     
         BE    VR24M                                                            
         CLI   8(R2),C'B'          2 MONTHS AFTER BASE DATE?                    
         BE    VR24M                                                            
         CLI   8(R2),C'C'          3 MONTHS AFTER BASE DATE?                    
         BE    VR24M                                                            
         CLI   8(R2),C'J'          5 DAYS BEFORE BASE DATE?                     
         BE    VR24M                                                            
         CLI   8(R2),C'K'          7 DAYS BEFORE BASE DATE?                     
         BE    VR24M                                                            
         CLI   8(R2),C'L'          10 DAYS BEFORE BASE DATE?                    
         BE    VR24M                                                            
         CLI   8(R2),C'M'          11 DAYS BEFORE BASE DATE?                    
         BE    VR24M                                                            
         CLI   8(R2),C'N'          14 DAYS BEFORE BASE DATE?                    
         BE    VR24M                                                            
         CLI   8(R2),C'O'          15 DAYS BEFORE BASE DATE?                    
         BE    VR24M                                                            
         CLI   8(R2),C'P'          20 DAYS BEFORE BASE DATE?                    
         BE    VR24M                                                            
         CLI   8(R2),C'Q'          25 DAYS BEFORE BASE DATE?                    
         BE    VR24M                                                            
         CLI   8(R2),C'R'          30 DAYS BEFORE BASE DATE?                    
         BE    VR24M                                                            
*                                                                               
         J     INVFDERR            OTHER OPTIONS ARE INVALID FOR NOW            
*                                                                               
VR24M    MVC   PESTPROF+03(01),8(R2)                                            
*                                                                               
VR26     LA    R2,ES2NNADH         NON-NEWSPAPER ADJUSTMENT                     
         CLI   5(R2),0                                                          
         BE    VR28                                                             
         CLC   8(1,R2),WKEPROF+04  SAME INPUT AS IN RECORD?                     
         BE    VR26M                                                            
         CLI   8(R2),C'0'          1 MONTH LATER THAN DATE CALC'D BY            
         BE    VR26M               BILLABLE DATE OVERRIDE FORMULA?              
         CLI   8(R2),C'1'          NO ADJUSTMENT TO DATE CALC'D BY              
         BE    VR26M               BILLABLE DATE OVERRIDE FORMULA?              
*                                                                               
         J     INVFDERR            OTHER OPTIONS ARE INVALID FOR NOW            
*                                                                               
VR26M    MVC   PESTPROF+04(01),8(R2)                                            
*                                                                               
VR28     DS    0H                  FOR FUTURE EST PROFILE VALUES                
*                                                                               
* VALIDATION OF EST OPTIONAL DATA FLDS                                          
*                                                                               
         XC    PESTPURO,PESTPURO   CLR PURCHASE ORDER $                         
         LA    R2,ES2PUROH         PURCHASE ORDER $                             
         CLI   5(R2),0                                                          
         BE    VR44                                                             
         SR    R3,R3                                                            
         IC    R3,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(2,8(R2)),(R3),0                                    
         CLI   DMCB,0                                                           
         JNE   INVFDERR                                                         
         MVC   PESTPURO,DMCB+4     PURCHASE ORDER $ IN PENNIES                  
*                                                                               
VR44     NI    PESTTEST,X'FF'-X'01'                                             
         TM    SVCLSTAT,X'01'      SFH CLT?                                     
         BZ    *+8                                                              
         OI    PESTTEST,X'01'      TURN ON ESTIMATE "SFH" STATUS                
         LA    R2,ES2SFHH          SPECIAL FINANCIAL HANDLING                   
         CLI   5(R2),0                                                          
         BE    VR46                                                             
         CLI   5(R2),1                                                          
         JNE   INVFDERR                                                         
         CLI   8(R2),C'N'          SFH=N?                                       
         BE    VR44M                                                            
         CLI   8(R2),C'Y'          SFH=Y?                                       
         JNE   INVFDERR                                                         
         OI    PESTTEST,X'01'      YES - TURN SFH "ON"                          
         B     VR46                                                             
VR44M    NI    PESTTEST,X'FF'-X'01'                                             
*                                                                               
VR46     XC    PESTCF,PESTCF       COST2 FACTOR                                 
         LA    R2,ES2COS2H                                                      
         CLI   5(R2),0                                                          
         BE    VR48                                                             
         CLI   F0PROF+4,C'$'       PROF ALLOW COS2 OPTION?                      
         JE    INVFDERR            ("$" IS ALLOWED FOR CLT)                     
         CLI   F0PROF+4,C'F'       PROF ALLOW COS2 OPTION?                      
         JNE   INVFDERR                                                         
         CLI   5(R2),10                                                         
         JH    INVFDERR            MAX INPUT LENGTH IS 10                       
         SR    R3,R3                                                            
         IC    R3,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(6,8(R2)),(R3)                                      
         CLI   DMCB,0                                                           
         JNE   INVFDERR                                                         
         L     R3,4(R1)                                                         
         C     R3,=F'9999999'                                                   
         JH    INVFDERR            MAX INPUT IS 9.999999                        
         LTR   R3,R3                                                            
         JL    INVFDERR            LESS THAN ZERO IS NO GOOD                    
         CVD   R3,DUB                                                           
         MVC   PESTCF,DUB+3        PESTCF IS PL5                                
*                                                                               
VR48     DS    0H                  FOR FUTURE EST OPTION FLDS                   
*                                                                               
* NEED TO CK SFH STATUS BIT, IT MAY NOT BE CHANGED ONCE IT IS SET               
*                                                                               
         CLI   MASTEMNL,C'Y'       MASTER TERMINAL?                             
         BE    VR50                                                             
         LA    R2,ES2SFHH          SPECIAL FINANCIAL HANDLING                   
         TM    PESTTEST,X'01'      "CURRENTLY" SFH?                             
         BZ    *+16                                                             
         TM    WKESTTST,X'01'      "FORMERLY" SFH?                              
         JZ    FLDCNERR            STATUS BIT HAS BEEN CHANGED                  
         B     VR50                                                             
         TM    WKESTTST,X'01'      "FORMERLY" SFH?                              
         JNZ   FLDCNERR            STATUS BIT HAS BEEN CHANGED                  
*                                                                               
         DROP  R6                  DONE WITH PESTELEM                           
*                                                                               
VR50     L     R6,AIO                                                           
         MVI   ELCODE,PEALLECQ     FIND X'66' ELEMENT (ALLOC EXT ELEM)          
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R2,ES2ALEXH                                                      
         CLI   5(R2),0                                                          
         JE    VR50X                                                            
         CLC   KEY+07(03),=C'ZZZ'                                               
         JNE   INVFDERR                                                         
         CLI   ALLO1FLG,YES        ALLOCATION EX FIELD ALLOWED ...              
         JNE   INVALLO2              ONLY IF ALLOCATION 1 PRESENT               
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         USING PESTALLX,R6                                                      
         MVI   PEALLELC,PEALLECQ   SET X'66' ELEMENT                            
         MVI   PEALLELN,PEALLELQ   SET LEN AS X'32' (DEC=50)                    
         MVC   PEALLZZZ,ES2ALEX                                                 
*                                                                               
         GOTO1 ADDELEM                                                          
         DROP  R6                  DONE WITH PESTALLX                           
VR50X    DS    0H                                                               
*                                                                               
VRX      DS    0H                                                               
         J     DR                  REDISPLAY VALIDATED RECORD                   
*                                                                               
* TABLE OF MASTER TERMINALS                                                     
*                                                                               
MASTTAB  DS    0H                                                               
* * * *  DC    C'DDNYF11T'         DDS                                          
* * * *  DC    C'DDNY720T'                                                      
* * * *  DC    C'DX03901T'                                                      
* * * *  DC    C'DDNYD26T'         DDS                                          
         DC    C'DDNYD03T'                                                      
* * * *  DC    C'DDNY700T'                                                      
         DC    C'DDNYE00T'                                                      
         DC    C'DX06200T'         (WAS DDNY700T)                               
         DC    C'HDTO80CT'         HDTO                                         
         DC    C'HDTO800T'         HDTO (WAS HDTO830T)                          
         DC    C'DDL1136T'         DDS-LA                                       
         DC    C'DDL1137T'         DDS-LA                                       
         DC    C'DDL1138T'         DDS-LA                                       
         DC    X'FF'               END OF TABLE                                 
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DR       DS    0H                  DISPLAY RECORD                               
         L     R6,AIO                                                           
         CLI   3(R6),X'07'         EST REC CODE?                                
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PESTELEM,R6                                                      
         MVI   ELCODE,X'07'        FIRST EST ELEM CODE                          
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    ES2BDCO,ES2BDCO                                                  
         MVC   ES2BDCO,PESTPROF+01                                              
         OI    ES2BDCOH+6,X'80'    BILL. DATE CALC. OVERRIDE                    
*                                                                               
         XC    ES2BDBD,ES2BDBD                                                  
         MVC   ES2BDBD,PESTPROF+02                                              
         OI    ES2BDBDH+6,X'80'    BILL. DATE -> BASE DATE                      
*                                                                               
         XC    ES2BDDA,ES2BDDA                                                  
         MVC   ES2BDDA,PESTPROF+03                                              
         OI    ES2BDDAH+6,X'80'    BILL. DATE -> DATE ADJ.                      
*                                                                               
         XC    ES2NNAD,ES2NNAD                                                  
         MVC   ES2NNAD,PESTPROF+04                                              
         OI    ES2NNADH+6,X'80'    NON-NEWSPAPER ADJUSTMENT                     
*                                                                               
         XC    ES2PURO,ES2PURO     PURCHASE ORDER $                             
         OC    PESTPURO,PESTPURO   ANY PURCHASE ORDER$?                         
         BZ    DR28U                                                            
         EDIT  PESTPURO,(11,ES2PURO),2,ALIGN=LEFT,ZERO=BLANK                    
DR28U    OI    ES2PUROH+6,X'80'                                                 
*                                                                               
         XC    ES2SFH,ES2SFH       SPECIAL FINANCIAL HANDLING                   
         TM    PESTTEST,X'01'      ESTIMATE SFH "ON"?                           
         BO    *+12                                                             
         TM    SVCLSTAT,X'01'      CLIENT SFH "ON"?                             
         BZ    DR30U                                                            
         MVI   ES2SFH,C'Y'                                                      
         TM    PESTTEST,X'01'      ESTIMATE SFH "ON"?                           
         BO    DR30U                                                            
         MVI   ES2SFH,C'N'         ESTIMATE SFH IS "OFF"                        
DR30U    OI    ES2SFHH+6,X'80'                                                  
*                                                                               
         XC    ES2COS2,ES2COS2     COST2 FACTOR                                 
         OC    PESTCF,PESTCF                                                    
         BZ    DR32U               NOTHING IN COST2 TO BE DISPLAYED             
         CP    PESTCF,=P'0'                                                     
         BNE   *+14                                                             
         MVC   ES2COS2(03),=C'0.0'  COS2=0.0                                    
         B     DR32U                                                            
         EDIT  (P5,PESTCF),(8,ES2COS2),6,ALIGN=LEFT,FILL=0                      
         LA    R1,ES2COS2+8                                                     
         LA    RF,8                CLR TRAILING ZEORS                           
DR32H    BCTR  R1,0                                                             
         CLI   0(R1),C'.'          DECIMAL POINT?                               
         BE    DR32M                                                            
         CLI   0(R1),C'0'          TRAILING ZERO?                               
         BNE   DR32U                                                            
         MVI   0(R1),0             CLR TRAILING ZERO                            
         BCT   RF,DR32H                                                         
*                                                                               
         DC    H'0'                INCORRECT OUTPUT FROM EDIT                   
*                                                                               
DR32M    AHI   R1,1                BUMP PASS DECIMAL POINT                      
         MVI   0(R1),C'0'          NON-SIGNIFICANT ZERO                         
DR32U    OI    ES2COS2H+6,X'80'                                                 
*                                                                               
DR34     DS    0H                  FOR FUTURE FLDS OF PESTELEM                  
*                                                                               
DR36     DS    0H                  DISPLAY ALLOCATION EXT X'66' ELEM            
         XC    ES2ALEX,ES2ALEX                                                  
         L     R6,AIO                                                           
         MVI   ELCODE,PEALLECQ     FIND X'66' ELEMENT                           
         BRAS  RE,GETEL                                                         
         JNE   DR36X                                                            
         USING PESTALLX,R6                                                      
         MVC   ES2ALEX(L'PEALLZZZ),PEALLZZZ                                     
         OI    ES2ALEXH+6,X'80'                                                 
*                                                                               
DR36X    DS    0H                                                               
*                                                                               
DRX      DS    0H                                                               
         J     EXIT                                                             
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DK       DS    0H                  DISPLAY KEY                                  
         L     R6,AIO                                                           
         CLI   3(R6),X'07'         EST REC CODE?                                
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PESTKEY,R6                                                       
         MVC   ES2MED,PESTKMED                                                  
         MVC   ES2CLT,PESTKCLT                                                  
         MVC   ES2PRD,PESTKPRD                                                  
         EDIT  (B2,PESTKEST),ES2EST,0,ALIGN=RIGHT,ZERO=NOBLANK,FILL=0           
         DROP  R6                                                               
*                                                                               
         MVI   USEIONUM,2          USE AIO2 FOR MED/CLT/PRD/EST RECS            
*                                                                               
         OI    ES2MEDH+6,X'80'     TRANSMIT MEDIA CODE                          
         XC    ES2MEDN,ES2MEDN                                                  
         OI    ES2MEDNH+6,X'80'    CLEARED MEDIA NAME                           
         MVI   ES2MEDH+5,1         INPUT LENGTH                                 
         LA    R2,ES2MEDH                                                       
         GOTO1 VALIMED                                                          
         MVC   ES2MEDN,MEDNM                                                    
         OI    ES2MEDNH+6,X'80'    TRANSMIT MEDIA NAME                          
*                                                                               
         OI    ES2CLTH+6,X'80'     TRANSMIT CLIENT CODE                         
         XC    ES2CLTN,ES2CLTN                                                  
         OI    ES2CLTNH+6,X'80'    CLEARED CLIENT NAME                          
         MVI   ES2CLTH+5,3         INPUT LENGTH                                 
         LA    R2,ES2CLTH                                                       
         GOTO1 VALICLT                                                          
         MVC   ES2CLTN,CLTNM                                                    
         OI    ES2CLTNH+6,X'80'    TRANSMIT CLIENT NAME                         
*                                                                               
         OI    ES2PRDH+6,X'80'     TRANSMIT PRD CODE                            
         XC    ES2PRDN,ES2PRDN                                                  
         OI    ES2PRDNH+6,X'80'    CLEARED PRD NAME                             
         MVI   ES2PRDH+5,3         INPUT LENGTH                                 
         LA    R2,ES2PRDH                                                       
         GOTO1 VALIPRD                                                          
         MVC   ES2PRDN,PRDNM                                                    
         OI    ES2PRDNH+6,X'80'    TRANSMIT PRD NAME                            
*                                                                               
         OI    ES2ESTH+6,X'80'     TRANSMIT EST CODE                            
         XC    ES2ESTN,ES2ESTN                                                  
         OI    ES2ESTNH+6,X'80'    CLEARED EST NAME                             
         MVI   ES2ESTH+5,3         INPUT LENGTH                                 
         LA    R2,ES2ESTH                                                       
         GOTO1 VALIEST                                                          
         MVC   ES2ESTN,ESTNM                                                    
         OI    ES2ESTNH+6,X'80'    TRANSMIT EST NAME                            
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
FLDCNERR MVI   ERROR,79            FIELD CANNOT BE CHANGED                      
         J     TRAPERR                                                          
*                                                                               
CLTRQERR MVI   ERROR,85            SPECIFIC CLT REQUIRED (SECURITY)             
         J     TRAPERR                                                          
*                                                                               
CLTACERR MVI   ERROR,89            CLIENT LIMIT ACCESS ERROR                    
         J     TRAPERR                                                          
*                                                                               
RECNFERR MVI   ERROR,53            RECORD NOT FOUND                             
         J     TRAPERR                                                          
*                                                                               
INVCLERR MVI   ERROR,62            INVALID CLIENT                               
         J     TRAPERR                                                          
*                                                                               
INVDTERR MVI   ERROR,68            INVALID DATE FORMAT                          
         J     TRAPERR                                                          
*                                                                               
MAXLNERR MVI   ERROR,90            MAXIMUM RECORD SIZE EXCEEDED                 
         J     TRAPERR                                                          
*                                                                               
PROFERR1 MVI   ERROR,96            MASTER CLT (7-9) MUST BE 000                 
         J     TRAPERR                                                          
*                                                                               
MCLNFERR MVI   ERROR,94            MASTER CLT NOT FOUND                         
         J     TRAPERR                                                          
*                                                                               
PFKEYERR MVI   ERROR,88            INVALID PFKEY                                
         LA    R2,CONACTH          POINT TO ACTION FLD                          
         J     TRAPERR                                                          
*                                                                               
INVALLO2 LHI   R2,344              ALLOCATION MUST BE FILLED BEFORE ...         
         BRAS  RE,GET_ETXT                      ALLOCATION EXTN ES2             
         LA    R2,ES2ALEXH                                                      
         J     TRAPERR2                                                         
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
         CLI   3(RE),X'07'         EST REC CODE?                                
         BNE   INITI50                                                          
         USING PESTKEY,R3                                                       
         LA    R2,ES2MEDH          MEDIA FLD ON MAINT SCR                       
         CLI   ACTNUM,ACTLIST      LIST?                                        
         BNE   *+8                                                              
         LA    R2,LSTMEDH          POINT TO MEDIA FLD ON LIST SCR               
         MVC   8(1,R2),PESTKMED                                                 
         MVI   5(R2),1             INPUT LENGTH                                 
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
         LA    R2,ES2CLTH          CLT FLD ON MAINT SCR                         
         CLI   ACTNUM,ACTLIST      LIST?                                        
         BNE   *+8                                                              
         LA    R2,LSTCLTH          POINT TO CLIENT FLD ON LIST SCR              
         MVC   8(3,R2),PESTKCLT                                                 
         MVI   5(R2),3             INPUT LENGTH                                 
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
         LA    R2,ES2PRDH          PRD FLD ON MAINT SCR                         
         CLI   ACTNUM,ACTLIST      LIST?                                        
         BNE   *+8                                                              
         LA    R2,LSTPRDH          POINT TO PRD FLD ON LIST SCR                 
         MVC   8(3,R2),PESTKPRD                                                 
         MVI   5(R2),3             INPUT LENGTH                                 
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
         LA    R2,ES2ESTH          EST FLD ON MAINT SCR                         
         CLI   ACTNUM,ACTLIST      LIST?                                        
         BNE   *+8                                                              
         LA    R2,LSTESTH          POINT TO EST FLD ON LIST SCR                 
         EDIT  (B2,PESTKEST),(3,8(R2)),0,ALIGN=RIGHT,                  +        
               ZERO=NOBLANK,FILL=0                                              
         MVI   5(R2),3             INPUT LENGTH                                 
         OI    6(R2),X'80'         TRANSMIT                                     
         DROP  R3                                                               
*                                                                               
INITI50  MVI   ACTELOPT,C'N'       NO ACTIVITY ELEM WILL BE ADDED               
*                                                                               
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKPFKEYS NTR1  BASE=*,LABEL=*      CKING FOR PK KEYS                            
*                                                                               
         CLI   PFAID,2             PF2, EST (MAINT)?                            
         BE    CKPFK10                                                          
         CLI   PFAID,3             PF3, CLT (MAINT)?                            
         BE    CKPFK10                                                          
         CLI   PFAID,4             PF4, PRD (MAINT)?                            
         BE    CKPFK10                                                          
         CLI   PFAID,5             PF5, CLT LIST?                               
         BE    CKPFK10                                                          
         CLI   PFAID,6             PF6, PRD LIST?                               
         BE    CKPFK10                                                          
         CLI   PFAID,7             PF7, EST LIST?                               
         BE    CKPFK10                                                          
         CLI   PFAID,8             PF8, EST BILL?                               
         BE    CKPFK10                                                          
         CLI   PFAID,9             PF9, EST COPY?                               
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
         CLI   PFAID,2             REC IS EST (FOR MAINT)?                      
         BNE   CKPFK13                                                          
CKPFK12H MVC   DUB,=C'ESTIMATE'                                                 
         B     CKPFK25                                                          
*                                                                               
CKPFK13  CLI   PFAID,3             REC IS CLT (FOR MAINT)?                      
         BNE   CKPFK14                                                          
CKPFK13H MVC   DUB,=C'CLIENT  '                                                 
         B     CKPFK25                                                          
*                                                                               
CKPFK14  CLI   PFAID,4             REC IS PRD (FOR MAINT)?                      
         BNE   CKPFK15                                                          
CKPFK14H MVC   DUB,=C'PRODUCT '                                                 
         B     CKPFK25                                                          
*                                                                               
CKPFK15  CLI   PFAID,5             REC IS CLT (FOR LIST)?                       
         BNE   CKPFK16                                                          
         B     CKPFK13H                                                         
*                                                                               
CKPFK16  CLI   PFAID,6             REC IS PRD (FOR LIST)?                       
         BNE   CKPFK17                                                          
         B     CKPFK14H                                                         
*                                                                               
CKPFK17  CLI   PFAID,7             REC IS EST (FOR LIST)?                       
         BNE   CKPFK18                                                          
         B     CKPFK12H                                                         
*                                                                               
CKPFK18  CLI   PFAID,8             REC IS EST BILL (FOR MAINT)?                 
         BNE   CKPFK19                                                          
         MVC   DUB,=C'ESBILL  '                                                 
         B     CKPFK25                                                          
*                                                                               
CKPFK19  CLI   PFAID,9             REC IS EST (FOR COPY)?                       
         BNE   CKPFK20                                                          
         B     CKPFK12H                                                         
*                                                                               
CKPFK20  DS    0H                  FOR FUTURE PFKEYS                            
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
         CLI   PFAID,8             EST BILL MAINT?                              
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
CKPFK33  CLI   PFAID,9             EST COPY?                                    
         BNE   CKPFK34                                                          
         MVC   DUB,=C'COPY    '                                                 
         B     CKPFK40                                                          
*                                                                               
CKPFK34  DS    0H                  FOR FUTURE PFKEYS                            
*                                                                               
         J     SETCCNEQ            NO OTHER PFKEY DEFINED YET                   
*                                                                               
* SET ACTION FLD                                                                
*                                                                               
CKPFK40  GOTO1 VGLOBBER,DMCB,=C'PUTD',DUB,8,GLVXACT                             
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTF',ES2MEDH,,GLVPRKEY   KEY                   
         GOTO1 VGLOBBER,DMCB,=C'PUTF',ES2MEDH,,GLVPRMD    MEDIA                 
         GOTO1 VGLOBBER,DMCB,=C'PUTF',ES2CLTH,,GLVPRCLT   CLIENT                
         GOTO1 VGLOBBER,DMCB,=C'PUTF',ES2PRDH,,GLVPRPRD   PRODUCT               
         GOTO1 VGLOBBER,DMCB,=C'PUTF',ES2ESTH,,GLVPREST   ESTIMATE              
*                                                                               
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
         SR    RE,RE                                                            
         ICM   RE,3,BEST                                                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QAREA+20(3),DUB                                                  
*                                                                               
         MVC   QAREA+68(7),=C'AUTOREQ'                                          
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
GET_ETXT ST    RE,SAVERE           GET ERROR TEXT ROUTINE                       
         XC    FULL,FULL                                                        
         L     R3,FULL             GET MSG FROM DEFAULT CONNECTED SYS           
         GOTOR GETTXT,DMCB+12,(R2),0,(C'E',DMCB),0,0,(R3)                       
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
       ++INCLUDE PRSFMA5D          ESTIMAT2 (ES2) MAINT SCREEN                  
         EJECT                                                                  
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMA4D          ESTIMATE LIST SCREEN                         
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE PRSFMWORKD                                                     
         ORG   SYSSPARE            WORKING AREA                                 
*                                                                               
SVWORK   DS    XL64                GENERAL WORKING STORAGE                      
MASTEMNL DS    CL1                 MASTER TERMINAL SWITCH                       
ALLO1FLG DS    CL1                 ALLOCATIONS EST FLAG                         
WKESTTST DS    CL(L'PESTTEST)                                                   
WKEPROF  DS    CL(L'PESTPROF)                                                   
LINID    DS    CL(L'FALINE)        LINE ID                                      
LINADDR  DS    CL(L'FAADDR)        TERMINAL ADDRESS                             
*                                                                               
SAVERE   DS    F                   FOR SAVING RETURN ADDRESSES                  
QCTL     DS    CL26                                                             
QAREA    DS    CL80                                                             
*                                                                               
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE PESTREC           EST REC DSECT                                
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
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE CTGENRFP                                                       
         EJECT                                                                  
*                                                                               
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022PRSFM2C   02/25/20'                                      
         END                                                                    
