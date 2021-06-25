*          DATA SET TASYSCALC  AT LEVEL 010 AS OF 03/01/17                      
*PHASE T00A8DB,*                                                                
         TITLE 'T00A8D - RATE CALCULATION ROUTINES'                             
***********************************************************************         
*        THIS GETS CALLED AT LEAST TWICE BY PAY (TAGEN51) FOR EACH CAST         
*        MEMBER BEING PAID - ONCE TO CALCULATE RATES TO DISPLAY ON              
*        SCREEN THE FIRST TIME (DISPLAY RTN), EACH TIME A CHANGE IS             
*        MADE (VALCAST RTN), AND AT THE VERY END WHEN THE PAYMENT IS            
*        ACTUALLY BEING PROCESSED (VALCAST RTN).  IF NOT PAYING WITH            
*        DETAIL, THIS ONLY GETS CALLED ONCE (ALLCNTL RTN).                      
***********************************************************************         
TACALC   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,TACALC,RA,R7                                                   
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     R8,4(R1)            R8=A(WORKING STORAGE)                        
         USING CALCD,R8                                                         
         L     R6,8(R1)            R6=A(COMMON FACILITIES FOR SYSTEM)           
         USING SYSCOMMD,R6                                                      
         L     R9,ASTARTSV         R9=A(GLOBAL SAVED VALUES)                    
         USING SYSWORKD,R9                                                      
         SPACE 3                                                                
         XC    TCLOCAL(TCEND-TCLOCAL),TCLOCAL  CLEAR LOCAL STORAGE              
         SPACE 1                                                                
         ST    RD,TCRD             SAVE RD FOR ERROR EXITS                      
         EJECT                                                                  
*              MAIN PROCESSING CONTROL                                          
         SPACE 1                                                                
         GOTOR INIT                INITIALIZE                                   
         BNE   ERRX                                                             
         SPACE 1                                                                
         BRAS  RE,EVECALC          IF EVENT PAYMENT, CALCULATE IT               
         BE    MAINX               AND EXIT                                     
         SPACE 1                                                                
         TM    TGUSSTA3,SOAPUSE    IF SOAP USE TYPE                             
         BZ    MAIN10                                                           
         CLI   TGUSEQU,USOP        AND IS NOT SOP USE (IS RESIDUAL)             
         BE    MAIN10                                                           
         MVI   OVERLAY,SORRATES    SET SOAP RESIDUAL RATE PHASE                 
         GOTOR LOADTBL             LOAD APPROPRIATE RATE TABLE                  
         SPACE 1                                                                
         TM    TCINPUT,TCINPAY     IF PAYMENT IS OVERRIDDEN                     
         BZ    MAIN2                                                            
         BRAS  RE,GRSCALC          CALCULATE GROSS AND SUBJECT TO P&H           
         B     MAINX               AND DON'T BOTHER WITH LOOK-UPS               
         SPACE                                                                  
MAIN2    BAS   RE,LOOKSOR          DO TABLE LOOK-UPS FOR SOR - GET RATE         
         BNE   ERR3                                                             
         B     MAINX               FINISH UP                                    
         SPACE                                                                  
MAIN10   CLI   TGUSMEDS,PRINT      TEST NOT PRINT USE                           
         BE    MAIN11                                                           
         GOTOR GETYEAR             SET VALUES FOR CONTRACT YR                   
         BNE   ERRX                                                             
         GOTOR LOADTBL             LOAD APPROPRIATE RATE TABLE                  
         SPACE 1                                                                
         CLC   TCSVYREQ,TGYREQU    IF SAVED YEAR IS DIFFERENT                   
         BE    MAIN11                                                           
         IC    R2,TGYREQU          SAVE TGYREQU IN R2                           
         MVC   TGYREQU,TCSVYREQ    RESET TGYREQU WITH SAVED YEAR                
         GOTOR GETYEAR             SET P&H RATE                                 
         BNE   ERRX                                                             
         STC   R2,TGYREQU          RESET TGYREQU                                
*                                                                               
MAIN11   DS    0H                                                               
*                                                                               
         TM    TCOPTS,TCOAPPLS     APPLY AS SESSION                             
         BO    MAIN11A                                                          
         TM    TGUSSTAT,SESSION    REUSE COME BACK ON 2ND PASS                  
         BZ    MAIN11B                                                          
         TM    TGUSSTA3,BRKBUNT    SO DOES ANY SESSION THAT BREAKS              
         BO    MAIN11B             DOWN BY UNIT                                 
         CLI   TGUSEQU,UVRE                                                     
         BE    MAIN11B                                                          
         CLI   TGUSEQU,UVNR                                                     
         BE    MAIN11B                                                          
         CLI   TGUSEQU,UEDS                                                     
         BE    MAIN11B                                                          
*                                                                               
MAIN11A  TM    TCINPUT,TCINPAY     IF PAYMENT IS OVERRIDDEN                     
         BZ    *+14                                                             
         MVC   TCGROSS,TCPAY       MOVE TO GROSS (TEMP. - SEE PAYCALC)          
         B     MAIN14              AND DON'T BOTHER WITH LOOK-UPS               
*                                                                               
MAIN11B  BRAS  RE,SETCAB           SET USE INFO FOR CAB FROM CBL                
         BNE   ERR3                OR SET MAX UNITS FOR CBL                     
*                                                                               
         TM    TGUSTYST,UPGRADE    TEST UPGRADE USE TYPE                        
         BZ    MAIN12                                                           
         BAS   RE,UPGNEWC          CALCULATE NEW RATE                           
         BAS   RE,UPGORIGC         CALCULATE ORIGINAL RATE                      
         BRAS  RE,UPGCALC          CALCULATE UPGRADE RATE (DIFFERENCE)          
         B     MAIN13              SKIP PAST REGULAR LOOK-UP                    
*                                                                               
MAIN12   BAS   RE,LOOKUP           DO TABLE LOOK-UPS - GET RATE (GROSS)         
         BE    MAIN12A                                                          
         TM    TCINPUT,TCINPAY                                                  
         BO    MAIN13X                                                          
         B     ERR3                                                             
MAIN12A  CLI   TGUSEQU,UACB        ADDENDUM CABLE?                              
         BNE   *+8                                                              
         BAS   RE,ACBCALC          CALCULATE ADDITIONAL AMOUNTS                 
*                                                                               
MAIN13   BRAS  RE,RESETCBL         RESET USE OR UNITS INFO FOR CBL              
*                                                                               
         TM    TCINPUT,TCINPAY     IF PAYMENT IS OVERRIDDEN                     
         BZ    MAIN14                                                           
MAIN13X  MVC   TCGROSS,TCPAY       MOVE TO GROSS (TEMP. - SEE PAYCALC)          
         TM    TGUSSTA2,NORATES    TEST HAVE RATES                              
         BZ    MAIN14                                                           
         L     RF,TCPAY                                                         
         TM    TCINPUT,TCINAPPL                                                 
         BZ    *+8                                                              
         A     RF,TCAPPLCR                                                      
         STCM  RF,15,WORK                                                       
MAIN13Y  GOTOR SVBRKDWN,DMCB,(0,WORK),(5,0)                                     
*                                                                               
MAIN14   TM    TGUSSTAT,SESSION    SESSIONS ARE SPECIAL                         
         BZ    MAIN15                                                           
         BAS   RE,BSSCALC          CALCULATE VARIOUS AMOUNTS                    
         BAS   RE,VITAUPGR         HANDLE VITA UPGRADE PAYMENTS                 
*                                                                               
MAIN15   BAS   RE,APPCALC          CALCULATE AMT TO BE APPLIED TO REUSE         
         BAS   RE,ACRCALC          CALCULATE APPLIED CREDITS                    
         BRAS  RE,ADJADAM          ADJUST FOR ADDITONAL AMOUNTS                 
         GOTOR PAYCALC             CALCULATE PAYMENT AMOUNT                     
         BAS   RE,AGTFCALC         CALCULATE AGENT FEE FOR PRINT                
*                                                                               
*        TM    TGUNEQU,ACT+UDA     CANADIANS ALWAYS GET I&R                     
         GOTOR UNITEST,DMCB,TGUNEQUS,ACT+UDA,0,0,0                              
         BNZ   MAIN18                                                           
*        TM    TGUNEQU,AFM         MUSICIANS ALWAYS GET P&H                     
         GOTOR UNITEST,DMCB,TGUNEQUS,AFM,0,0,0                                  
         BO    MAIN17                                                           
*                                                                               
MAIN16   TM    TCCASTA2,TACASTDP   IF DON'T PAY CANADIAN RATES                  
         BO    MAIN17                                                           
         TM    TCCASTA2,TACASTCR   OR CAST'S PAY CANADIAN RATES BIT OFF         
         BO    MAIN18                                                           
*                                                                               
         USING TACOD,R2                                                         
         L     R2,TCATACO                                                       
         TM    TACOSTAT,TACOSCRT   OR COMM'S PAY CANADIAN RATES BIT OFF         
         BO    MAIN18                                                           
         CLI   TACOCTYP,CCTY04A    OR IF COMMERCIAL IS ACTRA TYPE 2404A         
         BE    MAIN16A                                                          
         CLI   TACOCTYP,CCTY2404   OR 2404                                      
         BNE   MAIN17                                                           
         DROP  R2                                                               
MAIN16A  CLI   TGUSEQU,UBSC        AND MAKING A BSC PAYMENT                     
         BE    MAIN18                                                           
*                                                                               
MAIN17   BAS   RE,PNHCALC          CALCULATE PENSION & HEALTH                   
         B     *+8                                                              
MAIN18   BRAS  RE,INRCALC          CALCULATE INSURANCE & RETIREMENT             
         BRAS  RE,AOSCALC          CALCULATE ACTRA ACCIDENT ON SET              
*                                                                               
         CLI   TGUSEQU,UACB                                                     
         BE    MAIN18B                                                          
         CLI   TGUSEQU,ULCB                                                     
         BE    MAIN18B                                                          
         B     MAIN18H                                                          
*                                                                               
MAIN18B  LA    R1,TCUPGRS                                                       
         OC    TCUPGRS,TCUPGRS                                                  
         BNZ   *+8                                                              
         LA    R1,TCGROSS                                                       
         L     RF,0(R1)                                                         
         STCM  RF,15,WORK                                                       
*                                                                               
         OC    TCOVCAMT,TCOVCAMT                                                
         BZ    MAIN18C                                                          
         S     RF,TCOVCAMT                                                      
         STCM  RF,15,WORK                                                       
MAIN18C  GOTOR SVBRKDWN,DMCB,('SBDADJST',WORK),(2,0)                            
*                                                                               
MAIN18H  OC    TCAPPLCR,TCAPPLCR                                                
         BZ    MAIN18J                                                          
         L     R1,TCAPPLCR                                                      
         LCR   RF,R1                                                            
         STCM  RF,15,WORK                                                       
         GOTOR SVBRKDWN,DMCB,(0,WORK),(4,0)                                     
*        GOTOR SVBRKDWN,DMCB,('SBDADJST',WORK),(4,0)                            
*                                                                               
MAIN18J  OC    TCGUAR,TCGUAR                                                    
         BZ    MAIN19                                                           
*        L     R1,TCGUAR           THIS HANDLED BY TACKPRT                      
*        LCR   RF,R1                                                            
*        STCM  RF,15,WORK                                                       
*        GOTOR SVBRKDWN,DMCB,('SBDADJST',WORK),(4,0)                            
*                                                                               
MAIN19   DS    0H                                                               
         CLI   TGUSEQU,UBSM                                                     
         BE    MAIN20                                                           
         CLI   TGUSEQU,UIMS                                                     
         BE    MAIN20                                                           
         OC    TCDOUBL,TCDOUBL     DOUBLING?                                    
         BZ    MAIN20                                                           
         GOTOR SVBRKDWN,DMCB,(0,TCDOUBL),(7,0)                                  
*                                                                               
MAIN20   BRAS  RE,HNWCALC          CALCULATE HEALTH & WELFARE                   
*                                                                               
         BRAS  RE,DLRAPHLD         ADD APP CRED FOR HLD COVERED BY DLR          
*                                                                               
MAINX    GOTOR FINISH              FINISHING UP ROUTINES                        
         B     YES                                                              
         EJECT                                                                  
*              ROUTINE DOES ALL TABLE LOOK-UPS - SETS GROSS AMOUNT              
*              RETURNS CC NOT EQUAL IF NO RATES FOUND                           
         SPACE 1                                                                
LOOKUP   NTR1                                                                   
         BRAS  RE,PREPITN                                                       
         BRAS  RE,PREPVRE                                                       
         BNE   LOOKNO                                                           
         BRAS  RE,PREPVNR                                                       
         BNE   LOOKNO                                                           
         BRAS  RE,PREPEDS                                                       
         SPACE 1                                                                
         CLI   TGUSEQU,URAD        ACTRA RADIO REUSE                            
         BNE   LOOK0                                                            
         ICM   R4,15,TCACAST                                                    
         BZ    LOOK0                                                            
         MVI   ELCODE,TACRELQ      LOOK FOR FIRST CREDIT HISTORY EL.            
         BAS   RE,GETEL                                                         
         BNE   LOOK0                                                            
         USING TACRD,R4                                                         
         MVC   TCGROSS,TACRAPPL    TAKE AMOUNT FROM SESSION                     
         B     LOOK1A                                                           
         SPACE 1                                                                
LOOK0    CLI   TGUSEQU,USOP        IF SOP USE                                   
         BE    LOOK1                                                            
         CLI   TGUSEQU,UPRT        OR PRT USE                                   
         BE    LOOK1                                                            
         CLI   TGUSEQU,UPRS        OR PRS USE                                   
         BNE   LOOK1AA                                                          
*                                                                               
LOOK1    MVC   TCGROSS,TCOVAMT     GROSS AMT IS FROM OVSCALE                    
         CLI   TGUSEQU,USOP        IF SOP USE                                   
         BNE   LOOKYES             DON'T SAVE THIS IN BREAKDOWN DETAIL          
*                                                                               
LOOK1A   OC    TCGROSS,TCGROSS                                                  
         BZ    LOOKYES                                                          
         GOTOR SVBRKDWN,DMCB,(0,TCGROSS),(1,0)                                  
         B     LOOKYES                                                          
*                                                                               
LOOK1AA  CLI   TGUSEQU,ULNF        IF LNF                                       
         BNE   LOOK1A2                                                          
         CLI   TGYREQU,CN16        AND PERFORMER CONTRACT YEAR IS               
         BL    LOOKNO              BEFORE 2016                                  
LOOK1A2  GOTOR PAXADJ1             SET NUMBER OF CLASS A PROGRAMS               
         BNE   LOOKNO              TO PAY                                       
*                                                                               
LOOK1B   TM    TGUSSTA2,NORATES    TEST HAVE RATES                              
         BO    LOOKNO                                                           
         CLI   TGUSEQU,UBSC        CANADIAN SESSION?                            
         BNE   LOOK1BB                                                          
         USING TACOD,R2                                                         
         L     R2,TCATACO                                                       
         CLI   TACOCTYP,CCTY04B    ACTRA 2404B?                                 
         BNE   LOOK1BB                                                          
         DROP  R2                                                               
         MVI   TGUSEQU,UBSS        USING BSS RATE BOOK                          
         BRAS  RE,GETUSE           LOOK UP USE TYPE                             
         MVI   TGUSEQU,UBSC        RESET USE TYPE                               
         B     *+8                                                              
LOOK1BB  BRAS  RE,GETUSE           LOOK UP USE TYPE                             
         BNE   LOOKNO              SET CC NOT EQ IF NOT IN RATE TABLE           
         BAS   RE,GETCAT           LOOK UP CATEGORY                             
*                                                                               
         MVC   TCUSEN,TCNUSES      SET N'USES PAID SO FAR                       
         MVC   TCUSENL,TCNUSESL    AND N'USES PAID SO FAR TO LIFT VERS.         
         LA    R2,TCUSETAB         R2=TABLE OF USE DATES/LIFT STATUS            
         LH    R0,TCTUSES          LOOP FOR N'USES                              
*                                                                               
         TM    TCPAYST,TCITN                                                    
         BZ    *+14                                                             
         MVC   TCUSEN,TCUNITS                                                   
         B     LOOK1C                                                           
*                                                                               
         GOTOR PAXADJ2             SET LAST CLASS A USE                         
*                                                                               
LOOK1C   LTR   R0,R0               TEST HAVE N'USES                             
         BNZ   LOOK2B              ALWAYS HAVE AT LEAST 1 ENTRY                 
*                                                                               
         LA    R0,1                FOR USES WITHOUT N'USES, SET TO ONE          
         XC    TCUSEDTE,TCUSEDTE   USETAB NOT FILLED IN                         
         MVI   TCLIFT,0                                                         
         B     LOOK6               SKIP FILTERING                               
*                                                                               
LOOK2    TM    TCPAYST,TCITN                                                    
         BO    LOOK6                                                            
         TM    TCOPTS,TCONUMUS     OR NUMBER OF USES NOT OVERRIDDEN             
         BO    LOOK2D                                                           
LOOK2B   TM    TCPAYST,TCITN                                                    
         BO    LOOK6                                                            
         SPACE 1                                                                
         TM    3(R2),TCPAXPRO      IF PAX PROGRAM                               
         BZ    LOOK2C                                                           
         CLI   TGUSEQU,UCLA        AND MAKING CLASS A PAYMENT                   
         BNE   LOOK2C                                                           
         CLI   TGYREQU,CN00        AND PERFORMER CONTRACT YEAR IS               
         BL    LOOK2C              2000 OR GREATER                              
         LA    R2,4(R2)                                                         
         B     LOOK2               IGNORE THE PAX PROGRAM                       
         SPACE 1                                                                
LOOK2C   MVC   TCUSEDTE,0(R2)      USE TCUSETAB TO SET USE DATE                 
         MVI   TCLIFT,0                                                         
         TM    3(R2),TCLFTPRO+TCLFTPR2                                          
         BZ    *+8                                                              
         MVI   TCLIFT,C'Y'         AND LIFT INDICATOR                           
         SPACE 1                                                                
LOOK2D   CLI   TCLIFT,C'Y'         TEST PAYMENT IS TO LIFT VERSION              
         BNE   LOOK4                                                            
         TM    TCCASTAT,TACASTLF+TACASTLO  CAST MEMBER MUST BE ON LIFT          
         BNZ   LOOK2F                                                           
         TM    TCCASTA4,TACAST2O+TACAS2LF+TACASALL+TACASL2O                     
         BNZ   LOOK2F                                                           
         SPACE 1                                                                
         CLI   TGUSEQU,UCLA        IF NOT ON LIFT                               
         BNE   LOOK8               AND CLASS A PAYMENT                          
         CLI   TGYREQU,CN00        TO 2000 OR LATER PERFORMER                   
         BL    LOOK8               JUST BUMP AHEAD IN USE TABLE                 
         LA    R2,4(R2)            DO NOT DECREMENT USE COUNTER                 
         B     LOOK2                                                            
         SPACE 1                                                                
LOOK2F   CLI   TCLFTSEC,15                                                      
         BE    LOOK5                                                            
         CLI   TCLFTSEC,10                                                      
         BE    LOOK5                                                            
         B     LOOK6                                                            
         SPACE 1                                                                
LOOK4    CLI   TCVERSEC,0          IF PAYING A VERSION                          
         BE    LOOK4D                                                           
         CLI   TCVERSEC,15         TEST IT'S LENGTH IS 15 SEC                   
         BE    LOOK5                                                            
         CLI   TCVERSEC,10         OR 10 SEC                                    
         BE    LOOK5                                                            
         B     LOOK6                                                            
LOOK4D   TM    TCCASTAT,TACASTLO   PAYMENT NOT TO LIFT - SKIP IF CAST           
         BO    *+12                                                             
         TM    TCCASTA4,TACAST2O+TACASL2O                                       
         BZ    LOOK6               ONLY ON LIFT                                 
         CLI   TGUSEQU,UCLA        IF ONLY ON LIFT                              
         BNE   LOOK8               AND CLASS A PAYMENT TO                       
         CLI   TGYREQU,CN00        2000 OR LATER PERFORMER                      
         BL    LOOK8               JUST BUMP AHEAD IN USE TABLE                 
         LA    R2,4(R2)            DO NOT DECREMENT USE COUNTER                 
         B     LOOK2                                                            
         SPACE 1                                                                
LOOK5    LH    R1,TCUSENL                                                       
         AHI   R1,1                BUMP USE NUMBER FOR LIFT                     
         STH   R1,TCUSENL                                                       
         SPACE 1                                                                
LOOK6    LH    R1,TCUSEN                                                        
         AHI   R1,1                BUMP TOTAL USE NUMBER                        
         STH   R1,TCUSEN                                                        
         SPACE 1                                                                
LOOK6A   BRAS  RE,SETCOLA          SET UP COST OF LIVING INCREASE TABLE         
         BRAS  RE,GETRATE          LOOK UP/ACCUMULATE RATE                      
         CLI   TGUSEQU,UTAG        IF TAG USE                                   
         BNE   LOOK6B                                                           
         XR    RF,RF                                                            
         IC    RF,TCTAGS                                                        
         STCM  RF,3,WORK                                                        
         GOTOR SVBRKDWN,DMCB,('SBDADJST',TCGROSS),(57,WORK)                     
*                                                                               
LOOK6B   BAS   RE,ADJBSS           BASE SESSIONS MAY NEED ADJUSTMENT            
         BRAS  RE,ADJNWA           ADJUST NORTHWEST ADDENDUMS                   
         BRAS  RE,ADJNBM           ADJUST NBM PAYMENTS                          
         BRAS  RE,ADJIHM           ADJUST IHM PAYMENTS                          
         BRAS  RE,ADJAFM10         ADJUST NEW USES FOR AFM 10 CONTRACT          
         BRAS  RE,ADJ2DTS          ADJUST 2ND DUB TO SHORT PAYMENTS             
         BRAS  RE,ADJPRO           ADJUST PRM AND PRR PAYMENTS                  
         BRAS  RE,ADJAFM           ADJUST AFM PAYMENTS                          
*                                                                               
         BRAS  RE,ADJLCB           ADJUST TCUNITS AND TCUSENUM FOR LCB          
         BNE   LOOK7                                                            
         NI    TCSTAT,ALL-TCSTUSES TURN OFF BY USES INDICATOR                   
         BRAS  RE,GETRATE          ADD IN ADDITIONAL FEE FOR MAJORS             
         XC    TCUNITS,TCUNITS     CLEAR DUMMY UNITS                            
         SPACE 1                                                                
LOOK7    CLI   TGUSEQU,UFGR        IF FGR USE                                   
         BNE   *+10                                                             
         XC    TCUNITS,TCUNITS     CLEAR DUMMY UNITS                            
         SPACE 1                                                                
         CLI   TGUSEQU,UCNM        IF CANADIAN NEW MEDIA                        
         BNE   *+8                                                              
         BRAS  RE,ADJCNM           ADJUST TCUNITS FOR CNM                       
         SPACE 1                                                                
LOOK8    LA    R2,4(R2)            BUMP TO NEXT USE TABLE ENTRY                 
         BCT   R0,LOOK2            LOOP FOR N'USES                              
         SPACE 1                                                                
         GOTOR ADJITN              ADJUST ITN PAYMENTS                          
         BRAS  RE,ADJVRE                                                        
         BRAS  RE,ADJVNR                                                        
         BRAS  RE,ADJEDS                                                        
         GOTOR READJCWP            READJUST CLA PAYMENTS WITH PAX NWK           
         SPACE 1                                                                
         OC    TCATASD,TCATASD     IF WE HAVE SESSION DETAILS ELEMENT           
         BNZ   LOOKYES             SKIP - WE'LL HANDLE LATER                    
         SPACE 1                                                                
*        TM    TGUNEQU,AFM         FOR AFM MEMBERS                              
         GOTOR UNITEST,DMCB,TGUNEQUS,AFM,0,0,0                                  
         BZ    *+14                                                             
         MVC   TCAFMSPH,TCGROSS    SAVE SUBJECT TO P&H (SCALE ONLY)             
         OI    TCSTAT,TCSTAFMP     SET HAVE SUBJ TO P&H FOR AFM MEMBER          
         SPACE 1                                                                
         TM    TCINPUT,TCINPAY     IF PAYMENT NOT OVERRIDEN                     
******** BO    LOOKYES                                                          
         SPACE 1                                                                
         CLI   TGUSEQU,UACB        IF USE IS ADDENDUM CABLE                     
         BNE   LOOK9                                                            
         CLC   TCADDST,=C'NW'      AND STATE IS NORTHWEST                       
         BNE   LOOK9                                                            
         TM    TCPAYST,TCMIL+TCMILFR   AND ADDING RATE FOR 1MILLION             
         BNZ   LOOKYES             CALCULATE OVERSCALE LATER                    
         SPACE 1                                                                
LOOK9    MVC   FULL,TCGROSS        SET BASIS FOR OVERSCALE CALCULATION          
         BAS   RE,OVSCALC          CALCULATE OVERSCALE                          
         MVC   TCGROSS,FULL        RESTORE TOTAL TO GROSS                       
         SPACE 1                                                                
LOOKYES  NI    TCSTAT,X'FF'-TCNOSVBK                                            
         B     YES                                                              
         SPACE 1                                                                
LOOKNO   NI    TCSTAT,X'FF'-TCNOSVBK                                            
         B     NO                                                               
         EJECT                                                                  
*              ROUTINE DOES ALL TABLE LOOK-UPS FOR SOAP RESIDUALS.              
*              FINDS GROSS, CALCULATES APPLIED CREDITS, SETS PAYMENT            
*              AMOUNTS AND CALCULATES P&H.                                      
*              RETURNS CC NOT EQUAL IF NO RATES FOUND                           
         SPACE 1                                                                
LOOKSOR  NTR1                                                                   
         TM    TGUSSTA2,NORATES    TEST HAVE RATES                              
         BO    NO                                                               
         BRAS  RE,CABPPV           CABLE OR PPV USE?                            
         BE    LKSOR2                                                           
         BRAS  RE,LOOKUSE          LOOK UP USE TYPE                             
         BNE   NO                  SET CC NOT EQ IF NOT IN RATE TABLE           
LKSOR2   GOTOR GETSORCA            LOOK UP CATEGORY                             
         SPACE 1                                                                
         ZIC   R0,TCNEPIS          R0=NUMBER OF EPISODES                        
         XC    TCEPIAIR,TCEPIAIR                                                
         USING ECASTABD,R2                                                      
         L     R2,TCATSAR                                                       
         LA    R2,ECSTEPIS         R2=A(EPISODE INFO IN TSAR REC)               
         SPACE                                                                  
         USING EPISD,R2                                                         
         USING EPISATBD,R3                                                      
LKSOR3   L     R3,TCAEPISA         R3=A(EPISODE AIR DATE TABLE)                 
         LTR   R3,R3               TEST HAVE A(EPISODE AIR DATE TAB)            
         BZ    LKSOR10                                                          
LKSOR4   CLI   0(R3),X'FF'         TEST END OF TABLE                            
         BNE   *+6                                                              
         DC    H'0'                DIE IF AIR DATE NOT FOUND                    
         CLC   EPINUM,EPATEPI                                                   
         BE    LKSOR5                                                           
         LA    R3,EPATNXT          BUMP TO NEXT AIR DATE ENTRY                  
         B     LKSOR4                                                           
         SPACE                                                                  
LKSOR5   MVC   TCEPIAIR,EPATAIR    SAVE AIR DATE                                
         MVC   TCNCON,EPATNCON     SAVE COUNTS OF PERFORMERS                    
         MVC   TCNU5,EPATNU5                                                    
         MVI   BYTE,0              BYTE = DISPLACEMENT FOR AIR DATE             
         L     R1,TCAIRDLU         R1=A(AIR DATE LOOK UP TABLE)                 
         CLI   TGUSEQU,USOR        FIND RIGHT ONE FOR USE                       
         BE    LKSOR6                                                           
         CLI   TGUSEQU,USRH                                                     
         BE    LKSOR6                                                           
         CLI   TGUSEQU,USOC                                                     
         BE    LKSOR6                                                           
         CLI   TGUSEQU,USON                                                     
         BE    LKSOR6                                                           
         CLI   TGUSEQU,USPP                                                     
         BE    LKSOR6                                                           
         L     R1,TCDAIRDL         USE TABLE FOR DIRECTORS                      
         CLI   TGUSEQU,USDR        IF USE IS SDR OR SDH OR SDC                  
         BE    LKSOR6                                                           
         CLI   TGUSEQU,USDH                                                     
         BE    LKSOR6                                                           
         CLI   TGUSEQU,USDC                                                     
         BE    LKSOR6                                                           
         CLI   TGUSEQU,USDN                                                     
         BE    LKSOR6                                                           
         CLI   TGUSEQU,USDP                                                     
         BE    LKSOR6                                                           
         L     R1,TCWAIRDL         ELSE USE TABLE FOR WRITERS                   
         SPACE                                                                  
         USING AIRDTABD,R1                                                      
LKSOR6   CLI   AIRDSTRT,X'FF'      IF END OF TABLE, USE LATEST RATES            
         BE    LKSOR8                                                           
         MVC   BYTE,AIRDDISP       SAVE LATEST DISPLACEMENT                     
         CLC   AIRDSTRT,TCEPIAIR   FIND AIR DATE RANGE                          
         BH    NO                  IF NO RATES, RETURN CC NO                    
         CLC   AIRDEND,TCEPIAIR                                                 
         BL    LKSOR7                                                           
         B     LKSOR8                                                           
         SPACE                                                                  
LKSOR7   LA    R1,AIRDNEXT                                                      
         B     LKSOR6                                                           
         SPACE                                                                  
LKSOR8   LH    R1,TCUSENSV                                                      
         ZIC   R3,BYTE                                                          
*                                  1/2 HOUR ARE DIFFERENT                       
**NO-OP  CLI   TGUSEQU,USRH                                                     
**JAN05  BNE   LKSOR9J                                                          
**       AHI   R3,-11                                                           
**       B     LKSOR9X                                                          
*                                                                               
**SOR9J  CLI   TGUSEQU,USDH                                                     
**       BE    *+8                                                              
**       CLI   TGUSEQU,USWH                                                     
**       BNE   LKSOR9X                                                          
**NO-OP  AHI   R3,-10                                                           
*                                                                               
LKSOR9X  AR    R1,R3                                                            
         STH   R1,TCUSENUM                                                      
         SPACE                                                                  
LKSOR10  BRAS  RE,GETSRATE         LOOK UP/ACCUMULATE RATE                      
         BRAS  RE,SACRCALC         CALCULATE APPIED CREDITS                     
         BAS   RE,SPNHCALC         GET P&H RATE AND ACCUMULATE AMOUNT           
         SPACE                                                                  
         LA    R2,EPINUMLN(R2)     BUMP TO NEXT EPISODE IN TSAR REC             
         BCT   R0,LKSOR3           LOOP FOR NUMBER OF EPISODES                  
         B     YES                                                              
         EJECT                                                                  
*              ROUTINE SETS P&H RATE AND ACCUMULATES P&H AMOUNT                 
*              FOR SOAP RESIDUALS.  R2=A(EPISODE INFO IN TSAR REC)              
         SPACE                                                                  
         USING EPISD,R2                                                         
SPNHCALC NTR1                                                                   
         TM    TCINPUT,TCINPNH     IF P&H AMOUNT INPUT, DON'T BOTHER            
         BO    SPNHCX                                                           
         TM    TGUSTYST,NOPNH      TEST FOR EXCLUDED USE TYPES                  
         BO    SPNHCX                                                           
*        TM    TGUNEQU,NON         EXCLUDE NON-UNION MEMBER                     
         GOTOR UNITEST,DMCB,TGUNEQUS,NON,0,0,0                                  
         BO    SPNHCX                                                           
         TM    TCSTAT2,TCSTCAN$    EXCLUDE CANADIAN $ PAYMENT                   
         BO    SPNHCX                                                           
         TM    TCSTAT2,TCSTEURO    EXCLUDE EURO PAYMENT                         
         BO    SPNHCX                                                           
         XR    R1,R1                                                            
         ICM   R1,7,EPIPAY         R1=PAYMENT AMOUNT FOR THIS EPISODE           
         CLI   TGUNEQU,AFT         MUST BE AFT UNION                            
         BE    SPNHC20                                                          
         CLI   TGUNEQU,DGA         OR DGA UNION                                 
         BE    SPNHC20                                                          
         CLI   TGUNEQU,WGA         OR WGA UNION                                 
         BNE   SPNHCX                                                           
         SPACE                                                                  
         L     R3,TCWPNHTB         USE TABLE FOR WRITERS IF WGA                 
         OC    TCEPIAIR,TCEPIAIR   TEST HAVE AIR DATE                           
         BZ    SPNHC15                                                          
         SPACE                                                                  
         USING WPNHTABD,R3                                                      
SPNHC5   CLI   WPNHTST,X'FF'       TEST NOT END OF TABLE                        
         BE    SPNHC15                                                          
         CLC   WPNHTST,TCEPIAIR    FIND AIR DATE RANGE                          
         BH    SPNHC10                                                          
         CLC   WPNHTEND,TCEPIAIR                                                
         BNL   SPNHC15                                                          
         SPACE                                                                  
SPNHC10  LA    R3,WPNHTNXT                                                      
         B     SPNHC5                                                           
         SPACE                                                                  
SPNHC15  LH    R0,WPNHTPEN         R0=PENSION RATE                              
         BAS   RE,MULTR0           CALCULATE PENSION                            
         STH   R1,EPIPENS          SAVE PENSION AMOUNT FOR THIS EPISODE         
         ICM   R1,7,EPIPAY         R1=PAYMENT AMOUNT FOR THIS EPISODE           
         LH    R0,WPNHTHLT         R0=HEALTH RATE                               
         BAS   RE,MULTR0           CALCULATE HEALTH                             
         STH   R1,EPIHLTH          SAVE HEALTH AMOUNT FOR THIS EPISODE          
         AH    R1,EPIPENS          ADD PENSION AND HEALTH TOGETHER              
         A     R1,TCPNH            ADD TO RUNNING TOTAL OF P&H                  
         ST    R1,TCPNH                                                         
         B     SPNHCX                                                           
         SPACE                                                                  
         USING PNHTABD,R3                                                       
         SPACE                                                                  
SPNHC20  TM    TCOPTS,TCOPNHR      TEST RATE IS NOT OVERRIDDEN                  
         BO    SPNHC35                                                          
         L     R3,TCPNHTAB         R3=A(AFT P&H AIR DATE TABLE)                 
         CLI   TGUNEQU,AFT         TEST AFT UNION                               
         BE    *+8                                                              
         L     R3,TCDPNHTB         ELSE USE TABLE FOR DGA                       
         SPACE                                                                  
         MVC   TCPNHR,PNHTPNH      INIT RATE TO MOST CURRENT                    
         OC    TCEPIAIR,TCEPIAIR   TEST HAVE AIR DATE                           
         BZ    SPNHC35                                                          
         SPACE                                                                  
SPNHC25  CLI   PNHTSTRT,X'FF'      TEST NOT END OF TABLE                        
         BE    SPNHC35                                                          
         CLC   PNHTSTRT,TCEPIAIR   FIND AIR DATE RANGE                          
         BH    SPNHC30                                                          
         CLC   PNHTEND,TCEPIAIR                                                 
         BL    SPNHC30                                                          
         MVC   TCPNHR,PNHTPNH      SAVE P&H RATE                                
         B     SPNHC35                                                          
         SPACE                                                                  
SPNHC30  LA    R3,PNHTNEXT                                                      
         B     SPNHC25                                                          
         SPACE                                                                  
SPNHC35  LH    R0,TCPNHR           R0=RATE                                      
         BAS   RE,MULTR0           CALCULATE P&H                                
         STH   R1,EPIPNH           SAVE P&H AMOUNT FOR THIS EPISODE             
         A     R1,TCPNH            ADD TO RUNNING TOTAL                         
         ST    R1,TCPNH                                                         
         SPACE 1                                                                
         CLI   TGUNEQU,DGA         IF UNION DGA                                 
         BNE   SPNHCX                                                           
         TM    TCINPUT2,TCINMDED   TEST WE ALREADY HAVE MISC. DED               
         BO    SPNHCX                                                           
         ICM   R1,7,EPIPAY         SET R1=BASIS                                 
         LA    R0,250              SET P&H RATE FOR EMPLOYEE CONTRBTN           
         SPACE 1                                                                
         BAS   RE,MULTR0           CALCULATE MISC. DED                          
         STH   R1,EPIMDED          SAVE MISC. DED FOR THIS EPISODE              
         A     R1,TCMDED           ADD TO RUNNING TOTAL                         
         ST    R1,TCMDED                                                        
SPNHCX   B     XIT                                                              
         EJECT                                                                  
         SPACE 3                                                                
*              UPGRADE ROUTINES - CALCULATE NEW RATES                           
         SPACE 1                                                                
UPGNEWC  NTR1                                                                   
         LA    R2,TGUPTOCD         LOOK UP 'TO' USE TYPE                        
         LA    R3,TGUPTOTY                                                      
         CLI   TGUSEQU,UCAB        CABLE UPGRADES ARE IN TABLES                 
         BNE   *+12                                                             
         LA    R2,TGUPCDE          SO JUST LOOK UP THE UPGRADE USE TYPE         
         LA    R3,TGUPTYP                                                       
         GOTO1 USEVAL,DMCB,(R2),(R3)  LOOK UP USE TYPE                          
         SPACE 1                                                                
         BAS   RE,LOOKUP           DO TABLE LOOK-UPS - GET NEW RATES            
         BNE   UPGERR                                                           
         CLI   TGUSEQU,UACB        ADDENDUM CABLE?                              
         BNE   *+8                                                              
         BAS   RE,ACBCALC          CALCULATE ADDITIONAL AMOUNTS                 
         SPACE 1                                                                
         MVC   TCUPGRS,TCGROSS     SAVE GROSS                                   
         MVC   TCUPUSES,TCTUSES    USES                                         
         MVC   TCUPMAJ,TCMAJORS    MAJORS                                       
         MVC   TCUPUNTS,TCUNITS    UNITS                                        
         MVC   TCUPINS,TCINSRTS    INSERTS                                      
         B     XIT                                                              
         SPACE 3                                                                
*              UPGRADE ROUTINES - CALCULATE ORIGINAL RATES                      
*                                                                               
UPGORIGC NTR1                                                                   
         MVC   TGRSTUSE,TGUSEQU                                                 
         MVC   TGRSTTYP,TGUSTYP                                                 
*                                                                               
         XC    TCGROSS,TCGROSS     CLEAR GROSS                                  
         CLI   TGUSEQU,UCAB        TEST CABLE UPGRADE                           
         BE    UPGO8                                                            
*                                                                               
         LA    R4,TCTAUHEL         R4=A(USAGE HISTORY ELEMENT)                  
         USING TAUHD,R4                                                         
*                                                                               
         CLI   TGUSEQU,UWSP        * WILDSPOTS AND IFB AND CBL AND SCB*         
         BE    UPGO2                                                            
         CLI   TGUSEQU,USWS                                                     
         BE    UPGO2                                                            
         CLI   TGUSEQU,UADW                                                     
         BE    UPGO2                                                            
         CLI   TGUSEQU,UIFB                                                     
         BE    UPGO2D                                                           
         CLI   TGUSEQU,UCBL                                                     
         BE    UPGO2E                                                           
         CLI   TGUSEQU,ULCB                                                     
         BE    UPGO5                                                            
         CLI   TGUSEQU,UACB                                                     
         BE    UPGO5                                                            
         CLI   TGUSEQU,USCB                                                     
         BE    UPGO2E                                                           
         CLI   TGUSEQU,USNW        IF UPGRADING TO SNW                          
         BNE   UPGO4                                                            
         CLI   TGUPTYP,USNWUN      FROM SNT, DON'T HAVE MAJORS OR UNITS         
         BE    UPGO3                                                            
UPGO2    CLI   TAUHMAJ,0           TEST NO MAJORS                               
         BNE   UPGO3                                                            
         OC    TAUHUNT,TAUHUNT     AND NO UNITS                                 
         BNZ   UPGO3                                                            
         B     UPGO2G                                                           
UPGO2D   OC    TAUHINS,TAUHINS     TEST NO INSERTS                              
         BZ    UPGO2G                                                           
         MVC   TCINSRTS,TAUHINS    ELSE SET ORIGINAL N'INSERTS                  
         B     UPGO8                                                            
UPGO2E   CLI   TAUHLCBU,0          IF CABLE IS UPGRADING A LCB CYCLE            
         BE    UPGO2F                                                           
         MVI   TGUSEQU,ULCB        CHANGE USE TYPE TO LCB                       
         MVI   TGUPFRCD,ULCB                                                    
         MVC   TGUSTYP,TAUHLCBU    AND SET USE TYPE                             
         MVC   TGUPFRTY,TAUHLCBU                                                
         GOTOR INIT                                                             
         GOTOR GETYEAR                                                          
         GOTOR LOADTBL                                                          
         B     UPGO5                                                            
UPGO2F   OC    TAUHCBUN,TAUHCBUN   ELSE,TEST NO UNITS FOR CBL                   
         BZ    UPGO2G                                                           
         MVC   TCUNITS,TAUHCBUN    ELSE SET ORIGINAL N'UNITS                    
         B     UPGO8                                                            
UPGO2G   XC    TCGROSS,TCGROSS     CLEAR GROSS - SKIP LOOKUP                    
         B     UPGOX                                                            
*                                                                               
UPGO3    MVC   TCMAJORS,TAUHMAJ    ELSE SET ORIGINAL MAJORS                     
         MVC   TCUNITS,TAUHUNT     AND UNITS                                    
         B     UPGO8                                                            
*                                                                               
UPGO4    CLI   0(R4),TAUHELQ       * CANADIAN USES *                            
         BNE   UPGO8                                                            
         MVC   TCUNITS,TAUHUNT     SET ORIGINAL UNITS                           
         MVC   TCNUSES,TAUHUSN     AND USES                                     
         B     UPGO8                                                            
*                                                                               
UPGO5    DS    0H                                                               
*PGO5    MVC   TCMAJORS,TAUHLCBM   SET ORIGINAL MAJORS                          
*                                                                               
UPGO8    GOTO1 USEVAL,DMCB,TGUPFRCD,TGUPFRTY  LOOK UP 'FROM' USE TYPE           
*                                                                               
         OI    TCSTAT,TCNOSVBK     DON'T SAVE BREAKDOWN DATA                    
         BAS   RE,LOOKUP           DO TABLE LOOK-UPS - GET OLD RATES            
         BNE   UPGERR                                                           
*                                                                               
         CLI   TGUSEQU,UACB        ADDENDUM CABLE?                              
         BNE   *+8                                                              
         BAS   RE,ACBCALCU                                                      
*                                                                               
         CLI   TGRSTUSE,UCBL                                                    
         BE    *+12                                                             
         CLI   TGRSTUSE,USCB                                                    
         BNE   UPGOX                                                            
         CLI   TAUHLCBU,0                                                       
         BE    UPGOX                                                            
         MVC   TGUSEQU,TGRSTUSE                                                 
         MVC   TGUSTYP,TGRSTTYP                                                 
         GOTO1 USEVAL,DMCB,TGUSEQU,TGUSTYP                                      
         GOTOR INIT                                                             
         GOTOR GETYEAR                                                          
         GOTOR LOADTBL                                                          
UPGOX    DS    0H                                                               
         MVI   TGBYTE,X'00'                                                     
         OC    TCGROSS,TCGROSS     THERE SHOULD BE AN AMOUNT                    
         BZ    XIT                                                              
         L     R1,TCGROSS                                                       
         LCR   RF,R1                                                            
         STCM  RF,15,WORK                                                       
*                                                                               
* SAVED LATER, AROUND MAIN18                                                    
*        GOTOR SVBRKDWN,DMCB,('SBDADJST',WORK),(2,0)                            
         B     XIT                                                              
*                                                                               
UPGERR   GOTO1 USEVAL,DMCB,TGUPCDE,TGUPTYP  LOOK UP UPGRADE USE ENTRY           
         B     ERR3                         TO RESET USE INFO                   
*                                                                               
RESETUSE DC    X'0'                                                             
RESETTYP DC    X'0'                                                             
         EJECT                                                                  
*              ROUTINE TO ESTABLISH CAST CATEGORY                               
         SPACE 1                                                                
GETCAT   NTR1                                                                   
         CLI   TGCAEQU,CTPIL       IF THIS IS A PILOT W/O HLDS                  
         BE    *+12                                                             
         CLI   TGCAEQU,CTPI        OR A PILOT WITH HLDS                         
         BNE   GETC1D                                                           
         TM    TGUSSTAT,SESSION    AND WE'RE NOT PAYING A SESSION               
         BO    GETC1D                                                           
         CLI   TGUSEQU,UPPF        OR A POSTPONEMENT FEE                        
         BE    GETC1D                                                           
         MVI   TCROW,1             PAY PRINCIPAL RATES                          
         B     GETCX                                                            
         SPACE 1                                                                
GETC1D   L     R2,TCAFMCOL         DETERMINE WHICH TABLE TO USE                 
*        TM    TGUSXUNI,AFM                                                     
         GOTOR UNITEST,DMCB,TGUSXUNS,AFM,0,0,0                                  
         BZ    GETC2               MUSICIAN                                     
         SPACE 1                                                                
*                                                                               
         L     R2,TCDIOCOL                                                      
         CLI   TGUSEQU,UDIO        TEST IF  UDIOCOLS NEEDED                     
         BE    GETC2                                                            
         CLI   TGUSEQU,UIVR        TEST IF  UDIOCOLS NEEDED                     
         BE    GETC2                                                            
*                                                                               
         L     R2,TCRTKCOL                                                      
         CLI   TGUSEQU,URTK        TEST IF  URTKCOLS NEEDED                     
         BE    GETC2                                                            
*                                                                               
         L     R2,TCRADCOL         RADIO                                        
         TM    TGMEEQU,RADIO                                                    
         BO    GETC2                                                            
         CLI   TGUSEQU,UBSC        CANADIAN SESSION?                            
         BE    GETC1D5                                                          
         CLI   TGUSEQU,UCNM        CANADIAN NEW MEDIA?                          
         BE    GETC1D5                                                          
         CLI   TGUSEQU,UNMC        NEW MEDIA CANADIAN?                          
         BNE   GETC1E                                                           
GETC1D5  CLI   TGMEEQU,NEWMEDIA    IF NEW MEDIA,                                
         BNE   GETC1E                                                           
         USING TACOD,RE                                                         
         L     RE,TCATACO                                                       
         CLI   TACOCTYP,CCTYADO    AND ACTRA TYPE AUDIO                         
         BE    GETC2               USE RADIO TABLE                              
         SPACE 1                                                                
GETC1E   L     R2,TCONCOL          ON-CAMERA                                    
         CLC   TCCAONOF(2),=C'ON'                                               
         BE    GETC2                                                            
         L     R2,TCOFFCOL         OFF-CAMERA                                   
         SPACE 1                                                                
GETC2    CLI   0(R2),X'FF'                                                      
         BE    ERR4                                                             
         CLC   TGCAEQU,1(R2)       MATCH ON CATEGORY EQUATE                     
         BE    GETC3                                                            
         CLI   TGUSEQU,UIVR        UIVR USES FIRST ENTRY IN TABLE               
         BE    ERR4                                                             
         LA    R2,2(R2)            L'TABLES SHOULD ALL BE THE SAME              
         B     GETC2                                                            
*                                                                               
GETC3    MVC   TCROW,0(R2)         MOVE IN ROW NO.                              
         CLI   TCROW,2                                                          
         BH    *+8                                                              
         OI    TCSTAT,TCSTPRIN     IF ROW <= 2, SET IS PRINCIPAL PERF           
*                                                                               
         L     R2,TCATACO          R2=A(COMM'L DETAILS EL)                      
         CLI   TGUSEQU,UDEM                                                     
         BE    GETC4                                                            
         CLI   TGUSEQU,USNA                                                     
         BNE   GETC5                                                            
GETC4    BAS   RE,ADJDEM           DEMO USE TYPE MAY NEED ADJUSTMENT            
         B     GETC10                                                           
*                                                                               
GETC5    CLI   TGUSEQU,UINS                                                     
         BNE   GETC5B                                                           
         BAS   RE,ADJINS           INDUSTRIAL MAY NEED ADJUSTMENT               
         B     GETC10                                                           
*                                                                               
GETC5B   CLI   TGUSEQU,UIDS                                                     
         BNE   GETC6                                                            
         BRAS  RE,ADJIDS           INDUSTRIAL MAY NEED ADJUSTMENT               
         B     GETC10                                                           
*                                                                               
GETC6    CLI   TGUSEQU,URTK                                                     
         BNE   *+12                                                             
         BRAS  RE,ADJRTK           ADJUST INDUSTRIAL RETAKE                     
         B     GETC10                                                           
*                                                                               
         CLI   TGUSEQU,UDIO                                                     
         BNE   *+12                                                             
         BRAS  RE,ADJDIO           ADJUST INDUSTRIAL AUDIO                      
         B     GETC10                                                           
*                                                                               
         CLI   TGUSEQU,UIVR                                                     
         BNE   *+12                                                             
         BRAS  RE,ADJIVR           ADJUST INTERACTIVE VOICE                     
         B     GETC10                                                           
*                                                                               
         CLI   TGUSEQU,UNMC                                                     
         BNE   GETC8                                                            
         L     RE,TCATACO                                                       
         CLI   TACOCTYP,CCTYVDO    IF COMMERCIAL IS ACTRA VIDEO,                
         BNE   GETC6B                                                           
GETC6B   CLI   TACOCTYP,CCTYADO    IF COMMERCIAL IS ACTRA AUDIO,                
         BNE   GETC10                                                           
         CLI   TGCAEQU,CTSV        OR A PILOT WITH HLDS                         
         BNE   GETC10                                                           
         MVI   TCROW,1                                                          
         B     GETC10                                                           
         CLI   TGCAEQU,CTMV                                                     
         BNE   GETC10                                                           
         MVI   TCROW,2                                                          
         B     GETC10                                                           
*                                                                               
GETC8    CLI   TGUSEQU,UPRM                                                     
         BE    *+12                                                             
         CLI   TGUSEQU,UPRR                                                     
         BNE   *+12                                                             
         BAS   RE,ADJPROMO         PROMO USES MAY NEED ADJUSTMENT               
         B     GETC10                                                           
*                                                                               
         TM    TGUSSTA3,ADDENUSE   IF ADDENDUM USE                              
         BZ    *+12                                                             
         BAS   RE,ADJADD           MAY NEED ADJUSTMENT                          
         B     GETC10                                                           
*                                                                               
         TM    TGMEEQU,RADIO       FOR MEDIA RADIO                              
         BZ    GETC10                                                           
         BRAS  RE,ADJRAD           MAKE ADJUSTMENTS                             
*                                                                               
*ETC10   TM    TGUSXUNI,AFM        TEST MUSICIAN                                
GETC10   GOTOR UNITEST,DMCB,TGUSXUNS,AFM,0,0,0                                  
         BO    GETCX                                                            
         GOTOR ADJMUS              ADJUST TABLE POINTERS                        
GETCX    B     XIT                                                              
         EJECT                                                                  
*              ADJUSTMENT ROUTINE FOR INDUSTRIALS (ON CAMERA)                   
         SPACE 1                                                                
ADJINS   DS    0H                                                               
*                                                                               
         CLI   TGCAEQU,CTSD        SOLO DANCER = ROW 6                          
         BNE   ADJINS1                                                          
         MVI   TCROW,6                                                          
         B     ADJINSX                                                          
*                                                                               
ADJINS1  CLI   TGCAEQU,CTGD        GROUP DANCER = ROW 9                         
         BE    ADJINS1R                                                         
         CLI   TGCAEQU,CTGD3                                                    
         BL    ADJINS2                                                          
         CLI   TGCAEQU,CTGD9                                                    
         BH    ADJINS2                                                          
ADJINS1R MVI   TCROW,9                                                          
         B     ADJINSX                                                          
*                                                                               
ADJINS2  CLI   TGCAEQU,CTG3        GROUP SINGER = ROW 12                        
         BL    ADJINS3                                                          
         CLI   TGCAEQU,CTG9                                                     
         BH    ADJINS3                                                          
         B     ADJINS10                                                         
*                                                                               
ADJINS3  CLI   TGCAEQU,CTG3M       GROUP SINGER = ROW 12                        
         BL    ADJINS5                                                          
         CLI   TGCAEQU,CTG9M                                                    
         BH    ADJINS5                                                          
         B     ADJINS10                                                         
*                                                                               
ADJINS5  CLI   TGCAEQU,CTGS3       GROUP SINGERS = ROW 12                       
         BL    ADJINS7                                                          
         CLI   TGCAEQU,CTGS9                                                    
         BH    ADJINS7                                                          
         B     ADJINS10                                                         
*                                                                               
ADJINS7  CLI   TGCAEQU,CTC3        GROUP CONTRACTOR = ROW 12                    
         BL    ADJINS8                                                          
         CLI   TGCAEQU,CTC9                                                     
         BH    ADJINS8                                                          
         B     ADJINS10                                                         
*                                                                               
ADJINS8  CLI   TGCAEQU,CTEX        EXTRA SHOULD NOT GET A RATE                  
         BNE   ADJINS9                                                          
         MVI   TCROW,60            NO RATE                                      
         B     ADJINSX                                                          
*                                                                               
ADJINS9  B     ADJINSX                                                          
*                                  FOR MEDIA TV                                 
ADJINS10 MVI   TCROW,12            ROW SHOULD BE 18 (NOT IN ON/OFFCOLS)         
ADJINSX  BR    RE                                                               
         SPACE 3                                                                
*              ADJUSTMENT ROUTINE FOR INDUSTRIAL RETAKES                        
         SPACE 1                                                                
ADJRTK   NTR1                                                                   
*                                                                               
         CLI   TGUSEQU,URTK        VERIFY URTK                                  
         BNE   ADJRTKX                                                          
*                                                                               
         CLC   TCCAONOF(2),=C'ON' IF ON CAMERA                                  
         BNE   ADJRTK10                                                         
*                                                                               
******   BRAS  RE,ADJINS           ADJUST                                       
         B     ADJRTKX                                                          
*                                                                               
ADJRTK10 DS    0H                                                               
*                                                                               
*******  BRAS  RE,ADJIDS           ADJUST                                       
*                                                                               
ADJRTKX  DS    0H                                                               
         XIT1                                                                   
         SPACE 3                                                                
*              ADJUSTMENT ROUTINE FOR INDUSTRIAL AUDIO                          
         SPACE 1                                                                
ADJDIO   NTR1                                                                   
*                                                                               
         CLI   TGUSEQU,UDIO        VERIFY UDIO                                  
         BNE   ADJDIOX                                                          
*                                                                               
ADJDIOX  DS    0H                                                               
         XIT1                                                                   
*              ADJUSTMENT ROUTINE FOR INTERACTIVE VOICE                         
         SPACE 1                                                                
ADJIVR   NTR1                                                                   
*                                                                               
         CLI   TGUSEQU,UIVR        VERIFY UIVR                                  
         BNE   ADJIVRX                                                          
*                                                                               
ADJIVRX  DS    0H                                                               
         XIT1                                                                   
         SPACE 3                                                                
*              ADJUSTMENT ROUTINE FOR DEMOS                                     
         SPACE 1                                                                
ADJDEM   DS    0H                                                               
         CLI   TGCAEQU,CTSS1       SWEETENING TRACKS 1-4                        
         BL    ADJD3                                                            
         CLI   TGCAEQU,CTSS4                                                    
         BH    ADJD3                                                            
         TM    TGMEEQU,LIKETV                                                   
         BZ    ADJD8               RADIO, SHOULD BE SAME RATE AS SOLO           
         B     ADJD10                                                           
*                                                                               
ADJD3    CLI   TGCAEQU,CTS         SOLOS                                        
         BE    ADJD5                                                            
         CLI   TGCAEQU,CTSM                                                     
         BE    ADJD5                                                            
         CLI   TGCAEQU,CTD         AND DUOS ONLY                                
         BNE   ADEMX                                                            
ADJD5    TM    TGMEEQU,RADIO       FOR MEDIA RADIO                              
         BZ    ADJD10                                                           
ADJD8    MVI   TCROW,9             ROW SHOULD BE 9 (NOT IN RADCOLS)             
         B     ADEMX                                                            
*                                  FOR MEDIA TV                                 
ADJD10   CLC   TCCAONOF,=C'OFF'    AND OFF-CAMERA                               
         BNE   ADEMX                                                            
         MVI   TCROW,18            ROW SHOULD BE 18 (NOT IN ON/OFFCOLS)         
ADEMX    BR    RE                                                               
         SPACE 3                                                                
*              ADJUSTMENT ROUTINE FOR ADDENDUMS                                 
         SPACE 1                                                                
ADJADD   DS    0H                                                               
         CLC   TCADDST,=C'KS'      IF KS, NO ADJUSTMENTS                        
         BE    ADJAX                                                            
         CLC   TCADDST,=C'GA'      IF GA                                        
         BNE   ADJA2                                                            
         TM    TGCATYPE,EXTRA      TEST FOR EXTRAS                              
         BZ    ADJA2                                                            
*DJA1    TM    TGUNEQU,AFT         IF UNION NOT AFTRA                           
ADJA1    LR    R0,RE                                                            
         GOTOR UNITEST,DMCB,TGUNEQUS,AFT,0,0,0                                  
         LR    RE,R0                                                            
         BO    ADJAX                                                            
         MVI   TCROW,60            SET NO RATES                                 
         CLI   TGCAEQU,CTEXB       EXTRA BUYOUT                                 
         BNE   *+8                                                              
         MVI   TCROW,9                                                          
         CLI   TGCAEQU,CTHMB       HAND MODEL BUYOUT                            
         BNE   *+8                                                              
         MVI   TCROW,10                                                         
         B     ADJAX               DON'T BOTHER ADJUSTING ROWS FOR KS           
         SPACE                                                                  
ADJA2    CLI   TGCAEQU,CTS         SOLOS                                        
         BE    ADJA3                                                            
         CLI   TGCAEQU,CTSM                                                     
         BE    ADJA3                                                            
         CLI   TGCAEQU,CTD         AND DUOS ONLY                                
         BNE   ADJAX                                                            
ADJA3    CLC   TCADDST,=C'GA'      IF GA, BRANCH TO ADJA9                       
         BE    ADJA9                                                            
         CLC   TCADDST,=C'TX'      IF TX                                        
         BNE   ADJAX                                                            
         CLI   TGUSEQU,UADD        ONLY DO IF DEMO                              
         BNE   ADJAX                                                            
         B     ADJA11                                                           
         SPACE                                                                  
ADJA9    DS    0H                                                               
*DJA9    CLI   TGUSEQU,UADD        FOR GA, DON'T BOTHER IF DEMO                 
*        BE    ADJAX                                                            
ADJA10   TM    TGUSXCAT,EXTRA+NOREUSE IF EXTRA AND NOREUSE NOT EXCLUDED         
         BO    ADJA15                                                           
ADJA11   TM    TGMEEQU,RADIO       FOR MEDIA RADIO                              
         BZ    *+12                                                             
         MVI   TCROW,9             ROW SHOULD BE 9 (NOT IN RADCOLS)             
         B     ADJAX                                                            
*                                  FOR MEDIA TV AND ON CAMERA                   
         MVI   TCROW,18            ROW SHOULD BE 18 (NOT IN ON/OFFCOLS)         
         CLC   TCCAONOF,=C'OFF'    FOR OFF CAMERA                               
         BNE   *+8                                                              
         MVI   TCROW,19            ROW SHOULD BE 19 (NOT IN ON/OFFCOLS)         
         B     ADJAX                                                            
         SPACE                                                                  
*                                  EXTRA AND NOREUSE EXCLUDED                   
ADJA15   TM    TGMEEQU,RADIO       FOR MEDIA RADIO                              
         BZ    *+12                                                             
         MVI   TCROW,7             ROW SHOULD BE 7 (FOR EXCLUDED CAT)           
         B     ADJAX                                                            
*                                  FOR MEDIA TV AND ON CAMERA                   
         MVI   TCROW,9             ROW SHOULD BE 9 (FOR EXCLUDED CAT)           
         CLC   TCCAONOF,=C'OFF'    FOR OFF CAMERA                               
         BNE   *+8                                                              
         MVI   TCROW,10            ROW SHOULD BE 10                             
ADJAX    BR    RE                                                               
         SPACE 3                                                                
*              ADJUSTMENT ROUTINE FOR PROMOS                                    
         SPACE 1                                                                
ADJPROMO DS    0H                                                               
         CLI   TGCAEQU,CTS         SOLOS                                        
         BE    ADJP5                                                            
         CLI   TGCAEQU,CTSM                                                     
         BE    ADJP5                                                            
         CLI   TGCAEQU,CTD         AND DUOS ONLY                                
         BE    ADJP5                                                            
*        TM    TGUNEQU,SAG         IF UNION IS SAG                              
         LR    R0,RE                                                            
         GOTOR UNITEST,DMCB,TGUNEQUS,SAG,0,0,0                                  
         LR    RE,R0                                                            
         BZ    ADJPX                                                            
         TM    TGCATYPE,EXTRA      TEST FOR EXTRAS                              
         BZ    ADJPX                                                            
         MVI   TCROW,14                                                         
         B     ADJPX                                                            
*                                  FOR MEDIA TV                                 
ADJP5    CLC   TCCAONOF,=C'OFF'    AND OFF-CAMERA                               
         BNE   ADJPX                                                            
         MVI   TCROW,18            ROW SHOULD BE 18 (NOT IN ON/OFFCOLS)         
ADJPX    BR    RE                                                               
         EJECT                                                                  
*                                                                               
*              ROUTINE TO ADJUST BASE SESSIONS                                  
         SPACE 1                                                                
ADJBSS   NTR1                                                                   
         TM    TGUSSTAT,SESSION    TEST THIS IS A SESSION                       
         BZ    ABSSX                                                            
         TM    TGMEEQU,CABLE       CABLE SESSIONS                               
         BZ    ABSSX                                                            
         TM    TCSTAT2,TCSTCGT1    IF GREATER THAN 1 COMM. PROD. TODAY          
         BZ    ABSSX                                                            
         TM    TCSTAT2,TCSTC1ST    AND THIS IS NOT THE FIRST ONE                
         BO    ABSSX                                                            
         L     R1,TCGROSS                                                       
         BAS   RE,ONETHIRD         THEN GROSS IS 1/3 TABLE RATE                 
         ST    R1,TCGROSS                                                       
ABSSX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              ROUTINE TO ADJUST CANADIAN NEW MEDIA - AUDIO ONLY                
         SPACE 1                                                                
ADJCNM   NTR1                                                                   
         OC    TCUNITS,TCUNITS     ANY ADDITIONAL CUTS?                         
         BZ    ACNMX                                                            
         CLC   TCUNITS,=H'2'       MORE THAN 2 CUTS?                            
         BNH   ACNMX                                                            
         L     R3,TCGROSS          SAVE RATE                                    
         XC    TCGROSS,TCGROSS                                                  
         LH    R1,TCUNITS                                                       
         AHI   R1,-2               DEDUCT 2 CUTS                                
         BNP   ACNMX                                                            
         XC    TCUNITS,TCUNITS     CLEAR DUMMY UNITS                            
         BRAS  RE,SETNMUSE         SET NEW MEDIA USENUM                         
         BRAS  RE,GETRATE          ADD IN FEE FOR ADDITIONAL CUTS               
         L     RE,TCGROSS                                                       
         STH   R1,HALF                                                          
         MH    RE,HALF             GROSS X NUM OF ADDTL CUTS                    
         AR    RE,R3               ADD RATE TO ADDTL CUT RATE                   
         ST    RE,TCGROSS                                                       
ACNMX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CALCULATE OVERSCALE                                   
         SPACE 1                                                                
*                                  FULL=BASIS FOR CALCULATION                   
OVSCALC  NTR1                                                                   
         TM    TGUSSTAT,NOOVSCAL   TEST NO OVERSCALE FOR THIS USE               
         BO    OVSCX                                                            
*                                                                               
         L     R2,FULL             R2=(BASIS FOR CALCULATION)                   
*                                                                               
         L     R0,TCOV1            R1=OVERSCALE PERCENTAGE                      
         TM    TCOV1,X'80'         PERCENT SCALE                                
         BZ    *+8                                                              
         N     R0,=X'7FFFFFFF'     STRIP OFF HOB                                
         L     R1,FULL                                                          
         BAS   RE,MULTR0           MULTIPLY BASIS * OVERSCALE RATE              
         TM    TCOV1,X'80'         % SCALE DOESN'T ADD BACK TO BASIS            
         BO    *+8                                                              
         A     R1,FULL             ADD BACK TO BASIS                            
         ST    R1,FULL                                                          
*                                                                               
         L     R0,TCOV2            MOVE IN 2ND OVERSCALE PERCENTAGE             
         BAS   RE,MULTR0           MULTIPLY BASIS * OVERSCALE RATE              
         A     R1,FULL             ADD BACK TO BASIS                            
         LR    R3,R1                                                            
*                                                                               
         SR    R1,R2                                                            
         ST    R1,FULL                                                          
         LTR   R1,R1               OVERSCALE=0                                  
         BZ    OVSC5                                                            
         ST    R1,TCOVCAMT         SAVE IT AROUND                               
         GOTOR SVBRKDWN,DMCB,('SBDADJST',FULL),(1,0)                            
*                                                                               
OVSC5    ST    R3,FULL                                                          
OVSCX    NI    TCSTAT,X'FF'-TCNOSVBK                                            
         B     XIT                                                              
         EJECT                                                                  
*              ADD ANY ADDITIONAL AMOUNTS FOR ADDENDUM CABLE                    
         SPACE 1                                                                
ACBCALC  NTR1                                                                   
         CLC   TCADDST,=C'NW'      IF STATE IS NOT NORTHWEST, EXIT              
         BNE   ACBCX                                                            
         TM    TCPAYST,TCMIL       ADDING THE RATE FOR 1 MILLION?               
         BNO   ACBCX                                                            
         ZIC   R0,TGUSTYP          SAVE USE TYPE                                
         MVI   TGUSTYP,UACB1M      GET RATE FOR 1 MILLION                       
         BAS   RE,LOOKUP           ADD THIS RATE TO PREVIOUS RATE               
         STC   R0,TGUSTYP          RESTORE USE TYPE                             
         SPACE 1                                                                
         MVC   FULL,TCGROSS        SET BASIS FOR OVERSCALE CALCULATION          
         BAS   RE,OVSCALC          CALCULATE OVERSCALE                          
         MVC   TCGROSS,FULL        RESTORE TOTAL TO GROSS                       
         SPACE 1                                                                
ACBCX    B     XIT                                                              
         EJECT                                                                  
*              ADD ADDITIONAL AMOUNTS FOR ADDENDUM CABLE UPGRADES               
         SPACE 1                                                                
ACBCALCU NTR1                                                                   
         CLC   TCADDST,=C'NW'      IF STATE IS NOT NORTHWEST, EXIT              
         BNE   ACBCUX                                                           
         TM    TCPAYST,TCMILFR     ADDING THE RATE FOR 1 MILLION?               
         BNO   ACBCUX                                                           
         ZIC   R0,TGUSTYP          SAVE USE TYPE                                
         MVI   TGUSTYP,UACB1M      GET RATE FOR 1 MILLION                       
         BAS   RE,LOOKUP           ADD THIS RATE TO PREVIOUS RATE               
         STC   R0,TGUSTYP          RESTORE USE TYPE                             
         SPACE 1                                                                
         MVC   FULL,TCGROSS        SET BASIS FOR OVERSCALE CALCULATION          
         BAS   RE,OVSCALC          CALCULATE OVERSCALE                          
         MVC   TCGROSS,FULL        RESTORE TOTAL TO GROSS                       
ACBCUX   B     XIT                                                              
*              GENERATE OVERSCALE/P&H/I&R/H&W FOR SESSIONS                      
         SPACE 1                                                                
         USING TASDD,R2                                                         
BSSCALC  NTR1                                                                   
         ICM   R2,15,TCATASD       R2=A(SESSION DETAILS ELEMENT)                
         BZ    XIT                                                              
         SPACE 1                                                                
         MVC   TASDFEE,TCGROSS     SAVE SESSION FEE                             
         XC    TCGROSS,TCGROSS     CLEAR GROSS PAYMENT AMOUNT                   
         SPACE 1                                                                
         TM    TCINPUT,TCINPAY     IF PAYMENT IS OVERRIDDEN                     
         BZ    BSSC020                                                          
         BAS   RE,LOOKUP           GET NORMAL FEE                               
         MVC   TASDFEE,TCGROSS     AND SAVE IN ELEMENT                          
         MVC   TCGROSS,TCPAY       RESTORE PAYMENT AMOUNT TO GROSS              
         BNE   BSSC120             IF NO RATES-SKIP SETTING HOURLY RATE         
         B     BSSC040             SKIP GETTING BSS RATE FOR DEM OT&DT          
         SPACE 1                                                                
BSSC020  CLI   TASDEQU,UDEM                   IF DEM USE                        
         BE    BSSC030                                                          
         CLI   TASDEQU,USNA                                                     
         BNE   BSSC040                                                          
BSSC030  OC    TASDOT(TASDTAG-TASDOT),TASDOT  AND HAVE OT,DT,TRV OR PDW         
         BZ    BSSC080                                                          
         GOTO1 USEVAL,DMCB,(0,=C'BSS'),=X'00'                                   
         BAS   RE,LOOKUP           GET RATES FOR BSS                            
         BNE   ERR3                                                             
         GOTO1 USEVAL,DMCB,(0,=C'DEM'),=X'00'  RESET USE INFO FOR DEM           
         L     R0,TCGROSS          USE BSS RATE TO CALC HOURLY RATE             
         XC    TCGROSS,TCGROSS                                                  
         B     BSSC100                                                          
         SPACE 1                                                                
BSSC040  CLI   TASDEQU,UBSM        IF MUSIC                                     
         BNE   BSSC050                                                          
         LA    R3,BSMTAB                                                        
         MVC   TASDHR,TASDFEE      HOURLY RATE = GROSS                          
         B     BSSC160                                                          
*                                                                               
BSSC050  CLI   TASDEQU,UIMS        IF MUSIC                                     
         BNE   BSSC060                                                          
         LA    R3,IMSTAB                                                        
         MVC   TASDHR,TASDFEE      HOURLY RATE = GROSS                          
         B     BSSC160                                                          
*                                                                               
BSSC060  CLI   TASDEQU,UIDS        IF INDUSTRIAL OFF CAMERA SESSION             
         BNE   BSSC070                                                          
         LA    R3,IDSTAB                                                        
         MVC   TASDHR,TASDFEE      HOURLY RATE = GROSS                          
         B     BSSC160                                                          
*                                                                               
BSSC070  CLI   TASDEQU,URTK        IF INDUSTRIAL RETAKES                        
         BNE   BSSC071                                                          
         LA    R3,RTKTAB                                                        
         MVC   TASDHR,TASDFEE      HOURLY RATE = GROSS                          
         B     BSSC160                                                          
*                                                                               
BSSC071  CLI   TASDEQU,UDIO        IF INDUSTRIAL AUDIO SESSIONS                 
         BNE   BSSC072                                                          
         LA    R3,DIOTAB                                                        
         MVC   TASDHR,TASDFEE      HOURLY RATE = GROSS                          
         B     BSSC160                                                          
*                                                                               
BSSC072  DS    0H                                                               
*                                                                               
         CLI   TASDEQU,UIVR        IF INTERACTIVE VOICE SESSIONS                
         BNE   BSSC073                                                          
         LA    R3,IVRTAB                                                        
         MVC   TASDHR,TASDFEE      HOURLY RATE = GROSS                          
         B     BSSC160                                                          
*                                                                               
BSSC073  DS    0H                                                               
*                                                                               
BSSC080  L     R0,TASDFEE                                                       
BSSC100  XR    R1,R1               HOURLY RATE = GROSS DIVIDED BY 8             
         SRDA  R0,31                                                            
         CLI   TGUSEQU,UCSS        CSS USE HAS 10 HOUR WORK DAY                 
         BE    *+12                                                             
         D     R0,=F'8'            ALL OTHER USES 8 HOUR WORK DAY               
         B     *+8                                                              
         D     R0,=F'10'                                                        
         AHI   R1,1                AND ROUNDED                                  
         SRA   R1,1                                                             
         ST    R1,TASDHR           SET HOURLY RATE                              
*                                                                               
BSSC120  LA    R3,BSSTAB           SET R3=A(ENTRY TABLE)                        
         CLI   TASDEQU,UADO        IF RADIO SESSION                             
         BE    BSSC140                                                          
         CLI   TASDEQU,UBSR                                                     
         BE    BSSC140                                                          
         CLI   TASDEQU,UARR                                                     
         BE    BSSC140                                                          
         CLI   TASDEQU,URRR        OR RADIO RERECORD                            
         BNE   *+8                                                              
BSSC140  LA    R3,BSRTAB           RADIO                                        
*                                                                               
         CLI   TASDEQU,UBSC                                                     
         BNE   BSSC160                                                          
         USING TACOD,RE                                                         
         L     RE,TCATACO                                                       
         CLI   TACOCTYP,CCTY04B    IF COMM IS ACTRA 2404B, USE BSSTAB           
         BE    *+8                                                              
         LA    R3,BSCTAB           CANADA                                       
         DROP  RE                                                               
*                                                                               
BSSC160  CLI   0(R3),X'FF'                                                      
         BE    BSSCX                                                            
         LH    RF,0(R3)            DISPLACMENT TO HANDLING ROUTINE              
         AR    RF,RB                                                            
         LA    RE,BSSC180          SET RE FOR COMMON NTR1                       
         NTR1                                                                   
         BR    RF                  ** OFF TO ROUTINE **                         
         SPACE 1                                                                
BSSC180  LA    R3,L'BSSTAB(R3)     BUMP TO NEXT TABLE ENTRY                     
         B     BSSC160                                                          
         SPACE 1                                                                
*SSCX    TM    TGUNEQU,AFM         IF AFM MEMBER                                
BSSCX    GOTOR UNITEST,DMCB,TGUNEQUS,AFM,0,0,0                                  
         BZ    XIT                                                              
         OC    TCAFMSPH,TCAFMSPH   AND HAVE AMOUNT SUBJECT TO P&H               
         BZ    *+8                                                              
         OI    TCSTAT,TCSTAFMP     SET HAVE SUBJ TO P&H FOR MUSICIANS           
         B     XIT                                                              
         EJECT                                                                  
*=====================================================================          
*              TELEVISION SESSION CALCULATION SUB-ROUTINES                      
*=====================================================================          
         USING TASDD,R2            R2=A(SESSION DETAILS ELEMENT)                
BSSSP    DS    0H                  N'SPOTS / N'DAYS                             
*                                                                               
         ZIC   R1,TASDSP                                                        
         ST    R1,WORK+4                                                        
         ZIC   RF,TASDDAY                                                       
         ST    RF,WORK+8                                                        
         LA    RF,TASDSPA                                                       
         MVI   BYTE,51                                                          
         CLC   TASDSP,TASDDAY      TAKE HIGHER OF SPOTS & DAYS                  
         BNL   BSSSP020                                                         
         ZIC   R1,TASDDAY                                                       
         ST    R1,WORK+4                                                        
         ZIC   RF,TASDSP                                                        
         ST    RF,WORK+8                                                        
         MVI   BYTE,52                                                          
         LA    RF,TASDDAA                                                       
*                                                                               
BSSSP020 DS    0H                                                               
*                                                                               
         CLI   TGUSEQU,UBSC        IF BSC PAYMENT                               
         BNE   BSSSP050                                                         
*                                                                               
         USING TACOD,RE                                                         
         L     RE,TCATACO                                                       
         CLI   TACOCTYP,CCTYADO    IF COMMERCIAL IS ACTRA AUDIO,                
         BNE   BSSSP028                                                         
         DROP  RE                                                               
         XC    TCGROSS,TCGROSS                                                  
         L     R1,TASDFEE                                                       
         ZIC   R0,TASDSP                                                        
         AHI   R0,-2               DEDUCT 2 CUTS                                
         BNP   BSSSP025                                                         
         MVC   TCUSENUM,=H'144'    SET USE FOR ADDTL CUT RATE                   
         BRAS  RE,GETRATE                                                       
         L     RE,TCGROSS                                                       
         STH   R0,HALF                                                          
         MH    RE,HALF             GROSS X NUM OF ADDTL CUTS                    
         A     RE,TASDFEE          ADD RATE TO ADDTL CUT RATE                   
         ST    RE,TASDFEE                                                       
         LR    R1,RE                                                            
BSSSP025 LR    R5,R1                                                            
         XC    WORK+4(4),WORK+4                                                 
         MVI   WORK+7,1                                                         
         MVC   WORK(4),TASDFEE                                                  
         GOTOR SVBRKDWN,DMCB,(0,WORK),(BYTE,WORK+4)                             
         LR    R1,R5                                                            
         XC    TCGROSS,TCGROSS                                                  
         B     BSSALL                                                           
*                                                                               
         USING TACOD,RE                                                         
BSSSP028 L     RE,TCATACO                                                       
         CLI   TACOCTYP,CCTY04B    IF COMMERCIAL IS ACTRA 2404B                 
         BE    BSSSP050            TREAT AS BSS/SAG PAYMENT                     
         DROP  RE                                                               
*                                                                               
         OC    TCATMTOT,TCATMTOT   AND TIMESHEET?                               
         BZ    BSSSP030                                                         
         LA    RE,TASDSPA                                                       
         CR    RF,RE               SPOTS OR DAYS?                               
         BNE   *+12                                                             
         BRAS  RE,TIMESPTA         CALCULATE ACTRA TIMESHEET SPOTS              
         B     *+8                                                              
         BRAS  RE,TIMEDAYA         CALCULATE ACTRA TIMESHEET DAYS               
*                                                                               
         USING TACOD,RE                                                         
BSSSP030 L     RE,TCATACO                                                       
         CLI   TACOCTYP,CCTY04A    IF COMMERCIAL IS ACTRA 2404A                 
         BNE   BSSSP100                                                         
         DROP  RE                                                               
*                                                                               
         MVI   TGUSEQU,UBSS        GET BSS RATE                                 
         BRAS  RE,GETYEAR                                                       
         BRAS  RE,LOADTBL                                                       
         MVC   TCUSENUM,=H'62'                                                  
         XC    TCGROSS,TCGROSS                                                  
         BAS   RE,GETCAT           LOOK UP CATEGORY                             
         BRAS  RE,GETRATE                                                       
         BRAS  RE,CALCHOUR         CALCULATE HOURLY RATE                        
         MVI   TGUSEQU,UBSC        RESET TO BSC                                 
         CHI   R1,0                                                             
         BNH   BSSSP040                                                         
         BCTR  R1,0                DEDUCT 1 DAY                                 
         LTR   R1,R1                                                            
         BZ    BSSSP035                                                         
         LR    R5,R1                                                            
         STCM  R1,15,WORK+4                                                     
         MVC   WORK(4),TCGROSS                                                  
         GOTOR SVBRKDWN,DMCB,(0,WORK),(BYTE,WORK+4)                             
         LR    R1,R5                                                            
         M     R0,TCGROSS          BSS RATE * DAYS-1                            
*                                                                               
BSSSP035 LR    R5,R1                                                            
         XC    WORK+4(4),WORK+4                                                 
         MVI   WORK+7,1                                                         
         MVC   WORK(4),TASDFEE                                                  
         GOTOR SVBRKDWN,DMCB,(0,WORK),(BYTE,WORK+4)                             
         LR    R1,R5                                                            
         A     R1,TASDFEE          BSC RATE - FIRST DAY                         
BSSSP040 XC    TCGROSS,TCGROSS                                                  
         B     BSSALL                                                           
*                                                                               
BSSSP050 CLI   TGUSEQU,UINS        IF INS PAYMENT                               
         BNE   BSSSP080                                                         
         CLI   TCROW,14            NARRATOR ONLY                                
         BNE   BSSSP080                                                         
         CHI   R1,1                                                             
         BE    BSSSP080                                                         
*                                                                               
         BCTR  R1,0                DEDUCT 1 DAY                                 
         LR    R5,R1               NARRATOR FIRST DAY RATE                      
         MVC   WORK+4(4),=X'00000001'                                           
         MVC   WORK(4),TASDFEE                                                  
         GOTOR SVBRKDWN,DMCB,(0,WORK),(BYTE,WORK+4)                             
         MVC   TCGROSS,TASDFEE                                                  
*                                                                               
         BRAS  RE,GETINDXT                                                      
         LR    R1,RF                                                            
         LTR   R1,R1                                                            
         BZ    BSSSP080                                                         
*                                                                               
         USING TNDXD,R1                                                         
         L     RF,TNDXNRD1         OFF-CAMERA ADDT'L HALF HOUR RATE             
         USING TACOD,RE                                                         
         L     RE,TCATACO                                                       
         CLI   TACOTYPE,CTYICAT2   CAT 2 INDUSTRIAL?                            
         BNE   *+8                                                              
         L     RF,TNDXNRD2                                                      
         STCM  RF,15,TASDFEE                                                    
         LR    R1,R5                                                            
         DROP  RE                                                               
*                                                                               
BSSSP080 OC    TCATMTOT,TCATMTOT   ARE WE PAYING A TIMESHEET?                   
         BZ    BSSSP100                                                         
         LA    RE,TASDSPA                                                       
         CR    RF,RE               SPOTS OR DAYS?                               
         BNE   BSSSP083                                                         
         CLC   TCCAONOF,=C'OFF'    IF OFF CAMERA,                               
         BNE   BSSSP082                                                         
         BRAS  RE,TMOFFSD          COMPARE OFF CAM SPOTS VS DAYS                
         BNE   BSSSP083            CC NEQ = DAYS > SPOTS                        
*                                                                               
BSSSP082 BRAS  RE,TIMESPT          CALCULATE TIMESHEET SPOTS                    
         B     *+8                                                              
BSSSP083 BRAS  RE,TIMEDAY          CALCULATE TIMESHEET DAYS                     
*                                                                               
         LR    R5,R1               SAVE IT BEFORE BREAKDOWN                     
*                                                                               
         CLI   TCCAD050,0          IF DAYS HAVE BEEN PAID AT                    
         BE    BSSSP085            0.5 X RATE                                   
         ZIC   R0,TCCAD050         ADD THEM AS FULL DAYS FOR                    
         AHI   R0,1                THE SAKE OF THE BREAKDOWN                    
         SRA   R0,1                                                             
         MHI   R0,100                                                           
         AR    R1,R0                                                            
*                                                                               
BSSSP085 CLI   TCCAD075,0          IF DAYS HAVE BEEN PAID AT                    
         BE    BSSSP090            0.75 X RATE                                  
         ZIC   R0,TCCAD075         ADD THEM AS FULL DAYS FOR                    
         MHI   R0,75               THE SAKE OF THE BREAKDOWN                    
         SR    R1,R0                                                            
         ZIC   R0,TCCAD075                                                      
         MHI   R0,100                                                           
         AR    R1,R0                                                            
*                                                                               
BSSSP090 XR    R0,R0                                                            
         D     R0,=F'100'          DIVIDE BY 100 FIRST                          
         STCM  R1,15,WORK+4                                                     
         MVC   WORK(4),TASDFEE                                                  
         GOTOR SVBRKDWN,DMCB,(0,WORK),(BYTE,WORK+4)                             
         LR    R1,R5               RESTORE IT                                   
         M     R0,TASDFEE                                                       
         D     R0,=F'100'          DIVIDE BY 100                                
         B     BSSALL              FINISH UP CALCULATION                        
*                                                                               
BSSSP100 DS    0H                                                               
*&&DO                              TALENT DOESN'T WANT 1.5X                     
         CLI   TGUSEQU,UINS                                                     
         BNE   BSSSP150                                                         
         CLI   TGCAEQU,CTC3        GROUP CONTRACTOR = 1.5 ( RATE )              
         BL    BSSSP150                                                         
         CLI   TGCAEQU,CTC9                                                     
         BH    BSSSP150                                                         
         L     RF,TASDFEE                                                       
         SRL   RF,1                DIVIDE BY 2                                  
         A     RF,TASDFEE                                                       
         ST    RF,TASDFEE                                                       
         L     RF,TASDHR                                                        
         SRL   RF,1                DIVIDE BY 2                                  
         A     RF,TASDHR                                                        
         ST    RF,TASDHR                                                        
*&&                                                                             
BSSSP150 STCM  R1,15,WORK+4        SAVE IT BEFORE BREAK DOWN                    
         MVC   WORK(4),TASDFEE                                                  
         GOTOR SVBRKDWN,DMCB,(0,WORK),(BYTE,WORK+4)                             
         ICM   R1,15,WORK+4        RESTORE IT                                   
         LA    RF,TASDDAA                                                       
                                                                                
         USING TACOD,RE                                                         
         L     RE,TCATACO                                                       
         CLI   TACOLEN,TACOLNQ2                                                 
         BL    BSSSP160                                                         
         TM    TACOSTA3,TACOSSMW   IF SOCIAL MEDIA WAVER ON                     
         BNO   BSSSP160            ONLY PAY ONE SPOT                            
         CHI   R1,1                IF MULTIPLE SPOTS                            
         BNH   BSSSP160                                                         
         LHI   R1,1                                                             
         DROP  RE                                                               
*                                                                               
BSSSP160 M     R0,TASDFEE          * SESSION FEE                                
         B     BSSALL              FINISH UP CALCULATION                        
         EJECT                                                                  
*---------------------------------------------------------------------          
* OVERTIME HOURS                                                                
*---------------------------------------------------------------------          
BSSOT    DS    0H                  OVERTIME HOURS                               
         L     R1,TASDHR           HOURLY RATE                                  
         LA    R0,150              * 1.5 (TIME AND A HALF)                      
         BAS   RE,MULTRATE                                                      
*                                                                               
         CLI   TGUSEQU,UBSC        IF BSC PAYMENT,                              
         BNE   BSSOT1                                                           
         USING TACOD,RE                                                         
         L     RE,TCATACO                                                       
         CLI   TACOCTYP,CCTY04A    AND COMMERCIAL IS NOT ACTRA 2404A            
         BE    BSSOT1                                                           
         CLI   TACOCTYP,CCTY04B    AND COMMERCIAL IS NOT ACTRA 2404B            
         BE    BSSOT1                                                           
         DROP  RE                                                               
         MVC   TCUSENUM,=H'81'                                                  
         L     R3,TCGROSS          SAVE TCGROSS                                 
         XC    TCGROSS,TCGROSS                                                  
         BRAS  RE,GETRATE                                                       
         L     R1,TCGROSS          OT HOURLY RATE                               
         ST    R3,TCGROSS          RESTORE TCGROSS                              
         B     BSSOT5                                                           
*                                                                               
BSSOT1   TM    TGUSSTA3,ADDENUSE   IF ADDENDUM USE                              
         BZ    BSSOT5                                                           
         CLC   TCADDST,=C'KS'      AND STATE IS KS                              
         BNE   BSSOT5                                                           
         CLI   TGYREQU,CN04        NEW RULE FOR 2004                            
         BL    BSSOT2                                                           
         CLC   TCCAONOF(2),=C'ON'  FOR ON-CAMERA CAST ONLY                      
         BE    BSSOT5              DO * 1.5 (TIME AND A HALF)                   
*                                                                               
BSSOT2   LA    RE,KSOVPRIN         USE OVERTIME RATE FOR KS PRINCIPALS          
         TM    TCSTAT,TCSTPRIN                                                  
         BO    BSSOT3                                                           
         LA    RE,KSOVTON          OR ON CAMERA FOR NON-PRINCIPALS              
         CLC   TCCAONOF(2),=C'ON'                                               
         BE    BSSOT3                                                           
         LA    RE,KSOVTOFF         OR OFF CAMERA FOR NON-PRINCIPALS             
*                                                                               
BSSOT3   CLI   TGYREQU,CN94        IF CONTRACT YEAR IS 94                       
         BNE   *+8                                                              
         LA    RE,4(RE)            BUMP TO CORRECT RATE                         
         CLI   TGYREQU,CN97        IF CONTRACT YEAR IS 97                       
         BNE   *+8                                                              
         LA    RE,8(RE)            BUMP TO CORRECT RATE                         
         CLI   TGYREQU,CN02        IF CONTRACT YEAR IS 02                       
         BNE   *+8                                                              
         LA    RE,12(RE)           BUMP TO CORRECT RATE                         
         CLI   TGYREQU,CN04        IF CONTRACT YEAR IS 04                       
         BNE   *+8                                                              
         LA    RE,16(RE)           BUMP TO CORRECT RATE                         
         CLI   TGYREQU,CN06        IF CONTRACT YEAR IS 06                       
         BNE   *+8                                                              
         LA    RE,20(RE)           BUMP TO CORRECT RATE                         
BSSOT4   L     R1,0(RE)                                                         
*                                                                               
BSSOT5   CLI   TGUSEQU,UINS        IF INS PAYMENT                               
         BNE   BSSOT7                                                           
         MVI   WORK+4,1            WANT OVERTIME                                
         MVC   WORK+8(4),TASDDAA   AMOUNT BEFORE OVERTIME                       
         BRAS  RE,OTCEILNG         CHECK OVERTIME CEILING, R1=RATE              
*                                                                               
BSSOT7   ZIC   R0,TASDOT           * N'OVERTIME HOURS                           
         OC    TCATMTOT,TCATMTOT   ARE WE PAYING A TIMESHEET?                   
         BZ    *+8                                                              
         BRAS  RE,TIMEOT           TIMESHEET - OVERTIME HOURS                   
*                                                                               
         STCM  R0,15,WORK+4        SAVE IT BEFORE BREAK DOWN                    
         ST    R1,WORK             SAVE RATE IN WORK                            
         GOTOR SVBRKDWN,DMCB,(0,WORK),('PBC15X',WORK+4)                         
         L     R1,WORK             RESTORE IT                                   
*                                                                               
         MR    R0,R0                                                            
         LA    RF,TASDOTA                                                       
*                                                                               
*        B     BSSALL              REMOVE WHEN UNCOMMENT BELOW                  
*                                                                               
         CLI   TGUSEQU,UINS        IF INS PAYMENT                               
         BNE   BSSALL              NO, FINISH UP CALCULATIONS                   
         ST    R1,0(RF)            YES, NO OVERSCALE ON OVERTIME                
         B     BSSALL3             FINISH UP CALCULATION                        
*                                                                               
*---------------------------------------------------------------------          
* DOUBLE TIME HOURS                                                             
*---------------------------------------------------------------------          
BSSDT    DS    0H                  DOUBLE TIME HOURS                            
         L     R1,TASDHR           HOURLY RATE                                  
         SLA   R1,1                * 2                                          
         CLI   TGUSEQU,UBSC        IF BSC PAYMENT,                              
         BNE   BSSDT10                                                          
         USING TACOD,RE                                                         
         L     RE,TCATACO                                                       
         CLI   TACOCTYP,CCTY04A    AND COMMERCIAL IS NOT ACTRA 2404A            
         BE    BSSDT10                                                          
         CLI   TACOCTYP,CCTY04B    AND COMMERCIAL IS NOT ACTRA 2404B            
         BE    BSSDT10                                                          
         DROP  RE                                                               
         MVC   TCUSENUM,=H'82'                                                  
         L     R3,TCGROSS          SAVE TCGROSS                                 
         XC    TCGROSS,TCGROSS                                                  
         BRAS  RE,GETRATE                                                       
         L     R1,TCGROSS          DT HOURLY RATE                               
         ST    R3,TCGROSS          RESTORE TCGROSS                              
*                                                                               
BSSDT10  CLI   TGUSEQU,UINS        IF INS PAYMENT                               
         BNE   BSSDT20                                                          
         MVI   WORK+4,2            WANT DOUBLE TIME                             
         MVC   WORK+8(4),TASDDAA   AMOUNT BEFORE OVERTIME                       
         BRAS  RE,OTCEILNG         CHECK OVERTIME CEILING, R1=RATE              
*                                                                               
BSSDT20  ZIC   R0,TASDDT           * N'DOUBLE TIME HOURS                        
         OC    TCATMTOT,TCATMTOT   ARE WE PAYING A TIMESHEET?                   
         BZ    *+8                                                              
         BRAS  RE,TIMEDT           TIMESHEET - DOUBLETIME HOURS                 
*                                                                               
         STCM  R0,15,WORK+4        SAVE IT BEFORE BREAK DOWN                    
         ST    R1,WORK             SAVE RATE INTO WORK                          
         GOTOR SVBRKDWN,DMCB,(0,WORK),('PBC20X',WORK+4)                         
         L     R1,WORK             RESTORE IT                                   
*                                                                               
         MR    R0,R0                                                            
         LA    RF,TASDDTA                                                       
*                                                                               
         CLI   TGUSEQU,UINS        IF INS PAYMENT                               
         BNE   BSSALL              NO, FINISH UP CALCULATIONS                   
         ST    R1,0(RF)            YES, NO OVERSCALE ON OVERTIME                
         B     BSSALL3             FINISH UP CALCULATION                        
*                                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
* TRAVEL TIME HOURS                                                             
*---------------------------------------------------------------------          
BSSTRV   DS    0H                  TRAVEL TIME HOURS HOURS/MINUTES              
         XR    RF,RF                                                            
         ICM   RF,3,TASDTRV        N'TRAVEL TIME HOURS/MINUTES                  
         OC    TCATMTOT,TCATMTOT   ARE WE PAYING A TIMESHEET?                   
         BNZ   BSSTR3                                                           
         L     R4,=F'15'           DETERMINE N'15 MIN. INCREMENTS               
         L     R5,=F'250000'       * CONVERT TO 25/100 HOUR                     
         BAS   RE,CVTTODEC         CONVERT TO DECIMAL                           
         L     R0,FULL             DECIMAL HRS/MINS                             
BSSTR3   L     R1,TASDHR           * HOURLY RATE                                
         SPACE 1                                                                
         CLI   TGUSEQU,UBSC        IF BSC PAYMENT,                              
         BNE   BSSTR4                                                           
         USING TACOD,RE                                                         
         L     RE,TCATACO                                                       
         CLI   TACOCTYP,CCTY04A    AND COMMERCIAL IS NOT ACTRA 2404A            
         BE    BSSTR4                                                           
         CLI   TACOCTYP,CCTY04B    AND COMMERCIAL IS NOT ACTRA 2404B            
         BE    BSSTR4                                                           
         DROP  RE                                                               
         MVC   TCUSENUM,=H'80'                                                  
         L     R3,TCGROSS          SAVE TCGROSS                                 
         XC    TCGROSS,TCGROSS                                                  
         ST    R0,WORK+32          SAVE IT CUZ GETRATE CLOBS FULL               
         BRAS  RE,GETRATE                                                       
         L     R0,WORK+32                                                       
         L     R1,TCGROSS          HOURLY RATE                                  
         ST    R3,TCGROSS          RESTORE TCGROSS                              
         SPACE 1                                                                
BSSTR4   CLI   TGCAEQU,CTS         IF SOLOIST                                   
         BNE   BSSTR5                                                           
         CLI   TCROW,1             AND NOT 'ON CAMERA'                          
         BE    BSSTR5                                                           
         BRAS  RE,ADJTRV           ADJUST TRAVEL RATE (R1)                      
BSSTR5   OC    TCATMTOT,TCATMTOT   ARE WE PAYING A TIMESHEET?                   
         BZ    BSSTR8                                                           
         CLC   TCCAONOF,=C'OFF'    IF OFF CAMERA                                
         BNE   BSSTR10                                                          
         TM    TGCASTAT,OKON       AND CATEGORY IS VALID FOR ON CAMERA          
         BZ    BSSTR10                                                          
         MVC   TCCAONOF,=C'ON '    CHANGE TO ON CAMERA                          
         BAS   RE,GETCAT           LOOK UP CATEGORY                             
         L     R3,TCGROSS          SAVE TCGROSS                                 
         XC    TCGROSS,TCGROSS                                                  
         ST    R0,WORK+32          SAVE IT CUZ GETRATE CLOBS FULL               
         BRAS  RE,GETRATE                                                       
         L     R0,WORK+32                                                       
         BRAS  RE,CALCHOUR         CALCULATE HOURLY RATE                        
         L     R1,TASDHR           SET HOURLY RATE                              
         ST    R3,TCGROSS          RESTORE TCGROSS                              
         MVC   TCCAONOF,=C'OFF'    RESET TO OFF CAMERA                          
         BAS   RE,GETCAT           RESTORE TCROW                                
         B     BSSTR10                                                          
*                                                                               
BSSTR8   LR    R5,R1                                                            
         ST    R1,WORK                                                          
         ST    R0,WORK+32                                                       
         GOTOR SVBRKDWN,DMCB,(0,WORK),('PVCTRV',WORK+32)                        
         L     R0,WORK+32          RESTORE R0 AND R1                            
         LR    R1,R5                                                            
*                                                                               
         BAS   RE,MULTR0                                                        
         B     *+8                                                              
BSSTR10  BRAS  RE,TIMETRVL         TIMESHEET - TRAVEL TIME                      
BSSTR20  LA    RF,TASDTRA                                                       
         B     BSSALL              FINISH UP CALCULATION                        
*---------------------------------------------------------------------          
* PRIOR DAY WARDROBE                                                            
*---------------------------------------------------------------------          
BSSPDW   DS    0H                  PRIOR DAY WARDROBE HOURS/MINUTES             
         XR    RF,RF                                                            
         ICM   RF,3,TASDPDW        * N'PDW HOURS/MINUTES                        
         OC    TCATMTOT,TCATMTOT   ARE WE PAYING A TIMESHEET?                   
         BNZ   BSSPD3                                                           
         L     R4,=F'15'           DETERMINE N'15 MIN. INCREMENTS               
         L     R5,=F'250000'       * CONVERT TO 25/100 HOUR                     
         BAS   RE,CVTTODEC         CONVERT TO DECIMAL                           
         L     R0,FULL             DECIMAL HRS/MINS                             
BSSPD3   L     R1,TASDHR           * HOURLY RATE                                
         SPACE 1                                                                
         CLI   TGUSEQU,UBSC        IF BSC PAYMENT,                              
         BNE   BSSPD5                                                           
         USING TACOD,RE                                                         
         L     RE,TCATACO                                                       
         CLI   TACOCTYP,CCTY04A    AND COMMERCIAL IS NOT ACTRA 2404A            
         BE    BSSPD5                                                           
         CLI   TACOCTYP,CCTY04B    AND COMMERCIAL IS NOT ACTRA 2404B            
         BE    BSSPD5                                                           
         DROP  RE                                                               
         MVC   TCUSENUM,=H'80'                                                  
         L     R3,TCGROSS          SAVE TCGROSS                                 
         XC    TCGROSS,TCGROSS                                                  
         ST    R0,WORK+32          SAVE IT CUZ GETRATE CLOBS FULL               
         BRAS  RE,GETRATE                                                       
         L     R0,WORK+32                                                       
         L     R1,TCGROSS          HOURLY RATE                                  
         ST    R3,TCGROSS          RESTORE TCGROSS                              
         SPACE 1                                                                
BSSPD5   OC    TCATMTOT,TCATMTOT   ARE WE PAYING A TIMESHEET?                   
         BNZ   BSSPD10                                                          
*                                                                               
         ST    R1,WORK             SAVE IT BEFORE BREAK DOWN                    
         ST    R0,WORK+32                                                       
         SR    R0,R0                                                            
         L     R1,WORK+32                                                       
         D     R0,=F'100'                                                       
         STCM  R1,15,WORK+4                                                     
         GOTOR SVBRKDWN,DMCB,(0,WORK),(56,WORK+4)                               
         L     R0,WORK+32          RESTORE R0 AND R1                            
         L     R1,WORK                                                          
*                                                                               
         BAS   RE,MULTR0                                                        
         B     *+8                                                              
BSSPD10  BRAS  RE,TIMEPDWD         TIMESHEET - PRIOR DAY WD                     
         LA    RF,TASDPDA                                                       
         B     BSSALL              FINISH UP CALCULATION                        
         EJECT                                                                  
*---------------------------------------------------------------------          
* TAGS                                                                          
*---------------------------------------------------------------------          
         USING TASDD,R2                                                         
         USING TAGFEED,R4                                                       
BSSTG    DS    0H                  N'TAGS                                       
         USING TACOD,RE                                                         
         CLI   TGUSEQU,UBSC        IF BSC PAYMENT,                              
         BNE   BSSTGA                                                           
         L     RE,TCATACO          IF COMMERCIAL HAS ANY ACTRA                  
         CLI   TACOCTYP,0          TYPE EXCEPT FOR 2404B,                       
         BE    BSSTGA              NO TAGS CALCULATED                           
         CLI   TACOCTYP,CCTY04B                                                 
         BNE   XIT                                                              
         DROP  RE                                                               
*                                                                               
BSSTGA   L     R4,TCTAGFEE         R4=A(TAG FEE TABLE)                          
         LA    RF,TASDTAA          RF=A(AREA TO STORE TAG AMOUNT)               
         ZIC   R1,TASDTAG          R1=N'TAGS                                    
         LTR   R1,R1               DON'T BOTHER IF NO TAGS                      
         BZ    XIT                                                              
**       OC    TCATMTOT,TCATMTOT   ARE WE PAYING A TIMESHEET?                   
**       BZ    *+8                                                              
**       BRAS  RE,TIMETAG          TIMESHEET - TAGS - RETURNED IN R1            
         C     R4,TCARATES                                                      
         BE    XIT                 DON'T BOTHER IF NO TABLE                     
*                                                                               
         OC    TASDSP,TASDSP       IF NO SPOTS                                  
         BNZ   BSSTG5                                                           
         OC    TASDDAY,TASDDAY     AND NO DAYS                                  
         BNZ   BSSTG5                                                           
         BCTR  R1,0                                                             
         LR    R3,R1               SAVE R3 = N'TAGS - 1                         
         GOTOR SVBRKDWN,DMCB,(0,TASDFEE),(65,=F'1')                             
         L     R1,TASDFEE          1ST TAG IS AT SESSION FEE                    
*                                                                               
         TM    TGUSSTA3,ADDENUSE   IF ADDENDUM USE                              
         BZ    BSSTG1                                                           
         CLC   TCADDST,=C'KS'      AND STATE IS KS                              
         BNE   BSSTG1                                                           
         CLC   TCCAONOF(2),=C'ON'  AND OFF CAMERA                               
         BE    BSSTG1                                                           
         L     R1,KSFRSTAG         GET 1ST TAG RATE                             
         CLI   TGYREQU,CN94        IF CONTRACT YEAR IS 94                       
         BNE   *+8                                                              
         L     R1,KSFSTG94         GET CORRECT RATE                             
         CLI   TGYREQU,CN97        IF CONTRACT YEAR IS 97                       
         BNE   *+8                                                              
         L     R1,KSFSTG97         GET CORRECT RATE                             
         CLI   TGYREQU,CN02        IF CONTRACT YEAR IS 02                       
         BNE   *+8                                                              
         L     R1,KSFSTG02                                                      
         CLI   TGYREQU,CN04        IF CONTRACT YEAR IS 04                       
         BNE   *+8                                                              
         L     R1,KSFSTG04                                                      
         CLI   TGYREQU,CN06        IF CONTRACT YEAR IS 06                       
         BNE   *+8                                                              
         L     R1,KSFSTG06                                                      
BSSTG1   LA    R5,BSSALL                                                        
         LA    RE,BSSTG2           SET RE FOR NTR1                              
         NTR1                                                                   
         BR    R5                  OFF TO BSSALL                                
*                                                                               
BSSTG2   LTR   R1,R3               SET R1 = N'TAGS - 1                          
         BZ    XIT                 DON'T BOTHER IF NO MORE TAGS                 
*                                                                               
BSSTG5   CLI   TGYREQU,CN94        IF CONTRACT YEAR 94 OR AFTER                 
         BL    BSSTG7                                                           
         TM    TGUSSTA3,ADDENUSE   IF NOT ADDENDUM USE                          
         BZ    BSSTG94                                                          
         CLC   TCADDST,=C'TX'      OR IF ADDENDUM AND STATE IS TX               
         BE    BSSTG94B                                                         
         CLC   TCADDST,=C'GA'      OR GA                                        
         BE    BSSTG94C            BRANCH TO HANDLE DIFFERENTLY                 
         CLC   TCADDST,=C'NW'      OR NW                                        
         BE    BSSTG94D                                                         
*                                                                               
BSSTG7   TM    TGUSSTA3,ADDENUSE   TEST ADDENDUM USE                            
         BZ    BSSTG10                                                          
         LA    R4,TAGFNEXT         BUMP TO START OF ADDENDUM RATES              
         BRAS  RE,GETORDER                                                      
         BNE   XIT                                                              
         ZIC   R2,TGBYTE             ORDER #                                    
         LA    R3,TAGFLNQ          * L'EACH ENTRY                               
         MR    R2,R2               = DISPLACEMENT INTO TABLE                    
         AR    R4,R3               ADD TO R4 TO GET CORRECT ENTRY               
*                                                                               
BSSTG10  L     R3,TAGFON           TAG FEE FOR ON CAMERA                        
         CLC   TCCAONOF(2),=C'ON'                                               
         BE    *+8                                                              
         L     R3,TAGFOFF          TAG FEE FOR OFF CAMERA                       
BSSTG15  MR    R0,R3                                                            
         B     BSSALL              FINISH UP CALCULATION (SAVES AT RF)          
*                                                                               
*        IF CONTRACT YEAR 94 OR LATER, MUST CALCULATE DIFFERENTLY               
*        CAUSE HAVE DISCOUNTS AFTER A NUMBER OF TAGS.  R1=N'TAGS                
*                                                                               
BSSTG94  MVC   TCUSENUM,=H'53'     SET USENUM FOR REGULAR TAGS                  
         CLI   TGUSEQU,UPUB        IF PUB USE,                                  
         BNE   BSSTG94T                                                         
         CLI   TGMEEQU,RADIO       AND MEDIA RADIO                              
         BNE   BSSTG94T                                                         
         MVC   TCUSENUM,=H'55'     SET USENUM FOR REG RADIO TAGS                
         B     BSSTG94T                                                         
BSSTG94B MVC   TCUSENUM,=H'247'    SET USENUM FOR TX                            
         B     BSSTG94T                                                         
BSSTG94C MVC   TCUSENUM,=H'249'    SET USENUM FOR GA                            
         B     BSSTG94T                                                         
BSSTG94D MVC   TCUSENUM,=H'340'    SET USENUM FOR NW                            
         CLI   TGUSWKS,2           IF NOT 2 WEEK CYCLE TYPE                     
         BE    BSSTG94T                                                         
         MVC   TCUSENUM,=H'341'    SET USENUM FOR 13 WEEK                       
         B     BSSTG94T                                                         
BSSTG94T STH   R1,TCUNITS          SET TCUNITS WITH N'TAGS                      
         ST    R1,WORK                                                          
*                                                                               
         NI    TCSTAT,ALL-TCSTUSES TURN OFF BY USES INDICATOR                   
         L     R3,TCGROSS          SAVE TCGROSS IN R3                           
         XC    TCGROSS,TCGROSS     CLEAR FOR GETRATE                            
         OI    TCSTAT2,TCSTSTAG                                                 
         BRAS  RE,GETRATE          ADD IN ADDITIONAL FEE FOR TAGS               
*        L     R1,TCGROSS          SAVE TAG AMOUNT IN R1                        
         GOTOR SVBRKDWN,DMCB,('SBDADJST',TCGROSS),(57,TCUNITS)                  
*                                                                               
         L     R1,TCGROSS          SAVE TAG AMOUNT IN R1                        
         ST    R3,TCGROSS          RESTORE OLD TCGROSS FROM R3                  
         XC    TCUNITS,TCUNITS     CLEAR DUMMY UNITS                            
         B     BSSALL              FINISH UP CALCULATIONS (SAVES AT RF)         
         EJECT                                                                  
*---------------------------------------------------------------------          
* NIGHT PREMIUM                                                                 
*---------------------------------------------------------------------          
BSSNP    DS    0H                  NIGHT PREMIUM                                
         OC    TCATMTOT,TCATMTOT   ARE WE PAYING A TIMESHEET?                   
         BZ    XIT                                                              
*                                                                               
         USING TACOD,RE                                                         
         L     RE,TCATACO          IF COMMERCIAL HAS ANY ACTRA                  
         CLI   TACOCTYP,0          TYPE EXCEPT FOR 2404B,                       
         BE    BSSNP10             USE DIFFERENT HOURLY RATES FOR NP            
         CLI   TACOCTYP,CCTY04B                                                 
         BE    BSSNP10                                                          
         CLI   TACOCTYP,CCTY04A                                                 
         BE    BSSNP05                                                          
         DROP  RE                                                               
*                                                                               
         L     R3,TCGROSS          SAVE TCGROSS                                 
         MVC   TCUSENUM,=H'80'     ACTRA STRAIGHT TIME HOURLY RATE              
         XC    TCGROSS,TCGROSS                                                  
         BRAS  RE,GETRATE          RATE RETURNED IN TCGROSS                     
         SR    R1,R1                                                            
         BRAS  RE,TIMENPS                                                       
*                                                                               
         MVC   TCUSENUM,=H'81'     ACTRA OVERTIME HOURLY RATE                   
         XC    TCGROSS,TCGROSS                                                  
         BRAS  RE,GETRATE          RATE RETURNED IN TCGROSS                     
         BRAS  RE,TIMENPO                                                       
*                                                                               
         MVC   TCUSENUM,=H'82'     ACTRA DOUBLETIME HOURLY RATE                 
         XC    TCGROSS,TCGROSS                                                  
         BRAS  RE,GETRATE          RATE RETURNED IN TCGROSS                     
         BRAS  RE,TIMENPD                                                       
         ST    R3,TCGROSS          RESTORE TCGROSS                              
         B     BSSALL2             AMOUNT IS IN R1                              
*                                                                               
BSSNP05  BRAS  RE,TIMENPA          2404A SAG HOURS (ST,OT, DT)                  
*                                                                               
         BRAS  RE,GETYEAR                                                       
         BRAS  RE,LOADTBL                                                       
         MVC   TCUSENUM,=H'80'     ACTRA STRAIGHT TIME HOURLY RATE              
         L     R3,TCGROSS          SAVE TCGROSS                                 
         XC    TCGROSS,TCGROSS                                                  
         BAS   RE,GETCAT           LOOK UP CATEGORY                             
         BRAS  RE,GETRATE          RATE RETURNED IN TCGROSS                     
         BRAS  RE,TIMENPS                                                       
         ST    R3,TCGROSS          RESTORE TCGROSS                              
         B     BSSALL2                                                          
*                                                                               
BSSNP10  CLC   TCCAONOF,=C'OFF'    IF OFF CAMERA,                               
         BNE   *+8                                                              
         BRAS  RE,CALCHR2          CALCULATE HOURLY RATE                        
*                                                                               
         BRAS  RE,TIMENP           TIMESHEET - 20%/10% OF HOURS IN R1           
*                                                                               
         SR    R0,R0                                                            
         M     R0,TASDHR           * HOURLY RATE                                
         D     R0,=F'10000'        DIVIDE BY 100                                
         LTR   R0,R0               ROUND UP IF ANY REMAINDER                    
         BZ    *+8                                                              
         AHI   R1,1                                                             
*                                                                               
         LTR   R1,R1                                                            
         BZ    BSSALL2                                                          
         ST    R1,WORK                                                          
         GOTOR SVBRKDWN,DMCB,('SBDADJST',WORK),(58,0)                           
         L     R1,WORK                                                          
*                                                                               
         B     BSSALL2             FINISH UP CALCULATIONS                       
         EJECT                                                                  
*---------------------------------------------------------------------          
* MEAL PENALTY                                                                  
*---------------------------------------------------------------------          
BSSMP    DS    0H                  MEAL PENALTY                                 
         OC    TCATMTOT,TCATMTOT   ARE WE PAYING A TIMESHEET?                   
         BZ    XIT                                                              
         BRAS  RE,TIMEMP           TIMESHEET - MEAL PENALTY AMT IN R1           
         B     BSSALL3             FINISH UP CALC - IGNORE OVERSCALE %          
         EJECT                                                                  
*---------------------------------------------------------------------          
* 16 HOUR RULE                                                                  
*---------------------------------------------------------------------          
BSS16    DS    0H                  16 HOUR RULE - REIMB EXP                     
         TM    TGCATYPE,EXTRA      EXTRAS ONLY                                  
         BZ    XIT                                                              
         OC    TCATMTOT,TCATMTOT   ARE WE PAYING A TIMESHEET?                   
         BZ    XIT                                                              
         BRAS  RE,TIME16HR         TIMESHEET - # OF HOURS IN R1                 
*                                                                               
         ST    R1,WORK+4           SAVE IT BEFORE BREAK DOWN                    
         MVC   WORK(4),TASDFEE                                                  
         GOTOR SVBRKDWN,DMCB,(0,WORK),(60,WORK+4)                               
         ICM   R1,15,WORK+4        RESTORE IT                                   
*                                                                               
         M     R0,TASDFEE          * SESSION FEE                                
         B     BSSALL2             FINISH UP CALCULATIONS                       
         EJECT                                                                  
*---------------------------------------------------------------------          
* SMOKE PAY                                                                     
*---------------------------------------------------------------------          
BSSAP    DS    0H                  ADD TO PAYMENT AMOUNT (SMOKE PAY)            
         OC    TCATMTOT,TCATMTOT   ARE WE PAYING A TIMESHEET?                   
         BZ    XIT                                                              
         BRAS  RE,TIMEAP           TIMESHEET - ADDITIONAL AMOUNT IN R1          
*                                                                               
         LTR   R1,R1                                                            
         BZ    BSSALL2                                                          
         ST    R1,FULL                                                          
         GOTOR SVBRKDWN,DMCB,('SBDADJST',FULL),(61,0)                           
*                                                                               
         BAS   RE,OVSCALC          CALCULATE OVERSCALE                          
         ICM   R1,15,FULL                                                       
         B     BSSALL3             FINISH UP CALCULATIONS                       
         EJECT                                                                  
*---------------------------------------------------------------------          
* ADJUSTMENT                                                                    
*---------------------------------------------------------------------          
BSSAJ    DS    0H                  ADD TO PAYMENT AMOUNT (ADJUSTMENT)           
         OC    TCATMTOT,TCATMTOT   ARE WE PAYING A TIMESHEET?                   
         BZ    XIT                                                              
         BRAS  RE,TIMEAJ           TIMESHEET - ADDITIONAL AMOUNT IN R1          
*                                                                               
         LTR   R1,R1                                                            
         BZ    BSSALL3                                                          
         ST    R1,WORK                                                          
         GOTOR SVBRKDWN,DMCB,('SBDADJST',WORK),(62,0)                           
         ICM   R1,15,WORK                                                       
*                                                                               
         B     BSSALL3             FINISH UP CALC - IGNORE OVERSCALE %          
         EJECT                                                                  
*---------------------------------------------------------------------          
* FRI/SAT HRS                                                                   
*---------------------------------------------------------------------          
BSSFS    DS    0H                  ADD TO PAYMENT AMOUNT (FRI/SAT HRS)          
         OC    TCATMTOT,TCATMTOT   ARE WE PAYING A TIMESHEET?                   
         BZ    XIT                                                              
         L     R1,TASDHR           HOURLY RATE                                  
         BRAS  RE,TIMEFS           TIMESHEET - ADDITIONAL AMOUNT IN R1          
         B     BSSALL2             FINISH UP CALCULATIONS                       
         EJECT                                                                  
*---------------------------------------------------------------------          
* REST PERIOD VIOLATIONS                                                        
*---------------------------------------------------------------------          
BSSRPV   DS    0H                  ADD TO PAYMENT AMOUNT (FRI/SAT HRS)          
         OC    TCATMTOT,TCATMTOT   ARE WE PAYING A TIMESHEET?                   
         BZ    XIT                                                              
         L     R1,TASDFEE          SESSION RATE                                 
         BRAS  RE,TIMERPV          TIMESHEET - ADDITIONAL AMOUNT IN R1          
*                                                                               
         LTR   R1,R1                                                            
         BZ    BSSALL3                                                          
         ST    R1,FULL             SAVE AMOUNT BEFORE BREAKDOWN                 
         ST    R3,WORK+4                                                        
         MVC   WORK(4),TASDFEE                                                  
         CLC   TASDFEE,=F'50000'   PAY LESSER OF SESSION FEE OR $500            
         BNH   *+10                                                             
         MVC   WORK(4),=F'50000'                                                
         GOTOR SVBRKDWN,DMCB,(0,WORK),(68,WORK+4)                               
         L     R1,FULL             RESTORE IT                                   
         L     RE,TCNOTSPH                                                      
         AR    RE,R1               ADD TO NOT SUBJ TO PNH AMOUNT                
         ST    RE,TCNOTSPH                                                      
*                                                                               
         B     BSSALL3             FINISH UP CALC - IGNORE OVERSCALE %          
         EJECT                                                                  
***********************************************************************         
*        ROUTINE HANDLES VITA UPGRADE PAYMENTS                        *         
***********************************************************************         
                                                                                
VITAUPGR NTR1                                                                   
         OC    TCUFCAT,TCUFCAT     IF UPGRADING FROM VITA                       
         BZ    XIT                                                              
         L     R2,TCGROSS          SAVE GROSS FOR CURRENT CATEGORY              
         XC    TCGROSS,TCGROSS                                                  
                                                                                
         ZICM  R0,TGCAT,3                                                       
         GOTO1 CATVAL,DMCB,TCUFCAT CHANGE TO OLD CATEGORY                       
         BAS   RE,LOOKUP           DO TABLE LOOK-UPS - GET OLD RATES            
         BNE   ERR4                                                             
         STCM  R0,7,TGCAT                                                       
         GOTO1 CATVAL,DMCB,TGCAT   RESET CATEGORY VARIABLES                     
                                                                                
         L     R3,TCGROSS          SUBTRACT OLD RATE FROM NEW RATE              
         SR    R2,R3                                                            
         ST    R2,TCGROSS          AND SAVE THE NEW GROSS                       
                                                                                
         XC    TCCSTBRK,TCCSTBRK                                                
         GOTOR SVBRKDWN,DMCB,(0,TCGROSS),('PBCDAY',=F'1')                       
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------          
*              MUSIC SESSION CALCULATION SUB-ROUTINES                           
*---------------------------------------------------------------------          
BSMHM    DS    0H                  WORK TIME HOURS/MINUTES                      
         BRAS  RE,BSMHMM           R1 COMES BACK W/ PAYMENT                     
         LA    RF,TASDMHMA                                                      
         B     BSSALL              FINISH UP CALCULATION                        
         EJECT                                                                  
*---------------------------------------------------------------------          
*              INDUSTRIAL MUSIC SESSION CALCULATION SUB-ROUTINES                
*---------------------------------------------------------------------          
IMSHM    DS    0H                  WORK TIME HOURS/MINUTES                      
         BRAS  RE,IMSHMM           R1 COMES BACK W/ PAYMENT                     
         LA    RF,TASDMHMA                                                      
         B     BSSALL              FINISH UP CALCULATION                        
         EJECT                                                                  
*---------------------------------------------------------------------          
*              INDUSTRIAL OFFCAM SESSION CALCULATION SUB-ROUTINES               
*---------------------------------------------------------------------          
IDSHM    DS    0H                  WORK TIME HOURS/MINUTES                      
         BRAS  RE,IDSHMM           R1 COMES BACK W/ PAYMENT                     
         LA    RF,TASDMHMA                                                      
         B     BSSALL              FINISH UP CALCULATION                        
         EJECT                                                                  
*---------------------------------------------------------------------          
*        INDUSTRIAL RETAKE CALCULATION SUB-ROUTINES - HOURS                     
*---------------------------------------------------------------------          
RTKHM    DS    0H                  WORK TIME HOURS/MINUTES                      
         BRAS  RE,RTKHMM           R1 COMES BACK W/ PAYMENT                     
         LA    RF,TASDIHMA                                                      
         B     BSSALL              FINISH UP CALCULATION                        
                                                                                
*---------------------------------------------------------------------          
*        INDUSTRIAL RETAKE CALCULATION SUB-ROUTINES - TRAVEL                    
*---------------------------------------------------------------------          
RTKTR    DS    0H                  TAVEL TIME                                   
         BRAS  RE,RTKTRV           R1 COMES BACK W/ PAYMENT                     
         LA    RF,TASDITRA                                                      
         B     BSSALL              FINISH UP CALCULATION                        
         EJECT                                                                  
*---------------------------------------------------------------------          
*        INDUSTRIAL AUDIO SESSIONS  CALCULATION SUB-ROUTINES - HOURS            
*---------------------------------------------------------------------          
DIOHM    DS    0H                  WORK TIME HOURS/MINUTES                      
         BRAS  RE,DIOHMM           R1 COMES BACK W/ PAYMENT                     
         LA    RF,TASDIRHA                                                      
         B     BSSALL              FINISH UP CALCULATION                        
         EJECT                                                                  
*---------------------------------------------------------------------          
*        INDUSTRIAL AUDIO SESSIONS  CALCULATION SUB-ROUTINES - RETAKES          
*---------------------------------------------------------------------          
DIORT    DS    0H                  RETAKES                                      
         BRAS  RE,DIORTK           R1 COMES BACK W/ PAYMENT                     
         LA    RF,TASDIRRA                                                      
         B     BSSALL              FINISH UP CALCULATION                        
         EJECT                                                                  
*---------------------------------------------------------------------          
*        INTERACTIVE VOICE SESSIONS  CALCULATION SUB-ROUTINES - HOURS           
*---------------------------------------------------------------------          
IVRHM    DS    0H                  WORK TIME HOURS/MINUTES                      
         BRAS  RE,IVRHMM           R1 COMES BACK W/ PAYMENT                     
         LA    RF,TASDIVHA                                                      
         B     BSSALL              FINISH UP CALCULATION                        
                                                                                
*---------------------------------------------------------------------          
*        INTERACTIVE VOICE E CALCULATION SUB-ROUTINES - TRAVEL                  
*---------------------------------------------------------------------          
IVRTR    DS    0H                  TAVEL TIME                                   
         BRAS  RE,IVRTRV           R1 COMES BACK W/ PAYMENT                     
         LA    RF,TASDIVTA                                                      
         B     BSSALL              FINISH UP CALCULATION                        
         EJECT                                                                  
*---------------------------------------------------------------------          
*              RADIO SESSION CALCULATION SUB-ROUTINES                           
*---------------------------------------------------------------------          
         USING TASDD,R2            R2=A(SESSION DETAILS ELEMENT)                
BSRSP    DS    0H                  N'SPOTS / HOURS.MINUTES                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         ICM   RF,3,TASDRHM        HOURS/MINUTES                                
         D     RE,=F'100'          RF=N'HOURS                                   
         LR    R1,RE               R1=N'MINUTES                                 
         MHI   RF,60                 N'HOURS * 60 MINUTES                       
         AR    R1,RF               + N'MINUTES = TOTAL N'MINUTES                
         XR    R0,R0                                                            
         D     R0,=F'90'           R1=NUMBER OF 90 MINUTE INCREMENTS            
         SPACE                                                                  
         LTR   R0,R0               ROUND UP IF ANY REMAINDER                    
         BZ    *+8                                                              
         AHI   R1,1                                                             
         LA    RF,TASDRHMA                                                      
*                                                                               
         IC    RE,TASDRSP                                                       
         CR    R1,RE               TAKE HIGHER OF SPOTS & 90 MIN INCR           
         BNL   *+10                                                             
         LR    R1,RE                                                            
         LA    RF,TASDRSPA                                                      
         STCM  RF,15,WORK+8                                                     
*                                                                               
         STCM  R1,15,WORK+4                                                     
         MVC   WORK(4),TASDFEE                                                  
         GOTOR SVBRKDWN,DMCB,(0,WORK),(64,WORK+4)                               
*                                                                               
         ICM   R1,15,WORK+4                                                     
         ICM   RF,15,WORK+8                                                     
         M     R0,TASDFEE          * SESSION FEE                                
         B     BSSALL              FINISH UP CALCULATION                        
*                                                                               
         USING TAGFEED,R4                                                       
         USING TASDD,R2            R2=A(TASD EL.)                               
BSRTG    DS    0H                  N'TAGS                                       
         L     R4,TCTAGFEE         R4=A(TAG FEE TABLE)                          
         C     R4,TCARATES                                                      
         BE    XIT                 DON'T BOTHER IF NO TABLE                     
         ZIC   R1,TASDRTG          R1=N'TAGS                                    
         LTR   R1,R1               OR IF NO TAGS                                
         BZ    XIT                                                              
         LA    RF,TASDRTGA         RF=A(AREA TO STORE TAG AMOUNT)               
         SPACE                                                                  
BSRTG0B  OC    TASDRSP,TASDRSP     IF NO SPOTS                                  
         BNZ   BSRTG6                                                           
         BCTR  R1,0                                                             
         LR    R3,R1               SAVE R3 = N'TAGS - 1                         
         L     R1,TASDFEE          1ST TAG IS AT SESSION FEE                    
         SPACE                                                                  
         TM    TGUSSTA3,ADDENUSE   IF ADDENDUM USE                              
         BZ    BSRTG1                                                           
         CLC   TCADDST,=C'GA'      AND STATE IS GA                              
         BNE   BSRTG0C                                                          
         L     R1,GAFSTRAG         GET CORRECT RATE                             
         SPACE                                                                  
BSRTG0C  CLC   TCADDST,=C'KS'      AND STATE IS KS                              
         BNE   BSRTG1                                                           
         CLI   TGCAEQU,CTCV        CHARACTER VOICE GETS NO TAG RATES            
         BE    XIT                                                              
         L     R1,KSFRSTGR         GET 1ST TAG RATE                             
         CLI   TGYREQU,CN94        IF CONTRACT YEAR IS 94                       
         BNE   *+8                                                              
         L     R1,KSFSTR94         GET CORRECT RATE                             
         CLI   TGYREQU,CN97        IF CONTRACT YEAR IS 97                       
         BNE   *+8                                                              
         L     R1,KSFSTR97         GET CORRECT RATE                             
         CLI   TGYREQU,CN02        IF CONTRACT YEAR IS 02                       
         BNE   *+8                                                              
         L     R1,KSFSTR02                                                      
         CLI   TGYREQU,CN04        IF CONTRACT YEAR IS 04                       
         BNE   *+8                                                              
         L     R1,KSFSTR04                                                      
         CLI   TGYREQU,CN06        IF CONTRACT YEAR IS 06                       
         BNE   *+8                                                              
         L     R1,KSFSTR06                                                      
BSRTG1   LA    R5,BSSALL                                                        
         LA    RE,BSRTG2           SET RE FOR NTR1                              
         NTR1                                                                   
         BR    R5                  OFF TO BSSALL                                
         SPACE                                                                  
BSRTG2   DS    0H                                                               
         ST    R1,WORK+8                                                        
         LTR   R1,R3               SET R1 = N'TAGS - 1                          
         BNZ   BSRTG6                                                           
         MVC   WORK+4(4),=X'00000001'                                           
         GOTOR SVBRKDWN,DMCB,(0,WORK+8),(57,WORK+4)                             
         B     XIT                 DON'T BOTHER IF NO MORE TAGS                 
         SPACE                                                                  
BSRTG6   CLI   TGYREQU,CN94        IF CONTRACT YEAR 94 OR AFTER                 
         BL    BSRTG7                                                           
         TM    TGUSSTA3,ADDENUSE   IF ADDENDUM USE                              
         BZ    BSRTG7                                                           
         CLC   TCADDST,=C'TX'      AND STATE IS TX                              
         BE    BSRTG94B            BRANCH TO HANDLE DIFFERENTLY                 
         CLC   TCADDST,=C'NW'      AND STATE IS NW                              
         BE    BSRTG94C            BRANCH TO HANDLE DIFFERENTLY                 
         CLC   TCADDST,=C'GA'      AND STATE IS GA                              
         BE    BSRTG94D            BRANCH TO HANDLE DIFFERENTLY                 
         CLC   TCADDST,=C'KS'      AND STATE IS KS                              
         BE    BSRTG94K            BRANCH TO HANDLE DIFFERENTLY                 
         SPACE                                                                  
BSRTG7   TM    TGUSSTA3,ADDENUSE   TEST ADDENDUM USE                            
         BZ    BSRTG10                                                          
         LA    R4,TAGFNEXT         BUMP TO START OF ADDENDUM RATES              
         BRAS  RE,GETORDER                                                      
         BNE   XIT                                                              
         ZIC   R2,TGBYTE             ORDER #                                    
         LA    R3,TAGFLNQ          * L'EACH ENTRY                               
         MR    R2,R2               = DISPLACEMENT INTO TABLE                    
         AR    R4,R3               ADD TO R4 TO GET CORRECT ENTRY               
         SPACE                                                                  
BSRTG10  TM    TGUSSTA3,ADDENUSE   IF ADDENDUM USE                              
         BZ    BSRTG15                                                          
         CLC   TCADDST,=C'KS'      AND STATE IS KS                              
         BNE   BSRTG15                                                          
         CLI   TGCAEQU,CTCV        AND NOT CHARACTER VOICE                      
         BNE   BSRTG20             USE KS TAG RATE IN TABLE                     
         B     XIT                                                              
BSRTG15  CLI   TGYREQU,CN00        IF CONTRACT YEAR IS PRE-00                   
         BNL   BSRTG94A                                                         
BSRTG20  STCM  R1,15,WORK+4                                                     
         GOTOR SVBRKDWN,DMCB,(0,TAGFRAD),(57,WORK+4)                            
         ICM   R1,15,WORK+4                                                     
         L     R3,TAGFRAD          TAG FEE                                      
         MR    R0,R3                                                            
         B     BSSALL              FINISH UP CALCULATION                        
         SPACE 2                                                                
*        IF CONTRACT YEAR 94 OR LATER, MUST CALCULATE DIFFERENTLY               
*        CAUSE HAVE DISCOUNTS AFTER A NUMBER OF TAGS.  R1=N'TAGS                
*                                                                               
BSRTG94A LHI   RE,55                                                            
         B     BSRTG94T                                                         
BSRTG94B LHI   RE,248              SET USENUM FOR TX                            
         B     BSRTG94T                                                         
BSRTG94C LHI   RE,390              SET USENUM FOR NW                            
         CLI   TGUSWKS,2           IF NOT 2 WEEK CYCLE TYPE                     
         BE    BSRTG94T                                                         
         LHI   RE,343              SET USENUM FOR 13 WEEK                       
         B     BSRTG94T                                                         
BSRTG94D LHI   RE,344              SET USENUM FOR GA                            
         B     BSRTG94T                                                         
BSRTG94K LHI   RE,345              SET USENUM FOR KS                            
BSRTG94T STH   RE,TCUSENUM                                                      
         STH   R1,TCUNITS          SET TCUNITS WITH N'TAGS                      
*                                                                               
         NI    TCSTAT,ALL-TCSTUSES TURN OFF BY USES INDICATOR                   
         L     R3,TCGROSS          SAVE TCGROSS IN R3                           
         XC    TCGROSS,TCGROSS     CLEAR FOR GETRATE                            
         OI    TCSTAT2,TCSTSTAG                                                 
         BRAS  RE,GETRATE          ADD IN ADDITIONAL FEE FOR TAGS               
*                                                                               
*        GOTOR SVBRKDWN,DMCB,('SBDADJST',TCGROSS),(57,TCUNITS)                  
         LH    R1,TCUNITS                                                       
         L     RF,TCGROSS                                                       
         OC    TASDRSP,TASDRSP                                                  
         BNZ   BSRTG94Z                                                         
         AHI   R1,1                                                             
         A     RF,TASDFEE                                                       
BSRTG94Z STH   R1,TCUNITS                                                       
         ST    RF,WORK+4                                                        
         GOTOR SVBRKDWN,DMCB,('SBDADJST',WORK+4),(57,TCUNITS)                   
*                                                                               
         LA    RF,TASDRTGA         RF=A(AREA TO STORE TAG AMOUNT)               
         L     R1,TCGROSS          SAVE TAG AMOUNT IN R1                        
         ST    R3,TCGROSS          RESTORE OLD TCGROSS FROM R3                  
         XC    TCUNITS,TCUNITS     CLEAR DUMMY UNITS                            
         B     BSSALL              FINISH UP CALCULATIONS (SAVES AT RF)         
         EJECT                                                                  
*              ROUTINE ENDS SESSION DETAIL CALCULATIONS                         
         SPACE 1                                                                
*                                  R1=AMOUNT                                    
*                                  RF=A(AREA TO STORE AMOUNT)                   
BSSALL   DS    0H                                                               
******   TM    TGUNEQU,AFM         FOR AFM MEMBERS                              
         LR    R0,R1                                                            
         GOTOR UNITEST,DMCB,TGUNEQUS,AFM,0,0,0                                  
         LR    R1,R0                                                            
         BZ    BSSA2                                                            
         L     R0,TCAFMSPH                                                      
         AR    R0,R1               ADD TO P&H BASIS BEFORE OVERSCALE            
         ST    R0,TCAFMSPH                                 CALCULATION          
*                                                                               
BSSA2    ST    R1,FULL             SET BASIS FOR CALC                           
         BAS   RE,OVSCALC          CALCULATE OVERSCALE - AMOUNT                 
*                                    RETURNED IN FULL                           
*                                                                               
BSSA3    TM    TCINPUT,TCINPAY     UNLESS PAYMENT IS OVERRIDDEN                 
         BO    BSSAX                                                            
         L     R1,FULL                                                          
         ST    R1,0(RF)            SAVE AMOUNT IN ELEMENT                       
         A     R1,TCGROSS                                                       
         ST    R1,TCGROSS          ADD TO GROSS                                 
         SPACE 1                                                                
BSSAX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE ENDS SESSION DETAIL CALCULATIONS                         
         SPACE 1                                                                
*                                  R1=AMOUNT                                    
BSSALL2  DS    0H                                                               
*        TM    TGUNEQU,AFM         FOR AFM MEMBERS                              
         LR    R0,R1                                                            
         GOTOR UNITEST,DMCB,TGUNEQUS,AFM,0,0,0                                  
         LR    R1,R0                                                            
         BZ    BSSA22                                                           
         L     R0,TCAFMSPH                                                      
         AR    R0,R1               ADD TO P&H BASIS BEFORE OVERSCALE            
         ST    R0,TCAFMSPH                                 CALCULATION          
*                                                                               
BSSA22   ST    R1,FULL             SET BASIS FOR CALC                           
         BAS   RE,OVSCALC          CALCULATE OVERSCALE - AMOUNT                 
*                                    RETURNED IN FULL                           
*                                                                               
         TM    TCINPUT,TCINPAY     UNLESS PAYMENT IS OVERRIDDEN                 
         BO    BSSA2X                                                           
         L     R1,FULL                                                          
         A     R1,TCGROSS                                                       
         ST    R1,TCGROSS          ADD TO GROSS                                 
*                                                                               
BSSA2X   B     XIT                                                              
         EJECT                                                                  
*              ROUTINE ENDS SESSION DETAIL CALCULATIONS                         
*              IGNORES OVERSCALE PERCENTAGE                                     
         SPACE 1                                                                
*                                  R1=AMOUNT                                    
BSSALL3  DS    0H                                                               
*        TM    TGUNEQU,AFM         FOR AFM MEMBERS                              
         LR    R0,R1                                                            
         GOTOR UNITEST,DMCB,TGUNEQUS,AFM,0,0,0                                  
         LR    R1,R0                                                            
         BZ    BSSA32                                                           
         L     R0,TCAFMSPH                                                      
         AR    R0,R1               ADD TO P&H BASIS BEFORE OVERSCALE            
         ST    R0,TCAFMSPH                                 CALCULATION          
         SPACE 1                                                                
BSSA32   TM    TCINPUT,TCINPAY     UNLESS PAYMENT IS OVERRIDDEN                 
         BO    BSSA3X                                                           
         A     R1,TCGROSS                                                       
         ST    R1,TCGROSS          ADD TO GROSS                                 
         SPACE 1                                                                
BSSA3X   B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CONVERT HOUR/MINUTE FIELDS TO DECIMAL                 
         SPACE 1                                                                
*                                  RF=HOURS/MINUTES                             
*                                  R4=N'MINUTES IN INCREMENT                    
*                                  R5=R4/60*1000000                             
CVTTODEC NTR1                                                                   
         XR    RE,RE               / 100                                        
         D     RE,=F'100'          RF=HOURS                                     
         LR    R1,RE               R1=MINUTES                                   
         XR    R0,R0                                                            
         DR    R0,R4               DETERMINE N'INCREMENTS                       
         LR    R0,R5               * CONVERT TO (R4/60*100)/100 HOUR            
         BAS   RE,MULTRATE                                                      
         SPACE 1                                                                
         MHI   RF,10000            RESTORE INTEGER N'HOURS                      
         AR    R1,RF               ADD 'EM UP                                   
         ST    R1,FULL                                                          
         B     XIT                                                              
         EJECT                                                                  
*              MULTIPLICATION ROUTINES                                          
         SPACE 1                                                                
MULTSCAL M     R0,TCGROSS          USE SCALE                                    
         B     MULTALL                                                          
MULTR0   MR    R0,R0               USE R0                                       
*                                                                               
MULTALL  D     R0,=F'5000'                                                      
MULTALLX LTR   R1,R1                                                            
         BM    *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         BR    RE                  RETURN ANSWER IN R1                          
         SPACE 3                                                                
MULTRATE MR    R0,R0               USE R0                                       
         D     R0,=F'50'                                                        
         B     MULTALLX                                                         
         SPACE 3                                                                
MULTACT  MR    R0,R0               USE R0                                       
         D     R0,=F'500000'                                                    
         B     MULTALLX                                                         
         SPACE 3                                                                
*              ROUTINE TAKES 1/3 R1 AND RETURNS IN R1                           
         SPACE 1                                                                
ONETHIRD DS    0H                                                               
         SR    R0,R0                                                            
         SLA   R1,1                                                             
         D     R0,=F'3'            TAKE ONE-THIRD                               
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                ROUNDED                                      
         BR    RE                                                               
         SPACE 3                                                                
*              ROUTINE ROUNDS AMOUNT IN R1 TO NEAREST NICKEL                    
         SPACE 1                                                                
NICKEL   DS    0H                                                               
         AHI   R1,2                ADD 2                                        
         XR    R0,R0                                                            
         D     R0,=F'5'            DIVIDE BY FIVE                               
         M     R0,=F'5'            MULTIPLY BY FIVE                             
         LTR   R1,R1               ADD 1 IF NEGATIVE                            
         BNM   *+8                                                              
         AHI   R1,1                                                             
         BR    RE                                                               
         EJECT                                                                  
*              CALCULATE AMOUNT TO BE APPLIED TOWARDS REUSE                     
         SPACE 1                                                                
APPCALC  NTR1                                                                   
         USING TACOD,RE                                                         
         L     RE,TCATACO          IF THIS IS PER CYCLE COMMERCIAL              
         TM    TACOSTA2,TACOPCYC   NEVER APPLY TO REUSE                         
         BO    APPC12                                                           
         CLI   TACOLEN,TACOLNQ2                                                 
         BL    APPC0AA                                                          
         TM    TACOSTA3,TACOSSMW   IF THIS COMM HAS SOCIAL MEDIA WAIVER         
         BO    APPCX               NEVER APPLY TO REUSE                         
         DROP  RE                                                               
         SPACE 1                                                                
APPC0AA  TM    TGUSSTA2,APPREUSE   TEST APPLYING TOWARDS REUSE                  
         BZ    APPCX                                                            
*        TM    TGUSXUNI,AFM        MUSICIANS                                    
         GOTOR UNITEST,DMCB,TGUSXUNS,AFM,0,0,0                                  
         BZ    APPCX                                                            
         TM    TGCATYPE,EXTRA      AND EXTRAS DON'T GET APPLIED CREDITS         
         BO    APPCX                                                            
*        TM    TGUNEQU,ACT+UDA     ACTRA PERFORMERS ...                         
         GOTOR UNITEST,DMCB,TGUNEQUS,ACT+UDA,0,0,0                              
         BZ    APPC0A                                                           
         CLI   OVERLAY,X'70'       IGNORE PERFORMERS GETTING CAN. RATES         
         BL    APPC0A              PHASES X'70' - X'78' CAN. RATES              
         CLI   OVERLAY,CANRATE9                                                 
         BH    APPC0A                                                           
*                                                                               
         USING TACOD,RE                                                         
         CLI   TGUSEQU,UBSC        UNLESS MAKING BSC PAYMENT                    
         BNE   APPCX                                                            
         L     RE,TCATACO                                                       
         CLI   TACOCTYP,CCTY04A    TO ACTRA TYPE 2404A COMMERCIAL               
         BE    APPC0A                                                           
         B     APPCX                                                            
         DROP  RE                                                               
*                                                                               
APPC0A   TM    TCCASTST,TCCAAPP0   IF APPLYING 0                                
         BO    APPC12              GO ADD TACREL                                
         SPACE 1                                                                
         TM    TGUSSTA2,HLDTYPE    UNLESS USE IS HOLDING FEE                    
         BO    APPC0B                                                           
         CLI   TGUSEQU,UREN        OR REINSTATEMENT                             
         BE    APPC0B                                                           
         CLI   TGUSEQU,USRE                                                     
         BE    APPC0B                                                           
         CLI   TGUSEQU,UARN                                                     
         BE    APPC0B                                                           
         OC    TCGROSS,TCGROSS                                                  
         BZ    APPCX               DON'T BOTHER IF GROSS=0                      
         SPACE 1                                                                
APPC0B   MVC   TCAPPLIC,TCGROSS    GROSS AMOUNT IS APPLICABLE                   
         CLI   TGUSEQU,UARN        FOR ARN,REN, OR SRE                          
         BE    *+20                                                             
         CLI   TGUSEQU,UREN                                                     
         BE    *+12                                                             
         CLI   TGUSEQU,USRE                                                     
         BNE   APPC0                                                            
         L     R1,TCGROSS                                                       
         SRA   R1,1                GROSS DIVIDED BY 2                           
         ST    R1,TCAPPLIC         = APPLICABLE AMOUNT                          
         SPACE                                                                  
APPC0    TM    TCCASTST,TCCAOVAM   IF HAVE OVERSCALE AMOUNT ON CAST             
         BZ    *+10                                                             
         MVC   TCAPPLIC,TCOVAMT    OVERSCALE AMOUNT IS APPLICABLE               
         SPACE 1                                                                
         TM    TCINPUT,TCINPRI     IF PAYING FIXED CYC ON GRT PRI COMML         
         BZ    APPC0B1                                                          
         TM    TCCASTST,TCCAOVAM   AND APPLICABLE AMT FROM OVSC ON CAST         
         BO    APPC0B4                                                          
APPC0B1  TM    TGUSSTA2,APPREUSE   OR APPLYING TOWARDS REUSE                    
         BZ    APPC2                                                            
         OC    TCATASD,TCATASD     AND WE DON'T HAVE A SESSION DETAILS          
         BNZ   APPC2               ELEMENT                                      
         TM    TCCASTST,TCCAOVAM   AND NO OVERSCALE AMOUNT PRESENT              
         BO    APPC2                                                            
         TM    TGUSSTA2,NORATES    AND THE USE HAS PROGRAMMED RATES             
         BO    APPC2                                                            
         TM    TGUSSTA3,ADDENUSE                                                
         BZ    APPC0B2                                                          
         OC    TCADDST,TCADDST                                                  
         BZ    APPC2                                                            
APPC0B2  OC    TCOV1,TCOV1         AND NO OVERSCALE PERCENT PRESENT             
         BZ    APPC0B3                                                          
         TM    TCINPUT,TCINPAY     (UNLESS ITS OVERRIDDEN)                      
         BZ    APPC2                                                            
APPC0B3  OI    TCSTAT2,TCSFTSCL    SET TO CREATE FTRACK FOR SCALE               
         SPACE 1                                                                
APPC0B4  L     R2,TCGROSS          SAVE ACTUAL PAYMENT                          
         XC    TCGROSS,TCGROSS     CLEAR IT                                     
******** L     R0,TCOV2                                                         
******** XC    TCOV2,TCOV2                                                      
         BAS   RE,LOOKUP           GET SCALE AMOUNT                             
         BNE   ERR3                                                             
******** ST    R0,TCOV2                                                         
         SPACE 1                                                                
         CLI   TGUSEQU,UREN        IF PAYMENT IS REINSTATEMENT                  
         BE    APPC0C                                                           
         CLI   TGUSEQU,UARN        ADDENDUM REINSTATEMENT                       
         BE    APPC0C                                                           
         CLI   TGUSEQU,USRE        OR SPANISH REINSTATEMENT                     
         BNE   APPC0D                                                           
APPC0C   L     RE,TCGROSS          DIVIDE PAYMENT AMOUNT BY 2                   
         SRA   RE,1                IN ORDER TO GET SCALE HOLDING                
         ST    RE,TCGROSS          FEE AMOUNT                                   
         SPACE 1                                                                
APPC0D   OC    TCOV1,TCOV1         IF WE DON'T HAVE OV1%                        
         BNZ   APPC1                                                            
         OC    TCACAST,TCACAST     AND WE HAVE A(CAST RECORD)                   
         BZ    APPC1                                                            
         L     R0,AIO                                                           
         MVC   AIO,TCACAST         SET AIO=A(CAST REC.)                         
         GOTO1 GETOV1,DMCB,(X'80',TGUSCDE),TCOV1  GET OV1%                      
         ST    R0,AIO                                                           
APPC1    MVC   FULL,TCGROSS        BASED ON SCALE AMOUNT,                       
         CLI   RECNUM,ES           FROM ESTIMATING                              
         BNE   APPC1A                                                           
         L     R4,TCACAST          AND HYPO CAST                                
         CLI   0(R4),0                                                          
         BE    APPC1X              SKIP OVERSCALING TWICE                       
APPC1A   TM    TCSTAT2,TCSFTSCL                                                 
         BO    APPC1X                                                           
         OI    TCSTAT,TCNOSVBK     DON'T SAVE BREAKDOWN DATA                    
         BAS   RE,OVSCALC          ADD IN OVERSCALE                             
APPC1X   L     R1,TCAPPLIC                                                      
         C     R1,FULL             IF IT'S NOT MORE THAN APPLICABLE AMT         
         BL    *+10                                                             
         MVC   TCAPPLIC,FULL       THIS AMOUNT IS APPLICABLE                    
         ST    R2,TCGROSS          RESTORE ACTUAL PAYMENT AMOUNT                
         SPACE 1                                                                
APPC2    TM    TGUSSTAT,SESSION    IF SESSION                                   
         BZ    APPC12                                                           
         ICM   R2,15,TCATASD       AND HAVE SESSION DETAILS ELEMENT             
         BZ    APPC8                                                            
         USING TASDD,R2                                                         
         TM    TGUSSTA2,BSSTYPE    TEST BSS TYPE USE                            
         BZ    APPC4                                                            
         CLI   TASDSP,0            IF NO SPOTS                                  
         BNE   APPC6                                                            
         CLI   TASDDAY,0                                                        
         BE    APPCX               AND NO DAYS - DON'T APPLY ANYTHING           
         B     APPC6                                                            
         SPACE 1                                                                
APPC4    CLI   TASDRSP,0           FOR BSR - IF NO SPOTS                        
         BNE   APPC6                                                            
         OC    TASDRHM,TASDRHM                                                  
         BZ    APPCX               AND NO HRS.MN - DON'T APPLY ANYTHING         
         SPACE 1                                                                
APPC6    TM    TCCASTST,TCCAOVAM   IF APPLIED AMOUNT NOT FROM OVSC AMT          
         BO    APPC8                                                            
         OC    TASDFEE,TASDFEE     TEST HAVE RATES (IN CASE ADDENDUM)           
         BZ    APPC8                                                            
         MVC   FULL,TASDFEE        BASIS IS SESSION FEE                         
         OI    TCSTAT,TCNOSVBK     DON'T SAVE BREAKDOWN DATA                    
         BAS   RE,OVSCALC          CALCULATE OVERSCALE                          
         MVC   TCAPPLIC,FULL       SET APPLICABLE AMOUNT                        
         SPACE 1                                                                
APPC8    DS    0H                                                               
*&&DO                                                                           
APPC8    TM    TGMEEQU,CABLE       FOR CABLE SESSIONS                           
         BZ    APPC12                                                           
         TM    TCSTAT2,TCSTCGT1    IF ONLY ONE COMM. PRODUCED TODAY             
         BO    APPC10                                                           
         XC    TCAPPLIC,TCAPPLIC   THEN NOT ELIGIBLE                            
         B     APPCX                                                            
         SPACE 1                                                                
APPC10   TM    TCSTAT2,TCSTC1ST    IF THIS IS FIRST ONE PRODUCED TODAY          
         BZ    APPC12                                                           
         L     R1,TCGROSS                                                       
         BAS   RE,ONETHIRD         THEN MAY ONLY APPLY 1/3 GROSS                
         ST    R1,TCAPPLIC                                                      
*&&                                                                             
         SPACE 1                                                                
APPC12   BRAS  RE,ADDTACR          ADD APPLIED CREDIT HISTORY ELEMENT           
         BRAS  RE,UPDTACR          OR UPDATE APPL CRED HISTORY ELEMENT          
         SPACE 1                                                                
APPCX    B     XIT                                                              
         EJECT                                                                  
*              CALCULATE APPLIED CREDITS                                        
***********************************************************************         
*              COULD BE COMING THROUGH THIS WITH APPLIED CREDITS                
*              ALREADY CALCULATED AND DISPLAYED (TCINAPPL) AND                  
*              TCGROSS ALREADY ADJUSTED (COULD BE 0), BUT STILL                 
*              NEED TO PROCCESS BECAUSE NEED TO ADJUST TACREL ON                
*              CAST RECORD AND SET TCRTCAST+TCRTTACR BITS FOR LATER.            
***********************************************************************         
         SPACE 1                                                                
ACRCALC  NTR1                                                                   
         TM    TCINPUT,TCINAPPL    TEST WE HAVE AMOUNT ALREADY                  
         BO    ACRC10                                                           
         TM    TCINPUT,TCINPRI     IF PAYING FIXED CYC ON GRT PRI COMML         
         BZ    ACRC4                                                            
         TM    TCCASTST,TCCAOVAM   AND APPLICABLE AMT FROM OVSC ON CAST         
         BZ    ACRC4                                                            
         MVC   TCAPPLCR,TCAPPLIC   THEN APPLYABLE AMT S/B APPLD TO GUAR         
         MVI   TCAPPLCD,APPLGUAR   SET WE'RE APPLYING GUARANTEE                 
         OI    TCINPUT,TCINAPPL    SET WE HAVE APPLIED AMOUNT                   
         B     ACRC10                                                           
         SPACE 1                                                                
ACRC4    TM    TCOPTS,TCONOAPP     TEST USER ASKED FOR NO CREDITS               
         BO    ACRCX                                                            
         TM    TCOPTS,TCOAPPLH     TEST USER ASKED FOR CREDITS                  
         BO    ACRC6                                                            
         TM    TCOPTS,TCOAPPLS                                                  
         BO    ACRC5                                                            
         SPACE 1                                                                
         USING TACOD,R2                                                         
*&&DO                                                                           
**NO-OP  JUN10/09                                                               
*                                                                               
         CLI   TGUSEQU,UFGR        IF FGN REUSE                                 
         BNE   ACRC4A                                                           
         L     R2,TCATACO          SKIP CHECK FOR NOAPPLCR SO WILL              
         CLI   TACOTYPE,CTYFGN     APPLY IF COMM'L TYPE IS FOREIGN              
         BE    ACRC4D                                                           
*&&                                                                             
*                                                                               
ACRC4A   TM    TCCASTAT,TACASXAC   TEST DON'T APPLY FIXED CYC GUAR              
         BO    ACRC4AA             AGAINST CAB, CBL, SCB OR LCB                 
         CLI   TGUSEQU,UCAB        ELSE TEST NOT CAB OR CBL OR SCB              
         BE    ACRC4D                                                           
         CLI   TGUSEQU,UCBL                                                     
         BE    ACRC4D                                                           
         CLI   TGUSEQU,UIRN                                                     
         BE    ACRC4D                                                           
         CLI   TGUSEQU,UNMR                                                     
         BE    ACRC4D                                                           
         CLI   TGUSEQU,UMVI                                                     
         BE    ACRC4D                                                           
         CLI   TGUSEQU,UMVN                                                     
         BE    ACRC4D                                                           
         CLI   TGUSEQU,USIR                                                     
         BE    ACRC4D                                                           
         CLI   TGUSEQU,USNM                                                     
         BE    ACRC4D                                                           
         CLI   TGUSEQU,USMI                                                     
         BE    ACRC4D                                                           
         CLI   TGUSEQU,USMN                                                     
         BE    ACRC4D                                                           
         CLI   TGUSEQU,USCB                                                     
         BE    ACRC4D                                                           
         CLI   TGUSEQU,ULCB                                                     
         BE    ACRC4D                                                           
         CLI   TGUSEQU,UACB                                                     
         BE    ACRC4D                                                           
ACRC4AA  TM    TCCASTAT,TACASXAP   TEST DON'T APPLY FIXED CYC GUAR              
         BO    ACRC4B              AGAINST PAX                                  
         CLI   TGUSEQU,UPAX        ELSE TEST NOT PAX                            
         BE    ACRC4D                                                           
ACRC4B   TM    TGUSSTAT,NOAPPLCR   TEST NO APPLIED CREDITS FOR THIS USE         
         BO    ACRCX                                                            
ACRC4D   TM    TGUSSTA2,NORATES    TEST NOT IN RATE TABLE                       
         BO    ACRCX                                                            
         B     ACRC10                                                           
ACRC5    MVI   TCAPPLCD,APPLSESS   USER WANTS APPLIED CODE FOR SESSION          
         MVC   FULL(3),=C'BSS'                                                  
         TM    TGUSSTA3,ADDENUSE   IF ADDENDUM USE                              
         BZ    ACRC5D                                                           
         MVC   FULL(3),=C'ADT'     LOOK UP SESSION RATE FOR ADDENDUM            
         CLI   TGMEEQU,RADIO       IF MEDIA OF COMMERCIAL IS RADIO              
         BNE   *+10                                                             
         MVC   FULL(3),=C'ADO'     LOOK UP SESSION RATE FOR ADDEND RAD          
         B     ACRC5G                                                           
         SPACE                                                                  
ACRC5D   CLI   TGMEEQU,RADIO          IF MEDIA OF COMMERCIAL IS RADIO           
         BNE   *+10                                                             
         MVC   FULL(3),=C'BSR'        LOOK UP SESSION RATE FOR RADIO            
         SPACE                                                                  
ACRC5G   CLI   TGUSEQU,UMUS        TEST MUS PAYMENT                             
         BNE   ACRC8                                                            
         MVC   TCAPPLCR,TCGROSS    SET APPLIED AMT = GROSS SO PAY = 0           
         TM    TCSTAT,TCSTAFMP     IF HAVE SUBJ TO P&H FOR MUSICIANS            
         BZ    *+10                                                             
         XC    TCAFMSPH,TCAFMSPH   CLEAR IT                                     
         B     ACRCX                                                            
         SPACE 1                                                                
ACRC6    TM    TGCATYPE,NOHLD      EXCLUDE IF CAT DOESN'T GET HLDS              
         BO    ACRCX                                                            
         CLC   TCCAONOF,=C'OFF'    EXCLUDE SOME OFF-CAMERA PERFS.               
         BNE   ACRC7                                                            
         TM    TGCATYPE,NOHLDOFF   NO HOLDS IF OFF-CAMERA                       
         BO    ACRCX                                                            
         CLI   TGYREQU,CN88        AS/OF '88 CONTRACT                           
         BL    ACRC7                                                            
         TM    TGCATYPE,NHLDOF88   THERE ARE SOME MORE                          
         BO    ACRCX                                                            
ACRC7    MVI   TCAPPLCD,APPLHLD    USER WANTS APPLIED CODE FOR HLD FEE          
         MVC   FULL(3),=C'HLD'                                                  
         TM    TGUSSTA3,ADDENUSE   IF ADDENDUM USE                              
         BZ    *+10                                                             
         MVC   FULL(3),=C'ADH'     LOOK FOR ADH                                 
         SPACE 1                                                                
ACRC8    BRAS  RE,GETTACR          IF NO APPLIED CREDIT EL., DO LOOKUP          
         BE    ACRC12                                                           
         SPACE 1                                                                
         ICM   R2,14,TGUSCDE       SAVE CURRENT USE CODE                        
         IC    R2,TGUSTYP          AND TYPE                                     
         L     R3,TCGROSS          AND GROSS                                    
         XC    TCGROSS,TCGROSS                                                  
         BRAS  RE,SAVUINFO         SAVE CURRENT USE NUMBER INFO                 
         SPACE 1                                                                
         MVI   BYTE,0                                                           
         CLC   FULL(3),=C'ADT'     IF ADDENDUM SESSION                          
         BE    ACRC9                                                            
         CLC   FULL(3),=C'ADO'                                                  
         BNE   *+8                                                              
ACRC9    MVI   BYTE,UADT13W        SET 13W TYPE (04 FOR ADT & ADO)              
         GOTO1 USEVAL,DMCB,FULL,BYTE                                            
         SPACE 1                                                                
         BAS   RE,LOOKUP           GO GET RATES                                 
         BNE   ERR3                                                             
         MVC   TCAPPLCR,TCGROSS    SAVE AS CREDIT AMOUNT                        
         OI    TCINPUT,TCINAPPL    SET WE HAVE IT                               
         ST    R3,TCGROSS          RESTORE ORIG. GROSS                          
         BRAS  RE,RESUINFO         RESTORE CURRENT USE NUMBER INFO              
         SPACE 1                                                                
         CLC   TCAPPLCR,TCGROSS    INSURE CREDIT AMOUNT NOT GT GROSS            
         BNH   *+10                                                             
         MVC   TCAPPLCR,TCGROSS                                                 
         SPACE 1                                                                
         ST    R2,FULL                  RESTORE ORIG. USE TO W/S                
         GOTO1 USEVAL,DMCB,FULL,FULL+3  LOOK UP ORIGINAL USE ENTRY              
         B     ACRCX                    DONE - NO APPLIED CREDIT EL.            
         SPACE 1                                                                
ACRC10   CLI   TCAPPLCD,APPLGUAR   GET OUT IF WE'RE APPLYING GUARANTEE          
         BE    ACRCX                                                            
         CLI   TCAPPLCD,0          IF THERE'S AN APPLIED CODE ALREADY           
         BE    ACRC11                                                           
         CLI   TCAPPLCD,APPLSESS   TEST APPLIED CODE IS FOR SESSION             
         BE    ACRC11                                                           
         CLI   TCAPPLCD,APPLHLD    OR HOLDING FEE                               
         BE    ACRC11                                                           
         CLI   TCAPPLCD,APPLWSPU   OR WILDSPOT UPGRADE                          
         BE    ACRC10B                                                          
         CLI   TCAPPLCD,APPLCAB    OR CABLE                                     
         BNE   ACRCX               ELSE DON'T BOTHER WITH TACREL                
         CLI   TGUSEQU,UPAX                                                     
         BE    ACRC10A                                                          
         TM    TCCASTAT,TACASXAC   TEST DON'T APPLY FIXED CYC GUAR              
         BO    ACRCX               AGAINST CAB, CBL, SCB OR LCB                 
         B     ACRC10AA                                                         
ACRC10A  TM    TCCASTAT,TACASXAP   TEST DON'T APPLY FIXED CYC GUAR              
         BO    ACRCX               AGAINST PAX                                  
ACRC10AA OC    TCGROSS,TCGROSS     DON'T DO TACREL IF GROSS IS 0                
         BZ    ACRCX                                                            
         SPACE                                                                  
ACRC10B  TM    TCOPTS,TCONOAPP     IF UPGRADE APPLIED CODE AND USER             
         BO    ACRCX               ASKED FOR NO CREDITS                         
         TM    TCCASTA3,TACASXFT   OR CAST NEVER CREDITS THEN SKIP TACR         
         BO    ACRCX                                                            
         SPACE                                                                  
ACRC11   BRAS  RE,GETTACR          SEE IF THERE'S AN APPLIED CREDIT EL.         
         BNE   ACRCX                                                            
         USING TACRD,R4                                                         
ACRC12   L     R4,TGELEM           R4=A(APPL. CREDIT HISTORY EL.)               
         TM    TCOPTS,TCOAPPLH     TEST USER ASKED FOR CREDITS                  
         BO    ACRC14                                                           
         TM    TCOPTS,TCOAPPLS                                                  
         BO    ACRC14                                                           
         CLI   TGMEEQU,CABLE       IF MEDIA IS NOT CABLE                        
         BE    ACRC12A                                                          
         CLI   TGUSEQU,UCBL        CBL                                          
         BE    ACRC12D                                                          
         CLI   TGUSEQU,UCAB        CAB                                          
         BE    ACRC12D                                                          
         CLI   TGUSEQU,USCB        SCB                                          
         BE    ACRC12D                                                          
         CLI   TGUSEQU,ULCB        LCB                                          
         BE    ACRC12D                                                          
         CLI   TGUSEQU,UACB        AND ACB CAN ONLY APPLY AGAINST               
         BE    ACRC12D             FIXED CYCLE GUARANTEES                       
ACRC12A  CLI   TGUSEQU,UIRN        FOR ALL MEDIAS, IRN                          
         BE    ACRC12D                                                          
         CLI   TGUSEQU,UNMR        NMR                                          
         BE    ACRC12D                                                          
         CLI   TGUSEQU,UMVI        MVI                                          
         BE    ACRC12D                                                          
         CLI   TGUSEQU,UMVN        MVN                                          
         BE    ACRC12D                                                          
         CLI   TGUSEQU,USIR        SIR                                          
         BE    ACRC12D                                                          
         CLI   TGUSEQU,USNM        SNM                                          
         BE    ACRC12D                                                          
         CLI   TGUSEQU,USMI        SMI                                          
         BE    ACRC12D                                                          
         CLI   TGUSEQU,USMN        SMN                                          
         BE    ACRC12D                                                          
         CLI   TGUSEQU,UPAX        PAX                                          
         BE    ACRC12D                                                          
         CLI   TGUSEQU,UIFB        AND IFB                                      
         BNE   ACRC12G                                                          
ACRC12D  TM    TACRSTAT,TACRSGUA   CAN ONLY APPLY AGAINST FIXED                 
         BZ    ACRCX               CYCLE GUARANTEES                             
ACRC12G  CLI   TCAPPLCD,APPLWSPU   IF APPLIED CODE IS WILDSPOT UPGRADE          
         BE    ACRC13                                                           
         CLI   TCAPPLCD,APPLCAB    OR CABLE                                     
         BNE   *+8                                                              
ACRC13   MVI   TCAPPLCD,0          CLEAR SO WILL RESET LATER                    
         SPACE                                                                  
ACRC14   L     R0,TACRBAL          R0=BALANCE REM. TO BE APPLIED                
         C     R0,TCGROSS          COMPARE BALANCE TO GROSS                     
         BNH   *+8                 IF NOT LARGER THEN SKIP                      
         L     R0,TCGROSS             ELSE MAKE APPLIED AMT = GROSS             
         SPACE                                                                  
         TM    TCINPUT,TCINAPPL    IF WE HAVE AMOUNT ALREADY                    
         BZ    *+8                                                              
         L     R0,TCAPPLCR         USE IT                                       
         SPACE 1                                                                
         ST    R0,TCAPPLCR         SAVE APPLIED AMOUNT                          
         SPACE 1                                                                
         TM    TCPAYST,TCCREDIT                                                 
         BO    *+6                                                              
         LCR   R0,R0               REVERSE SIGN IF NOT CREDIT PAYMENT           
         A     R0,TACRBAL          AND ADD TO BALANCE                           
         BNM   *+6                                                              
         XR    R0,R0               INSURE IT DOESN'T GO NEGATIVE                
         ST    R0,TACRBAL          AND SAVE NEW BALANCE                         
         SPACE 1                                                                
         MVC   TCTACREL,TACREL     SAVE UPDATED EL. IN LOCAL STORAGE            
         SPACE 1                                                                
         OI    TCRTRN,TCRTCAST+TCRTTACR  SET CAST REC/APPL CRED EL CHGD         
         SPACE 1                                                                
ACRCX    OC    TCAPPLCR,TCAPPLCR   IF APPLIED AMOUNT IS 0                       
         BNZ   ACRCX2                                                           
         TM    TCINPUT,TCINAPPL    AND IT WASN'T INPUT                          
         BO    XIT                                                              
         MVI   TCAPPLCD,0          CLEAR APPLIED CODE                           
         B     XIT                                                              
         SPACE                                                                  
ACRCX2   CLI   TCAPPLCD,0          IF NO APPLIED CODE ALREADY                   
         BNE   XIT                                                              
         MVI   TCAPPLCD,APPLSESS   SET APPLIED CODE FOR SESSION                 
         TM    TCSTAT,TCSTAPPH     IF APPLIED CREDIT AMT FROM HLD FEE           
         BZ    *+8                                                              
         MVI   TCAPPLCD,APPLHLD    SET APPLIED CODE FOR HOLDING FEE             
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CALCULATES AGENT FEE AMOUNT FOR PRINT PAYMENTS           
         SPACE                                                                  
AGTFCALC NTR1                                                                   
         CLI   TGUSMEDS,PRINT      TEST PRINT PAYMENT                           
         BNE   XIT                                                              
         SPACE                                                                  
         LH    R0,TCCARATE         R0=AGENT FEE RATE                            
         L     R1,TCPAY            R1=PAYMENT AMOUNT                            
         BAS   RE,MULTR0           MULTIPLY RATE AND PAYMENT                    
         ST    R1,TCAGTFEE         SAVE AGENT FEE                               
         B     XIT                                                              
         EJECT                                                                  
*              CALCULATE PENSION AND HEALTH CONTRIBUTION                        
         SPACE 1                                                                
PNHCALC  NTR1                                                                   
         TM    TGUSTYST,NOPNH      TEST FOR EXCLUDED USE TYPES                  
         BO    PNHCX                                                            
*        TM    TGUNEQU,NON         EXCLUDE NON-UNION MEMBER                     
         GOTOR UNITEST,DMCB,TGUNEQUS,NON,0,0,0                                  
         BO    PNHCX                                                            
         TM    TCSTAT2,TCSTCAN$    EXCLUDE CANADIAN $ PAYMENT                   
         BO    PNHCX                                                            
         TM    TCSTAT2,TCSTEURO    EXCLUDE EURO PAYMENT                         
         BO    PNHCX                                                            
         SPACE                                                                  
         TM    TCINPUT,TCINPNH     IF THERE'S P&H BASIS OVERRIDE                
         BZ    PNHC5                                                            
         L     R1,TCSUBPNH         USE IT FOR BASIS                             
         B     PNHC11                                                           
         SPACE 1                                                                
PNHC5    L     R1,TCPAY            ELSE USE PAYMENT AMOUNT AS BASIS             
*                                                                               
         ICM   RF,15,TCCAADNS      SUBTRACT ADDITIONAL AMOUNT NOT               
         SR    R1,RF               SUBJECT TO P&H FROM PAYMENT AMOUNT           
*                                                                               
         CLI   TGUSEQU,USOP        IF SOP USE                                   
         BNE   PNHC8                                                            
         CLI   TCEXPICD,C'1'       AND INCLUDE CODE IS FOR WARDROBE FEE         
         BNE   PNHC8                                                            
         A     R1,TCEXP            ADD WARDROBE FEE TO SUBJ TO P&H              
         SPACE 1                                                                
*NHC8    TM    TGUNEQU,AFM         FOR MUSICIANS                                
PNHC8    LR    R0,R1                                                            
         GOTOR UNITEST,DMCB,TGUNEQUS,AFM,0,0,0                                  
         LR    R1,R0                                                            
         BZ    PNHC10                                                           
         TM    TCSTAT,TCSTAFMP     IF THERE'S SUBJ TO P&H FOR AFM               
         BZ    PNHC10                                                           
         L     R1,TCAFMSPH         USE IT FOR BASIS                             
         SPACE 1                                                                
         CLI   TCAPPLCD,APPLWSPU   TEST APPLIED CODE NOT WSP UPGRADE            
         BE    PNHC10                                                           
         CLI   TCAPPLCD,APPLCAB    AND NOT CABLE                                
         BE    PNHC10                                                           
         S     R1,TCAPPLCR         - APPLIED AMOUNT                             
         BNM   *+6                                                              
         XR    R1,R1               INSURE IT DOESN'T GO NEGATIVE                
         SPACE 1                                                                
PNHC10   ST    R1,TCSUBPNH         R1=AMOUNT SUBJECT TO P&H                     
PNHC11   OC    TCATMTOT,TCATMTOT   IF WE ARE PAYING TIMESHEETS,                 
         BZ    PNHC12                                                           
         TM    TCINPUT,TCINPNH     TEST WE ALREADY HAVE PNH AMT                 
         BO    PNHC12                                                           
         L     R1,TCSUBPNH         SUBTRACT ANY NON-SUBJECT TO PNH              
         S     R1,TCNOTSPH         AMOUNTS (MEAL PENALTY)                       
         ST    R1,TCSUBPNH                                                      
PNHC12   LH    R0,TCPNHR           R0=RATE                                      
         BAS   RE,MULTR0           CALCULATE P&H                                
         ST    R1,TCPNH                                                         
         SPACE 1                                                                
PNHCX    B     XIT                                                              
         EJECT                                                                  
*&&DO                                                                           
*              ROUTINE RETURNS CC EQUAL IF THIS CAT GETS FIXED H&W 3X           
TSTHNW3X NTR1                                                                   
         LA    R2,HNW3XTAB         R2=A(TABLE OF CATS THAT GET H&W 3X)          
THNW3X5  CLI   0(R2),0             TEST END OF TABLE                            
         BE    NO                                                               
         CLC   TGCAEQU,0(R2)       SEE IF CATEGORY MATCHES TABLE                
         BE    YES                                                              
         LA    R2,1(R2)            BUMP TO NEXT ENTRY                           
         B     THNW3X5             KEEP LOOPING                                 
*&&                                                                             
         EJECT                                                                  
*              ERRORS/EXITS                                                     
         SPACE 1                                                                
ERR3     MVI   TCERROR,TCERIUSE                                                 
ERR4     MVI   TCERROR,TCERICAT                                                 
         B     ERRX                                                             
ERR5     MVI   TCERROR,TCERHDLR                                                 
         SPACE 1                                                                
ERRX     L     RD,TCRD                                                          
         B     NO                                                               
         SPACE 3                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE                                                                  
XITR4    XIT1  REGS=(R4)           RETURN R4                                    
         SPACE 3                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, TABLES, ETC.                                          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         SPACE 1                                                                
*              TABLE OF CATEGORIES THAT GET FIXED H&W 3X                        
*&&DO                                                                           
HNW3XTAB DS    0C                                                               
         DC    AL1(CTAMC,CTLAC,CTLAO,CTLOC,CTOMC,0)                             
*&&                                                                             
         SPACE 3                                                                
*              SESSION CALCULATION TABLE                                        
         SPACE 1                                                                
BSSTAB   DS    0H                  TELEVISION                                   
         DC    AL2(BSSSP-TACALC)   SPOTS / DAYS                                 
         DC    AL2(BSSOT-TACALC)   OVERTIME HOURS                               
         DC    AL2(BSSDT-TACALC)   DOUBLE TIME HOURS                            
         DC    AL2(BSSTRV-TACALC)  TRAVEL TIME HOURS/MINUTES                    
         DC    AL2(BSSPDW-TACALC)  PRIOR-DAY WARDROBE HOURS/MINUTES             
         DC    AL2(BSSTG-TACALC)   TAGS                                         
         DC    AL2(BSSNP-TACALC)   NIGHT PREMIUM                                
         DC    AL2(BSSMP-TACALC)   MEAL PENALTY                                 
         DC    AL2(BSS16-TACALC)   16-HOUR RULE                                 
         DC    AL2(BSSAP-TACALC)   ADD TO PAYMENT AMOUNT (SMOKE PAY)            
         DC    AL2(BSSAJ-TACALC)   ADD TO PAYMENT AMOUNT (ADJUSTMENT)           
         DC    AL2(BSSFS-TACALC)   ADD TO PAYMENT AMOUNT (FRI/SAT HRS)          
         DC    AL2(BSSRPV-TACALC)  ADD TO PAYMENT AMOUNT (REST PER VIO)         
         DC    X'FF'                                                            
         SPACE 1                                                                
BSMTAB   DS    0H                  MUSIC                                        
         DC    AL2(BSMHM-TACALC)   HOURS/MINUTES                                
         DC    X'FF'                                                            
*                                                                               
IMSTAB   DS    0H                  MUSIC                                        
         DC    AL2(IMSHM-TACALC)   HOURS/MINUTES                                
         DC    X'FF'                                                            
*                                                                               
IDSTAB   DS    0H                  INDUSTRIAL OFF CAMERA SESSION                
         DC    AL2(IDSHM-TACALC)   HOURS/MINUTES                                
         DC    X'FF'                                                            
*                                                                               
RTKTAB   DS    0H                  INDUSTRIAL RETAKES                           
         DC    AL2(RTKHM-TACALC)   HOURS/MINUTES                                
         DC    AL2(RTKTR-TACALC)   TRAVEL TIME HOURS/MINUTES                    
         DC    X'FF'                                                            
*                                                                               
DIOTAB   DS    0H                  INDUSTRIAL AUDIO SESSIONS                    
         DC    AL2(DIOHM-TACALC)   HOURS/MINUTES                                
         DC    AL2(DIORT-TACALC)   RETAKES                                      
         DC    X'FF'                                                            
*                                                                               
IVRTAB   DS    0H                  INTERACTIVE VOICE SESSIONS                   
         DC    AL2(IVRHM-TACALC)   HOURS/MINUTES                                
         DC    AL2(IVRTR-TACALC)   TRAVEL TIME HOURS/MINUTES                    
         DC    X'FF'                                                            
*                                                                               
BSRTAB   DS    0H                  RADIO                                        
         DC    AL2(BSRSP-TACALC)   SPOTS / HOURS.MINUTES                        
         DC    AL2(BSRTG-TACALC)   TAGS                                         
         DC    X'FF'                                                            
*                                                                               
BSCTAB   DS    0H                  CANADA                                       
         DC    AL2(BSSSP-TACALC)   SPOTS / DAYS                                 
         DC    AL2(BSSOT-TACALC)   OVERTIME HOURS                               
         DC    AL2(BSSDT-TACALC)   DOUBLE TIME HOURS                            
         DC    AL2(BSSTRV-TACALC)  TRAVEL TIME HOURS/MINUTES                    
         DC    AL2(BSSPDW-TACALC)  PRIOR-DAY WARDROBE HOURS/MINUTES             
         DC    AL2(BSSTG-TACALC)   TAGS                                         
         DC    AL2(BSSNP-TACALC)   NIGHT PREMIUM                                
         DC    AL2(BSSMP-TACALC)   MEAL PENALTY                                 
         DC    AL2(BSSAJ-TACALC)   ADD TO PAYMENT AMOUNT (ADJUSTMENT)           
         DC    X'FF'                                                            
         SPACE 2                                                                
         EJECT                                                                  
KSOVPRIN DC    F'7155'             OVTIME RATE - KS ADDEND - PRINCIPALS         
         DC    F'5320'             RATE FOR '94                                 
         DC    F'5700'             RATE FOR '97                                 
         DC    F'5955'             RATE FOR '02                                 
         DC    F'6370'             RATE FOR '04 (CALCULATE ON CAMERA)           
         DC    F'6750'             RATE FOR '06 (CALCULATE ON CAMERA)           
KSOVTON  DC    F'3641'             NON-PRINCIPALS ON CAMERA                     
         DC    F'3265'             RATE FOR '94                                 
         DC    F'3500'             RATE FOR '97                                 
         DC    F'3660'             RATE FOR '02                                 
         DC    F'3660'             RATE FOR '04 (CALCULATE IT INSTEAD)          
KSOVTOFF DC    F'3641'             NON-PRINCIPALS OFF                           
         DC    F'2780'             RATE FOR '94                                 
         DC    F'3000'             RATE FOR '97                                 
         DC    F'3135'             RATE FOR '02                                 
         DC    F'3355'             RATE FOR '04                                 
         DC    F'3555'             RATE FOR '06                                 
         SPACE 3                                                                
KSFRSTAG DC    F'29648'            FIRST TAG RATE FOR KS ADDEN OFF CAM          
KSFSTG94 DC    F'21815'            RATE FOR '94                                 
KSFSTG97 DC    F'23600'            RATE FOR '97                                 
KSFSTG02 DC    F'24660'            RATE FOR '02                                 
KSFSTG04 DC    F'26385'            RATE FOR '04                                 
KSFSTG06 DC    F'27970'            RATE FOR '06                                 
KSFRSTGR DC    F'16796'            RADIO                                        
KSFSTR94 DC    F'11755'            RATE FOR '94                                 
KSFSTR97 DC    F'12700'            RATE FOR '97                                 
KSFSTR02 DC    F'13970'            RATE FOR '02                                 
KSFSTR04 DC    F'14950'            RATE FOR '04                                 
KSFSTR06 DC    F'15845'            RATE FOR '06                                 
         SPACE 3                                                                
GAFSTRAG DC    F'9500'             FIRST TAG RATE FOR GA ADDEN OFF CAM          
*                                                                               
         DROP  RA,R7               DROP SECONDARY BASE REGISTERS                
         EJECT                                                                  
*              CALCULATE INSURANCE AND RETIREMENT CONTRIBUTION                  
         SPACE 1                                                                
INRCALC  NTR1  BASE=*,LABEL=*                                                   
         CLI   TGUSEQU,UCPN        IF CPN PAYMENT                               
         BE    INRCX                                                            
         CLI   TGUSEQU,UPEN        OR PEN PAYMENT, DO NOT TAKE I&R              
         BE    INRCX                                                            
*        TM    TGUNEQU,UDA         ONLY FOR UDA                                 
         GOTOR UNITEST,DMCB,TGUNEQUS,UDA,0,0,0                                  
         BO    INRC5                                                            
*        TM    TGUNEQU,ACT         AND ACT                                      
         GOTOR UNITEST,DMCB,TGUNEQUS,ACT,0,0,0                                  
         BZ    INRCX                                                            
         SPACE 1                                                                
INRC5    L     R1,TCPAY            BASE ON PAYMENT AMOUNT                       
         TM    TCINPUT,TCINPNH     TEST WE HAVE BASIS OVERRIDE                  
         BZ    *+8                                                              
         L     R1,TCSUBPNH         USE IT                                       
         SPACE 1                                                                
         LTR   R2,R1               R1 & R2=BASIS FOR I&R AMOUNT                 
         BZ    INRCX               GET OUT IF NO BASIS                          
         SPACE 1                                                                
*&&UK                              NO-OP FOR 99 CONTRACT                        
*        TM    TGUNEQU,ACT         IF ACT                                       
         LR    R3,R1                                                            
         GOTOR UNITEST,DMCB,TGUNEQUS,ACT,0,0,0                                  
         LR    R1,R3                                                            
         BZ    INRC7                                                            
         LA    R0,2675             SET RATE FOR ACTRAWORKS CONTRIBUTION         
         BAS   RE,MULTACT          CALCULATE AMOUNT                             
         ST    R1,TCACTWRK                                                      
         LR    R1,R2               SET R1=BASIS                                 
*&&                                                                             
         SPACE 1                                                                
INRC7    LA    R0,1200             SET I&R RATE FOR EMPLOYER CONTRBTN           
         CLC   TCPCYCS,=X'A40601'  ON OR AFTER JUNE 2004                        
         BL    *+8                                                              
         LA    R0,1300             ELSE 13%                                     
*                                                                               
         OC    TCPCYCS,TCPCYCS     IF NO CYCLE START DATE                       
         BNZ   INRC7A                                                           
         CLI   TGYREQU,CN04        AND CONTRACT YR IS 04 OR HIGHER,             
         BL    INRC7A                                                           
         LA    R0,1300             USE 13%                                      
*                                                                               
*        TM    TGUNEQU,UDA         12% IF UNION UDA                             
INRC7A   LR    R3,R1                                                            
         GOTOR UNITEST,DMCB,TGUNEQUS,UDA,0,0,0                                  
         LR    R1,R3                                                            
         BO    INRC8                                                            
         LA    R0,1000             ELSE 10%                                     
         CLC   TCPCYCS,=X'A40701'  ON OR AFTER JULY 2004                        
         BL    INRC7B                                                           
         LA    R0,1100             ELSE 11%                                     
         CLC   TCPCYCS,=X'A70701'  ON OR AFTER JULY 2007                        
         BL    INRC7B                                                           
         LA    R0,1200             ELSE 12%                                     
*                                                                               
INRC7B   OC    TCPCYCS,TCPCYCS     IF NO CYCLE START DATE                       
         BNZ   INRC8                                                            
         CLI   TGYREQU,CN03        AND CONTRACT YR IS 03 OR HIGHER,             
         BL    INRC8                                                            
         LA    R0,1100             USE 11%                                      
         CLI   TGYREQU,CN07        AND CONTRACT YR IS 07 OR HIGHER,             
         BL    INRC8                                                            
         LA    R0,1200             USE 12%                                      
*                                                                               
INRC8    BAS   RE,MULTR00          CALCULATE I&R                                
         ST    R1,TCINR                                                         
         SPACE 1                                                                
         TM    TCINPUT2,TCINMDED   TEST WE ALREADY HAVE MISC. DED               
         BO    INRC10                                                           
         LR    R1,R2               SET R1=BASIS                                 
         LA    R0,450              SET I&R RATE FOR EMPLOYEE CONTRBTN           
*        TM    TGUNEQU,UDA         4.5% IF UNION UDA                            
         LR    R3,R1                                                            
         GOTOR UNITEST,DMCB,TGUNEQUS,UDA,0,0,0                                  
         LR    R1,R3                                                            
         BO    *+8                                                              
         LA    R0,400              ELSE 4%                                      
         SPACE 1                                                                
         BAS   RE,MULTR00          CALCULATE I&R                                
         ST    R1,TCMDED           SAVE AS MISC. DED.                           
         SPACE 1                                                                
INRC10   TM    TCINPUT2,TCINDUES   TEST WE ALREADY HAVE UNION DUES/FEES         
         BO    INRCX                                                            
         TM    TCCASTST,TCCADUES   AND IF MUST WITHHOLD ACTRA DUES              
         BZ    INRCX               FOR MEMBERS                                  
         SPACE                                                                  
         CLI   TGUSEQU,UPEN        AND IS NOT PEN OR CPN USE                    
         BE    INRCX                                                            
         CLI   TGUSEQU,UCPN                                                     
         BE    INRCX                                                            
         LR    R1,R2               SET R1=BASIS                                 
         LA    R0,200              SET UNION DUES RATE FOR DEDUCTION            
         CLC   TCPCYCS,=X'A80301'  ON OR AFTER MARCH 01 2008                    
         BL    *+8                                                              
         LA    R0,225              SET UNION DUES RATE FOR DEDUCTION            
         BAS   RE,MULTR00          CALCULATE I&R                                
         ST    R1,TCDUES           SAVE UNION DUES                              
         B     INRCX                                                            
         SPACE                                                                  
*&&UK                              NO-OP 3/99                                   
*NRC15   TM    TGUNEQU,ACT         IF NOT ACT MEMBER, BUT IS UNION ACT          
INRC15   LR    R3,R1                                                            
         GOTOR UNITEST,DMCB,TGUNEQUS,AFT,0,0,0                                  
         LR    R1,R3                                                            
         BZ    INRCX                                                            
         TM    TGUSSTAT,SESSION    AND PAYING A RESIDUAL (AND DOR)              
         BO    INRCX                                                            
         LR    R1,R2               SET R1=BASIS                                 
         LA    R0,1070             SET UNION FEES RATE FOR DEDUCTION            
         BAS   RE,MULTR00          CALCULATE I&R                                
         ST    R1,TCDUES           SAVE UNION FEES                              
*&&                                                                             
         SPACE 1                                                                
INRCX    J     XIT                                                              
         SPACE 3                                                                
MULTR00  MR    R0,R0               USE R0                                       
         D     R0,=F'5000'                                                      
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         BR    RE                  RETURN ANSWER IN R1                          
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
*              CALCULATE ACTRA ACCIDENT ON SET                                  
*---------------------------------------------------------------------          
AOSCALC  NTR1  BASE=*,LABEL=*                                                   
         XC    ELEMENT,ELEMENT                                                  
                                                                                
*        TM    TGUNEQU,ACT         ACTRA ONLY                                   
         GOTOR UNITEST,DMCB,TGUNEQUS,ACT,0,0,0                                  
         BZ    AOSCX                                                            
         TM    TGUSSTAT,SESSION    SESSION PAYMENT ONLY                         
         BZ    AOSCX                                                            
         CLI   TGUSEQU,UCAU        (BUT NOT CAU)                                
         BE    AOSCX                                                            
         CLI   TCW4TYPE,TAW4TYES   CAN'T BE AN ESTATE                           
         BE    AOSCX                                                            
*                                                                               
         USING TACAD,R4                                                         
         OC    TCACAST,TCACAST                                                  
         BZ    AOSC030                                                          
         L     R4,TCACAST                                                       
         B     AOSC050                                                          
*                                                                               
AOSC030  L     R4,AIO              CAST RECORD POINTED BY AIO                   
AOSC050  CLI   0(R4),0             NO CAST?  COULD BE HYPO CAST                 
         BNE   AOSC060                                                          
         CLI   RECNUM,ES           BUT HAS TO BE FROM ESTIMATING                
         BE    *+6                                                              
         DC    H'0'                NO, ABEND                                    
*                                                                               
         MVC   TCAOSUNT,=C'QC '    DEFAULT QUEBEC FOR HYPO                      
         MVC   TCACTAOS,TCGROSS                                                 
         B     AOSCX                                                            
*                                                                               
AOSC060  MVI   ELCODE,TACAELQ                                                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST HAVE CAST DETAIL ELEMENT                
*                                                                               
         LA    R1,AOSMUST          IF TAX UNIT IS ONE OF THESE                  
AOSC100  CLI   0(R1),0                                                          
         BE    AOSC200                                                          
         CLC   TACAUNIT,0(R1)      MATCH?                                       
         BE    AOSC500             YES, CHARGE AOS                              
         AHI   R1,7                                                             
         B     AOSC100                                                          
*                                                                               
AOSC200  CLI   TCW4TYPE,TAW4TYIN   INDIVIDUAL                                   
         BE    AOSC210                                                          
         CLI   TCW4TYPE,TAW4TYFO   FOREIGNER                                    
         BE    AOSC210                                                          
         CLI   TCW4TYPE,TAW4TYCA   CANADIAN                                     
         BNE   AOSCX               NONE OF THESE, SO DON'T CHARGE AOS           
*                                                                               
AOSC210  LA    R1,AOSCOND                                                       
AOSC220  CLI   0(R1),0                                                          
         BE    AOSCX                                                            
         CLC   TACAUNIT,0(R1)      MATCH?                                       
         BE    AOSC300             YES, CHARGE AOS                              
         AHI   R1,7                                                             
         B     AOSC220                                                          
*                                                                               
AOSC300  CLI   TCW4TYPE,TAW4TYCA   CANADIAN                                     
         BNE   AOSC500             NOT CANADIAN                                 
*                                                                               
         LR    R3,R4               SAVE REGISTERS FOR LATER                     
         LR    R5,R1                                                            
         L     RE,AIO2             SAVE KEY FOR WHAT IS ORIGINALLY              
         MVC   ELEMENT(L'KEY),0(RE)      IN AIO2                                
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'20',0)  GET W4 RECORD                     
         L     R4,AIO2             W4 RECORD SHOULD BE IN AIO1                  
         CLI   0(R4),TLW4CDQ                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   ELCODE,TAA2ELQ      FIND ADDRESS                                 
         BRAS  RE,GETEL                                                         
         BNE   AOSC490                                                          
         USING TAA2D,R4                                                         
         CLC   TAA2CTRY,=C'CA'     CANADIAN RESIDENT?                           
         BNE   AOSC490             NO, OK TO CHARGE AOS                         
         LA    RF,AOSCOND                                                       
AOSC400  CLI   0(RF),0             NOT A RESIDENT OF BC,NB OR NL                
         BE    AOSC490             OK TO CHARGE AOS                             
         CLC   TAA2ST,0(RF)        MATCH?                                       
         BE    AOSCX               YES, RESIDENT, DON'T CHARGE AOS              
         AHI   RF,7                BUMP TO NEXT ONE                             
         B     AOSC400                                                          
*                                                                               
         USING TACAD,R4                                                         
AOSC490  LR    R4,R3               RESTORE R4                                   
         LR    R1,R5               RESTORE R1                                   
AOSC500  L     R0,TCGROSS          CALC 1% FOR AOS                              
         ST    R0,TCACTAOS                                                      
         MVC   TCAOSSSN,3(R1)      SAVE SSN FOR AOS CHECK                       
         CLC   TCAOSUNT,=C'   '    DID WE SAVE UNIT BEFORE?                     
         BH    *+10                                                             
         MVC   TCAOSUNT,TACAUNIT   SAVE THE CAST UNIT ONCE                      
*                                                                               
AOSCX    OC    ELEMENT,ELEMENT                                                  
         BZ    AOSCXIT                                                          
         MVC   KEY,ELEMENT         RESTORE KEY FROM AIO2                        
         GOTO1 HIGH                                                             
         MVC   AIO,AIO2            RESTORE RECORD IN AIO2                       
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
AOSCXIT  XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
*                PRV    SSN IN HEX                                              
AOSMUST  DC    C'AB ',X'0001CA6D'  MUST CHARGE AOS ON THESE PROVINCES           
         DC    C'MB ',X'0001CA6F'                                               
         DC    C'NS ',X'0001CA75'                                               
         DC    C'ON ',X'0001B8CB'                                               
         DC    C'PE ',X'0001CA70'                                               
         DC    C'QC ',X'0001CA6E'                                               
         DC    C'SK ',X'0001CA71'                                               
         DC    X'00'                                                            
*                                                                               
AOSCOND  DC    C'BC ',X'0001CA72'   CONDITIONAL AOS BASED ON W4 TYPE            
         DC    C'NB ',X'0001CA74'                                               
         DC    C'NL ',X'0001CA73'                                               
         DC    X'00'                                                            
         EJECT                                                                  
*---------------------------------------------------------------------          
*              ROUTINE TO ESTABLISH PAY RATES                                   
*---------------------------------------------------------------------          
GETRATE  NTR1  BASE=*,LABEL=*                                                   
         ZIC   R3,TCROW            ROW NUMBER                                   
         SLL   R3,2                *4 (L'EACH ROW)                              
         BRAS  RE,CHKNEW           IF USING NEW STYLE RATE TABLES               
         BNE   *+8                                                              
         AHI   R3,8                +8 FOR EXTRA BYTES IN HEADER                 
*                                  R3=DISP. TO ROW                              
*                                                                               
         LA    R4,4(R3)            +4 (L'ROW)                                   
         STC   R4,TCMINLTB         = MINIMUM L'TABLE                            
*                                                                               
         USING USETBLD,R1                                                       
         XR    R4,R4               R4=RUNNING TOTAL                             
         L     R2,TCUSETBL         R2=A(USE RATE TABLE)                         
         MVI   TGDUB,0                                                          
         LA    R1,TGDUB            R1=A(TGDUB)=WHERE HEADING WILL BE            
*                                                                               
GETR2    CLI   0(R2),X'FF'         END OF TABLE                                 
         BE    GETRX                                                            
         MVC   TGDUB+1(4),0(R2)    SAVE ENTRY TO TGDUB+1                        
         BRAS  RE,CHKNEW           IF USING NEW STYLE RATE TABLES               
         BNE   *+10                                                             
         MVC   TGDUB,0(R2)         SAVE ENTRY TO TGDUB (2 BYTE USENUM)          
         CLC   TCUSENUM,USETBNUM   MATCH ON USE NUMBER FROM TCUSELUT            
         BNE   GETR4                                                            
*                                                                               
         TM    TGUSSTA3,ADDENUSE   IF ADDENDUM USE                              
         BZ    GETR2Y                                                           
         CLC   TCADDST,=C'NW'      AND STATE IS NW                              
         BNE   GETR2Y                                                           
         TM    TCPAYST,TCSINGLE    AND USING SINGLE MARKET RATE                 
         BZ    GETR2Y                                                           
         OC    TCUNITS,TCUNITS                                                  
         BNZ   GETR2X                                                           
         CLI   USETBYNM-USETBLD(R2),1                                           
         BE    GETR2X                                                           
         B     GETR4                                                            
*                                                                               
GETR2Y   CLI   TGUSEQU,UCBL        IF CABLE USE AND                             
         BE    *+8                                                              
         CLI   TGUSEQU,USCB        AND SPANISH CABLE                            
         BNE   GETR2Z                                                           
         CLI   TGYREQU,CN03        CONTRACT >=2003                              
         BNL   GETR2X                                                           
         CLI   TGYREQU,CN00        CONTRACT >= 2000, BUT < 2003                 
         BL    GETR2X                                                           
         CLC   TCPCYCS,=X'A11029'  YEAR 1                                       
         BH    GETR2Y2                                                          
         CLI   USETBYNM-USETBLD(R2),1                                           
         BNE   GETR4                                                            
         B     GETR2X                                                           
*                                                                               
GETR2Y2  CLC   TCPCYCS,=X'A21029'  YEAR 2                                       
         BH    GETR2Y3                                                          
         CLI   USETBYNM-USETBLD(R2),2                                           
         BNE   GETR4                                                            
         B     GETR2X                                                           
*                                                                               
GETR2Y3  CLI   USETBYNM-USETBLD(R2),3                                           
         BNE   GETR4                                                            
*                                                                               
GETR2Z   CLI   TGUSEQU,UCSS        IF CREW SESSION USE                          
         BNE   GETR2X                                                           
         CLI   TGYREQU,CN00        CONTRACT >= 2000,                            
         BL    GETR2X                                                           
         CLC   TCPCYCS,=X'A30531'  YEAR 1                                       
         BH    GETR2Z2                                                          
         CLI   USETBYNM-USETBLD(R2),1                                           
         BNE   GETR4                                                            
         B     GETR2X                                                           
*                                                                               
GETR2Z2  CLC   TCPCYCS,=X'A40531'  YEAR 2                                       
         BH    GETR2Z3                                                          
         CLI   USETBYNM-USETBLD(R2),2                                           
         BNE   GETR4                                                            
         B     GETR2X                                                           
*                                                                               
GETR2Z3  CLI   USETBYNM-USETBLD(R2),3                                           
         BNE   GETR4                                                            
*                                                                               
GETR2X   TM    TCPAYST,TCITN                                                    
         BO    GETR2XX                                                          
         OC    TCTUSES,TCTUSES     IF THERE ARE USE NUMBERS                     
         BZ    GETR6                                                            
         OC    TCUNITS,TCUNITS     AND NO UNITS                                 
         BNZ   GETR6                                                            
GETR2XX  XC    FULL,FULL           MUST MATCH USE NUMBER RANGE                  
         BRAS  RE,CHKNEW                                                        
         BE    GETR3                                                            
         MVC   FULL+1(1),USETBSTO                                               
         MVC   FULL+3(1),USETBNDO                                               
         B     GETR3X                                                           
*                                                                               
GETR3    MVC   FULL,USETBST        REDEFINE RANGE INTO 4 BYTES                  
GETR3X   TM    TCPAYST2,TCVNR+TCVRE+TCEDS                                       
         BZ    *+10                                                             
         MVC   FULL,=X'000100FF'   FAKE OUT TO MATCH BSS                        
         CLI   FULL+3,X'FF'                                                     
         BNE   *+8                                                              
         MVI   FULL+2,X'FF'                                                     
         CLC   TCUSEN,FULL         USE NUMBER MUST FIT IN RANGE                 
         BL    GETR4                                                            
         CLC   TCUSEN,FULL+2                                                    
         BH    GETR4                                                            
         OI    TCSTAT,TCSTUSES     SET BY USES INDICATOR                        
         B     GETR9                                                            
*                                                                               
GETR4    SR    R0,R0               BUMP TO NEXT ENTRY                           
         IC    R0,USETBLNO                                                      
         BRAS  RE,CHKNEW                                                        
         BNE   *+8                                                              
         ICM   R0,3,USETBLN                                                     
         AR    R2,R0                                                            
         B     GETR2                                                            
*                                                                               
GETR6    OC    TCUNITS,TCUNITS     IF THERE ARE UNITS                           
         BNZ   GETR7                                                            
         OC    TCINSRTS,TCINSRTS   OR INSERTS                                   
         BNZ   GETR7                                                            
         OC    TCTAGS,TCTAGS       OR TAGS                                      
         BNZ   GETR7                                                            
         OC    TCDEMO,TCDEMO       OR DEMOS, THEN ADD THEM UP                   
         BNZ   GETR7                                                            
*                                                                               
         OC    TCMAJORS,TCMAJORS   IF NO MAJORS AND NO UNITS                    
         BZ    GETR6A                                                           
         SR    R0,R0                                                            
         B     GETR6B                                                           
GETR6A   TM    TGUSTYST,UNITS      ON A USE THAT REQUIRES USES                  
         BNZ   GETRX               EXIT                                         
*                                                                               
GETR6B   OI    TCSTAT,TCSTUSES     ELSE SIMULATE BY USES                        
         B     GETR9                                                            
*                                                                               
GETR7    SR    R0,R0                                                            
         IC    R0,USETBSTO                                                      
         BRAS  RE,CHKNEW                                                        
         BNE   *+8                                                              
         ICM   R0,3,USETBST        APPLY INCREMENT FOR EACH UNIT/INS            
*                                  TIL ALL UNITS/INS ARE ACCOUNTED FOR          
         XC    HALF,HALF                                                        
         MVC   HALF+1(1),USETBNDO  HALF=MAX N'UNITS/INS FOR THIS ENTRY          
         BRAS  RE,CHKNEW                                                        
         BNE   *+10                                                             
         MVC   HALF,USETBEND       HALF=MAX N'UNITS/INS FOR THIS ENTRY          
         CLI   HALF+1,X'FF'                                                     
         BNE   *+10                                                             
         MVC   HALF,=X'7FFF'       INSURE ALL UNITS/INS ARE ADDED IN            
*                                                                               
         CLI   TGUSEQU,UTAG        TEST FOR TAG PAYMENT                         
         BNE   GETR7A                                                           
         ZIC   R5,TCTAGS           R5=TOTAL NUMBER OF TAGS FOR USE              
         OI    TCSTAT2,TCSTSTAG                                                 
         B     GETR7D                                                           
*                                                                               
GETR7A   ZIC   R5,TCDEMO           R5=TOTAL NUMBER OF DEMOS FOR USE             
         CLI   TGUSEQU,UDEM        TEST FOR DEM OR CDM PAYMENT                  
         BE    GETR7D                                                           
         CLI   TGUSEQU,USNA                                                     
         BE    GETR7D                                                           
         CLI   TGUSEQU,UCDM                                                     
         BE    GETR7D                                                           
*                                                                               
         LH    R5,TCUNITS          R5=TOTAL NUMBER OF UNITS FOR USE             
         CLI   TGUSEQU,UCBL        IF CBL, THEN SET R5=1000 MAX UNITS           
         BE    *+8                                                              
         CLI   TGUSEQU,USCB        OR SCB                                       
         BNE   GETR7C                                                           
         CHI   R5,1000                                                          
         BNH   GETR7D                                                           
         CLI   TGYREQU,CN00        NEW YEAR?                                    
         BNL   *+12                                                             
         LHI   R5,1000                                                          
         B     GETR7D                                                           
*                                                                               
         CLI   TGYREQU,CN13        NEW 2013 CONTRACT?                           
         BNL   GETR7B                                                           
         CHI   R5,2000             CONTRACT 2000 LIMITS 2000 MAX UNITS          
         BNH   GETR7D                                                           
         LHI   R5,2000                                                          
         B     GETR7C                                                           
*                                                                               
GETR7B   CHI   R5,3000             CONTRACT 2013 LIMITS 3000 MAX UNITS          
         BNH   GETR7D                                                           
         LHI   R5,3000                                                          
*                                                                               
GETR7C   CLI   TGUSEQU,UVAR        IF VAR OR VNW                                
         BE    GETR7C5                                                          
         CLI   TGUSEQU,UVNW                                                     
         BE    GETR7C5                                                          
         TM    TGUSTYST,INSERTS    OR IF INSERTS REQUIRED                       
         BZ    *+8                                                              
GETR7C5  LH    R5,TCINSRTS         R5=TOTAL NUMBER OF INSERTS FOR USE           
*                                                                               
GETR7D   TM    TGUSTYST,NOINCREM   DON'T CALCULATE INCREMENTALLY                
         BZ    GETR8                                                            
         TM    TGUSTYST,INCGE50    UNTIL REACHING 50                            
         BZ    *+12                                                             
         CHI   R0,50               IF GE 50 INCREMENT AS USUAL                  
         BNL   GETR8                                                            
         CH    R5,HALF             SKIP TO MAXIMUM ENTRY                        
         BH    GETR4                                                            
*                                                                               
GETR8    CR    R0,R5               R0=SMALLEST N'UNITS/INSRTS FOR THIS          
         BH    GETRX                  ENTRY OR NUMBER APPLIED SO FAR            
*                                  IF N'APPLIED GT N'PAYING - FINISHED          
*                                                                               
GETR9    BRAS  RE,CHKNEW                                                        
         BE    GETR9A                                                           
         CLC   USETBLNO,TCMINLTB   NOT IF TABLE NOT LONG ENOUGH                 
         BL    GETR12                                                           
         B     GETR9D                                                           
*                                                                               
GETR9A   CLC   USETBLN+1(1),TCMINLTB       NOT IF TABLE NOT LONG ENOUGH         
         BL    GETR12              (NO INCREMENT FOR GIVEN ROW)                 
*                                                                               
GETR9D   LA    RE,0(R2,R3)         RE=A(FULLWORD INCREMENT)                     
         CLI   0(RE),0             TEST THERE'S A MULTIPLICATION FACTOR         
         BNE   GETR10                                                           
         L     RF,0(RE)            NO - RF=RATE                                 
         BRAS  RE,DRINK            APPLY COLA (RETURNS RATE IN FULL)            
         A     R4,FULL             ADD TO TOTAL                                 
*                                                                               
         CLI   TGUSEQU,UCLA        IF USE IS NOT CLASS A                        
         BE    GETR12                                                           
         CLI   TGUSEQU,ULCB        IF USE IS NOT LOCAL CABLE                    
         BE    GETR12                                                           
         CLI   TGUSEQU,UACB        IF USE IS NOT ADDENDUM CABLE                 
         BE    GETR12                                                           
*                                                                               
GETR9G   ST    R1,TGFULL           SAVE BREAKDOWN INFO                          
*        CLI   TGUSEQU,UFGR                                                     
*        BNE   GETR12                                                           
         TM    TCPAYST2,TCEDS                                                   
         BO    GETR9H                                                           
         TM    TGUSSTAT,SESSION                                                 
         BO    GETR12                                                           
         TM    TCPAYST2,TCVNR                                                   
         BO    GETR12                                                           
GETR9H   GOTOR SVBRKDWN,DMCB,(0,FULL),0                                         
         L     R1,TGFULL                                                        
         B     GETR12              CHECK FOR NEXT UNIT                          
*                                                                               
GETR10   L     RF,0(RE)            CALCULATE NEW AMOUNT                         
         SR    RE,RE                                                            
         SLDL  RE,8                RE=MULTIPLICATION FACTOR                     
         SRL   RF,8                RF=AMOUNT                                    
         MR    RE,RE               MULTIPLY                                     
*                                                                               
         CVD   RF,DUB              REDUCE RESULT                                
         SRP   DUB,62,5                                                         
         CVB   RF,DUB                                                           
*                                                                               
         BRAS  RE,DRINK            APPLY COLA (RETURNS RATE IN FULL)            
         A     R4,FULL             ADD TO TOTAL                                 
*                                                                               
         B     GETR9G              GO BACK TO SAVE DETAIL AND CONTINUE          
GETR12   TM    TCSTAT,TCSTUSES     IF BY USES                                   
         BO    GETRX               THEN FINISH UP                               
         TM    TGUSTYST,NOINCREM   NOT INCREMENTING                             
         BZ    GETR14                                                           
         TM    TGUSTYST,INCGE50    UNTIL REACHING 50                            
         BZ    GETRX                                                            
         CHI   R5,50               THEN FINISHED IF LE 50 UNITS                 
         BNH   GETRX                                                            
*                                                                               
GETR14   CH    R0,HALF             IF REACHED MAXIMUM FOR THIS ENTRY            
         BE    GETR4               THEN SKIP TO NEXT                            
         AHI   R0,1                ELSE ADD ONE                                 
         B     GETR8               AND TRY TO ADD IT IN                         
*                                                                               
GETRX    DS    0H                                                               
         ST    R1,TGFULL           SAVE BREAKDOWN INFO                          
*        CLI   TGUSEQU,UFGR                                                     
*        BNE   GETRX5                                                           
         TM    TGUSSTAT,SESSION                                                 
         BO    GETRX5                                                           
         TM    TGUSTYST,UNITS      UNITS?                                       
         BO    GETRX5              YES, SAVED THE BREAKDOWN ALREADY             
*                                                                               
GETRX3   ST    R4,WORK                                                          
         CLI   TGUSEQU,UCLA        IF USE IS NOT CLASS A                        
         BE    GETRX4                                                           
         CLI   TGUSEQU,ULCB        IF USE IS NOT LOCAL CABLE                    
         BE    GETRX4                                                           
         CLI   TGUSEQU,UACB        IF USE IS NOT ADDENDUM CABLE                 
         BE    GETRX4                                                           
         CLI   TGUSEQU,ULNA        IF USE IS NOT LATE NIGHT ABC                 
         BE    GETRX4                                                           
         CLI   TGUSEQU,ULNN        IF USE IS NOT LATE NIGHT NBC                 
         BE    GETRX4                                                           
         CLI   TGUSEQU,ULNC        IF USE IS NOT LATE NIGHT CBS                 
         BE    GETRX4                                                           
         CLI   TGUSEQU,ULNF        IF USE IS NOT LATE NIGHT FOX                 
         BE    GETRX4                                                           
         CLI   TGUSEQU,UPAX        IF USE IS NOT PAX                            
         BE    GETRX4                                                           
         TM    TCPAYST2,TCVNR      IF USE IS NOT VNR                            
         BO    GETRX4                                                           
         GOTOR SVBRKDWN,DMCB,(0,WORK),0                                         
GETRX4   L     R1,TGFULL                                                        
*                                                                               
GETRX5   DS    0H                                                               
         XC    WORK,WORK                                                        
         BRAS  RE,SWEETMLT         HANDLE SWEETENING AND MULTITRACKING          
         BRAS  RE,ADJCLA           CLASS A'S MAY NEED ADJUSTMENT                
         BRAS  RE,ADJDEMO          DEMOS MAY NEED ADJUSTMENT                    
*                                                                               
         OC    WORK(4),WORK                                                     
         BZ    GETRX6                                                           
         GOTOR SVBRKDWN,DMCB,('SBDADJST',WORK),(8,0)                            
*                                                                               
GETRX6   A     R4,TCGROSS          ADD TO TOTAL SO FAR                          
         ST    R4,TCGROSS                                                       
         NI    TCSTAT2,X'FF'-TCSTSTAG                                           
*                                                                               
         XIT1                                                                   
         DROP  R1                                                               
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
GETINDXT NTR1  BASE=*,LABEL=*                                                   
         L     RF,TCINDEXT         START WITH YEAR 1'S INDEXT                   
         LTR   RF,RF                                                            
         JZ    GINDXTXT                                                         
         CLC   TCPCYCS,=X'B61101'  CYCLE HAS TO BE NOV 1, 2016 OR LATER         
         JL    GINDXTXT                                                         
         AHI   RF,128              POINT TO YEAR 2'S INDEXT                     
GINDXTXT XIT1  REGS=(RF)                                                        
         EJECT                                                                  
*---------------------------------------------------------------------          
*              ROUTINE TO CHECK IF OT CEILINGS APPLY - INS ONLY                 
*              WORK+4, BYTE = 1 OVERTIME RATE                                   
*                             2 DOUBLE TIME RATE                                
*              WORK+8, AMOUNT BEFORE OVERTIME                                   
*              --------------------                                             
*              RETURNS R1 = RATE                                                
*---------------------------------------------------------------------          
OTCEILNG NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,GETINDXT                                                      
         LR    R5,RF                                                            
         USING TNDXD,R5                                                         
         LA    RF,TNDXPCL                                                       
         CLI   TCROW,1             DAY PERFORMER                                
         BE    OTC090                                                           
*                                                                               
         AHI   RF,12                                                            
         CLI   TCROW,3             3-DAY PERFORMER                              
         BE    OTC090                                                           
*                                                                               
         AHI   RF,12                                                            
         CLI   TCROW,4             WEEKLY PERFORMERS P5D AND P6D                
         BE    OTC090                                                           
         CLI   TCROW,5                                                          
         BNE   OTCX                                                             
*                                                                               
OTC090   CLC   WORK+8(4),0(RF)     SEE IF CEILING HAS BEEN PASSED               
         BL    OTCX                                                             
         AHI   RF,4                                                             
         CLI   WORK+4,1            WANT OVERTIME?                               
         BE    OTC95               YES, GET THAT CEILING RATE                   
         AHI   RF,4                BUMP TO DOUBLE TIME CEILING RATE             
OTC95    L     R1,0(RF)                                                         
OTCX     XIT1  REGS=(R1)                                                        
*                                                                               
         DROP  R5                                                               
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
*              ROUTINE TO ADJUST MUSIC PAYMENTS                                 
*---------------------------------------------------------------------          
ADJAFM   NTR1  BASE=*,LABEL=*                                                   
*        TM    TGUSXUNI,AFM        TEST MUSICIAN                                
         XC    TCDOUBL,TCDOUBL                                                  
         GOTOR UNITEST,DMCB,TGUSXUNS,AFM,0,0,0                                  
         BO    AAFMX                                                            
*                                                                               
         TM    TGCASTAT,LEADER+CONTRCTR  FOR LEADERS AND CONTRACTORS            
         BZ    AAFM0                                                            
         BAS   RE,DOUBLING         HANDLE DOUBLING BEFORE ADJUSTMENT            
         OC    TGDUB,TGDUB                                                      
         BZ    AAFM0                                                            
         MVC   TCDOUBL,TGDUB                                                    
*        GOTOR SVBRKDWN,DMCB,(0,TGDUB),(7,0)                                    
*                                                                               
AAFM0    CLI   OVERLAY,X'70'       IGNORE PERFORMERS GETTING CAN. RATES         
         BL    AAFM1               PHASES X'70' - X'78' CAN. RATES              
         CLI   OVERLAY,CANRATE9                                                 
         BH    AAFM1                                                            
         B     AAFM6                                                            
*                                                                               
         USING TACOD,R2                                                         
AAFM1    L     R2,TCATACO                                                       
         CLI   TGUSEQU,UFMU        FOR FMU                                      
         BNE   *+12                                                             
         CLI   TACOAFM,C'1'        IF CAST IS 1                                 
         BE    AAFM6               DON'T BOTHER WITH INCREASE %                 
*                                                                               
         LA    R2,AFMTAB           ADJUSTMENT TABLE FOR MUSICIANS               
         CLI   TGUSEQU,UIMS        DIFFERENT TABLE FOR IMS                      
         BNE   *+8                                                              
         LA    R2,AFMIMSTB                                                      
AAFM2    CLI   0(R2),X'FF'                                                      
         BE    AAFM6                                                            
         CLC   TGCAEQU,2(R2)       FIND FACTOR FOR CATEGORY                     
         BE    *+12                                                             
         LA    R2,L'AFMTAB(R2)                                                  
         B     AAFM2                                                            
*                                                                               
         LH    R1,0(R2)            INCREASE PERCENTAGE                          
         MHI   R1,100                                                           
         BAS   RE,MLTSCAL          CALCULATE ADJUSTMENT (RETURNS IN R1)         
         A     R1,TCGROSS          ADD BACK TO GROSS                            
         CLI   TGYREQU,CN14        NO ROUNDING FOR '14                          
         BE    AAFM4                                                            
         CLI   TGUSEQU,UIMS        NO ROUNDING FOR IMS                          
         BE    *+8                                                              
         BRAS  RE,NICKL            ROUND TO NEAREST NICKEL                      
AAFM4    ST    R1,TCGROSS          NOW HAVE ADJUSTED GROSS                      
*                                                                               
AAFM6    TM    TGCASTAT,LEADER+CONTRCTR  EXCEPT FOR LEADERS/CONTRACTORS         
         BNZ   AAFM7               HANDLE DOUBLING AFTER ADJUSTMENT             
         BAS   RE,DOUBLING         (SAVES IN TGDUB)                             
         OC    TGDUB,TGDUB                                                      
         BZ    AAFM7                                                            
         MVC   TCDOUBL,TGDUB                                                    
*        GOTOR SVBRKDWN,DMCB,(0,TGDUB),(7,0)                                    
*                                                                               
AAFM7    TM    TGUSSTAT,SESSION                                                 
         BO    AAFM8                                                            
         GOTOR SVBRKDWN,DMCB,('SBDADJST',TCGROSS),0                             
*                                                                               
AAFM8    L     R1,TCGROSS                                                       
         A     R1,TGDUB            ADD DOUBLING INCREASE                        
         ST    R1,TCGROSS          NOW HAVE FULLY ADJUSTED GROSS                
AAFMX    XIT1                                                                   
         EJECT                                                                  
*---------------------------------------------------------------------          
*              SPECIAL ROUTINE FOR MUSICIAN DOUBLING                            
*---------------------------------------------------------------------          
DOUBLING NTR1                                                                   
         XC    TGDUB,TGDUB                                                      
         CLI   TGUSEQU,UFGM        NOT FOR FGM USE                              
         BE    AAFMX                                                            
         MVC   HALF,=H'1500'                                                    
         MVC   FULL,=F'3000'                                                    
         CLI   TGUSEQU,UIMS                                                     
         BNE   *+16                                                             
         MVC   HALF,=H'2000'                                                    
         MVC   FULL,=F'5000'                                                    
*                                                                               
         ZIC   R1,TCCADBL          R1=N'DOUBLES (EBCDIC)                        
         SLL   R1,28               SCRUNCH OFF HOBS                             
         SRL   R1,28                                                            
         LTR   R1,R1               TEST ANY DOUBLES                             
         BZ    AAFMX                                                            
         BCTR  R1,0                IGNORE 1ST DOUBLE FOR NOW                    
         MH    R1,HALF             DOUBLES AFTER 1 ARE AT 15% OR 20%            
         A     R1,FULL             1ST DOUBLE IS 30% OR 50%                     
         BRAS  RE,MLTSCAL          MULTIPLY BY SCALE (RETURNS IN R1)            
         CLI   TGYREQU,CN14        NO ROUNDING FOR '14                          
         BE    DBL20                                                            
         CLI   TGUSEQU,UIMS        NO ROUNDING FOR IMS                          
         BE    *+8                                                              
         BRAS  RE,NICKL            ROUND TO NEAREST NICKEL                      
DBL20    ST    R1,TGDUB            SAVE IN GLOBAL STORAGE                       
         B     AAFMX                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
       ++INCLUDE TASYSAFM                                                       
         EJECT                                                                  
*---------------------------------------------------------------------          
NICKL    NTR1  BASE=*,LABEL=*                                                   
         AHI   R1,2                ADD 2                                        
         XR    R0,R0                                                            
         D     R0,=F'5'            DIVIDE BY FIVE                               
         M     R0,=F'5'            MULTIPLY BY FIVE                             
         LTR   R1,R1               ADD 1 IF NEGATIVE                            
         BNM   *+8                                                              
         AHI   R1,1                                                             
         XIT1  REGS=(R1)                                                        
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
MLTSCAL  NTR1  BASE=*,LABEL=*                                                   
         M     R0,TCGROSS          USE SCALE                                    
         D     R0,=F'5000'                                                      
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         XIT1  REGS=(R1)                                                        
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
*              ROUTINE TO ADJUST SHORT VERSION CLASS A PAYMENTS                 
*              R4=AMOUNT TO ADJUST, RETURNS RESULT IN R4                        
*---------------------------------------------------------------------          
ADJCLA   NTR1  BASE=*,LABEL=*                                                   
         CLI   TGYREQU,CN85        1985 CONTRACT YEAR OR LATER                  
         BL    ACLAXX                                                           
         CLI   TGUSEQU,UCLA        CLASS A USES                                 
         BNE   ACLAXX                                                           
         CLI   TCLIFT,C'Y'         BRANCH IF THIS IS A LIFT                     
         BE    ACLA4                                                            
*                                                                               
         CLI   TCVERSEC,0          IF PAYING A VERSION                          
         BE    ACLA2                                                            
         CLI   TCVERSEC,15         LENGTH MUST BE 15 SEC                        
         BE    ACLA5                                                            
         CLI   TCVERSEC,10         OR 10 SEC                                    
         BE    ACLA5                                                            
         B     ACLAX                                                            
*                                                                               
         USING TACOD,R2                                                         
ACLA2    L     R2,TCATACO          ELSE USE COMM'L LENGTH                       
         CLI   TACOSEC,15          LEN MUST BE 15 SECS OR LESS                  
         BH    ACLAX                                                            
         LH    R1,TCUSEN           USE OVERALL USE NUMBER                       
         B     ACLA6                                                            
*                                                                               
ACLA4    CLI   TCLFTSEC,15         LIFTS MUST ALSO BE 15 SECS OR LESS           
         BH    ACLAX                                                            
ACLA5    LH    R1,TCUSENL          ELSE USE USE NUMBER FOR LIFT ONLY            
*                                                                               
ACLA6    LA    RE,4                MUST BE A MULTIPLE OF 4                      
         CLI   TGYREQU,CN88        OR IF 88 CONTRACT OR LATER                   
         BL    *+8                                                              
         LA    RE,5                MUST BE A MULTIPLE OF 5                      
*                                                                               
         XR    R0,R0                                                            
         DR    R0,RE                                                            
         CHI   R0,1                OR 1 MORE THAN THAT                          
         BH    ACLAX                                                            
*                                                                               
         LTR   R1,R1               MUST AT LEAST BE THAT USE NUMBER             
         BZ    ACLAX                                                            
*                                  PAYMENT ELIGIBLE FOR 50% DISCOUNT            
         LR    R1,R4               AMOUNT                                       
*                                                                               
         S     R1,WORK             SUBTRACT OFF SWEETENING/MULT DIFF            
*                                                                               
         L     RF,WORK                                                          
         MHI   RF,10               * 10                                         
         SRA   RF,1                / 2                                          
         CVD   RF,DUB                                                           
         SRP   DUB,63,5            DIVIDE BY 10 AND ROUND                       
         CVB   RF,DUB                                                           
         ST    RF,WORK                                                          
*                                                                               
         MHI   R1,10               * 10                                         
         SRA   R1,1                / 2                                          
         CVD   R1,DUB                                                           
         SRP   DUB,63,5            DIVIDE BY 10 AND ROUND                       
         CVB   R1,DUB                                                           
*                                                                               
         CLI   TGYREQU,CN88        IF PREVIOUS TO 88 CONTRACT                   
         BNL   *+8                                                              
         BAS   RE,NICK4            ROUND TO NEAREST NICKEL                      
*                                                                               
         LR    R4,R1               SET NEW RATE                                 
         ST    R4,FULL                                                          
         A     R4,WORK                                                          
*                                                                               
         OI    TCSTAT2,TCSTCDIS    CLASS A DISCOUNT STATUS                      
*                                                                               
ACLAX    GOTOR SVBRKDWN,DMCB,(0,FULL),0                                         
ACLAXX   XIT1  REGS=(R4)           RETURN RESULT IN R4                          
         EJECT                                                                  
*              ROUTINE ROUNDS AMOUNT IN R1 TO NEAREST NICKEL                    
         SPACE 1                                                                
NICK4    DS    0H                                                               
         AHI   R1,2                ADD 2                                        
         XR    R0,R0                                                            
         D     R0,=F'5'            DIVIDE BY FIVE                               
         M     R0,=F'5'            MULTIPLY BY FIVE                             
         LTR   R1,R1               ADD 1 IF NEGATIVE                            
         BNM   *+8                                                              
         AHI   R1,1                                                             
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
*              MUSIC SESSION CALCULATION SUB-ROUTINES                           
*---------------------------------------------------------------------          
         USING TASDD,R2            R2=A(SESSION DETAILS ELEMENT)                
BSMHMM   NTR1  BASE=*,LABEL=*      WORK TIME HOURS/MINUTES                      
         XC    BLOCK(16),BLOCK                                                  
         XR    R1,R1                                                            
         ICM   R1,3,TASDMHM        HOURS/MINUTES                                
         XR    R0,R0                                                            
         D     R0,=F'100'          R1=N'HOURS                                   
         LR    RF,R0               SAVE # MINS INTO RF                          
         ST    R1,BLOCK                                                         
*                                                                               
         L     R3,TASDHR                                                        
         OC    TCDOUBL,TCDOUBL     DOUBLING INCLUDED IN HOURLY RATE?            
         BZ    *+8                                                              
         S     R3,TCDOUBL          YES, SUBTRACT IT                             
         ST    R3,WORK             SAVE HOURLY RATE IN WORK                     
         M     R0,WORK             R1 = PAYMENT FOR # HOURS                     
*                                                                               
         LTR   RF,RF                                                            
         BZ    BSMHMMX             DON'T BOTHER IF 0 MINUTES                    
*                                                                               
         XR    RE,RE                                                            
         D     RE,=F'20'           RF=N'20 MINUTE INCREMENTS                    
         ST    RF,BLOCK+4                                                       
*                                                                               
         XR    R4,R4               CALCULATE RATE PER 20 MINS                   
         L     R5,WORK             HOURLY RATE / 3                              
         SLA   R5,1                                                             
         D     R4,=F'3'            R5=RATE                                      
         LTR   R5,R5                                                            
         BM    *+8                                                              
         AHI   R5,1                ROUNDED                                      
         SRA   R5,1                                                             
*                                                                               
         MR    RE,R5               MINS PAYMENT = TIME X RATE                   
         AR    R1,RF               R1 = PAYMENT FOR HOURS + MINS                
*                                                                               
BSMHMMX  ST    R1,BLOCK+8                                                       
         ST    R1,WORK+8                                                        
         XR    R1,R1                                                            
         ICM   R1,3,TASDMHM                                                     
         ST    R1,WORK+4                                                        
         GOTOR SVBRKDWN,DMCB,(0,WORK),(66,WORK+4)                               
*                                                                               
         OC    TCDOUBL,TCDOUBL     DOUBLING?                                    
         BZ    BSMHMMX9            NO                                           
         XR    R0,R0                                                            
         L     R1,BLOCK            # OF HOURS                                   
         M     R0,TCDOUBL                                                       
*                                                                               
         XR    RE,RE                                                            
         L     RF,BLOCK+4          # OF 20 MIN CHUNKS                           
         LTR   RF,RF                                                            
         BZ    BSMHMMX5                                                         
*                                                                               
         XR    R4,R4               CALCULATE RATE PER 20 MINS                   
         L     R5,TCDOUBL          HOURLY RATE / 3                              
         SLA   R5,1                                                             
         D     R4,=F'3'            R5=RATE                                      
         LTR   R5,R5                                                            
         BM    *+8                                                              
         AHI   R5,1                ROUNDED                                      
         SRA   R5,1                                                             
*                                                                               
         MR    RE,R5               MINS PAYMENT = TIME X RATE                   
         AR    R1,RF               R1 = PAYMENT FOR HOURS + MINS                
BSMHMMX5 ST    R1,BLOCK+12                                                      
         GOTOR SVBRKDWN,DMCB,(0,BLOCK+12),(7,0)                                 
*                                                                               
BSMHMMX9 L     R1,BLOCK+8          RESTORE IT                                   
         OC    TCDOUBL,TCDOUBL     DOUBLING?                                    
         BZ    *+8                                                              
         A     R1,BLOCK+12         ADD IT BACK IN                               
         LA    RF,TASDMHMA                                                      
         XIT1  REGS=(R1)                                                        
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
*              INDUSTRIAL MUSIC SESSION CALCULATION SUB-ROUTINES                
*---------------------------------------------------------------------          
* TIME - 2 HOUR MIN @ RATE                                                      
* TIME - 30 MIN @ 15 MIN RATE                                                   
* TIME * 1.5 (15 MIN RATE)                                                      
*---------------------------------------------------------------------          
         USING TASDD,R2            R2=A(SESSION DETAILS ELEMENT)                
IMSHMM   NTR1  BASE=*,LABEL=*      WORK TIME HOURS/MINUTES                      
         XC    BLOCK(16),BLOCK                                                  
         XR    R1,R1                                                            
         ICM   R1,3,TASDMHM        HOURS/MINUTES                                
         XR    R0,R0                                                            
         D     R0,=F'100'          R1=N'HOURS (1 HOUR SESSION)                  
         LR    RF,R0               SAVE # MINS INTO RF                          
         ST    R1,BLOCK            SAVE # HOURS INTO BLOCK                      
*                                                                               
         L     R3,TASDHR                                                        
         OC    TCDOUBL,TCDOUBL     DOUBLING INCLUDED IN HOURLY RATE?            
         BZ    *+8                                                              
         S     R3,TCDOUBL          YES, SUBTRACT IT                             
*                                                                               
         ST    R3,WORK             SAVE 2 HOUR RATE IN WORK                     
         XR    R4,R4                                                            
*                                                                               
         LTR   R1,R1               0 HOURS?                                     
         BZ    IMSHMM3             JUST ADD IN MINIMUM                          
*                                                                               
         AHI   R1,-2               SUBTRACT 2 HOUR MINIMUM                      
         LTR   R1,R1                                                            
         BZ    IMSHMM3                                                          
         XR    R0,R0                                                            
         M     R0,=F'4'            CONVERT REMAINING HOURS TO 15 MINS           
         LR    R4,R1               SAVE IT FOR LATE                             
         XR    R1,R1                                                            
*                                                                               
IMSHMM3  A     R1,WORK             ADD IN MINIMUM 2 HOUR PAYMENT                
         LTR   RF,RF                                                            
         BNZ   IMSHMM5             DON'T BOTHER IF 0 MINUTES                    
         LTR   R4,R4                                                            
         BZ    IMSHMMX                                                          
*                                                                               
IMSHMM5  XR    RE,RE                                                            
         D     RE,=F'15'           RF=N'15 MINUTE INCREMENTS                    
         ST    RF,BLOCK+4                                                       
         AR    RF,R4               ADD THOSE FROM REMAINING HOURS               
*                                                                               
         XR    R4,R4               CALCULATE RATE PER 15 MINS                   
         L     R5,WORK             2 HOUR RATE / 8                              
         SLA   R5,1                                                             
         D     R4,=F'8'            R5=RATE                                      
         LTR   R5,R5                                                            
         BM    *+8                                                              
         AHI   R5,1                ROUNDED                                      
         SRA   R5,1                                                             
*                                                                               
         LTR   RF,RF               FIRST 1/2 HOUR AT 15 MIN RATE                
         BZ    IMSHMMX                                                          
         AR    R1,R5                                                            
         BCTR  RF,0                                                             
         LTR   RF,RF                                                            
         BZ    IMSHMMX                                                          
         AR    R1,R5                                                            
         BCTR  RF,0                                                             
*                                                                               
         LTR   RF,RF                                                            
         BZ    IMSHMMX                                                          
*                                                                               
         XR    R4,R4                                                            
         LR    RE,R5               SAVE 15 MIN RATE                             
         SLA   R5,1                CALCULATE HALF OF 15 MIN RATE                
         D     R4,=F'2'            R5=RATE                                      
         LTR   R5,R5                                                            
         BM    *+8                                                              
         AHI   R5,1                ROUNDED                                      
         SRA   R5,1                                                             
         AR    R5,RE               150%                                         
*                                                                               
         XR    RE,RE               REST AT 150% OF 15 MIN RATE                  
         MR    RE,R5               MINS PAYMENT = TIME X RATE                   
         AR    R1,RF               R1 = PAYMENT FOR HOURS + MINS                
*                                                                               
IMSHMMX  ST    R1,BLOCK+8                                                       
         ST    R1,WORK+8                                                        
         XR    R1,R1                                                            
         ICM   R1,3,TASDMHM                                                     
         ST    R1,WORK+4                                                        
         GOTOR SVBRKDWN,DMCB,(0,WORK),(66,WORK+4)                               
*                                                                               
         OC    TCDOUBL,TCDOUBL     DOUBLING?                                    
         BZ    IMSHMMX9                                                         
         LA    R5,50               MINIMUM FOR 1 DOUBLE IS 50%                  
         MVC   WORK(1),TCCADBL                                                  
         NI    WORK,X'0F'                                                       
         ZIC   RF,WORK             N'DOUBLES                                    
         BCTR  RF,0                                                             
*                                                                               
         LTR   RF,RF                                                            
         BZ    IMSHMMX5                                                         
IMSHMMX3 AHI   R5,20               ADDITIONAL DOUBLES ADD 20%                   
         BCT   RF,IMSHMMX3                                                      
*                                                                               
IMSHMMX5 XR    R0,R0                                                            
         L     R1,BLOCK+8          PAYMENT                                      
         MR    R0,R5               CALCULATE DOUBLE AMOUNT                      
*                                                                               
         SLA   R1,1                ROUND IT                                     
         D     R0,=F'100'                                                       
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
*                                                                               
         ST    R1,BLOCK+12                                                      
         GOTOR SVBRKDWN,DMCB,(0,BLOCK+12),(7,0)                                 
*                                                                               
IMSHMMX9 L     R1,BLOCK+8          RESTORE IT                                   
         OC    TCDOUBL,TCDOUBL     DOUBLING?                                    
         BZ    *+8                                                              
         A     R1,BLOCK+12         ADD IT BACK IN                               
         LA    RF,TASDMHMA                                                      
         XIT1  REGS=(R1)                                                        
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
*              INDUSTRIAL OFFCAM SESSION CALCULATION SUB-ROUTINES               
*---------------------------------------------------------------------          
* TIME - 1 HOUR MIN @ RATE                                                      
* TIME - ADDT'L 30 MIN RATE AFTER                                               
*---------------------------------------------------------------------          
         USING TASDD,R2            R2=A(SESSION DETAILS ELEMENT)                
IDSHMM   NTR1  BASE=*,LABEL=*      WORK TIME HOURS/MINUTES                      
         XC    BLOCK(16),BLOCK                                                  
         XR    R1,R1                                                            
         ICM   R1,3,TASDMHM        HOURS/MINUTES                                
         XR    R0,R0                                                            
         D     R0,=F'100'          R1=N'HOURS (1 HOUR SESSION)                  
         ST    R0,BLOCK+4          SAVE # MINS INTO BLOCK+4                     
         ST    R1,BLOCK            SAVE # HOURS INTO BLOCK                      
*                                                                               
         L     R3,TASDHR                                                        
         CLI   TGCAEQU,CTC3        GROUP CONTRACTOR = 1/2 ( RATE )              
         BL    IDSHM100                                                         
         CLI   TGCAEQU,CTC9                                                     
         BH    IDSHM100                                                         
         SRL   R3,1                DIVIDE BY 2                                  
*        A     R3,TASDHR           ** TALENT DOESN'T WANT 1.5X                  
IDSHM100 ST    R3,WORK+4           SAVE 1 HOUR RATE IN WORK                     
         ST    R3,BLOCK+8                                                       
*                                                                               
         TM    TGCASTAT,SINGER     SINGERS HAVE AN HOURLY RATE                  
         BO    IDSHM300                                                         
*                                                                               
         GOTOR SVBRKDWN,DMCB,(0,WORK+4),(69,=X'00000001')                       
*                                                                               
         L     RF,BLOCK+4          # OF MINS                                    
         L     R4,BLOCK                                                         
         AHI   R4,-1               SUBTRACT 1 HOUR MINIMUM                      
         BM    IDSHM290            LESS THAN 1 HOUR, LEAVE                      
         BZ    IDSHM150                                                         
         SLL   R4,1                REMAIN HOURS TO 30 MINS (*2)                 
*                                                                               
IDSHM150 LTR   RF,RF               HAS TO BE SOME MINUTES                       
         BZ    IDSHM250            DON'T BOTHER IF 0 MINUTES                    
         LA    RE,1                                                             
         C     RF,=F'30'           LESS THAN 30 MINS, ADD 1                     
         BNH   IDSHM200                                                         
         LA    RE,2                30 OR MORE, ADD 2                            
*                                                                               
IDSHM200 AR    R4,RE               R4=N'30 MINUTE BLOCKS                        
*                                                                               
IDSHM250 LTR   R4,R4               ANY 30 MINUTE BLOCKS?                        
         BZ    IDSHM290            NO, DONE                                     
*                                                                               
         ST    R4,WORK+4                                                        
         BRAS  RE,GETINDXT                                                      
         LR    R1,RF                                                            
         USING TNDXD,R1                                                         
         XR    RE,RE                                                            
         L     RF,TNDXOFH1         OFF-CAMERA ADDT'L HALF HOUR RATE             
         USING TACOD,R5                                                         
         L     R5,TCATACO                                                       
         CLI   TACOTYPE,CTYICAT2   CAT 2 INDUSTRIAL?                            
         BNE   *+8                                                              
         L     RF,TNDXOFH2                                                      
         ST    RF,WORK                                                          
         MR    RE,R4                                                            
*                                                                               
         L     R1,BLOCK+8                                                       
         AR    R1,RF                                                            
         ST    R1,BLOCK+8                                                       
*                                                                               
         GOTOR SVBRKDWN,DMCB,(0,WORK),(69,WORK+4)                               
*                                                                               
IDSHM290 L     R1,BLOCK+8          PAYMENT AMOUNT                               
         LA    RF,TASDMHMA                                                      
         B     IDSHMXX                                                          
*--------------------------------------------------------------------           
IDSHM300 LTR   R0,R0               # OF MINS                                    
         BZ    IDSHM310                                                         
         AHI   R1,1                REMAINING MINS = ADDITIONAL HOUR             
         ST    R1,BLOCK                                                         
         XC    BLOCK+4(4),BLOCK+4                                               
IDSHM310 ST    R1,WORK+4           HOURS                                        
         LR    R4,R1                                                            
         XR    RE,RE                                                            
         LR    RF,R3               HOURLY RATE                                  
         ST    RF,WORK                                                          
         MR    RE,R4                                                            
*                                                                               
         ST    RF,BLOCK+8                                                       
*                                                                               
         GOTOR SVBRKDWN,DMCB,(0,WORK),(69,WORK+4)                               
*                                                                               
         L     R1,BLOCK+8          PAYMENT AMOUNT                               
         LA    RF,TASDMHMA                                                      
         B     IDSHMXX                                                          
*                                                                               
IDSHMXX  XIT1  REGS=(R1)                                                        
         DROP  R5                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
*              INDUSTRIAL RETAKE SESSION CALCULATION SUB-ROUTINES               
*---------------------------------------------------------------------          
*                                                                               
*        RETAKE SESSIONS                                                        
*                                                                               
* TIME - MINIMUM SESSION RATE                                                   
* TIME - ADDT'L 60 MIN RATE AFTER                                               
*---------------------------------------------------------------------          
                                                                                
RTKHMM   NTR1  BASE=*,LABEL=*      WORK TIME HOURS/MINUTES                      
*                                                                               
         USING TASDD,R2            R2=A(SESSION DETAILS ELEMENT)                
*                                                                               
         CLC   =C'ON',TCCAONOF    IF ON-CAMERA                                  
         BNE   RTKHMONN                                                         
*                                                                               
*        CHECK IF A CONTRACTOR                                                  
*                                                                               
         ICM   RF,15,TASDFEE       GET SESSION RATE                             
*                                                                               
         CLI   TGCACDE,C'C'        IF A CONTRACTOR                              
         BNE   RTKHMONA                                                         
*                                                                               
         LR    RE,RF               ADD 50% PREMIUM                              
         AHI   RE,1                ADD 1 FOR ROUNDING                           
         SRA   RE,1                HALVE RATE                                   
         AR    RF,RE               150% OF RATE                                 
*                                                                               
         STCM  RF,15,TASDFEE       NEW SESSION RATE                             
*                                                                               
RTKHMONA DS    0H                                                               
*                                                                               
*        CHECK IF A WEEKLY PERFORMER                                            
*                                                                               
         CLI   TGCAEQU,CTP3D       PRINCIPAL 3 DAY                              
         BE    *+8                                                              
         CLI   TGCAEQU,CTS3D       SOLO      3 DAY                              
         BE    *+8                                                              
         CLI   TGCAEQU,CTG3D       GROUP     3 DAY                              
         BNE   *+16                                                             
         LHI   R3,1                   DIVISOR FOR 1/2 DAY RATE                  
         LHI   R1,3                   DIVISOR FOR DAILY RATE                    
         B     RTKHMON1                                                         
*                                                                               
         CLI   TGCAEQU,CTP5D       PRINCIPAL 5 DAY                              
         BE    *+8                                                              
         CLI   TGCAEQU,CTS5D       SOLO      5 DAY                              
         BE    *+8                                                              
         CLI   TGCAEQU,CTG5D       GROUP     5 DAY                              
         BNE   *+16                                                             
         LHI   R3,2                   DIVISOR FOR 1/2 DAY RATE                  
         LHI   R1,5                   DIVISOR FOR DAILY RATE                    
         B     RTKHMON1                                                         
*                                                                               
         CLI   TGCAEQU,CTP6D       PRINCIPAL 6 DAY                              
         BE    *+8                                                              
         CLI   TGCAEQU,CTS6D       SOLO      6 DAY                              
         BE    *+8                                                              
         CLI   TGCAEQU,CTG6D       GROUP     6 DAY                              
         BNE   *+16                                                             
         LHI   R3,2                   DIVISOR FOR 1/2 DAY RATE                  
         LHI   R1,5                   DIVISOR FOR DAILY RATE                    
         B     RTKHMON1                                                         
*                                                                               
         CLI   TGCAEQU,CTPHD       IF HALF DAY PERFORMER                        
         BNE   RTKHMM10                                                         
*                                                                               
         CLC   TASDIHM,=H'400'     IF OVER 4 HOURS WORKED                       
         BNH   RTKHMM10                                                         
*                                                                               
         BRAS  RE,GETINDXT                                                      
         LR    R1,RF               ESTABLISH INDUSTRIAL EXTRAS TABLE            
         USING TNDXD,R1                                                         
*                                                                               
         L     RF,TNDXPHD1         PRINCIPAL DAILY RATE - CAT1                  
*                                                                               
         L     RE,TCATACO                                                       
         USING TACOD,RE            ESTABLISH COMMERCIAL DETAILS ELEMENT         
*                                                                               
         CLI   TACOTYPE,CTYICAT2   CAT 2 INDUSTRIAL?                            
         BNE   *+8                                                              
         L     RF,TNDXPHD2         PRINCIPAL DAILY RATE - CAT2                  
*                                                                               
         DROP  R1                                                               
         DROP  RE                                                               
*                                                                               
RTKHMM10 DS    0H                                                               
*                                                                               
         LHI   R1,1                DEFAULT TO DAILY RATE                        
*                                                                               
         B     RTKHMON2                                                         
*                                                                               
RTKHMON1 DS    0H                                                               
*                                                                               
         SR    RE,RE                                                            
*                                                                               
         DR    RE,R1               GET DAILY RATE                               
         SLA   RE,1                DOUBLE REMAINDER                             
         CR    RE,R1                                                            
         BL    *+8                                                              
         AHI   RF,1                ROUND RESULT                                 
*                                                                               
RTKHMON2 DS    0H                                                               
*                                                                               
         STCM  RF,15,TASDFEE       RESET SESSION DAILY RATE                     
*                                                                               
         SR    RE,RE                                                            
         D     RE,=F'8'            GET HOURLY RATE                              
         SLA   RE,1                DOUBLE  REMAINDER                            
         C     RE,=F'8'            COMPARE REMAINDER*2 TO DIVISOR               
         BL    *+8                                                              
         AHI   RF,1                ROUND                                        
*                                                                               
         STCM  RF,15,TASDHR        SET HOURLY RATE                              
*                                                                               
*        PAY FOR AT LEAST ONE DAY                                               
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,TASDIHM        GET HOURS                                    
*                                                                               
         ST    R1,FULL             SAVE TOTAL HOURS                             
*                                                                               
         MVC   TASDIHMA,TASDFEE    DAILY SESSION RATE                           
         MVC   TASDIHM,=H'800'     MINIMUM 8 HRS                                
*                                                                               
         CHI   R1,400              IF 4 HRS OR LESS                             
         BH    RTKHMN21                                                         
*                                                                               
         CLI   TGCAEQU,CTPHD       SKIP IF HALF DAY PERFORMER                   
         BE    RTKHMN20                                                         
*                                                                               
         CHI   R3,2                IF WEEKLY PERFORMER                          
         BNE   RTKHMN21                                                         
*                                                                               
         ICM   R1,15,TASDIHMA         GET DAILY RATE                            
         AHI   R1,1                   FOR ROUNDING                              
         SRA   R1,1                   GET HALF DAY RATE                         
*                                                                               
         STCM  R1,15,TASDIHMA         SET HALF DAY RATE                         
         STCM  R1,15,TASDFEE          SET HALF DAY RATE                         
*                                                                               
RTKHMN20 DS    0H                                                               
*                                                                               
         MVC   TASDIHM,=H'400'        MINIMUM 4 HRS                             
*                                                                               
RTKHMN21 DS    0H                                                               
*                                                                               
*        SAVE BREAKOUT OF ONE SESSION'S PAY FOR CHECKS                          
*                                                                               
         GOTOR SVBRKDWN,DMCB,(0,TASDIHMA),(69,=X'00000001')                     
*                                                                               
*        CHECK FOR OVERTIME                                                     
*                                                                               
         L     R1,FULL             RETRIEVE TOTAL HOURS                         
*                                                                               
         CHI   R1,800              8 HRS OR LESS                                
         BNH   RTKHMON3               PAY ONE DAY RATE                          
*                                                                               
         SR    R0,R0                                                            
         D     R0,=F'100'          CALCULATE HOURS                              
         LTR   R0,R0               IF EXTRA MINUTES                             
         BZ    *+8                                                              
         AHI   R1,1                   ADD AN HOUR                               
*                                                                               
         SHI   R1,8                OVERTIME HOURS                               
*                                                                               
         ST    R1,FULL             SAVE OVERTIME HOURS                          
*                                                                               
         CHI   R1,2                IF INTO DOUBLE OVER TIME                     
         BNH   *+8                                                              
         LHI   R1,2                   SET FOR 2 OVERTIME HOURS                  
*                                                                               
         SR    RE,RE                                                            
         ICM   RF,15,TASDHR        GET HOURLY RATE                              
         M     RE,=F'15'           *1.5                                         
         ST    RF,WORK                                                          
*                                                                               
         MR    RE,R1               OVERTIME PAY                                 
*                                                                               
         D     RE,=F'10'           SHIFT DECIMAL RIGHT 1                        
         SLA   RE,1                                                             
         C     RE,=F'10'           CHECK FOR ROUNDING                           
         BL    *+8                                                              
         AHI   RF,1                                                             
*                                                                               
         STC   R1,TASDIOT          SAVE OVERTIME HOURS                          
         STCM  RF,15,TASDIOTA      SAVE OVERTIME PAY                            
*                                                                               
*        SAVE BREAKOUT OF OVERTIME PAY FOR CHECKS                               
*                                                                               
         XR    R0,R0                                                            
         L     R1,WORK                                                          
         D     R0,=F'10'                                                        
         STCM  R1,15,WORK                                                       
         ZIC   R1,TASDIOT                                                       
         ST    R1,WORK+32                                                       
         GOTOR SVBRKDWN,DMCB,(0,WORK),('PBC15X',WORK+32)                        
*                                                                               
*        DOUBLE OVERTIME                                                        
*                                                                               
         L     R1,FULL             GET OVERTIME HOURS                           
*                                                                               
         SHI   R1,2                GET DOUBLE OVER TIME                         
         BNP   RTKHMON3               NONE                                      
*                                                                               
         SR    RE,RE                                                            
         ICM   RF,15,TASDHR        GET HOURLY RATE                              
         M     RE,=F'2'            *2                                           
         ST    RF,WORK                                                          
*                                                                               
         MR    RE,R1               OVERTIME PAY                                 
*                                                                               
         STC   R1,TASDIDT          SAVE DOUBLE OVERTIME HOURS                   
         STCM  RF,15,TASDIDTA      SAVE DOUBLE OVERTIME PAY                     
*                                                                               
*        SAVE BREAKOUT OF DOUBLE OVERTIME PAY FOR CHECKS                        
*                                                                               
         ZIC   R1,TASDIDT                                                       
         ST    R1,WORK+32                                                       
         GOTOR SVBRKDWN,DMCB,(0,WORK),('PBC20X',WORK+32)                        
*                                                                               
RTKHMON3 DS    0H                                                               
*                                                                               
RTKHMONX DS    0H                                                               
*                                                                               
*        RETURN TOTAL PAYMENT                                                   
*                                                                               
         ICM   R1,15,TASDFEE       SESSION FEE                                  
*                                                                               
         ICM   RF,15,TASDIOTA                                                   
         AR    R1,RF               PLUS OVERTIME PAY                            
*                                                                               
         ICM   RF,15,TASDIDTA                                                   
         AR    R1,RF               PLUS DOUBLE OVERTIME PAY                     
*                                                                               
         B     RTKHMXX                                                          
*                                                                               
RTKHMONN DS    0H                                                               
*                                                                               
*        OFF-LINE PERFORMER                                                     
*                                                                               
         CLC   TCCAONOF,=C'OFF'   IF OFF-CAMERA                                 
         BNE   RTKHMOFN                                                         
*                                                                               
         TM    TASDIST,TASDIENQ   IF ENTIRE SCRIPT                              
         BO    RTKHMOF1                                                         
*                                                                               
         TM    TASDIST,TASDI60Q   IF PARTIAL SCRIPT AND OVER 60 DAYS            
         BO    RTKHMOF1                                                         
*                                                                               
         TM    TASDIST,TASDI60Q+TASDIENQ+TASDIPTQ  IF NO OPTS                   
         BZ    RTKHMOF1               TREAT AS ENTIRE SCRIPT                    
*                                                                               
         TM    TASDIST,TASDIPTQ    IF PARTIAL SCRIPT                            
         BNO   *+14                                                             
         CLC   TASDIHM,=H'30'         AND OVER 30 MIN                           
         BH    RTKHMOF1                  TREAT AS FULL SESSION RATE             
*                                                                               
         B     RTKHMOF2                                                         
*                                                                               
RTKHMOF1 DS    0H                                                               
*                                                                               
         XC    BLOCK(16),BLOCK                                                  
         XR    R1,R1                                                            
*                                                                               
         ICM   R1,3,TASDIHM        HOURS/MINUTES                                
         BZ    RTKHM290            SKIP IF NO TIME ENTERED                      
*                                                                               
         XR    R0,R0                                                            
         D     R0,=F'100'          R1=N'HOURS (1 HOUR SESSION)                  
*                                                                               
         ST    R0,BLOCK+4          SAVE # MINS INTO BLOCK+4                     
         ST    R1,BLOCK            SAVE # HOURS INTO BLOCK                      
*                                                                               
*        PERFORMER PAID FOR AT LEAST 1 HOUR                                     
*                                                                               
         L     R3,TASDHR           GET HOURLY RATE                              
*                                                                               
*        CHECK IF A CONTRACTOR                                                  
*                                                                               
         CLI   TGCACDE,C'C'        IF A CONTRACTOR                              
         BNE   RTKHMOFA                                                         
*                                                                               
         LR    RE,R3               ADD 50% PREMIUM                              
         AHI   RE,1                ADD 1 FOR ROUNDING                           
         SRA   RE,1                HALVE RATE                                   
         AR    R3,RE               150% OF RATE                                 
*                                                                               
         STCM  R3,15,TASDHR        NEW HOURLY RATE                              
*                                                                               
RTKHMOFA DS    0H                                                               
*                                                                               
         ST    R3,WORK+4           SAVE 1 HOUR RATE IN WORK                     
         ST    R3,BLOCK+8                                                       
*                                                                               
         GOTOR SVBRKDWN,DMCB,(0,WORK+4),(69,=X'00000001')                       
*                                                                               
         L     RF,BLOCK+4          # OF MINS                                    
         L     R4,BLOCK            # OF HRS                                     
*                                                                               
         SHI   R4,1                SUBTRACT 1 HOUR MINIMUM                      
         BM    RTKHM290            LESS THAN 1 HOUR, DONE                       
*                                                                               
*        SINGERS, GROUPS, CONTRACTORS PAID HOURLY                               
*                                                                               
         TM    TGCASTAT,SINGER     SINGERS                                      
         BO    RTKHM240                                                         
         CLI   TGCACDE,C'S'        IF A SINGER                                  
         BE    *+8                                                              
         CLI   TGCACDE,C'G'        IF A GROUP                                   
         BE    *+8                                                              
         CLI   TGCACDE,C'C'        IF A CONTRACTOR                              
         BNE   RTKHMOFB                                                         
*                                                                               
RTKHM240 LTR   RF,RF               ANY MINUTES                                  
         BZ    *+8                                                              
         AHI   R4,1                   ADDS 1 HOUR TO TOTAL HRS                  
*                                                                               
         L     RF,TASDHR           HOURLY RATE                                  
*                                                                               
         B     RTKHMOFC                                                         
*                                                                               
RTKHMOFB DS    0H                                                               
*                                                                               
         SLL   R4,1                REMAIN HOURS TO 30 MINS (*2)                 
*                                                                               
         LTR   RF,RF               HAS TO BE SOME MINUTES                       
         BZ    RTKHM250            DON'T BOTHER IF 0 MINUTES                    
*                                                                               
         LA    RE,1                DEFAULT TO ONE 30 MINUTE BLOCK               
*                                                                               
         CHI   RF,30               IF OVER 30 MINS                              
         BNH   *+8                                                              
         AHI   RE,1                  ADD ANOTHER 30 MIN BLOCK                   
*                                                                               
         AR    R4,RE               R4=N'30 MINUTE BLOCKS                        
*                                                                               
RTKHM250 LTR   R4,R4               ANY 30 MINUTE BLOCKS?                        
         BZ    RTKHM290            NO, DONE                                     
*                                                                               
         ST    R4,WORK+4           SAVE # 0F 30 MIN BLOCKS                      
*                                                                               
         L     R5,TCATACO          ESTABLISH COMMERCIAL DETAILS ELEMENT         
         USING TACOD,R5                                                         
*                                                                               
         BRAS  RE,GETINDXT                                                      
         LR    R1,RF               ESTABLISH INDUSTRIAL RATES TABLE             
         USING TNDXD,R1                                                         
*                                                                               
         XR    RE,RE                                                            
*                                                                               
         L     RF,TNDXX301         OFF-CAMERA RTK ADDT'L HALF HR RATE           
*                                                                               
         CLI   TACOTYPE,CTYICAT2   CAT 2 INDUSTRIAL?                            
         BNE   *+8                                                              
         L     RF,TNDXX302                                                      
*                                                                               
         ST    RF,WORK             HALF HOUR RATE                               
*                                                                               
RTKHMOFC DS    0H                                                               
*                                                                               
         MR    RE,R4               TOTAL OVERTIME PAY                           
*                                                                               
         L     R1,BLOCK+8          ADD OVERTIME TO SESSION RATE                 
         AR    R1,RF                                                            
         ST    R1,BLOCK+8                                                       
*                                  ADD IN OVERTIME                              
         GOTOR SVBRKDWN,DMCB,(0,WORK),(69,WORK+4)                               
*                                                                               
RTKHM290 L     R1,BLOCK+8          PAYMENT AMOUNT                               
         LA    RF,TASDIHMA                                                      
         B     RTKHMXX                                                          
*                                                                               
*        PARTIAL SCRIPT                                                         
*                                                                               
RTKHMOF2 DS    0H                                                               
*                                                                               
         TM    TASDIST,TASDIPTQ   SKIP IF NOT PARTIAL SCRIPT                    
         BNO   RTKHMOF3                                                         
*                                                                               
         XC    BLOCK(16),BLOCK                                                  
         XR    R1,R1                                                            
*                                                                               
         ICM   R1,3,TASDIHM        HOURS/MINUTES                                
         BZ    RTKHM390            SKIP IF NO TIME ENTERED                      
*                                                                               
         XR    R0,R0                                                            
         D     R0,=F'100'          R1=N'HOURS (1 HOUR SESSION)                  
*                                                                               
         ST    R0,BLOCK+4          SAVE # MINS INTO BLOCK+4                     
         ST    R1,BLOCK            SAVE # HOURS INTO BLOCK                      
*                                                                               
*        PERFORMER PAID FOR AT LEAST 1/2 HOUR                                   
*                                                                               
         L     R5,TCATACO          ESTABLISH COMMERCIAL DETAILS ELEMENT         
         USING TACOD,R5                                                         
*                                                                               
         BRAS  RE,GETINDXT                                                      
         LR    R1,RF               ESTABLISH INDUSTRIAL RATES TABLE             
         USING TNDXD,R1                                                         
*                                                                               
         L     RF,TNDX1301         OFF-CAMERA RTK FIRST HALF HR RATE            
*                                                                               
         CLI   TACOTYPE,CTYICAT2   CAT 2 INDUSTRIAL?                            
         BNE   *+8                                                              
         L     RF,TNDX1302                                                      
*                                                                               
         ST    RF,WORK+4           SAVE 1/2 HOUR RATE IN WORK                   
         ST    RF,BLOCK+8                                                       
*                                                                               
         GOTOR SVBRKDWN,DMCB,(0,WORK+4),(69,=X'00000001')                       
*                                                                               
         L     RF,BLOCK+4          # OF MINS                                    
         L     R4,BLOCK            # OF HRS                                     
*                                                                               
         SLL   R4,1                NUMBER OF 1/2 HRS                            
*                                                                               
         LTR   RF,RF               HAS TO BE SOME MINUTES                       
         BZ    RTKHM340            DON'T BOTHER IF 0 MINUTES                    
*                                                                               
         LA    RE,1                DEFAULT TO ONE 30 MINUTE BLOCK               
*                                                                               
         CHI   RF,30               IF OVER 30 MINS                              
         BNH   *+8                                                              
         AHI   RE,1                  ADD ANOTHER 30 MIN BLOCK                   
*                                                                               
         AR    R4,RE               R4=N'30 MINUTE BLOCKS                        
*                                                                               
RTKHM340 DS    0H                                                               
*                                                                               
         SHI   R4,1                SUBTRACT 1 HALF HOUR MINIMUM                 
         BM    RTKHM390            LESS THAN 1 HALF HOUR, DONE                  
*                                                                               
         ST    R4,WORK+4           SAVE # 0F 30 MIN BLOCKS                      
*                                                                               
         L     R5,TCATACO          ESTABLISH COMMERCIAL DETAILS ELEMENT         
         USING TACOD,R5                                                         
*                                                                               
         BRAS  RE,GETINDXT                                                      
         LR    R1,RF               ESTABLISH INDUSTRIAL RATES TABLE             
         USING TNDXD,R1                                                         
*                                                                               
         XR    RE,RE                                                            
*                                                                               
         L     RF,TNDXX301         OFF-CAMERA RTK ADDT'L HALF HR RATE           
*                                                                               
         CLI   TACOTYPE,CTYICAT2   CAT 2 INDUSTRIAL?                            
         BNE   *+8                                                              
         L     RF,TNDXX302                                                      
*                                                                               
         ST    RF,WORK             HALF HOUR RATE                               
*                                                                               
         MR    RE,R4               TOTAL OVERTIME PAY                           
*                                                                               
         L     R1,BLOCK+8          ADD OVERTIME TO SESSION RATE                 
         AR    R1,RF                                                            
         ST    R1,BLOCK+8                                                       
*                                  ADD IN OVERTIME                              
         GOTOR SVBRKDWN,DMCB,(0,WORK),(69,WORK+4)                               
*                                                                               
RTKHM390 L     R1,BLOCK+8          PAYMENT AMOUNT                               
         LA    RF,TASDIHMA                                                      
         B     RTKHMXX                                                          
*                                                                               
*        NO RETAKE OPTIONS                                                      
*                                                                               
RTKHMOF3 DS    0H                                                               
*                                                                               
RTKHMOFX DS    0H                                                               
*                                                                               
         L     R1,BLOCK+8          PAYMENT AMOUNT                               
         LA    RF,TASDIHMA                                                      
         B     RTKHMXX                                                          
*                                                                               
RTKHMOFN DS    0H                                                               
         DC    H'0'                                                             
*                                                                               
RTKHMXX  XIT1  REGS=(R1)                                                        
         DROP  R1,R5                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
* TRAVEL TIME HOURS                                                             
*---------------------------------------------------------------------          
RTKTRV   NTR1  BASE=*,LABEL=*      TRAVEL TIME HOURS HOURS/MINUTES              
*                                                                               
         SR    R0,R0                                                            
         SR    R1,R1                                                            
*                                                                               
         ICM   R1,3,TASDITR        N'TRAVEL TIME HOURS/MINUTES                  
         BZ    RTKTRVX             SKIP IF NONE                                 
*                                                                               
         CLC   TCCAONOF(2),=C'ON'  IF ON CAMERA                                 
         BNE   RTKTRONN                                                         
*                                                                               
         D     R0,=F'100'          CALCULATE HOURS                              
         LTR   R0,R0               IF EXTRA MINUTES                             
         BZ    *+8                                                              
         AHI   R1,1                   ADD AN HOUR                               
*                                                                               
         ICM   RF,15,TASDHR        GET HOURLY RATE                              
         MR    RE,R1               CALCULATE TRAVEL PAY                         
*                                                                               
         STCM  RF,15,TASDITRA      SAVE TRAVEL PAY                              
*                                                                               
RTKTRONX DS    0H                                                               
         MHI   R1,10000                                                         
         ST    R1,WORK+32          SAVE PAYMENT FOR TRAVEL FOR CHECK            
         GOTOR SVBRKDWN,DMCB,(0,TASDHR),('PVCTRV',WORK+32)                      
*                                                                               
         ICM   R1,15,TASDITRA      PAYMENT FOR RETURN                           
*                                                                               
         B     RTKTRVX                                                          
*                                                                               
RTKTRONN DS    0H                                                               
*                                                                               
*        OFF-CAMERA TRAVEL TIME                                                 
*                                                                               
RTKTROF  DS    0H                                                               
*                                                                               
         D     R0,=F'100'          CALCULATE HOURS                              
*                                                                               
                                                                                
******   SLL   R1,1                DOUBLE HRS FOR 30MIN PERIODS                 
*                                                                               
         LTR   R0,R0               IF EXTRA MINUTES                             
         BZ    RTKTROF1                                                         
*                                                                               
         AHI   R1,1                   ADD AN EXTRA  PERIOD                      
*                                                                               
*****    CHI   R0,30               IF OVER 30 MINS                              
*****    BNH   *+8                                                              
*****    AHI   R1,1                   ADD ANOTHER 30 MINUTE PERIOD              
*                                                                               
RTKTROF1 DS    0H                                                               
*                                                                               
         ICM   RF,15,TASDFEE       GET SESSION FEE                              
*                                                                               
         CLI   TGCACDE,C'P'        SKIP IF NOT PERFORMAER                       
         BNE   RTKTROFA                                                         
*                                                                               
         L     R5,TCATACO          ESTABLISH COMMERCIAL DETAILS ELEMENT         
         USING TACOD,R5                                                         
*                                                                               
         BRAS  RE,GETINDXT                                                      
         LR    R7,RF               ESTABLISH INDUSTRIAL RATES TABLE             
         USING TNDXD,R7                                                         
*                                                                               
         XR    RE,RE                                                            
*                                                                               
         L     RF,TNDXX301         OFF-CAMERA RTK ADDT'L HALF HR RATE           
*                                                                               
         CLI   TACOTYPE,CTYICAT2   CAT 2 INDUSTRIAL?                            
         BNE   *+8                                                              
         L     RF,TNDXX302                                                      
*                                                                               
         SLA   RF,1                DOUBLE FOR HOURLY RATE                       
*                                                                               
         DROP  R7                                                               
*                                                                               
RTKTROFA DS    0H                                                               
*                                                                               
         ST    RF,WORK                                                          
*                                                                               
         MR    RE,R1               * NUMBER OF 60 MIN PERIODS                   
*                                                                               
         STCM  RF,15,TASDITRA      SAVE TRAVEL RATE                             
*                                                                               
         MHI   R1,10000                                                         
         ST    R1,WORK+32                                                       
         GOTOR SVBRKDWN,DMCB,(0,WORK),('PVCTRV',WORK+32)                        
*                                                                               
         ICM   R1,15,TASDITRA      RETURN TRAVEL TIME PAYMENT                   
*                                                                               
RTKTRVX  DS    0H                                                               
         XIT1  REGS=(R1)                                                        
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
*              INDUSTRIAL AUDIO SESSION CALCULATION SUB-ROUTINES                
*---------------------------------------------------------------------          
*                                                                               
*        AUDIO SESSIONSS                                                        
*                                                                               
* TIME - MINIMUM SESSION RATE                                                   
* TIME - ADDT'L 60 MIN RATE AFTER                                               
*---------------------------------------------------------------------          
                                                                                
DIOHMM   NTR1  BASE=*,LABEL=*      WORK TIME HOURS/MINUTES                      
*                                                                               
         USING TASDD,R2            R2=A(SESSION DETAILS ELEMENT)                
*                                                                               
         L     R5,TCATACO          ESTABLISH COMMERCIAL DETAILS ELEMENT         
         USING TACOD,R5                                                         
*                                                                               
         BRAS  RE,GETINDXT                                                      
         LR    R3,RF               ESTABLISH INDUSTRIAL RATES TABLE             
         USING TNDXD,R3                                                         
*                                                                               
*        P3M REVERTS TO P IF ANY SESSION OVER 1/2 HR                            
*                                                                               
         CLI   TGCAEQU,CTP3M       SKIP IF NOT 3 MINUTE PRINCIPAL               
         BNE   DIOHMM03                                                         
*                                                                               
*        CALCULATE NUMBER OF 1/2 HRS                                            
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,TASDIRH        GET HOURS & MINS                             
*                                                                               
         SR    R0,R0                                                            
         D     R0,=F'100'          SPLIT INTO HRS & MINS                        
*                                                                               
         SLA   R1,1                DOUBLE HRS FOR 1/2 HRS                       
*                                                                               
         LTR   R0,R0               IF SOME MINUTES                              
         BZ    *+8                                                              
         AHI   R1,1                   ADD 1 1/2 HR                              
*                                                                               
         CHI   R0,30               IF OVER 30 MINS                              
         BNH   *+8                                                              
         AHI   R1,1                   ADD ANOTHER 1/2 HR                        
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,TASDIRP        GET NUMBER OF PROGRAMS                       
         BNZ   *+8                 IF NONE                                      
         LA    RF,1                   DEFAULT TO ONE                            
*                                                                               
         CR    R1,RF               IF MORE 1/2 HRS THAN PROGRAMS                
         BH    DIOHMM03               TREAT AS PRINCIPAL                        
*                                                                               
         LR    R1,RF               IF LESS ASSUME 1 1/2 HR PER PGM              
*                                                                               
         LA    RF,TNDXP3M1         ASSUME CAT 1                                 
*                                                                               
         CLI   TACOTYPE,CTYICAT2   CAT 2 INDUSTRIAL?                            
         BNE   *+8                                                              
         LA    RF,4(RF)               POINT TO CAT 2 RATE                       
*                                                                               
         ICM   RF,15,0(RF)         GET APPROPRIATE 1/2 HR RATE                  
         MR    RE,R1               * # OF HALF HOURS                            
*                                                                               
         ICM   RE,15,TASDIRHA      UPDATE TIME PORTION OF FEE                   
         AR    RE,RF                                                            
         STCM  RE,15,TASDIRHA      FINAL RATE                                   
*                                                                               
         B     DIOHMMX                                                          
*                                                                               
DIOHMM03 DS    0H                                                               
*                                                                               
         ICM   RF,15,TASDFEE       GET SESSION HOURLY RATE                      
*                                                                               
         STCM  RF,15,TASDHR        SET HOURLY RATE                              
*                                                                               
*        SECTION FOR PRINCIPAL AND NON-PRINCIPAL CAST                           
*                                                                               
         CLI   TGCAEQU,CTP         SKIP IF NOT PRINCIPAL                        
         BE    *+8                                                              
         CLI   TGCAEQU,CTNP        OR NON PRINCIPAL                             
         BE    *+8                                                              
         CLI   TGCAEQU,CTP3M       OR PRINCIPAL 3 MINUTES                       
         BNE   DIOHMMPN                                                         
*                                                                               
*        PAY FOR AT LEAST ONE HOUR PER PROGRAM                                  
*                                                                               
         SR    R1,R1                                                            
         IC    R1,TASDIRP          GET NUMBER OF PROGRAMS                       
*                                                                               
         MR    RE,R1               HOURLY RATE * # OF PRGS FOR MINIMUM          
*                                                                               
         STCM  RF,15,TASDIRHA      MINIMUM FEE                                  
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,TASDIRH        GET HOURS & MINS                             
*                                                                               
         SR    R0,R0                                                            
         D     R0,=F'100'          SPLIT INTO HRS & MINS                        
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,TASDIRP        GET NUMBER OF PROGRAMS                       
*                                                                               
         SR    R1,RF               NUMBER OF EXTRA HOURS                        
         BNM   *+12                IF FEWER HRS THAN PROGRAMS                   
         SR    R1,R1                  DEFAULT TO NO EXTRA HOURS                 
         SR    R0,R0                  AND FEE ALREADY SET                       
         B     DIOHMM10                                                         
*                                                                               
         SLA   R1,1                DOUBLE EXCESS HRS FOR EXCESS 1/2HRS          
         ST    R1,FULL             SAVE                                         
*                                                                               
*        CALCULATE # 0F 1/2HRS TO PAY                                           
*                                                                               
         LTR   R0,R0               IF ANY MINUTES                               
         BZ    *+8                                                              
         AHI   R1,1                   ASSUME 1 1/2HR EXTRA                      
*                                                                               
         CHI   R0,30               IF MORE THAN 30 MINUTES                      
         BNH   *+8                                                              
         AHI   R1,1                   ADD ANOTHER 1/2HR                         
*                                                                               
         LA    RF,TNDXPX1          PRINCIPAL EXTRA 1/2 HR RATE CAT1             
*                                                                               
         CLI   TGCAEQU,CTNP        IF NON PRINCIPAL                             
         BNE   *+8                                                              
         LA    RF,TNDXNPX1            NON PRINCIPAL 1/2 HR RATE CAT1            
*                                                                               
         CLI   TACOTYPE,CTYICAT2   CAT 2 INDUSTRIAL?                            
         BNE   *+8                                                              
         LA    RF,4(RF)               POINT TO CAT 2 RATE                       
*                                                                               
         ICM   RF,15,0(RF)         GET APPROPRIATE 1/2 HR RATE                  
         MR    RE,R1               * # OF HALF HOURS                            
*                                                                               
         ICM   RE,15,TASDIRHA      UPDATE TIME PORTION OF FEE                   
         AR    RF,RE                                                            
         STCM  RF,15,TASDIRHA                                                   
*                                                                               
DIOHMM10 DS    0H                                                               
*                                                                               
DIOHMMPX DS    0H                                                               
         B     DIOHMMX                                                          
*                                                                               
DIOHMMPN DS    0H                                                               
*                                                                               
*        SECTION FOR SINGERS                                                    
DIOHMM15 DS    0H                                                               
*                                                                               
*        PAY FOR AT LEAST ONE HOUR PER PROGRAM                                  
*                                                                               
         SR    R1,R1                                                            
         IC    R1,TASDIRP          GET NUMBER OF PROGRAMS                       
*                                                                               
         MR    RE,R1               HOURLY RATE * # OF PRGS FOR MINIMUM          
*                                                                               
         STCM  RF,15,TASDIRHA      MINIMUM FEE                                  
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,TASDIRH        GET HOURS & MINS                             
*                                                                               
         SR    R0,R0                                                            
         D     R0,=F'100'          SPLIT INTO HRS & MINS                        
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,TASDIRP        GET NUMBER OF PROGRAMS                       
*                                                                               
         SR    R1,RF               NUMBER OF EXTRA HOURS                        
         BNM   *+12                IF FEWER HRS THAN PROGRAMS                   
         SR    R1,R1                  DEFAULT TO NO EXTRA HOURS                 
         SR    R0,R0                  AND FEE ALREADY SET                       
         B     DIOHMM20                                                         
*                                                                               
*        CALCULATE # 0F HOURS TO PAY                                            
*                                                                               
         LTR   R0,R0               IF ANY MINUTES                               
         BZ    *+8                                                              
         AHI   R1,1                   ADD 1 HOUR                                
*                                                                               
         L     RF,TASDHR           GET HOURLY RATE                              
*                                                                               
         MR    RE,R1               * # OF HOURS                                 
*                                                                               
         ICM   RE,15,TASDIRHA      UPDATE TIME PORTION OF FEE                   
         AR    RE,RF                                                            
         STCM  RE,15,TASDIRHA      FINAL RATE                                   
*                                                                               
DIOHMM20 DS    0H                                                               
*                                                                               
         ICM   RF,15,TASDIRHA      GET HRS/MIN FEE                              
*                                                                               
         CLI   TGCACDE,C'C'        IF A CONTRACTOR                              
         BNE   DIOHMM25                                                         
*                                                                               
         LR    RE,RF               ADD 50% PREMIUM                              
         AHI   RE,1                ADD 1 FOR ROUNDING                           
         SRA   RE,1                HALVE RATE                                   
         AR    RF,RE               150% OF RATE                                 
*                                                                               
         STCM  RF,15,TASDIRHA      FINAL RATE                                   
*                                                                               
DIOHMM25 DS    0H                                                               
*                                                                               
*        PREMIUM FOR STEP OUT PERFORMERS                                        
*                                                                               
         CLI   TGCAEQU,CTSO        IF STEP OUT SINGER                           
         BNE   DIOHMM30                                                         
*                                                                               
         LA    RE,TNDXSTP1         ASSUME CATEGORY 1                            
*                                                                               
         CLI   TACOTYPE,CTYICAT2   CAT 2 INDUSTRIAL?                            
         BNE   *+8                                                              
         LA    RE,4(RE)               POINT TO CAT 2 RATE                       
*                                                                               
         ICM   RF,15,0(RE)         STEP OUT PREMIUM                             
*                                                                               
         SR    R1,R1                                                            
         IC    R1,TASDIRP          GET NUMBER OF PROGRAMS                       
         MR    RE,R1               TOTAL STEP OUT PREMIUM                       
*                                                                               
         LR    RE,RF               SAVE PREMIUM                                 
*                                                                               
         ICM   RF,15,TASDIRHA      GET HRS/MIN FEE                              
         AR    RF,RE               ADD PREMIUM TO RATE                          
         STCM  RF,15,TASDIRHA      FINAL RATE                                   
*                                                                               
DIOHMM30 DS    0H                                                               
*                                                                               
DIOHMMX  DS    0H                                                               
*                                                                               
*        SAVE BREAKOUT OF ONE SESSION'S PAY FOR CHECKS                          
*                                                                               
         GOTOR SVBRKDWN,DMCB,(0,TASDIRHA),('PVCIND',=X'00000001')               
*                                                                               
*        RETURN TOTAL PAYMENT                                                   
*                                                                               
         ICM   R1,15,TASDIRHA      SESSION FEE FOR TIME                         
*                                                                               
DIOHMXX  XIT1  REGS=(R1)                                                        
         DROP  R3,R5                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
*              INDUSTRIAL AUDIO SESSION CALCULATION SUB-ROUTINES                
*---------------------------------------------------------------------          
*                                                                               
*        RETAKES                                                                
*                                                                               
*---------------------------------------------------------------------          
                                                                                
DIORTK   NTR1  BASE=*,LABEL=*      WORK TIME HOURS/MINUTES                      
*                                                                               
         USING TASDD,R2            R2=A(SESSION DETAILS ELEMENT)                
*                                                                               
         L     R5,TCATACO          ESTABLISH COMMERCIAL DETAILS ELEMENT         
         USING TACOD,R5                                                         
*                                                                               
         BRAS  RE,GETINDXT                                                      
         LR    R3,RF               ESTABLISH INDUSTRIAL RATES TABLE             
         USING TNDXD,R3                                                         
*                                                                               
*        CALCULATE # 0F 1/2HRS TO PAY                                           
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,TASDIRR        GET HOURS & MINS OF RETAKES                  
         BZ    DIORTKX             SKIP IF NONE                                 
*                                                                               
         SR    R0,R0                                                            
         D     R0,=F'100'          SPLIT INTO HRS & MINS                        
*                                                                               
         SLA   R1,1                DOUBLE HRS FOR # OF 1/2HRS                   
*                                                                               
         LTR   R0,R0               IF ANY MINUTES                               
         BZ    *+8                                                              
         AHI   R1,1                   ASSUME 1 1/2HR EXTRA                      
*                                                                               
         CHI   R0,30               IF MORE THAN 30 MINUTES                      
         BNH   *+8                                                              
         AHI   R1,1                   ADD ANOTHER 1/2HR                         
*                                                                               
*        FIND SESSION RATE                                                      
*                                                                               
         CLI   TGCAEQU,CTP         IF PRINCIPAL                                 
         BNE   DIORTK05                                                         
*                                                                               
         LA    RF,TNDXRTK1            PRINCIPAL RETAKE 1/2 HR RATE CAT1         
*                                                                               
         CLI   TACOTYPE,CTYICAT2   CAT 2 INDUSTRIAL?                            
         BNE   *+8                                                              
         LA    RF,4(RF)               POINT TO CAT 2 RATE                       
*                                                                               
         ICM   RF,15,0(RF)         GET RETAKE RATE                              
*                                                                               
         B    DIORTK07                                                          
*                                                                               
DIORTK05 DS    0H                                                               
*                                                                               
         ICM   RF,15,TASDHR        GET HOURLY RATE                              
*                                                                               
         AHI   RF,1                ADD ONE FOR ROUNDING                         
         SRA   RF,1                HALF HOUR RATE                               
*                                                                               
DIORTK07 DS    0H                                                               
*                                                                               
         MR    RE,R1               * # OF HALF HOURS                            
*                                                                               
         ICM   RE,15,TASDIRRA      UPDATE TIME PORTION OF FEE                   
         AR    RF,RE                                                            
         STCM  RF,15,TASDIRRA                                                   
*                                                                               
DIORTK10 DS    0H                                                               
*                                                                               
*        SAVE BREAKOUT OF ONE SESSION'S PAY FOR CHECKS                          
*                                                                               
         GOTOR SVBRKDWN,DMCB,(0,TASDIRRA),('PVCADJ',=X'00000001')               
*                                                                               
*        RETURN RTAKE FEE                                                       
*                                                                               
         ICM   R1,15,TASDIRRA      SESSION FEE                                  
*                                                                               
DIORTKPX DS    0H                                                               
         B     DIORTKX                                                          
*                                                                               
DIORTKPN DS    0H                                                               
*                                                                               
DIORTKX  XIT1  REGS=(R1)                                                        
         DROP  R3,R5                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
*              INTERACTIVE VOICE SESSION CALCULATION SUB-ROUTINES               
*---------------------------------------------------------------------          
*                                                                               
*        TIME - HOURS/MINUTES                                                   
*                                                                               
*        RATE IS PAID IN 1/2 HOUR UNITS WOTH A MINIMUM OF 1 HOUR                
*                                                                               
*---------------------------------------------------------------------          
                                                                                
IVRHMM   NTR1  BASE=*,LABEL=*      WORK TIME HOURS/MINUTES                      
*                                                                               
         USING TASDD,R2            R2=A(SESSION DETAILS ELEMENT)                
*                                                                               
         L     R5,TCATACO          ESTABLISH COMMERCIAL DETAILS ELEMENT         
         USING TACOD,R5                                                         
*                                                                               
         BRAS  RE,GETINDXT                                                      
         LR    R3,RF               ESTABLISH INDUSTRIAL RATES TABLE             
         USING TNDXD,R3                                                         
*                                                                               
*        CALCULATE NUMBER OF 1/2 HRS                                            
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,TASDIVH        GET HOURS & MINS                             
         BZ    IVRHMMX             SKIP IF NO TIME ENTERED                      
*                                                                               
         SR    R0,R0                                                            
         D     R0,=F'100'          SPLIT INTO HRS & MINS                        
*                                                                               
         SLA   R1,1                DOUBLE HRS FOR 1/2 HRS                       
*                                                                               
         LTR   R0,R0               IF SOME MINUTES                              
         BZ    *+8                                                              
         AHI   R1,1                   ADD 1 1/2 HR                              
*                                                                               
         CHI   R0,30               IF OVER 30 MINS                              
         BNH   *+8                                                              
         AHI   R1,1                   ADD ANOTHER 1/2 HR                        
*                                                                               
         CHI   R1,2                MINIMUM OF 2 HALF HOURS PAID                 
         BNL   *+8                                                              
         LHI   R1,2                                                             
*                                                                               
         MVC   TASDIVHA,TASDHR     GET HOURLY RATE                              
         AHI   R1,-2                                                            
         BZ    IVRHMMX                                                          
*                                                                               
         ICM   RF,15,TNDXIVH1      GET ADDITIONAL 1/2 HOUR RATE                 
*                                                                               
IVRHMM05 MR    RE,R1               * # OF HALF HOURS                            
*                                                                               
         ICM   RE,15,TASDIVHA      UPDATE TIME PORTION OF FEE                   
         AR    RE,RF                                                            
         STCM  RE,15,TASDIVHA      FINAL RATE                                   
*                                                                               
IVRHMMX  DS    0H                                                               
*                                                                               
*        SAVE BREAKOUT OF ONE SESSION'S PAY FOR CHECKS                          
*                                                                               
         GOTOR SVBRKDWN,DMCB,(0,TASDIVHA),('PVCIND',=X'00000001')               
*                                                                               
*        RETURN TOTAL PAYMENT                                                   
*                                                                               
         ICM   R1,15,TASDIVHA      SESSION FEE FOR TIME                         
*                                                                               
IVRHMXX  XIT1  REGS=(R1)                                                        
         DROP  R3,R5                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
* TRAVEL TIME - HALF HOURS                                                      
*---------------------------------------------------------------------          
IVRTRV   NTR1  BASE=*,LABEL=*      TRAVEL TIME HOURS HOURS/MINUTES              
*                                                                               
         SR    R0,R0                                                            
         SR    R1,R1                                                            
*                                                                               
         ICM   R1,3,TASDIVT        N'TRAVEL TIME HOURS/MINUTES                  
         BZ    IVRTRVX             SKIP IF NONE                                 
*                                                                               
         D     R0,=F'100'          CALCULATE HOURS                              
*                                                                               
         SLA   R1,1                DOUBLE FOR # OF HALF HOURS                   
*                                                                               
         LTR   R0,R0               IF EXTRA MINUTES                             
         BZ    *+8                                                              
         AHI   R1,1                   ADD A HALF HOUR                           
*                                                                               
         CHI   R0,30               IF MORE THAN 30 MINUTES                      
         BNH   *+8                                                              
         AHI   R1,1                ADD ANOTHER HALF HOUR                        
*                                                                               
         ICM   RF,15,TASDHR        GET HOURLY RATE                              
         AHI   RF,1                ADD ONE FOR ROUNDING                         
         SRA   RF,1                HALVE FOR HALF HOUR RATE                     
*                                                                               
         MR    RE,R1               CALCULATE TRAVEL PAY                         
*                                                                               
         STCM  RF,15,TASDIVTA      SAVE TRAVEL PAY                              
*                                                                               
*                                  SAVE PAYMENT FOR TRAVEL FOR CHECK            
         GOTOR SVBRKDWN,DMCB,(0,TASDIVTA),('PVCTRV',=X'00000001')               
*                                                                               
         ICM   R1,15,TASDIVTA      PAYMENT FOR RETURN                           
*                                                                               
IVRTRVX  DS    0H                                                               
         XIT1  REGS=(R1)                                                        
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*              ROUTINE SAVES CURRENT USE INFO BEFORE LOOKING                    
*              UP SESSION OR HOLDING FEE RATE FOR APPLIED CREDITS               
         SPACE                                                                  
SAVUINFO NTR1  BASE=*,LABEL=*                                                   
         MVC   TCTUSSV,TCTUSES     SAVE TOTAL N'USES                            
         MVC   TCNUSSV,TCNUSES          N'USES PAID PREVIOUSLY                  
         MVC   TCNUSLSV,TCNUSESL                                                
         MVC   TCUSETSV,TCUSETAB        1ST ENTRY IN TCUSETAB                   
         MVC   TCUNITSV,TCUNITS         N'UNITS                                 
         MVC   TCINSSV,TCINSRTS         N'INSERTS                               
         SPACE                                                                  
         XC    TCTUSES,TCTUSES     CLEAR TOTAL N'USES                           
         XC    TCNUSES,TCNUSES           N'USES PAID PREVIOUSLY                 
         XC    TCNUSESL,TCNUSESL                                                
         XC    TCUSETAB(4),TCUSETAB      1ST ENTRY IN TCUSETAB                  
         XC    TCUNITS,TCUNITS           N'UNITS                                
         XC    TCINSRTS,TCINSRTS         N'INSERTS                              
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE RESTORES CURRENT USE NUMBER INFO SAVED                   
*              BY SAVUINFO                                                      
         SPACE                                                                  
RESUINFO NTR1  BASE=*,LABEL=*                                                   
         MVC   TCTUSES,TCTUSSV     RESTORE TOTAL N'USES                         
         MVC   TCNUSES,TCNUSSV             N'USES PAID PREVIOUSLY               
         MVC   TCNUSESL,TCNUSLSV                                                
         MVC   TCUSETAB(4),TCUSETSV        1ST ENTRY IN TCUSETAB                
         MVC   TCUNITS,TCUNITSV            N'UNITS                              
         MVC   TCINSRTS,TCINSSV            N'INSERTS                            
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO APPLY COST OF LIVING INCREASE                         
         SPACE 1                                                                
*                                  RF=AMOUNT TO APPLY                           
DRINK    NTR1  BASE=*,LABEL=*                                                   
         LR    R1,RF               R1=AMOUNT                                    
         LA    R3,TCCOLAS          R3=APPLICABLE COLA INCREASE RATES            
         LA    R4,TCNCOLAS         R4=MAXIMUM NUMBER OF COLA RATES              
*                                                                               
DRNK2    OC    0(L'TCCOLAS,R3),0(R3) TEST NO MORE COLAS TO BE APPLIED           
         BZ    DRNKX                                                            
         LH    RE,0(R3)            RE=COLA RATE                                 
         LR    RF,R1               RF=AMOUNT SO FAR                             
         MR    RE,RE               DETERMINE INCREASE                           
         D     RE,=F'500'                                                       
         LTR   RF,RF               ROUND INCREASE NORMALLY                      
         BM    *+8                                                              
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         AR    R1,RF               ADD TO ORIGINAL                              
*                                                                               
         CLI   TGYREQU,CN85        IF THIS IS 85 CONTRACT OR LATER              
         BL    *+8                                                              
         BAS   RE,NICK5            ROUND NEW PAYMENT TO NEAREST NICKEL          
*                                                                               
         LA    R3,L'TCCOLAS(R3)    BUMP TO NEXT                                 
         BCT   R4,DRNK2                                                         
*                                                                               
DRNKX    ST    R1,FULL             RETURN IN FULL                               
         XIT1                                                                   
*                                                                               
NICK5    DS    0H                                                               
         AHI   R1,2                ADD 2                                        
         XR    R0,R0                                                            
         D     R0,=F'5'            DIVIDE BY FIVE                               
         M     R0,=F'5'            MULTIPLY BY FIVE                             
         LTR   R1,R1               ADD 1 IF NEGATIVE                            
         BNM   *+8                                                              
         AHI   R1,1                                                             
         BR    RE                                                               
         EJECT                                                                  
*              ROUTINE TO ESTABLISH NUMERIC USE CODE FOR RATE LOOK-UP           
*              RETURNS CC NOT EQUAL IF NOT FOUND                                
         SPACE 1                                                                
GETUSE   NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,LOOKUSE          SET TCUSENUM USING USELUT                    
         BNE   GETUNO                                                           
         CLI   TGUSEQU,URRN        RADIO REGIONAL NETWORK                       
         BNE   GETU5                                                            
         CLI   TCMAJORS,0          IF WITH MAJORS                               
         BE    *+10                                                             
         MVC   TCUSENUM,=H'34'     USE NUMBER IS 34                             
         B     GETUYES                                                          
         SPACE 1                                                                
GETU5    CLI   TGUSEQU,UFGR        FOREIGN REUSE                                
         BNE   GETU10                                                           
         CLI   TCMAJORS,0          DON'T BOTHER IF NO MAJORS (OLD TYPE)         
         BE    GETUYES                                                          
         XR    R1,R1               R1=UNITS                                     
         TM    TCMAJORS,UK         IF HAVE UK                                   
         BZ    *+8                                                              
         AHI   R1,3                ADD 3 UNITS                                  
         TM    TCMAJORS,EUR        IF HAVE EUR                                  
         BZ    *+8                                                              
         AHI   R1,2                ADD 2 UNITS                                  
         SPACE                                                                  
         CLI   TGYREQU,CN94        SKIP IF CONTRACT YEAR < 94                   
         BL    GETU7                                                            
         TM    TCMAJORS,JAP        ELSE IF HAVE JAP                             
         BZ    *+8                                                              
         AHI   R1,1                ADD 1 UNIT                                   
GETU7    TM    TCMAJORS,AP         IF HAVE AP                                   
         BZ    GETU7A                                                           
         AHI   R1,1                ADD 1 UNIT                                   
         CLI   TGYREQU,CN13        SKIP IF CONTRACT YEAR < 13                   
         BL    GETU7A                                                           
         AHI   R1,1                ADD 1 UNIT MORE                              
GETU7A   TM    TCMAJORS,REST       IF HAVE REST                                 
         BZ    *+8                                                              
         AHI   R1,1                ADD 1 UNIT                                   
         STH   R1,TCUNITS          SAVE IN TCUNITS                              
         SPACE                                                                  
GETU10   CLI   TGUSEQU,UBSC        CANADIAN SESSION                             
         BNE   GETU10A                                                          
         CLI   TGMEEQU,NEWMEDIA    IF NEW MEDIA,                                
         BNE   GETU10B                                                          
         USING TACOD,RE                                                         
         L     RE,TCATACO                                                       
         CLI   TACOCTYP,CCTYADO    AND ACTRA TYPE AUDIO                         
         BNE   GETU10B                                                          
         DROP  RE                                                               
         MVI   TGMEEQU,RADIO       SET AS MEDIA RADIO                           
         BRAS  RE,LOOKUSE          SET TCUSENUM USING USELUT                    
         MVI   TGMEEQU,NEWMEDIA    THEN RESET BACK TO NEW MEDIA                 
         BNE   GETUNO                                                           
         B     GETU10B                                                          
GETU10A  CLI   TGUSEQU,UDOR        CANADIAN DORMANCY FEE (TV)                   
         BE    GETU10B                                                          
         CLI   TGUSEQU,UCNM        CANADIAN NEW MEDIA                           
         BE    GETU10B                                                          
         CLI   TGUSEQU,UNMC        NEW MEDIA CANADIAN                           
         BNE   GETU13                                                           
GETU10B  BRAS  RE,GETCORDR         FIND ORDER # OF COMML ACTRA TYPE             
         BE    GETU11              (RETURNED IN TGBYTE)                         
         OI    TGUSSTA2,NORATES    DONT HAVE RATES                              
         B     GETUNO              (RETURNED IN TGBYTE)                         
GETU11   LH    R3,TCUSENUM         USENUM                                       
         ZIC   R1,TGBYTE                                                        
         AR    R3,R1               + ORDER #                                    
         STH   R3,TCUSENUM         = ADJUSTED USENUM                            
         B     GETUYES                                                          
         SPACE                                                                  
GETU13   CLI   TGUSEQU,UWSP        WILDSPOTS                                    
         BNE   GETU15                                                           
*        CLI   TGUSEQU,UADW        WILDSPOTS                                    
*        BNE   GETU15                                                           
         L     R2,TCMAJLUT                                                      
         CLC   TCMAJORS,1(R2)      ADJUST BASED ON ACTUAL MAJORS                
         BE    *+12                                                             
         LA    R2,2(R2)                                                         
         B     *-14                                                             
         LH    R1,TCUSENUM                                                      
         ZIC   RE,0(R2)                                                         
         AR    R1,RE                                                            
         STH   R1,TCUSENUM                                                      
         B     GETUYES                                                          
         SPACE 1                                                                
GETU15   TM    TGUSSTA3,ADDENUSE   IF ADDENDUM USE                              
         BZ    GETU16                                                           
         SPACE                                                                  
         BRAS  RE,GETORDER         FIND ORDER # OF ADDENDUM STATE               
         BNE   GETUNO              (RETURNED IN TGBYTE)                         
         SPACE                                                                  
         LH    R3,TCUSENUM           USENUM                                     
         ZIC   R1,TGBYTE                                                        
         AR    R3,R1               + ORDER #                                    
         STH   R3,TCUSENUM         = ADJUSTED USENUM                            
         B     GETUYES                                                          
         SPACE                                                                  
GETU16   CLI   TGUSEQU,UIMS         IF IMS PAYMENT, AND                         
         BNE   GETU17                                                           
         CLC   TCIPCYCS,=X'A00213'  IF ON OR AFTER 2/13/00                      
         BL    GETUYES                                                          
         MVC   TCUSENUM,=H'257'     ADJUST TCUSENUM FOR NEW IMS RATES           
         CLC   TCIPCYCS,=X'A10118'  IF BETWEEN 2/13/00-1/18/01                  
         BL    GETUYES                                                          
         MVC   TCUSENUM,=H'258'     ADJUST TCUSENUM AGAIN                       
         CLC   TCIPCYCS,=X'A20216'  IF BETWEEN 1/18/01-2/16/02                  
         BL    GETUYES                                                          
         MVC   TCUSENUM,=H'370'     ADJUST TCUSENUM AGAIN                       
         CLC   TCIPCYCS,=X'A30216'  IF BETWEEN 2/16/02-2/16/03                  
         BL    GETUYES                                                          
         MVC   TCUSENUM,=H'371'     ADJUST TCUSENUM AGAIN                       
         CLC   TCIPCYCS,=X'A40216'  IF BETWEEN 2/16/03-2/16/04                  
         BL    GETUYES                                                          
         MVC   TCUSENUM,=H'372'     ADJUST TCUSENUM AGAIN                       
         CLC   TCIPCYCS,=X'A51201'  IF BETWEEN 2/16/04-12/01/05                 
         BL    GETUYES                                                          
         MVC   TCUSENUM,=H'373'     ADJUST TCUSENUM AGAIN                       
         B     GETUYES                                                          
*                                                                               
*ETU17   TM    TGUSXUNI,AFM        IF MUSIC PAYMENT                             
GETU17   GOTOR UNITEST,DMCB,TGUSXUNS,AFM,0,0,0                                  
         BO    GETU22                                                           
         CLI   TGYREQU,CN98        AND CONTRACT YR >= 98, ADJUST AFM            
         BL    GETU22                                                           
         LA    R1,ADJTAB           R1=A(ADJUSTMENT TABLE)                       
GETU19   CLC   TCUSENUM,0(R1)      SEE IF MATCH OLD USE NUMBER                  
         BE    GETU20                                                           
         LA    R1,L'ADJTAB(R1)     IF NOT, BUMP TO NEXT ENTRY                   
         CLI   0(R1),X'FF'         CHECK NOT END OF TABLE                       
         BE    GETUYES                                                          
         B     GETU19                                                           
*                                                                               
GETU20   MVC   TCUSENUM,2(R1)      REPLACE TCUSENUM WITH NEW USE NUM            
         CLI   TGYREQU,CN01                                                     
         BL    GETUYES                                                          
         MVC   TCUSENUM,4(R1)      REPLACE TCUSENUM WITH 01 USE NUM             
         CLI   TGYREQU,CN04                                                     
         BL    GETUYES                                                          
         MVC   TCUSENUM,6(R1)      REPLACE TCUSENUM WITH 04 USE NUM             
         CLI   TGYREQU,CN07                                                     
         BL    GETUYES                                                          
         MVC   TCUSENUM,8(R1)      REPLACE TCUSENUM WITH 07 USE NUM             
         CLI   TGYREQU,CN10                                                     
         BL    GETUYES                                                          
         MVC   TCUSENUM,10(R1)     REPLACE TCUSENUM WITH 09 USE NUM             
*2013    CLI   TGYREQU,CN13                                                     
*RATES   BL    GETUYES                                                          
*NOTUSED MVC   TCUSENUM,12(R1)     REPLACE TCUSENUM WITH 13 USE NUM             
         CLI   TGYREQU,CN14                                                     
         BL    GETUYES                                                          
         MVC   TCUSENUM,14(R1)     REPLACE TCUSENUM WITH 14 USE NUM             
         B     GETUYES                                                          
*                                                                               
GETU22   CLI   TGUSEQU,UFGM        IF FGM                                       
         BNE   GETU25                                                           
         CLI   TGYREQU,CN87        AND CONTRACT YEAR IS 87                      
         BNE   GETUYES                                                          
         LH    R1,TCUSENUM         ADJUST USE NUMBER CAUSE RATES                
         AHI   R1,4                DIFFER FROM 89'S                             
         STH   R1,TCUSENUM                                                      
         B     GETUYES                                                          
         SPACE                                                                  
GETU25   CLI   TGUSEQU,UREN        IF REINSTATEMENT,                            
         BNE   GETU27                                                           
         BRAS  RE,ADJRSMW          ADJUST TCUSENUM FOR SOCIAL MEDIA WVR         
         BE    GETUYES                                                          
*                                                                               
GETU27   CLI   TGYREQU,CN92        IF CONTRACT YEAR IS 92                       
         BNE   GETUYES                                                          
         CLI   TGUSEQU,UBSM        FOR BSM                                      
         BNE   GETU30                                                           
         MVC   TCUSENUM,=H'59'     ADJUST USE NUMBER BECAUSE RATES              
         B     GETUYES             DIFFER FROM 91'S                             
         SPACE                                                                  
GETU30   CLI   TGUSEQU,UNBM        FOR NBM, ADJUST USE NUMBER                   
         BE    GETU40                                                           
         CLI   TGUSEQU,UMUS        FOR MUS                                      
         BNE   GETUYES                                                          
         MVC   TCUSENUM,=H'58'     ADJUST USE NUMBER FOR 8 WEEK TYPE            
         CLI   TGUSWKS,8           IF NOT 8 WEEK CYCLE TYPE                     
         BE    GETUYES                                                          
GETU40   MVC   TCUSENUM,=H'57'     ADJUST USE NUMBER FOR 13 WEEK TYPE           
*                                                                               
GETUYES  XR    RC,RC                                                            
GETUNO   LTR   RC,RC                                                            
         XIT1                                                                   
*              TABLE OF NEW USE NUMS FOR 98 AFM CONTRACT                        
         SPACE 1                                                                
*                                  +0 = OLD USE NUMBER                          
*                                  +2 = 98 USE NUMBER                           
*                                  +4 = 01 USE NUMBER                           
*                                  +6 = 04 USE NUMBER                           
*                                  +8 = 07 USE NUMBER                           
*                                 +10 = 10 USE NUMBER                           
*                                 +12 = 13 USE NUMBER  (NOT USED)               
*                                 +14 = 14 USE NUMBER                           
ADJTAB   DS    0CL16                                                            
         DC    AL2(35,79,359,375,400,415,423,431)                               
         DC    AL2(09,89,360,378,403,418,426,434)                               
         DC    AL2(36,114,361,376,401,416,424,432)                              
         DC    AL2(45,124,362,379,404,419,427,435)                              
         DC    AL2(47,149,364,381,406,421,429,437)                              
         DC    AL2(46,159,363,380,405,420,428,436)                              
         DC    AL2(48,184,365,382,407,422,430,438)                              
         DC    AL2(60,194,366,377,402,417,425,433)                              
         DC    AL2(500,501,502,503,504,505,506,507)                             
         DC    AL2(510,511,512,513,514,515,516,517)                             
         DC    AL2(520,521,522,523,524,525,526,527)                             
         DC    AL2(530,531,532,533,534,535,536,537)                             
         DC    X'FF'                                                            
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE LOOKS FOR APPLIED CREDIT ELEMENTS                        
*              GETS THE LATEST ELEMENT WITH THE CORRECT CYCLE DATES             
*              RETURNS ITS ADDRESS IN TGELEM                                    
         SPACE 1                                                                
GETTACR  NTR1  BASE=*,LABEL=*                                                   
         NI    TCSTAT,ALL-TCSTAPPH                                              
         XC    TGELEM,TGELEM                                                    
         XC    TGDUB,TGDUB                                                      
         ICM   R4,15,TCACAST       THEY'RE IN CAST RECORD                       
         BZ    GTCR10                                                           
         MVI   ELCODE,TACRELQ      SET ELEMENT CODE                             
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
GTCR2    BRAS  RE,NEXTEL                                                        
         BNE   GTCR10                                                           
         USING TACRD,R4                                                         
         TM    TCCASTA3,TACASXFT   IF CAST DOES NOT APPLY TO HOLDING            
         BO    GTCR2A              FEE FTRACKS                                  
         CLI   TGMEEQU,CABLE       OR PAYING CABLE COMMERCIAL                   
         BNE   GTCR3                                                            
GTCR2A   CLC   TACRUSE,=C'HLD'     NEVER APPLY AGAINST A HOLDING                
         BE    GTCR2               FEE GENERATED FTRACK                         
         CLC   TACRUSE,=C'SHL'                                                  
         BE    GTCR2                                                            
         CLC   TACRUSE,=C'ADH'                                                  
         BE    GTCR2                                                            
         SPACE 1                                                                
GTCR3    TM    TACRSTAT,TACRHDLR   IGNORE HOLDING FEE FTRACKS ADDED             
         BO    GTCR2               BY DEALER PAYMENTS                           
         TM    TACRSTAT,TACRSSEA   NEVER APPLY TO SEASONAL HOLDING FEE          
         BO    GTCR2                                                            
         CLC   TCIPCYCS,TACRSTRT   CYCLE START MUST FALL BETWEEN START          
         BL    GTCR2                                                            
         CLC   TCIPCYCS,TACREND    AND END                                      
         BH    GTCR2                                                            
         OC    TACRINV,TACRINV     IF THERE'S AN INV NUM                        
         BZ    *+14                                                             
         CLC   TACRINV,TGDUB       TEST THIS INV LATER THAN SAVED               
         BNH   GTCR2                                                            
         SPACE                                                                  
         CLC   TACRUSE,=C'GRR'     IF FTRACK WAS CREATED BY GRR                 
         BNE   GTCR6                                                            
         CLI   TACRTYPE,0          TO COVER A SPECIFIC USE                      
         BE    GTCR6                                                            
         CLC   TACRTYPE,TGUSEQU    ONLY APPLY AGAINST IT IF PAYING              
         BNE   GTCR2               THAT USE                                     
         SPACE                                                                  
         USING TACOD,RE                                                         
GTCR6    CLI   TGUSEQU,UFGR        IF PAYING FOREIGN REUSE                      
         BNE   GTCR7                                                            
         L     RE,TCATACO                                                       
         CLI   TACOTYPE,CTYFGN     TO A FOREIGN COMMERCIAL                      
         BNE   GTCR7                                                            
         CLC   TACRUSE,=C'FGS'     APPLY AGAINST FGS CREATED FTRACKS            
         BE    GTCR9                                                            
         CLC   TACRUSE,=C'LFT'     AND LFT CREATED FTRACKS                      
         BE    GTCR9                                                            
         DROP  RE                                                               
         SPACE 1                                                                
GTCR7    CLI   TGUSEQU,ULNA        IF PAYING LATE NIGHT USE                     
         BE    GTCR8                                                            
         CLI   TGUSEQU,ULNC                                                     
         BE    GTCR8                                                            
         CLI   TGUSEQU,ULNN                                                     
         BE    GTCR8                                                            
         CLI   TGUSEQU,ULNF                                                     
         BE    GTCR8                                                            
         CLI   TGUSEQU,UINR                                                     
         BE    GTCR8                                                            
         CLI   TGUSEQU,UFGR                                                     
         BNE   GTCR9                                                            
GTCR8    TM    TACRSTAT,TACRSTRK+TACRSGUA                                       
         BNO   GTCR2                                                            
         CLC   TACRUSE,=C'BSS'     ONLY BSS OR HLD OVERSCALE                    
         BE    GTCR9               ELEMENTS APPLY                               
         CLC   TACRUSE,=C'HLD'                                                  
         BE    GTCR9                                                            
         CLI   TGUSEQU,UFGR        OR IF USE IS FGR                             
         BNE   GTCR2                                                            
         CLC   TACRUSE,=C'LFT'     LFT AND SLF OVERSCALE                        
         BE    GTCR9               ELEMENTS APPLY AS WELL                       
         CLC   TACRUSE,=C'SLF'                                                  
         BE    GTCR9                                                            
         SPACE                                                                  
GTCR9    STCM  R4,7,TGELEM+1       SAVE A(LAST ELEMENT)                         
         MVC   TGDUB(L'TACRINV),TACRINV                                         
         B     GTCR2               KEEP LOOKING TILL NO MORE ELEMENTS           
         SPACE                                                                  
GTCR10   L     R4,TGELEM                                                        
         LTR   R4,R4               TEST FOUND AN ELEMENT                        
         BZ    GTCRNO                                                           
         OC    TACRBAL,TACRBAL     TEST FOR REMAINING BALANCE                   
         BZ    GTCRNO                                                           
         TM    TACRBAL,X'80'       TEST BALANCE IS POSITIVE                     
         BO    GTCRNO                                                           
         CLC   TACRUSE,=C'HLD'     IF ELEMENT FOUND IS FROM HLD FEE             
         BE    GTCR15                                                           
         CLC   TACRUSE,=C'SHL'     OR SPANISH HOLDING FEE                       
         BE    GTCR15                                                           
         CLC   TACRUSE,=C'ADH'     OR ADDENDUM HOLDING FEE                      
         BE    GTCR15                                                           
         CLC   TACRUSE,=C'REN'     OR REINSTATEMENT                             
         BE    GTCR15                                                           
         CLC   TACRUSE,=C'GRR'     OR RADIO GUARANTEE                           
         BNE   *+8                                                              
GTCR15   OI    TCSTAT,TCSTAPPH     SET APPLIED CREDIT IS FROM HLD FEE           
         B     GTCRYES             AND CC EQ                                    
*                                                                               
GTCRYES  XR    RC,RC                                                            
GTCRNO   LTR   RC,RC                                                            
         XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*----------------------------------------------------------------------         
*              ROUTINE TO ESTABLISH APPLICABLE COLAS                            
*----------------------------------------------------------------------         
         USING COLAD,R2                                                         
SETCOLA  NTR1  BASE=*,LABEL=*                                                   
         XC    TCCOLAS(TCLCOLAS),TCCOLAS                                        
*                                                                               
         CLI   OVERLAY,X'70'       IGNORE PERFORMERS GETTING CAN. RATES         
         BL    SETC1               PHASES X'70' - X'78' CAN. RATES              
         CLI   OVERLAY,CANRATE9                                                 
         BH    SETC1                                                            
         B     SETCX                                                            
SETC1    CLI   TGUSEQU,UCAB        IGNORE FOR CAB USE                           
         BE    SETCX                                                            
         CLI   TGUSEQU,UCBL                                                     
         BE    SETCX                                                            
         CLI   TGUSEQU,USCB                                                     
         BE    SETCX                                                            
*                                                                               
         LA    R2,COLATAB          R2=A(COST OF LIVING INCREASE TABLE)          
         LA    R3,TCCOLAS          R3=A(COST OF LIVING ACCUMULATORS)            
*                                                                               
*ETC2    MVC   BYTE,COLAUN         ALLOWABLE UNIONS                             
*        NC    BYTE,TGUNEQU        TEST THIS UNION APPLIES                      
SETC2    GOTOR UNITEST,DMCB,(X'80',COLAUN1),TGUNEQUS                            
         BZ    SETC6                                                            
*                                                                               
         CLI   COLAYEAR,ALL        TEST APPLIES TO ALL CONTRACT YEARS           
         BE    *+14                                                             
         CLC   TGYREQU,COLAYEAR    ELSE MUST MATCH CONTRACT YEAR                
         BNE   SETC6                                                            
         GOTOR GETDTE              GET DATE RETURNED IN WORK                    
*                                                                               
         CLC   WORK(3),COLAFRST    MUST FALL WITHIN SPECIFIED PERIOD            
         BL    SETC6                                                            
         CLC   WORK(3),COLALAST                                                 
         BH    SETC6                                                            
*                                                                               
         TM    COLASTAT,X'80'      SPECIAL ROUTINE FOR 75, 77 CONTRACTS         
         BZ    *+12                                                             
         BAS   RE,SPEC7577         RETURNS CONDITION CODE                       
         BNE   SETC6                                                            
*                                                                               
         LA    R1,COLAREG          SET TO SAVE REGULAR COLA                     
         TM    TGCATYPE,EXTRA      TEST FOR EXTRAS                              
         BZ    *+8                                                              
         LA    R1,COLAEXT          THEY GET DIFFERENT RATE                      
*                                                                               
         MVC   0(L'TCCOLAS,R3),0(R1)                                            
         LA    R3,L'TCCOLAS(R3)    BUMP TO NEXT COLA ACCUMULATOR                
*                                                                               
SETC6    LA    R2,COLANEXT         NEXT TABLE ENTRY                             
*                                                                               
         CLI   COLAUN,0            END OF TABLE                                 
         BNE   SETC2                                                            
*                                                                               
SETCX    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*----------------------------------------------------------------------         
*              SPECIAL COLA ROUTINE FOR 1975 AND 1977 CONTRACTS                 
*----------------------------------------------------------------------         
SPEC7577 DS    0H                                                               
         OC    TCOV1(8),TCOV1      IF NO OVERSCALE HAS BEEN NEGOTIATED          
         BNZ   SPECNO                                                           
*                                                                               
         MVC   WORK(3),TCUSEDTE    AND THE USE DATE                             
         OC    WORK(3),WORK                                                     
         BNZ   *+10                                                             
         MVC   WORK(3),TCIPCYCS    OR THE CYCLE START DATE                      
*                                                                               
         CLC   WORK(3),=X'800515'  IS AFTER MAY15/80                            
         BNH   SPECNO                                                           
*                                                                               
         CR    RB,RB               THEN RETURN CC EQUAL (TAKE COLA)             
         BR    RE                                                               
*                                                                               
SPECNO   LTR   RB,RB               RETURN CC NOT EQUAL                          
         BR    RE                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
*              COST OF LIVING INCREASE TABLE                                    
*---------------------------------------------------------------------          
COLATAB  DS    0C                                                               
         DC    AL1(ALL-AFM,ALL,ALL,ALL)                                         
         DC    AL1(CN75),X'770516781218',X'00',AL2(91,91)                       
         DC    AL1(ALL-AFM,ALL,ALL,ALL)                                         
         DC    AL1(CN77),X'000000FFFFFF',X'00',AL2(91,91)                       
         DC    AL1(ALL-AFM,ALL,ALL,ALL)                                         
         DC    AL1(CN75),X'751116770516',X'80',AL2(100,50)                      
         DC    AL1(ALL-AFM,ALL,ALL,ALL)                                         
         DC    AL1(CN77),X'751116770516',X'80',AL2(100,50)                      
         DC    AL1(ALL-AFM,ALL,ALL,ALL)                                         
         DC    AL1(CN79),X'800516FFFFFF',X'00',AL2(100,50)                      
         DC    AL1(ALL-AFM,ALL,ALL,ALL)                                         
         DC    AL1(CN80),X'000000FFFFFF',X'00',AL2(100,50)                      
         DC    AL1(ALL-AFM,ALL,ALL,ALL)                                         
         DC    AL1(CN75),X'751116781219',X'00',AL2(58,80)                       
         DC    AL1(ALL-AFM,ALL,ALL,ALL)                                         
         DC    AL1(CN77),X'751116781219',X'00',AL2(58,80)                       
         DC    AL1(ALL-AFM,ALL,ALL,ALL)                                         
         DC    AL1(CN79),X'790207820207',X'00',AL2(58,80)                       
         DC    AL1(ALL-AFM,ALL,ALL,ALL)                                         
         DC    AL1(CN82),X'830807850207',X'00',AL2(58,80)                       
         DC    AL1(ALL-AFM,ALL,ALL,ALL)                                         
         DC    AL1(CN85),X'860807FFFFFF',X'00',AL2(00,50)                       
         DC    AL1(ALL-AFM,ALL,ALL,ALL)                                         
         DC    AL1(CN86),X'860807FFFFFF',X'00',AL2(00,50)                       
         DC    AL1(ALL-AFM,ALL,ALL,ALL)                                         
         DC    AL1(CN87),X'860807FFFFFF',X'00',AL2(00,50)                       
         DC    AL1(0)                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO ADJUST SHORT VERSION CLASS A PAYMENTS                 
*              R4=AMOUNT TO ADJUST, RETURNS RESULT IN R4                        
         SPACE 1                                                                
ADJDEMO  NTR1  BASE=*,LABEL=*                                                   
         CLC   TCUSENUM,=AL2(43)   IF USE TYPE DEMO (AFT RADIO)                 
         BNE   ADEMOX                                                           
         CLI   TCDEMO,5            AND 5 OR MORE DEMOS BEING PAID               
         BL    ADEMOX                                                           
         CLI   TGYREQU,CN00        AND CONTRACT YEAR IS 00                      
         BNE   ADEMOX                                                           
         CLI   TCROW,9             IF CATEGORY IS SOLO/DUO                      
         BE    ADEMO10                                                          
         CLI   TGCAEQU,CTG3        OR SINGERS 3-5                               
         BE    ADEMO10                                                          
         CLI   TGCAEQU,CTG6        OR SINGERS 6-8                               
         BE    ADEMO10                                                          
         CLI   TGCAEQU,CTG9        OR SINGERS 9+                                
         BE    ADEMO10                                                          
         CLI   TGCAEQU,CTG3M       OR W/MULTI 3-5                               
         BE    ADEMO10                                                          
         CLI   TGCAEQU,CTG6M       OR W/MULTI 6-8                               
         BE    ADEMO10                                                          
         CLI   TGCAEQU,CTG9M       OR W/MULTI 9+                                
         BNE   ADEMOX                                                           
         SPACE 1                                                                
ADEMO10  ZIC   RE,TCDEMO           CALCULATE PER DEMO RATE                      
         LR    R1,R4               R1=CAST MEMBER GROSS                         
         XR    R0,R0               RE=# OF DEMOS                                
         DR    R0,RE                                                            
         MHI   R1,4                                                             
         LR    R4,R1               R4=FIRST 4 DEMO TOTAL                        
         SPACE                                                                  
         ZIC   RE,TCDEMO                                                        
         AHI   RE,-4               RE=# OF DEMOS TO MULTIPLY                    
         CLI   TCROW,9                                                          
         BNE   *+12                                                             
         MHI   RE,3825             BY $38.25 IF SOLO/DUO                        
         B     *+8                                                              
         MHI   RE,2500             BY $25 IF GROUP                              
         AR    R4,RE               RETURN NEW TOTAL IN R4                       
ADEMOX   XIT1  REGS=(R4)                                                        
         LTORG                                                                  
         EJECT                                                                  
*              ADJUSTMENT ROUTINE FOR MEDIA RADIO                               
         SPACE 1                                                                
         USING TACOD,R2            R2=A(COMM'L DETAILS ELEM)                    
ADJRAD   NTR1  BASE=*,LABEL=*                                                   
         CLI   TGUSEQU,UDLR        IF DEALER USE                                
         BE    ARAD3                                                            
         CLI   TGUSEQU,UPUB        OR PUB USE                                   
         BE    ARAD3                                                            
         CLI   TGUSEQU,UPBS        OR PBS USE                                   
         BNE   ARAD5                                                            
ARAD3    CLI   TGCAEQU,CTP         TEST FOR ACTORS                              
         BE    ARAD7                                                            
         CLI   TGCAEQU,CTACR                                                    
         BE    ARAD7                                                            
         CLI   TGCAEQU,CTANN       AND ANNOUNCERS                               
         BE    ARAD7                                                            
         B     ARADX                                                            
         SPACE                                                                  
ARAD5    TM    TACOSTA2,TACOSANO   ELSE, TEST ANN IS ONLY NON-MUSCN             
         BZ    ARADX                                                            
         CLI   TGCAEQU,CTANN       FOR ANNOUNCERS ONLY                          
         BNE   ARADX                                                            
ARAD7    BRAS  RE,ROWDECR          SUBTRACT 1 FROM ROW                          
ARADX    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO ESTABLISH PAY RATES FOR SOAP RESIDUALS                
*              R2=A(EPISODE INFO IN TSAR REC)                                   
*                                                                               
         USING EPISD,R2                                                         
GETSRATE NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,CABPPV           CABLE OR PPV USE?                            
         BE    GETSR5                                                           
         ZIC   R3,TCROW            ROW NUMBER                                   
         BCTR  R3,0                DON'T MULTIPLY FIRST ROW BY 4                
         SLL   R3,2                *4 (L'EACH ROW) = DISP. TO ROW = R3          
         AHI   R3,12               L'1ST ROW                                    
*                                                                               
         LA    R4,4(R3)            +4 (L'ROWS)                                  
         STC   R4,TCMINLTB         = MINIMUM L'TABLE                            
*                                                                               
         L     R4,TCUSETBL         R4=A(USE RATE TABLE) W/2 BYTE USENUM         
         USING USETBLD,R4                                                       
         XR    R1,R1                                                            
*                                                                               
GETSR2   CLI   0(R4),X'FF'         END OF TABLE                                 
         BE    GETSRX                                                           
         CLC   TCUSENUM,USETBNUM   MATCH ON USE NUMBER FROM TCUSELUT            
         BE    GETSR4                                                           
*                                                                               
         ZICM  R1,USETBLN,2        BUMP TO NEXT ENTRY (2 BYTE LENGTH)           
         AR    R4,R1                                                            
         B     GETSR2                                                           
*                                                                               
GETSR4   CLC   USETBLN+1(1),TCMINLTB      IS TABLE LONG ENOUGH?                 
         BL    GETSRX              (NO INCREMENT FOR GIVEN ROW)                 
*                                                                               
         LA    RE,0(R4,R3)         RE=A(FULLWORD INCREMENT)                     
         L     RF,0(RE)            RF=RATE                                      
         B     GETSR8                                                           
         DROP  R4                                                               
*                                                                               
GETSR5   ICM   R1,15,TCGRSEPI      FOR CABLE                                    
         BRAS  RE,SETRTEP          SET TOTAL RATE/EPISODE                       
         TM    TCOPTS,TCNEWSOC     IF NEW RATE OPTION USED                      
         BZ    GETSR6                                                           
         LH    R0,TCSOCR           TOTAL RATE/EPISODE IS OVERRIDE VAL           
         B     GETSR6                                                           
*                                                                               
GETSR5D  OC    TCEPIAIR,TCEPIAIR   IF NOT TALENT AND NO AIR DATE                
         BZ    GETSR6                                                           
         CLC   TCEPIAIR,=X'840701' OR AIR DATE ON OR AFTER 7/1/84               
         BNL   GETSR6                                                           
         LA    R0,250              TOTAL RATE/EPISODE IS 2.5% OF GROSS          
*                                                                               
GETSR6   MR    R0,R0               USE R0                                       
         D     R0,=F'5000'                                                      
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
*                                                                               
         LR    RF,R1               SET RF=RATE/EPISODE                          
         CLI   TGUSEQU,USDC                                                     
         BE    GETSR20                                                          
         CLI   TGUSEQU,USDN                                                     
         BE    GETSR20                                                          
         CLI   TGUSEQU,USDP                                                     
         BE    GETSR20                                                          
         CLI   TGUSEQU,USOC                                                     
         BE    GETSR9                                                           
         CLI   TGUSEQU,USON                                                     
         BE    GETSR9                                                           
         CLI   TGUSEQU,USPP                                                     
         BE    GETSR9                                                           
         B     *+12                                                             
*                                                                               
GETSR8   TM    TGCATYPE,WRITER     FOR WRITERS                                  
         BZ    GETSR20                                                          
         XR    R1,R1                                                            
         ICM   R1,7,EPIWPCT        ONLY GETS EPIWPCT OF IT                      
         MR    RE,R1                                                            
         D     RE,=F'50000'                                                     
         LTR   RF,RF                                                            
         BM    *+8                                                              
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         B     GETSR20                                                          
*                                                                               
GETSR9   OC    TCNCON(2),TCNCON    FOR AFTRA TALENT, IF HAVE COUNTS/EPI         
         BZ    GETSR20                                                          
         ZIC   R1,TCNCON           R1=CREDITS TO DIVIDE RATE INTO               
         CLI   TCNU5,0             IF HAVE U/5'S                                
         BE    GETSR12                                                          
         AR    R1,R1               DOUBLE THE # OF CONTRACT AND F/C'S           
         ZIC   R0,TCNU5                                                         
         AR    R1,R0               ADD THE # OF U/5'S                           
GETSR12  SR    RE,RE                                                            
         SLA   RF,1                                                             
         DR    RE,R1               DIVIDE RATE BY NUMBER OF CREDITS             
         LTR   RF,RF                                                            
         BM    *+8                                                              
         AHI   RF,1                                                             
         SRA   RF,1                RF=ROUNDED RATE/CREDIT                       
*                                                                               
         CLI   TGCAEQU,CTU5        IF U5, GETS 1 CREDIT                         
         BE    GETSR20                                                          
         CLI   TGCAEQU,CTU52                                                    
         BE    GETSR20                                                          
         CLI   TCNU5,0             ELSE IF DON'T HAVE U/5'S,                    
         BE    GETSR20             ALSO GETS 1 CREDIT                           
         AR    RF,RF               ELSE GETS 2 CREDITS                          
*                                                                               
GETSR20  STCM  RF,7,EPIPAY         SAVE RATE FOR THIS EPISODE                   
         A     RF,TCGROSS          ADD TO TOTAL SO FAR                          
         ST    RF,TCGROSS                                                       
GETSRX   XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*----------------------------------------------------------------------         
*----------------------------------------------------------------------         
SPDYSESS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING TASDD,R2            R2=A(SESSION DETAILS ELEMENT)                
         ZIC   R1,TASDSP                                                        
         LA    RF,TASDSPA                                                       
         CLC   TASDSP,TASDDAY      TAKE HIGHER OF SPOTS & DAYS                  
         BNL   *+12                                                             
         IC    R1,TASDDAY                                                       
         LA    RF,TASDDAA                                                       
*                                                                               
         OC    TCATMTOT,TCATMTOT   ARE WE PAYING A TIMESHEET?                   
         BZ    SDSP10                                                           
         LA    RE,TASDSPA                                                       
         CR    RF,RE               SPOTS OR DAYS?                               
         BNE   *+12                                                             
         BRAS  RE,TIMESPT          CALCULATE TIMESHEET SPOTS                    
         B     *+8                                                              
         BRAS  RE,TIMEDAY          CALCULATE TIMESHEET DAYS                     
*                                                                               
         LR    R5,R1               SAVE IT BEFORE BREAK DOWN                    
         STCM  R1,15,WORK                                                       
         GOTOR SVBRKDWN,DMCB,(0,TASDFEE),(51,WORK)                              
         LR    R1,R5               RESTORE IT                                   
*                                                                               
         M     R0,TASDFEE                                                       
         D     R0,=F'100'          DIVIDE BY 100                                
         B     SDSXIT              FINISH UP CALCULATION                        
*                                                                               
SDSP10   LR    R5,R1               SAVE IT BEFORE BREAK DOWN                    
         STCM  R1,15,WORK                                                       
         GOTOR SVBRKDWN,DMCB,(0,TASDFEE),(51,WORK)                              
         LR    R1,R5               RESTORE IT                                   
*                                                                               
         M     R0,TASDFEE          * SESSION FEE                                
SDSXIT   XIT1  REGS=(R1)           FINISH UP CALCULATION                        
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE SETS NUMERIC USE CODE BASED ON USELUT                    
*              RETURNS CC NOT EQUAL IF NOT FOUND                                
         SPACE                                                                  
         USING USELUTD,R4                                                       
LOOKUSE  NTR1  BASE=*,LABEL=*                                                   
         L     R2,TCUSELUT         R2=A(USE LOOK-UP TABLE)                      
         MVI   DUB,0                                                            
         LA    R4,DUB              R1=A(DUB)=WHERE ENTRY WILL BE MOVED          
         SPACE 1                                                                
LOOKU20  CLI   0(R2),X'FF'         END OF TABLE                                 
         BE    LOOKUNO                                                          
         MVC   DUB+1(8),0(R2)      SAVE ENTRY TO DUB+1                          
         LA    R3,USELLNQ-1(R2)    POINT R3 TO NEXT ENTRY                       
         BRAS  RE,CHKNEW           IF USING NEW STYLE RATE TABLES               
         BNE   LOOKU30                                                          
         MVC   DUB(9),0(R2)        SAVE ENTRY TO DUB (2 BYTE USENUM)            
         LA    R3,USELLNQ(R2)      ADJUST R3                                    
         SPACE                                                                  
LOOKU30  CLC   TGUSEQU,USELCDE     MUST MATCH ON USE CODE                       
         BNE   LOOKU40                                                          
         CLI   USELTYPE,ALL        ALL TYPES                                    
         BE    *+14                                                             
         CLC   USELTYPE,TGUSTYP    ELSE MATCH ON TYPE                           
         BNE   LOOKU40                                                          
*        MVC   BYTE,USELUNI        AND UNION                                    
*        NC    BYTE,TGUNEQU                                                     
         GOTOR UNITEST,DMCB,(X'80',USELUNI1),TGUNEQUS                           
         BZ    LOOKU40                                                          
         MVC   BYTE,USELMED        AND MEDIA                                    
         NC    BYTE,TGMEEQU                                                     
         BNZ   LOOKU60                                                          
LOOKU40  LR    R2,R3               BUMP TO NEXT                                 
         B     LOOKU20                                                          
         SPACE 1                                                                
LOOKU60  MVC   TCUSENUM,USELNUM    SAVE NUMERIC USE NUMBER                      
*                                                                               
         CLI   TGUSEQU,URTK        IF RTK INDUSTRIAL                            
         BNE   LOOKU610                                                         
*                                                                               
         CLC   =C'OFF',TCCAONOF       IF RTK OFF CAMERA                         
         BNE   LOOKU610                                                         
*                                                                               
         LH    R1,TCUSENUM               BUMP USE NUMBER UP 2                   
         AHI   R1,2                                                             
         STH   R1,TCUSENUM                                                      
*                                                                               
LOOKU610 DS    0H                                                               
*                                                                               
         CLI   TGUSEQU,UINS        IF INS                                       
         BE    *+8                                                              
         CLI   TGUSEQU,UIDS        OR IDS INDUSTRIAL                            
         BE    *+8                                                              
         CLI   TGUSEQU,URTK        OR RTK INDUSTRIAL                            
         BE    *+8                                                              
         CLI   TGUSEQU,UDIO        OR DIO INDUSTRIAL                            
         BNE   LOOKU90                                                          
*                                                                               
         USING TACOD,R2                                                         
         L     R2,TCATACO                                                       
*                                                                               
         CLI   TACOTYPE,CTYICAT2   IF CAT 2 INDUSTRIAL?                         
         BNE   LOOKU90                                                          
*                                                                               
         LH    R1,TCUSENUM            BUMP USE NUMBER UP 1                      
         AHI   R1,1                                                             
         STH   R1,TCUSENUM                                                      
*                                                                               
LOOKU90  CLI   TGUSEQU,UINS        IF INS                                       
         BE    *+8                                                              
         CLI   TGUSEQU,UIDS        OR IDS INDUSTRIAL                            
         BE    *+8                                                              
         CLI   TGUSEQU,USTR        OR STR INDUSTRIAL                            
         BE    *+8                                                              
         CLI   TGUSEQU,URTK        OR RTK INDUSTRIAL                            
         BE    *+8                                                              
         CLI   TGUSEQU,UDIO        OR DIO INDUSTRIAL                            
         BE    *+8                                                              
         CLI   TGUSEQU,UIVR        OR IVR INDUSTRIAL                            
         BNE   LOOKU95                                                          
*                                                                               
         CLC   TCPCYCS,=X'B61101'  CYCLE HAS TO BE NOV 1, 2016 OR LATER         
         JL    LOOKU95             NO, YEAR 1                                   
         LH    R1,TCUSENUM         YEAR 2, BUMP USE NUMBER UP 5                 
         AHI   R1,5                                                             
         STH   R1,TCUSENUM                                                      
*                                                                               
LOOKU95  MVC   TCUSENSV,TCUSENUM   SAVE USE NUMBER                              
*                                                                               
LOOKUYS  XR    RC,RC               INDICATE NO ERRORS                           
*                                                                               
LOOKUNO  LTR   RC,RC                                                            
         XIT1                                                                   
*                                                                               
         DROP  R2,R4                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO SAVE PAYMENT BREAKDOWN COMPONENT                      
*                                P1 BYTE 0    X'80'=ADJUSTMENT RATE             
*                                   BTYE 1-3  A(RATE TO ADD TO TABLE)           
*                                P2 BYTE 0-1  CODE FOR DESCRIPTION              
*                                   BYTE 2-3  HOUR UNIT, SESSIONS               
*                                                                               
SVBRKDWN NTR1  BASE=*,LABEL=*                                                   
         CLI   ACTNUM,ACTPAY       IF NOT SAVING BREAKDOWN INFORMATION          
         BNE   SBDX                EXIT                                         
         TM    TCSTAT,TCNOSVBK                                                  
         BO    SBDX                                                             
         TM    TGUSSTA2,NORATES    TEST HAVE RATES                              
         BZ    SBD05               YES, CONTINUE                                
*        TM    TGUNEQU,ACT         NO RATES AND ACTRA                           
*        GOTOR UNITEST,SBDMCB,TGUNEQUS,ACT,0,0,0                                
         GOTOR UNITEST,PARAS,TGUNEQUS,ACT,0,0,0                                 
         BO    SBDX                DON'T SHOW BREAKDOWN                         
         LA    R1,DMCB                                                          
*                                                                               
SBD05    L     R3,0(R1)            R3=A(RATE)                                   
         L     R4,4(R1)            R4=A(HOURLY UNIT)                            
         LA    R4,0(R4)            STRIP HOB                                    
         MVC   TGBYTE,0(R1)        TGBYTE=STATUS BYTE                           
*                                                                               
         MVI   TCCSTBRK,PBBRKSES   SET AS SESSION BREAKDOWN                     
         TM    TCPAYST2,TCEDS                                                   
         BO    SBD10                                                            
         TM    TGUSSTAT,SESSION    UNLESS PAYING REUSE                          
         BZ    SBD10                                                            
         TM    TGUSSTA3,BRKBUNT    OR A SESSION THAT BREAKS DOWN BY             
         BO    SBD10               UNIT                                         
         TM    TCSTAT2,TCSTSTAG    OR CALCULATING TAGS ON A SESSION             
         BZ    SBD20                                                            
SBD10    MVI   TCCSTBRK,PBBRKUNT   THEN SET UNIT/USE BREAKDOWN                  
*                                                                               
         USING PAYBRKDD,R2                                                      
SBD20    LA    R2,TCCSTBRK                                                      
         LA    RF,L'TCCSTBRK(R2)   RF=A(END OF PAYMENT BREAKDOWN BLOCK)         
         LA    R2,PBBRKDAT         R2=A(CURRENT ENTRY IN BKDOWN BLOCK)          
         DROP  R2                                                               
*                                                                               
         USING PBDATAD,R2                                                       
SBD30    CR    R2,RF               IF PROGRAM DIES HERE, BREAKDOWN              
         BL    *+6                 IS TOO LARGE FOR TCCSTBRK TO HOLD            
         DC    H'00'                                                            
*                                                                               
         OC    PBCODE(PBUNTLNQ),PBCODE   IF EMPTY ENTRY FOUND,                  
         BNZ   SBD40                     SKIP AHEAD TO SAVE ...                 
         TM    TGBYTE,SBDADJST     IF ADJUSTING PREVIOUSLY SAVED                
         BO    SBD70               AMOUNT, DO SO NOW                            
         CLI   TCCSTBRK,PBBRKUNT         UNIT/USE BREAKDOWN                     
         BE    SBD70                                                            
         CLI   TCCSTBRK,PBBRKSES         OR SESSION BREAKDOWN                   
         BE    SBD200                                                           
         DC    H'00'                                                            
*                                                                               
SBD40    CLI   4(R1),1             IF NEW ENTRY IS OVERSCALE                    
         BNE   SBD42                                                            
         CLI   PBCODE,1            AND OVERSCALE HAS ALREADY BEGUN              
         BE    SBD43                                                            
         B     SBD50                                                            
*                                                                               
SBD42    CLI   4(R1),8             IF NEW ENTRY IS SWEET/MULT                   
         BNE   SBD45               BEING ACCUMULATED                            
         CLI   PBCODE,8            AND SWEET/MULT HAS ALREADY BEGUN             
         BNE   SBD50               BEING ACCUMULATED                            
*                                                                               
SBD43    ZICM  RE,PBUNITRT,4                                                    
         A     RE,0(R3)            ADD ON TOP OF IT                             
         STCM  RE,15,PBUNITRT                                                   
         B     SBDX                AND EXIT                                     
*                                                                               
SBD45    CLI   PBCODE,5            JUST PAYMENT                                 
         BNE   SBD50                                                            
         CLI   4(R1),0             COULD BE AN ADJUSTED 5                       
         BE    SBD70                                                            
*                                                                               
SBD50    CLI   TCCSTBRK,PBBRKUNT   IF SAVING A USE/UNIT BREAKDOWN               
         BNE   SBD60               AND THIS RATE WAS ALREADY SAVED              
         CLI   4(R1),0                                                          
         BNE   SBD60                                                            
         CLC   PBUNITRT,0(R3)      (AT LEAST ONCE)                              
         BE    SBD70               ADJUST THE EXISTING ENTRY                    
*                                                                               
SBD60    LA    R2,PBUNTLNQ(R2)                                                  
         B     SBD30                                                            
*====================================================================           
SBD70    MVC   PBUNITRT,0(R3)      SAVE RATE AND CODE                           
         MVC   PBCODE,4(R1)                                                     
*                                                                               
         TM    TGUSTYST,USES+UNITS                                              
         BNZ   SBD80                                                            
         CLI   TGUSEQU,UDEM                                                     
         BE    SBD80                                                            
         CLI   TGUSEQU,USNA                                                     
         BE    SBD80                                                            
         CLI   TGUSEQU,UCDM                                                     
         BE    SBD80                                                            
         CLI   TGUSEQU,UTAG        TAG SHOULD SAVE NUMBER OF TAGS               
         BE    SBD71                                                            
         TM    TCPAYST2,TCVNR                                                   
         BO    SBD71                                                            
         CLI   TGUSEQU,UVAR                                                     
         BE    SBD80                                                            
         CLI   TGUSEQU,ULCB                                                     
         BE    SBD80                                                            
         CLI   TGUSEQU,UACB                                                     
         BE    SBD80                                                            
         TM    TCSTAT2,TCSTSTAG                                                 
         BO    SBD80                                                            
SBD71    CLI   PBCODE,PVCTAG       TAGS?                                        
         BE    SBD71A                                                           
         CLI   PBCODE,PB1VAR                                                    
         BE    SBD71A                                                           
         CLI   PBCODE,PBE4VR                                                    
         BNE   SBD72                                                            
SBD71A   XR    RF,RF                                                            
         ICM   RF,3,0(R4)          NUMBER OF TAGS                               
         STCM  RF,15,PBUNITUN                                                   
         B     SBD80                                                            
*                                                                               
SBD72    CLI   PBCODE,0                                                         
         BNE   SBD80                                                            
         MVI   PBCODE,5                                                         
         XC    PBUNITST(12),PBUNITST                                            
*                                                                               
SBD80    TM    TGBYTE,SBDADJST     OR IF THIS IS AN ADJUSTMENT RATE             
         BO    SBDX                                                             
         CLI   PBCODE,0            OR CODE IS PROVIDED                          
         BNE   SBDX                                                             
*                                                                               
         ZICM  RE,PBUNITUN,4       SAVE NUMBER OF TIMES THIS RATE               
         AHI   RE,1                HAS BEEN ENCOUNTERED                         
         STCM  RE,15,PBUNITUN                                                   
*                                                                               
         OC    TCTUSES,TCTUSES                                                  
         BZ    SBD90                                                            
         LH    R0,TCUSEN                                                        
*                                                                               
SBD90    TM    TCSTAT2,TCSTSTAG    IF CALCULATING TAGS ON A SESSION             
         BZ    SBD100                                                           
         CLI   TGUSEQU,UTAG                                                     
         BE    SBD100                                                           
         MVI   PBCODE,57           SET AS TAGS                                  
*                                                                               
SBD100   STCM  R0,15,PBUNITEN      SAVE UNIT/USE RANGE TOO                      
*                                                                               
         OC    PBUNITST,PBUNITST                                                
         BNZ   SBDX                                                             
         STCM  R0,15,PBUNITST                                                   
         B     SBDX                                                             
         EJECT                                                                  
*----------------------------------------------------------------------         
* SESSION                                                                       
*----------------------------------------------------------------------         
SBD200   DS    0H                                                               
         TM    TGBYTE,SBDADJST     OR IF THIS IS AN ADJUSTMENT                  
         BZ    SBD210                                                           
         MVC   PBUNITUN,=X'00000001'      DEFAULT                               
         B     SBD230                                                           
*                                                                               
SBD210   CLI   4(R1),PBCAPP                                                     
         BE    SBD230                                                           
         CLI   4(R1),PBCPLY                                                     
         BE    SBD230                                                           
         CLI   4(R1),PBCDBL                                                     
         BE    SBD230                                                           
         CLI   4(R1),PVCADJ                                                     
         BE    SBD230                                                           
         CLI   4(R1),PVCHNW                                                     
         BE    SBD230                                                           
*                                                                               
SBD220   LTR   R4,R4                                                            
         BZ    SBDX                                                             
         L     RF,0(R4)                                                         
         LTR   RF,RF                                                            
         BZ    SBDX                                                             
         CLC   0(L'PBUNITRT,R3),=X'000000000000'                                
         BE    SBDX                                                             
         MVC   PBUNITUN,0(R4)      SAVE HOURLY UNIT                             
         MVC   PBUNITEN,4(R4)                                                   
SBD230   MVC   PBUNITRT,0(R3)      SAVE RATE AND CODE                           
         MVC   PBCODE,4(R1)                                                     
SBDX     XIT1                                                                   
         DROP  R2                                                               
*                                                                               
SBDADJST EQU   X'80'               ADJUSTMENT                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO SUBTRACT AMOUNT FROM 1ST PAYMENT BREAKDOWN            
*                                P1 BYTE 0    X'80'=ADJUSTMENT RATE             
*                                   BTYE 1-3  A(RATE TO ADD TO TABLE)           
*                                                                               
         USING PAYBRKDD,R2                                                      
SUBRKDWN NTR1  BASE=*,LABEL=*                                                   
         L     R3,0(R1)            A(AMOUNT TO SUBTRACT)                        
         LA    R2,TCCSTBRK                                                      
         LA    R2,PBBRKDAT                                                      
         USING PBDATAD,R2                                                       
         OC    PBCODE(PBUNTLNQ),PBCODE   MUST HAVE BREAKOUT DETAIL              
         BZ    SUBX                                                             
         L     RF,0(R3)            GET AMOUNT                                   
         ICM   RE,15,PBUNITRT                                                   
         SR    RE,RF                                                            
         STCM  RE,15,PBUNITRT                                                   
SUBX     XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
*              ROUTINE CALCULATES GROSS AMOUNT AND SETS SUBJECT                 
*              TO P&H FOR SOAP RESIDUALS                                        
         SPACE                                                                  
GRSCALC  NTR1  BASE=*,LABEL=*                                                   
         L     R1,TCPAY            PAYMENT                                      
         A     R1,TCAPPLCR         + APPLIED AMOUNT                             
         ST    R1,TCGROSS          = GROSS                                      
         SPACE 1                                                                
         MVC   TCSUBPNH,TCPAY      SUBJECT TO P&H = PAYMENT AMOUNT              
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
ROWDECR  NTR1  BASE=*,LABEL=*                                                   
         ZIC   R1,TCROW            SUBTRACT ONE FROM ROW INDICATOR              
         AHI   R1,-1                                                            
         BP    *+8                                                              
         LA    R1,1                INSURE NON-ZERO ROW                          
         STC   R1,TCROW                                                         
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO SET RATE/EPISODE FOR SOAP CABLE AND PPV               
*              RETURN RATE IN R0                                                
         SPACE                                                                  
         USING EPISD,R2                                                         
SETRTEP  NTR1  BASE=*,LABEL=*                                                   
         LA    R0,200              DEFAULT TO 2%                                
         SPACE 1                                                                
         CLI   TGUSEQU,USPP        3.6% FOR SOAP PAY PER VIEW                   
         BNE   SRE10                                                            
         LA    R0,360                                                           
         SPACE 1                                                                
SRE10    CLI   TGUSEQU,USDP        1.2% FOR SOAP DIRECTIOR PPV                  
         BE    SRE20                                                            
         CLI   TGUSEQU,USWP        AND SOAP WRITERS PPV                         
         BNE   SRE30                                                            
SRE20    LA    R0,120                                                           
         B     SREX                                                             
SRE30    CLI   TGUSEQU,USON        6% FOR SOAP PERFORMER NEW MEDIA              
         BNE   SREX                                                             
         LA    R0,600                                                           
         SPACE 1                                                                
SREX     XIT1  REGS=(R0)                                                        
         DROP  R2                                                               
         SPACE 2                                                                
         EJECT                                                                  
*              ROUTINE DETERMINES IF USE IS A SOAP CABLE OR PAY                 
*              PER VIEW                                                         
         SPACE                                                                  
CABPPV   NTR1  BASE=*,LABEL=*                                                   
         CLI   TGUSEQU,USOC                                                     
         BE    CABPPVY                                                          
         CLI   TGUSEQU,USON                                                     
         BE    CABPPVY                                                          
         CLI   TGUSEQU,USPP                                                     
         BE    CABPPVY                                                          
         CLI   TGUSEQU,USDC                                                     
         BE    CABPPVY                                                          
         CLI   TGUSEQU,USDN                                                     
         BE    CABPPVY                                                          
         CLI   TGUSEQU,USDP                                                     
         BE    CABPPVY                                                          
         CLI   TGUSEQU,USWC                                                     
         BE    CABPPVY                                                          
         CLI   TGUSEQU,USWN                                                     
         BE    CABPPVY                                                          
         CLI   TGUSEQU,USWP                                                     
         BNE   CABPPVN                                                          
CABPPVY  XR    RC,RC                                                            
CABPPVN  LTR   RC,RC                                                            
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE SETS THE USE INFO FOR CAB IF NEEDED                      
*              OR SETS THE MAX UNITS FOR NEW CONTRACT RULE                      
*              SETS CC NO IF UPGRADE TYPE FOR CAB                               
         SPACE                                                                  
SETCAB   NTR1  BASE=*,LABEL=*                                                   
         CLI   TGUSEQU,UCBL        TEST FOR CBL USE                             
         BE    *+12                                                             
         CLI   TGUSEQU,USCB        OR SCB USE                                   
         BNE   SETCABX                                                          
         CLI   TGYREQU,CN91        IF CONTRACT YEAR BEFORE 91                   
         BNL   SETCAB7                                                          
         TM    TGUSTYST,UPGRADE    IF UPGRADE                                   
         BO    SETCABNO            SET CC NO TO SKIP LOOKUP (NO CHECK)          
         SPACE                                                                  
SETCAB5  MVC   TCUNITSV,TCUNITS    SAVE UNITS FOR CBL & SCB                     
         XC    TCUNITS,TCUNITS     CLEAR UNITS FOR CAB                          
         MVI   HALF,UCAB                                                        
         MVI   BYTE,UCABTV                                                      
         GOTO1 USEVAL,DMCB,(X'80',HALF),BYTE                                    
         B     SETCABX                                                          
         SPACE                                                                  
SETCAB7  CLI   TGYREQU,CN92        IF CONTRACT YEAR 91 OR 92                    
         BH    SETCABX                                                          
         MVC   TCUNITSV,TCUNITS    SAVE UNITS INPUT                             
         SPACE 1                                                                
         CLC   TCIPCYCS,=X'920807' IF CYCLE START DATE BEFORE 8/7/92            
         BNL   SETCABX                                                          
         CLC   TCUNITS,=H'160'     AND IF UNITS INPUT > 160                     
         BNH   SETCABX                                                          
         MVC   TCUNITS,=H'160'     SET UNITS TO 160 FOR OLD MAX                 
SETCABX  XR    RC,RC                                                            
SETCABNO LTR   RC,RC                                                            
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE RESETS THE USE INFO TO CBL IF NEEDED                     
         SPACE                                                                  
RESETCBL NTR1  BASE=*,LABEL=*                                                   
         CLI   TGUSEQU,UCAB        IF CAB USE                                   
         BNE   RSETCAB5                                                         
         CLI   TGUSTYP,UCABTV      AND BROADCAST TV TYPE                        
         BNE   RCX                                                              
         USING TACOD,R2                                                         
         L     R2,TCATACO                                                       
         MVC   TCUNITS,TCUNITSV    RESTORE UNITS FOR CBL, SCB                   
         MVI   HALF,UCBL           RESET USE TO CBL                             
         CLI   TACOTYPE,CTYSPAN    IF COMM'L TYPE IS SPANISH                    
         BNE   *+8                                                              
         MVI   HALF,USCB           RESET USE TO SCB                             
         MVI   BYTE,0                                                           
         GOTO1 USEVAL,DMCB,(X'80',HALF),BYTE                                    
         B     RCX                                                              
         SPACE                                                                  
RSETCAB5 CLI   TGUSEQU,UCBL        IF CBL USE                                   
         BE    *+12                                                             
         CLI   TGUSEQU,USCB        OR SCB                                       
         BNE   RCX                                                              
         CLI   TGYREQU,CN91        AND IF CONTRACT YEAR 91 OR 92                
         BL    RCX                                                              
         CLI   TGYREQU,CN92                                                     
         BH    RCX                                                              
         MVC   TCUNITS,TCUNITSV    RESTORE UNITS                                
RCX      XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE HANDLES APPLIED CREDITS FOR THIS EPISODE                 
*              AND ACCUMULATES PAYMENT AMOUNT AND SAVES SUBJ TO P&H             
*              R2=A(EPISODE INFO IN TSAR REC)                                   
         SPACE                                                                  
         USING EPISD,R2                                                         
SACRCALC NTR1  BASE=*,LABEL=*                                                   
         XR    R1,R1                                                            
         ICM   R1,7,EPIPAY         R1=PAYMENT AMOUNT FOR THIS EPISODE           
         SPACE                                                                  
         CLC   TCEPIAIR,=X'950301' IF AIR DATE ON OR AFTER 3/1/95               
         BNL   SACRCX              DON'T APPLY CREDITS                          
         SPACE                                                                  
         TM    EPISTAT,EPISTBAL    TEST HAVE CREDIT BALANCE LEFT                
         BZ    SACRCX                                                           
         XR    R3,R3                                                            
         ICM   R3,7,EPIBAL         R3=CREDIT BALANCE                            
         CR    R1,R3               IF PAYMENT AMOUNT < CREDIT BALANCE           
         BNL   *+6                                                              
         LR    R3,R1               ONLY APPLY PAYMENT AMOUNT                    
         SR    R1,R3                                                            
         STCM  R1,7,EPIPAY         SAVE PAYMENT AMT IN TSAR REC                 
         STCM  R3,7,EPIAPPL        SAVE APPLIED AMOUNT IN TSAR REC              
         SPACE                                                                  
         LTR   R3,R3               TEST APPLIED AMOUNT NOT 0                    
         BZ    SACRCX                                                           
         MVI   TCAPPLCD,APPLSESS   SET APPLIED CODE FOR SESSION                 
         A     R3,TCAPPLCR                                                      
         ST    R3,TCAPPLCR         ACCUMULATE TOTAL APPLIED CREDITS             
         SPACE                                                                  
SACRCX   A     R1,TCPAY            ACCUMULATE THIS EPISODE'S PAY AMOUNT         
         ST    R1,TCPAY            INTO TOTAL PAY                               
         ST    R1,TCSUBPNH         AND ALSO SAVE AS SUBJ TO P&H                 
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              UPGRADES - CALCULATE DIFFERENCE AND SET CYCLE ELEMENT            
         SPACE 1                                                                
UPGCALC  NTR1  BASE=*,LABEL=*                                                   
         L     R1,TCUPGRS          NEW GROSS                                    
         L     RF,TCGROSS          (SAVE ORIGINAL GROSS IN RF)                  
         SPACE                                                                  
         CLI   TGUSEQU,UCAB        IF NOT CABLE UPGRADE                         
         BE    UPGC2                                                            
         SR    R1,RF               LESS ORIGINAL GROSS                          
         BNM   *+6                                                              
         SR    R1,R1               (MAKE SURE DOESN'T GO NEGATIVE)              
UPGC2    ST    R1,TCGROSS          IS NEW GROSS                                 
         SPACE                                                                  
         CLI   TGUSEQU,UWSP        TEST FOR WILDSPOT                            
         BE    UPGC3                                                            
         CLI   TGUSEQU,USNT                                                     
         BE    UPGC3                                                            
         CLI   TGUSEQU,UADW                                                     
         BE    UPGC3                                                            
         CLI   TGUSEQU,USWS                                                     
         BE    UPGC3                                                            
         CLI   TGUSEQU,USNW                                                     
         BE    UPGC3                                                            
         CLI   TGUSEQU,UIFB        OR IFB                                       
         BE    UPGC3                                                            
         CLI   TGUSEQU,UCBL        OR CBL                                       
         BE    UPGC3                                                            
         CLI   TGUSEQU,USCB        OR SCB                                       
         BE    UPGC3                                                            
         CLI   TGUSEQU,ULCB        OR LCB                                       
         BE    UPGC3                                                            
         CLI   TGUSEQU,UACB        OR ACB                                       
         BE    UPGC3                                                            
         CLI   TGUSEQU,UCAB        OR CABLE                                     
         BNE   UPGC5                                                            
UPGC3    TM    TCINPUT,TCINAPPL    IF NO APPLIED AMOUNT ALREADY                 
         BO    UPGC5                                                            
         LTR   RF,RF               TEST AMOUNT NOT 0                            
         BZ    UPGC5                                                            
         ST    RF,TCAPPLCR         SAVE ORIGINAL AMOUNT AS APPLIED              
         MVI   TCAPPLCD,APPLWSPU   SET APPLIED CDE FOR WILDSPOT UPGRADE         
         CLI   TGUSEQU,UCBL                                                     
         BE    UPGC4                                                            
         CLI   TGUSEQU,USCB                                                     
         BE    UPGC4                                                            
         CLI   TGUSEQU,ULCB                                                     
         BE    UPGC4                                                            
         CLI   TGUSEQU,UACB                                                     
         BE    UPGC4                                                            
         CLI   TGUSEQU,UCAB        IF CABLE OR CBL OR SCB OR LCB                
         BNE   *+8                                                              
UPGC4    MVI   TCAPPLCD,APPLCAB    SET APPLIED CODE FOR CABLE                   
         SPACE 1                                                                
UPGC5    MVC   TCMAJORS,TCUPMAJ    RESTORE NEW SUBSIDIARY DETAILS               
         MVC   TCUNITS,TCUPUNTS    NEW MAJORS, UNITS                            
         MVC   TCTUSES,TCUPUSES    AND N'USES                                   
         MVC   TCINSRTS,TCUPINS    AND N'INSERTS                                
         SPACE 1                                                                
UPGCX    GOTO1 USEVAL,DMCB,TGUPCDE,TGUPTYP  LOOK UP UPGRADE USE ENTRY           
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE FINDS THE ORDER NUMBER OF THE RATES FOR THIS             
*              COMMERICAL ACTRA TYPE AND RETURNS IT IN TGBYTE.                  
*              SETS CC NOT EQ IF NOT FOUND, ELSE SETS CC EQUAL.                 
         SPACE                                                                  
GETCORDR NTR1  BASE=*,LABEL=*                                                   
         LA    R4,ACTRATAB         R4=A(ACTRA TYPE ORDER TABLE)                 
         CLI   TGUSEQU,UCNM        CANADIAN NEW MEDIA                           
         BE    *+8                                                              
         CLI   TGUSEQU,UNMC        NEW MEDIA CANADIAN                           
         BNE   *+8                                                              
         LA    R4,ACTNMTAB         R4=A(ACTRA TYPE NEW MEDIA TABLE)             
         XR    R1,R1               R1=ORDER #                                   
         USING TACOD,R2                                                         
         L     R2,TCATACO                                                       
         SPACE                                                                  
GETCORD5 CLI   0(R4),X'FF'         TEST END OF TABLE                            
         BE    GCNO                                                             
         CLC   TACOCTYP,0(R4)      ADJUST USENUM BASED ON ACTRA TYPE            
         BE    GETCORDX                                                         
         SPACE                                                                  
         CLI   TACOCTYP,CCTY04A    IF ACTRA TYPE IS 2404A                       
         BE    GETCORD6                                                         
         CLI   TACOCTYP,CCTY2404   OR 2404                                      
         BNE   GETCORD7                                                         
GETCORD6 CLI   0(R4),CCTYNATL      TREAT LIKE NATIONAL                          
         BE    GETCORDX                                                         
         SPACE                                                                  
GETCORD7 AHI   R1,1                BUMP ORDER #                                 
         LA    R4,L'ACTRATAB(R4)   BUMP TO NEXT ENTRY IN TABLE                  
         B     GETCORD5                                                         
         SPACE                                                                  
GETCORDX STC   R1,TGBYTE                                                        
         B     GCYES                                                            
GCYES    XR    RC,RC                                                            
GCNO     LTR   RC,RC                                                            
         XIT1                                                                   
         SPACE 2                                                                
*              TABLE OF ORDER OF BSC CANRATES BY COMML ACTRA TYPE               
         SPACE                                                                  
ACTRATAB DS    0CL1                                                             
         DC    AL1(CCTYNATL)       NATIONAL                                     
         DC    AL1(CCTYS7)         SHORT LIFE 7                                 
         DC    AL1(CCTYS14)        SHORT LIFE 14                                
         DC    AL1(CCTYS31)        SHORT LIFE 31                                
         DC    AL1(CCTYS45)        SHORT LIFE 45                                
         DC    AL1(CCTYR1)         LOC/RG CAT 1                                 
         DC    AL1(CCTYR2)         LOC/RG CAT 2                                 
         DC    AL1(CCTYR3)         LOC/RG CAT 3                                 
ACTNMTAB DC    AL1(CCTYVDO)        VIDEO                                        
         DC    AL1(CCTYADO)        AUDIO                                        
         DC    X'FF'                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE FINDS THE USE NUMBER FOR NEW MEDIA RATES                 
*              FOR ADDITIONAL CUTS FOR ACTRA TYPE AUDIO                         
*              SETS CC NOT EQ IF NOT FOUND, ELSE SETS CC EQUAL.                 
         SPACE                                                                  
SETNMUSE NTR1  BASE=*,LABEL=*                                                   
         LA    R4,CNMTAB           R4=A(CANADIAN NEW MEDIA TABLE)               
SNMUSE10 CLI   0(R4),X'FF'         TEST END OF TABLE                            
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLC   TGUSTYP,0(R4)       ADJUST USENUM BASED ON USE TYPE              
         BE    SNMUSE20                                                         
         LA    R4,L'CNMTAB(R4)     BUMP TO NEXT ENTRY IN TABLE                  
         B     SNMUSE10                                                         
SNMUSE20 MVC   TCUSENUM,2(R4)                                                   
         SPACE                                                                  
         XIT1                                                                   
         SPACE 2                                                                
*              TABLE OF USENUMS OF CNM USE TYPES                                
         SPACE                                                                  
CNMTAB   DS    0CL4                                                             
         DC    AL1(UCNM4W),C'0',H'140'                     4 WEEKS              
         DC    AL1(UCNM8W),C'0',H'141'                     8 WEEKS              
         DC    AL1(UCNM26W),C'0',H'142'                    26 WEEKS             
         DC    AL1(UCNM1Y),C'0',H'143'                     1 YEAR               
         DC    X'FF'                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE FINDS THE ORDER NUMBER OF THE RATES FOR THIS             
*              ADDENDUM STATE AND RETURNS IT IN TGBYTE.  SETS CC NOT EQ         
*              IF NOT FOUND, ELSE SETS CC EQUAL.                                
         SPACE                                                                  
GETORDER NTR1  BASE=*,LABEL=*                                                   
         LA    R4,ADDENTAB         R4=A(ADDENDUM ORDER TABLE)                   
         XR    R1,R1               R1=ORDER #                                   
         SPACE                                                                  
GETORD5  CLI   0(R4),X'FF'         TEST END OF TABLE                            
         BE    GONO                                                             
         CLC   TCADDST,0(R4)       ADJUST USENUM BASED ON ADDNDUM STATE         
         BE    GETORDX                                                          
         AHI   R1,1                BUMP ORDER #                                 
         LA    R4,2(R4)            BUMP TO NEXT ENTRY IN TABLE                  
         B     GETORD5                                                          
         SPACE                                                                  
GETORDX  STC   R1,TGBYTE                                                        
         B     GOYES                                                            
GOYES    XR    RC,RC                                                            
GONO     LTR   RC,RC                                                            
         XIT1                                                                   
         SPACE 2                                                                
*              TABLE OF ORDER OF ADDENDUM RATES                                 
         SPACE                                                                  
ADDENTAB DS    0CL2                                                             
         DC    C'GA'               GEORGIA                                      
         DC    C'KS'               KANSAS                                       
         DC    C'TX'               TEXAS                                        
         DC    C'NW'               NORTHWEST                                    
         DC    X'FF'                                                            
         EJECT                                                                  
*        ROUTINE TO CHECK IF USING 2 BYTE USENUM                                
*                                  XIT - CC EQU 2 BYTE USENUM                   
         SPACE 1                                                                
CHKNEW   NTR1  BASE=*,LABEL=*                                                   
         CLI   OVERLAY,X'70'       IGNORE PERFORMERS GETTING CAN. RATES         
         BL    CNN10               PHASES X'70' - X'78' CAN. RATES              
         CLI   OVERLAY,CANRATE9                                                 
         BH    CNN10                                                            
         B     CNNO                                                             
CNN10    CLI   OVERLAY,SORRATES    OR SOAP RESIDUAL RATES                       
         BE    CNYES               (ALL CONTRACT YEARS)                         
         CLI   TGYREQU,CN97        AND IF CONTRACT YEAR IS 97 OR LATER          
         BL    CNNO                                                             
         B     CNYES               RETURN CC EQU FOR 2 BYTE USENUM              
CNYES    CR    RB,RB                                                            
         B     *+6                                                              
CNNO     LTR   RB,RB                                                            
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE CALCULATES PAYMENT AMOUNT                                
         SPACE 1                                                                
PAYCALC  NTR1  BASE=*,LABEL=*                                                   
         TM    TCINPUT,TCINPAY     TEST WE ALREADY HAVE IT                      
         BO    PAYC2                                                            
         L     R1,TCGROSS          GROSS                                        
         CLI   TCAPPLCD,APPLWSPU   TEST APPLIED CODE NOT WSP UPGRADE            
         BE    PAYC1                                                            
         CLI   TCAPPLCD,APPLCAB    AND NOT CABLE                                
         BE    PAYC1                                                            
         S     R1,TCAPPLCR         - APPLIED AMOUNT                             
PAYC1    ST    R1,TCPAY            = PAYMENT                                    
         B     PAYCX                                                            
         SPACE 1                                                                
PAYC2    L     R1,TCPAY            PAYMENT                                      
         SPACE 1                                                                
         TM    TCINPUT,TCINOVSC    IF PAYMENT AMOUNT IS FROM OVERSCALE          
         BZ    PAYC3               AMOUNT                                       
         TM    TGUSSTA2,APPREUSE   AND CAN APPLY TO AN FTRACK                   
         BO    PAYC3                                                            
         S     R1,TCAPPLCR         SUBTRACT APPLIED AMOUNT FROM                 
         ST    R1,TCPAY            PAYMENT AMOUNT AND SAVE IT                   
         SPACE 1                                                                
PAYC3    CLI   TCAPPLCD,APPLWSPU   TEST APPLIED CODE NOT WSP UPGRADE            
         BE    PAYC4                                                            
         CLI   TCAPPLCD,APPLCAB    AND NOT CABLE                                
         BE    PAYC4                                                            
         A     R1,TCAPPLCR         + APPLIED AMOUNT                             
PAYC4    ST    R1,TCGROSS          = GROSS                                      
         SPACE 1                                                                
PAYCX    XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              INITIALIZATION ROUTINES                                          
         SPACE 1                                                                
         DS    0D                                                               
INIT     NTR1  BASE=*,LABEL=*                                                   
         SPACE                                                                  
         MVI   TCRTRN,0            INIT. RETURN CODE                            
         MVI   TCERROR,0                 ERROR CODE                             
         MVC   TCSVYREQ,TGYREQU    SAVE YEAR EQU IN CASE OVERRIDE LATER         
         SPACE 1                                                                
         OC    TCIPCYC,TCIPCYC     IF NO CYCLE DATES FOR INDIVIDUAL             
         BNZ   *+10                                                             
         MVC   TCIPCYC,TCPCYC      USE CYCLE DATES FOR INVOICE                  
         SPACE 1                                                                
         TM    TGUSSTAT,ONCEONLY   SOME USE TYPES ALLOWED ONLY ONCE             
         BZ    *+12                                                             
         CLI   TCTAUHEL,0          TEST IF THERE'S A LAST CYCLE                 
         BNE   ERR1                YES - RETURN ERROR CODE                      
         SPACE 1                                                                
         TM    TCCASTA2,TACASEUR   IF PERFORMER'S PAY EUROS BIT IS ON           
         BZ    *+8                                                              
         OI    TCSTAT2,TCSTEURO    MARK EURO PAYMENT                            
         SPACE 1                                                                
         USING TACOD,R2                                                         
         L     R2,TCATACO                                                       
         MVC   TCADDST,TACOADST    SAVE ADDENDUM STATE                          
         SPACE                                                                  
         TM    TACOSTAT,TACOSCAN   IF COMMERCIAL'S PAY CAN. $ BIT IS ON         
         BZ    *+8                                                              
         OI    TCSTAT2,TCSTCAN$    MARK CANADIAN $ PAYMENT                      
         SPACE 1                                                                
         CLI   TGUSEQU,UCBL        IF CBL OR SCB OR LCB USE                     
         BE    INIT1                                                            
         CLI   TGUSEQU,ULCB                                                     
         BE    INIT1A                                                           
         CLI   TGUSEQU,USCB                                                     
         BNE   INIT2                                                            
INIT1    CLI   TGYREQU,CN03                                                     
         BNL   INIT2                                                            
*INIT1    CLC   TCIPCYCS,=X'940207' IF CYCLE START ON OR AFTER 2/7/94           
*         BL    INIT2                                                           
*         CLC   TCIPCYCS,=X'971231' IF CYCLE BEFORE 12/31/97                    
*         BH    INIT2                                                           
         MVI   TGYREQU,CN00        USE 00 RATES                                 
         B     INIT3                                                            
INIT1A   CLI   TGYREQU,CN06        ANYTHING BEFORE 2006,                        
         BNL   INIT3                                                            
         MVI   TGYREQU,CN03        USE 03 RATES                                 
         B     INIT3                                                            
         SPACE 1                                                                
INIT2    TM    TGUSSTA3,ADDENUSE   IF ADDENDUM USE                              
         BZ    INIT3                                                            
         CLI   TGUSEQU,UARN        IF ARN USE, JUST USE CONTRACT YEAR           
         BE    INIT3               FROM CAST MEMBER                             
         GOTOR GETDTE              GET DATE COMM'L PRODUCED                     
*                                             IN WORK                           
         CLI   TGYREQU,CN94        IF CONTRACT YEAR IS 94                       
         BNE   INIT2D                                                           
         CLC   TCADDST,=C'TX'      AND STATE IS TX                              
         BE    INIT2B                                                           
         CLC   TCADDST,=C'GA'      OR GA                                        
         BE    INIT2B                                                           
         CLC   TCADDST,=C'KS'      OR KS                                        
         BNE   INIT3                                                            
         MVC   TGFULL(3),=X'941205'  EFFECTIVE DATE IS 12/05/94 FOR KS          
         B     *+10                                                             
INIT2B   MVC   TGFULL(3),=X'940715'  FOR TX AND GA IT'S 7/15/94                 
         CLC   WORK(3),TGFULL        IF BEFORE EFFECTIVE DATE                   
         BNL   INIT3                                                            
         MVI   TGYREQU,CN93          DOESN'T GET 94 RATES, SET 93 YEAR          
         B     INIT3                                                            
INIT2D   CLI   TGYREQU,CN97          IF CONTRACT YEAR IS 97                     
         BNE   INIT2H                                                           
         CLC   TCADDST,=C'TX'        AND STATE IS TX                            
         BE    INIT2E                                                           
         CLC   TCADDST,=C'GA'        OR GA                                      
         BE    INIT2F                                                           
         CLC   TCADDST,=C'KS'        OR KS                                      
         BNE   INIT3                                                            
         MVC   TGFULL(3),=X'970902'  EFFECTIVE DATE IS 9/02/97                  
         B     INIT2G                                                           
INIT2E   MVC   TGFULL(3),=X'970815'  EFFECTIVE DATE IS 8/15/97 FOR TX           
         B     INIT2G                                                           
INIT2F   MVC   TGFULL(3),=X'971015'  EFFECTIVE DATE IS 10/15/97 FOR GA          
INIT2G   CLC   WORK(3),TGFULL        IF BEFORE EFFECTIVE DATE                   
         BNL   INIT3                                                            
         MVI   TGYREQU,CN94          DOESN'T GET 97 RATES, SET 94 YEAR          
         B     INIT3                                                            
INIT2H   CLI   TGYREQU,CN00          IF CONTRACT YEAR IS 00                     
         BNE   INIT2K                                                           
         CLC   TCADDST,=C'KS'        KANSAS - USE '97 RATES                     
         BE    INIT2J1                                                          
         CLC   TCADDST,=C'TX'        IF STATE IS TX                             
         BE    INIT2I                                                           
         CLC   TCADDST,=C'GA'        OR GA                                      
         BNE   INIT3                                                            
         MVC   TGFULL(3),=X'A10915'  EFFECTIVE DATE IS 9/15/01 FOR GA           
         B     INIT2J                                                           
INIT2I   MVC   TGFULL(3),=X'A10415'  EFFECTIVE DATE IS 4/15/01 FOR TX           
INIT2J   CLC   WORK(3),TGFULL        IF BEFORE EFFECTIVE DATE                   
         BNL   INIT3                                                            
INIT2J1  MVI   TGYREQU,CN97          DOESN'T GET 00 RATES, SET 97 YEAR          
         B     INIT3                                                            
INIT2K   CLI   TGYREQU,CN02          IF CONTRACT YEAR IS 02                     
         BNE   INIT2L                                                           
         CLC   TCADDST,=C'KS'        AND STATE IS KANSAS                        
         BNE   INIT3                                                            
         MVC   TGFULL(3),=X'A20101'  EFFECTIVE DATE IS 1/1/02 FOR KS            
         CLC   WORK(3),TGFULL        IF BEFORE EFFECTIVE DATE                   
         BNL   INIT3                                                            
         MVI   TGYREQU,CN97          DOESN'T GET 02 RATES, SET 97 YEAR          
         B     INIT3                                                            
*                                                                               
INIT2L   CLI   TGYREQU,CN03          IF CONTRACT YEAR IS 03                     
         BNE   INIT2M                                                           
         CLC   TCADDST,=C'TX'        AND STATE IS TEXAS                         
         BE    INIT2L3                                                          
         CLC   TCADDST,=C'NW'        AND STATE IS NORTHWEST                     
         BNE   INIT3                                                            
         MVC   TGFULL(3),=X'A40101'  EFFECTIVE DATE 1/1/04 FOR NW               
         B     INIT2L5                                                          
INIT2L3  MVC   TGFULL(3),=X'A40315'  EFFECTIVE DATE IS 3/15/04 FOR TX           
INIT2L5  CLC   WORK(3),TGFULL        IF BEFORE EFFECTIVE DATE                   
         BNL   INIT3                                                            
         MVI   TGYREQU,CN00          DOESN'T GET 04 RATES, SET 00 YEAR          
         B     INIT3                                                            
*                                                                               
INIT2M   CLI   TGYREQU,CN04          IF CONTRACT YEAR IS 04                     
         BNE   INIT3                                                            
         CLC   TCADDST,=C'KS'        AND STATE IS KANSAS                        
         BNE   INIT3                                                            
         MVC   TGFULL(3),=X'A40201'  EFFECTIVE DATE IS 2/1/04 FOR KS            
         CLC   WORK(3),TGFULL        IF BEFORE EFFECTIVE DATE                   
         BNL   INIT3                                                            
         MVI   TGYREQU,CN02          DOESN'T GET 04 RATES, SET 02 YEAR          
         B     INIT3                                                            
*                                                                               
INIT3    OC    TCPAY,TCPAY         TEST HAVE PAYMENT ALREADY                    
         BZ    *+12                                                             
         OI    TCINPUT,TCINPAY     SET FLAG                                     
         B     INIT5                                                            
         CLI   TGUSEQU,UIFB        ELSE IF IFB USE                              
         BNE   INIT5                                                            
         CLC   TCINSRTS,=H'1'      AND N'INSERTS = 1 (PAYMENT AMT = 0)          
         BNE   *+8                                                              
         OI    TCINPUT,TCINPAY     SET WE HAVE PYMNT AMT SO 0 WILL SHOW         
         SPACE 1                                                                
INIT5    OC    TCAPPLCR,TCAPPLCR   TEST HAVE APPLIED CREDITS                    
         BZ    *+8                                                              
         OI    TCINPUT,TCINAPPL                                                 
         SPACE 1                                                                
         OC    TCSUBPNH,TCSUBPNH   TEST HAVE AMOUNT SUBJECT TO P&H              
         BZ    *+8                                                              
         OI    TCINPUT,TCINPNH                                                  
         SPACE 1                                                                
         TM    TGUSSTA2,HLDTYPE    IF THIS IS A HOLDING FEE TYPE                
         BZ    *+8                                                              
         BAS   RE,CHKDLR           CHECK FOR HOLDS COVERED BY DLR CYC.          
         SPACE 1                                                                
         XR    RC,RC               SET CC YES                                   
NO3      LTR   RC,RC                                                            
INITX    XIT1                                                                   
         SPACE 1                                                                
ERR1     MVI   TCERROR,TCERPD1X                                                 
         B     NO3                                                              
         EJECT                                                                  
*              ROUTINE TO CHECK IF HOLD COVERED BY A DEALER CYCLE               
         SPACE 1                                                                
CHKDLR   NTR1                                                                   
         TM    TCPAYST,TCHLDLR     CHECK IF COMING FROM HOLDING FEES            
         BO    CHKD1                                                            
         CLI   OFFLINE,C'Y'        ELSE, DON'T BOTHER IF OFFLINE                
         BE    CHKDX                                                            
         SPACE                                                                  
CHKD1    TM    TCOPTS,TCNOCKDL     DON'T BOTHER IF REQUESTED                    
         BO    CHKDX               NOT TO CHECK                                 
         CLI   TGCTEQU,CTYSEAS2    OR IF COMMERCIAL IS SEASONAL                 
         BE    CHKDX                                                            
         SPACE                                                                  
         TM    TCINPUT,TCINPAY     IF AMOUNT OVERIDDEN                          
         BZ    CHKD3                                                            
         TM    TCINPUT,TCINOVSC    TEST IF FROM OVERSCALE AMOUNT                
         BZ    CHKDX               IF NOT, DON'T BOTHER                         
         SPACE                                                                  
         USING TLCAD,R3                                                         
CHKD3    XC    HALF,HALF                                                        
         TM    TCPAYST2,TCHASVER   IF COMMERCIAL HAS VERSIONS                   
         BZ    CHKD4                                                            
         L     R3,TCACAST          SET CAST SEQUENCE NUMBER IN HALF             
         MVC   HALF,TLCASEQ                                                     
         DROP  R3                                                               
         SPACE 1                                                                
CHKD4    LA    R3,KEY              BUILD PARTIAL KEY FOR RECORD                 
         USING TLUHD,R3                                                         
         XC    TLUHKEY,TLUHKEY                                                  
         MVI   TLUHCD,TLUHCDQ      RECORD CODE                                  
         MVC   TLUHCOM,TGCOM       INTERNAL COMMERCIAL NUMBER                   
         MVC   TLUHCSEQ,HALF       CAST INPUT SEQUENCE NUMBER                   
         MVC   TLUHUSE,=C'DLR'     DLR USE CODE                                 
         GOTO1 HIGH                GET DIRECTORY RECORD                         
         SPACE 1                                                                
         CLC   TLUHKEY(TLUHINV-TLUHD),KEYSAVE  DID WE FIND REC FOR USE          
         BNE   CHKDX               NO                                           
         SPACE 1                                                                
CHKD8    MVC   TGFULL,AIO          SAVE ORIGINAL AIO                            
         LA    R1,BLOCK                                                         
         ST    R1,AIO              READ INTO BLOCK                              
         SPACE 1                                                                
         GOTO1 GETREC              GET USAGE HISTORY RECORD                     
         L     R4,AIO                                                           
         MVC   AIO,TGFULL          RESTORE AIO                                  
         USING TAUHD,R4                                                         
         MVI   ELCODE,TAUHELQ      GET USAGE HISTORY ELEMENT                    
         BRAS  RE,GETEL                                                         
         BNE   CHKDX                                                            
         CLI   TAUHTYPE,UDLRA8                                                  
         JNL   CHKDX                                                            
         CLI   TAUHLFT,C'A'        IF NOT ALL PAID                              
         BE    CHKD9                                                            
         CLI   TAUHLFT,C'Y'        CHECK IF PAYMENT WAS TO LIFT                 
         BNE   CHKD8D                                                           
         TM    TCCASTAT,TACASTLF   AND CAST IS NOT ON LIFT - PAY HOLD           
         BO    *+12                                                             
         TM    TCCASTA4,TACAS2LF+TACASALL                                       
         BZ    CHKDX                                                            
         B     CHKD9                                                            
CHKD8D   TM    TCCASTAT,TACASTLO   ELSE DLR PAID TO MAIN - PAY HOLD IF          
         BO    CHKDX                   CAST IS ON LIFT ONLY                     
         TM    TCCASTA4,TACAST2O+TACASL2O                                       
         BNZ   CHKDX                                                            
         SPACE 1                                                                
CHKD9    CLC   TAUHSTRT,TCIPCYCS   MUST START BEFORE HOLD                       
         BNL   CHKDX                                                            
         CLC   TAUHEND,TCIPCYCS    MUST END ON/AFTER HOLD STARTS                
         BL    CHKDX                                                            
         TM    TAUHDLST,TAUHDLCR   IGNORE IF CREDIT PAYMENT                     
         BO    CHKDX                                                            
         SPACE 1                                                                
         BAS   RE,CYCEND           CALCULATE HLD END DATE BASED ON DLR          
         CLC   TCIPCYCS,TGFULL     IF THIS THE 1ST FIXED CYCLE INTO DLR         
         BH    CHKDX                                                            
         SPACE 1                                                                
CHKD10   TM    TCINPUT,TCINPAY+TCINOVSC+TCINPRI IF PAYAMT FROM OVSC AND         
         BNO   CHKD15              FIXED CYCLE PMT ON PRIMARY COMML             
         OI    TCCASTST,TCCAAPP0   SET TO APPLY 0 AND PAY OVSC                  
         B     CHKDX                                                            
         SPACE 1                                                                
CHKD15   OI    TCINPUT,TCINPAY     ELSE SIMULATE WE HAVE PAYMENT AMOUNT         
         OI    TCRTRN,TCRTDLR                                                   
         MVC   TCDLRINV,TLUHINV                                                 
         SPACE 1                                                                
CHKDX    B     INITX               NOT COVERED - PAY THIS HOLD                  
         EJECT                                                                  
*              ROUTINE CALCULATES CYCLE END DATE USING TAUHSTRT                 
*              AND RETURNS IT IN TGFULL(3).  R4=A(TAUH EL)                      
         SPACE 1                                                                
         USING TAUHD,R4                                                         
CYCEND   NTR1                                                                   
         XC    TGFULL(3),TGFULL                                                 
         CLI   TGUSWKS,0           GET OUT IF L'CYCLE = 0                       
         BE    CYCEX                                                            
         MVI   WORK,X'40'                                                       
         MVC   WORK+1(16),WORK                                                  
         GOTO1 DATCON,DMCB,(1,TAUHSTRT),(8,WORK)                                
         SPACE 1                                                                
         MVC   BYTE,TGUSWKS        SET L'CYCLE FROM USE TABLE                   
         SPACE 1                                                                
         CLI   BYTE,13             IF CYCLE DEFINED AS 13 WEEKS                 
         BNE   *+16                                                             
         TM    TCAYSTAT,TAAYS13W   TEST THIS AGENCY USES 13 WEEK CYCLES         
         BO    *+8                                                              
         MVI   BYTE,X'80'+3        ELSE SET TO 3 MONTHS                         
         SPACE 1                                                                
         ZIC   RF,BYTE             ISOLATE NUMBER                               
         SLL   RF,26                                                            
         SRL   RF,26                                                            
         SPACE 1                                                                
         MVC   WORK+8(2),=C'-('    BUILD DISPLAY FORMAT WITH HYPHEN             
         SPACE 1                                                                
         EDIT  (RF),(3,WORK+10),ALIGN=LEFT,WRK=BLOCK                            
         LR    R1,R0                                                            
         LA    R1,WORK+10(R1)      BUMP PAST NUMBER                             
         SPACE 1                                                                
         MVI   0(R1),C'W'          SET WEEKS OR MONTHS                          
         TM    BYTE,X'80'                                                       
         BZ    *+8                                                              
         MVI   0(R1),C'M'                                                       
         TM    BYTE,X'40'                                                       
         BZ    *+8                                                              
         MVI   0(R1),C'D'                                                       
         SPACE 1                                                                
         MVI   1(R1),C')'          END WITH TRAILING PARENTHESIS                
         SPACE 1                                                                
         LA    R3,BLOCK            R3=A(OUTPUT BLOCK FROM PERVAL)               
         USING PERVALD,R3                                                       
         GOTO1 PERVAL,DMCB,(14,WORK),(0,(R3))  DON'T DO 1 DAY LESS              
         SPACE 1                                                                
         MVC   TGFULL(3),PVALPEND  SAVE END DATE                                
         SPACE 1                                                                
CYCEX    B     INITX                                                            
         EJECT                                                                  
         SPACE                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE LOADS APPROPRIATE RATE TABLE                             
         SPACE 1                                                                
LOADTBL  NTR1  BASE=*,LABEL=*                                                   
         CLC   OVERLAY,TCARATES    IS PHASE ALREADY LOADED                      
         BE    LOAD4               YES                                          
         SPACE 1                                                                
         CLI   OVERLAY,QTACONCU    TEST PHASE IS CORE RESIDENT                  
         BE    *+12                                                             
         CLI   OVERLAY,QTACONPR                                                 
         BNE   LOAD1                                                            
         MVC   DMCB+4(3),=X'D9000A'                                             
         B     LOAD1D                GO LOAD IT                                 
         SPACE 1                                                                
LOAD1    CLI   OFFLINE,C'Y'          SPECIAL CALL IF OFFLINE                    
         BNE   LOAD2                                                            
         MVC   DMCB+4(3),=X'D90702'  SET TO LOAD T702 PHASE                     
LOAD1D   MVC   DMCB+7(1),OVERLAY     FOR THIS OVERLAY NUMBER                    
         GOTO1 CALLOV,DMCB,0,,0      LOAD IT                                    
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                MISSING PHASE                                
         MVC   TCARATES,0(R1)      RETURNS ADDRESS IN P1                        
         B     LOAD3                                                            
         SPACE 1                                                                
LOAD2    L     R2,EFHREC           SET A(FIELD) FOR ERRORS                      
         GOTO1 LOADSOPH,DMCB,0     LOAD IT                                      
         ST    R3,TCARATES         RETURNS ADDRESS IN R3                        
         SPACE 1                                                                
LOAD3    MVC   TCARATES(1),OVERLAY SAVE PHASE NUMBER IN HOB                     
         SPACE 1                                                                
LOAD4    LA    RE,TCTABLES         DETERMINE EXACT TABLE ADDRESSES              
         L     RF,TCARATES         RF=A(DISP. TO FIRST TABLE)                   
         LA    R0,TCNTABS          R0=N'TABLES                                  
*                                                                               
         CLI   OVERLAY,X'8E'       ONLY TACONCURR HAS INDEXT                    
         BE    *+8                                                              
         CLI   OVERLAY,X'8F'       ONLY TACONPREV HAS INDEXT                    
         BNE   LOAD6                                                            
         LA    R0,TCNTABS2                                                      
*                                                                               
LOAD6    L     R1,0(RF)            DISPLACEMENT TO TABLE                        
         A     R1,TCARATES         + A(PHASE)                                   
         ST    R1,0(RE)            = A(TABLE)                                   
*                                                                               
         LA    RE,4(RE)            BUMP TO NEXT AREA IN STORAGE                 
         LA    RF,4(RF)             AND TO DISP. OF NEXT TABLE                  
         BCT   R0,LOAD6                                                         
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*              SPECIAL ROUTINE FOR SWEETENING AND MULTITRACKING                 
*              R4=AMOUNT TO APPLY INCREASE TO, RETURNS RESULT IN R4             
*                                                                               
SWEETMLT NTR1  BASE=*,LABEL=*                                                   
         TM    TGUSSTA2,HLDTYPE    NOT FOR HOLDING FEE TYPES                    
         BO    SWMX                                                             
         CLI   TGUSEQU,UIFB        NOT FOR IFB                                  
         BE    SWMX                                                             
         CLI   TGUSEQU,UDEM        NOT FOR DEMOS                                
         BE    SWMX                                                             
         CLI   TGUSEQU,USNA                                                     
         BE    SWMX                                                             
         CLI   TGUSEQU,UCDM                                                     
         BE    SWMX                                                             
         CLI   TGUSEQU,UADD        IF ADDENDUM DEMOS                            
         BNE   *+14                                                             
         CLC   TCCAONOF(2),=C'ON'  ONLY FOR ON CAMERA                           
         BNE   SWMX                                                             
*                                                                               
         TM    TGCASTAT,SWMULT     ELIGIBLE FOR SWEETEN/MULTITRACK              
         BZ    SWMX                                                             
         L     R2,TCSWTTBL         SWEETENING TABLE                             
*                                                                               
SWM2     CLI   0(R2),X'FF'         END OF TABLE - TAKE DEFAULT                  
         BE    SWM4                                                             
         CLC   TGCAEQU,0(R2)       MATCH ON SPECIFIC CATEGORY                   
         BE    SWM4                                                             
         LA    R2,3(R2)            BUMP TO NEXT                                 
         B     SWM2                                                             
*                                                                               
SWM4     LH    R1,1(R2)                                                         
         MHI   R1,100              R1=PERCENTAGE INCREASE                       
         LR    R0,R4               R0=AMOUNT                                    
         MR    R0,R0               USE R0                                       
         D     R0,=F'5000'                                                      
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         ST    R1,WORK                                                          
*                                                                               
*&&DO                                                                           
         LTR   R1,R1                                                            
         BZ    SWM5                                                             
         GOTOR SVBRKDWN,DMCB,(0,WORK),(8,0)                                     
*                                                                               
         L     R1,WORK                                                          
*&&                                                                             
SWM5     AR    R4,R1               ADD TO ORIGINAL AMOUNT                       
*                                                                               
         CLI   TGYREQU,CN13        IF CONTRACT YEAR IS 13                       
         BL    SWMX                                                             
         CLI   0(R2),X'FF'         END OF TABLE - TAKE DEFAULT                  
         BNE   SWMX                                                             
         CLI   TGYREQU,CN13        IF CNTRCT YEAR > 13, NO NICKL RND            
         BNL   SWMX                                                             
         LR    R1,R4                                                            
         BRAS  RE,NICKL            ROUND IT TO NEAREST NICKEL                   
         LR    R4,R1                                                            
*                                                                               
SWMX     BAS   RE,SWEETDEM         HANDLE SWEETENING FOR DEMOS                  
*                                                                               
SWMX4    XIT1  REGS=(R4)           RETURN RESULT IN R4                          
         EJECT                                                                  
*              SPECIAL ROUTINE FOR SWEETENING DEMOS                             
*              R4=AMOUNT TO APPLY INCREASE TO, RETURNS RESULT IN R4             
*                                                                               
SWEETDEM NTR1                                                                   
         CLI   TGUSEQU,UDEM        DEMOS ONLY                                   
         BE    SWDEM00                                                          
         CLI   TGUSEQU,USNA        DEMOS ONLY                                   
         BNE   SWDEMX                                                           
SWDEM00  LA    RE,SWDEMTAB                                                      
SWDEM10  CLI   0(RE),X'FF'         IF NOT A SWEETENING TRACK, EXIT              
         BE    SWDEMX                                                           
         CLC   TGCAEQU,0(RE)                                                    
         BE    SWDEM30                                                          
         LA    RE,3(RE)                                                         
         B     SWDEM10                                                          
SWDEM30  MH    R4,1(RE)            MULTIPLY RATE BY THE NUMBER OF               
         L     RF,WORK             FIX WORK AS WELL                             
         MH    RF,1(RE)                                                         
         ST    RF,WORK                                                          
*                                                                               
SWDEMX   B     SWMX4               SWEETENING TRACKS + 1                        
*                                                                               
SWDEMTAB DC    AL1(CTSS1),AL2(2)                                                
         DC    AL1(CTSS2),AL2(3)                                                
         DC    AL1(CTSS3),AL2(4)                                                
         DC    AL1(CTSS4),AL2(5)                                                
         DC    X'FF'                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE FINDS A DATE TO USE AS THE DATE THE COMMERCIAL           
*              WAS PRODUCED AND RETURNS IT IN WORK(3)                           
         SPACE                                                                  
         DS    0D                                                               
GETDTE   NTR1  BASE=*,LABEL=*                                                   
         SPACE                                                                  
         MVC   WORK(3),TCCAFCYC    USE CAST FIRST FIXED CYCLE                   
         OC    WORK(3),WORK                                                     
         BNZ   GETDTX                                                           
         MVC   WORK(3),TCCAFRST    ELSE USE CAST FIRST SERVICES DATE            
         OC    WORK(3),WORK                                                     
         BNZ   GETDTX                                                           
         CLC   TCCAONOF(2),=C'ON'  IF OFF CAMERA                                
         BE    GETDT5                                                           
         MVC   WORK(3),TCRECDTE    USE RECORD DATE                              
         B     *+10                                                             
GETDT5   MVC   WORK(3),TCFLMDTE    ELSE USE FILM DATE                           
         OC    WORK(3),WORK                                                     
         BNZ   GETDTX                                                           
         SPACE 1                                                                
         USING TACOD,R2                                                         
         L     R2,TCATACO                                                       
         MVC   WORK(3),TACOFCYC    ELSE USE COMMERCIAL FFC                      
GETDTX   XIT1                                                                   
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE SETS UP FOR CURRENT CONTRACT YEAR                        
         SPACE 1                                                                
         DS    0D                                                               
GETYEAR  NTR1  BASE=*,LABEL=*                                                   
         CLI   RECNUM,ET           REC ETV DOES NOT HAVE CYCLE DATES            
         BE    GETY0                                                            
         CLI   RECNUM,ER           REC ER DOES NOT HAVE CYCLE DATES             
         BNE   GETY1               SO SET IT TO TODAY                           
GETY0    GOTO1 DATCON,DMCB,(5,0),(1,TCIPCYCS)                                   
                                                                                
GETY1    BAS   RE,ACTRA13          DO WE NEED TO FORCE YEAR 13?                 
                                                                                
*&&DO                                                                           
***      NO-OP 08/14                                                            
***                                                                             
         LA    R2,YEARTAB          LOOK UP CONTRACT YEAR IN TABLE               
         USING YEARD,R2                                                         
GETY2    CLI   0(R2),0                                                          
         BE    ERR2                                                             
*        MVC   BYTE,YEARUN         MATCH ON UNION                               
*        NC    BYTE,TGUNEQU                                                     
         GOTOR UNITEST,DMCB,(X'80',YEARUN1),TGUNEQUS                            
         BZ    GETY2A                                                           
         CLC   TGYREQU,YEARYR      AND ON CONTRACT YEAR                         
         BE    GETY2C                                                           
GETY2A   LA    R2,YEARNEXT         BUMP TO NEXT ENTRY                           
         B     GETY2                                                            
                                                                                
GETY2C   DS    0H                                                               
*&&                                                                             
*                                                                               
         GOTO1 YRVAL,DMCB,(X'C0',TGYREQU)                                       
         BNE   ERR2                                                             
         L     R2,TGAYEAR                                                       
         USING YRTABD,R2                                                        
                                                                                
         MVC   OVERLAY,YRPHASE     PHASE NUMBER FOR TAX MODULE                  
**NO-OP  MVC   OVERLAY,YEARPHAS    PHASE NUMBER FOR TAX MODULE                  
         TM    TGUSSTA4,INDUSTRL   INDUSTRIAL USE?                              
         BZ    GETY2E                                                           
         CLI   TGYREQU,CN11        2011 OR LATER PERFORMER                      
         BNL   GETY2E                                                           
         MVI   OVERLAY,X'8F'       LAST YEAR'S RATES                            
*                                                                               
**GETY2E MVC   TCFXDHNW,YEARFHNW   FIXED H&W AMOUNT                             
GETY2E   MVC   TCFXDHNW,YRFHNW     FIXED H&W AMOUNT                             
         TM    TCOPTS,TCOPNHR      UNLESS RATE IS OVERRIDDEN                    
         BO    GETY4                                                            
         CLI   TGUSEQU,USOP        IF SOP USE                                   
         BNE   GETY2G                                                           
         XC    TCPNHR,TCPNHR       INIT RATE TO 0%                              
*        TM    TGUNEQU,AFT         IF UNION IS AFT                              
         GOTOR UNITEST,DMCB,TGUNEQUS,AFT,0,0,0                                  
         BZ    GETY4                                                            
         MVC   TCPNHR,=H'1560'     SET P&H RATE TO 15.6%, EFF FEB232011         
*****    MVC   TCPNHR,=H'1510'     SET P&H RATE TO 15.1%, EFF NOV162009         
         B     GETY4               (ALWAYS GET LATEST RATE)                     
         SPACE                                                                  
GETY2G   CLI   TGUSEQU,UINS        IF INS USE                                   
         BE    GETY2I                                                           
**NO-OP  CLI   TGUSEQU,UIMS        OR IMS USE                                   
**06/11  BE    GETY2I                                                           
         CLI   TGUSEQU,URTK        OR RTK USE                                   
         BE    GETY2I                                                           
         CLI   TGUSEQU,UDIO        OR DIO USE                                   
         BE    GETY2I                                                           
         CLI   TGUSEQU,UIVR        OR IVR USE                                   
         BE    GETY2I                                                           
         CLI   TGUSEQU,USTR        OR STR USE                                   
         BE    GETY2I                                                           
         CLI   TGUSEQU,UIDS        OR IDS USE                                   
         BE    GETY2H                                                           
         CLI   TGUSEQU,UINR        OR INR USE                                   
         BE    GETY2H                                                           
         CLI   TGUSEQU,UISU        OR IF ISU USE AND                            
         BNE   GETY2K                                                           
*                                                                               
         USING TACOD,R1                                                         
GETY2H   L     R1,TCATACO                                                       
         CLI   TACOTYPE,CTYICAT1   INDUSTRIAL TYPE 1                            
         BE    GETY2I                                                           
         CLI   TACOTYPE,CTYICAT2   INDUSTRIAL TYPE 2                            
         BE    GETY2I                                                           
         CLI   TACOTYPE,CTYIND     INDUSTRIAL COMMERCIAL                        
         BNE   GETY2K                                                           
GETY2I   CLI   TGUSEQU,UIDS        IF IDS USE, MUST BE >=09                     
         BE    GETY2J                                                           
         MVC   TCPNHR,=H'1330'     SET P&H RATE TO 13.30%                       
         CLI   TGYREQU,CN05        IF CONTRACT YEAR >=05                        
         BL    GETY4                                                            
         MVC   TCPNHR,=H'1430'     SET P&H RATE TO 14.30%                       
         CLI   TGYREQU,CN08        IF CONTRACT YEAR >=08                        
         BL    GETY4                                                            
         MVC   TCPNHR,=H'1480'     SET P&H RATE TO 14.80%                       
GETY2J   CLI   TGYREQU,CN09        IF CONTRACT YEAR >=09                        
         BL    GETY4                                                            
         MVC   TCPNHR,=H'1530'     SET P&H RATE TO 15.30%                       
         CLI   TGYREQU,CN11        IF CONTRACT YEAR >=11                        
         BL    GETY4                                                            
         MVC   TCPNHR,=H'1550'     SET P&H RATE TO 15.50%                       
         CLI   TGYREQU,CN15        IF CONTRACT YEAR >=15                        
         BL    GETY4                                                            
         MVC   TCPNHR,=H'1600'     SET P&H RATE TO 16.00%                       
         B     GETY4                                                            
         DROP  R1                                                               
         SPACE                                                                  
GETY2K   CLI   TGUSEQU,UPRM        IF PROMO USE                                 
         BE    *+12                                                             
         CLI   TGUSEQU,UPRR                                                     
         BNE   GETY2R                                                           
         XC    TCPNHR,TCPNHR                                                    
*        TM    TGUNEQU,SAG         AND IF UNION IS SAG                          
         GOTOR UNITEST,DMCB,TGUNEQUS,SAG,0,0,0                                  
         BZ    GETY2M                                                           
         CLI   TGYREQU,CN09        2009 OR LATER PERFORMER                      
         BL    GETY2L                                                           
         MVC   TCPNHR,=H'1530'     SET P&H RATE TO 15.30%                       
         TM    TGCATYPE,EXTRA      TEST FOR EXTRAS                              
         BZ    GETY4                                                            
         MVC   TCPNHR,=H'1500'     SET P&H RATE TO 15.00%                       
         B     GETY4                                                            
GETY2L   MVC   TCPNHR,=H'1480'     SET P&H RATE TO 14.80%                       
         TM    TGCATYPE,EXTRA      TEST FOR EXTRAS                              
         BZ    GETY4                                                            
         MVC   TCPNHR,=H'1450'     SET P&H RATE TO 14.50%                       
         B     GETY4                                                            
*ETY2M   TM    TGUNEQU,AFT         ELSE IF UNION IS AFT                         
GETY2M   GOTOR UNITEST,DMCB,TGUNEQUS,AFT,0,0,0                                  
         BZ    GETY4                                                            
         MVC   TCPNHR,=H'1660'     SET P&H RATE TO 16.6%                        
         B     GETY4                                                            
         SPACE                                                                  
GETY2R   CLI   TGUSEQU,UINF        IF INFOMERCIAL                               
         BE    *+12                                                             
         CLI   TGUSEQU,UIFS                                                     
         BNE   GETY3                                                            
*        TM    TGUNEQU,AFT         AND UNION IS NOT AFT - USE SAG RATES         
         GOTOR UNITEST,DMCB,TGUNEQUS,AFT,0,0,0                                  
         BZ    GETY3                                                            
         MVC   TCPNHR,=H'1100'     ELSE SET P&H RATE TO 11%                     
         B     GETY4                                                            
         SPACE                                                                  
***GETY3 MVC   TCPNHR,YEARPNHR     SET P&H RATE BASED ON MEDIA                  
GETY3    MVC   TCPNHR,YRPNHR       SET P&H RATE BASED ON MEDIA                  
         TM    TGMEEQU,RADIO                                                    
         BO    *+10                                                             
**NO-OP  MVC   TCPNHR,YEARPNHT                                                  
         MVC   TCPNHR,YRPNHT                                                    
         CLI   TGYREQU,CN91        IF THIS IS 91 CONTRACT                       
         BNE   GETY3D                                                           
         GOTOR GETDTE              GET DATE COMM'L PRODUCED                     
         CLC   WORK(3),=X'920207'  IN WORK - IF ON OR AFTER 2/7/92              
         BL    GETY3C                                                           
         TM    TGMEEQU,RADIO       AND NOT RADIO                                
         BO    GETY3D                                                           
         MVC   TCPNHR,=H'1250'     RATE = 12.5%                                 
         B     GETY3D                                                           
         SPACE                                                                  
GETY3C   TM    TGUSSTA3,ADDENUSE   BEFORE 2/7/92, IF ADDENDUM USE               
         BZ    GETY3D                                                           
         CLC   TCADDST,=C'KS'      AND STATE IS KS                              
         BNE   GETY3D                                                           
         MVC   TCPNHR,=H'1000'     RATE = 10.0%                                 
         SPACE 1                                                                
*ETY3D   TM    TGUNEQU,AFM         TEST FOR AFM MEMBERS                         
GETY3D   GOTOR UNITEST,DMCB,TGUNEQUS,AFM,0,0,0                                  
         BZ    GETY4A                                                           
**NO-OP  MVC   TCPNHR,YEARPHMT     THEY HAVE DIFFERENT P&H RATES                
         MVC   TCPNHR,YRPHMT       THEY HAVE DIFFERENT P&H RATES                
         TM    TGMEEQU,RADIO                                                    
         BZ    *+10                                                             
**NO-OP  MVC   TCPNHR,YEARPHMR                                                  
         MVC   TCPNHR,YRPHMR                                                    
         CLI   TGYREQU,CN13         IF CONTRACT YEAR IS EARLIER THAN            
         BNL   GETY3I               2013                                        
         CLC   TCIPCYCS,=X'B00601'  AND CYCLE AFTER JUN01/2010                  
         BL    GETY3I                                                           
         MVC   TCPNHR,=H'1248'      AFM P&H = 12.48%                            
         CLC   TCIPCYCS,=X'B10401'  IF CYCLE AFTER APR01/2011                   
         BL    GETY3I                                                           
         MVC   TCPNHR,=H'1308'      AFM P&H = 13.08%                            
         DROP  R2                                                               
*                                                                               
GETY3I   CLI   TGUSEQU,UIMS        SPECIAL H&W FOR IMS                          
         BNE   GETY5                                                            
         MVC   TCPNHR,=H'1200'      AFM P&H = 12.00%                            
         MVC   TCFXDHNW,=AL2(1554)                                              
         CLC   TCIPCYCS,=X'A20216'  IF BEFORE 2/16/02                           
         BL    GETY5                                                            
         MVC   TCFXDHNW,=AL2(1700)                                              
         CLC   TCIPCYCS,=X'A30216'  IF BETWEEN 2/16/02-2/16/03                  
         BL    GETY5                                                            
         MVC   TCFXDHNW,=AL2(1850)                                              
         CLC   TCIPCYCS,=X'A40216'  IF BETWEEN 2/16/03-2/16/04                  
         BL    GETY5                                                            
         MVC   TCFXDHNW,=AL2(2000)                                              
         CLC   TCIPCYCS,=X'A51201'  IF BETWEEN 2/16/04-12/01/05                 
         BL    GETY5                                                            
         MVC   TCFXDHNW,=AL2(2100)                                              
         B     GETY5                                                            
         SPACE 1                                                                
*ETY4    TM    TGUNEQU,AFM         TEST AFM CAST MEMBER                         
GETY4    GOTOR UNITEST,DMCB,TGUNEQUS,AFM,0,0,0                                  
         BO    GETY5                                                            
*ETY4A   TM    TGUNEQU,ACT+UDA     TEST ACTRA CAST MEMBER                       
GETY4A   GOTOR UNITEST,DMCB,TGUNEQUS,ACT+UDA,0,0,0                              
         BZ    GETYX                                                            
         TM    TGMEEQU,RADIO       RADIO ALWAYS GETS CANADIAN RATES             
         BO    GETY6                                                            
         SPACE                                                                  
GETY5    TM    TCCASTA2,TACASTDP   DON'T PAY CANADIAN RATES                     
         BO    GETYX                                                            
         TM    TCCASTA2,TACASTCR   IF CAST'S PAY CANADIAN RATES BIT ON          
         BO    GETY6                                                            
         SPACE                                                                  
         USING TACOD,R1                                                         
         L     R1,TCATACO                                                       
         TM    TACOSTAT,TACOSCRT   OR COMML'S PAY CANADIAN RATES BIT ON         
         BO    GETY6                                                            
         CLI   TACOCTYP,CCTY04A    OR IF COMMERCIAL IS ACTRA TYPE 2404A         
         BE    GETY5A                                                           
         CLI   TACOCTYP,CCTY2404   OR 2404                                      
         BNE   GETYX                                                            
GETY5A   CLI   TGUSEQU,UBSC        AND MAKING A BSC PAYMENT                     
         BNE   GETYX                                                            
         DROP  R1                                                               
         SPACE                                                                  
**********************************************************************          
*  OLD METHOD                                                        *          
**********************************************************************          
*&&DO                                                                           
GETY6    MVI   OVERLAY,CANRATE1    INIT AS YEAR 1 CANADIAN RATES PHASE          
         CLI   TGYREQU,CN00        IF CONTRACT YEAR 00                          
         BL    GETY6A                                                           
         MVI   OVERLAY,CANRATE2    USE YEAR 2 CANADIAN RATE PHASE               
         CLI   TGYREQU,CN01        IF CONTRACT YEAR 01                          
         BL    GETY6B                                                           
         MVI   OVERLAY,CANRATE3    USE YEAR 3 CANADIAN RATE PHASE               
         CLI   TGYREQU,CN02        IF CONTRACT YEAR 02                          
         BL    GETY6C                                                           
         MVI   OVERLAY,CANRATE4    USE YEAR 4 CANADIAN RATE PHASE               
         CLI   TGYREQU,CN03        IF CONTRACT YEAR 03                          
         BL    GETY6D                                                           
         MVI   OVERLAY,CANRATE5    USE YEAR 5 CANADIAN RATE PHASE               
         CLI   TGYREQU,CN04        IF CONTRACT YEAR 04                          
         BL    GETY6E                                                           
         MVI   OVERLAY,CANRATE6    USE YEAR 6 CANADIAN RATE PHASE               
         CLI   TGYREQU,CN05        IF CONTRACT YEAR 05                          
         BL    GETY6F                                                           
         MVI   OVERLAY,CANRATE7    USE YEAR 7 CANADIAN RATE PHASE               
         CLI   TGYREQU,CN06        IF CONTRACT YEAR 06                          
         BL    GETY6G                                                           
         MVI   OVERLAY,CANRATE8    USE YEAR 8 CANADIAN RATE PHASE               
         CLI   TGYREQU,CN07        IF CONTRACT YEAR 07                          
         BL    GETY6H                                                           
         MVI   OVERLAY,CANRATE9                                                 
         B     GETY6H                                                           
         SPACE 1                                                                
GETY6A   CLC   TCPCYCS,=X'A00201'  ELSE,CHECK IF CYCLE STARTS                   
         BL    GETYX               ON/AFTER DATE YEAR 2 CONTRACT                
         MVI   OVERLAY,CANRATE2    TAKES EFFECT(2/1/00),PAY YEAR2 RATES         
         B     GETYX                                                            
GETY6B   CLC   TCPCYCS,=X'A10201' ELSE,CHECK IF CYCLE STARTS                    
         BL    GETYX              ON/AFTER DATE YEAR 3 CONTRACT                 
         MVI   OVERLAY,CANRATE3   TAKES EFFECT(2/1/01),PAY YEAR3 RATES          
         B     GETYX                                                            
GETY6C   CLC   TCPCYCS,=X'A20701' ELSE,CHECK IF CYCLE STARTS                    
         BL    GETYX              ON/AFTER DATE YEAR 4 CONTRACT                 
         MVI   OVERLAY,CANRATE4   TAKES EFFECT(7/1/02),PAY YEAR4 RATES          
         B     GETYX                                                            
GETY6D   CLC   TCPCYCS,=X'A30701' ELSE,CHECK IF CYCLE STARTS                    
         BL    GETYX              ON/AFTER DATE YEAR 5 CONTRACT                 
         MVI   OVERLAY,CANRATE5   TAKES EFFECT(7/1/03),PAY YEAR5 RATES          
         B     GETYX                                                            
GETY6E   CLC   TCPCYCS,=X'A40701' ELSE,CHECK IF CYCLE STARTS                    
         BL    GETYX              ON/AFTER DATE YEAR 6 CONTRACT                 
         MVI   OVERLAY,CANRATE6   TAKES EFFECT(7/1/04),PAY YEAR6 RATES          
         B     GETYX                                                            
GETY6F   CLC   TCPCYCS,=X'A50701' ELSE,CHECK IF CYCLE STARTS                    
         BL    GETYX              ON/AFTER DATE YEAR 7 CONTRACT                 
         MVI   OVERLAY,CANRATE7   TAKES EFFECT(7/1/05),PAY YEAR7 RATES          
         B     GETYX                                                            
GETY6G   CLC   TCPCYCS,=X'A60701' ELSE,CHECK IF CYCLE STARTS                    
         BL    GETYX              ON/AFTER DATE YEAR 8 CONTRACT                 
         MVI   OVERLAY,CANRATE8   TAKES EFFECT(7/1/06),PAY YEAR8 RATES          
         B     GETYX                                                            
GETY6H   CLC   TCPCYCS,=X'A70701' ELSE,CHECK IF CYCLE STARTS                    
         BL    GETYX              ON/AFTER DATE YEAR 9 CONTRACT                 
         MVI   OVERLAY,CANRATE9   TAKES EFFECT(7/1/07),PAY YEAR9 RATES          
         B     GETYX                                                            
*&&                                                                             
**********************************************************************          
*  NEW METHOD - AS OF 7/26/07                                        *          
**********************************************************************          
GETY6    DS    0H                                                               
*&&DO                                                                           
GETY6    MVI   OVERLAY,CANRATE7    TAKES EFFECT(7/1/05),PAY YEAR7 RATES         
         CLC   TCPCYCS,=X'A60701'  ELSE,CHECK IF CYCLE STARTS                   
         BL    GETYX               ON/AFTER DATE YEAR 8 CONTRACT                
         MVI   OVERLAY,CANRATE8    TAKES EFFECT(7/1/06),PAY YEAR8 RATES         
         CLC   TCPCYCS,=X'A70701'  ELSE,CHECK IF CYCLE STARTS                   
         BL    GETYX               ON/AFTER DATE YEAR 9 CONTRACT                
         MVI   OVERLAY,CANRATE9    TAKES EFFECT(7/1/07),PAY YEAR9 RATES         
         CLC   TCPCYCS,=X'A81201'  ELSE,CHECK IF CYCLE STARTS                   
         BL    GETYX               ON/AFTER DATE YEAR 9 CONTRACT                
*&&                                                                             
         MVI   OVERLAY,CANRATE1    TAKES EFFCT(12/1/08),PAY YEAR1 RATES         
         CLC   TCPCYCS,=X'A90701'  ELSE,CHECK IF CYCLE STARTS                   
         BL    GETYX               ON/AFTER DATE YEAR 2 CONTRACT                
         MVI   OVERLAY,CANRATE2    TAKES EFFECT(7/1/09),PAY YEAR2 RATES         
         CLC   TCPCYCS,=X'B00701'  ELSE,CHECK IF CYCLE STARTS                   
         BL    GETYX               ON/AFTER DATE YEAR 3 CONTRACT                
         MVI   OVERLAY,CANRATE3    TAKES EFFECT(7/1/10),PAY YEAR3 RATES         
         CLC   TCPCYCS,=X'B11031'  ELSE,CHECK IF CYCLE STARTS                   
         BL    GETYX                                                            
*                                  (2011 CONTRACT)                              
         MVI   OVERLAY,CANRATE4    TAKES EFFECT(10/31/11) YEAR1 RATES           
         CLC   TCPCYCS,=X'B20701'  ELSE,CHECK IF CYCLE STARTS                   
         BL    GETYX                                                            
         MVI   OVERLAY,CANRATE5    TAKES EFFECT(7/1/12) YEAR2 RATES             
         CLC   TCPCYCS,=X'B30701'  ELSE,CHECK IF CYCLE STARTS                   
         BL    GETYX                                                            
         MVI   OVERLAY,CANRATE6    TAKES EFFECT(7/1/13) YEAR3 RATES             
         CLC   TCPCYCS,=X'B40825'  ELSE,CHECK IF CYCLE STARTS                   
         BL    GETYX                                                            
         MVI   OVERLAY,CANRATE7    TAKES EFFECT(8/25/14) YEAR1 RATES            
         CLC   TCPCYCS,=X'B50701'  ELSE,CHECK IF CYCLE STARTS                   
         BL    GETYX                                                            
         MVI   OVERLAY,CANRATE8    TAKES EFFECT(7/1/15) YEAR2 RATES             
         CLC   TCPCYCS,=X'B60701'  ELSE,CHECK IF CYCLE STARTS                   
         BL    GETYX                                                            
         MVI   OVERLAY,CANRATE9    TAKES EFFECT(7/1/16) YEAR3 RATES             
         SPACE 1                                                                
GETYX    XR    RC,RC               SET CC YES                                   
NO2      LTR   RC,RC                                                            
         XIT1                                                                   
         SPACE 3                                                                
ERR2     MVI   TCERROR,TCERIYR                                                  
         B     NO2                                                              
         EJECT                                                                  
**********************************************************************          
*  DO WE NEED TO FORCE CONTRACT YEAR 13 OR 16                        *          
**********************************************************************          
ACTRA13  NTR1                                                                   
                                                                                
         USING TACOD,R1                                                         
         L     R1,TCATACO                                                       
         CLC   TACOFCYC,=X'B30401'   IS FFC APR01/13 OR LATER?                  
         JL    GETYX                 NO, LEAVE                                  
                                                                                
         CLI   TACOCTYP,CCTY04A      ACTRA TYPE 2404A?                          
         JE    ACT1310               YES, FORCE CONTRACT YEAR 13                
                                                                                
         CLI   TACOCTYP,CCTY04B      ACTRA TYPE 2404B?                          
         JE    ACT1310                                                          
                                                                                
         CLI   TACOCTYP,CCTY2404     ACTRA TYPE 2404?                           
         JNE   GETYX                                                            
                                                                                
ACT1310  TM    TGUSSTAT,SESSION      REUSE PAYMENTS ONLY                        
         JO    GETYX                                                            
                                                                                
*        TM    TGUSSTAT,SESSION      SESSION?                                   
*        JO    ACT13SET                                                         
*        CLI   TGUSEQU,ULFT          OR LIFTS?                                  
*        JE    ACT13SET                                                         
*        CLI   TGUSEQU,UALF                                                     
*        JE    ACT13SET                                                         
*        CLI   TGUSEQU,USLF                                                     
*        JNE   GETYX                                                            
                                                                                
ACT13SET MVI   TGYREQU,CN13          YES, FORCE CONTRACT YEAR 13                
                                                                                
         CLC   TACOFCYC,=X'B60401'   IS FFC APR01/16 OR LATER?                  
         JL    GETYX                 NO, DONE                                   
         MVI   TGYREQU,CN16          YES, FORCE CONTRACT YEAR 16                
                                                                                
         J     GETYX                                                            
                                                                                
         DROP  R1                                                               
         EJECT                                                                  
*&&DO                                                                           
*              CONTRACT YEAR INFORMATION TABLE                                  
*********************************************************************           
******** ANY CHANGES TO THIS TABLE MUST ALSO BE MADE TO TAYRTABLE ***           
********        THEN LINK TAYRTABLE INTO TALIM AND TAGEN41        ***           
*********************************************************************           
         SPACE 1                                                                
YEARTAB  DS    0C                                                               
*&&                                                                             
*&&DO                                                                           
         DC    AL1(ALL,ALL,ALL,ALL,CN79)                                        
         DC    X'63',AL2(00000400,0900,0850,0900,0850)                          
         DC    AL1(ALL,ALL,ALL,ALL,CN80)                                        
         DC    X'63',AL2(00000400,0900,0850,0900,0850)                          
*&&                                                                             
*&&DO                                                                           
         DC    AL1(ALL,ALL,ALL,ALL,CN81)                                        
         DC    X'64',AL2(00000400,0900,0850,0900,0850)                          
         DC    AL1(ALL,ALL,ALL,ALL,CN82)                                        
         DC    X'65',AL2(00000400,1000,0950,1000,0950)                          
         DC    AL1(ALL,ALL,ALL,ALL,CN83)                                        
         DC    X'66',AL2(00000500,1000,0950,1000,0950)                          
         DC    AL1(ALL,ALL,ALL,ALL,CN84)                                        
         DC    X'67',AL2(00000500,1000,0950,1000,0950)                          
         DC    AL1(ALL,ALL,ALL,ALL,CN85)                                        
         DC    X'67',AL2(00000600,1100,1050,1000,0950)                          
         DC    AL1(ALL,ALL,ALL,ALL,CN86)                                        
         DC    X'67',AL2(00000600,1100,1050,1000,0950)                          
         DC    AL1(ALL-AFM,ALL,ALL,ALL,CN87)                                    
         DC    X'67',AL2(0700,1100,1050,1000,0950)                              
         DC    AL1(AFM+NON,ALL,ALL,ALL,CN87)                                    
         DC    X'69',AL2(0700,1100,1050,1000,0950)                              
         DC    AL1(ALL-AFM,ALL,ALL,ALL,CN88)                                    
         DC    X'69',AL2(0700,1150,1150,1000,0950)                              
         DC    AL1(ALL,ALL,ALL,ALL,CN89)                                        
         DC    X'69',AL2(00001000,1150,1150,1000,0950)                          
         DC    AL1(ALL,ALL,ALL,ALL,CN90)                                        
         DC    X'69',AL2(00001000,1150,1150,1000,0950)                          
         DC    AL1(ALL,ALL,ALL,ALL,CN91)                                        
         DC    X'6A',AL2(00001200,1150,1250,1000,0950)                          
         DC    AL1(ALL,ALL,ALL,ALL,CN92)                                        
         DC    X'6A',AL2(00001200,1250,1250,1000,0950)                          
         DC    AL1(ALL,ALL,ALL,ALL,CN93)                                        
         DC    X'6C',AL2(00001200,1250,1250,1000,1000)                          
         DC    AL1(ALL,ALL,ALL,ALL,CN94)                                        
         DC    X'6D',AL2(00001200,1265,1265,1000,1000)                          
         DC    AL1(ALL,ALL,ALL,ALL,CN95)                                        
         DC    X'6D',AL2(00001200,1265,1265,1000,1000)                          
         DC    AL1(ALL,ALL,ALL,ALL,CN96)                                        
         DC    X'6D',AL2(00001200,1265,1265,1000,1000)                          
         DC    AL1(ALL,ALL,ALL,ALL,CN97)                                        
         DC    X'6E',AL2(00001300,1265,1265,1000,1000)                          
         DC    AL1(ALL,ALL,ALL,ALL,CN98)                                        
         DC    X'6E',AL2(00001300,1265,1265,1000,1000)                          
         DC    AL1(ALL,ALL,ALL,ALL,CN99)                                        
         DC    X'6E',AL2(00001300,1265,1265,1000,1000)                          
         DC    AL1(ALL,ALL,ALL,ALL,CN00)                                        
         DC    X'61',AL2(00001300,1330,1330,1000,1000)                          
         DC    AL1(ALL,ALL,ALL,ALL,CN01)                                        
         DC    X'61',AL2(00001300,1330,1330,1000,1000)                          
         DC    AL1(AFM+ACT+AFT,ALL,ALL,ALL,CN02)                                
         DC    X'61',AL2(00001300,1330,1330,1000,1000)                          
         DC    AL1(ALL,ALL,ALL,ALL,CN01)                                        
         DC    X'61',AL2(00001300,1330,1330,1000,1000)                          
         DC    AL1(ALL,ALL,ALL,ALL,CN03)                                        
         DC    X'62',AL2(00001300,1430,1430,1000,1000)                          
         DC    AL1(ALL,ALL,ALL,ALL,CN04)                                        
         DC    X'62',AL2(00001500,1430,1430,1200,1200)                          
         DC    AL1(ALL,ALL,ALL,ALL,CN05)                                        
         DC    X'62',AL2(00001500,1430,1430,1200,1200)                          
*                                                                               
         DC    AL1(ALL,ALL,ALL,ALL,CN06)                                        
         DC    X'63',AL2(00001500,1480,1480,1200,1200)                          
         DC    AL1(ALL,ALL,ALL,ALL,CN07)                                        
         DC    X'63',AL2(00001700,1480,1480,1200,1200)                          
         DC    AL1(ALL,ALL,ALL,ALL,CN08)                                        
         DC    X'63',AL2(00001700,1480,1480,1200,1200)                          
         DC    AL1(ALL,ALL,ALL,ALL,CN09)                                        
         DC    X'8F',AL2(00001700,1550,1550,1200,1200)                          
         DC    AL1(ALL,ALL,ALL,ALL,CN10)                                        
         DC    X'8F',AL2(00001700,1550,1550,1200,1200)                          
         DC    AL1(ALL,ALL,ALL,ALL,CN11)                                        
         DC    X'8F',AL2(00001700,1550,1550,1200,1200)                          
         DC    AL1(ALL,ALL,ALL,ALL,CN13)                                        
         DC    X'8E',AL2(00001700,1680,1680,1408,1408)                          
         DC    AL1(ALL,ALL,ALL,ALL,CN14)                                        
         DC    X'8E',AL2(00001700,1680,1680,1408,1408)                          
         DC    X'00'                                                            
         SPACE 1                                                                
*&&                                                                             
*********************************************************************           
******** ANY CHANGES TO THIS TABLE MUST ALSO BE MADE TO TAYRTABLE ***           
********        THEN LINK TAYRTABLE INTO TALIM AND TAGEN41        ***           
*********************************************************************           
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO TEST 4-BYTE UNION AREA AGAINST EQUATES                
*                                  P1=A(4-BYTE BLOCK OF UNION BYTES             
*                                       TO TEST - TGUNEQUS, TGCAUNIS            
*                                       OR TGUSXUNS)                            
*                                     BYTE 0 X'80'=TEST THIS BLOCK              
*                                                  AGAINST A BLOCK OF           
*                                                  STORAGE INSTEAD OF           
*                                                  STRAIGHT EQUATES             
*                                  P2=UNION EQUATES TO TEST FIRST               
*                                     BYTE FOR                                  
*                                     OR, IF BYTE 0 OF P1 IS X'80',             
*                                     A(4-BYTE BLOCK OF UNION BYTES             
*                                       TO TEST AGAINST 4-BYTE BLOCK            
*                                       OF UNION BYTES SPECIFIED BY             
*                                       P1)                                     
*                                  P3=UNION EQUATES TO TEST SECOND              
*                                     BYTE FOR                                  
*                                     IF BYTE 0 OF P1 IS X'80' THIS             
*                                     PARAMETER SHOULD BE EMPTY                 
*                                  P4=UNION EQUATES TO TEST THIRD               
*                                     BYTE FOR                                  
*                                     IF BYTE 0 OF P1 IS X'80' THIS             
*                                     PARAMETER SHOULD BE EMPTY                 
*                                  P5=UNION EQUATES TO TEST FOURTH              
*                                     BYTE FOR                                  
*                                     IF BYTE 0 OF P1 IS X'80' THIS             
*                                     PARAMETER SHOULD BE EMPTY                 
*              RETURN CONDITION CODE                                            
         SPACE 1                                                                
UNITEST  NTR1  BASE=*,LABEL=*                                                   
       ++INCLUDE TAUNITEST                                                      
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO ADJUST NORTHWEST ADDENDUMS                            
         SPACE 1                                                                
ADJNWA   NTR1  BASE=*,LABEL=*                                                   
         TM    TGUSSTA3,ADDENUSE   TEST THIS IS AN ADDENDUM                     
         BZ    ANWAX                                                            
         CLC   TCADDST,=C'NW'      AND STATE IS NW                              
         BNE   ANWAX                                                            
         TM    TCPAYST,TCUNLMTD                                                 
         BNO   ANWAX                                                            
         L     R1,TCGROSS                                                       
         LR    RE,R1               THEN GROSS IS 250% OF TABLE RATE             
         SLL   RE,1                MULT BY 2                                    
         SRL   R1,1                DIVIDE BY 2                                  
         AR    R1,RE                                                            
         ST    R1,TCGROSS                                                       
ANWAX    XIT1                                                                   
         EJECT                                                                  
         SPACE                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO ADJUST NBM PAYMENTS                                   
         SPACE 1                                                                
ADJNBM   NTR1  BASE=*,LABEL=*                                                   
         CLI   TGUSEQU,UNIM        IF USE IS INM                                
         BNE   ANBM10                                                           
         CLI   TGYREQU,CN14        AND EARLIER THAN CONTRACT YEAR 14            
         BNL   ANBMX                                                            
         CLI   TGUSTYP,UNIM8WK     AND TYPE IS 8 WEEKS                          
         BE    ANBM30              TREAT LIKE NBM 8 WEEK                        
         B     ANBMX                                                            
*                                                                               
ANBM10   CLI   TGUSEQU,UNBM        IF USE IS NBM                                
         BNE   ANBMX                                                            
         CLI   TGUSTYP,UNBM2YR     AND TYPE IS 2 YEARS                          
         BNE   ANBM20                                                           
         L     R1,TCGROSS                                                       
         LR    RE,R1               THEN GROSS IS 150% OF TABLE RATE             
         SRL   RE,1                DIVIDE BY 2                                  
         AR    R1,RE               AND ADD TO TABLE RATE                        
         B     ANBM90                                                           
*                                                                               
ANBM20   CLI   TGUSTYP,UNBM8WK     OR TYPE IS 8 WEEKS                           
         BNE   ANBMX                                                            
ANBM30   CLI   TGYREQU,CN10        ONLY FOR 2010 AND LATER                      
         BNL   ANBM40                                                           
         XR    R1,R1               BEFORE 2010, ZERO                            
         B     ANBM90                                                           
ANBM40   L     R1,TCGROSS                                                       
         MH    R1,=H'38'           38% OF YEARLY                                
         SR    R0,R0                                                            
         D     R0,=F'100'          DIVIDE BY 100                                
         CHI   R0,50                                                            
         BL    *+8                                                              
         AHI   R1,1                                                             
*                                                                               
ANBM90   ST    R1,TCGROSS                                                       
         GOTOR SVBRKDWN,DMCB,('SBDADJST',TCGROSS),(5,0)                         
ANBMX    XIT1                                                                   
         SPACE                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO ADJUST IHM PAYMENTS                                   
         SPACE 1                                                                
ADJIHM   NTR1  BASE=*,LABEL=*                                                   
         CLI   TGUSEQU,UIHM                                                     
         BNE   AIHMX                                                            
         CLI   TGYREQU,CN07                                                     
         BE    AIHM10                                                           
         CLI   TGYREQU,CN04                                                     
         BE    AIHM10                                                           
         CLI   TGYREQU,CN10                                                     
         BL    AIHMX                                                            
AIHM10   L     R1,TCGROSS                                                       
         MHI   R1,2                                                             
         CLI   TGUSTYP,UIHMTO1                                                  
         BNE   *+8                                                              
         MHI   R1,2                                                             
         ST    R1,TCGROSS                                                       
         GOTOR SVBRKDWN,DMCB,('SBDADJST',TCGROSS),(5,0)                         
AIHMX    XIT1                                                                   
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*          DATA SET TASYSCALC  AT LEVEL 212 AS OF 06/07/10                      
*              ROUTINE TO ADJUST NEW AFM USES FOR 2010 CONTRACT                 
         SPACE 1                                                                
ADJAFM10 NTR1  BASE=*,LABEL=*                                                   
         CLI   TGUSEQU,UMVM                                                     
         BE    AAFM10A                                                          
         CLI   TGUSEQU,UNIM                                                     
         BE    AAFM10A                                                          
         CLI   TGUSEQU,UIHM                                                     
         BNE   AAFM10X                                                          
AAFM10A  CLI   TGYREQU,CN10                                                     
         BNL   AAFM10X                                                          
         CLI   TGYREQU,CN07                                                     
         BE    AAFM10X                                                          
         CLI   TGYREQU,CN04                                                     
         BE    AAFM10X                                                          
         XC    TCGROSS,TCGROSS                                                  
AAFM10X  XIT1                                                                   
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ADJUST PAYMENT AMOUNTS FOR 2ND DUB TO SHORT PAYMENT                    
***********************************************************************         
                                                                                
ADJ2DTS  NTR1  BASE=*,LABEL=*                                                   
         CLI   TGUSEQU,UMUS        IF MAKING 2ND DUB TO SHORT                   
         JNE   XIT                 PAYMENT                                      
         CLI   TGUSTYP,UMUS2DS                                                  
         JNE   XIT                                                              
         L     RF,TCGROSS          PAYMENT AMOUNT IS DOUBLE THE                 
         MHI   RF,2                DUB RATE                                     
         ST    RF,TCGROSS                                                       
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO ADJUST PRM AND PRR PAYMENTS                           
         SPACE 1                                                                
ADJPRO   NTR1  BASE=*,LABEL=*                                                   
         CLI   TGUSEQU,UPRM        IF USE IS PRM                                
         BE    APRO10                                                           
         CLI   TGUSEQU,UPRR        OR PRR                                       
         BNE   APROX                                                            
APRO10   CLC   TCCAONOF,=C'OFF'    AND CAST IS OFF CAMERA                       
         BNE   APROX                                                            
         GOTOR GETDTE              GET COMMERCIAL PRODUCED DATE                 
         CLC   WORK(3),=X'A21116'  IF PRODUCED DATE IS EARLIER                  
         BNL   APROX               THAN 11/16/2002                              
         MVC   TCGROSS,=F'21500'   RATE IS $215 (NOT $220)                      
         GOTOR SVBRKDWN,DMCB,('SBDADJST',TCGROSS),0                             
APROX    XIT1                                                                   
         SPACE                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO TACK ON ADDITONAL AMOUNT                              
         SPACE 1                                                                
ADJADAM  NTR1  BASE=*,LABEL=*                                                   
         OC    TCCAADPH,TCCAADPH   IF ADDITIONAL AMOUNT SUBJECT TO              
         JNZ   AADAM10             P&H                                          
         OC    TCCAADNS,TCCAADNS   OR ADDITIONAL AMOUNT NOT SUBJECT             
         JZ    XIT                 TO P&H IS PRESENT                            
AADAM10  LA    R2,TCGROSS                                                       
         TM    TCINPUT,TCINPAY+TCINOVSC+TCINPRI                                 
         JNO   AADAM20                                                          
         LA    R2,TCPAYI                                                        
         OC    TCPAYI,TCPAYI                                                    
         JNZ   AADAM20                                                          
         LA    R2,TCPAYC                                                        
AADAM20  ZICM  R3,0(R2),4                                                       
                                                                                
         ICM   RF,15,TCCAADPH      ADD THEM TO GROSS                            
         AR    R3,RF                                                            
         ICM   RF,15,TCCAADNS                                                   
         AR    R3,RF                                                            
         STCM  R3,15,0(R2)                                                      
                                                                                
         STCM  R3,15,WORK                                                       
                                                                                
         OC    TCCAADPH,TCCAADPH   IF ADDITIONAL AMOUNT SUBJECT TO              
         JZ    AADAM30             P&H, SAVE BREAKDOWN                          
         GOTOR SVBRKDWN,DMCB,(0,TCCAADPH),('PVCADJ',0)                          
                                                                                
AADAM30  OC    TCCAADNS,TCCAADNS   IF ADDITIONAL AMOUNT NOT SUBJECT             
         JZ    XIT                 TO P&H IS PRESENT, SAVE BREAKDOWN            
         GOTOR SVBRKDWN,DMCB,(0,TCCAADNS),('PBCPLY',0)                          
         J     XIT                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE ADJUSTS TCUSENUM FOR REN IF COMMERCIAL HAS               
*              SOCIAL MEDIA WAIVER, DO NOT DOUBLE (JUST PAY SINGLE)             
         SPACE                                                                  
ADJRSMW  NTR1  BASE=*,LABEL=*                                                   
         USING TACOD,RE                                                         
         L     RE,TCATACO          IF REINSTATEMENT AND,                        
         CLI   TACOLEN,TACOLNQ2                                                 
         JL    NO                                                               
         TM    TACOSTA3,TACOSSMW   IF SOCIAL MEDIA WAIVER IS ON,                
         JNO   NO                                                               
         DROP  RE                                                               
                                                                                
         MVC   TCUSENUM,=H'63'     SET USENUM FOR UHLD (NOT DOUBLE)             
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE ADJUSTS TCUNITS AND TCUSENUM FOR LCB USE WITH            
*              MAJORS AND SETS CC EQUAL, ELSE SETS CC NOT EQUAL                 
         SPACE                                                                  
ADJLCB   NTR1  BASE=*,LABEL=*                                                   
         CLI   TGUSEQU,ULCB                                                     
         BNE   AJLCBNO                                                          
         CLI   TGUSTYP,ULCBMAX     IF MAX TYPE, DON'T BOTHER                    
         BE    AJLCBNO                                                          
         CLI   TGYREQU,CN94        OR IF CONTRACT YEAR IS 94 OR LATER           
         BNL   AJLCBNO             NO ADDITIONAL FEES FOR MAJORS                
         SPACE                                                                  
         XR    R1,R1               R1=UNITS                                     
         TM    TCMAJORS,NY         IF HAVE NY                                   
         BZ    *+8                                                              
         AHI   R1,3                ADD 3 UNITS                                  
         TM    TCMAJORS,LA         IF HAVE LA                                   
         BZ    *+8                                                              
         AHI   R1,1                ADD 1 UNIT                                   
         TM    TCMAJORS,CHI        IF HAVE CHI                                  
         BZ    *+8                                                              
         AHI   R1,1                ADD 1 UNIT                                   
         STH   R1,TCUNITS          SAVE IN TCUNITS                              
         OC    TCUNITS,TCUNITS                                                  
         BZ    AJLCBNO             SET CC NOT EQUAL IF NO MAJORS                
         MVC   TCUSENUM,=H'237'    ELSE SET USENUM FOR MAJORS RATES             
         B     AJLCBYES            AND SET CC EQUAL                             
         SPACE                                                                  
AJLCBYES XR    RC,RC                                                            
AJLCBNO  LTR   RC,RC                                                            
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO DETERMINE # OF PAX USES THAT SHOULD BE                
*              SUBTRACTED FROM CLA PAYMENTS TO 2000 AND LATER PERFS             
         SPACE 1                                                                
PAXADJ1  NTR1  BASE=*,LABEL=*                                                   
         CLI   TGUSEQU,UCLA        IF MAKING CLASS A PAYMENT                    
         BNE   PA1YES                                                           
         CLI   TGYREQU,CN00        AND PERFORMER CONTRACT YEAR IS               
         BL    PA1YES              2000 OR GREATER                              
         SPACE 1                                                                
         CLI   TCVERSEC,0          IF VERSION PAYMENT                           
         BE    PA1A                                                             
         LH    RE,TCTUSESL         SUBTRACT TOTAL PAX USES FROM                 
         SH    RE,TCTXUSEL         LIFT USES                                    
         STH   RE,TCTUSESL                                                      
         LH    RE,TCTUSES          SUBTRACT TOTAL PAX USES FROM                 
         SH    RE,TCTXUSES         TOTAL USES                                   
         STH   RE,TCTUSES                                                       
         B     PA1D                                                             
         SPACE 1                                                                
PA1A     TM    TCCASTAT,TACASTLF   IF PERFORMER IS NOT ON LIFT                  
         BO    PA1B                                                             
         TM    TCCASTA4,TACAS2LF+TACASALL                                       
         BNZ   PA1B                                                             
         LH    RE,TCTUSES                                                       
         SH    RE,TCTUSESL                                                      
         SH    RE,TCTXUSES                                                      
         AH    RE,TCTXUSEL                                                      
         STH   RE,TCTUSES                                                       
         B     PA1D                                                             
         SPACE 1                                                                
PA1B     TM    TCCASTAT,TACASTLO                                                
         BO    PA1C                                                             
         TM    TCCASTA4,TACAST2O+TACASL2O                                       
         BNZ   PA1C                                                             
         LH    RE,TCTUSES          IF PERFORMER IS ON LIFT                      
         SH    RE,TCTXUSES                                                      
         STH   RE,TCTUSES                                                       
         B     PA1D                                                             
         SPACE 1                                                                
PA1C     MVC   TCTUSESC,TCTUSES                                                 
         LH    RE,TCTUSESL                                                      
         SH    RE,TCTXUSEL                                                      
         STH   RE,TCTUSES                                                       
         SPACE 1                                                                
PA1D     LTR   RE,RE               IF TOTAL NUMBER OF USES IS NOW ZERO          
         BNZ   PA1YES                                                           
         GOTOR ADJITN              ADJUST ITN PAYMENTS                          
         GOTOR READJCWP            READJUST THE NUMBER OF USES                  
         B     PA1NO               & DO NOT CALC ANY RATE FOR THIS PERF         
         SPACE                                                                  
PA1YES   XR    RC,RC                                                            
PA1NO    LTR   RC,RC                                                            
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO ADJUST, FOR 2000 AND LATER PERFORMERS, THE            
*              NUMBER OF CLASS A USES THAT HAVE ALREADY BEEN PAID               
         SPACE 1                                                                
PAXADJ2  NTR1  BASE=*,LABEL=*                                                   
         CLI   TGUSEQU,UCLA        IF MAKING CLASS A PAYMENT                    
         BNE   PA2X                                                             
         CLI   TGYREQU,CN00        AND PERFORMER CONTRACT YEAR IS               
         BL    PA2X                2000 OR GREATER                              
         SPACE 1                                                                
         CLI   TCVERSEC,0          IF COMMERCIAL HAS NO VERSION                 
         BNE   PA2A                                                             
         CLI   TCLFTSEC,0          OR LIFT                                      
         BNE   PA2B                                                             
         LH    R1,TCUSEN           SUBTRACT TOTAL NUMBER OF PAX                 
         SH    R1,TCNUSESP         USES FROM TOTAL NUMBER OF                    
         STH   R1,TCUSEN           USES                                         
         B     PA2E                                                             
         SPACE 1                                                                
PA2A     LH    R1,TCUSEN           IF COMMERCIAL HAS VERSIONS                   
         SH    R1,TCNUSESP         SUBTRACT TOTAL NUMBER OF PAX                 
         STH   R1,TCUSEN           USES FROM TOTAL NUMBER OF USES               
         LH    R1,TCUSENL                                                       
         SH    R1,TCLUSESP         SUBTRACT NUMBER OF PAX USES TO               
         STH   R1,TCUSENL          LIFT FROM NUMBER OF LIST USES                
         B     PA2E                                                             
         SPACE 1                                                                
PA2B     TM    TCCASTAT,TACASTLF   IF COMMERCIAL HAS LIFT AND                   
         BO    *+12                                                             
         TM    TCCASTA4,TACAS2LF+TACASALL                                       
         BZ    PA2C                IF PERFORMER ON BOTH MAIN                    
         TM    TCCASTAT,TACASTLO   AND LIFT                                     
         BO    PA2D                                                             
         TM    TCCASTA4,TACAST2O+TACASL2O                                       
         BNZ   PA2D                                                             
         LH    R1,TCUSEN           SUBTRACT TOTAL NUMBER OF PAX                 
         SH    R1,TCNUSESP         USES FROM TOTAL NUMBER OF USES               
         STH   R1,TCUSEN                                                        
         LH    R1,TCUSENL          SUBTRACT NUMEBR OF PAX USES TO               
         SH    R1,TCLUSESP         LIFT FROM NUMBER OF LIFT USES                
         STH   R1,TCUSENL                                                       
         B     PA2E                                                             
         SPACE 1                                                                
PA2C     LH    R1,TCUSEN           IF COMMERCIAL HAS LIFT AND                   
         SH    R1,TCNUSESL         IF PERFORMER ONLY ON MAIN                    
         SH    R1,TCNUSESP         FROM TOTAL # OF USES, SUBTRACT               
         AH    R1,TCLUSESP         # OF LIFT USES & # OF PAX USES               
         STH   R1,TCUSEN           THEN ASS # OF PAX LIFT USES                  
         B     PA2E                                                             
         SPACE 1                                                                
PA2D     LH    R1,TCUSENL          IF PERFORMER ONLY ON LIFT                    
         SH    R1,TCLUSESP                                                      
         STH   R1,TCUSEN                                                        
         LH    R1,TCUSENL                                                       
         SH    R1,TCLUSESP                                                      
         STH   R1,TCUSENL                                                       
         SPACE 1                                                                
PA2E     LH    R1,TCUSEN                                                        
         CHI   R1,0                                                             
         BNL   *+12                                                             
         LHI   R1,0                                                             
         STH   R1,TCUSEN                                                        
         SPACE                                                                  
         LH    R1,TCUSENL                                                       
         CHI   R1,0                                                             
         BNL   *+12                                                             
         LHI   R1,0                                                             
         STH   R1,TCUSENL                                                       
PA2X     XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              READJUST # OF USES TO PAY FOR CLASS A PAYMENTS FOR PAX           
         SPACE 1                                                                
READJCWP NTR1  BASE=*,LABEL=*                                                   
         CLI   TGUSEQU,UCLA        IF MAKING CLASS A PAYMENT                    
         BNE   REACWPX                                                          
         CLI   TGYREQU,CN00        AND PERFORMER CONTRACT YEAR IS               
         BL    REACWPX             2000 OR GREATER                              
         SPACE                                                                  
         CLI   TCVERSEC,0          IF VERSION PAYMENT                           
         BE    RCWP10                                                           
         LH    RE,TCTUSES                                                       
         AH    RE,TCTXUSES         ADD NUMBER OF PAX USES BACK TO               
         STH   RE,TCTUSES          TOTAL NUMBER OF USES TO PAY                  
         LH    RE,TCTUSESL         ADJUST THE NUMBER OF LIFT USES               
         AH    RE,TCTXUSEL         PAID                                         
         STH   RE,TCTUSESL                                                      
         B     REACWPX                                                          
         SPACE                                                                  
RCWP10   TM    TCCASTAT,TACASTLF   IF PERFORMER NOT ON LIFT                     
         BO    RCWP20                                                           
         TM    TCCASTA4,TACAS2LF+TACASALL                                       
         BNZ   RCWP20                                                           
         LH    RE,TCTUSES                                                       
         SH    RE,TCTXUSEL                                                      
         AH    RE,TCTXUSES                                                      
         AH    RE,TCTUSESL                                                      
         STH   RE,TCTUSES                                                       
         B     REACWPX                                                          
         SPACE 1                                                                
RCWP20   LH    RE,TCTUSES                                                       
         AH    RE,TCTXUSES                                                      
         STH   RE,TCTUSES                                                       
         SPACE 1                                                                
         TM    TCCASTAT,TACASTLO   IF PERFORMER ONLY ON LIFT                    
         BO    *+12                                                             
         TM    TCCASTA4,TACAST2O+TACASL2O                                       
         BZ    REACWPX                                                          
         MVC   TCTUSES,TCTUSESC                                                 
REACWPX  XIT1                                                                   
         EJECT                                                                  
*              ROUTINE TO PREPARE FOR ITN PAYMENT                               
         SPACE 1                                                                
PREPITN  NTR1  BASE=*,LABEL=*                                                   
         XC    BLOCK(79),BLOCK                                                  
         XC    BLOCK+79(251),BLOCK+79                                           
         LA    R2,BLOCK                                                         
         USING ITNBLKD,R2                                                       
         CLI   TGUSEQU,UITN        IF USE IS ITN                                
         BNE   PITNX                                                            
         CLI   RECNUM,PY           AND NOT COMING FROM PYM/LIST                 
         BE    PITNX                                                            
         SPACE                                                                  
         OC    TCTUSES,TCTUSES     IF ITN USES ALREADY PRESENT                  
         BZ    *+12                                                             
         OI    TCPAYST,TCITNMAN    SET ENTERED MANUALLY                         
         B     PITN60              AND SKIP TO END OF ROUTINE                   
         SPACE                                                                  
         XC    TCUNITS,TCUNITS     CLEAR STARTING USE NUMBER                    
         XC    TCTUSES,TCTUSES     CLEAR NUMBER OF USES TO PAY                  
         SPACE                                                                  
         L     R4,AIO              SAVE KEY OF AIO                              
         MVC   ITNPIKY,0(R4)                                                    
         SPACE                                                                  
         XC    ITNVERS,ITNVERS                                                  
         LA    R4,KEY              BUILD PASSIVE KEY FOR CAST                   
         USING TLCAPD,R4                                                        
         XC    TLCAPKEY,TLCAPKEY                                                
         MVI   TLCAPCD,TLCACCDQ    RECORD CODE                                  
         MVC   TLCACSSN,TGSSN      SOCIAL SECURITY NUMBER                       
         MVC   TLCACCOM,TGCOM      COMMERCIAL                                   
         MVC   TLCACCAT,TGCAT      CATEGORY                                     
         GOTO1 HIGH                                                             
         CLC   KEY(TLCACCAT+L'TLCACCAT-TLCAPCD),KEYSAVE                         
         BE    *+6                                                              
         DC    H'00'                                                            
         GOTO1 GETREC              GET CAST RECORD                              
         SPACE                                                                  
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTVER))                                     
         BNE   PITN05                                                           
         USING TAFND,R4                                                         
         L     R4,TGELEM           SAVE WHICH VERSIONS THE CAST                 
         ZIC   RE,TAFNLEN          MEMBER IS ON                                 
         SHI   RE,4                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ITNVERS(0),TAFNNAME                                              
         SPACE                                                                  
PITN05   LA    R4,KEY              BUILD PASSIVE KEY FOR INVOICE                
         USING TLINPD,R4                                                        
         XC    TLINPKEY,TLINPKEY                                                
         MVI   TLINPCD,TLINHCDQ    RECORD CODE                                  
         MVC   TLINHCOM,TGCOM      INTERNAL COMMERCIAL NUMBER                   
         GOTO1 HIGH                GET DIRECTORY RECORD                         
         B     PITN20                                                           
PITN10   GOTO1 SEQ                                                              
PITN20   CLC   KEY(TLINHCOM+L'TLINHCOM-TLINPCD),KEYSAVE                         
         BNE   PITN40                                                           
         GOTO1 GETREC              READ INVOICE RECORD                          
         SPACE                                                                  
         USING TLIND,R4                                                         
         L     R4,AIO              R4=A(INVOICE RECORD)                         
         MVC   ITNIAGY,TLINAGY                                                  
         DROP  R4                                                               
         SPACE                                                                  
         USING TAIND,R4                                                         
         MVI   ELCODE,TAINELQ      SKIP CANCELLED AND CANCELLER                 
         BRAS  RE,GETEL            INVOICES                                     
         BNE   PITN10                                                           
         TM    TAINSTAT,TAINSCIN+TAINSCAN                                       
         BNZ   PITN10                                                           
         DROP  R4                                                               
         SPACE 1                                                                
         USING TAPDD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAPDELQ                                                   
         BRAS  RE,GETEL            R4=A(PAYMENT DETAILS ELEMENT)                
         BNE   PITN10                                                           
         CLC   TAPDUSE,=C'CLA'     HAS TO BE FOR CLASS A USE                    
         BNE   PITN10                                                           
         SPACE                                                                  
         MVC   ITNSAVS,TAPDPST1    SAVE STATUS                                  
         SPACE                                                                  
         MVI   ITNVSTA,0                                                        
         L     R4,AIO              R4=A(INVOICE RECORD)                         
         MVI   ELCODE,TAVRELQ                                                   
         BRAS  RE,GETEL            IF INVOICE WAS PAID TO VERSION               
         BNE   PITN25                                                           
         USING TAVRD,R4                                                         
         MVC   ITNVSTA,TAVRVERS    SAVE VERSION LETTER IN ITNVSTA               
         SPACE                                                                  
         CLI   ITNVERS,251                                                      
         BE    PITN25                                                           
         SPACE                                                                  
         LA    RE,ITNVERS                                                       
PITN21   CLI   0(RE),0             IF CAST MEMBER IS NOT ON THE                 
         BE    PITN10              INVOICE'S VERSION, SKIP THIS                 
         CLC   ITNVSTA,0(RE)       INVOICE                                      
         BE    PITN25                                                           
         LA    RE,1(RE)                                                         
         B     PITN21                                                           
         SPACE                                                                  
PITN25   MVI   ITNAPRG,C'N'                                                     
         SPACE                                                                  
         L     R4,AIO              R4=A(INVOICE RECORD)                         
         MVI   ELCODE,TANPELQ                                                   
         BRAS  RE,GETEL            READ ALL PROGRAM ELEMENTS                    
         B     *+8                                                              
PITN30   BRAS  RE,NEXTEL                                                        
         BNE   PITN35                                                           
         MVI   ITNAPRG,C'Y'                                                     
         SPACE                                                                  
         USING TANPD,R4                                                         
         CLC   TANPDATE,TCIPCYCS   IF DATE DOES NOT FIT WITHIN                  
         BL    PITN30              CYCLE, REJECT IT                             
         CLC   TANPDATE,TCIPCYCE                                                
         BH    PITN30                                                           
         SPACE                                                                  
         CLI   ITNVSTA,0           IF INVOICE NOT TO VERSION                    
         BNE   PITN33                                                           
         CLI   ITNVERS,0                                                        
         BNE   PITN32A                                                          
         TM    TCCASTAT,TACASTLO   IF CAST MEMBER IS ONLY ON                    
         BO    *+12                                                             
         TM    TCCASTA4,TACAST2O+TACASL2O                                       
         BZ    PITN32              LIFT                                         
         CLI   TANPLFT,C'Y'        THEN ONLY ACCEPT LIFT PROGRAMS               
         BNE   PITN30                                                           
         B     PITN33                                                           
         SPACE                                                                  
PITN32   TM    TCCASTAT,TACASTLF   IF CAST MEMBER IS NOT ON LIFT                
         BO    PITN33                                                           
         TM    TCCASTA4,TACAS2LF+TACASALL                                       
         BNZ   PITN33                                                           
         CLI   TANPLFT,C'Y'        THEN REJECT LIFT PROGRAMS                    
         BE    PITN30                                                           
         B     PITN33                                                           
         SPACE                                                                  
PITN32A  CLC   ITNVERS,=X'02FF'    IF COMMERCIAL NOW HAS VERSIONS               
         BNE   PITN32B             BUT DID NOT AT PAY TIME                      
         CLI   TANPLFT,C'Y'        AND CAST MEMBER IS ONLY ON LIFT              
         BNE   PITN30              THEN ONLY ACCEPT LIFT PAYMENTS               
         B     PITN33                                                           
         SPACE                                                                  
PITN32B  CLI   ITNVERS,251         IF CAST MEMBER IS NOT ON LIFT                
         BE    PITN33                                                           
         CLI   ITNVERS,2                                                        
         BE    PITN33                                                           
         CLI   ITNVERS+1,2                                                      
         BE    PITN33                                                           
         CLI   TANPLFT,C'Y'        THEN REJECT LIFT PROGRAMS                    
         BE    PITN30                                                           
         SPACE                                                                  
PITN33   CLI   TGYREQU,CN00        IF CAST YEAR >= 2000                         
         BL    PITN34                                                           
         CLI   TANPNWK,C'X'        THEN SKIP PAX PROGRAMS                       
         BE    PITN30                                                           
         SPACE                                                                  
PITN34   LH    RE,TCUNITS                                                       
         TM    ITNSAVS,TAPDPCRD    DECREMENT # OF CLA USES                      
         BZ    *+12                IF CREDIT INVOICE                            
         SHI   RE,1                                                             
         B     *+8                                                              
         AHI   RE,1                OTHERWISE                                    
         STH   RE,TCUNITS          INCREMENT # OF CLA USES                      
         SPACE                                                                  
         CLI   TANPNWK,C'I'        IF PROGRAM IS ITN                            
         BNE   PITN30                                                           
         LH    RE,TCTUSES                                                       
         TM    ITNSAVS,TAPDPCRD    DECREMENT ITN COUNTER                        
         BZ    *+12                IF CREDIT INVOICE                            
         SHI   RE,1                                                             
         B     *+8                                                              
         AHI   RE,1                OTHERWISE                                    
         STH   RE,TCTUSES          INCREMENT ITN COUNTER                        
         B     PITN30                                                           
         SPACE                                                                  
PITN35   CLI   ITNAPRG,C'Y'        IF NO PROGRAM ELEMENTS ARE                   
         BE    PITN10              FOUND ON CREDIT INVOICE                      
         TM    ITNSAVS,TAPDPCRD                                                 
         BZ    PITN10                                                           
         MVI   ELCODE,TANUELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TANUTINV))                                     
         BNE   PITN10                                                           
         L     R4,TGELEM                                                        
         USING TANUD,R4            GET THE NUMBER OF THE                        
         MVC   ITNCINV,TANUMBER    INVOICE THATS BEING CREDITED                 
         XC    ITNCINV,=6X'FF'                                                  
         DROP  R4                                                               
         SPACE                                                                  
         MVC   ITNINKY,KEY                                                      
         XC    KEY,KEY                                                          
         LA    R4,KEY              READ THE INVOICE RECORD THATS                
         USING TLIND,R4            BEING CREDITED                               
         MVI   TLINCD,TLINCDQ                                                   
         MVC   TLINAGY,ITNIAGY                                                  
         MVC   TLININV,ITNCINV                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(TLININV+L'TLININV-TLIND),KEYSAVE                             
         BNE   PITN39                                                           
         GOTO1 GETREC                                                           
         SPACE                                                                  
         L     R4,AIO              R4=A(CREDITED INVOICE RECORD)                
         MVI   ELCODE,TANPELQ                                                   
         BRAS  RE,GETEL            READ ALL PROGRAM ELEMENTS                    
         B     *+8                                                              
PITN36   BRAS  RE,NEXTEL                                                        
         BNE   PITN39                                                           
         SPACE                                                                  
         USING TANPD,R4                                                         
         CLC   TANPDATE,TCIPCYCS   IF DATE DOES NOT FIT WITHIN                  
         BL    PITN36              CYCLE, REJECT IT                             
         CLC   TANPDATE,TCIPCYCE                                                
         BH    PITN36                                                           
         CLI   TGYREQU,CN00                                                     
         BL    *+12                                                             
         CLI   TANPNWK,C'X'                                                     
         BE    PITN36                                                           
         SPACE                                                                  
         LH    RE,TCUNITS                                                       
         SHI   RE,1                DECREMENT TOTAL USE COUNTER                  
         STH   RE,TCUNITS                                                       
         SPACE                                                                  
         CLI   TANPNWK,C'I'        IF PROGRAM IS ITN                            
         BNE   PITN36                                                           
         LH    RE,TCTUSES                                                       
         SHI   RE,1                                                             
         STH   RE,TCTUSES          DECREMENT ITN COUNTER                        
         B     PITN36                                                           
         SPACE                                                                  
PITN39   MVC   KEY,ITNINKY                                                      
         GOTO1 HIGH                                                             
         B     PITN10                                                           
         SPACE                                                                  
PITN40   LH    RE,TCUNITS          CONVERT TCUNITS FROM CLASS A                 
         SH    RE,TCTUSES          COUNTER TO STARTING USE #                    
         STH   RE,TCUNITS                                                       
         SPACE                                                                  
         LH    RE,TCUNITS                                                       
         CHI   RE,0                IF START USE IS NEGATIVE #                   
         BNL   *+10                CONVERT TO ZERO                              
         XC    TCUNITS,TCUNITS                                                  
         SPACE                                                                  
         LH    RE,TCTUSES                                                       
         CHI   RE,0                IF ITN USES IS NEGATIVE #                    
         BNL   PITN50              CONVERT TO ZERO                              
         XC    TCTUSES,TCTUSES                                                  
         SPACE                                                                  
PITN50   MVC   KEY,ITNPIKY         RESET KEY                                    
         GOTO1 HIGH                AND READ ORIGINAL AIO RECORD BACK            
         CLC   KEY(32),KEYSAVE                                                  
         BNE   PITN60                                                           
         GOTO1 GETREC                                                           
         SPACE                                                                  
PITN60   OI    TCPAYST,TCITN       NOW SET ITN PAYMENT STATUS BYTE              
         MVI   TGUSEQU,UCLA        AND TREAT LIKE CLASS A PAYMENT               
         MVI   TGUSTYP,UCLAREG                                                  
         SPACE                                                                  
         XC    TCUSETAB,TCUSETAB   CLEAR CLASS A VARIABLES                      
         XC    TCNUSESL,TCNUSESL                                                
         XC    TCNUSESP,TCNUSESP                                                
         XC    TCTXUSES,TCTXUSES                                                
         DROP  R2                                                               
PITNX    XIT1                                                                   
         SPACE                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO ADJUST ITN PAYMENTS                                   
         SPACE 1                                                                
ADJITN   NTR1  BASE=*,LABEL=*                                                   
         TM    TCPAYST,TCITN       IS USE IS ITN DISGUISED AS CLA               
         BZ    AITNX                                                            
         MVI   TGUSEQU,UITN        RESET INTERNALLY TO ITN                      
         MVI   TGUSTYP,0                                                        
         SPACE 1                                                                
         L     RE,TCGROSS          SAVE ORIGINAL TCGROSS                        
         SPACE 1                                                                
         L     R1,TCGROSS                                                       
         MHI   R1,10               THEN GROSS IS 50% OF CLA                     
         SRA   R1,1                TABLE RATE                                   
         CVD   R1,DUB                                                           
         SRP   DUB,63,5                                                         
         CVB   R1,DUB                                                           
         ST    R1,TCGROSS                                                       
*                                                                               
         SR    R1,RE               PUT 50% DISCOUNT INTO BRKDWN TABLE           
         ST    R1,FULL                                                          
         GOTOR SVBRKDWN,DMCB,('SBDADJST',FULL),(3,0)                            
*                                                                               
AITN5    NI    TCPAYST,X'FF'-TCITN                                              
*                                                                               
         TM    TCPAYST,TCITNMAN    IF AUTOMATIC ITN PAYMENT                     
         BO    AITNX                                                            
         XC    TCUNITS,TCUNITS     CLEAR STARTING USE NUMBER                    
         XC    TCTUSES,TCTUSES     AND NUMBER OF USES TO PAY                    
AITNX    XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO PREPARE FOR VRE PAYMENT                               
                                                                                
PREPVRE  NTR1  BASE=*,LABEL=*                                                   
         CLI   TGUSEQU,UVRE                                                     
         JNE   YES                                                              
         OC    TCTUSES,TCTUSES                                                  
         JZ    NO                                                               
         MVC   TCTUSSV,TCTUSES                                                  
         MVC   TCTUSES,=H'1'                                                    
         BRAS  RE,SETSESS                                                       
         OI    TCPAYST2,TCVRE                                                   
         J     YES                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO PREPARE FOR VNR PAYMENT                               
                                                                                
PREPVNR  NTR1  BASE=*,LABEL=*                                                   
         CLI   TGUSEQU,UVNR                                                     
         JNE   YES                                                              
         TM    TCPAYST2,TCVNR1U                                                 
         JO    PVNR10                                                           
         OC    TCTUSES,TCTUSES                                                  
         JZ    NO                                                               
PVNR10   MVC   TCTUSSV,TCTUSES                                                  
         MVC   TCTUSES,=H'1'                                                    
         BRAS  RE,SETSESS                                                       
         OI    TCPAYST2,TCVNR                                                   
         J     YES                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO PREPARE FOR EDS PAYMENT                               
                                                                                
PREPEDS  NTR1  BASE=*,LABEL=*                                                   
         CLI   TGUSEQU,UEDS                                                     
         JNE   XIT                                                              
         BRAS  RE,SETSESS                                                       
         OI    TCPAYST2,TCEDS                                                   
         J     XIT                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO SET USE TYPE AS PROPER SESSION FOR USE TYPE           
                                                                                
SETSESS  NTR1  BASE=*,LABEL=*                                                   
         USING TACOD,R4                                                         
         L     R4,TCATACO                                                       
         MVI   TGUSEQU,UPRM                                                     
         CLI   TACOTYPE,CTYPROMO                                                
         JE    XIT                                                              
         MVI   TGUSEQU,UBSR                                                     
         CLI   TACOMED,TACOMEDR                                                 
         JE    XIT                                                              
         MVI   TGUSEQU,UBSS                                                     
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO RESET FOR VRE PAYMENT                                 
                                                                                
ADJVRE   NTR1  BASE=*,LABEL=*                                                   
         TM    TCPAYST2,TCVRE                                                   
         JZ    XIT                                                              
         XC    WORK,WORK                                                        
         MVC   WORK+2(2),TCTUSSV                                                
         GOTOR SVBRKDWN,DMCB,(0,TCGROSS),('PBVARS',WORK)                        
                                                                                
         L     RF,TCGROSS                                                       
         MH    RF,TCTUSSV                                                       
         ST    RF,TCGROSS                                                       
                                                                                
         MVC   TCTUSES,TCTUSSV                                                  
         MVI   TGUSEQU,UVRE                                                     
         J     XIT                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO RESET FOR VNR PAYMENT                                 
                                                                                
ADJVNR   NTR1  BASE=*,LABEL=*                                                   
         TM    TCPAYST2,TCVNR                                                   
         JZ    XIT                                                              
                                                                                
         XR    R2,R2                                                            
         TM    TCPAYST2,TCVNR1U                                                 
         JZ    AVNR10                                                           
         L     R2,TCGROSS                                                       
         GOTOR SVBRKDWN,DMCB,(0,TCGROSS),('PB1VAR',=H'1')                       
                                                                                
AVNR10   OC    TCTUSSV,TCTUSSV                                                  
         JZ    AVNR20                                                           
         L     RF,TCGROSS                                                       
         MHI   RF,150                                                           
         XR    RE,RE                                                            
         D     RE,=F'100'                                                       
         CHI   RE,50                                                            
         JL    *+8                                                              
         AHI   RF,1                                                             
         ST    RF,WORK                                                          
         MVC   WORK+4(2),TCTUSSV                                                
         GOTOR SVBRKDWN,DMCB,(0,WORK),('PBE4VR',WORK+4)                         
                                                                                
         MH    RF,TCTUSSV                                                       
         AR    R2,RF                                                            
         ST    R2,TCGROSS                                                       
                                                                                
AVNR20   MVC   TCTUSES,TCTUSSV                                                  
         MVI   TGUSEQU,UVNR                                                     
         J     XIT                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO RESET FOR EDS PAYMENT                                 
                                                                                
ADJEDS   NTR1  BASE=*,LABEL=*                                                   
         TM    TCPAYST2,TCEDS                                                   
         JZ    XIT                                                              
         MVI   TGUSEQU,UEDS                                                     
         J     XIT                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO ESTABLISH CAST CATEGORY FOR SOAP RESIDUALS            
         SPACE                                                                  
GETSORCA NTR1  BASE=*,LABEL=*                                                   
         L     R2,TCCATCOL                                                      
GETSCA5  CLI   0(R2),X'FF'                                                      
         BE    ERRGS                                                            
         CLC   TGCAEQU,1(R2)       MATCH ON CATEGORY EQUATE                     
         BE    GETSCA8                                                          
         LA    R2,2(R2)            L'TABLES SHOULD ALL BE THE SAME              
         B     GETSCA5                                                          
         SPACE 1                                                                
GETSCA8  MVC   TCROW,0(R2)         MOVE IN ROW NO.                              
         B     GETSCAX                                                          
         SPACE                                                                  
ERRGS    MVI   TCERROR,TCERICAT                                                 
         L     RD,TCRD                                                          
         LTR   RC,RC                                                            
GETSCAX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*              ADJUSTMENT ROUTINE FOR MUSICIANS                                 
         SPACE 1                                                                
         USING TACOD,R2            R2=A(COMM'L DETAILS EL)                      
ADJMUS   NTR1  BASE=*,LABEL=*                                                   
         CLI   OVERLAY,X'70'       IGNORE PERFORMERS GETTING CAN. RATES         
         BL    ADJM1               PHASES X'70' - X'78' CAN. RATES              
         CLI   OVERLAY,CANRATE9                                                 
         BH    ADJM1                                                            
         B     ADJMX                                                            
*                                                                               
*                                  RESET ROW BASED ON AFM RATE IN COMML         
ADJM1    CLI   TGCAEQU,CTZZZ       IF CATEGORY ZZZ                              
         BE    ADJM3                                                            
         CLI   TGCAEQU,CTZZ        OR IF CATEGORY ZZ                            
         BE    ADJM3                                                            
         CLI   TGUSEQU,UBSM        OR IF BSM, CHECK ON/OFF CAM                  
         BE    ADJM2                                                            
         CLI   TGUSEQU,UIMS        SPECIAL ADJ IF IMS USE                       
         BNE   ADJM5                                                            
         CLC   TCCAONOF(2),=C'ON'  IF OFF CAMERA                                
         BE    ADJM1A                                                           
         MVI   TCROW,2             RESET ROW TO GET DIFFERENT RATES             
         B     ADJMX                                                            
ADJM1A   CLI   TACOAFM,C'1'        IF ON CAMERA AND ONE PERSON ALONE,           
         BNE   ADJMX                                                            
         MVI   TCROW,3             MAKE ROW = 3 TO GET 1 PERSON RATES           
         B     ADJMX                                                            
*                                                                               
ADJM2    CLC   TCCAONOF(2),=C'ON'  IF ON CAMERA                                 
         BNE   ADJM5                                                            
*                                                                               
ADJM3    MVI   TCROW,60            RESET ROW SO WON'T GET RATES                 
         B     ADJMX                                                            
*                                                                               
ADJM5    MVI   TCROW,1             MAKE ROW = 1                                 
         CLI   TACOAFM,C'1'             IF CAST IS 1                            
         BE    ADJMX                                                            
         MVI   TCROW,2                  ROW = 2                                 
         CLI   TACOAFM,C'2'             IF CAST IS 2-4                          
         BE    ADJMX                                                            
         MVI   TCROW,3                  ROW = 3 FOR OVER 4                      
ADJMX    XIT1                                                                   
         DROP  R2                                                               
         SPACE 3                                                                
         EJECT                                                                  
*              CALCULATE HEALTH AND WELFARE CONTRIBUTION                        
         SPACE 1                                                                
HNWCALC  NTR1  BASE=*,LABEL=*                                                   
*        TM    TGUNEQU,AFM         APPLIES TO AFM ONLY                          
         GOTOR UNITEST,DMCB,TGUNEQUS,AFM,0,0,0                                  
         BZ    HNWCX                                                            
         TM    TCSTAT2,TCSTCAN$    IF CANADIAN $ PAYMENT GET OUT                
         BO    HNWCX                                                            
         TM    TCSTAT2,TCSTEURO    IF EURO PAYMENT GET OUT                      
         BO    HNWCX                                                            
         CLI   OVERLAY,X'70'       IGNORE PERFORMERS GETTING CAN. RATES         
         BL    HNWC0               PHASES X'70' - X'78' CAN. RATES              
         CLI   OVERLAY,CANRATE9                                                 
         BH    HNWC0                                                            
         B     HNWCX                                                            
*                                                                               
HNWC0    L     R3,TCPAY            BASE ON PAYMENT AMOUNT                       
*                                                                               
         TM    TCINPUT,TCINPNH     TEST WE HAVE BASIS OVERRIDE FOR P&H          
         BZ    *+12                                                             
         L     R3,TCSUBPNH         USE IT                                       
         B     HNWC1                                                            
*                                                                               
         TM    TCSTAT,TCSTAFMP     TEST HAVE SUBJ TO P&H FOR AFM                
         BZ    *+8                                                              
         L     R3,TCAFMSPH         USE IT                                       
*                                                                               
HNWC1    LTR   R3,R3               R3=BASIS FOR H&W AMOUNT                      
         BZ    HNWCX               GET OUT IF NO BASIS                          
*                                                                               
         LA    R1,TGHWLCLS         DETERMINE IF LOCAL HAS H&W FUND              
         LA    R0,TGNLCLS          USE TABLE BUILT BY CONTROLLER                
         CLC   TGLCL,0(R1)                                                      
         BE    HNWC2               IF IN TABLE, LOCAL HAS FUND                  
         LA    R1,3(R1)                                                         
         BCT   R0,*-14                                                          
*                                                                               
         TM    TCINPUT,TCINHWIN    IF H&W ALREADY IN PAYMNT AMT                 
         BO    HNWC1B                                                           
         OI    TCSTAT,TCSTHPAY     ELSE ADD H&W TO PAYMENT AMOUNT LATER         
         B     HNWC2                                                            
*                                                                               
HNWC1B   OI    TCSTAT,TCSTHPNL     H&W ADDED AND NO LOCAL FUND                  
*                                                                               
HNWC2    CLI   TGYREQU,CN85        TEST '85 CONTRACT OR LATER                   
         BL    HNWC4                                                            
         TM    TGUSSTAT,TAKEHNW    TEST WHETHER TO TAKE IT                      
         BZ    HNWC4                                                            
*                                                                               
         CLI   TGYREQU,CN14        FOR '14 AND LATER                            
         BL    HNWC2A                                                           
         MHI   R3,6                TAKE 6 PERCENT                               
         B     HNWC3A                                                           
HNWC2A   CLI   TGYREQU,CN07        FOR '07 AND LATER                            
         BL    HNWC3                                                            
         MHI   R3,3                TAKE 3 PERCENT                               
         B     HNWC3A                                                           
HNWC3    CLI   TGYREQU,CN89        FOR '89 AND LATER                            
         BL    *+8                                                              
         SLA   R3,1                TAKE 2 PERCENT                               
HNWC3A   CVD   R3,DUB                                                           
         SRP   DUB,62,5            ELSE TAKE 1 PERCENT OF PAYMENT               
         CVB   R3,DUB                                                           
         ST    R3,TCHNW            SAVE AS H&W AMOUNT SO FAR                    
         SPACE 1                                                                
HNWC4    CLI   TGUSEQU,UBSM        ADD FIXED RATE FOR SESSION PAYMENTS          
         BNE   HNWC5                                                            
         TM    TCCASTST,TCCANOFX   TEST CAST MEMBER GETS FIXED RATE             
         BO    HNWC8                                                            
         LH    R3,TCFXDHNW                                                      
         SPACE 1                                                                
         TM    TCCASTST,TCCA1XFX   TEST NOT GETTING FIXED 1X ONLY               
         BO    HNWC6                                                            
         TM    TGCASTAT,HNW2X      TEST ELIGIBLE FOR FIXED 2X                   
         BZ    *+8                                                              
         AH    R3,TCFXDHNW         ADD IN CONSTANT AGAIN                        
         B     HNWC6                                                            
*                                                                               
HNWC5    CLI   TGUSEQU,UIMS        TEST IF INDUSTRIAL MUSIC SESSION             
         BNE   HNWC8               FLAT RATE H&W                                
         LH    R3,TCFXDHNW                                                      
         ST    R3,TCHNW                                                         
         B     HNWC8                                                            
*                                                                               
HNWC6    TM    TCPAY,X'80'         IF FEES ARE NEGATIVE                         
         BZ    *+6                                                              
         LNR   R3,R3               THEN SO IS H&W                               
         A     R3,TCHNW            ADD TO EXISTING CONTRIBUTION                 
         ST    R3,TCHNW                                                         
*                                                                               
HNWC8    DS    0H                                                               
*                                                                               
         TM    TCSTAT,TCSTHPNL     H&W ADDED AND NO LOCAL FUND                  
         BZ    HNWC8B                                                           
         GOTOR SVBRKDWN,DMCB,(0,TCHNW),(67,0)                                   
         GOTOR SUBRKDWN,DMCB,(0,TCHNW),0                                        
         B     HNWC9                                                            
*                                                                               
HNWC8B   TM    TCSTAT,TCSTHPAY     IF NO FUND                                   
         BZ    HNWCX                                                            
         OC    TCHNW,TCHNW         AND THERE'S H&W                              
         BZ    HNWCX                                                            
*        CLI   TGUSEQU,UMRR        (IF MUSIC SESSION REUSE, JUST %)             
*        BE    HNWCX                                                            
*                                                                               
         L     R1,TCPAY            ADD H&W TO PAYMENT AMOUNT                    
         A     R1,TCHNW                                                         
         ST    R1,TCPAY                                                         
         L     R1,TCGROSS          AND TO GROSS(ALREADY CALLED PAYCALC)         
         A     R1,TCHNW                                                         
         ST    R1,TCGROSS                                                       
         GOTOR SVBRKDWN,DMCB,(0,TCHNW),(67,0)                                   
*                                                                               
HNWC9    XC    TCHNW,TCHNW         ALSO CLEAR H&W AMOUNT (TCHNW)                
*                                                                               
HNWCX    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO ADD APPLIED CREDIT FOR HOLDING FEES COVERED           
*              BY DEALER PAYMENT                                                
         SPACE 1                                                                
DLRAPHLD NTR1  BASE=*,LABEL=*                                                   
         CLI   TGUSEQU,UDLR        IF MAKING A DEALER PAYMENT                   
         JNE   XIT                                                              
         CLI   TGMEEQU,RADIO       ON A NON-RADIO                               
         JE    XIT                                                              
         CLI   TGCTEQU,CTYSEAS2    NON-SEASONAL COMMERCIAL                      
         JE    XIT                                                              
         CLI   OFFLINE,C'Y'        AND RUNNING ONLINE                           
         JE    XIT                                                              
         TM    TCPAYST,TCCREDIT    AND THIS IS NOT A CREDIT PAYMENT             
         JO    XIT                                                              
*                                                                               
         USING TAOAD,R4                                                         
*        TM    TCCASTST,TCCADGPC   IF THIS IS PRIMARY COMMERCIAL                
*        JZ    DAH20               FOR TYPE 6 GUARANTEE                         
         L     R4,TCACAST                                                       
         MVI   ELCODE,TAOAELQ                                                   
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
DAH10    BRAS  RE,NEXTEL                                                        
         JNE   DAH20                                                            
         ZIC   R0,TAOANUM                                                       
         LA    R1,TAOASBEL         EXIT IF SET UP TO PAY OVERSCALE              
DAH15    CLC   =C'HLD',0(R1)       HOLDING FEE                                  
         JE    XIT                                                              
         CLC   =C'SHL',0(R1)       OR SPANISH HOLDING FEE                       
         JE    XIT                                                              
         CLC   =C'ADH',0(R1)       OR ADDENDUM HOLDING FEE                      
         JE    XIT                                                              
         LA    R1,7(R1)                                                         
         BCT   R0,DAH15                                                         
         J     DAH10                                                            
         DROP  R4                                                               
*                                                                               
DAH20    TM    TGCATYPE,NOHLD      DOES THIS PERFORMER QUALIFY FOR              
         JO    XIT                 HOLDING FEES?                                
         CLC   TCCAONOF,=C'OFF'                                                 
         JNE   DAH30                                                            
         TM    TGCATYPE,NOHLDOFF                                                
         JO    XIT                                                              
         CLI   TGYREQU,CN88                                                     
         JL    DAH30                                                            
         TM    TGCATYPE,NHLDOF88                                                
         JO    XIT                                                              
*                                                                               
         USING DAHD,R5                                                          
DAH30    LA    R5,PARAS            SETUP WORK AREA                              
*                                                                               
         USING TACOD,R4                                                         
         L     R4,TCATACO          DEFAULT APPLICABLE DATE                      
         MVC   DAHAPPDT,TACOFCYC   TO COMM'L FIRST FIXED CYCLE                  
         DROP  R4                                                               
*                                                                               
DAH40    OC    TCCAFCYC,TCCAFCYC   IF CAST FIRST FIXED CYCLE PRESENT            
         JZ    DAH50                                                            
         MVC   DAHAPPDT,TCCAFCYC   USE IT AS APPLICABLE DATE                    
*                                                                               
         USING TACRD,R4                                                         
DAH50    L     R4,TCACAST                                                       
         MVI   ELCODE,TACRELQ      SCAN APPLIED CREDIT HISTORY ELS.             
         XR    R2,R2               CLEAR R2 IN CASE NO HLDS PAID YET            
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
DAH60    BRAS  RE,NEXTEL                                                        
         JNE   DAH80                                                            
*                                                                               
         CLC   TACRUSE,=C'HLD'     IF HOLDING FEE                               
         JE    DAH70                                                            
         CLC   TACRUSE,=C'SHL'     OR SPANISH HOLDING FEE                       
         JE    DAH70                                                            
         CLC   TACRUSE,=C'ADH'     OR ADDENDUM HOLDING FEE                      
         JE    DAH70                                                            
         CLC   TACRUSE,=C'SRE'     OR SPANISH REINSTATEMENT                     
         JE    DAH70                                                            
         CLC   TACRUSE,=C'REN'     OR REINSTATEMENT                             
         JE    DAH70                                                            
         CLC   TACRUSE,=C'ARN'     OR ADDENDUM REINSTATEMENT FOUND              
         JNE   DAH60                                                            
DAH70    CLC   TACRSTRT,TCIPCYCS   WITH A CYCLE THAT STARTED EARLIER            
         JH    DAH60               THIS DEALER CYCLE ...                        
         LR    R2,R4               SAVE A(LAST HOLD TACR EL)                    
         J     DAH60                                                            
         DROP  R4                                                               
*                                                                               
         USING TACRD,R2                                                         
DAH80    LTR   R2,R2               IF APPLIED CREDIT HISTORY WAS                
         JZ    DAH90               FOUND, CALCULATE NEXT CYCLE START            
         GOTO1 DATCON,DMCB,(1,TACREND),DAHWORK                                  
         GOTO1 ADDAY,DMCB,DAHWORK,DAHWORK+6,1                                   
         GOTO1 DATCON,DMCB,DAHWORK+6,(1,DAHCYCS)                                
         J     DAH100                                                           
         DROP  R2                                                               
*                                                                               
*                                  IF APPLIED CREDIT HISTORY WAS NOT            
DAH90    MVC   DAHWORK,=17C' '     FOUND, CALCULATE NEXT CYCLE                  
         GOTO1 DATCON,DMCB,(1,DAHAPPDT),(8,DAHWORK)                             
         MVI   BYTE,13                                                          
         TM    TCAYSTAT,TAAYS13W                                                
         JO    *+8                                                              
         MVI   BYTE,X'80'+3                                                     
*                                                                               
         ZIC   RF,BYTE             ISOLATE NUMBER                               
         SLL   RF,26                                                            
         SRL   RF,26                                                            
*                                                                               
         MVC   DAHWORK+8(2),=C'-('    BUILD DISPLAY FORMAT WITH HYPHEN          
*                                                                               
         EDIT  (RF),(3,DAHWORK+10),ALIGN=LEFT                                   
         LR    R1,R0                                                            
         LA    R1,DAHWORK+10(R1)      BUMP PAST NUMBER                          
*                                                                               
         MVI   0(R1),C'W'          SET WEEKS OR MONTHS                          
         TM    BYTE,X'80'                                                       
         JZ    *+8                                                              
         MVI   0(R1),C'M'                                                       
         TM    BYTE,X'40'                                                       
         JZ    *+8                                                              
         MVI   0(R1),C'D'                                                       
         MVI   1(R1),C')'          END WITH TRAILING PARENTHESIS                
*                                                                               
         USING PERVALD,R3                                                       
         LA    R3,BLOCK            R3=A(OUTPUT BLOCK FROM PERVAL)               
         GOTO1 PERVAL,DMCB,(14,DAHWORK),(0,(R3))                                
         MVC   DAHCYCS,PVALPEND    NOW HAVE NEXT START                          
         DROP  R3                                                               
*                                                                               
DAH100   CLC   DAHCYCS,TCPCYCS     COVERED CYCLE CANNOT BEGIN EARLIER           
         JL    XIT                 THAN DEALER CYCLE                            
*                                                                               
         MVC   DAHWORK,=17C' '                                                  
         GOTO1 DATCON,DMCB,(1,DAHCYCS),(8,DAHWORK)                              
         MVI   BYTE,13                                                          
         TM    TCAYSTAT,TAAYS13W                                                
         BO    *+8                                                              
         MVI   BYTE,X'80'+3                                                     
*                                                                               
         ZIC   RF,BYTE             ISOLATE NUMBER                               
         SLL   RF,26                                                            
         SRL   RF,26                                                            
*                                                                               
         MVC   DAHWORK+8(2),=C'-(' BUILD DISPLAY FORMAT WITH HYPHEN             
*                                                                               
         EDIT  (RF),(3,DAHWORK+10),ALIGN=LEFT                                   
         LR    R1,R0                                                            
         LA    R1,DAHWORK+10(R1)   BUMP PAST NUMBER                             
*                                                                               
         MVI   0(R1),C'W'          SET WEEKS OR MONTHS                          
         TM    BYTE,X'80'                                                       
         BZ    *+8                                                              
         MVI   0(R1),C'M'                                                       
         TM    BYTE,X'40'                                                       
         BZ    *+8                                                              
         MVI   0(R1),C'D'                                                       
         MVI   1(R1),C')'          END WITH TRAILING PARENTHESIS                
*                                                                               
         USING PERVALD,R3                                                       
         LA    R3,BLOCK            R3=A(OUTPUT BLOCK FROM PERVAL)               
         GOTO1 PERVAL,DMCB,(14,DAHWORK),('PVINSGLS+PVIN1DYL',(R3))              
         MVC   DAHCYCE,PVALPEND    NOW HAVE CYCLE END                           
         DROP  R3                                                               
*                                                                               
         USING TACRD,R4                                                         
         LA    R4,ELEMENT          BUILD APPLIED CREDIT HISTORY ELEMENT         
         XC    ELEMENT,ELEMENT                                                  
*                                                                               
         MVI   TACREL,TACRELQ      BUILD THE ELEMENT                            
         MVI   TACRLEN,TACRLNQ                                                  
         MVC   TACRSTRT(6),DAHCYCS CYCLE DATES                                  
         MVC   TACRINV,TGINV       INVOICE NUMBER                               
         XC    TACRINV,=6X'FF'     UNCOMPLEMENT IT                              
*                                                                               
         MVC   TACRUSE,=C'HLD'     USE CODE                                     
         CLI   TGCTEQU,CTYSPAN                                                  
         JNE   *+10                                                             
         MVC   TACRUSE,=C'SHL'                                                  
         CLI   TGCTEQU,CTYADD                                                   
         JNE   *+10                                                             
         MVC   TACRUSE,=C'ADH'                                                  
*                                                                               
         OI    TACRSTAT,TACRHDLR                                                
         DROP  R4,R5                                                            
*                                                                               
         OC    TCACAST,TCACAST     IF WE HAVE A(CAST RECORD)                    
         BZ    DAH110                                                           
*                                                                               
         CLI   TGUSWKS,8                                                        
         BE    DAH110                                                           
*                                                                               
         L     R0,AIO                                                           
         MVC   AIO,TCACAST         ADD ELEMENT TO CAST RECORD                   
         GOTO1 ADDL                                                             
         ST    R0,AIO              RESTORE I/O AREA                             
*                                                                               
DAH110   OI    TCRTRN,TCRTCAST     SET CAST REC CHGD                            
         J     XIT                                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              FINISHING-UP ROUTINES                                            
         SPACE 1                                                                
FINISH   NTR1  BASE=*,LABEL=*                                                   
         CLC   TGSSN,=C'953967876' IF THIS IS SAG FOUNDATION                    
         BNE   FIN3                                                             
         MVC   TCMDED,TCPAY        MOVE PAYMENT TO MISC. DEDUCTION              
FIN3     CLI   TCW4TYPE,TAW4TYCO   IF THIS IS A CORPORATION                     
         BE    FIN5                                                             
         CLI   TCW4TYPE,TAW4TYCA   OR A CANADIAN                                
         BE    FIN5                                                             
         CLI   TCW4TYPE,TAW4TYTR   OR A TRUSTEE                                 
         BE    FIN5                                                             
         CLI   TCW4TYPE,TAW4TYFO   OR A FOREIGNER                               
         BNE   FIN7                                                             
FIN5     MVC   TCPAYC,TCPAY        MOVE PAYMENT TO CORPORATION ACCUM            
         B     *+10                                                             
FIN7     MVC   TCPAYI,TCPAY        ELSE MOVE TO INDIVIDUAL ACCUM                
         SPACE 1                                                                
         TM    TCOPTS,TCRESCRS     TEST USER WANTS CREDITS RESOLVED             
         BZ    FIN8                                                             
         L     R0,TCAPPLCR         COMPLEMENT AMOUNT IN APPLIED CREDITS         
         LCR   R0,R0                                                            
         ST    R0,TCAPPLCR                                                      
         CLI   TCAPPLCD,APPLGUAR   TEST APPLIED CODE IS GUARANTEE               
         BNE   FIN8                                                             
         ST    R0,TCGUAR           MOVE APPLIED CREDITS TO GUAR CREDITS         
         XC    TCAPPLCR,TCAPPLCR   AND CLEAR APPLIED CREDITS                    
         SPACE 1                                                                
         USING TACOD,RE                                                         
FIN8     L     RE,TCATACO          IF THIS IS PER CYCLE COMMERCIAL              
         TM    TACOSTA2,TACOPCYC   NEVER APPLY TO REUSE                         
         BZ    FINX                                                             
         MVI   TCAPPLCD,0                                                       
         NI    TCINPUT,X'FF'-TCINAPPL                                           
         DROP  RE                                                               
         SPACE 1                                                                
FINX     XIT1                                                                   
         EJECT                                                                  
*              ROUTINE ADDS APPLIED CREDIT HISTORY EL. TO CAST RECORD           
         SPACE 1                                                                
ADDTACR  NTR1  BASE=*,LABEL=*                                                   
         TM    TCPAYST2,TCRETRO                                                 
         BO    ADDTX                                                            
         LA    R4,ELEMENT          R4=A(APPLIED CREDIT HISTORY EL.)             
         USING TACRD,R4                                                         
         XC    TACREL(TACRLNQ),TACREL                                           
         SPACE 1                                                                
         MVI   TACREL,TACRELQ      BUILD THE ELEMENT                            
         MVI   TACRLEN,TACRLNQ                                                  
         MVC   TACRSTRT(6),TCIPCYC CYCLE DATES                                  
         MVC   TACRUSE,TGUSCDE     USE CODE                                     
         MVC   TACRTYPE,TGUSTYP    USE TYPE                                     
         SPACE 1                                                                
         USING TACOD,RE                                                         
         L     RE,TCATACO          GROSS AND APPLIED AMOUNT                     
         TM    TACOSTA2,TACOPCYC   ARE ZERO(ONLY ADDING FOR                     
         BO    ADDT6               HOLDING FEE NOTICE GENERATION)               
         DROP  RE                                                               
         SPACE 1                                                                
         MVC   TGDUB(4),TCGROSS                                                 
         MVC   TGDUB+4(4),TCAPPLIC                                              
         TM    TCPAYST,TCCREDIT    IF CREDIT PAYMENT                            
         BZ    ADDT5                                                            
         L     RE,TGDUB            REVERSE SIGN ON AMOUNTS                      
         LCR   RE,RE                                                            
         ST    RE,TGDUB                                                         
         L     RE,TGDUB+4                                                       
         LCR   RE,RE                                                            
         ST    RE,TGDUB+4                                                       
ADDT5    MVC   TACRSCAL,TGDUB      SAVE GROSS AMOUNT                            
         MVC   TACRAPPL,TGDUB+4    AMOUNT TO BE APPLIED                         
         SPACE                                                                  
         CLI   TGUSEQU,UADC        IF COMBINED SESS/WSP, BALANCE=0              
         BE    *+10                                                             
         MVC   TACRBAL,TACRAPPL    ELSE BAL REMAINING = AMT TO BE APPLD         
         SPACE                                                                  
ADDT6    MVC   TACRINV,TGINV       INVOICE NUMBER                               
         XC    TACRINV,=6X'FF'     UNCOMPLEMENT IT                              
         SPACE 1                                                                
         OC    TGGUA,TGGUA         IF NOT ON GUARANTEE                          
         BNZ   *+16                                                             
         TM    TCCASTST,TCCAOVAM   AND OVERSCALE AMOUNT DEFINED ON CAST         
         BZ    *+8                                                              
         OI    TACRSTAT,TACRSTRK+TACRSGUA  SET TRACK PAYMENTS AGAINST           
*                                          IT AND IT'S A FIXED CYC GUAR         
         SPACE 1                                                                
         USING TACOD,RE                                                         
         TM    TGUSSTA2,HLDTYPE                                                 
         BZ    ADDT7                                                            
         L     RE,TCATACO                                                       
         CLI   TACOTYPE,CTYSEAS2                                                
         BNE   ADDT7                                                            
         OI    TACRSTAT,TACRSSEA                                                
         DROP  RE                                                               
         SPACE 1                                                                
ADDT7    MVC   TCTACREL,TACREL     SAVE IT IN LOCAL STORAGE                     
         SPACE 1                                                                
         OC    TCACAST,TCACAST     IF WE HAVE A(CAST RECORD)                    
         BZ    ADDT8                                                            
         L     R0,AIO                                                           
         MVC   AIO,TCACAST         ADD ELEMENT TO CAST RECORD                   
         GOTO1 ADDL                                                             
         ST    R0,AIO              RESTORE I/O AREA                             
         SPACE 1                                                                
ADDT8    OI    TCRTRN,TCRTCAST+TCRTTACR  SET CAST REC CHGD/TACREL ADDED         
ADDTX    XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE UPDATES APPLIED CREDIT HISTORY ELEMENT                   
         SPACE 1                                                                
UPDTACR  NTR1  BASE=*,LABEL=*                                                   
         TM    TCPAYST2,TCRETRO    IF MAKING RETRO PAYMENT                      
         JZ    XIT                                                              
                                                                                
         USING TACRD,R4                                                         
         ICM   R4,15,TCACAST                                                    
         MVI   ELCODE,TACRELQ      FIND APPLIED CREDIT HISTORY                  
         BRAS  RE,GETEL            ELEMENT FOR ORIGINAL INVOICE                 
         J     *+8                                                              
UTACR10  BRAS  RE,NEXTEL                                                        
         JNE   XIT                                                              
         CLC   TACRSTRT(6),TCIPCYC CYCLE DATES                                  
         JNE   UTACR10                                                          
         CLC   TACRUSE,TGUSCDE                                                  
         JNE   UTACR10                                                          
         OC    TACRBAL,TACRBAL                                                  
         JZ    XIT                                                              
                                                                                
******** MVC   TACRAPPL,TCAPPLIC   BUMP UP AMOUNT TO BE APPLIED                 
                                                                                
         L     RE,TCAPPLIC                                                      
         TM    TGUSSTA2,HLDTYPE                                                 
         JO    UTACR15                                                          
         CLI   TGUSEQU,UREN                                                     
         JE    UTACR15                                                          
         CLI   TGUSEQU,USRE                                                     
         JE    UTACR15                                                          
         CLI   TGUSEQU,UARN                                                     
         JE    UTACR15                                                          
         L     RF,TCRAPPLC                                                      
         SR    RE,RF                                                            
UTACR15  TM    TCPAYST,TCCREDIT                                                 
         JZ    UTACR20                                                          
         LCR   RE,RE                                                            
         L     RF,TACRAPPL                                                      
         AR    RF,RE                                                            
******** ST    RF,TACRAPPL                                                      
UTACR20  ST    RE,TCAPPLIC                                                      
                                                                                
         L     RF,TACRBAL                                                       
         AR    RF,RE               BUMP UP BALANCE                              
         ST    RF,TACRBAL                                                       
                                                                                
         MVC   TACRINV,TGINV       INVOICE NUMBER                               
         XC    TACRINV,=6X'FF'     UNCOMPLEMENT IT                              
                                                                                
         OI    TCRTRN,TCRTCAST+TCRTTACR                                         
         MVC   TCTACREL,TACREL                                                  
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*              CALCULATE HOURLY RATE FOR ACTRA TYPE 2404A COMMERCIALS           
         SPACE 1                                                                
         USING TASDD,R2                                                         
CALCHOUR NTR1  BASE=*,LABEL=*                                                   
         L     R0,TCGROSS          HOULRY RATE = GROSS/8                        
         XR    R1,R1                                                            
         SRDA  R0,31                                                            
         D     R0,=F'8'            8 HOUR WORK DAY                              
         AHI   R1,1                AND ROUNDED                                  
         SRA   R1,1                                                             
         ST    R1,TASDHR           SET HOURLY RATE                              
         XIT1                                                                   
         DROP  R2                                                               
         LTORG                                                                  
         EJECT                                                                  
*              CALCULATE HOURLY RATE FOR OFF CAMERA PERFORMERS                  
         SPACE 1                                                                
         USING TASDD,R2                                                         
CALCHR2  NTR1  BASE=*,LABEL=*                                                   
         L     R0,TASDFEE          HOULRY RATE = SESSION FEE/2                  
         XR    R1,R1                                                            
         SRDA  R0,31                                                            
         D     R0,=F'2'            2 HOUR WORK DAY                              
         AHI   R1,1                AND ROUNDED                                  
         SRA   R1,1                                                             
         ST    R1,TASDHR           SET HOURLY RATE                              
         XIT1                                                                   
         DROP  R2                                                               
         LTORG                                                                  
         EJECT                                                                  
*              TIMESHEET - OFF CAMERA SPOTS VS DAYS                             
*              ADD UP DAYS IN SUBTOTAL ELEMENTS                                 
*              GRAND TOTAL ELEMENT COUNTS EACH TIMESHEET DAY AS ONE             
*              EVEN THOUGH A DAY CAN PAY MULTIPLE SESSION FEES                  
*              RETURNS CC NEQ IF NUMBER OF DAYS > NUMBER OF SPOTS               
         SPACE 1                                                                
TMOFFSD  NTR1  BASE=*,LABEL=*                                                   
         MVI   TGBYTE2,0           NUMBER OF DAYS                               
         MVI   TGBYTE3,0           NUMBER OF SPOTS                              
         L     R4,TCATMTOT         SUBTOTAL ELEMENT                             
         USING TATTD,R4                                                         
         B     TMOFSD03                                                         
*                                                                               
TMOFSD02 ZIC   R5,TATTLEN                                                       
         AR    R4,R5               BUMP TO NEXT SUBTOTAL ELEMENT                
         CLI   0(R4),TATTELQ       MORE SUBTOTAL ELEMENTS?                      
         BNE   TMOFSD09                                                         
*                                                                               
TMOFSD03 CLC   TATTDATE,=X'FFFFFF' GRAND TOTAL ELEMENT                          
         BNE   TMOFSD05                                                         
         MVC   TGBYTE3,TATTSPOT    TOTAL NUMBER OF SPOTS                        
         B     TMOFSD02                                                         
TMOFSD05 CLC   TATTDATE,=X'FFFFFE' GET ALL OTHER TOTAL ELEMENTS                 
         BH    TMOFSD02                                                         
         CLC   TATTDATE,=X'FFFFF0'                                              
         BL    TMOFSD02                                                         
TMOFSD07 ZIC   RE,TATTDAYS         NUMBER OF DAYS                               
         ZIC   R1,TGBYTE2          KEEP RUNNING TOTAL OF DAYS                   
         AR    R1,RE                                                            
         STC   R1,TGBYTE2                                                       
         B     TMOFSD02                                                         
*                                                                               
TMOFSD09 CLC   TGBYTE3,TGBYTE2    COMPARE NUMBER OF SPOTS WITH DAYS             
         BL    TMOFNO                                                           
*                                                                               
TMOFYES  XR    RC,RC                                                            
TMOFNO   LTR   RC,RC                                                            
         XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
         SPACE 1                                                                
*              TIMESHEET - SPOTS                                                
*              SAT/SUN/HOLIDAY - ONLY 1ST SPOT ON EACH DAY IS DOUBLED           
*              WEATHER CANCELLATION GETS 1/2 OR 3/4 PAYCHECK                    
*              TRAVEL TO DISTANT LOCATION ON SATURDAY IS PAID REGULAR           
         SPACE 1                                                                
TIMESPT  NTR1  BASE=*,LABEL=*                                                   
         SR    R1,R1                                                            
         L     R4,TCATMTOT         SUBTOTAL ELEMENT                             
         USING TATTD,R4                                                         
         B     TMSPT05                                                          
*                                                                               
TMSPT02  ZIC   R5,TATTLEN                                                       
         AR    R4,R5               BUMP TO NEXT SUBTOTAL ELEMENT                
         CLI   0(R4),TATTELQ       MORE SUBTOTAL ELEMENTS?                      
         BNE   TMSPTX                                                           
TMSPT05  ZIC   RE,TATTSPOT                                                      
*                                                                               
         CLC   TATTDATE,=X'FFFFFB' SUNDAYS?                                     
         BE    TMSPT07                                                          
         CLC   TATTDATE,=X'FFFFFC' OR SATURDAYS?                                
         BNE   TMSPT30                                                          
TMSPT07  ZIC   RF,TATTDAYS         NUMBER OF DAYS = SPOTS PAID DOUBLE           
         SR    RE,RF               SPOTS-DAYS = SPOTS NOT PAID DOUBLE           
         TM    TGCATYPE,EXTRA      EXTRAS ALWAYS GET DOUBLE                     
         BO    TMSPT08                                                          
         CLC   TCOV1,=F'10000'     IF > THAN 100% OVERSCALE,                    
         BNH   TMSPT08                                                          
         MHI   RF,150              PAY TIME AND A HALF                          
         B     *+8                                                              
TMSPT08  MHI   RF,200              IF <= 100% OVERSCALE, PAY DOUBLE             
         AR    R1,RF               R1 = TOTAL # OF ADJUSTED SPOTS               
         MHI   RE,100              SPOTS NOT DOUBLED                            
         AR    R1,RE               R1 = TOTAL # OF ADJUSTED SPOTS               
         B     TMSPT02             GET NEXT ELEMENT                             
*                                                                               
TMSPT30  CLC   TATTDATE,=X'FFFFF6' WTHR CANC 1/2 SATURDAYS?                     
         BE    TMSPT33                                                          
         CLC   TATTDATE,=X'FFFFF5' WTHR CANC 1/2 SUNDAYS?                       
         BNE   TMSPT40                                                          
TMSPT33  ZIC   RF,TATTDAYS         NUMBER OF DAYS = SPOTS PAID DOUBLE           
         SR    RE,RF               SPOTS-DAYS = SPOTS NOT PAID DOUBLE           
         TM    TGCATYPE,EXTRA      EXTRAS ALWAYS GET 1/2 DOUBLE                 
         BO    TMSPT35                                                          
         CLC   TCOV1,=F'10000'     IF > THAN 100% OVERSCALE,                    
         BNH   TMSPT35                                                          
         MHI   RF,75               PAY TIME AND A HALF (1/2 OF 150)             
         B     *+8                                                              
TMSPT35  MHI   RF,100              IF <= 100% OVERSCALE, PAY 1/2 DOUBLE         
         AR    R1,RF               R1 = TOTAL # OF ADJUSTED SPOTS               
         MHI   RE,50               SPOTS NOT DOUBLED                            
         AR    R1,RE               R1 = TOTAL # OF ADJUSTED SPOTS               
         B     TMSPT02             GET NEXT ELEMENT                             
*                                                                               
TMSPT40  CLC   TATTDATE,=X'FFFFF2' WTHR CANC 3/4 SATURDAYS?                     
         BE    TMSPT43                                                          
         CLC   TATTDATE,=X'FFFFF1' WTHR CANC 3/4 SUNDAYS?                       
         BNE   TMSPT50                                                          
TMSPT43  ZIC   RF,TATTDAYS         NUMBER OF DAYS = SPOTS PAID DOUBLE           
         SR    RE,RF               SPOTS-DAYS = SPOTS NOT PAID DOUBLE           
         TM    TGCATYPE,EXTRA      EXTRAS ALWAYS GET 3/4 DOUBLE                 
         BO    TMSPT45                                                          
         CLC   TCOV1,=F'10000'     IF > THAN 100% OVERSCALE,                    
         BNH   TMSPT45                                                          
         MHI   RF,113              PAY TIME AND A HALF (3/4 OF 150)             
         B     *+8                                                              
TMSPT45  MHI   RF,150              IF <= 100% OVERSCALE, PAY 3/4 DOUBLE         
         AR    R1,RF               R1 = TOTAL # OF ADJUSTED SPOTS               
         MHI   RE,75               SPOTS NOT DOUBLED                            
         AR    R1,RE               R1 = TOTAL # OF ADJUSTED SPOTS               
         B     TMSPT02             GET NEXT ELEMENT                             
*                                                                               
TMSPT50  CLC   TATTDATE,=X'FFFFFD' HOLIDAYS?                                    
         BNE   TMSPT60                                                          
         ZIC   RF,TATTDAYS         NUMBER OF DAYS = SPOTS PAID DOUBLE           
         SR    RE,RF               SPOTS-DAYS = SPOTS NOT PAID DOUBLE           
         MHI   RF,200              ALWAYS PAY DOUBLE FOR HOLIDAY                
         AR    R1,RF               R1 = TOTAL # OF ADJUSTED SPOTS               
         MHI   RE,100              SPOTS NOT DOUBLED                            
         AR    R1,RE               R1 = TOTAL # OF ADJUSTED SPOTS               
         B     TMSPT02             GET NEXT ELEMENT                             
*                                                                               
TMSPT60  CLC   TATTDATE,=X'FFFFF7' WTHR CANC 1/2 HOLIDAYS?                      
         BNE   TMSPT70                                                          
         ZIC   RF,TATTDAYS         NUMBER OF DAYS = SPOTS PAID DOUBLE           
         SR    RE,RF               SPOTS-DAYS = SPOTS NOT PAID DOUBLE           
         MHI   RF,100              ALWAYS PAY 1/2 DOUBLE FOR HOLIDAY            
         AR    R1,RF               R1 = TOTAL # OF ADJUSTED SPOTS               
         MHI   RE,50               SPOTS NOT DOUBLED                            
         AR    R1,RE               R1 = TOTAL # OF ADJUSTED SPOTS               
         B     TMSPT02             GET NEXT ELEMENT                             
*                                                                               
TMSPT70  CLC   TATTDATE,=X'FFFFF3' WTHR CANC 3/4 HOLIDAYS?                      
         BNE   TMSPT80                                                          
         ZIC   RF,TATTDAYS         NUMBER OF DAYS = SPOTS PAID DOUBLE           
         SR    RE,RF               SPOTS-DAYS = SPOTS NOT PAID DOUBLE           
         MHI   RF,150              ALWAYS PAY 3/4 DOUBLE FOR HOLIDAY            
         AR    R1,RF               R1 = TOTAL # OF ADJUSTED SPOTS               
         MHI   RE,75               SPOTS NOT DOUBLED                            
         AR    R1,RE               R1 = TOTAL # OF ADJUSTED SPOTS               
         B     TMSPT02             GET NEXT ELEMENT                             
*                                                                               
TMSPT80  CLC   TATTDATE,=X'FFFFFE' REGULAR DAYS?                                
         BE    TMSPT83                                                          
         CLC   TATTDATE,=X'FFFFF0' SATURDAY DISTANT LOCATION?                   
         BNE   TMSPT90                                                          
TMSPT83  MHI   RE,100                                                           
         AR    R1,RE               ADD TO TOTAL # OF ADJUSTED SPOTS             
         B     TMSPT02                                                          
*                                                                               
TMSPT90  CLC   TATTDATE,=X'FFFFF8' WTHR CANC 1/2 REG DAYS?                      
         BNE   TMSPT100                                                         
         MHI   RE,50               1/2 PAYCHECK                                 
         AR    R1,RE               ADD TO TOTAL # OF ADJUSTED SPOTS             
         B     TMSPT02                                                          
*                                                                               
TMSPT100 CLC   TATTDATE,=X'FFFFF4' WTHR CANC 3/4 REG DAYS?                      
         BNE   TMSPTX                                                           
         MHI   RE,75               3/4 PAYCHECK                                 
         AR    R1,RE               ADD TO TOTAL # OF ADJUSTED SPOTS             
         B     TMSPT02                                                          
*                                                                               
TMSPTX   XIT1  REGS=(R1)                                                        
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
         SPACE 1                                                                
*              TIMESHEET - SPOTS  - 2404A ONLY                                  
         SPACE 1                                                                
TIMESPTA NTR1  BASE=*,LABEL=*                                                   
         SR    R1,R1                                                            
         L     R4,TCATMTOT         SUBTOTAL ELEMENT                             
         USING TATTD,R4                                                         
         B     TMSPA05                                                          
*                                                                               
TMSPA02  ZIC   R5,TATTLEN                                                       
         AR    R4,R5               BUMP TO NEXT SUBTOTAL ELEMENT                
         CLI   0(R4),TATTELQ       MORE SUBTOTAL ELEMENTS?                      
         BNE   TMSPAX                                                           
*                                                                               
TMSPA05  CLC   TATTDATE,=X'FFFFFF' TOTAL OF ALL DAYS?                           
         BNE   TMSPA02                                                          
         ZIC   R1,TATTSPOT                                                      
*                                                                               
TMSPAX   XIT1  REGS=(R1)                                                        
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*              TIMESHEET - DAYS                                                 
         SPACE 1                                                                
TIMEDAY  NTR1  BASE=*,LABEL=*                                                   
         SR    R1,R1                                                            
         L     R4,TCATMTOT         SUBTOTAL ELEMENT                             
         USING TATTD,R4                                                         
         B     TMDAY05                                                          
*                                                                               
TMDAY02  ZIC   R5,TATTLEN                                                       
         AR    R4,R5               BUMP TO NEXT SUBTOTAL ELEMENT                
         CLI   0(R4),TATTELQ       MORE SUBTOTAL ELEMENTS?                      
         BNE   TMDAYX                                                           
TMDAY05  ZIC   RE,TATTDAYS                                                      
*                                                                               
         CLC   TATTDATE,=X'FFFFFB' SUNDAYS?                                     
         BE    TMDAY07                                                          
         CLC   TATTDATE,=X'FFFFFC' OR SATURDAYS?                                
         BNE   TMDAY30                                                          
TMDAY07  TM    TGCATYPE,EXTRA      EXTRAS ALWAYS GET DOUBLE                     
         BO    TMDAY08                                                          
         CLC   TCOV1,=F'10000'     IF > 100% OVERSCALE                          
         BNH   TMDAY08                                                          
         ZIC   R0,TCCAD150                                                      
         AR    R0,RE                                                            
         STC   R0,TCCAD150                                                      
         MHI   RE,150              PAY TIME AND A HALF                          
         B     *+8                                                              
TMDAY08  MHI   RE,200              IF <= 100% OVERSCALE, PAY DOUBLE             
         AR    R1,RE               R1 = TOTAL # OF ADJUSTED DAYS                
         B     TMDAY02             GET NEXT ELEMENT                             
*                                                                               
TMDAY30  CLC   TATTDATE,=X'FFFFF6' WTHR CANC 1/2 SATURDAYS?                     
         BE    TMDAY33                                                          
         CLC   TATTDATE,=X'FFFFF5' WTHR CANC 1/2 SUNDAYS?                       
         BNE   TMDAY40                                                          
TMDAY33  TM    TGCATYPE,EXTRA      EXTRAS ALWAYS GET DOUBLE                     
         BO    TMDAY35                                                          
         CLC   TCOV1,=F'10000'     IF > 100% OVERSCALE                          
         BNH   TMDAY35                                                          
         ZIC   R0,TCCAD075                                                      
         AR    R0,RE                                                            
         STC   R0,TCCAD075                                                      
         MHI   RE,75               PAY TIME AND A HALF (1/2 OF 150)             
         B     *+8                                                              
TMDAY35  MHI   RE,100              IF <= 100% OVERSCALE, PAY 1/2 DOUBLE         
         AR    R1,RE               R1 = TOTAL # OF ADJUSTED DAYS                
         B     TMDAY02             GET NEXT ELEMENT                             
*                                                                               
TMDAY40  CLC   TATTDATE,=X'FFFFF2' WTHR CANC 3/4 SATURDAYS?                     
         BE    TMDAY43                                                          
         CLC   TATTDATE,=X'FFFFF1' WTHR CANC 3/4 SUNDAYS?                       
         BNE   TMDAY50                                                          
TMDAY43  TM    TGCATYPE,EXTRA      EXTRAS ALWAYS GET DOUBLE                     
         BO    TMDAY45                                                          
         CLC   TCOV1,=F'10000'     IF > 100% OVERSCALE                          
         BNH   TMDAY08                                                          
         MHI   RE,113              PAY TIME AND A HALF (3/4 OF 150)             
         B     TMDAY46                                                          
TMDAY45  ZIC   R0,TCCAD150         IF <= 100% OVERSCALE,                        
         AR    R0,RE                                                            
         STC   R0,TCCAD150                                                      
         MHI   RE,150              PAY 3/4 DOUBLE                               
TMDAY46  AR    R1,RE               R1 = TOTAL # OF ADJUSTED DAYS                
         B     TMDAY02             GET NEXT ELEMENT                             
*                                                                               
TMDAY50  CLC   TATTDATE,=X'FFFFFD' HOLIDAYS?                                    
         BNE   TMDAY60                                                          
         MHI   RE,200              ALWAYS PAY DOUBLE FOR HOLIDAY                
         AR    R1,RE               R1 = TOTAL # OF ADJUSTED DAYS                
         B     TMDAY02             GET NEXT ELEMENT                             
*                                                                               
TMDAY60  CLC   TATTDATE,=X'FFFFF7' WTHR CANC 1/2 HOLIDAYS?                      
         BNE   TMDAY70                                                          
         MHI   RE,100              ALWAYS PAY 1/2 DOUBLE FOR HOLIDAY            
         AR    R1,RE               R1 = TOTAL # OF ADJUSTED DAYS                
         B     TMDAY02             GET NEXT ELEMENT                             
*                                                                               
TMDAY70  CLC   TATTDATE,=X'FFFFF3' WTHR CANC 3/4 HOLIDAYS?                      
         BNE   TMDAY80                                                          
         ZIC   R0,TCCAD150                                                      
         AR    R0,RE                                                            
         STC   R0,TCCAD150                                                      
         MHI   RE,150              ALWAYS PAY 3/4 DOUBLE FOR HOLIDAY            
         AR    R1,RE               R1 = TOTAL # OF ADJUSTED DAYS                
         B     TMDAY02             GET NEXT ELEMENT                             
*                                                                               
TMDAY80  CLC   TATTDATE,=X'FFFFFE' REGULAR DAYS?                                
         BE    TMDAY85                                                          
         CLC   TATTDATE,=X'FFFFF0' SATURDAY DISTANT LOCATION?                   
         BNE   TMDAY90                                                          
TMDAY85  MHI   RE,100                                                           
         AR    R1,RE               ADD TO TOTAL # OF ADJUSTED DAYS              
         B     TMDAY02             GET NEXT ELEMENT                             
*                                                                               
TMDAY90  CLC   TATTDATE,=X'FFFFF8' WTHR CANC 1/2 REG DAYS?                      
         BNE   TMDAY100                                                         
         ZIC   R0,TCCAD050                                                      
         AR    R0,RE                                                            
         STC   R0,TCCAD050                                                      
         MHI   RE,50               1/2 PAYCHECK                                 
         AR    R1,RE               ADD TO TOTAL # OF ADJUSTED DAYS              
         B     TMDAY02             GET NEXT ELEMENT                             
*                                                                               
TMDAY100 CLC   TATTDATE,=X'FFFFF4' WTHR CANC 3/4 REG DAYS?                      
         BNE   TMDAYX                                                           
         ZIC   R0,TCCAD075                                                      
         AR    R0,RE                                                            
         STC   R0,TCCAD075                                                      
         MHI   RE,75               3/4 PAYCHECK                                 
         AR    R1,RE               ADD TO TOTAL # OF ADJUSTED DAYS              
         B     TMDAY02             GET NEXT ELEMENT                             
*                                                                               
TMDAYX   XIT1  REGS=(R1)                                                        
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*              TIMESHEET - DAYS - 2404A ONLY                                    
         SPACE 1                                                                
TIMEDAYA NTR1  BASE=*,LABEL=*                                                   
         SR    R1,R1                                                            
         L     R4,TCATMTOT         SUBTOTAL ELEMENT                             
         USING TATTD,R4                                                         
         B     TMDYA05                                                          
*                                                                               
TMDYA02  ZIC   R5,TATTLEN                                                       
         AR    R4,R5               BUMP TO NEXT SUBTOTAL ELEMENT                
         CLI   0(R4),TATTELQ       MORE SUBTOTAL ELEMENTS?                      
         BNE   TMDYAX                                                           
*                                                                               
TMDYA05  CLC   TATTDATE,=X'FFFFFF' TOTAL OF ALL DAYS?                           
         BNE   TMDYA02                                                          
         ZIC   R1,TATTDAYS                                                      
*                                                                               
TMDYAX   XIT1  REGS=(R1)                                                        
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*              TIMESHEET - OVERTIME                                             
         SPACE 1                                                                
TIMEOT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
TMOT01   SR    R0,R0                                                            
         L     R4,TCATMTOT         SUBTOTAL ELEMENT                             
         USING TATTD,R4                                                         
         B     TMOT05                                                           
*                                                                               
TMOT02   ZIC   R5,TATTLEN                                                       
         AR    R4,R5               BUMP TO NEXT SUBTOTAL ELEMENT                
         CLI   0(R4),TATTELQ       MORE SUBTOTAL ELEMENTS?                      
         BNE   TMOTX                                                            
TMOT05   ZIC   RE,TATTOVTM                                                      
*                                                                               
         USING TACOD,R2                                                         
         L     R2,TCATACO          IF COMMERCIAL HAS ANY ACTRA                  
         CLI   TACOCTYP,0          TYPE EXCEPT FOR 2404B,                       
         BE    TMOT06                                                           
         CLI   TACOCTYP,CCTY04B                                                 
         BE    TMOT06                                                           
         CLC   TATTDATE,=X'FFFFFF' TOTAL OF ALL DAYS                            
         BNE   TMOT02                                                           
         AR    R0,RE               TAKE ALL OVERTIME HOURS                      
         B     TMOTX                                                            
         DROP  R2                                                               
*                                                                               
TMOT06   CLC   TATTDATE,=X'FFFFFB' SUNDAYS?                                     
         BE    TMOT07                                                           
         CLC   TATTDATE,=X'FFFFFC' OR SATURDAYS?                                
         BNE   TMOT10                                                           
TMOT07   TM    TGCATYPE,EXTRA      EXTRAS? SKIP (USE DBLTIME RATE)              
         BO    TMOT02                                                           
         CLC   TCOV1,=F'10000'     IF > 100% OVERSCALE,                         
         BNH   TMOT02                                                           
         AR    R0,RE               ADD TO TOTAL # OF OVERTIME HRS               
         ZIC   RE,TATTDBTM                                                      
         AR    R0,RE               ADD DOUBLETIME HOURS (SAME RATE)             
         B     TMOT02              GET NEXT ELEMENT                             
*                                                                               
TMOT10   CLC   TATTDATE,=X'FFFFFD' HOLIDAY? SKIP (USE DBLTIME RATE)             
         BE    TMOT02              GET NEXT ELEMENT                             
*                                                                               
         CLC   TATTDATE,=X'FFFFFE' REGULAR DAY?                                 
         BE    TMOT20                                                           
         CLC   TATTDATE,=X'FFFFF0' SATURDAY DISTANT LOCATION?                   
         BNE   TMOT02                                                           
TMOT20   AR    R0,RE               ADD TO TOTAL # OF OVERTIME HOURS             
         TM    TGCATYPE,EXTRA      IF NOT AN EXTRA,                             
         BO    TMOT02                                                           
         CLC   TCOV1,=F'10000'     AND > 100% OVERSCALE,                        
         BNH   TMOT02                                                           
         ZIC   RE,TATTDBTM                                                      
         AR    R0,RE               ADD DOUBLETIME HOURS (SAME RATE)             
         B     TMOT02                                                           
*                                                                               
TMOTX    XIT1  REGS=(R0)                                                        
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*              TIMESHEET - DOUBLETIME                                           
         SPACE 1                                                                
TIMEDT   NTR1  BASE=*,LABEL=*                                                   
         SR    R0,R0                                                            
         L     R4,TCATMTOT         SUBTOTAL ELEMENT                             
         USING TATTD,R4                                                         
         B     TMDT05                                                           
*                                                                               
TMDT02   ZIC   R5,TATTLEN                                                       
         AR    R4,R5               BUMP TO NEXT SUBTOTAL ELEMENT                
         CLI   0(R4),TATTELQ       MORE SUBTOTAL ELEMENTS?                      
         BNE   TMDTX                                                            
TMDT05   ZIC   RE,TATTDBTM                                                      
*                                                                               
         USING TACOD,R2                                                         
         L     R2,TCATACO          IF COMMERCIAL HAS ANY ACTRA                  
         CLI   TACOCTYP,0          TYPE EXCEPT FOR 2404B,                       
         BE    TMDT06                                                           
         CLI   TACOCTYP,CCTY04B                                                 
         BE    TMDT06                                                           
         CLC   TATTDATE,=X'FFFFFF' TOTAL OF ALL DAYS                            
         BNE   TMDT02                                                           
         AR    R0,RE               TAKE ALL DOUBLETIME HOURS                    
         B     TMDTX                                                            
         DROP  R2                                                               
*                                                                               
TMDT06   CLC   TATTDATE,=X'FFFFFB' SUNDAYS?                                     
         BE    TMDT07                                                           
         CLC   TATTDATE,=X'FFFFFC' OR SATURDAYS? USE REG. DBLTIME RATE          
         BNE   TMDT10                                                           
TMDT07   TM    TGCATYPE,EXTRA      EXTRAS ALWAYS GET REG. DBLTIME RATE          
         BO    TMDT09                                                           
         CLC   TCOV1,=F'10000'     IF > 100% OVERSCALE, SKIP                    
         BH    TMDT02                                                           
TMDT09   AR    R0,RE               ADD TO TOTAL # OF DOUBLETIME HRS             
         ZIC   R1,TATTOVTM         ADD OVERTIME HOURS (SAME RATE)               
         AR    R0,R1                                                            
         B     TMDT02              GET NEXT ELEMENT                             
*                                                                               
TMDT10   CLC   TATTDATE,=X'FFFFFD' HOLIDAYS? USE REGULAR DBLTIME RATE           
         BNE   TMDT20                                                           
         AR    R0,RE               ADD TO TOTAL # OF DOUBLETIME HRS             
         ZIC   R1,TATTOVTM         ADD OVERTIME HOURS (SAME RATE)               
         AR    R0,R1                                                            
         B     TMDT02              GET NEXT ELEMENT                             
*                                                                               
TMDT20   CLC   TATTDATE,=X'FFFFFE' REGULAR DAYS?                                
         BE    TMDT25                                                           
         CLC   TATTDATE,=X'FFFFF0' SATURDAY DISTANT LOCATION?                   
         BNE   TMDT02                                                           
TMDT25   TM    TGCATYPE,EXTRA      IF NOT AN EXTRA,                             
         BO    TMDT30                                                           
         CLC   TCOV1,=F'10000'     AND > 100% OVERSCALE, SKIP                   
         BH    TMDT02                                                           
TMDT30   AR    R0,RE               ADD TO TOTAL # OF DOUBLETIME HRS             
         B     TMDT02                                                           
*                                                                               
TMDTX    XIT1  REGS=(R0)                                                        
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*              TIMESHEET - TRAVEL TIME                                          
*              HRS.MINS * RATE RETURNED IN R1                                   
*              EXTRAS - STRAIGHT TIME ON SATURDAYS, 1.5 ON SUN/HOLIDAYS         
*              PRINCIPALS - 1.5 ON SAT/SUN/HOLIDAYS                             
         SPACE 1                                                                
TIMETRVL NTR1  BASE=*,LABEL=*                                                   
         ST    R1,TCTMFULL         TCTMFULL = RATE                              
         SR    R1,R1                                                            
         L     R4,TCATMTOT         SUBTOTAL ELEMENT                             
         USING TATTD,R4                                                         
         B     TMTRVL05                                                         
*                                                                               
TMTRVL02 ZIC   R5,TATTLEN                                                       
         AR    R4,R5               BUMP TO NEXT SUBTOTAL ELEMENT                
         CLI   0(R4),TATTELQ       MORE SUBTOTAL ELEMENTS?                      
         BNE   TMTRVLX                                                          
TMTRVL05 ZICM  RF,TATTTRVL,2       TRAVEL HRS.MINS                              
*                                                                               
         USING TACOD,R2                                                         
         L     R2,TCATACO          IF COMMERCIAL HAS ANY ACTRA                  
         CLI   TACOCTYP,0          TYPE EXCEPT FOR 2404B,                       
         BE    TMTRVL06                                                         
         CLI   TACOCTYP,CCTY04B                                                 
         BE    TMTRVL06                                                         
         CLC   TATTDATE,=X'FFFFFF' TOTAL OF ALL DAYS                            
         BNE   TMTRVL02                                                         
         DROP  R2                                                               
*                                                                               
         STH   RF,HALF                                                          
         BRAS  RE,HRS2MIN                                                       
         LH    RF,HALF                                                          
         MHI   RF,500                                                           
         SR    RE,RE               MINS *100 *100 /60                           
         D     RE,=F'3'                                                         
         AR    R1,RF               R1 = TOTAL # OF ADJUSTED MINUTES             
         B     TMTRVLX                                                          
*                                                                               
TMTRVL06 CLC   TATTDATE,=X'FFFFFB' SUNDAYS?                                     
         BE    TMTRVL20                                                         
         CLC   TATTDATE,=X'FFFFF5' WTHR CXL 1/2 SUN?                            
         BE    TMTRVL20                                                         
         CLC   TATTDATE,=X'FFFFF1' WTHR CXL 3/4 SUN?                            
         BE    TMTRVL20                                                         
         CLC   TATTDATE,=X'FFFFFC' SATURDAYS?                                   
         BE    TMTRVL08                                                         
         CLC   TATTDATE,=X'FFFFF6' WTHR CXL 1/2 SAT?                            
         BE    TMTRVL08                                                         
         CLC   TATTDATE,=X'FFFFF2' WTHR CXL 3/4 SAT?                            
         BNE   TMTRVL10                                                         
TMTRVL08 TM    TGCATYPE,EXTRA      EXTRAS GET STRAIGHT TIME ON SATURDAY         
         BNO   TMTRVL20                                                         
         STH   RF,HALF                                                          
         BRAS  RE,HRS2MIN                                                       
         LH    RF,HALF                                                          
         MHI   RF,500                                                           
         SR    RE,RE               MINS *100 *100 /60                           
         D     RE,=F'3'                                                         
         AR    R1,RF               R1 = TOTAL # OF ADJUSTED MINUTES             
         B     TMTRVL02                                                         
*                                                                               
TMTRVL10 CLC   TATTDATE,=X'FFFFFD' HOLIDAYS?                                    
         BE    TMTRVL20                                                         
         CLC   TATTDATE,=X'FFFFF7' WTHR CXL 1/2 HOLIDAY?                        
         BE    TMTRVL20                                                         
         CLC   TATTDATE,=X'FFFFF3' WTHR CXL 3/4 HOLIDAY?                        
         BNE   TMTRVL30                                                         
TMTRVL20 STH   RF,HALF                                                          
         BRAS  RE,HRS2MIN          CONVERT TO MINUTES                           
         LH    RF,HALF             PRINCIPALS GET TIME AND A HALF               
         MHI   RF,250              MINS *150 *100 /60                           
         AR    R1,RF               R1 = TOTAL # OF ADJUSTED MINUTES             
         B     TMTRVL02            GET NEXT ELEMENT                             
*                                                                               
TMTRVL30 CLC   TATTDATE,=X'FFFFFE' REGULAR DAYS?                                
         BE    TMTRVL35                                                         
         CLC   TATTDATE,=X'FFFFF8' WTHR CXL 1/2 REGULAR DAY?                    
         BE    TMTRVL35                                                         
         CLC   TATTDATE,=X'FFFFF4' WTHR CXL 3/4 REGULAR DAY?                    
         BE    TMTRVL35                                                         
         CLC   TATTDATE,=X'FFFFF0' SATURDAY DISTANT LOCATION?                   
         BNE   TMTRVL02                                                         
TMTRVL35 STH   RF,HALF                                                          
         BRAS  RE,HRS2MIN                                                       
         LH    RF,HALF                                                          
         MHI   RF,500                                                           
         SR    RE,RE               MINS *100 *100 /60                           
         D     RE,=F'3'                                                         
         AR    R1,RF               R1 = TOTAL # OF ADJUSTED MINUTES             
         B     TMTRVL02                                                         
*                                                                               
TMTRVLX  CVD   R1,DUB              CONVERT ADJUSTED MINUTES TO PACKED           
         L     RF,TCTMFULL                                                      
*                                                                               
         LR    R5,R1               SAVE IT BEFORE BREAK DOWN                    
         ST    R5,WORK+4                                                        
         MVC   WORK(4),TCTMFULL                                                 
         GOTOR SVBRKDWN,DMCB,(0,WORK),('PVCTRV',WORK+4)                         
         LR    R1,R5               RESTORE IT                                   
*                                                                               
         CVD   RF,TGDUB                                                         
         MVC   TCRATEPK,TGDUB+5                                                 
         MP    DUB,TCRATEPK        MULTIPLY BY PACKED RATE                      
         SRP   DUB,60,5            DIVIDE BY 10,000 AND ROUND                   
         CVB   R1,DUB                                                           
         XIT1  REGS=(R1)                                                        
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*              TIMESHEET - PRIOR DAY WARDROBE                                   
*              HRS.MINS * RATE RETURNED IN R1                                   
         SPACE 1                                                                
TIMEPDWD NTR1  BASE=*,LABEL=*                                                   
         ST    R1,TCTMFULL         TCTMFULL = RATE                              
         SR    R1,R1                                                            
*                                                                               
         CLC   TCOV1,=F'10000'     IF > 100% OVERSCALE,                         
         BH    TPDWDX              NO COMPENSATION                              
*                                                                               
         L     R4,TCATMTOT         SUBTOTAL ELEMENT                             
         USING TATTD,R4                                                         
         B     TMPDWD05                                                         
*                                                                               
TMPDWD02 ZIC   R5,TATTLEN                                                       
         AR    R4,R5               BUMP TO NEXT SUBTOTAL ELEMENT                
TMPDWD05 CLI   0(R4),TATTELQ       MORE SUBTOTAL ELEMENTS?                      
         BE    TMPDWD02                                                         
         CLI   0(R4),TATPELQ       FIND PDWD ELEMENT - AFTER ALL TOTALS         
         BNE   TPDWDX              EXIT IF WE DON'T FIND ONE                    
*                                                                               
         USING TACOD,R2                                                         
         L     R2,TCATACO          IF COMMERCIAL HAS ANY ACTRA                  
         CLI   TACOCTYP,0          TYPE EXCEPT FOR 2404B,                       
         BE    TPDWD08                                                          
         CLI   TACOCTYP,CCTY04B                                                 
         BE    TPDWD08                                                          
         DROP  R2                                                               
*                                  ACTRA - ALL DAYS ARE PAID THE SAME           
         USING TATPD,R4                                                         
         ZICM  R1,TATPREG,2        REGULAR DAYS                                 
         ZICM  RE,TATPHOL,2        HOLIDAYS                                     
         AR    R1,RE               ADD TO TOTAL                                 
         ZICM  RE,TATPSAT,2        SATURDAYS                                    
         AR    R1,RE               ADD TO TOTAL                                 
         ZICM  RE,TATPSUN,2        SUNDAYS                                      
         AR    R1,RE               ADD TO TOTAL                                 
         B     TPDWD10                                                          
*                                                                               
TPDWD08  ZICM  R1,TATPREG,2        REGULAR DAYS                                 
         ZICM  RE,TATPHOL,2        HOLIDAYS - DOUBLE                            
         MHI   RE,2                                                             
         AR    R1,RE               ADD TO TOTAL                                 
         ZICM  RE,TATPSAT,2        SATURDAYS - DOUBLE IF PRINCIPAL              
         TM    TGCATYPE,EXTRA      EXTRAS GET STRAIGHT TIME ON SATURDAY         
         BO    *+8                                                              
         MHI   RE,2                                                             
         AR    R1,RE               ADD TO TOTAL                                 
         ZICM  RE,TATPSUN,2        SUNDAYS - DOUBLE                             
         MHI   RE,2                                                             
         AR    R1,RE               ADD TO TOTAL                                 
*                                                                               
         LR    R5,R1               SAVE IT BEFORE BREAK DOWN                    
         STCM  R1,15,WORK+4                                                     
         MVC   WORK(4),TCTMFULL                                                 
         GOTOR SVBRKDWN,DMCB,(0,WORK),(56,WORK+4)                               
         LR    R1,R5               RESTORE IT                                   
*                                                                               
TPDWD10  M     R0,TCTMFULL         MULTIPLY TOTAL BY RATE                       
         D     R0,=F'100'          DIVIDE BY 100                                
         CHI   R0,50                                                            
         BL    *+8                                                              
         AHI   R1,1                                                             
TPDWDX   XIT1  REGS=(R1)                                                        
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*              TIMESHEET - TAGS                                                 
         SPACE 1                                                                
TIMETAG  NTR1  BASE=*,LABEL=*                                                   
         SR    R1,R1                                                            
*                                                                               
         USING TACOD,R2                                                         
         L     R2,TCATACO          IF COMMERCIAL HAS ANY ACTRA                  
         CLI   TACOCTYP,0          TYPE EXCEPT FOR 2404B, NO TAGS               
         BE    TMTAG01             NO TAGS                                      
         CLI   TACOCTYP,CCTY04B                                                 
         BNE   TMTAGX                                                           
         DROP  R2                                                               
*                                                                               
TMTAG01  L     R4,TCATMTOT         SUBTOTAL ELEMENT                             
         USING TATTD,R4                                                         
         B     TMTAG05                                                          
*                                                                               
TMTAG02  ZIC   R5,TATTLEN                                                       
         AR    R4,R5               BUMP TO NEXT SUBTOTAL ELEMENT                
         CLI   0(R4),TATTELQ       MORE SUBTOTAL ELEMENTS?                      
         BNE   TMTAGX                                                           
*                                                                               
TMTAG05  CLC   TATTDATE,=X'FFFFFF' TOTAL ELEMENT                                
         BNE   TMTAG02                                                          
         ZIC   R1,TATTTAG          TOTAL NUMBER OF TAGS                         
*                                                                               
TMTAGX   XIT1  REGS=(R1)                                                        
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*              TIMESHEET - 16 HOUR RULE                                         
         SPACE 1                                                                
TIME16HR NTR1  BASE=*,LABEL=*                                                   
         SR    R1,R1                                                            
*                                                                               
         USING TACOD,R2                                                         
         L     R2,TCATACO          IF COMMERCIAL HAS ANY ACTRA                  
         CLI   TACOCTYP,0          TYPE EXCEPT FOR 2404B,                       
         BE    TM16HR01            NO 16 HOUR RULE CALCULATED                   
         CLI   TACOCTYP,CCTY04B                                                 
         BNE   TM16HRX                                                          
         DROP  R2                                                               
*                                                                               
TM16HR01 TM    TGCATYPE,EXTRA      EXTRAS ONLY                                  
         BNO   TM16HRX                                                          
         L     R4,TCATMTOT         SUBTOTAL ELEMENT                             
         USING TATTD,R4                                                         
         B     TM16HR05                                                         
*                                                                               
TM16HR02 ZIC   R5,TATTLEN                                                       
         AR    R4,R5               BUMP TO NEXT SUBTOTAL ELEMENT                
         CLI   0(R4),TATTELQ       MORE SUBTOTAL ELEMENTS?                      
         BNE   TM16HRX                                                          
TM16HR05 ZIC   RE,TATT16HR         # OF HRS OVER 16                             
*                                                                               
         CLC   TATTDATE,=X'FFFFFB' SUNDAYS?                                     
         BE    TM16HR10                                                         
         CLC   TATTDATE,=X'FFFFFC' SATURDAYS?                                   
         BE    TM16HR10                                                         
         CLC   TATTDATE,=X'FFFFFD' HOLIDAYS?                                    
         BNE   TM16HR20                                                         
TM16HR10 MHI   RE,2                DOUBLE                                       
         AR    R1,RE               R1 = TOTAL # OF ADJUSTED HOURS               
         B     TM16HR02            GET NEXT ELEMENT                             
*                                                                               
TM16HR20 CLC   TATTDATE,=X'FFFFFE' REGULAR DAYS?                                
         BE    TM16HR25                                                         
         CLC   TATTDATE,=X'FFFFF0' SATURDAY DISTANT LOCATION?                   
         BNE   TM16HR02                                                         
TM16HR25 AR    R1,RE               ADD TO TOTAL # OF ADJUSTED HOURS             
         B     TM16HR02                                                         
*                                                                               
TM16HRX  XIT1  REGS=(R1)                                                        
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*              TIMESHEET - NIGHT PREMIUM                                        
         SPACE 1                                                                
TIMENP   NTR1  BASE=*,LABEL=*                                                   
         SR    R1,R1                                                            
         L     R4,TCATMTOT         SUBTOTAL ELEMENT                             
         USING TATTD,R4                                                         
         B     TMNP05                                                           
*                                                                               
TMNP02   ZIC   R5,TATTLEN                                                       
         AR    R4,R5               BUMP TO NEXT SUBTOTAL ELEMENT                
         CLI   0(R4),TATTELQ       MORE SUBTOTAL ELEMENTS?                      
         BNE   TMNPX                                                            
TMNP05   ZICM  RF,TATTNP10,2       # OF NIGHT PREMIUM 10% HRS                   
*                                                                               
         CLC   TATTDATE,=X'FFFFFB' SUNDAYS?                                     
         BE    TMNP07                                                           
         CLC   TATTDATE,=X'FFFFFC' OR SATURDAYS?                                
         BE    TMNP07                                                           
         CLC   TATTDATE,=X'FFFFF6' WTHR CANC 1/2 SATURDAYS?                     
         BE    TMNP07                                                           
         CLC   TATTDATE,=X'FFFFF5' WTHR CANC 1/2 SUNDAYS?                       
         BE    TMNP07                                                           
         CLC   TATTDATE,=X'FFFFF2' WTHR CANC 3/4 SATURDAYS?                     
         BE    TMNP07                                                           
         CLC   TATTDATE,=X'FFFFF1' WTHR CANC 3/4 SUNDAYS?                       
         BNE   TMNP50                                                           
TMNP07   TM    TGCATYPE,EXTRA      EXTRAS ALWAYS DOUBLE                         
         BNZ   TMNP08                                                           
         CLC   TCOV1,=F'10000'     IF > 100% OVERSCALE,                         
         BNH   TMNP08                                                           
         MHI   RF,150              PAY TIME AND A HALF                          
         B     *+8                                                              
TMNP08   MHI   RF,200              IF <= 100% OVERSCALE, PAY DOUBLE             
         SR    RE,RE                                                            
         D     RE,=F'10'           GET 10% OF NP10 HRS                          
         AR    R1,RF               R1 = TOTAL # OF ADJUSTED HOURS               
*                                                                               
         TM    TGCATYPE,EXTRA      EXTRAS ONLY                                  
         BZ    TMNP02                                                           
         SR    RE,RE                                                            
         ZICM  RF,TATTNP20,2                                                    
         MHI   RF,200              EXTRAS ALWAYS DOUBLE                         
         D     RE,=F'5'            GET 20% OF NP20 HRS                          
         AR    R1,RF               R1 = TOTAL # OF ADJUSTED HOURS               
         B     TMNP02              GET NEXT ELEMENT                             
*                                                                               
TMNP50   CLC   TATTDATE,=X'FFFFFD' HOLIDAYS?                                    
         BE    TMNP55                                                           
         CLC   TATTDATE,=X'FFFFF7' WTHR CANC 1/2 HOLIDAYS?                      
         BE    TMNP55                                                           
         CLC   TATTDATE,=X'FFFFF3' WTHR CANC 3/4 HOLIDAYS?                      
         BNE   TMNP80                                                           
TMNP55   MHI   RF,200              PAY DOUBLE                                   
         SR    RE,RE                                                            
         D     RE,=F'10'           GET 10% OF NP10 HRS                          
         AR    R1,RF               R1 = TOTAL # OF ADJUSTED HOURS               
         TM    TGCATYPE,EXTRA      EXTRAS ONLY                                  
         BZ    TMNP02                                                           
         SR    RE,RE                                                            
         ZICM  RF,TATTNP20,2                                                    
         MHI   RF,200              PAY DOUBLE                                   
         D     RE,=F'5'            GET 20% OF NP20 HRS                          
         AR    R1,RF               R1 = TOTAL # OF ADJUSTED HOURS               
         B     TMNP02              GET NEXT ELEMENT                             
*                                                                               
TMNP80   CLC   TATTDATE,=X'FFFFFE' REGULAR DAYS?                                
         BE    TMNP85                                                           
         CLC   TATTDATE,=X'FFFFF8' WTHR CANC 1/2 REG DAYS?                      
         BE    TMNP85                                                           
         CLC   TATTDATE,=X'FFFFF4' WTHR CANC 3/4 REG DAYS?                      
         BE    TMNP85                                                           
         CLC   TATTDATE,=X'FFFFF0' SATURDAY DISTANT LOCATION?                   
         BNE   TMNP02                                                           
TMNP85   MHI   RF,100                                                           
         SR    RE,RE                                                            
         D     RE,=F'10'           GET 10% OF NP10 HRS                          
         AR    R1,RF               R1 = TOTAL # OF ADJUSTED HOURS               
         TM    TGCATYPE,EXTRA      EXTRAS ONLY                                  
         BZ    TMNP02                                                           
         SR    RE,RE                                                            
         ZICM  RF,TATTNP20,2                                                    
         MHI   RF,100                                                           
         D     RE,=F'5'            GET 20% OF NP20 HRS                          
         AR    R1,RF               R1 = TOTAL # OF ADJUSTED HOURS               
         B     TMNP02                                                           
*                                                                               
TMNPX    XIT1  REGS=(R1)                                                        
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*              TIMESHEET - NIGHT PREMIUM - ACTRA STRAIGHT TIME                  
         SPACE 1                                                                
TIMENPS  NTR1  BASE=*,LABEL=*                                                   
         L     R4,TCATMTOT         SUBTOTAL ELEMENT                             
         USING TATTD,R4                                                         
         B     TMNPS05                                                          
*                                                                               
TMNPS02  ZIC   R5,TATTLEN                                                       
         AR    R4,R5               BUMP TO NEXT SUBTOTAL ELEMENT                
         CLI   0(R4),TATTELQ       MORE SUBTOTAL ELEMENTS?                      
         BNE   TMNPSX                                                           
TMNPS05  ZICM  RF,TATTNPST,2       # OF NIGHT PREMIUM STRAIGHT HOURS            
*                                                                               
         CLC   TATTDATE,=X'FFFFF9' TOTAL OF ALL ACTRA DAYS                      
         BNE   TMNPS02                                                          
*                                                                               
         SR    RE,RE               ROUND UP TO NEAREST HOUR                     
         D     RE,=F'100'          SEPARATE HOURS AND MINUTES                   
         LTR   RE,RE               NO MINUTES TO ROUND UP                       
         BZ    *+8                                                              
         AHI   RF,1                ROUND UP ONE HOUR                            
         MHI   RF,100                                                           
*                                                                               
         MHI   RF,100                                                           
         SR    RE,RE                                                            
         USING TACOD,R2                                                         
         L     R2,TCATACO                                                       
         CLI   TACOCTYP,CCTY04A    IF 2404A,                                    
         BE    TMNPS10                                                          
         CLC   TCPCYCS,=X'B40825'  OR IF CYCLE STARTS BEFORE YEAR 1             
         BNL   TMNPS20                                                          
         DROP  R2                                                               
                                                                                
TMNPS10  D     RE,=F'4'            GET 25% OF STRAIGHT                          
         B     TMNPS30                                                          
TMNPS20  D     RE,=F'5'            GET 20% OF STRAIGHT - NEW RULE               
*                                                                               
*                                  RF = TOTAL # OF ADJUSTED HOURS               
TMNPS30  M     RE,TCGROSS          * HOURLY RATE                                
         D     RE,=F'10000'        DIVIDE BY 100                                
         LTR   RE,RE               ROUND UP IF ANY REMAINDER                    
         BZ    *+8                                                              
         AHI   RF,1                                                             
*                                                                               
         AR    R1,RF                                                            
*                                                                               
TMNPSX   XIT1  REGS=(R1)                                                        
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*              TIMESHEET - NIGHT PREMIUM - ACTRA OVERTIME                       
         SPACE 1                                                                
TIMENPO  NTR1  BASE=*,LABEL=*                                                   
         L     R4,TCATMTOT         SUBTOTAL ELEMENT                             
         USING TATTD,R4                                                         
         B     TMNPO05                                                          
*                                                                               
TMNPO02  ZIC   R5,TATTLEN                                                       
         AR    R4,R5               BUMP TO NEXT SUBTOTAL ELEMENT                
         CLI   0(R4),TATTELQ       MORE SUBTOTAL ELEMENTS?                      
         BNE   TMNPOX                                                           
TMNPO05  ZICM  RF,TATTNPOT,2       # OF NIGHT PREMIUM OVERTIME HOURS            
*                                                                               
         CLC   TATTDATE,=X'FFFFF9' TOTAL OF ALL ACTRA DAYS                      
         BNE   TMNPO02                                                          
*                                                                               
         SR    RE,RE               ROUND UP TO NEAREST HOUR                     
         D     RE,=F'100'          SEPARATE HOURS AND MINUTES                   
         LTR   RE,RE               NO MINUTES TO ROUND UP                       
         BZ    *+8                                                              
         AHI   RF,1                ROUND UP ONE HOUR                            
         MHI   RF,100                                                           
*                                                                               
         MHI   RF,100                                                           
         SR    RE,RE                                                            
         USING TACOD,R2                                                         
         L     R2,TCATACO                                                       
         CLI   TACOCTYP,CCTY04A    IF 2404A,                                    
         BE    TMNPO10                                                          
         CLC   TCPCYCS,=X'B40825'  OR IF CYCLE STARTS BEFORE YEAR 1             
         BNL   TMNPO20                                                          
         DROP  R2                                                               
                                                                                
TMNPO10  D     RE,=F'4'            GET 25% OF OVERTIME HOURS                    
         B     TMNPO30                                                          
TMNPO20  D     RE,=F'5'            GET 20% OF STRAIGHT - NEW RULE               
*                                                                               
*                                  RF = TOTAL # OF ADJUSTED HOURS               
TMNPO30  M     RE,TCGROSS          * HOURLY RATE                                
         D     RE,=F'10000'        DIVIDE BY 100                                
         LTR   RE,RE               ROUND UP IF ANY REMAINDER                    
         BZ    *+8                                                              
         AHI   RF,1                                                             
*                                                                               
         AR    R1,RF               ADD TO STRAIGHT HOURS                        
*                                                                               
TMNPOX   XIT1  REGS=(R1)                                                        
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*              TIMESHEET - NIGHT PREMIUM - ACTRA DOUBLETIME                     
         SPACE 1                                                                
TIMENPD  NTR1  BASE=*,LABEL=*                                                   
         L     R4,TCATMTOT         SUBTOTAL ELEMENT                             
         USING TATTD,R4                                                         
         B     TMNPD05                                                          
*                                                                               
TMNPD02  ZIC   R5,TATTLEN                                                       
         AR    R4,R5               BUMP TO NEXT SUBTOTAL ELEMENT                
         CLI   0(R4),TATTELQ       MORE SUBTOTAL ELEMENTS?                      
         BNE   TMNPDX                                                           
TMNPD05  ZICM  RF,TATTNPDT,2       # OF NIGHT PREMIUM DOUBLETIME HOURS          
*                                                                               
         CLC   TATTDATE,=X'FFFFF9' TOTAL OF ALL ACTRA DAYS                      
         BNE   TMNPD02                                                          
*                                                                               
         SR    RE,RE               ROUND UP TO NEAREST HOUR                     
         D     RE,=F'100'          SEPARATE HOURS AND MINUTES                   
         LTR   RE,RE               NO MINUTES TO ROUND UP                       
         BZ    *+8                                                              
         AHI   RF,1                ROUND UP ONE HOUR                            
         MHI   RF,100                                                           
*                                                                               
         MHI   RF,100                                                           
         SR    RE,RE                                                            
         USING TACOD,R2                                                         
         L     R2,TCATACO                                                       
         CLI   TACOCTYP,CCTY04A    IF 2404A,                                    
         BE    TMNPD10                                                          
         CLC   TCPCYCS,=X'B40825'  OR IF CYCLE STARTS BEFORE YEAR 1             
         BNL   TMNPD20                                                          
         DROP  R2                                                               
                                                                                
TMNPD10  D     RE,=F'4'            GET 25% OF DOUBLETIME HOURS                  
         B     TMNPD30                                                          
TMNPD20  D     RE,=F'5'            GET 20% OF STRAIGHT - NEW RULE               
*                                                                               
*                                  RF = TOTAL # OF ADJUSTED HOURS               
TMNPD30  M     RE,TCGROSS          * HOURLY RATE                                
         D     RE,=F'10000'        DIVIDE BY 100                                
         LTR   RE,RE               ROUND UP IF ANY REMAINDER                    
         BZ    *+8                                                              
         AHI   RF,1                                                             
*                                                                               
         AR    R1,RF               ADD TO STRAIGHT AND OT HOURS                 
*                                                                               
TMNPDX   XIT1  REGS=(R1)                                                        
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*              TIMESHEET - NIGHT PREMIUM - 2404A SAG HOURS                      
*              ADDS OVERTIME AND DOUBLETIME NP HOURS TOGETHER                   
*              ADDS STRAIGHT TIME NP HOURS IF NOT FIRST DAY                     
         SPACE 1                                                                
         USING TASDD,R2                                                         
TIMENPA  NTR1  BASE=*,LABEL=*                                                   
         SR    R1,R1                                                            
         L     R4,TCATMTOT         SUBTOTAL ELEMENT                             
         USING TATTD,R4                                                         
         B     TMNPA05                                                          
*                                                                               
TMNPA02  ZIC   R5,TATTLEN                                                       
         AR    R4,R5               BUMP TO NEXT SUBTOTAL ELEMENT                
         CLI   0(R4),TATTELQ       MORE SUBTOTAL ELEMENTS?                      
         BNE   TMNPAX                                                           
TMNPA05  ZICM  RF,TATTNPOT,2       # OF NIGHT PREMIUM OT HOURS                  
*                                                                               
         CLC   TATTDATE,=X'FFFFF9' TOTAL OF ACTRA/SAG DAYS (1ST DAY)            
         BNE   TMNPA10                                                          
         TM    TATTSTAT,TATTSART+TATTSSRT   ACTRA/SAG RATES?                    
         BNO   TMNPA02                                                          
         LR    R0,RF                                                            
         SR    RE,RE               CALCULATE 1.5 (TIME AND A HALF)              
         D     RE,=F'2'                                                         
         AR    RF,R0                                                            
         B     TMNPA20             IF 1ST DAY, STRAIGHT HRS ARE ACTRA           
*                                                                               
TMNPA10  CLC   TATTDATE,=X'FFFFFA' TOTAL OF ALL SAG DAYS                        
         BNE   TMNPA02                                                          
         LR    R0,RF                                                            
         SR    RE,RE               CALCULATE TIME AND A HALF                    
         D     RE,=F'2'                                                         
         AR    RF,R0                                                            
         ZICM  R0,TATTNPST,2       # OF NP STRAIGHT HOURS                       
         AR    RF,R0               ADD OT WITH STRAIGHT                         
*                                                                               
TMNPA20  ZICM  R0,TATTNPDT,2       # OF NIGHT PREMIUM DT HOURS                  
         MHI   R0,2                CALCULATE DOUBLETIME                         
         AR    RF,R0               ADD OT + STRAIGHT WITH DT                    
         MHI   RF,100                                                           
         SR    RE,RE                                                            
         D     RE,=F'4'            GET 25% OF SAG HOURS                         
*                                                                               
*                                  RF = TOTAL # OF ADJUSTED HOURS               
         M     RE,TASDHR           * HOURLY RATE                                
         D     RE,=F'10000'        DIVIDE BY 100                                
         LTR   RE,RE               ROUND UP IF ANY REMAINDER                    
         BZ    *+8                                                              
         AHI   RF,1                                                             
*                                                                               
         AR    R1,RF                                                            
         B     TMNPA02                                                          
*                                                                               
TMNPAX   XIT1  REGS=(R1)                                                        
         DROP  R2,R4                                                            
         LTORG                                                                  
         EJECT                                                                  
*              TIMESHEET - MEAL PENALTY                                         
*              NOT SUBJECT TO PNH                                               
         SPACE 1                                                                
TIMEMP   NTR1  BASE=*,LABEL=*                                                   
         SR    R1,R1                                                            
*        OC    TCPAY,TCPAY         IF CAST RECORD HAS PAYMENT OVERRIDE,         
*        BNZ   TMMPX               DO NOT DEDUCT MEAL PENALTY FROM P&H          
         L     R4,TCATMTOT         SUBTOTAL ELEMENT                             
         USING TATTD,R4                                                         
         B     TMMP05                                                           
*                                                                               
TMMP02   ZIC   R5,TATTLEN                                                       
         AR    R4,R5               BUMP TO NEXT SUBTOTAL ELEMENT                
         CLI   0(R4),TATTELQ       MORE SUBTOTAL ELEMENTS?                      
         BNE   TMMPX                                                            
*                                                                               
TMMP05   CLC   TATTDATE,=X'FFFFFF' TOTAL ELEMENT                                
         BNE   TMMP02                                                           
         ZICM  R1,TATTNSPH,4       NOT SUBJ TO PNH - ADD TO PYMT AMNT           
         MVC   TCNOTSPH,TATTNSPH   SAVE AS NOT SUBJ TO PNH AMOUNT               
*                                                                               
         LTR   R1,R1               SAVE IT FOR BREAKDOWN                        
         BZ    TMMPX                                                            
         ST    R1,WORK                                                          
         GOTOR SVBRKDWN,DMCB,('SBDADJST',WORK),(59,0)                           
         L     R1,WORK                                                          
*                                                                               
TMMPX    XIT1  REGS=(R1)                                                        
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*              TIMESHEET - ADDITIONAL PAYMENT AMOUNT                            
*              SUBJECT TO PNH (SMOKE PAY)                                       
         SPACE 1                                                                
TIMEAP   NTR1  BASE=*,LABEL=*                                                   
         SR    R1,R1                                                            
         L     R4,TCATMTOT         SUBTOTAL ELEMENT                             
         USING TATTD,R4                                                         
         B     TMAP05                                                           
*                                                                               
TMAP02   ZIC   R5,TATTLEN                                                       
         AR    R4,R5               BUMP TO NEXT SUBTOTAL ELEMENT                
         CLI   0(R4),TATTELQ       MORE SUBTOTAL ELEMENTS?                      
         BNE   TMAPX                                                            
*                                                                               
TMAP05   CLC   TATTDATE,=X'FFFFFF' TOTAL ELEMENT                                
         BNE   TMAP02                                                           
         ZICM  R1,TATTPYMT,4       TOTAL ADD TO PAYMENT AMT (SMOKE PAY)         
*                                                                               
TMAPX    XIT1  REGS=(R1)                                                        
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*              TIMESHEET - ADJUSTMENT AMOUNT                                    
*              SUBJECT TO PNH, GETS ADDED TO PYMT AMOUNT                        
*              DOES NOT GET MULTIPLIED BY OVERSCALE %                           
         SPACE 1                                                                
TIMEAJ   NTR1  BASE=*,LABEL=*                                                   
         SR    R1,R1                                                            
         L     R4,TCATMTOT         SUBTOTAL ELEMENT                             
         USING TATTD,R4                                                         
         B     TMAJ05                                                           
*                                                                               
TMAJ02   ZIC   R5,TATTLEN                                                       
         AR    R4,R5               BUMP TO NEXT SUBTOTAL ELEMENT                
         CLI   0(R4),TATTELQ       MORE SUBTOTAL ELEMENTS?                      
         BNE   TMAJX                                                            
*                                                                               
TMAJ05   CLC   TATTDATE,=X'FFFFFF' TOTAL ELEMENT                                
         BNE   TMAJ02                                                           
         ZICM  R1,TATTADJ,4        TOTAL ADJUSTMENT AMOUNT                      
*                                                                               
TMAJX    XIT1  REGS=(R1)                                                        
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*              TIMESHEET - FRIDAY TO SATURDAY HOURS (EXTRAS ONLY)               
*              SUBJECT TO PNH, GETS ADDED TO PYMT AMOUNT                        
         SPACE 1                                                                
TIMEFS   NTR1  BASE=*,LABEL=*                                                   
         ST    R1,TCTMFULL         TCTMFULL = RATE                              
         SR    R1,R1                                                            
*                                                                               
         USING TACOD,R2                                                         
         L     R2,TCATACO          IF COMMERCIAL HAS ANY ACTRA                  
         CLI   TACOCTYP,0          TYPE EXCEPT FOR 2404B,                       
         BE    TMFS01              NO FRI TO SAT HOURS CALCULATED               
         CLI   TACOCTYP,CCTY04B                                                 
         BNE   TMFSX                                                            
         DROP  R2                                                               
*                                                                               
TMFS01   TM    TGCATYPE,EXTRA      EXTRAS ONLY                                  
         BZ    TMFSX                                                            
*                                                                               
         L     R4,TCATMTOT         SUBTOTAL ELEMENT                             
         USING TATTD,R4                                                         
         B     TMFS05                                                           
*                                                                               
TMFS02   ZIC   R5,TATTLEN                                                       
         AR    R4,R5               BUMP TO NEXT SUBTOTAL ELEMENT                
         CLI   0(R4),TATTELQ       MORE SUBTOTAL ELEMENTS?                      
         BNE   TMFSX                                                            
*                                                                               
TMFS05   CLC   TATTDATE,=X'FFFFFF' TOTAL ELEMENT                                
         BNE   TMFS02                                                           
         ZICM  R1,TATTXSAT,2       TOTAL NUMBER OF SAT. HOURS                   
         BZ    TMFS08                                                           
*                                                                               
         LR    R5,R1               SAVE IT BEFORE BREAK DOWN                    
         MVC   WORK(4),TCTMFULL                                                 
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         STCM  R1,15,WORK+4                                                     
         GOTOR SVBRKDWN,DMCB,(0,WORK),(63,WORK+4)                               
         LR    R1,R5               RESTORE IT                                   
         SR    R0,R0                                                            
*                                                                               
TMFS08   M     R0,TCTMFULL         MULTIPLY BY HOURLY RATE                      
         D     R0,=F'100'          DIVIDE BY 100                                
TMFSX    XIT1  REGS=(R1)                                                        
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*              TIMESHEET - REST PERIOD VIOLATION AMOUNT                         
*              NOT SUBJECT TO PNH, GETS ADDED TO PYMT AMOUNT                    
*              DOES NOT GET MULTIPLIED BY OVERSCALE %                           
         SPACE 1                                                                
TIMERPV  NTR1  BASE=*,LABEL=*                                                   
         ST    R1,TCTMFULL         TCTMFULL = SESSION FEE                       
         SR    R1,R1                                                            
         L     R4,TCATMTOT         SUBTOTAL ELEMENT                             
         USING TATTD,R4                                                         
         B     TMRPV05                                                          
*                                                                               
TMRPV02  ZIC   R5,TATTLEN                                                       
         AR    R4,R5               BUMP TO NEXT SUBTOTAL ELEMENT                
         CLI   0(R4),TATTELQ       MORE SUBTOTAL ELEMENTS?                      
         BNE   TMRPVX                                                           
*                                                                               
TMRPV05  CLC   TATTDATE,=X'FFFFFF' TOTAL ELEMENT                                
         BNE   TMRPV02                                                          
         ZIC   R1,TATTRPVL         TOTAL NUMBER OF REST PER VIOL.               
         LR    R3,R1               SAVE NUMBER OF REST PER VIOL.                
         L     RE,=F'50000'                                                     
         CLC   TCTMFULL,=F'50000'  PAY LESSER OF SESSION FEE OR $500            
         BH    *+8                                                              
         L     RE,TCTMFULL                                                      
         MR    R0,RE               MULTIPLY BY SESSION FEE OR $500              
*                                                                               
TMRPVX   XIT1  REGS=(R1,R3)                                                     
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              CONVERT HOURS TO MINUTES                                         
*--------------------------------------------------------------------*          
HRS2MIN  NTR1  BASE=*,LABEL=*                                                   
         LH    RF,HALF                                                          
         SR    RE,RE                                                            
         D     RE,=F'100'                                                       
         LR    R1,RE               NUMBER OF MINUTES                            
         MHI   RF,60                                                            
         AR    RF,R1                                                            
         STH   RF,HALF                                                          
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*              ADJUSTMENT ROUTINE FOR INDUSTRIALS (OFF CAMERA)                  
         SPACE 1                                                                
ADJIDS   NTR1  BASE=*,LABEL=*                                                   
         CLI   TGCAEQU,CTS         SOLO = ROW 3                                 
         JE    ADJIDS1                                                          
         CLI   TGCAEQU,CTD         DUO  = ROW 3                                 
         JNE   ADJIDS3                                                          
ADJIDS1  MVI   TCROW,3                                                          
         J     XIT                                                              
                                                                                
ADJIDS3  CLI   TGCAEQU,CTC3        GROUP CONTRACTOR = ROW 6                     
         JL    ADJIDS5                                                          
         CLI   TGCAEQU,CTC9                                                     
         JH    ADJIDS5                                                          
         MVI   TCROW,6             SAME RATE AS GROUP SINGER X 1.5              
         J     XIT                                                              
                                                                                
ADJIDS5  CLI   TGCAEQU,CTGS        GROUP SINGER = ROW 6                         
         JE    ADJIDS6                                                          
         CLI   TGCAEQU,CTGS3                                                    
         JL    ADJIDS5B                                                         
         CLI   TGCAEQU,CTGS9                                                    
         JH    ADJIDS5B                                                         
ADJIDS5B CLI   TGCAEQU,CTG3                                                     
         JL    XIT                                                              
         CLI   TGCAEQU,CTG9                                                     
         JH    XIT                                                              
ADJIDS6  MVI   TCROW,6                                                          
         J     XIT                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO ADJUST HOURLY TRAVEL RATE FOR SOLOISTS                
         SPACE 1                                                                
*                                  R1 RETURNS ADJUSTED RATE                     
*                                                                               
ADJTRV   NTR1  BASE=*,LABEL=*                                                   
         ZIC   R1,TCROW            HOLD CURRENT TCROW                           
         MVI   TCROW,1             FORCE ON CAMERA CATEGORY                     
         L     R3,TCGROSS          HOLD CURRENT TCGROSS                         
         XC    TCGROSS,TCGROSS     CLEAR TCGROSS                                
         BRAS  RE,GETRATE          GET ON CAMERA RATE                           
         L     R0,TCGROSS                                                       
         ST    R3,TCGROSS          RESTORE ORIGINAL TCGROSS                     
         STC   R1,TCROW            RESTORE ORIGINAL TCROW                       
*                                                                               
         XR    R1,R1               HOURLY RATE = GROSS DIVIDED BY 8             
         SRDA  R0,31                                                            
         D     R0,=F'8'                                                         
         AHI   R1,1                AND ROUNDED                                  
         SRA   R1,1                                                             
         XIT1  REGS=(R1)           RETURN R1                                    
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        CALCULATE RATES FOR EVENT PAYMENT                                      
***********************************************************************         
                                                                                
EVECALC  NTR1  BASE=*,LABEL=*                                                   
         CLI   TGUSEQU,UEVE        IF MAKING EVE PAYMENT                        
         JNE   NO                                                               
                                                                                
         USING TLTMD,R3                                                         
         LA    R3,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   TLTMCD,TLTMCDQ                                                   
         MVI   TLTMSTA,TLTMSPPL                                                 
         MVC   TLTMCOM,TGCOM                                                    
         MVC   TLTMINV,TGINV                                                    
         MVC   TLTMSSN,TGSSN                                                    
         MVC   TLTMSORT+4(L'TLCASEQ),TGCSORT+4                                  
         GOTO1 HIGH                                                             
         CLC   TLTMKEY,KEYSAVE                                                  
         JE    *+6                                                              
         DC    H'00'                                                            
         DROP  R3                                                               
                                                                                
         XC    TCPAY,TCPAY                                                      
                                                                                
         L     R0,AIO                                                           
         MVC   AIO,TCAETREC                                                     
         GOTO1 GETREC                                                           
         ST    R0,AIO                                                           
                                                                                
         L     R4,TCAETREC                                                      
         MVI   ELCODE,TATDELQ                                                   
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
EC10     BRAS  RE,NEXTEL                                                        
         JNE   YES                                                              
                                                                                
         USING TATDD,R5                                                         
         LR    R5,R4                                                            
         NI    TATDSTAT,X'FF'-TATDSTAX-TATDSWAG-TATDSSTX                        
                                                                                
         GOTO1 RECVAL,DMCB,TLPMCDQ,(X'A4',TATDPMTY),('TLPMSCDQ',0)              
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         USING TAYDD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAYDELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         ICM   RF,15,TATDAMNT                                                   
                                                                                
         TM    TAYDSTAT,TAYDSWAG                                                
         JZ    EC20                                                             
         L     RE,TCGROSS                                                       
         AR    RE,RF                                                            
         ST    RE,TCGROSS                                                       
         ST    RE,TCPAY                                                         
         OI    TATDSTAT,TATDSWAG                                                
         J     EC30                                                             
                                                                                
EC20     L     RE,TCEXP                                                         
         AR    RE,RF                                                            
         ST    RE,TCEXP                                                         
         TM    TAYDSTAT,TAYDSTAX                                                
         JZ    EC30                                                             
         OI    TATDSTAT,TATDSSTX                                                
         CLI   TCW4TYPE,TAW4TYCO                                                
         JE    EC30                                                             
         CLI   TCW4TYPE,TAW4TYCA                                                
         JE    EC30                                                             
         CLI   TCW4TYPE,TAW4TYTR                                                
         JE    EC30                                                             
         CLI   TCW4TYPE,TAW4TYFO                                                
         JE    EC30                                                             
         L     RE,TCTXNW                                                        
         AR    RE,RF                                                            
         ST    RE,TCTXNW                                                        
         OI    TATDSTAT,TATDSTAX                                                
         DROP  R4,R5                                                            
                                                                                
EC30     MVI   ELCODE,TATDELQ                                                   
         LR    R4,R5                                                            
         J     EC10                                                             
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
CALCD    DSECT                                                                  
       ++INCLUDE TASYSCALCD                                                     
         EJECT                                                                  
*&&DO                                                                           
*              DSECT TO COVER YEARTAB                                           
         SPACE 1                                                                
YEARD    DSECT                                                                  
YEARUN   DS    XL1                 UNION(S)                                     
         ORG   YEARUN                                                           
YEARUN1  DS    XL1                 UNION(S)                                     
YEARUN2  DS    XL1                                                              
YEARUN3  DS    XL1                                                              
YEARUN4  DS    XL1                                                              
YEARYR   DS    XL1                 CONTRACT YEAR(S)                             
YEARPHAS DS    XL1                 CONTRACT RATE PHASE NUMBER                   
YEARFHNW DS    XL2                 FIXED H&W AMOUNT                             
YEARPNHT DS    XL2                 P&H RATE FOR TV                              
YEARPNHR DS    XL2                 P&H RATE FOR RADIO                           
YEARPHMT DS    XL2                 AFM P&H RATE FOR TV                          
YEARPHMR DS    XL2                 AFM P&H RATE FOR RADIO                       
YEARNEXT EQU   *                                                                
         SPACE 3                                                                
*&&                                                                             
*              DSECT TO COVER COLATAB                                           
         SPACE 1                                                                
COLAD    DSECT                                                                  
COLAUN   DS    XL1                 UNION(S)                                     
         ORG   COLAUN                                                           
COLAUN1  DS    XL1                 UNION(S)                                     
COLAUN2  DS    XL1                                                              
COLAUN3  DS    XL1                                                              
COLAUN4  DS    XL1                                                              
COLAYEAR DS    XL1                 CONTRACT YEAR(S)                             
COLAFRST DS    PL3                 EARLIEST FIRST SERVICES DATE                 
COLALAST DS    PL3                 LATEST FIRST SERVICES DATE                   
COLASTAT DS    XL1                 STATUS BYTE                                  
*                                  X'80'=SPECIAL ROUTINE FOR 75, 77             
COLAREG  DS    XL2                 APPLICABLE COST OF LIVING INCREASE           
COLAEXT  DS    XL2                 EXTRAS COST OF LIVING INCREASE               
COLANEXT EQU   *                                                                
         SPACE 3                                                                
*              DSECT TO COVER ONE ENTRY OF TAGFEE TABLE                         
         SPACE 1                                                                
TAGFEED  DSECT                                                                  
TAGFON   DS    F                   TV SESSION - ON CAMERA                       
TAGFOFF  DS    F                                OFF                             
TAGFRAD  DS    F                   RADIO SESSION                                
TAGFNEXT EQU   *                                                                
TAGFLNQ  EQU   *-TAGFEED                                                        
         EJECT                                                                  
*              DSECT TO COVER INDEXT TABLE                                      
         SPACE 1                                                                
TNDXD    DSECT                                                                  
TNDXOFH1 DS    F                   OFF-CAM SESS 1/2 ADDT'L CAT 1                
TNDXOFH2 DS    F                   OFF-CAM SESS 1/2 ADDT'L CAT 2                
TNDXPCL  DS    F                   DAY PERFORMER CEILING                        
TNDXPOT  DS    F                      OVERTIME RATE                             
TNDXPDT  DS    F                      DOUBLE TIME RATE                          
TNDXP3CL DS    F                   3-DAY PERFORMER CEILING                      
TNDXP3OT DS    F                      OVERTIME RATE                             
TNDXP3DT DS    F                      DOUBLE TIME RATE                          
TNDXPWCL DS    F                   WEEKLY PERFORMER CEILING                     
TNDXPWOT DS    F                      OVERTIME RATE                             
TNDXPWDT DS    F                      DOUBLE TIME RATE                          
TNDXNRD1 DS    F                   NARR ADDT'L DAY RATE CAT 1                   
TNDXNRD2 DS    F                   NARR ADDT'L DAY RATE CAT 2                   
TNDXX301 DS    F                   ENTIRE  SCRIPT ADDT'L 1/2 HOUR CAT1          
TNDXX302 DS    F                   ENTIRE  SCRIPT ADDT'L 1/2 HOUR CAT2          
TNDX1301 DS    F                   PARTIAL SCRIPT FIRST  1/2 HOUR CAT1          
TNDX1302 DS    F                   PARTIAL SCRIPT FIRST  1/2 HOUR CAT2          
TNDXPX1  DS    F                   PRINCIPAL RATE EXTRA  1/2 HOUR CAT1          
TNDXPX2  DS    F                   PRINCIPAL RATE EXTRA  1/2 HOUR CAT2          
TNDXNPX1 DS    F                   NON PRINCIPAL  EXTRA  1/2 HOUR CAT1          
TNDXNPX2 DS    F                   NON PRINCIPAL  EXTRA  1/2 HOUR CAT2          
TNDXRTK1 DS    F                   PRINCIPAL RETAKE      1/2 HOUR CAT1          
TNDXRTK2 DS    F                   PRINCIPAL RETAKE      1/2 HOUR CAT2          
TNDXSTP1 DS    F                   STEP OUT PREMIUM               CAT1          
TNDXSTP2 DS    F                   STEP OUT PREMIUM               CAT2          
TNDXP3M1 DS    F                   P3M FIRST 1/2 HR RATE          CAT1          
TNDXP3M2 DS    F                   P3M FIRST 1/2 HR RATE          CAT2          
TNDXPHD1 DS    F                   PHD DAILY RATE                 CAT1          
TNDXPHD2 DS    F                   P3M DAILY RATE                 CAT2          
TNDXIVH1 DS    F                   IVR ADD'L 1/2 HR RATE          CAT1          
TNDXIVH2 DS    F                   IVR ADD'L 1/2 HR RATE          CAT2          
TNDXLNQ  EQU   *-TNDXD                                                          
         EJECT                                                                  
*              DSECT TO COVER USELUT ENTRY                                      
         SPACE 1                                                                
USELUTD  DSECT                                                                  
USELNUM  DS    XL2                 USE NUMBER                                   
USELCDE  DS    X                   USE CODE EQU                                 
USELTYPE DS    X                   USE TYPE                                     
USELUNI  DS    X                   VALID UNIONS                                 
         ORG   USELUNI                                                          
USELUNI1 DS    X                   VALID UNIONS                                 
USELUNI2 DS    X                                                                
USELUNI3 DS    X                                                                
USELUNI4 DS    X                                                                
USELMED  DS    X                   VALID MEDIA                                  
USELLNQ  EQU   *-USELUTD                                                        
         SPACE 3                                                                
*              DSECT TO COVER USETBLS HEADER                                    
         SPACE 1                                                                
USETBLD  DSECT                                                                  
USETBNUM DS    XL2                 USE NUMBER                                   
USETBLN  DS    XL2                 ENTRY LENGTH                                 
USETBST  DS    XL2                 START USE/UNIT NUM                           
USETBEND DS    XL2                 END USE/UNIT NUM                             
USETBYNM DS    X                   YEAR NUM (CABLE ONLY, FOR NOW)               
         DS    XL3                 SPARE                                        
*                                  OLDER THAN 97                                
         ORG   USETBLN                                                          
USETBLNO DS    X                   ENTRY LENGTH                                 
USETBSTO DS    X                   START USE/UNIT NUM                           
USETBNDO DS    X                   END USE/UNIT NUM                             
         EJECT                                                                  
*              DSECT TO COVER TABLE OF ECAST RECORDS IN TSAR FOR SOR            
****** IF CHANGE ECASTABD OR EPISD, MUST ALSO CHANGE IN TAGENPAYD *****         
****** MAKE SURE IT FITS IN TSARAREA AND BLOCK *****                            
         SPACE 1                                                                
ECASTABD DSECT                                                                  
ECSTSORT DS    XL1                 CAST SORT                                    
ECSTSSN  DS    CL9                 SSN                                          
ECSTCAT  DS    CL3                 CATEGORY                                     
ECSTSTAT DS    XL1                 CAST STATUS                                  
ECSTS2ND EQU   X'80'               CAST FROM 2ND COMM'L (FOR AFT CABLE)         
ECSTKLNQ EQU   *-ECASTABD          LENGTH OF KEY                                
*                                                                               
ECSTNEPI DS    XL1                    NUMBER OF EPISODES                        
ECSTEPIS DS    (MAXEPIS)XL(EPINUMLN)  EPISODE INFO                              
MAXEPIS  EQU   30                     MAXIMUM NUMBER OF EPISODES                
ECSTRLNQ EQU   *-ECASTABD                                                       
         SPACE 2                                                                
*              DSECT TO COVER ECSTEPIS                                          
EPISD    DSECT                                                                  
EPINUM   DS    H                   EPISODE NUMBER (IN BINARY)                   
EPISTAT  DS    XL1                 STATUS                                       
EPISTBAL EQU   X'80'               CREDIT BALANCE LEFT                          
EPISTHW  EQU   X'40'               HEAD WRITER ROLE                             
EPISTSW  EQU   X'20'               SCRIPT WRITER ROLE                           
EPISTBW  EQU   X'10'               BREAKDOWN WRITER ROLE                        
EPIAPPL  DS    XL3                 APPLIED CREDITS                              
EPIPNH   DS    H                   P&H AMOUNT                                   
EPIPAY   DS    XL3                 PAYMENT AMOUNT                               
EPIBAL   DS    XL3                 CREDIT BALANCE                               
EPINUMLN EQU   *-EPISD                                                          
         ORG   EPIAPPL                                                          
EPIWPCT  DS    XL3                 WRITER ROLE PERCENTAGE                       
EPIPENS  DS    H                   WGA PENSION                                  
         ORG   EPIBAL                                                           
         DS    X                                                                
EPIHLTH  DS    H                   WGA HEALTH                                   
         ORG   EPIBAL                                                           
         DS    X                                                                
EPIMDED  DS    H                   DGA MISC. DED                                
*                                                                               
         ORG   EPISTAT             JUST IN FIRST TSAR REC TEMPORARILY:          
EPINCON  DS    X                   # OF CONTRACT AND F/C'S FOR CABLE            
EPINU5   DS    X                   # OF U/5'S FOR CABLE                         
         EJECT                                                                  
*              DSECT TO COVER AIR DATE DISPLACEMENTS TABLE                      
         SPACE 1                                                                
AIRDTABD DSECT                                                                  
AIRDSTRT DS    XL3                 START OF AIR DATE RANGE                      
AIRDEND  DS    XL3                 END OF AIR DATE RANGE                        
AIRDDISP DS    XL1                 DISPLACEMENT INTO LOOK UP TABLE              
AIRDNEXT EQU   *                                                                
         SPACE 3                                                                
*              DSECT TO COVER P&H TABLES FOR SOAP RESIDUALS                     
         SPACE 1                                                                
PNHTABD  DSECT                                                                  
PNHTSTRT DS    XL3                 START OF AIR DATE RANGE                      
PNHTEND  DS    XL3                 END OF AIR DATE RANGE                        
PNHTPNH  DS    XL2                 P&H RATE                                     
PNHTNEXT EQU   *                                                                
         SPACE 3                                                                
*              DSECT TO COVER P&H TABLES FOR WRITERS FOR SOAP RESIDUALS         
         SPACE 1                                                                
WPNHTABD DSECT                                                                  
WPNHTST  DS   XL3                 START OF AIR DATE RANGE                       
WPNHTEND DS   XL3                 END OF AIR DATE RANGE                         
WPNHTPEN DS   XL2                 PENSION RATE                                  
WPNHTHLT DS   XL2                 HEALTH RATE                                   
WPNHTNXT EQU  *                                                                 
         SPACE 3                                                                
*              DSECT TO COVER EPISAIR TABLE                                     
****** IF CHANGE EPISATBD, MUST ALSO CHANGE IN TAGENPAYD *****                  
         SPACE 1                                                                
EPISATBD DSECT                                                                  
EPATEPI  DS    XL2                 EPISODE NUMBER                               
EPATAIR  DS    XL3                 AIR DATE PWOS                                
EPATNCON DS    X                   # OF CONTRACT AND F/C'S FOR CABLE            
EPATNU5  DS    X                   # OF U/5'S FOR CABLE                         
EPATNXT  EQU   *                                                                
         EJECT                                                                  
*              DSECT TO COVER DLRAPHLD ROUTINE'S WORK AREA                      
         SPACE 1                                                                
DAHD     DSECT                                                                  
DAHAPPDT DS    XL3                 APPLICABLE DATE                              
DAHCYCS  DS    XL3                 NEXT CYCLE START DATE                        
DAHCYCE  DS    XL3                 NEXT CYCLE END DATE                          
DAHWORK  DS    CL17                WORK AREA                                    
         EJECT                                                                  
*              DSECT TO COVER PREPITN ROUTINE'S WORK AREA                       
         SPACE 1                                                                
ITNBLKD  DSECT                                                                  
ITNSAVS  DS    XL1                                                              
ITNVSTA  DS    XL1                                                              
ITNPIKY  DS    XL32                                                             
ITNAPRG  DS    XL1                                                              
ITNCINV  DS    XL6                                                              
ITNINKY  DS    XL32                                                             
ITNIAGY  DS    XL6                                                              
ITNVERS  DS    XL251                                                            
         EJECT                                                                  
* TASYSVALD                                                                     
         PRINT OFF                                                              
SYSCOMMD DSECT                                                                  
       ++INCLUDE TASYSVALD                                                      
         PRINT ON                                                               
* TASYSWORKD                                                                    
         PRINT OFF                                                              
SYSWORKD DSECT                                                                  
       ++INCLUDE TASYSWORKD                                                     
         PRINT ON                                                               
* TAGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE TAGENFILE                                                      
         PRINT ON                                                               
* TAPAYBRKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE TAPAYBRKD                                                      
         PRINT ON                                                               
* TASYSEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE TASYSEQUS                                                      
         PRINT ON                                                               
* TASYSDSECT                                                                    
         PRINT OFF                                                              
       ++INCLUDE TASYSDSECT                                                     
         PRINT ON                                                               
* TAGENEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE TAGENEQUS                                                      
         PRINT ON                                                               
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
* DDPERVALD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* DDCOREQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010TASYSCALC 03/01/17'                                      
         END                                                                    
