*          DATA SET SPOTGOAL   AT LEVEL 017 AS OF 11/08/19                      
*PHASE T00A51B,*                                                                
         SPACE 1                                                                
***********************************************************************         
* SPOTGOAL - T00A51                                                   *         
*                                                                     *         
* INPUT  : P1=A(SPOT BLOCK)                                           *         
*          SBAIO1 = A(GOAL RECORD)                                    *         
*          SBAIO2 = A(CPP GUIDE)                                      *         
*          SBAIO3 = A(2ND CPP GUIDE IF NEEDED)                        *         
*        : P2 BYTE 0 = X'FE' --> PASSING PW% FOUND IN EST HEADER      *         
*                      X'FD' --> VALUE IS COST2 FACTOR (NOT PW %)     *         
*              " 1-3 = A(4-BYTE PW% FROM EST HDR)                     *         
*                                                                     *         
* OUTPUT : P1 BYTE 0 = 0 IF OK                                        *         
*                   NE 0 IF ERROR                                     *         
*                                                                     *         
* BUILD A TABLE OF CHUNKS POINTED TO BY SBACHUNK AND                  *         
* COVERED BY SGLCHNKD.                                                *         
* EXTRACT VALUES SECTION OF SPOTBLOCK (SBEVALS) CONTROLS GOAL EXTRACT.*         
* CALLING PROGRAM CAN USE CHUNKS TO BUILD SORT RECORDS FOR REPORTING. *         
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
* SPOTGOAL - T00A51   MOD LOG                                         *         
*                                                                     *         
*---------------------------------------------------------------------*         
* USER     JIRA       DATE                 CHANGE LOG                 *         
* ---- ------------ -------- ---------------------------------------- *         
* AKAT SPEC-33500   11/08/19 SUPPORT FOR 2 DECIMAL IMPRESSIONS        *         
* AKAT SPEC-29753   11/14/18 FIX 2-DECIMAL BUG FOR ORDEM (PURCH LOCK) *         
* AKAT SPEC-20862   10/31/18 FULL 2-DECIMAL DEMO SUPPORT              *         
* AKAT SPEC-12212   02/16/18 2-DECIMAL DEMO SUPPORT                   *         
*---------------------------------------------------------------------*         
* 08DEC06 13 AKAT - SGLEDEM 2 DECIMAL DEMOS BUG                       *         
* 13SEP06 12 AKAT - SUPPORT NEW GOAL LOCKIN FIELDS (X'A1' ELEMENT)    *         
* 13JUN06 11 AKAT - SUPPORT FOR NEW SPOT LENGTHS                      *         
* 29JUN05 10 AKAT - POINT TO START OF CHUNK FOR 2 DECIMAL DEMO SUPPORT*         
* 31MAR05  9 AKAT - 2 DECIMAL DEMOS SUPPORT                           *         
* 05MAR03  8 EJOR - USE SPSLNTAB                                      *         
* 06AUG01  6 EJOR - CHANGE SLNTAB                                     *         
* 01MAR01  5 EJOR - FIX C2 DIVIDE BY ZERO!                            *         
* 10JAN01  4 EJOR - NEW CHUNK SIZE FOR COST2                          *         
* 20SEP00  3 WHOA - MODIFY THE WIM GOALS NOT THE CLT GOALS            *         
* 06SEP00  2 MHER/EJOR - PROCESS COST2 FACTORS                        *         
*---------------------------------------------------------------------*         
* 16MAR98 46 MHER/EJOR - GET TAX FROM PWCALC INSTEAD OF CALCULATING   *         
* 26DEC95 45 EJOR - INCREASE CPP TABLE                                *         
* 09NOV95 44 MHER - IGNORE PRD2 IF IT'S NOT REALLY A PRD              *         
* 06APR95 42 MHER -  TO SUPPRESS TAX OR NOT                           *         
* 06APR95 41 MHER - CHANGE GOAL CALCULATIONS TO HAVE OPTION           *         
* 05APR95 40 GLEE - OPTION TO CALC GOAL W/ OR W/O TAX                 *         
* 14DEC94 39 EFJ -- GET PW DATA FROM AREA PASSED BY APPL (NO READ)    *         
* 11NOV94 38 GLEE - CHANGED GETPW CODE TO NOT DIE WHEN NO PW RECD     *         
* 01NOV94 37 EFJ -- MH, NOT M FOR HALFWORD                            *         
*                -- DIVIDE BY 100000, NOT 100 FOR TAX                 *         
* 27OCT94 36 GLEE - PICK UP PW% FROM 2ND PARM                         *         
*                                                                     *         
***********************************************************************         
         TITLE 'T00A51 - SPOTPAK GOAL EXTRACT MODULE'                           
SPOTGOAL CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,**SPGOAL,RA,RR=RE,CLEAR=YES                          
         USING WORKD,RC            RC = A(LOCAL WORKING STORAGE)                
         ST    R1,APARM            SAVE A(PARM LIST)                            
         MVC   PARMS,0(R1)         SAVE PARMS TOO !                             
         L     R8,0(R1)                                                         
         USING SBLOCKD,R8          R8 = A(SPOTBLOCK)                            
         L     R9,SBCOMFAC         R9 = A(COMFACS)                              
         USING COMFACSD,R9                                                      
         L     R6,SBAIO1           R6 = A(GOAL RECORD)                          
         USING GOALRECD,R6                                                      
         L     R7,SBACHUNK         R7 = A(CHUNK)                                
         USING SGLCHNKD,R7                                                      
         XC    SGNEXT,SGNEXT       CLEAR START OF CHUNK                         
         ST    RE,RELO                                                          
         MVC   USERRD,4(RD)        SAVE LINK BACK TO USER                       
         MVI   0(R1),0             SET NORMAL COMPLETION                        
*                                                                               
         MVC   VCALLOV,CCALLOV     SET ADDRESSES FROM COMFACS                   
*                                                                               
         GOTO1 VCALLOV,DMCB,0,X'D9000A79'                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VPWCALC,0(R1)                                                    
*                                                                               
         MVI   XFF,X'FF'                                                        
         MVC   XFF+1(L'XFF-1),XFF                                               
         LA    RE,CPPTAB           CLEAR CPP TABLE                              
         LA    RF,CPPTABL                                                       
         XCEF                                                                   
*                                                                               
         MVC   PRD1,GKEYPRD        SET PRODUCT(S)                               
         TM    GKEYAGY,X'40'       TEST PRD2 NOT A PRODUCT                      
         BO    *+10                                                             
         MVC   PRD2,GKEYPRD2                                                    
         MVC   DPT,GKEYDPT             DAYPART                                  
         MVC   SLN1,GKEYSLN            SPOT LENGTH(S)                           
         MVI   SLN2,0                                                           
         MVC   SLNT,GKEYSEC                                                     
         CLI   PRD2,0              TEST PIGGYBACKS                              
         BE    SG1                                                              
         ZIC   RF,SLNT             YES - CALCULATE SECOND SPOT LENGTH           
         ZIC   RE,SLN1                                                          
         SR    RF,RE                                                            
         STC   RF,SLN2                                                          
*                                                                               
SG1      BAS   RE,EQTAB            FIND EQUIVALENCE TABLE                       
         MVI   SPLIT,0                                                          
         CLC   SBQPRD,=C'POL'      TEST POOL REQUEST                            
         BNE   SG8                                                              
         CLI   PRD2,0              YES-TEST 2ND PROD                            
         BE    SG2                                                              
         CLI   SBESPLIT,N          YES-TEST SPLIT PIGGYBACKS                    
         BE    SG2                                                              
         MVI   SPLIT,1             YES-PROCESS FIRST PRODUCT                    
*                                                                               
SG2      XC    KEY,KEY             GENERATE POOL CHUNK                          
         MVC   KDPT,DPT                                                         
         MVI   KPRD1,FF                                                         
         MVC   KSLN1,SLNT                                                       
         MVC   KSLNT,SLNT                                                       
         CLI   SPLIT,0                                                          
         BE    SG16                                                             
         MVC   KSLN1,SLN1                                                       
         MVC   KSLNT,SLN1                                                       
         CLI   SPLIT,1                                                          
         BE    SG16                                                             
         MVC   KSLN1,SLN2                                                       
         MVC   KSLNT,SLN2                                                       
         B     SG16                                                             
*                                                                               
SG8      MVI   SPLIT,0                                                          
         CLI   PRD2,0              TEST PIGGYBACKS                              
         BE    SG10                                                             
         CLI   SBESPLIT,N          YES-TEST SPLIT PIGGYBACKS                    
         BE    SG10                                                             
         MVI   SPLIT,1             YES - PROCESS PRODUCT 1                      
*                                                                               
SG10     XC    KEY,KEY             SET CHUNK KEY                                
         MVC   KDPT,DPT            DPT                                          
         CLI   SPLIT,2             TEST 2ND PRD OF SPLIT                        
         BE    SG12                YES                                          
         MVC   KPRD1,PRD1          PRD1                                         
         MVC   KSLN1,SLN1          SLN1                                         
         MVC   KSLNT,SLN1          SLN TOTAL                                    
         CLI   SPLIT,1             TEST 1ST PRD OF SPLIT                        
         BE    SG14                YES                                          
         MVC   KPRD2,PRD2          NO - SET PRD2                                
         MVC   KSLN2,SLN2                   SLN2                                
         MVC   KSLNT,SLNT                   SLN TOTAL                           
         B     SG14                                                             
*                                                                               
SG12     MVC   KPRD1,PRD2          2ND PRD OF SPLIT - PRD1                      
         MVC   KSLN1,SLN2                             SLN1                      
         MVC   KSLNT,SLN2                             SLN TOTAL                 
*                                                                               
SG14     CLI   SBEPRD,0            TEST PRODUCT FILTER                          
         BE    SG16                                                             
         CLC   KPRD1,SBEPRD        YES                                          
         BNE   SG40                                                             
*                                                                               
SG16     MVC   BYTE,KSLNT          SET SPOT LENGTH                              
         BAS   RE,GETEQU           GET EQUIVALENCY FACTOR                       
         MVC   EQFACT,FULL                                                      
*                                                                               
         LA    R2,GDELEM           SCAN THE GOAL ELEMENTS                       
         USING GLEMENT,R2                                                       
*                                                                               
SG18     CLI   0(R2),0             TEST END OF RECORD                           
         BE    SG40                                                             
         CLI   0(R2),X'21'         TEST GOAL WEEK ELEMENT                       
         BE    SG30                                                             
         CLI   0(R2),X'A1'         LOCKIN ELEMENT?                              
         BE    SG30                YES                                          
*                                                                               
SG20     ZIC   R0,1(R2)            NEXT GOAL ELEMENT                            
         AR    R2,R0                                                            
         B     SG18                                                             
*                                                                               
SG30     MVC   GOALDATE,GLWEEK                                                  
         BAS   RE,FINDDATE         FIND DATE TABLE DATE                         
         BNE   SG20                                                             
         BAS   RE,GENCHUNK         GENERATE CHUNK DATA                          
         B     SG20                                                             
*                                                                               
SG40     CLI   KPRD1,FF            TEST POL CHUNK                               
         BNE   SG42                                                             
         CLI   GKEYPRD,FF          YES-TEST CPP GUIDE                           
         BE    SG50                YES-FINISHED                                 
         CLI   SPLIT,1             NO-TEST NEED CHUNK FOR 2ND SLN               
         BNE   SG8                 NO-GENERATE PRODUCT CHUNK                    
         MVI   SPLIT,2                                                          
         B     SG2                                                              
*                                                                               
SG42     CLI   SPLIT,1                                                          
         BNE   SG50                                                             
         MVI   SPLIT,2                                                          
         B     SG10                                                             
*                                                                               
SG50     CLI   SBELOCK,Y           TEST LOCK-IN DATA REQUIRED                   
         BNE   SGX                                                              
         L     R6,SBAIO1                                                        
         LA    R2,GDELEM           SCAN THE GOAL ELEMENTS                       
         USING GLKELEM,R2                                                       
*                                                                               
SG52     CLI   0(R2),0             TEST END OF RECORD                           
         BE    SGX                                                              
         CLI   0(R2),X'30'                                                      
         BL    SG54                                                             
         CLI   0(R2),X'33'                                                      
         BNH   SG56                                                             
*                                                                               
SG54     ZIC   R0,1(R2)            NEXT GOAL ELEMENT                            
         AR    R2,R0                                                            
         B     SG52                                                             
*                                                                               
SG56     CLI   SBESPILL,C'O'       TEST ORIG MARKET ONLY                        
         BNE   *+12                                                             
         CLI   0(R2),X'31'                                                      
         BH    SG54                                                             
         CLI   SBESPILL,C'S'       TEST SPILL MARKET ONLY                       
         BNE   *+12                                                             
         CLI   0(R2),X'32'                                                      
         BL    SG54                                                             
         TM    SBQPER,SBQPWK       TEST PERIODS ARE WEEKS                       
         BZ    SG58                                                             
         CLI   0(R2),X'31'         YES - ONLY ACCEPT WEEKLY LOCKIN              
         BE    SG58                                                             
         CLI   0(R2),X'33'                                                      
         BNE   SG54                                                             
*                                                                               
SG58     XC    KEY,KEY             SET THE KEY                                  
         MVC   KDPT,DPT                                                         
         MVC   KPRD1,GLKPRD                                                     
         MVC   KSLN1,GLKTSC                                                     
         MVC   KSLNT,KSLN1                                                      
         MVC   GOALDATE,GLKDAT                                                  
         BAS   RE,FINDDATE         SET KDATE                                    
         BNE   SG54                                                             
         MVC   BYTE,KSLNT                                                       
         BAS   RE,GETEQU           GET EQUIVALENCY FACTOR                       
         MVC   EQFACT,FULL                                                      
*                                                                               
SG59     MVC   HALF,=H'100'        MONTHLY IN DOLLARS                           
         CLI   0(R2),X'31'                                                      
         BE    *+12                                                             
         CLI   0(R2),X'33'                                                      
         BNE   *+10                                                             
         MVC   HALF,=H'10'         WEEKLY IN DIMES                              
         L     R7,SBACHUNK                                                      
*                                                                               
SG60     OC    SGNEXT,SGNEXT       FIND RIGHT CHUNK                             
         BNZ   SG62                                                             
         TM    SBEFLAG,SBEWIPW+SBE2COS     EXTRACT WESTERN PW/COST2             
         BZ    SG61                         NO - SMALLER CHUNK                  
         XC    SGLCHNKD(SGLPWL),SGLCHNKD    CREATE NEW CHUNK                    
         MVC   SGKEY,KEY                                                        
         LA    RE,SGLPWL(R7)                                                    
         ST    RE,SGNEXT                                                        
         XC    0(4,RE),0(RE)                                                    
         B     SG64                                                             
*                                                                               
SG61     XC    SGLCHNKD(SGLCHNKL),SGLCHNKD    CREATE NEW CHUNK                  
         MVC   SGKEY,KEY                                                        
         LA    RE,SGLCHNKL(R7)                                                  
         ST    RE,SGNEXT                                                        
         XC    0(4,RE),0(RE)                                                    
         B     SG64                                                             
SG62     CLC   SGKEY,KEY                                                        
         BE    SG64                                                             
         ICM   R7,15,SGNEXT                                                     
         B     SG60                                                             
*                                                                               
SG64     SR    RF,RF                                                            
         ICM   RF,3,GLKSPT                                                      
         L     R1,SGLSPTS                                                       
         AR    R1,RF                                                            
         ST    R1,SGLSPTS                                                       
         ICM   RF,15,GLKDLR                                                     
         MH    RF,HALF                                                          
         L     R1,SGLDOL                                                        
         AR    R1,RF                                                            
         ST    R1,SGLDOL                                                        
         BAS   RE,DOLEQUIV                                                      
         L     R1,SGLEDOL                                                       
         AR    R1,RF                                                            
         ST    R1,SGLEDOL                                                       
         CLI   SBEDEMTY,0          TEST ACCOUNTING ONLY                         
         BE    SG70                                                             
         MVC   FULL,GLKDEM+3                                                    
         NI    FULL,X'FF'-X'80'                                                 
         SR    RE,RE               SET DEMO PRECISION                           
         LH    RF,HALF                                                          
         D     RE,=F'10'                                                        
         STH   RF,HALF                                                          
         L     RF,FULL                                                          
         MH    RF,HALF                                                          
         L     R1,SGLDEM                                                        
         AR    R1,RF                                                            
         ST    R1,SGLDEM                                                        
         BAS   RE,DEMEQUIV                                                      
         L     R1,SGLEDEM                                                       
         AR    R1,RF                                                            
         ST    R1,SGLEDEM                                                       
*                                                                               
SG70     CLC   SBQPRD,=C'POL'      TEST POL REQUEST                             
         BNE   SG72                                                             
         CLI   KPRD1,FF            YES-TEST DONE POL CHUNK YET                  
         BE    SG72                YES                                          
         MVI   KPRD1,FF            NO-SET PRD=POL AND DO AGAIN                  
         B     SG59                                                             
*                                                                               
SG72     B     SG54                NEXT GOAL ELEMENT                            
*                                                                               
SGX      BAS   RE,ADJPRECL         FOR LOCKED IN DEMOS SGLDEM/SGLEDEM           
         B     EXIT                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
* FIND DATE TABLE DATE FROM GOAL DATE                                           
* INPUT  : GOALDATE                                                             
* OUTPUT : CC EQ DATE FOUND AND KDATE = DATE                                    
*             NE DATE NOT FOUND                                                 
*                                                                               
FINDDATE L     R3,SBADATE                                                       
         L     R4,SBNDATES                                                      
         CLI   SBEBEF,Y            TEST NEED BEFORE REQUEST DATA                
         BNE   FD10                                                             
         CLC   GOALDATE,0(R3)                                                   
         BNL   FD10                                                             
         XC    KDATE,KDATE                                                      
         B     FDEQX                                                            
*                                                                               
FD10     MVC   KDATE,0(R3)         SCAN DATES TABLE                             
         CLC   GOALDATE,0(R3)                                                   
         BL    *+14                                                             
         CLC   GOALDATE,2(R3)                                                   
         BNH   FDEQX                                                            
         LA    R3,4(R3)                                                         
         BCT   R4,FD10                                                          
*                                                                               
         CLI   SBEAFTER,Y          TEST NEED AFTER REQUEST DATA                 
         BNE   FDNEX                                                            
         BCTR  R3,0                                                             
         BCTR  R3,0                                                             
         CLC   GOALDATE,0(R3)                                                   
         BNH   FDNEX                                                            
         MVC   KDATE,XFF                                                        
         B     FDEQX                                                            
*                                                                               
FDNEX    LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                               
FDEQX    CR    RE,RE                                                            
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO GENERATE CHUNK DATA                                            
* INPUT  : R2 = A(GOAL WEEK ELEMENT)                                            
*          KEY = CHUNK KEY                                                      
*                                                                               
GENCHUNK NTR1                                                                   
         USING GLEMENT,R2                                                       
         MVC   DEM,GLGRP           DEMO                                         
*                                                                               
         BAS   RE,ADJPREC          ADJUST DECIMAL PRECISION                     
*                                                                               
GC00     MVC   DOL,GLBUDGET        DOLLARS                                      
         CLI   GKEYPRD,FF          TEST CPP GUIDE                               
         BNE   GC01                                                             
         CLC   DEM,=F'1000'        YES - RATING MUST BE 100.0                   
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,DOL              SO DIVIDE DOLLARS AND RATING BY 100          
         SRDA  RE,31               TO AVOID BIG NUMBERS                         
         D     RE,=F'100'                                                       
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         ST    RF,DOL                                                           
         MVC   DEM,=F'10'                                                       
*                                                                               
GC01     OC    DOL,DOL             TEST CPP GUIDE NEEDED                        
         BZ    GC02                                                             
         OC    DEM,DEM                                                          
         BNZ   GC04                                                             
         CLI   SBEDEMTY,0          TEST ACCOUNTING ONLY                         
         BE    GC04                                                             
*                                                                               
GC02     CLI   SBCPROF+8,C'0'      TEST NO CPP GUIDES                           
         BH    GC04                YES                                          
         CLI   GKEYDPT,C'+'        NO CPP GUIDE FOR DPTS + OR -                 
         BE    GC04                                                             
         CLI   GKEYDPT,C'-'                                                     
         BE    GC04                                                             
         BAS   RE,CPPCONV          NO-COMPUTE DOLLARS/POINTS                    
*                                                                               
GC04     L     R7,SBACHUNK                                                      
*                                                                               
         TM    SBEFLAG,SBEWIPW+SBE2COS     EXTRACT WESTERN PW/COST2             
         BZ    *+8                          NO                                  
         BAS   RE,GETPW                                                         
*                                                                               
GC10     OC    SGNEXT,SGNEXT       FIND RIGHT CHUNK                             
         BNZ   GC12                                                             
         TM    SBEFLAG,SBEWIPW+SBE2COS     EXTRACT WESTERN PW/COST2             
         BZ    GC11                         NO - USE SMALLER CHUNK              
         XC    SGLCHNKD(SGLPWL),SGLCHNKD    CREATE NEW CHUNK                    
         MVC   SGKEY,KEY                                                        
         LA    RE,SGLPWL(R7)                                                    
         ST    RE,SGNEXT                                                        
         XC    0(4,RE),0(RE)                                                    
         B     GC14                                                             
GC11     XC    SGLCHNKD(SGLCHNKL),SGLCHNKD    CREATE NEW CHUNK                  
         MVC   SGKEY,KEY                                                        
         LA    RE,SGLCHNKL(R7)                                                  
         ST    RE,SGNEXT                                                        
         XC    0(4,RE),0(RE)                                                    
         B     GC14                                                             
GC12     CLC   SGKEY,KEY                                                        
         BE    GC14                                                             
         ICM   R7,15,SGNEXT                                                     
         B     GC10                                                             
*                                                                               
GC14     CLI   SBEDEMTY,0          TEST ACCOUNTING ONLY                         
         BE    GC16                                                             
         L     RF,DEM              EQUIVALENCE THE DEMO                         
         BAS   RE,DEMEQUIV                                                      
         ST    RF,EDEM                                                          
*                                                                               
GC16     CLI   SPLIT,0             TEST PIGGYBACK SPLIT                         
         BE    GC18                                                             
         ZIC   R1,KSLN1            YES - APPORTION DOLLARS BETWEEN PRDS         
         AR    R1,R1                                                            
         M     R0,DOL                                                           
         ZIC   RE,SLNT                                                          
         DR    R0,RE                                                            
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         ST    R1,DOL                                                           
*                                                                               
GC18     L     RF,DOL              EQUIVALENCE THE DOLLARS                      
         BAS   RE,DOLEQUIV                                                      
         ST    RF,EDOL                                                          
*                                                                               
         CLI   GLCODE,X'A1'        X'A1' LOCK-IN ELEMENT?                       
         BNE   GC18A               NO                                           
         L     R1,SGKDOL           ACCUMULATE DOLLARS                           
         A     R1,DOL                                                           
         ST    R1,SGKDOL                                                        
         L     R1,SGKEDOL          EQUIV DOLLARS                                
         A     R1,EDOL                                                          
         ST    R1,SGKEDOL                                                       
         B     GC18B                                                            
*                                                                               
GC18A    L     R1,SGDOL            ACCUMULATE DOLLARS                           
         A     R1,DOL                                                           
         ST    R1,SGDOL                                                         
         L     R1,SGEDOL                      EQUIV DOLLARS                     
         A     R1,EDOL                                                          
         ST    R1,SGEDOL                                                        
*                                                                               
GC18B    TM    SBEFLAG,SBEWIPW+SBE2COS     EXTRACT WESTERN PW/COST2             
         BZ    GC20                         NO - SKIP SUM                       
         L     R1,SGLPWDOL         WESTERN PW $$                                
         A     R1,PWGLDOL                                                       
         ST    R1,SGLPWDOL                                                      
         L     R1,SGLPWCTX         WESTERN PW CLIENT GOAL TAX                   
         A     R1,PWGLCTX                                                       
         ST    R1,SGLPWCTX                                                      
         L     R1,SGLPWATX         WESTERN PW AGENCY GOAL TAX                   
         A     R1,PWGLATX                                                       
         ST    R1,SGLPWATX                                                      
*                                                                               
GC20     CLI   SBEDEMTY,0          TEST ACCOUNTING ONLY                         
         BE    GCX                                                              
         CLI   GLCODE,X'A1'        X'A1' LOCK-IN ELEMENT?                       
         BNE   GC20A               NO                                           
         L     R1,SGKDEM           DEMO 1                                       
         A     R1,DEM                                                           
         ST    R1,SGKDEM                                                        
         L     R1,SGKEDEM          EQUIV DEMO 1                                 
         A     R1,EDEM                                                          
         ST    R1,SGKEDEM                                                       
         B     GCX                                                              
*                                                                               
GC20A    L     R1,SGDEM                       DEMO 1                            
         A     R1,DEM                                                           
         ST    R1,SGDEM                                                         
         L     R1,SGEDEM                      EQUIV DEMO 1                      
         A     R1,EDEM                                                          
         ST    R1,SGEDEM                                                        
*                                                                               
GCX      B     EXIT                                                             
         EJECT                                                                  
*                                                                               
ADJPREC  NTR1                                                                   
*                                                                               
         OC    SBESTDEM(3),SBESTDEM  SBESTDEM SET?                              
         BZ    ADJPX               NO, DONE                                     
*                                                                               
         LA    RE,SBESTDEM+1       FIRST DEMO+1                                 
         CLI   SBESTDEM+2,0        NON-TRADITIONAL DEMO?                        
         BNE   ADJPREC1            NO                                           
*                                                                               
         OC    SBPDEMOS,SBPDEMOS   DEMO MENU OR DEMO OPTION SET?                
         BZ    *+12                NO - USE SBCOMDEM                            
         ICM   RE,15,ACOMLSTG      HAVE A(COMSCORE DEMO LIST)?                  
         B     *+8                 GO TEST IF SET                               
         ICM   RE,15,SBCOMDEM      HAVE A(COMSCORE DEMO OVERRIDE LIST)?         
         BZ    ADJPREC4            NO                                           
*                                                                               
         XR    RF,RF               CLEAR RF                                     
         ICM   RF,1,SBESTDEM+1     GET INDEX                                    
         JZ    *+2                 IF 0, SOMETHING IS VERY WRONG                
         BCTR  RF,0                -1                                           
         MHI   RF,8                INDEX INTO DEMO LIST                         
         AR    RE,RF               INDEX TO CURRENT DEMO                        
*                                                                               
ADJPREC1 CLI   0(RE),C'R'          RATING?                                      
         BE    ADJPREC2            YES                                          
         CLI   0(RE),C'E'          EXTENDED RATING?                             
         BNE   ADJPREC4            NO                                           
*                                                                               
ADJPREC2 TM    DEM,X'40'           HAVE 2 DECIMAL DEMO VALUE?                   
         BZ    ADJPREC3            NO                                           
         NI    DEM,X'3F'           DROP FLAG FROM VALUE                         
         TM    SBEFLAG4,SBE42DEC   REPORT SUPPORT 2 DEC?                        
ADJPRC2A BNZ   ADJPX               YES - LEAVE IT ALONE                         
         L     R0,DEM              ADJUST 2 DECIMAL PRECISION TO 1              
         SRDA  R0,31               R1 = 2*DEM                                   
         D     R0,=F'10'           DIVIDE BY 10                                 
         AHI   R1,1                ADD 1 FOR ROUNDING                           
         SRA   R1,1                NOW IN 1 DECIMAL VALUE                       
         ST    R1,DEM              1 DECIMAL VALUE IN DEM                       
         B     ADJPX               DONE                                         
                                                                                
ADJPREC3 TM    SBEFLAG4,SBE42DEC   REPORT SUPPORT 2 DEC                         
ADJPRC2B BZ    ADJPX               NO - LEAVE IT ALONE                          
*                                                                               
         L     R1,DEM              1 DECIMAL DEMO                               
         MHI   R1,10               MULTIPLY BY 10                               
         ST    R1,DEM              NOW IN 2 DECIMAL EQUIVALENT                  
         B     ADJPX               DONE                                         
*                                                                               
ADJPREC4 TM    DEM,X'40'           HAVE 2 DECIMAL DEMO VALUE?                   
         BZ    ADJPREC5            NO                                           
         NI    DEM,X'3F'           DROP FLAG FROM VALUE                         
         TM    SBEFLAG9,SBE92DEC   REPORT 2 DECIMAL FOR IMPRESSIONS?            
         B     ADJPRC2A            GO TEST CC                                   
*                                                                               
ADJPREC5 TM    SBEFLAG9,SBE92DEC   REPORT 2 DECIMAL FOR IMPRESSIONS?            
         B     ADJPRC2B            GO TEST CC                                   
*                                                                               
ADJPX    B     EXIT                                                             
*                                                                               
ADJPRECL NTR1                                                                   
*                                                                               
         OC    SBESTDEM(3),SBESTDEM  SBESTDEM SET?                              
         BZ    ADJP2X              NO, DONE                                     
*                                                                               
         CLI   SBESTDEM+2,0        NON-TRADITIONAL DEMO?                        
         BNE   ADJPRECA            NO                                           
*                                                                               
         OC    SBPDEMOS,SBPDEMOS   DEMO MENU OR DEMO OPTION SET?                
         BZ    *+12                NO - USE SBCOMDEM                            
         ICM   RE,15,ACOMLSTG      HAVE A(COMSCORE DEMO LIST)?                  
         B     *+8                 GO TEST IF SET                               
         ICM   RE,15,SBCOMDEM      HAVE A(COMSCORE DEMO OVERRIDE LIST)?         
         BZ    ADJP2X              NO, DONE                                     
*                                                                               
         XR    RF,RF               CLEAR RF                                     
         ICM   RF,1,SBESTDEM+1     GET INDEX                                    
         JZ    *+2                 IF 0, SOMETHING IS VERY WRONG                
         BCTR  RF,0                -1                                           
         MHI   RF,8                INDEX INTO DEMO LIST                         
         AR    RE,RF               INDEX TO CURRENT DEMO                        
         CLI   0(RE),C'R'          RATING?                                      
         BE    ADJPRECC            YES                                          
         CLI   0(RE),C'E'          EXTENDED RATING?                             
         B     ADJPRECB            GO TEST CC                                   
*                                                                               
ADJPRECA CLI   SBESTDEM+1,C'R'     RATING?                                      
         BE    *+12                YES                                          
         CLI   SBESTDEM+1,C'E'     EXTENDED RATING?                             
ADJPRECB BNE   ADJP2C              NO - IMPRESSION                              
*                                                                               
ADJPRECC TM    SBEFLAG4,SBE42DEC   REPORT SUPPORT 2 DEC                         
         B     *+8                 GO TEST CC                                   
ADJP2C   TM    SBEFLAG9,SBE92DEC   REPORT SUPPORT 2 DEC IMPRESSIONS?            
         BZ    ADJP2X              NO - LEAVE IT ALONE                          
*                                                                               
         L     R7,SBACHUNK         START FROM FIRST CHUNK ENTRY                 
*                                                                               
ADJ00    OC    SGNEXT,SGNEXT                                                    
         BZ    ADJP2X                                                           
*                                                                               
         L     R1,SGLDEM                                                        
         MHI   R1,10                                                            
         ST    R1,SGLDEM                                                        
*                                                                               
         L     R1,SGLEDEM                                                       
         MHI   R1,10                                                            
         ST    R1,SGLEDEM                                                       
*                                                                               
         ICM   R7,15,SGNEXT                                                     
         B     ADJ00                                                            
*                                                                               
ADJP2X   B     EXIT                                                             
* SUBROUTINE TO LOOK UP PW PERCENTAGE & RETURN AGY GOAL $$$                     
*  PW% IS PULLED OFF FROM ESTTABLE OR PARAM2, NEVER FROM PW RECORD              
* INPUT  :                                                                      
*                                                                               
* OUTPUT :                                                                      
*                                                                               
GETPW    NTR1                                                                   
         SR    R4,R4                                                            
         L     R1,APARM            GET PW% FROM 2ND PARAM                       
         CLI   4(R1),X'FE'         IS THERE A PW PCT                            
         BE    *+12                                                             
         CLI   4(R1),X'FD'         IS THERE A COST2 PCT                         
         BNE   GETPW2                NOPE                                       
*                                                                               
         SR    RF,RF               GET ADDRESS OF VALUE                         
         ICM   RF,7,5(R1)                                                       
         BZ    GETPW2                                                           
         ICM   R4,15,0(RF)         R4 = PW%                                     
*                                                                               
GETPW2   DS    0H                                                               
         ICM   R1,15,SBAESTTB      A(ESTTABLE)                                  
         BNZ   GETPW5                                                           
         CLI   PARM2,X'FD'         IS THERE A COST2 PCT                         
         BE    GETPW07               YES                                        
         B     GETPW10                                                          
*                                                                               
GETPW5   LA    RE,255                                                           
         CLI   KPRD1,X'FE'         TEST UNALLOCATED                             
         BE    *+8                 YES-USE PRODUCT POL                          
         IC    RE,KPRD1                                                         
         BCTR  RE,0                                                             
         SLL   RE,8                                                             
         SR    RF,RF                                                            
         IC    RF,GKEYEST                                                       
         BCTR  RF,0                                                             
         AR    RE,RF                                                            
         LA    R1,0(R1,RE)                                                      
         SR    RE,RE                                                            
         ICM   RE,1,0(R1)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  RE,0                                                             
         MHI   RE,ESTBUFFL                                                      
         ICM   R1,15,SBAESTBF                                                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    RE,0(R1,RE)                                                      
         USING ESTBUFFD,RE                                                      
*                                                                               
GETPW07  SR    R4,R4                                                            
         ICM   R4,7,EBUFPCT        GET DEFAULT PERCENTAGE                       
         N     R4,=X'007FFFFF'                                                  
         CLI   PARM2,X'FD'         TEST COST2 PCT                               
         BNE   GETPW10                                                          
         ICM   R4,7,PARM2+1                                                     
         BZ    EXIT                                                             
         B     GETC2                                                            
         DROP  RE                                                               
*                                                                               
GETPW10  DS    0H                                                               
         SR    R5,R5                                                            
         ICM   R5,7,SBAWIPW        TEST PW DATA PASSED                          
         BNZ   *+6                 SOMETHING'S WRONG                            
         DC    H'0'                                                             
         USING SBWIPWD,R5                                                       
*                                                                               
GETPW12  DS    0H                                                               
         XC    PWBLOCK,PWBLOCK                                                  
         LA    R3,PWBLOCK                                                       
         USING PWBLKD,R3                                                        
         MVI   PWACT,PWGETGOL                                                   
         MVC   PWACTGOL,DOL                                                     
         ST    R4,PWPCT                                                         
         MVC   PWTAXRT+2(2),SBPWTAX                                             
*                                                                               
         GOTO1 VPWCALC,DMCB,(R3)                                                
         CLI   PWERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   PWGLDOL,PWVAL       WIMGRS INCLUDING TAX                         
         MVC   PWGLATX,PWWIMTAX                                                 
         MVC   PWGLCTX,PWCLTTAX                                                 
*                                                                               
         TM    SBEFLAG,SBEPWNTX    TEST SUPPRESS PW TAX                         
         BZ    GETPWX                                                           
*                                                                               
         L     R1,PWGLDOL          SUBTRACT TAX FROM PW DOLLARS                 
         S     R1,PWWIMTAX                                                      
         ST    R1,PWGLDOL                                                       
*                                                                               
         L     R1,DOL              SUBTRACT TAX FROM CLIENT DOLLARS             
         S     R1,PWCLTTAX                                                      
         ST    R1,DOL                                                           
*                                                                               
GETPWX   DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
* COMPUTE IM DOLLARS USING COST2 FACTOR IN R4  (DIVIDE)                         
         SPACE 1                                                                
GETC2    L     R0,DOL              CLT GOALS                                    
         CVD   R0,DUB              MAKE AMOUNT PACKED                           
         ZAP   P16,DUB                                                          
         SRP   P16,6,0             MULTIPLY BY 1,000,000                        
         CVD   R4,DUB              C2 FACTOR                                    
*                                                                               
         LA    R1,L'DUB-1          DIVISOR IS HOW MANY BYTES?                   
         LA    RF,DUB                                                           
GETC2A00 CLI   0(RF),0                                                          
         BNE   GETC2A10                                                         
         LA    RF,1(RF)                                                         
         BCT   R1,GETC2A00         LOOP OR UNTIL WE'RE DOWN TO 1 BYTE           
*                                                                               
GETC2A10 EX    R1,*+8              THIS WAY WE AVOID MORE EXCEPTIONS            
         B     *+10                                                             
         DP    P16,0(0,RF)         GROSS/NET X FACTOR                           
*                                                                               
         LR    RF,R1               FIGURE HOW MANY BYTES OF P16 TO              
         AHI   RF,2                   CONVERT TO BINARY                         
         LA    R1,16                                                            
         SR    R1,RF                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         ZAP   DUB,P16(0)                                                       
         CVB   R0,DUB                                                           
         ST    R0,PWGLDOL                                                       
         B     EXIT                                                             
*                                                                               
         DROP  R3,R5                                                            
         EJECT                                                                  
* EQUIVALENCE DEMO VALUE                                                        
* INPUT  : RF = DEMO VALUE                                                      
*          EQFACT = EQUIV FACTOR                                                
* OUTPUT : RF = EQUIVALENCED DEMO VALUE                                         
*                                                                               
DEMEQUIV ST    RE,SAVERE                                                        
         SR    RE,RE                                                            
         L     R1,EQFACT                                                        
         MR    RE,R1                                                            
         SLDA  RE,1                                                             
         D     RE,=F'1000'                                                      
         A     RF,=F'1'                                                         
         SRL   RF,1                                                             
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
* EQUIVALENCE DOLLAR                                                            
* INPUT  : RF = DOLLARS                                                         
*          EQFACT = EQUIV FACTOR                                                
* OUTPUT : RF = EQUIVALENCED DOLLARS                                            
*                                                                               
DOLEQUIV ST    RE,SAVERE                                                        
         SR    RE,RE                                                            
         M     RE,=F'1000'                                                      
         SLDA  RE,1                                                             
         SR    R1,R1                                                            
         ICM   R1,15,EQFACT                                                     
         BNZ   *+8                                                              
         L     R1,=F'1000'                                                      
         DR    RE,R1                                                            
         LTR   RF,RF                                                            
         BM    *+8                                                              
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
* CONVERT DOLLARS VIA CPP GUIDE                                                 
*                                                                               
CPPCONV  NTR1                                                                   
         OC    CPPTAB(6),CPPTAB    TEST CPP WEEK TABLE SET YET                  
         BNZ   CP20                                                             
         MVC   BYTE,SLNT           NO - GET CPP EQUIVALENCY FACTOR              
         BAS   RE,GETEQU                                                        
         MVC   CPPEQFAC,FULL                                                    
         SR    R0,R0               CREATE CPP TABLE                             
         LA    R2,2                                                             
         LA    R3,CPPTAB                                                        
         L     R6,SBAIO2           CPP GUIDE 1                                  
         OC    0(256,R6),0(R6)                                                  
         BZ    CP16                                                             
CP10     LA    R4,GDELEM                                                        
         LA    R5,CPPTABX                                                       
*                                                                               
CP12     CLI   0(R4),0                                                          
         BE    CP16                                                             
         CLI   0(R4),X'21'                                                      
         BNE   CP14                                                             
         MVC   0(2,R3),GLWEEK-GLEMENT(R4)                                       
         MVC   2(4,R3),GLBUDGET-GLEMENT(R4)                                     
         LA    R3,6(R3)                                                         
         CR    R3,R5                                                            
         BL    CP14                                                             
         DC    H'0'                                                             
*                                                                               
CP14     IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     CP12                                                             
*                                                                               
CP16     BCT   R2,*+8              TRY 2ND CPP GUIDE                            
         B     CP20                                                             
         L     R6,SBAIO3           CPP GUIDE 2                                  
         OC    0(256,R6),0(R6)                                                  
         BNZ   CP10                                                             
*                                                                               
CP20     LA    R3,CPPTAB           FIND GOAL WEEK IN CPP TABLE                  
*                                                                               
CP22     OC    0(6,R3),0(R3)                                                    
         BZ    CP30                NOT FOUND                                    
         CLC   GOALDATE,0(R3)                                                   
         BE    *+12                                                             
         LA    R3,6(R3)                                                         
         B     CP22                                                             
         OC    2(4,R3),2(R3)       TEST ZERO CPP                                
         BZ    CP30                                                             
         SR    R0,R0                                                            
         OC    DOL,DOL             TEST BUDGET=0                                
         BNZ   CP24                                                             
         L     R1,DEM              POINTS                                       
         AR    R1,R1               X2                                           
         M     R0,CPPEQFAC         EF                                           
         ICM   RE,15,2(R3)                                                      
         MR    R0,RE               POINTS X CPP                                 
         LHI   RE,1000             EF30                                         
         MHI   RE,1000             SCALE                                        
         DR    R0,RE                                                            
         A     R1,=F'1'            ROUND                                        
         SRL   R1,1                                                             
         ST    R1,DOL                                                           
         B     CP30                                                             
*                                                                               
CP24     L     R1,DOL              DOLLARS                                      
         M     R0,=F'2000'         SCALE X1000 X2                               
         ICM   RE,15,2(R3)                                                      
         DR    R0,RE               CPP $                                        
         A     R1,=F'1'            ROUND                                        
         SRL   R1,1                / 2                                          
         AR    R1,R1               X 2                                          
         M     R0,=F'1000'         EF30                                         
         L     RE,CPPEQFAC         EF                                           
         DR    R0,RE                                                            
         A     R1,=F'1'            ROUND                                        
         SRL   R1,1                                                             
         ST    R1,DEM              GOAL POINTS                                  
*                                                                               
CP30     B     CPX                                                              
*                                                                               
CPX      B     EXIT                                                             
         EJECT                                                                  
* ROUTINE TO FIND THE EQUIVALENCE TABLE                                         
*                                                                               
EQTAB    ST    RE,SAVERE                                                        
         LA    RF,L'SBDPTTAB/5     RF=COUNTER                                   
         LA    RE,SBDPTTAB         RE=A(DPT TABLE)                              
         CLC   DPT,0(RE)           MATCH ON DAYPART CODE                        
         BE    EQTAB2                                                           
         LA    RE,5(RE)                                                         
         BCT   RF,*-14                                                          
         B     EQTABX                                                           
*                                                                               
EQTAB2   ZIC   RF,1(RE)            DAYPART INTERNAL CODE                        
         N     RF,=F'15'           ZERO HIGH ORDER NIBBLE                       
         IC    RF,SBDPEQTB(RF)     TABLE IND                                    
         LTR   RF,RF               TEST FOR ZERO                                
         BZ    *+6                 YES-ACTUALLY SHOULD BE 1 !!                  
         BCTR  RF,0                                                             
         MHI   RF,60                                                            
*                                                                               
         LA    RF,SBEQTAB(RF)       EQUIV TABLE                                 
         ST    RF,AEQTAB                                                        
*                                                                               
EQTABX   L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
* ROUTINE TO GET THE EQUIVALENCE FACTOR FOR A SECONDS LENGTH                    
* INPUT  : BYTE = SECONDS LENGTH                                                
* OUTPUT : FULL = EQUIVALENCY FACTOR                                            
*                                                                               
GETEQU   NTR1                                                                   
         MVC   FULL,=F'1000'       DEFAULT TO BASE                              
         ICM   R2,15,AEQTAB        RF=A(EQUIV TABLE FOR DPT)                    
         BZ    GETEQUX                                                          
*                                                                               
         MVC   DMCB+4(4),=X'D9000A57'                                           
         GOTO1 VCALLOV,DMCB,0         GET SPSLENTAB                             
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     R1,DMCB             POINT TO START OF PHASE                      
         LH    RE,0(R1)            ENTRY LENGTH                                 
         L     RF,2(R1)            EOT DSPL                                     
         AR    RF,R1               RELOCATE EOT ADDRESS                         
         AHI   R1,6                POINT TO FIRST ENTRY                         
*                                                                               
         MVI   HALF,C'T'                                                        
         CLI   SBMED,C'T'                                                       
         BE    GETEQU1                                                          
         CLI   SBMED,C'N'                                                       
         BE    GETEQU1                                                          
         CLI   SBMED,C'C'                                                       
         BE    GETEQU1                                                          
*                                                                               
         MVI   HALF,C'R'                                                        
         CLI   SBMED,C'R'                                                       
         BE    GETEQU1                                                          
         CLI   SBMED,C'X'                                                       
         BE    GETEQU1                                                          
         DC    H'0'                                                             
*                                                                               
GETEQU1  CLC   =C'00',0(R1)        TEST DEFAULT TABLE                           
         BE    GETEQU2                                                          
         CLC   0(2,R1),SBAGY       ELSE MATCH AGY                               
         BNE   *+14                                                             
GETEQU2  CLC   HALF(1),2(R1)       AND MEDIA                                    
         BE    GETEQU3                                                          
*                                                                               
         BXLE  R1,RE,GETEQU1                                                    
         DC    H'0'                                                             
*                                                                               
GETEQU3  AHI   R1,4                POINT BEYOND HEADER                          
         ZIC   R3,BYTE             GET SLN                                      
         AR    R3,R3               X 2                                          
         AR    R1,R3               POINT TO ENTRY                               
         CLI   1(R1),0             SLN VALID?                                   
         BE    GETEQUX             NO                                           
         ZIC   R3,0(R1)            GET INDEX INTO EQU TABLE                     
         AR    R2,R3               INDEX INTO TABLE                             
         MVC   FULL+2(2),0(R2)                                                  
*                                                                               
GETEQUX  B     EXIT                                                             
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
PATCH    DC    XL32'00'                                                         
         EJECT                                                                  
* DSECT TO COVER SPOTGOAL WORKING STORAGE                                       
*                                                                               
WORKD    DSECT                                                                  
APARM    DS    A                                                                
PARMS    DS    0XL8                                                             
PARM1    DS    A                                                                
PARM2    DS    A                                                                
RELO     DS    A                                                                
USERRD   DS    A                   USER REGISTER 13                             
SAVERE   DS    A                                                                
*                                                                               
VCALLOV  DS    V                                                                
VPWCALC  DS    V                                                                
*                                                                               
DMCB     DS    8F                                                               
DUB      DS    D                                                                
P16      DS    PL16                                                             
FULL     DS    F                                                                
HALF     DS    H                                                                
*                                                                               
DOL      DS    F                                                                
EDOL     DS    F                                                                
DEM      DS    F                                                                
EDEM     DS    F                                                                
AEQTAB   DS    A                   A(EQUIVALENCE TABLE)                         
EQFACT   DS    F                   SLN EQUIVALENCY FACTOR                       
CPPEQFAC DS    F                   CPP EQUIVALENCY FACTOR                       
PWGLDOL  DS    F                   WESTERN PW GOAL $                            
PWGLCTX  DS    F                   WI PW CLIENT GOAL TAX                        
PWGLATX  DS    F                   WI PW AGENCY GOAL TAX                        
*                                                                               
BYTE     DS    X                                                                
ELCDLO   DS    X                                                                
ELCDHI   DS    X                                                                
XFF      DS    XL16                                                             
*                                                                               
DPT      DS    C                                                                
PRD1     DS    X                                                                
PRD2     DS    X                                                                
SLN1     DS    X                                                                
SLN2     DS    X                                                                
SLNT     DS    X                                                                
SPLIT    DS    X                                                                
GOALDATE DS    XL2                                                              
*                                                                               
KEY      DS    0CL8                                                             
KDPT     DS    C                                                                
KPRD1    DS    X                                                                
KPRD2    DS    X                                                                
KSLNT    DS    X                                                                
KSLN1    DS    X                                                                
KSLN2    DS    X                                                                
KDATE    DS    XL2                                                              
*                                                                               
CPPTAB   DS    106XL6              CPP TABLE                                    
CPPTABX  EQU   *                                                                
CPPTABL  EQU   CPPTABX-CPPTAB                                                   
*                                                                               
PWBLOCK  DS    XL48                                                             
*                                                                               
WORKX    EQU   *                                                                
         SPACE 2                                                                
Y        EQU   C'Y'                                                             
N        EQU   C'N'                                                             
FF       EQU   X'FF'                                                            
         EJECT                                                                  
SBLOCKD  DSECT                                                                  
       ++INCLUDE SPOTBLOCK                                                      
         EJECT                                                                  
* SPGENGOAL                                                                     
*        PRINT OFF                                                              
GOALRECD DSECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* SPPWBLOCK                                                                     
         PRINT OFF                                                              
       ++INCLUDE SPPWBLOCK                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* SPGENWIPW                                                                     
         PRINT OFF                                                              
       ++INCLUDE SPGENWIPW                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017SPOTGOAL  11/08/19'                                      
         END                                                                    
