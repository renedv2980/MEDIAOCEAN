*          DATA SET SPSFM78    AT LEVEL 016 AS OF 06/10/13                      
*PHASE T21778A                                                                  
*INCLUDE RECUP                                                                  
*INCLUDE SPDSTCAL                                                               
*INCLUDE XSORT                                                                  
         TITLE 'T21778 - BUY MOVE REPORT'                                       
T21778   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 FTPRDETX-FTPRDETB,T21778,R7,RR=R3                                
*                                                                               
MAXSPOTS EQU   208                 MAX SPOTS IN BUYREC                          
*                                                                               
         LR    R0,RC               SAVE OFF A(WORKING STORAGE)                  
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
*                                                                               
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
*                                                                               
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    RD,SAVERD                                                        
*                                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         ST    R3,RELO                                                          
         ST    RB,MYBASE1                                                       
         ST    R7,MYBASE2                                                       
*                                                                               
         ST    R0,AFTPETAB                                                      
*                                                                               
         BRAS  RE,INIT                                                          
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PR                                                               
*                                                                               
ADCONLST DS    0A                                                               
         DC    V(RECUP)                                                         
         DC    A(0)                GETRATE (NOW CORERES)                        
         DC    A(0)                =V(MKTLIST)  WAS V(DUMMY)                    
         DC    A(0)                =V(MKTLSTX)                                  
         DC    A(SAVEBUY)                                                       
         DC    V(XSORT)            WAS A(CHKDBL), USING IT FOR XSORT            
         DC    A(BLDEL)                                                         
         DC    A(VALIMGR)                                                       
         DC    A(0)                =V(GETBUY)                                   
         DC    A(BLDMKT)                                                        
ADCONLSX EQU   *                                                                
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
*                                                                               
VK       DS    0H                                                               
         LA    R2,BXFMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
         CLI   QMED,C'N'                                                        
         BNE   VK010                                                            
         MVC   HALF,=Y(NONETCPY)                                                
         B     ERR2BYTE                                                         
*                                                                               
VK010    CLI   QMED,C'C'           MEDIA C?                                     
         BNE   VK015               NO                                           
         MVC   HALF,=Y(1023)       YES, MED C NOT VALID (AS PER MEL!)           
         B     ERR2BYTE            4/26/04                                      
*                                                                               
VK015    LA    R2,BXFOPTSH                                                      
         BRAS  RE,VALOPT                                                        
*                                                                               
         LA    R2,BXFFRCLH         'FROM' CLIENT                                
         GOTO1 VALICLT                                                          
         MVC   FRCPROF,SVCPROF                                                  
*                                                                               
         BRAS  RE,STPRDETB         SETUP PRD/EST TABLE                          
*                                                                               
         MVC   FRCLI,QCLT          SAVE EBCDIC CODE                             
         MVC   FRCLIN,CLTNM        AND CLIENT NAME                              
         L     R6,AIO              PICK UP CLIENT OPTIONS                       
         USING CLTHDRD,R6                                                       
         TM    COPT2,COP2FRZ       IS CLIENT FROZEN?                            
         BNZ   ERFRZCLT            YES                                          
         MVC   SVCOPT1,COPT1                                                    
         MVC   FRCOFFC,COFFICE                                                  
         DROP  R6                                                               
*                                                                               
         MVI   FRPOLNPW,0                                                       
         CLI   SVCPROF,C'0'        TEST BRAND POOL CLIENT                       
         BE    VK020               NO                                           
         CLI   SVAPROF+11,C'Y'     TEST AGENCY RADIO NPW OPTION                 
         BNE   VK020               NO                                           
         CLI   QMED,C'R'           TEST FOR RADIO                               
         BNE   VK020               NO                                           
         MVI   FRPOLNPW,X'80'                                                   
*                                                                               
VK020    LA    R2,BXFFRPRH         'FROM' PRODUCT                               
         GOTO1 VALIPRD                                                          
         CLI   SVCPROF,C'0'        TEST BRAND POOL CLIENT                       
         JE    *+14                NO                                           
         CLC   =C'POL',WORK        TEST FOR POOL                                
         JE    TRAPERR             YES-DO NOT ALLOW IT-REQUIRE A BRAND          
         MVC   QPRD(3),WORK                                                     
         MVC   FRPROD,WORK         PRODUCT CODE AND NAME                        
         MVC   FRPRODN,PRDNM                                                    
*                                                                               
         LA    R2,BXFFRESH         'FROM' ESTIMATE                              
         GOTO1 VALINUM                                                          
         MVC   BEST,ACTUAL                                                      
         BRAS  RE,GETEST                                                        
*                                                                               
         MVC   FREDYMNU,SVDAYMNU   SAVE OFF DAYPART MENU                        
         MVC   FREDEMOS,SVDEMOS             DEMO CATEGORIES                     
         MVC   FRESTN,SVESTNAM                                                  
         MVC   FRESTPOL,POLESTSW   SAVE POOL ESTIMATE SWITCH                    
         MVC   QPERFR,WORK         SAVE ESTIMATE START/END                      
*                                                                               
***      CLC   =C'POL',QPRD        'FROM' A POL ESTIMATE?                       
***      BNE   VK030                                                            
         BRAS  RE,FILESTTB         YES, FILL WHAT PRDS HAVE THIS EST            
*                                                                               
VK030    L     RE,AIO                                                           
         USING ESTHDRD,RE                                                       
         TM    ECNTRL,X'0C'        TEST LOCKED/HELD                             
         BNZ   ERFESLCK                                                         
         DROP  RE                                                               
*                                                                               
         LA    R2,BXFFRDTH         EDIT 'FROM' PERIOD                           
         CLI   5(R2),2             TEST INPUT LEN = 2                           
         BNE   VK033                                                            
         CLC   =C'ES',8(R2)                                                     
         BNE   VK033               USE 'FROM' ESTIMATE'S START/END              
         GOTO1 DATCON,DMCB,(X'10',QPERFR),(17,8(R2))                            
         MVI   5(R2),17                                                         
         OI    6(R2),X'80'                                                      
*                                                                               
VK033    L     R6,AIO                                                           
         USING ESTHDRD,R6                                                       
         OC    ELOCKYM,ELOCKYM     ESTIMATE LOCKED?                             
         BZ    VK036               YES                                          
         CLI   5(R2),0             ANY 'FROM' PERIOD?                           
         BE    ERFESLCK            EST LOCKED, SPOT CAN'T BE MOVED              
*                                                                               
VK036    CLI   5(R2),0                                                          
         BE    VK050                                                            
         GOTO1 PERVAL,DMCB,(5(R2),8(R2)),(0,BLOCK)                              
*                                                                               
         LA    R4,BLOCK            A(OUTPUT)                                    
         USING PERVALD,R4                                                       
         MVI   ERROR,INVDATE                                                    
         CLI   PVALASSM,0          2 FULL DATES ENTERED?                        
         BE    VK040               YES - SO CONTINUE                            
         CLI   PVALASSM,X'70'      ELSE - FULL END DATE ASSUMED?                
         BNE   TRAPERR             NO - SO ERROR                                
*                                                                               
VK040    MVI   ERROR,38            DATE OUTSIDE OF ESTIMATE PERIOD              
         CLC   PVALESTA,QPERFRST                                                
         BL    TRAPERR                                                          
         CLC   PVALEEND,QPERFRND                                                
         BH    TRAPERR                                                          
*                                                                               
         OC    ELOCKYM,ELOCKYM     ANY LOCK MONTH?                              
         BZ    VK049               NONE                                         
*                                                                               
         MVC   FULL(2),ELOCKYM                                                  
         NI    FULL+1,X'FF'-X'C0'  REMOVE SUSBSEQUENT AND PRIOR BITS            
         MVI   FULL+2,X'0F'                                                     
         GOTO1 DATCON,DMCB,(3,FULL),(0,WORK)                                    
*                                                                               
         XC    DMCB(4),DMCB        GET A(GETBROAD)                              
         MVC   DMCB+4(4),=X'D9000A1D'                                           
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(1,WORK),WORK+10,GETDAY,ADDAY                          
         CLI   DMCB,X'FF'          INVALID DATE??                               
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    ELOCKMON,X'C0'      SUBSEQUENT OR PRIOR BITS ON?                 
         BZ    VK043               NO, PLAIN MONTH                              
*                                                                               
         TM    ELOCKMON,X'40'      THE MONTH AND SUBSEQUENT?                    
         BZ    *+10                                                             
         MVC   WORK+16(6),QPERFRND USE EST END DATE AS LOCK END DATE            
*                                                                               
         TM    ELOCKMON,X'80'      THE MONTH AND PRIOR?                         
         BZ    *+10                                                             
         MVC   WORK+10(6),QPERFRST USE EST START DATE AS LOCK START             
*                                                                               
VK043    CLC   PVALEEND,WORK+10    END DATE BEFORE  START OF LOCK?              
         BL    VK049               YES, NOTHING TO WORRY ABOUT                  
         CLC   PVALESTA,WORK+16    NO, START DATE AFTER END OF LOCK?            
         BH    VK049                   YES                                      
         B     ERFESLCK                                                         
*                                                                               
VK049    MVC   BFRNMDWK,PVALNDYS   SAVE # OF DAYS AND WEEKS                     
         MVC   QPERFR,PVALESTA     SAVE DATE(S)                                 
         DROP  R4,R6                                                            
*                                                                               
VK050    GOTO1 DATCON,DMCB,QPERFRST,(3,BPERFRST)                                
         GOTO1 (RF),(R1),QPERFRND,(3,BPERFRND)                                  
         GOTO1 (RF),(R1),QPERFRST,(2,CPERFRST)                                  
         GOTO1 (RF),(R1),QPERFRND,(2,CPERFRND)                                  
*                                                                               
         XC    QMKT,QMKT                                                        
         XC    QMGR,QMGR                                                        
         XC    QSTA,QSTA                                                        
         MVI   STRQFLG1,0                                                       
         MVI   STALCNT,0           NO STATIONS IN LIST YET                      
*                                                                               
         LA    R2,BXFFRSTH         'FROM' STATION                               
         CLI   5(R2),0                                                          
         BNE   VK055                                                            
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
ERFESLCK LA    R2,BXFFRESH         'FROM' ESTIMATE                              
         MVC   HALF,=Y(1391)       EST LOCKED, SPOTS CAN'T BE MOVED             
         B     ERR2BYTE                                                         
*                                                                               
ERTESLCK LA    R2,BXFTOESH         'TO' ESTIMATE                                
         MVC   HALF,=Y(1391)       EST LOCKED, SPOTS CAN'T BE MOVED             
         B     ERR2BYTE                                                         
*                                                                               
VK055    XC    BLOCK(MAXOPTS*32),BLOCK                                          
         GOTO1 SCANNER,DMCB,(R2),(X'8F',BLOCK),0                                
*                                                                               
         MVI   ERROR,INVALID                                                    
         CLI   4(R1),0                                                          
         BE    TRAPERR                                                          
         LLC   R3,4(R1)                                                         
         LA    R4,BLOCK                                                         
*                                                                               
VK060    CLI   OFFLINE,C'Y'        OFFLINE PROCESS?                             
         BE    VK063               NO NEED TO SETUP CURSOR                      
*                                                                               
         L     RF,SYSPARMS         R4 = A(TIOB)                                 
         L     R1,0(RF)            SETUP ATIOB FOR ERRORS                       
         USING TIOBD,R1                                                         
         OI    TIOBINDS,TIOBSETC                                                
         LR    RE,R2                                                            
         SR    RE,RA                                                            
         STH   RE,TIOBCURD                                                      
         MVC   TIOBCURI,4(R4)      DISPL INTO 'FROM' STATION FIELD              
         DROP  R1                                                               
VK063    MVI   ERROR,INVSTAT                                                    
*                                                                               
         CLC   12(3,R4),=C'MGR='   DELIBERATE COMPARE OF 3 BYTES                
         BNE   VK065                                                            
         CLI   STRQFLG1,0          DO WE HAVE OTHER TYPES?                      
         BNE   ERMIXTYP            YES, MIXED TYPES VIOLATE REPORT OPT          
         MVC   FAKEFLDH,BXFFRSTH                                                
         SR    R1,R1                                                            
         ICM   R1,1,1(R4)                                                       
         BZ    TRAPERR             CAN'T JUST BE MGR=                           
         XC    FAKEFLD,FAKEFLD                                                  
         MVC   FAKEFLD(4),=C'MGR='                                              
         BCTR  R1,0                                                             
         EX    R1,VK60EXMV                                                      
*                                                                               
         LA    R1,5(R1)            LENGTH ADJUSTED FOR  MGR=                    
         STC   R1,FAKEFLDH+5(1)                                                 
         LA    R2,FAKEFLDH                                                      
         BRAS  RE,VALIMGR                                                       
         BNE   TRAPERR                                                          
         OI    STRQFLG1,SRQFMGRP   WE NOW HAVE A MARKET GROUP                   
         B     VK090               LOOP THROUGH ALL SCANNER ENTRIES             
*                                                                               
VK60EXMV MVC   FAKEFLD+4(0),22(R4)  COPY THE DIGITS                             
*                                                                               
VK065    TM    2(R4),X'80'         TEST VALID NUMERIC                           
         BZ    VK070                                                            
         CLI   STRQFLG1,0          DO WE HAVE OTHER TYPES?                      
         BNE   ERMIXTYP            YES, MIXED TYPES VIOLATE REPORT OPT          
         MVC   FAKEFLDH,BXFFRSTH                                                
         XC    FAKEFLD,FAKEFLD                                                  
         MVC   FAKEFLDH+5(1),0(R4) COPY THE LENGTH                              
         OI    FAKEFLDH+4,X'08'                                                 
         MVC   FAKEFLD(10),12(R4)  COPY THE DIGITS                              
         LA    R2,FAKEFLDH                                                      
         GOTO1 VALIMKT                                                          
         OI    STRQFLG1,SRQFMRKT   WE NOW HAVE A MARKET                         
         B     VK090                                                            
*                                                                               
ERMIXTYP MVC   HALF,=Y(330)        INVALID OPTION COMBO                         
         B     ERR2BYTE                                                         
*                                                                               
VK070    DS    0H                                                               
         CLC   =C'ALL',12(R4)                                                   
         BNE   VK080                                                            
         CLI   STRQFLG1,0          DO WE HAVE OTHER TYPES?                      
         BNE   ERMIXTYP            YES, MIXED TYPES VIOLATE REPORT OPT          
         OI    STRQFLG1,SRQFALL    WE NOW HAVE ALL STATIONS                     
         B     VK090                                                            
*                                                                               
VK080    TM    STRQFLG1,X'FF'-SRQFLIST  ERROR IF ANYTHING ELSE OTHER            
         BNZ   ERMIXTYP            YES, MIXED TYPES VIOLATE REPORT OPT          
         MVC   FAKEFLDH,BXFFRSTH                                                
         XC    FAKEFLD,FAKEFLD                                                  
         MVC   FAKEFLDH+5(1),0(R4) COPY THE LENGTH                              
         MVC   FAKEFLD(10),12(R4)  COPY THE CALL LETTERS                        
         LA    R2,FAKEFLDH                                                      
         GOTO1 VALISTA                                                          
*                                                                               
         L     R5,AIO              A(MASTER RECORD)                             
         USING STARECD,R5          MASTER RECORD DSECT                          
         TM    SFLAG1,SLOCK        STATION HAS VENDOR LOCK FLAG?                
         BNZ   VK240               YES                                          
         L     R2,AIO3                                                          
         LLC   R1,STALCNT                                                       
         MHI   R1,7                                                             
         AR    R2,R1                                                            
         GOTO1 MSPACK,DMCB,SMKT,STAKCALL,2(R2)  SKIP 1ST 2 BYTES AS IT          
         DROP  R5                                  IS FOR THE MGRNUM            
*                                                                               
         OI    STRQFLG1,SRQFLIST   COULD BE A LIST OF ONE STATION               
         LLC   R1,STALCNT          BUMP NUMBER OF STATIONS IN LIST              
         LA    R1,1(R1)                                                         
         STC   R1,STALCNT                                                       
*                                                                               
VK090    LA    R4,32(R4)           R4 = A(NEXT SCANNER ENTRY)                   
         LA    R2,BXFFRSTH         'FROM' STATION                               
         BCT   R3,VK060                                                         
***************                                                                 
***************                                                                 
***************                                                                 
VK120    CLI   OFFLINE,C'Y'        OFFLINE PROCESS?                             
         BE    VK123               DIDN'T SETUP CURSOR                          
         L     RF,SYSPARMS         DON'T SETUP CURSOR ANYMORE                   
         L     R1,0(RF)            OTHERWISE IT LEADS ONE TO THINK THE          
         USING TIOBD,R1            ERROR IS OCCURING IN STATION FIELD           
         NI    TIOBINDS,X'FF'-TIOBSETC                                          
         DROP  R1                                                               
*                                                                               
VK123    XC    FRKEY,FRKEY                                                      
         MVC   FRAGYMD,BAGYMD                                                   
         MVC   FRCLT,BCLT                                                       
         MVC   FRPRD,BPRD                                                       
         MVC   FRMKT,BMKTSTA                                                    
         MVC   FRSTA,BMKTSTA+L'FRMKT                                            
         MVC   FREST,BEST                                                       
*                                                                               
         LA    R2,BXFTOCLH         'TO' CLIENT                                  
         CLI   5(R2),0                                                          
         JE    VK130                                                            
         GOTO1 VALICLT                                                          
*                                                                               
         L     R6,AIO              PICK UP CLIENT RECORD ADDRESS                
         USING CLTHDRD,R6                                                       
         TM    COPT2,COP2FRZ       IS CLIENT FROZEN?                            
         BNZ   ERFRZCLT            YES                                          
         MVC   SVCOPT1,COPT1                                                    
         MVC   SVCOFFC,COFFICE                                                  
         DROP  R6                                                               
*                                                                               
         BRAS  RE,CMPADJCD                                                      
         BE    VK125                                                            
         MVC   HALF,=Y(1387)                                                    
         B     ERR2BYTE            ADJACENCY CODE CONFLICT                      
*                                                                               
ERFRZCLT MVC   HALF,=Y(1390)                                                    
         B     ERR2BYTE            CLT IS FROZEN, SPOT CAN'T BE MOVED           
*                                                                               
VK125    BRAS  RE,STPRDETB         SETUP PRD/EST TABLE                          
*                                                                               
VK130    MVC   TOCLI,QCLT          SAVE 'TO' CLIENT CODE AND NAME               
         MVC   TOCLIN,CLTNM                                                     
* READ B0 PROFILE                                                               
         XC    SVB0PROF,SVB0PROF      READ B0 PROFILE                           
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0B0'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),QMED                                                   
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCOFFC                                               
         GOTO1 GETPROF,DMCB,WORK,SVB0PROF,DATAMGR                               
*                                                                               
         MVI   SVPOLNPW,0                                                       
         CLI   SVCPROF+0,C'0'      TEST BRAND POOL CLIENT                       
         BE    VK135                                                            
         CLI   SVAPROF+11,C'Y'     TEST FOR AGENCY OPTION                       
         BNE   VK135                                                            
         CLI   QMED,C'R'           TEST FOR RADIO                               
         BNE   *+8                                                              
         MVI   SVPOLNPW,X'80'      TURN ON SWITCH                               
*                                                                               
VK135    LA    R2,BXFTOPRH         'TO' PRODUCT                                 
         CLI   5(R2),0                                                          
         BNE   VK140                                                            
         MVI   ERROR,MISSING                                                    
         CLC   BCLT,FRCLT          TEST FROM CLIENT=TO CLIENT                   
         BE    VK145               YES-DO NOT NEED PRODUCT VALIDATION           
         B     TRAPERR                                                          
*                                                                               
VK140    GOTO1 VALIPRD                                                          
         CLI   SVCPROF,C'0'        TEST BRAND POOL CLIENT                       
         BE    *+14                NO                                           
         CLC   =C'POL',WORK        TEST FOR POOL                                
         BE    TRAPERR             YES-REQUIRE A SPECIFIC BRAND                 
         MVC   QPRD(3),WORK                                                     
*                                                                               
VK145    MVC   TOPROD,QPRD                                                      
         MVC   TOPRODN,PRDNM                                                    
         LA    R2,BXFTOESH         'TO' ESTIMATE                                
         GOTO1 VALINUM                                                          
         MVC   BEST,ACTUAL                                                      
         BRAS  RE,GETEST                                                        
*                                                                               
         CLC   FREDYMNU,SVDAYMNU   DO WE HAVE A DIFFERENT DAYPART MENU?         
         BE    VK150                                                            
         MVC   HALF,=Y(1385)       DAYPART MENUS HAVE TO BE THE SAME            
         B     ERR2BYTE                                                         
*                                                                               
VK150    BRAS  RE,CMPDEMOS                                                      
         BE    VK155                                                            
         MVC   HALF,=Y(1386)       DEMO CATEGORY CONFLICT                       
         B     ERR2BYTE                                                         
*                                                                               
VK155    DS    0H                                                               
***      CLC   =C'POL',QPRD        'TO' A POL ESTIMATE?                         
***      BNE   VK160                                                            
         BRAS  RE,FILESTTB         YES, FILL WHAT PRDS HAVE THIS EST            
*                                                                               
VK160    L     RE,AIO                                                           
         USING ESTHDRD,RE                                                       
         TM    ECNTRL,X'0C'        TEST LOCKED/HELD                             
         BNZ   ERTESLCK                                                         
         DROP  RE                                                               
*                                                                               
         MVC   TOESTN,SVESTNAM                                                  
         MVC   TOESTPOL,POLESTSW                                                
         MVC   QPERTO,WORK                                                      
         BRAS  RE,DNAMES           PREPARE DEMO NAME OUTPUT                     
*                                                                               
VK170    LA    R2,BXFTODTH         EDIT 'TO' PERIOD                             
         CLI   5(R2),2             TEST INPUT LEN = 2                           
         BNE   VK173                                                            
         CLC   =C'ES',8(R2)                                                     
         BNE   VK180               USE 'TO' ESTIMATE'S START/END                
         GOTO1 DATCON,DMCB,(X'10',QPERTO),(17,8(R2))                            
         MVI   5(R2),17                                                         
         OI    6(R2),X'80'                                                      
*                                                                               
VK173    DS    0H                                                               
         L     R6,AIO                                                           
         USING ESTHDRD,R6                                                       
         OC    ELOCKYM,ELOCKYM     TEST LOCK DATE SPECIFIED                     
         BZ    VK176                                                            
         CLI   5(R2),0             ANY 'FROM' PERIOD?                           
         BE    ERTESLCK            EST LOCKED, SPOT CAN'T BE MOVED              
*                                                                               
VK176    CLI   5(R2),0             'TO' BLANK?                                  
         BE    VK210                                                            
*                                                                               
         GOTO1 PERVAL,DMCB,(5(R2),8(R2)),(0,BLOCK)                              
*                                                                               
         LA    R4,BLOCK            A(OUTPUT)                                    
         USING PERVALD,R4                                                       
         MVI   ERROR,INVDATE                                                    
         CLI   PVALASSM,0          2 FULL DATES ENTERED?                        
         BE    VK180               YES - SO CONTINUE                            
         CLI   PVALASSM,X'70'      ELSE - FULL END DATE ASSUMED?                
         BNE   TRAPERR             NO - SO ERROR                                
*                                                                               
VK180    MVI   ERROR,38            DATE OUTSIDE OF ESTIMATE PERIOD              
         CLC   PVALESTA,QPERTOST                                                
         BL    TRAPERR                                                          
         CLC   PVALEEND,QPERTOND                                                
         BH    TRAPERR                                                          
*                                                                               
         OC    ELOCKYM,ELOCKYM     ANY LOCK MONTH?                              
         BZ    VK200               NONE                                         
*                                                                               
         MVC   FULL(2),ELOCKYM                                                  
         NI    FULL+1,X'FF'-X'C0'  REMOVE SUSBSEQUENT AND PRIOR BITS            
         MVI   FULL+2,X'0F'                                                     
*                                                                               
         GOTO1 DATCON,DMCB,(3,FULL),(0,WORK)                                    
*                                                                               
         XC    DMCB(4),DMCB        GET A(GETBROAD)                              
         MVC   DMCB+4(4),=X'D9000A1D'                                           
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(1,WORK),WORK+10,GETDAY,ADDAY                          
         CLI   DMCB,X'FF'          INVALID DATE??                               
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    ELOCKMON,X'C0'      SUBSEQUENT OR PRIOR BITS ON?                 
         BZ    VK190               NO, PLAIN MONTH                              
*                                                                               
         TM    ELOCKMON,X'40'      THE MONTH AND SUBSEQUENT?                    
         BZ    *+10                                                             
         MVC   WORK+16(6),QPERFRND USE EST END DATE AS LOCK END DATE            
*                                                                               
         TM    ELOCKMON,X'80'      THE MONTH AND PRIOR?                         
         BZ    *+10                                                             
         MVC   WORK+10(6),QPERFRST USE EST START DATE AS LOCK START             
*                                                                               
VK190    CLC   PVALEEND,WORK+10    END DATE BEFORE  START OF LOCK?              
         BL    VK200               YES, NOTHING TO WORRY ABOUT                  
         CLC   PVALESTA,WORK+16    NO, START DATE AFTER END OF LOCK?            
         BH    VK200                   YES                                      
         B     ERTESLCK                                                         
*                                                                               
VK200    MVC   BTONMDWK,PVALNDYS                                                
         MVC   QPERTO,PVALESTA     SAVE DATE(S)                                 
         DROP  R4,R6                                                            
*                                                                               
VK210    GOTO1 DATCON,DMCB,QPERTOST,(3,BPERTOST)                                
         GOTO1 (RF),(R1),QPERTOND,(3,BPERTOND)                                  
         GOTO1 (RF),(R1),QPERTOST,(2,CPERTOST)                                  
         GOTO1 (RF),(R1),QPERTOND,(2,CPERTOND)                                  
*                                                                               
         XC    DYSDIFFR,DYSDIFFR                                                
*&&DO                                                                           
         CLI   BXFOPT2,C'Y'        TEST DATES=SAME                              
         BE    VK220               YES - SKIP BACKWARDS IN TIME TEST            
         CLI   BXFOPT2,C'B'        TEST DATES=BACKWARDS (SFWESTERN)             
         BE    VK220               YES - SKIP BACKWARDS IN TIME TEST            
         MVI   ERROR,BADPER                                                     
         CLC   QPERFR,QPERTO                                                    
         BH    TRAPERR                                                          
*&&                                                                             
* CAN ONLY COPY FROM ONE MARKET GROUP TO SAME MARKET GROUP, AND                 
* ONE MARKET TO SAME MARKET, OR ALL MARKETS.  ONLY EXCEPTION IS                 
* COPY FROM ONE STATION TO A DIFFERENT STATION                                  
*                                                                               
VK220    LA    R2,BXFTOSTH         'TO' STATION                                 
         XC    BMKTSTA,BMKTSTA                                                  
         CLI   5(R2),0             ANY "TO" STATION?                            
         BE    VK310               NO                                           
*                                                                               
         TM    STRQFLG1,SRQFLIST   DEALING WITH A LIST OF STATIONS?             
         BZ    VK230               NO                                           
         CLI   STALCNT,1           DO WE HAVE MORE THAN ONE STATION?            
         BH    ERMIXTYP            YES, CAN'T HAVE MULTI TO ONE                 
*                                                                               
VK230    MVI   ERROR,INVALID                                                    
         OC    QSTA,QSTA           MUST HAVE 'FROM' STATION (NOT ALL)           
         BZ    TRAPERR                                                          
         TM    4(R2),X'08'         NUMERIC IS MARKET (NOT ALLOWED)              
         BO    TRAPERR                                                          
         GOTO1 VALISTA                                                          
*                                                                               
         L     RE,AIO              A(MASTER RECORD)                             
         USING STARECD,RE          MASTER RECORD DSECT                          
         TM    SFLAG1,SLOCK        STATION HAS VENDOR LOCK FLAG?                
         BZ    VK250               NO                                           
VK240    MVC   HALF,=Y(1388)       YES - ERROR                                  
         XC    BLOCK,BLOCK                                                      
         MVI   BLOCK,L'QSTA+1                                                   
         MVC   BLOCK+1(L'QSTA),QSTA                                             
         B     ERR2TEXT            GO REPORT STA &1 LOCKED ERROR MSG            
         DROP  RE                  DROP RE                                      
*                                                                               
VK250    CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BNE   VK310                                                            
*                                                                               
         L     RE,AIO1                                                          
         USING STARECD,RE                                                       
         LA    RF,500(RE)                                                       
         USING MKTRECD,RF                                                       
*                                                                               
         MVC   SVDMLMKT,MKTALST    SET ALPHA MARKET CODE                        
         MVC   SVDMLRSV,MKTRSVC    SET RATING SERVICE                           
*                                                                               
         MVC   SVDMLST0,SRS1CALL                                                
         MVC   SVDMLST1,SRS2CALL                                                
*                                                                               
         LHI   R1,SQNORS1I         SET FOR STATION 1 IMPS FLAG                  
         MVI   SVDMLFLG,2          SET NSI LOOKUP FLAG                          
         CLI   MKTRSVC,C'0'        TEST NSI MARKET                              
         BE    *+12                                                             
         LHI   R1,SQNORS2I         SET FOR STATION 2 IMPS FLAG                  
         MVI   SVDMLFLG,1          SET BBM LOOKUP FLAG                          
*                                                                               
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    SFLAG1,0 ** EXECUTED **                                          
         BZ    *+8                                                              
         OI    SVDMLFLG,X'80'      SET TO SUPPRESS IMPS                         
         DROP  RE,RF                                                            
*                                                                               
VK310    XC    TOKEY,TOKEY                                                      
         MVC   TOAGYMD,BAGYMD                                                   
         MVC   TOCLT,BCLT                                                       
         MVC   TOPRD,BPRD                                                       
         MVC   TOMKT,BMKTSTA                                                    
         MVC   TOSTA,BMKTSTA+L'TOMKT                                            
         MVC   TOEST,BEST                                                       
*                                                                               
         LA    R2,BXFTOCLH                                                      
         MVI   ERROR,SAMEMOVE                                                   
         CLC   FRKEY,TOKEY         TEST SAME MOVING DETAILS                     
         BE    TRAPERR             YES                                          
         MVI   ERROR,BADMOVE                                                    
         CLC   FRESTPOL,TOESTPOL   TEST MOVING POOL TO NON POOL                 
         BNE   TRAPERR             YES                                          
*                                                                               
         MVI   ERROR,BADCOPY                                                    
         CLC   FRCLI,TOCLI         TEST SAME CLIENT                             
         BNE   VK320                                                            
         CLI   FRESTPOL,C'Y'       TEST COPYING FROM POL EST                    
         BNE   VK320                                                            
         CLC   FREST,TOEST         TEST COPY ON SAME EST                        
         BNE   VK320               NO - OK                                      
         CLC   FRSTA,TOSTA         TEST COPY TO SAME STATION                    
         BE    TRAPERR                                                          
*                                                                               
VK320    MVI   DATEOPT,C'Y'        DEFAULT OPTION IS NOW Y                      
         MVI   DEMOOPT,C'Y'          THESE FIELDS WERE IN BUY/TRANSFER          
         MVI   CMTOPT,C'Y'                                                      
*                                                                               
         MVI   ERROR,INVALID                                                    
VK340    LA    R2,BXFOPT5H         DATE SHIFT FIELD                             
         CLI   5(R2),0                                                          
         BE    VK370                                                            
         CLI   8(R2),C'Y'                                                       
         BE    VK350                                                            
         CLI   8(R2),C'N'                                                       
         BE    VK370                                                            
         B     TRAPERR                                                          
*                                                                               
VK350    MVI   ERROR,MISSING                                                    
         CLI   BXFFRDTH+5,0        DATE SHIFT IS REQUESTED, "FROM" AND          
         BNE   *+12                  "TO" PERIODS ARE NEEDED                    
         LA    R2,BXFFRDTH           AND SHOULD HAVE THE SAME # OF WKS          
         B     TRAPERR                                                          
*                                                                               
         CLI   BXFTODTH+5,0                                                     
         BNE   *+12                                                             
         LA    R2,BXFTODTH                                                      
         B     TRAPERR                                                          
*                                                                               
VK353    CLC   BFRNMDWK,BTONMDWK                                                
         BE    VK356                                                            
         MVC   HALF,=Y(1389)       # OF WKS IN "FROM" AND "TO" PERIODS          
         LA    R2,BXFTODTH            MUST BE EQUAL                             
         B     ERR2BYTE            ERROR OTHERWISE                              
*                                                                               
VK356    XC    WORK,WORK                                                        
         MVI   WORK+10,C'-'                                                     
         GOTO1 DATCON,DMCB,(0,QPERFRST),(21,WORK)  MMMDD/YYYY                   
         GOTO1 DATCON,DMCB,(0,QPERTOST),(21,WORK+11)  MMMDD/YYYY                
         CLC   QPERFRST,QPERTOST                                                
         BNH   VK360                                                            
         MVC   WORK+11(10),WORK                                                 
         GOTO1 DATCON,DMCB,(0,QPERTOST),(21,WORK)  SWAP FROM AND TO             
VK360    GOTO1 PERVAL,DMCB,(21,WORK),(0,BLOCK)                                  
*                                                                               
         LA    R6,BLOCK            A(OUTPUT)                                    
         USING PERVALD,R6                                                       
         XR    R1,R1               NUMBER OF DAYS IS INCLUSIVE                  
         ICM   R1,3,PVALNDYS                                                    
         BCTR  R1,0                LESS ONE = THE DIFFERENCE IN DAYS            
*                                                                               
         CLC   QPERFRST,QPERTOST                                                
         BNH   *+6                                                              
         LNR   R1,R1               NEGATIVE DAYS TO GO BACK IN TIME             
         ST    R1,DYSDIFFR                                                      
         DROP  R6                                                               
*                                                                               
VK370    DS    0H                                                               
*&&DO                                                                           
         CLI   5(R2),0                                                          
         BE    VK380                                                            
         MVC   CMTOPT,8(R2)                                                     
         CLI   CMTOPT,C'Y'                                                      
         BE    VK380                                                            
         CLI   CMTOPT,C'N'                                                      
         BE    VK380                                                            
         B     TRAPERR                                                          
*                                                                               
VK380    LA    R2,BXFOPT3H                                                      
         CLI   5(R2),0                                                          
         BE    VK400                                                            
         MVC   DEMOOPT,8(R2)                                                    
         CLI   DEMOOPT,C'N'                                                     
         BE    VK400                                                            
         CLI   DEMOOPT,C'Y'                                                     
         BNE   TRAPERR                                                          
*                                                                               
         CLI   QMED,C'T'           TEST FOR TV                                  
         BNE   VK400                                                            
         CLI   SVCXTRA+5,C'Y'      TEST US SPILL                                
         BE    VK390               YES                                          
         CLI   SVCXTRA+5,C'D'      TEST US DPT SUMMARY SPILL                    
         BE    VK390               YES                                          
         CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BNE   VK400               NO                                           
*                                                                               
VK390    OC    TOMKT(L'TOMKT+L'TOSTA),TOMKT  TEST STATION-STATION COPY          
         BZ    VK400               NO                                           
         MVI   ERROR,CPYSPILL      CANNOT COPY SPILL DEMOS TO NEW STA           
         CLC   TOMKT(L'TOMKT+L'TOSTA),FRMKT TEST COPY TO NEW STA                
         BNE   TRAPERR             YES                                          
*&&                                                                             
VK400    DS    0H                                                               
*                                                                               
* IF SAME DATES OPT, MAKE SURE ESTIMATES OVERLAP *                              
         MVI   ERROR,NOOVERLP                                                   
         CLC   QPERTOST,QPERFRND   DO WE HAVE AN INTERSECTION?                  
         BH    VK430                                                            
         CLC   QPERTOND,QPERFRST                                                
         BL    VK430                                                            
VK420    MVI   ERROR,POLNPWR       BRAND POOL RADIO NPW CONFLICT                
         CLC   SVPOLNPW,FRPOLNPW                                                
         BE    VK440                                                            
         B     TRAPERR                                                          
*                                                                               
VK430    CLI   BXFOPT5,C'Y'        DATE SHIFT?                                  
         BE    VK420                                                            
         B     TRAPERR                                                          
*                                                                               
VK440    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
* SUBROUTINE TO VALIDATE FREE-FORM OPTIONS *                                    
         SPACE 1                                                                
VALOPT   NTR1                                                                   
         MVI   TRACEOPT,0                                                       
         MVI   TESTOPT,0                                                        
         CLI   5(R2),0             TEST FOR INPUT                               
         BE    VALOPTX             NONE                                         
         LA    R0,MAXOPTS                                                       
         XC    BLOCK(MAXOPTS*32),BLOCK                                          
         GOTO1 SCANNER,DMCB,(R2),((R0),BLOCK)                                   
*                                                                               
         MVI   ERROR,INVALID                                                    
         CLI   4(R1),0                                                          
         BE    TRAPERR                                                          
         LLC   R3,4(R1)                                                         
         LA    R4,BLOCK                                                         
*                                                                               
VALOPT2  CLC   12(5,R4),=C'TRACE'                                               
         BNE   VALOPT4                                                          
         CLI   22(R4),C'Y'                                                      
         BNE   *+12                                                             
         MVI   TRACEOPT,C'Y'                                                    
         B     VALOPT10                                                         
         CLI   22(R4),C'N'                                                      
         BE    VALOPT10                                                         
         B     TRAPERR                                                          
*                                                                               
* NOTE: IT IS BETTER TO USE THE WRITE=NO CARD THAN THE TEST OPTION              
*       IF YOU WANT A TRACE! (EJOR 8/10/07)                                     
VALOPT4  DS    0H                                                               
         CLC   12(4,R4),=C'TEST'                                                
         BNE   TRAPERR                                                          
         MVI   TESTOPT,C'Y'                                                     
*                                                                               
VALOPT10 LA    R4,32(R4)                                                        
         BCT   R3,VALOPT2                                                       
*                                                                               
VALOPTX  B     EXIT                                                             
         SPACE 2                                                                
MAXOPTS  EQU   8                                                                
         EJECT                                                                  
******************************************                                      
* SUBROUTINE TO READ ESTIMATE HEADER     *                                      
* ON EXIT ESTHDR IS IN IOAREA1 AND       *                                      
* POLESTSW IS SET IF POL ESTIMATE OPEN   *                                      
******************************************                                      
         SPACE 1                                                                
GETEST   NTR1                                                                   
*                                                                               
         LA    R4,KEY                                                           
         USING ESTHDRD,R4                                                       
         XC    KEY,KEY                                                          
         MVC   EKEYAM,BAGYMD                                                    
         MVC   EKEYCLT,BCLT                                                     
         MVC   EKEYPRD,QPRD                                                     
         MVC   EKEYEST,BEST                                                     
         BRAS  RE,SPHIGH                                                        
         MVI   ERROR,INVEST                                                     
         CLC   EKEY,KEYSAVE                                                     
         BNE   TRAPERR                                                          
*                                                                               
         MVC   AIO,AIO1                                                         
         BRAS  RE,SPGET                                                         
         L     R4,AIO                                                           
         MVC   SVESTNAM,EDESC      EXTRACT ESTIMATE VALUES                      
         MVC   SVESTREP,EREP                                                    
         MVC   SVBOOK,EBOOK                                                     
         MVC   SVHUTADJ,EHUTADJ                                                 
         MVC   SVWGTLST,EWGTLST                                                 
         MVC   SVDAYMNU,EDAYMENU                                                
         XC    SVDEMOS,SVDEMOS                                                  
         MVC   SVDEMOS(60),EDEMOS                                               
         MVC   WORK(12),ESTART                                                  
         LA    R4,KEY              RESTORE POINTER TO KEY                       
*                                                                               
         MVI   POLESTSW,C'Y'       PRESET                                       
         CLC   EKEYPRD,=C'POL'                                                  
         BE    EXIT                                                             
*                                                                               
         MVC   EKEYPRD,=C'POL'     TEST FOR POL ESTIMATE OPEN                   
         BRAS  RE,SPHIGH                                                        
         CLC   EKEY,KEYSAVE                                                     
         BE    *+8                                                              
         MVI   POLESTSW,C'N'                                                    
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
* SUB-ROUTINE TO PREPARE DEMO NAMES FOR OUTPUT ON REPORT                        
*                                                                               
DNAMES   NTR1                                                                   
         XC    WORK,WORK           CLEAR AREA FOR DEMO LIST                     
         LA    R0,4                MAXIMUM DEMOS ON REPORT                      
         SR    R1,R1               R1=COUNTER                                   
         LA    RE,WORK                                                          
         LA    RF,SVDEMOS          RF=A(EST HEADER DEMO LIST)                   
         MVC   SVDEMNAM,SPACES     PRE-CLEAR DEMO NAMES                         
*                                                                               
DNAMES2  CLI   1(RF),0             TEST FOR EOL                                 
         BE    DNAMES4                                                          
         MVC   0(3,RE),0(RF)       COPY DEMO OVER                               
         LA    RE,3(RE)                                                         
         LA    RF,3(RF)                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,DNAMES2                                                       
*                                                                               
DNAMES4  MVI   0(RE),X'FF'         SET EOL MARKER                               
         STC   R1,SVNDEMS          SAVE N'DEMOS                                 
         LTR   R1,R1               TEST FOR ANY DEMOS (RADIO)                   
         BZ    DNAMESX             NO                                           
         XC    DMCB(4),DMCB        GET A(DEMOCON)                               
         MVC   DMCB+4(4),=X'D9000AE0'                                           
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VDEMOCON,DMCB                                                    
*                                                                               
DNAMES6  XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
         CLI   SVAPROF+7,C'C'      TEST FOR CANADIAN AGENCY AND CLIENT          
         BNE   *+16                                                             
         CLI   SVCXTRA,C'U'                                                     
         BE    *+8                                                              
         MVI   DBSELMED,C'C'                                                    
         LLC   R2,SVNDEMS                                                       
         LA    R3,SVDEMNAM                                                      
         LA    R4,WORK                                                          
*                                                                               
DNAMES8  GOTO1 VDEMOCON,DMCB,(1,0(R4)),(2,0(R3)),(C'S',DBLOCK),AIO              
         LA    R3,8(R3)            NEXT OUTPUT AREA                             
         LA    R4,3(R4)            NEXT DEMO VALUE                              
         BCT   R2,DNAMES8                                                       
*                                                                               
DNAMESX  B     EXIT                                                             
         EJECT                                                                  
****************************                                                    
* PROCESS BUY RECORDS *                                                         
****************************                                                    
*  SOON JCL WILL NOT RUN PROPERLY IF THERE IS A WIDTH=165 CARD                  
****************************                                                    
         SPACE 1                                                                
PR       DS    0H                                                               
         BRAS  RE,PRNTMODE                                                      
         B     EXIT                                                             
* SUBROUTINE TO TEST ID PRESENT IF REQUIRED *                                   
         SPACE 1                                                                
TESTID   BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO READ STATION MARKET RECORD AND GET MARKET ZONE                 
*                                                                               
NEXTEL   SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         JZ    NEXTELD0                                                         
         AR    R6,R0                                                            
NEXTEL2  CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         CLC   0(1,R6),ELCDLO                                                   
         JL    NEXTEL                                                           
         CLC   0(1,R6),ELCDHI                                                   
         JH    NEXTEL                                                           
         CR    RE,RE               SET CC EQUAL AND EXIT                        
         BR    RE                                                               
NEXTELX  LTR   RE,RE               SET CC NOT EQUAL                             
         BR    RE                                                               
NEXTELD0 DC    H'0'                                                             
         EJECT                                                                  
* SUBROUTINE TO READ MKTGRP RECORD FOR ENTRY AT 0(R4)                           
* AND EXTRACT CODES AND NAMES                                                   
         SPACE 1                                                                
GETMGR   NTR1                                                                   
         CLC   SVMGRKEY+9(2),0(R4)                                              
         BE    EXIT                                                             
         MVC   SVMGRKEY+9(2),0(R4)                                              
         BRAS  RE,SPHIGH                                                        
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         BRAS  RE,EXTMGR                                                        
         B     EXIT                                                             
         SPACE 2                                                                
* ROUTINE EXTRACTS MKTGRP TITLES/BREAK LENGTHS FROM MGRDEF RECORD               
* OR CODES/NAMES FROM MKTGRP RECORDS                                            
         SPACE 1                                                                
EXTMGR   NTR1                                                                   
*                                                                               
         L     R6,AIO                                                           
         USING MKGRECD,R6                                                       
*                                                                               
         MVC   WORK(1),MKGKMID                                                  
         UNPK  WORK+1(5),MKGKMGRP(3)                                            
*                                                                               
         LA    R6,24(R6)                                                        
*                                                                               
EXTMG2   CLI   0(R6),X'01'                                                      
         BNE   EXTMG10                                                          
         USING MKGEL01,R6                                                       
         MVC   MGR1BK,MKGBK1       BREAK NAMES                                  
         MVC   MGR2BK,MKGBK2                                                    
         MVC   MGR3BK,MKGBK3                                                    
* SET BREAK LENGTHS                                                             
         SR    R4,R4                                                            
         SR    R5,R5                                                            
         IC    R4,MKGBK1LN                                                      
         STC   R4,MGR1LEN                                                       
         IC    R5,MKGBK2LN                                                      
         AR    R4,R5                                                            
         STC   R4,MGR2LEN                                                       
         IC    R5,MKGBK3LN                                                      
         AR    R4,R5                                                            
         STC   R4,MGR3LEN                                                       
         B     EXTMGX                                                           
         SPACE 1                                                                
EXTMG10  CLI   0(R6),X'10'                                                      
         BNE   EXTMG14                                                          
         USING MKGEL10,R6                                                       
*                                                                               
         MVC   MGR1NM,MKGNAM1      GROUP NAMES                                  
         MVC   MGR2NM,MKGNAM2                                                   
         MVC   MGR3NM,MKGNAM3                                                   
*                                                                               
EXTMG12  MVC   MGR1,SPACES                                                      
         LA    R1,MGR1                                                          
         LLC   RE,MGR1LEN                                                       
         EX    RE,EXTMGMVC                                                      
*                                                                               
         MVC   MGR2,SPACES                                                      
         LA    R1,MGR2                                                          
         LLC   RE,MGR2LEN                                                       
         EX    RE,EXTMGMVC                                                      
*                                                                               
         MVC   MGR3,SPACES                                                      
         LA    R1,MGR3                                                          
         LLC   RE,MGR3LEN                                                       
         EX    RE,EXTMGMVC                                                      
         SPACE 2                                                                
EXTMG14  SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   EXTMG2                                                           
***MGX   XIT1                                                                   
EXTMGX   B     EXIT                                                             
         SPACE 1                                                                
EXTMGMVC MVC   0(0,R1),WORK                                                     
         EJECT                                                                  
*                                                                               
* COMMON SUB-ROUTINE FOR IO TO PERMIT GLOBAL TRACING                            
*                                                                               
         USING GETBUYD,GETBLK      GETBUY DSECT                                 
SPHIGH   NTR1  BASE=MYBASE1,LABEL=NO                                            
         L     R7,MYBASE2                                                       
         MVC   KEYSAVE,KEY                                                      
         MVC   COMMAND,=C'DMRDHI'                                               
         MVC   DMFILE,=C'SPTDIR'                                                
         CLI   TRACEOPT,C'Y'                                                    
         BNE   SPDIR                                                            
* BUILD TRACE PARAMS                                                            
         LA    R0,KEYSAVE                                                       
         ST    R0,TRIO1                                                         
         MVI   TRIO1,13                                                         
         LA    R0,KEY                                                           
         ST    R0,TRIO2                                                         
         MVI   TRIO2,18                                                         
         B     SPDIR                                                            
*                                                                               
SPSEQ    NTR1  BASE=MYBASE1,LABEL=NO                                            
         L     R7,MYBASE2                                                       
         MVC   COMMAND,=C'DMRSEQ'                                               
         MVC   DMFILE,=C'SPTDIR'                                                
         CLI   TRACEOPT,C'Y'                                                    
         BNE   SPDIR                                                            
         LA    R0,KEY                                                           
         ST    R0,TRIO1                                                         
         MVI   TRIO1,13                                                         
         LA    R0,KEY                                                           
         ST    R0,TRIO2                                                         
         MVI   TRIO2,18                                                         
         B     SPDIR                                                            
*                                                                               
SPDIR    CLI   KEY,X'10'           BUY RECORD?                                  
         BL    SPDIR10             NO                                           
         LA    RE,KEYSAVE          RE = KEYSAVE                                 
         ST    RE,GBYKEYIN         STORE A(KEYSAVE)                             
         LA    RE,KEY              RE = KEY                                     
         ST    RE,GBYKEYOT         STORE A(KEY)                                 
*                                                                               
         MVI   GBYACT,GBYHIGH      READ HIGH                                    
         CLC   COMMAND,=C'DMRSEQ'  READ SEQ?                                    
         BNE   *+8                 NO                                           
         MVI   GBYACT,GBYSEQ       YES - READ SEQ                               
*                                                                               
         MVC   KEYSAVE,KEY         SAVE OFF THE KEY                             
         MVC   GBYDMIN,DMINBTS     PASS DMINBTS                                 
         MVC   GBYDMOUT,DMOUTBTS   PASS DMOUTBTS                                
         GOTO1 VGETBUY,GETBLK      CALL GETBUY                                  
         B     SPDIR15             TRACE AND/OR EXIT                            
*                                                                               
SPDIR10  GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),DMFILE,KEY,KEY                    
*                                                                               
SPDIR15  CLI   TRACEOPT,C'Y'                                                    
         BNE   EXIT                                                             
         B     SPTRACE                                                          
*                                                                               
SPGET    NTR1  BASE=MYBASE1,LABEL=NO                                            
         L     R7,MYBASE2                                                       
         MVC   COMMAND,=C'GETREC'                                               
         B     SPFILE                                                           
*                                                                               
SPPUT    NTR1  BASE=MYBASE1,LABEL=NO                                            
         L     R7,MYBASE2                                                       
         MVC   COMMAND,=C'PUTREC'                                               
         B     SPFILE                                                           
*                                                                               
SPADD    NTR1  BASE=MYBASE1,LABEL=NO                                            
         L     R7,MYBASE2                                                       
         L     RF,AIO              IF ADDING A BUYREC,                          
         CLI   0(RF),X'10'          MAKE SURE DTOP ADD FLAG IS OFF!             
         BL    *+8                                                              
         NI    BDSTAT3-BUYREC(RF),X'FF'-BDST3_DSKADD                            
*                                                                               
         MVC   COMMAND,=C'ADDREC'                                               
         L     RF,TWAMASTC         HONOR WRITE=N (EJOR 8/10/07)                 
         CLI   MCWRITE-MASTD(RF),C'N'                                           
         BNE   SPFILE                                                           
** GET PARMS RIGHT FOR SPTRACE IF WRITE=N!                                      
         LA    RF,COMMAND                                                       
         ST    RF,DMCB                                                          
         LA    RF,=C'SPTFILE'                                                   
         ST    RF,DMCB+4                                                        
         B     SPFIL15                                                          
*                                                                               
SPFILE   CLI   KEY,X'10'           BUY RECORD?                                  
         BL    SPFIL10             NO                                           
*                                                                               
         LA    RE,KEY+14           A(DISK ADDRESS)                              
         ST    RE,GBYDA            STORE A(DISK ADDRESS)                        
         MVC   GBYIOA,AIO1         A(BUY RECORD) IN AIO1                        
***                                                                             
***      MVI   GBYACT,GBYADD                                                    
***      CLC   COMMAND,=C'ADDREC'  ADDREC?                                      
***      BE    SPFIL05             YES                                          
***      MVI   GBYACT,GBYGET                                                    
***      CLC   COMMAND,=C'GETREC'  GETREC?                                      
***      BE    SPFIL05             YES                                          
***      MVI   GBYACT,GBYPUT       NO - PUTREC                                  
*** COMMENTING OUT THE ABOVE AS THE FIRST CHARACTER OF COMMAND WORKS            
         MVC   GBYACT,COMMAND                                                   
*                                                                               
SPFIL05  MVC   GBYDMIN,DMINBTS     PASS DMINBTS                                 
         MVC   GBYDMOUT,DMOUTBTS   PASS DMOUTBTS                                
         GOTO1 VGETBUY,GETBLK      CALL GETBUY                                  
         B     SPFIL15             TRACE AND/OR EXIT                            
*                                                                               
SPFIL10  GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),=C'SPTFILE',             X        
               KEY+14,AIO,DMWORK                                                
         TM    DMCB+8,X'FF'-X'02'  DELETED REC IS OK!                           
         BZ    *+6                                                              
         DCHO                                                                   
*                                                                               
SPFIL15  CLI   TRACEOPT,C'Y'                                                    
         BNE   EXIT                                                             
         LA    R0,KEY+14                                                        
         ST    R0,TRIO1                                                         
         MVI   TRIO1,4                                                          
         L     R0,AIO                                                           
         ST    R0,TRIO2                                                         
         MVI   TRIO2,16                                                         
         B     SPTRACE                                                          
         EJECT                                                                  
SPTRACE  DS    0H                                                               
         L     RE,DMCB                                                          
         MVC   P(6),0(RE)                                                       
         L     RE,DMCB+4                                                        
         MVC   P+8(6),0(RE)                                                     
*                                                                               
         LA    R4,P+16                                                          
         LLC   R0,TRIO1                                                         
         GOTO1 HEXOUT,DMCB,TRIO1,(R4),(R0),=C'TOG'                              
         A     R4,DMCB+16                                                       
         LA    R4,2(R4)                                                         
*                                                                               
         LLC   R0,TRIO2            GET OUTPUT REC LEN                           
         GOTO1 HEXOUT,DMCB,TRIO2,(R4),(R0),=C'TOG'                              
*                                                                               
         BRAS  RE,PRNTIT                                                        
         B     EXIT                                                             
         EJECT                                                                  
* DO NOT USE DMCB IN HERE IDIOT!                                                
*                                                                               
ERR2BYTE OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,HALF                                                     
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,2                                                         
         DROP  RF                                                               
         GOTO1 ERREX                                                            
*                                                                               
ERR2TEXT OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,HALF                                                     
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,2                                                         
         LA    RE,BLOCK                                                         
         STCM  RE,7,GTASUBST                                                    
         DROP  RF                                                               
         GOTO1 ERREX                                                            
*                                                                               
TRAPERR  L     RD,SAVERD                                                        
         GOTO1 ERREX                                                            
*                                                                               
EQXIT    CR    RB,RB                                                            
         B     EXIT                                                             
*                                                                               
NEQXIT   LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
         EJECT                                                                  
HDHK     NTR1                                                                   
*                                                                               
         MVC   H1+17(1),QMED        MEDIA                                       
         MVC   H2+17(3),FRCLI      FROM CLIENT CODE                             
         MVC   H2+21(20),FRCLIN                                                 
         MVC   H3+17(3),FRPROD     FROM PRODUCT CODE AND NAME                   
         MVC   H3+21(20),FRPRODN                                                
         LLC   R0,FREST                                                         
         EDIT  (R0),(3,H4+17),ALIGN=LEFT                                        
         MVC   H4+21(20),FRESTN                                                 
*                                                                               
         MVC   H5+17(3),TOCLI      TO CLIENT CODE AND NAME                      
         MVC   H5+21(20),TOCLIN                                                 
         MVC   H6+17(3),TOPROD                                                  
         MVC   H6+21(20),TOPRODN                                                
         LLC   R0,TOEST                                                         
         EDIT  (R0),(3,H7+17),ALIGN=LEFT                                        
         MVC   H7+21(20),TOESTN                                                 
*                                                                               
         MVC   H6+88(8),THISSTA                                                 
         CLI   THISSTA+4,C'A'                                                   
         BNE   *+14                                                             
         MVC   H6+92(3),=C'-AM'                                                 
         B     HDHK2                                                            
*                                                                               
         CLI   THISSTA+4,C'F'                                                   
         BNE   *+14                                                             
         MVC   H6+92(3),=C'-FM'                                                 
         B     HDHK2                                                            
*                                                                               
         CLI   THISSTA,C'0'        CABLE BE NUMERIC                             
         BNL   *+10                                                             
         MVC   H6+92(3),=C'-TV'                                                 
*                                                                               
HDHK2    GOTO1 MSUNPK,PARAS,(X'80',TOKEYSV+4),WORK,WORK+4                       
         MVC   H7+88(8),WORK+4                                                  
         CLI   H7+88,C'0'         CABLE BE NUMERIC                              
         BL    *+12                                                             
         MVI   H7+92,C'/'                                                       
         B     HDHK4                                                            
*                                                                               
         MVC   H7+92(3),=C'-AM'                                                 
         CLI   WORK+8,C'A'                                                      
         BE    HDHK4                                                            
         MVC   H7+92(3),=C'-FM'                                                 
         CLI   WORK+8,C'F'                                                      
         BE    HDHK4                                                            
         MVC   H7+92(3),=C'-TV'                                                 
*                                                                               
HDHK4    LA    R3,H9+(PRDEM1-PRD)  INDEX TO POSITION FOR DEMOS                  
         MVC   0(L'SVDEMNAM,R3),SVDEMNAM DEMO NAMES                             
         LA    R0,L'SVDEMNAM                                                    
         GOTO1 UNDERLIN,PARAS,((R0),(R3)),132(R3)                               
*                                                                               
HDHKX    B     EXIT                                                             
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* PRINT SPEC POOL                                                               
*                                                                               
HDSPECS  DS    0D                                                               
         SSPEC H1,3,C'MEDIA'                                                    
         SSPEC H1,47,C'BUY MOVE REPORT'                                         
         SSPEC H2,47,C'---------------'                                         
         SSPEC H1,75,AGYNAME                                                    
         SSPEC H2,75,AGYADD                                                     
         SSPEC H2,3,C'FROM CLIENT'                                              
         SSPEC H3,3,C'FROM PRODUCT'                                             
         SSPEC H4,3,C'FROM ESTIMATE'                                            
         SSPEC H5,3,C'TO CLIENT'                                                
         SSPEC H6,3,C'TO PRODUCT'                                               
         SSPEC H7,3,C'TO ESTIMATE'                                              
         SSPEC H4,75,REPORT                                                     
         SSPEC H4,87,RUN                                                        
         SSPEC H5,75,REQUESTOR                                                  
         SSPEC H5,100,PAGE                                                      
         SSPEC H6,75,C'FROM STATION'                                            
         SSPEC H7,75,C'TO STATION'                                              
*                                                                               
         SSPEC H9,2,C'LINE'                                                     
         SSPEC H10,2,C'----'                                                    
         SSPEC H9,11,C'BUY PERIOD'                                              
         SSPEC H10,8,C'-----------------'                                       
         SSPEC H9,27,C'WKS'                                                     
         SSPEC H10,27,C'---'                                                    
         SSPEC H9,33,C'DAY'                                                     
         SSPEC H10,32,C'-----'                                                  
         SSPEC H9,42,C'TIME'                                                    
         SSPEC H10,39,C'-----------'                                            
         SSPEC H9,52,C'NPW'                                                     
         SSPEC H10,52,C'---'                                                    
         SSPEC H9,57,C'DP'                                                      
         SSPEC H10,57,C'--'                                                     
         SSPEC H9,61,C'LEN'                                                     
         SSPEC H10,61,C'---'                                                    
         SSPEC H9,66,C'PROGRAM NAME'                                            
         SSPEC H10,66,C'----------------'                                       
         SSPEC H9,89,C'COST'                                                    
         SSPEC H10,87,C'---------'                                              
*                                                                               
         DC    X'00'                                                            
         SPACE 2                                                                
* COPY ERROR MESSAGE TABLE                                                      
*                                                                               
MSGTAB   DS    0CL50                                                            
         DC    CL50'** MOVING POOL TO NON-POOL OR VICE VERSA **'                
         DC    CL50'** CANNOT MOVE PIGGYBACK BUY **'                            
         DC    CL50'** DEMO LOOKUP ERROR - NO RATING SERVICE **'                
         DC    CL50'** DEMO LOOKUP ERROR - BAD RATING SERVICE **'               
         DC    CL50'** DEMO LOOKUP ERROR - NO BOOK ON BUYLINE **'               
         DC    CL50'ERROR: TOO MANY SPOTS ON BUYLINE - CANNOT MOVE **'          
         DC    CL50'ERROR: STATION AAAAT IS LOCKED - CANNOT MOVE'               
         DC    CL50'** DEMO LOOKUP ERROR - NO LONGER SUPPORTED**'               
         EJECT                                                                  
***********************************************************************         
* PRINT REC MODE                                                                
***********************************************************************         
PRNTMODE NTR1  BASE=*,LABEL=*                                                   
         L     R0,AFTPETAB         SPOT DOESN'T HAVE MORE THAN 220 PRD          
         LA    R1,MAXNPRDS*FTPRDELQ                                             
         L     RE,AIO2                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE               COPY THE  TABLE                              
*                                                                               
         L     R0,VMKTLIST         STATION LIST                                 
         LA    R1,15*7             CAN'T HAVE MORE THAN 15 STATIONS             
         L     RE,AIO3                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE               COPY THE  TABLE                              
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+7(1),STALCNT                                                
         GOTO1 VXSORT,DMCB,VMKTLIST,,7,7,0                                      
*                                                                               
         LA    R1,HDHK                                                          
         ST    R1,HEADHOOK                                                      
         L     R1,=A(HDSPECS)                                                   
         A     R1,RELO                                                          
         ST    R1,SPECS                                                         
*                                                                               
         XC    REQTOTS,REQTOTS                                                  
         ZAP   REQDOLS,=P'0'                                                    
         XC    MGRTOTS,MGRTOTS                                                  
         ZAP   MGRDOLS,=P'0'                                                    
         XC    MKTTOTS,MKTTOTS                                                  
         ZAP   MKTDOLS,=P'0'                                                    
         XC    STATOTS,STATOTS                                                  
         ZAP   STADOLS,=P'0'                                                    
*                                                                               
         XC    GETBLK,GETBLK       CLEAR THE SPGETBUY BLOCK                     
         USING GETBUYD,GETBLK      GETBUY DSECT                                 
         MVC   GBYCOMF,ACOMFACS    A(COMFACS)                                   
         MVI   GBYACT,GBYINIT      INIT                                         
         MVC   GBYAGY,AGENCY       AGENCY                                       
*                                                                               
         GOTO1 VGETBUY,GETBLK      INIT CALL TO SET 1 OR 2 BYTE BUYLINE         
*                                                                               
         CLC   AGENCY,=C'SJ'       AGENCY SJ?                                   
         JE    PR60                YES, CHECK IF CLIENT CODE IS TBL             
*                                                                               
PR0      LA    RE,DMWORK           A(DMWORK)                                    
         ST    RE,GBYDMWRK         STORE A(DMWORK)                              
*                                                                               
         BRAS  RE,BLDMKT                                                        
         JNE   PR65                                                             
*                                                                               
PR1      L     R4,VMKTLIST                                                      
*                                                                               
PR2      OC    QMGR,QMGR           TEST MKTGRP PRESENT                          
         JNZ   PR70                YES, NOT AS COMMON AS NO MKTGRP              
*                                                                               
PR2A     GOTO1 MSUNPK,DMCB,(X'80',2(R4)),THISMKT,THISSTA                        
*                                                                               
         BRAS  RE,GETSTA                                                        
*                                                                               
         CLC   LASTMKT,THISMKT     IF SAME MKT                                  
         JE    PR2G                                                             
         MVC   LASTMKT,THISMKT     DON'T RE-READ RECORD                         
         BRAS  RE,GETMKT                                                        
*                                                                               
PR2G     CLI   THISSTA,C'0'        CABLE STATION?                               
         JNL   PR85                YES, NOT AS COMMON AS OTHERS                 
*                                                                               
PR2M     MVI   FORCEHED,C'Y'       FORCE PAGE BREAK                             
         XC    KEY,KEY                                                          
         MVC   KEY(1),FRAGYMD                                                   
         MVC   KEY+1(2),FRCLT                                                   
         MVC   KEY+3(1),FRPRD                                                   
         MVC   KEY+4(5),2(R4)      MKT-STA                                      
         MVC   KEY+9(1),FREST                                                   
*                                                                               
         XC    TOKEYSV,TOKEYSV                                                  
         MVC   TOKEYSV(10),TOKEY     SET A-M/CLT/PRD/MSTA/EST                   
         CLI   TOESTPOL,C'Y'       TEST COPYING TO POL ESTIMATE                 
         JNE   *+8                                                              
         MVI   TOKEYSV+3,X'FF'     YES-FORCE PRODUCT TO POL                     
         OC    TOKEY+6(3),TOKEY+6    TEST STA SPECIFIED                         
         JNZ   PR3                                                              
         PACK  DUB,SVNEWMKT                                                     
         CVB   R0,DUB                                                           
         STH   R0,TOKEYSV+4        SET NEW MKT                                  
         MVC   TOKEYSV+6(3),KEY+6  SET CURRENT STATION                          
*                                                                               
PR3      BRAS  RE,SPHIGH                                                        
*                                                                               
         CLC   KEY(10),KEYSAVE                                                  
         JNE   PRKYNEQL                                                         
PR4      MVC   FRKEYSV,KEY                                                      
         L     R5,AIO1                                                          
         ST    R5,AIO                                                           
         USING BUYRECD,R5                                                       
         MVI   MISCFLG1,0                                                       
*                                                                               
         BRAS  RE,SPGET            GET BUY RECORD                               
         TM    BUYRCNTL,X'80'      TEST FOR DELETED RECORD                      
         JO    PR47                YES-NOTE PRODUCT POINTER CAN BE LIVE         
*****                                                                           
* CHECK FOR SPILL HANDLED IN BLDMKT                                             
*****    CLC   KEY+4(2),BUYMSTA    TEST SPILL                                   
*****    JNE   PR47                YES                                          
*                                                                               
         L     R0,ASAVEBUY         SAVE OLD RECORD IN SAVEBUY                   
         LR    RE,R5                                                            
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)         FROM LENGTH                                  
         LA    R1,2(RF)            TO LEN = FROM LEN +2                         
         MVCL  (R0),(RE)                                                        
*                                                                               
         OC    BDMGDATE,BDMGDATE   IS THIS A MAKEGOOD LINE?                     
         JNZ   PRMKGDLN                                                         
*                                                                               
PR4B     CLI   TOESTPOL,C'Y'       TEST COPY TO POL                             
         JE    *+12                                                             
         CLI   FRESTPOL,C'Y'       TEST COPY FROM POL                           
         JNE   PR4G                NEITHER - SKIP TEST                          
*                                                                               
         MVC   BYTE,TOPRD                                                       
         CLI   TOESTPOL,C'Y'       TEST POOL 'TO CLIENT'                        
         JNE   *+8                 NO                                           
         MVI   BYTE,X'FF'          YES-LOOK FOR POL IN RECORD                   
*                                                                               
         CLC   BYTE,BUYKPRD        TEST POL TO NON-POL OR VICE-VERSA            
         JNE   PR75                                                             
*                                                                               
PR4G     CLI   BDMASPRD+1,0        TEST FOR PIGGYBACK                           
         JNE   PR80                NO                                           
*                                                                               
PR5      MVC   HALF,BDMASPRD       SAVE PRODUCT ALLOCATION                      
         XC    BDMASPRD,BDMASPRD   CLEAR ALLOCATIONS                            
         CLI   TOESTPOL,C'N'       TEST COPYING TO NON-POOL EST                 
         JE    *+12                YES, MOST ARE EITHER TPOL OR BPOL            
         CLI   SVCPROF+0,C'0'      TEST BRAND POOL CLIENT                       
         JE    *+14                NO, IT IS TRUE POL                           
         MVC   BDMASPRD(1),TOPRD   ALLOCATE TO 'TO' PRODUCT                     
         J     PR6                                                              
*                                  COPYING TO TRUE POOL CLIENT                  
         CLC   FRCLT,TOCLT         TEST COPYING TO DIFFERENT CLIENT             
         JNE   *+14                YE-UNALLOCATE LINE                           
         MVC   BDMASPRD,HALF       NO-KEEP ORIGINAL ALLOCATION                  
         J     PR6                                                              
*                                                                               
         CLI   TOPRD,X'FF'         TEST COPYING TO POL                          
         JE    *+10                YES                                          
         MVC   BDMASPRD(1),TOPRD   NO-ALLOCATE TO 'TO PRODUCT'                  
*                                                                               
PR6      XC    BDMGDATE,BDMGDATE                                                
         MVI   BDMGSPOT,0                                                       
         MVC   BDCHG,BTODAY                                                     
         MVI   BDWHY,X'84'                                                      
         OI    BDSTAT,X'10'        SET BUY XFR CREATED                          
****     MVC   BDREP,SVESTREP      USE NEW REP OR NONE                          
****     MVC   BDNTAX,SVTAX        STATION TAX                                  
*                                                                               
* DELETE ALL ELEMENTS EXCEPT BDELEM/DEMEL/PBELEM *                              
* KEEP SPOT ELEMENTS IF DATEOPT=Y                                               
*                                                                               
         XC    SVPURP,SVPURP                                                    
****     MVI   ELCDLO,PKGCODEQ     X'05'-PACKAGE ELEMENT                        
         MVI   ELCDLO,NDCSPLQ      X'03'-SPILL DEMO ELEM                        
         MVI   ELCDHI,X'FF'                                                     
         LA    R6,BDELEM                                                        
*                                                                               
PR7      BRAS  RE,NEXTEL                                                        
         JNE   PR10                                                             
*                                                                               
PR8      CLC   0(1,R6),ELCDLO      X'03'-TEST FOR SPILL DEMO ELEM               
         JE    PR8X                IT IS, DELETE IT                             
         CLI   0(R6),PKGCODEQ      X'05'-PACKAGE ELEMENT                        
         JL    PR7                 PRESERVE THESE AS OLD CODE DID               
*                                                                               
         CLI   0(R6),RCPOLOQ       SEE IF WE HAVE ANY SPOTS IN PERIOD           
         BE    PR8SPT                X'0B' OR X'0C'                             
         CLI   0(R6),RCPOTOQ        ALSO LOOK FOR AFFIDS AND PAID SPOTS         
         BE    PR8SPT                                                           
*                                                                               
         CLI   0(R6),ACCODEQ       X'10' - AFFID ELEM?                          
         BE    PR8AFF                YES                                        
*                                                                               
PR8A     CLI   DATEOPT,C'Y'                                                     
         JE    PR7                                                              
         CLI   DATEOPT,C'X'        KEEPS MORE THAN JUST SPOT ELEMS              
         JE    PR7                                                              
*                                                                               
         CLI   0(R6),DLUCODEQ      X'24'-DEMO LOOK-UP OVERRIDE ELEM?            
         JE    PR8DMO                                                           
*                                                                               
         CLI   0(R6),CMCODEQ       X'66'-TEST COMMENT ELEMENT                   
         JE    PR8CMT                                                           
*                                                                               
         CLI   0(R6),IDELCODQ      X'70'-TEST ID ELEMENT                        
         JE    PR8IDEL                                                          
*                                                                               
         CLI   0(R6),X'6B'         X'6B'-CANADIAN PST ELEMENT                   
         JE    PR7                 KEEP THIS                                    
*                                                                               
PR8X     GOTO1 VRECUP,DMCB,AIO,(R6) DELETE THIS ELEMENT                         
*                                                                               
         BRAS  RE,NEXTEL2                                                       
         JE    PR8                                                              
*                                                                               
PR10     DS    0H                                                               
         TM    MISCFLG1,MF1HAVSP   DO WE HAVE A SPOT IN PERIOD?                 
         JZ    PR47                NO, NOTHING TO MOVE THEN, NEXT LINE          
         TM    MISCFLG1,MF1PDSPT+MF1MKGLN+MF1AFFID   ANY OF THESE TRUE?         
         JZ    PR14                                                             
         BRAS  RE,PRTSKPLN         SHOW WHY LINE WAS SKIPPED                    
         J     PR47                NOW GET NEXT BUYLINE                         
*                                                                               
PR14     BRAS  RE,BLDDEM         O BUILD DEMO ELEMENT                           
*                                                                               
         MVI   ELCDLO,2            FIND OLD DEMO ELEMENT                        
         MVI   ELCDHI,2                                                         
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         JNE   PR14DCH0                                                         
*                                                                               
         CLI   DEMOOPT,C'Y'        PRESERVE DEMO ELEMENT?                       
         JE    *+10                YES                                          
         XC    2(2,R6),2(R6)       RESET BOOK IN DEMO ELEMENT                   
         SPACE 2                                                                
* CALENDARIZE FOR NEW BUY DESCRIPTION PERIOD *                                  
         SPACE 1                                                                
PR20     DS    0H                                                               
         SR    R0,R0                                                            
         LLC   R1,BDDAY                                                         
         SLL   R1,25                                                            
         SR    RE,RE               RESET DAY NUMBER COUNTER                     
         SLDL  R0,1                                                             
         LTR   R0,R0                                                            
         JNZ   *+8                                                              
         JCT   RE,*-10                                                          
         BCTR  RE,0                ADD 1                                        
         LPR   R0,RE                                                            
         STC   R0,DAY1                                                          
* NOW GET END DAY NUMBER                                                        
         LTR   R1,R1                                                            
         JZ    *+12                                                             
         SLL   R1,1                                                             
         JCT   RE,*-10                                                          
         LPR   R0,RE                                                            
         STC   R0,DAYX                                                          
*                                                                               
         CLI   DATEOPT,C'Y'        TEST PRESERVE BUY DESC DATES                 
         JE    PR23                YES                                          
         CLI   DATEOPT,C'X'        TEST EXACT COPY                              
         JE    PR23                                                             
PR21     MVC   BDSTART,BPERTOST    SET DATES TO NEW REQ (OR EST) PERIOD         
         MVC   BDEND,BPERTOND                                                   
* CHANGE DATES TO AGREE WITH BUY DESCRIPTION DAYS *                             
* DETERMINE START DATE DAY                                                      
         GOTO1 DATCON,DMCB,(3,BDSTART),WORK                                     
*                                                                               
         GOTO1 GETDAY,DMCB,WORK,DUB                                             
         SPACE 1                                                                
* ADVANCE START DATE TO CORRECT DAY (END DATE CORRECTED LATER)                  
         SPACE 1                                                                
         LLC   R0,DAY1                                                          
         LLC   RE,0(R1)                                                         
         SR    R0,RE                                                            
         JNM   *+8                                                              
         AHI   R0,7                ADVANCE TO PROPER DAY                        
         GOTO1 ADDAY,DMCB,WORK,WORK+12,(R0)                                     
         GOTO1 DATCON,DMCB,WORK+12,(3,BDSTART)                                  
*                                                                               
PR22     DS    0H                                                               
         BRAS  RE,BLDEL                                                         
         BRAS  RE,CALEND                                                        
         JNZ   PR22X               IF CC NOT EQ, TOO MANY ELEMENTS              
         BRAS  RE,CHKELS                                                        
         JE    PR24                                                             
*                                                                               
PR22X    GOTO1 PRTERR,SPOTSMSG                                                  
         J     PR45                                                             
***************                                                                 
* KEEP EXISTING SCHEDULE ON BUY RECORD                                          
* MUST HAVE OVERLAPPING TO AND FROM ESTIMATES AND SPOTS                         
* ON RECORD WILL ONLY BE KEPT IF THEY FALL WITHIN NEW ESTIMATE                  
*                                                                               
* WE CAN MODIFY THE DATES AND THE ALLOCATIONS HERE AS WELL                      
***************                                                                 
PR23     GOTO1 =A(SKED),RR=RELO                                                 
         JNE   PR45                NO SPOTS ON RECORD-SKIP IT                   
*****    CLI   BXFOPT5,C'Y'        DATE SHIFT?                                  
*****    BNE   PR24                                                             
         MVC   BDSTART,BPERTOST    SET DATES TO NEW REQ (OR EST) PERIOD         
         MVC   BDEND,BPERTOND                                                   
*                                                                               
         LHI   R3,1                ALWAYS ONE WEEK TO START                     
         GOTO1 DATCON,DMCB,(3,BDSTART),(0,WORK)                                 
         GOTO1 DATCON,DMCB,(3,BDEND),(0,WORK+6)                                 
PR23A    GOTO1 ADDAY,DMCB,WORK,WORK,7                                           
         CLC   WORK,WORK+6                                                      
         JH    PR23B                                                            
         LA    R3,1(R3)                                                         
         J     PR23A                                                            
*                                                                               
PR23B    STC   R3,BDWKS                                                         
*                                                                               
*                                                                               
PR24     BRAS  RE,GETSPILL         GET SPILL MARKET LIST                        
         JNZ   PR22X               IF ZERO, HAD ROOM FOR SPILL                  
*                                                                               
PR25     CLI   DEMOOPT,C'Y'        TEST DEMOS = SAME                            
         JNE   PR26                NO - LOOK THEM UP                            
* COPY OLD SPILL DEMOS IF NEEDED                                                
         BRAS  RE,SETSPILL                                                      
         J     PR30                                                             
*                                                                               
PRLKUERR LLC   R1,ERROR                                                         
         GOTO1 PRTERR,(R1)                                                      
         J     PR45                                                             
*                                                                               
PR26     DS    0H                                                               
         BRAS  RE,DMLKUP                                                        
         CLI   ERROR,0             TEST FOR LOOKUP ERROR                        
         JNE   PRLKUERR            NO                                           
         SPACE 2                                                                
* CORRECT END DATE NOW *                                                        
         SPACE 1                                                                
PR30     MVI   ELCDLO,6                                                         
         MVI   ELCDHI,13                                                        
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL           FIND LAST REGEL                              
         JNE   PR30DCH0                                                         
         LR    R3,R6                                                            
         BRAS  RE,NEXTEL                                                        
         JE    *-6                                                              
* SET BDEND TO LOWER OF LAST DAY OF ROTATOR OR BUY DESC END                     
         GOTO1 DATCON,DMCB,(2,2(R3)),WORK                                       
         LLC   R0,DAYX                                                          
         LLC   RE,DAY1                                                          
         SR    R0,RE                                                            
         GOTO1 ADDAY,DMCB,WORK,WORK+12,(R0)   GET DATE OF LAST ROT DAY          
         GOTO1 DATCON,DMCB,WORK+12,(3,BDEND)                                    
*                                                                               
         CLC   BDEND,BPERTOND                                                   
         JNH   *+10                                                             
         MVC   BDEND,BPERTOND                                                   
*&&DO                                                                           
         BRAS  RE,TESTPST          KEEP OLD PST ELEM!!                          
*&&                                                                             
         OC    FRSTA,FRSTA         WAS THIS A ONE STATION REQUEST?              
         JNZ   PR35                YES - LOCK STAT CHECKED AT REQ TIME          
         BRAS  RE,STALOCK          "TO" STATION LOCKED?                         
         JNE   PRLKERR             NO                                           
*                                                                               
PR35     MVC   BUYKCLT,TOCLT       SET TO KEY VALUES                            
         MVC   BUYKPRD,TOPRD                                                    
         CLI   TOESTPOL,C'Y'       TEST COPYING TO POL ESTIMATE                 
         JNE   *+8                                                              
         MVI   BUYKPRD,X'FF'       YES-FORCE PRODUCT TO POL                     
         MVC   BUYMSTA,TOMKT                                                    
         OC    BUYMSTA(2),BUYMSTA  TEST NO MARKET                               
         JNZ   *+10                                                             
         MVC   BUYMSTA,TOKEYSV+4   MOVE MKT/STA                                 
         OC    BUYMSTA+2(3),BUYMSTA+2  TEST NO STATION                          
         JNZ   *+10                                                             
         MVC   BUYMSTA+2(3),TOKEYSV+6                                           
         MVC   BUYKEST,TOEST                                                    
*                                                                               
         BRAS  RE,NEXTBUY          GET NEXT BUYLINE NUMBER                      
         LA    RE,255              255 MAX FOR 1 BYTE BUYLINES                  
         CLI   GBY1OR2,2           2 BYTE BUYLINES?                             
         JNE   *+8                 NO                                           
         LA    RE,499              YES - ALLOW UP TO 499 FOR NOW                
*                                                                               
         XR    RF,RF               CLEAR RF                                     
         ICM   RF,3,KEY+11         NEW BUYLINE NUMBER                           
         CR    RF,RE               NEW BUYLINE <= MAX?                          
         JH    PRMXLINS            NO, ERROR                                    
*                                                                               
PR40     CLI   TESTOPT,C'Y'        SKIP UPDATE IF TEST OPTION                   
         JE    *+8                 YES                                          
         BRAS  RE,SPADD                                                         
*                                                                               
         XC    BUYTOTS,BUYTOTS     CLEAR BUY LINE TOTALS                        
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,11                                                        
         MVI   ELCDHI,13                                                        
         CLI   TOESTPOL,C'Y'       TEST COPYING TO POOL ESTIMATE                
         JE    PR42                YES                                          
         MVI   ELCDLO,6            CHANGE TO NON-POOL REGELEM CODES             
         MVI   ELCDHI,8                                                         
*                                                                               
PR42     BRAS  RE,NEXTEL                                                        
         JNE   PR43                                                             
         TM    RSTATUS-REGELEM(R6),RSMINUSQ+RSMINSDQ  MINUS OR MINUSED?         
         JNZ   PR42                                   YES, DON'T COUNT          
         GOTO1 VGETRATE,DMCB,(BUYKPRD,SPOTS),AIO,(R6)                           
         LM    RE,RF,SPOTS         GET VALUES RETURNED BY GETRATE               
         A     RE,BUYSPOTS         UPDATE BUY LINE SPOTS AND DOLLARS            
         A     RF,BUYDOLS                                                       
         STM   RE,RF,BUYSPOTS                                                   
         J     PR42                                                             
*                                                                               
PR43     L     RE,STABUYS          INCREMENT BUY LINE COUNT                     
         LA    RE,1(RE)                                                         
         ST    RE,STABUYS                                                       
*                                                                               
         L     RE,STASPOTS         RE=STATION SPOTS                             
         A     RE,BUYSPOTS         UPDATE STATION SPOT COUNT                    
         ST    RE,STASPOTS                                                      
         L     RF,BUYDOLS          UPDATE STATION DOLLAR COUNT                  
         CVD   RF,DUB                                                           
         AP    STADOLS,DUB                                                      
*                                                                               
         BRAS  RE,FMTBUY           FORMAT DISPLAY LINE                          
*                                                                               
PR45     MVC   KEY,FRKEYSV         RESTORE DIRECTORY FOR NEXT BUY               
         BRAS  RE,SPHIGH                                                        
                                                                                
         CLI   TESTOPT,C'Y'        SKIP UPDATE IF TEST OPTION                   
         BE    PR47                                                             
*                                  SHOULD HAVE MODIFY ORIGINAL OPTION           
*                                  SHOULD HAVE COPY ALLOCATION OPTION           
         BRAS  RE,SPGET                                                         
         L     RE,ASAVEBUY                                                      
         XR    RF,RF                                                            
         ICM   RF,3,BUYRLEN-BUYREC(RE)                                          
*                                                                               
         L     R0,AIO                                                           
         LA    R1,2(RF)            2 MORE BYTES TO CLEAR AFTER                  
         MVCL  R0,RE                                                            
********                                                                        
**  ANY SPOTS LEFT?  WE SHOULD DELETE THE BUYLINE IF NO SPOTS                   
********                                                                        
         BRAS  RE,SPPUT            SAVE THE ORIGINAL BUY RECORD                 
*                                                                               
PR47     BRAS  RE,SPSEQ                                                         
*                                                                               
         CLC   KEY(10),KEYSAVE     A/M/C/P/MKT/STA/EST                          
         JE    PR4                 GET NEXT BUY RECORD                          
*                                                                               
PR50     OC    STABUYS,STABUYS     TEST ANY BUYS COPIED                         
         JZ    *+8                                                              
         BRAS  RE,ENDSTA           PRINT STATION TOTALS (ONE EST ONLY)          
*                                                                               
         CLC   0(4,R4),7(R4)       NEXT ENTRY SAME MARKET                       
         JE    *+8                                                              
         BRAS  RE,ENDMKT                                                        
*                                                                               
         CLC   0(2,R4),7(R4)       TEST END OF MGR                              
         JE    *+8                 NO                                           
         BRAS  RE,ENDMGR           YES-DISPLAY MARKET GROUP TOTALS              
*                                                                               
PR55     LA    R4,7(R4)            NEXT LIST ENTRY                              
         OC    0(7,R4),0(R4)                                                    
         JNZ   PR2                                                              
*                                                                               
PRX      BRAS  RE,ENDRPT                                                        
         J     EXIT                                                             
*                                                                               
PRKYNEQL TM    STRQFLG1,SRQFLIST   DEALING WITH LIST? -NOT- BLDMKT              
         JNZ   PR55                YES, NEXT IN VMKTLIST                        
*                                                                               
PRKYDCH0 DC    H'0'                                                             
PR14DCH0 DC    H'0'                                                             
PR30DCH0 DC    H'0'                                                             
*                                                                               
PRMXLINS MVC   P(35),=C'** ERROR ** MORE THAN 255 BUY LINES'                    
         EDIT  (RE),(3,P+22)       EDIT ERROR MSG IN CASE IT'S 499              
         BRAS  RE,PRNTIT                                                        
         B     PR50                                                             
*                                                                               
PRMKGDLN OI    MISCFLG1,MF1MKGLN                                                
         J     PR4B                                                             
*                                                                               
PRLKERR  GOTO1 PRTERR,LOCKMSG      GO PRINT ERROR MESSAGE                       
         J     PR45                GO READ SEQ                                  
*                                                                               
PR8DMO   CLI   DEMOOPT,C'Y'        PRESERVE DEMO ELEMENT?                       
         JE    PR7                 YES, NEXTEL                                  
         J     PR8X                NO, DELETE IT                                
*                                                                               
PR8CMT   CLI   CMTOPT,C'Y'         TEST PRESERVE COMMENTS                       
         JE    PR7                 YES, NEXTEL                                  
         J     PR8X                NO, DELETE IT                                
*                                                                               
         USING REGELEM,R6                                                       
PR8SPT   CLI   BXFOPT5,C'Y'        DATE SHIFTING?                               
         JNE   PR8SPT10            NO, SPOT MUST BE IN 'TO' PERIOD              
         CLC   2(2,R6),CPERFRST    YES, SPOT MUST BE IN 'FROM' PERIOD           
         JL    PR8SPT50                                                         
         CLC   2(2,R6),CPERFRND                                                 
         JNH   PR8SPT20            WE HAVE A SPOT!                              
         J     PR8SPT50                                                         
                                                                                
PR8SPT10 CLC   2(2,R6),CPERTOST    SPOT DATE W/IN "TO" PERIOD                   
         JL    PR8SPT50                                                         
         CLC   2(2,R6),CPERTOND                                                 
         JH    PR8SPT50                                                         
*                                                                               
PR8SPT20 OI    MISCFLG1,MF1HAVSP   WE HAVE A SPOT IN PERIOD                     
*                                                                               
PR8SPT50 OC    RPAY,RPAY                                                        
         JZ    PR8SPTX                                                          
         OI    MISCFLG1,MF1PDSPT   PAID SPOT  REGARDLESS OF PERIOD              
*                                                                               
PR8SPTX  J     PR8A                                                             
*                                                                               
PR8AFF   OI    MISCFLG1,MF1AFFID   AFFID ELEM REGARDLESS OF PERIOD              
PR8AFFX  J     PR8A                                                             
         DROP  R6                                                               
*                                                                               
PR8IDEL  CLI   SVCXTRA+2,C'Y'      TEST CLT REQUIRES ID                         
         JE    PR7                 YES                                          
         CLI   SVAPROF+9,C'Y'      TEST AGY REQUIRES ID                         
         JE    PR7                 NO - RETURN                                  
         CLI   SVB0PROF+9,C'Y'     TEST PURPOSE CODES REQD                      
         JNE   PR8X                                                             
         MVC   SVPURP,3(R6)        HOPE THIS IS ONE AND SAVE IT                 
         J     PR7                                                              
*                                                                               
PR60     CLC   =X'CC2B',BCLT       CLIENT CODE IS TBL?                          
         JNE   PR0                                                              
         MVI   GBY1OR2,2           YES, SET BUYLINES IN 2-BYTE MODE             
         J     PR0                                                              
*                                                                               
PR65     MVC   P+10(28),=C'ERROR: ** NO DATA TO MOVE **'                        
         BRAS  RE,PRNTIT                                                        
         J     EXIT                                                             
*                                                                               
PR70     BRAS  RE,GETMGR                                                        
         J     PR2A                                                             
*                                                                               
PR75     GOTO1 PRTERR,POLMSG                                                    
         B     PR45                READ NEXT RECORD                             
*                                                                               
PR80     GOTO1 PRTERR,PIGMSG                                                    
         B     PR45                                                             
*                                                                               
PR85     MVI   THISSTA+4,C'/'      CABLE BE NUMERIC                             
         J     PR2M                                                             
         EJECT                                                                  
* SUBROUTINE TO FIND NEXT 'TO' BUYLINE NUMBER *                                 
*                                                                               
NEXTBUY  NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY,TOKEYSV                                                      
         OC    KEY+11(2),KEY+11    LINE NUMBER PRESENT ALREADY?                 
         BNZ   NEXTBUY6            YES - CONTINUE                               
         OI    DMINBTS,X'08'       PASS DELETES                                 
         BRAS  RE,SPHIGH                                                        
         B     NEXTBUY4                                                         
*                                                                               
NEXTBUY2 MVC   KEYSAVE,KEY                                                      
         BRAS  RE,SPSEQ                                                         
*                                                                               
NEXTBUY4 CLC   KEY(11),KEYSAVE     A-M/C/P/MKT/STA/EST                          
         BE    NEXTBUY2                                                         
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE LAST KEY READ                        
         NI    DMINBTS,X'FF'-X'08' TURN OFF PASS DELETES                        
*                                                                               
NEXTBUY6 XR    RE,RE                                                            
         ICM   RE,3,KEY+11                                                      
         LA    RE,1(RE)                                                         
         STCM  RE,3,KEY+11                                                      
         STCM  RE,3,TOKEYSV+11                                                  
*                                                                               
         MVC   BUYKEY(10),KEY      MOVE KEY TO RECORD                           
         MVC   BUYKEY+10(2),KEY+11                                              
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* SUB-ROUTINE TO CHECK SPOT ELEMENT COUNT                                       
*                                                                               
* ON EXIT, CC=EQ IF OK, NEQ IF TOO MANY SPOT ELEMENTS                           
*                                                                               
CHKELS   NTR1                                                                   
         MVI   ELCDLO,X'0B'                                                     
         CLI   TOESTPOL,C'Y'       TEST COPYING TO POL                          
         BE    *+8                                                              
         MVI   ELCDLO,X'06'        NON-POL                                      
         MVC   ELCDHI,ELCDLO                                                    
*                                                                               
         LA    R6,BDELEM                                                        
         LA    R3,MAXSPOTS         SET COUNTER OF MAXIMUM ELEMENTS              
         BRAS  RE,NEXTEL                                                        
         BNE   EQXIT                                                            
         BCT   R3,*-8                                                           
         B     NEQXIT                                                           
         EJECT                                                                  
*                                                                               
* SUBROUTINE TO FORMAT BUY DATA TO PRINT LINE *                                 
PRNTIT   NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)     PRINT A LINE                                 
         B     EXIT                                                             
*                                                                               
* SUB-ROUTINE TO PRINT AN ERROR MESSAGE FOR COPY PROBLEMS                       
* AT ENTRY, R1 CONTAINS MESSAGE NUMBER                                          
PRTERR   NTR1                                                                   
         LR    R0,R1                                                            
         BCTR  R1,0                                                             
         MH    R1,=Y(L'MSGTAB)                                                  
         L     RE,=A(MSGTAB)                                                    
         A     RE,RELO                                                          
         LA    RE,0(R1,RE)         INDEX INTO TABLE                             
         MVC   P+10(L'MSGTAB),0(RE)                                             
         CHI   R0,LOCKMSG          STATION LOCKED MESSAGE?                      
         BNE   *+10                NO                                           
         MVC   P+25(5),WORK+4      YES - MOVE IN STATION                        
         MVC   P+1(5),=C'LINE='                                                 
         XR    R0,R0                                                            
         ICM   R0,3,BUYKBUY                                                     
         EDIT  (R0),(3,P+6),ALIGN=LEFT                                          
         GOTO1 SPOOL,DMCB,(R8)     PRINT A LINE                                 
         BASR  RE,RF               SKIP A LINE AFTER                            
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* SUBROUTINES TO PRINT TOTALS AT CONTROL BREAKS *                               
*                                                                               
ENDSTA   NTR1                                                                   
         XC    SVSPILL,SVSPILL                                                  
         MVI   ALLOWLIN,3          MUST HAVE 3 LINES ON PAGE FOR TOTS           
         GOTO1 SPOOL,DMCB,(R8)     PRINT A LINE                                 
         BASR  RE,RF                                                            
         MVC   P+20(20),=C'**STATION XXXXXXXX**'                                
         MVC   P+20+10(8),THISSTA                                               
         GOTO1 PRBUCK,DMCB,STATOTS,P+45                                         
         BRAS  RE,PRNTIT                                                        
         GOTO1 UPBUCK,(R1),STATOTS,MKTTOTS                                      
         XC    STATOTS,STATOTS                                                  
         ZAP   STADOLS,=P'0'                                                    
         B     EXIT                                                             
ENDMKT   NTR1                                                                   
         MVI   ALLOWLIN,3          MUST HAVE 3 LINES ON PAGE FOR TOTS           
         GOTO1 SPOOL,DMCB,(R8)     PRINT A LINE                                 
         BASR  RE,RF                                                            
         MVC   P+20(22),=C'**MARKET XXXX TOTALS**'                              
         GOTO1 MSUNPK,DMCB,TOKEYSV+4,WORK,WORK+4                                
         MVC   P+29(4),WORK        EXTRACT MARKET NUMBER                        
         GOTO1 PRBUCK,(R1),MKTTOTS,P+45                                         
         BRAS  RE,PRNTIT                                                        
         GOTO1 UPBUCK,(R1),MKTTOTS,MGRTOTS                                      
         GOTO1 (RF),(R1),MKTTOTS,REQTOTS                                        
         XC    MKTTOTS,MKTTOTS                                                  
         ZAP   MKTDOLS,=P'0'                                                    
         B     EXIT                                                             
*                                                                               
ENDMGR   NTR1                                                                   
         OC    QMGR,QMGR                                                        
         BZ    ENDMGRX                                                          
         MVI   ALLOWLIN,3          MUST HAVE 3 LINES ON PAGE FOR TOTS           
         GOTO1 SPOOL,DMCB,(R8)     PRINT A LINE                                 
         BASR  RE,RF                                                            
         MVC   P+20(23),=C'**MKTGRP XXXXX TOTALS**'                             
         MVC   P+29(5),QMGR                                                     
         OC    P+29(5),SPACES                                                   
         GOTO1 PRBUCK,DMCB,MGRTOTS,P+45                                         
         BRAS  RE,PRNTIT                                                        
         XC    MGRTOTS,MGRTOTS                                                  
         ZAP   MGRDOLS,=P'0'                                                    
ENDMGRX  B     EXIT                                                             
*                                                                               
ENDRPT   NTR1                                                                   
         MVI   ALLOWLIN,3          MUST HAVE 3 LINES ON PAGE FOR TOTS           
         GOTO1 SPOOL,DMCB,(R8)     PRINT A LINE                                 
         BASR  RE,RF                                                            
         MVC   P+20(18),=C'**REQUEST TOTALS**'                                  
         GOTO1 PRBUCK,DMCB,REQTOTS,P+45                                         
         BRAS  RE,PRNTIT                                                        
*                                                                               
         OC    REQSPOTS,REQSPOTS   ANY SPOTS??                                  
         JNZ   ENDRPTX                                                          
         MVC   P+5(26),=26C'*'                                                  
         BRAS  RE,PRNTIT                                                        
         MVC   P+5(26),=CL26'ERROR   **NO SPOTS FOUND**'                        
         BRAS  RE,PRNTIT                                                        
         MVC   P+5(26),=26C'*'                                                  
         BRAS  RE,PRNTIT                                                        
*                                                                               
ENDRPTX  XC    REQTOTS,REQTOTS                                                  
         ZAP   REQDOLS,=P'0'                                                    
         B     EXIT                                                             
         SPACE 2                                                                
* SUB-ROUTINE TO UPDATE BUCKETS                                                 
*                                                                               
* AT ENTRY, P1=A(SOURCE BUCKETS), P2=A(DESTINATION BUCKETS)                     
*                                                                               
UPBUCK   STM   RE,R1,WORK                                                       
         LM    RE,RF,0(R1)         RE=A(SOURCE), RF=A(DESTINATION)              
*                                                                               
         LA    R0,2                R0=COUNT                                     
UPBUCK10 L     R1,0(RF)            GET DESTINATION BUCKET                       
         A     R1,0(RE)            ADD SOURCE BUCKET                            
         ST    R1,0(RF)                                                         
         LA    RE,4(RE)            NEXT SOURCE                                  
         LA    RF,4(RF)            AND DESTINATION                              
         BCT   R0,UPBUCK10                                                      
***************                                                                 
* DOLLAR TOTALS                                                                 
***************                                                                 
         AP    0(L'REQDOLS,RF),0(L'STADOLS,RE)                                  
*                                                                               
UPBUCKX  LM    RE,R1,WORK                                                       
         BR    RE                                                               
*                                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO PRINT BUCKETS                                                  
* AT ENTRY, P1=A(BUCKETS), P2=A(OUTPUT)                                         
*                                                                               
PRBUCK   NTR1                                                                   
         LM    R2,R3,0(R1)         R2=A(BUCKETS), R3=A(OUTPUT)                  
         MVC   0(9,R3),=C'BUYLINES='                                            
         L     R0,0(R2)                                                         
         EDIT  (R0),(5,9(R3)),ALIGN=LEFT,ZERO=NOBLANK                           
         LA    R3,15(R3)                                                        
*                                                                               
         MVC   0(6,R3),=C'SPOTS='                                               
         L     R0,4(R2)                                                         
         EDIT  (R0),(6,6(R3)),ALIGN=LEFT,ZERO=NOBLANK                           
         LA    R3,14(R3)                                                        
*                                                                               
         MVC   0(8,R3),=C'DOLLARS='                                             
         EDIT  (P8,8(R2)),(16,12(R3)),2,ZERO=NOBLANK,MINUS=YES                  
         B     EXIT                                                             
         EJECT                                                                  
GETMKT   NTR1                                                                   
         MVI   SVMZONE,0                                                        
         LA    R6,KEY                                                           
         USING MKTRECD,R6                                                       
         MVI   MKTKEY,C'0'                                                      
         MVC   MKTKEY+1(L'MKTKEY-1),MKTKEY                                      
         MVI   MKTKTYPE,C'S'                                                    
         MVC   MKTKMED,QMED                                                     
         MVC   MKTKMKT,THISMKT                                                  
         MVC   MKTKAGY,AGENCY                                                   
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO3,DMWORK              
         CLI   8(R1),0             TEST IF RECORD FOUND                         
         BNE   GETMKTX                                                          
         L     R6,AIO3                                                          
         MVC   SVMZONE,MKTZONE                                                  
*                                                                               
GETMKTX  B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* COMPARE THE NUMBER AND THE DEMO CATEGORIES IN THE 'FROM' AND 'TO'             
*   ESTIMATES                                                                   
***********************************************************************         
CMPDEMOS NTR1  BASE=*,LABEL=*                                                   
         LA    RE,FREDEMOS         'FROM' ESTIMATE DEMOS                        
         LA    R1,0                                                             
CDMO010  OC    0(3,RE),0(RE)       DO I HAVE A DEMO CATEGORY HERE?              
         JZ    CDMO020                                                          
         AHI   R1,1                                                             
         LA    RE,3(RE)                                                         
         J     CDMO010                                                          
*                                                                               
CDMO020  LR    R0,R1                                                            
         LA    RE,SVDEMOS          'TO' ESTIMATE DEMOS                          
         LA    R1,0                                                             
CDMO025  OC    0(3,RE),0(RE)       DO I HAVE A DEMO CATEGORY HERE?              
         JZ    CDMO030                                                          
         AHI   R1,1                                                             
         LA    RE,3(RE)                                                         
         J     CDMO025                                                          
*                                                                               
CDMO030  CR    R1,R0               NEED TO MATCH ON # OF CATEGORIES             
         JNE   CDMOSNO                                                          
*                                                                               
         LA    RE,SVDEMOS          NOW MAKE SURE EACH DEMO CATEGORY             
CDMO040  LR    R2,R0                 EXISTS IN 'FROM' ESTIMATE                  
         LA    RF,FREDEMOS                                                      
CDMO045  CLC   0(3,RE),0(RF)                                                    
         JE    CDMO050             HAVE A MATCH, CHECK NEXT DEMO CAT            
         LA    RF,3(RF)                                                         
         BCT   R2,CDMO045                                                       
         J     CDMOSNO             DEMO DOES NOT EXIST IN 'FROM' EST            
*                                                                               
CDMO050  LA    RE,3(RE)            NEXT DEMO IN 'TO' ESTIMATE                   
         JCT   R1,CDMO040          SEE IF THIS EXISTS IN 'FROM'                 
*                                                                               
CDMOSYS  J     EQXIT                                                            
CDMOSNO  J     NEQXIT                                                           
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* COMPARE THE NUMBER AND THE DEMO CATEGORIES IN THE 'FROM' AND 'TO'             
*   ESTIMATES                                                                   
***********************************************************************         
CMPADJCD NTR1  BASE=*,LABEL=*                                                   
         CLI   SVCPROF+9,C'0'      ANY 'TO' CLIENT ADJ CODE CTRL?               
         JE    CADJCYS             NONE, NO PROBLEM                             
         CLI   FRCPROF+9,C'0'      YES, ANY 'FROM' CLT ADJ CODE CTRL?           
         JE    CADJCNO                  NONE, WE HAVE A PROBLEM                 
*                                                                               
         CLC   FRCPROF+9(1),SVCPROF+9  SAME ADJ CODE CTRL?                      
         JNE   CADJCNO                 NO, GIVE THE ERROR                       
*                                                                               
         CLI   SVCPROF+9,X'F2'     NUMERIC ADJ CODES?                           
         JE    CADJCYS             YES, NO NEED FOR AJ PROFILES                 
*                                                                               
* READ AJ PROFILE FOR 'FROM' CLIENT                                             
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0AJ'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),QMED                                                   
         MVC   WORK+7(3),FRCLI                                                  
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),FRCOFFC                                               
         GOTO1 GETPROF,DMCB,WORK,WORK+16,DATAMGR                                
*                                                                               
* READ AJ PROFILE FOR 'TO' CLIENT                                               
         XC    WORK(16),WORK                                                    
         MVC   WORK(4),=C'S0AJ'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),QMED                                                   
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCOFFC                                               
         GOTO1 GETPROF,DMCB,WORK,WORK+32,DATAMGR                                
*                                                                               
         CLC   WORK+16(16),WORK+32  SAME INCL/EXCL AND LIST?                    
         JE    CADJCYS              YES, WE'RE DONE AND GOOD                    
         CLC   WORK+16(1),WORK+32   INCLUDE OR EXCLUDE HAS TO BE SAME           
         JNE   CADJCNO              NO, ADJACENCY CODE CONFLICT                 
*                                                                               
         LA    RE,WORK+17           RE = A(1ST OF ADJ CODES) FOR 'FROM'         
         LA    RF,WORK+31           RF = A(LAST OF ADJ CODES)                   
CADJC010 CLI   0(RE),C'*'           EOL FOR 'FROM'?                             
         JE    CADJC050             YES                                         
*                                                                               
         LA    R2,WORK+33                                                       
         LA    R3,WORK+47                                                       
CADJC020 CLI   0(R2),C'*'                                                       
         JE    CADJCNO                                                          
         CLC   0(1,RE),0(R2)        MATCH ON THIS ADJ CODE?                     
         JE    CADJC040             YES, NEXT 'FROM' ADJ CODE                   
         LA    R2,1(R2)             NEXT 'TO' ADJ CODE                          
         CR    R2,R3                EOL FOR 'TO' ADJ CODES?                     
         JNH   CADJC020             NO                                          
         J     CADJCNO              YES                                         
*                                                                               
CADJC040 LA    RE,1(RE)             NEXT 'FROM' ADJ CODE                        
         CR    RE,RF                EOL FOR 'FROM' ADJ CODES?                   
         JNH   CADJC010             NO                                          
*                                                                               
CADJC050 LA    RF,WORK+17           YES, FIND # OF 'FROM' ADJ CODES             
         SR    RE,RF                                                            
*                                                                               
         LA    R2,WORK+33           ALSO FIND # OF 'TO' ADJ CODES               
         LA    R3,WORK+47                                                       
CADJC060 CLI   0(R2),C'*'                                                       
         JE    CADJC070                                                         
         LA    R2,1(R2)             NEXT 'TO' ADJ CODE                          
         CR    R2,R3                EOL FOR 'TO' ADJ CODES?                     
         JNH   CADJC060             NO                                          
*                                                                               
CADJC070 LA    R2,WORK+33           YES, FIND # OF 'FROM' ADJ CODES             
         SR    R3,R2                                                            
*                                                                               
         CR    RE,R2                # OF ADJ CODES ARE SAME?                    
         JNE   CADJCNO              NO, IT IS NOT                               
*                                                                               
CADJCYS  J     EQXIT                                                            
CADJCNO  J     NEQXIT                                                           
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE MARKET GROUP                                                         
*                                                                               
* ON ENTRY:    R4                  A(SCANNER ENTRY)                             
***********************************************************************         
VALIMGR  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    QMGR,QMGR                                                        
         LLC   RE,5(R2)            GET INPUT LENGTH                             
         SHI   RE,5                ADJUST FOR 'MGR='                            
         LTR   RE,RE                                                            
         BM    VMGRNEQX                                                         
         EX    RE,VMGREXMV                                                      
*                                                                               
         LR    R5,RE                                                            
         BCTR  R5,0                L'DIGITS FOLLOWING LETTER                    
         MVC   WORK(5),QMGR                                                     
*                                                                               
         CLI   WORK,C'A'                                                        
         BL    VMGRNEQX                                                         
         CLI   WORK,C'Z'                                                        
         BH    VMGRNEQX                                                         
*                                                                               
         LA    R1,WORK+1                                                        
         CLI   0(R1),C'0'          FIRST CHAR MUST BE PRESENT                   
         BL    VMGRNEQX                                                         
*                                                                               
VMGR2    CLI   0(R1),C'*'                                                       
         BNE   *+8                                                              
         MVI   0(R1),C'0'                                                       
         CLI   0(R1),0                                                          
         BE    VMGR4                                                            
         CLI   0(R1),C'0'                                                       
         BL    VMGRNEQX                                                         
         CLI   0(R1),C'9'                                                       
         BH    VMGRNEQX                                                         
         LA    R1,1(R1)                                                         
         BCT   RE,VMGR2                                                         
*                                                                               
* READ FOR MKTGRP RECORD *                                                      
         SPACE 1                                                                
VMGR4    XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D02'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(2),BCLT       TRY FOR CLIENT EXCEPTION RECORD              
         MVC   KEY+8(1),QMGR                                                    
         PACK  DUB,WORK+1(5)                                                    
         MVC   KEY+9(2),DUB+5                                                   
         GOTO1 ASPHIGH                                                          
         CLC   KEY(9),KEYSAVE      TYPE/A-M/C/MGRPID                            
         BNE   VMGR8                                                            
         UNPK  DUB,KEY+9(3)                                                     
         EX    R5,VMGREXCM                                                      
         BE    VMGREQX                                                          
*                                                                               
VMGR8    MVI   ERROR,INVMGRP                                                    
         CLI   QMGR,C'G'           MGRPID A-F REQUIRES CLIENT                   
         BL    VMGRNEQX                                                         
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         XC    KEY+3(2),KEY+3      CLEAR CLIENT                                 
         GOTO1 ASPHIGH                                                          
         CLC   KEY(9),KEYSAVE      TYPE/A-M/C/MGRPID                            
         BNE   VMGRNEQX                                                         
         UNPK  DUB,KEY+9(3)                                                     
         EX    R5,VMGREXCM                                                      
         BE    VMGREQX                                                          
*                                                                               
VMGRNEQX LTR   RB,RB               SET CC NEQ                                   
         J     EXIT                                                             
*                                                                               
VMGREQX  CR    RB,RB                                                            
         J     EXIT                                                             
*                                                                               
VMGREXMV MVC   QMGR(0),12(R2)                                                   
*                                                                               
VMGREXCM CLC   QMGR+1(0),DUB+3                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DISPLAY  WHY THE BUYLINE WAS SKIPPED WHEN THERE ARE SPOTS WITHIN THE          
*   SPECIFIED PERIOD.                                                           
* 3 CONDITIONS CURRENTLY                                                        
*    MF1AFFID - AFFID SPOT EXISTS                                               
*    MF1PDSPT - PAID SPOTS EXISTS                                               
*    MF1MKGLN - BUYLINE IS A MAKEGOOD LINE                                      
***********************************************************************         
PRTSKPLN NTR1  BASE=*,LABEL=*                                                   
         LA    R3,P                                                             
         USING LNSKPRSD,R3                                                      
         TM    MISCFLG1,MF1AFFID   BUYLINE HAS AN AFFID?                        
         BNZ   PSKPAFFD                                                         
         TM    MISCFLG1,MF1PDSPT   BUYLINE HAS A PAID SPOT?                     
         BNZ   PSKPPDSP                                                         
*                                                                               
PSKPMKGD MVC   LNSKPRSN(26),=C'BUYLINE IS A MAKEGOOD LINE'                      
         J     PSKPPRLN                                                         
*                                                                               
PSKPPDSP MVC   LNSKPRSN(23),=C'BUYLINE HAS A PAID SPOT'                         
         J     PSKPPRLN                                                         
*                                                                               
PSKPAFFD MVC   LNSKPRSN(20),=C'BUYLINE HAS AN AFFID'                            
*                                                                               
PSKPPRLN MVC   LNSKPTX1,=C'ERROR: BUYLINE'                                      
         MVC   LNSKPTX2,=CL23'EXCLUDED FROM BUY MOVE:'                          
         L     RE,AIO                                                           
         XR    RF,RF                                                            
         ICM   RF,3,BUYKBUY-BUYKEY(RE)                                          
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LNSKPLIN,DUB                                                     
         GOTO1 SPOOL,DMCB,(R8)     PRINT A LINE                                 
*                                                                               
PSKPX    J     EXIT                                                             
         DROP  R3                                                               
         LTORG                                                                  
*                                                                               
LNSKPRSD DSECT                                                                  
LNSKPTX1 DS    CL14                ERROR: BUYLINE                               
         DS    CL1                                                              
LNSKPLIN DS    CL3                 '999'                                        
         DS    CL1                                                              
LNSKPTX2 DS    CL23                EXCLUDED FROM BUY MOVE:                      
         DS    CL1                                                              
LNSKPRSN DS    CL50                REASON TEXT                                  
*                                                                               
T21778   CSECT                                                                  
         EJECT                                                                  
**********************************************************************          
* SUBROUTINE TO BUILD LIST OF MKTGRPS/MARKETS/STATIONS WITH ACTIVITY *          
* WHEN MARKET GROUPS NOT PRESENT                                     *          
* OUTPUT IS 7 BYTE ENTRIES IN MKTLIST MGRP(2)/MKT(2)/STA(3)          *          
**********************************************************************          
         DS    0D                                                               
BLDMKT   NTR1  BASE=*,LABEL=*                                                   
         TM    STRQFLG1,SRQFLIST   DO WE HAVE A LIST OF STATIONS?               
         BNZ   BLDMKTX             YES, VMKTLIST IS FILLED IN ALREADY           
*                                                                               
         L     R0,VMKTLIST                                                      
         L     R1,VMKTLSTX                                                      
         SR    R1,R0                                                            
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR OUT MKTLIST                            
*                                                                               
         CLI   QMGR,0              TEST MKTGRPS                                 
         BNE   BLDMGR              YES                                          
*                                                                               
         L     R4,VMKTLIST                                                      
         MVC   KEY(L'BUYKEY),FRKEY INITIALIZE KEY                               
         OC    FRSTA,FRSTA         TEST ONE STATION REQUEST                     
         BZ    BLDMKT2             NO                                           
*                                                                               
         GOTO1 ASPHIGH                                                          
         CLC   KEY(BUYKBUY-BUYKEY),KEYSAVE TEST SAME ESTIMATE                   
         BNE   BLDMKTX             NO-NO ACTIVITY TO COPY                       
         MVC   2(5,R4),FRMKT       ADD ONE ENTRY TO LIST                        
         B     BLDMKTX                                                          
*                                                                               
BLDMKT2  GOTO1 ASPHIGH                                                          
         CLC   KEY(4),FRKEY        A-M/C/P                                      
         BNE   BLDMKTX                                                          
*                                                                               
         OC    FRMKT,FRMKT         TEST FOR SINGLE MARKET REQUEST               
         JZ    BLDMKT3             NO                                           
         CLC   KEY(6),FRKEY        TEST IF MARKET FOUND                         
         JE    BLDMKT3             YES                                          
         JH    BLDMKTX             EXIT IF BEYOND THE MARKET                    
         MVC   KEY+4(2),FRMKT      SET THE RIGHT MARKET IF LOW                  
         XC    KEY+6(7),KEY+6      AND READ FOR IT                              
         J     BLDMKT2                                                          
*                                                                               
BLDMKT3  MVC   KEY+9(1),FREST      READ FOR IT IF LOW                           
         XC    KEY+10(3),KEY+10    CLEAR BUYLINE AREA                           
         GOTO1 ASPHIGH                                                          
*                                                                               
BLDMKT3A CLC   KEY(10),KEYSAVE     A-M/C/P/M/S/E                                
         JNE   BLDMKT6             NO                                           
*                                                                               
         CLI   KEY+10,X'80'        SPILL POINTER?                               
         JE    BLDMKT6             YES, SKIP IT (SEE THE TEST AT PR4)           
         CLI   GOALOPT,C'Y'        TEST ONLY IF GOALS                           
         JNE   BLDMKT4                                                          
*                                                                               
         CLC   KEY+4(2),FRKEYSV+4  TEST SAME MARKET AS LAST TIME                
         JE    BLDMKT4                                                          
*                                                                               
         MVC   FRKEYSV,KEY         SAVE CURRENT BUY KEY                         
         BRAS  RE,TESTGLS          TEST FOR GOALS IN RECEIVING MKT              
         MVC   KEY,FRKEYSV         RESTORE KEY                                  
         MVC   KEYSAVE,KEY         AND KEYSAVE                                  
         JNE   BLDMKT8             NEXT MARKET IF NO GOALS                      
*                                                                               
BLDMKT4  MVC   2(5,R4),KEY+4       ADD MKT/STA TO LIST                          
         LA    R4,7(R4)                                                         
*                                                                               
         C     R4,VMKTLSTX                                                      
         JH    BMKTDCH0                                                         
*                                                                               
BLDMKT6  MVC   KEY,KEYSAVE         RESTORE TO LAST KEYARG                       
         MVC   KEY+9(3),=3X'FF'    NEXT STA, BY FILLING EST AND BUYKBUY         
         J     BLDMKT2                                                          
*                                                                               
BLDMKT8  MVC   KEY,KEYSAVE         RESTORE TO LAST KEYARG                       
         MVC   KEY+6(3),=3X'FF'    FORCE NEXT MKT                               
         J     BLDMKT2                                                          
*                                                                               
BLDMKTX  L     R4,VMKTLIST                                                      
         OC    0(7,R4),0(R4)       TEST ANY ENTRIES IN LIST                     
         JNZ   BLDMKTXY                                                         
BLDMKTXN LTR   RB,RB               SET CC NOT =                                 
         J     EXIT                                                             
*                                                                               
BLDMKTXY CR    RB,RB               SET CC =                                     
         J     EXIT                                                             
*                                                                               
BMKTDCH0 DC    H'0'                MARKET LIST NEEDS EXPANDING                  
         EJECT                                                                  
* SUBROUTINE TO BUILD MARKET STATION LIST FOR A MARKET GROUP  *                 
* OUTPUT IS SERIES OF 7 BYTE ENTRIES IN MKTLIST               *                 
         SPACE 1                                                                
BLDMGR   DS    0H                                                               
         L     R1,AIO2             MGRLIST BUILD AREA                           
         LA    R0,8                                                             
         XC    0(250,R1),0(R1)                                                  
         LA    R1,250(R1)                                                       
         BCT   R0,*-10                                                          
         SPACE 1                                                                
* READ MGRDEF RECORD *                                                          
         SPACE 1                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D02'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+8(1),QMGR       MGRPID                                       
         CLI   QMGR,C'F'           TEST FOR CLIENT EXCEPTION                    
         BH    *+10                                                             
         MVC   KEY+3(2),BCLT                                                    
         MVC   SVMGRKEY,KEY                                                     
*                                                                               
         GOTO1 ASPHIGH                                                          
*                                                                               
         CLC   KEY(13),KEYSAVE     TEST KEY FOUND                               
         BE    *+6                                                              
         DC    H'0'                DIE ON ERRORS                                
         MVC   AIO,AIO1                                                         
         GOTO1 ASPGET                                                           
*                                                                               
         GOTO1 AEXTMGR             GET TITLES & BREAK LENGTHS                   
*                                                                               
BLDMGR2  XC    KEY,KEY             READ PASSIVE MKTGRP POINTERS                 
         MVC   KEY(2),=X'0D82'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         CLI   QMGR,C'F'                                                        
         BH    *+10                                                             
         MVC   KEY+3(2),BCLT                                                    
         MVC   KEY+8(1),QMGR                                                    
*                                                                               
         CLI   QMGR+1,C' '         TEST MARKET GROUP FILTER                     
         BNH   BLDMGR10                                                         
         MVC   WORK(4),QMGR+1                                                   
         LA    R1,WORK                                                          
         LA    R0,4                                                             
BLDMGR8  CLI   0(R1),C'0'                                                       
         BNL   *+8                                                              
         MVI   0(R1),C'0'                                                       
         LA    R1,1(R1)                                                         
         BCT   R0,BLDMGR8                                                       
         PACK  DUB(3),WORK(5)                                                   
         MVC   KEY+9(2),DUB                                                     
         MVC   MGRNUM,DUB                                                       
*                                                                               
BLDMGR10 L     R5,AIO2             POINT TO LIST BUILD AREA                     
         GOTO1 ASPHIGH             GET FIRST MKTGRP POINTER                     
         B     BLDMGR16                                                         
         EJECT                                                                  
BLDMGR14 GOTO1 ASPSEQ              GET NEXT MARKET LIST RECORD                  
*                                                                               
BLDMGR16 CLC   KEY(9),KEYSAVE                                                   
         BNE   BLDMGR30            DONE IF NO MATCH                             
*                                                                               
         CLI   QMGR+1,C'0'         TEST MARKET GROUP NUMBER PRESENT             
         BL    BLDMGR20                                                         
         LA    R1,QMGR+4           POINT TO LAST BYTE OF MKTGRP                 
         LA    RE,4                                                             
BLDMGR18 CLI   0(R1),C'0'          FILTER ON MARKET GROUP NUMBER                
         BNL   *+10                                                             
         BCTR  R1,0                                                             
         BCT   RE,BLDMGR18                                                      
         UNPK  DUB,KEY+9(3)                                                     
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   QMGR+1(0),DUB+3                                                  
         BNE   BLDMGR30            DONE IF NO MATCH                             
*                                                                               
BLDMGR20 DS    0H                                                               
         MVC   0(2,R5),KEY+11      SET MARKET NUMBER FROM KEY                   
         LA    R5,2(R5)                                                         
         B     BLDMGR14                                                         
         EJECT                                                                  
* NOW USE LIST JUST BUILT TO SEARCH FOR STATIONS WITH ACTIVITY *                
         SPACE 1                                                                
BLDMGR30 DS    0H                                                               
         L     R5,AIO2             POINT TO LIST                                
         OC    0(2,R5),0(R5)       TEST ANY MARKETS IN LIST                     
         BZ    BLDMKTX             NO - DONE                                    
*                                                                               
         L     R4,VMKTLIST                                                      
*                                                                               
BLDMGR32 MVC   KEY,FRKEY                                                        
         MVC   KEY+4(2),0(R5)      SET NEXT MKT                                 
*                                                                               
BLDMGR34 GOTO1 ASPHIGH                                                          
         CLC   KEY(4),FRKEY        SAME A-M/C/P                                 
         BNE   BLDMGRX                                                          
         MVC   KEY+9(1),FREST                                                   
         XC    KEY+10(3),KEY+10    CLEAR BUY LINE AREA                          
         GOTO1 ASPHIGH                                                          
         CLC   KEY(10),KEYSAVE     A-M/C/P/M/S/E                                
         BNE   BLDMGR38                                                         
         CLI   GOALOPT,C'Y'                                                     
         BNE   BLDMGR36                                                         
*                                                                               
         CLC   KEY+4(2),FRKEYSV    TEST SAME MARKET AS PREVIOUS                 
         BE    BLDMGR38                                                         
         MVC   FRKEYSV,KEY         SAVE CURRENT BUY KEY                         
         BRAS  RE,TESTGLS          TEST FOR GOALS IN RECEIVING MKT              
         MVC   KEY,FRKEYSV         RESTORE KEY                                  
         MVC   KEYSAVE,KEY                                                      
         BNE   BLDMGR38            SKIP IF NO GOALS                             
*                                                                               
BLDMGR36 MVC   0(2,R4),MGRNUM      ADD MGR NUMBER TO LIST                       
         MVC   2(5,R4),KEY+4       ADD MKT/STA TO LIST                          
         LA    R4,7(R4)                                                         
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE LAST KEYARG                          
         MVC   KEY+9(3),=3X'FF'    FORCE NEXT STA                               
         B     BLDMGR34                                                         
*                                                                               
BLDMGR38 LA    R5,2(R5)            POINT TO NEXT MKT                            
         OC    0(2,R5),0(R5)       TEST DONE                                    
         BNZ   BLDMGR32            NO                                           
*                                                                               
BLDMGRX  L     R4,VMKTLIST                                                      
         B     BLDMKTX                                                          
         EJECT                                                                  
* SUBROUTINE TO TEST FOR GOAL ACTIVITY *                                        
         SPACE 1                                                                
TESTGLS  NTR1                                                                   
         MVC   KEYSAVE,KEY         SAVE CURRENT BUY KEY                         
         XC    KEY,KEY                                                          
         MVI   KEY,X'02'                                                        
         MVC   KEY+1(4),TOAGYMD    A-M/C/P                                      
         MVC   KEY+5(2),KEYSAVE+4  MKT                                          
         MVC   KEY+7(1),TOEST      EST                                          
         GOTO1 ASPHIGH                                                          
         CLC   KEY(8),KEYSAVE      SET CC                                       
         J     EXIT                                                             
         EJECT                                                                  
********************************************************                        
* THIS ROUTINE CREATES NEW DEMO ELEM FROM ESTHDR DEMOS *                        
* AND INSERTS OLD DEMO VALUES FROM EXISTING DEMO ELEM  *                        
* POL SEQUENCE IS USED FOR BRD POL BY BRAND            *                        
********************************************************                        
         SPACE 1                                                                
         DS    0D                                                               
BLDDEM   NTR1  BASE=*,LABEL=*                                                   
         XC    ELEM,ELEM           BUILD NEW DEMO ELEM                          
         LA    R1,ELEM+24                                                       
         LA    R4,SVDEMOS          R4=A(ESTIMATE HEADER DEMO LIST)              
*                                                                               
BLDDEM3  CLI   1(R4),0                                                          
         BE    BLDDEM4                                                          
         MVC   0(3,R1),0(R4)       COPY DEMO                                    
         LA    R1,8(R1)                                                         
         LA    R4,3(R4)                                                         
         B     BLDDEM3                                                          
*                                                                               
BLDDEM4  LA    R0,ELEM                                                          
         SR    R1,R0                                                            
         STC   R1,ELEM+1           SET LEN                                      
         MVI   ELEM,X'02'          AND ELEM CODE                                
*                                                                               
         MVI   ELCDLO,2            LOOK FOR OLD DEMO ELEM                       
         MVI   ELCDHI,2                                                         
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   BLDDEM22            NONE FOUND                                   
         ST    R6,FULL             SAVE DEMEL ADDRESS                           
         MVC   ELEM+2(22),2(R6)    MOVE PRGM DESC/BOOK/ETC                      
* MOVE MATCHING DEMOS                                                           
         LLC   R3,1(R6)                                                         
         AHI   R3,-24                                                           
         BNP   BLDDEM20                                                         
         LA    R6,24(R6)                                                        
         SPACE 1                                                                
* PROCESSING FOR CONVERTED DEMOS *                                              
         SPACE 1                                                                
BLDDEM12 SRL   R3,3                SET FOR BCT                                  
*                                                                               
BLDDEM14 LLC   R0,ELEM+1                                                        
         AHI   R0,-24                                                           
         BNP   BLDDEM20            NO DEMOS IN NEW ELEM                         
         SRL   R0,3                R0=N'NEW DEMOS                               
         LA    R1,ELEM+24                                                       
BLDDEM16 CLC   0(3,R6),0(R1)       TEST OLD DEMO VS. NEW DEMO                   
         BE    BLDDEM18            FOUND A MATCH                                
         LA    R1,8(R1)            NEXT NEW DEMO                                
         BCT   R0,BLDDEM16                                                      
         B     *+10                                                             
BLDDEM18 MVC   0(8,R1),0(R6)       COPY OLD DEMO/VALUE INTO NEW                 
         LA    R6,8(R6)            NEXT OLD DEMO                                
         BCT   R3,BLDDEM14                                                      
         B     BLDDEM20                                                         
         SPACE 1                                                                
* DELETE EXISTING DEMO ELEM *                                                   
         SPACE 1                                                                
BLDDEM20 L     R6,FULL             RESTORE DEMEL ADDRESS                        
         GOTO1 VRECUP,DMCB,BUYREC,(R6)                                          
         B     BLDDEM25                                                         
         SPACE 1                                                                
* ADD NEW AFTER BDELEM (OR AFTER 02 FOR SPILL) *                                
         SPACE 1                                                                
BLDDEM22 DS    0H                                                               
         LA    R6,BDELEM                                                        
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),2                                                          
         BE    *-10                                                             
BLDDEM25 GOTO1 VRECUP,DMCB,BUYREC,ELEM,(R6)                                     
*                                                                               
         CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BNE   BLDDEMX                                                          
         CLI   QMED,C'T'                                                        
         BNE   BLDDEMX                                                          
                                                                                
*===============================================================                
* BUILD DEMO LOOKUP ELEMENT AND INSERT IN BUY                                   
*===============================================================                
                                                                                
         XC    ELEM,ELEM                                                        
E        USING DLUELEM,ELEM                                                     
         MVI   ELEM,X'24'                                                       
         MVI   ELEM+1,11                                                        
         MVC   E.DLUBAMKT,SVDMLMKT  SET MARKET                                  
         MVC   E.DLUBSTOV,SVDMLST0  AND OVERRIDE STATION (IF ANY)               
         TM    SVDMLFLG,X'01'      TEST BBM LOOKUP                              
         BZ    *+10                                                             
         MVC   E.DLUBSTOV,SVDMLST1                                              
         MVC   E.DLUBFLGS,SVDMLFLG  SET IMPS/RTG SVC FLAG                       
         DROP  E                                                                
*                                                                               
         MVI   ELCDLO,X'24'                                                     
         MVI   ELCDHI,X'24'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   BLDDEM27                                                         
         GOTO1 VRECUP,DMCB,BUYREC,(R6)                                          
*                                                                               
BLDDEM27 GOTO1 VRECUP,DMCB,BUYREC,ELEM,(R6)                                     
*                                                                               
BLDDEMX  J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
* CREATE REGELEMS FOR ALL WEEKS IN BUY PERIOD                                   
*                                                                               
         DS    0D                                                               
CALEND   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R6,BDELEM           FIND LAST ELEM                               
         SR    R3,R3               CLEAR COUNTER                                
CAL4     LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),X'24'         SAVED DEMO LOOK-UP OVERRIDE ELEM?            
         BE    CAL5                YES, 0B'S GO BEFORE THIS ELEMENT             
         CLI   0(R6),0             TEST E-O-R                                   
         BNE   CAL4                                                             
* GET EBCDIC START/END DATES                                                    
CAL5     GOTO1 DATCON,DMCB,(3,BDSTART),WORK                                     
         GOTO1 (RF),(R1),(3,BDEND),WORK+12                                      
*                                                                               
         LA    RE,7                                                             
         CLI   BDWKIND,C'O'                                                     
         BE    CAL6                                                             
         LA    RE,14                                                            
         CLI   BDWKIND,C'A'                                                     
         BE    CAL6                                                             
         LA    RE,21                                                            
         CLI   BDWKIND,C'T'                                                     
         BE    CAL6                                                             
         LA    RE,28                                                            
         CLI   BDWKIND,C'F'                                                     
         BE    CAL6                                                             
         DC    H'0'                                                             
CAL6     ST    RE,FULL             SAVE NUMBER OF DAYS BETWEEN SPOTS            
*                                                                               
CAL8     GOTO1 DATCON,DMCB,WORK,(2,ELEM+2)                                      
         LA    R3,1(R3)            INCREMENT WEEK COUNT                         
         LA    R0,1                NON-POL GETS 1 ELEM/WEEK                     
         CLI   SVPOLNPW,0                                                       
         BNE   CAL10                                                            
         CLI   ELEM,X'06'                                                       
         BE    *+8                                                              
         IC    R0,BDNOWK           POL GETS 1 ELEM PER SPOT                     
*                                                                               
CAL10    GOTO1 VRECUP,DMCB,BUYREC,ELEM,(C'R',(R6))                              
         CLI   8(R1),C'R'          TEST GOT TOO BIG                             
         BNE   CALERR                                                           
         LLC   RE,1(R6)                                                         
         AR    R6,RE                                                            
         BCT   R0,CAL10            LOOP FOR NUMBER OF ELEMENTS                  
*                                                                               
CAL11    L     R0,FULL             RESTORE NUM OF DAYS BETWEEN SPOTS            
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R0)                                      
         CLC   WORK+6(6),WORK+12   TEST PAST END                                
         BH    CAL12                                                            
         MVC   WORK(6),WORK+6                                                   
         B     CAL8                                                             
*                                                                               
CAL12    STC   R3,BDWKS            SET NUMBER OF WEEKS IN BDELEM                
         CR    RB,RB               SET CC =                                     
         B     CALX                                                             
*                                                                               
CALERR   LTR   RB,RB               SET CC NOT EQUAL                             
*                                                                               
CALX     DS    0H                                                               
         J     EXIT                                                             
         EJECT                                                                  
* PROVIDE DEMO LOOK UP LINKAGE - USER PARAMS ARE IN DMCB                        
*                                                                               
         DS    0D                                                               
DMLKUP   NTR1  BASE=*,LABEL=*                                                   
         MVI   ERROR,0             CLEAR ERROR BYTE                             
*                                                                               
         LA    R6,BDELEM           FIND DEMO ELEM                               
DMLK1    LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),2                                                          
         BE    DMLK1X                                                           
         CLI   0(R6),0                                                          
         BNE   DMLK1                                                            
         DC    H'0'                                                             
*                                                                               
DMLK1X   CLI   SVAPROF+7,C'C'      TEST CANAD                                   
         BNE   DMLK2                                                            
         MVI   BYTE,1                                                           
         CLI   BXFMED,C'N'                                                      
         BE    DMLK4                                                            
DMLK2    MVC   BYTE,FRAGYMD                                                     
         NI    BYTE,X'0F'                                                       
         CLI   BYTE,1              TEST SPOT TV                                 
         BNE   DMLKX                                                            
DMLK4    DS    0H                                                               
         CLI   SVCXTRA+5,C'Y'      TEST US SPILL                                
         BE    DMLK4X                                                           
         CLI   SVCXTRA+5,C'D'      TEST US DPT SUMMARY SPILL                    
         BE    DMLK4X                                                           
         CLI   SVAPROF+7,C'C'                                                   
         BNE   *+8                                                              
DMLK4X   O     R6,=X'01000000'     SET TO LOOK UP SPILL                         
*                                                                               
         OC    2(2,R6),2(R6)       TEST BOOK IN ELEMENT                         
         BNZ   *+10                                                             
         MVC   2(2,R6),SVBOOK                                                   
         MVC   HALF+1(1),SVHUTADJ                                               
* WEIGHTS                                                                       
         MVC   4(20,R6),SVWGTLST                                                
*                                                                               
DMLK6    XC    WORK,WORK                                                        
         MVC   WORK+8(4),ACOMFACS                                               
         MVC   WORK+12(4),DATAMGR                                               
         MVC   WORK+16(4),CALLOV                                                
*                                                                               
         MVC   HALF,SVCPROF+3      SET RATING SVC BYTE                          
         NI    HALF,X'0F'                                                       
* SET TO CALL DEMO LOOK-UP MODULES *                                            
         CLI   SVAPROF+7,C'C'                                                   
         BNE   DMLK20                                                           
         CLI   SVCXTRA,C'U'        TEST US CLIENT                               
         BE    DMLK20                                                           
         OI    HALF,X'80'          SET CANADIAN IND                             
*                                                                               
DMLK20   L     RE,AIO2             PASS A WORK AREA IN DM6                      
         LA    RE,1600(RE)                                                      
         ST    RE,DMCB+20                                                       
*                                                                               
         GOTO1 VGETDEM,DMCB,BUYREC,(R6),(HALF,HALF+1),AIO2,WORK                 
         XC    DMCB+20(4),DMCB+20  CLEAR ADDRESS ON RETURN                      
         CLI   0(R1),0             TEST IF OK                                   
         BE    DMLKX                                                            
         SPACE 1                                                                
* CHECK FOR ERRORS *                                                            
         SPACE 1                                                                
         MVI   ERROR,NOSVCMSG                                                   
         CLI   0(R1),X'11'                                                      
         BE    DMLKX                                                            
         MVI   ERROR,BADSVMSG                                                   
         CLI   0(R1),X'41'                                                      
         BE    DMLKX                                                            
         MVI   ERROR,NOBKMSG                                                    
         CLI   0(R1),X'80'         TEST E-O-F                                   
         BE    DMLKX               YES - MISSING BOOK ERROR                     
         MVI   ERROR,NODEMO                                                     
         CLI   0(R1),X'12'         NO LONGER SUPPORTED?                         
         BE    DMLKX               YES - GIVE ERROR                             
         CLI   0(R1),X'45'         TEST MISSING BOOK                            
         BE    *+6                                                              
         DC    H'0'                SHOULD NOT OCCUR                             
         MVI   ERROR,0             RESET ERROR CODE                             
DMLKX    J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*====================================================== *                       
* READ SPILL REC AND ADD SPILL DEMO EL FOR EACH MKT     *                       
*                                                       *                       
*    ** NOTE **   R7 HAS A(5 BYTE MKT-STA)              *                       
*=======================================================*                       
         SPACE 1                                                                
         DS    0D                                                               
GETSPILL NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R0,TOMKT            POINT TO MKT/STA                             
         OC    TOSTA,TOSTA         MAKE SURE STATION IS THERE                   
         BNZ   *+8                 AND IF NOT,                                  
         LA    R0,TOKEYSV+4        USE THIS ONE INSTEAD                         
         GOTO1 MSUNPK,DMCB,(R0),WORK,WORK+4                                     
         CLI   SVCXTRA+5,C'D'      TEST DPT SUMMARY SPILL                       
         BE    GDPTSPL                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D13'                                                  
         MVC   KEY+2(2),AGENCY                                                  
         MVC   KEY+4(1),SVCPROF+3                                               
         MVC   KEY+5(4),WORK+4                                                  
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   GETSPLX             EXIT                                         
         MVC   WORK(20),KEY                                                     
         MVC   KEY+10(2),TOCLT                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+10                                                             
         MVC   KEY,WORK                                                         
         L     R0,AIO                                                           
         L     R2,AIO3                                                          
         USING SDEFRECD,R2                                                      
         ST    R2,AIO                                                           
         GOTO1 GETREC                                                           
         ST    R0,AIO                                                           
         CLC   13(2,R2),=H'256'                                                 
         BNH   *+6                                                              
         DC    H'0'                                                             
         LA    R7,SDEFEL                                                        
         DROP  R2                                                               
* CREATE SPILL DEMO EL IN ELEM                                                  
         LA    R6,BDELEM                                                        
         LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),2                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    ELEM,ELEM           COPY ELCODE/BOOK                             
         MVI   ELEM,3              SET SPILL DEMEL CODE                         
         MVI   ELEM+1,24           RESET LENGTH                                 
         MVC   ELEM+2(2),2(R6)     COPY BOOK                                    
* MOVE ALL RATINGS TO SPILL ELEM                                                
         LLC   R0,1(R6)                                                         
         AHI   R0,-24                                                           
         BZ    GETSPL2                                                          
         SRL   R0,3                SET FOR BCT                                  
         LA    R1,ELEM+24                                                       
         LA    R6,24(R6)           POINT TO FIRST DEMO                          
*                                                                               
GETSPL1A CLI   1(R6),C'R'                                                       
         BNE   GETSPL1B                                                         
         XC    0(8,R1),0(R1)                                                    
         MVC   0(3,R1),0(R6)       MOVE DEMO DESC                               
         LA    R1,8(R1)                                                         
         LLC   RE,ELEM+1           BUMP ELEMENT LENGTH                          
         LA    RE,8(RE)                                                         
         STC   RE,ELEM+1                                                        
*                                                                               
GETSPL1B LA    R6,8(R6)                                                         
         BCT   R0,GETSPL1A                                                      
         SPACE 1                                                                
* ADD SPILL DEMEL FOR EACH SPILL MKT *                                          
         SPACE 1                                                                
GETSPL2  CLI   0(R7),5                                                          
         BNE   GETSPL6                                                          
         USING SDEFEL05,R7                                                      
*                                                                               
         CLC   BUYREC+4(2),2(R7)   TEST SPILL MKT EQ ACTUAL MKT                 
         BE    GETSPL6             YES - SKIP                                   
         TM    8(R7),X'80'         TEST '*' FEATURE                             
         BO    GETSPL6             YES SKIP                                     
         MVC   ELEM+4(4),2(R7)     SET AGY MKT/RTG SVC MKT CODES                
*                                                                               
E        USING NDELEM,ELEM                                                      
         MVC   E.NDAGYMKT,SDEFAMKT                                              
         MVC   E.NDBKTYPE,SDEFBKTY                                              
         MVC   E.NDRTGSVC,SDEFRSVC                                              
         MVC   E.NDMKTALF,SDEFALPH                                              
*                                                                               
         MVC   E.NDSTA,SVDMLST0                                                 
         CLI   SDEFRSVC,C'1'        TEST BBM LOOKUP                             
         BNE   *+10                                                             
         MVC   E.NDSTA,SVDMLST1                                                 
         DROP  E,R7                                                             
*                                                                               
GETSPL4  LA    R6,BDELEM                                                        
         LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),2                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LLC   R0,1(R6)                                                         
         AR    R6,R0               INSERT AFTER 02                              
         GOTO1 VRECUP,DMCB,BUYREC,ELEM,(C'R',(R6))                              
         CLI   8(R1),C'R'          TEST GOT TOO BIG                             
         BNE   GETSPLX2            EXIT WITH ERROR                              
*                                                                               
GETSPL6  LLC   R0,1(R7)                                                         
         AR    R7,R0                                                            
         CLI   0(R7),0                                                          
         BNE   GETSPL2                                                          
*                                                                               
         B     GETSPLX             EXIT                                         
         EJECT                                                                  
GDPTSPL  CLC   SVSPLSTA,BUYKEY+6   TEST SAME STATION                            
         BNE   GDPTSP1                                                          
         CLC   SVSPLCLT,BUYKEY+1   TEST SAME CLIENT                             
         BNE   GDPTSP1                                                          
         CLI   SVSPLLST,X'FF'      TEST ANY STATIONS                            
         BE    GDPTSP10            NO                                           
         B     GDPTSP22                                                         
*                                                                               
GDPTSP1  MVC   SVSPLSTA,BUYKEY+6   SAVE STATION                                 
         MVC   SVSPLCLT,BUYKEY+1   SAVE CLIENT                                  
         LA    R4,SVSPLLST                                                      
*                                                                               
         MVI   0(R4),X'FF'         SET EOL FLAG                                 
         XC    KEY,KEY                                                          
         MVC   KEY(3),=C'SDN'                                                   
         TM    SVCPROF+3,X'01'                                                  
         BZ    *+8                                                              
         MVI   KEY+2,C'A'                                                       
         MVC   KEY+3(4),WORK+4     STATION                                      
         MVI   KEY+7,C'T'                                                       
         B     *+10                                                             
*                                                                               
GDPTSP2  MVC   KEY(10),WORK        MOVE LAST KEY READ                           
         MVI   KEY+10,X'FF'        FORCE NEXT SPILL MARKET                      
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'DEMDIR',KEY,WORK                      
*                                                                               
         CLC   KEY(8),WORK         SAME STATION                                 
         BNE   GDPTSP10                                                         
*                                                                               
         LA    R0,SVSPILLX                                                      
         CR    R4,R0                                                            
         BNL   GDPTSP10       **** NOP ERROR AND JUST STOP *****                
         MVC   0(2,R4),WORK+8      MOVE MARKET NUMBER                           
         LA    R4,4(R4)                                                         
         MVI   0(R4),X'FF'                                                      
         B     GDPTSP2                                                          
         SPACE 1                                                                
* NOW READ MARKET TRANSLATION RECORDS FOR AGY MKT NUMBERS *                     
         SPACE 1                                                                
GDPTSP10 DS    0H                                                               
         CLI   SVSPLLST,X'FF'      TEST ANY DATA                                
         BE    GETSPLX             NO                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D46'                                                  
         MVC   KEY+2(1),SVKEY      SET A-M                                      
         MVI   KEY+3,C'N'                                                       
         CLI   SVCPROF+3,C'0'                                                   
         BE    *+8                                                              
         MVI   KEY+3,C'A'                                                       
         LA    R4,SVSPLLST         POINT TO RSMKT LIST                          
*                                                                               
GDPTSP12 MVC   KEY+4(2),0(R4)      MOVE RSMKT                                   
         GOTO1 HIGH                                                             
         CLC   KEY(6),KEYSAVE                                                   
         BNE   GDPTSP18                                                         
*                                                                               
         L     R0,AIO                                                           
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         ST    R0,AIO                                                           
*                                                                               
         L     R6,AIO2                                                          
         LA    R6,24(R6)           POINT TO FIRST ELEMENT                       
         USING MTREL05,R6                                                       
         B     GDPTSP17                                                         
*                                                                               
GDPTSP14 OC    MTRCLT,MTRCLT       TEST DEFAULT ELEMENT                         
         BNZ   *+10                NO                                           
         MVC   2(2,R4),MTRAGMK     MOVE IN DEFAULT                              
         CLC   MTRCLT,SVSPLCLT     MATCH CLIENT                                 
         BNE   GDPTSP16            NO                                           
         MVC   2(2,R4),MTRAGMK     USE CLIENT SPECIFIC                          
         B     GDPTSP18            AND STOP SEARCH NOW                          
*                                                                               
GDPTSP16 SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
*                                                                               
GDPTSP17 CLI   0(R6),5                                                          
         BE    GDPTSP14                                                         
         CLI   0(R6),0                                                          
         BNE   GDPTSP16                                                         
*                                                                               
GDPTSP18 LA    R4,4(R4)            NEXT LIST ENTRY                              
         CLI   0(R4),X'FF'                                                      
         BNE   GDPTSP12                                                         
*                                                                               
GDPTSP20 DS    0H                                                               
*                                                                               
GDPTSP22 LA    R6,BDELEM                                                        
         LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),2             FIND 02 DEMO ELEM                            
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   ELEM(24),0(R6)                                                   
         MVI   ELEM,3              SET SPILL DEMEL CODE                         
         MVI   ELEM+1,24           RESET LENGTH                                 
* MOVE ALL RATINGS TO SPILL ELEM                                                
         LLC   R0,1(R6)                                                         
         SHI   R0,24                                                            
         BZ    GDPTSP28                                                         
         SRL   R0,3                SET FOR BCT                                  
         LA    R1,ELEM+24                                                       
         LA    R6,24(R6)           POINT TO FIRST DEMO                          
*                                                                               
GDPTSP24 CLI   1(R6),C'R'                                                       
         BNE   GDPTSP26                                                         
         XC    0(8,R1),0(R1)                                                    
         MVC   0(3,R1),0(R6)       MOVE DEMO CODE                               
         LA    R1,8(R1)                                                         
         LLC   RE,ELEM+1           BUMP ELEMENT LENGTH                          
         LA    RE,8(RE)                                                         
         STC   RE,ELEM+1                                                        
*                                                                               
GDPTSP26 LA    R6,8(R6)                                                         
         BCT   R0,GDPTSP24                                                      
         SPACE 1                                                                
* ADD SPILL DEMO ELEM FOR EACH MARKET *                                         
         SPACE 1                                                                
GDPTSP28 LA    R4,SVSPLLST                                                      
*                                                                               
GDPTSP30 CLC   BUYREC+4(2),2(R4)   TEST SPILL EQ ACTUAL MKT                     
         BE    GDPTSP32                                                         
         OC    2(2,R4),2(R4)       TEST COULDN'T TRANSLATE                      
         BZ    GDPTSP32                                                         
         MVC   ELEM+4(2),2(R4)     MOVE AGENCY MARKET                           
         MVC   ELEM+6(2),0(R4)     MOVE RTGSVC MARKET                           
*                                                                               
         LA    R6,BDELEM                                                        
         LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),2                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LLC   R0,1(R6)                                                         
         AR    R6,R0               INSERT AFTER 02 ELEM                         
         GOTO1 VRECUP,DMCB,BUYREC,ELEM,(C'R',(R6))                              
         CLI   8(R1),C'R'          TEST GOT TOO BIG                             
         BNE   GETSPLX2            EXIT WITH ERROR                              
*                                                                               
GDPTSP32 LA    R4,4(R4)            NEXT MKT                                     
         CLI   0(R4),X'FF'         TEST EOL                                     
         BNE   GDPTSP30                                                         
*                                                                               
GETSPLX  CR    RB,RB               SET CC EQUAL                                 
         B     GETSPLXX                                                         
GETSPLX2 LTR   RB,RB                                                            
*                                                                               
GETSPLXX J    EXIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*====================================================== *                       
* THIS ROUTINE CALLED WHEN DEM=SAME IS SPECIFIED        *                       
* IF THERE ARE SPILL DEMO ELEMENTS, NEED TO GET THE OLD *                       
*   DEMO VALUES IN THE NEW SPILL ELEMENTS               *                       
*=======================================================*                       
         SPACE 1                                                                
         DS    0D                                                               
SETSPILL NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   ELCDLO,3                                                         
         MVI   ELCDHI,3                                                         
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL           TEST ANY SPILL DEMELS                        
         BNE   SETSPLX             NO - EXIT                                    
         LR    R7,R6               SAVE DEMEL ADDRESS                           
         SPACE 1                                                                
* TRY TO FIND A DEMEL FOR SAME MARKET IN ORIGINAL RECORD *                      
         SPACE 1                                                                
SETSPL10 L     R6,ASAVEBUY                                                      
         LA    R6,BDELEM-BUYRECD(R6)                                            
*                                                                               
SETSPL12 BRAS  RE,NEXTEL                                                        
         BNE   SETSPL30                                                         
         CLC   4(2,R6),4(R7)       MATCH MARKET NUMBERS                         
         BNE   SETSPL12            NO - CONTINUE                                
         SPACE 1                                                                
* HAVE THE ELEMENT - NOW MATCH THE DEMOS                                        
         SPACE 1                                                                
         LA    R1,24(R7)                                                        
         LLC   R0,1(R7)            GET LENGTH                                   
         SHI   R0,24                                                            
         BNP   SETSPL30                                                         
         SRL   R0,3                DIVIDE BY 8                                  
         SPACE 1                                                                
* LOOK FOR THE VALUE IN THE OLD DEMO ELEMENT *                                  
         SPACE 1                                                                
SETSPL20 LR    RE,R6                                                            
         LLC   RF,1(RE)                                                         
         SHI   RF,24                                                            
         BNP   SETSPL30                                                         
         SRL   RF,3                                                             
         LA    RE,24(RE)           POINT TO FIRST DEMO                          
*                                                                               
SETSPL22 CLC   0(3,R1),0(RE)       MATCH DEMO                                   
         BE    SETSPL24                                                         
         LA    RE,8(RE)                                                         
         BCT   RF,SETSPL22                                                      
         B     SETSPL26                                                         
*                                                                               
SETSPL24 MVC   0(8,R1),0(RE)       MOVE DEMO VALUES                             
*                                                                               
SETSPL26 LA    R1,8(R1)            NEXT DEMO IN NEW DEMEL                       
         BCT   R0,SETSPL20                                                      
*                                                                               
SETSPL30 LLC   R0,1(R7)                                                         
         AR    R7,R0               POINT TO NEXT ELEMENT                        
         CLI   0(R7),3             ANOTHER SPILL DEMEL                          
         BE    SETSPL10            YES - GO PROCESS                             
*                                                                               
SETSPLX  J     EXIT                                                             
*                                                                               
SPNXTEL  SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLI   0(R6),0             TEST E-O-R                                   
         BE    SPNXTELX                                                         
         CLC   0(1,R6),ELCDLO                                                   
         BL    SPNXTEL                                                          
         CLC   0(1,R6),ELCDHI                                                   
         BH    SPNXTEL                                                          
         CR    RE,RE               SET CC EQ                                    
         BR    RE                                                               
SPNXTELX LTR   RE,RE               SET CC NEQ                                   
         BR    RE                                                               
         LTORG                                                                  
         SPACE 2                                                                
         DS    0D                                                               
         EJECT                                                                  
*====================================================== *                       
* IF TO STA <> FROM STATION THEN OLD PST ELEMENT IS     *                       
*   DELETED & NEW ONE IS ADDED                          *                       
*=======================================================*                       
         SPACE 1                                                                
         DS    0D                                                               
TESTPST  NTR1  BASE=*,LABEL=*                                                   
         OC    TOSTA,TOSTA         IF THE STATIONS ARE THE SAME                 
         BZ    TPST10                                                           
         CLC   TOSTA,FRSTA         WE HAVE PST CODES                            
         BE    TPST10                                                           
         XC    SVPST,SVPST                                                      
         LA    R6,KEY                                                           
         USING STARECD,R6                                                       
         MVI   STAKEY,C'0'                                                      
         MVC   STAKEY+1(L'STAKEY-1),STAKEY                                      
         MVI   STAKTYPE,C'S'                                                    
         MVC   STAKMED,QMED                                                     
         GOTO1 MSUNPK,DMCB,(X'80',TOMKT),WORK,WORK+4                            
         MVC   STAKCALL,WORK+4                                                  
         CLI   STAKCALL+4,C' '                                                  
         BH    *+10                                                             
         MVC   STAKCALL+4(1),QMED                                               
         MVC   STAKAGY,AGENCY                                                   
         MVC   STAKCLT,QCLT                                                     
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO3,DMWORK              
         CLI   8(R1),0             TEST IF RECORD FOUND                         
         BNE   TPSTX                                                            
         L     R6,AIO3                                                          
         MVC   SVPST,SPST                                                       
*                                                                               
TPST10   MVI   ELCDLO,X'6B'                                                     
         MVI   ELCDHI,X'6B'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL           TEST ANY PST ELEMS                           
         BNE   TPST20              NO                                           
         GOTO1 VRECUP,DMCB,BUYREC,(R6)  DELETE OLD ELEMENT                      
*                                                                               
TPST20   OC    SVPST,SVPST                                                      
         BZ    TPSTX                                                            
         XC    ELEM,ELEM           ADD NEW PST ELEMENT                          
         MVC   ELEM(2),=X'6B0C'                                                 
         MVC   ELEM+2(L'SVPST),SVPST                                            
         GOTO1 VRECUP,DMCB,BUYREC,ELEM,(R6)                                     
*                                                                               
TPSTX    J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
         DS    0D                                                               
STALOCK  NTR1  BASE=*,LABEL=*                                                   
         LA    R6,KEY              R6=KEY                                       
         USING STARECD,R6          MASTER RECORD DSECT                          
         MVI   STAKEY,C'0'         INIT TO CHARACTER '0'                        
         MVC   STAKEY+1(L'STAKEY-1),STAKEY                                      
         MVI   STAKTYPE,C'S'       "S" RECORD                                   
         MVC   STAKMED,QMED         MEDIA                                       
         GOTO1 MSUNPK,DMCB,(X'80',TOKEYSV+4),WORK,WORK+4                        
         CLI   WORK+8,C' '          MEDIA FILLED IN BY STAPACK?                 
         BH    *+10                 YES                                         
         MVC   WORK+8(1),QMED       NO - SET IT FROM QMED                       
         MVC   STAKCALL,WORK+4      SET CALL LETTERS                            
         MVC   STAKAGY,AGENCY       SET AGENCY                                  
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO3,DMWORK              
         CLI   8(R1),0              RECORD FOUND?                               
         JNE   EQXIT                NO                                          
         L     R6,AIO3              A(MASTER RECORD)                            
         TM    SFLAG1,SLOCK         MASTER RECORD LOCKED?                       
         JNZ   NEQXIT               YES - CC NEQ                                
         J     EQXIT                NO - CC EQU                                 
         DROP  R6                   DROP R6                                     
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*==============================================*                                
* CREATE PROTOTYPE ELEMENT - N.B. USES BUELPRD *                                
*==============================================*                                
         SPACE 1                                                                
         DS    0D                                                               
BLDEL    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   ELEM,6                                                           
         MVI   ELEM+1,10                                                        
         CLI   BDTIME,0            TEST P/B                                     
         BE    *+8                 NO                                           
         MVI   ELEM+1,2                                                         
         MVC   ELEM+7(1),BDNOWK                                                 
         CLI   TOESTPOL,C'Y'       TEST COPYING TO POL ESTIMATE                 
         BNE   BLDELX              NO                                           
* POL                                                                           
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'0B'                                                       
         MVI   ELEM+1,10                                                        
         CLI   SVPOLNPW,0                                                       
         BE    BLDEL10                                                          
         IC    R0,BDNOWK                                                        
         SLL   R0,2                USE 6 BITS FOR NPW                           
         STC   R0,ELEM+7                                                        
*                                                                               
BLDEL10  CLI   BDMASPRD,0          TEST NO ALLOCATION                           
         BE    BLDELX                                                           
         MVI   ELEM+1,14                                                        
         MVC   ELEM+10(1),BDMASPRD                                              
         MVC   ELEM+11(1),BDSEC                                                 
*                                                                               
BLDELX   J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*===============================================================*               
* SUB-ROUTINE TO KEEP SCHEDULE ON BUY RECORD                    *               
*===============================================================*               
         SPACE 1                                                                
SKED     NTR1  BASE=*,LABEL=*                                                   
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'0B'                                                     
         CLI   TOESTPOL,C'Y'       TEST COPYING TO POL ESTIMATE                 
         BE    *+8                                                              
         MVI   ELCDLO,X'06'                                                     
*                                                                               
         IC    R0,ELCDLO                                                        
         AHI   R0,1                                                             
         STC   R0,ELCDHI                                                        
*                                                                               
         SR    R3,R3               R3=N'SPOT ELEMENTS ON BUY RECORD             
         XC    FULL,FULL           FULL(2)=BUY START,FULL+2(2)=BUY END          
*                                                                               
* R4 WILL MOVE DEPENDING ON WHETHER THE COPY IS KEEPING OR DELETING ELM         
         NI    MISCFLG1,X'FF'-MF1LVR4E                                          
         L     R4,ASAVEBUY                                                      
         LA    R4,BDELEM-BUYKEY(R4)  R4 = A(1ST ELEM IN ORIG RECORD)            
         BRAS  RE,NXTR4EL          R4 = A(1ST SPOT ELEM)                        
         BNE   SKED20              EOR                                          
*                                                                               
SKED2    BRAS  RE,NEXTEL           SEARCH ON THE COPIED FIRST                   
         BNE   SKED20              EOR                                          
*                                                                               
SKED4    NI    MISCFLG1,X'FF'-MF1LVR4E-MF1DTADJ                                 
         CLI   BXFOPT5,C'Y'        DATE SHIFTING?                               
         BNE   SKED5               NO, SPOT MUST BE IN 'TO' PERIOD              
         CLC   2(2,R6),CPERFRST    YES, SPOT MUST BE IN 'FROM' PERIOD           
         BL    SKED6                                                            
         CLC   2(2,R6),CPERFRND                                                 
         BNH   SKED10              WE HAVE A SPOT WE CAN USE                    
         B     SKED6                                                            
*                                                                               
SKED5    CLC   2(2,R6),CPERFRST    YES, SPOT MUST BE IN 'FROM' PERIOD           
         BL    SKED6                                                            
         CLC   2(2,R6),CPERFRND                                                 
         BH    SKED6                                                            
         CLC   2(2,R6),CPERTOST       AND ALSO IN "TO" PERIOD                   
         BL    SKED6                                                            
         CLC   2(2,R6),CPERTOND                                                 
         BNH   SKED10              WE HAVE A SPOT!                              
*                                                                               
SKED6    DS    0H                                                               
*********                                                                       
******     SHOW SPOT DATE AND THE PRODUCT CODE WE'RE DELETING                   
         BRAS  RE,PRTR6SPT         ONLY PRINT OUT SPOTS                         
*********                                                                       
*                                  ELEM NOT IN PERIOD OR UNDESIRABLE            
         GOTO1 VRECUP,DMCB,BUYREC,(R6)  DELETE THE ELEMENT                      
*****                                                                           
* REMEMBER - WE ARE POINTING AT THE NEXT ELEMENT NOW                            
*****                                                                           
SKED7    CLI   0(R6),0             TEST E-O-R                                   
         BE    SKED20                                                           
         CLI   0(R6),X'10'         DELETE ANY ASSOCIATED ELEMS                  
         BL    SKED8               (X'10'-X'19')                                
         CLI   0(R6),X'19'                                                      
         BNH   SKED6                                                            
*                                                                               
SKED8    BRAS  RE,NEXTEL2          TEST NEED THIS ELEMENT                       
         BNE   SKED20                                                           
*                                                                               
         CLI   DATEOPT,C'X'        TEST EXACT COPY                              
         BE    SKED8X                                                           
         CLC   0(1,R6),ELCDHI      TEST ELEM IS AN OTO                          
         JE    SKED6               YES DELETE                                   
*                                                                               
SKED8X   BRAS  RE,NXTR4EL          R4 = A(NEXT SPOT ELEM) IN ORIG               
         CLI   DATEOPT,C'X'        TEST EXACT COPY                              
         JE    SKED4                                                            
         CLC   0(1,R4),ELCDHI      TEST ELEM IS AN OTO                          
         JE    SKED8X              YES, SKIP THAT AS WELL                       
         J     SKED4                                                            
**********************************                                              
* WE HAVE A SPOT THAT MIGHT STAY IN THE COPY                                    
**********************************                                              
SKED10   DS    0H                                                               
*&&DO                                                                           
** THIS BLOCK HAS BEEN COMMENTED OUT AS IT REMOVES +OTO                         
** IT USED TO RELEVANT IN BUY/TRANSFER, BUT NOT IN THIS CODE                    
         CLC   0(1,R6),ELCDHI      IS THIS AN OTO                               
         JNE   SKED12                                                           
         CLI   DATEOPT,C'X'        TEST EXACT COPY                              
         JNE   SKED6               NO - DELETE ELEM AND ASSOCIATES              
*&&                                                                             
SKED12   TM    RSTATUS-REGELEM(R6),RSMINUSQ+RSMINSDQ  MINUS OR MINUSED?         
         JNZ   SKED6                                  YES, DON'T COPY           
*                                                                               
         OC    FULL(2),FULL        TEST IF START DATE SET                       
         JNZ   *+10                YES                                          
         MVC   FULL(2),2(R6)                                                    
         MVC   FULL+2(2),2(R6)                                                  
*                                                                               
         GOTO1 ABLDEL,DMCB,(RC)    ELEM                                         
*                                                                               
         BRAS  RE,CHGALLOC         CHANGE THE ALLOCATION                        
         JNE   SKED6               COULDN'T, DELETE THE SPOT                    
*                                                                               
         CLI   DATEOPT,C'X'        TEST EXACT COPY                              
         JE    SKED19              WE ALREADY MODIFIED IT IN CHGALLOC           
*                                                                               
         LA    RE,ELEM                                                          
         USING REGELEM,RE                                                       
         MVC   RCODE,ELCDLO                                                     
         MVC   RDATE,RDATE-REGELEM(R6) EXTRACT SPOT DATE                        
         MVC   RSTATUS,RSTATUS-REGELEM(R6)                                      
*                                                                               
         NI    RSTATUS,X'FF'-X'40'-X'10'-X'02'  UNSET -,MG,MGPNDG               
*                                                                               
         TM    RSTATUS,X'04'       TEST FOR HIATUS                              
         JZ    *+8                                                              
         MVI   RLEN,10             IF SO, IGNORE ANY ALLOCATION                 
*                                                                               
         CLI   RCODE,X'0B'         TEST FOR POL ELEMENT                         
         JL    SKED16                                                           
         MVC   RNUM,RNUM-REGELEM(R6) EXTRACT NPW                                
         J     SKED18                                                           
*                                                                               
SKED16   MVC   RPCOST,RPCOST-REGELEM(R6)                                        
         CLI   BDMASPRD,0                                                       
         JE    SKED18                                                           
         MVC   RPTIME,BDSEC        NOTE - ASSUMES NO PIGGYBACK !!               
         MVC   RPPRD,BDMASPRD      SET ALLOCATION                               
*                                                                               
SKED18   GOTO1 VRECUP,DMCB,BUYREC,(R6)                                          
         GOTO1 (RF),(R1),BUYREC,ELEM,(R6)                                       
*                                                                               
SKED19   AHI   R3,1                BUMP COUNT OF ELEMS                          
*                                                                               
         BRAS  RE,DELR4ELM         DELETE THE ELEMENT FROM ORIGINAL             
*                                                                               
         CLC   FRCLT,TOCLT         TEST SAME CLIENT                             
         BE    SKED2               YES - GO PROCESS NEXT ELEMENT                
         LLC   R0,1(R6)                                                         
         AR    R6,R0               POINT TO NEXT ELEMENT                        
         OI    MISCFLG1,MF1LVR4E   LEAVE R4 ALONE AS WE DELETE ELEM             
         B     SKED7               AND DELETE ASSOCIATED ELEMENTS               
         DROP  RE                                                               
*                                                                               
SKED20   LTR   R3,R3               TEST IF ANY SPOT ELEMENTS LEFT               
         BZ    SKNEQXIT            NO-SKIP RECORD                               
         GOTO1 DATCON,DMCB,(2,FULL),(3,BDSTART)                                 
         GOTO1 (RF),(R1),(2,FULL+2),(3,BDEND)                                   
SKEQXIT  CR    RB,RB                                                            
         J     EXIT                                                             
SKNEQXIT LTR   RB,RB                                                            
         J     EXIT                                                             
*                                                                               
NXTR4EL  TM    MISCFLG1,MF1LVR4E   LEAVE R4 ALONE?                              
         JNZ   NXTR4ELY                                                         
NXTR4EL1 SR    R0,R0               SPECIAL NEXTEL FOR PARSING THRU              
         ICM   R0,1,1(R4)             ELEMENTS OF ASAVEBUY IN SKED              
         JZ    NXTR4ELD0              ROUTINE                                   
         AR    R4,R0                                                            
*                                                                               
NXTR4EL2 CLI   0(R4),0             EOR RETURNS NEQ                              
         JE    NXTR4ELN                                                         
         CLC   0(1,R4),ELCDLO                                                   
         JL    NXTR4EL1                                                         
         CLC   0(1,R4),ELCDHI                                                   
         JH    NXTR4EL1                                                         
NXTR4ELY CR    RE,RE               SET CC EQUAL AND EXIT                        
         BR    RE                                                               
NXTR4ELN LTR   RE,RE               SET CC NOT EQUAL                             
         BR    RE                                                               
NXTR4ELD0 DC    H'0'                                                            
*                                                                               
***************                                                                 
* DELETE SPOT ELEMENT POINTED BY R4 OF ASAVEBUY                                 
***************                                                                 
DELR4ELM NTR1                                                                   
*                                                                               
DR4ELM10 GOTO1 VRECUP,DMCB,ASAVEBUY,(R4)  DELETE THE ELEMENT                    
*****                                                                           
* REMEMBER - WE ARE POINTING AT THE NEXT ELEMENT NOW                            
*****                                                                           
         CLI   0(R4),0             TEST E-O-R                                   
         BE    DR4ELMX                                                          
         CLI   0(R4),X'10'         DELETE ANY ASSOCIATED ELEMS                  
         BL    DR4ELM20            (X'10'-X'19')                                
         CLI   0(R4),X'19'                                                      
         BNH   DR4ELM10                                                         
*                                                                               
DR4ELM20 BRAS  RE,NXTR4EL2         TEST NEED THIS ELEMENT                       
         BNE   DR4ELMX                                                          
*                                                                               
         CLI   DATEOPT,C'X'        TEST EXACT COPY                              
         BE    DR4ELMX                                                          
         CLC   0(1,R6),ELCDHI      TEST ELEM IS AN OTO                          
         BE    DR4ELM10            YES DELETE                                   
*                                                                               
DR4ELMX  J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CHANGE THE ALLOCATION OF THE SPOT                                             
* ON ENTRY:    (R6)                SPOT ELEMENT                                 
*                                                                               
* NOTE: WORK WILL GET CLOBBERED                                                 
***********************************************************************         
CHGALLOC NTR1                                                                   
         USING REGELEM,R6                                                       
         CLI   RCODE,X'0B'         SPOT FOR BRAND BUYING?                       
         JL    CHGA020             YES, NOTHING TO CHANGE EXCEPT DATE           
*                                                                               
         CLI   RLEN,RLPOL1LQ       LENGTH OF 14, ALLOCATED?                     
         JL    CHGA020             NO, NOTHING TO CHANGE EXCEPT DATE            
*                                                                               
         LA    R2,RPALLOC                                                       
*                                                                               
CHGA010  XC    WORK,WORK                                                        
         MVC   WORK(1),0(R2)       FIND THE BINARY PRODUCT CODE                 
         GOTO1 BINSRCH,BINPARMS,(X'00',WORK),AFTPETAB                           
         CLI   BINPARMS,X'01'      DID WE FIND IT?                              
         JE    CHGAN               NO, SO WE KNOW TO DELETE                     
*                                                                               
         L     R4,BINPARMS         R4=A(BINARY PRODUCT)                         
         USING FTPRDETD,R4                                                      
         CLI   FTPRDETP,0          PRODUCT EXISTS IN 'TO' CLIENT?               
         JE    CHGAN               NO, SO WE KNOW TO DELETE                     
         CLI   FTPRDETE,0          ESTIMATE EXISTS FOR 'TO' CLT/PRD ?           
         JE    CHGAN               NO, SO WE KNOW TO DELETE                     
*                                                                               
         MVC   0(1,R2),FTPRDETP    NEW BINARY PRODUCT CODE                      
*                                                                               
         CLI   RLEN,RLPOL2LQ       LENGTH OF 18, PIGGYBACK ALLOCATED?           
         JL    CHGA020             NO, WE'RE DONE                               
*                                                                               
         LA    R0,RPALLOC2         DID WE ALREADY CONVERT PIGGYBACK?            
         CR    R2,R0                                                            
         JE    CHGA020             YES, WE'RE DONE                              
         LA    R2,RPALLOC2                                                      
         J     CHGA010             NO, CONVERT PIGGY AS WELL                    
*                                                                               
CHGA020  CLI   BXFOPT5,C'Y'        DATE SHIFT OPTION?                           
         BNE   CHGA030                                                          
         OC    DYSDIFFR,DYSDIFFR   ANY DAYS DIFFERENCE?                         
         BZ    CHGA025             NONE, SO NOTHING TO CHANGE HERE              
         XC    WORK,WORK                                                        
         GOTO1 DATCON,DMCB,(2,RDATE),(0,WORK)                                   
*                                                                               
         MVC   DMCB+8(4),DYSDIFFR  PARAM 3 - FULL WORD # OF DAYS                
         GOTO1 ADDAY,DMCB,WORK,WORK+6    ADD DAYS DIFFERENCE                    
         GOTO1 DATCON,DMCB,(0,WORK+6),(2,RDATE)  DATE IS ADJUSTED               
CHGA025  OI    MISCFLG1,MF1DTADJ   DATE WAS ADJUSTED                            
*                                                                               
CHGA030  CLC   RDATE,CPERTOST      SPOT DATE W/IN "TO" PERIOD                   
         BL    CHGAN                                                            
         CLC   RDATE,CPERTOND                                                   
         BH    CHGAN               SPOT DATE 'NOT' IN "TO" PERIOD               
*                                                                               
CHGA040  XC    ELEM,ELEM                                                        
         LLC   RF,RLEN                                                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),RCODE                                                    
*                                                                               
CHGAY    J     EQXIT                                                            
CHGAN    J     NEQXIT                                                           
*                                                                               
         DROP  R4                                                               
***********************************************************************         
* DISPLAY  DATE AND PRODUCT OF SPOT WE'RE NOT MOVING                            
* ON ENTRY:    (R6)                SPOT ELEMENT                                 
*                                                                               
* NOTE: WORK WILL GET CLOBBERED                                                 
***********************************************************************         
PRTR6SPT NTR1                                                                   
         LA    R3,P                                                             
         USING NTMOVPRD,R3                                                      
*                                                                               
         USING REGELEM,R6                                                       
         CLI   RCODE,X'0B'         ONLY WANT SPOTS                              
         JL    PR6SPX              YES, NOTHING TO CHANGE EXCEPT DATE           
         CLI   RCODE,X'0C'         SPOT FOR BRAND BUYING?                       
         JH    PR6SPX              YES, NOTHING TO CHANGE EXCEPT DATE           
*                                                                               
**** WE CAN CHECK IF THE SPOT IS WITHIN THE PERIOD, OTHERWISE PR6SPX            
         CLI   BXFOPT5,C'Y'        DATE SHIFTING?                               
         BNE   PR6SP005            NO, SPOT MUST BE IN 'TO' PERIOD              
         TM    MISCFLG1,MF1DTADJ   WAS THE DATE ADJUSTED?                       
         BNZ   PR6SP005            THEN SPOT MUST BE IN 'TO' PERIOD             
         CLC   RDATE,CPERFRST      YES, SPOT MUST BE IN 'FROM' PERIOD           
         BL    PR6SPX                                                           
         CLC   RDATE,CPERFRND                                                   
         BNH   PR6SP010            WE HAVE A SPOT!                              
         B     PR6SPX                                                           
*                                                                               
PR6SP005 CLC   RDATE,CPERFRST      SPOT MUST BE IN 'FROM' PERIOD                
         BL    PR6SPX                AND ALSO IN "TO" PERIOD                    
         CLC   RDATE,CPERFRND                                                   
         BH    PR6SPX                                                           
         CLC   RDATE,CPERTOST                                                   
         BL    PR6SPX                                                           
         CLC   RDATE,CPERTOND                                                   
         BH    PR6SPX              DON'T REPORT ON SPOT IF NOT                  
*                                                                               
PR6SP010 TM    RSTATUS,RSMINUSQ+RSMINSDQ  MINUS OR MINUSED?                     
         JNZ   PR6SPX                     YES, DON'T REPORT THESE               
*                                                                               
         CLI   RLEN,RLPOL1LQ       LENGTH OF 14, ALLOCATED?                     
         JL    PR6SP050            NO, SHOULD WE JUST DISPLAY DATE?             
*                                                                               
         LA    R2,RPALLOC                                                       
*                                                                               
PR6SP020 XC    WORK,WORK                                                        
         MVC   WORK(1),0(R2)       FIND THE BINARY PRODUCT CODE                 
         GOTO1 BINSRCH,BINPARMS,(X'00',WORK)                                    
         CLI   BINPARMS,X'01'      DID WE FIND IT?                              
         JE    PR6SP050            NO, SO WE KNOW TO DELETE                     
*                                                                               
         L     R4,BINPARMS         R4=A(BINARY PRODUCT)                         
         USING FTPRDETD,R4                                                      
         LA    R0,RPALLOC2         WE LOOKING AT PIGGYBACK?                     
         CR    R2,R0                                                            
         BE    PR6SP025            YES                                          
         MVC   NTMPPR1,FTPRDEQP    DISPLAY EBCDIC PRD CODE                      
         B     PR6SP030            YES                                          
*                                                                               
PR6SP025 MVC   NTMPPR2,FTPRDEQP    DISPLAY EBCDIC PIGGY CODE                    
*                                                                               
PR6SP030 CLI   FTPRDETP,0          PRODUCT EXISTS IN 'TO' CLIENT?               
         JE    PR6SP040            SHOW PRODUCT DOESN'T EXIST IN 'TO'           
         CLI   FTPRDETE,0          ESTIMATE EXISTS FOR 'TO' CLT/PRD ?           
         JE    PR6SP045            SHOW PRD/EST DOESN'T EXIST IN 'TO'           
*                                                                               
         CLI   RLEN,RLPOL2LQ       LENGTH OF 18, PIGGYBACK ALLOCATED?           
         JL    PR6SP050            NO, SHOW THE SPOT DATE                       
*                                                                               
         LA    R0,RPALLOC2         DID WE ALREADY CONVERT PIGGYBACK?            
         CR    R2,R0                                                            
         JE    PR6SP050            YES, WE'RE DONE                              
         LA    R2,RPALLOC2                                                      
         J     PR6SP020            NO, CONVERT PIGGY AS WELL                    
*                                                                               
PR6SP040 MVC   NTMPRESN(26),=C'MISSING PRD IN ''TO'' CLIENT'                    
         J     PR6SP050            YES, DISPLAY DATE                            
*                                                                               
PR6SP045 MVC   NTMPRESN(31),=C'MISSING PRD/EST FOR ''TO'' CLIENT'               
         J     PR6SP050            YES, DISPLAY DATE                            
*                                                                               
PR6SP050 GOTO1 DATCON,DMCB,(2,RDATE),(17,NTMPDATE)                              
         CLI   RCODE,X'0B'                                                      
         JE    PR6SPPRT                                                         
*                                                                               
         MVI   NTMPOTOS,C'+'       SO THIS COULD BE A +OTO                      
         TM    RSTATUS,RSMINUSQ    MINUS SPOT?                                  
         BZ    PR6SPPRT                                                         
         MVI   NTMPOTOS,C'-'        OR A -OTO                                   
*                                                                               
PR6SPPRT MVC   NTMPSPOT,=C'ERROR: SPOT'                                         
         MVC   NTMPOFLN,=C'FROM LINE'                                           
         L     RE,AIO                                                           
         XR    RF,RF                                                            
         ICM   RF,3,BUYKBUY-BUYKEY(RE)                                          
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  NTMPLINE,DUB                                                     
         MVC   NTMPNTMV,=CL13'**NOT MOVED**'                                    
         GOTO1 SPOOL,DMCB,(R8)     PRINT A LINE                                 
*                                                                               
PR6SPX   J     EXIT                                                             
         DROP  R3,R4,R6                                                         
NTMOVPRD DSECT                                                                  
NTMPSPOT DS    CL11                C'ERROR: SPOT'                               
         DS    CL2                                                              
NTMPOTOS DS    CL1                 OTO SIGN:'+' OR '-'                          
NTMPDATE DS    CL8                 MMMDD/YY                                     
         DS    CL2                                                              
NTMPPR1  DS    CL3                 PRODUCT 1                                    
         DS    CL1                                                              
NTMPPR2  DS    CL3                 PIGGYBACK PRODUCT                            
         DS    CL1                                                              
NTMPOFLN DS    CL9                 'FROM LINE'                                  
         DS    CL1                                                              
NTMPLINE DS    CL3                 '999'                                        
         DS    CL2                                                              
NTMPNTMV DS    CL13                '**NOT MOVED**'                              
         DS    CL2                                                              
NTMPRESN DS    CL50                REASON TEXT                                  
*                                                                               
T21778   CSECT                                                                  
         EJECT                                                                  
*                                                                               
* SUB-ROUTINE TO READ STATION MASTER RECORD AND EXTRACT TAX                     
* AT ENTRY, THISSTA CONTAINS CALL LETTERS TO READ FOR                           
*                                                                               
GETSTA   NTR1  BASE=*,LABEL=*                                                   
         XC    SVTAX,SVTAX                                                      
         XC    SVPST,SVPST                                                      
         MVC   SVNEWMKT,THISMKT                                                 
         LA    R6,KEY                                                           
         USING STARECD,R6                                                       
         MVI   STAKEY,C'0'                                                      
         MVC   STAKEY+1(L'STAKEY-1),STAKEY                                      
         MVI   STAKTYPE,C'S'                                                    
         MVC   STAKMED,QMED                                                     
         MVC   STAKCALL,THISSTA                                                 
         MVC   STAKAGY,AGENCY                                                   
         MVC   STAKCLT,QCLT                                                     
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO3,DMWORK              
         CLI   8(R1),0             TEST IF RECORD FOUND                         
         BNE   GETSTAX                                                          
         L     R6,AIO3                                                          
         MVC   SVTAX,SNEWTAX                                                    
         MVC   SVNEWMKT,SMKT                                                    
         MVC   SVPST,SPST                                                       
         MVI   SVDSTFLG,C'N'                                                    
         TM    SFLAG1,SQNODST                                                   
         BNO   *+8                                                              
         MVI   SVDSTFLG,C'Y'       STATION NOT ON DAYLIGHT SAVINGS TIME         
*                                                                               
GETSTAX  J     EXIT                                                             
         DROP  R6                                                               
         LTORG                                                                  
INIT     NTR1  BASE=*,LABEL=*                                                   
         LA    R0,(ADCONLSX-ADCONLST)/4                                         
         LAY   R1,ADCONLST                                                      
         LA    R2,ADCONS                                                        
*                                                                               
INIT2    L     RE,0(R1)                                                         
         A     RE,RELO                                                          
         ST    RE,0(R2)                                                         
         LA    R2,4(R2)                                                         
         LA    R1,4(R1)                                                         
         BCT   R0,INIT2                                                         
*                                  SET GENERAL SUB-ROUTINE ADDRESSES            
INIT4    LAY   R0,SPHIGH                                                        
         ST    R0,ASPHIGH                                                       
         LAY   R0,SPSEQ                                                         
         ST    R0,ASPSEQ                                                        
         LAY   R0,SPGET                                                         
         ST    R0,ASPGET                                                        
         LAY   R0,SPADD                                                         
         ST    R0,ASPADD                                                        
         LAY   R0,EXTMGR                                                        
         ST    R0,AEXTMGR                                                       
*                                                                               
INIT6    XC    DMCB(4),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A20'                                           
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VGETDEM,0(R1)                                                    
*                                                                               
         MVI   DMCB+7,X'78'                                                     
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VDBLBOOK,0(R1)                                                   
*                                                                               
         MVI   DMCB+7,X'5F'                                                     
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VGETRATE,0(R1)                                                   
*                                                                               
         MVI   DMCB+7,X'2B'                                                     
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VGETBUY,0(R1)                                                    
*                                                                               
         XC    LASTMKT,LASTMKT                                                  
*                                                                               
         L     R0,VADUMMY             STORAGE MEANT FOR PRINTREC MODE           
         ST    R0,VMKTLIST                                                      
         A     R0,=A(128*256)         32K                                       
         ST    R0,VMKTLSTX                                                      
*                                                                               
         CLI   OFFLINE,C'Y'        OFFLINE PROCESS?                             
         BNE   INIT7               NO - WE DON'T HAVE A(SSB)!                   
         L     RF,TWAMASTC         A(MASTC)                                     
         L     RE,MCSSB-MASTD(RF)  A(SSB)                                       
         USING SSBD,RE             SSB DSECT                                    
         OI    SSOSTAT2,SSOSROLC   RECOVER OFFLINE COPIES FOR 133               
         MVI   SSORPRG,SSORP       SET PROGRAM ID FOR RCVRHDR                   
         DROP  RE                  DROP SSB USING                               
*                                                                               
INIT7    J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO BUILD PRD/EST TABLE FOR "FROM" AND "TO"                         
***********************************************************************         
STPRDETB NTR1  BASE=*,LABEL=*                                                   
         LA    RE,BXFFRCLH                                                      
         CR    R2,RE                                                            
         JNE   STPRDE10                                                         
         GOTO1 ,BINPARMS,,AIO2,0,FTPRDELQ,(0,L'FTPRDEFP),MAXNPRDS               
*                                                                               
STPRDE10 L     R6,AIO                                                           
         LA    R0,CCLTIFC-CLTHDR(R6) SO WE DON'T GO BEYOND PRD LIST             
         LA    R6,CLIST-CLTHDR(R6)   POINT TO THE PRD LIST                      
*                                                                               
         LA    RE,BXFFRCLH           ON "FROM" CLIENT?                          
         CR    R2,RE                                                            
         JNE   STPRDE50              NO                                         
*                                                                               
         LA    R4,ELEM                                                          
         USING FTPRDETD,R4                                                      
STPRDE20 CLI   0(R6),0               EOT?                                       
         JE    STPRDEX                                                          
         CR    R6,R0                 OR BEYOND TABLE?                           
         JE    STPRDEX                                                          
*                                                                               
         CLC   =C'POL',0(R6)         DON'T NEED POL IN OUR TABLE                
         JE    STPRDE30                                                         
         XC    ELEM(FTPRDELQ),ELEM                                              
         MVC   FTPRDEQP,0(R6)        COPY QPRD AND BPRD                         
         MVC   FTPRDEFP,3(R6)                                                   
*                                                                               
         CLC   BXFFRCL,BXFTOCL       SAME CLIENT?                               
         JE    STPRDE40              YES, THEN BPRDS ARE SAME                   
         CLI   BXFTOCLH+5,0            OR NO "TO" CLIENT SPECIFIED              
         JE    STPRDE40                                                         
*                                                                               
STPRDE25 GOTO1 BINSRCH,BINPARMS,(X'01',FTPRDEFP)                                
         OC    BINPARMS+1(3),BINPARMS+1   TABLE FULL?                           
         JZ    STPRDED0                   YES, DIE                              
*                                                                               
STPRDE30 LA    R6,4(R6)              NEXT PRD CODE                              
         J     STPRDE20                                                         
*                                                                               
STPRDE40 MVC   FTPRDETP,FTPRDEFP     SAME BINARY PRODUCT CODES                  
         J     STPRDE25                                                         
         DROP  R4                                                               
*************************************                                           
* INSTEAD OF CONSTANTLY BRANCHING (POSSIBLY FOR 220 TIMES), "TO" CLIENT         
*   HAS ITS OWN SECTION HERE                                                    
*************************************                                           
STPRDE50 CLC   BXFFRCL,BXFTOCL       SAME CLIENT?                               
         JE    STPRDEX               YES, WE DID THIS DURING "FROM"             
*                                                                               
STPRDE60 CLI   0(R6),0               "TO" CLIENT HAS NO MORE PRDS?              
         JE    STPRDEX                                                          
         CR    R6,R0                 OR BEYOND TABLE?                           
         JNL   STPRDEX                                                          
*                                                                               
         CLC   =C'POL',0(R6)         DON'T NEED POL IN OUR TABLE                
         JE    STPRDE80                                                         
*                                                                               
         L     R4,AIO2                                                          
         USING FTPRDETD,R4                                                      
         XR    RE,RE                                                            
         ICM   RE,15,BINPARMS+8      GET NUMBER OF ENTRIES                      
         JZ    STPRDEX               NO ENTRIES                                 
*                                                                               
STPRDE65 CLC   FTPRDEQP,0(R6)        SAME EBCDIC PRODUCT CODE?                  
         JE    STPRDE70              YES, SAVE OFF THE "TO" BPRD                
         LA    R4,FTPRDELQ(R4)       NO, LOOP THROUGH THE TABLE                 
         JCT   RE,STPRDE65                FOR THE NUMBER OF ENTRIES             
*                                                                               
         J     STPRDE80               NO MATCH, SO WE DON'T CARE                
*                                                                               
STPRDE70 MVC   FTPRDETP,3(R6)        SAVE OFF ITS "TO" BPRD                     
         DROP  R4                                                               
*                                                                               
STPRDE80 LA    R6,4(R6)              NEXT PRD CODE IN "TO" CLIENT               
         J     STPRDE60                                                         
*                                                                               
STPRDEX  J     EXIT                                                             
STPRDED0 DC    H'0'                                                             
MAXNPRDS EQU   220                                                              
         LTORG                                                                  
FTPRDETD DSECT                                                                  
FTPRDETB DS    XL(MAXNPRDS*FTPRDELQ)                                            
FTPRDETX EQU   *                                                                
         ORG   FTPRDETB                                                         
FTPRDEFP DS    XL1                   "FROM" BINARY PRD  *KEY*                   
FTPRDEQP DS    CL3                   EBCDIC PRD CODE                            
FTPRDETP DS    XL1                   "TO"   BINARY PRD                          
FTPRDEFE DS    XL1                   "FROM" BINARY EST                          
FTPRDETE DS    XL1                   "TO"   BINARY EST                          
FTPRDELQ EQU   *-FTPRDEFP                                                       
T21778   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO FILL IN THE ESTIMATE INTO THOSE PRDS THAT HAVE THE EST          
***********************************************************************         
FILESTTB NTR1  BASE=*,LABEL=*                                                   
         LA    R6,KEY                                                           
         USING ESTHDRD,R6                                                       
         XC    KEY,KEY                                                          
         MVC   EKEYAM,BAGYMD                                                    
         MVC   EKEYCLT,BCLT                                                     
         MVC   EKEYEST,BEST          LOOKS FOR ALL PRDS WITH THIS EST           
FILESTHI BRAS  RE,SPHIGH                                                        
*                                                                               
         CLC   KEY(EKEYPRD-EKEY),KEYSAVE  SAME MEDIA/CLIENT?                    
         JNE   FILESTX                                                          
         CLC   EKEYEST,BEST          SAME ESTIMATE #?                           
         JE    FILEST20                                                         
         JL    FILEST10              <                                          
FILEST05 LLC   R1,EKEYPRD+2          > - SO BUMP EBCDIC PRD BY 1                
         LA    R1,1(R1)                                                         
         STC   R1,EKEYPRD+2                                                     
*                                                                               
FILEST10 MVC   EKEYEST,BEST          SET EST IN KEY TO IT                       
         XC    EKEYEST+1(EKCNTRL-(EKEYEST+1)),EKEYEST+1  CLEAR THE REST         
         J     FILESTHI                                                         
*                                                                               
FILEST20 OC    EKEYEST+1(EKCNTRL-(EKEYEST+1)),EKEYEST+1   BILLING?              
         JNZ   FILEST05              WE DON'T WANT BILLING RECORDS              
*                                                                               
         L     R4,AIO2                                                          
         USING FTPRDETD,R4                                                      
         XR    RE,RE                                                            
         ICM   RE,15,BINPARMS+8      GET NUMBER OF ENTRIES                      
         JZ    FILESTX               NO ENTRIES                                 
*                                                                               
FILEST30 CLC   FTPRDEQP,EKEYPRD      SAME EBCDIC PRODUCT CODE?                  
         JE    FILEST40              YES, SAVE OFF THE BEST                     
         LA    R4,FTPRDELQ(R4)       NO, LOOP THROUGH THE TABLE                 
         JCT   RE,FILEST30                FOR THE NUMBER OF ENTRIES             
*                                                                               
         J     FILEST05               NO MATCH, SHOULDN'T HAPPEN                
*                                                                               
FILEST40 LA    RE,BXFFRESH           ARE WE ON "FROM" EST?                      
         CR    R2,RE                                                            
         JNE   FILEST45                                                         
         MVC   FTPRDEFE,EKEYEST      YES, SAVE EST AS "FROM" EST                
         J     FILEST05              CHECK NEXT PRD                             
*                                                                               
FILEST45 MVC   FTPRDETE,EKEYEST      YES, SAVE EST AS "TO"   EST                
         J     FILEST05              CHECK NEXT PRD                             
         DROP  R4                                                               
*                                                                               
FILESTX  J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
FMTBUY   NTR1  BASE=*,LABEL=*                                                   
         LA    R2,P                                                             
         USING PRD,R2                                                           
         XR    R0,R0                                                            
         ICM   R0,3,BUYKBUY                                                     
         EDIT  (R0),(3,PRLIN),ALIGN=LEFT                                        
         GOTO1 DATCON,DMCB,(3,BDSTART),(8,PRSTART)                              
         CLC   BDSTART,BDEND       TEST SAME START AND END DATE                 
         BE    FMT2                YES                                          
         MVI   PRDASH,C'-'                                                      
         GOTO1 DATCON,DMCB,(3,BDEND),(8,PREND)                                  
*                                                                               
FMT2     LLC   R0,BDWKS                                                         
         EDIT  (R0),(3,PRWKS),ALIGN=LEFT                                        
         GOTO1 UNDAY,DMCB,BDDAY,PRDAY                                           
         MVC   PRTIME,SPACES                                                    
         GOTO1 UNTIME,DMCB,BDTIMST,PRTIME                                       
         LLC   R0,BDNOWK                                                        
         EDIT  (R0),(3,PRNPW),ALIGN=LEFT                                        
         MVC   PRDP,BDDAYPT                                                     
         LLC   R0,BDSEC                                                         
         EDIT  (R0),(3,PRLEN),ALIGN=LEFT                                        
         MVC   PRPROG(L'BDPROGRM),BDPROGRM                                      
*                                                                               
         CLI   SVPURP,C' '                                                      
         BNH   FMT3                                                             
         MVC   PRPROG+132(5),=C'PURP='                                          
         MVC   PRPROG+137(12),SVPURP                                            
*                                                                               
FMT3     LA    R3,PRCOST           EDIT OUT LINE COST                           
         TM    BDCIND2,X'04'       BDCIND IS A CHARACTER                        
         BZ    FMT3A                                                            
         MVC   BYTE,BDCIND                                                      
         B     FMT4                                                             
*                                                                               
FMT3A    MVI   BYTE,C'C'                                                        
         TM    BDCIND2,X'80'                                                    
         BO    FMT4                                                             
         MVI   BYTE,C' '                                                        
         TM    BDCIND,X'20'                                                     
         BO    FMT4                                                             
         MVI   BYTE,C'F'                                                        
         TM    BDCIND,X'80'                                                     
         BO    FMT4                                                             
         MVI   BYTE,C'Q'                                                        
         TM    BDCIND,X'40'                                                     
         BO    FMT4                                                             
         MVI   BYTE,C'N'                                                        
         TM    BDCIND,X'10'                                                     
         BO    FMT4                                                             
         MVI   BYTE,C'V'                                                        
         TM    BDCIND,X'08'                                                     
         BO    FMT4                                                             
         MVI   BYTE,C'S'                                                        
         TM    BDCIND,X'04'                                                     
         BO    FMT4                                                             
         MVI   BYTE,C'X'                                                        
         TM    BDCIND,X'02'                                                     
         BO    FMT4                                                             
         MVI   BYTE,C'P'                                                        
*                                                                               
FMT4     SR    R0,R0                                                            
         ICM   R0,7,BDCOST                                                      
         BZ    FMT5                                                             
         TM    BDCIND2,X'20'       TEST CANADIAN                                
         BO    *+16                                                             
         TM    BDCIND2,X'10'       TEST COST IN DOLLARS                         
         BZ    *+8                                                              
         MHI   R0,100                                                           
         C     R0,=F'9999999'      IF COST TOO BIG                              
         BH    FMT6                 DROP CENTS                                  
*                                                                               
FMT5     EDIT  (R0),(8,(R3)),2                                                  
         B     FMT8                                                             
*                                                                               
FMT6     SRDA  R0,32                                                            
         D     R0,=F'100'                                                       
         LR    R0,R1                                                            
         EDIT  (R0),(8,(R3))                                                    
*                                                                               
FMT8     TM    BDCIND2,X'04'       BDCIND IS A CHARACTER                        
         BZ    FMT8A                                                            
         TM    BDCIND2,BDC2NEG     TEST NEGATIVE COST                           
         BZ    FMT9                                                             
         B     FMT8B                                                            
FMT8A    TM    BDCIND,X'01'        TEST NEGATIVE COST                           
         BZ    *+8                                                              
FMT8B    MVI   8(R3),C'-'                                                       
*                                                                               
* FLOAT COST CHAR TO LEFT OF COST                                               
*                                                                               
FMT9     CLI   BYTE,C' '                                                        
         BE    FMT10                                                            
* FIND FIRST BLANK TO LEFT OF COST                                              
         LA    R1,6(R3)                                                         
         LA    R0,7                                                             
         CLI   0(R1),C' '                                                       
         BE    *+10                                                             
         BCTR  R1,0                                                             
         BCT   R0,*-10                                                          
         MVC   0(1,R1),BYTE        MOVE SPECIAL COST CODE                       
*                                                                               
FMT10    LA    R6,BDELEM                                                        
         MVI   ELCDLO,2            FIND DEMO EL                                 
         MVI   ELCDHI,2                                                         
         BRAS  RE,NEXTEL                                                        
         BNE   FMT15               NOT ON RECORD                                
         LLC   R4,1(R6)                                                         
         SHI   R4,24                                                            
         BNP   FMT15               NO DEMOS                                     
         SRL   R4,3                R2=N'DEMOS                                   
         CLI   SVNDEMS,0           TEST FOR ANY DEMOS IN EST HDR                
         BE    FMT15               NO                                           
         CLM   R4,1,SVNDEMS        DISPLAY NO MORE THAN 4 DEMOS                 
         BL    *+8                                                              
         IC    R4,SVNDEMS                                                       
         LA    R6,24(R6)           R6=A(DEMO FIELDS)                            
         LA    R3,PRDEM1           R3=OUTPUT POINTER                            
*                                                                               
FMT12    ICM   R0,15,4(R6)         GET DEMO VALUE                               
         N     R0,ANDMSK           MAKE SURE OVERRIDE BIT OFF                   
         EDIT  (R0),(6,(R3)),1                                                  
*                                                                               
FMT14    TM    4(R6),X'80'         TEST FOR OVERRIDE                            
         BZ    *+8                                                              
         MVI   6(R3),C'*'          YES-DENOTE AS OVERRIDE                       
         LA    R3,L'PRDEM1+1(R3)   NEXT OUTPUT AREA                             
         LA    R6,8(R6)            NEXT DEMO FIELD                              
         BCT   R4,FMT12                                                         
*                                                                               
FMT15    GOTO1 SPOOL,DMCB,(R8)     PRINT A LINE                                 
         BASR  RE,RF               SKIP A LINE AFTER                            
         J     EXIT                                                             
*                                                                               
ANDMSK   DS    0F                                                               
         DC    X'7FFFFFFF'                                                      
         LTORG                                                                  
         EJECT                                                                  
         SPACE 2                                                                
         DS    0D                                                               
         DC    C'*SAVEBUY'                                                      
SAVEBUY  DS    800D                                                             
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
* BUY TRANSFER MODULE WORKING STORAGE                                           
*                                                                               
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
RELO     DS    A                                                                
MYBASE1  DS    A                                                                
MYBASE2  DS    A                                                                
SAVERD   DS    A                                                                
TRIO1    DS    A                                                                
TRIO2    DS    A                                                                
VGETDEM  DS    A                                                                
VDBLBOOK DS    A                                                                
VDEMOCON DS    A                                                                
*                                                                               
ASPHIGH  DS    A                                                                
ASPSEQ   DS    A                                                                
ASPGET   DS    A                                                                
ASPADD   DS    A                                                                
AEXTMGR  DS    A                                                                
*BLDMKT  DS    A                                                                
         DS    A                                                                
*                                                                               
ADCONS   DS    0A                                                               
VRECUP   DS    A                                                                
VGETRATE DS    A                                                                
VMKTLIST DS    A                                                                
VMKTLSTX DS    A                                                                
ASAVEBUY DS    A                                                                
VXSORT   DS    A                                                                
ABLDEL   DS    A                                                                
AVALIMGR DS    A                                                                
VGETBUY  DS    A                                                                
ABLDMKT  DS    A                                                                
*                                                                               
AFTPETAB DS    A                      "FROM"/"TO" PRD EST TABLE                 
BINPARMS DS    6F                     BINSRCH PARAMETERS                        
*                                                                               
DYSDIFFR DS    F                   NEED FULL WORD                               
*                                                                               
FAKEFLDH DS    XL8                                                              
FAKEFLD  DS    CL79                                                             
*                                                                               
QPERFR   DS    0CL12                                                            
QPERFRST DS    CL6                                                              
QPERFRND DS    CL6                                                              
QPERTO   DS    0CL12                                                            
QPERTOST DS    CL6                                                              
QPERTOND DS    CL6                                                              
*                                                                               
BPERFRST DS    XL3                                                              
BPERFRND DS    XL3                                                              
CPERFRST DS    XL2                 COMPRESSED                                   
CPERFRND DS    XL2                                                              
BFRNMWKS DS    XL2                                                              
*                                                                               
BPERTOST DS    XL3                                                              
BPERTOND DS    XL3                                                              
CPERTOST DS    XL2                 COMPRESSED                                   
CPERTOND DS    XL2                                                              
BFRNMDWK DS    XL4                                                              
BTONMDWK DS    XL4                                                              
*                                                                               
STRQFLG1 DS    XL1                 STATION REQUEST FLAG 1                       
SRQFMGRP EQU   X'80'               - REQUESTING A MARKET GROUP                  
SRQFMRKT EQU   X'40'               - REQUESTING A MARKET                        
SRQFALL  EQU   X'20'               - REQUESTING ALL STATIONS                    
SRQFLIST EQU   X'10'               - REQUESTING A LIST OF STATIONS              
*                                                                               
MISCFLG1 DS    XL1                 MISC FLAG 1                                  
MF1PDSPT EQU   X'80'               - WE ENCOUNTERED A PAID SPOT                 
MF1MKGLN EQU   X'40'               - BUYLINE IS A MAKEGOOD LINE                 
MF1AFFID EQU   X'20'               - WE HAVE AN AFFID ELEM                      
MF1HAVSP EQU   X'10'               - WE HAVE A SPOT IN PERIOD                   
MF1DTADJ EQU   X'02'               - SPOT DATE WAS ADJUSTED                     
MF1LVR4E EQU   X'01'               - LEAVE R4 ALONE, WE'RE IN POSITION          
*                                                                               
ELCDLO   DS    XL1                                                              
ELCDHI   DS    XL1                                                              
DAY1     DS    XL1                                                              
DAYX     DS    XL1                                                              
POLESTSW DS    XL1                                                              
SVPOLNPW DS    X                                                                
SVCOPT1  DS    X                                                                
SVCOFFC  DS    C                                                                
SVB0PROF DS    CL16                                                             
SVDSTFLG DS    C                                                                
SVMZONE  DS    C                                                                
* FOR CANADIAN SOFT DEMOS                                                       
SVDMLST0 DS    CL4                                                              
SVDMLST1 DS    CL4                                                              
SVDMLFLG DS    XL1                                                              
SVDMLMKT DS    CL3                                                              
SVDMLRSV DS    CL1                                                              
* VALUES SAVED FROM ESTHDR                                                      
SVESTNAM DS    CL20                                                             
SVBOOK   DS    XL2                                                              
SVHUTADJ DS    XL1                                                              
SVDAYMNU DS    XL1                                                              
SVESTREP DS    XL2                                                              
SVWGTLST DS    XL20                                                             
SVDEMOS  DS    CL63                                                             
SVNDEMS  DS    X                   N'DEMOS ON REPORT (MAX=4)                    
SVDEMNAM DS    CL31                DEMO NAMES FOR REPORT HEADLINE               
SVPURP   DS    CL12                                                             
*                                                                               
QMGR     DS    CL5                                                              
DEMOOPT  DS    C                                                                
GOALOPT  DS    C                                                                
CMTOPT   DS    C                                                                
DATEOPT  DS    C                                                                
TRACEOPT DS    XL1                                                              
TESTOPT  DS    CL1                 C'Y'=TEST ONLY-DO NOT UPDATE                 
*                                                                               
         DS    0D                                                               
FRKEY    DS    XL20                                                             
         ORG   FRKEY                                                            
FRAGYMD  DS    XL1                                                              
FRCLT    DS    XL2                                                              
FRPRD    DS    XL1                                                              
FRMKT    DS    XL2                                                              
FRSTA    DS    XL3                                                              
FREST    DS    XL1                                                              
FRBUY    DS    XL3                                                              
         ORG   FRKEY+L'FRKEY                                                    
TOKEY    DS    XL20                                                             
         ORG   TOKEY                                                            
TOAGYMD  DS    XL1                                                              
TOCLT    DS    XL2                                                              
TOPRD    DS    XL1                                                              
TOMKT    DS    XL2                                                              
TOSTA    DS    XL3                                                              
TOEST    DS    XL1                                                              
TOBUY    DS    XL3                                                              
         ORG   TOKEY+L'TOKEY                                                    
FRKEYSV  DS    XL20                                                             
TOKEYSV  DS    XL20                                                             
*                                                                               
* EXTRACTED 'FROM' AND 'TO' VALUES                                              
*                                                                               
FRCLI    DS    CL3                 'FROM' CLIENT CODE (EBCDIC)                  
FRCLIN   DS    CL20                'FROM' CLIENT NAME                           
FRCOFFC  DS    XL(L'COFFICE)       'FROM' CLIENT PROFILE                        
FRCPROF  DS    XL(L'CPROF)         'FROM' CLIENT PROFILE                        
FRCAJPRF DS    CL16                'FROM' CLIENT'S AJ PROFILE                   
FRPOLNPW DS    X                   X'80' - POLNPW                               
FRPROD   DS    CL3                 'FROM' PRODUCT CODE (EBCDIC)                 
FRPRODN  DS    CL20                'FROM' PRODUCT NAME                          
FRESTN   DS    CL20                'FROM' ESTIMATE NAME                         
FRESTPOL DS    C                   Y=POOL ESTIMATE,N=NON-POOL                   
FREDYMNU DS    C                   'FROM' ESTIMATE DAYPART MENU                 
FREDEMOS DS    XL(L'SVDEMOS)       'FROM' ESTIMATE DEMO CATEGORIES              
TOCLI    DS    CL3                 'TO' CLIENT CODE (EBCDIC)                    
TOCLIN   DS    CL20                'TO' CLIENT NAME                             
TOPROD   DS    CL3                 'TO' PRODUCT CODE (EBCDIC)                   
TOPRODN  DS    CL20                'TO' PRODUCT NAME                            
TOESTN   DS    CL20                'TO' ESTIMATE NAME                           
TOESTPOL DS    C                   Y=POOL ESTIMATE, N=NON-POOL                  
*                                                                               
LASTMKT  DS    CL4                 LAST MARKET (CHARACTER)                      
THISMKT  DS    CL4                 MARKET (CHARACTER)                           
THISSTA  DS    CL8                 STATION (CHARACTER)                          
SVNEWMKT DS    CL4                 MARKET NUMBER FOR 'TO' CLT                   
SVTAX    DS    XL2                 THIS STATION'S TAX RATE                      
SVPST    DS    CL10                THIS STATION'S PST CODES                     
MGRNUM   DS    XL2                 MARKET GROUP NUMBER                          
*                                                                               
         DS    0F                                                               
REQTOTS  DS    0XL8                                                             
REQBUYS  DS    F                                                                
REQSPOTS DS    F                                                                
REQDOLS  DS    PL8                                                              
*                                                                               
MGRTOTS  DS    0XL8                                                             
MGRBUYS  DS    F                                                                
MGRSPOTS DS    F                                                                
MGRDOLS  DS    PL8                                                              
*                                                                               
MKTTOTS  DS    0XL8                                                             
MKTBUYS  DS    F                                                                
MKTSPOTS DS    F                                                                
MKTDOLS  DS    PL8                                                              
*                                                                               
STATOTS  DS    0XL8                                                             
STABUYS  DS    F                                                                
STASPOTS DS    F                                                                
STADOLS  DS    PL8                                                              
*                                  GETRATE BLOCK                                
BUYTOTS  DS    0XL8                                                             
BUYSPOTS DS    F                                                                
BUYDOLS  DS    F                                                                
*                                                                               
SPOTS    DS    F                                                                
GROSS    DS    F                                                                
NET      DS    F                                                                
ADJ      DS    F                                                                
*                                                                               
SVMGRKEY DS    XL13                                                             
*                                                                               
SVSPILL  DS    XL54                SAVE AREA FOR DPT SUMMARY SPILL              
         ORG   SVSPILL                                                          
SVSPLSTA DS    XL3                 STATION (PACKED)                             
SVSPLCLT DS    XL2                 CLIENT CODE                                  
SVSPLLST DS    12XL4               RSMKT/AGMKT                                  
SVSPILLX DS    XL1                 ALLOW FOR EOL FLAG                           
         SPACE 1                                                                
MGR1LEN  DS    CL1                 MARKET GROUP LEVEL 1 LENGTH                  
MGR2LEN  DS    CL1                 MARKET GROUP LEVEL 2 LENGTH (1+2)            
MGR3LEN  DS    CL1                 MARKET GROUP LEVEL 3 LENGTH (1+2+3)          
         SPACE 1                                                                
MGR1BK   DS    CL12                MARKET GROUP LEVEL 1 BREAK NAME              
         DS    CL1                                                              
MGR1     DS    CL5                 MARKET GROUP LEVEL 1 CODE                    
         DS    CL1                                                              
MGR1NM   DS    CL24                MARKET GROUP LEVEL 1 GROUP NAME              
         SPACE 1                                                                
MGR2BK   DS    CL12                MARKET GROUP LEVEL 2 BREAK NAME              
         DS    CL1                                                              
MGR2     DS    CL5                 MARKET GROUP LEVEL 2 CODE                    
         DS    CL1                                                              
MGR2NM   DS    CL24                MARKET GROUP LEVEL 2 GROUP NAME              
         SPACE 1                                                                
MGR3BK   DS    CL12                MARKET GROUP LEVEL 3 BREAK NAME              
         DS    CL1                                                              
MGR3     DS    CL5                 MARKET GROUP LEVEL 3 CODE                    
         DS    CL1                                                              
MGR3NM   DS    CL24                MARKET GROUP LEVEL 3 GROUP NAME              
         SPACE 2                                                                
STALCNT  DS    XL1                 NUMBER OF STATION IN LIST                    
GETBLK   DS    XL64                BLOCK FOR SPGETBUY                           
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
* COPY ERROR EQUATES                                                            
*                                                                               
POLMSG   EQU   1                   POL TO NON-POL                               
PIGMSG   EQU   2                   COPYING PIGGYBACK                            
NOSVCMSG EQU   3                   NO RATING SERVICE ON BUY LINE                
BADSVMSG EQU   4                   BAD RATING SERVICE                           
NOBKMSG  EQU   5                   NO BOOK                                      
SPOTSMSG EQU   6                   TOO MANY SPOTS ON BUY RECORD                 
LOCKMSG  EQU   7                   STATION IS LOCKED                            
CANTLOCK EQU   7                   UNABLE TO LOCK DAY/TIME                      
NODEMO   EQU   8                   NOT SUPPORTED IN DEM                         
ESTLCKD  EQU   328                 ESTIMATE HEADER LOCKED                       
NONETCPY EQU   810                 CANNOT TRANSFER NETWORK                      
         SPACE 2                                                                
* DSECT TO COVER BUY LINE PRINT DETAIL                                          
*                                                                               
PRD      DSECT                                                                  
         DS    X                                                                
PRLIN    DS    CL5                                                              
         DS    CL1                 SPARE                                        
PRPER    DS    0CL17                                                            
PRSTART  DS    CL8                                                              
PRDASH   DS    C                                                                
PREND    DS    CL8                                                              
         DS    CL2                 SPARE                                        
PRWKS    DS    CL2                                                              
         DS    CL3                                                              
PRDAY    DS    CL5                                                              
         DS    CL2                 SPARE                                        
PRTIME   DS    CL11                                                             
         DS    CL2                 SPARE                                        
PRNPW    DS    CL2                                                              
         DS    CL3                                                              
PRDP     DS    C                                                                
         DS    CL3                                                              
PRLEN    DS    CL3                                                              
         DS    CL2                                                              
PRPROG   DS    CL19                                                             
         DS    CL2                                                              
PRCOST   DS    CL9                                                              
         DS    CL2                 SPARE                                        
PRDEM1   DS    CL7                                                              
         DS    CL1                 SPARE                                        
PRDEM2   DS    CL7                                                              
         DS    CL1                 SPARE                                        
PRDEM3   DS    CL7                                                              
         DS    CL1                 SPARE                                        
PRDEM4   DS    CL7                                                              
         DS    CL1                 SPARE                                        
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
       ++INCLUDE SPDSTBLK                                                       
         EJECT                                                                  
       ++INCLUDE SPSFMFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM2AD                                                       
         ORG   CONHEADH-64                                                      
       ++INCLUDE DDGENTWA                                                       
         PRINT OFF                                                              
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDPARSNIPD                                                     
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDMASTD                                                        
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
MKGRECD  DSECT                                                                  
       ++INCLUDE SPGENMKG                                                       
SDEFRECD DSECT                                                                  
       ++INCLUDE SPGENSDEF                                                      
MTRRECD  DSECT                                                                  
       ++INCLUDE SPGENMTR                                                       
       ++INCLUDE SPGETBUYD                                                      
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE FATIOB                                                         
SSBD     DSECT                                                                  
       ++INCLUDE FASSBOFF                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016SPSFM78   06/10/13'                                      
         END                                                                    
