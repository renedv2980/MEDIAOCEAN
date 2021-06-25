*          DATA SET SPWRI26    AT LEVEL 100 AS OF 12/16/04                      
*PHASE T20426A,*                                                                
*INCLUDE FINDOUT                                                                
*                                                                               
*********************************************************************           
*                                                                   *           
*          SPWRI26 (T20426) - MEDIA CALENDAR (DC) REPORT            *           
*                                                                   *           
*-------------------------------------------------------------------*           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* 03NOV03 18 AKT -- FIX MGROUP X'40' BUGS                           *           
* 03MAR00 97 EFJ -- SUPPORT FOR SPOTBUY TO SPLIT BUYS               *           
*                -- MOVE SETWKS TO CREATE ADDRESSABILITY            *           
*                -- PASS !@#$%^& ACOMFACS TO FINDOUT!               *           
* 24MAY96 96 EFJ -- SEPERATE MOLSON FROM MILLER OUTLETS FOR AGY TH  *           
*                -- SKIP SECOND MILLER CORPORATE OUTLET             *           
* 18APR96 95 EFJ -- INCREASE FINDOUT BUFFER                         *           
* 15APR96 94 MHER - USE B9A PROFILE                                 *           
*                                                                   *           
*                                                                   *           
*********************************************************************           
         TITLE 'T20426 - MEDIA CALENDAR'                                        
T20426   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKL,T20426,RA                                                  
         LR    R8,RC                                                            
         USING WORKD,R8                                                         
         MVC   SAVERD,4(RD)        SAVE A(CALLING PROGRAM'S SAVE AREA)          
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R9,ASYSD                                                         
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
*                                                                               
         L     R6,=A(GDAREA)                                                    
         USING GDSECT,R6                                                        
*                                                                               
         L     R1,TWADCONS                                                      
         L     R1,TSPFUSER-TWADCOND(R1)                                         
         ST    R1,ASAVE                                                         
*                                  REPORT CALLING MODE                          
         CLI   RPMODE,RPFIRST      REQUEST FIRST HOOK                           
         BE    FRST                                                             
         CLI   RPMODE,RPINIT       INITIALIZATION                               
         BE    INIT                                                             
         CLI   RPMODE,RPINPUT      SPOTIO HOOK                                  
         BE    INPUT                                                            
         CLI   RPMODE,RPFINAL      REQUEST LAST                                 
         BE    FIN                                                              
         CLI   RPMODE,RPDRHOOK     DRIVER HOOK                                  
         BE    DRHOOK                                                           
         CLI   RPMODE,RPRUNLST     RUNLAST                                      
         BE    LAST                                                             
         B     XIT                                                              
*                                                                               
EQXIT    CR    RB,RB                                                            
         B     XIT                                                              
*                                                                               
NEXIT    LTR   RB,RB                                                            
         B     XIT                                                              
*                                                                               
XIT      XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* REQUEST FIRST                                                       *         
***********************************************************************         
         SPACE 1                                                                
FRST     L     RE,ASAVE            RESTORE SAVED VALUES                         
         LA    RF,SAVVALSL                                                      
         LA    R0,SAVVALS                                                       
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         CLI   OFFLINE,C'Y'        TEST OFFLINE                                 
         BNE   FRSTX                                                            
         OC    ADISTAB,ADISTAB     TEST VERY FIRST REQUEST                      
         BNZ   FRSTX                                                            
         LA    R4,DTMAX            YES-GET STORAGE FOR DISTRIBUTOR TAB          
         LR    R3,R4                                                            
         LA    R5,DTLEN                                                         
         MR    R2,R5               MAX N'RECORDS * L'ENTRY                      
         LA    R3,20(R3)                                                        
         ST    R3,DMCB+4                                                        
         ST    R3,DMCB+8                                                        
         GOTO1 COVAIL,DMCB,C'GET'                                               
         ICM   RE,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RE,ADISTAB                                                       
         LA    RF,20(RE)                                                        
         ST    RF,0(RE)            +0 = A(TABLE)                                
         XC    4(4,RE),4(RE)       +4 = NUMBER OF RECORDS SO FAR                
         ST    R5,8(RE)            +8 = L'RECORD ENTRY                          
         LA    R1,L'DTKEY                                                       
         ST    R1,12(RE)           +12 = LENGTH OF KEY                          
         ST    R4,16(RE)           +16 = MAX NUMBER OF RECORD                   
*                                                                               
         LA    R4,RTMAX            GET STORAGE FOR RECAP TABLE                  
         LR    R3,R4                                                            
         LA    R5,RTLEN                                                         
         MR    R2,R5               MAX N'RECORDS * L'ENTRY                      
         LA    R3,20(R3)                                                        
         ST    R3,DMCB+4                                                        
         ST    R3,DMCB+8                                                        
         GOTO1 COVAIL,DMCB,C'GET'                                               
         ICM   RE,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RE,ARCPTAB                                                       
         LA    RF,20(RE)                                                        
         ST    RF,0(RE)            +0 = A(TABLE)                                
         XC    4(4,RE),4(RE)       +4 = NUMBER OF RECORDS SO FAR                
         ST    R5,8(RE)            +8 = L'RECORD ENTRY                          
         LA    R1,L'RTKEY                                                       
         ST    R1,12(RE)           +12 = LENGTH OF KEY                          
         ST    R4,16(RE)           +16 = MAX NUMBER OF RECORD                   
*                                                                               
         DS    0H                  GET STORAGE FOR FINDOUT TABLES               
         GOTO1 COVAIL,DMCB,C'GET',560000,560000    560K                         
         ICM   RE,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RE,AFOTABS                                                       
*                                                                               
FRSTX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
*INIT    MVI   CMTOPT,0            PRINT COMMENTS OPTION                        
*        LA    R2,CALCOMH                                                       
*        CLI   5(R2),0                                                          
*        BE    INIT2                                                            
*        MVC   CMTOPT,8(R2)                                                     
*        CLI   CMTOPT,C'Y'                                                      
*        BE    INIT2                                                            
*        CLI   CMTOPT,C'N'                                                      
*        BNE   EINV                                                             
*                                                                               
INIT     MVI   AFFILOPT,0          PRINT NETWORK AFFILIATE OPTION               
         LA    R2,CALNETH                                                       
         CLI   5(R2),0                                                          
         BE    INIT3                                                            
         MVC   AFFILOPT,8(R2)                                                   
         CLI   AFFILOPT,C'Y'                                                    
         BE    INIT3                                                            
         CLI   AFFILOPT,C'N'                                                    
         BNE   EINV                                                             
*                                                                               
INIT3    MVI   CHANOPT,0            PRINT CHANNEL OPTION                        
         LA    R2,CALCHAH                                                       
         CLI   5(R2),0                                                          
         BE    INIT4                                                            
         MVC   CHANOPT,8(R2)                                                    
         CLI   CHANOPT,C'Y'                                                     
         BE    INIT4                                                            
         CLI   CHANOPT,C'N'                                                     
         BNE   EINV                                                             
*                                                                               
INIT4    MVI   DISTOPT,C'Y'         DISTRIBUTORS OPTION                         
         LA    R2,CALDISH                                                       
         CLI   5(R2),0                                                          
         BE    INIT6                                                            
         MVC   DISTOPT,8(R2)                                                    
         CLI   DISTOPT,C'Y'                                                     
         BE    INIT6                                                            
         CLI   DISTOPT,C'N'                                                     
         BNE   EINV                                                             
*                                                                               
INIT6    MVI   SUMOPT,C'N'          MARKET SUMMARY OPTION                       
         LA    R2,CALSUMH                                                       
         CLI   5(R2),0                                                          
         BE    INIT10                                                           
         MVC   SUMOPT,8(R2)                                                     
         CLI   SUMOPT,C'Y'                                                      
         BE    INIT10                                                           
         CLI   SUMOPT,C'N'                                                      
         BNE   EINV                                                             
*                                                                               
INIT10   OI    SBQSKIP,SBQSKGL+SBQSKBIL   DON'T READ GOALS OR BILLS             
         MVI   MYFIRSTH,12         FIRST HEADING ON HEAD12                      
         MVC   TITLE,BLANKS                                                     
         MVC   TITLE(14),=C'MEDIA CALENDAR'                                     
         GOTO1 CENTER,DMCB,TITLE,63                                             
*                                                                               
         MVI   RCPSW,C'N'          NO RECAP YET                                 
         MVI   CANCEL,0                                                         
         MVI   SBQPER,SBQPWK+SBQPQT  WEEKLY AND QTR ANALYSIS                    
         MVI   SBQPERLO,1          ALL WEEKS                                    
         MVI   SBQPERHI,X'FF'                                                   
         MVI   SBEDEMTY,0          NO DEMOS                                     
         MVI   SBEUNALL,C'N'       NO UNALLOCATED SPOTS                         
         OI    DATAIND4,DIESTFLT   ESTIMATE FILTERS                             
         OI    DATAIND4,DISLN      SPOT LENGTHS                                 
         OI    DATAIND7,DIRTLSCH   ESTIMATE RETAIL SCHEMES                      
         CLI   AFFILOPT,C'Y'       TEST AFFILIATES NEEDED                       
         BNE   *+8                                                              
         OI    DATAIND2,DIAFFIL                                                 
         OI    OUTIND,OUTIHEAD     SHUFFLE EXTRA HEADLINE                       
         MVI   FRSTLAST,C'Y'       ASK FOR RUNFRST AND RUNLAST                  
         OI    DRINDS,GLPALTOT     PRINT ALL TOTALS                             
         OI    DRINDS2,GLMIDHED    MIDHEAD ON CHANGE OF RECORD                  
*                                                                               
         XC    LEVELS,LEVELS       SET THE LEVELS                               
         LA    R1,LEVELS                                                        
         LA    RE,RPTLEVS                                                       
         LA    RF,1                                                             
         MVI   MIDLEV,0                                                         
         CLI   SBQMGRD,0           SET MARKET GROUPS                            
         BH    *+14                                                             
         XC    6(6,RE),6(RE)                                                    
         B     INIT12                                                           
         CLC   SBMGR1LN,SBMGR2LN                                                
         BNE   *+14                                                             
         XC    8(4,RE),8(RE)                                                    
         B     INIT12                                                           
         CLC   SBMGR2LN,SBMGR3LN                                                
         BNE   INIT12                                                           
         XC    10(2,RE),10(RE)                                                  
*                                                                               
INIT12   CLI   0(RE),X'FF'                                                      
         BE    INITX                                                            
         CLI   0(RE),0                                                          
         BE    INIT14                                                           
         MVC   0(1,R1),0(RE)                                                    
         CLI   1(RE),1                                                          
         BNE   *+8                                                              
         STC   RF,LSTHEDLV         LAST HEADLINE LEVEL                          
         CLI   1(RE),2                                                          
         BNE   *+8                                                              
         STC   RF,MKTLEV           MARKET LEVEL                                 
         CLI   1(RE),3                                                          
         BNE   *+8                                                              
         STC   RF,MIDLEV           MIDLINE LEVEL                                
         LA    R1,1(R1)                                                         
         LA    RF,1(RF)                                                         
*                                                                               
INIT14   LA    RE,2(RE)                                                         
         B     INIT12                                                           
*                                                                               
INITX    B     XIT                                                              
         SPACE 2                                                                
RPTLEVS  DC    AL1(QCLT,0)         HEADLINES                                    
         DC    AL1(QPRD,0)                                                      
         DC    AL1(QEST,0)                                                      
         DC    AL1(QMKTGR1,0)                                                   
         DC    AL1(QMKTGR2,0)                                                   
         DC    AL1(QMKTGR3,0)                                                   
         DC    AL1(QMKT,2)                                                      
         DC    AL1(QTEXT,1)                                                     
         DC    AL1(QSTA,3)         MIDLINE                                      
         DC    AL1(QROT,0)         ROWS                                         
         DC    AL1(QTIMES,0)                                                    
         DC    AL1(QPROG,0)                                                     
         DC    AL1(QLEN,0)                                                      
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* ERRORS                                                              *         
***********************************************************************         
         SPACE 1                                                                
EMGRP    XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(22),=C'MARKET GROUPS REQUIRED'                           
         B     MYCURSER                                                         
*                                                                               
EINV     MVI   ERROR,INVALID                                                    
         B     CURSOR                                                           
*                                                                               
MYCURSER MVI   ERROR,X'FE'         OWN ERROR MESSAGE                            
*                                                                               
CURSOR   DS    0H                                                               
         GOTO1 CURSERR                                                          
         EJECT                                                                  
***********************************************************************         
* SPOTIO INPUT HOOK                                                   *         
***********************************************************************         
         SPACE 1                                                                
INPUT    L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         CLI   SBMODE,SBPROCCL     CLIENT FIRST                                 
         BE    CLIENT                                                           
         CLI   SBMODE,SBPROCMK     MARKET FIRST                                 
         BE    MARKET                                                           
         CLI   SBMODE,SBPROCSP     PROCBUY                                      
         BE    BUY                                                              
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* CLIENT FIRST                                                        *         
***********************************************************************         
         SPACE 1                                                                
CLIENT   XC    WORK,WORK           GET DC PROFILE                               
         MVC   WORK(4),=C'S0DC'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),SBQMED                                                 
         MVC   WORK+7(3),SBCLT                                                  
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SBCOFF                                                
         GOTO1 GETPROF,DMCB,WORK,DCPROF,DATAMGR                                 
         CLI   CHANOPT,0           TEST CHANNEL OPTION SET                      
         BNE   *+10                                                             
         MVC   CHANOPT,DCCHAN      NO-SET FROM PROFILE                          
         NI    DATAIND3,255-DICHAN                                              
         CLI   CHANOPT,C'Y'                                                     
         BNE   *+8                                                              
         OI    DATAIND3,DICHAN                                                  
*                                                                               
         MVC   WORK+2(2),=C'B9'    B9 PROFILE                                   
         GOTO1 (RF),(R1),WORK,B9PROF,DATAMGR                                    
         MVC   SVB9PROF,B9PROF                                                  
*                                                                               
         MVC   WORK+2(2),=C'F0'    F0 PROFILE                                   
         GOTO1 (RF),(R1),WORK,F0PROF,DATAMGR                                    
*                                                                               
         MVC   WORK(4),=C'SB9A'                                                 
         NI    WORK,X'BF'                                                       
         GOTO1 (RF),(R1),WORK,B9APROF                                           
*                                                                               
         MVI   MILLER,C'N'                                                      
         CLC   =C'TH',AGENCY                                                    
         BNE   CLT10                                                            
*                                                                               
* THIS ISN'T REALLY NECESSARY                                                   
**NOP**  CLI   B9PROF,0                                                         
**NOP**  BE    CLT10                                                            
         CLC   =C'ML',SBCLTIFC                                                  
         BNE   CLT10                                                            
         CLI   SBCLTIFC+2,C' '                                                  
         BH    CLT10                                                            
         BAS   RE,BLDPRD           GO BUILD TABLE OF MOLSON PRD'S               
         MVI   MILLER,C'Y'                                                      
*                                                                               
CLT10    DS    0H                                                               
         MVI   MULTIDSW,C'N'       INIT MULTI DISTRIBUTOR SWITCH                
         CLI   SBQMGRD,0           ONLY WITH MARKET GROUPS                      
         BE    CLTX                                                             
         CLI   DISTOPT,C'Y'        DISTRIBUTORS OPTION                          
         BNE   CLTX                                                             
         CLI   DCCOMP,0            TEST DC PROFILE PRESENT                      
         BE    *+14                                                             
         MVC   B9COMP(6),DCCOMP    YES-SET B9 ACC OPTIONS FROM DC               
         MVI   B9MEDCAL,C'Y'           FORCE MEDIA CALENDAR                     
         CLI   B9COMP,0            TEST B9 PRESENT                              
         BE    CLTX                                                             
         CLI   B9MEDCAL,C'Y'       AND MEDIA CALENDAR WANTED                    
         BNE   CLTX                                                             
         MVI   MULTIDSW,C'Y'       YES-SET MULTI DISTRIBUTOR SWITCH ON          
         CLC   SBQEST,SBQESTND     TEST EST=NO                                  
         BE    *+12                                                             
         CLI   SBQSEPES,C'Y'                                                    
         BNE   CLT99               YES-ERROR                                    
*                                                                               
***         XC    GDSECT(GDSECTL),GDSECT  INITIALIZE FINDOUT DSECT              
         LA    R0,GDSECT                                                        
         LHI   R1,GDSECTL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   GDCOMP(3),B9COMP    COMPANY/UNIT/LEDGER                          
         MVC   GDMGRLEN,B9MGRLEN   MARKET GROUP LENGTH                          
         CLI   B9MKTS,C'Y'         BY MARKET?                                   
         BNE   *+8                                                              
         MVI   GDMKTLEN,4          YES-MARKET LENGTH = 4                        
         MVC   GDOUTLEN,B9OUTLET   OUTLET CODE LENGTH                           
***         MVC   GDDMGR,DATAMGR                                                
         MVC   GDACOMS,SBCOMFAC                                                 
         MVC   GDATABS,AFOTABS                                                  
         L     RF,TWAMASTC                                                      
         MVC   GDAUTL,MCUTL-MASTD(RF) A(UTL)                                    
         MVC   GDACCSYS,MCS2SENO-MASTD(RF) ACCOUNTIMG SYSTEM SE NUM             
         CLI   B9APROF+8,0                                                      
         BE    *+10                                                             
         MVC   GDACCSYS,B9APROF+8                                               
         B     CLTX                                                             
*                                                                               
CLT99    MVC   MYPRINT,BLANKS                                                   
         MVI   CANCEL,C'E'                                                      
         MVI   SBMODE,SBSTOP       STOP PROCESSING THIS REQUEST                 
         MVI   RPMODE,RPSKIOHK                                                  
*                                                                               
CLTX     B     XIT                                                              
         SPACE 2                                                                
* ROUTINE TO BUILD A TABLE OF MOLSON PRODUCTS FOR THIS CLIENT                   
* TABLE IS USED TO DISTINGUISH MILLER FROM MOLSON PRODUCTS                      
* WHICH ARE ALL MUSHED UNDER MILLER CLIENT CODES                                
*                                                                               
BLDPRD   NTR1                                                                   
         L     R3,SBACLTRC                                                      
         LA    R3,CLIST-CLTHDRD(R3)       R3=A(CLIST)                           
         XC    PRDTAB,PRDTAB                                                    
*                                                                               
BP10     CLI   0(R3),C' '                                                       
         BNH   XIT                                                              
         XC    KEY,KEY             READ PRODUCT RECORD                          
         LA    R2,KEY                                                           
         USING PRDHDRD,R2                                                       
         MVC   PKEYAM,SBBAGYMD                                                  
         MVC   PKEYCLT,SBBCLT                                                   
         MVC   PKEYPRD,0(R3)                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(7),KEY                                                       
         BE    *+6                 PRODUCT RECORD FOUND?                        
         DC    H'0'                                                             
         L     R2,SBAIO2                                                        
         ST    R2,AIO                                                           
         GOTO1 GETREC                                                           
         CLC   PGRP1,=X'E50200'    V02                                          
         BE    BP30                                                             
         CLC   PGRP2,=X'E50200'                                                 
         BE    BP30                                                             
         CLC   PGRP3,=X'E50200'                                                 
         BE    BP30                                                             
         CLC   PGRP4,=X'E50200'                                                 
         BE    BP30                                                             
         CLC   PGRP5,=X'E50200'                                                 
         BE    BP30                                                             
*                                                                               
BP20     LA    R3,4(R3)                                                         
         B     BP10                                                             
*                                                                               
* THIS IS A MOLSON PRODUCT - SET FLAG IN TABLE                                  
BP30     DS    0H                                                               
         ZIC   R5,3(R3)            PRD #                                        
         LA    R5,PRDTAB(R5)                                                    
         OI    0(R5),PRDMOLS                                                    
         B     BP20                                                             
         EJECT                                                                  
***********************************************************************         
* MARKET FIRST                                                        *         
***********************************************************************         
         SPACE 1                                                                
MARKET   CLI   GDMGRLEN,0          NEED TO SET MARKET GROUP ?                   
         BE    MKT2                                                             
         MVC   GDMGR,BLANKS        YES-                                         
         MVC   MGRPNAME,BLANKS                                                  
         CLC   SBBMGR,=X'9999'     UNKNOWN MARKET GROUP                         
         BE    MKT2                                                             
         UNPK  DUB(5),SBBMGR(3)                                                 
         ZIC   RF,GDMGRLEN         N'REQUIRED MKT GRP DIGITS                    
         ZIC   RE,SBMGR3LN                                                      
         SR    RE,RF                                                            
         BNM   *+6                                                              
         SR    RE,RE                                                            
         LA    RE,DUB(RE)          USE LAST N POSITIONS                         
         BCTR  RF,0                                                             
         EX    RF,*+4              MOVE MARKET GROUP TO FINDOUT DSECT           
         MVC   GDMGR(0),0(RE)                                                   
         GOTO1 GETMGRNM,MGRPNAME   GET MARKET GROUP NAME                        
         OC    SVMGRBK,SVMGRBK     TEST MARKET GROUP BREAK NAME SET             
         BNZ   MKT2                                                             
         LA    R1,SBMGR3BK         NO-SET IT NOW                                
         CLC   SBMGR3LN,SBMGR2LN                                                
         BNE   MKT1                                                             
         LA    R1,SBMGR2BK                                                      
         CLC   SBMGR2LN,SBMGR1LN                                                
         BNE   MKT1                                                             
         LA    R1,SBMGR1BK                                                      
*                                                                               
MKT1     MVC   SVMGRBK,0(R1)                                                    
*                                                                               
MKT2     MVI   MKTSW,C'Y'          PRINT THIS MARKET                            
         CLI   MULTIDSW,C'Y'       ALWAYS PRINT FOR NO DISTRIBUTORS             
         BNE   MKTX                                                             
         MVC   GDSCHM,BLANKS       CLEAR SCHEME CODE                            
         CLI   GDMKTLEN,0          MIGHT NEED MARKET                            
         BE    *+10                                                             
         MVC   GDMKT,SBMKT                                                      
         MVI   GDOUTNO,0           FIRST TIME FOR FINDOUT                       
         GOTO1 =V(FINDOUT),DMCB,GDSECT                                          
         OC    GDNOUTS,GDNOUTS     TEST ANY DISTRIBUTORS                        
         BNZ   MKTX                                                             
         MVI   MKTSW,C'N'          NO-SUPPRESS THIS MARKET                      
*                                                                               
MKTX     B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* BUY HOOK                                                            *         
***********************************************************************         
         SPACE 1                                                                
BUY      CLI   MKTSW,C'Y'          MARKET MIGHT BE REJECTED                     
         BNE   BUYX                                                             
         OC    WKDSPLS,WKDSPLS     TEST WEEK TABLES SET YET                     
         BNZ   BY1                                                              
         BRAS  RE,SETWKS           NO-DO THEM NOW                               
         MVC   GLOPTS+5(1),NUMWKS  PASS N'WEEKS TO DRIVER                       
         CLI   NUMQTRS,4           TEST MORE THAN 4 QUARTERS                    
         BNH   BY1                                                              
         MVI   SBMODE,SBSTOP       YES-CANCEL NOW                               
         MVI   CANCEL,C'Q'                                                      
         B     BUYX                                                             
*                                                                               
BY1      L     R3,SBAIO1           A(BUY RECORD)                                
         USING BUYREC,R3                                                        
         MVI   MKTIND,X'FF'                                                     
         MVC   SBEMKT,SBBMKT       EXTRACT MARKET IN DIRECTORY POINTER          
         CLI   DCSPILL,C'Y'        TEST SPILL REQUESTED                         
         BE    *+14                                                             
         CLC   SBBMKT,BUYMSTA      NO - REJECT SPILL MARKET                     
         BNE   BY30                                                             
         MVC   SBBMKT,BUYMSTA      SET MARKET FOR SPOTBUY                       
*                                                                               
         CLC   SBQPGRF,BLANKS      TEST PRDGRP FILTERING                        
         BNH   *+10                                                             
         MVC   SBEPRD,SBBPRD       YES-ONLY EXTRACT KEYED PRODUCT               
*                                                                               
         MVI   SBBPRD,0            INITIALIZE PRODUCTS                          
         MVI   SBBPRD2,0                                                        
         XC    SBPRD,SBPRD                                                      
         XC    SBPRD2,SBPRD2                                                    
         MVI   SVPRD,0             INIT SAVED PRODUCT                           
         MVI   SVEST,0             INIT SAVED ESTIMATE                          
*                                                                               
         LA    RE,SBLOCK           TELL SPOTBUY TO SPLIT BUYS                   
         AHI   RE,SBEFLAG2-SBLOCK                                               
         OI    0(RE),SBESPLBY                                                   
*                                                                               
BY1A     GOTO1 SPOTBUY,PARAS,SBLOCK   ** CALL SPOTBUY **                        
*                                                                               
         L     R5,SBACHUNK                                                      
         USING SCHUNKD,R5                                                       
         MVI   SPILL,C'N'                                                       
         CLC   SBEMKT,SBBMKT       TEST SPILL MARKET                            
         BE    BY2                                                              
         MVI   SPILL,C'Y'          YES-                                         
         MVC   SBBMKT,SBEMKT       RESTORE MARKET FOR DRIVER                    
*                                                                               
BY2      OC    SCNEXT,SCNEXT       TEST END OF CHUNKS                           
         BZ    BY30                                                             
         OC    SCSPOTS,SCSPOTS     TEST ANY SPOTS                               
         BZ    BY20                NO-SKIP TO NEXT CHUNK                        
*                                                                               
         CLI   SBQBPRD,0           TEST PRD=ALL                                 
         BNE   *+16                                                             
         CLI   SCPRD1,X'FF'        YES-REJECT PRD=POL                           
         BE    BY20                                                             
         B     *+14                                                             
         CLC   SCPRD1,SBQBPRD      SINGLE PRODUCT                               
         BNE   BY20                                                             
         CLC   SBBPRD,SCPRD1       TEST CHANGE OF PRODUCT                       
         BE    BY4                                                              
         MVC   SBBPRD,SCPRD1       YES - SET PRODUCT CODE FOR DRIVER            
         BAS   RE,EXTPRD           EXTRACT PRODUCT DETAILS                      
         BNE   BY20                                                             
         CLI   SBQMGR,0            TEST MARKET GROUPS                           
         BE    BY4                                                              
         CLC   SBQPGRF,BLANKS      AND NOT MARKET GROUP FILTERING               
         BH    BY4                                                              
         CLI   BUYKPRD,X'FF'       AND PRD=POL                                  
         BNE   BY4                                                              
         OC    SBPGRPEX(2),SBPGRPEX  AND THERE ARE PRDGRP EXCEPTIONS            
         BZ    BY4                                                              
         BAS   RE,GETMGR           YES-GET THE MARKET GROUP                     
*                                                                               
BY4      ZIC   RE,SCSLN1           SPOT LENGTH                                  
         ZIC   RF,SCSLN2                                                        
         AR    RE,RF                                                            
         STC   RE,SBLEN                                                         
*                                                                               
         CLC   SBBPRD,SVPRD        TEST CHANGE OF PRODUCT/ESTIMATE              
         BNE   *+14                                                             
         CLC   SBBEST,SVEST                                                     
         BE    BY10                                                             
         MVC   SVPRD,SBBPRD        YES                                          
         MVC   SVEST,SBBEST                                                     
         ZIC   RE,SBBPRD           FIND ENTRY IN PRD/EST TABLE                  
         BCTR  RE,0                                                             
         SLL   RE,8                                                             
         ZIC   RF,SBBEST                                                        
         BCTR  RF,0                                                             
         AR    RE,RF                                                            
         A     RE,SBAESTTB                                                      
         SR    R1,R1                                                            
         ICM   R1,1,0(RE)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  R1,0                                                             
         MH    R1,=Y(ESTBUFFL)                                                  
         A     R1,SBAESTBF                                                      
         USING ESTBUFFD,R1                                                      
         MVC   SBESTFLT,EBFILT     EXTRACT ESTIMATE FILTERS                     
         MVC   SBRTLSCH,EBRTLSCH   EXTRACT RETAIL SCHEME CODE                   
*                                                                               
         CLI   MULTIDSW,C'X'       TEST MULTI-DISTRIB OFF FOR LAST EST          
         BNE   *+8                                                              
         MVI   MULTIDSW,C'Y'       YES-TURN BACK ON                             
         CLI   MULTIDSW,C'Y'       FOR MULTI DISTRIBUTOR REPORT,                
         BNE   BY14                                                             
         CLI   DCCOMP,0            TEST DC PROFILE ACC COMPANY                  
         BNE   BY6                                                              
         CLI   SBRTLSCH,C'*'       NO-TEST WHETHER ESTIMATE QUALIFIES           
         BH    BY6                 HAS RETAIL CODE                              
         BE    *+14                NO SCHEME                                    
         CLC   SBESTFLT+1(2),BLANKS   OLD PLACE FOR SCHEME                      
         BNE   BY6                                                              
         MVI   MULTIDSW,C'X'       SUPPRESS FOR THIS ESTIMATE                   
*                                                                               
BY6      LA    R0,SVGDMAX                                                       
         LA    R2,SVGDTAB          BIULD LIST OF DISTRIBUTOR ADDRESSES          
         XC    0(4,R2),0(R2)                                                    
         LA    R2,4(R2)                                                         
         BCT   R0,*-10                                                          
         CLI   MULTIDSW,C'X'       EXCEPT FOR NON-RETAIL ESTIMATE               
         BE    BY10                                                             
         MVI   GDOUTNO,1           GET 1ST DISTRIBUTOR OUTLET                   
         CLC   GDNSCHMS,=F'2'      TEST CORPORATE ONLY                          
         BL    *+16                YES-NEVER SUPPRESS IT                        
         CLI   DCCORP,C'N'         TEST TO SUPPRESS CORPORATE                   
         BNE   *+8                                                              
         MVI   GDOUTNO,2           YES-START AT 2ND OUTLET                      
         CLI   SVB9PROF,0          TEST RETAIL CLIENT                           
         BE    BY7                                                              
         MVC   GDSCHM,SBRTLSCH     YES-SET THE SCHEME                           
         CLI   SBRTLSCH,C'*'                                                    
         BH    BY7                                                              
         MVC   GDSCHM,SBESTFLT+1   OLD PLACE FOR SCHEME                         
*                                                                               
BY7      LA    R0,SVGDMAX                                                       
         LA    R2,SVGDTAB                                                       
*                                                                               
BY8      DS    0H                                                               
         GOTO1 =V(FINDOUT),DMCB,GDSECT                                          
         OC    GDOUTAD,GDOUTAD     ARE WE DONE?                                 
         BZ    BY10                                                             
*                                                                               
         CLI   MILLER,C'Y'                                                      
         BNE   BY9                                                              
         L     RF,GDOUTAD                                                       
         ZIC   R1,SBBPRD                                                        
         LA    R1,PRDTAB(R1)                                                    
         TM    0(R1),PRDMOLS       MOLSON PRODUCT?                              
         BZ    *+16                 NO                                          
         CLI   GDOCODE-GDOUTD(RF),C'5'                                          
         BNE   BY9A                                                             
         B     *+12                                                             
         CLI   GDOCODE-GDOUTD(RF),C'5'                                          
         BE    BY9A                                                             
*                                                                               
BY8A     CLC   GDNSCHMS,=F'2'      TEST CORPORATE ONLY                          
         BL    BY9                 YES-NEVER SUPPRESS IT                        
         CLI   DCCORP,C'N'         TEST TO SUPPRESS CORPORATE                   
         BNE   BY9                                                              
         CLC   =C'000005',GDOCODE-GDOUTD(RF)  MILLER HAS 2 CORP OUTLETS         
         BE    BY9A                                                             
*                                                                               
BY9      CLC   GDSCHM,BLANKS       IF THERE'S A SCHEME,                         
         BE    *+14                                                             
         OC    GDSHRP,GDSHRP       TEST THIS DISTRIBUTOR HAS A SHARE            
         BZ    *+14                                                             
         MVC   0(4,R2),GDOUTAD     SET A(THIS DISTRIBUTOR INFO)                 
         LA    R2,4(R2)                                                         
BY9A     ZIC   R1,GDOUTNO          ADVANCE TO NEXT DISTRIBUTOR                  
         LA    R1,1(R1)                                                         
         STC   R1,GDOUTNO                                                       
         BCT   R0,BY8                                                           
         DC    H'0'                                                             
*                                                                               
BY10     CLI   MULTIDSW,C'N'       TEST MULTI DISTRIBUTORS                      
         BE    BY14                                                             
         LA    R2,SVGDTAB          YES-LOOP THROUGH DISTRIBUTORS                
         LA    R0,SVGDMAX                                                       
         L     R3,0(R2)                                                         
*                                                                               
         USING GDOUTD,R3                                                        
BY12     LA    R7,DISTREC          ADD TO DISTRIBUTOR TABLE                     
         XC    DISTREC,DISTREC                                                  
         USING DTABLE,R7                                                        
         MVC   DTMGR,GDMGR                                                      
         MVC   DTMGRNM,MGRPNAME                                                 
         LTR   R3,R3                                                            
         BNZ   *+20                                                             
         MVC   DTDISTCD,=C'9999999'                                             
         MVC   DTDISTNM(3),=C'MBC'                                              
         B     BY13                                                             
         MVC   DTDISTCD,GDOCODE                                                 
         MVC   DTDISTNM,GDONAME                                                 
         MVC   DTADDR1,GDOAL1                                                   
         MVC   DTADDR2,GDOAL2                                                   
         MVC   DTADDR3,GDOAL3                                                   
         MVC   DTADDR4,GDOAL4                                                   
*                                                                               
BY13     DS    0H                                                               
         GOTO1 BINADD,DMCB,DISTREC,ADISTAB                                      
*                                                                               
         CLI   DCRECAP,C'Y'        RETAIL RECAP?                                
         BNE   BY14                                                             
         LA    R7,RECAPREC         YES-ADD TO RECAP TABLE                       
         USING RTABLE,R7                                                        
         XC    RECAPREC,RECAPREC                                                
         MVC   RTPRD,SBPRD                                                      
         MVC   RTMED,SBQMED                                                     
         MVC   RTMGR,GDMGR                                                      
         LTR   R3,R3                                                            
         BNZ   *+14                                                             
         MVC   RTDISTCD,=C'9999999'                                             
         B     *+10                                                             
         MVC   RTDISTCD,GDOCODE                                                 
         MVC   RTEST,SBBEST                                                     
         GOTO1 (RF),(R1),RECAPREC,ARCPTAB                                       
         L     R7,ABINREC          A(RECORD IN TABLE)                           
         ICM   R1,15,RTSPOTS       ADD IN SPOTS                                 
         A     R1,SCSPOTS                                                       
         ST    R1,RTSPOTS                                                       
         OC    RTSTART,RTSTART     TEST START SET YET                           
         BZ    *+14                                                             
         CLC   RTSTART,SCDATE      OR NEW START                                 
         BNH   *+10                                                             
         MVC   RTSTART,SCDATE      YES-SET START DATE                           
         OC    RTEND,RTEND         TEST END SET YET                             
         BZ    *+14                                                             
         CLC   RTEND,SCDATE        OR NEW END                                   
         BNL   BY14                                                             
         MVC   RTEND,SCDATE        YES-SET END DATE                             
*                                                                               
BY14     CLI   MULTIDSW,C'X'       PRINT MEDIA CALENDAR FOR NON-RETAIL          
         BE    *+10                ESTIMATES                                    
         LTR   R3,R3                                                            
         BZ    BY20                                                             
         L     R1,SBADATE          FIND WHICH QTR THIS WEEK'S IN                
         LA    RE,WKDSPLS                                                       
         L     RF,SBNDATES                                                      
*                                                                               
BY16     CLC   SCDATE,0(R1)                                                     
         BE    *+18                                                             
         LA    R1,4(R1)                                                         
         LA    RE,2(RE)                                                         
         BCT   RF,BY16                                                          
         DC    H'0'                                                             
         MVC   GLOPTS+3(1),0(RE)   QUARTER NUMBER                               
         MVC   WEEKNUM,1(RE)       WEEK NUMBER WITHIN QTR                       
*                                                                               
         ST    R5,SBACURCH         A(CURRENT EXTRACT CHUNK)                     
         MVI   GLMODE,GLINPUT      CALL DRIVER FOR INPUT                        
         BAS   RE,GODRIVER                                                      
*                                                                               
         CLI   MULTIDSW,C'Y'       TEST MULTI DISTRIBUTORS                      
         BNE   BY20                                                             
         LA    R2,4(R2)            YES-ADVANCE TO NEXT DISTRIBUTOR              
         ICM   R3,15,0(R2)                                                      
         BZ    BY20                                                             
         BCT   R0,BY12                                                          
*                                                                               
BY20     L     R5,SCNEXT           NEXT CHUNK                                   
         B     BY2                                                              
*                                                                               
BY30     LA    RE,SBLOCK                                                        
         AHI   RE,SBACONT-SBLOCK                                                
         OC    0(4,RE),0(RE)                                                    
         BNZ   BY1A                                                             
*                                                                               
         XC    SBEMKT,SBEMKT                                                    
*                                                                               
BUYX     MVI   RPMODE,RPSKIOHK     SKIP SPWRI01'S PROCESSING                    
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO EXTRACT PRODUCT DETAILS FROM PRODUCT BUFFER              *         
* INPUT  : SBBPRD = PRODUCT CODE                                      *         
* OUTPUT : CC EQ - OK                                                 *         
*          CC NE - THE PRODUCT GROUP IS UNKNOWN AND THERE IS PRODUCT  *         
*                  GROUP FILTERING                                    *         
***********************************************************************         
         SPACE 1                                                                
EXTPRD   LR    R0,RE                                                            
         ZIC   RE,SBBPRD                                                        
         BCTR  RE,0                                                             
         MH    RE,=Y(PRDBUFFL)                                                  
         L     R1,SBAPRDBF                                                      
         AR    R1,RE                                                            
         USING PRDBUFFD,R1                                                      
         MVC   SBPRD,PBALPH        ALPHA PRODUCT                                
         MVC   SBPRDNM,PBNAME      PRODUCT NAME                                 
         MVC   SBPRDINT,PBINT      PRODUCT INTERFACE CODE                       
         LA    RE,SBLOCK                                                        
         USING SBLOCK,RE                                                        
         MVC   SBUP1FLD,PBUFLD1                                                 
         MVC   SBUP2FLD,PBUFLD2                                                 
         DROP  RE                                                               
         MVC   SBBPGR,PBGROUP      SET PRODUCT GROUP                            
         OC    SBBPGR,SBBPGR                                                    
         BNZ   EXTPRDY                                                          
         CLC   SBQPGRF,BLANKS      PRODUCT GROUP UNKNOWN                        
         BH    EXTPRDN             IF PRDGRP FILTER, THEN RETURN ERROR          
         MVC   SBBPGR,=X'9999'                                                  
*                                                                               
EXTPRDY  LR    RE,R0                                                            
         CR    RE,RE                                                            
         BR    RE                                                               
*                                                                               
EXTPRDN  LTR   RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* SUB-ROUTINE TO GET MARKET GROUP FOR PRODUCT GROUP EXCEPTIONS        *         
* INPUT  : SBBMKT=MARKET CODE                                         *         
*          SBBPGR=PRODUCT GROUP                                       *         
* OUTPUT : SBBMGR=MARKET GROUP                                        *         
***********************************************************************         
         SPACE 1                                                                
GETMGR   LR    R0,RE                                                            
         SR    RE,RE                                                            
         ICM   RE,3,SBBMKT         INDEX INTO MARKET GRP ASSIGN TABLE           
         AR    RE,RE                                                            
         MVC   HALF,SBBPGR                                                      
         OC    HALF,SBPG1MSK                                                    
         LA    R0,SBEXMAX                                                       
         LA    R1,SBPGRPEX                                                      
         LA    RF,SBAMGTB2                                                      
*                                                                               
GM2      OC    0(2,R1),0(R1)                                                    
         BZ    GM4                                                              
         MVC   FULL(2),0(R1)                                                    
         OC    FULL(2),SBPG1MSK                                                 
         CLC   HALF,FULL           TEST IT'S THE EXCEPTION GROUP                
         BNE   *+12                                                             
         A     RE,0(RF)            YES-THEN USE APPROPRIATE MGRP TABLE          
         B     GM6                                                              
         LA    R1,2(R1)                                                         
         LA    RF,4(RF)                                                         
         BCT   R0,GM2                                                           
*                                                                               
GM4      A     RE,SBAMGTAB                                                      
*                                                                               
GM6      MVC   SBBMGR,=X'9999'                                                  
         OC    0(2,RE),0(RE)                                                    
         BZ    GMX                                                              
         MVC   SBBMGR,0(RE)                                                     
*                                                                               
GMX      LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO HOOK TO CALLING PROGRAM TO CALL DRIVER                   *         
***********************************************************************         
         SPACE 1                                                                
GODRIVER NTR1                                                                   
         L     RF,ADRIVER                                                       
         L     RE,SAVERD                                                        
         LM    R0,RC,20(RE)                                                     
         BASR  RE,RF                                                            
         XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* REQUEST LAST                                                        *         
***********************************************************************         
         SPACE 1                                                                
FIN      L     R1,ASPOOLD          SAVE THE SPOOL ID                            
         USING SPOOLD,R1                                                        
         MVC   SPLID,SPOOLID                                                    
         DROP  R1                                                               
         L     R0,ASAVE            SAVE INTER-REQUEST VALUES                    
         LA    R1,SAVVALSL                                                      
         LA    RE,SAVVALS                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* RUNLAST - PRINT DISTRIBUTOR RECAP                                   *         
***********************************************************************         
         SPACE 1                                                                
LAST     L     RE,ASAVE            RESTORE SAVED VALUES                         
         LA    RF,SAVVALSL                                                      
         LA    R0,SAVVALS                                                       
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVI   RCPSW,C'Y'          PRINT DISTRIBUTOR RECAP                      
         L     R2,ARCPTAB                                                       
         ICM   R3,15,4(R2)         R3=N'RECORDS                                 
         BZ    XIT                                                              
         L     R7,0(R2)            R7=A(FIRST TABLE ENTRY)                      
         XC    SVMGR,SVMGR                                                      
         XC    SVDISTCD,SVDISTCD                                                
         BAS   RE,DRIVINIT         INITIALIZE DRIVER                            
         L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         MVI   GLMODE,GLINIT                                                    
         GOTO1 DRIVER,DMCB,(R4)                                                 
*                                                                               
LAST2    CLC   0(L'RTKEY,R7),BLANKS                                             
         BNH   LAST6                                                            
         MVC   RECAPREC,0(R7)                                                   
         USING RTABLE,R7                                                        
         OC    RTSPOTS,RTSPOTS     TEST ANY SPOTS                               
         BZ    LAST6                                                            
         MVC   SBQMED,RTMED        SET THE MEDIA                                
         CLC   RTMGR,SVMGR         TEST MGR OR DISTRIBUTOR CHANGED              
         BNE   *+14                                                             
         CLC   RTDISTCD,SVDISTCD                                                
         BE    LAST4                                                            
         MVC   SVMGR,RTMGR         YES-GET DISTRIBUTOR NAME FROM                
         MVC   SVDISTCD,RTDISTCD       DISTRIBUTOR TABLE                        
         XC    DISTREC,DISTREC                                                  
         LA    R5,DISTREC                                                       
         USING DTABLE,R5                                                        
         MVC   DTMGR,SVMGR                                                      
         MVC   DTDISTCD,SVDISTCD                                                
         GOTO1 BINFIND,DMCB,DISTREC,ADISTAB                                     
         ICM   R1,15,ABINREC                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   DISTREC,0(R1)       SAVE DISTRIBUTOR TABLE ENTRY                 
*                                                                               
LAST4    MVI   GLMODE,GLINPUT      CALL DRIVER FOR INPUT                        
         GOTO1 DRIVER,DMCB,(R4)                                                 
*                                                                               
LAST6    LA    R7,RTLEN(R7)        NEXT RECAP RECORD                            
         BCT   R3,LAST2                                                         
*                                                                               
         L     R1,ASPOOLD                                                       
         USING SPOOLD,R1                                                        
         MVC   SPOOLID,SPLID                                                    
         DROP  R1                                                               
         GOTO1 OPENPQ              INITIALIZE SPOOL                             
         MVI   GLMODE,GLOUTPUT     CALL DRIVER FOR OUTPUT                       
         GOTO1 DRIVER,DMCB,(R4)                                                 
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALIZATION FOR DRIVER FOR RECAP REPORT                          *         
***********************************************************************         
         SPACE 1                                                                
DRIVINIT NTR1  ,                                                                
         GOTO1 CALLOV,DMCB,(X'76',0),0    LOAD DPG PHASE                        
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   ADPGPROG,0(R1)                                                   
         MVI   MYFIRSTH,10         FIRST HEADING ON LINE 10                     
*        MVC   LNAMPOOL,=F'1000000'  1 MEGABYTE NAME POOL                       
         GOTO1 INITDRIV            INITIALIZE DRIVER                            
         L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         LA    R1,DRHOOK           DRIVER'S APPLICATION HOOK                    
         ST    R1,GLAHOOK                                                       
         MVC   GLOPTS+2(1),RCPSW   RECAP SWITCH                                 
         MVC   TITLE,BLANKS                                                     
         MVC   TITLE(20),=C'MEDIA CALENDAR RECAP'                               
         GOTO1 CENTER,DMCB,TITLE,63                                             
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DRIVER HOOK                                                         *         
***********************************************************************         
         SPACE 1                                                                
DRHOOK   L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         CLI   GLHOOK,GLINIT       INITIALIZATION                               
         BE    DRVINIT                                                          
         CLI   GLHOOK,GLRESOLV     RESOLVE ADDRESSES                            
         BE    RESOLVE                                                          
         CLI   GLHOOK,GLROUT       EXECUTE ROUTINES                             
         BE    EXEC                                                             
         CLI   GLHOOK,GLOUTPUT     ABOUT TO EXECUTE OUTPUT STAGE                
         BE    OUT                                                              
         CLI   GLHOOK,GLHEAD       HEADHOOK                                     
         BE    HEDHK                                                            
*                                                                               
DRHOOKX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DRIVER INITIALIZATION                                               *         
***********************************************************************         
         SPACE 1                                                                
DRVINIT  MVC   GLOPTS+2(1),RCPSW   RECAP SWITCH                                 
         MVC   GLOPTS+4(1),SUMOPT  MARKET SUMMARY OPTION                        
         B     DRHOOKX                                                          
         EJECT                                                                  
***********************************************************************         
* DRIVER HOOK ROUTINE TO RESOLVE ADDRESSES                            *         
***********************************************************************         
         SPACE 1                                                                
RESOLVE  LA    R1,RTNLIST          SEARCH LIST FOR ROUTINE NAME                 
*                                                                               
RESOLVE2 CLI   0(R1),X'FF'                                                      
         BE    XIT                                                              
         CLC   0(8,R1),GLLABEL                                                  
         BE    *+12                                                             
         LA    R1,12(R1)                                                        
         B     RESOLVE2                                                         
         MVC   GLAROUT,8(R1)       YES-RETURN ADDRESS                           
         B     XIT                                                              
         SPACE 1                                                                
RTNLIST  DS    0F                                                               
         DC    CL8'IDIST   ',A(IDIST)                                           
         DC    CL8'ODIST   ',A(ODIST)                                           
         DC    CL8'FDIST   ',A(FDIST)                                           
         DC    CL8'OSTAT   ',A(OSTAT)                                           
         DC    CL8'FSTA    ',A(FSTA)                                            
         DC    CL8'TSTA    ',A(TSTA)                                            
         DC    CL8'ISPW    ',A(ISPW)                                            
         DC    CL8'OSPW    ',A(OSPW)                                            
         DC    CL8'HSPW    ',A(HSPW)                                            
         DC    CL8'OSUM    ',A(OSUM)                                            
         DC    CL8'IPROD   ',A(IPROD)                                           
         DC    CL8'ITYPE   ',A(ITYPE)                                           
         DC    CL8'IMGRP   ',A(IMGRP)                                           
         DC    CL8'HMGRP   ',A(HMGRP)                                           
         DC    CL8'IDISTNM ',A(IDISTNM)                                         
         DC    CL8'IESTIM  ',A(IESTIM)                                          
         DC    CL8'IDATES  ',A(IDATES)                                          
         DC    CL8'ODATES  ',A(ODATES)                                          
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* DRIVER HOOK TO EXECUTE ROUTINES                                     *         
***********************************************************************         
         SPACE 1                                                                
EXEC     L     R2,GLAIFLD          R2=A(INPUT)                                  
         L     R3,GLAOFLD          R3=A(OUTPUT)                                 
         CLI   GLMODE,GLINPUT      TEST INPUT PHASE                             
         BNE   *+8                                                              
         MVI   INDATA,1            YES-ALL DATA IS SIGNIFICANT                  
         L     RF,GLAROUT          BRANCH TO ROUTINE                            
         BR    RF                                                               
         EJECT                                                                  
***********************************************************************         
* I/O ROUTINES                                                        *         
***********************************************************************         
         SPACE 1                                                                
IDIST    MVC   0(11,R2),XFF                                                     
         CLI   MULTIDSW,C'Y'       TEST MULTI DISTRIBUTORS                      
         BNE   XIT                                                              
         LA    R7,DISTREC                                                       
         USING DTABLE,R7                                                        
         MVC   0(4,R2),DTMGR       YES-MARKET GROUP CODE                        
         MVC   4(7,R2),DTDISTCD        AND DISTRIBUTOR CODE                     
         B     XIT                                                              
         SPACE 2                                                                
ODIST    XC    DISTREC,DISTREC     DISTRIBUTOR OUTPUT                           
         CLC   0(11,R2),XFF        TEST ANY DISTRIBUTOR                         
         BE    XIT                 NO                                           
         LA    R7,DISTREC                                                       
         MVC   DTMGR,0(R2)         EXTRACT MARKET GROUP                         
         MVC   DTDISTCD,4(R2)      AND DISTRIBUTOR CODE                         
         GOTO1 BINFIND,DMCB,DISTREC,ADISTAB                                     
         ICM   R1,15,ABINREC                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   DISTREC,0(R1)       SAVE DIST INFO FOR HEADHOOK                  
         B     XIT                                                              
         DROP  R7                                                               
         SPACE 2                                                                
FDIST    XC    MKTTOTS,MKTTOTS     DISTRIBUTOR FIRST -                          
         OI    GLINDS2,GLMIDHED    MAKE SURE MIDHEAD SET FOR THIS RPT           
         CLI   MULTIDSW,C'N'                                                    
         BE    XIT                                                              
         L     R1,GLAIFLD          GET ADDRESS OF FIELD                         
         CLC   0(11,R1),XFF        TEST FOR DISTRIBUTOR                         
         BNE   *+14                                                             
         CLC   LASTDIST,0(R1)      NONE-DON'T SET PAGE TO 1 IF NO               
         BE    XIT                      DISTRIBUTOR LAST TIME EITHER            
         MVC   LASTDIST,0(R1)                                                   
         L     R1,ASPOOLD          RESET PAGE NUMBER TO 1                       
         USING SPOOLD,R1                                                        
         MVC   PAGE,=H'1'                                                       
         DROP  R1                                                               
         B     XIT                                                              
         SPACE 2                                                                
OSTAT    MVC   0(7,R3),=C'STATION'  STATION MIDLINE                             
         MVC   SBSTA,0(R2)                                                      
         MVC   SBCBLNET,5(R2)                                                   
         MVC   10(4,R3),0(R2)                                                   
         CLI   0(R2),C'0'          TEST CABLE                                   
         BL    OSTAT2                                                           
         MVI   14(R3),C'/'                                                      
         MVC   15(3,R3),5(R2)                                                   
         LA    R5,20(R3)                                                        
         B     OSTAT6                                                           
*                                                                               
OSTAT2   LA    R5,14(R3)                                                        
         CLI   3(R2),C' '                                                       
         BH    *+6                                                              
         BCTR  R5,0                                                             
         MVC   0(3,R5),=C'-TV'                                                  
         CLI   4(R2),C'T'                                                       
         BE    OSTAT4                                                           
         CLI   4(R2),C' '                                                       
         BNH   OSTAT4                                                           
         MVC   1(1,R5),4(R2)                                                    
         MVI   2(R5),C'M'                                                       
*                                                                               
OSTAT4   LA    R5,5(R5)                                                         
*                                                                               
OSTAT6   MVC   SVSTA,10(R3)        SAVE FORMATTED STATION                       
         BAS   RE,GETSTA           GET STATION DETAILS                          
         OC    SBCHAN,SBCHAN                                                    
         BZ    *+20                                                             
         MVC   0(2,R5),=C'CH'      FORMAT CHANNEL                               
         MVC   3(4,R5),SBCHAN                                                   
         LA    R5,9(R5)                                                         
         OC    SBAFFIL,SBAFFIL                                                  
         BZ    XIT                                                              
         MVC   0(3,R5),SBAFFIL     FORMAT AFFILIATE                             
         B     XIT                                                              
         SPACE 2                                                                
FSTA     XC    TOTSPOTS,TOTSPOTS   FIRST FOR STATION                            
         B     XIT                                                              
         SPACE 2                                                                
TSTA     MVC   0(8,R3),SVSTA       STATION TOTAL                                
         MVC   9(3,R3),=C'TOT'                                                  
         ZIC   R1,GLARGS                                                        
         BCTR  R1,0                                                             
         MH    R1,=Y(L'QTRDATES)                                                
         LA    R1,QTRDATES(R1)                                                  
         MVC   198(L'QTRDATES,R3),0(R1)                                         
         CLI   NUMWKS,14           TEST MORE THAN 1 QTR                         
         BNH   XIT                                                              
         ZIC   RE,GLOPTS+1         YES-TEST FOR CONTROL BREAK ON                
         LA    RE,7(RE)                STATION OR ABOVE                         
         CLM   RE,1,GLCBLEV                                                     
         BL    XIT                                                              
         MVI   2*198(R3),0                                                      
         LA    R3,3*198(R3)        YES-PRINT TOTAL NUMBER OF SPOTS              
         MVC   0(8,R3),SVSTA           FOR REQUEST PERIOD FOR THIS              
         MVC   9(3,R3),=C'TOT'         STATION                                  
         MVC   198(L'PERDATES,R3),PERDATES                                      
         L     R3,GLAINTP1                                                      
         LA    R3,3*198(R3)                                                     
         A     R3,TOTDISP                                                       
         EDIT  TOTSPOTS,(3,(R3))                                                
         B     XIT                                                              
         SPACE 2                                                                
ISPW     L     R5,SBACURCH         SPOTS PER WEEK COLUMN                        
         USING SCHUNKD,R5                                                       
         ZIC   R1,WEEKNUM                                                       
         BCTR  R1,0                                                             
         SLL   R1,1                                                             
         LA    R1,0(R1,R2)                                                      
         MVC   0(2,R1),SCSPOTS+2                                                
         B     XIT                                                              
         SPACE 2                                                                
OSPW     LA    RF,14               SPOTS PER WEEK OUTPUT                        
         SR    R1,R1                                                            
         SR    R5,R5                                                            
         SR    RE,RE                                                            
         CLI   SUMOPT,C'Y'         TEST MARKET SUMMARIES WANTED                 
         BNE   OSPW2                                                            
         TM    GLINDS,GLTOTLIN     AND THIS IS A TOTAL LINE                     
         BZ    OSPW2                                                            
         LA    R1,MKTTOTS          YES-ACCUMULATE SPOT TOTALS FOR THIS          
         LA    R6,WKDSPLS              QUARTER                                  
         LA    R0,56                                                            
*                                                                               
OSPW1    CLC   GLRECNO,0(R6)                                                    
         BE    OSPW2                                                            
         LA    R1,4(R1)                                                         
         LA    R6,2(R6)                                                         
         BCT   R0,OSPW1                                                         
         DC    H'0'                                                             
*                                                                               
OSPW2    ICM   RE,3,0(R2)                                                       
         BZ    OSPW4                                                            
         AR    R5,RE                                                            
         EDIT  (RE),(3,(R3))                                                    
         LTR   R1,R1                                                            
         BZ    OSPW4                                                            
         A     RE,0(R1)                                                         
         ST    RE,0(R1)                                                         
*                                                                               
OSPW4    LA    R2,2(R2)                                                         
         LA    R3,4(R3)                                                         
         LTR   R1,R1                                                            
         BZ    *+8                                                              
         LA    R1,4(R1)                                                         
         BCT   RF,OSPW2                                                         
         OC    TOTDISP,TOTDISP     TEST DISPLACEMENT TO TOTAL SPOTS             
         BNZ   OSPW6               COLUMN SET YET                               
         L     R1,GLAINTP1         NO-SET IT NOW                                
         LR    RE,R3                                                            
         SR    RE,R1                                                            
         ST    RE,TOTDISP                                                       
*                                                                               
OSPW6    LTR   R5,R5                                                            
         BZ    XIT                                                              
         EDIT  (R5),(3,(R3))       THIS STATION                                 
         TM    GLINDS,GLTOTLIN     TEST TOTAL LINE                              
         BO    XIT                                                              
         A     R5,TOTSPOTS         NO-ADD TO TOTAL SPOTS FOR STATION            
         ST    R5,TOTSPOTS                                                      
         B     XIT                                                              
         SPACE 2                                                                
HSPW     MVC   0(16,R3),DASHES                                                  
         MVC   17(19,R3),=C'NUMBER OF TELECASTS'                                
         MVC   37(17,R3),DASHES                                                 
         MVI   55(R3),C'T'                                                      
         MVI   198+57(R3),C'O'                                                  
         MVI   198+198+58(R3),C'T'                                              
         LA    R1,198(R3)                                                       
         ZIC   R7,GLARGS                                                        
         BCTR  R7,0                                                             
         MH    R7,=Y(L'WKSQ1)                                                   
         LA    R7,WKSQ1(R7)                                                     
         LA    R6,14                                                            
*                                                                               
HSPW2    CLC   0(5,R7),BLANKS                                                   
         BNH   XIT                                                              
         MVC   0(3,R1),0(R7)                                                    
         MVC   198+1(2,R1),3(R7)                                                
         LA    R1,4(R1)                                                         
         LA    R7,5(R7)                                                         
         BCT   R6,HSPW2                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* OUTPUT ROUTINE FOR MARKET SUMMARY                                   *         
***********************************************************************         
         SPACE 1                                                                
OSUM     MVI   BYTE,0                                                           
         XC    DUB,DUB                                                          
         LA    R1,MKTTOTS                                                       
         LA    RE,WKDSPLS                                                       
         LA    RF,WKSQ1                                                         
         ST    RF,FULL                                                          
         SR    R6,R6                                                            
         LA    R7,56                                                            
*                                                                               
OSUM2    MVC   0(7,R3),=C'*TOTAL*'                                              
         MVI   198(R3),0                                                        
         MVC   2*198(9,R3),=C'NO TLCSTS'                                        
         LA    R2,12(R3)                                                        
*                                                                               
OSUM4    CLI   BYTE,0              TEST FIRST WEEK                              
         BE    OSUM5               YES                                          
         CLC   BYTE,0(RE)          NO-TEST CHANGE OF QUARTER                    
         BE    OSUM5                                                            
         CLI   12(R3),C' '         YES-TEST ANYTHING PRINTED FOR LAST           
         BNH   *+12                    QTR                                      
         MVI   3*198(R3),0                                                      
         LA    R3,4*198(R3)        YES-ADVANCE 4 LINES                          
         L     RF,FULL                                                          
         LA    RF,L'WKSQ1(RF)      NEXT QUARTER DATES                           
         ST    RF,FULL                                                          
         MVC   BYTE,0(RE)                                                       
         B     OSUM2                                                            
*                                                                               
OSUM5    ICM   R5,15,0(R1)         TEST ANY SPOTS FOR THIS WEEK                 
         BZ    OSUM6                                                            
         MVC   0(5,R2),0(RF)       YES-PRINT THE WEEK DATE                      
         ST    R5,DUB+4                                                         
         AR    R6,R5               ADD TO TOTAL FOR WHOLE PERIOD                
         LA    R5,2*198(R2)                                                     
         EDIT  (B4,DUB+4),(5,(R5)) PRINT THE SPOT COUNT                         
         LA    R2,8(R2)            ADVANCE TO NEXT WEEK POSITION                
         ST    R2,DUB              SAVE A(END OF LAST QTR LINE)                 
*                                                                               
OSUM6    MVC   BYTE,0(RE)          SAVE CURRENT QUARTER NUMBER                  
         LA    R1,4(R1)            NEXT WEEK                                    
         LA    RE,2(RE)                                                         
         LA    RF,5(RF)                                                         
         BCT   R7,OSUM4                                                         
*                                  DONE--                                       
         NI    GLINDS2,255-GLMIDHED  FORCEHEAD FOR NEXT REPORT                  
         CLI   12(R3),C' '         TEST ANY WEEKS ON THIS LINE                  
         BH    OSUM8                                                            
         MVC   0(7,R3),BLANKS      NO-REMOVE CAPTION                            
         MVI   198(R3),C' '                                                     
         MVC   2*198(9,R3),BLANKS                                               
         ICM   R2,15,DUB           AND PLACE TOTAL AT END OF LAST LINE          
         BNZ   OSUM8                                                            
         DC    H'0'                                                             
*                                                                               
OSUM8    LTR   R6,R6               FORMAT THE TOTAL                             
         BZ    XIT                                                              
         MVC   0(5,R2),=C'TOTAL'                                                
         LA    R5,2*198(R2)                                                     
         EDIT  (R6),(5,(R5))                                                    
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* I/O ROUTINES FOR DISTRIBUTOR RECAP                                  *         
***********************************************************************         
         SPACE 1                                                                
IPROD    LA    R7,RECAPREC                                                      
         USING RTABLE,R7                                                        
         MVC   0(3,R2),RTPRD                                                    
         B     XIT                                                              
         SPACE 1                                                                
ITYPE    LA    R7,RECAPREC                                                      
         MVC   0(2,R2),=C'TV'                                                   
         CLI   RTMED,C'T'                                                       
         BE    XIT                                                              
         MVC   0(5,R2),=C'RADIO'                                                
         B     XIT                                                              
         SPACE 1                                                                
IMGRP    LA    R5,DISTREC                                                       
         USING DTABLE,R5                                                        
         MVC   0(24,R2),DTMGRNM                                                 
         B     XIT                                                              
         SPACE 1                                                                
HMGRP    MVC   0(12,R3),SVMGRBK                                                 
         B     XIT                                                              
         SPACE 1                                                                
IDISTNM  LA    R5,DISTREC                                                       
         MVC   0(36,R2),DTDISTNM                                                
         B     XIT                                                              
         DROP  R5                                                               
         SPACE 1                                                                
IESTIM   LA    R7,RECAPREC                                                      
         MVC   0(1,R2),RTEST                                                    
         B     XIT                                                              
         SPACE 1                                                                
IDATES   LA    R7,RECAPREC                                                      
         MVC   0(2,R2),RTSTART                                                  
         MVC   2(2,R2),RTEND                                                    
         B     XIT                                                              
         SPACE 1                                                                
ODATES   GOTO1 DATCON,DMCB,(2,(R2)),(4,(R3))                                    
         MVI   6(R3),C'-'                                                       
         GOTO1 (RF),(R1),(2,2(R2)),(4,8(R3))                                    
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* HOOK PRIOR TO DRIVER'S OUTPUT STAGE                                 *         
***********************************************************************         
         SPACE 1                                                                
OUT      CLI   CANCEL,0                                                         
         BE    XIT                                                              
         MVI   RPMODE,RPSTOP                                                    
         MVC   MYPRINT,BLANKS                                                   
         CLI   CANCEL,C'E'                                                      
         BNE   *+14                                                             
         MVC   MYPRINT(39),=C'** ESTIMATE=NO IS AN ILLEGAL REQUEST **'          
         B     *+10                                                             
         MVC   MYPRINT(32),=C'** REQUEST PERIOD IS TOO LONG **'                 
         GOTO1 SBPRINT,DMCB,MYPRINT-1,=C'BL01'                                  
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* HEADHOOK                                                            *         
***********************************************************************         
         SPACE 1                                                                
HEDHK    CLI   RCPSW,C'Y'          TEST RECAP REPORT                            
         BNE   HEDHK2                                                           
         GOTO1 GENHEAD             YES                                          
         B     HEDHKX                                                           
*                                                                               
HEDHK2   OC    DISTREC,DISTREC     DISTRIBUTOR TO PRINT?                        
         BZ    HEDHKX                                                           
         LA    R7,DISTREC          YES-                                         
         USING DTABLE,R7                                                        
         L     R2,AH4                                                           
         A     R2,PWIDTH                                                        
         MVC   52(20,R2),=C'DISTRIBUTOR NUMBER -'                               
         MVC   73(6,R2),DTDISTCD                                                
         A     R2,PWIDTH                                                        
         MVC   52(L'DTDISTNM,R2),DTDISTNM                                       
         A     R2,PWIDTH                                                        
         MVC   52(L'DTADDR1,R2),DTADDR1                                         
         A     R2,PWIDTH                                                        
         MVC   52(L'DTADDR2,R2),DTADDR2                                         
         A     R2,PWIDTH                                                        
         MVC   52(L'DTADDR3,R2),DTADDR3                                         
         A     R2,PWIDTH                                                        
         MVC   52(L'DTADDR4,R2),DTADDR4                                         
*                                                                               
HEDHKX   B     XIT                                                              
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* GET STAION DETAILS                                                  *         
***********************************************************************         
         SPACE 1                                                                
GETSTA   NTR1  ,                                                                
         XC    SBCHAN,SBCHAN                                                    
         XC    SBAFFIL,SBAFFIL                                                  
         CLI   CHANOPT,C'Y'        TEST CHANNEL OR AFFILIATE NEEDED             
         BE    *+12                                                             
         CLI   AFFILOPT,C'Y'                                                    
         BNE   GETSTAX                                                          
         ICM   R2,15,SBASTABF      YES-GET FROM STATION BUFFER                  
         BZ    GETSTAX                                                          
         MVC   DUB(5),SBSTA                                                     
         CLI   SBSTA+4,C' '                                                     
         BH    *+8                                                              
         MVI   DUB+4,C'T'                                                       
         MVC   DUB+5(3),SBCBLNET                                                
         GOTO1 MSPACK,DMCB,SBMKT,DUB,WORK                                       
         LA    R3,8(R2)                                                         
         L     R5,0(R2)                                                         
         L     R1,4(R2)                                                         
         ST    R1,DMCB+20                                                       
         LA    R1,STABUFFL                                                      
         ST    R1,DMCB+12                                                       
         GOTO1 BINSRCH,DMCB,(0,WORK+2),(R3),(R5),,(0,3)                         
         CLI   0(R1),1             TEST RECORD FOUND                            
         BE    GETSTAX                                                          
         ICM   R2,15,0(R1)         YES                                          
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING STABUFFD,R2                                                      
         CLI   AFFILOPT,C'Y'                                                    
         BNE   *+10                                                             
         MVC   SBAFFIL,STBAFFIL    EXTRACT THE AFFILIATE                        
         CLI   CHANOPT,C'Y'                                                     
         BNE   GETSTAX                                                          
         MVC   SBCHAN,STBCHAN      AND CHANNEL                                  
*                                                                               
GETSTAX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* BINSRCH ROUTINES                                                    *         
***********************************************************************         
         SPACE 1                                                                
BINADD   NTR1  ,                                                                
         LM    R2,R3,0(R1)         A(RECORD)/A(TABLE)                           
         LM    R4,R8,0(R3)                                                      
         GOTO1 BINSRCH,DMCB,(X'01',(R2)),(R4),(R5),(R6),(R7),(R8)               
         ICM   RE,15,0(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RE,ABINREC          SAVE A(RECORD IN TABLE)                      
         MVC   4(4,R3),8(R1)       UPDATE N'RECORDS IN TABLE                    
         B     XIT                                                              
*                                                                               
BINFIND  NTR1  ,                                                                
         LM    R2,R3,0(R1)         A(RECORD)/A(TABLE)                           
         LM    R4,R8,0(R3)                                                      
         GOTO1 BINSRCH,DMCB,(X'00',(R2)),(R4),(R5),(R6),(R7),(R8)               
         CLI   0(R1),1                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   ABINREC,0(R1)                                                    
         B     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         DS    0F                                                               
ASAVE    DS    A                                                                
ABINREC  DS    A                                                                
SVGDTAB  DS    (SVGDMAX)A                                                       
SVGDMAX  EQU   50                                                               
TOTSPOTS DS    F                                                                
TOTDISP  DC    F'0'                                                             
*                                                                               
DISTREC  DS    CL(DTLEN)                                                        
RECAPREC DS    CL(RTLEN)                                                        
*                                                                               
CMTOPT   DS    CL1                                                              
AFFILOPT DS    CL1                                                              
CHANOPT  DS    CL1                                                              
DISTOPT  DS    CL1                                                              
SUMOPT   DS    CL1                                                              
*                                                                               
MULTIDSW DS    CL1                                                              
MKTSW    DS    CL1                                                              
RCPSW    DS    CL1                                                              
SPILL    DS    CL1                                                              
CANCEL   DS    CL1                                                              
*                                                                               
SVPRD    DS    XL1                                                              
SVEST    DS    XL1                                                              
SVSTA    DS    CL8                                                              
SVMGR    DS    CL4                                                              
SVDISTCD DS    CL7                                                              
*                                                                               
MGRPNAME DS    CL24                                                             
LASTDIST DS    CL11                                                             
*                                                                               
NUMWKS   DS    XL1                                                              
NUMQTRS  DS    XL1                                                              
WEEKNUM  DS    XL1                                                              
WKSQ1    DC    CL70' '             14 X 5-BYTE WEEKS                            
WKSQ2    DC    CL70' '                                                          
WKSQ3    DC    CL70' '                                                          
WKSQ4    DC    CL70' '                                                          
WKDSPLS  DC    XL(56*2)'00'        56 WEEKS - QTRNUM/WKNUM                      
QTRDATES DS    4CL17               FORMATTED QTR DATES                          
PERDATES DS    CL17                FORMATTED PERIOD DATES                       
*                                                                               
         DS    0F                                                               
MKTTOTS  DS    XL(14*4*4)          MARKET SPOT TOTALS                           
*                                                                               
PRDTAB   DS    XL220                                                            
PRDMOLS  EQU   X'80'                                                            
*                                                                               
MILLER   DS    C                   FLAG FOR MILLER CLT (Y/N)                    
*                                                                               
DCPROF   DS    0CL16                                                            
DCCOM    DS    CL1                                                              
DCSPILL  DS    CL1                                                              
DCCHAN   DS    CL1                                                              
DCCORP   DS    CL1                                                              
DCRECAP  DS    CL1                                                              
DCCOMP   DS    CL1                                                              
DCUNIT   DS    CL1                                                              
DCLEDGER DS    CL1                                                              
DCMGRLEN DS    CL1                                                              
DCMKTS   DS    CL1                                                              
DCOUTLET DS    CL1                                                              
         DS    CL5                                                              
*                                                                               
B9PROF   DS    0CL16                                                            
B9COMP   DS    CL1                                                              
B9UNIT   DS    CL1                                                              
B9LEDGER DS    CL1                                                              
B9MGRLEN DS    CL1                                                              
B9MKTS   DS    CL1                                                              
B9OUTLET DS    CL1                                                              
         DS    CL4                                                              
B9MEDCAL DS    CL1                                                              
         DS    CL5                                                              
*                                                                               
SVB9PROF DS    CL16                                                             
B9APROF  DS    CL16                                                             
*                                                                               
F0PROF   DS    0CL16                                                            
         DS    CL1                                                              
F0POLSCM DS    CL1                                                              
         DS    CL14                                                             
*                                                                               
         DS    XL1                                                              
MYPRINT  DS    CL132                                                            
BLANKS   DC    CL132' '                                                         
DASHES   DC    CL20'--------------------'                                       
XFF      DC    CL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'                           
*                                                                               
***********************************************************************         
* VALUES SAVED BETWEEN REQUESTS                                                 
*                                                                               
SAVVALS  DS    0X                                                               
*                                                                               
ADISTAB  DS    A                                                                
ARCPTAB  DS    A                                                                
AFOTABS  DS    A                                                                
SPLID    DS    XL1                                                              
SVMGRBK  DS    CL12                                                             
*                                                                               
SAVVALSL EQU   *-SAVVALS                                                        
***********************************************************************         
*                                                                               
*                                                                               
GDAREA   DS    (GDSECTL)C          FINDOUT WORK AREA                            
         EJECT                                                                  
***********************************************************************         
* SET WEEKS TABLES                                                    *         
***********************************************************************         
         SPACE 1                                                                
SETWKS   NTR1  BASE=*,LABEL=*                                                   
         L     R4,AWEEKS           COUNT N'WEEKS IN REQUEST PERIOD              
         SR    R1,R1                                                            
         CLI   0(R4),0                                                          
         BE    *+12                                                             
         LA    R4,4(R4)                                                         
         BCT   R1,*-12                                                          
         LPR   R1,R1                                                            
         STC   R1,NUMWKS                                                        
*                                                                               
         LA    R0,4                SET UP RELATIONSHIPS BETWEEN                 
         LA    R2,WKSQ1            WEEKS AND QUARTERS                           
         ST    R2,FULL                                                          
         L     R3,AQTRS                                                         
         L     R4,AWEEKS                                                        
         LA    R5,WKDSPLS                                                       
         LA    R6,1                                                             
         LA    R7,1                                                             
*                                                                               
SETW2    CLI   0(R4),0             TEST END OF WEEKS                            
         BE    SETW8                                                            
         CLI   NUMWKS,14           TEST NO MORE THAN 14 WEEKS                   
         BNH   SETW6               YES-ALL WEEKS GO TO QTR1                     
*                                                                               
SETW4    CLC   2(2,R4),2(R3)       TEST WEEK IN THIS QTR                        
         BNH   SETW6                                                            
         LA    R3,4(R3)            NO-TRY NEXT QTR                              
         LA    R6,1(R6)            NEXT QTR NUMBER                              
         LA    R7,1                FIRST WEEK                                   
         L     R2,FULL                                                          
         LA    R2,L'WKSQ1(R2)                                                   
         ST    R2,FULL                                                          
         BCT   R0,SETW4                                                         
         MVI   NUMQTRS,5           TOO MANY QUARTERS                            
         B     SETWX                                                            
*                                                                               
SETW6    GOTO1 DATCON,DMCB,(2,(R4)),(4,(R2))                                    
         STC   R6,0(R5)            QTR NUMBER                                   
         STC   R6,NUMQTRS          SET N'QTRS IN REQUEST                        
         STC   R7,1(R5)            WEEK NUMBER WITHIN QTR                       
         LA    R7,1(R7)                                                         
         LA    R2,5(R2)            NEXT WEEK                                    
         LA    R4,4(R4)                                                         
         LA    R5,2(R5)                                                         
         B     SETW2                                                            
*                                                                               
SETW8    LA    R2,QTRDATES         FORMAT QUARTER DATES                         
         L     R3,AQTRS                                                         
         LA    R0,4                                                             
*                                                                               
SETW10   CLI   0(R3),0                                                          
         BE    SETWX                                                            
         GOTO1 DATCON,DMCB,(2,(R3)),(5,(R2))                                    
         GOTO1 (RF),(R1),(2,2(R3)),(5,9(R2))                                    
         MVI   8(R2),C'-'                                                       
         CH    R0,=H'4'                                                         
         BNE   *+10                                                             
         MVC   PERDATES(9),0(R2)   FORMAT PERIOD DATES                          
         MVC   PERDATES+9(8),9(R2)                                              
         LA    R2,L'QTRDATES(R2)                                                
         LA    R3,4(R3)                                                         
         BCT   R0,SETW10                                                        
*                                                                               
SETWX    XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DISTRIBUTOR TABLE DSECT                                             *         
***********************************************************************         
         SPACE 1                                                                
DTABLE   DSECT                                                                  
DTKEY    DS    0CL11                                                            
DTMGR    DS    CL4                 MARKET GROUP CODE (DIGITS ONLY)              
DTDISTCD DS    CL7                 DISTRIBUTOR CODE                             
DTMGRNM  DS    CL24                MARKET GROUP NAME                            
DTDISTNM DS    CL36                DISTRIBUTOR NAME                             
DTADDR1  DS    CL26                DISTRIBUTOR ADDRESS LINES                    
DTADDR2  DS    CL26                                                             
DTADDR3  DS    CL26                                                             
DTADDR4  DS    CL26                                                             
DTLEN    EQU   *-DTABLE                                                         
DTMAX    EQU   4000                MAX N'ENTRIES                                
         SPACE 2                                                                
***********************************************************************         
* RECAP TABLE DSECT                                                   *         
***********************************************************************         
         SPACE 1                                                                
RTABLE   DSECT                                                                  
RTKEY    DS    0CL16                                                            
RTPRD    DS    CL3                 PRODUCT                                      
RTMED    DS    CL1                 MEDIA                                        
RTMGR    DS    CL4                 MARKET GROUP CODE (DIGITS ONLY)              
RTDISTCD DS    CL7                 DISTRIBUTOR CODE                             
RTEST    DS    XL1                 ESTIMATE NUMBER                              
RTSTART  DS    XL2                 EARLIEST SPOT DATE                           
RTEND    DS    XL2                 LATEST SPOT DATE                             
RTSPOTS  DS    XL4                 NUMBER OF SPOTS                              
RTLEN    EQU   *-RTABLE                                                         
RTMAX    EQU   4000                MAX N'ENTRIES                                
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
         SPACE 1                                                                
WORKD    DSECT                                                                  
*                                                                               
SAVERD   DS    F                                                                
         DS    F                                                                
*                                                                               
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
* OTHER DSECTS ARE HIDDEN IN HERE                                               
*                                                                               
*SPWRIWORKD                                                                     
*SPOTTABD                                                                       
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDBIGBOX                                                                       
*DMPRTQL                                                                        
*DDBUFFALOD                                                                     
*DDCNTRL2                                                                       
*DDMASTD                                                                        
*DRGLOBAL                                                                       
*DRIVETABLE                                                                     
*DRINTRECD                                                                      
*SPGENBUY                                                                       
*SPWRIFFD                                                                       
         PRINT   OFF                                                            
       ++INCLUDE SPWRIWORKD                                                     
       ++INCLUDE SPOTTABD                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE DDBUFFALOD                                                     
       ++INCLUDE DDCNTRL2                                                       
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE DRIVETABLE                                                     
       ++INCLUDE DRINTRECD                                                      
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
         PRINT ON                                                               
**       ++INCLUDE FOLINKD                                                      
       ++INCLUDE SPRTLBLK                                                       
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE SPWRIFFD                                                       
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE SPWRIF1D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPWRID6D                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE DDTWADCOND                                                     
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'100SPWRI26   12/16/04'                                      
         END                                                                    
