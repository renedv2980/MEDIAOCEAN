*          DATA SET SPWRI03    AT LEVEL 020 AS OF 09/26/08                      
*PHASE T20403B,*                                                                
         TITLE 'T20403 - SPOTPAK WRITER POST BUY EXCEPTION REPORT'              
***********************************************************************         
*                                                                     *         
*                 M O D I F I C A T I O N S   L O G                   *         
*                                                                     *         
*--DATE---LVL-BY-----CHANGE-------------------------------------------*         
*                                                                     *         
* 08SEP08 20  AKT -- SBQBOOK RE-WORKED                                *         
* 01FEB05 19  AKT -- 2 DECIMAL DEMOS SUPPORT                          *         
* 03NOV03 18  AKT -- FIX MGROUP X'40' BUGS                            *         
* 14OCT02 17  EFJ -- 2 CHAR MGR SCHEMES                               *         
* 13JUN02 16  EFJ -- ONLY MEDIA T FOR MVMFI                           *         
* 20DEC00 14  EFJ -- FIX SPLIT BUY TEST                               *         
* 02MAR00 13  EFJ -- SUPPORT SPOTBUY SPLIT BUYS                       *         
* 19JAN99 12  EFJ -- OOPS...                                          *         
* 06JAN99 11  EFJ -- SBQBKS ADDRESSABILITY                            *         
* 16JUN98 10  NRK -- Y2K COMPLIANCE                                   *         
* 16JUN98 HISTORY LOST                                                *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
T20403   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKL,T20403,RA,RR=R2                                            
         LR    R6,RC                                                            
         USING WORKD,R6                                                         
         ST    R2,RELO                                                          
         MVC   SAVERD,4(RD)        SAVE A(CALLING PROGRAM'S SAVE AREA)          
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         ST    RC,AGEND                                                         
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
*                                                                               
         LA    R0,AXTRAN           SET EXTENTION ROUTINES                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         L     R1,=A(T20403X)                                                   
         A     R1,RELO                                                          
         ST    R1,AXTRA(RE)                                                     
         STC   RF,AXTRA(RE)                                                     
         LA    RF,1(RF)                                                         
         LA    RE,4(RE)                                                         
         BCT   R0,*-16                                                          
*                                  REPORT CALLING MODE                          
         CLI   RPMODE,RPINIT       INITIALIZATION                               
         BE    INIT                                                             
         CLI   RPMODE,RPVAL        VALIDATION                                   
         BE    VALID                                                            
         CLI   RPMODE,RPINPUT      INPUT                                        
         BE    INPUT                                                            
         CLI   RPMODE,RPDRHOOK     DRIVER HOOK                                  
         BE    DRHOOK                                                           
         B     XIT                                                              
*                                                                               
XIT      XIT1  ,                                                                
         EJECT                                                                  
* INITIALIZATION                                                                
*                                                                               
INIT     MVI   SVPRD,0                                                          
         MVI   SVEST,0                                                          
         L     R1,=A(MYDEMOS)                                                   
         A     R1,RELO                                                          
         XC    0(L'MYDEMOS,R1),0(R1)                                            
         XC    SVBUYKEY,SVBUYKEY                                                
         OI    SBQSKIP,SBQSKBIL    SKIP READING STATION BILL RECORDS            
         OI    SBQDATA,SBQDGOAL+SBQDPUR+SBQDRERT+SBQDAFFD   SET DATA            
         OI    SBQDPTLN,SBQDLSGR   SUPPRESS DAYPART GROUP TOTALS                
         OI    COLIND,COLIDEM      INDICATE DEMOS IS THIS RPT                   
         OI    COLIND,COLIRND      ROUND OUT PENNIES                            
         OI    COLIND,COLINDR      NO DEMO ROUNDING                             
***                                                                             
* READ AGENCY LEVEL 00 PROFILE FOR 2 DECIMAL DEMOS                              
***                                                                             
         XC    WORK,WORK           PREPARE FOR PROFILE READ                     
         MVC   WORK(4),=C'S000'                                                 
         LA    R1,SBAGYREC                                                      
         MVC   WORK+4(2),1(R1)     GET AGENCY LEVEL 00 PROFILE                  
         GOTO1 GETPROF,DMCB,(X'90',WORK),WORK+16,DATAMGR                        
         CLI   WORK+25,C'Y'        2 DECIMAL RATINGS?                           
         BNE   INIT05              NO                                           
*                                                                               
         LA    R1,SBLOCK                                                        
         OI    SBEFLAG4-SBLOCK(R1),SBE42DEC      YES                            
*                                                                               
INIT05   CLI   WGTOPT,0            IF MARKET WEIGHTING OPTION NOT SET,          
         BNE   *+8                 TURN IT OFF                                  
         MVI   WGTOPT,C'N'                                                      
*                                                                               
         MVC   PCTTOLA,=H'100'     DEFAULT TOLERENCE IS 10PCT                   
         MVI   DATAOPT,C'1'        DEFAULT IS PURCH VS GOAL                     
         MVI   INDEXOPT,C'R'       ACHIEVED                                     
         MVI   COMPARE,C'P'        VS PURCHASED                                 
         CLI   RPTSCRN,0           TEST USER REPORT SCREEN                      
         BE    INIT20                                                           
         LA    R2,PBEPCAH          YES-VALIDATE OPTIONS                         
         GOTO1 AVALPCT             PERCENT ABOVE                                
         BNE   *+10                                                             
         MVC   PCTTOLA,HALF                                                     
         LA    R2,PBEPCBH          PERCENT BELOW                                
         GOTO1 AVALPCT                                                          
         BNE   *+14                                                             
         MVC   PCTTOLB,HALF                                                     
         B     *+10                                                             
         MVC   PCTTOLB,PCTTOLA     YES-LET PCT BELOW = PCT ABOVE                
         LA    R2,PBEDTAH                                                       
         CLI   5(R2),0             DATA                                         
         BE    INIT10                                                           
         MVC   DATAOPT,PBEDTA                                                   
         CLI   DATAOPT,C'1'                                                     
         BL    EINV                                                             
         CLI   DATAOPT,C'3'                                                     
         BH    EINV                                                             
*                                                                               
INIT10   LA    R2,PBEIN1H          INDEX                                        
         SR    RE,RE                                                            
         ICM   RE,1,5(R2)                                                       
         BZ    INIT13                                                           
         BCTR  RE,0                                                             
         LA    R1,NDXTAB1                                                       
*                                                                               
INIT12   CLI   0(R1),0                                                          
         BE    EINV                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R1),8(R2)                                                    
         BE    *+12                                                             
         LA    R1,L'NDXTAB1(R1)                                                 
         B     INIT12                                                           
         MVC   INDEXOPT,9(R1)                                                   
         CLI   INDEXOPT,C'A'       AFFIDAVIT INVALID FOR MEDIA N OR C           
         BNE   INIT13                                                           
         CLI   SBQMED,C'N'                                                      
         BE    EINV                                                             
         CLI   SBQMED,C'C'                                                      
         BE    EINV                                                             
*                                                                               
INIT13   LA    R2,PBEIN2H          VERSUS                                       
         SR    RE,RE                                                            
         ICM   RE,1,5(R2)                                                       
         BZ    INIT15                                                           
         BCTR  RE,0                                                             
         LA    R1,NDXTAB2                                                       
*                                                                               
INIT14   CLI   0(R1),0                                                          
         BE    EINV                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R1),8(R2)                                                    
         BE    *+12                                                             
         LA    R1,L'NDXTAB2(R1)                                                 
         B     INIT14                                                           
         MVC   COMPARE,0(R1)                                                    
         CLI   COMPARE,C'L'                                                     
         BNE   *+8                                                              
         MVI   COMPARE,C'O'                                                     
         CLI   DATAOPT,C'1'                                                     
         BNE   *+16                                                             
         CLI   COMPARE,C'O'                                                     
         BE    EINV                                                             
         B     INIT15                                                           
         CLI   DATAOPT,C'2'                                                     
         BNE   *+16                                                             
         CLI   COMPARE,C'G'                                                     
         BE    EINV                                                             
         B     INIT15                                                           
         CLI   COMPARE,C'P'                                                     
         BE    EINV                                                             
         B     INIT15                                                           
*                                                                               
NDXTAB1  DS    0CL10                                                            
         DC    CL9'ACHIEVED ',C'R'                                              
         DC    CL9'AFFIDAVIT',C'A'                                              
         DC    X'00'                                                            
*                                                                               
NDXTAB2  DC    CL9'GOAL'                                                        
         DC    CL9'ORDERED'                                                     
         DC    CL9'LOCKIN'                                                      
         DC    CL9'PURCHASED'                                                   
         DC    X'00'                                                            
*                                                                               
INIT15   LA    R2,PBEAFFH          AFFID REPORT OPTION                          
         MVI   AFFIDS,C'N'                                                      
         CLI   5(R2),0                                                          
         BE    INIT16                                                           
         MVC   AFFIDS,8(R2)                                                     
         CLI   AFFIDS,C'N'                                                      
         BE    INIT16                                                           
         CLI   AFFIDS,C'Y'                                                      
         BNE   EINV                                                             
         CLI   SBQMED,C'N'         AFFIDS=Y INVALID FOR MEDIA = N OR C          
         BE    EINV                                                             
         CLI   SBQMED,C'C'                                                      
         BE    EINV                                                             
*                                                                               
INIT16   LA    R2,PBEBK1H          BOOKS                                        
         LR    R3,R9                                                            
         AHI   R3,SBQBOOK-SYSD                                                  
         MVI   SBQUPGRD,1          DISALLOW UPGRADE                             
         GOTO1 AVALBOOK            ACTUAL BOOK                                  
*                                                                               
         XC    SBQUPGRD,SBQUPGRD   ALLOW UPGRADE FOR LATEST OR PREVIOUS         
         LA    R2,PBEBK2H                                                       
         LR    R3,R9                                                            
         AHI   R3,SBQBOOK-SYSD+4                                                
         XC    FULL,FULL                                                        
         GOTO1 AVALBOOK            LATEST BOOK                                  
         MVC   AUPGHDR,FULL                                                     
         OC    SBQUPGRD,SBQUPGRD   TEST UPGRADE                                 
         BNZ   *+10                                                             
         LR    R3,R9               NO-                                          
         AHI   R3,SBQBOOK-SYSD+8                                                
*                                                                               
         LA    R2,PBEBK3H                                                       
         GOTO1 AVALBOOK            PREVIOUS BOOK                                
         CLC   8(3,R2),=C'ACT '                                                 
         BE    EBKS                                                             
         MVC   AUPGHDR,FULL                                                     
*                                                                               
         MVI   DPTOPT,C'N'                                                      
         LA    R2,PBEDPTH          DAYPART DETAIL OPTION                        
         CLI   5(R2),0                                                          
         BE    INIT16A             DEFAULT=NO                                   
         MVC   DPTOPT,8(R2)                                                     
         CLI   8(R2),C'N'                                                       
         BE    INIT16A                                                          
         CLI   8(R2),C'Y'                                                       
         BNE   EINV                                                             
*                                                                               
INIT16A  MVI   ALLOPT,C'N'                                                      
         LA    R2,PBEALLH          ALL MARKETS/DAYPARTS OPTION                  
         CLI   5(R2),0                                                          
         BE    INIT16B             DEFAULT=NO                                   
         MVC   ALLOPT,8(R2)                                                     
         CLI   8(R2),C'N'                                                       
         BE    INIT16B                                                          
         CLI   8(R2),C'Y'                                                       
         BNE   EINV                                                             
*                                                                               
INIT16B  MVI   MORDER,C'N'                                                      
         LA    R2,PBEMRKH          MARKET ORDER                                 
         CLI   5(R2),0                                                          
         BE    INIT17              DEFAULT=NUMERIC                              
         MVC   MORDER,8(R2)                                                     
         CLI   8(R2),C'N'                                                       
         BE    INIT17                                                           
         CLI   8(R2),C'A'                                                       
         BE    INIT17                                                           
         CLI   8(R2),C'R'                                                       
         BNE   EINV                                                             
         OI    DATAIND5,DIMKTRNK   INDICATE TO GET MARKET RANKS                 
*                                                                               
INIT17   LA    R2,PBECOMH          BUYLINE COMMENTS                             
         CLI   5(R2),0                                                          
         BE    INIT17A                                                          
         CLI   8(R2),C'N'                                                       
         BE    INIT17A                                                          
         CLI   8(R2),C'Y'                                                       
         BNE   EINV                                                             
         OI    ROWIND,ROWIBYCM                                                  
*                                                                               
INIT17A  MVI   SINGRPT,0           VALIDATE FURTHER OPTIONS                     
         MVI   COSTOPT,C'N'                                                     
         LA    R2,PBEFOPH                                                       
         CLI   5(R2),0                                                          
         BE    INIT17F                                                          
         GOTO1 SCANNER,DMCB,(R2),BLOCK,0                                        
         SR    R0,R0                                                            
         ICM   R0,1,4(R1)                                                       
         BZ    EINV                                                             
         LA    R4,BLOCK                                                         
*                                                                               
INIT17B  SR    RE,RE                                                            
         ICM   RE,1,0(R4)                                                       
         BZ    EINV                                                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R4),=C'REPORT'                                              
         BNE   INIT17C                                                          
         MVC   SINGRPT,22(R4)                                                   
         CLI   SINGRPT,C'1'                                                     
         BL    EINV                                                             
         CLI   SINGRPT,C'3'                                                     
         BH    EINV                                                             
         NI    SINGRPT,X'0F'                                                    
         MVI   AFFIDS,C'N'                                                      
         CLI   SINGRPT,3                                                        
         BNE   *+8                                                              
         MVI   AFFIDS,C'Y'                                                      
         B     INIT17E                                                          
*                                                                               
INIT17C  EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R4),=C'COSTS'                                               
         BNE   INIT17D                                                          
         MVI   COSTOPT,C'Y'                                                     
         CLI   1(R4),0                                                          
         BE    INIT17E                                                          
         MVC   COSTOPT,22(R4)                                                   
         CLI   COSTOPT,C'Y'                                                     
         BE    INIT17E                                                          
         CLI   COSTOPT,C'N'                                                     
         BNE   EINV                                                             
*                                                                               
INIT17D  EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R4),=C'VARIANCE'                                            
         BNE   EINV                                                             
         MVI   VAROPT,C'Y'                                                      
         CLI   1(R4),0                                                          
         BE    INIT17E                                                          
         MVC   VAROPT,22(R4)                                                    
         CLI   VAROPT,C'Y'                                                      
         BE    INIT17E                                                          
         CLI   VAROPT,C'N'                                                      
         BNE   EINV                                                             
*                                                                               
INIT17E  LA    R4,32(R4)                                                        
         BCT   R0,INIT17B                                                       
*                                                                               
INIT17F  TM    DOWNOPT,DOWNON      REPORT OPTION MANDATORY FOR                  
         BZ    INIT18              DOWNLOADING                                  
         CLI   SINGRPT,0                                                        
         BE    ENOREP                                                           
*                                                                               
INIT18   LA    R2,PBETITH          TITLE                                        
         MVC   TITLE,SPACES                                                     
         MVC   TITLE(25),=C'POST BUY EXCEPTION REPORT'                          
         CLI   5(R2),0                                                          
         BE    INIT19                                                           
         GOTO1 ANY                                                              
         MVC   TITLE,WORK                                                       
*                                                                               
INIT19   GOTO1 CENTER,DMCB,TITLE,63                                             
*                                                                               
INIT20   LH    RE,=H'1000'         SET LOWER AND UPPER PCT TOLERENCES           
         LH    RF,PCTTOLB                                                       
         LR    R1,RE                                                            
         SR    R1,RF                                                            
         ST    R1,LOLIM                                                         
         LH    RF,PCTTOLA                                                       
         AR    RE,RF                                                            
         ST    RE,HILIM                                                         
*                                                                               
INIT22   SR    R1,R1               FIND EXTRA LENGTH OF RECORD                  
         CLI   SBQPGRD,C' '        DUE TO PRODUCT AND MARKET GROUPS             
         BNH   INIT24              AND MARKETS RANKED                           
         LA    R1,2(R1)                                                         
         CLC   SBPGR1LN,SBPGR2LN                                                
         BE    INIT24                                                           
         LA    R1,2(R1)                                                         
*                                                                               
INIT24   CLI   SBQMGRD,0                                                        
         BE    INIT26                                                           
         LA    R1,2(R1)                                                         
         CLC   SBMGR1LN,SBMGR2LN                                                
         BE    INIT26                                                           
         LA    R1,2(R1)                                                         
         CLC   SBMGR2LN,SBMGR3LN                                                
         BE    INIT26                                                           
         LA    R1,2(R1)                                                         
*                                                                               
INIT26   ST    R1,LGRPS            EXTRA LENGTH FOR PRD AND MKT GROUPS          
         CLI   MORDER,C'R'         TEST MARKETS RANKED                          
         BNE   *+8                                                              
         LA    R1,2(R1)            YES-ADD 2 FOR MKT RANK NUMBER                
         CLI   MORDER,C'A'         TEST MARKETS RANKED                          
         BNE   *+8                                                              
         LA    R1,8(R1)            YES-ADD 8 FOR MKT NAME                       
         CLI   COSTOPT,C'Y'        TEST COST OPTION                             
         BNE   *+8                                                              
         LA    R1,4(R1)            YES-ADD 4 FOR COST                           
         CLI   DPTOPT,C'Y'         TEST DAYPART REPORT                          
         BNE   *+8                                                              
         LA    R1,8(R1)            YES-ADD 8 FOR DAYPART ROW                    
         ST    R1,EXTRALEN         EXTRA LENGTH FOR REPORTS 2 AND 3             
*                                                                               
         LA    R4,RPTLEVS          SET LEVELS                                   
         CLI   DPTOPT,C'Y'                                                      
         BNE   *+8                                                              
         LA    R4,RPTLEVS2                                                      
         CLI   SBQPGRD,C' '                                                     
         BH    *+14                                                             
         XC    1(2,R4),1(R4)       SET PRODUCT GROUPS                           
         B     INIT28                                                           
         CLC   SBPGR1LN,SBPGR2LN                                                
         BNE   INIT28                                                           
         MVI   2(R4),0                                                          
*                                                                               
INIT28   CLI   SBQMGRD,0           SET MARKET GROUPS                            
         BH    *+14                                                             
         XC    5(3,R4),5(R4)                                                    
         B     INIT30                                                           
         CLC   SBMGR1LN,SBMGR2LN                                                
         BNE   *+14                                                             
         XC    6(2,R4),6(R4)                                                    
         B     INIT30                                                           
         CLC   SBMGR2LN,SBMGR3LN                                                
         BNE   INIT30                                                           
         MVI   7(R4),0                                                          
*                                                                               
INIT30   LA    R1,LEVELS           SET THE LEVELS                               
         LA    RF,1                                                             
*                                                                               
INIT31   CLI   0(R4),X'FF'                                                      
         BE    INIT36                                                           
         CLI   0(R4),0                                                          
         BE    INIT34                                                           
         MVC   0(1,R1),0(R4)                                                    
         CLI   0(R1),QMKT                                                       
         BNE   INIT32                                                           
         STC   RF,MKTLEV           MARKET LEVEL                                 
         CLI   DPTOPT,C'Y'         TEST DAYPART REPORT                          
         BE    *+12                                                             
         STC   RF,LSTHEDLV         NO-LAST HEADLINE LEVEL                       
         B     INIT33                                                           
         CLI   MORDER,C'R'         YES-MARKET COULD BE MARKET RANKED            
         BNE   *+12                    OR MARKET NAME                           
         MVI   0(R1),QMKTR                                                      
         B     INIT33                                                           
         CLI   MORDER,C'A'                                                      
         BNE   INIT33                                                           
         MVI   0(R1),QMKTNM                                                     
         B     INIT33                                                           
*                                                                               
INIT32   CLI   0(R1),QDPT                                                       
         BNE   INIT33                                                           
         CLI   DPTOPT,C'Y'                                                      
         BNE   INIT33                                                           
         STC   RF,LSTHEDLV         LAST HEADLINE LEVEL FOR DAYPART              
         STC   RF,DPTLEV           VERSION OF PBE                               
*                                                                               
INIT33   CLI   0(R1),QSTA                                                       
         BNE   *+8                                                              
         STC   RF,MIDLEV           MIDLINE LEVEL                                
         LA    R1,1(R1)                                                         
         LA    RF,1(RF)                                                         
*                                                                               
INIT34   LA    R4,1(R4)                                                         
         B     INIT31                                                           
*                                                                               
INIT36   MVI   MYFIRSTH,12         SET DRIVER'S FIRST HEADLINE                  
*                                                                               
INITX    B     XIT                                                              
         SPACE 2                                                                
* REPORT LEVELS BASED ON REPORT 2 (DETAIL REPORT)                               
*                                                                               
RPTLEVS  DC    AL1(QCLT)           HEADLINES                                    
         DC    AL1(QPRDGR1)                                                     
         DC    AL1(QPRDGR2)                                                     
         DC    AL1(QPRD)                                                        
         DC    AL1(QEST)                                                        
         DC    AL1(QMKTGR1)                                                     
         DC    AL1(QMKTGR2)                                                     
         DC    AL1(QMKTGR3)                                                     
         DC    AL1(QRPTSEQ)                                                     
         DC    AL1(QMKT)                                                        
         DC    AL1(QRPTSEQ)                                                     
         DC    AL1(QSTA)           MIDLINE                                      
         DC    AL1(QBUY)           DETAIL                                       
         DC    X'FF'                                                            
         SPACE 1                                                                
* REPORT LEVELS BASED ON REPORT 2 (DETAIL REPORT)                               
* FOR DAYPART VERSION OF THE PBE                                                
*                                                                               
RPTLEVS2 DC    AL1(QCLT)           HEADLINES                                    
         DC    AL1(QPRDGR1)                                                     
         DC    AL1(QPRDGR2)                                                     
         DC    AL1(QPRD)                                                        
         DC    AL1(QEST)                                                        
         DC    AL1(QMKTGR1)                                                     
         DC    AL1(QMKTGR2)                                                     
         DC    AL1(QMKTGR3)                                                     
         DC    AL1(QMKT)                                                        
         DC    AL1(QRPTSEQ)                                                     
         DC    AL1(QDPT)                                                        
         DC    AL1(QRPTSEQ)                                                     
         DC    AL1(QSTA)           MIDLINE                                      
         DC    AL1(QBUY)           DETAIL                                       
         DC    X'FF'                                                            
         EJECT                                                                  
* FURTHER REQUEST VALIDATION                                                    
*                                                                               
VALID    LA    R2,PBEMEDH                                                       
         CLC   =X'28BB',USERID     FOR MVMFI, MEDIA T ONLY                      
         BNE   *+12                                                             
         CLI   SBQMED,C'T'                                                      
         BNE   EINV                                                             
*                                                                               
         OC    SBBCLT,SBBCLT       ALL CLIENTS IS INVALID                       
         BZ    ECLT                                                             
         CLI   SBQSTA,C'0'         CABLE REQUESTS ARE INVALID                   
         BNL   ESTA                                                             
         CLC   SBQSTA(3),=C'ALL'                                                
         BNE   *+16                                                             
         CLI   SBQSTA+4,C'/'                                                    
         BE    ESTA                                                             
         B     *+14                                                             
         CLC   SBQSTA,SPACES       IF ALL STATIONS, EXCLUDE CABLE               
         BH    *+14                                                             
         MVC   SBQSTA(3),=C'ALL'                                                
         MVI   SBQSTA+4,C'-'                                                    
*                                                                               
         LA    R2,PBEBK3H          VALIDATE BOOKS FIELDS                        
         CLI   5(R2),0             TEST PREVIOS BOOK SET                        
         BNE   VAL2                                                             
*                                                                               
         LA    R1,4095(R9)                                                      
         LA    R1,1(R1)                                                         
         USING SYSD+4096,R1                                                     
         MVC   FULL,SBQBOOK        NO-3RD BOOK IS PRIOR YEAR TO THE             
*                                                                               
         CLC   SBQBOOK(4),SBQBOOK+4      LATER OF THE OTHER TWO                 
         BNL   *+10                                                             
         MVC   FULL,SBQBOOK+4                                                   
         DROP  R1                                                               
*                                                                               
         CLC   FULL(3),=C'ACT '                                                 
         BNE   VAL1                                                             
         GOTO1 ANY                 SET AN ERROR AND LEAVE                       
*                                                                               
VAL1     EQU   *                                                                
*                                                                               
*Y2K         PACK  DUB,FULL(2)                                                  
*Y2K         SP    DUB,=P'1'                                                    
*Y2K         BNM   *+10                                                         
*Y2K         ZAP   DUB,=PL2'99'                                                 
*Y2K         UNPK  FULL(2),DUB                                                  
*Y2K         OI    FULL+1,X'F0'                                                 
*Y2K         MVC   SBQBKS+4(4),FULL                                             
*                                                                               
         MVC   DUB(4),FULL         MOVE YYMM TO DUB FOR ADDAY                   
         MVC   DUB+4(2),=C'01'     FORCE DD TO 01                               
*                                                                               
* SUBTRACT ONE YEAR AND DON'T GIVE ME FUNNY (Y2K) DATES                         
*                                                                               
         GOTO1 ADDAY,DMCB,(C'Y',DUB),(X'20',DUB),F'-1'                          
         LR    R1,R9               SAVE NEW YYMM                                
         AHI   R1,SBQBOOK+8-SYSD                                                
         MVC   0(4,R1),DUB                                                      
*                                                                               
VAL2     LA    R2,PBEOPTH                                                       
         CLI   SBQPGRD,C' '        TEST PRODUCT GROUPS                          
         BNH   VALX                                                             
         TM    SBQPIND,SBQPOLAL    AND POL=BOTH                                 
         BO    EPOLO               YES-ERROR                                    
*                                                                               
VALX     B     XIT                                                              
         EJECT                                                                  
* ERROR EXITS AND MESSAGES                                                      
*                                                                               
EPOLO    MVC   CONHEAD(L'EPOLOM),EPOLOM                                         
         B     MYCURSOR                                                         
*                                                                               
EBKS     MVC   CONHEAD(L'EBKSM),EBKSM                                           
         B     MYCURSOR                                                         
*                                                                               
ENOREP   MVC   CONHEAD(L'ENOREPM),ENOREPM                                       
         B     MYCURSOR                                                         
*                                                                               
EINV     MVI   ERROR,INVALID                                                    
         B     CURSOR                                                           
*                                                                               
ECLT     MVI   ERROR,INVCLT                                                     
         LA    R2,PBECLTH                                                       
         B     CURSOR                                                           
*                                                                               
ESTA     MVI   ERROR,INVSTA                                                     
         LA    R2,PBESTAH                                                       
         B     CURSOR                                                           
*                                                                               
MYCURSOR MVI   ERROR,X'FE'                                                      
CURSOR   GOTO1 CURSERR                                                          
*                                                                               
EPOLOM   DC    C'INVALID POL OPTION'                                            
EBKSM    DC    C'**ERROR** MUST SPECIFY 3 BOOKS IN MMMYY FORMAT'                
ENOREPM  DC    C'REPORT OPTION MISSING'                                         
         EJECT                                                                  
INPUT    L     R4,AGLOBAL          ** INPUT ROUTINE **                          
         USING GLOBALD,R4                                                       
         CLI   SBMODE,SBPROCSP     HOOK MODE PASSED FROM SPOTIO                 
         BE    PROCBUY                                                          
         B     XIT                                                              
         EJECT                                                                  
* PROCESS BUY RECORD                                                            
*                                                                               
PROCBUY  L     R3,SBAIO1                                                        
         USING BUYRECD,R3                                                       
         CLI   DPTOPT,C'Y'         TEST DAYPART REPORT                          
         BNE   BY1                                                              
         GOTO1 AGETDPTB            YES-GET DAYPART TABLE                        
         GOTO1 SETDPT,BDDAYPT      AND SET DAYPART DETAILS                      
*                                                                               
BY1      XC    POLDEMOS,POLDEMOS                                                
         MVI   SBEUNALL,C'N'       MAKE SURE NO UNALLOCATED                     
         MVC   BYTE,BUYKPRD        SET THE KEY PRODUCT                          
         CLI   BUYKPRD,X'FF'       TEST PRD=POL                                 
         BNE   BY20                                                             
         CLC   =C'POL',SBQPRD      YES-TEST PRD=POL REQUEST                     
         BE    BY20                YES                                          
         MVC   BYTE,SBQBPRD        NO-SET THE REQUESTED PRODUCT                 
         CLI   BYTE,0              TEST PRD=ALL                                 
         BNE   BY20                NO                                           
         XC    PRDLST,PRDLST       YES-BUILD LIST OF PRODUCTS                   
         LA    R5,BDELEM                                                        
*                                                                               
BY2      CLI   0(R5),0             SEARCH RECORD FOR POL BUY ELEMENTS           
         BE    BY12                                                             
         CLI   0(R5),11                                                         
         BL    BY10                                                             
         CLI   0(R5),13                                                         
         BH    BY10                                                             
         USING REGELEM,R5                                                       
         TM    RSTATUS,X'04'       TEST HIATUS SPOT                             
         BO    BY10                YES-SKIP                                     
         TM    RSTATUS,X'C0'       TEST MISSED/MAKE-GOOD                        
         BNZ   BY10                YES-NO DEMOS REQUIRED                        
         CLI   RLEN,10             TEST ALLOCATED                               
         BNH   BY10                NO-SKIP                                      
         LA    R0,2                                                             
         LA    RE,RPPRD            BUILD LIST OF PRODUCTS                       
*                                                                               
BY4      CLC   SBQPGRF,SPACES      TEST PRODUCT GROUP FILTER                    
         BNH   BY6                                                              
         ICM   R1,15,SBAPRDBF      YES--                                        
         BZ    BY6                                                              
         LLC   RF,0(RE)            INDEX INTO PRODUCT BUFFER                    
         BCTR  RF,0                                                             
         MH    RF,=Y(PRDBUFFL)                                                  
         AR    R1,RF                                                            
         USING PRDBUFFD,R1                                                      
         OC    PBGROUP,PBGROUP     TEST PRODUCT IN PRODUCT GROUP                
         BZ    BY9                 NO                                           
         DROP  R1                                                               
*                                                                               
BY6      LA    R1,PRDLST                                                        
*                                                                               
BY8      CLI   0(R1),0                                                          
         BNE   *+14                                                             
         MVC   0(1,R1),0(RE)                                                    
         B     BY9                                                              
         CLC   0(1,RE),0(R1)                                                    
         BE    BY9                                                              
         LA    R1,1(R1)                                                         
         B     BY8                                                              
*                                                                               
BY9      CLI   RLEN,14             TEST PIGGYBACK BUY                           
         BNH   BY10                                                             
         CLI   SBESPLIT,C'N'       YES-TEST SPLITTING PIGGIES                   
         BE    BY10                                                             
         LA    RE,RPPRD+4          YES-ADD 2ND PRODUCT TO LIST                  
         BCT   R0,BY4                                                           
*                                                                               
BY10     LLC   R0,1(R5)            NEXT BUY ELEMENT                             
         AR    R5,R0                                                            
         B     BY2                                                              
*                                                                               
BY12     OC    PRDLST,PRDLST       TEST ANY PRODUCTS                            
         BZ    BYX                                                              
         CLI   PRDLST+1,0          YES-TEST ONLY ONE PRODUCT                    
         BNE   *+14                                                             
         MVC   BYTE,PRDLST         YES-SET THE PRODUCT                          
         B     BY20                                                             
         LA    R1,PRDLST           NO-GET THEIR DEMOS                           
*                                                                               
BY13     CLI   0(R1),0                                                          
         BE    BY19                                                             
         BAS   RE,GETDEMOS                                                      
         LLC   R0,NDEMOS                                                        
         LA    R2,DEMOS            MOVE DEMOS TO POL DEMO LIST                  
*                                                                               
BY14     CLI   1(R2),0                                                          
         BE    BY18                                                             
         LA    R5,POLDEMOS                                                      
         LA    RF,4                                                             
*                                                                               
BY15     CLI   1(R5),0                                                          
         BNE   BY16                                                             
         MVC   0(3,R5),0(R2)                                                    
         B     BY17                                                             
*                                                                               
BY16     CLC   0(3,R5),0(R2)                                                    
         BE    BY17                                                             
         LA    R5,3(R5)                                                         
         BCT   RF,BY15                                                          
         DC    H'0'                TOO MANY POL DEMOS                           
*                                                                               
BY17     LA    R2,3(R2)            NEXT DEMO                                    
         BCT   R0,BY14                                                          
*                                                                               
BY18     LLC   RE,NDEMOS                                                        
         MH    RE,=H'3'                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8              CHECK EACH PRODUCT HAS SAME DEMOS            
         B     *+10                                                             
         CLC   DEMOS(0),POLDEMOS                                                
         BE    *+6                                                              
         DC    H'0'                DIE IF NOT                                   
         LA    R1,1(R1)            NEXT PRODUCT                                 
         B     BY13                                                             
*                                                                               
BY19     LA    R5,POLDEMOS         SET NUMBER OF POL DEMOS                      
**       LA    R1,4                                                             
**       CLI   1(R5),0                                                          
**       BE    *+12                                                             
**       LA    R5,3(R5)                                                         
**       BCT   R1,*-12                                                          
**       SH    R1,=H'4'                                                         
**       LPR   R1,R1                                                            
**       STC   R1,NPOLDEMS                                                      
         MVC   NPOLDEMS,NDEMOS     ASSUMES EACH PRODUCT HAS SAME DEMOS          
         B     BY21                                                             
*                                                                               
BY20     CLC   BYTE,SVPRD          TEST PRD/EST CHANGE                          
         BNE   *+14                                                             
         CLC   BUYKEST,SVEST                                                    
         BE    BY28                                                             
         MVC   SVPRD,BYTE          YES-SET DEMO LIST                            
         MVC   SVEST,BUYKEST                                                    
         LA    R1,SVPRD                                                         
         BAS   RE,GETDEMOS                                                      
*                                                                               
BY21     MVC   DUB(6),=C'RSPIXQ'   SET DEMOS LIST RATINGS/SHARES/PUTS           
         LA    R2,3                                                             
         LA    R5,DUB                                                           
         LA    RE,SBEDEMOS                                                      
*                                                                               
BY22     LLC   R0,NDEMOS                                                        
         LA    RF,DEMOS                                                         
         OC    POLDEMOS,POLDEMOS                                                
         BZ    BY24                                                             
         LLC   R0,NPOLDEMS                                                      
         LA    RF,POLDEMOS                                                      
*                                                                               
BY24     CLI   1(RF),0             TEST NO DEMO HERE                            
         BE    BY25                                                             
         MVC   0(3,RE),0(RF)                                                    
         MVC   1(1,RE),0(R5)                                                    
         CLI   1(RF),C'R'                                                       
         BE    BY26                                                             
         CLI   1(RF),C'E'                                                       
         BE    BY26                                                             
         MVC   1(1,RE),3(R5)                                                    
         CLI   1(RF),C'I'                                                       
         BE    BY26                                                             
******** CLI   1(RF),X'21'         TEST USER DEMO                               
******** BE    BY25                                                             
******** DC    H'0'                                                             
*                                                                               
BY25     XC    0(3,RE),0(RE)       PUT DUMMY DEMO                               
         MVI   1(RE),C'R'                                                       
*                                                                               
BY26     LA    RE,3(RE)                                                         
         LA    RF,3(RF)                                                         
         BCT   R0,BY24                                                          
         LA    R5,1(R5)                                                         
         BCT   R2,BY22                                                          
         LLC   RE,NDEMOS           SET TOTAL NUMBER OF DEMOS                    
         OC    POLDEMOS,POLDEMOS                                                
         BZ    *+8                                                              
         IC    RE,NPOLDEMS                                                      
         MH    RE,=H'3'                                                         
         STC   RE,SBENDEM                                                       
*                                                                               
BY28     CLI   AFFIDS,C'Y'         TEST AFFID DETAIL RPT                        
         BNE   BYX                                                              
         MVI   GLOPTS+4,C'Y'       YES-TELL DRIVER                              
         CLI   SINGRPT,0                                                        
         BE    *+8                                                              
         MVI   GLOPTS+4,0                                                       
         L     R3,SBAIO1                                                        
         USING BUYREC,R3                                                        
         MVI   MKTIND,FF                                                        
         MVC   SBEMKT,SBBMKT       EXTRACT MARKET IN DIRECTORY POINTER          
         CLI   SBQSPILL,C'N'       TEST SPILL REQUESTED                         
         BNE   BY30                                                             
         CLC   SBBMKT,BUYMSTA      NO - REJECT SPILL MARKET                     
         BNE   BYX                                                              
         B     BY32                                                             
*                                  YES -                                        
BY30     MVC   SBBMKT,BUYMSTA      SET MARKET FOR SPOTBUY                       
         CLI   SBQSPILL,C'C'       TEST COMBINED ORIG + SPILL                   
         BE    BY32                                                             
         CLI   SBQSPILL,C'S'       TEST SEPERATE ORIG + SPILL                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   MKTIND,C'O'         YES -                                        
         CLC   SBEMKT,SBBMKT       TEST SPILL OR ORIG MARKET                    
         BE    BY32                                                             
         MVI   MKTIND,C'S'                                                      
*                                                                               
BY32     MVI   SBBPRD,0                                                         
         MVI   SBBPRD2,0                                                        
         XC    SBPRD,SBPRD                                                      
         XC    SBPRD2,SBPRD2                                                    
         MVI   SBEDEMTY,C'A'       AFFID LOOKUP                                 
         XC    SBSPHOOK,SBSPHOOK                                                
*                                                                               
         LA    RF,SBLOCK                                                        
         AHI   RF,SBEFLAG2-SBLOCK                                               
         OI    0(RF),SBESPLBY      SPLIT BUYS                                   
*                                                                               
BY33     GOTO1 SPOTBUY,DMCB,SBLOCK     ** CALL SPOTBUY **                       
         L     R2,SBASPTTB         R2=A(SPOT TABLE)                             
         L     R3,SBNSPTEN         R3=N'ENTRIES                                 
         L     R5,SBLSPTEN         R5=L'ENTRY                                   
         LTR   R3,R3                                                            
         BZ    BYX                                                              
         USING SPTTABD,R2                                                       
*                                                                               
BY34     TM    SPTIND,SPTDUMMY     TEST DUMMY ENTRY                             
         BO    BY40                                                             
         OC    SPTADATE(4),SPTADATE     TEST AFFID DATE/TIME                    
         BZ    BY40                                                             
         CLI   SBEPRD,0            YES-TEST PRODUCT FILTER                      
         BE    *+14                                                             
         CLC   SPTPRD1,SBEPRD      YES-CHECK THE PRODUCT                        
         BNE   BY40                                                             
         CLI   SBQBPRD,X'FF'       TEST POL REQUEST                             
         BNE   BY35                                                             
         CLI   SBBPRD,X'FF'        YES-TEST POL DETAILS SET YET                 
         BE    BY38                                                             
         MVI   SBBPRD,X'FF'                                                     
         B     BY36                                                             
*                                                                               
BY35     CLC   SPTPRD1,SBBPRD      TEST CHANGE OF PRODUCT                       
         BE    BY37                                                             
         MVC   SBBPRD,SPTPRD1      YES-SET PRODUCT DETAILS                      
*                                                                               
BY36     LLC   RE,SBBPRD                                                        
         BCTR  RE,0                                                             
         MH    RE,=Y(PRDBUFFL)                                                  
         L     R1,SBAPRDBF                                                      
         AR    R1,RE                                                            
         USING PRDBUFFD,R1                                                      
         MVC   SBPRD,PBALPH                                                     
         MVC   SBPRDNM,PBNAME                                                   
         MVC   SBBPGR,PBGROUP      SET PRODUCT GROUP                            
         OC    SBBPGR,SBBPGR                                                    
         BNZ   BY37                                                             
         MVC   SBBPGR,=X'9999'                                                  
*                                                                               
BY37     CLI   SBQBPRD,X'FF'       IF NOT POL REQUEST,                          
         BE    BY38                                                             
         CLC   SPTPRD2,SBBPRD2     TEST CHANGE OF SECOND PRD                    
         BE    BY38                                                             
         MVC   SBBPRD2,SPTPRD2     YES                                          
         XC    SBPRD2,SBPRD2                                                    
         CLI   SBBPRD2,0                                                        
         BE    BY38                                                             
         LLC   RE,SBBPRD2                                                       
         BCTR  RE,0                                                             
         MH    RE,=Y(PRDBUFFL)                                                  
         L     R1,SBAPRDBF                                                      
         AR    R1,RE                                                            
         USING PRDBUFFD,R1                                                      
         MVC   SBPRD2,PBALPH                                                    
         DROP  R1                                                               
*                                                                               
BY38     CLI   MORDER,C'R'         TEST MARKETS SHOULD BE RANKED                
         BNE   BY39                                                             
         GOTO1 SPOTMKRK,DMCB,SBLOCK     YES-GET MARKET'S RANK                   
*                                                                               
BY39     ST    R2,SBACURCH         SBACURCH=A(SPOT TABLE ENTRY)                 
         BAS   RE,DRIVIN           CALL DRIVER FOR INPUT                        
*                                                                               
BY40     LA    R2,0(R5,R2)                                                      
         BCT   R3,BY34                                                          
*                                                                               
         LA    RF,SBLOCK                                                        
         AHI   RF,SBACONT-SBLOCK                                                
         OC    0(4,RF),0(RF)       ANY BUY CONTINUATION?                        
         BNZ   BY33                 YES - GO GET THE REST                       
*                                                                               
BYX      MVI   GLOPTS+4,C'N'       TELL DRIVER NOT AFFIDS REPORT NOW            
         CLI   SINGRPT,0                                                        
         BE    XIT                                                              
         MVI   GLOPTS+4,0                                                       
         B     XIT                                                              
         DROP  R2                                                               
         SPACE 2                                                                
*                                                                               
GETDEMOS LR    R0,RE                                                            
         LLC   RE,0(R1)                                                         
         BCTR  RE,0                                                             
         SLL   RE,8                                                             
         LLC   RF,BUYKEST                                                       
         BCTR  RF,0                                                             
         AR    RE,RF                                                            
         A     RE,SBAESTTB                                                      
         SR    R1,R1                                                            
         ICM   R1,1,0(RE)          POINTER TO ESTIMATE BUFFER ENTRY             
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  R1,0                                                             
         MH    R1,=Y(ESTBUFFL)                                                  
         A     R1,SBAESTBF                                                      
         USING ESTBUFFD,R1                                                      
         MVC   DEMOS,EBDEMOS                                                    
         LR    RE,R0                                                            
         BR    RE                                                               
         DROP  R1,R3                                                            
         EJECT                                                                  
* DRIVER INPUT ROUTINE                                                          
*                                                                               
         USING GLOBALD,R4                                                       
DRIVIN   LR    R0,RE                                                            
         MVI   GLMODE,GLINPUT                                                   
         BAS   RE,GODRIVER                                                      
         CLI   MKTIND,C'O'         TEST ORIGINATING MARKET                      
         BE    *+12                                                             
         CLI   MKTIND,C'S'         OR SPILL MARKET                              
         BNE   DRIVINX                                                          
         MVC   SVMKTIND,MKTIND                                                  
         MVC   SVMAXTLV,MAXTOTLV                                                
         MVI   MKTIND,FF           SET COMBINED MARKET                          
         CLC   MKTLEV,MAXTOTLV     YES-                                         
         BNH   *+10                                                             
         MVC   MAXTOTLV,MKTLEV     DO NOT GENERATE ANY TOTALS                   
         BAS   RE,GODRIVER         CALL DRIVER FOR COMBINED MARKET              
         MVC   MKTIND,SVMKTIND                                                  
         MVC   MAXTOTLV,SVMAXTLV                                                
*                                                                               
DRIVINX  LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
* DRIVER HOOK                                                                   
*                                                                               
DRHOOK   L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         CLI   GLHOOK,GLINIT       INITIALIZATION                               
         BE    DRVINIT                                                          
         CLI   GLHOOK,GLRESOLV     RESOLVE ADDRESSES                            
         BE    RESOLVE                                                          
         CLI   GLHOOK,GLROUT       EXECUTE ROUTINES                             
         BE    EXEC                                                             
         CLI   GLHOOK,GLINCOMP     INTERNAL COMPUTES                            
         BE    INTCOMP                                                          
         CLI   GLHOOK,GLPRINT      PRINT A LINE                                 
         BE    PRINT                                                            
         CLI   GLHOOK,GLFIRST      FIRSTS                                       
         BE    FIRSTS                                                           
         CLI   GLHOOK,GLLAST       LASTS                                        
         BE    LASTS                                                            
         CLI   GLHOOK,GLHEAD       HEADHOOK                                     
         BE    HEAD                                                             
*                                                                               
DRHOOKX  B     XIT                                                              
         EJECT                                                                  
* DRIVER INITIALIZATION                                                         
*                                                                               
DRVINIT  MVC   GLOPTS+2(1),DATAOPT     SET DATA OPTION                          
         MVC   GLOPTS+3(1),COMPARE     SET COMPARE OPTION                       
         CLI   WIDTHOPT,C'W'                                                    
         BNE   *+8                                                              
         MVI   GLOPTS+5,C'Y'           SET REPORT WIDTH (Y=WIDE)                
         MVC   GLOPTS+6(1),SINGRPT     DOWNLOAD REPORT OPTION                   
         MVC   GLOPTS+7(1),INDEXOPT    SET INDEX OPTION                         
         MVC   GLOPTS+8(1),MORDER      SET MARKETS RANKED OPTION                
         OC    SBQUPGRD,SBQUPGRD       SET UPGRADE INDICATOR                    
         BZ    *+8                                                              
         MVI   GLOPTS+9,C'Y'                                                    
         MVC   GLOPTS+10(1),COSTOPT    SET BUY COST OPTION                      
         MVC   GLOPTS+11(1),DPTOPT     SET DAYPART DETAIL OPTION                
         B     XIT                                                              
         EJECT                                                                  
* DRIVER HOOK ROUTINE TO RESOLVE ADDRESSES                                      
*                                                                               
RESOLVE  LA    R1,RTNLIST          SEARCH LIST FOR ROUTINE NAME                 
*                                                                               
RESOLVE2 CLI   0(R1),FF                                                         
         BE    XIT                                                              
         CLC   0(8,R1),GLLABEL                                                  
         BE    *+12                                                             
         LA    R1,12(R1)                                                        
         B     RESOLVE2                                                         
         MVC   GLAROUT,8(R1)       YES-RETURN ADDRESS                           
         B     XIT                                                              
         SPACE 1                                                                
RTNLIST  DS    0F                                                               
         DC    CL8'ODPTUSR ',A(ODPT)                                            
         DC    CL8'ISPTORD ',A(ISPTORD)                                         
         DC    CL8'OSPTRUN ',A(OSPT)                                            
         DC    CL8'OMGR1USR',A(OMGR1)                                           
         DC    CL8'OMGR2USR',A(OMGR2)                                           
         DC    CL8'OMGR3USR',A(OMGR3)                                           
         DC    CL8'OMKTUSR1',A(OMKT1)                                           
         DC    CL8'OMKTUSR2',A(OMKT2)                                           
         DC    CL8'OBOOKUSR',A(OBOOK)                                           
         DC    CL8'IAFFID  ',A(IAFF)                                            
         DC    CL8'OAFFID  ',A(OAFF)                                            
         DC    CL8'ODOLUSR ',A(ODOL)                                            
         DC    CL8'IDEMOUSR',A(IDEMO)                                           
         DC    CL8'ODEMOUSR',A(ODEMO)                                           
         DC    CL8'OTOL    ',A(OTOL)                                            
         DC    CL8'OINDEX  ',A(OINDEX)                                          
         DC    CL8'HDEMUSR ',A(HDEM)                                            
         DC    X'FF'                                                            
         EJECT                                                                  
* INTERNAL COMPUTES HOOK                                                        
*                                                                               
INTCOMP  CLC   GLLABEL,=CL8'ODOLUSR'                                            
         BE    EXEC                                                             
         B     XIT                                                              
         EJECT                                                                  
* DRIVER HOOK TO EXECUTE ROUTINES                                               
*                                                                               
EXEC     L     R2,GLAIFLD          R2=A(INPUT)                                  
         L     R3,GLAOFLD          R3=A(OUTPUT)                                 
         L     RF,GLAROUT          BRANCH TO ROUTINE                            
         BR    RF                                                               
         EJECT                                                                  
* DAYPART OUTPUT ROUTINE                                                        
*                                                                               
ODPT     LA    R4,DPTHEAD          FORMAT DPT HEAD TO TEMP SAVE AREA            
         MVC   DPTHEAD,SPACES                                                   
         MVC   0(11,R4),=C'*** DAYPART'                                         
         MVC   12(3,R4),5(R2)                                                   
         MVC   16(3,R4),=C'***'                                                 
         B     XIT                                                              
         SPACE 2                                                                
* ORDERED SPOTS INPUT ROUTINE                                                   
*                                                                               
ISPTORD  XC    0(4,R2),0(R2)                                                    
         CLI   SBMODE,SBPROCSP     TEST PROCESSING BUY RECORD                   
         BNE   ISORDX                                                           
         CLI   SBEDEMTY,C'A'       TEST FIRST TIME THROUGH THE CHUNKS           
         BNE   ISORDX                                                           
         L     R1,SBAIO1                                                        
         CLC   SVBUYKEY,0(R1)                                                   
         BE    ISORDX                                                           
         MVC   SVBUYKEY,0(R1)                                                   
         L     R3,SBACURCH         A(CURRENT CHUNK)                             
         USING SCHUNKD,R3                                                       
         ICM   RE,15,SCSPOTS       N'RUN SPOTS                                  
         L     R5,SBAIO1                                                        
         USING BUYREC,R5                                                        
*                                                                               
ISORD4   LA    R1,BDELEM           SCAN BUY RECORD FOR POOL BUY ELEMS           
         SR    R0,R0                                                            
*                                                                               
ISORD6   CLI   0(R1),0                                                          
         BE    ISORD10                                                          
         CLI   0(R1),X'06'         TEST BUY ORIGINAL ELEMENT                    
         BE    *+12                                                             
         CLI   0(R1),X'0B'                                                      
         BNE   ISORD8                                                           
         USING REGELEM,R1          YES-                                         
         TM    RSTATUS,X'42'       TEST SPOT IS MINUSED AND MADEGOOD            
         BNO   ISORD8                                                           
         CLI   SCPRD1,X'FF'        YES-TEST CHUNK PRD IS POOL                   
         BE    ISORD7                  YES                                      
         CLI   0(R1),X'0B'         TEST POOL BUY                                
         BNE   ISORD7              NO                                           
         CLI   SCPRD1,X'FE'        YES-TEST CHUNK PRD IS UNALLOCATED            
         BNE   *+16                                                             
         CLI   RLEN,10             YES-CHECK BUY ELEM IS UNALLOCATED            
         BE    ISORD7                                                           
         B     ISORD8                                                           
         CLC   SCPRD1,RPPRD        MATCH FIRST PRODUCT                          
         BNE   ISORD8                                                           
         CLI   SCPRD2,0            TEST SECOND CHUNK PRODUCT                    
         BNE   *+16                                                             
         CLI   RLEN,14             NO-TEST SINGLE PRD ELEMENT                   
         BE    ISORD7                                                           
         B     ISORD8                                                           
         CLI   RLEN,14             YES-TEST SINGLE PRD ELEMENT                  
         BNH   ISORD8                                                           
         CLC   SCPRD2,RPPRD+4      MATCH SECOND PRODUCT                         
         BNE   ISORD8                                                           
*                                                                               
ISORD7   LA    RE,1(RE)            ADD MADEGOOD SPOT TO TOTAL                   
*                                                                               
ISORD8   IC    R0,1(R1)            SCAN ALL BUY ELEMENTS                        
         AR    R1,R0                                                            
         B     ISORD6                                                           
         DROP  R1                                                               
*                                                                               
ISORD10  STCM  RE,15,0(R2)         MOVE TOTAL ORDERED SPOTS TO DRIVER           
         LTR   RE,RE               INPUT AREA                                   
         BZ    ISORDX                                                           
         MVI   INDATA,1            INDICATE SIGNIFICANT COLUMN DATA             
*                                                                               
ISORDX   B     XIT                                                              
         DROP  R3,R5                                                            
         EJECT                                                                  
* AFFIDAVIT ROUINES FOR AFFID DETAIL REPORT                                     
*                                                                               
IAFF     L     R5,SBACURCH                                                      
         USING SPTTABD,R5                                                       
         MVC   0(2,R2),SPTADATE                                                 
         MVC   2(2,R2),SPTATIME                                                 
         B     XIT                                                              
         DROP  R5                                                               
*                                                                               
OAFF     GOTO1 DATCON,DMCB,(2,0(R2)),(4,0(R3))                                  
         GOTO1 DATCON,DMCB,(2,0(R2)),(0,WORK)                                   
         GOTO1 GETDAY,DMCB,WORK,6(R3)                                           
         CLC   6(3,R3),SPACES                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   WORK(2),2(R2)                                                    
         XC    WORK+2(2),WORK+2                                                 
         GOTO1 UNTIME,DMCB,WORK,10(R3)                                          
         B     XIT                                                              
         EJECT                                                                  
* DOLLAR OUTPUT ROUTINE                                                         
*                                                                               
ODOL     EDIT  (4,0(R2)),(8,0(R3)),MINUS=YES                                    
         B     XIT                                                              
         SPACE 2                                                                
* DEMO INPUT ROUTINE FOR AFFID DETAIL REPORT                                    
*                                                                               
IDEMO    L     R5,SBACURCH                                                      
         USING SPTTABD,R5                                                       
         LA    RE,SPTDEMOS                                                      
         SR    RF,RF                                                            
         CLI   GLARGS+1,C'R'                                                    
         BE    IDEMO2                                                           
         IC    RF,NDEMOS                                                        
         SLL   RF,2                                                             
         CLI   GLARGS+1,C'S'                                                    
         BE    IDEMO2                                                           
         SLL   RF,1                                                             
         CLI   GLARGS+1,C'P'                                                    
         BE    IDEMO2                                                           
         DC    H'0'                                                             
*                                                                               
IDEMO2   LA    RE,SPTDEMOS(RF)                                                  
         LLC   R1,GLARGS                                                        
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         AR    R1,RE                                                            
         MVC   0(4,R2),0(R1)                                                    
         OC    0(4,R2),0(R2)       TEST SIGNIFICANT DATA                        
         BZ    *+8                                                              
         MVI   INDATA,1            YES                                          
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
* MARKET GROUP OUTPUT ROUTINES                                                  
*                                                                               
OMGR1    LA    R4,MGR1HEAD         FORMAT MGR1 HEAD TO TEMP SAVE AREA           
         MVC   0(12,R4),SBMGR1BK                                                
**NOP         MVC   13(1,R4),SBQMGRD                                            
         BRAS  RE,MGRCODE                                                       
         LA    R1,SBMGR1LN                                                      
         BAS   RE,OGRPCODE                                                      
         CLC   19(L'UNKNOWN,R4),UNKNOWN                                         
         BE    XIT                                                              
         MVC   SBBMGR,0(R2)                                                     
         GOTO1 GETMGRNM,SBMGR1NM                                                
         MVC   19(24,R4),SBMGR1NM                                               
         B     XIT                                                              
*                                                                               
OMGR2    LA    R4,MGR2HEAD         FORMAT MGR2 HEAD TO TEMP SAVE AREA           
         MVC   0(12,R4),SBMGR2BK                                                
**NOP         MVC   13(1,R4),SBQMGRD                                            
         BRAS  RE,MGRCODE                                                       
         LA    R1,SBMGR2LN                                                      
         BAS   RE,OGRPCODE                                                      
         CLC   19(L'UNKNOWN,R4),UNKNOWN                                         
         BE    XIT                                                              
         MVC   SBBMGR,0(R2)                                                     
         GOTO1 GETMGRNM,SBMGR2NM                                                
         MVC   19(24,R4),SBMGR2NM                                               
         B     XIT                                                              
*                                                                               
OMGR3    LA    R4,MGR3HEAD         FORMAT MGR3 HEAD TO TEMP SAVE AREA           
         MVC   0(12,R4),SBMGR3BK                                                
**NOP         MVC   13(1,R4),SBQMGRD                                            
         BRAS  RE,MGRCODE                                                       
         LA    R1,SBMGR3LN                                                      
         BAS   RE,OGRPCODE                                                      
         CLC   19(L'UNKNOWN,R4),UNKNOWN                                         
         BE    XIT                                                              
         MVC   SBBMGR,0(R2)                                                     
         GOTO1 GETMGRNM,SBMGR3NM                                                
         MVC   19(24,R4),SBMGR3NM                                               
         B     XIT                                                              
*                                                                               
OGRPCODE UNPK  DUB(5),0(3,R2)                                                   
         LLC   RF,0(R1)                                                         
         BCTR  RF,0                                                             
         LA    R1,14(R4)                                                        
         CLI   0(R1),C' '                                                       
         BNH   *+8                                                              
         AHI   R1,1                                                             
         EX    RF,OGRPMVC                                                       
         EX    RF,OGRPCLC                                                       
         BNE   *+10                                                             
         MVC   19(L'UNKNOWN,R4),UNKNOWN                                         
         BR    RE                                                               
*                                                                               
OGRPMVC  MVC   0(0,R1),DUB                                                      
OGRPCLC  CLC   0(0,R1),=C'9999'                                                 
UNKNOWN  DC    C'** UNKNOWN **'                                                 
         EJECT                                                                  
* MARKET OUTPUT ROUTINE FOR SUMMARY REPORT                                      
*                                                                               
OMKT1    MVC   SBBMKT,0(R2)                                                     
         EDIT  SBBMKT,(4,SBMKT),FILL=0                                          
         GOTO1 GETMKTNM                                                         
         MVC   0(4,R3),SBMKT                                                    
         MVC   5(14,R3),SBMKTNM                                                 
         CLI   WIDTHOPT,C'W'                                                    
         BNE   XIT                                                              
         MVC   5(L'SBMKTNM,R3),SBMKTNM                                          
         B     XIT                                                              
         SPACE 2                                                                
* MARKET OUTPUT ROUTINE FOR DETAIL REPORT                                       
*                                                                               
OMKT2    LA    R4,MKTHEAD          FORMAT MKT HEAD TO TEMP SAVE AREA            
         MVC   MKTHEAD,SPACES                                                   
         MVC   0(6,R4),=C'MARKET'                                               
         CLI   MORDER,C'R'         TEST MARKETS RANKED                          
         BNE   *+8                                                              
         LA    R2,2(R2)            YES-SKIP PAST RANK NUMBER                    
         CLI   MORDER,C'A'         TEST MARKETS IN ALPHA ORDER                  
         BNE   *+8                                                              
         LA    R2,8(R2)            YES-SKIP PAST 8-CHAR MKT NAME                
         MVC   SBBMKT,0(R2)                                                     
         EDIT  SBBMKT,(4,SBMKT),FILL=0                                          
         MVC   7(4,R4),SBMKT                                                    
         GOTO1 GETMKTNM                                                         
         MVC   12(L'SBMKTNM,R4),SBMKTNM                                         
         CLI   2(R2),C'O'                                                       
         BNE   *+10                                                             
         MVC   12+1+L'SBMKTNM(6,R4),=C'*ORIG*'                                  
         CLI   2(R2),C'S'                                                       
         BNE   *+10                                                             
         MVC   12+1+L'SBMKTNM(7,R4),=C'*SPILL*'                                 
         LA    R1,L'MKTHEAD                                                     
         ST    R1,DMCB+4                                                        
         GOTO1 SQUASHER,DMCB,(R4)                                               
         B     XIT                                                              
         EJECT                                                                  
* BOOK OUTPUT ROUTINE                                                           
*                                                                               
OBOOK    MVC   0(5,R3),=C'ESTIM'                                                
         LR    R1,R9                                                            
         AHI   R1,SBQBOOK-SYSD                                                  
         MVC   WORK(4),8(R1)                                                    
         MVC   WORK+4(4),4(R1)                                                  
         MVC   WORK+8(4),0(R1)                                                  
         LA    R0,3                                                             
         LR    R2,R3                                                            
         LA    R5,WORK                                                          
*                                                                               
OBOOK2   LA    R2,198(R2)                                                       
         MVC   0(3,R2),0(R5)                                                    
         CLC   0(3,R5),=C'ACT'                                                  
         BNE   OBOOK3                                                           
         TM    3(R5),X'80'         TEST ACT/YY                                  
         BZ    OBOOK4                                                           
         LLC   RE,3(R5)                                                         
         SH    RE,=H'128'                                                       
*Y2K         CVD   RE,DUB                                                       
*Y2K         OI    DUB+7,X'0F'                                                  
*Y2K         UNPK  4(2,R2),DUB                                                  
         MVI   3(R2),C'/'                                                       
*                                                                               
         STC   RE,FULL             MOVE YEAR TO FULL                            
         MVC   FULL+1(2),=X'0101'  DUMMY UP MMDD                                
         GOTO1 DATCON,DMCB,(3,FULL),(X'20',DUB) CONVERT TO EBCDIC               
         MVC   4(2,R2),DUB         MOVE OUT YEAR                                
         B     OBOOK4                                                           
*                                                                               
OBOOK3   OC    0(4,R5),0(R5)       TEST BOOK IS PRESENT                         
         BNZ   *+14                                                             
         MVC   0(5,R2),=C'UPGRD'   NO-THEN IT'S AN UPGRADE                      
         B     OBOOK4                                                           
         MVC   DUB(4),0(R5)                                                     
         MVC   DUB+4(2),=C'01'                                                  
         GOTO1 DATCON,DMCB,(0,DUB),(6,(R2))                                     
*                                                                               
OBOOK4   LA    R5,4(R5)                                                         
         BCT   R0,OBOOK2                                                        
         LA    R2,198(R2)                                                       
         MVC   0(5,R2),=C'AFFID'                                                
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
* SPOTS RUN OUTPUT ROUTINE                                                      
*                                                                               
OSPT     MVC   NSPOTS,0(R2)                                                     
         EDIT  NSPOTS,(5,(R3))                                                  
         B     XIT                                                              
         SPACE 2                                                                
* DEMO OUTPUT ROUTINE                                                           
*                                                                               
ODEMO    ICM   RF,15,0(R2)                                                      
         BZ    ODEMO4                                                           
         CLI   GLARGS+2,C'B'       TEST BONUS RATINGS                           
         BE    ODEMO2              YES-DON'T UNWEIGHT                           
         CLI   GLRECNO,2           FOR MARKET DETAIL REPORT,                    
         BNE   ODEMO2                                                           
         TM    GLINDS,GLTOTLIN     AND NOT TOTAL LINE                           
         BO    ODEMO2                                                           
         CLC   NSPOTS,=F'1'        TEST N'SPOTS = 1                             
         BE    ODEMO2                                                           
         OC    NSPOTS,NSPOTS                                                    
         BZ    ODEMO2                                                           
         SR    RE,RE               NO-UNWEIGHT DEMO BY N'SPOTS                  
         SLL   RF,1                                                             
         D     RE,NSPOTS                                                        
         A     RF,=F'1'                                                         
         SRL   RF,1                                                             
*                                                                               
ODEMO2   BRAS  RE,DEMROUND                                                      
*                                                                               
ODEMO4   OC    SBQUPGRD,SBQUPGRD   TEST FOR UPGRADE FORMULA                     
         BZ    XIT                                                              
         CLI   GLRECNO,2           AND IT'S THE MARKET DETAIL RPT               
         BNE   XIT                                                              
         CLI   VAROPT,C'Y'         AND VARIANCE REQUESTED                       
         BNE   XIT                                                              
         L     R5,GLADTENT         YES-                                         
         USING DROD,R5                                                          
         LA    R1,ESTDEM           SAVE ESTIMATED DEMOS                         
         CLC   DROENTN,=CL8'BYPDEM'                                             
         BE    ODEMO6                                                           
         LA    R1,ESTDEM2                                                       
         CLC   DROENTN,=CL8'BYPDEM2'                                            
         BE    ODEMO6                                                           
         LA    R1,UPGDEM           SAVE UPGRADE DEMOS                           
         CLC   DROENTN,=CL8'BYUDEM'                                             
         BE    ODEMO6                                                           
         LA    R1,UPGDEM2                                                       
         CLC   DROENTN,=CL8'BYUDEM2'                                            
         BNE   ODEMO8                                                           
*                                                                               
ODEMO6   ST    RF,0(R1)                                                         
         B     XIT                                                              
*                                                                               
ODEMO8   L     R1,UPGDEM                                                        
         L     RF,ESTDEM                                                        
         CLC   DROENTN,=CL8'BYUSHR'                                             
         BE    ODEMO10                                                          
         L     R1,UPGDEM2                                                       
         L     RF,ESTDEM2                                                       
         CLC   DROENTN,=CL8'BYUSHR2'                                            
         BNE   XIT                                                              
         DROP  R5                                                               
*                                                                               
ODEMO10  LA    R5,0(R3)            PRINT UPG/EST AS A PERCENTAGE                
         SH    R5,=H'198'          ABOVE UPGRADE SHARE                          
         LTR   RF,RF                                                            
         BNZ   *+14                                                             
         MVC   3(4,R5),=C'0.0%'                                                 
         B     XIT                                                              
         LTR   R1,R1                                                            
         BNZ   *+14                                                             
         MVC   3(4,R5),=C'HIGH'                                                 
         B     XIT                                                              
         M     RE,=F'2000'                                                      
         DR    RE,R1                                                            
         LTR   RE,RE                                                            
         BM    *+8                                                              
         SRA   RF,1                                                             
         EDIT  (RF),(7,(R5)),1,TRAIL=C'%'                                       
         B     XIT                                                              
         EJECT                                                                  
* TOLERANCE MIDLINE ROUTINE FOR MARKET SUMMARY REPORT                           
*                                                                               
OTOL     L     RE,GLADTENT         DETERMINE TOLERANCE                          
         L     RE,DROIADD-DROD(RE)                                              
         LH    RE,DRINDISP-DRIND(RE)                                            
         LR    R1,R2                                                            
         SR    R1,RE               R1=A(RECORD)                                 
         L     RE,LGRPS                                                         
         AR    R1,RE                                                            
         CLI   DPTOPT,C'Y'         TEST DAYPART REPORT                          
         BNE   OTOL1                                                            
         LA    R1,8(R1)            YES-ADD IN L'DPT ROW                         
         CLI   MORDER,C'R'         TEST MARKETS RANKED                          
         BNE   *+8                                                              
         LA    R1,2(R1)            YES-ADD 2 FOR MKT RANK NUMBER                
         CLI   MORDER,C'A'         TEST MARKETS RANKED                          
         BNE   *+8                                                              
         LA    R1,8(R1)            YES-ADD 8 FOR MKT NAME                       
*                                                                               
OTOL1    LA    R5,DACTDEM(R1)      COMPUTE THE INDEX                            
         SR    RE,RE                                                            
         L     RF,0(R5)                                                         
         M     RE,=F'2000'                                                      
         LA    R5,DGOLDEM(R1)                                                   
         CLI   COMPARE,C'G'                                                     
         BE    OTOL4                                                            
         CLI   COMPARE,C'P'                                                     
         BE    OTOL2                                                            
         CLI   COMPARE,C'O'                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   DATAOPT,C'2'                                                     
         BE    OTOL4                                                            
*                                                                               
OTOL2    LA    R5,DPURDEM(R1)                                                   
*                                                                               
OTOL4    L     R1,0(R5)                                                         
         LTR   R1,R1                                                            
         BNZ   *+10                                                             
         SR    RF,RF                                                            
         B     OTOL6                                                            
         DR    RE,R1                                                            
         LA    RF,1(RF)                                                         
         SRA   RF,1                                                             
*                                                                               
OTOL6    L     RE,HILIM                                                         
         MVC   0(16,R3),=CL16'ABOVE TOLERANCE'                                  
         CR    RF,RE               COMPARE INDEX TO HIGH TOLERANCE              
         BH    XIT                                                              
         L     RE,LOLIM                                                         
         MVC   0(16,R3),=CL16'BELOW TOLERANCE'                                  
         CR    RF,RE               COMPARE INDEX TO LOW TOLERANCE               
         BL    XIT                                                              
         MVC   0(16,R3),=CL16'WITHIN TOLERANCE'                                 
         B     XIT                                                              
         EJECT                                                                  
* INDEX OUTPUT ROUTINE                                                          
*                                                                               
OINDEX   ZAP   DUB,0(8,R2)         ROUND OUT THE DECIMAL PLACE                  
         CVB   R0,DUB                                                           
         SRDA  R0,32                                                            
         SLDA  R0,1                                                             
         D     R0,=F'10'                                                        
         LTR   R1,R1                                                            
         BM    *+8                                                              
         LA    R1,1(R1)                                                         
         SRA   R1,1                                                             
         CVD   R1,DUB                                                           
         MVC   0(8,R2),DUB                                                      
         MVI   GLHOOK,GLEDIT                                                    
         B     XIT                                                              
         EJECT                                                                  
* DEMO HEADLINE ROUTINE                                                         
*                                                                               
HDEM     DS    0H                                                               
         GOTO1 AHDEM                                                            
         B     XIT                                                              
         EJECT                                                                  
* PRINT A LINE                                                                  
*                                                                               
PRINT    CLI   SUPPRESS,C'Y'       TEST SUPPRESS PRINT                          
         BNE   XIT                                                              
         MVI   GLHOOK,GLDONT                                                    
         B     XIT                                                              
         EJECT                                                                  
* DRIVER HOOK FIRSTS                                                            
*                                                                               
FIRSTS   OI    DATAIND3,DIBOOK     MAKE SURE THE DEMO HEADLINE ROUTINES         
*                                  ARE NOT BOOK SENSITIVE                       
         CLI   GLRECNO,1           TEST FIRST REPORT (MARKET SUMMARY)           
         BNE   FIRSTS2                                                          
         MVI   SUPPRESS,C'N'       YES-PRINT EVERY LINE                         
         MVI   GLSPACE,1                                                        
         CLI   DPTOPT,C'Y'         TEST DAYPART REPORT AND MARKET FIRST         
         BNE   XIT                                                              
         CLC   MKTLEV,GLARGS                                                    
         BE    FMTHD1              YES-FORMAT HEADLINE REPORT                   
         B     XIT                                                              
*                                                                               
FIRSTS2  CLI   GLRECNO,2           TEST MARKET DETAIL                           
         BNE   FIRSTS3                                                          
         MVI   GLSPACE,2                                                        
         CLI   DPTOPT,C'Y'         TEST DAYPART REPORT                          
         BNE   *+18                                                             
         CLC   DPTLEV,GLARGS       AND DAYPART FIRST                            
         BE    FMTHD2              YES-FORMAT HEADLINE REPORT                   
         B     XIT                                                              
         CLC   MKTLEV,GLARGS       ELSE, TEST MARKET FIRST                      
         BE    FMTHD2              YES-FORMAT HEADLINE REPORT                   
         B     XIT                                                              
*                                                                               
FIRSTS3  CLI   GLRECNO,3           AFFID REPORT                                 
         BNE   XIT                                                              
         MVI   GLSPACE,1                                                        
         CLI   WITHIN,C'Y'         TEST MARKET WITHIN TOLERANCE                 
         BE    XIT                                                              
         MVI   SUPPRESS,C'N'       NO-PRINT THIS MARKET                         
         B     XIT                                                              
         SPACE 2                                                                
FMTHD1   L     R1,GLATHID          FORMAT MARKET SUMMARY HEADLINE               
         USING GLINTD,R1           REPORT FOR DAYPART REPORT                    
         LLC   RE,GLARGS                                                        
         SLL   RE,2                                                             
         LA    RE,GLARECL0(RE)                                                  
         L     RE,0(RE)            RE=A(MARKET TOTAL RECORD)                    
         DROP  R1                                                               
         L     R1,LGRPS            ADD IN ANY EXTRA LENGTHS                     
         LA    R1,8(R1)            DAYPART = 8 BYTES                            
         CLI   MORDER,C'R'         ANY EXTRA MARKET BYTES                       
         BNE   *+8                                                              
         LA    R1,2(R1)                                                         
         CLI   MORDER,C'A'                                                      
         BNE   *+8                                                              
         LA    R1,8(R1)                                                         
         AR    RE,R1                                                            
         MVC   GLDOL,DGOLDOL(RE)   EXTRACT VALUES FROM TOTAL RECORD             
         MVC   GLDEM,DGOLDEM(RE)                                                
         MVC   BYDOL,DPURDOL(RE)                                                
         MVC   BYPDEM,DPURDEM(RE)                                               
         MVC   BYRDEM,DACTDEM(RE)                                               
         MVC   BYADEM,BYRDEM                                                    
         BAS   RE,FMTSUM                                                        
         B     XIT                                                              
         SPACE 2                                                                
FMTHD2   L     R1,GLATHID          FORMAT MARKET OR DAYPART SUMMARY             
         USING GLINTD,R1           IN THE HEADLINES                             
         LLC   RE,GLARGS                                                        
         SLL   RE,2                                                             
         LA    RE,GLARECL0(RE)                                                  
         L     RE,0(RE)            RE=A(MARKET OR DAYPART TOTAL RECORD)         
         DROP  R1                                                               
         L     R1,EXTRALEN                                                      
         AR    RE,R1                                                            
         MVC   GLDOL,DGLDOL(RE)    EXTRACT VALUES FROM TOTAL RECORD             
         MVC   GLDEM,DGLDEM(RE)                                                 
         MVC   BYDOL,DBYDOL(RE)                                                 
         MVC   BYPDEM,DBYPDEM(RE)                                               
         MVC   BYRDEM,DBYRDEMR(RE)                                              
         MVC   BYADEM,DBYADEMR(RE)                                              
         CLI   WIDTHOPT,C'W'                                                    
         BNE   *+16                                                             
         MVC   BYRDEM,DBYRDEMW(RE)                                              
         MVC   BYADEM,DBYADEMW(RE)                                              
         BAS   RE,FMTSUM                                                        
*                                                                               
         MVI   SUPPRESS,C'N'                                                    
         MVI   WITHIN,C'N'                                                      
         CLI   ALLOPT,C'Y'         TEST PRINT ALL MARKETS/DAYPARTS              
         BE    XIT                 YES-SKIP TOLERENCE TESTS                     
         L     R1,INDEXVAL                                                      
         C     R1,LOLIM            TEST WITHIN TOLERENCE                        
         BNH   XIT                 NO                                           
         C     R1,HILIM                                                         
         BNL   XIT                 NO                                           
         MVI   SUPPRESS,C'Y'                                                    
         MVI   WITHIN,C'Y'                                                      
         B     XIT                                                              
         SPACE 2                                                                
* ROUTINE TO FORMAT SUMMARY TO MIDDLE OF HEADLINES                              
*                                                                               
FMTSUM   NTR1  ,                                                                
         MVC   HEDSUML1,SPACES                                                  
         MVC   HEDSUML1+11(34),=C'GOAL PURCHASED INDEX  ACTUAL INDEX'           
         CLI   INDEXOPT,C'A'                                                    
         BNE   *+10                                                             
         MVC   HEDSUML1+33(6),=C' AFFID'                                        
         CLI   DATAOPT,C'2'                                                     
         BNE   *+10                                                             
         MVC   HEDSUML1+8(7),=C'ORDERED'                                        
         CLI   DATAOPT,C'3'                                                     
         BNE   *+16                                                             
         MVC   HEDSUML1+16(2),SPACES                                            
         MVC   HEDSUML1+18(7),=C'ORDERED'                                       
         MVC   HEDSUML2,SPACES                                                  
         MVC   HEDSUML2(4),=C'GRPS'                                             
         MVC   HEDSUML3,SPACES                                                  
         MVC   HEDSUML3(7),=C'DOLLARS'                                          
*                                  EDIT THE VALUES                              
         LA    R2,HEDSUML2+8                                                    
         L     R4,GLDEM                                                         
*                                                                               
FS3      L     R1,BYRDEM                                                        
         CLI   INDEXOPT,C'A'                                                    
         BNE   *+8                                                              
         L     R1,BYADEM                                                        
         L     R3,BYPDEM                                                        
*                                                                               
         BRAS  RE,DEMR             EDIT THE VALUES IN R1,R3 AND R4              
*                                                                               
         L     R0,BYPDEM                                                        
         L     RF,GLDEM                                                         
         BAS   RE,INDEX                                                         
         LA    R2,HEDSUML2+26                                                   
         BAS   RE,EDINDEX          INDEX PURCHASED V GOAL                       
*                                                                               
         LR    R0,R1                                                            
         L     RF,BYPDEM                                                        
         CLI   COMPARE,C'P'        TEST COMPARING AGAINST PURCHASED             
         BE    FMTSUM2                                                          
         L     RF,GLDEM                                                         
         ST    RF,FULL                                                          
         BRAS  RE,DEMR2                                                         
         L     RF,FULL                                                          
         CLI   COMPARE,C'G'        TEST COMPARING AGAINST GOAL                  
         BE    FMTSUM2                                                          
         CLI   DATAOPT,C'2'        NO-THEN COMPARING AGAINST LOCKIN             
         BE    FMTSUM2             DATAOPT=2-PURCH VS LOCKIN                    
         L     RF,BYPDEM           ELSE LOCKIN VS GOAL                          
*                                                                               
FMTSUM2  BAS   RE,INDEX                                                         
         ST    R1,INDEXVAL         SAVE INDEX VALUE                             
         LA    R2,HEDSUML2+40                                                   
         BAS   RE,EDINDEX          INDEX ACTUAL V PURCHASED/GOAL                
         L     R1,GLDOL                                                         
         LA    R2,HEDSUML3+8                                                    
         BAS   RE,EDITDOL          GOAL DOLLARS                                 
         L     R1,BYDOL                                                         
         LA    R2,HEDSUML3+18                                                   
         BAS   RE,EDITDOL          PURCHASED DOLLARS                            
         L     R0,BYDOL                                                         
         L     RF,GLDOL                                                         
         BAS   RE,INDEX                                                         
         LA    R2,HEDSUML3+26                                                   
         BAS   RE,EDINDEX          INDEX PURCH V GOAL                           
         B     XIT                                                              
         SPACE 2                                                                
EDITDOL  LTR   R1,R1                                                            
         BNZ   *+10                                                             
         MVI   6(R2),C'0'                                                       
         BR    RE                                                               
         EDIT  (R1),(7,(R2))                                                    
         BR    RE                                                               
*                                                                               
EDINDEX  DS    0H                                                               
         CH    R1,=H'10000'                                                     
         BL    *+12                                                             
         MVC   1(4,R2),=C'HIGH'                                                 
         BR    RE                                                               
         EDIT  (R1),(5,(R2)),1                                                  
         BR    RE                                                               
*                                                                               
INDEX    LH    R1,=H'10000'                                                     
         LTR   RF,RF                                                            
         BZR   RE                                                               
         SRDA  R0,32                                                            
         MH    R1,=H'2000'                                                      
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BM    *+8                                                              
         LA    R1,1(R1)                                                         
         SRA   R1,1                                                             
         BR    RE                                                               
*                                                                               
ROUND    SRDA  R0,32                                                            
         SLDA  R0,1                                                             
         D     R0,=F'100'                                                       
         LTR   R1,R1                                                            
         BM    *+8                                                              
         LA    R1,1(R1)                                                         
         SRA   R1,1                                                             
         BR    RE                                                               
         EJECT                                                                  
* DRIVER HOOK LASTS                                                             
*                                                                               
LASTS    CLI   GLRECNO,2           TEST DETAIL REPORT                           
         BNE   LASTS2                                                           
         CLI   DPTOPT,C'Y'         TEST DAYPART REPORT AND DPT LAST             
         BNE   *+18                                                             
         CLC   DPTLEV,GLARGS                                                    
         BE    LASTS4                                                           
         B     XIT                                                              
         CLC   MKTLEV,GLARGS       OR MARKET REPORT AND MARKET LAST             
         BE    LASTS4              YES-SUPPRESS TOTAL LINE                      
         B     XIT                                                              
*                                                                               
LASTS2   CLI   GLRECNO,1           TEST SUMMARY REPORT                          
         BNE   XIT                                                              
         CLI   DPTOPT,C'Y'         YES-TEST DAYPART REPORT AND MARKET           
         BNE   XIT                     LAST                                     
         CLC   MKTLEV,GLARGS                                                    
         BNE   XIT                                                              
*                                                                               
LASTS4   MVI   SUPPRESS,C'Y'       SUPPRESS TOTAL LINE                          
         B     XIT                                                              
         EJECT                                                                  
* DRIVER HEADHOOK                                                               
*                                                                               
HEAD     L     R1,AH4                                                           
         LA    R1,1(R1)                                                         
         LR    RE,R1                                                            
         A     RE,PWIDTH                                                        
         CLI   0(RE),C' '          TEST WHETHER HEADS'VE BEEN FORMATTED         
         BH    HD1                 YES                                          
         MVC   0(50,R1),SPACES     NO-REMOVE MEDIA FROM FIRST HEADLINE          
         B     HDX                    AND EXIT                                  
*                                                                               
HD1      L     R5,=A(HEADTAB)                                                   
         USING HEADTABD,R5                                                      
*                                                                               
HD2      CLI   0(R5),0                                                          
         BE    HD10                                                             
         CLI   HDDPT,C'A'          TEST FOR ALL REPORTS                         
         BE    *+14                                                             
         CLC   HDDPT,DPTOPT        NO-MATCH THE DAYPART OPTION                  
         BNE   HD3                                                              
         CLC   HDREP,GLRECNO       DRIVER REPORT NUMBER                         
         BNE   HD3                                                              
         CLC   HDNMGR,GLOPTS+1     N'MARKET GROUPS                              
         BE    HD4                                                              
*                                                                               
HD3      LA    R5,HEADTABL(R5)                                                  
         B     HD2                                                              
*                                                                               
HD4      SR    R2,R2                                                            
         ICM   R2,1,HDMGR1                                                      
         BZ    HD6                                                              
         LA    R3,DREGMGR                                                       
         CLI   WIDTHOPT,C'W'                                                    
         BNE   *+8                                                              
         LA    R3,DWIDMGR                                                       
         BAS   RE,HDPOS                                                         
         MVC   0(L'MGR1HEAD,RF),MGR1HEAD                                        
         ICM   R2,1,HDMGR2                                                      
         BZ    HD6                                                              
         BAS   RE,HDPOS                                                         
         MVC   0(L'MGR2HEAD,RF),MGR2HEAD                                        
         ICM   R2,1,HDMGR3                                                      
         BZ    HD6                                                              
         BAS   RE,HDPOS                                                         
         MVC   0(L'MGR3HEAD,RF),MGR3HEAD                                        
*                                                                               
HD6      ICM   R2,1,HDMKTDPT       TEST MARKET OR DAYPART TO PRINT              
         BZ    HD8                                                              
         LA    R3,DREGMKT          YES-                                         
         CLI   WIDTHOPT,C'W'                                                    
         BNE   *+8                                                              
         LA    R3,DWIDMKT                                                       
         CLI   DPTOPT,C'Y'                                                      
         BNE   HD7                                                              
         LA    R3,DREGDPT                                                       
         CLI   WIDTHOPT,C'W'                                                    
         BNE   HD7                                                              
         LA    R3,DWIDDPT                                                       
*                                                                               
HD7      BAS   RE,HDPOS                                                         
         CLI   DPTOPT,C'Y'         PRINT DAYPART FOR DAYPART REPORT             
         BNE   *+14                                                             
         MVC   0(L'DPTHEAD,RF),DPTHEAD                                          
         B     HD8                                                              
         MVC   0(L'MKTHEAD,RF),MKTHEAD    ELSE, PRINT THE MARKET                
*                                                                               
HD8      ICM   R2,1,HDSUM                                                       
         BZ    HD10                                                             
         LA    R3,DREGSUM                                                       
         CLI   WIDTHOPT,C'W'                                                    
         BNE   *+8                                                              
         LA    R3,DWIDSUM                                                       
         BAS   RE,HDPOS                                                         
         MVC   0(L'HEDSUML1,RF),HEDSUML1                                        
         A     RF,PWIDTH                                                        
         MVC   0(L'HEDSUML2,RF),HEDSUML2                                        
         A     RF,PWIDTH                                                        
         MVC   0(L'HEDSUML3,RF),HEDSUML3                                        
         DROP  R5                                                               
*                                                                               
HD10     LA    R3,96               R3=DISPLACEMENT TO RHS                       
         CLI   WIDTHOPT,C'W'                                                    
         BNE   *+8                                                              
         LA    R3,128                                                           
         L     R2,AH4                                                           
         A     R2,PWIDTH           R2=A(HEAD5)                                  
         LA    R5,0(R3,R2)                                                      
         CLC   0(3,R5),=C'DF='     TEST EXTRA LINE FOR DEMO FACTORING           
         BNE   *+8                                                              
         A     R2,PWIDTH                                                        
         A     R2,PWIDTH           R2=A(HEAD6 OR HEAD7)                         
         CLI   INDEXOPT,C'A'       TEST INDEX OPTION = AFFID                    
         BE    *+12                                                             
         CLI   GLRECNO,1           OR NOT MARKET SUMMARY                        
         BE    HD12                                                             
         LA    R5,0(R3,R2)         YES-SHOW AFFID RERATE BOOK                   
         MVC   0(9,R5),=C'AFFID ON '                                            
         MVC   9(21,R5),12(R5)                                                  
         MVC   30(4,R5),SPACES                                                  
*                                                                               
HD12     A     R2,PWIDTH           HEAD7 OR HEAD8                               
         LA    R5,0(R3,R2)                                                      
         CLI   GLRECNO,2           TEST DETAIL REPORT                           
         BNE   HD14                                                             
         OC    SBQUPGRD,SBQUPGRD   AND UPGRADE                                  
         BZ    HD14                                                             
         ICM   R1,15,AUPGHDR                                                    
         BZ    HD14                                                             
         MVC   0(34,R5),SPACES     YES-PRINT UPGRADE HERE                       
         LLC   RE,5(R1)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),8(R1)                                                    
         A     R5,PWIDTH           NEXT LINE                                    
*                                                                               
HD14     MVC   0(34,R5),SPACES     CLEAR RHS                                    
         A     R5,PWIDTH           NEXT LINE                                    
         MVC   0(34,R5),SPACES                                                  
         MVC   0(10,R5),=C'TOLERANCE'    FORMAT TOLERANCES                      
         MVC   11(6,R5),=C'ABOVE='                                              
         EDIT  PCTTOLA,(5,17(R5)),1,ALIGN=LEFT,TRAIL=C'%'                       
         A     R5,PWIDTH                                                        
         MVC   0(34,R5),SPACES                                                  
         MVC   0(10,R5),=C'TOLERANCE'                                           
         MVC   11(6,R5),=C'BELOW='                                              
         EDIT  PCTTOLB,(5,17(R5)),1,ALIGN=LEFT,TRAIL=C'%'                       
         B     HDX                                                              
*                                                                               
HDX      B     XIT                                                              
         SPACE 1                                                                
HDPOS    L     R1,AH4                                                           
         SH    R2,=H'4'                                                         
         BNP   *+12                                                             
         A     R1,PWIDTH                                                        
         BCT   R2,*-4                                                           
         LA    RF,0(R3,R1)                                                      
         BR    RE                                                               
         EJECT                                                                  
* ROUTINE TO HOOK TO CALLING PROGRAM TO CALL DRIVER                             
*                                                                               
GODRIVER NTR1                                                                   
         L     RF,ADRIVER                                                       
         L     RE,SAVERD                                                        
         LM    R0,RC,20(RE)                                                     
         BASR  RE,RF                                                            
         XIT1  ,                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* HEADLINE DISPLACEMENTS                                                        
*                                                                               
DWIDMGR  EQU   64                  WIDE-    MARKET GROUP                        
DWIDMKT  EQU   67                           MARKET                              
DWIDDPT  EQU   70                           DAYPART                             
DWIDSUM  EQU   58                           SUMMARY                             
DREGMGR  EQU   48                  REGULAR- MARKET GROUP                        
DREGMKT  EQU   51                           MARKET                              
DREGDPT  EQU   54                           DAYPART                             
DREGSUM  EQU   43                           SUMMARY                             
         SPACE 1                                                                
* DRIVER MARKET TOTAL RECORD DISPLACEMENTS                                      
*                                                                               
DGLDOL   EQU   71                  GOAL DOLLARS                                 
DGLDEM   EQU   75                  GOAL DEMO                                    
DBYDOL   EQU   83                  BUY DOLLARS                                  
DBYPDEM  EQU   107                 BUY PURCHSED DEMO                            
DBYRDEMR EQU   251                 BUY RERATED DEMO (ACTUAL BOOK) REG           
DBYRDEMW EQU   323                 BUY RERATED DEMO (ACTUAL BOOK) WIDE          
DBYADEMR EQU   299                 BUY AFFID DEMO (ACTUAL BOOK) REG             
DBYADEMW EQU   395                 BUY AFFID DEMO (ACTUAL BOOK) WIDE            
         SPACE 1                                                                
* DRIVER MARKET SUMMARY RECORD DISPLACEMENTS                                    
*                                                                               
DGOLDOL  EQU   23                  GOAL DOLLARS                                 
DGOLDEM  EQU   27                  GOAL DEMO                                    
DPURDEM  EQU   47                  BUY PURCHASED DEMO                           
DPURDOL  EQU   55                  BUY PURCHASED DOLLARS                        
DACTDEM  EQU   79                  BUY ACTUAL DEMO                              
         EJECT                                                                  
* WORKING STORAGE                                                               
*                                                                               
         SPACE 1                                                                
GLDOL    DS   F                    GOAL DOLLARS                                 
GLDEM    DS   F                    GOAL DEMO                                    
BYDOL    DS   F                    BUY DOLLARS                                  
BYPDEM   DS   F                    BUY PURCHSED DEMO                            
BYRDEM   DS   F                    BUY RERATED DEMO                             
BYADEM   DS   F                    BUY AFFID DEMO                               
INDEXVAL DS   F                    INDEX VALUE                                  
*                                                                               
ESTDEM   DS   F                    SAVED DEMOS FOR UPGRADE VARIANCE             
ESTDEM2  DS   F                                                                 
UPGDEM   DS   F                                                                 
UPGDEM2  DS   F                                                                 
*                                                                               
AUPGHDR  DS   A                    A(UPGRADE FIELD HEADER)                      
LOLIM    DS   F                    LOWER INDEX TOLERENCE                        
HILIM    DS   F                    UPPER INDEX TOLERENCE                        
NSPOTS   DS   F                                                                 
EXTRALEN DS   F                                                                 
LGRPS    DS   F                                                                 
PCTTOLA  DS   H                                                                 
PCTTOLB  DS   H                                                                 
*                                                                               
SUPPRESS DS    CL1                                                              
WITHIN   DS    CL1                                                              
DATAOPT  DS    CL1                                                              
INDEXOPT DS    CL1                                                              
COMPARE  DS    CL1                                                              
AFFIDS   DS    CL1                                                              
SINGRPT  DS    XL1                                                              
MORDER   DS    CL1                                                              
ALLOPT   DS    CL1                                                              
DPTOPT   DS    CL1                                                              
COSTOPT  DS    CL1                                                              
VAROPT   DS    CL1                                                              
DPTLEV   DS    CL1                                                              
*                                                                               
SVPRD    DS    XL1                                                              
SVEST    DS    XL1                                                              
SVMKTIND DS    CL(L'MKTIND)                                                     
SVMAXTLV DS    CL(L'MAXTOTLV)                                                   
SVBUYKEY DS    XL(L'BUYKEY)                                                     
*                                                                               
DEMOS    DS    XL(L'EBDEMOS)                                                    
POLDEMOS DS    XL12                                                             
NPOLDEMS DS    XL1                                                              
*                                                                               
XFF      DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'                           
FF       EQU   X'FF'                                                            
*                                                                               
HEDSUML1 DS    CL45                                                             
HEDSUML2 DS    CL45                                                             
HEDSUML3 DS    CL45                                                             
MGR1HEAD DS    CL44                                                             
MGR2HEAD DS    CL44                                                             
MGR3HEAD DS    CL44                                                             
DPTHEAD  DS    CL19                                                             
MKTHEAD  DS    CL44                                                             
*                                                                               
MYDEMOS  DS    XL80                                                             
NMYDEMOS EQU   8                                                                
*                                                                               
DBAREA   DS    XL(L'DBLOCK)                                                     
         EJECT                                                                  
* HEADLINE POSITION TABLE                                                       
*                                                                               
HEADTAB  DC    C'N',X'01',X'00',X'000000',X'00',X'00'                           
         DC    C'N',X'01',X'01',X'050000',X'00',X'00'                           
         DC    C'N',X'01',X'02',X'050600',X'00',X'00'                           
         DC    C'N',X'01',X'03',X'050607',X'00',X'00'                           
         DC    C'Y',X'01',X'00',X'000000',X'00',X'07'                           
         DC    C'Y',X'01',X'01',X'050000',X'00',X'07'                           
         DC    C'Y',X'01',X'02',X'050600',X'00',X'08'                           
         DC    C'Y',X'01',X'03',X'050607',X'00',X'08'                           
         DC    C'A',X'02',X'00',X'000000',X'05',X'07'                           
         DC    C'A',X'02',X'01',X'050000',X'06',X'08'                           
         DC    C'A',X'02',X'02',X'040500',X'06',X'08'                           
         DC    C'A',X'02',X'03',X'040506',X'07',X'08'                           
         DC    C'A',X'03',X'00',X'000000',X'05',X'07'                           
         DC    C'A',X'03',X'01',X'050000',X'06',X'08'                           
         DC    C'A',X'03',X'02',X'040500',X'06',X'08'                           
         DC    C'A',X'03',X'03',X'040506',X'07',X'08'                           
         DC    X'00'                                                            
         SPACE 1                                                                
HEADTABD DSECT                                                                  
HDDPT    DS    X                   DAYPART OPTION                               
HDREP    DS    X                   REPORT NUMBER                                
HDNMGR   DS    X                   N'MARKET GROUPS                              
HDMGR1   DS    X                   HEADLINE FOR MARKET GROUP 1                  
HDMGR2   DS    X                   HEADLINE FOR MARKET GROUP 2                  
HDMGR3   DS    X                   HEADLINE FOR MARKET GROUP 3                  
HDMKTDPT DS    X                   HEADLINE FOR MARKET OR DAYPART               
HDSUM    DS    X                   HEADLINE FOR MKT OR DPT SUMMARY              
HEADTABL EQU   *-HEADTABD                                                       
         SPACE 1                                                                
T20403   CSECT                                                                  
         DS    0F                                                               
         EJECT                                                                  
* EXTENTION ROUTINES                                                            
*                                                                               
T20403X  NMOD1 0,**403X**,RA                                                    
         L     RC,AGEND                                                         
         USING GEND,RC                                                          
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
*                                                                               
         B     VALPCT                                                           
         B     VALBOOK                                                          
         B     GETDPTTB                                                         
         B     HEDDEM                                                           
*                                                                               
XIT2     XIT1  ,                                                                
         EJECT                                                                  
* VALIDATE PERCENT OPTION                                                       
* ENTRY : R2=A(PERCENT HEADER)                                                  
* EXIT  : CC EQ - HALF=PERCENT                                                  
*         CC NE - PERCENT NOT ENTERED                                           
*                                                                               
VALPCT   CLI   5(R2),0                                                          
         BE    VPCTNO                                                           
         GOTO1 SCANNER,DMCB,(R2),(1,BLOCK),0                                    
         CLI   4(R1),1                                                          
         BNE   EPCT                                                             
         LA    R4,BLOCK                                                         
         CLI   1(R4),0                                                          
         BNE   EPCT                                                             
         TM    2(R4),X'80'         TEST NUMERIC                                 
         BZ    *+16                                                             
         L     R1,4(R4)            YES                                          
         MH    R1,=H'10'                                                        
         B     VPCT8                                                            
         CLI   0(R4),2             NO                                           
         BL    EPCT                                                             
         CLI   0(R4),4                                                          
         BH    EPCT                                                             
         LLC   R3,0(R4)                                                         
         LA    RE,12(R4)                                                        
         MVI   BYTE,0                                                           
         SR    R1,R1                                                            
         LA    RF,FULL                                                          
*                                                                               
VPCT2    CLI   0(RE),C'.'                                                       
         BNE   VPCT4                                                            
         CLI   BYTE,0                                                           
         BNE   EPCT                                                             
         MVI   BYTE,1                                                           
         CLI   2(RE),C' '                                                       
         BNE   EPCT                                                             
         B     VPCT6                                                            
*                                                                               
VPCT4    CLI   0(RE),C'0'                                                       
         BL    EPCT                                                             
         CLI   0(RE),C'9'                                                       
         BH    EPCT                                                             
         MVC   0(1,RF),0(RE)                                                    
         LA    R1,1(R1)                                                         
         LA    RF,1(RF)                                                         
*                                                                               
VPCT6    LA    RE,1(RE)                                                         
         BCT   R3,VPCT2                                                         
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FULL(1)                                                      
         CVB   R1,DUB                                                           
*                                                                               
VPCT8    LTR   R1,R1                                                            
         BNP   EPCT                                                             
         CH    R1,=H'1000'                                                      
         BNL   EPCT                                                             
         STH   R1,HALF                                                          
*                                                                               
VPCTYES  CR    RB,RB                                                            
         B     VPCTX                                                            
*                                                                               
VPCTNO   LTR   RB,RB                                                            
*                                                                               
VPCTX    B     XIT2                                                             
         EJECT                                                                  
* VALIDATE BOOK FIELD                                                           
* INPUT :  R2=A(FIELD HEADER)                                                   
*          R3=A(OUTPUT BOOK YYMM)                                               
* OUTPUT:  FULL=A(UPGRADE FIELD HEADER)                                         
*                                                                               
VALBOOK  MVC   0(4,R3),=C'ACT '                                                 
         CLI   5(R2),0                                                          
         BE    VBOOKX                                                           
         CLC   8(3,R2),=C'ACT '                                                 
         BNE   VBOOK2                                                           
         CLI   5(R2),3                                                          
         BE    VBOOKX                                                           
         CLI   5(R2),6             TEST ACT/YY                                  
         BNE   EBK                                                              
         CLI   11(R2),C'/'                                                      
         BNE   EBK                                                              
         CLI   12(R2),C'0'                                                      
         BL    EBK                                                              
         CLI   13(R2),C'0'                                                      
         BL    EBK                                                              
*Y2K         PACK  DUB,12(2,R2)        YES-                                     
*Y2K         CVB   RE,DUB                                                       
*Y2K         STC   RE,3(R3)                                                     
*                                                                               
         MVC   DUB(2),12(R2)       MOVE IN YEAR                                 
         MVC   DUB+2(4),=C'0101'   DUMMY UP MMDD                                
         GOTO1 DATCON,DMCB,(0,DUB),(3,FULL) CONVERT TO BINARY                   
         MVC   3(1,R3),FULL        STORE BINARY YEAR                            
         OI    3(R3),X'80'         INDICATE YEAR                                
         B     VBOOKX                                                           
*                                                                               
VBOOK2   GOTO1 DATVAL,DMCB,(2,8(R2)),DUB                                        
         OC    0(4,R1),0(R1)                                                    
         BZ    VBOOK4                                                           
         MVC   0(4,R3),DUB                                                      
         B     VBOOKX                                                           
*                                                                               
VBOOK4   OC    SBQUPGRD,SBQUPGRD   TEST UPGRADE ALREADY REFINED                 
         BNZ   EBK                 YES-INVALID BOOK                             
         BAS   RE,VALUPG           VALIDATE FOR UPGRADE EXPRESSION              
         ST    R2,FULL             PASS A(UPGRADE FIELD HEADER)                 
         XC    0(4,R3),0(R3)       CLEAR THE BOOK FIELD                         
*                                                                               
VBOOKX   B     XIT2                                                             
         EJECT                                                                  
* VALIDATE UPGRADE EXPRESSION                                                   
* R2=A(FIELD HEADER)                                                            
*                                                                               
VALUPG   NTR1  ,                                                                
         XC    SBQUPVAL(SBQUPL),SBQUPVAL                                        
         GOTO1 SCANNER,DMCB,(20,(R2)),BLOCK,C',=,='                             
         SR    R5,R5                                                            
         ICM   R5,1,4(R1)          R5=N'SCANNER LINES                           
         BZ    EBK                                                              
         MVI   BYTE,0              FIELD INPUT INDICATORS                       
         LA    R3,BLOCK                                                         
*                                                                               
VALUPG1  SR    R1,R1                                                            
         ICM   R1,1,0(R3)          TEST L'FIRST HALF OF ENTRY                   
         BZ    EBK                                                              
         CLI   0(R3),L'UPGNAME                                                  
         BH    EBK                                                              
         CLI   1(R3),0             TEST L'SECOND HALF OF ENTRY                  
         BE    EBK                                                              
         BCTR  R1,0                R1=L'FIRST HALF OF INPUT-1                   
         LA    R4,UPGTAB                                                        
         USING UPGTABD,R4          R4=A(UPGRADE TABLE)                          
*                                                                               
VALUPG2  CLI   UPGNAME,0           TEST E-O-T                                   
         BE    EBK                                                              
         CLI   UPGSHRT,C' '        TEST ENTRY HAS SHORT KEYWORD                 
         BE    VALUPG4                                                          
         LA    RE,2                                                             
         CLI   UPGSHRT+2,C' '                                                   
         BNE   *+6                                                              
         BCTR  RE,0                                                             
         CLI   UPGSHRT+1,C' '                                                   
         BNE   *+6                                                              
         BCTR  RE,0                                                             
         CR    R1,RE               TEST INPUT LEN = SHORT NAME LEN              
         BNE   VALUPG4                                                          
         EX    R1,*+8              YES - MATCH ON SHORT NAME                    
         B     *+10                                                             
         CLC   UPGSHRT(0),12(R3)                                                
         BE    VALUPG6                                                          
*                                                                               
VALUPG4  EX    R1,*+8              MATCH ON LONG NAME                           
         B     *+10                                                             
         CLC   UPGNAME(0),12(R3)                                                
         BE    VALUPG6                                                          
         LA    R4,UPGTABL(R4)      BUMP TO NEXT UPGRADE TABLE ENTRY             
         B     VALUPG2                                                          
*                                                                               
VALUPG6  MVC   HALF(1),BYTE        TEST KEYWORD INPUT PREVIOUSLY                
         NC    HALF(1),UPGIND1                                                  
         BNZ   EUPG                                                             
         XC    WORK,WORK                                                        
         LLC   R1,1(R3)                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK+8(0),22(R3)                                                 
         STC   R1,WORK+5                                                        
         LA    R1,8(R1)                                                         
         STC   R1,WORK                                                          
         OC    BYTE,UPGIND1        SET THIS KEYWORD INPUT                       
         SR    RF,RF                                                            
         ICM   RF,3,UPGROUT                                                     
         LA    RF,T20403X(RF)      RF=A(VALIDATION ROUTINE)                     
         BASR  RE,RF                                                            
         LA    R3,22+20(R3)                                                     
         BCT   R5,VALUPG1                                                       
*                                                                               
         OC    SBQUPGRD,SBQUPGRD   TEST FOR UPGRADE FORMULA                     
         BZ    EUPG                                                             
         OC    SBQUPFBK,SBQUPFBK   MUST HAVE SHARE BOOK                         
         BZ    ENOFRBK                                                          
*                                                                               
VALUPGX  B     XIT2                                                             
         SPACE 2                                                                
UPGTAB   DS    0H               ** UPGRADE VALIDATION TABLE **                  
         DC    C'PUT     ',C'   ',X'1000',AL2(VALUPA-T20403X)                   
         DC    C'SHR     ',C'   ',X'0800',AL2(VALUPA-T20403X)                   
         DC    C'RTG     ',C'   ',X'0800',AL2(VALUPA-T20403X)                   
         DC    C'RP      ',C'   ',X'1800',AL2(VALUPA-T20403X)                   
         DC    C'TUPGRADE',C'UPT',X'8000',AL2(VALUPE-T20403X)                   
         DC    C'PUPGRADE',C'UPP',X'8000',AL2(VALUPE-T20403X)                   
         DC    C'BOOK    ',C'BK ',X'4000',AL2(VALUPB-T20403X)                   
UPGTABX  DC    AL1(0)                                                           
*                                                                               
UPGTABD  DSECT                                                                  
UPGNAME  DS    CL8                 KEYWORD NAME                                 
UPGSHRT  DS    CL3                 SHORT KEYWORD NAME                           
UPGIND1  DS    XL1                 INDICATORS                                   
UPGIND2  DS    XL1                 INDICATORS                                   
UPGROUT  DS    AL2                 DISPLACEMENT OF VALIDATION ROUTINE           
UPGTABL  EQU   *-UPGTABD                                                        
*                                                                               
T20403   CSECT                                                                  
         DS    0H                                                               
         SPACE 2                                                                
VALUPE   LR    R0,RE               ** VALIDATE UPGRADE EXPRESSION **            
         MVC   SBQUPFIL,UPGSHRT+2  SET FILE INDICATOR                           
         GOTO1 UPVAL,DMCB,WORK,PARAS,(C'/',ACOMFACS)                            
         CLI   PARAS+1,0                                                        
         BE    EUPG                                                             
         MVC   SBQUPGRD,PARAS+4    UPGRADE EXPRSSN FROM UPGRADE ELEM            
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 1                                                                
VALUPB   LR    R0,RE               ** VALIDATE OVERRIDE BOOK **                 
         GOTO1 BOOKVAL,DMCB,(C'N',WORK),(1,FULL),SCANNER                        
         SR    RF,RF                                                            
         ICM   RF,1,4(R1)          RF=N'BOOKS                                   
         BZ    EBK                                                              
         TM    FULL,X'BF'          TEST FOR FUNNY BOOK FORMATS                  
         BNZ   EBK                                                              
         MVC   SBQUPFBK,FULL+1                                                  
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 1                                                                
VALUPA   LR    R0,RE               ** VALIDATE PUT/SHR AVERAGING **             
         CLI   WORK+5,1                                                         
         BNE   EUPG                                                             
         CLI   WORK+8,C'1'                                                      
         BE    *+12                                                             
         CLI   WORK+8,C'2'                                                      
         BNE   EUPG                                                             
         TM    UPGIND1,X'10'                                                    
         BZ    *+10                                                             
         MVC   SBQUPPUT,WORK+8                                                  
         TM    UPGIND1,X'08'                                                    
         BZ    *+10                                                             
         MVC   SBQUPSHR,WORK+8                                                  
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
* GET DAYPART TABLE                                                             
* INPUT  : SBDPTMEN = DAYPART MENU                                              
* OUTPUT : SBDPTTAB SET                                                         
*                                                                               
GETDPTTB OC    SBADPTTB,SBADPTTB   TEST DAYPART TABLES BUFFER                   
         BZ    GDX                                                              
         CLC   SBDPTMEN,SVDPTMEN   YES-TEST DAYPART MENU CHANGE                 
         BE    GDX                                                              
         MVC   SVDPTMEN,SBDPTMEN   YES-GET DAYPART TABLE                        
         LA    R1,ALPHATAB                                                      
         SR    RE,RE                                                            
*                                                                               
GD10     CLI   0(R1),0                                                          
         BNE   *+12                                                             
         LA    RE,25                                                            
         B     GD20                                                             
         CLC   SBDPTMEN,0(R1)                                                   
         BE    *+12                                                             
         LA    R1,1(R1)                                                         
         BCT   RE,GD10                                                          
         LPR   RE,RE                                                            
*                                                                               
GD20     MH    RE,=H'180'                                                       
         L     R1,SBADPTTB                                                      
         LA    R1,0(RE,R1)                                                      
         XC    SBDPTTAB,SBDPTTAB                                                
         MVC   SBDPTTAB(180),0(R1)                                              
*                                                                               
GDX      B     XIT2                                                             
         SPACE 1                                                                
ALPHATAB DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789',X'00'                    
         EJECT                                                                  
* DEMO HEADING ROUTINE                                                          
*                                                                               
HEDDEM   DS    0H                                                               
         L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         MVC   0(7,R3),SPACES                                                   
         LLC   R5,GLARGS                                                        
         BCTR  R5,0                                                             
         MH    R5,=H'7'                                                         
         LA    R5,DEMNAMES(R5)                                                  
         CLI   GLARGS+1,C'R'       TEST DEMO MODIFIER IS RATING                 
         BE    HDEM10                                                           
         L     R1,ADEMLST                                                       
         LLC   R2,GLARGS                                                        
         BCTR  R2,0                                                             
         MH    R2,=H'3'                                                         
         LA    R2,0(R1,R2)                                                      
         CLI   2(R2),0                                                          
         BE    XIT2                                                             
         MVC   FULL(3),0(R2)                                                    
         MVC   FULL+1(1),GLARGS+1    MODIFIER IS S OR P                         
         LA    R0,NMYDEMOS         MIGHT HAVE FOUND NAME ALREADY                
         L     R2,=A(MYDEMOS)                                                   
*                                                                               
HDEM2    OC    0(3,R2),0(R2)                                                    
         BZ    HDEM4                                                            
         CLC   0(3,R2),FULL                                                     
         BE    HDEM6               NAME FOUND                                   
         LA    R2,10(R2)                                                        
         BCT   R0,HDEM2                                                         
         L     R2,=A(MYDEMOS)                                                   
         XC    0(L'MYDEMOS,R2),0(R2)                                            
*                                                                               
HDEM4    MVC   0(3,R2),FULL        GET THE NAME                                 
*                                                                               
         L     R5,=A(DBAREA)                                                    
         USING DBLOCKD,R5                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVI   DBFUNCT,DBGETDEM                                                 
         MVI   DBTPTT,C'T'                                                      
         MVC   DBCOMFCS,SBCOMFAC                                                
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'T'                                                    
         LA    R1,SBAGYREC                                                      
         USING AGYHDR,R1                                                        
         CLI   AGYPROF+7,C'C'                                                   
         BNE   *+8                                                              
         MVI   DBSELMED,C'C'                                                    
         GOTO1 DEMOCON,DMCB,(1,FULL),(2,3(R2)),(C'S',(R5)),0                    
         DROP  R1,R5                                                            
*                                                                               
HDEM6    LA    R5,3(R2)            R5=A(DEMO NAME)                              
*                                                                               
HDEM10   OC    0(7,R5),0(R5)                                                    
         BZ    XIT2                                                             
         MVC   0(7,R3),0(R5)                                                    
         B     XIT2                                                             
         EJECT                                                                  
* ERROR EXITS AND MESSAGES                                                      
*                                                                               
EPCT     MVC   CONHEAD(L'EPCTM),EPCTM                                           
         B     MYCURS                                                           
*                                                                               
EBK      MVC   CONHEAD(L'EBKM),EBKM                                             
         B     MYCURS                                                           
*                                                                               
EUPG     MVC   CONHEAD(L'EUPGM),EUPGM                                           
         B     MYCURS                                                           
*                                                                               
ENOFRBK  MVC   CONHEAD(L'ENOFRBKM),ENOFRBKM                                     
         B     MYCURS                                                           
*                                                                               
MYCURS   MVI   ERROR,X'FE'                                                      
CURS     GOTO1 CURSERR                                                          
*                                                                               
EPCTM    DC    C'INVALID PERCENT'                                               
EBKM     DC    C'INVALID BOOK'                                                  
EUPGM    DC    C'INVALID UPGRADE EXPRESSION'                                    
ENOFRBKM DC    C'MUST SPECIFY SHARE BOOK'                                       
         EJECT                                                                  
SVDPTMEN DS    CL1                                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
WORKD    DSECT                                                                  
*                                                                               
AXTRA    DS    0F               ** EXTENTION ROUTINE ADDRESSES **               
AVALPCT  DS    A                                                                
AVALBOOK DS    A                                                                
AGETDPTB DS    A                                                                
AHDEM    DS    A                                                                
AXTRAN   EQU   (*-AXTRA)/L'AXTRA                                                
*                                                                               
SAVERD   DS    A                                                                
RELO     DS    A                                                                
AGEND    DS    A                                                                
         DS    0D                                                               
PRDLST   DS    XL256                                                            
*                                                                               
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
*                                                                               
T20403   CSECT                                                                  
         DS    0F                                                               
*                                                                               
DEMROUND NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    COLIND,COLINDR      DEMO ROUNDING                                
         BZ    DEMR05              YES                                          
*                                                                               
         LA    R1,SBLOCK                                                        
         TM    SBEFLAG4-SBLOCK(R1),SBE42DEC  WANT 2 DECIMAL PLACES?             
         BZ    DEMR05              NO                                           
*                                                                               
         XR    R1,R1                                                            
         L     RE,GLADTENT                                                      
         L     RE,DROIADD-DROD(RE) A(INPUT RECORD)                              
*                                                                               
         LA    R2,DRINARGS-DRIND(RE)    INPUT ARGS = DEMO CAT                   
         CLC   =C'IDEMOUSR',DRINROUT-DRIND(RE)                                  
         BE    *+8                                                              
         LA    R2,1(R2)                                                         
         ICM   R1,1,0(R2)                                                       
         BZ    DEMR05                                                           
*                                                                               
         CLI   GLARGS+1,C'R'       MAKE SURE WE ARE DOING RATING                
         BNE   DEMR05                                                           
*                                                                               
         BCTR  R1,0                TEST THIS DEMO IS RATING                     
         MHI   R1,3                                                             
         L     R2,ADEMLST                                                       
         AR    R1,R2                                                            
         CLI   1(R1),C'R'                                                       
         BE    DEMR00                                                           
         CLI   1(R1),C'E'                                                       
         BNE   DEMR05                                                           
*                                                                               
DEMR00   CLC   =C'BYUDEM ',DROENTN-DROD(RE)                                     
         BNE   DEMR02                                                           
*                                                                               
DEMR01   LTR   RF,RF                                                            
         BZ    DEMR05                                                           
*                                                                               
         SR    RE,RE                                                            
         D     RE,=F'10'                                                        
         B     DEMR05                                                           
*                                                                               
DEMR02   EDIT  (RF),(7,(R3)),2     PRINT 2 DECIMAL DEMOS                        
         B     DEMRX                                                            
*                                                                               
DEMR05   EDIT  (RF),(7,(R3)),1                                                  
*                                                                               
DEMRX    J     XIT                                                              
*                                                                               
DEMR     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    COLIND,COLINDR      DEMO ROUNDING                                
         BZ    DEMR10              YES                                          
*                                                                               
         LA    RF,SBLOCK                                                        
         TM    SBEFLAG4-SBLOCK(RF),SBE42DEC  WANT 2 DECIMAL PLACES?             
         BZ    DEMR10              NO                                           
*                                                                               
         EDIT  (R4),(7,(R2)),2     GOAL DEMO                                    
         EDIT  (R3),(7,10(R2)),2   PURCHASED DEMO                               
         EDIT  (R1),(7,24(R2)),2   ACTUAL DEMO                                  
         B     DEMX                                                             
*                                                                               
DEMR10   EDIT  (R4),(7,(R2)),1     GOAL DEMO                                    
         EDIT  (R3),(7,10(R2)),1   PURCHASED DEMO                               
         EDIT  (R1),(7,24(R2)),1   ACTUAL DEMO                                  
*                                                                               
DEMX     J     XIT                                                              
*                                                                               
DEMR2    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    COLIND,COLINDR      DEMO ROUNDING                                
         BZ    D2X                 YES                                          
*                                                                               
         LA    RF,SBLOCK                                                        
         TM    SBEFLAG4-SBLOCK(RF),SBE42DEC  WANT 2 DECIMAL PLACES?             
         BZ    D2X                 NO                                           
*                                                                               
         L     RF,FULL                                                          
         SR    RE,RE                                                            
         D     RE,=F'10'                                                        
         ST    RF,FULL                                                          
*                                                                               
D2X      J     XIT                                                              
*                                                                               
MGRCODE  NTR1  BASE=*,LABEL=*                                                   
         MVI   13(R4),C'?'                                                      
         LA    RF,SPMGRTAB                                                      
         LHI   R0,(SPMGRTBX-SPMGRTAB)/L'SPMGRTAB                                
         CLC   SBQMGRD,2(RF)                                                    
         BE    MGRC10                                                           
         AHI   RF,L'SPMGRTAB                                                    
         BCT   R0,*-14                                                          
         J     XIT                                                              
*                                                                               
MGRC10   MVC   13(2,R4),0(RF)                                                   
         J     XIT                                                              
         SPACE 2                                                                
       ++INCLUDE SPMGRTAB                                                       
         EJECT                                                                  
* OTHER DSECTS ARE HIDDEN IN HERE                                               
*                                                                               
*SPWRIWORKD                                                                     
*SPOTTABD                                                                       
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDBIGBOX                                                                       
*DRGLOBAL                                                                       
*DRIVETABLE                                                                     
*DRINTRECD                                                                      
*DEDBLOCK                                                                       
*SPGENAGY                                                                       
*SPGENBUY                                                                       
*SPWRIFFD                                                                       
         PRINT OFF                                                              
       ++INCLUDE SPWRIWORKD                                                     
       ++INCLUDE SPOTTABD                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE DRIVETABLE                                                     
       ++INCLUDE DRINTRECD2                                                     
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPWRIFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE SPWRIF1D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPWRIE1D                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020SPWRI03   09/26/08'                                      
         END                                                                    
