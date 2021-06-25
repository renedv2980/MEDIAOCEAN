*          DATA SET SPSYSDRIVE AT LEVEL 030 AS OF 05/01/02                      
*********************************************************************           
*********************************************************************           
*********************************************************************           
*********************************************************************           
*********************************************************************           
*********************************************************************           
*********************************************************************           
*********************************************************************           
*********************************************************************           
*                                                                   *           
*  THIS MODULE IS DEAD.  IT HAS BEEN REPLACED BY SPSYSDRV1,2,3      *           
*        IT IS HERE FOR REFERENCE ONLY!                             *           
*                                                                   *           
*********************************************************************           
*********************************************************************           
*********************************************************************           
*********************************************************************           
*********************************************************************           
*********************************************************************           
*********************************************************************           
*********************************************************************           
*PHASE T00A46A,+0                                                               
*INCLUDE BLDMGA                                                                 
         TITLE 'SPDRIVER - SYSTEM DRIVER FOR SPOTPAK WRITER'                    
*                                                                               
*********************************************************************           
*                                                                   *           
*          SPSYSDRIVE (T00A46) - SYSTEM DRIVER FOR SPOT WRITER      *           
*                                                                   *           
*-------------------------------------------------------------------*           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* 05JAN94 00 EFJ -- HISTORY LOST.  LEVEL RESET                      *           
* 07JAN94 02 EFJ -- INFOMERCIAL REWORK                              *           
* 13JAN94 03 EFJ -- NEW KEYWORDS FOR INFOMERCIALS                   *           
* 27JAN94 04 TCS -- ONLY PRINT START DATE FOR SINGLE DAY BUYS       *           
* 03FEB94 05 EFJ -- NEW ROWS FOR INFOMERCIALS - IFRSPDT/DTW/DTM     *           
*                -- REMOVE TEMP HARD CODE                           *           
* 14FEB94 06 EFJ -- SUPPORT FOR NEW ROW - CLT CML NUM               *           
* 15FEB94 07 EFJ -- FIX ERROR IN ROUTLIST ENTRY (OOPS...)           *           
* 16FEB94 08 EFJ -- DISPLAY IFRSPDTW AS RANGE, NOT JUST START DATE  *           
* 17FEB94 09 EFJ -- YET ANOTHER INFOMERCIAL RE-WORK                 *           
* 22FEB94 10 TCS -- ADD NETWORK TO CANADIAN MEDIA=* REQUEST         *           
* 22MAR94 11 EFJ -- SUPPORT FOR SECOND COST KEYWORDS                *           
*                -- MOVE CMML AND UDEF ROUTINES TO DRIVE3           *           
*                -- SET DICLT IN DATAIND5 WHERE IT BELONGS (NOT 8)  *           
* 28MAR94 12 EFJ -- SUPPORT FOR ATTCODE KEYWORD                     *           
* 08APR94 13 EFJ -- NEW KEYWORDS - ATIMEM & ADATER                  *           
*                -- MOVE OADATE, OATIME & OAPROG TO DRIVE3          *           
* 12APR94 14 EFJ -- SUPPORT ADJACENCY CODE COL FILTER               *           
* 19APR94 15 EFJ -- SUPPORT SPOT LENGTH COL FILTER                  *           
* 25APR94 16 EFJ -- NEW INFO COL KEYWORD                            *           
* 26APR94 17 EFJ -- MARKET WEIGHT KEYWORD                           *           
* 05MAY94 18 EFJ -- BYCDATE & GLCDATE KEYWORDS                      *           
* 13MAY94 19 EFJ -- BUYC2 KEYWORD (SAME AS BUYC WITH 2ND COST)      *           
* 18MAY94 20 EFJ -- COST2PS KEYWORD (COST2 PER SPOT)                *           
* 23MAY94 21 EFJ -- NEW KEYWORD - ADATED                            *           
* 23MAY94 22 EFJ -- NEW KEYWORD - STAFFCH                           *           
* 06JUN94 23 TCS -- SUPPORT NEW MAKEGOODS                           *           
* 15JUN94 24 TCS -- CORRECT BUG IN CPP FORMAT ROUTINE               *           
* 21JUN94 25 EFJ -- MERGE W/SPSYSDRIVA - NEW STACK KEYWORDS         *           
* 22JUN94 26 EFJ -- FIX MERGE BUG                                   *           
* 20JUL94 27 EFJ -- FIX BUG IN OUTMKT (TESTING)                     *           
*                                                                   *           
*                                                                   *           
*********************************************************************           
SPDRIVER CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**SPDV**,R7,R8,RR=R3                                           
         L     RA,0(R1)                                                         
         USING GLOBALD,RA                                                       
         L     RC,GLAWORKD                                                      
         USING GEND,RC                                                          
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
*                                                                               
         LA    R0,AXTRAN           SET EXTENTION ROUTINES                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         L     R1,=A(EXTRA)                                                     
         A     R1,RELO                                                          
         ST    R1,AXTRA(RE)                                                     
         STC   RF,AXTRA(RE)                                                     
         LA    RF,1(RF)                                                         
         LA    RE,4(RE)                                                         
         BCT   R0,*-16                                                          
*                                                                               
         CLI   GLHOOK,GLINIT       INITIALIZATION                               
         BE    INIT                                                             
*                                                                               
         CLI   GLHOOK,GLRESOLV     RESOLVE ADDRESSES                            
         BE    RESOLVE                                                          
*                                                                               
         CLI   GLHOOK,GLINCOMP     INTERNAL COMPUTES                            
         BE    INTCOMP                                                          
*                                                                               
         CLI   GLHOOK,GLROUT       EXECUTE ROUTINES                             
         BE    EXEC                                                             
*                                                                               
         CLI   GLHOOK,GLPUTSRT     PUT SORT RECORD                              
         BE    PUTSRT                                                           
*                                                                               
XIT      XIT1  ,                                                                
         SPACE 2                                                                
RELO     DS    F                   RELOCATION FACTOR                            
         EJECT                                                                  
* INITIALIZATION                                                                
*                                                                               
INIT     XC    SVREP,SVREP                                                      
         LA    R1,LEVELS           DETERMINE THE DETAIL LEVEL                   
         LA    R0,L'LEVELS                                                      
         SR    RE,RE                                                            
         CLI   0(R1),0                                                          
         BE    *+16                                                             
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,*-16                                                          
         STC   RE,DETLEV                                                        
         B     XIT                                                              
         EJECT                                                                  
* RESOLVE ROUTINE ADDRESSES                                                     
*                                                                               
RESOLVE  L     R1,=A(ROUTLIST)                                                  
*                                                                               
SR10     CLI   0(R1),FF                                                         
         BE    SRX                                                              
         CLC   0(8,R1),GLLABEL                                                  
         BE    SR20                                                             
         LA    R1,20(R1)                                                        
         B     SR10                                                             
*                                                                               
SR20     L     RF,8(R1)            ROUTINE ADDRESS                              
         A     RF,RELO                                                          
         ST    RF,GLAROUT                                                       
*                                                                               
         TM    12(R1),DIBYDEM+DIDEMNDX    TEST BUY DEMO INPUT LABEL             
         BNZ   SR22                                                             
         TM    13(R1),DIGLDEM      OR GOAL                                      
         BO    SR22                                                             
         TM    17(R1),DISTDEM      OR DEMO STACK                                
         BO    SR22                                                             
         TM    18(R1),DISLDEM      OR STATION LOCKIN DEMO                       
         BZ    SR30                                                             
*                                                                               
SR22     CLC   NDEMOS,GLARGS+1     YES - GLARGS+1 = DEMO NUMBER                 
         BNL   SR30                                                             
         MVC   NDEMOS,GLARGS+1     KEEP TRACK OF MAX DEMOS                      
*                                                                               
SR30     OC    DATAIND,12(R1)                                                   
         OC    DATAIND2,13(R1)                                                  
         OC    DATAIND3,14(R1)                                                  
         OC    DATAIND4,15(R1)                                                  
         OC    DATAIND5,16(R1)                                                  
         OC    DATAIND6,17(R1)                                                  
         OC    DATAIND7,18(R1)                                                  
         OC    DATAIND8,19(R1)                                                  
*                                                                               
SRX      B     XIT                                                              
         EJECT                                                                  
* CONTROL INTERNAL COMPUTES                                                     
*                                                                               
INTCOMP  LA    R1,ICOMPTAB                                                      
*                                                                               
INTCOMP2 CLI   0(R1),0             SEE IF ALLOW INTERNAL COMPUTE FOR            
         BE    XIT                 THIS OUTPUT ROUTINE                          
         CLC   GLLABEL,0(R1)                                                    
         BE    EXEC                                                             
         LA    R1,8(R1)                                                         
         B     INTCOMP2                                                         
         SPACE 2                                                                
ICOMPTAB DC    CL8'OCPP    '                                                    
         DC    CL8'ODCPP   '                                                    
         DC    CL8'OBYDEM  '                                                    
         DC    CL8'OGLDEM  '                                                    
         DC    CL8'OORDEM  '                                                    
         DC    CL8'OSLDEM  '                                                    
         DC    CL8'OSPT    '                                                    
         DC    CL8'OBYAR   '                                                    
         DC    CL8'OBILBL  '                                                    
         DC    X'00'                                                            
         EJECT                                                                  
* EXECUTING ROUTINES                                                            
*                                                                               
EXEC     MVC   WORK,BLANKS         PRESET WORK AREAS                            
         ZAP   DUB,=P'0'                                                        
         L     R2,GLAIFLD          R2=A(INPUT)                                  
         L     R3,GLAOFLD          R3=A(OUTPUT)                                 
         CLI   GLMODE,GLOUTPUT                                                  
         BE    SX12                                                             
*                                  ** INPUT ROUTINE **                          
         CLI   SBMODE,SBPROCBH     BILL HEADER?                                 
         BNE   EXEC5                                                            
         L     R4,SBAIO1           (R4) -> TO BILL HEADER RECORD                
         L     R5,AAORLK           ADDRESS OF AOR BLOCK                         
         B     EXEC10                                                           
*                                                                               
EXEC5    L     R4,SBACURCH         ADDRESS CURRENT CHUNK FOR INPUT              
         USING SCHUNKD,R4                                                       
         LR    R5,R4                                                            
         USING SGLCHNKD,R5                                                      
*                                                                               
EXEC10   CLC   GLLABEL(5),=C'IUDEF'  TEST USER DEFINITION FIELD                 
         BE    SX10                YES-SKIP ALL FILTERS                         
*                                                                               
         CLI   GLARGS,0            TEST ANY ARGS                                
         BE    SX05                                                             
         LA    R0,2                YES -                                        
         LA    RE,GLARGS           ARGS MAY HAVE RECORD TYPE(S)                 
         SR    R1,R1                                                            
*                                                                               
SX01     LA    RF,RECTAB           TEST THIS RECORD NEEDED FOR THIS             
*                                  INPUT FIELD                                  
SX02     CLI   0(RF),FF                                                         
         BE    SX04                                                             
         CLC   0(1,RE),0(RF)                                                    
         BNE   SX03                                                             
         CLI   1(RF),0             TEST ARGS NOT USED FOR RECORD TYPE           
         BE    SX05                YES - OK                                     
         CLC   SBMODE,1(RF)        NO - COMPARE RECORD TYPES                    
         BE    SX05                EQ - OK                                      
         LA    R1,1                                                             
         B     SX04                NE - CONTINUE TO SCAN ARGS                   
*                                                                               
SX03     LA    RF,2(RF)                                                         
         B     SX02                                                             
*                                                                               
SX04     LA    RE,1(RE)                                                         
         CLI   0(RE),0                                                          
         BE    *+8                                                              
         BCT   R0,SX01                                                          
         LTR   R1,R1                                                            
         BNZ   XIT                 REJECT                                       
         B     SX05                                                             
*                                                                               
RECTAB   DC    C'E',AL1(SBPROCES)        ESTIMATES                              
         DC    C'B',AL1(SBPROCSP)        BUYS                                   
         DC    C'G',AL1(SBPROCGL)        GOALS                                  
         DC    C'S',AL1(SBPROCBL)        BILLS                                  
         DC    C'H',AL1(SBPROCBH)        BILL HEADERS                           
         DC    C'I',AL1(SBPROCIN)        INFOMERCIALS                           
         DC    C'F',AL1(SBPROCSP)        BUY INFOMERCIALS                       
         DC    C'L',AL1(SBPROCSL)        STATION LOCKIN                         
         DC    C'X',AL1(0)               (USED FOR DEMO/DOLLAR INDEXES)         
         DC    C'N',AL1(0)               (USED FOR NAMES)                       
         DC    C'Q',AL1(0)               (USED FOR QTR PERIOD)                  
         DC    C'M',AL1(0)               (USED FOR MON PERIOD)                  
         DC    C'W',AL1(0)               (USED FOR WEEK PERIOD)                 
         DC    C'K',AL1(0)               (USED FOR STACK)                       
         DC    X'FF'                                                            
*                                                                               
SX05     ICM   R6,15,GLARGS+12     TEST PERIOD DATE                             
         BZ    SX08                                                             
         MVC   HALF,SCDATE                                                      
         CLI   SBMODE,SBPROCSP     YES - ONLY ACCEPT CHUNK IF                   
         BE    SX07                      DATE MATCHES                           
         MVC   HALF,SGDATE                                                      
         CLI   SBMODE,SBPROCGL                                                  
         BE    SX07                                                             
         L     R1,SBACHUNK                                                      
         MVC   HALF,0(R1)                                                       
         CLI   SBMODE,SBPROCES     AUTHORIZATION $                              
         BE    SX07                                                             
         CLI   SBMODE,SBPROCBL                                                  
         BNE   SX08                                                             
         LR    RE,R5               STATION BILL                                 
         USING STABELEM,RE                                                      
         MVC   HALF,STABBDT                                                     
         CLI   DATEOPT,DOBILL      TEST DATE=BD                                 
         BE    SX07                YES-USE DATE OF BILLING                      
         L     R1,ABILMNTH         NO-EXTRACT BILLING PERIOD START DATE         
         LA    R0,NBILMNTH            FROM BILLING MONTHS TABLE                 
*                                                                               
SX06     OC    0(6,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   STABPER,0(R1)                                                    
         BNE   *+14                                                             
         MVC   HALF,2(R1)                                                       
         B     SX07                                                             
         LA    R1,6(R1)                                                         
         BCT   R0,SX06                                                          
         DC    H'0'                                                             
         DROP  RE                                                               
*                                                                               
SX07     CLC   HALF,0(R6)                                                       
         BL    XIT                                                              
         CLC   HALF,2(R6)                                                       
         BH    XIT                                                              
*                                                                               
SX08     CLI   GLARGS+10,0         TEST DAYPART FILTER                          
         BE    SX08B                                                            
         TM    GLARGS+11,GLIIADJ   SEE IF IT'S REALLY ADJACENCY CODE            
         BZ    SX08A                NO                                          
*                                                                               
         CLI   SBMODE,SBPROCSP     TEST BUY RECORD                              
         BNE   SX08B                NO - SKIP CHECK                             
         L     RE,SBAIO1                                                        
         USING BUYRECD,RE                                                       
         CLC   BDPROGT,GLARGS+10                                                
         BNE   XIT                                                              
         B     SX08B                                                            
         DROP  RE                                                               
*                                                                               
SX08A    CLC   SBDPTCD,GLARGS+10                                                
         BNE   XIT                                                              
*                                                                               
SX08B    OC    GLARGS+7(3),GLARGS+7   TEST STATION OR PRODUCT FILTER            
         BZ    SX09A                                                            
         TM    GLARGS+11,GLIIPRD                                                
         BO    *+18                                                             
         CLC   SBBSTA,GLARGS+7     STATION FILTER                               
         BNE   XIT                                                              
         B     SX09A                                                            
         CLC   SBPRD,GLARGS+7      PRODUCT FILTER                               
         BNE   XIT                                                              
*                                                                               
SX09A    TM    GLARGS+11,GLIILEN   FILTER ON SPOT LENGTH?                       
         BZ    SX09B                NO                                          
         CLC   SBLEN,GLARGS+6                                                   
         BNE   XIT                                                              
*                                                                               
SX09B    CLI   SBMODE,SBPROCSP     TEST BUY RECORD                              
         BNE   SX10                                                             
         LA    R1,GLARGS           YES-                                         
         USING GLARGSD,R1                                                       
         TM    GLIIND,GLIIMAT      REJECT IF MATCHED FILTER AND SPOT IS         
         BZ    *+14                UNMATCHED                                    
         OC    SCADATE,SCADATE                                                  
         BZ    XIT                                                              
         TM    GLIIND,GLIIUNM      REJECT IF UNMATCHED FILTER AND SPOT          
         BZ    SX10                IS MATCHED                                   
         OC    SCADATE,SCADATE                                                  
         BNZ   XIT                                                              
         DROP  R1                                                               
*                                                                               
SX10     L     R1,GLADTENT         ADDRESS INPUT FIELD                          
         USING DRIND,R1                                                         
         MVC   GLMAXTLV,MAXTOTLV   SET MAX TOT LEVEL FOR DRIVER                 
         CLI   DRINLEV,1           TEST LEVEL ONE                               
         BH    SX11                                                             
         MVI   INDATA,0            YES - RESET DATA INDICATOR                   
         XC    LEVELSWS,LEVELSWS         RESET PUT TO SORT SWITCHES             
*                                                                               
SX11     L     R3,SBAIO1           R3=A(IO AREA)                                
         SR    R6,R6                                                            
         B     SXX                                                              
         DROP  R1                                                               
*                                  ** OUTPUT ROUTINE **                         
SX12     MVC   OUTAREA,BLANKS      PRESET SOME FIELDS FOR OUTPUT                
         L     R1,GLADTENT                                                      
         USING DROD,R1                                                          
         CLI   DROLTYP,C'N'        TEST NO PRINT                                
         BNE   SX14                                                             
         CLI   GLHOOK,GLINCOMP     YES-GO THROUGH IF INTERNAL COMPUTE           
         BE    SX14                                                             
         L     RF,=A(OBILBL)       EXIT NOW UNLESS IT'S BILLABLE                
         A     RF,RELO                                                          
         SR    RE,RE                                                            
         ICM   RE,7,GLAROUT+1                                                   
         CR    RF,RE                                                            
         BNE   XIT                                                              
*                                                                               
SX14     MVC   MYPOSO,DROPOS                                                    
         MVC   MYOLEN,DROLEN                                                    
         OC    TOTLEVS,TOTLEVS     TEST COUNTING REQUIRED                       
         BZ    SXX                                                              
         MVC   BYTE,COUNTLEV       YES-TEST THIS IS THE COUNT LEVEL             
         NI    BYTE,X'7F'                                                       
         CLC   DROLEV,BYTE                                                      
         BNE   SXX                                                              
         GOTO1 PUTCNTEL            YES-PUT OUT A COUNT ELEMENT                  
         DROP  R1                                                               
*                                                                               
SXX      L     RF,GLAROUT       ** BRANCH TO I/O ROUTINE **                     
         L     RE,=A(DRIVE3)       IS THE ROUTINE IN DRIVE3?                    
         SLL   RE,8                                                             
         SRL   RE,8                                                             
         SLL   RF,8                                                             
         SRL   RF,8                                                             
         CR    RF,RE                                                            
         BNL   SXX10                  YES, GO THERE                             
*                                                                               
         L     RE,=A(DRIVE2)       IS THE ROUTINE IN MAIN SECTION               
         SLL   RE,8                                                             
         SRL   RE,8                                                             
         CR    RF,RE                                                            
         BLR   RF                     YES-GO THERE                              
SXX10    L     R0,AGENOUT                                                       
         LA    R1,LEVELSWS                                                      
         BR    RE                  IN DRIVE2 OR DRIVE3                          
         EJECT                                                                  
* INPUT ROUTINES FOR ROWS                                                       
*                                                                               
ICOMPUTE ZAP   0(8,R2),=P'0'       COMPUTE                                      
         B     XIT                                                              
*                                                                               
IDUMMY   MVI   0(R2),0             DUMMY                                        
         B     XIT                                                              
*                                                                               
IMED     MVC   0(1,R2),SBQMED      MEDIA                                        
         CLI   SBQMED,C'*'                                                      
         BNE   XIT                                                              
         MVC   0(1,R2),SBMED                                                    
         B     XIT                                                              
*                                                                               
ISUBMED  MVC   0(1,R2),SBMED       SUBMEDIA                                     
         B     XIT                                                              
*                                                                               
ICGR1    MVC   0(2,R2),SBBCGR      CLIENT GROUP 1                               
         CLC   0(2,R2),=X'9999'                                                 
         BE    *+10                                                             
         OC    0(2,R2),SBCG1MSK                                                 
         LA    R1,SBCGR1NM                                                      
         B     ICGRNM                                                           
*                                                                               
ICGR2    MVC   0(2,R2),SBBCGR      CLIENT GROUP 2                               
         LA    R1,SBCGR2NM                                                      
*                                                                               
ICGRNM   CLI   GLARGS,C'N'                                                      
         BNE   XIT                                                              
         MVC   8(2,R2),0(R2)                                                    
         CLC   SBBCGR,=X'9999'                                                  
         BE    ICGRNM2                                                          
         MVC   DUB(2),SBBCGR                                                    
         MVC   SBBCGR,0(R2)                                                     
         GOTO1 GETCGRNM                                                         
         MVC   SBBCGR,DUB                                                       
         CLC   0(24,R1),=CL24'** UNKNOWN **'                                    
         BNE   ICGRNM4                                                          
ICGRNM2  MVC   0(8,R1),XFE                                                      
ICGRNM4  MVC   0(8,R2),0(R1)                                                    
         B     XIT                                                              
*                                                                               
ICLTOFF  MVC   0(1,R2),SBCOFF      CLIENT OFFICE                                
         B     XIT                                                              
*                                                                               
ICLT     MVC   0(3,R2),SBCLT       CLIENT                                       
         CLI   GLARGS,C'N'                                                      
         BNE   XIT                                                              
         MVC   0(8,R2),SBCLTNM                                                  
         MVC   8(3,R2),SBCLT                                                    
         B     XIT                                                              
*                                                                               
ICLTACOF MVC   0(2,R2),SBCACCOF   CLIENT ACCOUNTING OFFICE CODE                 
         B     XIT                                                              
*                                                                               
ICLTINT  MVC   0(8,R2),SBCLTIFC   CLIENT INTERFACE NUMBER                       
         B     XIT                                                              
*                                                                               
IPGR1    MVC   0(2,R2),SBBPGR      PRODUCT GROUP 1                              
         CLC   0(2,R2),=X'9999'                                                 
         BE    XIT                                                              
         OC    0(2,R2),SBPG1MSK                                                 
         B     XIT                                                              
*                                                                               
IPGR2    MVC   0(2,R2),SBBPGR      PRODUCT GROUP 2                              
         B     XIT                                                              
*                                                                               
IPRD     CLI   GLARGS,C'N'         PRODUCT                                      
         BNE   *+14                                                             
         MVC   0(8,R2),SBPRDNM                                                  
         LA    R2,8(R2)                                                         
         MVC   0(3,R2),SBPRD                                                    
         MVC   3(3,R2),SBPRD2                                                   
         CLC   SBPRD,=C'POL'                                                    
         BNE   XIT                                                              
         MVC   0(3,R2),XFD                                                      
         B     XIT                                                              
*                                                                               
IEDATES  DS    0H                  EST DATES                                    
         GOTO1 GETESTNM                                                         
         BNE   XIT                                                              
         MVC   0(L'SBESTSTP,R2),SBESTSTP                                        
         MVC   2(L'SBESTNDP,R2),SBESTNDP                                        
         B     XIT                                                              
*                                                                               
IEST     CLI   GLARGS,C'N'         ESTIMATE                                     
         BE    *+12                                                             
         CLI   GLARGS,C'D'                                                      
         BNE   IEST2                                                            
         GOTO1 GETESTNM            GET ESTIMATE DETAILS                         
         CLI   GLARGS,C'N'                                                      
         BE    IEST1                                                            
         ZIC   RE,GLARGS+1         DEMO                                         
         BCTR  RE,0                                                             
         MH    RE,=H'3'                                                         
         LA    RE,SBESTDEM(RE)                                                  
         MVC   3(3,R2),0(RE)                                                    
         B     IEST2                                                            
*                                                                               
IEST1    MVC   0(8,R2),SBESTNM     NAME                                         
         LA    R2,8(R2)                                                         
*                                                                               
IEST2    MVC   1(1,R2),SBBEST                                                   
         MVC   2(1,R2),SBBPRD                                                   
         TM    SBQPIND,SBQPOLES    TEST FORCE POOL ESTIMATES                    
         BO    *+12                                                             
         TM    DATAIND2,DIPRD      OR PRODUCT IS NOT A ROW                      
         BO    *+8                                                              
         MVI   2(R2),X'FF'         YES-THEN LUMP ALL ESTS UNDER POL             
         MVI   0(R2),0                                                          
         CLI   SBQSEPES,C'Y'                                                    
         BE    XIT                                                              
         MVC   1(1,R2),ESTSTART                                                 
         B     XIT                                                              
*                                                                               
IESTFLT  MVC   0(3,R2),SBESTFLT    ESTIMATE FILTERS                             
         B     XIT                                                              
*                                                                               
IRTLSCH  MVC   0(2,R2),SBRTLSCH    RETAIL SCHEME CODE                           
         B     XIT                                                              
*                                                                               
IMGR1    MVC   0(2,R2),SBBMGR      MARKET GROUP 1                               
         CLC   0(2,R2),=X'9999'                                                 
         BE    IMGR12                                                           
         CLC   0(2,R2),=X'9998'                                                 
         BE    IMGR12                                                           
         OC    0(2,R2),SBMG1MSK                                                 
IMGR12   LA    R1,SBMGR1NM                                                      
         B     IMGRNM                                                           
*                                                                               
IMGR2    MVC   0(2,R2),SBBMGR      MARKET GROUP 2                               
         CLC   0(2,R2),=X'9999'                                                 
         BE    IMGR22                                                           
         CLC   0(2,R2),=X'9998'                                                 
         BE    IMGR22                                                           
         OC    0(2,R2),SBMG2MSK                                                 
IMGR22   LA    R1,SBMGR2NM                                                      
         B     IMGRNM                                                           
*                                                                               
IMGR3    MVC   0(2,R2),SBBMGR      MARKET GROUP 3                               
         LA    R1,SBMGR3NM                                                      
*                                                                               
IMGRNM   CLI   GLARGS,C'N'                                                      
         BNE   XIT                                                              
         MVC   8(2,R2),0(R2)                                                    
         CLC   SBBMGR,=X'9999'                                                  
         BE    IMGRNM2                                                          
         CLC   SBBMGR,=X'9998'                                                  
         BE    IMGRNM2                                                          
         MVC   DUB(2),SBBMGR                                                    
         MVC   SBBMGR,0(R2)                                                     
         GOTO1 GETMGRNM                                                         
         MVC   SBBMGR,DUB                                                       
         CLC   0(24,R1),=CL24'** UNKNOWN **'                                    
         BNE   IMGRNM4                                                          
IMGRNM2  MVC   0(8,R1),XFE                                                      
IMGRNM4  MVC   0(8,R2),0(R1)                                                    
         B     XIT                                                              
*                                                                               
*                                                                               
IMKTR    MVC   0(2,R2),SBMRNUM     MARKET IN RANK ORDER                         
         LA    R2,2(R2)                                                         
*                                                                               
IMKT     MVC   0(2,R2),SBBMKT      MARKET                                       
         MVC   2(1,R2),MKTIND                                                   
         CLI   GLARGS,C'N'                                                      
         BNE   XIT                                                              
         MVC   8(3,R2),0(R2)                                                    
         MVC   0(8,R2),SBMKTNM                                                  
         B     XIT                                                              
*                                                                               
IMKTWT   DS    0H                  MARKET WEIGHT                                
         MVC   0(4,R2),SBMKTWGT                                                 
         B     XIT                                                              
*                                                                               
IMKTRNK  MVC   0(1,R2),SBMKTRNK    MARKET RANK                                  
         B     XIT                                                              
*                                                                               
ISGR1    MVC   0(2,R2),SBBSGR      STATION GROUP 1                              
         CLC   0(2,R2),=X'9999'                                                 
         BE    *+10                                                             
         OC    0(2,R2),SBSG1MSK                                                 
         LA    R1,SBSGR1NM                                                      
         B     ISGRNM                                                           
*                                                                               
ISGR2    MVC   0(2,R2),SBBSGR      STATION GROUP 2                              
         LA    R1,SBSGR2NM                                                      
*                                                                               
ISGRNM   CLI   GLARGS,C'N'                                                      
         BNE   XIT                                                              
         MVC   8(2,R2),0(R2)                                                    
         CLC   SBBSGR,=X'9999'                                                  
         BE    ISGRNM2                                                          
         MVC   DUB(2),SBBSGR                                                    
         MVC   SBBSGR,0(R2)                                                     
         GOTO1 GETSGRNM                                                         
         MVC   SBBSGR,DUB                                                       
         CLC   0(24,R1),=CL24'** UNKNOWN **'                                    
         BNE   ISGRNM4                                                          
ISGRNM2  MVC   0(8,R1),XFE                                                      
ISGRNM4  MVC   0(8,R2),0(R1)                                                    
         B     XIT                                                              
*                                                                               
ISTA     XC    0(8,R2),0(R2)                                                    
         MVC   0(5,R2),SBSTA                                                    
         CLI   SBMED,C'N'                                                       
         BNE   *+12                                                             
         MVI   4(R2),C'N'                                                       
         B     XIT                                                              
         CLC   SBCBLNET,BLANKS                                                  
         BNH   XIT                                                              
         MVC   5(3,R2),SBCBLNET                                                 
         B     XIT                                                              
*                                                                               
ICNET    MVC   0(3,R2),SBCBLNET    CABLE NETWORK                                
         CLC   SBCBLNET,BLANKS                                                  
         BH    XIT                                                              
         MVC   0(3,R2),XFE                                                      
         B     XIT                                                              
*                                                                               
ISTANET  MVC   0(8,R2),BLANKS      STATION/NETWORK                              
         MVC   0(4,R2),SBSTA                                                    
         CLI   SBMED,C'N'          TEST MEDIA=NETWORK                           
         BNE   XIT                                                              
         CLC   SBBMKT,=X'FFFE'     YES-IF MKT=NETWORKS                          
         BE    XIT                 THEN PASS NETWORK ONLY (=STATION)            
         MVC   4(4,R2),SBNETWK                                                  
         B     XIT                                                              
*                                                                               
INETSTA  MVC   0(4,R2),SBNETWK     NETWORK/STATION                              
         CLI   SBMED,C'T'                                                       
         BNE   *+10                                                             
         MVC   0(4,R2),=C'*TV*'                                                 
         MVC   4(4,R2),SBSTA                                                    
         TM    SBINDS,SBINETBL     TEST NETWORK BILLING                         
         BZ    XIT                                                              
         MVC   4(4,R2),BLANKS      YES-NETWORK ONLY                             
         B     XIT                                                              
*                                                                               
ISTANM   LA    R1,SYSD             STATION NAME                                 
         AH    R1,=Y(SBSTANM-SYSD)                                              
         MVC   0(24,R2),0(R1)                                                   
         B     XIT                                                              
*                                                                               
ISTACITY LA    R1,SYSD             STATION CITY                                 
         AH    R1,=Y(SBSTACTY-SYSD)                                             
         MVC   0(24,R2),0(R1)                                                   
         B     XIT                                                              
*                                                                               
ISTAST   MVC   0(3,R2),SBSTAST     STATION STATE                                
         B     XIT                                                              
*                                                                               
ISTAAD   LA    RE,SYSD             STATION ADDRESS                              
         LR    R1,RE                                                            
         AH    RE,=Y(SBSTANM-SYSD)                                              
         MVC   0(24,R2),0(RE)                                                   
         AH    R1,=Y(SBSTACTY-SYSD)                                             
         MVC   24(24,R2),0(R1)                                                  
         MVC   48(3,R2),SBSTAST                                                 
         B     XIT                                                              
*                                                                               
IAFFIL   MVC   0(3,R2),SBAFFIL     AFFILIATE                                    
         B     XIT                                                              
*                                                                               
ICHAN    MVC   0(4,R2),SBCHAN      CHANNEL                                      
         B     XIT                                                              
*                                                                               
ISIZE    MVC   0(1,R2),SBSIZE      SIZE                                         
         B     XIT                                                              
*                                                                               
IFORMAT  MVC   0(4,R2),SBSTAFOR    FORMAT                                       
         B     XIT                                                              
*                                                                               
ISTAFAX  MVC   0(12,R2),SBSTAFAX   STATION FAX                                  
         B     XIT                                                              
*                                                                               
IMSO     LR    R1,R9               CABLE SYSTEM MSO NAME                        
         AH    R1,=Y(SBCBLMSO-SYSD)                                             
         MVC   0(15,R2),0(R1)                                                   
         B     XIT                                                              
*                                                                               
IICNM    LR    R1,R9               CABLE SYSTEM INTERCONNECT NAME               
         AH    R1,=Y(SBCBLIC-SYSD)                                              
         MVC   0(20,R2),0(R1)                                                   
         B     XIT                                                              
*                                                                               
IDPT     MVC   0(4,R2),SBDPTGRP    DAYPART                                      
         MVC   4(4,R2),SBDPT                                                    
         B     XIT                                                              
*                                                                               
IDPTLEN  MVC   0(9,R2),SBDPTLEN    DAYPART/LENGTH                               
         B     XIT                                                              
*                                                                               
ILEN     MVC   0(1,R2),SBLEN       SPOT LENGTH                                  
         B     XIT                                                              
*                                                                               
*                                  RATING BOOK                                  
IBOOK    MVC   0(4,R2),=X'FFFF0000'  ESTIMATED                                  
         CLI   SBEDEMTY,C'R'       TEST DEMO TYPE IS RERATE OR AFFID            
         BE    *+12                                                             
         CLI   SBEDEMTY,C'A'                                                    
         BNE   XIT                                                              
         MVC   0(4,R2),SBQBOOK     YES-SET THE ACTUAL BOOK                      
         B     XIT                                                              
*                                                                               
ITGT     DS    0H                  TARGET DEMO                                  
         GOTO1 GETESTNM                                                         
         ZIC   R4,GLARGS                                                        
         BCTR  R4,0                                                             
         MH    R4,=H'3'                                                         
         LA    R4,SBESTDEM(R4)                                                  
         GOTO1 AGETTGT,DMCB,(R4),DEMONAME                                       
         MVC   0(7,R2),DEMONAME                                                 
         B     XIT                                                              
*                                                                               
ITEXT    MVI   0(R2),C' '          TEXT                                         
         B     XIT                                                              
*                                                                               
*                                                                               
IPER     LA    R6,SCDATE           PERIOD                                       
         CLI   SBMODE,SBPROCSP                                                  
         BE    IPER4                                                            
         LA    R6,SGDATE                                                        
         CLI   SBMODE,SBPROCGL                                                  
         BE    IPER4                                                            
         LA    R6,SLDATE-SSLCHNKD(R4)                                           
         CLI   SBMODE,SBPROCSL                                                  
         BE    IPER4                                                            
         L     R6,SBACHUNK                                                      
         CLI   SBMODE,SBPROCES                                                  
         BE    IPER4                                                            
         LA    R6,HALF                                                          
         CLI   SBMODE,SBPROCBL                                                  
         BE    *+14                                                             
         MVC   0(4,R2),XFD         DON'T BLOW UP                                
         B     XIT                                                              
         LR    RE,R5               BILLING RECORD-                              
         USING STABELEM,RE                                                      
         MVC   HALF,STABBDT                                                     
         CLI   DATEOPT,DOBILL      TEST DATE=BD                                 
         BE    IPER4               YES-USE BILLING DATE                         
         L     R1,ABILMNTH         NO-EXTRACT BILLING PERIOD START DATE         
         LA    R0,NBILMNTH            FROM BILLING MONTHS TABLE                 
*                                                                               
IPER2    OC    0(6,R1),0(R1)                                                    
         BZ    IPER12              NOT FOUND                                    
         CLC   STABPER,0(R1)                                                    
         BNE   *+14                                                             
         MVC   HALF,2(R1)                                                       
         B     IPER4                                                            
         LA    R1,6(R1)                                                         
         BCT   R0,IPER2                                                         
         DC    H'0'                                                             
         DROP  RE                                                               
*                                                                               
IPER4    XC    0(4,R2),0(R2)                                                    
         L     R1,ADAYS                                                         
         CLI   GLARGS,C'D'                                                      
         BE    IPER6                                                            
         L     R1,AWEEKS                                                        
         CLI   GLARGS,C'W'                                                      
         BE    IPER6                                                            
         CLI   GLARGS,C'F'         CHILD SPOT FLIGHTS USE WEEKS TABLE           
         BE    IPER6                                                            
         L     R1,AMONTHS                                                       
         CLI   GLARGS,C'M'                                                      
         BE    IPER6                                                            
         L     R1,AQTRS                                                         
         CLI   GLARGS,C'Q'                                                      
         BE    IPER6                                                            
         L     R1,AYEARS                                                        
         CLI   GLARGS,C'Y'                                                      
         BE    IPER6                                                            
         DC    H'0'                                                             
*                                                                               
IPER6    OC    0(4,R1),0(R1)                                                    
         BZ    IPER8                                                            
         CLC   0(2,R6),0(R1)                                                    
         BL    XIT                 BEFORE PERIOD                                
         CLC   0(2,R6),2(R1)                                                    
         BNH   IPER10                                                           
         LA    R1,4(R1)                                                         
         B     IPER6                                                            
*                                                                               
IPER8    MVC   0(4,R2),XFE         AFTER PERIOD                                 
         B     XIT                                                              
*                                                                               
IPER10   MVC   0(2,R2),0(R1)       PERIOD START                                 
         MVC   2(2,R2),2(R1)       PERIOD END                                   
         B     XIT                                                              
*                                                                               
IPER12   MVC   0(4,R2),XFD         PERIOD UNKNOWN                               
         B     XIT                                                              
         EJECT                                                                  
* INPUT ROUTINES FOR BUY DETAILS                                                
*                                                                               
IBUY     GOTO1 AIBUY                                                            
         B     XIT                                                              
         SPACE 2                                                                
ILINE    L     RE,SBAIO1           BUYLINE NUMBER                               
         USING BUYRECD,RE                                                       
         MVC   0(13,R2),XFE                                                     
         CLI   SBMODE,SBPROCSP                                                  
         BNE   XIT                                                              
         MVC   0(13,R2),BUYKEY                                                  
         B     XIT                                                              
         SPACE 2                                                                
IPROG    L     RE,SBAIO1           PROGRAM NAME                                 
         USING BUYRECD,RE                                                       
         MVC   0(17,R2),BLANKS                                                  
         MVC   0(11,R2),UNKNOWN                                                 
         CLI   SBMODE,SBPROCSP                                                  
         BNE   XIT                                                              
         MVC   0(17,R2),BDPROGRM                                                
         B     XIT                                                              
         DROP  RE                                                               
         SPACE 2                                                                
IROT     L     RE,SBAIO1           DAY ROTATION                                 
         USING BUYRECD,RE                                                       
         CLI   SBMODE,SBPROCSP                                                  
         BNE   XIT                                                              
         SR    R1,R1                                                            
         ICM   R1,8,BDDAY                                                       
         BZ    XIT                                                              
         STCM  R1,8,1(R2)                                                       
         LA    RF,1                                                             
IROT2    SLL   R1,1                                                             
         LTR   R1,R1                                                            
         BM    *+12                                                             
         LA    RF,1(RF)                                                         
         B     IROT2                                                            
         STC   RF,0(R2)                                                         
         B     XIT                                                              
         SPACE 2                                                                
ITIMES   L     RE,SBAIO1           TIMES                                        
         MVC   0(4,R2),XFE                                                      
         CLI   SBMODE,SBPROCSP                                                  
         BNE   XIT                                                              
         MVC   0(4,R2),BDTIMST                                                  
         OC    0(4,R2),0(R2)       MIDNIGHT?                                    
         BNZ   XIT                                                              
         MVI   0(R2),C'M'                                                       
         B     XIT                                                              
         DROP  RE                                                               
         SPACE  2                                                               
IBYID    L     RE,SBAIO1           BUY ID                                       
         USING BUYRECD,RE                                                       
         MVC   0(11,R2),UNKNOWN                                                 
         SR    R0,R0                                                            
         LA    R1,BDELEM                                                        
IBYID2   CLI   0(R1),0                                                          
         BE    XIT                                                              
         CLI   0(R1),X'70'                                                      
         BE    IBYID4                                                           
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     IBYID2                                                           
IBYID4   MVC   0(12,R2),3(R1)                                                   
         B     XIT                                                              
         SPACE 2                                                                
IADJ     CLI   SBMODE,SBPROCSP     PROGRAM ADJACENCY CODE                       
         BNE   XIT                                                              
         L     RE,SBAIO1                                                        
         MVC   0(1,R2),BDPROGT                                                  
         B     XIT                                                              
         DROP  RE                                                               
         SPACE 2                                                                
IBYTYPE  MVC   0(1,R2),SBBYTYPE    BUY TYPE                                     
         B     XIT                                                              
         SPACE 2                                                                
IPREP    MVC   0(3,R2),SBPREP      PAYING REP                                   
         MVC   SBREP,SBPREP                                                     
         B     IREPNM                                                           
*                                                                               
ITREP    MVC   0(3,R2),SBTREP      TIME SHEET REP                               
         MVC   SBREP,SBTREP                                                     
         B     IREPNM                                                           
*                                                                               
*                                  SPECIAL REP                                  
ISREP    CLI   SBMODE,SBPROCSP     TEST READING BUY RECORDS                     
         BNE   ISREP2                                                           
         CLI   GLARGS,C'B'         YES-TEST FROM BUY RECORD ONLY                
         BE    ISREP2              YES                                          
         CLC   SBSREP,ZEROS        NO-TEST REP=0                                
         BNE   ISREP2                                                           
         SR    RE,RE               YES-USE ESTIMATE'S SPECIAL REP               
         ICM   RE,3,SBESTSR                                                     
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SBSREP,DUB                                                       
ISREP2   MVC   0(3,R2),SBSREP                                                   
         MVC   SBREP,SBSREP                                                     
*                                                                               
IREPNM   CLI   GLARGS,C'N'                                                      
         BNE   XIT                                                              
         CLC   SVREP,SBREP                                                      
         BE    IREPNM2                                                          
         MVC   SVREP,SBREP                                                      
         MVC   SBREPNM,BLANKS                                                   
         CLC   SBREP,ZEROS                                                      
         BE    IREPNM2                                                          
         GOTO1 GETREPNM                                                         
         BE    IREPNM2                                                          
         GOTO1 AGETREP                                                          
         GOTO1 PUTREPNM                                                         
IREPNM2  MVC   0(22,R2),SBREPNM                                                 
         B     XIT                                                              
         SPACE 2                                                                
INAMES   MVC   0(12,R2),SBBYRNM    BUYER NAME                                   
         CLI   GLARGS+1,1                                                       
         BE    XIT                                                              
         MVC   0(12,R2),SBBLRNM    BILLER NAME                                  
         B     XIT                                                              
         SPACE 2                                                                
IADATE   MVC   0(2,R2),XFF         AFFIDAVIT DATE                               
         CLI   SBMODE,SBPROCSP                                                  
         BNE   XIT                                                              
         OC    SCADATE,SCADATE                                                  
         BZ    XIT                                                              
         MVC   0(2,R2),SCADATE                                                  
         B     XIT                                                              
         SPACE 2                                                                
IADAY    MVC   1(3,R2),=C'ZZZ'     AFFIDAVIT DAY                                
         MVI   0(R2),8                                                          
         CLI   SBMODE,SBPROCSP                                                  
         BNE   XIT                                                              
         OC    SCADATE,SCADATE                                                  
         BZ    XIT                                                              
         GOTO1 DATCON,DMCB,(2,SCADATE),DUB                                      
         GOTO1 GETDAY,(R1),DUB,FULL                                             
         CLC   FULL(3),BLANKS                                                   
         BE    XIT                                                              
         MVC   0(1,R2),0(R1)                                                    
         MVC   1(3,R2),FULL                                                     
         B     XIT                                                              
         SPACE 2                                                                
IATIME   MVC   0(2,R2),XFF         AFFIDAVIT TIME                               
         CLI   SBMODE,SBPROCSP                                                  
         BNE   XIT                                                              
         OC    SCATIME,SCATIME                                                  
         BZ    XIT                                                              
         MVC   0(2,R2),SCATIME                                                  
         B     XIT                                                              
*                                                                               
IAPROG   MVC   0(16,R2),BLANKS     AFFIDAVIT PROGRAM                            
         MVC   0(11,R2),UNKNOWN                                                 
         CLI   SBMODE,SBPROCSP                                                  
         BNE   XIT                                                              
         MVC   0(16,R2),SBLKPROG                                                
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------- BILL HEADER INPUT ROUTINES                    
         SPACE 2                                                                
         DROP  R4,R5                                                            
         USING BILLRECD,R4                                                      
         USING MAAORLKD,R5                                                      
         SPACE 2                                                                
IBHINV   CLI   GLARGS+2,C'M'       INVOICE NUMBER                               
         BNE   *+14                                                             
         MVC   0(1,R2),SBMED                                                    
         LA    R2,1(R2)                                                         
         MVC   2(4,R2),BINVNO+2                                                 
         ZIC   RF,BKEYMBIL                                                      
         SLL   RF,28                                                            
         SRL   RF,28               RF=BILL MONTH                                
         SR    RE,RE                                                            
         LA    R6,SYSD                                                          
         AH    R6,=Y(SBB1XPRF-SYSD)                                             
         SR    R1,R1                                                            
         ICM   R1,1,4(R6)          TEST INVOICE NUMBER BASE YEAR                
         BZ    IBHINV2                                                          
         PACK  DUB,BDATE(2)        YES-TEST BILL YEAR IS BEYOND BASE            
         CVB   RE,DUB                                                           
         SR    RE,R1                                                            
         BNP   IBHINV2                                                          
         MH    RE,=H'12'           YES-CALCULATE MONTH RELATIVE TO BASE         
*                                                                               
IBHINV2  AR    RF,RE               FIRST TWO CHARS ARE THE MONTH                
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(2,R2),DUB                                                      
         B     IBHX                                                             
*                                                                               
*                                                                               
IBHRETL  CLI   BRETAIL,0          IF NON-RETAIL BILL                            
         BE    *+10               THEN RETAIL ACC NUMBER NOT SET                
         MVC   0(12,R2),BRETACCT  RETAIL ACCOUNT                                
         B     IBHX                                                             
*                                                                               
*                                                                               
ISTATYP  CLI   BRETAIL,0          IF RETAIL BILL                                
         BE    *+10               THEN STATION TYPE NOT SET                     
         MVC   0(1,R2),BLMED      STATION TYPE                                  
         B     IBHX                                                             
*                                                                               
*                                                                               
IBHTYPE  MVC   0(4,R2),BLANKS     BILL TYPE                                     
         MVC   0(2,R2),BTYPE      B4-B7                                         
         TM    BILSTAT,BSTSCOMQ   TEST SEPARATE COMMISSION BILL                 
         BZ    *+14                                                             
         MVC   0(3,R2),=C'COM'                                                  
         B     IBHTYPE2                                                         
         TM    BILSTAT,BSTTAORQ   TEST AOR                                      
         BZ    IBHX                                                             
         MVC   0(3,R2),=C'AOR'                                                  
IBHTYPE2 MVC   3(1,R2),BTYPE+1                                                  
         B     IBHX                                                             
*                                                                               
*                                                                               
IAORTYP  TM    BILSTAT,BSTTAORQ   TRUE AOR BILL?                                
         BNO   *+12                                                             
         MVI   0(R2),C'T'         --YES                                         
         B     IAORTYPX                                                         
         TM    BILSTAT,BSTCAORQ   AOR CLIENT BILL?                              
         BNO   *+8                                                              
         MVI   0(R2),C'C'         --YES                                         
IAORTYPX B     IBHX                                                             
*                                                                               
*                                                                               
ICOMTYP  TM    BILSTAT,BSTCMONQ   COMMISSION ONLY BILL?                         
         BNO   *+12                                                             
         MVI   0(R2),C'O'         --YES                                         
         B     ICOMTYPX                                                         
         TM    BILSTAT,BSTSADJQ   COMMISSION ADJUSTMENT INV?                    
         BNO   *+8                                                              
         MVI   0(R2),C'A'         --YES                                         
ICOMTYPX B     IBHX                                                             
*                                                                               
*                                                                               
IBHGRS   LA    R6,B4                                                            
         MVC   0(4,R2),SBBILGRS    GROSS AMOUNT                                 
         BAS   RE,BHROUND                                                       
         B     IBHX                                                             
*                                                                               
*                                                                               
IBHNET   LA    R6,B4                                                            
         MVC   0(4,R2),SBBILNET    NET AMOUNT                                   
         BAS   RE,BHROUND                                                       
         B     IBHX                                                             
*                                                                               
*                                                                               
IBHACT   LA    R6,B4                                                            
         ICM   RE,15,BILLCOST      ACTUAL AMOUNT                                
         ICM   RF,15,BILLGST                                                    
         SRA   RF,8                                                             
         AR    RE,RF               ADD GST IF ANY                               
         ST    RE,0(R2)                                                         
         BAS   RE,BHROUND                                                       
         B     IBHX                                                             
*                                                                               
*                                                                               
IBHAGY   LA    R6,B4                                                            
         ICM   RE,15,BILLCOST                                                   
         TM    BILSTAT,BSTTAORQ   IF TRUE AOR BILL                              
         BO    IBHAGY5            AGY COM = ACTUAL AMOUNT                       
         ICM   RF,15,SBBILNET     OTHERWISE                                     
         SR    RE,RF              AGY COM = ACTUAL-NET AMOUNT                   
IBHAGY5  STCM  RE,15,0(R2)                                                      
         BAS   RE,BHROUND                                                       
         B     IBHX                                                             
*                                                                               
*                                                                               
IBHTAX   LA    R6,B4                                                            
         MVC   0(4,R2),SBBILTAX   TAX AMOUNT                                    
         BAS   RE,BHROUND                                                       
         B     IBHX                                                             
*                                                                               
*                                                                               
IBHGST   LA    R6,B4                                                            
         ICM   R1,15,BILLGST      GST AMOUNT                                    
         SRA   R1,8                                                             
         ST    R1,0(R2)                                                         
         BAS   RE,BHROUND                                                       
         B     IBHX                                                             
*                                                                               
*                                 CLIENT CURRENCY FIELDS                        
IBHCGRS  LA    R6,B4                                                            
         MVC   0(4,R2),BCCGRS     GROSS                                         
         BAS   RE,BHROUND                                                       
         B     IBHX                                                             
*                                                                               
*                                                                               
IBHCNET  LA    R6,B4                                                            
         MVC   0(4,R2),BCCNET     NET AMOUNT                                    
         BAS   RE,BHROUND                                                       
         B     IBHX                                                             
*                                                                               
*                                                                               
IBHCACT  LA    R6,B4                                                            
         MVC   0(4,R2),BCCACT     ACTUAL AMOUNT                                 
         BAS   RE,BHROUND                                                       
         B     IBHX                                                             
*                                                                               
*                                                                               
IBHCTAX  LA    R6,B4                                                            
         MVC   0(4,R2),BCCTAX    TAX AMOUNT                                     
         BAS   RE,BHROUND                                                       
         B     IBHX                                                             
*                                                                               
*                                                                               
IBHPRDI  MVC   0(4,R2),SBPRDINT   PRODUCT INTERFACE CODE                        
         B     IBHX                                                             
*                                                                               
*                                                                               
IAOREFDT TM    BILSTAT,BSTTAORQ   IF AOR BILL - (CLIENT OR TRUE)                
         BO    IAOREFD5                                                         
         TM    BILSTAT,BSTCAORQ                                                 
         BNO   IBHX                                                             
IAOREFD5 MVC   0(2,R2),MAAOREFD   SET -AOR EFFECTIVE DATE                       
         B     IBHX                                                             
*                                                                               
*                                                                               
IAORBAS  MVC   0(1,R2),MAAORBAS   AOR BASIS                                     
         B     IBHX                                                             
*                                                                               
*                                                                               
IAORAMT  TM    BILSTAT,BSTCAORQ   DON'T SHOW FOR CLIENT BILL                    
         BO    IBHX                                                             
         MVC   0(4,R2),MAAORAMT   AOR AMOUNT                                    
         BAS   RE,BHROUND                                                       
         B     IBHX                                                             
*                                                                               
*                                                                               
IAORAGYN MVC   0(30,R2),MAAORAGY   AOR AGENCY NAME                              
         B     IBHX                                                             
*                                                                               
*                                                                               
IAORINCA MVC   0(14,R2),MAAORCOM   AOR INCOME/COMMISSION ACCOUNT                
         B     IBHX                                                             
*                                                                               
*                                                                               
IAORPCT  MVC   0(4,R2),MAAORPCT   AOR PERCENTAGE                                
         B     IBHX                                                             
*                                                                               
*                                                                               
IAORRPA  MVC   0(14,R2),MAAORRCV   AOR RECEIVABLE/PAYABLE ACCOUNT               
         B     IBHX                                                             
*                                                                               
*                                                                               
IBHSING  ICM   R1,3,SINGNUM       UNIQUE NUMBER FOR BILL HEADER RECORDS         
         LA    R1,1(R1)                                                         
         STCM  R1,3,SINGNUM                                                     
         MVC   0(2,R2),SINGNUM                                                  
         B     IBHX                                                             
*                                                                               
*                                                                               
IBHIDT   MVC   0(6,R2),BQDATE     INVOICE DATE                                  
         B     IBHX                                                             
*                                                                               
*                                                                               
IBHRDT   MVC   0(6,R2),BDATE      RUN DATE                                      
         B     IBHX                                                             
*                                                                               
*                                                                               
IBHDDT   MVC   0(3,R2),BDUEDATE   DUE DATE                                      
         B     IBHX                                                             
*                                                                               
*                                                                               
IBHPDT   MVC   0(2,R2),BILPOST    POST DATE                                     
         B     IBHX                                                             
*                                                                               
*                                                                               
IBHSDT   MVC   0(4,R2),BMONSERV   MONTH OF SERVICE                              
         B     IBHX                                                             
         EJECT                                                                  
* BILL HEADER INPUT ROUTINE EXIT                                                
*                                                                               
IBHX     LTR   R6,R6              TEST FOR ROW OR COLUMN                        
         BZ    XIT                                                              
         CLI   INDATA,0           COLUMN-TEST ANY COL DATA YET                  
         BNE   XIT                                                              
         ZIC   RE,0(R6)                  NO - TEST ANY DATA IN THIS COL         
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),1(R6)                                                    
         BE    *+8                                                              
         MVI   INDATA,1            YES                                          
         B     XIT                                                              
         SPACE 2                                                                
*                                  ** ROUNDING ROUTINE **                       
BHROUND  TM    COLIND,COLIRND      TEST ROUNDING REQUIRED                       
         BZR   RE                                                               
         L     R0,0(R2)            YES                                          
         SRDA  R0,31                                                            
         D     R0,=F'100'                                                       
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         ST    R1,0(R2)                                                         
         BR    RE                                                               
         EJECT                                                                  
* OUTPUT ROUTINES                                                               
*                                                                               
OMED     MVC   LABLAREA(5),=C'MEDIA'       ** MEDIA **                          
         MVC   SBMED,0(R2)                                                      
         CLI   GLARGS,C'N'         NAME ONLY                                    
         BE    OMED2                                                            
         MVC   CODEAREA(1),0(R2)                                                
         CLI   GLARGS,C'C'         CODE ONLY                                    
         BE    GENOUT                                                           
         CLI   GLARGS,C'W'         WARNER-LAMBERT                               
         BNE   OMED2                                                            
         MVI   CODEAREA,C'S'                                                    
         MVC   CODEAREA+1(1),0(R2)                                              
         B     GENOUT                                                           
*                                                                               
OMED2    LA    RE,SBMEDNM                                                       
         LA    R1,SYSD                                                          
         CLI   SBQMED,C'*'         TEST MEDIA ALL REQUEST                       
         BNE   OMED4                                                            
         CLI   0(R2),C'T'          YES-                                         
         BE    OMED4                                                            
         LR    RE,R1                                                            
         CLI   0(R2),C'R'                                                       
         BNE   *+12                                                             
         AH    RE,=Y(SBMEDNMR-SYSD)                                             
         B     OMED4                                                            
         CLI   0(R2),C'N'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         AH    RE,=Y(SBMEDNMN-SYSD)                                             
*                                                                               
OMED4    MVC   NAMEAREA(L'SBMEDNM),0(RE)                                        
         AH    R1,=Y(SBCMEDNM-SYSD)                                             
         CLC   0(L'SBCMEDNM,R1),BLANKS   TEST SPECIAL MED NAME FOR CLT          
         BNH   GENOUT                                                           
         MVC   NAMEAREA(L'SBCMEDNM),0(R1)  YES                                  
         B     GENOUT                                                           
         SPACE 2                                                                
OSUBMED  MVC   LABLAREA(6),=C'TV/NET'      ** SUBMEDIA **                       
         MVC   CODEAREA(1),0(R2)                                                
         B     GENOUT                                                           
         EJECT                                                                  
* CLIENT OUTPUT ROUTINES                                                        
*                                                                               
         SPACE 2                                                                
OCLTOFF  MVC   LABLAREA(13),=C'CLIENT OFFICE'  ** CLIENT OFFICE **              
         MVC   CODEAREA(1),0(R2)                                                
         B     GENOUT                                                           
         SPACE 2                                                                
OCLTINT  MVC   LABLAREA(14),=C'INTERFACE CODE'  CLIENT INTERFACE CODE           
         MVC   CODEAREA(8),0(R2)                                                
         B     GENOUT                                                           
         SPACE 2                                                                
OCLTACOF MVC   LABLAREA(14),=C'ACCOUNT OFFICE' ACCOUNTING OFFICE CODE           
         MVC   CODEAREA(2),0(R2)                                                
         B     GENOUT                                                           
         SPACE 2                                                                
OCGR1    CLI   GLARGS,C'N'         ** CLIENT GROUP 1 **                         
         BNE   *+8                                                              
         LA    R2,8(R2)                                                         
         CLC   0(2,R2),XFF                                                      
         BE    GENOUT                                                           
         MVC   SBBCGR,0(R2)                                                     
         MVC   LABLAREA(L'SBCGR1BK),SBCGR1BK                                    
         MVC   CODEAREA(1),SBQCGRD                                              
         LA    R1,SBCGR1LN                                                      
         BAS   RE,OGRPCODE                                                      
         CLI   GLARGS,C'C'                                                      
         BE    GENOUT                                                           
         CLC   NAMEAREA(L'UNKNOWN),UNKNOWN                                      
         BE    GENOUT                                                           
         GOTO1 GETCGRNM,SBCGR1NM                                                
         MVC   NAMEAREA(L'SBCGR1NM),SBCGR1NM                                    
         B     GENOUT                                                           
         SPACE 2                                                                
OCGR2    CLI   GLARGS,C'N'         ** CLIENT GROUP 2 **                         
         BNE   *+8                                                              
         LA    R2,8(R2)                                                         
         CLC   0(2,R2),XFF                                                      
         BE    GENOUT                                                           
         MVC   SBBCGR,0(R2)                                                     
         MVC   LABLAREA(L'SBCGR2BK),SBCGR2BK                                    
         MVC   CODEAREA(1),SBQCGRD                                              
         LA    R1,SBCGR2LN                                                      
         BAS   RE,OGRPCODE                                                      
         CLI   GLARGS,C'C'                                                      
         BE    GENOUT                                                           
         CLC   NAMEAREA(L'UNKNOWN),UNKNOWN                                      
         BE    GENOUT                                                           
         GOTO1 GETCGRNM,SBCGR2NM                                                
         MVC   NAMEAREA(L'SBCGR2NM),SBCGR2NM                                    
         B     GENOUT                                                           
         SPACE 2                                                                
OCLT     CLI   GLARGS,C'N'         ** CLIENT **                                 
         BNE   *+8                                                              
         LA    R2,8(R2)                                                         
         CLC   0(3,R2),XFF         TEST DETAIL TOTAL                            
         BE    GENOUT                                                           
         MVC   LABLAREA(6),=C'CLIENT'                                           
         MVC   SBCLT,0(R2)                                                      
         CLI   GLARGS,C'N'                                                      
         BE    OCLT2                                                            
         MVC   CODEAREA(3),SBCLT                                                
         CLI   GLARGS,C'C'                                                      
         BE    GENOUT                                                           
OCLT2    GOTO1 GETCLTNM                                                         
         MVC   NAMEAREA(L'SBCLTNM),SBCLTNM                                      
         B     GENOUT                                                           
         EJECT                                                                  
* STATION OUTPUT ROUTIMES                                                       
*                                                                               
         SPACE 1                                                                
OSGR1    CLI   GLARGS,C'N'         ** STATION GROUP 1 **                        
         BNE   *+8                                                              
         LA    R2,8(R2)                                                         
         CLC   0(2,R2),XFF                                                      
         BE    GENOUT                                                           
         MVC   SBBSGR,0(R2)                                                     
         MVC   LABLAREA(L'SBSGR1BK),SBSGR1BK                                    
         MVC   CODEAREA(1),SBQSGRD                                              
         LA    R1,SBSGR1LN                                                      
         BAS   RE,OGRPCODE                                                      
         CLI   GLARGS,C'C'                                                      
         BE    GENOUT                                                           
         CLC   NAMEAREA(L'UNKNOWN),UNKNOWN                                      
         BE    GENOUT                                                           
         GOTO1 GETSGRNM,SBSGR1NM                                                
         MVC   NAMEAREA(L'SBSGR1NM),SBSGR1NM                                    
         B     GENOUT                                                           
*                                                                               
OSGR2    CLI   GLARGS,C'N'         ** STATION GROUP 2 **                        
         BNE   *+8                                                              
         LA    R2,8(R2)                                                         
         CLC   0(2,R2),XFF                                                      
         BE    GENOUT                                                           
         MVC   SBBSGR,0(R2)                                                     
         MVC   LABLAREA(L'SBSGR2BK),SBSGR2BK                                    
         MVC   CODEAREA(1),SBQSGRD                                              
         LA    R1,SBSGR2LN                                                      
         BAS   RE,OGRPCODE                                                      
         CLI   GLARGS,C'C'                                                      
         BE    GENOUT                                                           
         CLC   NAMEAREA(L'UNKNOWN),UNKNOWN                                      
         BE    GENOUT                                                           
         GOTO1 GETSGRNM,SBSGR2NM                                                
         MVC   NAMEAREA(L'SBSGR2NM),SBSGR2NM                                    
         B     GENOUT                                                           
*                                                                               
OSTA     DS    0H                                                               
         TM    SBMODE,SBPROCIN                                                  
         BZ    *+24                                                             
         CLC   0(5,R2),CAA         ALL STATION (IN INFTAB)?                     
         BH    *+14                                                             
         MVC   CODEAREA(3),=C'ALL'                                              
         B     GENOUT                                                           
*                                                                               
         CLC   0(5,R2),CAA         TEST 'ALL' STATIONS                          
         BNE   *+12                                                             
         MVI   PRTSW,C'N'          YES - DON'T PRINT THIS LINE                  
         B     XIT                                                              
         CLC   0(5,R2),XFF         TEST DETAIL TOTAL                            
         BE    GENOUT                                                           
         MVC   LABLAREA(7),=C'STATION'                                          
         CLI   GLARGS,C'N'                                                      
         BNE   *+10                                                             
         MVC   LABLAREA(7),=C'NETWORK'                                          
         MVC   SBSTA,0(R2)                                                      
         MVC   CODEAREA(4),0(R2)                                                
         CLI   4(R2),C'N'          TEST NETWORK                                 
         BE    GENOUT                                                           
         OC    5(3,R2),5(R2)       TEST CABLE NETWORK                           
         BZ    OSTA2                                                            
         MVI   CODEAREA+4,C'/'                                                  
         MVC   CODEAREA+5(3),5(R2)                                              
         B     GENOUT                                                           
OSTA2    MVI   CODEAREA+4,C'-'                                                  
         MVC   CODEAREA+5(1),4(R2)                                              
         MVI   CODEAREA+6,C'M'                                                  
         CLI   4(R2),C'T'                                                       
         BE    *+12                                                             
         CLI   4(R2),C' '                                                       
         BH    GENOUT                                                           
         MVC   CODEAREA+5(2),=C'TV'                                             
         B     GENOUT                                                           
*                                                                               
*                                  ** CABLE NETWORK **                          
OCNET    MVC   LABLAREA(13),=C'CABLE NETWORK'                                   
         MVC   CODEAREA(3),0(R2)                                                
         CLC   0(3,R2),XFE                                                      
         BNE   GENOUT                                                           
         MVC   CODEAREA(3),=C'???'                                              
         B     GENOUT                                                           
*                                                                               
OSTANET  CLC   0(4,R2),XFF         ** STATION/NETWORK **                        
         BE    GENOUT                                                           
         CLC   0(4,R2),CAA         TEST 'ALL' STATIONS                          
         BNE   *+12                                                             
         MVI   PRTSW,C'N'          YES - DON'T PRINT THIS LINE                  
         B     XIT                                                              
         MVC   LABLAREA(7),=C'STATION'                                          
         CLI   GLARGS,C'N'                                                      
         BNE   *+10                                                             
         MVC   LABLAREA(7),=C'NETWORK'                                          
         MVC   CODEAREA(4),0(R2)                                                
         CLC   4(4,R2),BLANKS                                                   
         BE    GENOUT                                                           
         MVI   CODEAREA+4,C'/'                                                  
         MVC   CODEAREA+5(4),4(R2)                                              
         B     GENOUT                                                           
*                                                                               
ONETSTA  CLC   0(4,R2),XFF         ** NETWORK/STATION **                        
         BE    GENOUT                                                           
         MVC   LABLAREA(9),=C'NETWK/STA'                                        
         MVC   CODEAREA(4),0(R2)                                                
         CLC   4(4,R2),BLANKS                                                   
         BE    GENOUT                                                           
         MVI   CODEAREA+4,C'/'                                                  
         MVC   CODEAREA+5(4),4(R2)                                              
         CLC   4(4,R2),XFF                                                      
         BNE   GENOUT                                                           
         MVC   CODEAREA+5(4),=C'*ALL'                                           
         B     GENOUT                                                           
*                                                                               
OSTANM   MVC   LABLAREA(12),=C'STATION NAME'   ** STATION NAME **               
         MVC   NAMEAREA(9),=C'*UNKNOWN*'                                        
         CLI   0(R2),X'FE'                                                      
         BE    GENOUT                                                           
         MVC   NAMEAREA(24),0(R2)                                               
         B     GENOUT                                                           
*                                                                               
OSTACITY MVC   LABLAREA(12),=C'STATION CITY'   ** STATION CITY **               
         MVC   NAMEAREA(9),=C'*UNKNOWN*'                                        
         CLI   0(R2),X'FE'                                                      
         BE    GENOUT                                                           
         MVC   NAMEAREA(24),0(R2)                                               
         B     GENOUT                                                           
*                                                                               
OSTAST   MVC   LABLAREA(13),=C'STATION STATE'   ** STATION STATE **             
         MVC   NAMEAREA(3),=C'???'                                              
         CLI   0(R2),X'FE'                                                      
         BE    GENOUT                                                           
         MVC   NAMEAREA(3),0(R2)                                                
         B     GENOUT                                                           
*                                                                               
OSTAAD   MVC   0(55,R3),BLANKS     ** STATION ADDRESS **                        
         CLI   0(R2),X'FE'                                                      
         BNE   *+14                                                             
         MVC   0(9,R3),=C'*UNKNOWN*'                                            
         B     XIT                                                              
         MVC   0(24,R3),0(R2)                                                   
         LA    R1,23(R3)                                                        
         BAS   RE,OSTAAD2                                                       
         MVC   0(24,R1),24(R2)                                                  
         LA    R1,23(R1)                                                        
         BAS   RE,OSTAAD2                                                       
         MVC   0(3,R1),48(R2)                                                   
         B     XIT                                                              
OSTAAD2  CLI   0(R1),C' '                                                       
         BH    *+10                                                             
         BCTR  R1,0                                                             
         B     OSTAAD2                                                          
         MVI   1(R1),C','                                                       
         MVI   2(R1),C' '                                                       
         LA    R1,3(R1)                                                         
         BR    RE                                                               
*                                  ** AFFILIATE **                              
OAFFIL   CLC   0(3,R2),=C'ALL'     TEST 'ALL' AFFILIATES                        
         BNE   *+12                                                             
         MVI   PRTSW,C'N'          YES - DON'T PRINT THIS LINE                  
         B     XIT                                                              
         CLC   0(3,R2),XFF         TEST DETAIL TOTAL                            
         BE    GENOUT                                                           
         MVC   LABLAREA(9),=C'AFFILIATE'                                        
         MVC   CODEAREA(3),0(R2)                                                
         B     GENOUT                                                           
*                                  ** CHANNEL **                                
OCHAN    CLI   0(R2),X'FF'                                                      
         BNE   OCHAN2                                                           
         OC    1(3,R2),1(R2)       TEST 'ALL' CHANNELS                          
         BNZ   *+12                                                             
         MVI   PRTSW,C'N'          YES - DON'T PRINT THIS LINE                  
         B     XIT                                                              
         CLC   0(4,R2),XFF         TEST DETAIL TOTAL                            
         BE    GENOUT                                                           
OCHAN2   MVC   LABLAREA(7),=C'CHANNEL'                                          
         MVC   CODEAREA(4),0(R2)                                                
         B     GENOUT                                                           
*                                  ** STATION SIZE **                           
OSIZE    MVC   LABLAREA(4),=C'SIZE'                                             
         MVC   CODEAREA(1),0(R2)                                                
         B     GENOUT                                                           
*                                  ** STATION FORMAT **                         
OFORMAT  MVC   LABLAREA(6),=C'FORMAT'                                           
         MVC   CODEAREA(4),0(R2)                                                
         B     GENOUT                                                           
*                                  ** STATION FAX **                            
OSTAFAX  MVC   LABLAREA(3),=C'FAX'                                              
         MVC   CODEAREA(12),0(R2)                                               
         B     GENOUT                                                           
*                                  ** CABLE SYSTEM MSO NAME **                  
OMSO     MVC   LABLAREA(8),=C'MSO NAME'                                         
         MVC   NAMEAREA(15),0(R2)                                               
         B     GENOUT                                                           
*                                  ** CABLE SYSTEM INTERCONNECT NAME **         
OICNM    MVC   LABLAREA(12),=C'INTERCONNECT'                                    
         MVC   NAMEAREA(20),0(R2)                                               
         B     GENOUT                                                           
         EJECT                                                                  
* MISC OUTPUT ROUTINES                                                          
*                                                                               
         SPACE 1                                                                
OBOOK    MVC   LABLAREA(4),=C'BOOK'   ** BOOK **                                
         CLC   0(2,R2),XFF                                                      
         BNE   *+14                                                             
         MVC   CODEAREA(5),=C'ESTIM'                                            
         B     GENOUT                                                           
         MVC   CODEAREA(3),0(R2)                                                
         CLC   0(3,R2),=C'ACT'                                                  
         BE    GENOUT                                                           
         MVC   DUB(4),0(R2)                                                     
         MVC   DUB+4(2),=C'01'                                                  
         GOTO1 DATCON,DMCB,(0,DUB),(6,CODEAREA)                                 
         B     GENOUT                                                           
*                                                                               
*                                  ** TARGET **                                 
OTGT     MVC   LABLAREA(6),=C'TARGET'                                           
         LA    R1,TGTDEM                                                        
         CLI   GLARGS,2                                                         
         BNE   *+12                                                             
         MVI   LABLAREA+6,C'2'                                                  
         LA    R1,TGTDEM2                                                       
         MVC   NAMEAREA(7),0(R2)                                                
         MVC   0(L'TGTDEM,R1),0(R2)                                             
         B     GENOUT                                                           
*                                                                               
* ESTIMATE DATES                                                                
OEDATES  DS    0H                                                               
         GOTO1 DATCON,DMCB,(2,0(R2)),(5,NAMEAREA)                               
         MVI   NAMEAREA+8,C'-'                                                  
         GOTO1 (RF),(R1),(2,2(R2)),(5,NAMEAREA+9)                               
         B     GENOUT                                                           
         EJECT                                                                  
* ESTIMATE OUTPUT ROUTINE                                                       
*                                                                               
OEST     CLI   GLARGS,C'N'                                                      
         BNE   *+8                                                              
         LA    R2,8(R2)                                                         
         CLC   0(3,R2),XFF         TEST DETAIL TOTAL                            
         BE    GENOUT                                                           
         MVC   LABLAREA(8),=C'ESTIMATE'                                         
         MVC   SBBEST,1(R2)                                                     
         CLI   SBBEST,0            TEST ESTIMATE=0                              
         BNE   OEST1                                                            
         TM    SBQREAD,SBQRDPG+SBQRDCLS  YES-OK FOR PG EST RECORDS              
         BNZ   OEST1                         AND CLEARANCE STATUS RECS          
         MVC   SBBEST,SBQEST       ESTIMATE MUST BE REQUEST START EST           
*                                                                               
OEST1    EDIT  SBBEST,(3,SBEST),FILL=0                                          
         CLI   SBQSEPES,C'Y'                                                    
         BE    OEST6                                                            
         CLC   SBQEST,SBQESTND                                                  
         BE    OEST6                                                            
         CLI   SBQEST,1                                                         
         BNE   OEST5                                                            
         CLI   SBQESTND,255                                                     
         BNE   OEST5                                                            
         MVC   CODEAREA(3),=C'ALL'                                              
         CLC   SBQESFLT,BLANKS     ESTIMATE FILTERING                           
         BNH   GENOUT                                                           
         MVC   NAMEAREA(10),=C'FILTERED ('                                      
         LA    R0,L'SBQESFLT                                                    
         LA    R1,SBQESFLT                                                      
         LA    RE,C'1'                                                          
         LA    RF,NAMEAREA+10                                                   
*                                                                               
OEST2    CLI   0(R1),C'*'                                                       
         BE    OEST4                                                            
         CLI   0(R1),C' '                                                       
         BE    OEST4                                                            
         STC   RE,0(RF)                                                         
         MVI   1(RF),C'='                                                       
         TM    0(R1),X'40'         TEST NEGATIVE FILTER                         
         BO    *+12                                                             
         MVI   2(RF),C'-'                                                       
         LA    RF,1(RF)                                                         
         MVC   2(1,RF),0(R1)                                                    
         OI    2(RF),X'40'         INSURE UPPER CASE                            
         MVI   3(RF),C','                                                       
         LA    RF,4(RF)                                                         
*                                                                               
OEST4    LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,OEST2                                                         
         BCTR  RF,0                                                             
         MVI   0(RF),C')'                                                       
         B     GENOUT                                                           
*                                                                               
OEST5    CLI   MYOLEN,7                                                         
         BNL   *+14                                                             
         MVC   CODEAREA(3),=C'ALL'                                              
         B     GENOUT                                                           
         MVC   CODEAREA(3),SBEST                                                
         MVI   CODEAREA+3,C'-'                                                  
         EDIT  SBQESTND,(3,SBEST),FILL=0                                        
         MVC   CODEAREA+4(3),SBEST                                              
         B     GENOUT                                                           
*                                                                               
OEST6    CLI   GLARGS,C'N'                                                      
         BE    OEST8                                                            
         MVC   CODEAREA(3),SBEST                                                
         CLI   GLARGS,C'C'                                                      
         BE    GENOUT                                                           
*                                                                               
OEST8    CLI   SBBEST,0            TEST EST=0                                   
         BNE   OEST10                                                           
         MVC   NAMEAREA(11),UNKNOWN                                             
         TM    SBQREAD,SBQRDPG                                                  
         BZ    GENOUT                                                           
         MVC   NAMEAREA(14),=C'* ESTIMATE 0 *'   FOR PG EST 0                   
         B     GENOUT                                                           
*                                                                               
OEST10   MVC   SBBPRD,2(R2)        GETESTNM NEEDS PRODUCT                       
         GOTO1 GETESTNM                                                         
         BNE   GENOUT                                                           
         CLI   GLARGS,C'D'                                                      
         BE    *+14                                                             
         MVC   NAMEAREA(L'SBESTNM),SBESTNM                                      
         B     GENOUT                                                           
         GOTO1 DATCON,DMCB,(2,SBESTSTP),(5,NAMEAREA)  EST DATES INSTEAD         
         MVI   NAMEAREA+8,C'-'                        OF NAME                   
         GOTO1 (RF),(R1),(2,SBESTNDP),(5,NAMEAREA+9)                            
         B     GENOUT                                                           
*                                                                               
OESTFLT  MVC   LABLAREA(11),=C'EST FILTERS'                                     
         MVC   CODEAREA(3),0(R2)                                                
         CLI   CODEAREA,C' '       MAKE SURE SQUASHER KEEPS BLANKS              
         BNE   *+8                                                              
         MVI   CODEAREA,1          (MAKE IT AN UNPRINTABLE CHARACTER)           
         CLI   CODEAREA+1,C' '                                                  
         BNE   *+8                                                              
         MVI   CODEAREA+1,1                                                     
         B     GENOUT                                                           
*                                                                               
ORTLSCH  MVC   LABLAREA(13),=C'RETAIL SCHEME'    RETAIL SCHEME CODE             
         MVC   CODEAREA(2),0(R2)                                                
         B     GENOUT                                                           
         EJECT                                                                  
* PRODUCT GROUP AND PRODUCT OUTPUT ROUTINES                                     
*                                                                               
OPGR1    CLC   0(2,R2),XFF                                                      
         BE    GENOUT                                                           
         MVC   SBBPGR,0(R2)                                                     
         MVC   LABLAREA(L'SBPGR1BK),SBPGR1BK                                    
         MVC   CODEAREA(1),SBQPGRD                                              
         LA    R1,SBPGR1LN                                                      
         BAS   RE,OGRPCODE                                                      
         CLI   GLARGS,C'C'                                                      
         BE    GENOUT                                                           
         CLC   NAMEAREA(L'UNKNOWN),UNKNOWN                                      
         BE    GENOUT                                                           
         GOTO1 GETPGRNM,SBPGR1NM                                                
         MVC   NAMEAREA(L'SBPGR1NM),SBPGR1NM                                    
         B     GENOUT                                                           
         SPACE 2                                                                
OPGR2    CLC   0(2,R2),XFF                                                      
         BE    GENOUT                                                           
         MVC   SBBPGR,0(R2)                                                     
         MVC   LABLAREA(L'SBPGR2BK),SBPGR2BK                                    
         MVC   CODEAREA(1),SBQPGRD                                              
         LA    R1,SBPGR2LN                                                      
         BAS   RE,OGRPCODE                                                      
         CLI   GLARGS,C'C'                                                      
         BE    GENOUT                                                           
         CLC   NAMEAREA(L'UNKNOWN),UNKNOWN                                      
         BE    GENOUT                                                           
         GOTO1 GETPGRNM,SBPGR2NM                                                
         MVC   NAMEAREA(L'SBPGR2NM),SBPGR2NM                                    
         B     GENOUT                                                           
         SPACE 2                                                                
OPRD     CLI   GLARGS,C'N'                                                      
         BNE   *+8                                                              
         LA    R2,8(R2)                                                         
         CLC   0(3,R2),XFF         TEST DETAIL TOTAL                            
         BE    GENOUT                                                           
         MVC   LABLAREA(7),=C'PRODUCT'                                          
         CLC   0(3,R2),XFE         TEST UNALLOCATED                             
         BNE   OPRD1                                                            
         MVI   SBBPRD,FF                                                        
         CLI   GLARGS,C'N'                                                      
         BE    *+10                                                             
         MVC   CODEAREA(3),=C'POL'                                              
         CLI   GLARGS,C'C'                                                      
         BE    OPRD4                                                            
         MVC   NAMEAREA(11),=C'UNALLOCATED'                                     
         B     OPRD4                                                            
*                                                                               
OPRD1    CLC   0(3,R2),XFD         TEST POL                                     
         BNE   *+10                                                             
         MVC   0(3,R2),=C'POL'                                                  
         CLI   GLARGS,C'N'                                                      
         BE    OPRD2                                                            
         MVC   CODEAREA(3),0(R2)                                                
         CLC   3(3,R2),BLANKS      TEST PIGGYBACK                               
         BNH   OPRD2                                                            
         MVI   CODEAREA+3,C'-'     YES                                          
         MVC   CODEAREA+4(3),3(R2)                                              
*                                                                               
OPRD2    L     R1,SBAPRDBF                                                      
         USING PRDBUFFD,R1                                                      
         LH    RE,=Y(PRDBUFFL)                                                  
         LA    R0,256                                                           
         CLC   PBALPH,0(R2)                                                     
         BE    *+16                                                             
         LA    R1,0(R1,RE)                                                      
         BCT   R0,*-14                                                          
         B     OPRD4                                                            
         MVC   SBBPRD,PBBCODE                                                   
         MVI   SBBPRD2,0                                                        
         MVC   SBPRDINT,PBINT                                                   
         CLI   GLARGS,C'C'                                                      
         BE    OPRD4                                                            
         MVC   NAMEAREA(L'PBNAME),PBNAME                                        
         CLC   3(3,R2),BLANKS      TEST PIGGYBACK                               
         BNH   OPRD4                                                            
         L     R1,SBAPRDBF         YES-GET BINARY CODE AND NAME                 
         LA    R0,256                                                           
         CLC   PBALPH,3(R2)                                                     
         BE    *+14                                                             
         LA    R1,0(R1,RE)                                                      
         BCT   R0,*-14                                                          
         DC    H'0'                                                             
         MVC   SBBPRD2,PBBCODE                                                  
         ZIC   RF,MYOLEN           FORMAT SECOND NAME                           
         CLI   GLARGS,C'N'                                                      
         BE    *+16                                                             
         CLI   MYPOSO,C'P'                                                      
         BNE   *+8                                                              
         SH    RF,=H'8'                                                         
         SRA   RF,1                                                             
         LTR   RF,RF                                                            
         BNP   OPRD4                                                            
         LA    RE,L'PBNAME                                                      
         CR    RF,RE                                                            
         BNH   *+6                                                              
         LR    RF,RE                                                            
         LA    R5,L'NAMEAREA                                                    
         SR    R5,RF                                                            
         BCTR  R5,0                                                             
         CR    R5,RE                                                            
         BNH   *+6                                                              
         LR    R5,RE                                                            
         BCTR  R5,0                                                             
         LA    RF,NAMEAREA(RF)                                                  
         MVI   0(RF),C'-'                                                       
         EX    R5,*+8                                                           
         B     OPRD4                                                            
         MVC   1(0,RF),PBNAME                                                   
*                                                                               
OPRD4    MVC   SBPRD,CODEAREA      SAVE PROUCT CODE                             
         MVC   SBPRDNM,NAMEAREA    AND NAME                                     
         MVC   SBPRD2,3(R2)        AND SECOND PRODUCT                           
         B     GENOUT                                                           
         DROP  R1                                                               
         EJECT                                                                  
* MARKET GROUP AND MARKET OUTPUT ROUTINES                                       
*                                                                               
OMGR1    CLI   GLARGS,C'N'                                                      
         BNE   *+8                                                              
         LA    R2,8(R2)                                                         
         CLC   0(2,R2),XFF                                                      
         BE    GENOUT                                                           
         MVC   LABLAREA(L'SBMGR1BK),SBMGR1BK                                    
         MVC   CODEAREA(1),SBQMGRD                                              
         LA    R1,SBMGR1LN                                                      
         BAS   RE,OGRPCODE                                                      
         CLI   GLARGS,C'C'                                                      
         BE    GENOUT                                                           
         CLC   NAMEAREA(L'UNKNOWN),UNKNOWN                                      
         BE    GENOUT                                                           
         CLC   NAMEAREA(L'NETWORKS),NETWORKS                                    
         BE    GENOUT                                                           
         MVC   SBBMGR,0(R2)                                                     
         GOTO1 GETMGRNM,SBMGR1NM                                                
         MVC   NAMEAREA(L'SBMGR1NM),SBMGR1NM                                    
         B     GENOUT                                                           
         SPACE 2                                                                
OMGR2    CLI   GLARGS,C'N'                                                      
         BNE   *+8                                                              
         LA    R2,8(R2)                                                         
         CLC   0(2,R2),XFF                                                      
         BE    GENOUT                                                           
         MVC   LABLAREA(L'SBMGR2BK),SBMGR2BK                                    
         MVC   CODEAREA(1),SBQMGRD                                              
         LA    R1,SBMGR2LN                                                      
         BAS   RE,OGRPCODE                                                      
         CLI   GLARGS,C'C'                                                      
         BE    GENOUT                                                           
         CLC   NAMEAREA(L'UNKNOWN),UNKNOWN                                      
         BE    GENOUT                                                           
         CLC   NAMEAREA(L'NETWORKS),NETWORKS                                    
         BE    GENOUT                                                           
         MVC   SBBMGR,0(R2)                                                     
         GOTO1 GETMGRNM,SBMGR2NM                                                
         MVC   NAMEAREA(L'SBMGR2NM),SBMGR2NM                                    
         B     GENOUT                                                           
         SPACE 2                                                                
OMGR3    CLI   GLARGS,C'N'                                                      
         BNE   *+8                                                              
         LA    R2,8(R2)                                                         
         CLC   0(2,R2),XFF                                                      
         BE    GENOUT                                                           
         MVC   LABLAREA(L'SBMGR3BK),SBMGR3BK                                    
         MVC   CODEAREA(1),SBQMGRD                                              
         LA    R1,SBMGR3LN                                                      
         BAS   RE,OGRPCODE                                                      
         CLI   GLARGS,C'C'                                                      
         BE    GENOUT                                                           
         CLC   NAMEAREA(L'UNKNOWN),UNKNOWN                                      
         BE    GENOUT                                                           
         CLC   NAMEAREA(L'NETWORKS),NETWORKS                                    
         BE    GENOUT                                                           
         MVC   SBBMGR,0(R2)                                                     
         GOTO1 GETMGRNM,SBMGR3NM                                                
         MVC   NAMEAREA(L'SBMGR3NM),SBMGR3NM                                    
         B     GENOUT                                                           
         SPACE 2                                                                
OMKTR    LA    R2,2(R2)            ** MARKET IN RANK ORDER **                   
*                                                                               
OMKT     GOTO1 AOMKT               ** MARKET **                                 
         BNE   XIT                                                              
         B     GENOUT                                                           
         SPACE 2                                                                
OMKTRNK  GOTO1 AOMKTRNK            ** MARKET RANK **                            
         BNE   XIT                                                              
         B     GENOUT                                                           
         EJECT                                                                  
* GENERAL ROUTINE FOR OUTPUTTING GROUP CODES                                    
*                                                                               
OGRPCODE LR    R0,RE                                                            
         UNPK  DUB(5),0(3,R2)                                                   
         ZIC   RF,0(R1)                                                         
         BCTR  RF,0                                                             
         EX    RF,OGRPMVC                                                       
         CLC   0(2,R2),=X'9999'                                                 
         BNE   *+10                                                             
         MVC   NAMEAREA(L'UNKNOWN),UNKNOWN                                      
         CLC   0(2,R2),=X'9998'          TEST POSSIBLE 'NETWORKS' GROUP         
         BNE   OGRPCD2                                                          
         LA    RE,SBAGYREC               YES-TEST CANADIAN MEDIA C OR N         
         USING AGYRECD,RE                                                       
         CLI   AGYPROF+7,C'C'                                                   
         BNE   OGRPCD2                                                          
         CLI   SBMED,C'N'                                                       
         BE    *+12                                                             
         CLI   SBMED,C'C'                                                       
         BNE   OGRPCD2                                                          
         MVC   NAMEAREA(L'NETWORKS),NETWORKS    YES-IT'S THE NETWORKS           
         EX    RF,OGRPAST                                                       
         DROP  RE                                                               
*                                                                               
OGRPCD2  CLI   GLARGS,C'C'                                                      
         BNE   *+10                                                             
         MVC   NAMEAREA,BLANKS                                                  
         CLI   GLARGS,C'N'                                                      
         BNE   *+10                                                             
         MVC   CODEAREA,BLANKS                                                  
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
OGRPMVC  MVC   CODEAREA+1(0),DUB                                                
OGRPAST  MVC   CODEAREA+1(0),=C'****'                                           
NETWORKS DC    C'**NETWORKS**'                                                  
         EJECT                                                                  
* DAYPART OUTPUT ROUTINE                                                        
*                                                                               
ODPT     CLI   1(R2),C''          SUPPRESS $ DAYPARTS (FIRST CHAR OF           
         BE    DP99                                     NAME IS C'')           
         CLI   5(R2),C''                                                       
         BE    DP99                                                             
         MVC   LABLAREA(7),=C'DAYPART'                                          
         LA    R3,CODEAREA                                                      
         OI    OUTIND,OUTIDTOT                                                  
         MVC   0(7,R3),BLANKS                                                   
         OC    0(8,R2),0(R2)                                                    
         BNZ   *+14                                                             
         MVC   0(7,R3),=C'UNKNOWN'                                              
         B     DPX                                                              
         CLC   4(4,R2),XFF         DAYPART GROUP TOTAL                          
         BNE   DP10                                                             
         CP    DPTCNTR,=P'1'       TEST FOR MORE THAN ONE DPT IN GRP            
         BNH   DP99                                                             
         MVC   0(3,R3),1(R2)                                                    
         MVI   3(R3),C'-'                                                       
         MVC   4(3,R3),=C'TOT'                                                  
         B     DPX                                                              
*                                                                               
DP10     NI    OUTIND,FF-OUTIDTOT  NOT DAYPART TOTAL                            
         OC    1(3,R2),1(R2)                                                    
         BZ    DP90                                                             
         CLC   DPTGRPSV,1(R2)      TEST FOR NEW DAYPART GROUP                   
         BE    DP90                                                             
         ZAP   DPTCNTR,=P'0'       RESET DPT COUNTER                            
         MVC   DPTGRPSV,1(R2)                                                   
*                                                                               
DP90     MVC   0(3,R3),5(R2)                                                    
         CP    DPTCNTR,=P'2'       AUGMENT DAYPART COUNTER                      
         BNL   *+10                                                             
         AP    DPTCNTR,=P'1'                                                    
         B     DPX                                                              
*                                                                               
DP99     MVI   PRTSW,C'N'          DON'T PRINT                                  
         B     XIT                                                              
*                                                                               
DPX      L     R3,GLAOFLD                                                       
         B     GENOUT                                                           
         SPACE 2                                                                
* DAYPART/LENGTH OUTPUT                                                         
*                                                                               
ODPTLEN  DS    0H                                                               
         GOTO1 AODPTLEN                                                         
         B     XIT                                                              
         SPACE 2                                                                
* SPOT LENGTH OUTPUT ROUTINE                                                    
*                                                                               
OLEN     MVC   LABLAREA(6),=C'LENGTH'                                           
         CLI   0(R2),0                                                          
         BNE   *+14                                                             
         MVC   CODEAREA(3),=C'???'                                              
         B     GENOUT                                                           
         CLI   0(R2),X'FF'                                                      
         BE    GENOUT                                                           
         EDIT  (1,0(R2)),(3,CODEAREA),ALIGN=LEFT                                
         B     GENOUT                                                           
         EJECT                                                                  
* PERIOD ROW OUTPUT ROUTINES                                                    
*                                                                               
OPER     MVC   BYTE,GLARGS         DAY/MONTH/QUARTER/YEAR                       
         MVC   LABLAREA(3),=C'DAY'                                              
         CLI   GLARGS,C'D'                                                      
         BE    OPER2                                                            
         MVC   LABLAREA(4),=C'YEAR'                                             
         CLI   GLARGS,C'Y'                                                      
         BE    OPER2                                                            
         MVC   LABLAREA(5),=C'MONTH'                                            
         CLI   GLARGS,C'M'                                                      
         BE    OPER2                                                            
         MVC   LABLAREA(7),=C'QUARTER'                                          
         CLI   GLARGS,C'Q'                                                      
         BE    OPER2                                                            
         DC    H'0'                                                             
OPER2    LA    R4,CODEAREA                                                      
         BAS   RE,PRIAFT                                                        
         BNE   OPERX                                                            
         L     R1,GLADTENT                                                      
         LA    R1,DROLEN-DROD(R1)                                               
         GOTO1 AFMTPER                                                          
OPERX    B     GENOUT                                                           
*                                                                               
*                                                                               
OWEEK    MVC   LABLAREA(4),=C'WEEK'                                             
         LA    R4,NAMEAREA                                                      
         BAS   RE,PRIAFT                                                        
         BNE   GENOUT                                                           
         GOTO1 DATCON,DMCB,(2,(R2)),DUB                                         
         CLI   GLARGS,C'Y'         TEST YYMMDD FORMAT                           
         BE    *+12                                                             
         TM    OPTIND2,OPTIYMD                                                  
         BZ    *+14                                                             
         MVC   0(6,R4),DUB                                                      
         B     GENOUT                                                           
         TM    OPTIND3,OPTICYMD    TEST CCYYMMDD FORMAT                         
         BZ    OWEEK2                                                           
         MVC   0(2,R4),=C'19'                                                   
         CLI   DUB,C'6'                                                         
         BH    *+10                                                             
         MVC   0(2,R4),=C'00'                                                   
         MVC   2(6,R4),DUB                                                      
         B     GENOUT                                                           
*                                                                               
OWEEK2   GOTO1 DATCON,DMCB,(2,(R2)),(7,(R4))                                    
         MVI   5(R4),C'-'                                                       
         GOTO1 (RF),(R1),(2,2(R2)),(7,6(R4))                                    
         B     GENOUT                                                           
*                                                                               
*                                                                               
OFLT     MVC   LABLAREA(6),=C'FLIGHT'     CHILD SPOT FLIGHT                     
         OC    0(4,R2),0(R2)                                                    
         BZ    GENOUT                                                           
         GOTO1 DATCON,DMCB,(2,(R2)),(5,NAMEAREA)                                
         MVI   NAMEAREA+8,C'-'                                                  
         GOTO1 (RF),(R1),(2,2(R2)),(5,NAMEAREA+9)                               
         B     GENOUT                                                           
*                                                                               
*                                                                               
ODAY     MVC   LABLAREA(3),=C'DAY'     DAY                                      
         LA    R4,NAMEAREA                                                      
         BAS   RE,PRIAFT                                                        
         BNE   GENOUT                                                           
         GOTO1 DATCON,DMCB,(2,(R2)),DUB                                         
         GOTO1 GETDAY,(R1),DUB,(R4)                                             
         CLC   0(3,R4),BLANKS                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVI   3(R4),C' '                                                       
         GOTO1 DATCON,(R1),DUB,(4,4(R4))                                        
         B     GENOUT                                                           
*                                                                               
PRIAFT   LR    R0,RE                                                            
         NI    OPTIND2,255-OPTITITL   PERIOD DOES NOT GO TO TITLE               
         OC    0(4,R2),0(R2)                                                    
         BNZ   *+14                                                             
         MVC   0(6,R4),=C'*PRIOR'                                               
         B     PRIAFTN                                                          
         CLC   0(4,R2),XFE                                                      
         BNE   *+14                                                             
         MVC   0(6,R4),=C'*AFTER'                                               
         B     PRIAFTN                                                          
         CLC   0(4,R2),XFF                                                      
         BNE   *+14                                                             
         MVC   0(5,R4),=C'*ALL*'                                                
         B     PRIAFTN                                                          
         CLC   0(4,R2),XFD                                                      
         BNE   PRIAFTY                                                          
         MVC   0(6,R4),=C'*????*'                                               
PRIAFTN  LTR   RE,R0               CC NE - DO NOT FORMAT DATES                  
         BR    RE                                                               
PRIAFTY  LA    R1,GLARGS                                                        
         TM    GLOIND-GLARGSD(R1),GLOITITL  TEST PERIOD SHOULD GO TO            
         BZ    *+14                         THE REPORT TITLE                    
         OI    OPTIND2,OPTITITL             YES-INDICATE IT'S SO                
         MVC   SBBQSTP(4),0(R2)                 AND SAVE THE DATES              
         LR    RE,R0                                                            
         CR    RE,RE               CC EQ - FORMAT DATES                         
         BR    RE                                                               
         EJECT                                                                  
* OUTPUT ROUTINES FOR BUY DETAILS                                               
*                                                                               
OBUY     GOTO1 AOBUY                                                            
         B     XIT                                                              
         SPACE 2                                                                
OLINE    CLC   0(13,R2),XFE        BUYLINE NUMBER                               
         BNE   *+14                                                             
         MVC   0(3,R3),=C'???'                                                  
         B     XIT                                                              
         ZIC   RE,BUYKBUY-BUYKEY(R2)                                            
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R3),DUB                                                      
         B     XIT                                                              
         SPACE 2                                                                
OPROG    MVC   LABLAREA(7),=C'PROGRAM'  PROGRAM NAME                            
         MVC   NAMEAREA(17),0(R2)                                               
         B     GENOUT                                                           
         SPACE 2                                                                
OROT     MVC   LABLAREA(8),=C'ROTATION'   DAYS ROTATION                         
         CLI   1(R2),0                                                          
         BNE   *+14                                                             
         MVC   NAMEAREA(7),=C'UNKNOWN'                                          
         B     GENOUT                                                           
         LA    R5,1(R2)                                                         
         GOTO1 DAYUNPK,DMCB,(R5),NAMEAREA                                       
         B     GENOUT                                                           
         SPACE 2                                                                
OTIMES   MVC   LABLAREA(5),=C'TIMES'    TIMES                                   
         CLC   0(4,R2),XFE                                                      
         BNE   *+14                                                             
         MVC   NAMEAREA(7),=C'UNKNOWN'                                          
         B     GENOUT                                                           
         CLI   0(R2),C'M'          MIDNIGHT?                                    
         BNE   *+10                                                             
         XC    0(4,R2),0(R2)                                                    
         CLI   GLARGS,C'M'         MILITARY TIME OPTION                         
         BNE   OTIMES2                                                          
         SR    RE,RE                                                            
         ICM   RE,3,0(R2)                                                       
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  NAMEAREA(4),DUB                                                  
         MVI   NAMEAREA+4,C'-'                                                  
         MVC   NAMEAREA+5(4),NAMEAREA                                           
         ICM   RE,3,2(R2)                                                       
         BZ    GENOUT                                                           
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  NAMEAREA+5(4),DUB                                                
         B     GENOUT                                                           
OTIMES2  DS    0H                                                               
         GOTO1 UNTIME,DMCB,(R2),NAMEAREA                                        
         B     GENOUT                                                           
         SPACE 2                                                                
OBYID    MVC   LABLAREA(6),=C'BUY ID'   BUY ID                                  
         CLC   SBCIDTIT,BLANKS                                                  
         BNH   *+10                                                             
         MVC   LABLAREA(10),SBCIDTIT                                            
         MVC   NAMEAREA(12),0(R2)                                               
         B     GENOUT                                                           
         SPACE 2                                                                
OBYTYPE  MVC   LABLAREA(8),=C'BUY TYPE' BUY TYPE                                
         MVC   NAMEAREA(4),=C'CASH'                                             
         CLI   0(R2),C'C'                                                       
         BE    GENOUT                                                           
         MVC   NAMEAREA(5),=C'TRADE'                                            
         CLI   0(R2),C'T'                                                       
         BE    GENOUT                                                           
         MVC   NAMEAREA(5),=C'*ALL*'                                            
         CLI   0(R2),X'FF'                                                      
         BE    GENOUT                                                           
         MVC   NAMEAREA(L'UNKNOWN),UNKNOWN                                      
         B     GENOUT                                                           
         SPACE 2                                                                
OADJ     MVC   LABLAREA(14),=C'ADJACENCY CODE'                                  
         MVC   CODEAREA(1),0(R2)                                                
         B     GENOUT                                                           
         SPACE 2                                                                
OPREP    MVC   LABLAREA(10),=C'PAYING REP'                                      
         B     OREP                                                             
*                                                                               
OTREP    MVC   LABLAREA(14),=C'TIME SHEET REP'                                  
         B     OREP                                                             
*                                                                               
OSREP    MVC   LABLAREA(11),=C'SPECIAL REP'                                     
*                                                                               
OREP     CLI   GLARGS,C'N'                                                      
         BNE   *+14                                                             
         MVC   NAMEAREA(22),0(R2)                                               
         B     GENOUT                                                           
         MVC   SBREP,0(R2)                                                      
         CLC   SBREP,ZEROS         TEST REP=000                                 
         BE    *+10                YES-LEAVE OUTPUT BLANK                       
         MVC   CODEAREA(3),SBREP                                                
         CLI   GLARGS,C'C'                                                      
         BE    GENOUT                                                           
         CLC   SVREP,SBREP                                                      
         BE    OREP2                                                            
         MVC   SVREP,SBREP                                                      
         MVC   SBREPNM,BLANKS                                                   
         CLC   SBREP,ZEROS                                                      
         BE    GENOUT                                                           
         GOTO1 GETREPNM                                                         
         BE    OREP2                                                            
         GOTO1 AGETREP                                                          
         GOTO1 PUTREPNM                                                         
OREP2    MVC   NAMEAREA(22),SBREPNM                                             
         B     GENOUT                                                           
         SPACE 2                                                                
ONAMES   MVC   NAMEAREA(12),0(R2)                                               
         CLI   GLARGS,1                                                         
         BNE   *+20                                                             
         MVC   SBBYRNM,0(R2)            BUYER NAME                              
         MVC   LABLAREA(5),=C'BUYER'                                            
         B     GENOUT                                                           
         MVC   SBBLRNM,0(R2)            BILLER NAME                             
         MVC   LABLAREA(6),=C'BILLER'                                           
         B     GENOUT                                                           
         EJECT                                                                  
*------------------------------------BILL HEADER OUTPUT ROUTINES                
*                                                                               
OBHINV   MVC   LABLAREA(6),=C'INV NO'                                           
         MVC   NAMEAREA(6),0(R2)                                                
         CLI   GLARGS,C'M'                                                      
         BNE   GENOUT                                                           
         MVC   NAMEAREA(7),0(R2)                                                
         B     GENOUT                                                           
*                                                                               
*                                                                               
OBHRETL  MVC   LABLAREA(9),=C'RETL ACCT'                                        
         MVC   NAMEAREA(12),0(R2)                                               
         B     GENOUT                                                           
*                                                                               
*                                                                               
OSTATYP  MVC   LABLAREA(8),=C'STA TYPE'                                         
         MVC   CODEAREA(1),0(R2)                                                
         B     GENOUT                                                           
*                                                                               
*                                                                               
OBHTYPE  MVC   LABLAREA(8),=C'BILL TYP'                                         
         MVC   CODEAREA(4),0(R2)                                                
         B     GENOUT                                                           
*                                                                               
*                                                                               
OAORTYP  MVC   LABLAREA(8),=C'AOR TYPE'                                         
         MVC   CODEAREA(1),0(R2)                                                
         B     GENOUT                                                           
*                                                                               
*                                                                               
OCOMTYP  MVC   LABLAREA(8),=C'COMM TYP'                                         
         MVC   CODEAREA(1),0(R2)                                                
         B     GENOUT                                                           
*                                                                               
*                                                                               
OBHPRDI  MVC   LABLAREA(9),=C'PRD INTER'                                        
         CLI   0(R2),X'FF'                                                      
         BNE   OBHPRDI5                                                         
         EDIT  (P3,1(R2)),(5,NAMEAREA)      PACKED VALUE                        
         B     GENOUT                                                           
OBHPRDI5 MVC   NAMEAREA(4),0(R2)            CHARACTERS                          
         B     GENOUT                                                           
*                                                                               
*                                                                               
OAOREFDT MVC   LABLAREA(9),=C'AOR EF DT'                                        
         OC    0(2,R2),0(R2)                                                    
         BZ    GENOUT                                                           
         XC    FULL,FULL                                                        
         MVC   FULL(2),0(R2)       YEAR AND MONTH                               
         MVI   FULL+2,X'01'        DUMMY DAY                                    
         GOTO1 DATCON,DMCB,(3,FULL),(7,NAMEAREA)                                
         B     GENOUT                                                           
*                                                                               
*                                                                               
OAORAGYN MVC   LABLAREA(9),=C'AOR AGYNM'                                        
         MVC   NAMEAREA(30),0(R2)                                               
         B     GENOUT                                                           
*                                                                               
*                                                                               
OAORINCA MVC   LABLAREA(8),=C'AOR COMM'                                         
         MVC   NAMEAREA(14),0(R2)                                               
         B     GENOUT                                                           
*                                                                               
*                                                                               
OAORPCT  MVC   LABLAREA(7),=C'AOR PCT'                                          
         EDIT  (4,0(R2)),(6,NAMEAREA),4    PACKED VALUE                         
         B     GENOUT                                                           
*                                                                               
*                                                                               
OAORRPA  MVC   LABLAREA(7),=C'REC/PAY'                                          
         MVC   NAMEAREA(14),0(R2)                                               
         B     GENOUT                                                           
*                                                                               
*                                           DATES                               
*                                                                               
OBHIDT   MVC   LABLAREA(12),=C'INVOICE DATE'  INVOICE DATE                      
         OC    0(6,R2),0(R2)                                                    
         BZ    GENOUT                                                           
         SR    R1,R1                                                            
         BAS   RE,OBHDATE                                                       
         B     GENOUT                                                           
*                                                                               
*                                                                               
OBHRDT   MVC   LABLAREA(8),=C'RUN DATE'   RUN DATE                              
         OC    0(6,R2),0(R2)                                                    
         BZ    GENOUT                                                           
         SR    R1,R1                                                            
         BAS   RE,OBHDATE                                                       
         B     GENOUT                                                           
*                                                                               
*                                                                               
OBHDDT   MVC   LABLAREA(8),=C'DUE DATE'   DUE DATE                              
         OC    0(3,R2),0(R2)                                                    
         BZ    GENOUT                                                           
         LA    R1,3                                                             
         BAS   RE,OBHDATE                                                       
         B     GENOUT                                                           
*                                                                               
*                                                                               
OBHPDT   MVC   LABLAREA(9),=C'POST DATE'  POST DATE                             
         OC    0(2,R2),0(R2)                                                    
         BZ    GENOUT                                                           
         LA    R1,2                                                             
         BAS   RE,OBHDATE                                                       
         B     GENOUT                                                           
*                                                                               
*                                                                               
OBHSDT   MVC   LABLAREA(8),=C'SRV DATE'          MONTH OF SERVICE               
         OC    0(4,R2),0(R2)                                                    
         BZ    GENOUT                                                           
         MVC   WORK(4),0(R2)                                                    
         MVC   WORK+4(2),=C'01'                                                 
         LA    R2,WORK                                                          
         SR    R1,R1                                                            
         BAS   RE,OBHMON                                                        
         B     GENOUT                                                           
*                                                                               
*                                             ROW DATES                         
OBHIMN   MVC   LABLAREA(8),=C'INV DATE'                                         
         B     OBHMON6                                                          
*                                                                               
*                                                                               
OBHRMN   MVC   LABLAREA(8),=C'RUN DATE'                                         
*                                                                               
OBHMON6  OC    0(6,R2),0(R2)                                                    
         BZ    GENOUT                                                           
         SR    R1,R1                                                            
         BAS   RE,OBHMON                                                        
         B     GENOUT                                                           
*                                                                               
*                                                                               
OBHDMN   MVC   LABLAREA(8),=C'DUE DATE'                                         
         OC    0(3,R2),0(R2)                                                    
         BZ    GENOUT                                                           
         LA    R1,3                                                             
         BAS   RE,OBHMON                                                        
         B     GENOUT                                                           
*                                                                               
*                                                                               
OBHPMN   MVC   LABLAREA(9),=C'POST DATE'                                        
         OC    0(2,R2),0(R2)                                                    
         BZ    GENOUT                                                           
         LA    R1,2                                                             
         BAS   RE,OBHMON                                                        
         B     GENOUT                                                           
*                                                                               
*                                                                               
OBHDATE  LR    R0,RE               GENERAL DATE OUTPUT ROUTINE                  
         ST    R2,DMCB                                                          
         STC   R1,DMCB                                                          
         XC    DUB,DUB                                                          
         LA    R1,DUB                                                           
         ST    R1,DMCB+4                                                        
         MVI   DMCB+4,5                                                         
         TM    OPTIND3,OPTICYMD                                                 
         BZ    *+8                                                              
         MVI   DMCB+4,0                                                         
         GOTO1 DATCON,DMCB                                                      
         LA    R1,NAMEAREA                                                      
         MVC   0(8,R1),DUB                                                      
         TM    OPTIND3,OPTICYMD                                                 
         BZ    OBHDATEX                                                         
         MVC   0(2,R1),=C'19'                                                   
         CLI   DUB,C'6'                                                         
         BH    *+10                                                             
         MVC   0(2,R1),=C'20'                                                   
         MVC   2(6,R1),DUB                                                      
OBHDATEX LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
OBHMON   LR    R0,RE               GENERAL MONTH OUTPUT ROUTINE                 
         ST    R2,DMCB                                                          
         STC   R1,DMCB                                                          
         XC    DUB,DUB                                                          
         LA    R1,DUB                                                           
         ST    R1,DMCB+4                                                        
         MVI   DMCB+4,6                                                         
         TM    OPTIND3,OPTICYMD                                                 
         BZ    *+8                                                              
         MVI   DMCB+4,0                                                         
         GOTO1 DATCON,DMCB                                                      
         MVC   NAMEAREA(6),DUB                                                  
         TM    OPTIND3,OPTICYMD                                                 
         BZ    OBHMONX                                                          
         MVC   NAMEAREA(2),=C'19'                                               
         CLI   DUB,C'6'                                                         
         BH    *+10                                                             
         MVC   NAMEAREA(2),=C'00'                                               
         MVC   NAMEAREA+2(4),DUB                                                
OBHMONX  LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
* PRODUCT INTERFACE CODE I/O ROUTINES                                           
*                                                                               
IPRDINT  MVC   0(4,R2),SBPRDINT                                                 
         B     XIT                                                              
*                                                                               
OPRDINT  MVC   LABLAREA(14),=C'INTERFACE CODE'                                  
         MVC   CODEAREA(4),0(R2)                                                
         B     GENOUT                                                           
         EJECT                                                                  
* HEADLINE ROUTINES                                                             
*                                                                               
         SPACE 1                                                                
HAR      MVI   GLARGS+15,1         AVERAGE RATING                               
         BAS   RE,HDEM2                                                         
         LA    R3,198(R3)                                                       
         MVI   GLARGS+15,2                                                      
         BAS   RE,HDEM2                                                         
         LA    R3,198(R3)                                                       
         MVC   0(7,R3),=C'AVERAGE'                                              
         B     XIT                                                              
*                                                                               
HDEM     DS    0H                  DEMO                                         
         CLI   GLARGS+15,0                                                      
         BE    *+12                                                             
         BAS   RE,HDEM2                                                         
         B     XIT                                                              
         LA    R0,4                                                             
         LA    R1,1                                                             
         STC   R1,GLARGS+15                                                     
         BAS   RE,HDEM2                                                         
         LA    R1,1(R1)                                                         
         LA    R3,198(R3)                                                       
         BCT   R0,*-16                                                          
         B     XIT                                                              
*                                                                               
HSTBDEM  MVI   GLARGS+15,1         BUY DEMO STACK                               
         BAS   RE,HDEMH12                                                       
         LA    R3,198(R3)                                                       
         MVI   GLARGS+15,2                                                      
         BAS   RE,HDEMH12                                                       
         B     XIT                                                              
*                                                                               
HDEM2    NTR1                                                                   
         CLI   GLARGS+15,2                                                      
         BH    *+12                                                             
         BAS   RE,HDEMH12          HEADS 1 AND 2                                
         B     XIT                                                              
         CLI   GLARGS+15,3         HEAD3                                        
         BNE   HD2                                                              
         CLI   GLARGS+4,C'B'       TEST BONUS DEMOS                             
         BNE   *+14                                                             
         MVC   0(6,R3),=C' BONUS'  YES                                          
         B     XIT                                                              
         LA    R1,GLARGS                                                        
         USING GLARGSD,R1                                                       
         CLI   GLARGS+4,C'N'       TEST NON-ADJUSTED DEMOS                      
         BNE   HD1                                                              
         MVC   0(7,R3),=C' NONADJ' YES                                          
         TM    GLHIND,GLHIWGT                                                   
         BZ    XIT                                                              
         LA    R3,198(R3)                                                       
*                                                                               
HD1      TM    GLHIND,GLHIWGT      TEST DEMO IS WEIGHTED                        
         BZ    HD4                                                              
         MVC   0(6,R3),=C' WGTED'  YES                                          
         B     XIT                                                              
         DROP  R1                                                               
*                                                                               
HD2      CLI   GLARGS+15,4         HEAD4                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   GLARGS+4,C'B'                                                    
         BNE   XIT                                                              
*                                                                               
HD4      TM    DATAIND3,DIBOOK     TEST BOOK IS A ROW                           
         BO    XIT                                                              
         OC    SBQBKS,SBQBKS       NO-TEST MORE THAN ONE RERATE BOOK            
         BZ    XIT                                                              
         CLI   GLARGS+1,C'R'       YES- TEST RERATE OR AFFID                    
         BE    *+12                                                             
         CLI   GLARGS+1,C'A'                                                    
         BNE   XIT                                                              
         BAS   RE,FMTBOOK          YES- FORMAT THE BOOK                         
         B     XIT                                                              
*                                                                               
*                                                                               
HDCPP    DS    0H                  CHILD SPOT DELIVERED CPP                     
         L     R1,GLADTENT                                                      
         ZIC   RF,DRHDWDTH-DRHDD(R1)                                            
         CH    RF,=H'9'                                                         
         BNH   *+8                                                              
         LA    RF,9                                                             
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),=C'DELIVERED'                                            
         LA    R3,198(R3)                                                       
         B     HCPP02                                                           
*                                                                               
HCPP     DS    0H                  COST PER POINT                               
         CLI   GLARGS+15,0                                                      
         BE    HCPP02                                                           
         BAS   RE,HCPP2                                                         
         B     XIT                                                              
HCPP02   LA    R0,3                                                             
         LA    R1,1                                                             
         STC   R1,GLARGS+15                                                     
         BAS   RE,HCPP2                                                         
         LA    R1,1(R1)                                                         
         LA    R3,198(R3)                                                       
         BCT   R0,*-16                                                          
         B     XIT                                                              
*                                                                               
HCPP2    NTR1                                                                   
         CLI   GLARGS+15,2                                                      
         BH    *+12                                                             
         BAS   RE,HDEMH12          HEADS 1 AND 2                                
         B     XIT                                                              
         CLI   GLARGS+15,3         HEAD3                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   0(7,R3),BLANKS                                                   
         OC    DEMONAME,DEMONAME                                                
         BZ    XIT                                                              
         MVC   2(3,R3),=C'CPP'                                                  
         CLI   DEMONAME,C'R'                                                    
         BE    XIT                                                              
         CLI   DEMONAME,C'E'                                                    
         BE    XIT                                                              
         MVI   4(R3),C'M'                                                       
         B     XIT                                                              
*                                                                               
*                                                                               
HDEMH12  LR    R0,RE               FORMAT DEMO AND CPP HEADS 1 AND 2            
         CLI   GLARGS+15,1         GLARGS+15=HEADLINE NUMBER                    
         BNE   HDH2                                                             
         MVC   0(7,R3),BLANKS                                                   
         GOTO1 AGETDEM,DEMONAME    GET DEMO NAME                                
         BNE   HDHX                                                             
         CLI   GLARGS+1,C'G'                                                    
         BNE   *+14                                                             
         MVC   0(7,R3),=C'GOAL   '                                              
         B     HDHX                                                             
         CLI   GLARGS+1,C'N'                                                    
         BNE   *+14                                                             
         MVC   0(8,R3),=C'NET GOAL'                                             
         B     HDHX                                                             
         CLI   GLARGS+1,C'O'                                                    
         BNE   *+14                                                             
         MVC   0(7,R3),=C'ORDERED'                                              
         B     HDHX                                                             
         CLI   GLARGS+1,C'P'                                                    
         BNE   *+14                                                             
         MVC   0(7,R3),=C'PURCH  '                                              
         B     HDHX                                                             
         CLI   GLARGS+1,C'R'                                                    
         BNE   *+14                                                             
         MVC   0(7,R3),=C'ACHVD  '                                              
         B     HDHX                                                             
         CLI   GLARGS+1,C'A'                                                    
         BNE   *+14                                                             
         MVC   0(7,R3),=C'AFDVT  '                                              
         B     HDHX                                                             
         CLI   GLARGS+1,C'L'                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   0(7,R3),=C'LOCKIN '                                              
         B     HDHX                                                             
*                                                                               
HDH2     CLI   GLARGS+15,2         HEAD2                                        
         BNE   HDHX                                                             
         MVC   0(7,R3),BLANKS                                                   
         TM    DOWNOPT,GLDLACTV    IF DOWNLOADING,                              
         BZ    HDH22                                                            
         GOTO1 AGETDEM,DEMONAME    NEED TO GET DEMO NAME AGAIN                  
         BNE   HDHX                                                             
HDH22    OC    DEMONAME,DEMONAME                                                
         BZ    HDHX                                                             
         MVC   0(7,R3),DEMONAME                                                 
*                                                                               
HDHX     LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
*                                  ** DEMO STACK HEADER ROUTINE **              
HSTDEM   MVC   0(7,R3),BLANKS                                                   
         GOTO1 AGETDEM,DEMONAME                                                 
         BNE   XIT                                                              
         MVC   0(7,R3),DEMONAME                                                 
         B     XIT                                                              
*                                                                               
FMTBOOK  LR    R0,RE               FORMAT BOOK TO HEADLINE                      
         LA    R5,SBQBOOK                                                       
         CLI   GLARGS+2,0                                                       
         BE    FMTBK2                                                           
         LA    R5,SBQBKS                                                        
         CLI   GLARGS+2,2                                                       
         BE    FMTBK2                                                           
         LA    R5,SBQBKS+4                                                      
         CLI   GLARGS+2,3                                                       
         BE    FMTBK2                                                           
         DC    H'0'                                                             
FMTBK2   MVC   DUB(4),0(R5)                                                     
         MVC   DUB+4(2),=C'01'                                                  
         GOTO1 DATCON,DMCB,(0,DUB),(6,0(R3))                                    
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
*                                  DEMO INDEX                                   
HDEMNDX  CLI   GLARGS+15,0                                                      
         BE    *+12                                                             
         BAS   RE,HDEMNDX2                                                      
         B     XIT                                                              
         LA    R1,1                                                             
         LA    RF,3                                                             
         STC   R1,GLARGS+15                                                     
         BAS   RE,HDEMNDX2                                                      
         LA    R1,1(R1)                                                         
         LA    R3,198(R3)                                                       
         BCT   RF,*-16                                                          
         B     XIT                                                              
*                                                                               
HDEMNDX2 LR    R0,RE                                                            
         CLI   GLARGS+15,1         HEAD1                                        
         BNE   HDEMNDX4                                                         
         MVC   0(7,R3),BLANKS                                                   
         GOTO1 AGETDEM,DEMONAME                                                 
         BNE   HDEMNDXX                                                         
         MVC   0(7,R3),DEMONAME                                                 
         B     HDEMNDXX                                                         
HDEMNDX4 BAS   RE,HINDEX           HEAD2                                        
HDEMNDXX LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
*                                  DOLLAR INDEX                                 
HDOLNDX  CLI   GLARGS+15,0                                                      
         BE    *+12                                                             
         BAS   RE,HDOLNDX2                                                      
         B     XIT                                                              
         LA    R1,1                                                             
         LA    RF,3                                                             
         STC   R1,GLARGS+15                                                     
         BAS   RE,HDOLNDX2                                                      
         LA    R1,1(R1)                                                         
         LA    R3,198(R3)                                                       
         BCT   RF,*-16                                                          
         B     XIT                                                              
*                                                                               
HDOLNDX2 LR    R0,RE                                                            
         CLI   GLARGS+15,1         HEAD1                                        
         BNE   *+14                                                             
         MVC   0(4,R3),=C'COST'                                                 
         B     *+8                                                              
         BAS   RE,HINDEX                                                        
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
*                                                                               
HINDEX   CLI   GLARGS+15,2         HEAD2                                        
         BNE   *+12                                                             
         MVC   0(7,R3),=C'INDEX V'                                              
         BR    RE                                                               
         CLI   GLARGS+15,3         HEAD3                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    SBQDATA,SBQDGOAL                                                 
         BZ    *+12                                                             
         MVC   0(4,R3),=C'GOAL'                                                 
         BR    RE                                                               
         TM    SBQDATA,SBQDORD                                                  
         BZ    *+12                                                             
         MVC   0(7,R3),=C'ORDERED'                                              
         BR    RE                                                               
         TM    SBQDATA,SBQDPUR                                                  
         BZ    *+12                                                             
         MVC   0(5,R3),=C'PURCH'                                                
         BR    RE                                                               
         DC    H'0'                                                             
         EJECT                                                                  
* FORMAT PERIOD HEADLINE                                                        
*                                                                               
HPER     ICM   R2,15,GLARGS        TEST ANY DATES                               
         BZ    XIT                                                              
         LR    R4,R3                                                            
         STCM  R2,8,BYTE           BYTE=PERIOD TYPE D/W/M/Q/H/Y                 
         L     R1,GLADTENT                                                      
         LA    R1,DRHDWDTH-DRHDD(R1)                                            
         GOTO1 AFMTPER                                                          
         B     XIT                                                              
         SPACE 2                                                                
* FORMAT BUY DETAILS HEADLINE                                                   
*                                                                               
HBUY     CLI   GLARGS+15,0                                                      
         BE    *+12                                                             
         BAS   RE,HBUY2                                                         
         B     XIT                                                              
         LA    R0,3                                                             
         LA    R1,1                                                             
         STC   R1,GLARGS+15                                                     
         BAS   RE,HBUY2                                                         
         LA    R1,1(R1)                                                         
         LA    R3,198(R3)                                                       
         BCT   R0,*-16                                                          
         B     XIT                                                              
*                                                                               
HBUY2    L     R2,GLADTENT         CLEAR HEADLINE                               
         ZIC   RF,DRHDWDTH-DRHDD(R2)                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    0(0,R3),0(R3)                                                    
         CLI   GLARGS+15,1         ARGS+15=HEADLINE NUMBER                      
         BNE   HBUY2A                                                           
         MVC   0(33,R3),=C'EST-LIN  BUY PERIOD  WKS  DPT LNG'                   
         CLI   GLARGS,C'C'                                                      
         BNE   *+10                                                             
         MVC   34(4,R3),=C'COST'                                                
         CLI   GLARGS,C'N'                                                      
         BNE   HBUY2X                                                           
         MVC   34(8,R3),=C'NET COST'                                            
         B     HBUY2X                                                           
*                                                                               
HBUY2A   CLI   GLARGS+15,2                                                      
         BNE   HBUY2B                                                           
         MVI   0(R3),C'-'                                                       
         MVC   1(32,R3),0(R3)                                                   
         CLI   GLARGS,C'C'                                                      
         BNE   HBUY2X                                                           
         MVC   33(10,R3),0(R3)                                                  
         B     HBUY2X                                                           
*                                                                               
HBUY2B   CLI   GLARGS+15,3                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   3(3,R3),=C'DAY'                                                  
         MVC   12(4,R3),=C'TIME'                                                
         MVC   26(7,R3),=C'PROGRAM'                                             
*                                                                               
HBUY2X   BR    RE                                                               
         SPACE 2                                                                
* FORMAT BUY ID HEADLINE                                                        
*                                                                               
HBYID    MVC   0(6,R3),=C'BUY ID'                                               
         CLC   SBCIDTIT,BLANKS                                                  
         BNH   XIT                                                              
         MVC   0(10,R3),SBCIDTIT                                                
         B     XIT                                                              
         SPACE 2                                                                
* NETWORK/STATION HEADLINE                                                      
*                                                                               
HNETSTA  MVC   0(9,R3),=C' NETWORK '                                            
         CLI   SBQNETWK,C'N'                                                    
         BE    XIT                                                              
         MVC   1(7,R3),=C'STATION'                                              
         CLI   SBQNETWK,C'L'                                                    
         BE    XIT                                                              
         MVC   0(9,R3),=C'NETWK/STA'                                            
         B     XIT                                                              
         SPACE 2                                                                
* COMMERCIAL HEADLINE                                                           
*                                                                               
HCML     MVC   0(4,R3),=C'FILM'                                                 
         CLI   SBMED,C'R'                                                       
         BNE   XIT                                                              
         MVC   0(8,R3),=C'COMMERCL'                                             
         CLI   GLARGS,C'C'                                                      
         BE    XIT                                                              
         MVC   0(10,R3),=C'COMMERCIAL'                                          
         B     XIT                                                              
         EJECT                                                                  
* GROUP HEADLINES                                                               
*                                                                               
HCGR1    MVC   0(L'SBCGR1BK,R3),SBCGR1BK                                        
         B     XIT                                                              
*                                                                               
HCGR2    MVC   0(L'SBCGR2BK,R3),SBCGR2BK                                        
         B     XIT                                                              
*                                                                               
HPGR1    MVC   0(L'SBPGR1BK,R3),SBPGR1BK                                        
         B     XIT                                                              
*                                                                               
HPGR2    MVC   0(L'SBPGR2BK,R3),SBPGR2BK                                        
         B     XIT                                                              
*                                                                               
HMGR1    MVC   0(L'SBMGR1BK,R3),SBMGR1BK                                        
         B     XIT                                                              
*                                                                               
HMGR2    MVC   0(L'SBMGR2BK,R3),SBMGR2BK                                        
         B     XIT                                                              
*                                                                               
HMGR3    MVC   0(L'SBMGR3BK,R3),SBMGR3BK                                        
         B     XIT                                                              
*                                                                               
HSGR1    MVC   0(L'SBSGR1BK,R3),SBSGR1BK                                        
         B     XIT                                                              
*                                                                               
HSGR2    MVC   0(L'SBSGR2BK,R3),SBSGR2BK                                        
         B     XIT                                                              
         EJECT                                                                  
* GENERAL TOTAL ROUTINE FOR DETAIL ROWS                                         
*                                                                               
TOTAL    GOTO1 ATOTAL                                                           
         B     XIT                                                              
         SPACE 2                                                                
* MARKET RANK TOTAL ROUTINE                                                     
*                                                                               
TMKTRNK  GOTO1 ATMKTRNK                                                         
         B     XIT                                                              
         EJECT                                                                  
* SHARED OUTPUT ROUTINE                                                         
*                                                                               
GENOUT   DS    0H                                                               
         GOTO1 AGENOUT                                                          
         B     XIT                                                              
         EJECT                                                                  
* ABOUT TO PASS DRIVER RECORD TO THE SORT                                       
*                                                                               
PUTSRT   CLI   INDATA,0            TEST ANY COLUMN DATA                         
*         BE    PUTSRT9             NO-SUPPRESS THIS RECORD                     
         OC    LEVELSWS,LEVELSWS   TEST ALREADY WANT TO SUPPRESS                
         BZ    PUTSRT2                                                          
         LA    RE,L'LEVELSWS                                                    
         LA    RF,LEVELSWS                                                      
         CLI   0(RF),C'Y'                                                       
         BE    PUTSRT2                                                          
         LA    RF,1(RF)                                                         
         BCT   RE,*-12                                                          
         B     PUTSRT9                                                          
*                                                                               
PUTSRT2  TM    OPTIND,OPTIREP      TEST REP=0 DISALLOWED                        
         BZ    PUTSRT3                                                          
         TM    DATAIND4,DISREP     YES-TEST SPECIAL REP IN ROWS                 
         BZ    *+14                                                             
         CLC   SBSREP,ZEROS        YES-TEST SPECIAL REP = 0                     
         BE    PUTSRT9                 YES-IGNORE IT                            
         TM    DATAIND5,DIPREP     TEST PAYING REP                              
         BZ    *+14                                                             
         CLC   SBPREP,ZEROS        YES-TEST PAYING REP = 0                      
         BE    PUTSRT9                                                          
         TM    DATAIND5,DITREP     TEST TIME SHEET REP                          
         BZ    PUTSRT3                                                          
         CLC   SBTREP,ZEROS        YES-TEST TIME SHEET REP = 0                  
         BE    PUTSRT9                                                          
*                                                                               
PUTSRT3  CLC   SBQCMLCL,BLANKS     TEST COMMERCIAL CLASS FILTER                 
         BNH   PUTSRT6                                                          
         LA    R1,SBCMLCLS                                                      
         LA    RE,SBQCMLCL                                                      
         LA    R0,L'SBQCMLCL                                                    
PUTSRT4  CLI   0(RE),C' '                                                       
         BNH   PUTSRT5                                                          
         CLI   0(RE),C'*'                                                       
         BE    PUTSRT5                                                          
         CLC   0(1,R1),0(RE)                                                    
         BNE   PUTSRT9                                                          
PUTSRT5  LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,PUTSRT4                                                       
*                                                                               
PUTSRT6  TM    ROWIND,ROWINOMK     TEST SUPPRESS MARKET DETAILS                 
         BZ    PUTSRT7                                                          
         CLI   MKTLEV,0            AND MARKET IS A ROW                          
         BE    PUTSRT7                                                          
         L     R1,GLAIO                                                         
         CLC   DETLEV,1(R1)        AND THIS IS A DETAIL RECORD                  
         BNE   PUTSRT7                                                          
         TM    GLINDS2,GLPUTDT     BUT NOT A DETAILED TOTAL                     
         BO    PUTSRT7                                                          
         TM    ININD,INIDPTOT      AND IT'S A DAYPART TOTAL                     
         BO    PUTSRT9             YES-THIS RECORD IS NOT NEEDED                
*                                                                               
PUTSRT7  B     PUTSRTX                                                          
*                                                                               
PUTSRT9  MVI   GLHOOK,GLDONT                                                    
*                                                                               
PUTSRTX  B     XIT                                                              
         EJECT                                                                  
FF       EQU   X'FF'                                                            
*                                                                               
AXTRA    DS    0F               ** EXTENTION ROUTINE ADDRESSES **               
AGETDEM  DS    A                                                                
AGETTGT  DS    A                                                                
AFMTPER  DS    A                                                                
AGETREP  DS    A                                                                
AIBUY    DS    A                                                                
AOBUY    DS    A                                                                
AOMKT    DS    A                                                                
AOMKTRNK DS    A                                                                
ATMKTRNK DS    A                                                                
ATOTAL   DS    A                                                                
AODPTLEN DS    A                                                                
AGENOUT  DS    A                                                                
AXTRAN   EQU   (*-AXTRA)/L'AXTRA                                                
*                                                                               
DEMONAME DS    CL7                                                              
LEVELSWS DS    CL32                                                             
SVREP    DS    CL3                                                              
DETLEV   DS    XL1                                                              
*                                                                               
SINGNUM  DS    XL2                MAKE EACH BILL HEADER REC UNIQUE              
*                                                                               
B4       DC    X'03',XL4'00000000'                                              
B8       DC    X'07',XL8'0000000000000000'                                      
CAA      DC    CL5'AAAAA'                                                       
XFF      DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'                           
XFE      DC    XL13'FEFEFEFEFEFEFEFEFEFEFEFEFE'                                 
XFD      DC    XL8'FDFDFDFDFDFDFDFD'                                            
ZEROS    DC    CL4'0000'                                                        
UNKNOWN  DC    C'**UNKNOWN**'                                                   
BLANKS   DC    65C' '                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*----------------------------------ROUTINE ADDRESS LIST                         
         SPACE 1                                                                
ROUTLIST DS    0F                                                               
         DC    C'ICOMPUTE',A(ICOMPUTE),AL4(0),AL4(0)                            
         DC    C'OCOMPUTE',A(OCOMPUTE),AL4(0),AL4(0)                            
         DC    C'IDUMMY  ',A(IDUMMY),AL4(0),AL4(0)                              
         DC    C'ODUMMY  ',A(ODUMMY),AL4(0),AL4(0)                              
         DC    C'IMED    ',A(IMED),AL1(DIMED,0,0,0),AL4(0)                      
         DC    C'OMED    ',A(OMED),AL1(DIMED,0,0,0),AL4(0)                      
         DC    C'ISUBMED ',A(ISUBMED),AL4(0),AL1(DISUBMED,0,0,0)                
         DC    C'OSUBMED ',A(OSUBMED),AL4(0),AL1(DISUBMED,0,0,0)                
         DC    C'ICGR1   ',A(ICGR1),AL4(0),AL4(0)                               
         DC    C'OCGR1   ',A(OCGR1),AL4(0),AL4(0)                               
         DC    C'ICGR2   ',A(ICGR2),AL4(0),AL4(0)                               
         DC    C'OCGR2   ',A(OCGR2),AL4(0),AL4(0)                               
         DC    C'ICLTOFF ',A(ICLTOFF),AL4(0),AL4(0)                             
         DC    C'OCLTOFF ',A(OCLTOFF),AL4(0),AL4(0)                             
*****         DC    C'ICLT    ',A(ICLT),AL4(0),AL1(0,0,0,DICLT)                 
*****         DC    C'OCLT    ',A(OCLT),AL4(0),AL1(0,0,0,DICLT)                 
         DC    C'ICLT    ',A(ICLT),AL4(0),AL1(DICLT,0,0,0)                      
         DC    C'OCLT    ',A(OCLT),AL4(0),AL1(DICLT,0,0,0)                      
         DC    C'IPGR1   ',A(IPGR1),AL4(0),AL4(0)                               
         DC    C'OPGR1   ',A(OPGR1),AL4(0),AL4(0)                               
         DC    C'IPGR2   ',A(IPGR2),AL4(0),AL4(0)                               
         DC    C'OPGR2   ',A(OPGR2),AL4(0),AL4(0)                               
         DC    C'IPRD    ',A(IPRD),AL1(0,DIPRD,0,0),AL4(0)                      
         DC    C'IPRDNM  ',A(IPRD),AL1(0,DIPRD+DIPRDNM,0,0),AL4(0)              
         DC    C'OPRD    ',A(OPRD),AL1(0,DIPRD,0,0),AL4(0)                      
         DC    C'IEST    ',A(IEST),AL1(0,DIEST,DIESTNM,0),AL4(0)                
         DC    C'IESTCD  ',A(IEST),AL1(0,DIEST,0,0),AL4(0)                      
         DC    C'IESTDT  ',A(IEST),AL1(0,DIEST,0,DIESTDT),AL4(0)                
         DC    C'OEST    ',A(OEST),AL1(0,DIEST,0,0),AL4(0)                      
         DC    C'IEDATES ',A(IEDATES),AL1(0,0,0,DIESTDT),AL4(0)                 
         DC    C'OEDATES ',A(OEDATES),AL4(0),AL4(0)                             
         DC    C'ITARGET ',A(ITGT),AL4(0),AL1(DITARGET,0,0,0)                   
         DC    C'OTARGET ',A(OTGT),AL4(0),AL1(DITARGET,0,0,0)                   
         DC    C'IESTFLT ',A(IESTFLT),AL1(0,0,0,DIESTFLT),AL4(0)                
         DC    C'OESTFLT ',A(OESTFLT),AL1(0,0,0,DIESTFLT),AL4(0)                
         DC    C'IRTLSCH ',A(IRTLSCH),AL4(0),AL1(0,0,DIRTLSCH,0)                
         DC    C'ORTLSCH ',A(ORTLSCH),AL4(0),AL1(0,0,DIRTLSCH,0)                
         DC    C'IMGR1   ',A(IMGR1),AL4(0),AL4(0)                               
         DC    C'OMGR1   ',A(OMGR1),AL4(0),AL4(0)                               
         DC    C'IMGR2   ',A(IMGR2),AL4(0),AL4(0)                               
         DC    C'OMGR2   ',A(OMGR2),AL4(0),AL4(0)                               
         DC    C'IMGR3   ',A(IMGR3),AL4(0),AL4(0)                               
         DC    C'OMGR3   ',A(OMGR3),AL4(0),AL4(0)                               
         DC    C'IMKT    ',A(IMKT),AL4(0),AL4(0)                                
         DC    C'IMKTNM  ',A(IMKT),AL4(0),AL4(0)                                
         DC    C'OMKT    ',A(OMKT),AL4(0),AL4(0)                                
         DC    C'IMKTR   ',A(IMKTR),AL4(0),AL1(DIMKTRNK,0,0,0)                  
         DC    C'OMKTR   ',A(OMKTR),AL4(0),AL1(DIMKTRNK,0,0,0)                  
         DC    C'IMKTRNK ',A(IMKTRNK),AL4(0),AL1(DIMKTRNK,0,0,0)                
         DC    C'OMKTRNK ',A(OMKTRNK),AL4(0),AL1(DIMKTRNK,0,0,0)                
         DC    C'IMKTWT  ',A(IMKTWT),AL4(0),AL4(0)                              
         DC    C'ISGR1   ',A(ISGR1),AL4(0),AL4(0)                               
         DC    C'OSGR1   ',A(OSGR1),AL4(0),AL4(0)                               
         DC    C'ISGR2   ',A(ISGR2),AL4(0),AL4(0)                               
         DC    C'OSGR2   ',A(OSGR2),AL4(0),AL4(0)                               
         DC    C'ISTA    ',A(ISTA),AL1(0,DISTA,0,0),AL4(0)                      
         DC    C'OSTA    ',A(OSTA),AL1(0,DISTA,0,0),AL4(0)                      
         DC    C'ICNET   ',A(ICNET),AL4(0),AL4(0)                               
         DC    C'OCNET   ',A(OCNET),AL4(0),AL4(0)                               
         DC    C'ISTANET ',A(ISTANET),AL1(0,DISTA,0,0),AL4(0)                   
         DC    C'OSTANET ',A(OSTANET),AL1(0,DISTA,0,0),AL4(0)                   
         DC    C'INETSTA ',A(INETSTA),AL4(0),AL1(0,DINETSTA,0,0)                
         DC    C'ONETSTA ',A(ONETSTA),AL4(0),AL1(0,DINETSTA,0,0)                
         DC    C'ISTANM  ',A(ISTANM),AL4(0),AL1(0,0,0,DISTANM)                  
         DC    C'OSTANM  ',A(OSTANM),AL4(0),AL1(0,0,0,DISTANM)                  
         DC    C'ISTACITY',A(ISTACITY),AL4(0),AL1(0,0,0,DISTANM)                
         DC    C'OSTACITY',A(OSTACITY),AL4(0),AL1(0,0,0,DISTANM)                
         DC    C'ISTAST  ',A(ISTAST),AL4(0),AL1(0,0,0,DISTANM)                  
         DC    C'OSTAST  ',A(OSTAST),AL4(0),AL1(0,0,0,DISTANM)                  
         DC    C'ISTAAD  ',A(ISTAAD),AL4(0),AL1(0,0,0,DISTANM)                  
         DC    C'OSTAAD  ',A(OSTAAD),AL4(0),AL1(0,0,0,DISTANM)                  
         DC    C'IAFFIL  ',A(IAFFIL),AL1(0,DIAFFIL,0,0),AL4(0)                  
         DC    C'OAFFIL  ',A(OAFFIL),AL1(0,DIAFFIL,0,0),AL4(0)                  
         DC    C'ICHAN   ',A(ICHAN),AL1(0,0,DICHAN,0),AL4(0)                    
         DC    C'OCHAN   ',A(OCHAN),AL1(0,0,DICHAN,0),AL4(0)                    
         DC    C'ISIZE   ',A(ISIZE),AL4(0),AL1(0,0,0,DISIZE)                    
         DC    C'OSIZE   ',A(OSIZE),AL4(0),AL1(0,0,0,DISIZE)                    
         DC    C'IFORMAT ',A(IFORMAT),AL4(0),AL1(0,0,0,DIFORMAT)                
         DC    C'OFORMAT ',A(OFORMAT),AL4(0),AL1(0,0,0,DIFORMAT)                
         DC    C'ISTAFAX ',A(ISTAFAX),AL4(0),AL1(0,0,0,DIFAX)                   
         DC    C'OSTAFAX ',A(OSTAFAX),AL4(0),AL1(0,0,0,DIFAX)                   
         DC    C'IMSO    ',A(IMSO),AL4(0),AL1(0,0,0,DICBLNM)                    
         DC    C'OMSO    ',A(OMSO),AL4(0),AL1(0,0,0,DICBLNM)                    
         DC    C'IICNM   ',A(IICNM),AL4(0),AL1(0,0,0,DICBLNM)                   
         DC    C'OICNM   ',A(OICNM),AL4(0),AL1(0,0,0,DICBLNM)                   
         DC    C'IDPT    ',A(IDPT),AL1(DIDPT,0,0,0),AL4(0)                      
         DC    C'ODPT    ',A(ODPT),AL1(DIDPT,0,0,0),AL4(0)                      
         DC    C'IDPTLEN ',A(IDPTLEN),AL1(DIDPTLEN,0,0,0),AL4(0)                
         DC    C'ODPTLEN ',A(ODPTLEN),AL1(DIDPTLEN,0,0,0),AL4(0)                
         DC    C'ILEN    ',A(ILEN),AL1(0,0,0,DISLN),AL4(0)                      
         DC    C'OLEN    ',A(OLEN),AL1(0,0,0,DISLN),AL4(0)                      
         DC    C'IPER    ',A(IPER),AL4(0),AL4(0)                                
         DC    C'OPER    ',A(OPER),AL4(0),AL4(0)                                
         DC    C'OWEEK   ',A(OWEEK),AL4(0),AL4(0)                               
         DC    C'IFLIGHT ',A(IPER),AL4(0),AL1(0,DICHILD,DIFLIGHT,0)             
         DC    C'OFLIGHT ',A(OFLT),AL4(0),AL1(0,DICHILD,DIFLIGHT,0)             
         DC    C'ODAY    ',A(ODAY),AL4(0),AL4(0)                                
         DC    C'IBUY    ',A(IBUY),AL4(0),AL4(0)                                
         DC    C'OBUY    ',A(OBUY),AL4(0),AL4(0)                                
         DC    C'ILINE   ',A(ILINE),AL4(0),AL4(0)                               
         DC    C'OLINE   ',A(OLINE),AL4(0),AL4(0)                               
         DC    C'IBOOK   ',A(IBOOK),AL1(0,0,DIBOOK,0),AL4(0)                    
         DC    C'OBOOK   ',A(OBOOK),AL1(0,0,DIBOOK,0),AL4(0)                    
         DC    C'IBYID   ',A(IBYID),AL4(0),AL4(0)                               
         DC    C'OBYID   ',A(OBYID),AL4(0),AL4(0)                               
         DC    C'IBYTYPE ',A(IBYTYPE),AL1(0,0,0,DIBYTYPE),AL4(0)                
         DC    C'OBYTYPE ',A(OBYTYPE),AL1(0,0,0,DIBYTYPE),AL4(0)                
         DC    C'IPREP   ',A(IPREP),AL4(0),AL1(DIPREP,0,0,0)                    
         DC    C'OPREP   ',A(OPREP),AL4(0),AL1(DIPREP,0,0,0)                    
         DC    C'ITREP   ',A(ITREP),AL4(0),AL1(DITREP,0,0,0)                    
         DC    C'OTREP   ',A(OTREP),AL4(0),AL1(DITREP,0,0,0)                    
         DC    C'ISREP   ',A(ISREP),AL1(0,0,0,DISREP),AL4(0)                    
         DC    C'OSREP   ',A(OSREP),AL1(0,0,0,DISREP),AL4(0)                    
         DC    C'ICML    ',A(ICML),AL4(0),AL1(DICML,0,0,0)                      
         DC    C'OCML    ',A(OCML),AL4(0),AL1(DICML,0,0,0)                      
         DC    C'ICMLCLS ',A(ICMLCLS),AL4(0),AL1(DICML,0,0,0)                   
         DC    C'OCMLCLS ',A(OCMLCLS),AL4(0),AL1(DICML,0,0,0)                   
         DC    C'ICMLCLNM',A(ICMLCLNM),AL4(0),AL1(DICML,0,0,0)                  
         DC    C'OCMLCLNM',A(OCMLCLNM),AL4(0),AL1(DICML,0,0,0)                  
         DC    C'ICMLNUM ',A(ICMLNUM),AL4(0),AL1(DICML,0,0,0)                   
         DC    C'IADJ    ',A(IADJ),AL4(0),AL4(0)                                
         DC    C'OADJ    ',A(OADJ),AL4(0),AL4(0)                                
         DC    C'INAMES  ',A(INAMES),AL1(0,0,0,DINAMES),AL4(0)                  
         DC    C'ONAMES  ',A(ONAMES),AL1(0,0,0,DINAMES),AL4(0)                  
         DC    C'IMCOM   ',A(IMCOM),AL4(0),AL4(0)                               
         DC    C'OMCOM   ',A(OMCOM),AL4(0),AL4(0)                               
         DC    C'IPRDINT ',A(IPRDINT),AL4(0),AL4(0)                             
         DC    C'OPRDINT ',A(OPRDINT),AL4(0),AL4(0)                             
         DC    C'IADATE  ',A(IADATE),AL4(0),AL1(0,DIDATE,0,0)                   
         DC    C'OADATE  ',A(OADATE),AL4(0),AL1(0,DIDATE,0,0)                   
         DC    C'IADAY   ',A(IADAY),AL4(0),AL1(0,DIDATE,0,0)                    
         DC    C'OADAY   ',A(OADAY),AL4(0),AL1(0,DIDATE,0,0)                    
         DC    C'IATIME  ',A(IATIME),AL4(0),AL1(0,DITIME,0,0)                   
         DC    C'OATIME  ',A(OATIME),AL4(0),AL1(0,DITIME,0,0)                   
         DC    C'IAPROG  ',A(IAPROG),AL4(0),AL1(0,DIAPROG,0,0)                  
         DC    C'OAPROG  ',A(OAPROG),AL4(0),AL1(0,DIAPROG,0,0)                  
         DC    C'IPROG   ',A(IPROG),AL4(0),AL4(0)                               
         DC    C'OPROG   ',A(OPROG),AL4(0),AL4(0)                               
         DC    C'IROT    ',A(IROT),AL4(0),AL4(0)                                
         DC    C'OROT    ',A(OROT),AL4(0),AL4(0)                                
         DC    C'ITIMES  ',A(ITIMES),AL4(0),AL4(0)                              
         DC    C'OTIMES  ',A(OTIMES),AL4(0),AL4(0)                              
         DC    C'ICOM    ',A(ICOM),AL4(0),AL4(0)                                
         DC    C'OCOM    ',A(OCOM),AL4(0),AL4(0)                                
         DC    C'IDR     ',A(IDR),AL4(0),AL1(0,DIDR,0,0)                        
         DC    C'ITEXT   ',A(ITEXT),AL4(0),AL4(0)                               
         DC    C'ICLREP  ',A(ICLREP),AL4(0),AL4(0)                              
         DC    C'OCLREP  ',A(OCLREP),AL4(0),AL4(0)                              
         DC    C'ICLDT   ',A(ICLDT),AL4(0),AL4(0)                               
         DC    C'OCLDT   ',A(OCLDT),AL4(0),AL4(0)                               
         DC    C'ICLSEQ  ',A(ICLSEQ),AL4(0),AL4(0)                              
         DC    C'OCLSEQ  ',A(OCLSEQ),AL4(0),AL4(0)                              
         DC    C'ICLCHK  ',A(ICLCHK),AL4(0),AL4(0)                              
         DC    C'OCLCHK  ',A(OCLCHK),AL4(0),AL4(0)                              
         DC    C'ICLCDT  ',A(ICLCDT),AL4(0),AL4(0)                              
         DC    C'OCLCDT  ',A(OCLCDT),AL4(0),AL4(0)                              
         DC    C'IUDEF   ',A(IUDEF),AL4(0),AL4(0)                               
         DC    C'OUDEF   ',A(OUDEF),AL4(0),AL4(0)                               
         DC    C'ISTDATA ',A(ISTDATA),AL4(0),AL4(0)                             
         DC    C'OSTDATA ',A(OSTDATA),AL4(0),AL4(0)                             
         DC    C'ISTDEM  ',A(ISTDEM),AL4(0),AL1(0,DISTDEM,0,0)                  
         DC    C'OSTDEM  ',A(OSTDEM),AL4(0),AL1(0,DISTDEM,0,0)                  
         DC    C'ISTBDEMP',A(ISTBDEM),AL1(DIDEMP,0,0,DIWEIGHT)                  
         DC    AL1(0,DISTDEM,0,0)                                               
         DC    C'ISTBDEMR',A(ISTBDEM),AL1(DIDEMR,0,0,DIWEIGHT)                  
         DC    AL1(0,DISTDEM,0,0)                                               
         DC    C'ISTBDEMA',A(ISTBDEM),AL1(DIDEMA,0,0,DIWEIGHT)                  
         DC    AL1(0,DISTDEM,0,0)                                               
         DC    C'OSTBDEM ',A(OSTBDEM),AL4(0),AL1(0,DISTDEM,0,0)                 
         DC    C'ISTDOL  ',A(ISTDOL),AL4(0),AL4(0)                              
         DC    C'OSTDOL  ',A(OSTDOL),AL4(0),AL4(0)                              
         DC    C'ISTACC  ',A(ISTACC),AL4(0),AL4(0)                              
         DC    C'OSTACC  ',A(OSTACC),AL4(0),AL4(0)                              
         DC    C'ISPT    ',A(ISPT),AL4(0),AL4(0)                                
         DC    C'ISPTPD  ',A(ISPTPD),AL1(0,DIBYPAID,0,0),AL4(0)                 
         DC    C'ISPTUNPD',A(ISPTUNPD),AL1(0,DIBYPAID,0,0),AL4(0)               
         DC    C'ISPTBILL',A(ISPTBILL),AL4(0),AL4(0)                            
         DC    C'ISPTBLBL',A(ISPTBLBL),AL4(0),AL4(0)                            
         DC    C'OSPT    ',A(OSPT),AL4(0),AL4(0)                                
         DC    C'OSPTBLBL',A(OSPTBLBL),AL4(0),AL4(0)                            
         DC    C'IAUTH   ',A(IAUTH),AL4(0),AL1(0,0,DIAUTH,0)                    
         DC    C'OAUTH   ',A(OAUTH),AL4(0),AL1(0,0,DIAUTH,0)                    
         DC    C'IBYDOL  ',A(IBYDOL),AL4(0),AL4(0)                              
         DC    C'IBYDOLTX',A(IBYDOLTX),AL4(0),AL4(0)                            
         DC    C'IBYDOLM ',A(IBYDOLM),AL4(0),AL4(0)                             
         DC    C'IBYNET  ',A(IBYNET),AL4(0),AL4(0)                              
         DC    C'IBYCOM  ',A(IBYCOM),AL4(0),AL4(0)                              
         DC    C'IBYCST  ',A(IBYCST),AL1(0,DIEFFCST,0,0),AL4(0)                 
         DC    C'IBYPAID ',A(IBYPAID),AL1(0,DIBYPAID,0,0),AL4(0)                
         DC    C'IBYPDTX ',A(IBYPDTX),AL1(0,DIBYPAID,0,0),AL4(0)                
         DC    C'IBYPDM  ',A(IBYPDM),AL1(0,DIBYPAID,0,0),AL4(0)                 
         DC    C'IBYUNPD ',A(IBYUNPD),AL1(0,DIBYPAID,0,0),AL4(0)                
         DC    C'IBYUPDTX',A(IBYUPDTX),AL1(0,DIBYPAID,0,0),AL4(0)               
         DC    C'IBYUNPDM',A(IBYUNPDM),AL1(0,DIBYPAID,0,0),AL4(0)               
         DC    C'IBYNETPD',A(IBYNETPD),AL1(0,DIBYPAID,0,0),AL4(0)               
         DC    C'IBYNETUN',A(IBYNETUN),AL1(0,DIBYPAID,0,0),AL4(0)               
         DC    C'IBYPDEM ',A(IBYDEM),AL1(DIDEMP,0,0,0),AL4(0)                   
         DC    C'IBYPDEMB',A(IBYDEMB),AL1(DIDEMP,0,DIPBON,0),AL4(0)             
         DC    C'IBYPDEMN',A(IBYDEMN),AL1(DIDEMP,0,DIPDEMN,0),AL4(0)            
         DC    C'IBYRDEM ',A(IBYDEM),AL1(DIDEMR,0,0,0),AL4(0)                   
         DC    C'IBYADEM ',A(IBYDEM),AL1(DIDEMA,0,0,0),AL4(0)                   
         DC    C'IBYUDEM ',A(IBYDEM),AL4(0),AL1(0,DIDEMU,0,0)                   
         DC    C'OBYDEM  ',A(ODEMO),AL1(0,0,0,DIWEIGHT),AL4(0)                  
         DC    C'IBYPAR  ',A(IBYAR),AL1(DIDEMP,0,0,0),AL4(0)                    
         DC    C'IBYRAR  ',A(IBYAR),AL1(DIDEMR,0,0,0),AL4(0)                    
         DC    C'IBYAAR  ',A(IBYAR),AL1(DIDEMA,0,0,0),AL4(0)                    
         DC    C'OBYAR   ',A(OAR),AL1(0,0,0,DIWEIGHT),AL4(0)                    
         DC    C'IBYCNT  ',A(IBYCNT),AL4(0),AL4(0)                              
         DC    C'IBYCDATE',A(IBYCDATE),AL4(0),AL4(0)                            
         DC    C'IGSTI   ',A(IGSTI),AL4(0),AL1(0,0,DIGSTI,0)                    
         DC    C'IGSTP   ',A(IGSTP),AL1(0,DIBYPAID,0,0)                         
         DC    AL1(0,0,DIGSTI,0)                                                
         DC    C'IGSTU   ',A(IGSTU),AL1(0,DIBYPAID,0,0)                         
         DC    AL1(0,0,DIGSTI,0)                                                
         DC    C'IGST    ',A(IGST),AL1(0,DIEFFCST,0,0)                          
         DC    AL1(0,0,DIGSTO,0)                                                
         DC    C'IGSTB   ',A(IGSTB),AL4(0),AL1(DIBILLC,0,DIGSTB,0)              
         DC    C'IGSTL   ',A(IGSTL),AL1(0,DIEFFCST,0,0)                         
         DC    AL1(DIBILLC,0,DIGSTO+DIGSTB,0)                                   
         DC    C'IGLDOL  ',A(IGLDOL),AL4(0),AL4(0)                              
         DC    C'IGLNET  ',A(IGLNET),AL4(0),AL4(0)                              
         DC    C'IGLDEM  ',A(IGLDEM),AL1(0,DIGLDEM,0,0),AL4(0)                  
         DC    C'OGLDEM  ',A(ODEMO),AL1(0,DIGLDEM,0,DIWEIGHT),AL4(0)            
         DC    C'IGLCDATE',A(IGLCDATE),AL4(0),AL4(0)                            
         DC    C'IORDOL  ',A(IORDOL),AL1(0,0,DILOCKIN,0),AL4(0)                 
         DC    C'IORSPT  ',A(IORSPT),AL1(0,0,DILOCKIN,0),AL4(0)                 
         DC    C'IORDEM  ',A(IORDEM),AL1(0,DIGLDEM,DILOCKIN,0),AL4(0)           
         DC    C'OORDEM  ',A(ODEMO),AL1(0,DIGLDEM,DILOCKIN,DIWEIGHT)            
         DC    AL4(0)                                                           
         DC    C'ISLSPT  ',A(ISLSPT),AL4(0),AL4(0)                              
         DC    C'ISLDOL  ',A(ISLDOL),AL4(0),AL4(0)                              
         DC    C'ISLDEM  ',A(ISLDEM),AL4(0),AL1(0,0,DISLDEM,0)                  
         DC    C'OSLDEM  ',A(ODEMO),AL1(0,0,0,DIWEIGHT)                         
         DC    AL1(0,0,DISLDEM,0)                                               
         DC    C'IBYPCPP ',A(IBYCPP),AL1(DICPP+DIDEMP,0,0,0),AL4(0)             
         DC    C'IBYRCPP ',A(IBYCPP),AL1(DICPP+DIDEMR,0,0,0),AL4(0)             
         DC    C'IBYACPP ',A(IBYCPP),AL1(DICPP+DIDEMA,0,0,0),AL4(0)             
         DC    C'IGLCPP  ',A(IGLCPP),AL1(DICPP,DIGLDEM,0,0),AL4(0)              
         DC    C'IORCPP  ',A(IORCPP),AL1(DICPP,DIGLDEM,DILOCKIN,0)              
         DC    AL4(0)                                                           
         DC    C'ISLCPP  ',A(ISLCPP),AL1(DICPP,0,0,DIWEIGHT),AL4(0)             
         DC    C'OCPP    ',A(OCPP),AL1(DICPP,0,0,DIWEIGHT),AL4(0)               
         DC    C'IDEMNDX ',A(IDEMNDX),AL1(DIDEMNDX,0,0,0),AL4(0)                
         DC    C'ODEMNDX ',A(ODEMNDX),AL1(DIDEMNDX,0,0,DIWEIGHT),AL4(0)         
         DC    C'IDOLNDX ',A(IDOLNDX),AL1(0,0,DIDOLNDX,0),AL4(0)                
         DC    C'ODOLNDX ',A(ODOLNDX),AL1(0,0,DIDOLNDX,0),AL4(0)                
         DC    C'ICLAMT  ',A(ICLAMT),AL4(0),AL4(0)                              
         DC    C'IBILL   ',A(IBILL),AL4(0),AL4(0)                               
         DC    C'IBILLCST',A(IBILLCST),AL4(0),AL1(DIBILLC,0,0,0)                
         DC    C'IBILLNET',A(IBILLNET),AL4(0),AL4(0)                            
         DC    C'IBILLCOM',A(IBILLCOM),AL4(0),AL4(0)                            
         DC    C'IBILLTAX',A(IBILLTAX),AL4(0),AL4(0)                            
         DC    C'IBILLM  ',A(IBILLM),AL4(0),AL4(0)                              
         DC    C'IBILBL  ',A(IBILBL),AL4(0),AL4(0)                              
         DC    C'IBILBLC ',A(IBILBLC),AL4(0),AL1(DIBILLC,0,0,0)                 
         DC    C'IBILBLNT',A(IBILBLNT),AL4(0),AL4(0)                            
         DC    C'IBILBLCM',A(IBILBLCM),AL4(0),AL4(0)                            
         DC    C'IBILBLTX',A(IBILBLTX),AL4(0),AL4(0)                            
         DC    C'IBILBLM ',A(IBILBLM),AL4(0),AL4(0)                             
         DC    C'OBILBL  ',A(OBILBL),AL4(0),AL4(0)                              
         DC    C'IASSDOL ',A(IASSDOL),AL4(0),AL1(0,0,0,DI2NDCOS)                
         DC    C'IASSNET ',A(IASSNET),AL4(0),AL1(0,0,0,DI2NDCOS)                
         DC    C'IATTCODE',A(IATTCODE),AL4(0),AL4(0)                            
         DC    C'OATTCODE',A(OATTCODE),AL4(0),AL4(0)                            
         DC    C'ICOST2PS',A(ICOST2PS),AL4(0),AL1(0,0,0,DI2NDCOS)               
         DC    C'OCOST2PS',A(OCOST2PS),AL4(0),AL4(0)                            
         DC    C'ISTAFFCH',A(ISTAFFCH),AL1(0,DISTA+DIAFFIL,DICHAN,0)            
         DC    AL4(0)                                                           
         DC    C'OSTAFFCH',A(OSTAFFCH),AL4(0),AL4(0)                            
         SPACE 1                                                                
* NEW ROUTINES FOR MCBUY MACRO                                                  
         DC    C'IESTLIN ',A(IESTLIN),AL4(0),AL4(0)                             
         DC    C'OESTLIN ',A(OESTLIN),AL4(0),AL4(0)                             
         DC    C'IBYDATES',A(IBYDATES),AL4(0),AL4(0)                            
         DC    C'OBYDATES',A(OBYDATES),AL4(0),AL4(0)                            
         DC    C'IBYWKS  ',A(IBYWKS),AL4(0),AL4(0)                              
         DC    C'IDPTCD  ',A(IDPTCD),AL4(0),AL4(0)                              
         SPACE 1                                                                
* CHILD SPOT ROUTINES                                                           
         DC    C'ICASH   ',A(ICASH),AL4(0),AL1(0,DICHILD,0,0)                   
         DC    C'OCASH   ',A(OCASH),AL4(0),AL1(0,DICHILD,0,0)                   
         DC    C'ITRADE  ',A(ITRADE),AL4(0),AL1(0,DICHILD,0,0)                  
         DC    C'OTRADE  ',A(OTRADE),AL4(0),AL1(0,DICHILD,0,0)                  
         DC    C'IDDOL   ',A(IDDOL),AL4(0),AL1(0,DICHILD,0,0)                   
         DC    C'ODDOL   ',A(ODDOL),AL4(0),AL1(0,DICHILD,0,0)                   
         DC    C'IDPCPP  ',A(IDCPP),AL1(DICPP+DIDEMP,0,0,0)                     
         DC    AL1(0,DICHILD,0,0)                                               
         DC    C'IDRCPP  ',A(IDCPP),AL1(DICPP+DIDEMR,0,0,0)                     
         DC    AL1(0,DICHILD,0,0)                                               
         DC    C'IDACPP  ',A(IDCPP),AL1(DICPP+DIDEMA,0,0,0)                     
         DC    AL1(0,DICHILD,0,0)                                               
         DC    C'ODCPP   ',A(ODCPP),AL1(DICPP,0,0,DIWEIGHT)                     
         DC    AL1(0,DICHILD,0,0)                                               
         SPACE 1                                                                
* DOWNLOAD ROUTINES                                                             
         DC    C'IDLMKT  ',A(IDLMKT),AL4(0),AL4(0)                              
         DC    C'ODLEST  ',A(ODLEST),AL4(0),AL4(0)                              
         DC    C'ODLSTA  ',A(ODLSTA),AL4(0),AL4(0)                              
         DC    C'ODLTIMES',A(ODLTIMES),AL4(0),AL4(0)                            
         DC    C'ODLATIME',A(ODLATIME),AL4(0),AL4(0)                            
         DC    C'ODLDAY  ',A(ODLDAY),AL4(0),AL4(0)                              
         DC    C'ODLLEN  ',A(ODLLEN),AL1(DILEN,0,0,0),AL4(0)                    
         DC    C'IDLGRP  ',A(IDLGRP),AL1(DIDEMA,0,0,0),AL4(0)                   
         DC    C'IDLMED  ',A(IDLMED),AL4(0),AL4(0)                              
         DC    C'IDLDATE ',A(IDLDATE),AL4(0),AL4(0)                             
         DC    C'ODLDATE ',A(ODLDATE),AL4(0),AL4(0)                             
         DC    C'IDLSTDAY',A(IDLSTDAY),AL4(0),AL4(0)                            
         DC    C'ODLADATE',A(ODLADATE),AL4(0),AL4(0)                            
         DC    C'IDLWKS  ',A(IDLWKS),AL4(0),AL4(0)                              
         DC    C'ODLWKS  ',A(ODLWKS),AL4(0),AL4(0)                              
         DC    C'IDLSPW  ',A(IDLSPW),AL4(0),AL4(0)                              
         DC    C'ODLSPW  ',A(ODLSPW),AL4(0),AL4(0)                              
         DC    C'IDLDPT  ',A(IDLDPT),AL4(0),AL4(0)                              
         DC    C'IDLDAYS ',A(IDLDAYS),AL4(0),AL4(0)                             
         DC    C'ODLCOST ',A(ODLCOST),AL4(0),AL4(0)                             
         DC    C'ODLGRP  ',A(ODLGRP),AL4(0),AL4(0)                              
         DC    C'IDLTYPE ',A(IDLTYPE),AL4(0),AL4(0)                             
         SPACE 1                                                                
* BILL HEADER ROUTINES                                                          
         DC    C'IBHINV  ',A(IBHINV),AL4(0),AL4(0)                              
         DC    C'OBHINV  ',A(OBHINV),AL4(0),AL4(0)                              
         DC    C'IBHRETL ',A(IBHRETL),AL4(0),AL4(0)                             
         DC    C'OBHRETL ',A(OBHRETL),AL4(0),AL4(0)                             
         DC    C'ISTATYP ',A(ISTATYP),AL4(0),AL4(0)                             
         DC    C'OSTATYP ',A(OSTATYP),AL4(0),AL4(0)                             
         DC    C'IBHTYPE ',A(IBHTYPE),AL4(0),AL4(0)                             
         DC    C'OBHTYPE ',A(OBHTYPE),AL4(0),AL4(0)                             
         DC    C'IAORTYP ',A(IAORTYP),AL4(0),AL4(0)                             
         DC    C'OAORTYP ',A(OAORTYP),AL4(0),AL4(0)                             
         DC    C'ICOMTYP ',A(ICOMTYP),AL4(0),AL4(0)                             
         DC    C'OCOMTYP ',A(OCOMTYP),AL4(0),AL4(0)                             
         DC    C'IBHGRS  ',A(IBHGRS),AL4(0),AL4(0)                              
         DC    C'IBHNET  ',A(IBHNET),AL4(0),AL4(0)                              
         DC    C'IBHACT  ',A(IBHACT),AL4(0),AL4(0)                              
         DC    C'IBHAGY  ',A(IBHAGY),AL4(0),AL4(0)                              
         DC    C'IBHTAX  ',A(IBHTAX),AL4(0),AL4(0)                              
         DC    C'IBHGST  ',A(IBHGST),AL4(0),AL4(0)                              
         DC    C'IBHCGRS ',A(IBHCGRS),AL4(0),AL4(0)                             
         DC    C'IBHCNET ',A(IBHCNET),AL4(0),AL4(0)                             
         DC    C'IBHCACT ',A(IBHCACT),AL4(0),AL4(0)                             
         DC    C'IBHCTAX ',A(IBHCTAX),AL4(0),AL4(0)                             
         DC    C'ICLTACOF',A(ICLTACOF),AL4(0),AL4(0)                            
         DC    C'OCLTACOF',A(OCLTACOF),AL4(0),AL4(0)                            
         DC    C'ICLTINT ',A(ICLTINT),AL4(0),AL4(0)                             
         DC    C'OCLTINT ',A(OCLTINT),AL4(0),AL4(0)                             
         DC    C'IBHPRDI ',A(IBHPRDI),AL4(0),AL4(0)                             
         DC    C'OBHPRDI ',A(OBHPRDI),AL4(0),AL4(0)                             
         DC    C'IAOREFDT',A(IAOREFDT),AL4(0),AL4(0)                            
         DC    C'OAOREFDT',A(OAOREFDT),AL4(0),AL4(0)                            
         DC    C'IAORBAS ',A(IAORBAS),AL4(0),AL4(0)                             
         DC    C'IAORAMT ',A(IAORAMT),AL4(0),AL4(0)                             
         DC    C'IAORAGYN',A(IAORAGYN),AL4(0),AL4(0)                            
         DC    C'OAORAGYN',A(OAORAGYN),AL4(0),AL4(0)                            
         DC    C'IAORINCA',A(IAORINCA),AL4(0),AL4(0)                            
         DC    C'OAORINCA',A(OAORINCA),AL4(0),AL4(0)                            
         DC    C'IAORPCT ',A(IAORPCT),AL4(0),AL4(0)                             
         DC    C'OAORPCT ',A(OAORPCT),AL4(0),AL4(0)                             
         DC    C'IAORRPA ',A(IAORRPA),AL4(0),AL4(0)                             
         DC    C'OAORRPA ',A(OAORRPA),AL4(0),AL4(0)                             
         DC    C'IBHIDT  ',A(IBHIDT),AL4(0),AL4(0)                              
         DC    C'OBHIDT  ',A(OBHIDT),AL4(0),AL4(0)                              
         DC    C'OBHIMN  ',A(OBHIMN),AL4(0),AL4(0)                              
         DC    C'IBHRDT  ',A(IBHRDT),AL4(0),AL4(0)                              
         DC    C'OBHRDT  ',A(OBHRDT),AL4(0),AL4(0)                              
         DC    C'OBHRMN  ',A(OBHRMN),AL4(0),AL4(0)                              
         DC    C'IBHDDT  ',A(IBHDDT),AL4(0),AL4(0)                              
         DC    C'OBHDDT  ',A(OBHDDT),AL4(0),AL4(0)                              
         DC    C'OBHDMN  ',A(OBHDMN),AL4(0),AL4(0)                              
         DC    C'IBHPDT  ',A(IBHPDT),AL4(0),AL4(0)                              
         DC    C'OBHPDT  ',A(OBHPDT),AL4(0),AL4(0)                              
         DC    C'OBHPMN  ',A(OBHPMN),AL4(0),AL4(0)                              
         DC    C'IBHSDT  ',A(IBHSDT),AL4(0),AL4(0)                              
         DC    C'OBHSDT  ',A(OBHSDT),AL4(0),AL4(0)                              
         DC    C'IBHSING ',A(IBHSING),AL4(0),AL4(0)                             
         SPACE 1                                                                
* INFOMERCIAL ROUTINES                                                          
         DC    C'IIFCPO  ',A(IIFCPO),AL4(0),AL4(0)                              
         DC    C'HIFCPO  ',A(HIFCPO),AL4(0),AL4(0)                              
         DC    C'IIFTR   ',A(IIFTR),AL4(0),AL4(0)                               
         DC    C'HIFTR   ',A(HIFTR),AL4(0),AL4(0)                               
         DC    C'IIFBEC  ',A(IIFBEC),AL4(0),AL4(0)                              
         DC    C'HIFBEC  ',A(HIFBEC),AL4(0),AL4(0)                              
         DC    C'IIFPRC  ',A(IIFPRC),AL4(0),AL4(0)                              
         DC    C'HIFPRC  ',A(HIFPRC),AL4(0),AL4(0)                              
         DC    C'IIFTO   ',A(IIFTO),AL4(0),AL4(0)                               
         DC    C'IIFBUD  ',A(IIFBUD),AL4(0),AL4(0)                              
         DC    C'IIFSTAT ',A(IIFSTAT),AL4(0),AL4(0)                             
         DC    C'OIFSTAT ',A(OIFSTAT),AL4(0),AL4(0)                             
         DC    C'IIFTYPE ',A(IIFTYPE),AL4(0),AL4(0)                             
         DC    C'OIFTYPE ',A(OIFTYPE),AL4(0),AL4(0)                             
         DC    C'IIFDATE ',A(IIFDATE),AL4(0),AL4(0)                             
         DC    C'IIFTIME ',A(IIFTIME),AL4(0),AL4(0)                             
         DC    C'OIFTIME ',A(OIFTIME),AL4(0),AL4(0)                             
         DC    C'IIFCTR  ',A(IIFCTR),AL4(0),AL4(0)                              
         DC    C'IIBYCHK ',A(IIBYCHK),AL4(0),AL4(0)                             
         DC    C'OIBYCHK ',A(OIBYCHK),AL4(0),AL4(0)                             
         DC    C'HIFUPS  ',A(HIFUPS),AL4(0),AL4(0)                              
         DC    C'IIFRSPDT',A(IIFRSPDT),AL4(0),AL1(0,0,0,DIRSPDT)                
         DC    C'IIFRSPDW',A(IIFRSPDW),AL4(0),AL4(0)                            
         DC    C'OIFRSPDW',A(OIFRSPDW),AL4(0),AL4(0)                            
         DC    C'IIFRSPDM',A(IIFRSPDM),AL4(0),AL4(0)                            
         DC    C'IIFCOST ',A(IIFCOST),AL4(0),AL4(0)                             
         DC    C'OIFCOST ',A(OIFCOST),AL4(0),AL4(0)                             
         DC    C'IIFINC  ',A(IIFINC),AL4(0),AL4(0)                              
         DC    C'IIFUPINC',A(IIFUPINC),AL4(0),AL4(0)                            
         DC    C'HIFUPINC',A(HIFUPINC),AL4(0),AL4(0)                            
         DC    C'IIFCCMD ',A(IIFCCMD),AL4(0),AL4(0)                             
         DC    C'IIFUPMD ',A(IIFUPMD),AL4(0),AL4(0)                             
         DC    C'HIFUPMD ',A(HIFUPMD),AL4(0),AL4(0)                             
         DC    C'IIFGMD  ',A(IIFGMD),AL4(0),AL4(0)                              
         DC    C'IIFPRFIT',A(IIFPRFIT),AL4(0),AL4(0)                            
         DC    C'IIFTINC ',A(IIFTINC),AL4(0),AL4(0)                             
         SPACE 1                                                                
* COLUMN HEADER ROUTINES                                                        
         DC    C'HDEM    ',A(HDEM),AL1(0,0,0,DIDEMHED),AL4(0)                   
         DC    C'HCPP    ',A(HCPP),AL1(0,0,0,DIDEMHED),AL4(0)                   
         DC    C'HDCPP   ',A(HDCPP),AL1(0,0,0,DIDEMHED),AL4(0)                  
         DC    C'HAR     ',A(HAR),AL1(0,0,0,DIDEMHED),AL4(0)                    
         DC    C'HDEMNDX ',A(HDEMNDX),AL1(0,0,0,DIDEMHED),AL4(0)                
         DC    C'HDOLNDX ',A(HDOLNDX),AL4(0),AL4(0)                             
         DC    C'HPER    ',A(HPER),AL4(0),AL4(0)                                
         DC    C'HCGR1   ',A(HCGR1),AL4(0),AL4(0)                               
         DC    C'HCGR2   ',A(HCGR2),AL4(0),AL4(0)                               
         DC    C'HPGR1   ',A(HPGR1),AL4(0),AL4(0)                               
         DC    C'HPGR2   ',A(HPGR2),AL4(0),AL4(0)                               
         DC    C'HMGR1   ',A(HMGR1),AL4(0),AL4(0)                               
         DC    C'HMGR2   ',A(HMGR2),AL4(0),AL4(0)                               
         DC    C'HMGR3   ',A(HMGR3),AL4(0),AL4(0)                               
         DC    C'HSGR1   ',A(HSGR1),AL4(0),AL4(0)                               
         DC    C'HSGR2   ',A(HSGR2),AL4(0),AL4(0)                               
         DC    C'HNETSTA ',A(HNETSTA),AL4(0),AL4(0)                             
         DC    C'HBUY    ',A(HBUY),AL4(0),AL4(0)                                
         DC    C'HBYID   ',A(HBYID),AL4(0),AL4(0)                               
         DC    C'HSTDEM  ',A(HSTDEM),AL1(0,0,0,DIDEMHED),AL4(0)                 
         DC    C'HSTBDEM ',A(HSTBDEM),AL1(0,0,0,DIDEMHED),AL4(0)                
         DC    C'HCML    ',A(HCML),AL4(0),AL4(0)                                
         DC    C'HUDEF   ',A(HUDEF),AL4(0),AL4(0)                               
         SPACE 1                                                                
* TOTAL ROUTINES                                                                
         DC    C'TOTAL   ',A(TOTAL),AL4(0),AL4(0)                               
         DC    C'TMKTRNK ',A(TMKTRNK),AL4(0),AL1(DIMKTRNK,0,0,0)                
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
         DROP  RB                                                               
         DROP  R7                                                               
         DROP  R8                                                               
         EJECT                                                                  
* START OF DRIVE2                                                               
*                                                                               
         ENTRY DRIVE2                                                           
         DS    0D                                                               
DRIVE2   LR    RB,RE                                                            
         LA    R7,2048(RB)                                                      
         LA    R7,2048(R7)                                                      
         LA    R8,2048(R7)                                                      
         LA    R8,2048(R8)                                                      
         USING DRIVE2,RB,R7,R8                                                  
*                                                                               
         ST    R0,AGENO            SAVE A(GENOUT ROUTINE)                       
         ST    R1,ALVLSWS          SAVE A(PUT TO SORT SWITCHES)                 
*                                                                               
         CLI   GLMODE,GLINPUT      TEST INPUT ROUTINE                           
         BNE   DRIVE2GO                                                         
         L     R4,SBACURCH         YES-ADDRESS THE CHUNKS                       
         USING SCHUNKD,R4                                                       
         LR    R5,R4                                                            
         USING SGLCHNKD,R5                                                      
         L     R1,GLADTENT                                                      
         USING DRIND,R1                                                         
         MVC   INLEVEL,DRINLEV     SAVE LEVEL                                   
         DROP  R1                                                               
*                                                                               
DRIVE2GO BR    RF                                                               
*                                                                               
XIT1     XIT1  ,                                                                
         EJECT                                                                  
* MEDIA COMMENTS I/O ROUTINES                                                   
*                                                                               
IMCOM    MVI   0(R2),1             DUMMY INPUT ROUTINE                          
         B     XIT1                                                             
*                                                                               
OMCOM    L     RE,GLATHID                                                       
         CLC   GLLEVEL,GLDETLEV-GLINTD(RE)   TEST TOTALING                      
         BL    OMCOMX              YES-NO PRINT                                 
         XC    KEY,KEY             COMMENT HEADER KEY                           
         LA    R2,KEY                                                           
         USING COMHDRD,R2                                                       
         MVC   COMKTYPE,=X'0D0C'                                                
         MVC   COMKAGY,SBBAGYMD                                                 
         MVI   COMCTYPE,C'M'                                                    
         MVC   COMKCLT,SBBCLT                                                   
         MVC   COMKPRD+2(1),SBBPRD                                              
         MVC   COMKEST,SBBEST                                                   
         MVC   COMKMKT,SBBMKT                                                   
         CLC   SVCOMKEY,KEY        TEST COMMENT HEADER KEY CHANGE               
         BE    OMCOMX              NO-NO PRINT                                  
         MVC   SVCOMKEY,KEY                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     TEST COMMENT EXISTS                          
         BNE   OMCOMX              NO-NO PRINT                                  
         L     R2,AIO1             GET COMMENT HEADER                           
         ST    R2,AIO                                                           
         GOTO1 GETREC                                                           
         LA    R2,24(R2)                                                        
         SR    R0,R0                                                            
         ZIC   R5,MYOLEN           R5=OUTPUT LENGTH                             
         LA    R6,20               VERY MAX OF 20 LINES                         
*                                                                               
OMCOM2   CLI   0(R2),0             FIND COMMENT ELEMENT                         
         BE    OMCOMX                                                           
         CLI   0(R2),5                                                          
         BE    OMCOM4                                                           
*                                                                               
OMCOM3   IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     OMCOM2                                                           
*                                                                               
OMCOM4   ZIC   R4,1(R2)                                                         
         SH    R4,=H'2'                                                         
         BNP   OMCOM3                                                           
*                                                                               
OMCOM6   LR    RE,R4                                                            
         CR    RE,R5                                                            
         BNH   *+6                                                              
         LR    RE,R5                                                            
         SR    R4,RE                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),2(R2)                                                    
         LA    R3,198(R3)                                                       
         BCT   R6,*+8                                                           
         B     OMCOMX                                                           
         LTR   R4,R4                                                            
         BNP   OMCOM3                                                           
         LA    R2,1(RE,R2)                                                      
         B     OMCOM6                                                           
*                                                                               
OMCOMX   B     XIT1                                                             
         DROP  R2                                                               
         EJECT                                                                  
* I/O ROUTINES FOR BUYLINE COMMENTS                                             
*                                                                               
         SPACE 1                                                                
ICOM     L     RE,SBAIO1           ** INPUT **                                  
         USING BUYRECD,RE                                                       
         XC    0(2,R2),0(R2)                                                    
         CLI   GLARGS+1,X'FF'                                                   
         BNE   *+10                                                             
         XC    0(4,R2),0(R2)                                                    
         CLI   SBMODE,SBPROCSP                                                  
         BNE   XIT1                                                             
         CLC   BUYKEY,SVBUYKEY     TEST CHANGE OF BUY RECORD                    
         BNE   ICOM4                                                            
         CLI   GLARGS+1,X'FF'      NO-COMMENT SEQUENCE NUMBERS MAY              
         BNE   ICOM2                  ALREADY BE SET                            
         CLI   COMSW+4,C'Y'                                                     
         BNE   XIT1                                                             
         MVC   0(2,R2),COMSEQ1                                                  
         MVC   2(2,R2),COMSEQX                                                  
         B     XIT1                                                             
*                                                                               
ICOM2    ZIC   RF,GLARGS+1                                                      
         BCTR  RF,0                                                             
         LA    R1,COMSW(RF)                                                     
         CLI   0(R1),C'Y'                                                       
         BNE   XIT1                                                             
         SLL   RF,1                                                             
         LA    RF,COMSEQ1(RF)                                                   
         MVC   0(2,R2),0(RF)                                                    
         B     XIT1                                                             
*                                                                               
ICOM4    MVC   SVBUYKEY,BUYKEY     NEW BUY RECORD                               
         LA    R0,5                CLEAR SAVED SEQUENCE NUMBERS                 
         LA    R1,COMSEQ1                                                       
         LA    RF,COMSW                                                         
*                                                                               
ICOM6    XC    0(2,R1),0(R1)                                                    
         MVI   0(RF),C'N'                                                       
         LA    R1,2(R1)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,ICOM6                                                         
         LA    R1,WORK2                                                         
         MVI   0(R1),C'C'                                                       
         LA    R6,BDELEM           SEARCH BUYLINE FOR COMMENT ELEMENTS          
         SR    R0,R0                                                            
         SR    R3,R3                                                            
*                                                                               
ICOM8    CLI   0(R6),0                                                          
         BE    ICOM12                                                           
         CLI   0(R6),X'66'                                                      
         BNE   ICOM10                                                           
         USING COMELEM,R6                                                       
         CLI   GLARGS+1,X'FF'      TEST ALL COMMENTS                            
         BE    *+14                                                             
         CLC   CMNUM,GLARGS+1      NO-MATCH COMMENT NUMBER                      
         BNE   ICOM10                                                           
         ZIC   RE,CMLEN                                                         
         SH    RE,=H'3'                                                         
         BNP   ICOM10                                                           
         LH    R3,COMSEQ           NEXT COMMENT SEQUENCE NUMBER                 
         LA    R3,1(R3)                                                         
         CH    R3,=H'32766'                                                     
         BNH   *+6                                                              
         DC    H'0'                                                             
         STH   R3,COMSEQ                                                        
         STH   R3,1(R1)                                                         
         STC   RE,3(R1)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   4(0,R1),CMDATA                                                   
         GOTO1 PUTGENEL            SAVE COMMENT IN NAME POOL                    
*                                                                               
         ZIC   RE,CMNUM            SAVE RELATIVE COMMENT NUMBER                 
         BCTR  RE,0                                                             
         LA    RF,COMSW(RE)                                                     
         MVI   0(RF),C'Y'                                                       
         SLL   RE,1                                                             
         LA    RE,COMSEQ1(RE)                                                   
         STH   R3,0(RE)                                                         
*                                                                               
         CLI   GLARGS+1,X'FF'      TEST ALL COMMENTS                            
         BE    *+12                                                             
         STH   R3,0(R2)            NO-PASS COMMENT SEQ TO DRIVER                
         B     XIT1                                                             
         OC    0(2,R2),0(R2)       YES-PASS FIRST COMMENT SEQ TO DRIVER         
         BNZ   ICOM10                                                           
         STH   R3,0(R2)                                                         
*                                                                               
ICOM10   IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     ICOM8                                                            
*                                                                               
ICOM12   CLI   GLARGS+1,X'FF'      TEST ALL COMMENTS                            
         BNE   XIT1                                                             
         LTR   R3,R3               AND THERE'S AT LEAST ONE COMMENT             
         BZ    XIT1                                                             
         STH   R3,2(R2)            YES-PASS LAST COMMENT SEQ TO DRIVER          
         STH   R3,COMSEQX                                                       
         MVI   COMSW+4,C'Y'                                                     
         B     XIT1                                                             
         DROP  R6,RE                                                            
         SPACE 2                                                                
OCOM     DS    0H                  ** OUTPUT **                                 
         SR    R4,R4                                                            
         SR    R5,R5                                                            
         ICM   R5,3,0(R2)                                                       
         BZ    OCOM6                                                            
         L     R6,AIO3                                                          
         MVI   0(R6),C' '                                                       
         MVC   1(255,R6),0(R6)                                                  
         MVC   256(256,R6),255(R6)                                              
*                                                                               
OCOM2    XC    WORK2,WORK2         COMMENT IS IN GENERAL ELEMENT                
         LA    R1,WORK2            IN NAME POOL                                 
         MVI   0(R1),C'C'                                                       
         STH   R5,1(R1)            COMMENT SEQ NUM                              
         MVI   3(R1),0                                                          
         GOTO1 GETGENEL                                                         
         SR    RE,RE                                                            
         ICM   RE,1,3(R1)          LENGTH OF COMMENT                            
         BZ    OCOM4                                                            
         AR    R4,RE               ACCUMULATE TOTAL LENGTH                      
         BCTR  RE,0                                                             
         EX    RE,*+4              MOVE COMMENT TO BLOCK                        
         MVC   0(0,R6),4(R1)                                                    
*                                                                               
OCOM4    CLI   GLARGS,X'FF'        TEST ALL COMMENTS                            
         BNE   OCOM6                                                            
         OC    2(2,R2),2(R2)       YES-TEST RANGE OF COMMENTS GIVEN             
         BZ    OCOM6                                                            
         LA    R5,1(R5)            YES-GET NEXT COMMENT                         
         CH    R5,2(R2)                                                         
         BH    OCOM6                                                            
         LA    R6,2(RE,R6)                                                      
         LA    R4,1(R4)            ADD ONE FOR SPACE                            
         B     OCOM2                                                            
*                                                                               
OCOM6    LTR   R4,R4               TEST ANY COMMENTS FOUND                      
         BNZ   *+14                                                             
         MVC   0(9,R3),=C'*UNKNOWN*'                                            
         B     XIT1                                                             
         ST    R4,DMCB+4           GET RID OF EXTRA SPACES                      
         L     R6,AIO3                                                          
         GOTO1 SQUASHER,DMCB,(R6)                                               
         L     RE,4(R1)                                                         
         ST    R6,DMCB             SET UP FOR CHOPPER                           
         STC   RE,DMCB             L'SOURCE                                     
         ST    R3,DMCB+4           A(OUTPUT)                                    
         MVC   DMCB+4(1),MYOLEN    LENGTH OF OUTPUT                             
         LA    R1,20               MAX N'LINES                                  
         ST    R1,DMCB+8                                                        
         MVI   DMCB+8,C'P'         SET P3 BYTE 1 TO C'P'                        
         CLI   WIDTHOPT,C'W'       UNLESS ITS WIDE                              
         BNE   *+8                                                              
         MVI   DMCB+8,198          WHEN PRINT LINES ARE 198 APART               
         GOTO1 CHOPPER,DMCB                                                     
         B     XIT1                                                             
         EJECT                                                                  
         EJECT                                                                  
* I/O ROUTINES FOR CLEARANCE RECORD ROWS                                        
*                                                                               
         SPACE 1                                                                
ICLCHK   MVC   0(6,R2),SCCHKNUM    CLEARANCE CHECK NUMBER                       
         MVI   6(R2),0                                                          
         TM    SCCHKIND,SCCHKREC   TEST CHECK IS RECONCILED                     
         BZ    XIT1                                                             
         MVI   6(R2),C'*'                                                       
         B     XIT1                                                             
*                                                                               
ICLCDT   MVC   0(2,R2),SCCHKDT     CLEARANCE CHECK DATE                         
         B     XIT1                                                             
*                                                                               
         USING CLSTEL01,R4                                                      
*                                                                               
ICLREP   MVC   0(1,R2),CLSTREPT    CLEARANCE REP                                
         MVC   1(3,R2),CLSTPYEE                                                 
         CLC   CLSTPYEE,=C'000'    TEST PAYEE=0                                 
         BH    XIT1                                                             
         MVC   0(4,R2),=C'DIR '    YES-DIRECT PAYMENT                           
         B     XIT1                                                             
*                                                                               
ICLSEQ   MVC   0(1,R2),CLSTCLSQ    SEQUENCE NUMBER WITHIN DATE                  
         B     XIT1                                                             
*                                                                               
ICLDT    MVC   0(2,R2),CLSTCLRD    CLEARANCE DATE                               
         B     XIT1                                                             
*                                                                               
*                                                                               
OCLREP   MVC   LABLAREA(13),=C'CLEARANCE REP'                                   
         MVC   CODEAREA(4),0(R2)                                                
         GOTO1 AGENO                                                            
         B     XIT1                                                             
*                                                                               
OCLDT    MVC   LABLAREA(14),=C'CLEARANCE DATE'                                  
         B     OCLDATE                                                          
*                                                                               
OCLCDT   MVC   LABLAREA(10),=C'CHECK DATE'                                      
*                                                                               
OCLDATE  GOTO1 DATCON,DMCB,(2,(R2)),(8,CODEAREA)                                
         GOTO1 AGENO                                                            
         B     XIT1                                                             
*                                                                               
OCLSEQ   ZIC   RE,0(R2)            CLEARANCE DATE SEQUENCE NUMBER               
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R3),DUB                                                      
         B     XIT1                                                             
*                                                                               
OCLCHK   MVC   LABLAREA(12),=C'CHECK NUMBER'                                    
         MVC   CODEAREA(7),0(R2)                                                
         GOTO1 AGENO                                                            
         B     XIT1                                                             
         EJECT                                                                  
*                                                                               
* INPUT ROUTINES FOR COLUMNS                                                    
*                                                                               
         USING SCHUNKD,R4                                                       
ISPT     LA    R6,BIN4             SPOTS                                        
         MVC   0(4,R2),SCSPOTS                                                  
         B     ISPTBUY                                                          
*                                                                               
ISPTPD   LA    R6,BIN4             PAID SPOTS                                   
         MVC   0(4,R2),SCPAYSP                                                  
         B     ISPTBUY                                                          
*                                                                               
ISPTUNPD LA    R6,BIN4             UNPAID SPOTS                                 
         L     RE,SCSPOTS                                                       
         S     RE,SCPAYSP                                                       
         ST    RE,0(R2)                                                         
*                                                                               
ISPTBUY  CLI   SBQMED,C'N'         TEST MEDIA = CANADIAN NETWORK                
         BE    *+12                             OR COMBINED MEDIA               
         CLI   SBQMED,C'C'                                                      
         BNE   ISPT2                                                            
         CLI   SBQNETWK,C'L'       AND NOT REPORTING LOCAL BUYS ONLY            
         BE    ISPT2                                                            
         XC    0(4,R2),0(R2)       YES-DO NOT REPORT SPOTS                      
         B     INX                                                              
*                                                                               
IORSPT   LA    R6,BIN4             ORDERED SPOTS (LOCK-IN)                      
         MVC   0(4,R2),SGLSPTS                                                  
         B     ISPT2                                                            
*                                                                               
ISLSPT   LA    R6,BIN4             STATION LOCKIN SPOTS                         
         L     R1,SBACURCH                                                      
         MVC   0(4,R2),SLSPOTS-SSLCHNKD(R1)                                     
*                                                                               
ISPT2    CLI   SBQMKTWT,C'N'                                                    
         BE    INX                                                              
         MVC   VAL,0(R2)                                                        
         BAS   RE,WGT                                                           
         MVC   4(4,R2),VALWGT                                                   
         TM    GLARGS+(GLIIND-GLARGSD),GLIIWGT  TEST WEIGHTED SPOTS             
         BZ    INX                                                              
         MVC   0(4,R2),VALWGT      YES                                          
         B     INX                                                              
*                                                                               
IDR      LA    R6,BIN4             DIRECT RESPONSE COUNT                        
         MVC   0(4,R2),SCRSVPS                                                  
         B     INX                                                              
*                                                                               
IAUTH    LA    R6,PAK8             AUTHORIZATION DOLLARS                        
         L     R1,SBACHUNK                                                      
         L     R1,4(R1)                                                         
         CVD   R1,DUB                                                           
         ZAP   0(8,R2),DUB                                                      
         B     INX                                                              
*                                                                               
IDOLNDX  CLI   SBMODE,SBPROCGL                                                  
         BNE   IDOLNDX2                                                         
         TM    SBQDATA,SBQDGOAL                                                 
         BO    IGLDOL                                                           
         TM    SBQDATA,SBQDORD                                                  
         BO    IORDOL                                                           
         B     INX                                                              
IDOLNDX2 CLI   SBMODE,SBPROCSP                                                  
         BNE   INX                                                              
         LA    R2,4(R2)                                                         
         B     IBYDOL                                                           
*                                                                               
IBYDOL   DS    0H                                                               
         LA    R6,BIN4             GROSS DOLLARS                                
         MVC   0(4,R2),SCGROSS                                                  
         L     R1,SCGSTI                                                        
         BAS   RE,GSTI                                                          
         L     R1,SCGSTO                                                        
         BAS   RE,GSTO                                                          
         BAS   RE,ROUND                                                         
         B     INX                                                              
*                                                                               
IASSDOL  DS    0H                                                               
         LA    R6,BIN4             ASSIGNED GROSS DOLLARS                       
         MVC   0(4,R2),SCGROSS2                                                 
*                                                                               
*         CLC   SCGROSS2,=X'80000000'  NO SECOND COST?                          
*         BNE   *+14                                                            
*         MVI   7(R2),X'80'         USE AS OUTPUT FLAG                          
*         XC    0(4,R2),0(R2)                                                   
*                                                                               
         L     R1,SCGSTI                                                        
         BAS   RE,GSTI                                                          
         L     R1,SCGSTO                                                        
         BAS   RE,GSTO                                                          
         BAS   RE,ROUND                                                         
         B     INX                                                              
*                                                                               
IBYDOLTX LA    R6,BIN4             GROSS TAX                                    
         MVC   0(4,R2),SCTAX                                                    
         BAS   RE,ROUND                                                         
         B     INX                                                              
*                                                                               
IBYDOLM  LA    R6,BIN4             GROSS DOLLARS MINUS TAX                      
         L     RE,SCGROSS                                                       
         S     RE,SCTAX                                                         
         STCM  RE,15,0(R2)                                                      
         L     R1,SCGSTI                                                        
         BAS   RE,GSTI                                                          
         L     R1,SCGSTO                                                        
         BAS   RE,GSTO                                                          
         BAS   RE,ROUND                                                         
         B     INX                                                              
*                                                                               
IBYNET   DS    0H                                                               
         LA    R6,BIN4             NET DOLLARS                                  
         MVC   0(4,R2),SCNET                                                    
         L     R1,SCGSTI                                                        
         BAS   RE,GSTI                                                          
         L     R1,SCGSTO                                                        
         BAS   RE,GSTO                                                          
         BAS   RE,ROUND                                                         
         B     INX                                                              
*                                                                               
IASSNET  DS    0H                                                               
         LA    R6,BIN4             ASSIGNED NET DOLLARS                         
         MVC   0(4,R2),SCNET2                                                   
*                                                                               
*         CLC   SCNET2,=X'80000000'  NO SECOND COST?                            
*         BNE   *+14                                                            
*         MVI   7(R2),X'80'         USE AS OUTPUT FLAG                          
*         XC    0(4,R2),0(R2)                                                   
*                                                                               
         L     R1,SCGSTI                                                        
         BAS   RE,GSTI                                                          
         L     R1,SCGSTO                                                        
         BAS   RE,GSTO                                                          
         BAS   RE,ROUND                                                         
         B     INX                                                              
*                                                                               
IBYCOM   LA    R6,BIN4             COMMISSION DOLLARS                           
         MVC   0(4,R2),SCCOM                                                    
         BAS   RE,ROUND                                                         
         B     INX                                                              
*                                                                               
IBYCST   LA    R6,BIN4             EFFECTIVE COST                               
         MVC   0(4,R2),SCEFFCST                                                 
         L     R1,SCGSTI                                                        
         BAS   RE,GSTI                                                          
         L     R1,SCGSTO                                                        
         BAS   RE,GSTO                                                          
         BAS   RE,ROUND                                                         
         B     INX                                                              
*                                                                               
IBYPAID  LA    R6,BIN4             PAID DOLLARS                                 
         MVC   0(4,R2),SCPAY                                                    
         L     R1,SCGSTP                                                        
         BAS   RE,GSTI                                                          
         BAS   RE,ROUND                                                         
         B     INX                                                              
*                                                                               
IBYPDTX  LA    R6,BIN4             PAID TAX                                     
         MVC   0(4,R2),SCPAYTX                                                  
         BAS   RE,ROUND                                                         
         B     INX                                                              
*                                                                               
IBYPDM   LA    R6,BIN4             PAID DOLLARS MINUS TAX                       
         L     RE,SCPAY                                                         
         S     RE,SCPAYTX                                                       
         STCM  RE,15,0(R2)                                                      
         L     R1,SCGSTP                                                        
         BAS   RE,GSTI                                                          
         BAS   RE,ROUND                                                         
         B     INX                                                              
*                                                                               
IBYUNPD  LA    R6,BIN4             UNPAID DOLLARS                               
         MVC   0(4,R2),SCUNP                                                    
         L     R1,SCGSTU                                                        
         BAS   RE,GSTI                                                          
         BAS   RE,ROUND                                                         
         B     INX                                                              
*                                                                               
IBYUPDTX LA    R6,BIN4             UNPAID TAX                                   
         MVC   0(4,R2),SCUNPTX                                                  
         BAS   RE,ROUND                                                         
         B     INX                                                              
*                                                                               
IBYUNPDM LA    R6,BIN4             UNPAID DOLLARS MINUS TAX                     
         L     RE,SCUNP                                                         
         S     RE,SCUNPTX                                                       
         STCM  RE,15,0(R2)                                                      
         L     R1,SCGSTU                                                        
         BAS   RE,GSTI                                                          
         BAS   RE,ROUND                                                         
         B     INX                                                              
*                                                                               
IBYNETPD LA    R6,BIN4             NET PAID DOLLARS                             
         MVC   0(4,R2),SCPAYN                                                   
         L     R1,SCGSTP                                                        
         BAS   RE,GSTI                                                          
         BAS   RE,ROUND                                                         
         B     INX                                                              
*                                                                               
IBYNETUN LA    R6,BIN4             NET UNPAID DOLLARS                           
         MVC   0(4,R2),SCUNPN                                                   
         L     R1,SCGSTU                                                        
         BAS   RE,GSTI                                                          
         BAS   RE,ROUND                                                         
         B     INX                                                              
*                                                                               
ICASH    LA    R6,PAK8             CHILD SPOT PAY EXPENDITURE                   
         L     R1,SCCSPAY                                                       
         CVD   R1,DUB                                                           
         MVC   0(8,R2),DUB                                                      
         B     INX                                                              
*                                                                               
ITRADE   LA    R6,PAK8             CHILD SPOT NTP EXPENDITURE                   
         L     R1,SCCSNTP                                                       
         CVD   R1,DUB                                                           
         MVC   0(8,R2),DUB                                                      
         B     INX                                                              
*                                                                               
IDDOL    LA    R6,PAK8             CHILD SPOT DELIVERED DOLLARS                 
         L     R1,SCCSTPT                                                       
         CVD   R1,DUB                                                           
         MVC   0(8,R2),DUB                                                      
         B     INX                                                              
*                                                                               
IGSTI    LA    R6,BIN4             INPUT GST ON GROSS DOLLARS                   
         MVC   0(4,R2),SCGSTI                                                   
         BAS   RE,ROUND                                                         
         B     INX                                                              
*                                                                               
IGST     LA    R6,BIN4             OUTPUT GST ON GROSS DOLLARS                  
         MVC   0(4,R2),SCGSTO                                                   
         BAS   RE,ROUND                                                         
         B     INX                                                              
*                                                                               
IGSTP    LA    R6,BIN4             PAID GST                                     
         MVC   0(4,R2),SCGSTP                                                   
         BAS   RE,ROUND                                                         
         B     INX                                                              
*                                                                               
IGSTU    LA    R6,BIN4             UNPAID GST                                   
         MVC   0(4,R2),SCGSTU                                                   
         BAS   RE,ROUND                                                         
         B     INX                                                              
*                                                                               
IGLDOL   LA    R6,BIN4             GOAL DOLLARS                                 
         MVC   0(4,R2),SGDOL                                                    
         BAS   RE,ROUND                                                         
         B     INX                                                              
*                                                                               
IGLNET   LA    R6,BIN4             NET GOAL DOLLARS                             
         SR    R0,R0                                                            
         L     R1,SGDOL                                                         
         M     R0,=F'85'           SGDOL * .85                                  
         LR    R0,R1                                                            
         SR    R1,R1                                                            
         SRDA  R0,31               TWICE RESULT IN R0R1                         
         D     R0,=F'10000'                                                     
         LTR   R1,R1               SEE IF THE ANSWER IS NEGATIVE                
         BM    *+8                 IF IT IS, WE DON'T NEED TO ADD 1             
         AH    R1,=H'1'            (NOT A LA INSTRUCTION)                       
         SRA   R1,1                DIVIDE BY 2                                  
         SR    R0,R0                                                            
         M     R0,=F'100'                                                       
         ST    R1,0(R2)            ROUNDED ANSWER                               
         BAS   RE,ROUND                                                         
         B     INX                                                              
*                                                                               
IORDOL   LA    R6,BIN4             ORDERED DOLLARS (LOCK-IN)                    
         MVC   0(4,R2),SGLDOL                                                   
         BAS   RE,ROUND                                                         
         B     INX                                                              
*                                                                               
ISLDOL   LA    R6,BIN4             STATION LOCKIN DOLLARS                       
         L     R1,SBACURCH                                                      
         MVC   0(4,R2),SLDOL-SSLCHNKD(R1)                                       
         BAS   RE,ROUND                                                         
         B     INX                                                              
*                                                                               
IBYCNT   LA    R6,BIN4             BUYLINE COUNT                                
         LA    R1,1                                                             
         ST    R1,0(R2)                                                         
         B     INX                                                              
         EJECT                                                                  
* DEMO COLUMN INPUT ROUTINES                                                    
*                                                                               
         SPACE 1                                                                
*                                  ** DEMO INDEX **                             
IDEMNDX  CLI   SBMODE,SBPROCGL     TEST READING GOAL RECORDS                    
         BNE   IDEMNDX2                                                         
         TM    SBQDATA,SBQDGOAL+SBQDORD   YES-TEST INDEXING GOALS               
         BZ    INX                            NO                                
         OC    SBPDEMOS,SBPDEMOS              YES-TEST DEMO MENU                
         BNZ   *+12                               OR DEMO OTPION SET            
         CLI   GLARGS+1,1                 NO-TEST PRIMARY DEMO                  
         BH    INX                            NO-INDEX MAKES NO SENSE           
         TM    SBQDATA,SBQDGOAL                                                 
         BO    IGLDEM                                                           
         TM    SBQDATA,SBQDORD                                                  
         BO    IORDEM                                                           
         B     INX                                                              
IDEMNDX2 CLI   SBMODE,SBPROCSP                                                  
         BNE   INX                                                              
         SR    R5,R5                                                            
         TM    SBQDATA,SBQDGOAL+SBQDORD                                         
         BZ    *+12                                                             
         LA    R2,8(R2)                                                         
         B     IDEMNDX4                                                         
         CLI   SBEDEMTY,C'P'                                                    
         BE    IBYDEM6                                                          
         LA    R2,8(R2)                                                         
         B     IDEMNDX6                                                         
IDEMNDX4 MVI   GLARGS+2,C'P'                                                    
         TM    SBQDATA,SBQDPUR                                                  
         BO    IBYDEM                                                           
IDEMNDX6 MVI   GLARGS+2,C'R'                                                    
         TM    SBQDATA,SBQDRERT                                                 
         BO    IBYDEM                                                           
         MVI   GLARGS+2,C'A'                                                    
         TM    SBQDATA,SBQDAFFD                                                 
         BO    IBYDEM                                                           
         DC    H'0'                                                             
*                                                                               
*                                  ** BUY DEMO **                               
IBYDEM   TM    ININD,INIBON        TEST PROCESSING BONUS DEMOS                  
         BO    INX                 YES-SKIP                                     
         B     IBYDEM2                                                          
*                                  ** BUY BONUS DEMOS **                        
IBYDEMB  TM    ININD,INIBON        TEST PROCESSING BONUS DEMOS                  
         BZ    INX                 NO-SKIP                                      
         B     IBYDEM2                                                          
*                                                                               
IBYDEMN  CLI   GLARGS+1,1          ** NON-ADJUSTED PURCHASED DEMO **            
         BNE   IBYDEM2             ONLY FOR PRIMARY DEMO                        
         CLI   SBEDEMTY,C'P'       CHECK FOR PURCHASED                          
         BNE   INX                                                              
         SR    R5,R5                                                            
         ICM   R5,1,DEMFAC         R5=DEMO FACTOR (0=NO FACTOR)                 
         B     IBYDEM4                                                          
*                                                                               
IBYDEM2  SR    R5,R5               R5=0 - NO DEMO FACTORING                     
*                                                                               
IBYDEM4  CLI   GLARGS+2,0          TEST DEMO TYPE SENSITIVITY                   
         BE    IBYDEM6                                                          
         CLC   SBEDEMTY,GLARGS+2   YES-TEST CORRECT DEMO TYPE                   
         BNE   INX                                                              
*                                                                               
IBYDEM6  CLC   CURBOOK,GLARGS+3    TEST CORRECT BOOK NUMBER                     
         BNE   INX                                                              
         LA    R6,BIN4                                                          
         MVI   EQUIV,C'N'                                                       
         BAS   RE,IBDEM                                                         
         MVC   0(4,R2),VAL                                                      
         MVC   4(4,R2),VALWGT                                                   
         TM    GLARGS+(GLIIND-GLARGSD),GLIIWGT  TEST WEIGHTED DEMO              
         BZ    INX                                                              
         MVC   0(4,R2),VALWGT      YES                                          
         B     INX                                                              
*                                                                               
*                                  ** BUY AVERAGE RATING **                     
IBYAR    CLC   SBEDEMTY,GLARGS+2                                                
         BNE   INX                                                              
         XC    0(16,R2),0(R2)                                                   
         LA    R6,BIN4                                                          
         SR    R5,R5                                                            
         MVI   EQUIV,C'N'                                                       
         BAS   RE,IBDEM                                                         
         BNE   INX                                                              
         MVC   0(4,R2),VAL                                                      
         MVC   4(4,R2),VALWGT                                                   
         MVC   11(1,R2),BYSPOTS                                                 
         CLI   SBQMKTWT,C'N'                                                    
         BE    INX                                                              
         MVC   VAL,8(R2)                                                        
         BAS   RE,WGT                                                           
         MVC   12(4,R2),VALWGT                                                  
         B     INX                                                              
*                                                                               
*                                  ** ORDERED DEMO (LOCK-IN) **                 
IORDEM   LA    R6,BIN4                                                          
         LA    R1,SGLDEM                                                        
         BAS   RE,IGDEM                                                         
         MVC   0(4,R2),VAL                                                      
         MVC   4(4,R2),VALWGT                                                   
         TM    GLARGS+(GLIIND-GLARGSD),GLIIWGT  TEST WEIGHTED DEMO              
         BZ    INX                                                              
         MVC   0(4,R2),VALWGT      YES                                          
         B     INX                                                              
*                                                                               
*                                  ** STATION LOCKIN DEMO **                    
ISLDEM   LA    R6,BIN4                                                          
         CLI   DEMOPT,0            TEST TARGET/SECONDARY DEMOS ONLY             
         BE    *+12                NO                                           
         BAS   RE,CHKDEM           YES - CHECK THIS DEMO FOR TGT/SEC            
         BNE   INX                       NO                                     
         L     RE,SBACURCH                                                      
         LA    RE,SLDEMOS-SSLCHNKD(RE)                                          
         ZIC   R1,GLARGS+1         POINT TO CORRECT DEMO VALUE                  
         BCTR  R1,0                                                             
         SLL   R1,3                                                             
         LA    R1,0(R1,RE)                                                      
         MVC   VAL,0(R1)                                                        
         XC    VALWGT,VALWGT                                                    
         CLI   SBQMKTWT,C'N'       TEST MARKET WEIGHTING                        
         BE    ISLDEM2                                                          
         ZIC   RE,GLARGS+1         YES - TEST THIS DEMO IS RATING               
         BCTR  RE,0                                                             
         MH    RE,=H'3'                                                         
         L     RF,ADEMLST                                                       
         LA    RE,0(RE,RF)                                                      
         CLI   1(RE),C'R'                                                       
         BNE   ISLDEM2                                                          
         BAS   RE,WGT              YES - THEN WEIGHT                            
*                                                                               
ISLDEM2  MVC   0(4,R2),VAL                                                      
         MVC   4(4,R2),VALWGT                                                   
         TM    GLARGS+(GLIIND-GLARGSD),GLIIWGT  TEST WEIGHTED DEMO              
         BZ    INX                                                              
         MVC   0(4,R2),VALWGT      YES                                          
         B     INX                                                              
*                                                                               
*                                  ** GOAL DEMO **                              
IGLDEM   LA    R6,BIN4                                                          
         LA    R1,SGDEM                                                         
         BAS   RE,IGDEM                                                         
         MVC   0(4,R2),VAL                                                      
         MVC   4(4,R2),VALWGT                                                   
         TM    GLARGS+(GLIIND-GLARGSD),GLIIWGT  TEST WEIGHTED DEMO              
         BZ    INX                                                              
         MVC   0(4,R2),VALWGT      YES                                          
         B     INX                                                              
*                                                                               
*                                  ** BUY CPP **                                
IBYCPP   TM    ININD,INIBON        SKIP IF PROCESSING BONUS SPOTS NOW           
         BO    INX                                                              
         CLC   SBEDEMTY,GLARGS+2   TEST CORRECT DEMO TYPE                       
         BNE   INX                                                              
         CLC   CURBOOK,GLARGS+3                                                 
         BNE   INX                                                              
         LA    R6,BIN8                                                          
         SR    R5,R5                                                            
         MVI   EQUIV,C'N'                                                       
         BAS   RE,IBDEM            GET DEMO VALUES                              
         BNE   INX                 (DEMO MIGHT BE REJECTED)                     
         MVC   0(4,R2),BYGROSS     COST                                         
         MVC   4(4,R2),VAL         POINTS                                       
         MVC   8(4,R2),VALWGT      WEIGHTED POINTS                              
         TM    ININD,INIEQUIV      TEST EQUIVALENCING                           
         BZ    IBYCPP2                                                          
         CLI   SBSPPROF,C'D'       YES - TEST EQUIV DOLLARS OR PTS              
         BNE   *+14                                                             
         MVC   0(4,R2),BYEGROSS                                                 
         B     IBYCPP2                                                          
         MVI   EQUIV,C'Y'                                                       
         BAS   RE,IBDEM                                                         
         MVC   4(4,R2),VAL                                                      
         MVC   8(4,R2),VALWGT                                                   
IBYCPP2  BAS   RE,ROUND                                                         
         B     INX                                                              
*                                                                               
*                                  ** CHILD SPOT DELIVERED CPP **               
IDCPP    CLC   SBEDEMTY,GLARGS+2   TEST CORRECT DEMO TYPE                       
         BNE   IDCPPX                                                           
         LA    R6,BIN12                                                         
         SR    R5,R5                                                            
         MVI   EQUIV,C'N'                                                       
         BAS   RE,IBDEM            GET DEMO VALUES                              
         BNE   IDCPPX              (DEMO MIGHT BE REJECTED)                     
         MVC   8(4,R2),VAL                                                      
         MVC   12(4,R2),VALWGT                                                  
         ICM   R1,15,CSDDOL        DELIVERED DOLLARS                            
         BZ    *+16                                                             
         BAS   RE,DOLSPLIT                                                      
         ST    R1,0(R2)            DOLLARS                                      
         ST    R0,4(R2)            PENNIES                                      
         TM    ININD,INIEQUIV      TEST EQUIVALENCING                           
         BZ    IDCPPX                                                           
         CLI   SBSPPROF,C'D'       YES - TEST EQUIV DOLLARS OR PTS              
         BNE   *+6                                                              
         DC    H'0'                WE DON'T HAVE EQUIV DELIVERED DOL            
         MVI   EQUIV,C'Y'                                                       
         BAS   RE,IBDEM                                                         
         MVC   8(4,R2),VAL                                                      
         MVC   12(4,R2),VALWGT                                                  
IDCPPX   B     INX                                                              
*                                                                               
*                                  ** GOAL CPP **                               
IGLCPP   LA    R6,BIN8                                                          
         OC    SBPDEMOS,SBPDEMOS   TEST DEMO MENU OR DEMO OPTION SET            
         BZ    *+12                                                             
         BAS   RE,CHKGLDEM         YES-CHECK THE DEMO IS CORRECT                
         BNE   INX                                                              
         MVC   0(4,R2),SGDOL       COST                                         
         MVC   4(4,R2),SGDEM       RATING                                       
         TM    ININD,INIEQUIV      TEST EQUIVALENCING                           
         BZ    IGLCPP2                                                          
         CLI   SBSPPROF,C'D'       YES - TEST EQUIV DOLLARS OR PTS              
         BNE   *+14                                                             
         MVC   0(4,R2),SGEDOL                                                   
         B     IGLCPP2                                                          
         MVC   4(4,R2),SGEDEM                                                   
IGLCPP2  DS    0H                                                               
         TM    GLARGS+2,X'01'      NET CPP?                                     
         BZ    IGLCPP4              NO                                          
         SR    R0,R0                                                            
         L     R1,0(R2)                                                         
         M     R0,=F'85'           DOL * .85                                    
         LR    R0,R1                                                            
         SR    R1,R1                                                            
         SRDA  R0,31               TWICE RESULT IN R0R1                         
         D     R0,=F'10000'                                                     
         LTR   R1,R1               SEE IF THE ANSWER IS NEGATIVE                
         BM    *+8                 IF IT IS, WE DON'T NEED TO ADD 1             
         AH    R1,=H'1'            (NOT A LA INSTRUCTION)                       
         SRA   R1,1                DIVIDE BY 2                                  
         SR    R0,R0                                                            
         M     R0,=F'100'                                                       
         ST    R1,0(R2)            ROUNDED ANSWER                               
*                                                                               
IGLCPP4  BAS   RE,ROUND                                                         
         CLI   SBQMKTWT,C'N'       TEST MARKET WEIGHTING                        
         BE    INX                                                              
         CLI   SBESTDEM+1,C'R'     YES-ONLY FOR RATINGS                         
         BNE   INX                                                              
         MVC   VAL,4(R2)                                                        
         BAS   RE,WGT                                                           
         MVC   8(4,R2),VALWGT                                                   
         B     INX                                                              
*                                                                               
*                                  ** ORDERED CPP (LOCK-IN) **                  
IORCPP   LA    R6,BIN8                                                          
         OC    SBPDEMOS,SBPDEMOS   TEST DEMO MENU OR DEMO OPTION SET            
         BZ    *+12                                                             
         BAS   RE,CHKGLDEM         YES-CHECK THE DEMO IS CORRECT                
         BNE   INX                                                              
         MVC   0(4,R2),SGLDOL      COST                                         
         MVC   4(4,R2),SGLDEM      RATING                                       
         TM    ININD,INIEQUIV      TEST EQUIVALENCING                           
         BZ    IORCPP2                                                          
         CLI   SBSPPROF,C'D'       YES - TEST EQUIV DOLLARS OR PTS              
         BNE   *+14                                                             
         MVC   0(4,R2),SGLEDOL                                                  
         B     IORCPP2                                                          
         MVC   4(4,R2),SGLEDEM                                                  
IORCPP2  BAS   RE,ROUND                                                         
         CLI   SBQMKTWT,C'N'       TEST MARKET WEIGHTING                        
         BE    INX                                                              
         CLI   SBESTDEM+1,C'R'     YES-ONLY FOR RATINGS                         
         BNE   INX                                                              
         MVC   VAL,4(R2)                                                        
         BAS   RE,WGT                                                           
         MVC   8(4,R2),VALWGT                                                   
         B     INX                                                              
*                                                                               
*                                  ** STATION LOCKIN CPP **                     
ISLCPP   LA    R6,BIN8                                                          
         CLI   DEMOPT,0            TEST TARGET/SECONDARY DEMOS ONLY             
         BE    *+12                                                             
         BAS   RE,CHKDEM           YES-CHECK THIS DEMO FOR TGT/SEC              
         BNE   INX                                                              
         L     R3,SBACURCH                                                      
         MVC   0(4,R2),SLDOL-SSLCHNKD(R3)   DOLLARS                             
         ZIC   R1,GLARGS+1                                                      
         BCTR  R1,0                                                             
         SLL   R1,3                                                             
         LA    R1,SLDEMOS-SSLCHNKD(R1,R3)                                       
         MVC   VAL,0(R1)                                                        
         TM    ININD,INIEQUIV      TEST EQUIVALENCING                           
         BZ    ISLCPP2                                                          
         CLI   SBSPPROF,C'D'       YES - TEST EQUIV DOLLARS OR PTS              
         BNE   *+14                                                             
         MVC   0(4,R2),SLEDOL-SSLCHNKD(R3)                                      
         B     ISLCPP2                                                          
         MVC   VAL,4(R1)                                                        
*                                                                               
ISLCPP2  XC    VALWGT,VALWGT                                                    
         CLI   SBQMKTWT,C'N'       TEST MARKET WEIGHTING                        
         BE    ISLCPP4                                                          
         ZIC   RE,GLARGS+1         YES - TEST THIS DEMO IS RATING               
         BCTR  RE,0                                                             
         MH    RE,=H'3'                                                         
         L     RF,ADEMLST                                                       
         LA    RE,0(RE,RF)                                                      
         CLI   1(RE),C'R'                                                       
         BNE   ISLCPP4                                                          
         BAS   RE,WGT              YES - THEN WEIGHT                            
*                                                                               
ISLCPP4  MVC   4(4,R2),VAL         DEMO                                         
         MVC   8(4,R2),VALWGT      WEIGHTED DEMO                                
         BAS   RE,ROUND                                                         
         B     INX                                                              
         EJECT                                                                  
* DOWNLOAD INPUT ROUTINES                                                       
*                                                                               
         SPACE 1                                                                
IDLMED   MVI   0(R2),C'S'          MEDIA                                        
         CLI   SBQMED,C'T'                                                      
         BE    INX                                                              
         MVI   0(R2),C'?'                                                       
         B     INX                                                              
*                                                                               
IDLMKT   MVC   0(24,R2),SBMKTNM    MARKET                                       
         B     INX                                                              
*                                                                               
IDLTYPE  MVC   0(1,R2),GLARGS      TYPE                                         
         B     INX                                                              
*                                                                               
         USING BUYRECD,R3                                                       
IDLDATE  MVC   0(3,R2),BDSTART     BUY START DATE                               
         B     INX                                                              
*                                                                               
IDLDPT   MVC   0(1,R2),BDDAYPT     BUY DAYPART                                  
         B     INX                                                              
*                                                                               
IDLWKS   MVC   0(1,R2),BDWKS       NUMBER OF BUY WEEKS                          
         B     INX                                                              
*                                                                               
IDLSPW   MVC   0(1,R2),BDNOWK      NUMBER OF SPOTS PER WEEK                     
         B     INX                                                              
*                                                                               
IDLSTDAY DS    0H                  BUY START DAY OF WEEK                        
         GOTO1 DATCON,DMCB,(3,BDSTART),DUB                                      
         GOTO1 GETDAY,(R1),DUB,FULL                                             
         CLC   FULL(3),=C'   '                                                  
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   0(1,R2),0(R1)                                                    
         OI    0(R2),X'F0'                                                      
         B     INX                                                              
*                                                                               
IDLDAYS  ICM   R1,8,BDDAY          DAYS ROTATION                                
         LA    R0,7                                                             
IDLDAYS2 SLL   R1,1                                                             
         MVI   0(R2),C' '                                                       
         LTR   R1,R1                                                            
         BNM   *+8                                                              
         MVI   0(R2),C'X'                                                       
         LA    R2,1(R2)                                                         
         BCT   R0,IDLDAYS2                                                      
         B     INX                                                              
*                                                                               
IDLGRP   CLC   SBEDEMTY,GLARGS+2   GROSS RATING POINTS                          
         BNE   INX                                                              
         MVC   0(4,R2),SCDEMOS                                                  
         B     INX                                                              
         EJECT                                                                  
* STACKING INPUT ROUTINES                                                       
*                                                                               
         SPACE 1                                                                
ISTDATA  MVI   3(R2),1         *** STDATA INPUT ***                             
         B     XIT1                                                             
         SPACE 1                                                                
         USING DSTACKD,R2                                                       
ISTDOL   XC    0(DSTACKL,R2),0(R2) *** ST$ INPUT ***                            
         TM    OPTIND,OPTIACST                                                  
         BO    ISTDOLX             IGNORE IF ACCOUNTING STACK                   
         CLI   SBMODE,SBPROCGL                                                  
         BNE   ISTDOL4                                                          
         ICM   R1,15,SGDOL                                                      
         BZ    ISTDOL2                                                          
         BAS   RE,DOLSPLIT                                                      
         ST    R0,DSGPEN                                                        
         ST    R1,DSGDOL                                                        
*                                                                               
ISTDOL2  ICM   R1,15,SGLDOL                                                     
         BZ    ISTDOLX                                                          
         BAS   RE,DOLSPLIT                                                      
         ST    R0,DSOPEN                                                        
         ST    R1,DSODOL                                                        
*                                                                               
ISTDOL4  CLI   SBMODE,SBPROCSP                                                  
         BNE   ISTDOLX                                                          
         ICM   R1,15,SCGROSS                                                    
         BZ    ISTDOLX                                                          
         BAS   RE,DOLSPLIT                                                      
         ST    R0,DSBPEN                                                        
         ST    R1,DSBDOL                                                        
*                                                                               
ISTDOLX  CLI   INDATA,0                                                         
         BNE   XIT1                                                             
         OC    0(DSTACKL,R2),0(R2)                                              
         BZ    XIT1                                                             
         MVI   INDATA,1                                                         
         B     XIT1                                                             
         SPACE 1                                                                
         USING RSTACKD,R2                                                       
ISTDEM   XC    0(RSTACKL,R2),0(R2) *** STD INPUT ***                            
         TM    OPTIND,OPTIACST                                                  
         BO    ISTDEMX             IGNORE IF ACCOUNTING STACK                   
         CLI   SBMODE,SBPROCGL                                                  
         BNE   ISTDEM4                                                          
         L     R1,SGDOL                                                         
         BAS   RE,DOLSPLIT                                                      
         ST    R0,RSGPEN                                                        
         ST    R1,RSGDOL                                                        
         LA    R1,SGDEM                                                         
         BAS   RE,IGDEM                                                         
         MVC   RSGDEM(4),VAL                                                    
         MVC   RSGDEM+4(4),VALWGT                                               
         LA    R1,SGLDEM                                                        
         BAS   RE,IGDEM                                                         
         MVC   RSODEM(4),VAL                                                    
         MVC   RSODEM+4(4),VALWGT                                               
         LA    R3,SGDOL                                                         
         MVC   RSGDEMCP,RSGDEM                                                  
         LA    R6,SGLDOL                                                        
         MVC   RSODEMCP,RSODEM                                                  
         TM    ININD,INIEQUIV                                                   
         BZ    ISTDEM2                                                          
         CLI   SBSPPROF,C'D'                                                    
         BNE   *+16                                                             
         LA    R3,SGEDOL                                                        
         LA    R6,SGLEDOL                                                       
         B     ISTDEM2                                                          
         LA    R1,SGEDEM                                                        
         BAS   RE,IGDEM                                                         
         MVC   RSGDEMCP(4),VAL                                                  
         MVC   RSGDEMCP+4(4),VALWGT                                             
         LA    R1,SGLEDEM                                                       
         BAS   RE,IGDEM                                                         
         MVC   RSODEMCP(4),VAL                                                  
         MVC   RSODEMCP+4(4),VALWGT                                             
*                                                                               
ISTDEM2  ICM   R1,15,0(R3)                                                      
         BZ    *+16                                                             
         BAS   RE,DOLSPLIT                                                      
         ST    R0,RSGPENCP                                                      
         ST    R1,RSGDOLCP                                                      
         ICM   R1,15,0(R6)                                                      
         BZ    *+16                                                             
         BAS   RE,DOLSPLIT                                                      
         ST    R0,RSOPEN                                                        
         ST    R1,RSODOL                                                        
         B     ISTDEMX                                                          
*                                                                               
ISTDEM4  CLI   SBMODE,SBPROCSP                                                  
         BNE   ISTDEMX                                                          
         SR    R5,R5                                                            
         MVI   EQUIV,C'N'                                                       
         BAS   RE,IBDEM                                                         
         CLI   SBEDEMTY,C'P'                                                    
         BNE   *+16                                                             
         MVC   RSPDEM,VAL                                                       
         MVC   RSPDEM+4(4),VALWGT                                               
         CLI   SBEDEMTY,C'R'                                                    
         BNE   *+16                                                             
         MVC   RSRDEM,VAL                                                       
         MVC   RSRDEM+4(4),VALWGT                                               
         CLI   SBEDEMTY,C'A'                                                    
         BNE   *+16                                                             
         MVC   RSADEM,VAL                                                       
         MVC   RSADEM+4(4),VALWGT                                               
         LA    R3,SCGROSS                                                       
         MVC   RSPDEMCP,RSPDEM                                                  
         MVC   RSRDEMCP,RSRDEM                                                  
         MVC   RSADEMCP,RSADEM                                                  
         TM    ININD,INIEQUIV                                                   
         BZ    ISTDEM6                                                          
         CLI   SBSPPROF,C'D'                                                    
         BNE   *+12                                                             
         LA    R3,SCEGROSS                                                      
         B     ISTDEM6                                                          
         SR    R5,R5                                                            
         MVI   EQUIV,C'Y'                                                       
         BAS   RE,IBDEM                                                         
         CLI   SBEDEMTY,C'P'                                                    
         BNE   *+16                                                             
         MVC   RSPDEMCP,VAL                                                     
         MVC   RSPDEMCP+4(4),VALWGT                                             
         CLI   SBEDEMTY,C'R'                                                    
         BNE   *+16                                                             
         MVC   RSRDEMCP,VAL                                                     
         MVC   RSRDEMCP+4(4),VALWGT                                             
         CLI   SBEDEMTY,C'A'                                                    
         BNE   *+16                                                             
         MVC   RSADEMCP,VAL                                                     
         MVC   RSADEMCP+4(4),VALWGT                                             
*                                                                               
ISTDEM6  DS    0H                                                               
         CLC   SCGROSS,0(R3)       EQUIVALENCING?                               
         BE    ISTDEM8              NO                                          
         SR    R0,R0                                                            
         ICM   R1,15,0(R3)         EGROSS                                       
         L     RF,SCNET            NET                                          
         MR    R0,RF               NET*EGROSS                                   
         SLDA  R0,1                2*(NET*EGROSS) (FOR ROUND)                   
         D     R0,SCGROSS          (2*(NET*EGROSS)/GROSS                        
         LTR   R1,R1               NEGATIVE?                                    
         BM    *+8                 IF IT IS, WE DON'T NEED TO ADD 1             
         AH    R1,=H'1'                                                         
         SRA   R1,1                DIVIDE BY 2                                  
         B     *+8                                                              
ISTDEM8  L     R1,SCNET                                                         
         BAS   RE,DOLSPLIT                                                      
         ST    R0,RSNCPPEN                                                      
         ST    R1,RSNCPDOL                                                      
*                                                                               
         ICM   R1,15,0(R3)         DOLLARS FOR CPP                              
         BZ    *+16                                                             
         BAS   RE,DOLSPLIT                                                      
         ST    R0,RSBPEN                                                        
         ST    R1,RSBDOL                                                        
         L     R1,SCGROSS          GROSS DOLLARS                                
         BAS   RE,DOLSPLIT                                                      
         ST    R0,RSGRSPEN                                                      
         ST    R1,RSGRSDOL                                                      
         L     R1,SCNET            NET DOLLARS                                  
         BAS   RE,DOLSPLIT                                                      
         ST    R0,RSNETPEN                                                      
         ST    R1,RSNETDOL                                                      
         MVC   RSSPOTS,SCSPOTS     SPOTS                                        
*                                                                               
ISTDEMX  CLI   INDATA,0                                                         
         BNE   XIT1                                                             
         OC    0(RSTACKL,R2),0(R2)                                              
         BZ    XIT1                                                             
         MVI   INDATA,1                                                         
         B     XIT1                                                             
         SPACE 1                                                                
         USING BSTACKD,R2                                                       
ISTBDEM  CLC   SBEDEMTY,GLARGS+2   TEST CORRECT DEMO TYPE                       
         BNE   ISTBDEMX                                                         
         ZIC   R3,GLARGS+1                                                      
         BCTR  R3,0                                                             
         MH    R3,=H'3'                                                         
         LA    R1,SBPDEMOS                                                      
         LA    R3,0(R1,R3)         R3=A(DEMO CATEGORY)                          
         CLI   DEMOPT,0            TEST TARGET/SECONDARY DEMOS ONLY             
         BE    *+12                                                             
         BAS   RE,CHKDEM           YES-CHECK THIS DEMO FOR TARGET/SEC           
         BNE   ISTBDEMX            REJECTED                                     
         MVI   HALF,0                                                           
         XC    RTG,RTG                                                          
         XC    IMP,IMP                                                          
         LA    RE,SCDEMOS                                                       
         ZIC   R1,GLARGS+1                                                      
         BCTR  R1,0                                                             
         SLL   R1,3                                                             
         LA    R1,0(R1,RE)         R1=A(DEMO VALUE)                             
         MVC   VAL,0(R1)                                                        
         CLI   1(R3),C'R'          TEST IT'S A RATING                           
         BE    *+12                                                             
         CLI   1(R3),C'E'                                                       
         BNE   ISTBD2                                                           
         MVC   RTG,0(R1)           YES                                          
         MVC   BSRTG(4),VAL                                                     
         TM    OPTIND2,OPTISTI     TEST NEED IMPS ALSO                          
         BZ    *+8                                                              
         MVI   HALF,C'I'           YES                                          
         CLI   SBQMKTWT,C'N'       TEST MARKET WEIGHTING                        
         BE    ISTBD4                                                           
         BAS   RE,WGT              YES-THEN WEIGHT                              
         MVC   BSRTG+4(4),VALWGT                                                
         B     ISTBD4                                                           
*                                                                               
ISTBD2   CLI   1(R3),C'I'          TEST FOR IMPS                                
         BNE   ISTBDEMX                                                         
         MVC   IMP,0(R1)           YES                                          
         MVC   BSIMP,0(R1)                                                      
         TM    OPTIND,OPTISTR      TEST NEED RATING ALSO                        
         BZ    ISTBD4                                                           
         MVI   HALF,C'R'           YES                                          
*                                                                               
ISTBD4   CLI   HALF,0              TEST IF NEED ANOTHER DEMO                    
         BE    ISTBD10                                                          
         MVC   HALF+1(1),2(R3)                                                  
         LA    R1,SBPDEMOS         YES-LOOK FOR IT                              
         LA    R0,L'SBPDEMOS/3                                                  
         LA    RE,SCDEMOS                                                       
*                                                                               
ISTBD6   OC    0(3,R1),0(R1)                                                    
         BZ    ISTBD10                                                          
         CLC   HALF,1(R1)                                                       
         BE    ISTBD8                                                           
         LA    R1,3(R1)                                                         
         LA    RE,8(RE)                                                         
         BCT   R0,ISTBD6                                                        
         B     ISTBD10                                                          
*                                                                               
ISTBD8   MVC   VAL,0(RE)           FOUND-                                       
         CLI   HALF,C'I'                                                        
         BNE   *+20                                                             
         MVC   IMP,0(RE)                                                        
         MVC   BSIMP,VAL                                                        
         B     ISTBD10                                                          
         MVC   RTG,0(RE)                                                        
         MVC   BSRTG(4),VAL                                                     
         CLI   SBQMKTWT,C'N'                                                    
         BE    ISTBD10                                                          
         BAS   RE,WGT                                                           
         MVC   BSRTG+4(4),VALWGT                                                
*                                                                               
ISTBD10  LA    R5,BYGROSS                                                       
         MVC   BSRTGCP,BSRTG       SET RATING AND IMPS FOR CPP                  
         MVC   BSIMPCP,BSIMP                                                    
         TM    ININD,INIEQUIV      TEST NEED TO EQUIVALENCE                     
         BZ    ISTBD12                                                          
         CLI   SBSPPROF,C'D'       YES-TEST EQUIVALENVE DOLLARS                 
         BNE   *+12                                                             
         LA    R5,BYEGROSS         YES                                          
         B     ISTBD12                                                          
         MVC   BSIMPCP,IMP+4       NO-THEN USE EQUIVALENCED DEMOS               
         MVC   BSRTGCP,RTG+4                                                    
         CLI   SBQMKTWT,C'N'                                                    
         BE    ISTBD12                                                          
         MVC   VAL,RTG+4                                                        
         BAS   RE,WGT                                                           
         MVC   BSRTGCP+4(4),VALWGT                                              
*                                                                               
ISTBD12  DS    0H                                                               
         CLC   BYGROSS,0(R5)       EQUIVALENCING?                               
         BE    ISTBD14              NO                                          
         MVC   FULL,BYGROSS                                                     
         SR    R0,R0                                                            
         ICM   R1,15,0(R5)         EGROSS                                       
         L     RF,SCNET            NET                                          
         MR    R0,RF               NET*EGROSS                                   
         SLDA  R0,1                2*(NET*EGROSS) (FOR ROUND)                   
         D     R0,FULL             (2*(NET*EGROSS)/GROSS                        
         LTR   R1,R1               NEGATIVE?                                    
         BM    *+8                 IF IT IS, WE DON'T NEED TO ADD 1             
         AH    R1,=H'1'                                                         
         SRA   R1,1                DIVIDE BY 2                                  
         B     *+8                                                              
ISTBD14  L     R1,SCNET                                                         
         BAS   RE,DOLSPLIT                                                      
         ST    R0,BSNCPPEN                                                      
         ST    R1,BSNCPDOL                                                      
*                                                                               
         ICM   R1,15,0(R5)         DOLLARS FOR CPP                              
         BZ    *+16                                                             
         BAS   RE,DOLSPLIT                                                      
         ST    R0,BSPEN                                                         
         ST    R1,BSDOL                                                         
         ICM   R1,15,BYGROSS       GROSS DOLLARS                                
         BAS   RE,DOLSPLIT                                                      
         ST    R0,BSGRSPEN                                                      
         ST    R1,BSGRSDOL                                                      
         MVC   BSSPOTS+3(1),BYSPOTS      SPOTS                                  
*                                                                               
ISTBDEMX OC    0(BSTACKL,R2),0(R2)                                              
         BZ    XIT1                                                             
         MVI   INDATA,1                                                         
         B     XIT1                                                             
         SPACE 1                                                                
         USING ASTACKD,R2                                                       
ISTACC   XC    0(ASTACKL,R2),0(R2) * STG/STN/STT/STG-/STN-/STC INPUT *          
         TM    OPTIND,OPTIACST                                                  
         BZ    ISTACCX             IGNORE IF NOT ACCOUNTING STACK               
         CLI   SBMODE,SBPROCSP                                                  
         BNE   ISTACC10                                                         
         CLI   GLARGS+1,C'G'       GROSS BUY DOLLARS                            
         BNE   ISTACC2                                                          
         L     R3,SCGROSS          ORDERED                                      
         L     R5,SCPAY            PAID                                         
         L     R6,SCUNP            UNPAID                                       
         B     ISTACC8                                                          
*                                                                               
ISTACC2  CLI   GLARGS+1,C'N'       NET BUY DOLLARS                              
         BNE   ISTACC4                                                          
         L     R3,SCNET            ORDERED                                      
         L     R5,SCPAYN           PAID                                         
         L     R6,SCUNPN           UNPAID                                       
         B     ISTACC8                                                          
*                                                                               
ISTACC4  CLI   GLARGS+1,C'T'       BUY TAX DOLLARS                              
         BNE   ISTACC5                                                          
         L     R3,SCTAX            ORDERED                                      
         L     R5,SCPAYTX          PAID                                         
         L     R6,SCUNPTX          UNPAID                                       
         B     ISTACC8                                                          
*                                                                               
ISTACC5  CLI   GLARGS+1,C'M'       GROSS BUY DOLLARS MINUS TAX                  
         BNE   ISTACC6                                                          
         L     R3,SCGROSS          ORDERED                                      
         S     R3,SCTAX                                                         
         L     R5,SCPAY            PAID                                         
         S     R5,SCPAYTX                                                       
         L     R6,SCUNP            UNPAID                                       
         S     R6,SCUNPTX                                                       
         B     ISTACC8                                                          
*                                                                               
ISTACC6  CLI   GLARGS+1,C'E'       NET BUY DOLLARS MINUS TAX                    
         BNE   ISTACC7                                                          
         L     R3,SCNET            NET                                          
         S     R3,SCTAX                                                         
         L     R5,SCPAYN           NET PAID                                     
         S     R5,SCPAYTX                                                       
         L     R6,SCUNPN           NET UNPAID                                   
         S     R6,SCUNPTX                                                       
         B     ISTACC8                                                          
*                                                                               
ISTACC7  CLI   GLARGS+1,C'C'       GROSS MINUS NET                              
         BNE   ISTACCX                                                          
         L     R3,SCCOM            ORDERED=COMMISSION                           
         L     R5,SCPAY                                                         
         S     R5,SCPAYN           PAID                                         
         L     R6,SCUNP                                                         
         S     R6,SCUNPN           UNPAID                                       
*                                                                               
ISTACC8  LR    R1,R3               ORDERED                                      
         BAS   RE,DOLSPLIT                                                      
         ST    R0,ASOPEN                                                        
         ST    R1,ASODOL                                                        
         LR    R1,R5               PAID                                         
         BAS   RE,DOLSPLIT                                                      
         ST    R0,ASPPEN                                                        
         ST    R1,ASPDOL                                                        
         LR    R1,R6               UNPAID                                       
         BAS   RE,DOLSPLIT                                                      
         ST    R0,ASUPEN                                                        
         ST    R1,ASUDOL                                                        
         B     ISTACCX                                                          
*                                                                               
ISTACC10 CLI   SBMODE,SBPROCBL                                                  
         BNE   ISTACCX                                                          
         LR    R6,R4                                                            
         USING STABELEM,R6                                                      
         L     R1,SBBILGRS         GROSS BILLED                                 
         CLI   GLARGS+1,C'G'                                                    
         BE    ISTACC14                                                         
         L     R1,SBBILNET         NET BILLED                                   
         CLI   GLARGS+1,C'N'                                                    
         BE    ISTACC14                                                         
         CLI   GLARGS+1,C'C'       GROSS MINUS NET BILLED                       
         BNE   ISTACC11                                                         
         L     RF,SBBILGRS                                                      
         SR    RF,R1                                                            
         LR    R1,RF                                                            
         B     ISTACC14                                                         
*                                                                               
ISTACC11 L     R1,SBBILTAX         BILLED TAX                                   
         CLI   GLARGS+1,C'T'                                                    
         BE    ISTACC14                                                         
         CLI   GLARGS+1,C'M'                                                    
         BNE   ISTACC12                                                         
         LR    RF,R1               BILLED MINUS TAX                             
         L     R1,SBBILGRS                                                      
         SR    R1,RF                                                            
         B     ISTACC14                                                         
*                                                                               
ISTACC12 CLI   GLARGS+1,C'E'                                                    
         BNE   ISTACCX                                                          
         LR    RF,R1               NET BILLED MINUS TAX                         
         L     R1,SBBILNET                                                      
         SR    R1,RF                                                            
*                                                                               
ISTACC14 BAS   RE,DOLSPLIT                                                      
         ST    R0,ASBPEN                                                        
         ST    R1,ASBDOL                                                        
         DROP  R6                                                               
*                                                                               
ISTACCX  CLI   INDATA,0                                                         
         BNE   XIT1                                                             
         OC    0(ASTACKL,R2),0(R2)                                              
         BZ    XIT1                                                             
         MVI   INDATA,1                                                         
         B     XIT1                                                             
         EJECT                                                                  
*                                                                               
* MISCELLANEOUS INPUT ROUTINES                                                  
*                                                                               
         SPACE 1                                                                
IGDEM    ST    RE,SVRE             ** GENERAL GOAL DEMO INPUT RTN **            
         XC    VAL,VAL                                                          
         XC    VALWGT,VALWGT                                                    
         OC    SBPDEMOS,SBPDEMOS   TEST DEMO MENU OR DEMO OPTION SET            
         BZ    *+12                                                             
         BAS   RE,CHKGLDEM         YES-CHECK THE DEMO IS CORRECT                
         BNE   IGDEMX                                                           
         MVC   VAL,0(R1)                                                        
         CLI   SBQMKTWT,C'N'       TEST MARKET WEIGHTING                        
         BE    IGDEMX                                                           
         CLI   SBESTDEM+1,C'R'     YES-ONLY FOR RATINGS                         
         BNE   IGDEMX                                                           
         BAS   RE,WGT                                                           
IGDEMX   L     RE,SVRE                                                          
         BR    RE                                                               
         SPACE 1                                                                
*                                  ** GENERAL BUY DEMO INPUT RTN **             
IBDEM    ST    RE,SVRE             INPUT  : R5=FACTOR FOR DEMO ADJUST           
         XC    VAL,VAL                      EQUIV=Y FOR EQUIVALENCING           
         XC    VALWGT,VALWGT                                                    
         CLI   DEMOPT,0            TEST TARGET/SECONDARY DEMOS ONLY             
         BE    *+12                NO                                           
         BAS   RE,CHKDEM           YES - CHECK THIS DEMO FOR TGT/SEC            
         BNE   IBDEMN                    NO                                     
         LA    RE,SCDEMOS          RE=A(DEMO VALUES)                            
         L     RF,ADEMLST          RF=A(DEMO LIST)                              
         CLI   GLARGS+4,0          TEST FOR NON-STANDARD DEMO MODIFIER          
         BE    IBDEM4                                                           
         ZIC   R0,SBENDEM          YES-GET ADDRESS OF DEMO VALUES FOR           
         MVC   HALF,=C'SX'             THIS MODIFIER                            
         CLI   GLARGS+4,C'S'                                                    
         BE    IBDEM2                                                           
         MVC   HALF,=C'PQ'                                                      
         CLI   GLARGS+4,C'P'                                                    
         BE    IBDEM2                                                           
         DC    H'0'                                                             
*                                                                               
IBDEM2   CLC   1(1,RF),HALF                                                     
         BE    IBDEM4                                                           
         CLC   1(1,RF),HALF+1                                                   
         BE    IBDEM4                                                           
         LA    RE,8(RE)                                                         
         LA    RF,3(RF)                                                         
         BCT   R0,IBDEM2                                                        
         B     IBDEMN                                                           
*                                                                               
IBDEM4   ZIC   R1,GLARGS+1         POINT TO CORRECT DEMO VALUE                  
         BCTR  R1,0                                                             
         SLL   R1,3                                                             
         LA    R1,0(R1,RE)                                                      
         MVC   VAL,0(R1)                                                        
         CLI   EQUIV,C'Y'                                                       
         BNE   *+10                                                             
         MVC   VAL,4(R1)                                                        
         LTR   R5,R5               TEST ADJUST PURCHASED DEMO                   
         BZ    IBDEM6                                                           
         L     RF,0(R1)            YES                                          
         M     RE,=F'200'                                                       
         DR    RE,R5                                                            
         LA    RF,1(RF)                                                         
         SRA   RF,1                                                             
         ST    RF,VAL                                                           
         L     RF,ADEMLST                                                       
*                                                                               
IBDEM6   CLI   SBQMKTWT,C'N'       TEST MARKET WEIGHTING                        
         BE    IBDEMY                                                           
         ZIC   RE,GLARGS+1         YES - TEST THIS DEMO IS RATING               
         BCTR  RE,0                                                             
         MH    RE,=H'3'                                                         
         LA    RE,0(RE,RF)                                                      
         CLI   1(RE),C'R'                                                       
         BNE   IBDEMY                                                           
         BAS   RE,WGT              YES - THEN WEIGHT                            
         B     IBDEMY                                                           
*                                                                               
IBDEMN   LTR   RB,RB               CC NE - DEMO REJECTED                        
         B     IBDEMX                                                           
IBDEMY   CR    RB,RB               CC EQ - NORMAL RETURN                        
IBDEMX   L     RE,SVRE                                                          
         BR    RE                                                               
*                                                                               
*                                  ** MARKET WEIGHT ROUTINE **                  
WGT      LR    R0,RE                                                            
         L     RF,VAL                                                           
         ICM   RE,15,SBMKTWGT                                                   
         MR    RE,RE                                                            
         ST    RF,VALWGT                                                        
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 1                                                                
*                                  ** ROUTINES TO ADD IN CANADIAN               
GSTI     LA    RF,GLARGS                                 GST **                 
         TM    GLIIND-GLARGSD(RF),GLIIGSTI                                      
         BZR   RE                                                               
         B     GSTADD                                                           
GSTO     LA    RF,GLARGS                                                        
         TM    GLIIND-GLARGSD(RF),GLIIGSTO                                      
         BZR   RE                                                               
GSTADD   L     RF,0(R2)                                                         
         AR    RF,R1                                                            
         ST    RF,0(R2)                                                         
         BR    RE                                                               
         SPACE 1                                                                
*                                  ** ROUNDING ROUTINE **                       
ROUND    TM    COLIND,COLIRND      TEST ROUNDING REQUIRED                       
         BZR   RE                                                               
         L     R0,0(R2)            YES                                          
         SRDA  R0,31                                                            
         D     R0,=F'100'                                                       
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         ST    R1,0(R2)                                                         
         BR    RE                                                               
         SPACE 1                                                                
*                                  ** SPLIT INTO DOLLARS & PENNIES **           
DOLSPLIT LR    R0,R1                                                            
         SRDA  R0,32                                                            
         D     R0,=F'100'                                                       
         BR    RE                                                               
*                                  ** CHECK DEMO IS TARGET/SECONDARY **         
CHKDEM   LR    R0,RE                                                            
         ZIC   RE,GLARGS+1         FIND THIS COLUMN'S DEMO                      
         BCTR  RE,0                                                             
         MH    RE,=H'3'                                                         
         L     R1,ADEMLST                                                       
         LA    RE,0(RE,R1)                                                      
         LA    R1,SBESTDEM         TARGET DEMO                                  
         CLI   DEMOPT,DEMOTGT      TEST TARGET DEMOS ONLY WANTED                
         BE    *+8                 YES                                          
         LA    R1,3(R1)            NO - THEN SECONDARY DEMOS ONLY               
         CLC   0(3,R1),0(RE)       COMPARE DEMOS                                
         BE    CHKDEMEQ                                                         
         B     CHKDEMNE                                                         
         SPACE 1                                                                
*                                  ** CHECK GOAL DEMO **                        
CHKGLDEM LR    R0,RE                                                            
         SR    RE,RE                                                            
         ICM   RE,1,GLARGS+1                                                    
         BZ    *+6                                                              
         BCTR  RE,0                                                             
         MH    RE,=H'3'                                                         
         LA    RF,SBPDEMOS                                                      
         LA    RE,0(RE,RF)                                                      
         CLC   SBESTDEM(3),0(RE)   MATCH THE DEMOS                              
         BE    CHKDEMEQ                                                         
         B     CHKDEMNE                                                         
*                                                                               
         SPACE 1                                                                
CHKDEMNE BAS   RE,CHKDEMLV                                                      
         MVI   0(RF),C'N'                                                       
         LTR   RE,R0                                                            
         BR    RE                                                               
*                                                                               
CHKDEMEQ BAS   RE,CHKDEMLV                                                      
         MVI   0(RF),C'Y'                                                       
         LR    RE,R0                                                            
         CR    RE,RE                                                            
         BR    RE                                                               
*                                                                               
CHKDEMLV ZIC   RF,INLEVEL                                                       
         A     RF,ALVLSWS                                                       
         BCTR  RF,0                                                             
         BR    RE                                                               
         EJECT                                                                  
* INPUT ROUTINES FOR CLEARANCE STATUS COLUMNS                                   
*                                                                               
         DROP  R5                                                               
         USING CLSTEL01,R5                                                      
ICLAMT   LA    R6,BIN4             CLEARANCE AMOUNT                             
         MVC   0(4,R2),CLSTNET                                                  
         BAS   RE,ROUND                                                         
         B     INX                                                              
         EJECT                                                                  
* INPUT ROUTINES FOR BILLING COLUMNS                                            
*                                                                               
         DROP  R5                                                               
         USING STABELEM,R5                                                      
ISPTBILL LA    R6,BIN4             BILLED SPOTS                                 
         ICM   R1,12,STABSPTS                                                   
         SRA   R1,16                                                            
         ST    R1,0(R2)                                                         
         B     ISPT2                                                            
*                                                                               
ISPTBLBL LA    R6,BIN4             BILLABLE SPOTS                               
         XC    0(8,R2),0(R2)                                                    
         CLI   SBMODE,SBPROCSP                                                  
         BNE   *+18                                                             
         MVC   0(4,R2),SCSPOTS                                                  
         BAS   RE,ROUND                                                         
         B     ISPT2                                                            
         CLI   SBMODE,SBPROCBL                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    8(2,R2),8(R2)                                                    
         MVC   10(2,R2),STABSPTS                                                
         LA    R2,8(R2)                                                         
         B     ISPT2                                                            
*                                                                               
IBILL    LA    R6,BIN4             GROSS BILLED                                 
         MVC   0(4,R2),SBBILGRS                                                 
         L     R1,BILLGST                                                       
         SRA   R1,8                                                             
         BAS   RE,GSTO                                                          
         BAS   RE,ROUND                                                         
         B     INX                                                              
*                                                                               
IBILLCST LA    R6,BIN4             BILLED COST                                  
         MVC   0(4,R2),BILLCOST                                                 
         L     R1,BILLGST                                                       
         SRA   R1,8                                                             
         BAS   RE,GSTO                                                          
         BAS   RE,ROUND                                                         
         B     INX                                                              
*                                                                               
IBILLNET LA    R6,BIN4             NET BILLED                                   
         MVC   0(4,R2),SBBILNET                                                 
         L     R1,BILLGST                                                       
         SRA   R1,8                                                             
         BAS   RE,GSTO                                                          
         BAS   RE,ROUND                                                         
         B     INX                                                              
*                                                                               
IBILLTAX LA    R6,BIN4             BILLED TAX                                   
         MVC   0(4,R2),SBBILTAX                                                 
         BAS   RE,ROUND                                                         
         B     INX                                                              
*                                                                               
IBILLM   LA    R6,BIN4             BILLED MINUS TAX                             
         L     R1,SBBILGRS                                                      
         S     R1,SBBILTAX                                                      
         ST    R1,0(R2)                                                         
         L     R1,BILLGST                                                       
         SRA   R1,8                                                             
         BAS   RE,GSTO                                                          
         BAS   RE,ROUND                                                         
         B     INX                                                              
*                                                                               
IBILLCOM LA    R6,BIN4             BILLED COMMISSION                            
         L     RE,SBBILGRS         COMMISSION = GROSS - NET                     
         L     RF,SBBILNET                                                      
         SR    RE,RF                                                            
         ST    RE,0(R2)                                                         
         BAS   RE,ROUND                                                         
         B     INX                                                              
*                                                                               
IGSTB    LA    R6,BIN4             BILLED GST                                   
         L     RF,BILLGST                                                       
         SRA   RF,8                                                             
         ST    RF,0(R2)                                                         
         BAS   RE,ROUND                                                         
         B     INX                                                              
*                                                                               
IBILBL   LA    R6,BIN4             GROSS BILLABLE                               
         XC    0(8,R2),0(R2)                                                    
         CLI   SBMODE,SBPROCSP                                                  
         BNE   IBILBL2                                                          
         MVC   0(4,R2),SCGROSS                                                  
         L     R1,SCGSTO                                                        
         BAS   RE,GSTO                                                          
         BAS   RE,ROUND                                                         
         B     INX                                                              
IBILBL2  CLI   SBMODE,SBPROCBL                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,4(R2)                                                         
         MVC   0(4,R2),SBBILGRS                                                 
         L     R1,BILLGST                                                       
         SRA   R1,8                                                             
         BAS   RE,GSTO                                                          
         BAS   RE,ROUND                                                         
         B     INX                                                              
*                                                                               
IBILBLTX LA    R6,BIN4             BILLABLE TAX                                 
         XC    0(8,R2),0(R2)                                                    
         CLI   SBMODE,SBPROCSP                                                  
         BNE   *+18                                                             
         MVC   0(4,R2),SCTAX                                                    
         BAS   RE,ROUND                                                         
         B     INX                                                              
         CLI   SBMODE,SBPROCBL                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,4(R2)                                                         
         MVC   0(4,R2),SBBILTAX                                                 
         BAS   RE,ROUND                                                         
         B     INX                                                              
*                                                                               
IBILBLM  LA    R6,BIN4             BILLABLE MINUS TAX                           
         XC    0(8,R2),0(R2)                                                    
         CLI   SBMODE,SBPROCSP                                                  
         BNE   IBILBLM2                                                         
         L     RE,SCGROSS                                                       
         S     RE,SCTAX                                                         
         ST    RE,0(R2)                                                         
         L     R1,SCGSTO                                                        
         BAS   RE,GSTO                                                          
         BAS   RE,ROUND                                                         
         BAS   RE,ROUND                                                         
         B     INX                                                              
IBILBLM2 CLI   SBMODE,SBPROCBL                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,SBBILGRS                                                      
         S     R1,SBBILTAX                                                      
         ST    R1,4(R2)                                                         
         LA    R2,4(R2)                                                         
         L     R1,BILLGST                                                       
         SRA   R1,8                                                             
         BAS   RE,GSTO                                                          
         BAS   RE,ROUND                                                         
         B     INX                                                              
*                                                                               
IBILBLC  LA    R6,BIN4             BILLABLE COST                                
         XC    0(8,R2),0(R2)                                                    
         CLI   SBMODE,SBPROCSP                                                  
         BNE   IBILBLC2                                                         
         MVC   0(4,R2),SCEFFCST                                                 
         L     R1,SCGSTO                                                        
         BAS   RE,GSTO                                                          
         BAS   RE,ROUND                                                         
         B     INX                                                              
IBILBLC2 CLI   SBMODE,SBPROCBL                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   4(4,R2),BILLCOST                                                 
         LA    R2,4(R2)                                                         
         L     R1,BILLGST                                                       
         SRA   R1,8                                                             
         BAS   RE,GSTO                                                          
         BAS   RE,ROUND                                                         
         B     INX                                                              
*                                                                               
IBILBLNT LA    R6,BIN4             NET BILLABLE                                 
         XC    0(8,R2),0(R2)                                                    
         CLI   SBMODE,SBPROCSP                                                  
         BNE   IBILBLN2                                                         
         MVC   0(4,R2),SCNET                                                    
         L     R1,SCGSTO                                                        
         BAS   RE,GSTO                                                          
         BAS   RE,ROUND                                                         
         B     INX                                                              
IBILBLN2 CLI   SBMODE,SBPROCBL                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   4(4,R2),SBBILNET                                                 
         LA    R2,4(R2)                                                         
         L     R1,BILLGST                                                       
         SRA   R1,8                                                             
         BAS   RE,GSTO                                                          
         BAS   RE,ROUND                                                         
         B     INX                                                              
*                                                                               
IBILBLCM LA    R6,BIN4             BILLABLE COMMISSION                          
         XC    0(8,R2),0(R2)                                                    
         CLI   SBMODE,SBPROCSP                                                  
         BNE   *+18                                                             
         MVC   0(4,R2),SCCOM                                                    
         BAS   RE,ROUND                                                         
         B     INX                                                              
         CLI   SBMODE,SBPROCBL                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,SBBILGRS         COMMISSION = GROSS - NET                     
         S     RE,SBBILNET                                                      
         ST    RE,4(R2)                                                         
         LA    R2,4(R2)                                                         
         BAS   RE,ROUND                                                         
         B     INX                                                              
*                                                                               
IGSTL    LA    R6,BIN4             BILLABLE GST                                 
         XC    0(8,R2),0(R2)                                                    
         CLI   SBMODE,SBPROCSP                                                  
         BNE   IGSTL2                                                           
         MVC   0(4,R2),SCGSTO                                                   
         BAS   RE,ROUND                                                         
         B     INX                                                              
IGSTL2   CLI   SBMODE,SBPROCBL                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,BILLGST                                                       
         SRA   RF,8                                                             
         ST    RF,4(R2)                                                         
         LA    R2,4(R2)                                                         
         BAS   RE,ROUND                                                         
         B     INX                                                              
         EJECT                                                                  
* INPUT ROUTINE EXIT FOR COLUMNS                                                
*                                                                               
INX      LTR   R6,R6              TEST THIS IS A COLUMN                         
         BZ    XIT1                                                             
         CLI   INDATA,0           YES-IF THERE'S NO COLUMN DATA YET,            
         BNE   XIT1                                                             
         TM    DRINDS,GLPALDET    TEST PRINT ALL DETAILS                        
         BO    INX2                                                             
         ZIC   RE,0(R6)           OR, TEST ANY DATA IN THIS COL                 
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),1(R6)                                                    
         BE    *+8                                                              
INX2     MVI   INDATA,1           YES-MAKE SURE SORT RECORD'S PASSED            
         B     XIT1                   TO DRIVER'S SORT                          
         EJECT                                                                  
* COLUMN OUTPUT ROUTINES                                                        
*                                                                               
ODUMMY   MVI   0(R3),C' '          ** DUMMY **                                  
         B     XIT1                                                             
         SPACE 1                                                                
*                                  ** SPOTS **                                  
OSPT     TM    GLARGS+(GLOIND-GLARGSD),GLOIWGT  TEST COLUMN IS WEIGHTED         
         BZ    OSPT2                                                            
         CLI   GLHOOK,GLINCOMP     YES-TEST INTERNAL COMPUTE TIME               
         BNE   OSPT2               NO-JUST FORMAT                               
         BAS   RE,WGTCOL           GET WEIGHTED SPOTS                           
         B     XIT1                                                             
*                                  NOT WEIGHTED-                                
OSPT2    CLI   GLHOOK,GLINCOMP     TEST INTERNAL COMPUTE TIME                   
         BE    XIT1                YES-NOT YET                                  
         L     R1,GLADTENT                                                      
         USING DROD,R1                                                          
         TM    DROFORM,X'01'       TEST SPOTS ARE CUMED                         
         BZ    *+12                                                             
         MVI   GLHOOK,GLEDIT       YES-LET DRIVER EDIT                          
         B     XIT1                                                             
         DROP  R1                                                               
         LR    R1,R2               FORMAT THE SPOTS                             
         BAS   RE,OSPTFMT                                                       
******** CLI   SBQMKTWT,C'N'       TEST TO UNWEIGHT                             
******** BE    XIT1                                                             
******** TM    OUTIND,OUTICRMK     YES - TEST ACROSS MARKETS                    
******** BZ    XIT1                                                             
******** OC    4(4,R2),4(R2)       YES - TEST SPOTS ARE WEIGHTED                
******** BZ    XIT1                                                             
******** LR    R5,R2               YES - UNWEIGHT FOR POSSIBLE FUTURE           
******** BAS   RE,UNWEIGHT               POINTS PER SPOT CALCULATION            
         B     XIT1                                                             
*                                                                               
OSPTBLBL L     RE,0(R2)            ** BILLABLE SPOTS **                         
         L     RF,8(R2)                                                         
         SR    RE,RF               SPOTS - BILLED SPOTS                         
         BZ    *+8                                                              
         NI    COLIND2,255-COLIBLB0  NON-ZERO BILLABLE COLUMN                   
         ST    RE,0(R2)                                                         
         LR    R1,R2                                                            
         BAS   RE,OSPTFMT                                                       
         CLI   SBQMKTWT,C'N'       TEST TO UNWEIGHT                             
         BE    XIT1                                                             
         TM    OUTIND,OUTICRMK     YES - TEST ACROSS MARKETS                    
         BZ    XIT1                                                             
         ICM   RE,15,4(R2)         YES - TEST SPOTS ARE WEIGHTED                
         BZ    XIT1                                                             
         ICM   RF,15,12(R2)                                                     
         BZ    XIT1                                                             
         SR    RE,RF               YES - CALCULATE WEIGHTED BILLABLE            
         ST    RE,4(R2)                                                         
         LR    R5,R2               UNWEIGHT FOR POSSIBLE FUTURE                 
         BAS   RE,UNWEIGHT         POINTS PER SPOT CALCULATION                  
         B     XIT1                                                             
*                                                                               
OAUTH    TM    COLIND,COLIRND      ** AUTHORIZATION DOLLARS **                  
         BZ    OAUTH2                                                           
         ZAP   DUB,0(R2)           ROUND OUT THE PENNIES IF REQUIRED            
         BAS   RE,ROUNDPAK                                                      
         ZAP   0(8,R2),DUB                                                      
OAUTH2   MVI   GLHOOK,GLEDIT       LET DRIVER EDIT                              
         B     XIT1                                                             
*                                                                               
ODOLNDX  ICM   R1,15,4(R2)         ** DOLLAR INDEX **                           
         BZ    ODOLNDX2                                                         
         ICM   RF,15,0(R2)                                                      
         BZ    ODOLNDX2                                                         
         SR    R0,R0                                                            
         M     R0,=F'200'                                                       
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BZ    ODOLNDX2                                                         
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         EDIT  (R1),(7,(R3))                                                    
         B     XIT1                                                             
ODOLNDX2 MVI   6(R3),C'0'                                                       
         B     XIT1                                                             
*                                                                               
OBILBL   CLI   GLHOOK,GLINCOMP     ** BILLABLE DOLLARS **                       
         BE    *+12                                                             
         MVI   GLHOOK,GLEDIT                                                    
         B     XIT1                                                             
         L     RE,0(R2)                                                         
         L     RF,4(R2)                                                         
         SR    RE,RF               BUY DOLLARS - BILLED DOLLARS                 
         BZ    *+8                                                              
         NI    COLIND2,255-COLIBLB0   NON-ZERO BILLABLE COLUMN                  
         ST    RE,0(R2)                                                         
         B     XIT1                                                             
*                                                                               
OCASH    LA    R1,CUMCASH          ** CHILD SPOT PAY DOLLARS **                 
         B     OCUME                                                            
*                                                                               
OTRADE   LA    R1,CUMTRADE         ** CHILD SPOT NTP DOLLARS **                 
         B     OCUME                                                            
*                                                                               
ODDOL    LA    R1,CUMDDOL          ** CHILD SPOT DELIVERED DOLLARS **           
         B     OCUME                                                            
*                                                                               
OCUME    MVI   GLHOOK,GLEDIT       GENERAL CUMING ROUTINE                       
         LA    RF,GLARGS                                                        
         USING GLARGSD,RF                                                       
         TM    GLOIND,GLOICUME     TEST THIS IS A CUMED COL                     
         BZ    XIT1                                                             
         DROP  RF                                                               
         TM    GLINDS,GLTOTLIN     YES-TEST TOTAL LINE                          
         BZ    *+14                                                             
         ZAP   0(8,R1),=P'0'       YES-CLEAR THE CUME                           
         B     XIT1                                                             
         AP    0(8,R1),0(8,R2)     ELSE ADD CURRENT VALUE TO CUME               
         ZAP   0(8,R2),0(8,R1)                                                  
         B     XIT1                                                             
*                                                                               
*OASSDOL  DS    0H                  SECOND COST                                 
*         TM    GLINDS,GLTOTLIN     TEST TOTAL LINE                             
*         BNZ   OAD10                YES                                        
*         CLI   7(R2),X'80'         NO SECOND COST?                             
*         BNE   *+14                                                            
*         MVC   0(11,R3),BLNKS                                                  
*         B     XIT1                                                            
*                                                                               
*OAD10    L     R1,0(R2)                                                        
*         EDIT  (R1),(11,(R3)),2,FLOAT=-,ZERO=NOBLANK                           
*         B     XIT1                                                            
         EJECT                                                                  
* DEMO OUTPUT ROUTINES                                                          
*                                                                               
         SPACE 1                                                                
*                                  ** DEMO INDEX **                             
ODEMNDX  CLI   SBQMKTWT,C'N'       TEST MARKET WEIGHTING                        
         BE    ODEMNDX2                                                         
         TM    OUTIND,OUTICRMK     AND ACROSS MARKETS                           
         BZ    ODEMNDX2                                                         
         OC    TOTWGT,TOTWGT       AND THERE'S A TOTAL WEIGHT                   
         BZ    ODEMNDX2                                                         
         ICM   RF,15,4(R2)         YES-TEST DEMOS ARE WEIGHTED                  
         BZ    ODEMNDX2                                                         
         ICM   R1,15,12(R2)                                                     
         BNZ   ODEMNDX3            YES-CALCULATE INDEX ON WEIGHTED DEMS         
*                                                                               
ODEMNDX2 ICM   R1,15,8(R2)                                                      
         BZ    ODEMNDX4                                                         
         ICM   RF,15,0(R2)                                                      
         BZ    ODEMNDX4                                                         
*                                                                               
ODEMNDX3 CVD   R1,DUB                                                           
         ZAP   BIG,DUB                                                          
         MP    BIG,=PL2'200'                                                    
         CVD   RF,DUB                                                           
         DP    BIG,DUB                                                          
         ZAP   DUB,BIG(8)                                                       
         CP    DUB,=P'0'                                                        
         BE    ODEMNDX4                                                         
         AP    DUB,=P'1'                                                        
         ZAP   BIG,DUB                                                          
         DP    BIG,=PL8'2'                                                      
         CVB   R1,BIG                                                           
         EDIT  (R1),(7,(R3))                                                    
         B     XIT1                                                             
*                                                                               
ODEMNDX4 MVI   6(R3),C'0'                                                       
         B     XIT1                                                             
*                                                                               
*                                  ** DEMO **                                   
*                                                                               
ODEMO    TM    GLARGS+(GLOIND-GLARGSD),GLOIWGT  TEST COLUMN IS WEIGHTED         
         BZ    ODEMO2                                                           
         CLI   GLHOOK,GLINCOMP     YES-TEST INTERNAL COMPUTE TIME               
         BNE   ODEMO10             NO-JUST FORMAT                               
         BAS   RE,WGTCOL           GET WEIGHTED DEMO                            
*                                                                               
ODEMO2   CLI   GLHOOK,GLINCOMP     TEST INTERNAL COMPUTE TIME                   
         BNE   ODEMO8                                                           
         TM    COLIND,COLINDR      YES-TEST DEMO ROUNING                        
         BO    XIT1                                                             
         LA    RF,2                YES                                          
*                                                                               
ODEMO4   ICM   R1,15,0(R2)                                                      
         BZ    ODEMO6                                                           
         SR    R0,R0               DIVIDE BY 10 TO GET ROUNDED DEMO             
         SLDA  R0,1                                                             
         D     R0,=F'10'                                                        
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         ST    R1,0(R2)                                                         
*                                                                               
ODEMO6   LA    R2,4(R2)                                                         
         BCT   RF,ODEMO4                                                        
         B     XIT1                                                             
*                                                                               
ODEMO8   MVC   DUB,0(R2)           MAYBE UNWEIGHT                               
         BAS   RE,ODEMWGT                                                       
         MVC   0(4,R2),DUB                                                      
*                                                                               
ODEMO10  L     R1,GLADTENT         EDIT                                         
         USING DROD,R1                                                          
         MVI   DRODEC,0                                                         
         TM    COLIND,COLINDR      TEST NO DEMO ROUNDING                        
         BZ    *+8                                                              
         MVI   DRODEC,1            YES                                          
         MVI   GLHOOK,GLEDIT                                                    
         B     XIT1                                                             
         DROP  R1                                                               
         SPACE 1                                                                
*                                  ** CPP **                                    
OCPP     CLI   GLHOOK,GLINCOMP     TEST INTERNAL COMPUTE HOOK                   
         BE    OCPP2               YES-ALWAYS CALCULATE CPP                     
         TM    COLIND,COLICPPT     TEST SUPPRESS TOTALS                         
         BZ    *+12                                                             
         TM    GLINDS,GLTOTLIN     YES-TEST TOTALING NOW                        
         BO    OCPPX               YES-EXIT                                     
         TM    OUTIND,OUTICRDP     TEST FOR CROSS DAYPARTS NOW                  
         BZ    *+12                                                             
         TM    SBQDPTLN,SBQDLCPP   YES - TEST FOR WHETHER TO PRINT              
         BZ    OCPPX                     NO - EXIT                              
         MVC   CPP,0(R2)                                                        
         OC    TOTWGT,TOTWGT       TEST THERE IS A TOTAL WEIGHT                 
         BZ    OCPP6                                                            
         MVC   DUB,4(R2)           YES-UNWEIGHT IF NECESSARY                    
         BAS   RE,ODEMWGT                                                       
         BNE   OCPP6               NO UNWEIGHTING                               
         MVC   CPPDEM,DUB          DEMO IS UNWEIGHTED-REDO CALCULATION          
         L     R1,4(R2)                                                         
         B     OCPP4                                                            
*                                  INTERNAL COMPUTE -                           
OCPP2    L     R1,0(R2)            COST                                         
         MVC   CPPDEM,4(R2)        POINTS                                       
         ST    R1,4(R2)            STORE COST IN CASE NEEDED IN FUTURE          
*                                                                               
OCPP4    CVD   R1,DOL                                                           
         XC    0(4,R2),0(R2)                                                    
         BAS   RE,OCPPCAL          CPP CALCULATION                              
         BZ    OCPPX                                                            
         MVC   0(4,R2),CPP                                                      
*                                                                               
OCPP6    MVC   DUB(4),CPP          FORMAT THE CPP                               
         MVI   BYTE,0                                                           
         BAS   RE,OCPPFMT                                                       
*                                                                               
OCPPX    B     XIT1                                                             
         SPACE 1                                                                
*                                  ** CHILD SPOT DELIVERED CPP **               
ODCPP    CLI   GLHOOK,GLINCOMP     TEST INTERNAL COMPUTE HOOK                   
         BE    ODCPP2              YES-ALWAYS CALCULATE CPP                     
         TM    COLIND,COLICPPT     TEST SUPPRESS TOTALS                         
         BZ    *+12                                                             
         TM    GLINDS,GLTOTLIN     YES-TEST TOTALING NOW                        
         BO    ODCPPX              YES-EXIT                                     
         TM    OUTIND,OUTICRDP     TEST FOR CROSS DAYPARTS NOW                  
         BZ    *+12                                                             
         TM    SBQDPTLN,SBQDLCPP   YES - TEST FOR WHETHER TO PRINT              
         BZ    ODCPPX                    NO - EXIT                              
         MVC   CPP,0(R2)                                                        
         OC    TOTWGT,TOTWGT       TEST THERE IS A TOTAL WEIGHT                 
         BZ    ODCPP6                                                           
         MVC   DUB,8(R2)           YES-UNWEIGHT IF NECESSARY                    
         BAS   RE,ODEMWGT                                                       
         BNE   ODCPP6              NO UNWEIGHTING                               
         MVC   CPPDEM,DUB          DEMO IS UNWEIGHTED-REDO CALCULATION          
         MVC   DOL,4(R2)                                                        
         B     ODCPP4                                                           
*                                  INTERNAL COMPUTE -                           
ODCPP2   LR    R5,R2                                                            
         BAS   RE,CALCDOL          ADD PENNIES TO DOLLARS                       
         MVC   CPPDEM,8(R2)                                                     
         MVC   4(8,R2),DOL         STORE COST IN CASE NEEDED IN FUTURE          
*                                                                               
ODCPP4   XC    0(4,R2),0(R2)                                                    
         BAS   RE,OCPPCAL          CALCULATE CPP                                
         BZ    ODCPPX                                                           
         MVC   0(4,R2),CPP                                                      
*                                                                               
ODCPP6   MVC   DUB(4),CPP          FORMAT CPP                                   
         MVI   BYTE,0                                                           
         BAS   RE,OCPPFMT                                                       
*                                                                               
ODCPPX   B     XIT1                                                             
*                                                                               
*                                                                               
*                                  ** AVERAGE RATING **                         
OAR      CLI   GLHOOK,GLINCOMP     TEST INTERNAL COMPUTE TIME                   
         BE    OAR2                YES-CALCULATE POINTS PER SPOT                
         OC    TOTWGT,TOTWGT       NO-TEST THERE'S A TOTAL WEIGHT               
         BZ    OAR4                                                             
         MVC   DUB,0(R2)           YES-UNWEIGHT IF NECESSARY                    
         BAS   RE,ODEMWGT                                                       
         BNE   OAR4                                                             
         MVC   0(4,R2),DUB                                                      
         LA    R5,8(R2)            UNWEIGHT SPOTS ALSO                          
         BAS   RE,UNWEIGHT                                                      
*                                                                               
OAR2     SR    RE,RE               CALCULATE POINTS PER SPOT                    
         L     RF,0(R2)                                                         
         SLDA  RE,1                                                             
         XC    0(4,R2),0(R2)                                                    
         ICM   R1,15,8(R2)                                                      
         BZ    OAR4                                                             
         TM    COLIND,COLINDR      TEST DEMOS ROUNDED                           
         BO    *+8                                                              
         MH    R1,=H'10'           YES-DIVIDE BY 10                             
         DR    RE,R1                                                            
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         ST    RF,0(R2)                                                         
*                                                                               
OAR4     L     R1,GLADTENT         EDIT                                         
         USING DROD,R1                                                          
         MVI   DRODEC,0                                                         
         TM    COLIND,COLINDR      TEST NO DEMO ROUNDING                        
         BZ    *+8                                                              
         MVI   DRODEC,1            YES                                          
         MVI   GLHOOK,GLEDIT                                                    
         B     XIT1                                                             
         DROP  R1                                                               
         EJECT                                                                  
* DOWNLOAD OUTPUT ROUTINES                                                      
*                                                                               
         SPACE 1                                                                
ODLEST   MVC   SBBEST,1(R2)        ESTIMATE                                     
         CLI   SBBEST,0            TEST ESTIMATE=0                              
         BNE   *+10                                                             
         MVC   SBBEST,SBQEST       ESTIMATE MUST BE REQUEST START EST           
         EDIT  SBBEST,(3,(R3)),ALIGN=LEFT                                       
         B     XIT1                                                             
*                                                                               
ODLSTA   MVC   0(5,R3),0(R2)       STATION                                      
         CLI   4(R3),C' '                                                       
         BNE   *+8                                                              
         MVI   4(R3),C'T'                                                       
         B     XIT1                                                             
*                                                                               
ODLADATE CLC   0(2,R2),EFFS        AFFID DATE                                   
         BNE   *+12                                                             
         MVI   PRTSW,C'N'                                                       
         B     XIT1                                                             
         GOTO1 DATCON,DMCB,(2,0(R2)),(R3)                                       
         B     XIT1                                                             
*                                                                               
ODLDATE  DS    0H                  START DATE                                   
         GOTO1 DATCON,DMCB,(3,0(R2)),(R3)                                       
         B     XIT1                                                             
*                                                                               
ODLDAY   CLI   0(R2),8             AFFID DAY OF WEEK                            
         BNE   *+12                                                             
         MVI   PRTSW,C'N'                                                       
         B     XIT1                                                             
         MVC   0(1,R3),0(R2)                                                    
         OI    0(R3),X'F0'                                                      
         B     XIT1                                                             
*                                                                               
ODLTIMES LA    R0,2                BUYLINE TIMES                                
         B     ODLTIME                                                          
*                                                                               
ODLATIME CLC   0(2,R2),EFFS        AFFID TIME                                   
         BNE   *+12                                                             
         MVI   PRTSW,C'N'                                                       
         B     XIT1                                                             
         LA    R0,1                                                             
         B     ODLTIME                                                          
*                                                                               
ODLTIME  SR    RE,RE                                                            
ODLTIME2 ICM   RE,3,0(R2)                                                       
         CH    RE,=H'600'          0600 ---> 3000                               
         BNL   *+8                                                              
         AH    RE,=H'2400'                                                      
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(4,R3),DUB                                                      
         LA    R2,2(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R0,ODLTIME2                                                      
         B     XIT1                                                             
*                                                                               
ODLLEN   ZIC   RE,0(R2)            SPOT LENGTH                                  
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R3),DUB                                                      
         B     XIT1                                                             
*                                                                               
ODLWKS   DS    0H                  NUMBER OF BUY WEEKS, AND                     
ODLSPW   ZIC   RE,0(R2)            SPOTS PER WEEK                               
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(2,R3),DUB                                                      
         B     XIT1                                                             
*                                                                               
ODLCOST  DS    0H                  COST                                         
         EDIT  (B4,0(R2)),(10,(R3)),2,FILL=0,ZERO=NOBLANK                       
         B     XIT1                                                             
*                                                                               
ODLGRP   DS    0H                  GRPS                                         
         EDIT  (B4,0(R2)),(4,(R3)),1,FILL=0,ZERO=NOBLANK                        
         B     XIT1                                                             
         EJECT                                                                  
* STACKING OUTPUT ROUTINES                                                      
*                                                                               
         SPACE 1                                                                
OSTDATA  LA    R0,8            *** STDATA OUTPUT ***                            
         LA    R4,STACKDEF                                                      
*                                                                               
OSTDATA2 CLI   0(R4),0                                                          
         BE    XIT1                                                             
         BAS   RE,STCHK                                                         
         BNE   OSTDATA6                                                         
         LA    R1,STACKTAB                                                      
*                                                                               
OSTDATA4 CLI   0(R1),0                                                          
         BE    OSTDATA5                                                         
         CLC   0(1,R4),0(R1)                                                    
         BE    *+12                                                             
         LA    R1,5(R1)                                                         
         B     OSTDATA4                                                         
         MVC   0(4,R3),1(R1)                                                    
*                                                                               
OSTDATA5 LA    R3,198(R3)                                                       
*                                                                               
OSTDATA6 LA    R4,2(R4)                                                         
         BCT   R0,OSTDATA2                                                      
         B     XIT1                                                             
         SPACE 2                                                                
STACKTAB DC    AL1(STSPACE),X'00',CL3' '                                        
         DC    AL1(STDIFF),C'DIFF'                                              
         DC    AL1(STNDX),C'INDX'                                               
         DC    AL1(STRTG),C'RTG '                                               
         DC    AL1(STIMP),C'IMP '                                               
         DC    AL1(STCPP),C'CPP '                                               
         DC    AL1(STCPM),C'CPM '                                               
         DC    AL1(STGOAL),C'GOAL'                                              
         DC    AL1(STGCPP),C'GCPP'                                              
         DC    AL1(STGDOL),C'GOL$'                                              
         DC    AL1(STORD),C'ORD '                                               
         DC    AL1(STOCPP),C'OCPP'                                              
         DC    AL1(STPUR),C'PUR '                                               
         DC    AL1(STAVPCH),C'APUR'                                             
         DC    AL1(STPCPP),C'PCPP'                                              
         DC    AL1(STACH),C'ACH '                                               
         DC    AL1(STAVACH),C'AACH'                                             
         DC    AL1(STRCPP),C'RCPP'                                              
         DC    AL1(STAFF),C'AFF '                                               
         DC    AL1(STAVAFF),C'AAFF'                                             
         DC    AL1(STACPP),C'ACPP'                                              
         DC    AL1(STGROSS),C'GRS '                                             
         DC    AL1(STNET),C'NET '                                               
         DC    AL1(STSPOTS),C'SPTS'                                             
         DC    AL1(STBUY),C'ORD '                                               
         DC    AL1(STPAID),C'PAID'                                              
         DC    AL1(STUNPD),C'UNPD'                                              
         DC    AL1(STBIL),C'BILL'                                               
         DC    AL1(STBILBL),C'BLBL'                                             
         DC    AL1(STPCPPN),C'NPCP'                                             
         DC    AL1(STRCPPN),C'NRCP'                                             
         DC    AL1(STACPPN),C'NACP'                                             
         DC    AL1(STCPPN),C'NCPP'                                              
         DC    AL1(STCPMN),C'NCPM'                                              
         DC    AL1(0)                                                           
         EJECT                                                                  
         USING RSTACKD,R2                                                       
OSTDEM   XC    DEM,DEM             *** STD OUTPUT ***                           
         XC    SVDEM,SVDEM                                                      
         XC    CPP,CPP                                                          
         XC    SVCPP,SVCPP                                                      
         ZAP   DOL,=P'0'                                                        
         ZAP   SVDOL,=P'0'                                                      
         MVI   STACKTYP,C'R'                                                    
         LA    R4,STACKDEF                                                      
         LA    R6,L'STACKDEF                                                    
*                                                                               
OSTDEM2  CLI   0(R4),0             TEST END OF STACK                            
         BE    OSTDEMX                                                          
         BAS   RE,STCHK            TEST STACK ENTRY SHOULD PRINT                
         BNE   OSTDEM40                                                         
         CLI   0(R4),STSPACE       SPACE                                        
         BE    OSTDEM38                                                         
         CLI   0(R4),STDIFF        DIFFERENCE                                   
         BNE   OSTDEM6                                                          
         CLI   LASTSTAK,C'D'       DOLLAR DIFFERENCE                            
         BE    OSTDOL1                                                          
         CLI   LASTSTAK,C'R'       DEMO DIFFERENCE                              
         BNE   OSTDEM4                                                          
         L     RE,DEM                                                           
         S     RE,SVDEM                                                         
         ST    RE,DUB                                                           
         B     OSTDEM14                                                         
*                                                                               
OSTDEM4  CLI   LASTSTAK,C'C'       CPP DIFFERENCE                               
         BNE   OSTDEM38                                                         
         L     RE,CPP                                                           
         S     RE,SVCPP                                                         
         ST    RE,DUB                                                           
         MVI   BYTE,C' '                                                        
         B     OSTDEM22                                                         
*                                                                               
OSTDEM6  CLI   0(R4),STNDX         INDEX                                        
         BNE   OSTDEM10                                                         
         CLI   LASTSTAK,C'D'       DOLLAR INDEX                                 
         BE    OSTDOL1                                                          
         L     RF,DEM                                                           
         L     R1,SVDEM                                                         
         CLI   LASTSTAK,C'R'       DEMO INDEX                                   
         BE    OSTDEM8                                                          
         L     RF,CPP                                                           
         L     R1,SVCPP                                                         
         CLI   LASTSTAK,C'C'       CPP INDEX                                    
         BNE   OSTDEM38                                                         
*                                                                               
OSTDEM8  LTR   R1,R1                                                            
         BZ    OSTDEM9                                                          
         M     RE,=F'2000'                                                      
         DR    RE,R1                                                            
         LTR   RF,RF                                                            
         BM    *+8                                                              
         LA    RF,1(RF)                                                         
         SRA   RF,1                                                             
         ST    RF,DUB                                                           
*                                                                               
OSTDEM9  XC    EBLOCK,EBLOCK                                                    
         MVI   EBDECS,1                                                         
         ZIC   R1,MYOLEN                                                        
         BCTR  R1,0                                                             
         STC   R1,EBLOUT                                                        
         LA    R1,DUB                                                           
         ST    R1,EBAIN                                                         
         MVI   EBLIN,4                                                          
         MVI   EBTIN,C'B'                                                       
         ST    R3,EBAOUT                                                        
         TM    DOWNOPT,DOWNON      TEST DOWNLOADING                             
         BZ    *+8                                                              
         OI    EBOPT,EBOQZEN       YES-ZERO=NOBLANK                             
         GOTO1 GLAEDITR,DMCB,EBLOCK                                             
         B     OSTDEM38                                                         
*                                                                               
OSTDEM10 LA    R1,RSGDEM           DEMOS                                        
         CLI   0(R4),STGOAL                                                     
         BE    OSTDEM12                                                         
         LA    R1,RSODEM                                                        
         CLI   0(R4),STORD                                                      
         BE    OSTDEM12                                                         
         LA    R1,RSPDEM                                                        
         CLI   0(R4),STPUR                                                      
         BE    OSTDEM12                                                         
         LA    R1,RSRDEM                                                        
         CLI   0(R4),STACH                                                      
         BE    OSTDEM12                                                         
         LA    R1,RSADEM                                                        
         CLI   0(R4),STAFF                                                      
         BNE   OSTDEM18                                                         
*                                                                               
OSTDEM12 MVC   SVDEM,DEM                                                        
         MVI   LASTSTAK,C'R'                                                    
         MVC   DUB,0(R1)                                                        
         BAS   RE,ODEMWGT                                                       
         MVC   DEM,DUB                                                          
*                                                                               
OSTDEM14 BAS   RE,ODEMFMT                                                       
         B     OSTDEM38                                                         
*                                                                               
OSTDEM18 LA    R1,RSGDEMCP         CPP                                          
         LA    R5,RSGDOLCP                                                      
         CLI   0(R4),STGCPP                                                     
         BE    OSTDEM20                                                         
         LA    R1,RSODEMCP                                                      
         LA    R5,RSODOL                                                        
         CLI   0(R4),STOCPP                                                     
         BE    OSTDEM20                                                         
         LA    R1,RSPDEMCP                                                      
         LA    R5,RSBDOL                                                        
         CLI   0(R4),STPCPP                                                     
         BE    OSTDEM20                                                         
         LA    R1,RSRDEMCP                                                      
         CLI   0(R4),STRCPP                                                     
         BE    OSTDEM20                                                         
         LA    R1,RSADEMCP                                                      
         CLI   0(R4),STACPP                                                     
         BE    OSTDEM20                                                         
*                                                                               
         LA    R1,RSPDEMCP                                                      
         LA    R5,RSNCPDOL                                                      
         CLI   0(R4),STPCPPN                                                    
         BE    OSTDEM20                                                         
         LA    R1,RSRDEMCP                                                      
         CLI   0(R4),STRCPPN                                                    
         BE    OSTDEM20                                                         
         LA    R1,RSADEMCP                                                      
         CLI   0(R4),STACPPN                                                    
         BNE   OSTDEM23                                                         
*                                                                               
OSTDEM20 MVC   DUB,0(R1)                                                        
         OC    TOTWGT,TOTWGT                                                    
         BZ    *+8                                                              
         BAS   RE,ODEMWGT          MAYBE UNWEIGHT THE DEMO VALUE                
         MVC   CPPDEM,DUB                                                       
         BAS   RE,CALCDOL          CALCULATE EXACT DOLLAR AMOUNT                
         MVC   SVCPP,CPP                                                        
         MVI   LASTSTAK,C'C'                                                    
         BAS   RE,OCPPCAL          CALCULATE CPP                                
         BZ    OSTDEM38                                                         
         MVC   DUB(4),CPP                                                       
         MVI   BYTE,0                                                           
*                                                                               
OSTDEM22 BAS   RE,OCPPFMT          FORMAT CPP                                   
         B     OSTDEM38                                                         
*                                                                               
OSTDEM23 MVI   LASTSTAK,C'D'       GROSS/NET DOLLARS                            
         LA    R5,RSGRSDOL                                                      
         CLI   0(R4),STGROSS                                                    
         BE    OSTDOL1                                                          
         LA    R5,RSNETDOL                                                      
         CLI   0(R4),STNET                                                      
         BE    OSTDOL1                                                          
         LA    R5,RSGDOL           GOAL DOLLARS                                 
         CLI   0(R4),STGDOL                                                     
         BE    OSTDOL1                                                          
         CLI   0(R4),STSPOTS       SPOTS                                        
         BNE   OSTDEM26                                                         
         MVI   LASTSTAK,C'S'                                                    
         LA    R1,RSSPOTS                                                       
         ZIC   R5,MYOLEN                                                        
         LR    RE,R5                                                            
         BCTR  RE,0                                                             
         STC   RE,MYOLEN                                                        
         BAS   RE,OSPTFMT                                                       
         STC   R5,MYOLEN                                                        
         B     OSTDEM38                                                         
*                                                                               
OSTDEM26 DS    0H                                                               
         MVI   LASTSTAK,C'R'                                                    
         ICM   R0,15,RSSPOTS                                                    
         BZ    OSTDEM38                                                         
         STCM  R0,15,FULL                                                       
         CLI   0(R4),STAVPCH                                                    
         BNE   *+12                                                             
         ICM   R0,15,RSPDEM                                                     
         B     OSTDEM28                                                         
         CLI   0(R4),STAVACH                                                    
         BNE   *+12                                                             
         ICM   R0,15,RSRDEM                                                     
         B     OSTDEM28                                                         
         CLI   0(R4),STAVAFF                                                    
         BNE   OSTDEM38                                                         
         ICM   R0,15,RSADEM                                                     
OSTDEM28 DS    0H                                                               
         SR    R1,R1                                                            
         SRDA  R0,31                                                            
         D     R0,FULL                                                          
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         ST    R1,DUB                                                           
         BAS   RE,ODEMFMT                                                       
*                                                                               
OSTDEM38 LA    R3,198(R3)          NEXT PRINT LINE                              
*                                                                               
OSTDEM40 LA    R4,2(R4)            NEXT IN STACK                                
         BCT   R6,OSTDEM2                                                       
*                                                                               
OSTDEMX  B     XIT1                                                             
         EJECT                                                                  
         USING BSTACKD,R2                                                       
OSTBDEM  MVI   STACKTYP,C'B'       *** STD(P/R/A) OUTPUT ***                    
         LA    R4,STACKDEF             (BUY DEMO STACK)                         
         LA    R6,L'STACKDEF                                                    
*                                                                               
OSTBD2   CLI   0(R4),0             TEST END OF STACK                            
         BE    OSTBDX                                                           
         BAS   RE,STCHK            TEST STACK ENTRY SHOULD PRINT                
         BNE   OSTBD14                                                          
         CLI   0(R4),STSPACE       SPACE                                        
         BE    OSTBD12                                                          
         CLI   0(R4),STIMP         IMPS                                         
         BNE   *+14                                                             
         MVC   DUB(4),BSIMP                                                     
         B     OSTBD4                                                           
         CLI   0(R4),STRTG         RTG                                          
         BNE   OSTBD6                                                           
         MVC   DUB,BSRTG                                                        
         BAS   RE,ODEMWGT                                                       
*                                                                               
OSTBD4   BAS   RE,ODEMFMT          FORMAT DEMO VALUE                            
         B     OSTBD12                                                          
*                                                                               
OSTBD6   DS    0H                                                               
         CLI   0(R4),STCPMN        NET CPM?                                     
         BNE   *+18                                                             
         MVC   DUB(4),BSIMPCP                                                   
         LA    R5,BSNCPDOL                                                      
         B     OSTBD9                                                           
*                                                                               
         CLI   0(R4),STCPM         CPM                                          
         BNE   *+14                                                             
         MVC   DUB(4),BSIMPCP                                                   
         B     OSTBD8                                                           
*                                                                               
         CLI   0(R4),STCPPN        NET CPP?                                     
         BNE   OSTBD7                                                           
         MVC   DUB,BSRTGCP                                                      
         OC    TOTWGT,TOTWGT       MAYBE UNWEIGHT                               
         BZ    *+8                                                              
         BAS   RE,ODEMWGT                                                       
         LA    R5,BSNCPDOL                                                      
         B     OSTBD9                                                           
*                                                                               
OSTBD7   CLI   0(R4),STCPP         CPP                                          
         BNE   OSTBD10                                                          
         MVC   DUB,BSRTGCP                                                      
         OC    TOTWGT,TOTWGT       MAYBE UNWEIGHT                               
         BZ    OSTBD8                                                           
         BAS   RE,ODEMWGT                                                       
*                                                                               
OSTBD8   MVC   CPPDEM,DUB                                                       
         LA    R5,BSDOL                                                         
OSTBD9   BAS   RE,CALCDOL          CALCULATE EXACT DOLLAR AMOUNT                
         BAS   RE,OCPPCAL          CALCULATE CPP/CPM                            
         BZ    OSTBD12                                                          
         MVC   DUB(4),CPP          FORMAT CPP/CPM                               
         MVI   BYTE,0                                                           
         BAS   RE,OCPPFMT                                                       
         B     OSTBD12                                                          
*                                                                               
OSTBD10  LA    R5,BSGRSDOL         GROSS DOLLARS                                
         CLI   0(R4),STGROSS                                                    
         BE    OSTDOL1                                                          
         CLI   0(R4),STSPOTS       SPOTS                                        
         BNE   OSTBD12                                                          
         LA    R1,BSSPOTS                                                       
         ZIC   R5,MYOLEN                                                        
         LR    RE,R5                                                            
         BCTR  RE,0                                                             
         STC   RE,MYOLEN                                                        
         BAS   RE,OSPTFMT                                                       
         STC   R5,MYOLEN                                                        
*                                                                               
OSTBD12  LA    R3,198(R3)          NEXT PRINT LINE                              
*                                                                               
OSTBD14  LA    R4,2(R4)            NEXT IN STACK                                
         BCT   R6,OSTBD2                                                        
*                                                                               
OSTBDX   B     XIT1                                                             
         EJECT                                                                  
         USING DSTACKD,R2                                                       
OSTDOL   ZAP   DOL,=P'0'           *** ST$ OUTPUT ***                           
         ZAP   SVDOL,=P'0'                                                      
         LA    R4,STACKDEF         LOOP THROUGH STACK                           
         LA    R6,L'STACKDEF                                                    
         MVI   STACKTYP,C'D'                                                    
*                                                                               
OSTDOL1  XC    EBLOCK,EBLOCK                                                    
         LA    R1,DUB                                                           
         ST    R1,EBAIN            INIT EDITOR BLOCK                            
         MVI   EBLIN,L'DUB                                                      
         MVI   EBTIN,C'P'                                                       
         MVC   EBLOUT,MYOLEN                                                    
         MVI   EBOPT,EBOQMEY                                                    
         TM    DOWNOPT,DOWNON      TEST DOWNLOADING                             
         BZ    OSTDOL2                                                          
         OI    EBOPT,EBOQZEN       YES-ZERO=NOBLANK                             
*                                                                               
OSTDOL2  CLI   0(R4),0             TEST END OF STACK                            
         BE    OSTDOLX                                                          
         BAS   RE,STCHK            TEST STACK ENTRY SHOULD PRINT                
         BNE   OSTDOL16                                                         
         CLI   0(R4),STSPACE       SPACE                                        
         BE    OSTDOL14                                                         
         CLI   0(R4),STDIFF        DIFFERENCE                                   
         BNE   OSTDOL4                                                          
         ZAP   DUB,DOL                                                          
         SP    DUB,SVDOL                                                        
         B     OSTDOL10                                                         
*                                                                               
OSTDOL4  CLI   0(R4),STNDX         INDEX                                        
         BNE   OSTDOL6                                                          
         CP    SVDOL,=P'0'                                                      
         BNE   OSTDOL5                                                          
         ZIC   R1,MYOLEN                                                        
         LA    R1,0(R1,R3)                                                      
         SH    R1,=H'5'                                                         
         MVC   0(4,R1),=C'HIGH'                                                 
         B     OSTDOL14                                                         
*                                                                               
OSTDOL5  ZAP   BIG,DOL                                                          
         MP    BIG,=P'2000'                                                     
         DP    BIG,SVDOL                                                        
         TM    BIG+7,X'01'                                                      
         BO    *+10                                                             
         AP    BIG(8),=P'1'                                                     
         ZAP   BIG2,BIG(8)                                                      
         DP    BIG2,=PL8'2'                                                     
         ZAP   DUB,BIG2(8)                                                      
         MVI   EBDECS,1                                                         
         B     OSTDOL12                                                         
*                                                                               
OSTDOL6  CLI   STACKTYP,C'D'       TEST ENTRY FROM ANOTHER STACK TYPE           
         BNE   OSTDOL8             YES-R5 ALREADY SET TO A(DOLLARS)             
         LA    R5,DSGDOL           GOAL DOLLARS                                 
         CLI   0(R4),STGOAL                                                     
         BE    OSTDOL8                                                          
         LA    R5,DSODOL           ORDERED DOLLARS                              
         CLI   0(R4),STORD                                                      
         BE    OSTDOL8                                                          
         LA    R5,DSBDOL           BUY (PURCHASED) DOLLARS                      
         CLI   0(R4),STPUR                                                      
         BNE   OSTDOL14                                                         
*                                                                               
OSTDOL8  BAS   RE,CALCDOL          CALCULATE EXACT AMOUNT                       
         ZAP   DUB,DOL                                                          
*                                                                               
OSTDOL10 MVI   EBDECS,0            NO DECIMAL POINT IF ROUNDING                 
         BAS   RE,ROUNDED                                                       
         BE    OSTDOL12                                                         
         MVI   EBDECS,2                                                         
*                                                                               
OSTDOL12 ST    R3,EBAOUT           FORMAT                                       
         GOTO1 GLAEDITR,DMCB,EBLOCK                                             
*                                                                               
OSTDOL14 CLI   STACKTYP,C'R'       TEST ENTRY FROM DEMO STACK ROUTINE           
         BE    OSTDEM38            YES-RETURN THERE                             
         CLI   STACKTYP,C'B'       TEST ENTRY FROM BUY DEMO STACK RTN           
         BE    OSTBD12             YES-RETURN THERE                             
         LA    R3,198(R3)          NEXT PRINT LINE                              
*                                                                               
OSTDOL16 LA    R4,2(R4)            NEXT IN STACK                                
         BCT   R6,OSTDOL2                                                       
*                                                                               
OSTDOLX  B     XIT1                                                             
         EJECT                                                                  
         USING ASTACKD,R2                                                       
OSTACC   XC    EBLOCK,EBLOCK       ** STG/STN/STT/STG-/STN-/STC O/P **          
         LA    R1,DUB                                                           
         ST    R1,EBAIN            INIT EDITOR BLOCK                            
         MVI   EBLIN,L'DUB                                                      
         MVI   EBTIN,C'P'                                                       
         MVC   EBLOUT,MYOLEN                                                    
         MVI   EBOPT,EBOQMEY                                                    
         TM    DOWNOPT,DOWNON      TEST DOWNLOADING                             
         BZ    *+8                                                              
         OI    EBOPT,EBOQZEN       YES-ZERO=NOBLANK                             
         ZAP   DOL,=P'0'                                                        
         ZAP   SVDOL,=P'0'                                                      
         LA    R4,STACKDEF         LOOP THROUGH STACK                           
         LA    R6,L'STACKDEF                                                    
*                                                                               
OSTACC2  CLI   0(R4),0             TEST END OF STACK                            
         BE    OSTACCX                                                          
         BAS   RE,STCHK            TEST STACK ENTRY SHOULD PRINT                
         BNE   OSTACC16                                                         
         CLI   0(R4),STSPACE       SPACE                                        
         BE    OSTACC14                                                         
         CLI   0(R4),STDIFF        DIFFERENCE                                   
         BNE   OSTACC4                                                          
         ZAP   DUB,DOL                                                          
         SP    DUB,SVDOL                                                        
         B     OSTACC10                                                         
*                                                                               
OSTACC4  CLI   0(R4),STNDX         INDEX                                        
         BNE   OSTACC6                                                          
         CP    SVDOL,=P'0'                                                      
         BNE   OSTACC5                                                          
         ZIC   R1,MYOLEN                                                        
         LA    R1,0(R1,R3)                                                      
         SH    R1,=H'5'                                                         
         MVC   0(4,R1),=C'HIGH'                                                 
         B     OSTACC14                                                         
*                                                                               
OSTACC5  ZAP   BIG,DOL                                                          
         MP    BIG,=P'2000'                                                     
         DP    BIG,SVDOL                                                        
         TM    BIG+7,X'01'                                                      
         BO    *+10                                                             
         AP    BIG(8),=P'1'                                                     
         ZAP   BIG2,BIG(8)                                                      
         DP    BIG2,=PL8'2'                                                     
         ZAP   DUB,BIG2(8)                                                      
         MVI   EBDECS,1                                                         
         B     OSTACC12                                                         
*                                                                               
OSTACC6  LA    R5,ASODOL           OREDERED DOLLARS                             
         CLI   0(R4),STBUY                                                      
         BE    OSTACC8                                                          
         LA    R5,ASPDOL           PAID DOLLARS                                 
         CLI   0(R4),STPAID                                                     
         BE    OSTACC8                                                          
         LA    R5,ASUDOL           UNPAID DOLLARS                               
         CLI   0(R4),STUNPD                                                     
         BE    OSTACC8                                                          
         LA    R5,ASBDOL           BILLED DOLLARS                               
         CLI   0(R4),STBIL                                                      
         BE    OSTACC8                                                          
         CLI   0(R4),STBILBL       BILLABLE DOLLARS                             
         BNE   OSTACC14                                                         
         LA    R5,ASBDOL                                                        
         BAS   RE,CALCDOL                                                       
         ZAP   DUB,DOL                                                          
         ZAP   DOL,SVDOL                                                        
         LA    R5,ASODOL                                                        
         BAS   RE,CALCDOL                                                       
         SP    DOL,DUB                                                          
         BZ    *+8                                                              
         NI    COLIND2,255-COLIBLB0                                             
         ZAP   DUB,DOL                                                          
         B     OSTACC10                                                         
*                                                                               
OSTACC8  BAS   RE,CALCDOL          CALCULATE EXACT AMOUNT                       
         ZAP   DUB,DOL                                                          
*                                                                               
OSTACC10 MVI   EBDECS,0            NO DECIMAL POINT IF ROUNDING                 
         BAS   RE,ROUNDED                                                       
         BE    OSTACC12                                                         
         MVI   EBDECS,2                                                         
*                                                                               
OSTACC12 ST    R3,EBAOUT           FORMAT                                       
         GOTO1 GLAEDITR,DMCB,EBLOCK                                             
*                                                                               
OSTACC14 LA    R3,198(R3)          NEXT PRINT LINE                              
*                                                                               
OSTACC16 LA    R4,2(R4)            NEXT IN STACK                                
         BCT   R6,OSTACC2                                                       
*                                                                               
OSTACCX  B     XIT1                                                             
         EJECT                                                                  
* MISCELLANEOUS OUTPUT ROUTINES                                                 
*                                                                               
         SPACE 1                                                                
OSPTFMT  LR    R0,RE               ** FORMAT SPOTS **                           
         XC    EBLOCK,EBLOCK                                                    
         ST    R1,EBAIN                                                         
         MVI   EBLIN,4                                                          
         MVI   EBTIN,C'B'                                                       
         ST    R3,EBAOUT                                                        
         MVC   EBLOUT,MYOLEN                                                    
         TM    0(R1),X'80'         TEST NEGATIVE SPOTS                          
         BZ    *+8                                                              
         OI    EBOPT,EBOQMEY                                                    
         GOTO1 GLAEDITR,DMCB,EBLOCK                                             
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
ODEMWGT  ST    RE,SVRE             ** GENERAL DEMO OUTPUT RTN **                
         CLI   SBQMKTWT,C'N'       TEST TO UNWEIGHT                             
         BE    ODEMWGT2                                                         
         TM    OUTIND,OUTICRMK     YES - TEST ACROSS MARKETS                    
         BZ    ODEMWGT2                                                         
         OC    DUB+4(4),DUB+4      YES - TEST DEMO IS WEIGHTED                  
         BZ    ODEMWGT2                                                         
         LA    R5,DUB              YES-UNWEIGHT                                 
         BAS   RE,UNWEIGHT                                                      
         L     RE,SVRE             RETURN CC EQ                                 
         CR    RE,RE                                                            
         BR    RE                                                               
ODEMWGT2 ICM   RE,15,SVRE          NO WEIGHTING - RETURN CC NE                  
         BR    RE                                                               
         SPACE 1                                                                
ODEMFMT  ST    RE,SVRE             ** DEMO FORMAT ROUTINE **                    
         XC    EBLOCK,EBLOCK                                                    
         MVC   EBLOUT,MYOLEN                                                    
         MVI   EBOPT,EBOQMEY                                                    
         MVI   EBROUND,1                                                        
         MVI   EBDECS,0                                                         
         TM    COLIND,COLINDR                                                   
         BZ    *+12                                                             
         MVI   EBROUND,0                                                        
         MVI   EBDECS,1                                                         
         LA    R1,DUB              DEMO VALUE IN DUB(4)                         
         ST    R1,EBAIN                                                         
         MVI   EBLIN,4                                                          
         MVI   EBTIN,C'B'                                                       
         ST    R3,EBAOUT                                                        
         GOTO1 GLAEDITR,DMCB,EBLOCK                                             
         L     RE,SVRE                                                          
         BR    RE                                                               
         SPACE 1                                                                
*                                  ** CPP CALCULATION ROUTINE **                
OCPPCAL  ST    RE,SVRE                                                          
         XC    CPP,CPP                                                          
         ICM   RF,15,CPPDEM        TEST FOR ZERO DEMO                           
         BZ    OCPPCALX                                                         
         ZAP   BIG,DOL                                                          
         TM    COLIND,COLIRND      TEST ROUND OPTION                            
         BZ    *+10                                                             
         MP    BIG,=PL2'100'       YES - ADD BACK PENNIES                       
         MP    BIG,=PL2'20'        X 10 FOR 2 DECIMAL POINT PRECISION           
         CVD   RF,DUB                                                           
         DP    BIG,DUB             DIVIDE TO GET CPP                            
         CVB   R1,BIG                                                           
         LTR   R1,R1               TEST FOR ZERO CPP                            
         BZ    OCPPCALX                                                         
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         ST    R1,CPP                                                           
OCPPCALX OC    CPP,CPP             RETURN CC EQ - CPP=ZERO                      
         L     RE,SVRE                       NE - CPP NE ZERO                   
         BR    RE                                                               
         SPACE 1                                                                
*                                  ** CPP FORMAT ROUTINE **                     
OCPPFMT  ST    RE,SVRE                                                          
         CLI   BYTE,0              TEST TRAILING CHARACTER SUPPLIED             
         BNE   OCPPFMT4            YES                                          
         MVI   BYTE,C' '                                                        
         CLI   SBSPPROF+4,C'Y'     TEST FOR EQUIV THE DETAIL LINES              
         BE    OCPPFMT2                                                         
         TM    DATAIND,DIDPTLEN    OR DOES DPT/LEN NOT FIGURE IN REPORT         
         BZ    OCPPFMT2            YES-THERE'S ALWAYS EQUIVALENCING             
         TM    GLINDS,GLTOTLIN     NO EQUIV ON DRIVER TOTAL LINE                
         BO    OCPPFMT4                                                         
         TM    OUTIND,OUTIDTOT     EQUIV IF THIS IS A DPTLEN TOTAL LINE         
         BZ    OCPPFMT4                                                         
OCPPFMT2 MVI   BYTE,C'+'           INDICATE A '+' FOR EQUIVALENCING             
OCPPFMT4 OC    DUB(4),DUB                                                       
         BZ    OCPPFMTX                                                         
         XC    EBLOCK,EBLOCK                                                    
         LA    R1,DUB                                                           
         ST    R1,EBAIN                                                         
         MVI   EBLIN,4                                                          
         MVI   EBTIN,C'B'                                                       
         ST    R3,EBAOUT                                                        
         ZIC   R5,MYOLEN                                                        
         TM    DUB,X'80'           TEST NEGATIVE                                
         BZ    *+12                                                             
         MVI   EBOPT,EBOQMEY                                                    
         B     *+6                                                              
         BCTR  R5,0                                                             
         STC   R5,EBLOUT                                                        
         TM    COLIND2,COLINOCT                                                 
         BO    *+12                                                             
         MVI   EBDECS,2            2 DECIMAL PLACES                             
         B     *+8                                                              
         MVI   EBROUND,2           UNLESS ROUNDING OUT PENNIES                  
         GOTO1 GLAEDITR,DMCB,EBLOCK                                             
         LA    R1,0(R3,R5)                                                      
         MVC   0(1,R1),BYTE        TRAILING CHARACTER                           
OCPPFMTX L     RE,SVRE                                                          
         BR    RE                                                               
         SPACE 1                                                                
*                                  ** DEMO UNWEIGHT ROUTINE **                  
UNWEIGHT LR    R0,RE                                                            
         ICM   R1,15,TOTWGT        TOTAL WEIGHT                                 
         BZ    UNWTX                                                            
         ICM   RF,15,4(R5)         DEMO VALUE                                   
         BZ    UNWT2                                                            
         SR    RE,RE                                                            
         SLDA  RE,1                X2                                           
         DR    RE,R1               DIVIDE BY TOTAL WEIGHT                       
         A     RF,=F'1'                                                         
         SRA   RF,1                ROUND                                        
*                                                                               
UNWT2    ST    RF,0(R5)                                                         
*                                                                               
UNWTX    LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
*                                  ** GET WEIGHTED VALUE **                     
WGTCOL   L     R1,0(R2)                                                         
         SR    R0,R0               DIVIDE BY 1000 TO GET WEIGHTED               
         SLDA  R0,1                VALUE (COVERAGE 10.0 = 100%)                 
         D     R0,=F'1000'                                                      
         LA    R1,1(R1)                                                         
         SRA   R1,1                                                             
         ST    R1,0(R2)                                                         
         BR    RE                                                               
         EJECT                                                                  
* CALCULATE THE DOLLARS                                                         
* INPUT  : R5=A(DOLLARS(4)/PENNIES(4))                                          
* OUTPUT : DOL=PACKED MONEY AMOUNT IN PENNIES OR DOLLARS IF ROUNDING            
*                                                                               
CALCDOL  LR    R0,RE               CALCULATE THE DOLLARS                        
         MVC   SVDOL,DOL                                                        
         L     R1,0(R5)                                                         
         CVD   R1,DOL                                                           
         L     RF,4(R5)                                                         
         TM    COLIND,COLIRND      ROUND OPTION                                 
         BO    CD2                                                              
         MP    DOL,=P'100'                                                      
         CVD   RF,PEN                                                           
         AP    DOL,PEN                                                          
         B     CDX                                                              
*                                                                               
CD2      SR    RE,RE                                                            
         SLDA  RE,1                                                             
         D     RE,=F'100'                                                       
         LTR   RF,RF                                                            
         BM    *+8                                                              
         LA    RF,1(RF)                                                         
         SRA   RF,1                                                             
         CVD   RF,MYDUB                                                         
         AP    DOL,MYDUB                                                        
*                                                                               
CDX      LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
* CHECK WHETHER DOLLAR AMOUNT IF ROUNDED                                        
* IF NOT, CHECK IF PENNIES WILL FIT IN COLUMN WIDTH - IF NOT, THEN              
* ROUND OUT THE PENNIES                                                         
* INPUT  : DUB=DOLLARS OR PENNIES                                               
* OUTPUT : CC EQ - ROUNDED                                                      
*          CC NE - NOT ROUNDED                                                  
*                                                                               
ROUNDED  LR    R0,RE                                                            
         TM    COLIND,COLIRND      TEST ALREADY ROUNDED                         
         BO    ROUNDEDY            YES                                          
         TM    COLIND2,COLINOCT    TEST NO CENTS OPTION                         
         BO    ROUNDED2            YES-ROUND OUT THE PENNIES                    
         ZAP   BIG,=P'1'           CHECK THAT PENNIES WILL FIT IN               
         ZIC   RE,MYOLEN           THE COLUMN WIDTH                             
         SH    RE,=H'2'                                                         
         BNP   *+14                                                             
         MP    BIG,=PL2'10'                                                     
         BCT   RE,*-6                                                           
         CP    DUB,BIG             COMPARE AGAINST UPPER LIMIT                  
         BL    ROUNDEDN            LOW-NO NEED TO ROUND                         
ROUNDED2 BAS   RE,ROUNDPAK         ROUND OUT THE PENNIES                        
ROUNDEDY LR    RE,R0                                                            
         CR    RE,RE                                                            
         BR    RE                                                               
ROUNDEDN LTR   RE,R0                                                            
         BR    RE                                                               
*                                                                               
ROUNDPAK ZAP   BIG,DUB             ROUND OUT PACKED PENNIES                     
         MP    BIG,=P'2'                                                        
         DP    BIG,=PL8'100'                                                    
         TM    BIG+7,X'01'                                                      
         BO    *+10                                                             
         AP    BIG(8),=P'1'                                                     
         ZAP   BIG2,BIG(8)                                                      
         DP    BIG2,=PL8'2'                                                     
         ZAP   DUB,BIG2(8)                                                      
         BR    RE                                                               
         SPACE 2                                                                
* CHECK WHETHER STACK ENTRY SHOULD PRINT OR NOT                                 
* INPUT  : R4=A(STACK DEFINITION ENTRY)                                         
* OUTPUT : CC EQ - PRINT THIS ENTRY                                             
*          CC NE - DROP THIS ENTRY                                              
*                                                                               
STCHK    LR    R0,RE                                                            
         TM    1(R4),X'80'         TEST DETAILS ONLY                            
         BZ    *+16                                                             
         TM    GLINDS,GLTOTLIN     YES-TEST TOTAL                               
         BO    STCHKNO             YES-IGNORE                                   
         B     STCHKYES                                                         
         TM    1(R4),X'40'         TEST TOTALS ONLY                             
         BZ    STCHKYES                                                         
         TM    GLINDS,GLTOTLIN     YES-TEST TOTAL                               
         BO    STCHKYES            YES-OK                                       
STCHKNO  LTR   RE,R0                                                            
         BR    RE                                                               
STCHKYES LR    RE,R0                                                            
         CR    RE,RE                                                            
         BR    RE                                                               
         EJECT                                                                  
* COMPUTE OUTPUT ROUTINE                                                        
*                                                                               
OCOMPUTE L     RE,GLADTENT         COMPUTE                                      
         USING DROD,RE                                                          
         L     R4,DROACOMP                                                      
         USING DRCMD,R4            ADDRESS COMP ELEMENT                         
         DROP  RE                                                               
         ZIC   R0,DRCMNEXP         NUMBER OF EXPRESSIONS                        
*                                                                               
OCOMP2   CLI   DRCMOP,C'V'         TEST VERTICAL PERCENT                        
         BE    *+16                                                             
         LA    R4,L'DRCMEXP(R4)                                                 
         BCT   R0,OCOMP2                                                        
         B     OCOMP10             NO  - RETURN                                 
         CLC   GLLEVEL,DRCMSUB     YES - TEST CURRENT LEVEL TOO LOW             
         BL    OCOMPX                    YES - NO EDIT (PCT>100)                
         ICM   R5,15,DRCMAIN       ADDRESS THE INPUT ENTRY                      
         BZ    OCOMP10                                                          
         USING DRIND,R5                                                         
         CLI   DRCMSUB+1,C'C'      SPECIAL HIDDEN CPP INDICATOR                 
         BE    OCOMP3                                                           
         LA    R0,6                                                             
         LA    R1,DRINROUT                                                      
         CLC   0(3,R1),=C'CPP'     IS IT CPP ?                                  
         BE    *+16                                                             
         LA    R1,1(R1)                                                         
         BCT   R0,*-14                                                          
         B     OCOMP10                                                          
         MVI   DRCMSUB+1,C'C'      YES                                          
*                                                                               
OCOMP3   CLC   GLLEVEL,DRCMSUB     CPP - TEST VERT PCT OF CURRENT LEVEL         
         BNE   OCOMP4                                                           
         MVI   DRCMTYPE,0                YES - CHANGE TYPE BACK TO              
         MVC   DRCMAIN,ACOMPIN                 INPUT DISPLACEMENT               
         B     OCOMP10                                                          
*                                                                               
OCOMP4   CLI   DRCMTYPE,1                NO  - TEST TYPE IS ALREADY LIT         
         BE    OCOMP10                         YES - THEN VERT PCT OK           
         MVI   DRCMTYPE,1          CHANGE TYPE TO LITERAL                       
         MVC   ACOMPIN,DRCMAIN     SAVE A(INPUT ENTRY)                          
         L     R1,GLATHID          GET THE FIELD IN THE TOTAL RECORD            
         USING GLINTD,R1                                                        
         ZIC   R6,DRCMSUB          (LEVEL NUMBER)                               
         SLL   R6,2                                                             
         LA    R6,GLARECL0(R6)                                                  
         DROP  R1                                                               
         L     R6,0(R6)            NOW HAVE A(RECORD AT THIS LEVEL)             
         LH    RE,DRINDISP                                                      
         AR    R6,RE               R6 = A(FIELD IN TOTAL RECORD)                
         ICM   R1,15,0(R6)         R1 = COST                                    
         XC    DRCMLIT,DRCMLIT     DEFAULT CPP = 0                              
         LTR   R1,R1                                                            
         BZ    OCOMP6                                                           
         ICM   RF,15,4(R6)         RF = DEMO                                    
         BZ    OCOMP6                                                           
         SR    R0,R0               CALCULTE CPP AT TOTAL LEVEL                  
         M     R0,=F'20'                                                        
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BZ    OCOMP6                                                           
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         STCM  R1,15,DRCMLIT       STORE CPP IN TOTAL RECORD                    
*                                                                               
OCOMP6   SR    RF,RF                                                            
         ICM   R1,15,DRCMLIT       GO BACK AND CALCULATE THIS VERT PCT          
         BZ    OCOMP8                                                           
         L     RE,GLADTENT                                                      
         L     R4,DROACOMP-DROD(RE)                                             
         CLI   DRCMTYPE,0          LOOK AT FIRST COMP EXPRESSION                
         BNE   OCOMP8                                                           
         CLI   DRCMOP,C'='                                                      
         BNE   OCOMP8                                                           
         ICM   R5,15,DRCMAIN                                                    
         BZ    OCOMP8                                                           
         L     RE,GLATHREC                                                      
         AH    RE,DRINDISP         RE = A(CPP FIELD IN CURRENT RECORD)          
         L     RF,0(RE)            RF = CPP                                     
         SR    RE,RE                                                            
         M     RE,=F'20000'                                                     
         DR    RE,R1                                                            
         LTR   RF,RF                                                            
         BM    *+8                                                              
         LA    RF,1(RF)                                                         
         SRL   RF,1                                                             
*                                                                               
OCOMP8   CVD   RF,MYDUB            MOVE THIS VERT PCT TO DRIVER FIELD           
         MVC   0(8,R2),MYDUB                                                    
*                                                                               
OCOMP10  MVI   GLHOOK,GLEDIT       LET DRIVER EDIT                              
*                                                                               
OCOMPX   B     XIT1                                                             
         EJECT                                                                  
         DS    0D                                                               
MYDUB    DS    D                                                                
DOL      DS    PL8                 DOLLARS                                      
SVDOL    DS    PL8                 SAVED DOLLARS                                
PEN      DS    PL8                 PENNIES                                      
BIG      DS    PL16                                                             
BIG2     DS    PL16                                                             
DEM      DS    XL8                 DEMO VALUE (UNWEIGHTED/WEIGHTED)             
RTG      DS    D                   RATING (REGULAR/EQIVALENCED)                 
IMP      DS    D                   IMPRESSION (REGULAR/EQUIVALENCED)            
SVDEM    DS    F                   SAVED DEMO VALUE                             
CPP      DS    F                   CPP                                          
SVCPP    DS    F                   SAVE CPP                                     
CPPDEM   DS    F                   DEMO VALUE FOR CPP                           
*                                                                               
CUMCASH  DC    PL8'0'              ACCUMULATORS FOR CHILD SPOT                  
CUMTRADE DC    PL8'0'              DOLLAR COLUMNS                               
CUMDDOL  DC    PL8'0'                                                           
*                                                                               
VAL      DS    F                                                                
VALWGT   DS    F                                                                
SVRE     DS    F                                                                
ALVLSWS  DS    A                                                                
AGENO    DS    A                                                                
*                                                                               
COMSEQ   DC    H'0'                                                             
COMSEQ1  DS    H                                                                
COMSEQ2  DS    H                                                                
COMSEQ3  DS    H                                                                
COMSEQ4  DS    H                                                                
COMSEQX  DS    H                                                                
COMSW    DS    CL5                                                              
*                                                                               
INLEVEL  DS    XL1                                                              
EQUIV    DS    CL1                                                              
LASTSTAK DS    CL1                                                              
STACKTYP DS    CL1                                                              
SVCOMKEY DS    XL13                                                             
SVBUYKEY DC    XL13'00'                                                         
*                                                                               
BLNKS    DC    CL32' '                                                          
EFFS     DC    XL10'FFFFFFFFFFFFFFFFFFFF'                                       
BIN4     DC    X'03',XL4'00000000'                                              
BIN8     DC    X'07',XL8'0000000000000000'                                      
BIN12    DC    X'0B',XL12'000000000000000000000000'                             
PAK8     DC    X'07',PL8'0'                                                     
*                                                                               
WORK2    DS    CL100                                                            
         SPACE 1                                                                
       ++INCLUDE DDEBLOCK                                                       
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
         DROP  RB                                                               
         DROP  R7                                                               
         DROP  R8                                                               
         EJECT                                                                  
* START OF DRIVE3                                                               
*                                                                               
         ENTRY DRIVE3                                                           
         DS    0D                                                               
DRIVE3   LR    RB,RE                                                            
         LA    R7,2048(RB)                                                      
         LA    R7,2048(R7)                                                      
         LA    R8,2048(R7)                                                      
         LA    R8,2048(R8)                                                      
         USING DRIVE3,RB,R7,R8                                                  
*                                                                               
         MVI   DATADISP+1,24                                                    
*                                                                               
         ST    R0,AGENO2           SAVE A(GENOUT ROUTINE)                       
         ST    R1,ALVLSWS2         SAVE A(PUT TO SORT SWITCHES)                 
*                                                                               
*         L     R1,GLADTENT                                                     
*         USING DRIND,R1                                                        
*         MVC   INLEVEL,DRINLEV     SAVE LEVEL                                  
*         DROP  R1                                                              
*                                                                               
*                                                                               
DRIVE3GO BR    RF                                                               
*                                                                               
DR3NO    DS    0H                                                               
         LTR   RB,RB               CC <> 0                                      
         B     DR3XIT                                                           
DR3YES   DS    0H                                                               
         CR    RB,RB               CC = 0                                       
DR3XIT   XIT1                                                                   
         EJECT                                                                  
IIFCPO   DS    0H                                                               
         BAS   RE,GETINFTB                                                      
         BNZ   DR3XIT                                                           
         USING INFTABD,R1                                                       
         ZIC   R6,GLARGS+2                                                      
         SLA   R6,2                * 4                                          
         LA    R6,INFCPOS(R6)                                                   
         MVC   0(L'INFCPOS,R2),0(R6)                                            
         B     DR3XIT                                                           
         DROP  R1                                                               
         SPACE                                                                  
HIFCPO   DS    0H                                                               
         BAS   RE,GETUPS                                                        
         MVC   198(10,R3),=C'TARGET CPO'                                        
         B     DR3XIT                                                           
         SPACE                                                                  
IIFTR    DS    0H                                                               
         BAS   RE,GETINFTB                                                      
         BNZ   DR3XIT                                                           
         USING INFTABD,R1                                                       
         ZIC   R6,GLARGS+2                                                      
         SLA   R6,2                * 4                                          
         LA    R6,INFRATE(R6)                                                   
         MVC   0(L'INFRATE,R2),0(R6)                                            
         B     DR3XIT                                                           
         DROP  R1                                                               
         SPACE                                                                  
HIFTR    DS    0H                                                               
         BAS   RE,GETUPS                                                        
         MVC   198(12,R3),=C'TARGET RATIO'                                      
         B     DR3XIT                                                           
         SPACE                                                                  
IIFBEC   DS    0H                                                               
         BAS   RE,GETINFTB                                                      
         BNZ   DR3XIT                                                           
         USING INFTABD,R1                                                       
         ZIC   R6,GLARGS+2                                                      
         SLA   R6,2                * 4                                          
         LA    R6,INFCOST(R6)                                                   
         MVC   0(L'INFCOST,R2),0(R6)                                            
         B     DR3XIT                                                           
         DROP  R1                                                               
         SPACE                                                                  
HIFBEC   DS    0H                                                               
         BAS   RE,GETUPS                                                        
         MVC   198(10,R3),=C'BREAK EVEN'                                        
         MVC   396(04,R3),=C'COST'                                              
         B     DR3XIT                                                           
         SPACE                                                                  
IIFPRC   DS    0H                  PRICES ON *ALL* STA REC ONLY                 
         BAS   RE,GETTBALL                                                      
         LTR   R1,R1                                                            
         BZ    DR3XIT                                                           
         USING INFTABD,R1                                                       
         ZIC   R6,GLARGS+2                                                      
         SLA   R6,2                * 4                                          
         LA    R6,INFPRCE(R6)                                                   
         MVC   0(L'INFPRCE,R2),0(R6)                                            
         B     DR3XIT                                                           
         DROP  R1                                                               
         SPACE                                                                  
HIFPRC   DS    0H                                                               
         BAS   RE,GETUPS                                                        
         MVC   198(4,R3),=C'COST'                                               
         B     DR3XIT                                                           
         SPACE                                                                  
IIFTO    DS    0H                                                               
         BAS   RE,GETINFTB                                                      
         BNZ   DR3XIT                                                           
         USING INFTABD,R1                                                       
         MVC   0(L'INFMISC,R2),INFMISC                                          
         B     DR3XIT                                                           
         DROP  R1                                                               
         SPACE                                                                  
IIFBUD   DS    0H                                                               
         BAS   RE,GETINFTB                                                      
         BNZ   DR3XIT                                                           
         USING INFTABD,R1                                                       
         MVC   0(L'INFMISC,R2),INFMISC+4                                        
         B     DR3XIT                                                           
         DROP  R1                                                               
         SPACE                                                                  
IIFSTAT  DS    0H                                                               
* P = PRE EMPT, M = MAKE GOOD, C = CONFIRMED, BLANK = NEW                       
         L     R6,SBAIO1                                                        
         MVI   ELCODE,X'0B'         POOL ORIGINAL                               
         USING REGELEM,R6                                                       
         BAS   RE,GETEL                                                         
         BE    IS20                                                             
         MVI   ELCODE,X'0C'         POOL OTO                                    
         BAS   RE,GETEL                                                         
         BE    IS20                                                             
* IS FOLLOWING ELEMENT A X'10'?                                                 
IS10     DS    0H                                                               
         MVI   0(R2),C' '         BLANK = NEW BUY                               
         ZIC   R0,1(R6)           LENGTH                                        
         AR    R6,R0              GET TO NEXT ELEMENT                           
         CLI   0(R6),X'10'        IS IT AFFADAVIT ELEMENT                       
         BNE   DR3XIT                                                           
         MVI   0(R2),C'C'         C= CONFIRMED                                  
         B     DR3XIT                                                           
* LOOK UP, FILL IN THE BUY STATUS.                                              
* IF RSTATUS = X'40' THEN P, IF RSTATUS = X'42' THEN M, IF                      
* FOLLOWING ELEMENT IS X'10', THEN C                                            
* P = PRE EMPT, M = MAKE GOOD, C = CONFIRMED                                    
*                                                                               
IS20     TM    RSTATUS,X'42'       MAKE GOOD                                    
         BNO   *+12                                                             
         MVI   0(R2),C'M'          MAKE GOOD                                    
         B     DR3XIT                                                           
         TM    RSTATUS,X'40'       SPOT HAS BEEN MINUSED                        
         BZ    IS10                                                             
         MVI   0(R2),C'P'          PRE EMPT                                     
         B     DR3XIT                                                           
         DROP  R6                                                               
         SPACE                                                                  
OIFSTAT  DS    0H                                                               
         MVC   LABLAREA(10),=C'BUY STATUS'                                      
         MVC   CODEAREA(L'INFOSTAT),0(R2)                                       
         B     GENO2                                                            
         SPACE                                                                  
IIFTYPE  DS    0H                                                               
         BAS   RE,GETINFEL                                                      
         LTR   R6,R6               A(BUY INFOMERCIAL ELEM)                      
         BZ    DR3XIT                                                           
         USING INFOELEM,R6                                                      
         MVC   0(L'INFOTYPE,R2),INFOTYPE                                        
         B     DR3XIT                                                           
         DROP  R6                                                               
         SPACE                                                                  
OIFTYPE  DS    0H                                                               
         MVC   LABLAREA(08),=C'BUY TYPE'                                        
         MVC   CODEAREA(L'INFOTYPE),0(R2)                                       
         B     GENO2                                                            
         SPACE                                                                  
IIFDATE  DS    0H                                                               
         BAS   RE,GETINFEL                                                      
         LTR   R6,R6               A(BUY INFOMERCIAL ELEM)                      
         BZ    DR3XIT                                                           
         USING INFOELEM,R6                                                      
         MVC   0(L'INFODATE,R2),INFODATE                                        
         B     DR3XIT                                                           
         DROP  R6                                                               
         SPACE                                                                  
IIFTIME  DS    0H                                                               
         BAS   RE,GETINFEL                                                      
         LTR   R6,R6               A(BUY INFOMERCIAL ELEM)                      
         BZ    DR3XIT                                                           
         USING INFOELEM,R6                                                      
         MVC   0(L'INFOTIMS,R2),INFOTIMS                                        
         B     DR3XIT                                                           
         DROP  R6                                                               
         SPACE                                                                  
OIFTIME  DS    0H                                                               
         XC    FULL,FULL                                                        
         MVC   FULL(2),0(R2)                                                    
         GOTO1 UNTIME,DMCB,FULL,WORK                                            
         MVC   0(5,R3),WORK                                                     
         B     DR3XIT                                                           
         SPACE                                                                  
IIFCTR   DS    0H                                                               
         TM    DATAIND8,DIRSPDT    ARE WE REPORTING RESPONSE DATA?              
         BZ    IFCTR10                                                          
         ICM   R6,15,SBACURCH      A(INFO RESPONSE DATA ELEM)                   
         BZ    DR3XIT                                                           
         USING INFDCNTS,R6                                                      
         CLI   GLARGS+3,X'FF'      NO RESPONSE COUNT FOR THIS FIELD             
         BE    DR3XIT               B.S. REQUEST                                
         ZIC   R1,GLARGS+3                                                      
         SLA   R1,1                * 2                                          
         LA    R1,INFDCTRS(R1)                                                  
         MVC   0(L'INFDCTRS,R2),0(R1)                                           
         B     DR3XIT                                                           
*                                                                               
IFCTR10  DS    0H                                                               
         BAS   RE,GETINFEL                                                      
         LTR   R6,R6               A(BUY INFOMERCIAL ELEM)                      
         BZ    DR3XIT                                                           
         USING INFOELEM,R6                                                      
         ZIC   R1,GLARGS+2                                                      
         SLA   R1,1                * 2                                          
         LA    R1,INFOCTRS(R1)                                                  
         MVC   0(L'INFOCTRS,R2),0(R1)                                           
         B     DR3XIT                                                           
         DROP  R6                                                               
         SPACE                                                                  
IIBYCHK  DS    0H                                                               
         BAS   RE,GETINFEL                                                      
         LTR   R6,R6               A(BUY INFOMERCIAL ELEM)                      
         BZ    DR3XIT                                                           
         USING INFOELEM,R6                                                      
         MVC   0(L'INFOCHEK,R2),INFOCHEK                                        
         B     DR3XIT                                                           
         DROP  R6                                                               
         SPACE                                                                  
OIBYCHK  MVC   LABLAREA(14),=C'BUYER CHECKING'                                  
         MVC   CODEAREA(L'INFOCHEK),0(R2)                                       
         B     GENO2                                                            
         SPACE                                                                  
HIFUPS   DS    0H                                                               
         BAS   RE,GETUPS                                                        
         MVC   198(09,R3),=C'RESPONSES'                                         
         B     DR3XIT                                                           
         EJECT                                                                  
IIFRSPDT DS    0H                                                               
         ICM   R6,15,SBACURCH      A(RESPONSE DATA ELEM)                        
         BZ    DR3XIT                                                           
         USING INFDCNTS,R6                                                      
         MVC   0(L'INFDDATE,R2),INFDDATE                                        
         B     DR3XIT                                                           
         DROP  R6                                                               
         SPACE 2                                                                
IIFRSPDW DS    0H                                                               
         TM    DATAIND8,DIRSPDT    ARE WE REPORTING RESPONSE DATA?              
         BZ    DR3XIT               NO - BS REQUEST                             
         ICM   R6,15,SBACURCH      A(INFO RESPONSE DATA ELEM)                   
         BZ    DR3XIT                                                           
         USING INFDCNTS,R6                                                      
         MVC   0(L'INFDDATE,R2),INFDDATE                                        
         GOTO1 DATCON,DMCB,(2,INFDDATE),(0,DUB)                                 
         DROP  R6                                                               
         GOTO1 GETDAY,(R1),DUB,FULL                                             
         CLI   0(R1),1             MONDAY DATE?                                 
         BE    DR3XIT               YES                                         
*                                                                               
         ZIC   R6,0(R1)                                                         
         BCTR  R6,0                                                             
         LNR   R6,R6                                                            
         GOTO1 ADDAY,(R1),DUB,WORK,(R6)                                         
         GOTO1 DATCON,(R1),(0,WORK),(2,0(R2))                                   
         B     DR3XIT                                                           
         SPACE 2                                                                
OIFRSPDW DS    0H                                                               
         MVC   LABLAREA(4),=C'WEEK'                                             
         LA    R4,NAMEAREA                                                      
         GOTO1 DATCON,DMCB,(2,0(R2)),(0,DUB)                                    
         LA    R6,6                                                             
         GOTO1 ADDAY,(R1),DUB,WORK,(R6)                                         
         GOTO1 DATCON,(R1),(0,DUB),(4,0(R4))                                    
         GOTO1 (RF),(R1),(0,WORK),(4,6(R4))                                     
         MVI   5(R4),C'-'                                                       
         B     GENO2                                                            
         SPACE 2                                                                
IIFRSPDM DS    0H                                                               
         TM    DATAIND8,DIRSPDT    ARE WE REPORTING RESPONSE DATA?              
         BZ    DR3XIT               NO - BS REQUEST                             
         ICM   R6,15,SBACURCH      A(INFO RESPONSE DATA ELEM)                   
         BZ    DR3XIT                                                           
         USING INFDCNTS,R6                                                      
* GET BROADCAST MONTH                                                           
         GOTO1 DATCON,DMCB,(2,INFDDATE),(0,DUB)                                 
         DROP  R6                                                               
         GOTO1 GETBROAD,(R1),(1,DUB),WORK,GETDAY,ADDAY                          
         LA    R6,10                                                            
         GOTO1 ADDAY,(R1),WORK,DUB,(R6)     MAKE SURE IN 'RIGHT' MONTH          
         GOTO1 DATCON,(R1),(0,DUB),(2,0(R2))                                    
         B     DR3XIT                                                           
         EJECT                                                                  
IIFCOST  DS    0H                                                               
         BAS   RE,GETINFEL                                                      
         LTR   R6,R6               A(BUY INFOMERCIAL ELEM)                      
         BZ    DR3XIT                                                           
         USING INFOELEM,R6         GET # CREDIT CARDS                           
         ZIC   R1,GLARGS+2                                                      
         SLA   R1,1                * 2                                          
         LA    R1,INFOCTRS(R1)                                                  
         DROP  R6                                                               
         SR    RF,RF                                                            
         ICM   RF,3,0(R1)                                                       
         ST    RF,0(R2)                                                         
*                                                                               
         L     R1,SBACURCH                                                      
         USING SCHUNKD,R1                                                       
         MVC   4(4,R2),SCGROSS                                                  
         DROP  R1                                                               
         B     DR3XIT                                                           
         SPACE 2                                                                
OIFCOST  DS    0H                                                               
         ICM   RF,15,0(R2)                                                      
         BZ    DR3XIT              DON'T DIVIDE BY 0!                           
         ST    RF,FULL             DIVISOR (CC OR LEADS)                        
         ICM   R4,15,4(R2)         DIVIDEND (BUY $$)                            
         SR    R5,R5                                                            
         SRDA  R4,31               2 X SCGROSS                                  
         D     R4,FULL                                                          
         LTR   R5,R5               NEGATIVE ANS?                                
         BM    *+8                                                              
         AH    R5,=H'1'            NOT AN LA INSTRUCTION                        
         SRA   R5,1                DIV BY 2                                     
         EDIT  (R5),(11,0(R3)),2                                                
         B     DR3XIT                                                           
         SPACE 2                                                                
IIFINC   DS    0H                                                               
         BAS   RE,CCINC                                                         
         BNZ   DR3XIT                                                           
         ZAP   0(8,R2),DUB                                                      
         B     DR3XIT                                                           
         SPACE 2                                                                
* CCINC: RETURNS CREDIT CARD INCOME IN DUB & CC=0 (OR CC <> 0)                  
CCINC    NTR1                                                                   
         BAS   RE,GETINFEL                                                      
         LTR   R6,R6               A(BUY INFOMERCIAL ELEM)                      
         BZ    DR3NO                                                            
         USING INFOELEM,R6         GET # CREDIT CARDS OR # LEADS                
         ZIC   R1,GLARGS+2                                                      
         SLA   R1,1                * 2                                          
         LA    R1,INFOCTRS(R1)                                                  
         SR    RF,RF                                                            
         ICM   RF,3,0(R1)                                                       
         BZ    DR3NO                                                            
         CVD   RF,DUB                                                           
         ZAP   FULL,DUB+4(4)                                                    
*                                                                               
         BAS   RE,GETTBALL                                                      
         LTR   R1,R1                                                            
         BZ    DR3NO                                                            
         USING INFTABD,R1                                                       
         ICM   RF,15,INFPRCE+20    TOTAL PRICE                                  
         CVD   RF,DUB                                                           
         DROP  R1                                                               
*                                                                               
         MP    DUB,FULL                                                         
         B     DR3YES                                                           
         DROP  R6                                                               
         SPACE 2                                                                
IIFUPINC DS    0H                                                               
         BAS   RE,UPINC                                                         
         BNZ   DR3XIT                                                           
         ZAP   0(8,R2),DUB                                                      
         B     DR3XIT                                                           
         SPACE 2                                                                
* UPINC: RETURNS UPSELL INCOME IN DUB & CC=0 (OR CC <> 0)                       
UPINC    NTR1                                                                   
         BAS   RE,GETINFEL                                                      
         LTR   R6,R6               A(BUY INFOMERCIAL ELEM)                      
         BZ    DR3NO                                                            
         USING INFOELEM,R6         GET # UPSELLS                                
         ZIC   R1,GLARGS+2                                                      
         SLA   R1,1                * 2                                          
         LA    R1,INFOCTRS(R1)                                                  
         SR    RF,RF                                                            
         ICM   RF,3,0(R1)                                                       
         BZ    DR3NO                                                            
         CVD   RF,DUB                                                           
         ZAP   FULL,DUB+4(4)                                                    
         DROP  R6                                                               
*                                                                               
         BAS   RE,GETTBALL                                                      
         LTR   R1,R1                                                            
         BZ    DR3NO                                                            
         USING INFTABD,R1          GET UPSELL RETAIL PRICE                      
         ZIC   R6,GLARGS+2                                                      
         SH    R6,=H'7'            ADJUST FOR PRICES                            
         SLA   R6,2                * 4                                          
         LA    R6,INFPRCE(R6)                                                   
         DROP  R1                                                               
         ICM   RF,15,0(R6)                                                      
         CVD   RF,DUB                                                           
*                                                                               
         MP    DUB,FULL                                                         
         B     DR3YES                                                           
         SPACE 2                                                                
HIFUPINC DS    0H                                                               
         BAS   RE,GETUPS                                                        
         MVC   198(06,R3),=C'INCOME'                                            
         B     DR3XIT                                                           
         SPACE 2                                                                
IIFCCMD  DS    0H                                                               
         BAS   RE,CCMD                                                          
         BNZ   DR3XIT                                                           
         ZAP   0(8,R2),DUB                                                      
         B     DR3XIT                                                           
         SPACE 2                                                                
*                                                                               
* CCMD: RETURNS CREDIT CARD MARGIN $$ IN DUB & CC=0 (OR CC <> 0)                
CCMD     NTR1                                                                   
         BAS   RE,GETINFEL                                                      
         LTR   R6,R6               A(BUY INFOMERCIAL ELEM)                      
         BZ    DR3NO                                                            
         USING INFOELEM,R6                                                      
         SR    RF,RF                                                            
         ICM   RF,3,INFOCTRS       # CREDIT CARDS                               
         CVD   RF,DUB                                                           
         ZAP   FULL,DUB+4(4)                                                    
         DROP  R6                                                               
*                                                                               
         BAS   RE,GETINFTB                                                      
         BNZ   DR3NO                                                            
         USING INFTABD,R1                                                       
         ICM   RF,15,INFCOST       BREAK EVEN COST                              
         DROP  R1                                                               
         CVD   RF,DUB                                                           
*                                                                               
         MP    DUB,FULL                                                         
         B     DR3YES                                                           
         SPACE 2                                                                
IIFUPMD  DS    0H                                                               
         BAS   RE,UPMD                                                          
         BNZ   DR3XIT                                                           
         ZAP   0(8,R2),DUB                                                      
         B     DR3XIT                                                           
         SPACE 2                                                                
HIFUPMD  DS    0H                                                               
         BAS   RE,GETUPS                                                        
         MVC   198(09,R3),=C'MARGIN $$'                                         
         B     DR3XIT                                                           
         SPACE 2                                                                
* UPMD: RETURNS UPSELL MARGIN $$ IN DUB & CC=0 (OR CC <> 0)                     
UPMD     NTR1                                                                   
         BAS   RE,GETINFEL                                                      
         LTR   R6,R6               A(BUY INFOMERCIAL ELEM)                      
         BZ    DR3NO                                                            
         USING INFOELEM,R6         GET NUMBER OF UPSELLS                        
         ZIC   R1,GLARGS+2                                                      
         SLA   R1,1                * 2                                          
         LA    R1,INFOCTRS(R1)                                                  
         SR    RF,RF                                                            
         ICM   RF,3,0(R1)                                                       
         BZ    DR3NO                                                            
         CVD   RF,DUB                                                           
         ZAP   FULL,DUB+4(4)                                                    
         DROP  R6                                                               
*                                                                               
         BAS   RE,GETINFTB                                                      
         BNZ   DR3NO                                                            
         USING INFTABD,R1          GET UPSELL BREAK EVEN COST                   
         ZIC   R6,GLARGS+2                                                      
         SH    R6,=H'6'            ADJUST FOR BEC                               
         SLA   R6,2                * 4                                          
         LA    R6,INFCOST(R6)                                                   
         DROP  R1                                                               
         ICM   RF,15,0(R6)                                                      
         CVD   RF,DUB                                                           
*                                                                               
         MP    DUB,FULL                                                         
         B     DR3YES                                                           
         SPACE 2                                                                
IIFGMD   DS    0H                                                               
         BAS   RE,GMD                                                           
         BNZ   DR3XIT                                                           
         ZAP   0(8,R2),DUB                                                      
         B     DR3XIT                                                           
         SPACE 2                                                                
* GMD: RETURNS GROSS MARGIN $$ IN DUB & CC=0 (OR CC <> 0)                       
GMD      NTR1                                                                   
         LA    R0,5                                                             
         MVI   GLARGS+2,7          FAKE OUT UPMD                                
         ZAP   WORK(8),=P'0'                                                    
         SR    R1,R1                                                            
*                                                                               
GMD10    BAS   RE,UPMD                                                          
         BNZ   *+10                                                             
         AP    WORK(8),DUB                                                      
         IC    R1,GLARGS+2                                                      
         LA    R1,1(R1)                                                         
         STC   R1,GLARGS+2                                                      
         BCT   R0,GMD10                                                         
*                                                                               
         BAS   RE,CCMD                                                          
         AP    DUB,WORK(8)                                                      
         B     DR3YES                                                           
         SPACE 2                                                                
IIFPRFIT DS    0H                                                               
         BAS   RE,GMD                                                           
         BNZ   DR3XIT                                                           
         ZAP   WORK(8),DUB                                                      
         L     R1,SBACURCH                                                      
         USING SCHUNKD,R1                                                       
         ICM   RF,15,SCGROSS       BYDOLS                                       
         CVD   RF,DUB                                                           
         DROP  R1                                                               
         SP    WORK(8),DUB                                                      
         ZAP   0(8,R2),WORK(8)                                                  
         B     DR3XIT                                                           
         SPACE 2                                                                
IIFTINC  DS    0H                                                               
         LA    R0,5                                                             
         MVI   GLARGS+2,7          FAKE OUT UPINC                               
         ZAP   WORK(8),=P'0'                                                    
         SR    R1,R1                                                            
*                                                                               
TINC10   BAS   RE,UPINC                                                         
         BNZ   *+10                                                             
         AP    WORK(8),DUB                                                      
         IC    R1,GLARGS+2                                                      
         LA    R1,1(R1)                                                         
         STC   R1,GLARGS+2                                                      
         BCT   R0,TINC10                                                        
*                                                                               
         MVI   GLARGS+2,0          FAKE OUT CCINC TO GET CC DATA                
         BAS   RE,CCINC                                                         
         AP    DUB,WORK(8)                                                      
         ZAP   0(8,R2),DUB                                                      
         B     DR3XIT                                                           
         EJECT                                                                  
*                                                                               
* GETINFTB: GET INFOMERCIAL TABLE ENTRY                                         
*  RETURNS: R1 = A(INFOMERCIAL ENTRY) & CC = 0 (OR CC <> 0)                     
GETINFTB NTR1                                                                   
*         CLC   GLARGS(2),=C'BI'                                                
*         BNE   DR3NO                                                           
         CLI   SBMODE,SBPROCIN                                                  
         BNE   GI10                                                             
         ICM   R1,15,SBACURCH                                                   
         BZ    DR3NO                                                            
         B     GIX                                                              
*                                                                               
GI10     CLI   SBMODE,SBPROCSP                                                  
         BNE   DR3NO                                                            
         ICM   R1,15,SBAINFTB      INFOMERCIAL TABLE                            
         BZ    DR3NO                                                            
         ICM   R0,15,4(R1)         # ENTRIES                                    
         BZ    DR3NO                                                            
         LA    R1,12(R1)           A(FIRST ENTRY)                               
         USING INFTABD,R1                                                       
*                                                                               
         CLC   INFSTA,SBBSTA       MATCH ON STATION?                            
         BNE   *+12                 NO - CHECK NEXT                             
         MVI   INFFLAG,X'FF'       MARK ENTRY AS USED                           
         B     GIX                                                              
         LA    R1,INFTABL(R1)                                                   
         BCT   R0,*-22                                                          
*                                                                               
* NO MATCH - SEE IF 'ALL' STATION ENTRY                                         
         BAS   RE,GETTBALL                                                      
         LTR   R1,R1                                                            
         BZ    DR3NO                                                            
         B     GIX                                                              
         DROP  R1                                                               
*                                                                               
GIX      DS    0H                                                               
         CR    RB,RB               CC = 0                                       
         XIT1  REGS=(R1)                                                        
         SPACE 2                                                                
* GETTBALL: GET TABLE 'ALL' ENTRY                                               
*  RETURNS R1=A(ALL ENTRY) OR R1=0                                              
GETTBALL DS    0H                                                               
         L     R1,SBAINFTB         INFOMERCIAL TABLE                            
         LA    R1,12(R1)           A(FIRST ENTRY)                               
         USING INFTABD,R1                                                       
         OC    INFSTA,INFSTA       ALL STA ENTRY?                               
         BNZ   *+10                 NO                                          
         MVI   INFFLAG,X'FF'       MARK ENTRY AS USED                           
         BR    RE                                                               
         SR    R1,R1                                                            
         BR    RE                                                               
         DROP  R1                                                               
         SPACE 2                                                                
* GETINFEL: GET A(BUY INFOMERCIAL ELEM)                                         
*  RETURNS R6=A(ELEM) OR R6 = 0                                                 
GETINFEL DS    0H                                                               
         SR    R6,R6                                                            
         CLI   SBMODE,SBPROCSP                                                  
         BNER  RE                                                               
*         CLC   GLARGS(2),=C'BF'                                                
*         BNER  RE                                                              
         LR    R1,RE               SAVE RE                                      
         L     R6,SBAIO1                                                        
         MVI   ELCODE,X'72'                                                     
         BAS   RE,GETEL                                                         
         LR    RE,R1               RESTORE RE                                   
         BER   RE                                                               
         SR    R6,R6                                                            
         BR    RE                                                               
         EJECT                                                                  
GETALL   NTR1                                                                   
         L     R6,AIO2                                                          
         USING INFORECD,R6                                                      
         CLC   INFKCLT,SBBCLT                                                   
         BNE   GA20                                                             
         CLC   INFKPRD,SBQBPRD                                                  
         BNE   GA20                                                             
         CLC   INFKEST,SBBEST                                                   
         BNE   GA20                                                             
         OC    INFKMKT,INFKMKT                                                  
         BZ    DR3YES                                                           
         B     DR3NO                                                            
         SPACE                                                                  
GA20     DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         MVC   INFKTYP,=X'0D79'                                                 
         MVC   INFKAGMD,SBBAGYMD                                                
         MVC   INFKCLT,SBBCLT                                                   
         MVC   INFKPRD,SBQBPRD                                                  
         MVC   INFKEST,SBBEST                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(07),KEYSAVE     TEST INFOREC EXISTS                          
         BNE   DR3NO                                                            
         OC    INFKMKT,INFKMKT     ALL STATION?                                 
         BNZ   DR3NO                                                            
         MVC   FULL,AIO                                                         
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         MVC   AIO,FULL                                                         
         B     DR3YES                                                           
         DROP  R6                                                               
         EJECT                                                                  
GETUPS   NTR1                                                                   
         BAS   RE,GETALL                                                        
         BE    *+14                                                             
         MVC   0(09,R3),=C'***UNK***'                                           
         B     DR3XIT                                                           
*                                                                               
         L     R6,AIO2                                                          
         MVI   ELCODE,X'01'                                                     
         USING INFNAMEL,R6                                                      
         BAS   RE,GETEL                                                         
         BE    *+14                                                             
         MVC   0(09,R3),=C'*UNKNOWN*'                                           
         B     DR3XIT                                                           
GU30     ZIC   R1,GLARGS                                                        
         BCTR  R1,0                ZERO ADJUST                                  
         MH    R1,=H'12'           INDEX TO RIGHT NAME                          
         LA    R1,INFNAMES(R1)                                                  
         MVC   0(L'INFNAMES,R3),0(R1)                                           
         DROP  R6                                                               
*                                                                               
         B     DR3XIT                                                           
         EJECT                                                                  
IESTLIN  DS    0H                                                               
         L     RE,SBAIO1           ADDRESS THE BUY RECORD                       
         USING BUYRECD,RE                                                       
         MVC   0(1,R2),BUYKEST                                                  
         MVC   1(1,R2),BUYKBUY                                                  
         B     DR3XIT                                                           
         DROP  RE                                                               
         SPACE                                                                  
OESTLIN  DS    0H                                                               
         ZIC   R1,0(R2)                                                         
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R3),DUB                                                      
         MVI   3(R3),C'-'                                                       
         IC    R1,1(R2)                                                         
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  4(3,R3),DUB                                                      
         B     DR3XIT                                                           
         SPACE                                                                  
IBYDATES DS    0H                                                               
         L     RE,SBAIO1           ADDRESS THE BUY RECORD                       
         USING BUYRECD,RE                                                       
         MVC   0(L'BDSTART+L'BDEND,R2),BDSTART                                  
         B     DR3XIT                                                           
         DROP  RE                                                               
         SPACE                                                                  
OBYDATES DS    0H                                                               
         GOTO1 DATCON,DMCB,(3,0(R2)),(4,0(R3))                                  
         MVI   5(R3),C'-'                                                       
         GOTO1 (RF),(R1),(3,3(R2)),(4,6(R3))                                    
         B     DR3XIT                                                           
         SPACE                                                                  
IBYWKS   DS    0H                                                               
         L     RE,SBAIO1           ADDRESS THE BUY RECORD                       
         USING BUYRECD,RE                                                       
         MVC   0(L'BDWKS,R2),BDWKS                                              
         B     DR3XIT                                                           
         DROP  RE                                                               
         SPACE                                                                  
IDPTCD   DS    0H                                                               
         L     RE,SBAIO1           ADDRESS THE BUY RECORD                       
         USING BUYRECD,RE                                                       
         MVC   0(L'BDDAYPT,R2),BDDAYPT                                          
         B     DR3XIT                                                           
         DROP  RE                                                               
         EJECT                                                                  
* COMMERCIAL INPUT AND OUTPUT ROUTINES                                          
*                                                                               
ICML     GOTO1 GETCML,0            COMMERCIAL                                   
         MVC   0(8,R2),SBCMLCD                                                  
         CLI   GLARGS,C'C'                                                      
         BE    DR3XIT                                                           
         MVC   8(2,R2),SBCMLSQ                                                  
         CLC   SBCMLCD(7),=C'UNKNOWN'                                           
         BNE   *+10                                                             
         XC    8(2,R2),8(R2)                                                    
         CLI   GLARGS,C'N'                                                      
         BNE   DR3XIT                                                           
         MVC   0(15,R2),SBCMLNM                                                 
         B     DR3XIT                                                           
*                                                                               
ICMLCLS  GOTO1 GETCML,0            COMMERCIAL CLASS                             
         MVC   0(4,R2),SBCMLCLS                                                 
         B     DR3XIT                                                           
*                                                                               
ICMLCLNM GOTO1 GETCML,1            COMMERCIAL CLASS NAME                        
         CLI   GLARGS+1,C'B'       TEST BOTH CODE AND NAME                      
         BNE   *+14                                                             
         MVC   0(4,R2),SBCMLCLS    YES                                          
         LA    R2,4(R2)                                                         
         MVC   0(24,R2),WORK       CLASS NAME                                   
         B     DR3XIT                                                           
*                                                                               
OCML     MVC   LABLAREA(4),=C'FILM'                                             
         CLI   GLARGS,C'N'                                                      
         BNE   *+14                                                             
         MVC   SBCMLNM,0(R2)                                                    
         B     OCML2                                                            
         MVC   CODEAREA(8),0(R2)                                                
         CLI   GLARGS,C'C'                                                      
         BE    OCMLX                                                            
         MVC   SBCMLSQ,8(R2)                                                    
         GOTO1 GETCML,0                                                         
OCML2    MVC   NAMEAREA(L'SBCMLNM),SBCMLNM                                      
         B     OCMLX                                                            
*                                                                               
OCMLCLS  MVC   LABLAREA(5),=C'CLASS'    COMMERCIAL CLASS                        
         MVC   CODEAREA(4),0(R2)                                                
         TM    OPTIND3,OPTIMTOT         TEST FOR MATCHTOT OPTION                
         BZ    OCMLX                                                            
         TM    GLINDS,GLTOTLIN          YES-TEST DETAIL LINE                    
         BZ    OCMLCLS6                 YES-ELIGIBLE FOR SUPPRESSION            
         CLI   CMCLSLEV,0               NO-TEST COMMERCIAL CLASS LEVEL          
         BNE   OCMLCLS4                    IS SET YET                           
         LA    R1,LEVELS                NO-GET IT NOW                           
         LA    RE,1                                                             
*                                                                               
OCMLCLS2 CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R1),QCMLCLS                                                    
         BE    *+16                                                             
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         B     OCMLCLS2                                                         
         STC   RE,CMCLSLEV                                                      
*                                                                               
OCMLCLS4 CLC   GLLEVEL,CMCLSLEV    TEST COMMERCIAL CLASS TOTAL                  
         BE    OCMLX               YES-ALWAYS PRINT                             
*                                                                               
OCMLCLS6 OC    CODEAREA(4),MYBLNKS                                              
         CLC   SVUDEF,CODEAREA     TEST UDEF MATCHES CLASS                      
         BNE   OCMLX                                                            
         MVI   PRTSW,C'N'          YES-SUPPRESS THE PRINT LINE                  
         B     OCMLX                                                            
*                                                                               
OCMLCLNM MVC   LABLAREA(5),=C'CLASS'    COMMERCIAL CLASS NAME                   
         CLI   GLARGS,C'B'                                                      
         BNE   *+14                                                             
         MVC   CODEAREA(4),0(R2)                                                
         LA    R2,4(R2)                                                         
         MVC   NAMEAREA(24),0(R2)                                               
         B     OCMLX                                                            
*                                                                               
OCMLX    B     GENO2                                                            
*                                                                               
ICMLNUM  DS    0H                  CLIENT COMMERCIAL NUMBER                     
         GOTO1 GETCML,0                                                         
         MVC   0(20,R2),BLOCK                                                   
         B     DR3XIT                                                           
         EJECT                                                                  
* I/O ROUTINES FOR USER DEFINITION FIELDS                                       
*                                                                               
         SPACE 1                                                                
IUDEF    LA    R3,GLARGS           LOOP THROUGH ARGS                            
         LA    R0,4                MAX 4 UDEF EXPRESSIONS                       
*                                                                               
IUDEF2   CLI   0(R3),0             TEST END OF ARGS LIST                        
         BE    IUDEF10                                                          
         CLI   1(R3),0             NO-TEST FIELD IS DEFINED FOR CLIENT          
         BE    IUDEF8                                                           
         LA    R6,UDEFTAB          YES-DETERMINE UDEF EXPRESSION                
*                                                                               
IUDEF4   CLI   0(R6),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,R3),0(R6)                                                    
         BE    *+12                                                             
         LA    R6,L'UDEFTAB(R6)                                                 
         B     IUDEF4                                                           
         SR    R4,R4               R4=A(USER FIELD)                             
         ICM   R4,3,1(R6)                                                       
         LA    R4,SBLOCK(R4)                                                    
         CLI   2(R3),C'D'          TEST DATA TYPE = DATE                        
         BNE   IUDEF6                                                           
         CLC   0(6,R4),MYBLNKS     AND FIELD IS SET                             
         BNH   IUDEF6                                                           
         GOTO1 DATVAL,DMCB,(0,(R4)),DUB   YES-GET THE DATE                      
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATCON,DMCB,DUB,(2,(R2))                                         
         LA    R2,2(R2)            SORT ON DATE                                 
*                                                                               
IUDEF6   ZIC   RE,1(R3)            LENGTH OF FIELD                              
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R2),0(R4)       MOVE INTO DRIVER INPUT FIELD                 
         LA    R2,1(RE,R2)                                                      
*                                                                               
IUDEF8   LA    R3,3(R3)            NEXT UDEF EXPRESSION                         
         BCT   R0,IUDEF2                                                        
*                                                                               
IUDEF10  B     DR3XIT                                                           
         SPACE 2                                                                
OUDEF    LR    R5,R3               R5=A(OUTPUT AREA)                            
         CLI   GLARGS+3,0          TEST MORE THAN 1 UDEF EXPRESSION             
         BNE   *+8                                                              
         LA    R5,NAMEAREA         NO-FORMAT TO NAMEAREA                        
         LA    R4,GLARGS           LOOP THROUGH ARGS                            
         LA    R0,4                MAX 4 UDEF EXPRESSIONS                       
*                                                                               
OUDEF2   CLI   0(R4),0             TEST END OF ARGS LIST                        
         BE    OUDEF8                                                           
         MVI   0(R5),0             NON-PRINTABLE CHAR                           
         CLI   1(R4),0             TEST FIELD IS DEFINED FOR CLIENT             
         BE    OUDEF6                                                           
         LA    R6,UDEFTAB          YES-DETERMINE UDEF EXPRESSION                
*                                                                               
OUDEF4   CLI   0(R6),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,R4),0(R6)                                                    
         BE    *+12                                                             
         LA    R6,L'UDEFTAB(R6)                                                 
         B     OUDEF4                                                           
         CLI   2(R4),C'D'          TEST DATA TYPE = DATE                        
         BNE   *+8                                                              
         LA    R2,2(R2)            YES-SKIP PAST COMPRESSED DATE                
         ZIC   R1,1(R4)            R1=L'DATA                                    
         LR    RE,R1                                                            
         CLC   MYOLEN,1(R4)        TEST OUTPUT LENGTH IS LESS                   
         BNL   *+8                                                              
         IC    RE,MYOLEN           YES                                          
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R5),0(R2)       MOVE DATA TO PRINT LINE                      
         XC    SVUDEF,SVUDEF                                                    
         CH    RE,=H'3'                                                         
         BNH   *+8                                                              
         LH    RE,=H'3'                                                         
         EX    RE,*+4                                                           
         MVC   SVUDEF(0),0(R2)     SAVE THE FIRST 4 BYTES OF UDEF VALUE         
         OC    SVUDEF,MYBLNKS                                                   
         LA    R2,0(R1,R2)         ADVANCE TO NEXT UDEF EXPRESSION              
         CLI   GLARGS+3,0          TEST ONLY ONE UDEF EXPRESSION                
         BNE   OUDEF6                                                           
         SR    R1,R1               YES-MOVE DESCRIPTION TO LABLAREA             
         ICM   R1,3,3(R6)                                                       
         LA    R1,SBLOCK(R1)                                                    
         MVC   LABLAREA,0(R1)                                                   
*                                                                               
OUDEF6   CLI   GLARGS+3,0          TEST ONLY ONE UDEF EXPRESSION                
         BNE   OUDEF7                                                           
         B     GENO2                                                            
*                                                                               
OUDEF7   LA    R4,3(R4)            NEXT UDEF EXPRESSION                         
         LA    R5,198(R5)          NEXT PRINT LINE                              
         BCT   R0,OUDEF2                                                        
*                                                                               
OUDEF8   B     DR3XIT                                                           
         SPACE 2                                                                
HUDEF    L     R1,GLADTENT                                                      
         ZIC   RF,DRHDWDTH-DRHDD(R1)    RF=COLUMN WIDTH                         
         LA    RE,L'SBUP1DES       SET RE FOR EXEXUTED MOVE                     
         CR    RE,RF                                                            
         BNH   *+6                                                              
         LR    RE,RF                                                            
         BCTR  RE,0                                                             
         SR    R0,R0               R0=N'UDEF EXPRESSIONS                        
         ICM   R0,1,GLARGS                                                      
         BZ    HUDEF8                                                           
         LA    R4,GLARGS+1                                                      
*                                                                               
HUDEF2   MVI   0(R3),0             INIT HEADING WITH NON-PRINTABLE CHAR         
         CLI   0(R4),0             TEST UDEF AT THIS PRINT POSITION             
         BE    HUDEF6                                                           
         LA    R6,UDEFTAB          YES-DETERMINE UDEF EXPRESSION                
*                                                                               
HUDEF4   CLI   0(R6),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,R4),0(R6)                                                    
         BE    *+12                                                             
         LA    R6,L'UDEFTAB(R6)                                                 
         B     HUDEF4                                                           
         SR    R1,R1                                                            
         ICM   R1,3,3(R6)                                                       
         LA    R1,SBLOCK(R1)       R1=A(UDEF DESCRIPTION)                       
         EX    RE,*+4              MOVE DESCRIPTION TO HEADING                  
         MVC   0(0,R3),0(R1)                                                    
*                                                                               
HUDEF6   LA    R3,198(R3)          NEXT PRINT LINE                              
         LA    R4,1(R4)            NEXT UDEF EXPRESSION                         
         BCT   R0,HUDEF2                                                        
*                                                                               
HUDEF8   B     DR3XIT                                                           
         SPACE 2                                                                
UDEFTAB  DS    0CL5                                                             
         DC    X'01',AL2(SBUP1FLD-SBLOCK),AL2(SBUP1DES-SBLOCK)                  
         DC    X'02',AL2(SBUP2FLD-SBLOCK),AL2(SBUP2DES-SBLOCK)                  
         DC    X'03',AL2(SBUE1FLD-SBLOCK),AL2(SBUE1DES-SBLOCK)                  
         DC    X'04',AL2(SBUE2FLD-SBLOCK),AL2(SBUE2DES-SBLOCK)                  
         DC    X'00'                                                            
*                                                                               
         EJECT                                                                  
IATTCODE MVC   0(1,R2),SBQMED      MEDIA                                        
         CLI   SBQMED,C'*'                                                      
         BNE   *+10                                                             
         MVC   0(1,R2),SBMED                                                    
*                                                                               
         CLI   0(R2),C'R'                                                       
         BNE   *+14                                                             
         MVC   0(2,R2),=C'SR'      SPOT RADIO                                   
         B     *+18                                                             
*                                                                               
         CLI   0(R2),C'X'                                                       
         BNE   *+18                                                             
         MVC   0(2,R2),=C'NR'      NET RADIO                                    
         LA    R2,2(R2)            A(EST NUMBER POSN)                           
         B     IATT10                                                           
*                                                                               
         CLI   0(R2),C'T'                                                       
         BE    *+12                                                             
         MVI   0(R2),C'?'          HUH?                                         
         B     *+8                                                              
         MVI   0(R2),C'S'          SPOT TV                                      
         LA    R2,1(R2)            A(EST NUMBER POSN)                           
*                                                                               
IATT10   EDIT  SBBEST,(3,0(R2)),FILL=0                                          
         B     DR3XIT                                                           
         SPACE                                                                  
OATTCODE DS    0H                                                               
         MVC   LABLAREA(05),=C'MEDIA'                                           
         MVC   CODEAREA(5),0(R2)                                                
         B     GENO2                                                            
         EJECT                                                                  
OADATE   MVC   LABLAREA(4),=C'DATE'     AFFIDAVIT DATE                          
         MVC   NAMEAREA(7),=C'UNKNOWN'                                          
         CLC   0(2,R2),=X'FFFF'                                                 
         BE    GENO2                                                            
         CLI   GLARGS,X'01'        REVERSE DATE OUTPUT?                         
         BNE   OADATE10             NO                                          
         GOTO1 DATCON,DMCB,(2,(R2)),(0,NAMEAREA)                                
         MVI   NAMEAREA+6,C' '     CLEAR 'N' FROM UNKNOWN                       
         B     GENO2                                                            
OADATE10 DS    0H                                                               
         CLI   GLARGS,X'02'        MM/DD/YY DATE OUTPUT?                        
         BNE   OADATE20             NO                                          
         GOTO1 DATCON,DMCB,(2,(R2)),(10,NAMEAREA)                               
         B     GENO2                                                            
OADATE20 DS    0H                                                               
         GOTO1 DATCON,DMCB,(2,(R2)),(8,NAMEAREA)                                
         B     GENO2                                                            
         SPACE 2                                                                
OADAY    MVC   LABLAREA(3),=C'DAY'      AFFIDAVIT DAY                           
         MVC   NAMEAREA(3),1(R2)                                                
         B     GENO2                                                            
         SPACE 2                                                                
OATIME   MVC   LABLAREA(4),=C'TIME'     AFFIDAVIT TIME                          
         CLC   0(2,R2),=X'FFFF'                                                 
         BNE   *+14                                                             
         MVC   NAMEAREA(7),=C'UNKNOWN'                                          
         B     GENO2                                                            
         XC    FULL,FULL                                                        
         MVC   FULL(2),0(R2)                                                    
         CLI   GLARGS,X'01'        OUTPUT IN MIL TIME?                          
         BNE   OATIME10             NO                                          
         EDIT  (2,FULL),(4,NAMEAREA),ZERO=NOBLANK,ALIGN=LEFT                    
         B     GENO2                                                            
*                                                                               
OATIME10 GOTO1 UNTIME,DMCB,FULL,NAMEAREA                                        
         B     GENO2                                                            
         SPACE 2                                                                
OAPROG   MVC   LABLAREA(7),=C'PROGRAM'  AFFIDAVIT PROGRAM                       
         MVC   NAMEAREA(16),0(R2)                                               
         B     GENO2                                                            
         EJECT                                                                  
IBYCDATE DS    0H                                                               
         L     R6,SBAIO1           ADDRESS BUY REC                              
         MVI   ELCODE,X'99'        GET ACTIVITY ELEM                            
         BAS   RE,GETEL                                                         
         BNE   DR3XIT                                                           
         USING ACTVELEM,R6                                                      
         MVC   0(3,R2),ACTVADD+2   CREATION DATE                                
         DROP  R6                                                               
         B     DR3XIT                                                           
         SPACE 2                                                                
IGLCDATE DS    0H                                                               
         L     R6,SBAIO1           ADDRESS GOAL REC                             
         USING GOALRECD,R6                                                      
         MVC   0(2,R2),GREDATE     CREATION DATE                                
         DROP  R6                                                               
         B     DR3XIT                                                           
         EJECT                                                                  
*                                                                               
ICOST2PS DS    0H                                                               
         L     R4,SBACURCH         ADDRESS CURRENT CHUNK FOR INPUT              
         USING SCHUNKD,R4                                                       
         ICM   RF,15,SCGROSS2                                                   
         BZ    DR3XIT                                                           
         STCM  RF,15,0(R2)                                                      
         MVC   4(4,R2),SCSPOTS                                                  
         B     DR3XIT                                                           
         DROP  R4                                                               
*                                                                               
OCOST2PS DS    0H                                                               
         TM    GLINDS,GLTOTLIN                                                  
         BZ    OC210                                                            
         ICM   RF,15,0(R2)                                                      
         B     OC220                                                            
OC210    MVC   FULL,4(R2)                                                       
         ICM   RE,15,0(R2)                                                      
         BZ    DR3XIT                                                           
         SR    RF,RF                                                            
         SRDA  RE,31               2*RE IN RERF                                 
         D     RE,FULL                                                          
         LTR   RF,RF               NEG?                                         
         BM    *+8                 DON'T NEED TO ADD 1                          
         AH    RF,=H'1'            NOT AN LA!!!                                 
         SRA   RF,1                DIVIDE RESULT BY 2                           
OC220    EDIT  (RF),(14,(R3)),2,MINUS=YES                                       
         B     DR3XIT                                                           
         EJECT                                                                  
*                                                                               
ISTAFFCH DS    0H                                                               
         MVC   0(5,R2),SBSTA       STATION                                      
         CLI   SBMED,C'N'                                                       
         BNE   *+8                                                              
         MVI   4(R2),C'N'                                                       
         MVC   5(3,R2),SBAFFIL     AFFILIATE                                    
         MVC   8(4,R2),SBCHAN      CHANNEL                                      
         B     DR3XIT                                                           
*                                                                               
OSTAFFCH DS    0H                                                               
         MVC   LABLAREA(12),=C'STA/AFFIL/CH'                                    
         MVC   NAMEAREA(4),0(R2)                                                
         CLI   4(R2),C'N'          TEST NETWORK                                 
         BE    OSTAFF20                                                         
         MVI   NAMEAREA+4,C'-'                                                  
         MVC   NAMEAREA+5(1),4(R2)                                              
         MVI   NAMEAREA+6,C'M'                                                  
         CLI   4(R2),C'T'                                                       
         BE    *+12                                                             
         CLI   4(R2),C' '                                                       
         BH    OSTAFF20                                                         
         MVC   NAMEAREA+5(2),=C'TV'                                             
OSTAFF20 DS    0H                                                               
         MVC   NAMEAREA+8(3),5(R2) AFFIL                                        
         MVC   NAMEAREA+12(4),8(R2) CHAN                                        
         B     GENO2                                                            
         EJECT                                                                  
*                                                                               
GENO2    GOTO1 AGENO2                                                           
         B     DR3XIT                                                           
         GETEL R6,DATADISP,ELCODE                                               
ALVLSWS2 DS    A                                                                
AGENO2   DS    A                                                                
CMCLSLEV DS    XL1                                                              
SVUDEF   DS    CL4                                                              
MYBLNKS  DC    CL32' '                                                          
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
         DROP  RB                                                               
         DROP  R7                                                               
         DROP  R8                                                               
         EJECT                                                                  
* EXTENTION ROUTINES                                                            
*                                                                               
EXTRA    NMOD1 0,**SPDX**,R7                                                    
         L     RC,GLAWORKD                                                      
         USING GEND,RC                                                          
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
*                                                                               
         B     GETDEM                                                           
         B     GETTGT                                                           
         B     FMTPER                                                           
         B     GETREP                                                           
         B     INBUY                                                            
         B     OUTBUY                                                           
         B     OUTMKT                                                           
         B     OUTMKTRK                                                         
         B     TOTMKTRK                                                         
         B     TOT                                                              
         B     OUTDPTLN                                                         
         B     GENO                                                             
*                                                                               
XIT2     XIT1  ,                                                                
         EJECT                                                                  
* ROUTINE TO GET DEMO NAME                                                      
*                                                                               
GETDEM   LR    R4,R1               R4=A(DEMO NAME) FOR OUTPUT                   
         ZIC   R1,GLARGS                                                        
         BCTR  R1,0                                                             
         MH    R1,=H'7'                                                         
         LA    R1,DEMNAMES(R1)                                                  
         TM    DATAIND5,DITARGET   TEST TARGET DEMO IN HEADLINES                
         BZ    GETDEM1                                                          
         LA    RE,TGTDEM           YES-EXTRACT DEMO NAME FROM                   
         CLI   GLARGS,1                TARGET OUTPUT ROUTINE'S                  
         BE    *+16                    SAVED AREA                               
         LA    RE,TGTDEM2                                                       
         CLI   GLARGS,2                                                         
         BNE   GETDEM1                                                          
         MVC   0(7,R4),0(RE)                                                    
         B     GETDEM6                                                          
*                                                                               
GETDEM1  CLI   GLARGS+3,0          TEST DEMO MODIFIER IS RATING                 
         BE    GETDEM4                                                          
         L     R1,ADEMLST          NO-GET THE NAME                              
         ZIC   R2,GLARGS                                                        
         BCTR  R2,0                                                             
         MH    R2,=H'3'                                                         
         LA    R2,0(R1,R2)                                                      
         MVC   FULL(3),0(R2)                                                    
         MVC   FULL+1(1),GLARGS+3    MODIFIER                                   
         CLI   1(R2),C'R'                                                       
         BE    GETDEM2                                                          
         CLI   1(R2),C'I'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   FULL+1,C'X'                                                      
         CLI   GLARGS+3,C'S'                                                    
         BE    GETDEM2                                                          
         MVI   FULL+1,C'Q'                                                      
         CLI   GLARGS+3,C'P'                                                    
         BE    GETDEM2                                                          
         DC    H'0'                                                             
*                                                                               
GETDEM2  LA    R2,FULL                                                          
         BAS   RE,GETDNAME         GET DEMO NAME                                
*                                                                               
GETDEM4  MVC   0(7,R4),0(R1)       SAVE DEMO NAME                               
*                                                                               
GETDEM6  OC    0(7,R4),0(R4)       TEST ANY DEMO NAME                           
         BNZ   *+10                                                             
         LTR   RE,RE                                                            
         B     GETDEMX                                                          
         CR    RE,RE                                                            
*                                                                               
GETDEMX  B     XIT2                                                             
         EJECT                                                                  
* ROUTINE TO GET TARGET DEMO NAME                                               
* INPUT  : PARM1=A(TARGET DEMO CODE)                                            
* OUTPUT : PARM2=A(DEMO NAME)                                                   
*                                                                               
GETTGT   LM    R2,R3,0(R1)                                                      
         BAS   RE,GETDNAME                                                      
         MVC   0(7,R3),0(R1)                                                    
         B     XIT2                                                             
         EJECT                                                                  
*                                                                               
* ROUTINE TO GET DEMO NAME                                                      
* INPUT  : R2=A(DEMO CODE)                                                      
* OUTPUT : R1=A(DEMO NAME)                                                      
*                                                                               
GETDNAME LR    R0,RE                                                            
         LA    RF,NMYDEMOS         MIGHT HAVE FOUND NAME ALREADY                
         LA    R5,MYDEMOS                                                       
*                                                                               
GETDN2   OC    0(3,R5),0(R5)                                                    
         BZ    GETDN4                                                           
         CLC   0(3,R5),0(R2)                                                    
         BE    GETDN6              NAME FOUND                                   
         LA    R5,10(R5)                                                        
         BCT   RF,GETDN2                                                        
         LA    R5,MYDEMOS                                                       
         XC    MYDEMOS,MYDEMOS                                                  
*                                                                               
GETDN4   MVC   0(3,R5),0(R2)       GET THE NAME                                 
*                                                                               
         LA    R6,DBAREA                                                        
         USING DBLOCKD,R6                                                       
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
         DROP  R1                                                               
         GOTO1 DEMOCON,DMCB,(1,(R2)),(2,3(R5)),(C'S',DBLOCK),0                  
*                                                                               
GETDN6   LA    R1,3(R5)            R1=A(DEMO NAME)                              
*                                                                               
GETDNX   LR    RE,R0                                                            
         BR    RE                                                               
         DROP  R6                                                               
         EJECT                                                                  
* GENERAL ROUTINE TO FORMAT A PERIOD                                            
* R1=A(OUTPUT WIDTH)                                                            
* R2=A(INPUT FIELD)                                                             
* R4=A(OUTPUT FIELD)                                                            
* BYTE=PERIOD TYPE D/W/M/Q                                                      
*                                                                               
FMTPER   OC    0(4,R2),0(R2)       TEST ANY DATES                               
         BZ    FPX                 NO                                           
         CLI   BYTE,C'Q'           QUARTERS                                     
         BNE   FP10                                                             
         MVI   0(R4),C'Q'                                                       
         LA    R4,1(R4)                                                         
         CLI   0(R1),7                                                          
         BL    *+14                                                             
         MVC   0(2,R4),=C'TR'                                                   
         LA    R4,2(R4)                                                         
         MVI   1(R4),C'/'                                                       
         GOTO1 DATCON,DMCB,(2,2(R2)),(0,DUB)                                    
         GOTO1 DATCON,DMCB,(0,DUB),(3,FULL)                                     
         MVC   2(2,R4),DUB                                                      
         ZIC   RF,FULL+1                                                        
         SR    R1,R1                                                            
         CLI   SBSPPROF+6,1                                                     
         BNH   FP1                                                              
         ZIC   RE,SBSPPROF+6                                                    
         SR    RF,RE                                                            
         BNM   *+12                                                             
         LA    RF,12(RF)                                                        
         LA    R1,1                                                             
         LA    RF,1(RF)                                                         
*                                                                               
FP1      LA    RF,1(RF)                                                         
         SR    RE,RE                                                            
         D     RE,=F'3'                                                         
         LTR   RF,RF               TEST QTR ENDS IN JAN                         
         BNZ   *+12                                                             
         LA    RF,4                YES-THEN QTR4 OF YEAR BEFORE                 
         B     *+10                                                             
         LTR   R1,R1               TEST NEED YEAR BEFORE                        
         BZ    FP2                                                              
         ZIC   RE,FULL                                                          
         BCTR  RE,0                                                             
         LTR   RE,RE                                                            
         BNM   *+8                                                              
         LA    RE,99                                                            
         CVD   RE,DUB                                                           
         UNPK  2(2,R4),DUB                                                      
         OI    3(R4),X'F0'                                                      
*                                                                               
FP2      STC   RF,0(R4)                                                         
         OI    0(R4),X'F0'                                                      
         B     FPX                                                              
*                                                                               
FP10     XC    DUB,DUB                                                          
         LA    R1,DUB                                                           
         ST    R1,DMCB+4                                                        
         MVI   DMCB+4,0                                                         
         CLI   BYTE,C'M'           MONTHS                                       
         BNE   FP20                                                             
         TM    OPTIND3,OPTICYMD    TEST CCYYMMDD                                
         BO    FP11                                                             
         CLI   DATEFORM,4          TEST BROADCAST OR CALENDAR MONTHS            
         BNL   FP12                                                             
         TM    OPTIND2,OPTIYMD     YES-                                         
         BO    FP11                                                             
         MVI   DMCB+4,6                                                         
*                                                                               
FP11     GOTO1 DATCON,DMCB,(2,2(R2))                                            
         MVC   0(6,R4),DUB                                                      
         TM    OPTIND2,OPTIYMD                                                  
         BZ    *+14                                                             
         MVC   4(2,R4),=C'00'                                                   
         B     FPX                                                              
         TM    OPTIND3,OPTICYMD    TEST CCYYMMDD OPTION                         
         BZ    FPX                                                              
         MVC   0(2,R4),=C'19'      YES - PRINT CCYYMM                           
         CLI   DUB,C'6'                                                         
         BH    *+10                                                             
         MVC   0(2,R4),=C'00'                                                   
         MVC   2(4,R4),DUB                                                      
         B     FPX                                                              
*                                                                               
FP12     TM    OPTIND2,OPTIYMD        OTHER MONTH TYPES                         
         BO    *+8                                                              
         MVI   DMCB+4,4                                                         
         GOTO1 DATCON,DMCB,(2,0(R2))                                            
         MVC   0(6,R4),DUB                                                      
         B     FPX                                                              
*                                                                               
FP20     CLI   BYTE,C'W'              WEEKS/DAYS                                
         BE    *+12                                                             
         CLI   BYTE,C'D'                                                        
         BNE   FP30                                                             
         TM    OPTIND2,OPTIYMD                                                  
         BO    *+16                                                             
         TM    OPTIND3,OPTICYMD                                                 
         BO    *+8                                                              
         MVI   DMCB+4,7                                                         
         GOTO1 DATCON,DMCB,(2,(R2))     MMMDD OR YYMMDD                         
         MVC   0(6,R4),DUB                                                      
         TM    OPTIND2,OPTIYMD                                                  
         BO    FPX                                                              
         TM    OPTIND3,OPTICYMD    TEST CCYYMMDD                                
         BZ    FP22                                                             
         MVC   0(2,R4),=C'19'      YES-                                         
         CLI   DUB,C'6'                                                         
         BH    *+10                                                             
         MVC   0(2,R4),=C'00'                                                   
         MVC   2(6,R4),DUB                                                      
         B     FPX                                                              
*                                                                               
FP22     CLI   BYTE,C'W'           TEST WEEK                                    
         BNE   FPX                                                              
         TM    SBQPER,SBQPDY       AND DAYS ALSO IN COLUMNS                     
         BZ    FPX                                                              
         MVI   5(R4),C'-'          YES-ALSO PRINT WEEK END DATE                 
         GOTO1 (RF),(R1),(2,2(R2)),(7,6(R4))                                    
         B     FPX                                                              
*                                                                               
FP30     CLI   BYTE,C'Y'           YEARS                                        
         BNE   FP34                                                             
         GOTO1 DATCON,DMCB,(2,2(R2)),(3,FULL)                                   
         MVC   0(2,R4),=C'19'                                                   
         CLI   FULL,80                                                          
         BNL   *+10                                                             
         MVC   0(2,R4),=C'20'                                                   
         ZIC   RF,FULL                                                          
         CLI   SBSPPROF+6,1                                                     
         BNH   FP32                                                             
         BCTR  RF,0                                                             
         LTR   RF,RF                                                            
         BNM   FP32                                                             
         MVC   0(2,R4),=C'19'                                                   
         LA    RF,99                                                            
*                                                                               
FP32     CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  2(2,R4),DUB                                                      
         B     FPX                                                              
*                                                                               
FP34     CLI   BYTE,C'H'           HALF YEARS                                   
         BNE   FPX                                                              
         GOTO1 DATCON,DMCB,(2,2(R2)),(3,FULL)  GET END DATE YMD                 
         ZIC   RF,FULL             RF=YEAR                                      
         ZIC   RE,FULL+1           RE=MONTH                                     
         CLI   SBSPPROF+6,1        TEST BASE MONTH = JAN                        
         BNH   FP36                                                             
         ZIC   R1,SBSPPROF+6       NO-                                          
         BCTR  R1,0                                                             
         SR    RE,R1                                                            
         BP    FP36                                                             
         LA    RE,12(RE)                                                        
         BCTR  RF,0                SUBTRACT 1 TO GET START YEAR                 
         LTR   RF,RF                                                            
         BNM   FP36                                                             
         LA    RF,99                                                            
*                                                                               
FP36     MVC   0(3,R4),=C'H1/'                                                  
         CH    RE,=H'6'            TEST H1                                      
         BNH   *+8                 YES                                          
         MVI   1(R4),C'2'          NO-THEN MUST BE H2                           
         CVD   RF,DUB              FORMAT THE YEAR                              
         OI    DUB+7,X'0F'                                                      
         UNPK  3(2,R4),DUB                                                      
*                                                                               
FPX      B     XIT2                                                             
         EJECT                                                                  
* ROUTINE TO GET A REP RECORD                                                   
* INPUT  : SBREP   = 3-CHAR REP CODE                                            
* OUTPUT : SBREPNM = REP NAME                                                   
*                                                                               
GETREP   LA    R6,KEY              READ REP RECORD                              
         USING REPRECD,R6                                                       
         MVI   REPKEY,C'0'                                                      
         MVC   REPKEY+1(16),REPKEY                                              
         MVI   REPKTYPE,C'R'                                                    
         MVC   REPKMED,SBQMED                                                   
         CLI   SBQMED,C'*'                                                      
         BNE    *+10                                                            
         MVC   REPKMED,SBMED                                                    
         MVC   REPKREP,SBREP                                                    
         MVC   REPKAGY,AGENCY                                                   
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         MVC   SBREPNM,=CL24'* UNKNOWN *'                                       
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO                      
         CLI   8(R1),0                                                          
         BNE   *+16                                                             
         MVC   SBREPNM,FILLER                                                   
         MVC   SBREPNM(L'RNAME),RNAME   EXTRACT REP NAME                        
         B     XIT2                                                             
         DROP  R6                                                               
         EJECT                                                                  
* INPUT ROUTINE FOR BUY RECORD DETAILS                                          
*                                                                               
INBUY    L     R3,SBAIO1           ADDRESS THE BUY RECORD                       
         USING BUYRECD,R3                                                       
         CLC   BUYKEY,BUYKEYSV     TEST NEW BUY KEY                             
         BE    INBUY8                                                           
         MVC   BUYKEYSV,BUYKEY                                                  
         MVC   BYEST,BUYKEST       YES-FORMAT BUY DETAILS                       
         MVC   BYLINE1,BUYKBUY                                                  
         MVI   BYLINE2,0                                                        
         MVC   BYSTART,BDSTART                                                  
         MVC   BYEND,BDEND                                                      
         MVC   BYWKS,BDWKS                                                      
         MVC   BYDPT,BDDAYPT                                                    
         MVC   BYLEN,BDSEC                                                      
         MVC   BYDAY,BDDAY                                                      
         MVC   BYTIME,BDTIMST                                                   
         MVC   BYSEDAY,BDSEDAY                                                  
         MVC   BYPROG,BDPROGRM                                                  
         MVC   BYDA,SBRECDA        RECORD ADDRESS                               
         CLI   GLARGS+1,C'C'       COST IS OPTIONAL                             
         BNE   INBUY1                                                           
         SR    R1,R1                                                            
         TM    GLARGS+2,X'01'      USE SECOND COST?                             
         BZ    INBUY0               NO                                          
*                                                                               
         LA    RE,BDELEM                                                        
         SR    R0,R0                                                            
*                                                                               
         IC    R0,1(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),0             END OF REC?                                  
         BE    *+16                 NO 2ND COST                                 
         CLI   0(RE),X'71'                                                      
         BNE   *-18                                                             
         ICM   R1,15,2(RE)                                                      
         B     *+8                                                              
*                                                                               
INBUY0   ICM   R1,7,BDCOST                                                      
         TM    BDSTAT,X'01'        TEST NETWORK                                 
         BZ    INBUY2                                                           
         TM    BDCIND2,X'01'       TEST IN PENNIES                              
         BO    INBUY2                                                           
         MH    R1,=H'100'          CONVERT DOLLARS TO PENNIES                   
         B     INBUY2                                                           
*                                                                               
INBUY1   CLI   GLARGS+1,C'N'       TEST NET COST REQUIRED                       
         BNE   INBUY3                                                           
         L     R4,SBACHUNK         YES-EXTRACT NET COST FROM CHUNK TAB          
         USING SCHUNKD,R4                                                       
         ICM   R1,15,SCNET                                                      
         BZ    INBUY2                                                           
         L     RF,SCSPOTS                                                       
         C     RF,=F'1'                                                         
         BE    INBUY2                                                           
         SR    R0,R0               DIVIDE BY N'SPOTS                            
         SLDA  R0,1                                                             
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BM    *+8                                                              
         LA    R1,1(R1)                                                         
         SRA   R1,1                                                             
*                                                                               
INBUY2   STCM  R1,15,BYCOST                                                     
*                                                                               
INBUY3   MVI   BYIND,0                                                          
         MVI   BYPKGIND,0                                                       
*                                                                               
         SR    R0,R0               LOOK FOR COMMENT AND PACKAGE                 
         LA    R1,BDELEM           ELEMENTS                                     
INBUY4   CLI   0(R1),0                                                          
         BE    INBUY7                                                           
         CLI   0(R1),X'66'                                                      
         BNE   *+12                                                             
         OI    BYIND,BYICOM                                                     
         B     INBUY6                                                           
         CLI   0(R1),5                                                          
         BNE   INBUY6                                                           
         USING PKGELEM,R1                                                       
         MVC   BYPKGIND,PKGIND     FOUND-SET PACKAGE INDICATOR                  
         CLI   PKGIND,2            TEST PACKAGE SLAVE                           
         BE    *+12                                                             
         CLI   PKGIND,6            OR REVISION SLAVE                            
         BNE   *+16                                                             
         MVC   BYLINE1,PKGLINES    YES-SET MASTER LINE NUMBER                   
         MVC   BYLINE2,BUYKBUY         AND SLAVE                                
         CLI   PKGIND,8            TEST MAKEGOOD SLAVE                          
         BNE   INBUY6                                                           
         MVC   BYLINE2,PKGLINES    YES-SET MAKEGOOD MASTER                      
         MVC   BYMGDATE,BDMGDATE           MAKEGOOD MISSED DATE                 
         MVC   BYMGSPOT,BDMGSPOT           MAKEGOOD SPOT NUMBER                 
         DROP  R1                                                               
*                                                                               
INBUY6   IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     INBUY4                                                           
*                                                                               
INBUY7   CLI   BYPKGIND,8          TEST OLD MAKEGOOD                            
         BE    INBUY8              YES                                          
         L     RF,AIO3             NO-GET TABLE OF NEW MAKEGOODS                
         USING MGABLKD,RF                                                       
         XC    0(MGALNQ,RF),0(RF)                                               
         MVI   MGAACT,MGAQBLN      BUILD A BUYLINE TABLE                        
         MVC   MGAACOM,SBCOMFAC                                                 
         LA    RE,MGALNQ(RF)                                                    
         ST    RE,MGATAB                                                        
         XC    0(MGERECL,RE),0(RE)                                              
         MVC   MGATABLN,=H'1000'                                                
         ST    R3,MGABUY                                                        
         GOTO1 =V(BLDMGA),DMCB,(RF)                                             
         L     RF,AIO3                                                          
         CLI   MGAERR,0                                                         
         BE    *+14                                                             
         CLI   MGAERR,MGAQDMIS                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,MGATAB                                                        
         OC    0(MGERECL,R1),0(R1) TEST FOR ANY MAKEGOOD ACTIVITY ON            
         BZ    INBUY8              THIS LINE                                    
         OI    BYIND,BYIMG         YES                                          
         DROP  RF                                                               
*                                                                               
INBUY8   TM    ROWIND,ROWIBYCM     TEST BUYLINE COMMENTS NEEDED                 
         BO    *+8                                                              
         NI    BYIND,255-BYICOM    NO-DON'T LOOK FOR THEM ON OUTPUT             
         CLI   GLARGS+1,C'C'                                                    
         BE    INBUY10                                                          
         CLI   GLARGS+1,C'N'                                                    
         BE    INBUY10                                                          
         MVC   0(L'BUYDET,R2),BUYDET                                            
         B     INBUYX                                                           
*                                                                               
INBUY10  MVC   0(L'BUYDETC,R2),BUYDETC                                          
*                                                                               
INBUYX   B     XIT2                                                             
         EJECT                                                                  
* OUTPUT ROUTINE FOR BUY RECORD DETAILS                                         
*                                                                               
OUTBUY   CLI   GLARGS,C'C'         TEST COST INCLUDED                           
         BE    *+14                                                             
         MVC   BUYDET,0(R2)                                                     
         B     *+10                                                             
         MVC   BUYDETC,0(R2)                                                    
         EDIT  BYEST,(3,(R3)),FILL=0    ESTIMATE                                
         MVI   3(R3),C'-'                                                       
         ZIC   R1,BYLINE1          BUYLINE NUMBER                               
         CLI   BYPKGIND,2          TEST PACKAGE SLAVE                           
         BE    *+12                                                             
         CLI   BYPKGIND,6          OR REVISION SLAVE                            
         BNE   *+8                                                              
         IC    R1,BYLINE2          YES-LINE NUMBER IS SLAVE LINE NUMBER         
         EDIT  (R1),(3,4(R3)),FILL=0                                            
         GOTO1 DATCON,DMCB,(3,BYSTART),(4,9(R3))                                
         CLC   BYSTART,BYEND                                                    
         BE    OUTBUY0                                                          
         MVI   14(R3),C'-'                                                      
         GOTO1 (RF),(R1),(3,BYEND),(4,15(R3))                                   
*                                                                               
OUTBUY0  DS    0H                                                               
         EDIT  BYWKS,(2,21(R3))                                                 
         MVC   26(1,R3),BYDPT                                                   
         EDIT  BYLEN,(3,30(R3))                                                 
         CLI   GLARGS,C'C'         OPTIONAL COST                                
         BNE   OUTBUY1                                                          
         MVI   34(R3),C'$'                                                      
         EDIT  BYCOST,(8,35(R3)),2,ALIGN=LEFT                                   
*                                                                               
OUTBUY1  LA    R4,198(R3)                                                       
         GOTO1 DAYUNPK,DMCB,(BYSEDAY,BYDAY),(0,3(R4))                           
         DS    0H                                                               
         GOTO1 UNTIME,DMCB,BYTIME,12(R4)                                        
         MVC   26(17,R4),BYPROG                                                 
         LA    R4,198(R4)                                                       
         CLI   BYPKGIND,0                                                       
         BE    OUTBUY5                                                          
         CLI   BYPKGIND,1          TEST PACKAGE MASTER                          
         BNE   *+14                                                             
         MVC   12(16,R4),=C'*PACKAGE MASTER*'                                   
         B     OUTBUY4A                                                         
         CLI   BYPKGIND,5          TEST REVISION MASTER                         
         BNE   *+14                                                             
         MVC   12(17,R4),=C'*REVISION MASTER*'                                  
         B     OUTBUY4A                                                         
         CLI   BYPKGIND,2          TEST PACKAGE SLAVE                           
         BNE   *+14                                                             
         MVC   12(4,R4),=C'PKG='                                                
         B     OUTBUY2                                                          
         CLI   BYPKGIND,6          OR REVISION SLAVE                            
         BNE   OUTBUY4                                                          
         MVC   12(4,R4),=C'REV='                                                
*                                                                               
OUTBUY2  EDIT  BYLINE1,(3,16(R4)),FILL=0   YES-EDIT THE MASTER LINE NUM         
         B     OUTBUY4A                                                         
*                                                                               
OUTBUY4  CLI   BYPKGIND,8          TEST MAKEGOOD SLAVE                          
         BNE   OUTBUY5                                                          
         MVC   12(12,R4),=C'M/G FOR LINE'   YES-                                
         EDIT  BYLINE2,(3,25(R4)),FILL=0    EDIT THE MAKEGOOD MASTER            
         OC    BYMGDATE,BYMGDATE                                                
         BZ    OUTBUY4A                                                         
         GOTO1 DATCON,DMCB,(2,BYMGDATE),(4,29(R4))   MAKEGOOD DATE              
         CLI   BYMGSPOT,1                                                       
         BNH   OUTBUY4A                                                         
         MVI   34(R4),C'-'                                                      
         EDIT  BYMGSPOT,(2,35(R4)),FILL=0            MAKEGOOD SPOT NUM          
*                                                                               
OUTBUY4A LA    R4,198(R4)                                                       
*                                                                               
OUTBUY5  TM    BYIND,BYICOM+BYIMG  TEST NEED COMMENTS OR MAKEGOOD               
         BZ    OUTBUYX                                                          
         XC    KEY,KEY             YES-READ THE BUY RECORD                      
         MVC   KEY+14(4),BYDA                                                   
         L     R5,AIO2                                                          
         ST    R5,AIO                                                           
         GOTO1 GETREC                                                           
         TM    BYIND,BYIMG         TEST MAKEGOOD ACTIVITY                       
         BZ    OUTBUY6                                                          
         L     RF,AIO3             YES-GET TABLE OF MAKEGOODS                   
         USING MGABLKD,RF                                                       
         XC    0(MGALNQ,RF),0(RF)                                               
         MVI   MGAACT,MGAQBLN      BUILD A BUYLINE TABLE                        
         MVC   MGAACOM,SBCOMFAC                                                 
         LA    RE,MGALNQ(RF)                                                    
         ST    RE,MGATAB                                                        
         MVC   MGATABLN,=H'1000'                                                
         ST    R5,MGABUY                                                        
         GOTO1 =V(BLDMGA),DMCB,(RF)                                             
         L     RF,AIO3                                                          
         CLI   MGAERR,0                                                         
         BE    *+14                                                             
         CLI   MGAERR,MGAQDMIS                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,MGATAB                                                        
         USING MGENTRYD,R1                                                      
         DROP  RF                                                               
         XC    0(2,RF),0(RF)                                                    
         SR    R0,R0                                                            
         XC    FULL,FULL                                                        
*                                                                               
OUTBUY5A OC    0(MGERECL,R1),0(R1)  LOOP THRU MAKEGOOD ENTRIES                  
         BZ    OUTBUY6                                                          
         CLC   MGECODE,=C'PR'      IGNORE MISSED NOT MADEGOOD                   
         BE    OUTBUY5D                                                         
         CLI   MGETYPE,1           TEST MAKEGOOD LINE                           
         BNE   OUTBUY5B                                                         
         LTR   R0,R0               YES-PRINT MAKEGOOD LINE ONCE                 
         BNZ   OUTBUY5D                                                         
         LA    R0,1                                                             
         MVC   0(17,R4),=C'MAKEGOOD GROUP = '                                   
         MVC   17(2,R4),MGECODE                                                 
         LA    R4,198(R4)                                                       
*                                                                               
OUTBUY5B CLI  MGETYPE,0            TEST MISSED SPOT                             
         BNE  OUTBUY5D                                                          
         LR   RE,RF                                                             
*                                                                               
OUTBUY5C OC   0(2,RE),0(RE)        YES-PRINT EACH MAKEGOOD CODE ONCE            
         BZ   *+22                                                              
         CLC  0(2,RE),MGECODE                                                   
         BE   OUTBUY5D                                                          
         LA   RE,2(RE)                                                          
         B    OUTBUY5C                                                          
         MVC  0(2,RE),MGECODE                                                   
         XC   2(2,RE),2(RE)                                                     
         ICM  RE,15,FULL                                                        
         BZ   *+18                                                              
         MVC  0(2,RE),=C', '                                                    
         LA   RE,2(RE)                                                          
         B    *+14                                                              
         MVC  0(15,R4),=C'MISSED GROUP = '                                      
         LA   RE,15(R4)                                                         
         MVC  0(2,RE),MGECODE                                                   
         LA   RE,2(RE)                                                          
         ST   RE,FULL                                                           
*                                                                               
OUTBUY5D LA    R1,MGERECL(R1)                                                   
         B     OUTBUY5A                                                         
         DROP  R1                                                               
*                                                                               
OUTBUY6  TM    BYIND,BYICOM        TEST FOR COMMENTS                            
         BZ    OUTBUYX                                                          
         LA    R5,BDELEM-BUYRECD(R5)                                            
         SR    R0,R0                                                            
*                                                                               
OUTBUY7  CLI   0(R5),0             FIND COMMENT ELEMENTS                        
         BE    OUTBUYX                                                          
         CLI   0(R5),X'66'                                                      
         BNE   OUTBUY8                                                          
         USING COMELEM,R5                                                       
         ZIC   R6,CMLEN                                                         
         SH    R6,=Y(CMDATA-COMELEM)                                            
         BNP   OUTBUY8                                                          
         GOTO1 CHOPPER,DMCB,((R6),CMDATA),(MYOLEN,(R4)),(198,4)                 
         ICM   RF,15,8(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    R4,198(R4)                                                       
         BCT   RF,*-4                                                           
*                                                                               
OUTBUY8  IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     OUTBUY7                                                          
         DROP  R5                                                               
*                                                                               
OUTBUYX  B     XIT2                                                             
         EJECT                                                                  
* MARKET OUTPUT ROUTINE                                                         
* OUTPUT : CC EQ - OK                                                           
*             NE - SUPPRES THE PRINT LINE                                       
*                                                                               
OUTMKT   CLI   GLARGS,C'N'         ** MARKET **                                 
         BNE   *+8                                                              
         LA    R2,8(R2)                                                         
         CLC   0(2,R2),XFFS        TEST DETAIL TOTAL                            
         BE    OUTMKTX                                                          
         CLC   0(2,R2),=X'FFFD'    TEST 'ALL' MARKETS                           
         BNE   *+12                                                             
         MVI   PRTSW,C'N'          YES-SUPPRESS PRINT LINE                      
         B     OUTMKT99                                                         
         MVC   SBBMKT,0(R2)        SET BINARY MARKET                            
         L     RF,GLADTENT                                                      
         USING DROD,RF                                                          
         L     RE,GLATHID                                                       
         CLC   DROLEV,GLDETLEV-GLINTD(RE)   TEST MARKET IS DETAIL LEVEL         
         BNE   OUTMKT4                                                          
         DROP  RF                                                               
         NI    OUTIND,FF-OUTICRMK          NOT ACROSS MARKETS                   
         CLC   SBBMKT,=X'FFFE'             TEST NETWORKS                        
         BNE   OUTMKT2                                                          
         MVC   SBMKT,XF0S                                                       
         MVC   SBMKTNM,FILLER                                                   
         MVC   SBMKTNM(8),=C'NETWORKS'                                          
         B     OUTMKT4                                                          
*                                                                               
OUTMKT2  EDIT  SBBMKT,(4,SBMKT),FILL=0     YES - SET MARKET DETAILS             
         GOTO1 GETMKTNM                    (WE DIDN'T GET MARKET FIRST)         
         CLI   SBQMKTWT,C'N'               TEST MARKET WEIGHTING                
         BE    OUTMKT4                                                          
         GOTO1 GETMKTWT                    YES - GET THE MKT WGT                
*                                                                               
OUTMKT4  TM    ROWIND,ROWINOMK     TEST SUPPRESS MARKET DETAILS                 
         BZ    *+12                                                             
         MVI   PRTSW,C'N'          YES-SUPPRESS PRINT LINE                      
         B     OUTMKT99                                                         
         MVC   LABLAREA(6),=C'MARKET'                                           
         LA    R1,NAMEAREA                                                      
         CLI   GLARGS,C'N'                                                      
         BE    OUTMKT6                                                          
         MVC   CODEAREA(4),SBMKT                                                
         CLI   GLARGS,C'C'                                                      
         BE    OUTMKT8                                                          
*                                                                               
OUTMKT6  MVC   NAMEAREA(L'SBMKTNM),SBMKTNM                                      
         LA    R1,L'SBMKTNM+1(R1)                                               
         CLI   MYLTYP,C'P'                                                      
         BE    OUTMKT7                                                          
         CLI   MYLTYP,C'H'         TEST HEADLINE WITH MARKET COVERAGE           
         BNE   OUTMKT8             AND SPILL OR ORIG                            
         CLI   SBQMKTWT,C'N'                                                    
         BE    OUTMKT8                                                          
         CLI   2(R2),C'O'                                                       
         BE    *+12                                                             
         CLI   2(R2),C'S'                                                       
         BNE   OUTMKT8                                                          
         LA    R1,NAMEAREA+18      YES-LEAVE ENOUGH ROOM IN HEADLINE            
         MVI   NAMEAREA+17,C' '                                                 
         B     OUTMKT8                                                          
*                                                                               
OUTMKT7  ZIC   RF,MYOLEN                                                        
         CLI   GLARGS,C'B'                                                      
         BNE   *+8                                                              
         SH    RF,=H'5'                                                         
         SH    RF,=H'10'                                                        
         BNP   OUTMKT8                                                          
         LA    R1,NAMEAREA(RF)                                                  
*                                                                               
OUTMKT8  CLI   2(R2),C'O'          TEST ORIGINATING MARKET                      
         BNE   OUTMKT10                                                         
         CLI   MYLTYP,C'P'         YES-TEST MARKET IN HEADLINES OR MID          
         BE    *+18                                                             
         MVC   0(6,R1),=C'*ORIG*'  YES                                          
         LA    R1,7(R1)                                                         
         B     OUTMKT12                                                         
         CLI   GLARGS,C'C'         MARKET IN DETAIL - TEST CODE ONLY            
         BNE   *+14                                                             
         MVC   198(4,R3),=C'*OR*'                     YES                       
         B     OUTMKT12                                                         
         LA    R1,NAMEAREA+17                                                   
         CLI   SBQMKTWT,C'N'                                                    
         BE    *+8                                                              
         LA    R1,NAMEAREA+7                                                    
         MVI   0(R1),C' '                                                       
         MVC   1(6,R1),=C'*ORIG*'                                               
         LA    R1,7(R1)                                                         
         B     OUTMKT12                                                         
*                                                                               
OUTMKT10 CLI   2(R2),C'S'          TEST SPILL MARKET                            
         BNE   OUTMKT12                                                         
         CLI   MYLTYP,C'P'                                                      
         BE    *+18                                                             
         MVC   0(7,R1),=C'*SPILL*'                                              
         LA    R1,8(R1)                                                         
         B     OUTMKT12                                                         
         CLI   GLARGS,C'C'                                                      
         BNE   *+14                                                             
         MVC   198(4,R3),=C'*SP*'                                               
         B     OUTMKT12                                                         
         LA    R1,NAMEAREA+16                                                   
         CLI   SBQMKTWT,C'N'                                                    
         BE    *+8                                                              
         LA    R1,NAMEAREA+6                                                    
         MVI   0(R1),C' '                                                       
         MVC   1(7,R1),=C'*SPILL*'                                              
         LA    R1,8(R1)                                                         
*                                                                               
OUTMKT12 CLI   GLARGS,C'C'                                                      
         BE    OUTMKTX                                                          
         CLI   SBQMKTWT,C'N'       TEST MARKET WEIGHTING                        
         BE    OUTMKTX                                                          
         ICM   R5,15,SBMKTWGT      YES-DISPLAY COVERAGE                         
         CLI   MYLTYP,C'M'                                                      
         BNE   OUTMKT14                                                         
         MVC   GLMIDXTR,FILLER                                                  
         MVC   GLMIDXTR(9),=C'COVERAGE='                                        
         LA    R6,GLMIDXTR+9                                                    
         B     OUTMKT16                                                         
*                                                                               
OUTMKT14 CLI   MYLTYP,C'H'                                                      
         BE    *+12                                                             
         MVI   0(R1),C' '                                                       
         LA    R1,1(R1)                                                         
         MVC   0(4,R1),=C'CVG='                                                 
         LA    R6,4(R1)                                                         
*                                                                               
OUTMKT16 EDIT  (R5),(6,(R6)),2,ALIGN=LEFT                                       
         AR    R6,R0                                                            
         CLI   MYLTYP,C'P'         TEST DETAIL LINE                             
         BNE   OUTMKT18                                                         
         LA    R1,NAMEAREA+L'NAMEAREA  YES-CLEAR REST TO SPACES                 
         SR    R1,R6                                                            
         BNP   OUTMKTX                                                          
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R6),FILLER                                                   
         B     OUTMKTX                                                          
*                                                                               
OUTMKT18 CLI   MYLTYP,C'H'                                                      
         BNE   OUTMKTX                                                          
         LA    R1,L'NAMEAREA                                                    
         ST    R1,DMCB+4                                                        
         GOTO1 SQUASHER,DMCB,NAMEAREA                                           
         B     OUTMKTX                                                          
*                                                                               
OUTMKT99 LTR   RB,RB                                                            
         B     XIT2                                                             
*                                                                               
OUTMKTX  CR    RB,RB                                                            
         B     XIT2                                                             
         EJECT                                                                  
* MARKET RANK OUTPUT ROUTINE                                                    
*                                                                               
OUTMKTRK SR    R0,R0                                                            
         ZIC   R1,0(R2)                                                         
         D     R0,=F'10'                                                        
         LTR   R0,R0               TEST THIS IS A CUME LINE                     
         BNZ   *+12                                                             
         STC   R1,SVMKTRNK         NO-SAVE THE MARKET RANK CODE                 
         B     OMKTRNK2                                                         
         TM    GLINDS,GLTOTLIN     YES-TEST TOTAL LINE                          
         BO    *+12                                                             
         OI    SVMKTRNK,X'80'      NO-INDICATE WE'RE CUMING NOW                 
         B     OMKTRNK9               AND SUPPRESS THE DETAIL LINE              
         TM    OUTIND,OUTIMRTO     YES-TEST IT'S MKTRNK TOTAL                   
         BZ    OMKTRNK9            NO-SUPPRESS OTHER TOTALS                     
         ZIC   RE,SVMKTRNK         YES-TEST THERE WERE DETAILS                  
         SLL   RE,25                                                            
         SRL   RE,25                                                            
         CR    R1,RE                                                            
         BNE   OMKTRNK9            NO-SUPPRESS                                  
*                                                                               
OMKTRNK2 MVC   LABLAREA(11),=C'MARKET RANK'                                     
         MVC   NAMEAREA(8),=C'MARKETS '                                         
         BCTR  R1,0                                                             
         MH    R1,=H'6'                                                         
         LTR   R0,R0                                                            
         BNZ   *+12                                                             
         LA    R1,MKTRNKTB(R1)                                                  
         B     OMKTRNK4                                                         
         CLI   0(R2),75                                                         
         BNE   *+14                                                             
         MVC   NAMEAREA(11),=C'ALL MARKETS'                                     
         B     OMKTRNKX                                                         
         LA    R1,TMKRKTAB-6(R1)                                                
*                                                                               
OMKTRNK4 MVC   NAMEAREA+8(6),0(R1)                                              
         B     OMKTRNKX                                                         
*                                                                               
OMKTRNK9 MVI   PRTSW,C'N'                                                       
         LTR   RB,RB               RETURN CC NE                                 
         B     XIT2                                                             
*                                                                               
OMKTRNKX CR    RB,RB               RETURN CC EQ                                 
         B     XIT2                                                             
*                                                                               
MKTRNKTB DC    CL6'1-10  '                                                      
         DC    CL6'11-20 '                                                      
         DC    CL6'21-30 '                                                      
         DC    CL6'31-40 '                                                      
         DC    CL6'41-50 '                                                      
         DC    CL6'51-100'                                                      
         DC    CL6'101+  '                                                      
         EJECT                                                                  
* MARKET RANK TOTAL ROUTINE                                                     
*                                                                               
TOTMKTRK CLI   GLARGS,C'H'                                                      
         BE    TMKTRNK2                                                         
         CLI   GLARGS,C'M'                                                      
         BE    TMKTRNK2                                                         
         ZIC   RF,GLARGS                                                        
         LA    RF,ROWWIDS-1(RF)                                                 
         ZIC   R1,0(RF)                                                         
         STC   R1,BYTE                                                          
         ZIC   RE,ROWWIDTH         ADJUST R3=A(ROW TO RIGHT OF                  
         CLI   GLARGS,1                        TOTALING ROW)                    
         BNH   *+10                                                             
         BCTR  RF,0                                                             
         IC    RE,0(RF)                                                         
         SR    RE,R1                                                            
         LA    R3,0(RE,R3)                                                      
         B     TMKTRNK4                                                         
*                                                                               
TMKTRNK2 MVC   BYTE,ROW1WIDE                                                    
         CLI   BYTE,7                                                           
         BNL   TMKTRNK4                                                         
         MVC   BYTE,ROWWIDTH                                                    
*                                                                               
TMKTRNK4 CLI   BYTE,6                                                           
         BL    TMKTRNK9                                                         
         MVC   BLOCK(80),FILLER                                                 
         CLI   SVMKTRNK,X'87'      TEST ALL MARKETS                             
         BNE   TMKTRNK6                                                         
         MVC   BLOCK(3),=C'ALL'                                                 
         MVC   BLOCK+4(4),=C'MKTS'                                              
         CLI   BYTE,7                                                           
         BL    *+10                                                             
         MVC   BLOCK+4(7),=C'MARKETS'                                           
         MVC   BLOCK+12(5),=C'TOTAL'                                            
         B     TMKTRNK8                                                         
*                                                                               
TMKTRNK6 MVC   BLOCK(4),=C'MKTS'                                                
         CLI   BYTE,7                                                           
         BL    *+10                                                             
         MVC   BLOCK(7),=C'MARKETS'                                             
         ZIC   R1,SVMKTRNK                                                      
         SLL   R1,25                                                            
         SRL   R1,25                                                            
         BCTR  R1,0                                                             
         MH    R1,=H'6'                                                         
         TM    SVMKTRNK,X'80'                                                   
         BZ    *+12                                                             
         LA    R1,TMKRKTAB-6(R1)                                                
         B     TMKTRNK7                                                         
         CLI   GLARGS,C'H'                                                      
         BE    *+12                                                             
         CLI   GLARGS,C'M'                                                      
         BNE   TMKTRNK9                                                         
         LA    R1,MKTRNKTB(R1)                                                  
*                                                                               
TMKTRNK7 MVC   BLOCK+8(6),0(R1)                                                 
         MVC   BLOCK+15(5),=C'TOTAL'                                            
*                                                                               
TMKTRNK8 GOTO1 SQUASHER,DMCB,BLOCK,80                                           
         LA    R4,4                                                             
         GOTO1 CHOPPER,DMCB,(80,BLOCK),(BYTE,0(R3)),(198,(R4))                  
         B     XIT2                                                             
*                                                                               
TMKTRNK9 MVI   GLHOOK,GLEDIT       LET DRIVER EDIT                              
         B     XIT2                                                             
*                                                                               
TMKRKTAB DC    CL6'1-20'                                                        
         DC    CL6'1-30'                                                        
         DC    CL6'1-40'                                                        
         DC    CL6'1-50'                                                        
         DC    CL6'1-100'                                                       
         EJECT                                                                  
* GENERAL TOTAL ROUTINE FOR DETAIL ROWS                                         
*                                                                               
TOT      ZIC   RF,GLARGS                                                        
         LA    RF,ROWWIDS-1(RF)    RF=L'SPACE FOR TOTAL                         
         CLI   0(RF),5             AT LEAST 5                                   
         BL    TOTAL9                                                           
         OC    COUNT,COUNT         TEST FOR COUNT                               
         BZ    TOTAL2                                                           
         MVI   PARAS,C'('                                                       
         EDIT  (2,COUNT),(8,PARAS+1),ALIGN=LEFT                                 
         LR    R1,R0                                                            
         LA    RE,PARAS+1(R1)                                                   
         MVI   0(RE),C')'                                                       
         LA    R1,2(R1)                                                         
         STC   R1,BYTE                                                          
         CLC   BYTE,0(RF)          BYTE=L'COUNT                                 
         BNH   TOTAL2                                                           
         XC    COUNT,COUNT         IF NO ROOM, DON'T PRINT                      
*                                                                               
TOTAL2   SR    R4,R4               TEST MARKET WEIGHTING                        
         BAS   RE,TESTWGT                                                       
         BNE   *+12                                                             
         CLI   0(RF),7             YES-AT LEAST SPACE FOR 7 CHARS               
         BNL   *+14                                                             
         OC    COUNT,COUNT                                                      
         BZ    TOTAL9                                                           
         ZIC   R6,0(RF)            R6=L'AVAILABLE SPACE                         
         ZIC   RE,ROWWIDTH         ADJUST R3=A(ROW TO RIGHT OF                  
         CLI   GLARGS,1                        TOTALING ROW)                    
         BNH   *+10                                                             
         BCTR  RF,0                                                             
         IC    RE,0(RF)                                                         
         SR    RE,R6                                                            
         LA    R3,0(RE,R3)                                                      
         MVC   BLOCK(80),FILLER                                                 
         MVC   BLOCK(5),=C'*ALL*'                                               
         OC    COUNT,COUNT         FORMAT COUNT                                 
         BZ    TOTAL4                                                           
         ZIC   RE,BYTE                                                          
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   BLOCK+26(0),PARAS                                                
*                                                                               
TOTAL4   LTR   R4,R4               FORMAT WEIGHT                                
         BZ    TOTAL6                                                           
         GOTO1 FMTWGT,BLOCK+6                                                   
         BE    TOTAL6                                                           
         OC    COUNT,COUNT                                                      
         BZ    TOTAL9                                                           
*                                                                               
TOTAL6   GOTO1 SQUASHER,DMCB,BLOCK,80                                           
         LA    R4,4                                                             
         GOTO1 CHOPPER,DMCB,(80,BLOCK),((R6),0(R3)),(198,(R4))                  
         B     XIT2                                                             
*                                                                               
TOTAL9   MVI   GLHOOK,GLEDIT       LET DRIVER DO IT                             
         B     XIT2                                                             
         EJECT                                                                  
* DAYPART/LENGTH OUTPUT ROUTINE                                                 
*                                                                               
OUTDPTLN CLI   1(R2),C''          SUPPRESS $ DAYPARTS                          
         BE    DL99                                                             
         CLI   5(R2),C''                                                       
         BE    DL99                                                             
         OI    OUTIND,OUTICRDP+OUTIDTOT                                         
         MVC   0(10,R3),FILLER                                                  
         CLC   0(9,R2),XFFS                                                     
         BNE   DL10                                                             
         CP    DLCNTR,=P'1'        TEST FOR MORE THAN ONE DETAIL                
         BNH   DL99                                                             
         TM    SBQDPTLN,SBQDLTOT   TEST FOR SPOT LENGTH TOTALS                  
         BZ    *+12                                                             
         CLI   SLCNTRS+2,0         YES - TEST FOR MORE THAN 1 SPTLEN            
         BE    DL99                      NO - DON'T PRINT TOTAL                 
         MVC   1(9,R3),=C'* TOTAL *'                                            
         B     DLX                                                              
*                                                                               
DL10     CLC   0(8,R2),XFFS        SPOT LENGTH TOTAL                            
         BNE   DL40                                                             
         CLI   8(R2),1             TEST LEN=1                                   
         BE    DL99                YES-DROP IT                                  
         LA    R0,L'SLCNTRS/2                                                   
         LA    R1,SLCNTRS                                                       
*                                                                               
DL20     CLC   0(1,R1),8(R2)                                                    
         BE    DL30                                                             
         LA    R1,2(R1)                                                         
         BCT   R0,DL20                                                          
         DC    H'0'                                                             
*                                                                               
DL30     CP    1(1,R1),=P'1'                                                    
         BNH   DL99                                                             
         MVC   1(6,R3),=C'TOTAL-'                                               
         B     DL97                                                             
*                                                                               
DL40     NI    OUTIND,FF-OUTICRDP  NOT CROSS DPT TOTAL                          
         CLC   4(5,R2),XFFS        DAYPART GROUP TOTAL                          
         BNE   DL50                                                             
         CP    DPTCNTR,=P'1'       TEST FOR MORE THAN ONE DPT IN GRP            
         BNH   DL99                                                             
         MVC   0(3,R3),1(R2)                                                    
         MVI   3(R3),C'-'                                                       
         MVC   4(6,R3),=C'TOTAL*'                                               
         B     DLX                                                              
*                                                                               
DL50     CLI   8(R2),X'FF'         TEST FOR DAYPART TOTAL                       
         BE    DL95                                                             
         NI    OUTIND,FF-OUTIDTOT  NOT DAYPART TOTAL                            
         CP    DLCNTR,=P'2'        AUGMENT DPTLEN DETAIL COUNTER                
         BNL   *+10                                                             
         AP    DLCNTR,=P'1'                                                     
         LA    R0,L'SLCNTRS/2      AUGMENT APPROPRIATE SPOT LENGTH CNTR         
         LA    R1,SLCNTRS                                                       
*                                                                               
DL60     CLI   0(R1),0                                                          
         BE    DL70                                                             
         CLC   0(1,R1),8(R2)                                                    
         BE    DL80                                                             
         LA    R1,2(R1)                                                         
         BCT   R0,DL60                                                          
         DC    H'0'                                                             
*                                                                               
DL70     MVC   0(1,R1),8(R2)                                                    
         ZAP   1(1,R1),=P'0'                                                    
*                                                                               
DL80     CP    1(1,R1),=P'2'                                                    
         BNL   *+10                                                             
         AP    1(1,R1),=P'1'                                                    
         MVI   7(R3),C'-'                                                       
         CP    LENCNTR,=P'1'       TEST LENGTH COUNTER = 1                      
         BNE   *+14                                                             
         CLC   LENSV,8(R2)         AND LENGTH SAME AS LAST                      
         BE    DL82                YES-AVOID INCREMENTING LEN COUNTER           
         MVC   LENSV,8(R2)                                                      
         CP    LENCNTR,=P'2'       INCREMENT LENGTH COUNTER                     
         BNL   DL82                                                             
         AP    LENCNTR,=P'1'                                                    
*                                                                               
DL82     CP    LENCNTR,=P'1'       IS IT GREATER THAN 1                         
         BH    DL97                                                             
         OC    1(3,R2),1(R2)       NO - FORMAT THE DAYPART                      
         BZ    DL90                                                             
         CLC   DPTGRPSV,1(R2)      TEST FOR NEW DAYPART GROUP                   
         BE    DL90                                                             
         ZAP   DPTCNTR,=P'0'       RESET DPT COUNTER                            
         MVC   DPTGRPSV,1(R2)                                                   
         MVC   0(3,R3),1(R2)                                                    
         MVI   3(R3),C'-'                                                       
*                                                                               
DL90     MVC   4(3,R3),5(R2)                                                    
         CP    DPTCNTR,=P'2'       AUGMENT DAYPART COUNTER                      
         BNL   *+10                                                             
         AP    DPTCNTR,=P'1'                                                    
         B     DL97                                                             
*                                                                               
DL95     CP    LENCNTR,=P'1'       DAYPART TOTAL                                
         BNH   *+14                - ONLY PRINT IF MORE THAN 1 LENGTH           
         MVC   7(3,R3),=C'TOT'                                                  
         B     *+8                                                              
         MVI   PRTSW,C'N'                                                       
         ZAP   LENCNTR,=P'0'       INIT LENGTH COUNTER                          
         MVI   LENSV,0                                                          
         B     DLX                                                              
*                                                                               
DL97     CLI   8(R2),100              EDIT THE SPOT LENGTH                      
         BNL   DL98                                                             
         EDIT  (1,8(R2)),(2,8(R3))                                              
         B     DLX                                                              
DL98     EDIT  (1,8(R2)),(3,7(R3))                                              
         B     DLX                                                              
*                                                                               
DL99     MVI   PRTSW,C'N'          DON'T PRINT                                  
*                                                                               
DLX      B     XIT2                                                             
         EJECT                                                                  
* SHARED OUTPUT ROUTINE                                                         
* LABLAREA HAS PREFIX                                                           
* CODEAREA HAS CODE                                                             
* NAMEAREA HAS NAME                                                             
*                                                                               
GENO     TM    GLINDS,X'40'        TEST TOTALS ROUTINE                          
         BO    TOTOUT              YES                                          
         CLI   MYLTYP,C'H'         FOR HEADLINES, MOVE OUT THE LOT              
         BNE   GENOUT2                                                          
         TM    DOWNOPT,GLDLACTV+GLDLHEAD   EXCEPT IF DOWNLOADING HEADS          
         BO    GENOUT2                                                          
         MVC   0(L'OUTAREA,R3),OUTAREA                                          
         B     GENOUTX                                                          
*                                                                               
GENOUT2  OC    CODENNAM,FILLER                                                  
         GOTO1 SQUASHER,DMCB,CODENNAM,49                                        
         TM    OPTIND2,OPTITRUN    TEST TRUNCATE OPTION                         
         BZ    GENOUT4                                                          
         ZIC   RE,MYOLEN           YES-MOVE FOR LENGTH OF ROW                   
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     GENOUTX                                                          
         MVC   0(0,R3),CODENNAM                                                 
*                                                                               
GENOUT4  LA    R1,CODENNAM         SET UP FOR CHOPPER                           
         ST    R1,DMCB             A(INPUT)                                     
         MVI   DMCB,49             L'INPUT                                      
         ST    R3,DMCB+4           A(OUTPUT)                                    
         MVC   DMCB+4(1),MYOLEN    LENGTH OF OUTPUT                             
         LA    R1,4                MAX N'LINES                                  
         ST    R1,DMCB+8                                                        
         MVI   DMCB+8,C'P'         SET P3 BYTE 1 TO C'P'                        
         CLI   WIDTHOPT,C'W'       UNLESS ITS WIDE                              
         BNE   *+8                                                              
         MVI   DMCB+8,198          WHEN PRINT LINES ARE 198 APART               
         GOTO1 CHOPPER,DMCB                                                     
*                                                                               
GENOUTX  B     XIT2                                                             
         EJECT                                                                  
* SHARED TOTAL ROUTINE                                                          
*                                                                               
TOTOUT   MVC   BLOCK(80),FILLER                                                 
         ZIC   R6,ROW1WIDE                                                      
         CLC   LABLAREA,FILLER                                                  
         BNH   TOTOUT2                                                          
         CLI   ROWWIDTH,7                                                       
         BL    TOTOUT2                                                          
         LA    RF,L'LABLAREA                                                    
         LA    R1,LABLAREA+L'LABLAREA-1                                         
         CLI   0(R1),C' '                                                       
         BH    *+10                                                             
         BCTR  R1,0                                                             
         BCT   RF,*-10                                                          
         LA    RF,1(RF)                                                         
         CR    RF,R6                                                            
         BNH   TOTOUT4                                                          
         ZIC   R6,ROWWIDTH                                                      
         CR    RF,R6                                                            
         BNH   TOTOUT4                                                          
*                                                                               
TOTOUT2  CH    R6,=H'3'                                                         
         BL    TOTOUTX                                                          
         MVC   BLOCK(3),=C'ALL'                                                 
         LA    R1,BLOCK+4                                                       
         CH    R6,=H'5'                                                         
         BL    TOTOUT6                                                          
         MVC   BLOCK(5),=C'*ALL*'                                               
         LA    R1,BLOCK+6                                                       
         CH    R6,=H'8'                                                         
         BL    TOTOUT6                                                          
         MVC   BLOCK(8),=C'*TOTALS*'                                            
         LA    R1,BLOCK+9                                                       
         B     TOTOUT6                                                          
*                                                                               
TOTOUT4  MVI   BLOCK,C'*'                                                       
         MVC   BLOCK+1(15),LABLAREA                                             
         MVC   BLOCK+17(7),=C'TOTALS*'                                          
         LA    R1,BLOCK+25                                                      
*                                                                               
TOTOUT6  BAS   RE,TESTWGT                                                       
         BNE   *+8                                                              
         BAS   RE,FMTWGT                                                        
         OC    COUNT,COUNT                                                      
         BZ    TOTOUT8                                                          
         MVI   BLOCK+45,C'('                                                    
         EDIT  (2,COUNT),(8,BLOCK+46),ALIGN=LEFT                                
         LR    R1,R0                                                            
         LA    R1,BLOCK+46(R1)                                                  
         MVI   0(R1),C')'                                                       
*                                                                               
TOTOUT8  GOTO1 SQUASHER,DMCB,BLOCK,80                                           
         LA    R1,5                                                             
         ST    R1,DMCB+8                                                        
         MVI   DMCB+8,198                                                       
         GOTO1 CHOPPER,DMCB,(80,BLOCK),((R6),0(R3))                             
*                                                                               
TOTOUTX  MVC   OUTAREA,FILLER                                                   
         B     XIT2                                                             
         EJECT                                                                  
* ROUTINE TO TEST WHETHER MARKET WEIGHTING COMMENT IS NEEDED IN TOTAL           
* OUTPUT : CC EQ - YES  R4=N'MARKETS,R5=TOTAL WEIGHT                            
*          CC NE - NO                                                           
*                                                                               
TESTWGT  LR    R0,RE                                                            
         CLI   MKTLEV,0            TEST MARKET IS A ROW                         
         BE    TESTWGT2                                                         
         TM    OUTIND,OUTICRMK     AND TOTAL IS ACROSS MARKETS                  
         BZ    TESTWGT2                                                         
         CLI   SBQMKTWT,C'N'       AND MARKET WEIGHTING IS IN EFFECT            
         BE    TESTWGT2                                                         
         ICM   R5,15,TOTWGT        AND THERE IS A DECENT MARKET WEIGHT          
         BZ    TESTWGT2                                                         
         ICM   R4,15,TOTMKT                                                     
         BZ    TESTWGT2                                                         
         LR    RE,R0               YES-THEN MARKET WEIGHTING IS ON              
         CR    RE,RE                                                            
         BR    RE                                                               
*                                                                               
TESTWGT2 LTR   RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
* ROUTINE TO FORMAT N'MARKETS AND TOTAL MARKET WEIGHT TO TOTAL LINE             
* INPUT  : R1=A(PRINT POSITION)                                                 
*          R4=N'MARKETS                                                         
*          R5=TOTAL WEIGHT                                                      
*          R6=MAX PRINT WIDTH                                                   
* OUTPUT : CC EQ - OK                                                           
*             NE - NOT ENOUGH ROOM                                              
*                                                                               
FMTWGT   NTR1  ,                                                                
         LR    R2,R1                                                            
         MVC   PARAS(10),FILLER                                                 
         EDIT  (R4),(3,PARAS),ALIGN=LEFT                                        
         LA    R1,5                                                             
         AR    R1,R0                                                            
         EDIT  (R5),(6,PARAS+3),2,ALIGN=LEFT                                    
         LA    RE,4                                                             
         AR    RE,R0                                                            
         CR    R1,RE                                                            
         BNL   *+6                                                              
         LR    R1,RE                                                            
         CR    R1,R6                                                            
         BH    FMTWGT2                                                          
         MVC   0(5,R2),=C'MKTS='                                                
         MVC   5(3,R2),PARAS                                                    
         MVC   9(4,R2),=C'CVG='                                                 
         MVC   13(6,R2),PARAS+3                                                 
         CR    RB,RB                                                            
         B     FMTWGTX                                                          
*                                                                               
FMTWGT2  LTR   RB,RB                                                            
*                                                                               
FMTWGTX  B     XIT2                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
BUYKEYSV DC    XL13'00'                                                         
XFFS     DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'                           
XF0S     DC    CL4'0000'                                                        
FILLER   DC    CL80' '                                                          
*                                                                               
SVMKTRNK DS    XL1                                                              
*                                                                               
BUYDET   DS    0XL44                                                            
BUYDETC  DS    0XL48                                                            
BYEST    DS    XL1                                                              
BYLINE1  DS    XL1                                                              
BYLINE2  DS    XL1                                                              
BYSTART  DS    XL3                                                              
BYEND    DS    XL3                                                              
BYWKS    DS    XL1                                                              
BYDPT    DS    CL1                                                              
BYLEN    DS    XL1                                                              
BYDAY    DS    XL1                                                              
BYTIME   DS    XL4                                                              
BYSEDAY  DS    XL1                                                              
BYPROG   DS    XL17                                                             
BYPKGIND DS    XL1                                                              
BYMGDATE DS    XL2                                                              
BYMGSPOT DS    XL1                                                              
BYIND    DS    XL1                                                              
BYICOM   EQU   X'80'               PRINT COMMENTS                               
BYIMG    EQU   X'40'               MAKEGOOD ACTIVITY                            
BYDA     DS    XL4                                                              
BYCOST   DS    XL4                 OPTIONAL COST                                
*                                                                               
MYDEMOS  DC    80X'00'                                                          
NMYDEMOS EQU   8                                                                
*                                                                               
DBAREA   DS    CL256                                                            
         SPACE 2                                                                
DBLOCKD  DSECT                                                                  
*                                                                               
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
*        INCLUDE SPGENSTAB                                                      
*        INCLUDE SPGENREP                                                       
*        INCLUDE SPGENCOM                                                       
*        INCLUDE SPGENCLRST                                                     
*        INCLUDE SPMGAD                                                         
*        INCLUDE DRGLOBAL                                                       
*        INCLUDE DRIVETABLE                                                     
*        INCLUDE DRINTRECD                                                      
*        INCLUDE DDSPOOLD                                                       
*        INCLUDE DDSPLWORKD                                                     
*        INCLUDE SPWRIFFD                                                       
*        INCLUDE SPWRIF1D                                                       
*        INCLUDE MAAORLKD                                                       
         PRINT OFF                                                              
       ++INCLUDE SPGENSTAB                                                      
REPRECD  DSECT                                                                  
       ++INCLUDE SPGENREP                                                       
COMHDRD  DSECT                                                                  
       ++INCLUDE SPGENCOM                                                       
       ++INCLUDE SPGENCLRST                                                     
       ++INCLUDE SPMGAD                                                         
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE DRIVETABLE                                                     
       ++INCLUDE DRINTRECD2                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SPWRIF1D                                                       
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE SPWRIWORKD                                                     
         EJECT                                                                  
* SPGENBUY                                                                      
         PRINT OFF                                                              
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* SPGENGOAL                                                                     
         PRINT OFF                                                              
GOALRECD DSECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* SPGENAGY                                                                      
         PRINT OFF                                                              
AGYRECD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* SPGENINFO                                                                     
         PRINT OFF                                                              
       ++INCLUDE SPGENINFO                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* SPGENBILL                                                                     
         PRINT OFF                                                              
BILLRECD DSECT                                                                  
       ++INCLUDE SPGENBILL                                                      
         EJECT                                                                  
       ++INCLUDE MAAORLKD                                                       
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030SPSYSDRIVE05/01/02'                                      
         END                                                                    
