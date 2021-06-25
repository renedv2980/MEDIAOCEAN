*          DATA SET SPMIS01    AT LEVEL 073 AS OF 09/04/20                      
*PHASE T20B01C                                                                  
         SPACE 1                                                                
*===================================================================*           
* 22AUG96 DISPLAY LOCKIN DATA (CHANGED BUCELLEN) - SPRI                         
* 04OCT96 SUPPRESS BUY READING IF CALLED BY SPOT/NWS                            
* 270CT97 STATION LIST TO SHOW STATIONS WITH NO SPOTS                           
* 04NOV98 SUPPORT FOR NET DOLLARS                                               
* 12MAY99 SUPPORT COMBINE CASH/TRD PRDS FOR WIM (TRD=Y)                         
* 08SEP00 CONVERT TO XSPFILE LOCKIN RECORDS                                     
* 25JAN01 SUPPORT COS2 (LIKE PW)                                                
* 10APR01 PURPOSE CODES                                                         
* 04MAY01 DISTINGUISH STATION FROM MARKET LOCKIN                                
* 15JUN01 PASS A(DMWORK) ON A GETREC, MORON                                     
*   APR04 2-DECIMAL RATING SUPPORT                                              
*   MAR05 FIX STATION LIST WITH LOCKIN DATA                                     
* 03MAY06 USE CORERES SLNTAB                                                    
* 15SEP19 SUPPORT 00A 2-DEC PRECISION IMPRESSIONS FOR US TV                     
*         MEDIA R DEMOS ONLY SUPPORT 1-DEC PRECISION DEMOS VALUES               
*===================================================================*           
         TITLE 'T20B01 - MIS BUY/GOAL RECORD PROCESSING'                        
T20B01   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20B01,RR=R8                                                   
         ST    R8,RELO                                                          
         USING GENOLD,RC                                                        
         L     RC,0(R1)            A(WORK AREA)                                 
         USING T20BFFD,RA                                                       
*                                                                               
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING T20B01+4096,R9                                                   
*                                                                               
M0       L     R1,ABUCKETS               GET BUCKET ADDRESS                     
         LH    RF,=Y(MISTWA2X-BUCKETS)     BUCKET LENGTH                        
         SR    RE,RE                                                            
         D     RE,=F'256'                                                       
         XC    0(256,R1),0(R1)                                                  
         LA    R1,256(R1)                                                       
         BCT   RF,*-10                                                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    0(0,R1),0(R1)                                                    
*                                                                               
         BAS   RE,GLINIT           SET UP DATES                                 
*                                                                               
         TM    SVOPT2,SVOPT2_SLK       TEST STATION LOCKIN REQ                  
         BO    M05                     DO NOT READ GOALS                        
         CLC   SAVBKEY+6(3),=3X'FF'    STATION LIST?                            
         BE    *+12                                                             
         CLI   SAVBKEY+6,0         STATION REQUESTED?                           
         BNE   *+8                 IF SO DO NOT PROCESS GOALS                   
         BAS   RE,GOALS            PROCESS GOALS                                
*                                                                               
M05      GOTO1 VCALLOV,DMCB,0,X'D9000A5F'                                       
         MVC   VGETRATE,0(R1)                                                   
*                                                                               
         MVC   SAVBKEY2,SAVBKEY    SAVE BUY KEY                                 
         MVC   SAVGKEY2,SAVGKEY    SAVE GOAL KEY                                
         MVC   SAVPER2,SAVPER      SAVE PERIOD                                  
         MVC   WEEKIND2,WEEKIND                                                 
         MVC   SAVESDM2,SAVESDMS   SAVE ESTHDR DEMOS                            
         SPACE 1                                                                
*                                                                               
         TM    SVOPT2,SVOPT2_SLK   STATION LOCKIN REQUEST                       
         BNO   *+8                                                              
         BAS   RE,LOCK             PROCESS LOCKIN                               
         BAS   RE,BUYS             PROCESS BUY RECORDS                          
         CLI   MISMED,C'C'         TEST MEDIA C                                 
         BE    *+8                 DON'T TURN THE BIT OFF                       
         NI    SAVBKEY,X'FF'-X'08'  RESET ORIGINAL BUY FLAG                     
         EJECT                                                                  
* WRITE TWAS TO DISK SAVE AREA *                                                
         SPACE 1                                                                
         LA    R2,1                NUMBER OF PAGES                              
         L     R3,AMISTWA2                                                      
         L     RF,VTWA                                                          
         MVC   DMCB+10(2),2(RF)    2 BYTE TERM NO.                              
         LA    R4,1                                                             
M10      DS    0H                                                               
         STC   R4,DMCB+8           PAGE NO                                      
         GOTO1 VDATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',,(R3)                        
*                                                                               
         LA    R4,1(R4)            NEXT PAGE                                    
         AH    R3,TWASIZE          NEXT OUTPUT AREA                             
         BCT   R2,M10                                                           
         B     EXIT                                                             
*                                                                               
RELO     DC    A(0)                                                             
         EJECT                                                                  
GLINIT   NTR1                                                                   
         MVC   DUB(6),SAVPER       REQUEST START DATE                           
         CLI   SVEOWSDY,0          TEST OUT-OF-WEEK DATA FLAG                   
         BNE   GLINIT1             YES - USE ESTIMATE START DAY                 
         CLI   SVEDAILY,C'Y'       TES DAILY ESTIMATE                           
         BE    GLINIT1             YES, USE ESTIMATE START DAY                  
         BAS   RE,GETMON           GET FIRST MONDAY IN DUB                      
*                                                                               
GLINIT1  GOTO1 VDATCON,DMCB,DUB,(2,MONDATES) FIRST MONDAY                       
         XC    MONDATES+2(108),MONDATES+2                                       
         SPACE 1                                                                
* CONVERT REQUEST START/END DATES *                                             
         SPACE 1                                                                
         GOTO1 VDATCON,DMCB,SAVPER,(2,SAVPERDT)                                 
         GOTO1 VDATCON,DMCB,SAVPER+6,(2,SAVPERDT+2)                             
*                                                                               
         LA    R0,7                SET ADDAY INCREMENT                          
         CLI   WEEKIND,C'W'        WEEKS REQUESTED?                             
         BE    GLINIT5                                                          
         LA    R0,1                SET FOR DAILY INCREMENT                      
         CLI   WEEKIND,C'D'        DAYS REQUESTED                               
         BNE   GLINITX                                                          
         SPACE 1                                                                
* CREATE UP TO 53 ADDITIONAL MONDAY DATES                                       
         SPACE 1                                                                
GLINIT5  CLI   SVEDAILY,C'Y'                                                    
         BNE   *+8                                                              
         LA    R0,1                                                             
         LA    R4,MONDATES+2                                                    
         LA    R5,53                                                            
*                                                                               
GLINIT7  GOTO1 VADDAY,DMCB,DUB,DMCB+12,(R0)  ADD TO  DAY                        
         CLC   DMCB+12(6),SAVPER+6           PAST REQUESTED END DATE?           
         BH    GLINITX                                                          
*                                                                               
         GOTO1 VDATCON,DMCB,DMCB+12,(2,(R4)) CONVERT MONDAY                     
*                                                                               
         MVC   DUB(6),DMCB+12                                                   
         LA    R4,2(R4)                                                         
         BCT   R5,GLINIT7                                                       
*                                                                               
         BCTR  R0,0                SET TO GET END OF LAST WEEK                  
         LTR   R0,R0                                                            
         BZ    GLINIT9                                                          
         GOTO1 VADDAY,(R1),DUB,DMCB+12,(R0)                                     
GLINIT9  DS    0H                                                               
         GOTO1 VDATCON,(R1),DMCB+12,(2,SAVPERDT+2)                              
         EJECT                                                                  
GLINITX  DS    0H                                                               
         GOTO1 VCALLOV,DMCB,0,X'D9000A1D'                                       
         MVC   WORK(4),0(R1)                                                    
         MVC   WORK+4(4),VADDAY                                                 
         MVC   WORK+8(4),VGETDAY                                                
         MVC   WORK+12(4),VDATCON                                               
         SR    R0,R0               SET FOR STD BDCST MNTHS                      
         CLI   SVSPPRF2,6          ALLOW ONLY BDCST MONTH OPTIONS               
         BL    GLINITX2             (4/4/5,4/5/4,5/4/4)                         
         CLI   SVSPPRF2,8                                                       
         BH    GLINITX2                                                         
         IC    R0,SVSPPRF2                                                      
GLINITX2 DS    0H                                                               
         XC    WORK2,WORK2                                                      
         MVC   WORK2+6(3),SVSPPRF6  RECONSTRUCT 15 BYTE PROF AREA               
         STC   R0,WORK2+2                                                       
         GOTO1 VMOBILE,DMCB,(12,SAVPER),((R0),POLMNTHS),WORK,          X        
               WORK2                                                            
*                                                                               
         L     RE,ABUCKLEN                                                      
         MVC   0(2,RE),=H'2'       PREPARE FOR RECUP                            
         L     RE,ABUCKETS                                                      
         MVI   0(RE),0                                                          
         B     EXIT                                                             
         EJECT                                                                  
GOALS    NTR1                                                                   
         MVI   GDCPPSW,0           CLEAR CPP SW NOT IN SAVED STORAGE            
         MVC   KEY(13),SAVGKEY     GOAL KEY                                     
*                                                                               
         CLI   SAVGKEY+4,X'FF'     POL?                                         
         BNE   *+8                                                              
         MVI   KEY+4,1             FIRST PRD                                    
*                                                                               
GOAL50   GOTO1 HIGH                                                             
         B     GOAL54                                                           
*                                                                               
GOAL52   GOTO1 SEQ                                                              
*                                                                               
GOAL54   CLC   KEY(4),SAVGKEY      A-M-CLT?                                     
         BNE   GOALXIT                                                          
         CLI   KEY+4,X'FF'         POOL?                                        
         BE    GOALXIT                                                          
*                                                                               
         CLI   SAVGKEY+4,X'FF'     REQUESTED PRD = POL ?                        
         BE    GOAL56                                                           
         CLC   KEY+4(3),SAVGKEY+4  PRD-MKT                                      
         BNE   GOALXIT                                                          
         B     GOAL100                                                          
*                                                                               
GOAL56   CLC   KEY+5(2),SAVGKEY+5  MKT?                                         
         BE    *+12                                                             
         BL    GOAL75                                                           
         B     GOAL60                                                           
*                                                                               
         CLI   SAVGKEY+7,0         EST REQUESTED?                               
         BE    GOAL58              NO                                           
*                                                                               
         CLC   KEY+7(1),SAVGKEY+7  EST                                          
         BE    GOAL130                                                          
         BL    GOAL52                                                           
         B     GOAL60                                                           
*                                                                               
GOAL58   ZIC   RE,KEY+7            GET ACTUAL EST                               
         LA    RE,SAVESLST(RE)                                                  
         CLI   0(RE),0             TEST ESTIMATE ACTIVE                         
         BNE   GOAL130             YES                                          
         MVC   KEY+8(5),=5X'FF'    FORCE NEXT EST                               
         B     GOAL50                                                           
*                                                                               
GOAL60   IC    RE,KEY+4            PRD                                          
         LA    RE,1(RE)                                                         
         STC   RE,KEY+4                                                         
GOAL75   MVC   KEY+5(3),SAVGKEY+5                                               
         XC    KEY+8(5),KEY+8                                                   
         B     GOAL50                                                           
         EJECT                                                                  
* SINGLE PRODUCT REQUEST *                                                      
         SPACE 1                                                                
GOAL100  CLI   SAVGKEY+7,0         ESTIMATE REQUESTED?                          
         BE    GOAL102                                                          
         CLC   KEY+7(1),SAVGKEY+7  TEST RIGHT EST                               
         BNE   GOALXIT                                                          
         B     GOAL115                                                          
*                                                                               
GOAL102  ZIC   RE,KEY+7                                                         
         LA    RE,SAVESLST(RE)                                                  
         CLI   0(RE),0             TEST EST ACTIVE                              
         BNE   GOAL115                                                          
         MVC   KEY+8(5),=5X'FF'                                                 
         B     GOAL50                                                           
*                                                                               
GOAL115  CLI   PIGCODE,0                                                        
         BE    GOAL140                                                          
         TM    KEY+11,X'40'        TEST PRD2 IS SUBREF                          
         BO    GOAL140             YES - NOT A PRODUCT                          
         CLC   PIGCODE,KEY+12      CHECK SECOND PRODUCT                         
         BNE   GOAL52                                                           
         B     GOAL140                                                          
*                                                                               
*<<<GOAL130  CLC   MISPRD(3),=C'POL'   CHECK FOR POOL REQUEST                   
*<<<         BE    GOAL140                                                      
*                                                                               
GOAL130  CLI   SAVGKEY+4,X'FF'     TEST POL/ALL REQUEST                         
         BE    GOAL140                                                          
*                                                                               
         CLI   KEY+11,X'80'        ELSE IGNORE PASSIVE KEYS                     
         BE    GOAL52                                                           
GOAL140  LA    R1,KEY+8            DAYPART                                      
         BAS   RE,GETDPT           GET DAYPART CODE                             
*                                                                               
         GOTO1 GETREC              GET GOAL RECORD                              
         OC    SVID,SVID                                                        
         BZ    GOAL142                                                          
         CLC   SVID,GDIDR                                                       
         BNE   GOAL52                                                           
*                                                                               
GOAL142  LA    R2,WORK             BUCKET                                       
         XC    WORK,WORK                                                        
         USING MISBUCKS,R2         BUILD BUCKET SHELL                           
         SPACE 1                                                                
* GET ALPHA PRODUCT *                                                           
         SPACE 1                                                                
         LA    RE,CPRDLIST         CLTHDR PRD LIST                              
GOAL150  CLI   0(RE),0                                                          
         BE    GOAL52              IGNORE - PRD MUST HAVE BEEN DELETED          
*                                                                               
         CLC   3(1,RE),KEY+4       PRD                                          
         BE    GOAL155                                                          
         LA    RE,4(RE)                                                         
         B     GOAL150                                                          
*                                                                               
GOAL155  MVC   BUCPRD,0(RE)                                                     
         MVI   BUCCODE,1           ELEMENT CODE                                 
         MVI   BUCELLEN,48         ELEMENT LENGTH                               
         MVC   BUCDPTCD,BYTE4      DAYPART CODE                                 
*                                                                               
         SR    RE,RE                                                            
         IC    RE,KEY+9            GET SLN                                      
         AR    RE,RE               X 2                                          
         A     RE,VSLNTAB          POINT TO ENTRY IN SLNTAB                     
         MVC   BUCLEN,1(RE)        USE THE LENGTH IT SAYS TO USE                
         EJECT                                                                  
* GET ALPHA PIGGYBACK PRODUCT *                                                 
         SPACE 1                                                                
         XC    BUCPRD2,BUCPRD2                                                  
         CLC   MISPRD(3),=C'POL'   CHECK FOR POOL REQUEST                       
         BE    GOAL170                                                          
         CLI   SAVGKEY+4,X'FF'                                                  
         BE    GOAL158                                                          
         CLI   PIGCODE,0                                                        
         BE    GOAL170                                                          
GOAL158  TM    KEY+11,X'40'        TEST PRD2 IS SUBREF                          
         BO    GOAL170                                                          
         CLI   KEY+12,0                                                         
         BE    GOAL170                                                          
*                                                                               
         LA    RE,CPRDLIST         CLTHDR PRD LIST                              
GOAL160  CLI   0(RE),0                                                          
         BE    GOAL52              IGNORE - PRD MUST HAVE BEEN DELETED          
*                                                                               
         CLC   3(1,RE),KEY+12      PIGGYBACK PRODUCT                            
         BE    GOAL165                                                          
         LA    RE,4(RE)                                                         
         B     GOAL160                                                          
*                                                                               
GOAL165  MVC   BUCPRD2,0(RE)                                                    
         SR    RE,RE                                                            
         IC    RE,KEY+10           SLN                                          
         AR    RE,RE               X 2                                          
         A     RE,VSLNTAB          POINT TO ENTRY FOR THIS SLN                  
         MVC   BUCLEN,1(RE)        AND USE THE LENGTH IT SAYS TO USE            
         CLC   BUCPRD,BUCPRD2                                                   
         BL    GOAL170                                                          
* SWAP THE FIELDS                                                               
         XC    BUCPRD,BUCPRD2                                                   
         XC    BUCPRD2,BUCPRD                                                   
         XC    BUCPRD,BUCPRD2                                                   
*                                                                               
GOAL170  CLI   SAVGKEY+4,X'FF'                                                  
         BE    GOAL175                                                          
         CLI   PIGCODE,0                                                        
         BNE   GOAL175                                                          
         SR    RE,RE                                                            
         IC    RE,KEY+9                                                         
         AR    RE,RE                                                            
         A     RE,VSLNTAB                                                       
         MVC   BUCLEN,1(RE)                                                     
*                                                                               
GOAL175  BAS   RE,BXLE             SET UP ELEMENT SCAN                          
*                                                                               
GOAL176  CLI   0(R3),0             GOAL REC HAS X'00' AT END?                   
         BE    GOAL52              YES...AVOID INFINITE BXLE LOOP               
         TM    SVOPT2,SVOPT2_MLK   TEST MARKET LOCKIN REQUEST                   
         BNO   GOAL178                                                          
*                                                                               
         CLI   0(R3),X'30'         GOAL LOCKIN ELEMENT                          
         BE    GOAL500                                                          
         CLI   0(R3),X'31'                                                      
         BE    GOAL500                                                          
         B     GOAL180                                                          
*                                                                               
GOAL178  CLI   FORMIND,C'L'        TEST LOCKED GOAL REQ                         
         BNE   GOAL179                                                          
         CLI   0(R3),X'A1'                                                      
         BE    GOAL190                                                          
         B     GOAL180                                                          
*                                                                               
GOAL179  CLI   0(R3),X'21'         GOAL WEEK ELEMENT?                           
         BE    GOAL190                                                          
*                                                                               
GOAL180  IC    R4,1(R3)            ELEM LEN                                     
         BXLE  R3,R4,GOAL176                                                    
         B     GOAL52                                                           
         EJECT                                                                  
* PROCESS GOAL WEEK ELEMENT                                                     
         SPACE 1                                                                
GOAL190  CLC   2(2,R3),MONDATES    GOAL ELEM DATE V. REQ START DATE             
         BL    GOAL180                                                          
*                                                                               
         CLC   2(2,R3),SAVPERDT+2  GOAL ELEM DATE V. REQ END DATE               
         BH    GOAL180                                                          
*                                                                               
         MVC   BUCDATE,2(R3)       GOAL ELEM DATE                               
         MVC   BUCGDOLS,8(R3)      GOAL DOLLARS                                 
         L     R0,4(R3)            GET GOAL POINTS                              
         N     R0,=X'3FFFFFFF'     DROP HOB'S                                   
         TM    4(R3),X'40'         TEST 2-DECIMAL                               
         JO    *+8                                                              
         MHI   R0,10               PUT POINTS X 100 IN BUCKET                   
         ST    R0,BUCGPTS          GOAL POINTS                                  
         OI    BUCGPTS,X'40'       SET 2-DEC FLAG                               
*                                                                               
         OC    SVCOS2,SVCOS2       TEST COST2 ESTIMATE                          
         BZ    GOAL192                                                          
         TM    SVOPTS,SVOPT$2      TEST CLIENT VERSION                          
         BO    GOAL192             YES - DO NOT ADJUST (DYJLTG?)                
* ADJUST GOAL DOLLARS BY COS2 FACTOR                                            
         ICM   R0,15,BUCGDOLS                                                   
         CVD   R0,DUB              MAKE AMOUNT PACKED                           
         ZAP   WORK2(16),DUB                                                    
         SRP   WORK2(16),6,0       SCALE UP BY 10**6                            
*                                                                               
         L     R0,SVCOS2           GET COS2 FACTOR                              
         CVD   R0,DUB              MAKE IT PACKED                               
         DP    WORK2(16),DUB       GOAL DOLS/FACTOR                             
*                                                                               
         ZAP   DUB,WORK2(8)        GET QUOTIENT                                 
         CVB   R0,DUB                                                           
         STCM  R0,15,BUCGDOLS                                                   
         B     GOAL200                                                          
*                                                                               
GOAL192  OC    SVEPWPCT,SVEPWPCT   TEST PW CLIENT                               
         BZ    GOAL200                                                          
         TM    SVOPTS,SVOPTPW      TEST CLIENT VERSION                          
         BZ    GOAL194             NO                                           
         TM    SVOPTS,SVOPTNTX     TEST SUPPRESS TAX                            
         BZ    GOAL200             NO - DONE                                    
* NEED TO COMPUTE GOAL DOLLARS / 1+TAXRATE                                      
         L     R1,8(R3)                                                         
         M     R0,=F'200000'       X 100000 X 2 (SCALE)                         
         SR    RF,RF                                                            
         ICM   RF,3,SVPWTAX                                                     
         A     RF,=F'100000'       ADD 100 PERCENT                              
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BNP   *+8                                                              
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         ST    R1,BUCGDOLS                                                      
         B     GOAL200                                                          
*                                                                               
* ADJUST GOAL DOLLARS                                                           
*                                                                               
GOAL194  LA    R7,WORK3                                                         
         USING PWBLKD,R7                                                        
         XC    WORK3,WORK3                                                      
         XC    WORK3A,WORK3A                                                    
*                                                                               
         MVI   PWACT,PWGETGOL                                                   
         TM    SVOPTS,SVOPTNTX     TEST SUPPRESS TAX                            
         BZ    *+8                                                              
         OI    PWFLAG,PWFLAG_NOTAX                                              
         MVC   PWACTGOL,8(R3)      GOAL DOLLARS                                 
         SR    R0,R0                                                            
         ICM   R0,7,SVEPWPCT                                                    
         ST    R0,PWPCT                                                         
         MVC   PWTAXRT+2(2),SVPWTAX                                             
         GOTO1 VPWCALC,DMCB,(R7)                                                
         CLI   PWERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BUCGDOLS,PWVAL      MOVE ADJUST GOALS                            
         DROP  R7                                                               
*                                                                               
GOAL200  OC    BUCGDOLS,BUCGDOLS   NO GOAL DOLLARS                              
         BZ    GOAL202                                                          
         L     R0,BUCGPTS                                                       
         N     R0,=X'3FFFFFFF'                                                  
         LTR   R0,R0                                                            
         BZ    *+8                                                              
         B     GOAL300                                                          
*                                                                               
GOAL202  CLI   SAVCLPRO+8,C'0'     CPP                                          
         BNE   GOAL300                                                          
         CLI   KEY+8,C'+'          TEST SPECIAL CCUSA DPT                       
         BE    GOAL300             WITH NO POINTS/NO CPP GUIDES                 
         CLI   KEY+8,C'-'                                                       
         BE    GOAL300                                                          
*                                                                               
         CLI   GDCPPES,0           TEST NEW CPP GUIDE USED                      
         BE    GOAL240             NO                                           
         CLI   GDCPPSW,0           TEST READ FOR THIS REQUEST                   
         BE    GOAL242             NO - READ IT NOW                             
*                                                                               
GOAL240  CLC   POLGIND,KEY+7       TEST HAVE CPP FOR THIS EST YET               
         BE    GOAL250                                                          
                                                                                
*==========================================================                     
* GET CPP GUIDE *                                                               
*==========================================================                     
                                                                                
GOAL242  MVI   GDCPPSW,C'Y'        SET WE HAVE READ IT THIS TIME                
         MVC   WORK2(13),KEY                                                    
         BAS   RE,GETGUIDE         GET CPP GUIDES                               
         MVC   KEY(13),WORK2       RESTORE KEY                                  
         GOTO1 READ                FOR SEQ CONTINUATION                         
         GOTO1 GETREC              GET GOAL REC                                 
         EJECT                                                                  
* COMPUTE DOLLARS FROM CPP USING TOT SECS                                       
         SPACE 1                                                                
GOAL250  DS    0H                                                               
         GOTO1 CPPCONV,DMCB,(BUCDPTCD,BUCGPTS),(KEY+10,BUCGDOLS),      X        
               (KEY+8,0)                                                        
*                                                                               
GOAL300  DS    0H                                                               
         CLI   SAVGKEY+4,X'FF'     POL NEEDS TO SUM ALL PRDS                    
         BE    GOAL310                                                          
         CLI   PIGCODE,0          ?????? TEST P/B REQUEST ?????                 
         BNE   GOAL320                                                          
*                                                                               
GOAL310  TM    GKEYAGY-GKEY+KEY,X'40'  TEST PRD2 IS SUBREF                      
         BO    GOAL320                                                          
         CLI   GKEYPRD2-GKEY+KEY,0     TEST PIGGYBACK GOAL                      
         BE    GOAL320                 NO                                       
         SPACE 1                                                                
* APPORTION DOLLARS BETWEEN BRANDS FOR PIGGYBACKS *                             
         SPACE 1                                                                
         ZIC   R0,GKEYSLN-GKEY+KEY    GET LEN THIS PRD                          
         AR    R0,R0                  X 2                                       
         L     R1,BUCGDOLS                                                      
         MR    R0,R0                                                            
         ZIC   RE,GKEYSEC-GKEY+KEY    GET TOTAL LEN                             
         DR    R0,RE                                                            
         A     R1,=F'1'                                                         
         SRL   R1,1                                                             
         ST    R1,BUCGDOLS                                                      
*                                                                               
GOAL320  MVI   BYTE,0                                                           
         CLC   MISSTA(4),=C'LIST'                                               
         BNE   GOAL350                                                          
         EJECT                                                                  
* FIRST STATION HAS GOALS                                                       
         SPACE 1                                                                
         MVI   BYTE,X'FF'                                                       
         MVC   BUCPRD,=X'000001'   GOAL STATION IND                             
         B     GOAL400                                                          
*                                                                               
GOAL350  CLI   WEEKIND,C'W'        WEEKS REQUESTED?                             
         BNE   GOAL400                                                          
         CLI   SAVGKEY+4,X'FF'     POL?                                         
         BNE   GOAL400                                                          
         MVI   BYTE,X'FF'                                                       
         MVC   BUCPRD,=C'POL'                                                   
*                                                                               
GOAL400  BAS   RE,FINDBUC          ADD GOAL DATA TO BUCKETS                     
*                                                                               
         CLI   BYTE,X'FF'                                                       
         BE    GOAL180             BUFFER TOO SMALL FOR ALL BRAND'S             
*                                  WEEKS-DPT-SLN COMBOS                         
         CLI   SAVGKEY+4,X'FF'     POOL?                                        
         BNE   GOAL180                                                          
         MVC   SVEPRODS(3),BUCPRD                                               
         MVC   SVEPRODS+3(3),BUCPRD2                                            
         MVC   BUCPRD,=C'ZZZ'    FOR SORTING                                    
         XC    BUCPRD2,BUCPRD2                                                  
         BAS   RE,FINDBUC                                                       
         MVC   BUCPRD,SVEPRODS     RESTORE PRD                                  
         MVC   BUCPRD2,SVEPRODS+3  RESTORE PRD                                  
         B     GOAL180             GET NEXT GOAL ELEMENT                        
* PROCESS GOAL LOCKIN ELEMENT                                                   
         SPACE 1                                                                
GOAL500  CLC   2(2,R3),MONDATES    COMPARE W/ REQ START DATE                    
         BL    GOAL180                                                          
         CLC   2(2,R3),SAVPERDT+2  COMPARE W/ REQ END DATE                      
         BH    GOAL180                                                          
         MVC   BUCDATE,2(R3)                                                    
*                                                                               
         MVC   BUCLSPTS+2(2),6(R3)                                              
*                                                                               
         ICM   R0,15,8(R3)                                                      
         MHI   R0,10                                                            
         CLI   0(R3),X'30'         CALC MONTHLY DOLLARS X 100                   
         BNE   *+8                                                              
         MHI   R0,10                                                            
         STCM  R0,15,BUCGDOLS                                                   
*                                                                               
         LA    R1,12(R3)           POIN TO THE DEMCODE                          
         ZIC   R7,1(R3)            ELEMENT LENGTH                               
         LA    R0,12               FIXED LENGTH                                 
         SR    R7,R0                                                            
         SR    R6,R6                                                            
         LTR   R7,R7                                                            
         BZ    GOAL180             NO CLKDEM?                                   
         D     R6,=F'7'            HOW MANY CLKDEM ARE THERE?                   
*                                                                               
GOAL505  CLC   SVDEMLST(3),0(R1)   DOES THE DEMCODE MATCH?                      
         BE    GOAL506                                                          
         LA    R1,7(R1)            BUMP TO THE NEXT DEMCODE                     
         BCT   R7,GOAL505                                                       
         B     GOAL180             NO MATCH FOUND                               
                                                                                
GOAL506  ICM   R0,15,3(R1)         EXTRACT THE VALUE                            
         CLI   0(R3),X'30'         CALC MONTHLY POINTS X 10                     
         BNE   *+8                                                              
         MHI   R0,10                                                            
         STCM  R0,15,BUCGPTS                                                    
*                                                                               
         CLC   MISSTA(4),=C'LIST'                                               
         BNE   GOAL508                                                          
         MVI   BYTE,X'FF'                                                       
         MVC   BUCPRD,=X'000001'   GOAL STATION IND                             
         B     GOAL510                                                          
*                                                                               
GOAL508  CLI   WEEKIND,C'W'        WEEKS REQUESTED?                             
         BNE   GOAL510                                                          
         CLI   SAVGKEY+4,X'FF'     POL?                                         
         BNE   GOAL510                                                          
         MVI   BYTE,X'FF'                                                       
         MVC   BUCPRD,=C'POL'                                                   
*                                                                               
GOAL510  BAS   RE,FINDBUC                                                       
*                                                                               
         CLI   BYTE,X'FF'                                                       
         BE    GOAL180             BUFFER TOO SMALL FOR ALL BRAND'S             
*                                  WEEKS-DPT-SLN COMBOS                         
         CLI   SAVBKEY+3,X'FF'     POOL?                                        
         BNE   GOAL180                                                          
         MVC   SVEPRODS(3),BUCPRD                                               
         MVC   SVEPRODS+3(3),BUCPRD2                                            
         MVC   BUCPRD,=C'ZZZ'    FOR SORTING                                    
         XC    BUCPRD2,BUCPRD2                                                  
         BAS   RE,FINDBUC                                                       
         MVC   BUCPRD,SVEPRODS     RESTORE PRD                                  
         MVC   BUCPRD2,SVEPRODS+3  RESTORE PRD                                  
         B     GOAL180             GET NEXT LOCKIN ELEMENT                      
GOALXIT  B     EXIT                                                             
         EJECT                                                                  
* THIS ROUTINE GETS POL-30 CPP GUIDES                                           
* AND SETS POLGIND TO  EST NUMBER READ                                          
         SPACE 1                                                                
GETGUIDE NTR1                                                                   
*                                                                               
         LA    R0,15               CLEAR CPP SAVE AREA                          
         LA    R1,POLGUIDE                                                      
         XC    0(49,R1),0(R1)                                                   
         LA    R1,49(R1)                                                        
         BCT   R0,*-10                                                          
*                                                                               
         LA    R4,POLMNTHS                                                      
*                                                                               
         MVC   POLGIND,KEY+7       SAVE GOAL EST NUMBER                         
         MVI   KEY+4,X'FF'                                                      
         MVI   BYTE4,0                                                          
         CLI   GDCPPES,0                                                        
         BE    GETGD2                                                           
         MVC   KEY+2(2),GDCPPCL                                                 
         MVC   KEY+7(1),GDCPPES                                                 
         MVC   POLGIND,KEY+7       SAVE FIRST CPP EST NUMBER                    
         MVC   BYTE4,GDCPPES2      SAVE SECOND CPP EST                          
*                                                                               
GETGD2   DS    0H                                                               
         XC    KEY+8(5),KEY+8                                                   
         GOTO1 HIGH                                                             
         B     GETGD6                                                           
*                                                                               
GETGD4   GOTO1 SEQ                                                              
*                                                                               
GETGD6   CLC   KEY(8),KEYSAVE      02/A-M/C/P/MKT/EST                           
         BE    GETGD8                                                           
         CLI   BYTE4,0             TEST NEED SECOND CPP GUIDE                   
         BE    GETGDX              NO                                           
GETGD7   XC    KEY,KEY                                                          
         MVC   KEY(7),KEYSAVE      RESTORE KEY                                  
         MVC   KEY+7(1),BYTE4                                                   
         MVI   BYTE4,0                                                          
         B     GETGD2                                                           
*                                                                               
GETGD8   GOTO1 GETREC                                                           
         EJECT                                                                  
* MATCH EXISTING DPT SLOT OR FIND NEW ONE                                       
         SPACE 1                                                                
         LA    R7,POLGUIDE                                                      
         LA    R0,15                                                            
GETGD9   CLC   0(1,R7),KEY+8                                                    
         BE    GETGD9X                                                          
         CLI   0(R7),0                                                          
         BE    GETGD9X                                                          
         LA    R7,49(R7)                                                        
         BCT   R0,GETGD9                                                        
         DC    H'0'                TOO MANY DPTS                                
GETGD9X  MVC   0(1,R7),KEY+8       MOVE DAYPART TO TABLE                        
         LA    R7,1(R7)            POINT TO FIRST MONTH BUCKET                  
         LA    R4,POLMNTHS         POINT TO MONTH LIST                          
*                                                                               
         SR    R0,R0                                                            
         LA    R3,GDELEM                                                        
*                                                                               
GETGD10  CLI   0(R3),X'21'                                                      
         BNE   GETGD12                                                          
         CLC   2(2,R3),0(R4)       ELEM TO TABLE MONTH START                    
         BL    GETGD12                                                          
         CLC   2(2,R3),2(R4)       ELEM TO TABLE MONTH END                      
         BH    GETGD14                                                          
         MVC   0(4,R7),8(R3)       MOVE DOLLARS                                 
         LA    R7,4(R7)            NEXT SAVE BUCKET                             
         LA    R4,4(R4)            NEXT MONTH                                   
         CLI   0(R4),X'FF'         TEST LAST MONTH                              
         BE    GETGD4                                                           
GETGD12  IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),0                                                          
         BNE   GETGD10                                                          
         B     GETGD4                                                           
*                                                                               
GETGD14  LA    R7,4(R7)            NEXT SAVE BUCKET                             
         LA    R4,4(R4)            NEXT MONTH                                   
         CLI   0(R4),X'FF'         TEST EOL                                     
         BNE   GETGD10                                                          
         B     GETGD4                                                           
*                                                                               
GETGDX   B     EXIT                                                             
         EJECT                                                                  
* SUBROUTINE TO CONVERT DOLLARS VIA CPP GUIDE                                   
*                                                                               
*        P1=A(GOAL POINTS)         BYTE 0=DAYPART CODE)                         
*        P2=A(GOAL DOLLARS)        BYTE 0=LENGTH                                
*        P3=        ---            BYTE 0=ALPHA DAYPART)                        
         SPACE 2                                                                
CPPCONV  NTR1                                                                   
         ST    R1,FULL             SAVE PARM REG                                
         XC    DUB,DUB                                                          
                                                                                
* GET EQUIVALENCE FACTOR FROM EQUREC *                                          
                                                                                
         SR    R5,R5                                                            
         IC    R5,4(R1)            GET SLN                                      
         AR    R5,R5               X 2                                          
         A     R5,VSLNTAB          POINT TO ENTRY IN SLN TABLE                  
         MVC   4(1,R1),1(R5)       USE EQSLN FROM NOW ON                        
         SR    R4,R4                                                            
         IC    R4,0(R5)            GET DSPL TO EQUIV FACTOR                     
         LA    R4,EQUSECT1(R4)     POINT TO FACTOR                              
*                                                                               
         MVC   DUB+6(2),0(R4)      EQUIV FACTOR                                 
                                                                                
* GET POL-30 CPP GUIDE PTS AND DOLLARS *                                        
                                                                                
         LA    R4,POLGUIDE                                                      
         LA    R5,15                                                            
CPPCV202 CLC   0(1,R4),8(R1)       MATCH DPT                                    
         BE    CPPCV204                                                         
         LA    R4,49(R4)                                                        
         CLI   0(R4),0                                                          
         BE    CPPCVXIT                                                         
         BCT   R5,CPPCV202                                                      
         B     CPPCVXIT                                                         
                                                                                
* FIND MONTHLY CPP *                                                            
                                                                                
CPPCV204 DS    0H                                                               
         LA    R4,1(R4)            POINT TO FIRST BUCKET                        
         LA    R5,POLMNTHS                                                      
CPPCV206 CLC   BUCDATE,0(R5)       IGNORE IF PRIOR TO MONTH START               
         BL    CPPCV208                                                         
         CLC   BUCDATE,2(R5)       USE IF NOT AFTER MONTH END                   
         BNH   CPPCV210                                                         
CPPCV208 LA    R4,4(R4)                                                         
         LA    R5,4(R5)                                                         
         CLI   0(R5),X'FF'                                                      
         BNE   CPPCV206                                                         
         B     CPPCVERR                                                         
*                                                                               
CPPCV210 LM    RE,RF,0(R1)         GET A(POINTS)/A(DOLLARS)                     
         OC    0(4,RF),0(RF)       TEST 0 DOLLARS                               
         BNZ   CPPPTS                                                           
                                                                                
* CALCULATE GOAL $ FROM GOAL POINTS USING CPP GUIDE                             
                                                                                
         L     R7,0(RE)            GOAL PTS (X 100)                             
         N     R7,=X'3FFFFFFF'     DROP 2-DEC FLAG                              
         MVC   FULL,0(R4)          POL-30 $                                     
*                                                                               
         M     R6,DUB+4            EQ FACTOR                                    
         M     R6,FULL             GP X PG$                                     
*                                                                               
         LHI   R8,10000            CPP PTS (100 POINTS IN HUNDREDTHS)           
         MHI   R8,1000             X 30 SEC EQU FACTOR                          
*                                                                               
         DR    R6,R8               GP X PG$ X EF                                
*                                  -------------                                
*                                  PGP X EF30                                   
         SRA   R8,1                                                             
         CR    R6,R8               REMAINDER                                    
         BL    *+8                                                              
         LA    R7,1(R7)            ROUND                                        
*                                                                               
         ST    R7,0(RF)            OUTPUT RESULT = GOAL $                       
         B     CPPCVXIT                                                         
         SPACE 1                                                                
* CALCULATE POINTS FROM DOLLARS  GOAL$ * 1000     EF30                          
*                                ------------  *  ----                          
*                                    CPP$          EF                           
         SPACE 1                                                                
CPPPTS   L     R7,0(RF)            GOAL $                                       
         M     R6,=F'20000'        X 10000 X 2                                  
         D     R6,0(R4)            CPP$                                         
         A     R7,=F'1'            ROUND                                        
         SRL   R7,1                / 2                                          
         AR    R7,R7               X 2                                          
*                                                                               
         M     R6,=F'1000'         EF30                                         
         LH    R8,DUB+6            EF                                           
         DR    R6,R8                                                            
         A     R7,=F'1'            ROUND                                        
         SRL   R7,1                                                             
         ST    R7,0(RE)            GOAL POINTS TO 2-DEC                         
         OI    0(RE),X'40'         SET 2-DEC FLAG                               
*                                                                               
CPPCVXIT B     EXIT                                                             
*                                                                               
CPPCVERR MVI   ERRAREA,X'FF'                                                    
         FOUT  MISMSGH,NOCPPMSG,60                                              
         B     MISERR                                                           
*                                                                               
NOCPPMSG DC    CL60'UNABLE TO FIND CPP GUIDE - CHECK MON-SUN REQUEST'           
         SPACE 1                                                                
* SET UP BXLE LOOP FOR ELEMENT SCANNING                                         
*                                                                               
BXLE     LA    R3,IOAREA+24                                                     
         SR    R4,R4                                                            
         MVC   HALF,IOAREA+13                                                   
         LH    R5,HALF                                                          
         LA    R5,IOAREA-1(R5)                                                  
         BR    RE                                                               
*                                                                               
MISERR   GOTO1 ERROR                                                            
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
EXIT     XIT1                                                                   
EXXMOD   EQU   EXIT                                                             
         EJECT                                                                  
* PROCESS BUY RECORDS *                                                         
         SPACE 1                                                                
BUYS     NTR1                                                                   
*                                                                               
         CLC   SVXFRCTL,=C'SPONWS'      NWS DOESN'T NEED BUY DATA               
         BE    EXIT                                                             
*                                                                               
         CLI   SVORIGNL,C'Y'                                                    
         BNE   *+8                                                              
         OI    SAVBKEY,X'08'            SET TO READ ORIGINAL BUYS               
*                                                                               
         MVC   KEY(13),SAVBKEY          BUY KEY                                 
*                                                                               
         CLC   SAVBKEY+6(3),=3X'FF'     STATION LIST?                           
         BNE   *+10                                                             
         XC    KEY+6(3),KEY+6                                                   
*                                                                               
BUY25    GOTO1 HIGH                                                             
         B     BUY30                                                            
*                                                                               
NEXTBUY  GOTO1 SEQ                 GET NEXT SPTDIR KEY                          
*                                                                               
BUY30    CLC   KEY(6),SAVBKEY      SAME A-M-CLT-PRD-MKT?                        
         BNE   EXIT                                                             
*                                                                               
BUY50    CLC   SAVBKEY+6(3),=3X'FF'     TEST STATION LIST                       
         BE    BUY60                                                            
         OC    SAVBKEY+6(3),SAVBKEY+6   TEST STATION REQUESTED                  
         BE    BUY60                    NO                                      
         L     RF,=X'FFFFFFFF'          SET RADIO MASK (0 BIT NTWK)             
         CLI   MISMED,C'R'              TEST RADIO                              
         BE    BUY52                                                            
*                                                                               
         L     RF,=X'FFFFFFE0'          SET TV/NTWK MASK (5 BITS)               
         CLI   SVAGPRF7,C'C'            TEST CANADA                             
         BNE   BUY51                                                            
         L     RF,=X'FFFFFF00'                                                  
BUY51    CLI   MISSTA,C'0'              TEST LOCAL CABLE                        
         BL    BUY52                    NO - USE 5 BIT MASK                     
*                                                                               
         L     RF,=X'FFFFFF80'          SET LOCAL CABLE (7 BITS)                
         TM    SAVBKEY+8,X'7F'          TEST NETWORK SPECIFIED                  
         BZ    BUY52                    NO - IGNORE NETWORK                     
         L     RF,=X'FFFFFFFF'          ELSE USE ALL BITS                       
*                                                                               
BUY52    SR    R0,R0                                                            
         ICM   R0,7,KEY+6                                                       
         NR    R0,RF                                                            
         SR    RE,RE                                                            
         ICM   RE,7,SAVBKEY+6                                                   
         NR    RE,RF                                                            
         CR    R0,RE                                                            
         BNE   EXIT                                                             
*                                                                               
BUY60    CLI   SVAGPRF7,C'C'       TEST CANADA                                  
         BE    BUY62                                                            
         CLC   MISSTA(4),=C'ALL-'  EXCLUDE CABLE                                
         BNE   BUY61                                                            
         CLI   KEY+6,X'E8'         YES - IS THIS CABLE                          
         BNL   NEXTBUY                                                          
         B     BUY62                                                            
*                                                                               
BUY61    CLC   MISSTA(4),=C'ALL/'  ONLY CABLE                                   
         BNE   BUY62                                                            
         CLI   KEY+6,X'E8'         YES                                          
         BL    NEXTBUY                                                          
*                                                                               
BUY62    CLI   SAVBKEY+9,0         EST REQUESTED?                               
         BE    BUY65                                                            
*                                                                               
         CLC   KEY+9(1),SAVBKEY+9  ESTIMATE                                     
         BE    BUY75                                                            
         BL    *+14                                                             
         MVC   KEY+9(4),=4X'FF'                                                 
         B     BUY25                                                            
*                                                                               
         MVC   KEY+9(1),SAVBKEY+9  ESTIMATE                                     
         XC    KEY+10(3),KEY+10                                                 
         B     BUY25                                                            
*                                                                               
BUY65    ZIC   RE,KEY+9                                                         
         LA    RE,SAVESLST(RE)                                                  
         CLI   0(RE),0             TEST ESTIMATE ACTIVE                         
         BNE   BUY75               YES - PROCESS                                
         MVC   KEY+10(3),=3X'FF'   ELSE FORCE NEXT EST                          
         B     BUY25                                                            
*                                                                               
BUY75    MVI   DMINBTS,X'08'       SET TO PASS DELETES                          
         GOTO1 GETREC              GET BUY RECORD                               
         MVI   DMINBTS,0           RESET                                        
*                                                                               
         TM    BUYRCNTL,X'80'       DELETED?                                    
         BO    NEXTBUY                                                          
         MVC   HALF,BUYRLEN         RECORD LENGTH                               
         LH    RE,HALF                                                          
         LA    RE,BUYREC(RE)                                                    
         MVI   0(RE),0             LAST ELEMENT INDICATOR                       
*                                                                               
         LA    R3,BDELEM                                                        
         OC    SVID,SVID                                                        
         BZ    BUY77                                                            
         MVI   ELCDLO,X'70'                                                     
         MVI   ELCDHI,X'70'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   NEXTBUY                                                          
         CLI   0(R3),X'70'                                                      
         BNE   NEXTBUY                                                          
         CLC   SVID,9(R3)                                                       
         BE    BUY77                                                            
         CLI   SVB0PROF+9,C'O'     TEST OPTIONAL PURPOSE CODES                  
         JE    BUY77                                                            
         CLI   SVB0PROF+9,C'Y'     TEST PURPOSE CODES                           
         BNE   NEXTBUY                                                          
         CLC   SVID,3(R3)                                                       
         BNE   NEXTBUY                                                          
*                                                                               
BUY77    TM    SVOPTS,X'40'        TEST 'NO TAX' REQ                            
         BZ    *+10                NO                                           
         XC    BDNTAX,BDNTAX                                                    
*                                                                               
         CLC   KEY+4(2),BUYREC+4   TEST SPILL PTR (UNEQ MKTS)                   
         BE    BUY80               EQUAL - NOT SPILL                            
*                                                                               
         TM    SVOPTS,X'80'        TEST 'NO-SPILL' REQ                          
         BO    NEXTBUY                                                          
         CLC   MISMED+1(3),=C'-NS' TEST 'NO-SPILL' ON MEDIA FIELD               
         BE    NEXTBUY                                                          
*                                                                               
         XC    BDCOST,BDCOST                                                    
* RESET COST OVRDS                                                              
         MVI   ELCDLO,11                                                        
         MVI   ELCDHI,13                                                        
         LA    R3,BDELEM                                                        
*                                                                               
BUY78    BAS   RE,NEXTEL                                                        
         BNE   BUY80                                                            
*                                                                               
         LA    RE,=X'000000'       SET TO CLEAR ALL OF BDCOST                   
         TM    BDSTAT,X'80'        TEST POL NPW                                 
         BZ    *+8                 NO                                           
         LA    RE,=X'FC0000'       FIRST 6 BITS ARE SPOTS                       
         NC    7(3,R3),0(RE)       SET OVRD AMT = 0                             
         B     BUY78                                                            
*                                                                               
BUY80    DS    0H                                                               
         LA    R1,BDDAYPT          DAYPART                                      
         BAS   RE,GETDPT           GET DAYPART CODE                             
*                                                                               
BUY200   XC    A50EL,A50EL                                                      
         MVI   ELCDLO,X'50'        GET A (NONT DEMO NAMES)                      
         MVI   ELCDHI,X'50'                                                     
         LA    R3,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         JNE   *+8                                                              
         ST    R3,A50EL                                                         
*                                                                               
         MVI   ELCDLO,2            FIND DEMO ELEMENT                            
         MVI   ELCDHI,2                                                         
         LA    R3,BDELEM                                                        
*                                                                               
         CLC   KEY+4(2),BUYREC+4   TEST SPILL PTR (UNEQ MKTS)                   
         BNE   BUY202                                                           
*                                                                               
         BAS   RE,NEXTEL                                                        
         BE    BUY210                                                           
         DC    H'0'                                                             
*                                                                               
BUY202   MVI   ELCDLO,3                                                         
         MVI   ELCDHI,3                                                         
*                                                                               
BUY204   BAS   RE,NEXTEL           FIND SPILL DEMO EL                           
         BNE   NEXTBUY             MUST NOT HAVE SPILL ANYMORE                  
         CLC   KEY+4(2),4(R3)      TEST RIGHT MKT                               
         BNE   BUY204                                                           
*                                                                               
BUY210   DS    0H                                                               
         XC    WORK2,WORK2                                                      
         CLI   1(R3),24            TEST NO DEMOS IN ELEM                        
         BE    BUY260                                                           
         SPACE 2                                                                
BUY250   LA    R5,SVDEMLST         ESTHDR DEMOS                                 
         LA    R4,WORK2                                                         
         LA    R8,4                BCT                                          
*                                                                               
BUY252   MVC   FULL,0(R5)          DEMO CODE                                    
         BAS   RE,GETDEMO          FIND DEMO                                    
*                                                                               
         LA    R5,3(R5)            NEXT DEMO                                    
         LA    R4,4(R4)                                                         
         BCT   R8,BUY252           LOOP TO GET DEMO DATA                        
*                                                                               
BUY260   XC    WORK,WORK                                                        
         LA    R2,WORK             BUCKET DSECT                                 
         USING MISBUCKS,R2                                                      
*                                                                               
         MVI   BUCCODE,1           ELEM CODE                                    
         MVI   BUCELLEN,48         ELEMENT LENGTH                               
         MVC   BUCDPTCD,BYTE4      DAYPART                                      
*                                                                               
         XC    WORK3,WORK3         CLEAR PRD/SLN TABLE                          
         XC    WORK3A,WORK3A                                                    
         MVC   WORK3(1),SAVBKEY+3  PRD                                          
         MVC   WORK3+1(1),BDSEC    LENGTH                                       
*                                                                               
         CLI   SAVBKEY+3,X'FF'     POOL REQUESTED?                              
         BE    BUY300                                                           
*                                                                               
         CLI   BDTIME,0            TEST P/B BUY                                 
         BE    BUY300              NO                                           
*                                                                               
         MVI   ELCDLO,4            FIND PBELEM                                  
         MVI   ELCDHI,4                                                         
         LA    R3,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   PIGCODE,0           WAS PIGGYBACKS REQUESTTED                    
         BE    BUY270                                                           
*                                                                               
         MVC   WORK3+1(1),4(R3)    SET PASSIVE LENGTH                           
         CLC   2(1,R3),SAVBKEY+3   TEST RIGHT PRODUCT                           
         BE    *+10                YES                                          
         MVC   WORK3+1(1),BDTIME   ELSE SET ACTIVE LENGTH                       
         B     BUY340                                                           
*                                                                               
BUY270   MVC   WORK+2(1),PIGCODE                                                
         CLC   2(1,R3),PIGCODE     TEST PASSIVE PRODUCT                         
         BE    BUY340                                                           
         B     NEXTBUY                                                          
         EJECT                                                                  
* FOR POL BUYS BUILD A PRD/SLN LIST IN WORK3 *                                  
         SPACE 1                                                                
BUY300   CLI   BUYKEY+3,X'FF'      TEST POL BUY                                 
         BNE   BUY340              NO - GO PROCESS                              
*                                                                               
         XC    WORK3,WORK3         CLEAR TABLE                                  
         XC    WORK3A,WORK3A                                                    
         MVI   ELCDLO,11                                                        
         MVI   ELCDHI,13                                                        
         LA    R3,BDELEM           1ST ELEM                                     
         CLC   MISSTA(4),=C'LIST'  TEST STATION LIST                            
         BNE   BUY310              NO                                           
*                                                                               
BUY306   BAS   RE,NEXTEL           SEE IF ANY ELEMENTS                          
         BNE   BUY308              NO                                           
         TM    6(R3),X'C0'         TEST MINUS OR MINUSSED                       
         BNZ   BUY306              YES - IGNORE                                 
         LA    R3,BDELEM           GOT AN ELEM - DO NORMAL PROCESS              
         B     BUY310                                                           
* IF NO ELEMENTS OR ALL -, FAKE ONE ENTRY IN TABLE                              
BUY308   MVC   BUCPRD,BUYKEY+6     STATION                                      
         MVC   BUCLEN,BDSEC        SPOTLEN                                      
         XC    BUCPRD2,BUCPRD2     CLEAR PIGGYBACK PROD FIELD                   
         MVC   BUCPSPTS,=X'80000000'  SET FLAG FOR POST                         
         BAS   RE,POST                                                          
         B     NEXTBUY                                                          
*                                                                               
BUY310   BAS   RE,NEXTEL                                                        
         BNE   BUY340                                                           
*                                                                               
BUY312   CLC   2(2,R3),SAVPERDT    TEST IN REQUESTED PERIOD                     
         BL    BUY310                                                           
         CLC   2(2,R3),SAVPERDT+2                                               
         BH    BUY310                                                           
*                                                                               
         CLI   1(R3),10            TEST ALLOCATED                               
         BH    BUY315                                                           
         CLI   SAVBKEY+3,X'FF'     TEST POL REQUEST                             
         BNE   BUY310              NO - IGNORE UNALLOCATED                      
*                                                                               
         MVI   HALF,X'FE'          BUILD ENTRY FOR UNALL                        
         MVC   HALF+1(1),BDSEC                                                  
*                                                                               
         LA    R5,HALF             POINT TO DATA                                
         BAS   RE,SETPSL                                                        
         B     BUY310                                                           
*                                                                               
BUY315   ZIC   R6,1(R3)            GET ELEM LENGTH                              
         SHI   R6,10                                                            
         SRL   R6,2                SET FOR BCT                                  
         LA    R5,10(R3)                                                        
*                                                                               
         CLI   SAVBKEY+3,X'FF'     TEST ALL PRODUCT REQUEST                     
         BE    BUY318                                                           
         CLI   PIGCODE,0           WERE PIGGYBACKS REQUESTED                    
         BE    BUY320                                                           
BUY318   BAS   RE,PIGCHECK         CHECK IF PIGGYBACK BUY QUALIFIES             
         BZ    BUY310                                                           
*                                                                               
BUY320   MVC   HALF,0(R5)          MOVE PRODUCT/LEN                             
*                                                                               
         TM    SVOPT2,SVOPT2_TRD   TEST MERGE CASH/TRADE PRDS                   
         BZ    *+8                                                              
         NI    HALF,X'7F'                                                       
*                                                                               
         CLI   SAVBKEY+3,X'FF'     TEST ALL PRODUCT REQUEST                     
         BE    BUY324                                                           
*                                                                               
         CLC   HALF(1),SAVBKEY+3   TEST RIGHT PRODUCT                           
         BNE   BUY325              NO - SKIP                                    
*                                                                               
BUY324   BAS   RE,SETPSL           SET ENTRY IN TABLE                           
*                                                                               
BUY325   LA    R5,4(R5)            NEXT PRD                                     
         BCT   R6,BUY320                                                        
         B     BUY310                                                           
         EJECT                                                                  
* SUBROUTINE TO TEST IF ENTRY AT 0(R5) IS PRESENT IN TABLE *                    
         SPACE 1                                                                
SETPSL   LA    R8,WORK3            TABLE                                        
*                                                                               
SETPSL2  CLI   0(R8),0             TEST E-O-L                                   
         BE    SETPSL4                                                          
         CLI   2(R8),0             IGNORE P/B ENTRIES                           
         BNE   SETPSL3                                                          
         CLC   0(2,R8),HALF        SAME PRD-SLN?                                
         BER   RE                                                               
SETPSL3  LA    R8,4(R8)                                                         
         B     SETPSL2                                                          
*                                                                               
SETPSL4  MVC   0(2,R8),HALF        SAVE PRD-SLN IN LIST                         
         XC    2(2,R8),2(R8)       ZERO UNUSED AREA                             
         BR    RE                                                               
         EJECT                                                                  
* PROCESS PRD/SLN TABLE *                                                       
         SPACE 1                                                                
BUY340   LA    R8,WORK3            PRD-SLN TABLE                                
         CLI   0(R8),0             TEST NO ENTRIES                              
         BE    NEXTBUY                                                          
*                                                                               
BUY342   LA    RE,BUYKEY+6         POINT TO STATION                             
         CLC   MISSTA(4),=C'LIST'                                               
         BE    BUY350                                                           
*                                                                               
         LA    RE,=C'ZZZ'          POINT TO ZZZ                                 
         CLI   0(R8),X'FD'         TEST FOR UNALL OR POL                        
         BH    BUY350                                                           
*                                                                               
         CLI   WEEKIND,C'W'        TEST WEEKS                                   
         BNE   *+12                                                             
         CLI   SAVBKEY+3,X'FF'     TEST POL REQUEST                             
         BE    BUY350              IF POL AND WEEKS POST TO ZZZ                 
*                                                                               
         LA    RE,CPRDLIST         CLTHDR PRD LIST                              
*                                                                               
BUY344   CLC   3(1,RE),0(R8)       FIND PRODUCT CODE                            
         BE    BUY350                                                           
         LA    RE,4(RE)                                                         
         CLI   0(RE),C' '                                                       
         BH    BUY344                                                           
         LA    RE,=C'***'                                                       
*                                                                               
BUY350   MVC   BUCPRD,0(RE)        SET PRD IN KEY                               
         MVC   BUCLEN,1(R8)        AND USE LENGTH FROM THERE                    
         XC    BUCPRD2,BUCPRD2     CLEAR PIGGYBACK PROD FIELD                   
*                                                                               
         CLC   MISSTA(4),=C'LIST'  NO P/B BUCKETS FOR STA LIST                  
         BE    BUY354                                                           
*                                                                               
         CLI   2(R8),0                                                          
         BE    BUY354                                                           
         LA    RE,CPRDLIST         CLTHDR PRD LIST                              
*                                                                               
BUY352   CLC   3(1,RE),2(R8)       FIND PRODUCT CODE FOR PIGGYBACK              
         BE    BUY353                                                           
         LA    RE,4(RE)                                                         
         CLI   0(RE),C' '                                                       
         BH    BUY352                                                           
         DC    H'0'                                                             
*                                                                               
BUY353   MVC   BUCPRD2,0(RE)       SET PRD IN KEY                               
         CLC   BUCPRD,BUCPRD2                                                   
         BL    BUY354                                                           
* SWAP THE FIELDS                                                               
         XC    BUCPRD,BUCPRD2                                                   
         XC    BUCPRD2,BUCPRD                                                   
         XC    BUCPRD,BUCPRD2                                                   
*                                                                               
BUY354   MVI   ELCDLO,6            SCAN FOR DATE ELEMENTS                       
         MVI   ELCDHI,13                                                        
         LA    R3,BDELEM           1ST ELEM                                     
*                                                                               
BUY355   BAS   RE,NEXTEL                                                        
         BNE   BUY420                                                           
*                                                                               
BUY357   CLC   2(2,R3),SAVPERDT    PRIOR TO START DATE                          
         BL    BUY355                                                           
*                                                                               
         CLC   2(2,R3),SAVPERDT+2  AFTER END DATE                               
         BH    BUY355                                                           
*                                                                               
         CLI   0(R8),X'FE'         TEST PROCESSING UNALL                        
         BNE   BUY360                                                           
         CLI   1(R3),10            TEST UNALLOCATED                             
         BE    BUY365              YES - PROCESS                                
         B     BUY355              ELSE IGNORE                                  
*                                                                               
BUY360   CLI   0(R3),11            TEST POL ELEM                                
         BL    *+12                NO                                           
         CLI   1(R3),10            IF NOT PROCESSING UNALL,                     
         BNH   BUY355                IGNORE UNALL SPOTS                         
*                                                                               
BUY365   CLI   WEEKIND,C'W'        WEEKS?                                       
         BNE   BUY380                                                           
*                                                                               
         OC    BUCDATE,BUCDATE     FIRST TIME?                                  
         BZ    BUY380                                                           
         CLC   BUCDATE,2(R3)       NEW WEEK?                                    
         BE    BUY380                                                           
*                                                                               
         BAS   RE,POST             GO ADD ELEM TO BUFFER                        
*                                                                               
BUY380   MVC   BUCDATE,2(R3)       ELEM DATE                                    
         XC    WORK2+32(16),WORK2+32                                            
         SPACE 1                                                                
* GET SPOTS AND DOLLARS *                                                       
         SPACE 1                                                                
         CLI   2(R8),0             PIGGYBACK SPOT?                              
         BE    BUY381              NO                                           
         BAS   RE,TSTPIG           CHECK RIGHT PRD ALLOCATION                   
         BNE   BUY355                                                           
         MVC   HALF(1),BUYKPRD                                                  
         MVC   HALF+1(1),BDSEC                                                  
         B     BUY382                                                           
*                                                                               
BUY381   MVC   HALF(2),0(R8)       MOVE PRD/LEN                                 
*                                                                               
BUY382   CLI   HALF,X'FE'                                                       
         BNE   *+8                                                              
         MVI   HALF,X'FF'          FORCE UNALL TO PRD POL                       
*                                                                               
         TM    SVOPTS,SVOPT$2                                                   
         BZ    *+10                                                             
         MVC   WORK2+32(4),=C'COS2'  SLIGHTLY BIZARRE REQUEST METHOD            
*                                                                               
         CLI   SVAGPRF7,C'C'                                                    
         BNE   BUY384              IF USA, NO EXCHG ELEM.                       
         MVI   BYTE,C'C'           DEFAULT CANADA                               
         MVI   BDPURP,X'FD'        GET CANADIAN NETWORK COSTS                   
         TM    SVOPTS,SVOPTUSA     SEE IF OPTION IS $U                          
         BZ    *+8                                                              
         MVI   BYTE,C'U'                                                        
*                                                                               
         GOTO1 VGETRATE,DMCB,(HALF,WORK2+32),(HALF+1,BUYREC),          X        
               (C'X',(R3)),(BYTE,EXCHAREA)                                      
         CLI   DMCB+12,X'FF'       CHECK ERROR RETURN                           
         BNE   BUY385                                                           
         FOUT  MISMSGH,NOXCHMSG,60                                              
         OI    MISOPTH+6,X'40'     PUT CURSOR TO THIS FIELD                     
         MVI   ERRAREA,X'FF'       ERROR IND                                    
         B     MISERR                                                           
NOXCHMSG DC    CL60'NO EXCHANGE DATA PRESENT'                                   
*                                                                               
BUY384   DS    0H                                                               
         TM    SVOPT2,SVOPT2_TRD   TEST MERGE CASH/TRADE PRDS                   
         BZ    BUY384X                                                          
         MVI   BDPURP,X'FE'        SET OPTION FOR GETRATE                       
BUY384X  GOTO1 VGETRATE,DMCB,(HALF,WORK2+32),(HALF+1,BUYREC),(R3)               
*                                                                               
BUY385   TM    SVOPTS,SVOPTPW                                                   
         BNO   BUY385A                                                          
* ADJUST BUY DOLLARS                                                            
         LA    R7,PWWORK                                                        
         USING PWBLKD,R7                                                        
         XC    PWWORK,PWWORK                                                    
*                                                                               
         MVI   PWACT,PWGETBUY                                                   
         TM    SVOPTS,SVOPTNTX     TEST SUPPRESS TAX                            
         BZ    *+8                                                              
         OI    PWFLAG,PWFLAG_NOTAX                                              
         MVC   PWACTBUY,WORK2+36   BUY DOLLARS                                  
         SR    R0,R0                                                            
         ICM   R0,7,SVEPWPCT                                                    
         ST    R0,PWPCT                                                         
         BAS   RE,GETPWPCT                                                      
         BNE   *+10                                                             
         MVC   PWPCT,FULL                                                       
         MVC   PWTAX,WORK2+44      MOVE TAX DOLLARS STUPID !!                   
         GOTO1 VPWCALC,DMCB,(R7)                                                
         CLI   PWERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   WORK2+36(4),PWVAL    MOVE ADJUST BUYS                            
         DROP  R7                                                               
*                                                                               
BUY385A  LM    R5,R6,WORK2+32      SPOTS AND DOLLARS                            
         TM    SVOPT2,SVOPT2_NET                                                
         BZ    *+8                                                              
         L     R6,WORK2+40         NET DOLLARS                                  
*                                                                               
         A     R5,BUCPSPTS         SPOTS                                        
         ST    R5,BUCPSPTS                                                      
         A     R6,BUCPDOLS         DOLLARS                                      
         ST    R6,BUCPDOLS                                                      
*                                                                               
         LA    R5,WORK2                                                         
         LA    RE,4                                                             
         LA    R7,BUCPPTS          PURCHASED POINTS                             
*                                                                               
BUY386   L     R6,0(R5)            DEMO                                         
         N     R6,=X'3FFFFFFF'     DROP 2-DEC FLAG                              
         MH    R6,WORK2+34         DEMO X SPOTS                                 
         L     R0,0(R7)                                                         
         N     R0,=X'3FFFFFFF'     DROP 2-DEC FLAG                              
         AR    R6,R0                                                            
         TM    0(R5),X'40'         TEST 2-DEC FLAG                              
         BZ    *+8                                                              
         O     R6,=X'40000000'     SET 2 -DEC FLAG IN RESULT                    
         ST    R6,0(R7)                                                         
*                                                                               
         LA    R5,4(R5)                                                         
         LA    R7,4(R7)                                                         
         BCT   RE,BUY386                                                        
*                                                                               
         B     BUY355                                                           
         SPACE 1                                                                
* END OF BUY RECORD *                                                           
         SPACE 1                                                                
BUY420   BAS   RE,POST             ADD TO TOTALS                                
*                                                                               
         LA    R8,4(R8)            NEXT PRD-SLN                                 
         CLI   0(R8),0             TEST EOL                                     
         BE    NEXTBUY                                                          
         B     BUY342                                                           
         SPACE 1                                                                
* CHECK PIGGYBACK ALLOCATIONS *                                                 
         SPACE 1                                                                
TSTPIG   CLI   1(R3),18            TEST P/B SPOT                                
         BNER  RE                                                               
         CLC   10(1,R3),0(R8)      MATCH PRD 1                                  
         BNE   TSTPIG2                                                          
         CLC   14(1,R3),2(R8)      MATCH PRD 2                                  
         BR    RE                  EXIT WITH CC SET                             
*                                                                               
TSTPIG2  CLC   10(1,R3),2(R8)      MATCH PRD 2                                  
         BNER  RE                                                               
         CLC   14(1,R3),0(R8)      MATCH PRD 1                                  
         BR    RE                  EXIT WITH CC SET                             
         EJECT                                                                  
* PROCESS LOCKIN RECORDS *                                                      
         SPACE 1                                                                
LOCK     NTR1                                                                   
         XC    KEY,KEY                                                          
K        USING SLKRECD,KEY                                                      
*                                                                               
         MVI   K.SLKKTYP,SLKKTYPQ                                               
         MVI   K.SLKKSUB,SLKKSUBQ                                               
         MVC   K.SLKKAGMD(3),SAVBKEY        A-M/CLT                             
         MVC   K.SLKKMKT(5),SAVBKEY+4       MKT/STA                             
*                                                                               
         CLC   K.SLKKSTA,=3X'FF'            LIST STATIONS?                      
         BNE   *+10                                                             
         XC    K.SLKKSTA,K.SLKKSTA                                              
*                                                                               
         GOTO1 XSPHIGH                                                          
*                                                                               
LOCK10   CLC   KEY(SLKKSTA-SLKKEY),KEYSAVE  TEST SAME MARKET                    
         BNE   LOCKX                                                            
*                                                                               
         OC    SAVBKEY+6(3),SAVBKEY+6       TEST ALL STATIONS                   
         BZ    LOCK20                                                           
         CLC   SAVBKEY+6(3),=3X'FF'         TEST LIST STATIONS                  
         BE    LOCK20                                                           
* NOT LIST OR ALL - TEST RIGHT STATION                                          
         CLC   K.SLKKSTA,KEYSAVE+(SLKKSTA-SLKKEY)                               
         BNE   LOCKX                                                            
*                                                                               
LOCK20   CLI   SAVBKEY+3,X'FF'                                                  
         BE    LOCK22                                                           
         MVC   K.SLKKPRD,SAVBKEY+3                                              
         MVC   K.SLKKPRD2,PIGCODE                                               
*                                                                               
LOCK22   XC    K.SLKKEST(5),K.SLKKEST                                           
         MVC   K.SLKKEST,SAVBKEY+9                                              
*                                                                               
LOCK30   GOTO1 XSPHIGH                      STATION NOW IN KEYSAVE              
         B     LOCK50                                                           
*                                                                               
LOCK40   GOTO1 XSPSEQ                                                           
*                                                                               
LOCK50   CLC   KEY(SLKKPRD-SLKKEY),KEYSAVE  TEST SAME STATION                   
         BNE   LOCK10                                                           
*                                                                               
LOCK70   CLI   SAVBKEY+3,X'FF'       ALL PRODUCTS?                              
         BE    LOCK80                                                           
         CLC   K.SLKKPRD,SAVBKEY+3   PRD(1)                                     
         BE    LOCK80                                                           
         MVC   K.SLKKPRD+1(6),=6X'FF'                                           
         B     LOCK30                                                           
*                                                                               
*                                                                               
LOCK80   CLI   SAVBKEY+9,0           ESTIMATE REQUESTED?                        
         BE    LOCK90                NO                                         
         CLC   K.SLKKEST,SAVBKEY+9   TEST RIGHT EST                             
         BE    LOCK400                                                          
         B     LOCK40                                                           
*                                                                               
LOCK90   SR    RE,RE                                                            
         IC    RE,K.SLKKEST                                                     
         LA    RE,SAVESLST(RE)                                                  
         CLI   0(RE),0                  TEST EST ACTIVE                         
         BNE   LOCK400                  YES                                     
         MVC   K.SLKKEST+1(4),=4X'FF'   ELSE NEXT ESTIMATE                      
         B     LOCK30                                                           
*                                                                               
LOCK400  LA    R2,WORK             BUCKET                                       
         XC    WORK,WORK                                                        
         MVI   BUCCODE,1           ELEMENT CODE                                 
         MVI   BUCELLEN,48         ELEMENT LENGTH                               
* GET ALPHA PRODUCT *                                                           
         LA    RE,CPRDLIST         CLTHDR PRD LIST                              
LOCK110  CLI   0(RE),0                                                          
         BE    LOCK40              IGNORE - PRD MUST HAVE BEEN DELETED          
*                                                                               
         CLC   3(1,RE),K.SLKKPRD   PRD                                          
         BE    LOCK120                                                          
         LA    RE,4(RE)                                                         
         B     LOCK110                                                          
*                                                                               
LOCK120  MVC   BUCPRD,0(RE)                                                     
         SR    RE,RE                                                            
         IC    RE,K.SLKKLEN                                                     
         AR    RE,RE                                                            
         A     RE,VSLNTAB                                                       
         MVC   BUCLEN,1(RE)        USE LEN FROM SLNTAB                          
*                                                                               
         LA    R1,K.SLKKDPT                                                     
         BAS   RE,GETDPT                                                        
         MVC   BUCDPTCD,BYTE4      DAYPART CODE                                 
* GET ALPHA PIGGYBACK PRODUCT *                                                 
         SPACE 1                                                                
         CLI   SAVBKEY+3,X'FF'     ALL PRODUCTS?                                
         BE    LOCK160                                                          
         CLC   SAVBKEY+6(3),=3X'FF'   STATION = LIST                            
         BE    LOCK160                IF SO, NO PIGGYBACKS                      
         CLI   PIGCODE,0                                                        
         BE    LOCK160                                                          
         CLI   K.SLKKPRD2,0                                                     
         BE    LOCK160                                                          
*                                                                               
LOCK130  LA    RE,CPRDLIST         CLTHDR PRD LIST                              
*                                                                               
LOCK140  CLI   0(RE),0                                                          
         BE    LOCK40              IGNORE - PRD MUST HAVE BEEN DELETED          
*                                                                               
         CLC   3(1,RE),K.SLKKPRD2  PIGGYBACK PRODUCT                            
         BE    LOCK150                                                          
         LA    RE,4(RE)                                                         
         B     LOCK140                                                          
*                                                                               
LOCK150  MVC   BUCPRD2,0(RE)                                                    
         SR    RE,RE                                                            
         IC    RE,K.SLKKLEN2       SLN                                          
         AR    RE,RE                                                            
         A     RE,VSLNTAB                                                       
         MVC   BUCLEN,1(RE)        USE SLN TABLE SAYS TO USE                    
         CLC   BUCPRD,BUCPRD2                                                   
         BL    LOCK160                                                          
* SWAP THE FIELDS                                                               
         XC    BUCPRD,BUCPRD2                                                   
         XC    BUCPRD2,BUCPRD                                                   
         XC    BUCPRD,BUCPRD2                                                   
*                                                                               
         DROP  K                                                                
         EJECT                                                                  
LOCK160  CLC   SAVBKEY+6(3),=3X'FF'   STATION = LIST                            
         BNE   *+10                                                             
         MVC   BUCPRD,KEY+SLKKSTA-SLKKEY  PUT STATION IN PRD FIELD              
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'GETREC',=C'XSPFILE',KEY+36,IOAREA,     X        
               DMWORK                                                           
*                                                                               
         LA    R4,IOAREA                                                        
         USING SLKRECD,R4                                                       
         LA    R3,SLKFSTEL                                                      
         DROP  R4                                                               
*                                                                               
         MVI   ELCDLO,LOKELCDQ     WEEKLY LOCKIN ELEMENT                        
         MVI   ELCDHI,LOKELCDQ                                                  
*                                                                               
LOCK170  BAS   RE,NEXTEL                                                        
         BNE   LOCK40                                                           
         USING LOKEL,R3                                                         
*                                                                               
         CLC   LOKWEEK,MONDATES    COMPARE W/ REQ START DATE                    
         BL    LOCK170                                                          
         CLC   LOKWEEK,SAVPERDT+2  COMPARE W/ REQ END DATE                      
         BH    LOCK170                                                          
         MVC   BUCDATE,LOKWEEK                                                  
*                                                                               
LOCK172  MVC   BUCLSPTS+2(2),LOKSPOTS                                           
         MVC   BUCGDOLS,LOKDOLS                                                 
         MVC   BUCGPTS,LOKDEM                                                   
*                                                                               
LOCK174  CLI   WEEKIND,C'W'        WEEKS REQUESTED?                             
         BNE   LOCK180                                                          
         CLI   SAVGKEY+4,X'FF'     POL?                                         
         BNE   LOCK180                                                          
         MVI   BYTE,X'FF'                                                       
         MVC   BUCPRD,=C'POL'                                                   
*                                                                               
LOCK180  BAS   RE,FINDBUC                                                       
*                                                                               
         CLI   BYTE,X'FF'                                                       
         BE    LOCK170             BUFFER TOO SMALL FOR ALL BRAND'S             
*                                  WEEKS-DPT-SLN COMBOS                         
         CLC   SAVBKEY+6(3),=3X'FF'   STATION = LIST                            
         BE    LOCK170                                                          
*                                  WEEKS-DPT-SLN COMBOS                         
         CLI   SAVBKEY+3,X'FF'     POOL?                                        
         BNE   LOCK170                                                          
         MVC   SVEPRODS(3),BUCPRD                                               
         MVC   SVEPRODS+3(3),BUCPRD2                                            
         MVC   BUCPRD,=C'ZZZ'    FOR SORTING                                    
         XC    BUCPRD2,BUCPRD2                                                  
         BAS   RE,FINDBUC                                                       
         MVC   BUCPRD,SVEPRODS     RESTORE PRD                                  
         MVC   BUCPRD2,SVEPRODS+3  RESTORE PRD                                  
         B     LOCK170             GET NEXT LOCKIN ELEMENT                      
         DROP  R3                                                               
LOCKX    B     EXIT                                                             
*                                                                               
XSPHIGH  MVC   KEYSAVE,KEY                                                      
         LA    RF,=C'DMRDHI'                                                    
         B     XSPDIR                                                           
*                                                                               
XSPSEQ   LA    RF,=C'DMRSEQ'                                                    
*                                                                               
XSPDIR   LR    R0,RE                                                            
         GOTO1 VDATAMGR,DMCB,(RF),=C'XSPDIR',KEYSAVE,KEY                        
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
* ROUND BUCKETS AND ADD TO TOTALS *                                             
         SPACE 1                                                                
POST     NTR1                                                                   
         OC    BUCPSPTS(32),BUCPSPTS                                            
         BZ    POSTX                                                            
*                                                                               
         SR    RE,RE                                                            
         IC    RE,BUCLEN           GET SLN                                      
         AR    RE,RE               X 2                                          
         A     RE,VSLNTAB                                                       
         MVC   BUCLEN,1(RE)        USE REPORTING SLN FROM TABLE                 
*                                                                               
POST1    CLC   BUCPSPTS,=X'80000000' THIS FLAG FOR STATION LIST                 
         BNE   POST2                                                            
         XC    BUCPSPTS,BUCPSPTS     SO CLEAR IT NOW                            
         B     POST6                                                            
*                                                                               
POST2    LA    RE,4                BCT                                          
         LA    R6,BUCPPTS          PURCHASED POINTS                             
*                                                                               
POST6    BAS   RE,FINDBUC          ADD TO BUCKET IN BUFFER                      
*                                                                               
         CLC   MISSTA(4),=C'LIST'                                               
         BE    POSTX                                                            
         CLC   BUCPRD,=C'ZZZ'      TEST POSTING TO POL                          
         BE    POSTX                                                            
         CLI   SAVBKEY+3,X'FF'     TEST DOING ALL PRODUCTS                      
         BNE   POSTX                                                            
         SPACE 1                                                                
* POST TO POL NOW *                                                             
         SPACE 1                                                                
         MVC   BUCPRD,=C'ZZZ'                                                   
         XC    BUCPRD2,BUCPRD2                                                  
         BAS   RE,FINDBUC                                                       
*                                                                               
POSTX    XC    BUCPSPTS(32),BUCPSPTS                                            
         B     EXIT                                                             
         EJECT                                                                  
NEXTEL   SR    R0,R0                                                            
         CLI   0(R3),0                                                          
         BE    NEXTELX                                                          
         ICM   R0,1,1(R3)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R3,R0                                                            
         CLC   0(1,R3),ELCDLO                                                   
         BL    NEXTEL                                                           
         CLC   0(1,R3),ELCDHI                                                   
         BH    NEXTEL                                                           
         CR    RE,RE               SET CC EQUAL                                 
         BR    RE                                                               
NEXTELX  LTR   RE,RE                                                            
         BR    RE                  SET CC NOT EQ                                
         EJECT                                                                  
* SUBROUTINE TO FIND AND ADD TO BUCKET IN MIS BUFFER                            
* IF BUCKET IS FOUND THEN COUNTERS ARE ADDED                                    
* IF NOT, THEN BUCKET IS ADDED VIA RECUP (SIMULATED BUY RECORD)                 
*                                                                               
FINDBUC  NTR1                                                                   
         USING MISBUCKS,R2         R2 POINTS TO NEW BUCKET ON ENTRY             
*                                                                               
         CLC   BUCPRD,=C'POL'                                                   
         BNE   *+10                                                             
         MVC   BUCPRD,=C'ZZZ'      FOR DISPLAY - SORT                           
         SR    R4,R4                                                            
         L     R3,ABUCKETS                                                      
         CLI   WEEKIND,C'W'        WEEKS REQUESTED?                             
         BE    *+14                                                             
         XC    BUCDATE,BUCDATE                                                  
         B     FIND100                                                          
         SPACE 1                                                                
* WEEKS REQUESTED - FIND MONDAY DATE                                            
*                                                                               
         CLI   SVEDAILY,C'Y'       TEST DAILY ESTIMATE                          
         BE    FIND100             YES - LEAVE DATE ALONE                       
*                                                                               
         GOTO1 VDATCON,DMCB,(2,BUCDATE),DUB  GET 6 BYTE DATE                    
*                                                                               
         BAS   RE,GETMON           GET MONDAY DATE                              
*                                                                               
         GOTO1 VDATCON,DMCB,DUB,(2,BUCDATE)  CONVERT BACK TO 2 BYTES            
         SPACE 1                                                                
* SEARCH THRU BUCKETS *                                                         
* SEQUENCE IS IN PROD-PB PROD-DATE-DAYPART-LENGTH                               
         SPACE 1                                                                
FIND100  CLI   0(R3),0             LAST BUCKET?                                 
         BE    NEWBUC                                                           
         CLC   BUCPRD(3),2(R3)       CHECK PRODUCT CODE                         
         BL    NEWBUC                                                           
         BH    BMPBUC                                                           
         CLC   BUCPRD2(3),9(R3)      CHECK PIGGYBACK PRODUCT CODE               
         BL    NEWBUC                                                           
         BH    BMPBUC                                                           
         CLC   BUCDATE(4),5(R3)      CHECK DATE-DAYPART-LENGTH                  
         BL    NEWBUC                                                           
         BE    ADDBUC                                                           
*                                                                               
BMPBUC   IC    R4,1(R3)            BUCKET LENGTH                                
         LA    R3,0(R4,R3)                                                      
         B     FIND100             NEXT BUCKET                                  
         SPACE 1                                                                
* ADD TO EXISTING BUCKET                                                        
         SPACE 1                                                                
ADDBUC   LA    R8,9                BCT                                          
         LA    R4,12(R3)           BUCPSPTS                                     
         LA    R5,BUCPSPTS                                                      
*                                                                               
ADDBUC2  TM    0(R4),X'80'         TEST NEGATIVE VALUE                          
         BO    ADDBUC4                                                          
         TM    0(R5),X'80'                                                      
         BO    ADDBUC4                                                          
         B     ADDBUC6                                                          
*                                                                               
ADDBUC4  L     RE,0(R4)                                                         
         A     RE,0(R5)                                                         
         ST    RE,0(R4)                                                         
         B     ADDBUC8                                                          
*                                                                               
ADDBUC6  L     RE,0(R4)                                                         
         N     RE,=X'3FFFFFFF'     DROP 2-DEC FLAG                              
         L     RF,0(R5)                                                         
         N     RF,=X'3FFFFFFF'     DROP 2-DEC FLAG                              
         AR    RE,RF                                                            
         TM    0(R4),X'40'         TEST 2-DEC FLAG                              
         BZ    *+8                                                              
         O     RE,=X'40000000'                                                  
         TM    0(R5),X'40'         TEST 2-DEC FLAG                              
         BZ    *+8                                                              
         O     RE,=X'40000000'                                                  
         ST    RE,0(R4)                                                         
*                                                                               
ADDBUC8  LA    R4,4(R4)                                                         
         LA    R5,4(R5)                                                         
         BCT   R8,ADDBUC2                                                       
         B     EXIT                                                             
         SPACE 1                                                                
* CREATE NEW BUCKET *                                                           
         SPACE 1                                                                
NEWBUC   L     RE,ABUCKLEN                                                      
         LH    R0,0(RE)            BUFFER LEN                                   
         AR    RE,R0               BUFFER END                                   
         SH    RE,=H'2'                                                         
         SR    RF,RF                                                            
         IC    RF,BUCELLEN         ELEMENT LENGTH                               
         AR    RE,RF                                                            
         L     RF,AMISTWA2                                                      
         AH    RF,=Y(MISTWA2X-MISTWA2) SAVE AREA LENGTH                         
         CR    RE,RF                                                            
         BH    NOCORE                                                           
*                                                                               
         GOTO1 VRECUP,DMCB,(X'FF',ABUCKLEN),(R2),(R3)                           
*                                                                               
         B     EXIT                                                             
*                                                                               
NOCORE   FOUT  MISMSGH,FULLMSG,60                                               
         XC    SAVBKEY2,SAVBKEY2   OLD BUY KEY                                  
         MVI   ERRAREA,X'FF'       ERROR IND                                    
         B     MISERR                                                           
*                                                                               
FULLMSG  DC    CL60'PRD-DPT-SLN COMBO MAXIMUM EXCEEDED.  REQUEST OFF-LIX        
               NE REPORT'                                                       
         EJECT                                                                  
*======================================================*                        
* SUBROUTINE BACKS DATE UP TO MONDAY OR START DAY OF   *                        
* ESTIMATE IF OUT OF WEEK DATA FLAG IS ON              *                        
*======================================================*                        
         SPACE 1                                                                
GETMON   NTR1                                                                   
         GOTO1 VGETDAY,DMCB,DUB,FULL                                            
*                                                                               
         CLI   DMCB,0              VALID?                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R4,R4                                                            
         IC    R4,DMCB             DAY                                          
         SR    R5,R5                                                            
         ICM   R5,1,SVEOWSDY       GET OUT-OF-WEEK START DAY                    
         BNZ   *+8                                                              
         LA    R5,1                                                             
         SR    R5,R4                                                            
         BZ    GETMONEX            EXIT IF HAVE WEEK START DAY                  
         BM    *+8                                                              
         SH    R5,=H'7'            MAKE SURE WE ARE BACKING UP                  
*                                                                               
         GOTO1 VADDAY,DMCB,DUB,DMCB+12,(R5)  BACK UP TO START DAY               
*                                                                               
         MVC   DUB(6),DMCB+12                                                   
GETMONEX B     EXIT                                                             
         SPACE 2                                                                
* DAYPART MENU TRANSLATE ROUTINE                                                
*                                                                               
* R1=A(DAYPART IN GOALREC OR BUYREC)                                            
* ON EXIT DAYPART IS TRANSLATED TO MENU CODE IN BYTE4                           
         SPACE 1                                                                
GETDPT   NTR1                                                                   
         MVI   BYTE4,X'0F'         DEFAULT                                      
*                                                                               
         LA    R5,SAVMENU          DAYPART MENU                                 
         LA    R6,5                                                             
         LA    R7,SAVMENU+L'SAVMENU-1     BXLE LOOP                             
*                                                                               
GETDPT2  CLI   0(R5),0             LAST ENTRY?                                  
         BE    EXIT                                                             
         CLC   0(1,R5),0(R1)       SAME DAYPART CODES?                          
         BE    GETDPT4                                                          
         BXLE  R5,R6,GETDPT2                                                    
         B     EXIT                                                             
*                                                                               
GETDPT4  MVC   BYTE4,1(R5)         DAYPART CODE                                 
         B     EXIT                                                             
         EJECT                                                                  
*====================================================                           
* R3 POINTS TO DEMO ELEMENT ON ENTRY                                            
* R4 POINTS TO OUTPUT AREA                                                      
* FULL HAS 3 BYTE DEMO CODE                                                     
*====================================================                           
                                                                                
         USING GTDMWRKD,R8                                                      
GETDEMO  NTR1  WORK=(R8,GTDMWRKL)                                               
         TM    SVOPTS,SVOPTPD      WANT POST BUY INFO                           
         BZ    GETDEM1             NOPE                                         
*                                                                               
         LR    R2,R3               POINT R2 TO DEMO ELEMENT                     
         LLC   R0,1(R3)            LENGTH                                       
         AR    R2,R0                                                            
         CLI   0(R2),X'22'         POST BUY?                                    
         BNE   *+12                                                             
         LA    R2,2(R2)            POINT TO DEMOS IN X'22' ELEM                 
         B     GETDEM1                                                          
*                                                                               
         CLI   0(R2),X'23'         POST BUY?                                    
         BNE   EXIT                NO POST BUY INFO FOR THIS 02/03 ELEM         
         LA    R2,6(R2)            POINT TO DEMOS IN X'23' ELEM                 
*                                                                               
GETDEM1  LA    R1,24(R3)           POINT TO FIRST DEMO                          
         LLC   R0,1(R3)            DEMO ELEM LEN                                
         AHI   R0,-24                                                           
         SRL   R0,3                SET FOR BCT                                  
*                                                                               
GETDEM2  CLC   0(3,R1),FULL        MATCH ON DEMO CAT?                           
         BE    GETDEM4                                                          
         LA    R1,NDEMLNQ(R1)      NEXT DEMO IN ORIG DEMO ELEM                  
         TM    SVOPTS,SVOPTPD      POST BUY OPTION ON?                          
         BZ    *+8                 NO                                           
         LA    R2,L'PDEMO(R2)      NEXT DEMO IN POST BUY ELEM                   
         BCT   R0,GETDEM2                                                       
         B     EXIT                                                             
*                                                                               
GETDEM4  SR    R7,R7                                                            
         ICM   R7,7,5(R1)          DEMO VALUE FROM 02/03                        
*                                                                               
         TM    SVOPTS,SVOPTPD      POST BUY OPTION ON?                          
         BZ    GETDEM5             NO                                           
         ICM   R7,7,0(R2)          DEMO VALUE FROM 22/23                        
         N     R7,=X'007FFFFF'     DROP X'800000'                               
*                                                                               
GETDEM5  CLI   FULL+1,X'21'        TEST USER DEMO                               
         BNE   GETDEM5A                                                         
*                                                                               
         LLC   RE,FULL+2                                                        
         BCTR  RE,0                                                             
         MHI   RE,L'EUSRNML        X 7                                          
         A     RE,ASVUSRNMS                                                     
         MVC   GTDMDTYP,0(RE)      SAVE USERDEF DEMO CAT TYPE                   
         J     GETDEM5X                                                         
*                                                                               
GETDEM5A MVC   GTDMDTYP,1(R1)      SAVE NSI RATING TYPE                         
         CLI   FULL+2,0            TEST NONT DEMO                               
         BNE   GETDEM5X            NO                                           
*                                                                               
         LLC   RE,FULL+1           GET USER DEMO INDEX                          
         BCTR  RE,0                                                             
         MHI   RE,NTDDLEN          X 9                                          
         A     RE,A50EL            POINT TO NAME IN BUYREC                      
         LA    RE,NTDOVHDQ(RE)     RE=A(NTDELEM CS DEMO NAME)                   
*                                                                               
         LLC   RF,FULL+1                                                        
         BCTR  RF,0                                                             
         SLL   RF,3                X 8                                          
         A     RF,ASVNTDMS                                                      
*                                                                               
         CLC   0(7,RE),0(RF)       TEST NAMES MATCH                             
         JNE   EXIT                                                             
         MVC   GTDMDTYP,0(RE)      SAVE COMSCORE DEMO CAT TYPE                  
*                                                                               
* TEST RATING TYPE                                                              
*                                                                               
GETDEM5X CLI   GTDMDTYP,C'R'       TEST RATING DEMO CAT                         
         JE    GETDEMR                                                          
         CLI   GTDMDTYP,C'E'       OR EXTENDED DEMO CAT                         
         JE    GETDEMR                                                          
*                                                                               
* HAVE IMPRESSION                                                               
*                                                                               
GETDEMI  CLI   SVAGPRF7,C'C'       TEST CANADA                                  
         JE    GETDEM9              YES, DOES NOT SUPPORT 2-DEC IMPS            
         CLI   MISMED,C'R'         TEST MEDIA R (US)                            
         JE    GETDEM9              YES, DOES NOT SUPPORT 2-DEC IMPS            
*                                                                               
         CLI   SV00APRF6,C'Y'      TEST 00A PROFILE WANT 2 DEC IMPS?            
         JE    GETDEM8              YES                                         
         J     GETDEM9              NO                                          
*                                                                               
* HAVE RATING                                                                   
*                                                                               
GETDEMR  CLI   MISMED,C'R'         TEST MEDIA R                                 
         JE    GETDEM9              YES, DOES NOT SUPPORT 2 DEC RATINGS         
*                                                                               
         CLI   SVAGPRF7,C'C'       SKIP TEST FOR CANADA B/C MEDIA X             
         JE    GETDEMR1             IS USED TO CAPTURE PRODUCTION COST          
         CLI   MISMED,C'X'         TEST MEDIA X (US)                            
         JE    GETDEM8              YES, SUPPORTS 2-DEC (NO PROF REQ)           
*                                                                               
GETDEMR1 CLI   SVSPPRF9,C'Y'       TEST 00 PROFILE WANT 2-DEC RATING?           
         JNE   GETDEM9              NO                                          
*                                                                               
* WANT 2 DECIMAL PRECISION                                                      
*                                                                               
GETDEM8  MVI   GTDMPREC,C'2'       SET FLAG WANT 2-DEC RATING                   
         TM    SVOPTS,SVOPTPD      POST BUY OPTION ON?                          
         BO    *+12                YES - ALWAYS SCALE                           
         TM    4(R1),X'40'         TEST DEMO IN 2 DEC PREC                      
         BO    GETDEM30             YES                                         
         MHI   R7,10                NO, SCALE FROM 1 TO 2 DECIMALS              
         B     GETDEM30                                                         
*                                                                               
* WANT 1 DECIMAL PRECISION                                                      
*                                                                               
GETDEM9  MVI   GTDMPREC,C'2'       SET FLAG WANT 2-DEC RATING                   
         TM    SVOPT2,SVOPT2_2DEC  WANT 2-DEC?                                  
         JZ    GETDEM20            NO, WE WANT 1 DEC DEMO VALUES                
         TM    4(R1),X'40'         YES, DEMO VALUE ALSO A 2 DEC VALUE?          
         JNZ   GETDEM30                 YES, ADD TO THE TOTAL                   
         MHI   R7,10                           TO A 2 DEC VALUE                 
         J     GETDEM30                                                         
*                                                                               
GETDEM20 MVI   GTDMPREC,C'1'       SET DEMO PRECISION FLAG                      
         TM    4(R1),X'40'         TEST DEMO IN 2 DEC PREC                      
         BZ    GETDEM30             NO                                          
* SCALE FROM 2 TO 1 DECIMAL PRECISION                                           
         M     R6,=F'2'                                                         
         D     R6,=F'10'                                                        
         AHI   R7,1                                                             
         SRL   R7,1                                                             
*                                                                               
GETDEM30 CLI   3(R1),0             TEST SVI=0                                   
         BNE   *+8                                                              
         MVI   3(R1),100                                                        
*                                                                               
         SR    R6,R6                                                            
         ICM   R6,1,3(R1)          SVI                                          
*                                                                               
         AR    R6,R6               X 2                                          
         MR    R6,R6               DEMO X SVI                                   
         D     R6,=F'100'                                                       
         AHI   R7,1                                                             
         SRL   R7,1                                                             
*                                                                               
GETDEMX  NI    0(R4),X'3F'         DROP 2 DEC FLAG                              
         A     R7,0(R4)                                                         
         ST    R7,0(R4)            SET DEMO VALUE                               
         CLI   GTDMPREC,C'2'       TEST FLAG DEMO IS 2-DEC?                     
         BNE   EXIT                 NO                                          
         OI    0(R4),X'40'         SET 2 DEC FLAG                               
         B     EXIT                                                             
         EJECT                                                                  
         DROP  R8                                                               
*                                                                               
GTDMWRKD DSECT                                                                  
GTDMDTYP DS    C                   DEMOTYPE                                     
GTDMPREC DS    X                   DEMO PRECISION                               
GTDMWRKL EQU   *-GTDMWRKD                                                       
T20B01   CSECT                                                                  
*==============================================================                 
* GET PW PERCENT FROM SVPWPCT FOR THIS WEEK                                     
*        ON ENTRY R3 POINTS TO BUY WEEK ELEMENT                                 
*        RETURN % IN FULL                                                       
*==============================================================                 
                                                                                
GETPWPCT NTR1                                                                   
         XC    FULL,FULL                                                        
         OC    SVPWPCT,SVPWPCT     TEST NO WEEKLY PCTS                          
         BZ    NO                                                               
         LA    R4,SVPWPCT                                                       
*                                                                               
GP10     MVC   FULL,2(R4)             SET PERCENT                               
         CLC   2(2,R3),L'SVPWPCT(R4)  TEST CORRECT WEEK                         
         BL    YES                                                              
         LA    R4,L'SVPWPCT(R4)                                                 
         B     GP10                                                             
*                                                                               
GPNO     B     NO                                                               
GPYES    B     YES                                                              
         EJECT                                                                  
*                                                                               
* PIGCHECK SEES IF THE ELEMENT MATCHES THE REQUESTED PIGGYBACK                  
*                                                                               
PIGCHECK NTR1                                                                   
*                                                                               
         CLC   MISPRD(3),=C'POL'   CHECK FOR POOL REQUEST                       
         BE    PIGOK                                                            
*                                                                               
         CLI   1(R3),18            CHECK ELEMENT FOR PIGGYBACK                  
         BE    PIG020                                                           
*                                                                               
         CLI   SAVBKEY+3,X'FF'     TEST ALL PRODUCT REQUEST                     
         BE    PIGOK                                                            
         B     PIGBY                                                            
*                                                                               
PIG020   CLI   SAVBKEY+3,X'FF'     TEST ALL PRODUCT REQUEST                     
         BE    PIG100                                                           
         CLC   0(1,R5),SAVBKEY+3                                                
         BNE   PIG050                                                           
         CLC   4(1,R5),PIGCODE                                                  
         BE    PIG100                                                           
         B     PIGBY                                                            
*                                                                               
PIG050   CLC   0(1,R5),PIGCODE                                                  
         BNE   PIGBY                                                            
         CLC   4(1,R5),SAVBKEY+3                                                
         BNE   PIGBY                                                            
*                                                                               
PIG100   LA    R8,WORK3            TABLE                                        
*                                                                               
PIG150   CLI   0(R8),0             TEST E-O-L                                   
         BE    PIG200                                                           
         CLC   0(1,R8),0(R5)       SAME ACTIVE PROD                             
         BNE   PIG170                                                           
         CLC   1(1,R8),BDSEC       SAME LENGTH                                  
         BNE   PIG170                                                           
         CLC   2(1,R8),4(R5)       SAME PASSIVE PRODUCT                         
         BE    PIGBY                                                            
PIG170   LA    R8,4(R8)                                                         
         B     PIG150                                                           
*                                                                               
*                                                                               
PIG200   LA    R8,WORK3            TABLE                                        
*                                                                               
PIG250   CLI   0(R8),0             TEST E-O-L                                   
         BE    PIG300                                                           
         CLC   0(1,R8),4(R5)       SAME ACTIVE PROD                             
         BNE   PIG270                                                           
         CLC   1(1,R8),BDSEC       SAME LENGTH                                  
         BNE   PIG270                                                           
         CLC   2(1,R8),0(R5)       SAME PASSIVE PRODUCT                         
         BE    PIGBY                                                            
PIG270   LA    R8,4(R8)                                                         
         B     PIG250                                                           
*                                                                               
PIG300   MVC   0(1,R8),0(R5)                                                    
         MVC   1(1,R8),BDSEC                                                    
         MVC   2(1,R8),4(R5)                                                    
         MVI   3(R8),X'00'                                                      
* SET RETURN CODE                                                               
PIGBY    SR    R1,R1                                                            
         B     PIGX                                                             
*                                                                               
PIGOK    LA    R1,1                                                             
*                                                                               
PIGX     LTR   R1,R1                                                            
         B     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPMISWORK                                                      
         EJECT                                                                  
GENOLD   DSECT                                                                  
*                                                                               
         ORG  IOAREA                                                            
       ++INCLUDE SPGENGOAL                                                      
         EJECT                                                                  
         ORG  IOAREA                                                            
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPGENXLK                                                       
         PRINT OFF                                                              
       ++INCLUDE SPPWBLOCK                                                      
       ++INCLUDE SPGENEST                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'073SPMIS01   09/04/20'                                      
         END                                                                    
