*          DATA SET RECNT29    AT LEVEL 048 AS OF 02/25/15                      
*PHASE T80229A                                                                  
*INCLUDE OUTDAY                                                                 
         TITLE 'T80229 - REPPAK MAKEGOOD OFFER DISPLAY'                         
*                                                                               
*******************************************************************             
*                                                                 *             
*        RECNT29 (T80229) --- MAKEGOOD OFFER DISPLAY              *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* 19MAY03 SKU SELF APPLY                                          *             
* 07JUL02 HQ  DEMO CAT AND VAULE DISPLAY                          *             
* 30JAN01 HWO IGNORE AGENCY OFFICE IN BRAND CHECK (BRANDCHK)      *             
* 03AUG00 SKU TEST FOR DELETED TARGET BUY                         *             
* 25JUN98 SKU SUPPORT CREDIT END DATE FORMAT                      *             
* 23JUL97 SKU 4K CONTRACT SUPPORT                                 *             
* 29MAY97 SKU SPECIAL BONUS HANDLING                              *             
* 14APR97 SKU MGO NOT ALLOWED FOR A BRAND ORDER IF ALL OF THE     *             
*             BRANDS WITH THIS CONTRACT ARE NOT CONFIRMED         *             
* 07APR97 SKU SPECIAL ROUTINE FOR FOX/PETRY MAKEGOOD FIX          *             
* 02DEC96 SKU BOTH REP/STA CAN CHG MG REGARDLESS OF OWNERSHIP     *             
* 16OCT96 SKU SUPPORT BONUSES AND PREEMPTS                        *             
* 07OCT96 SKU SUPPORT LOW POWER STATION                           *             
* 19MAR96 SKU OPEN UP GROUP COMMENT LINE FOR REJECTED MG          *             
*             ALSO MISSED COMMENTS AND DETAIL COMMENTS            *             
* 13DEC95 SKU 2K CONTRACT SUPPORT                                 *             
* 06MAR95 BU  CHANGE GROUP CODE FORMAT TO 2 CHARS.                *             
* 02FEB95 BU  SET FOR FULL-SCREEN ENTRY                           *             
* 12JAN95 BU  ORIGINAL ENTRY                                      *             
*                                                                 *             
*******************************************************************             
*                                                                               
T80229   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T80229                                                         
         L     RC,0(R1)            WORK                                         
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
*                                                                               
         LA    R2,CONCACTH         MUST BE CONFIRMED AT LEAST ONCE              
         LA    R3,479                                                           
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BNE   ERROR                                                            
         USING RCONXEL,R6                                                       
         TM    RCONCONF,X'40'+X'20'                                             
         BZ    ERROR                                                            
         DROP  R6                                                               
                                                                                
         GOTO1 =A(FOXFIX),DMCB,(RC),(RA),RR=Y                                   
                                                                                
         BAS   RE,BRANDCHK                                                      
                                                                                
         NI    MGODAYSH+1,X'DF'    TURN OFF PROTECTED BITS TO FOUT              
         NI    MGOTIMSH+1,X'DF'    SPACES. TURN ON AGAIN AT END                 
         NI    MGOLENH+1,X'DF'                                                  
         NI    MGODTESH+1,X'DF'                                                 
         NI    MGODTE2H+1,X'DF'                                                 
         NI    MGONPWH+1,X'DF'                                                  
         NI    MGORATEH+1,X'DF'                                                 
         NI    MGOMDTEH+1,X'DF'                                                 
         NI    MGOMOPTH+1,X'DF'                                                 
         NI    MGOEXPDH+1,X'DF'                                                 
         NI    MGOMMSDH+1,X'DF'                                                 
         NI    MGOMDESH+1,X'DF'                                                 
*                                                                               
         LA    R4,MGOMDYSH                                                      
         LR    R3,R4               SAVE LINE ADDRESS                            
         LA    RF,5                SET LOOP ON LINE                             
         LA    RE,3                SET # LINES LOOP                             
DISP0030 EQU   *                                                                
         NI    1(R4),X'DF'                                                      
         ZIC   R2,0(R4)            BUMP TO NEXT FIELD ON LINE                   
         AR    R4,R2               BUMP TO NEXT FIELD ON LINE                   
         BCT   RF,DISP0030         GO BACK TO SET NEXT                          
         LR    R4,R3               RESET LINE ADDRESS                           
         LA    R4,MGOMDY2H-MGOMDYSH(R4)                                         
*                                  BUMP TO SAME LOCATION, NEXT LINE             
         LA    RF,5                RESET LINE LOOP                              
         BCT   RE,DISP0030         GO BACK FOR NEXT LINE                        
*                                                                               
*   NOTE:  ABOVE LOOPS ARE SET TO THE DESCRIPTION OF THE RECNTE5                
*        SCREEN.  A CHANGE TO NUMBER OF FIELDS WILL DICTATE A                   
*        CHANGE TO THIS CODE.  THIS SHOULD BE MADE SOFT.                        
*                                                                               
DISP0040 DS    0H                                                               
***>>>   CLC   =C'MGO',BUYACT                                                   
***>>>   BNE   DISP0080                                                         
*                                                                               
DISP0060 EQU   *                                                                
***>>>   GOTO1 VFOUTBLK,DMCB,MGOHAGYH,MGORATE,0                                 
***>>>   GOTO1 VFOUTBLK,DMCB,MGOMDYSH,MGOLAST,0                                 
***>>>   B     DISP0120                                                         
*                                                                               
DISP0080 EQU   *                                                                
* DON'T CLEAR DEFAULTS FOR OFFER ALL/CHOICE & PLEASE ADVISE/ASSUME FLDS         
         GOTO1 VFOUTBLK,DMCB,MGOHAGYH,MGOSTATH,0                                
         GOTO1 VFOUTBLK,DMCB,MGOEXPDH,MGOLAST,0                                 
DISP0120 EQU   *                                                                
         GOTO1 =A(DISPCON),DMCB,(RC),(RA),RR=Y                                  
*                                  DISPLAY CONTRACT HEADER INFO                 
*                                                                               
*                                                                               
*        DISPLAY ORIGINAL BUYLINE                                               
*                                                                               
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         XC    TWAMKGDT,TWAMKGDT                                                
         OC    TWAMKGDS,TWAMKGDS   ANY RECORD SELECTED?                         
         BZ    DISP1600            REQUEST FOR NEW:  NO DISPLAY                 
         CLC   TWAMKGDS,=F'-1'     REQUEST FOR NEW FROM CONTROL?                
         BE    DISP1600            YES                                          
         CLC   TWAMKGDS,=F'-2'     TARGET BUY NO LONGER ON FILE?                
         BNE   DISP0150            NO                                           
         MVC   MGOTBUY(7),=C'MISSING'                                           
         OI    MGOTBUYH+6,X'80'    XMIT                                         
         B     DISP1500            YES                                          
*                                                                               
DISP0150 EQU   *                                                                
         TM    TWAMKGFG,X'80'      'SELECT' ACTION?                             
         BNO   DISP0160            NO                                           
         MVI   ADDORSEL,C'S'       YES                                          
         B     DISP0200                                                         
DISP0160 EQU   *                                                                
         TM    TWAMKGFG,X'40'      'ADD' ACTION?                                
         BNO   DISP0200            NO                                           
         MVI   ADDORSEL,C'A'       YES                                          
DISP0200 EQU   *                                                                
*                                                                               
         XC    KEY,KEY             RECORD SELECTED: LOAD D/A                    
         MVC   KEY+28(4),TWAMKGDS                                               
         DROP  RF                                                               
*                                                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
         GOTO1 VMOVEREC,DMCB,IOAREA,RBUYREC                                     
*                                  PRIMARY RECORD WILL ALWAYS BE                
*                                     BUY RECORD                                
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   TWAMKGL#,RBUYKLIN   SAVE ORIGINAL BUYLINE #                      
         DROP  RF                                                               
*                                                                               
         EDIT  RBUYKLIN,(3,MGOTBUY),ALIGN=LEFT                                  
         LA    RF,MGOTBUY                                                       
         AR    RF,R0                                                            
         MVC   1(3,RF),=C'***'                                                  
         LA    RF,MGOTBUYH                                                      
         OI    6(RF),X'80'         SET FIELD TO 'TRANSMIT'                      
*                                                                               
*                                  GET 1ST DAY-TIME ELEMENT                     
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,MGODAYS                                                       
         LA    R7,MGOTIMS                                                       
         SR    R8,R8                                                            
         IC    R8,MGODAYSH                                                      
         LA    R8,MGODAYSH-1(R8)   DAY FIELD END-1                              
         SR    R9,R9                                                            
         IC    R9,MGOTIMSH                                                      
         LA    R9,MGOTIMSH-1(R9)   TIME FIELD END-1                             
DISP0360 DS    0H                                                               
         MVC   WORK,MYSPACES                                                    
*                                  DISPLAY DAY-TIME FIELDS                      
         GOTO1 =V(OUTDAY),DMCB,3(R6),2(R6),WORK,RR=Y                            
         LA    RE,WORK                                                          
         SR    R4,R4                                                            
         CLI   0(RE),C' '                                                       
         BE    *+16                                                             
         LA    R4,1(R4)                                                         
         LA    RE,1(RE)                                                         
         B     *-16                                                             
*                                  ADD DAY LENGTH FIELD                         
         LA    RE,0(R4,R3)                                                      
         CR    RE,R8               ROOM LEFT?                                   
         BH    DISP0400                                                         
         STC   R4,BYTE                                                          
         BCTR  R4,R0                                                            
         EX    R4,*+8              MOVE DAY                                     
         B     *+10                                                             
         MVC   0(0,R3),WORK                                                     
*                                                                               
*                                  TIME                                         
         MVI   WORK,C' '                                                        
         MVC   WORK+1(L'WORK-1),WORK                                            
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A11'                                           
         GOTO1 CALLOV,DMCB,0       GET ADDRESS OF UNTIME                        
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),4(R6),WORK                                             
         LA    RE,WORK             GET TIME LEN                                 
         SR    R2,R2                                                            
         CLI   0(RE),C' '                                                       
         BE    *+16                                                             
         LA    R2,1(R2)                                                         
         LA    RE,1(RE)                                                         
         B     *-16                                                             
         LA    RE,0(R2,R7)                                                      
         CR    RE,R9               ROOM LEFT?                                   
         BH    DISP0400                                                         
         STC   R2,BYTE2                                                         
         BCTR  R2,R0                                                            
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),WORK                                                     
         BAS   RE,NEXTEL                                                        
         BNE   DISP0400                                                         
         ZIC   R2,BYTE             DAY LENGTH                                   
         LA    R3,0(R2,R3)         NEXT DAY                                     
         ZIC   R2,BYTE2            TIME LARGER                                  
         LA    R7,0(R2,R7)         NEXT TIME                                    
         MVI   0(R3),C'*'          FIELD SEPARATOR                              
         MVI   0(R7),C'*'                                                       
         LA    R3,1(R3)                                                         
         LA    R7,1(R7)                                                         
         B     DISP0360                                                         
         EJECT                                                                  
*                                                                               
*        DISPLAY BUYLINE DEMO CATEGORIES AND VALUE                              
DISP0400 DS    0H                                                               
         GOTOR CKSTPROF                                                         
         BNE   DISP0420                                                         
         GOTOR DISDEM                                                           
*                                                                               
DISP0420 DS    0H                                                               
         MVC   HALF,RBUYDUR                                                     
         NI    HALF,X'7F'                                                       
         EDIT  (2,HALF),(3,MGOLEN),ALIGN=LEFT                                   
         STC   R0,MGOLENH+5        NEW OUTPUT LENGTH.  THIS ACTUALLY            
*                                  BECOMES INPUT FOR REMAINING                  
*                                  COMBO CONTRACTS                              
         LR    RE,R0                                                            
         LA    RE,MGOLEN(RE)                                                    
         TM    RMKGDUR,X'80'       MINUTES?                                     
         BZ    DISP0440                                                         
         MVI   0(RE),C'M'                                                       
*                                                                               
         ZIC   RF,MGOLENH+5        ADJUST FOR NEW LENGTH                        
         LA    RF,1(RF)                                                         
         STC   RF,MGOLENH+5                                                     
         EJECT                                                                  
*                                  DISPLAY CROSS DAY                            
DISP0440 DS    0H                  CHECK PROFILE, OR IF DDS, DISPLAY            
         XC    MGOXDAY,MGOXDAY     CLEAR                                        
         LR    RF,RA               CROSS DAYS                                   
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BE    DISP0480                                                         
                                                                                
         TM    PROFILES+CNTXDAYB,CNTXDAYA                                       
         BZ    DISP0600            PROFILE SET TO USE THIS?                     
                                                                                
DISP0480 DS    0H                                                               
         CLI   TWAECON,C'B'        IS IT EC/BIAS?                               
         BNE   DISP0600                                                         
         DROP  RF                                                               
                                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'13'        EC BIAS ELEMENT                              
         BAS   RE,GETEL                                                         
         BNE   DISP0600                                                         
         USING RCONCCEL,R6                                                      
                                                                                
         MVC   MGOXDAY,=7C'.'      CLEAR                                        
         ZIC   RF,RCONCCCD         CROSS DAY BYTE                               
         SLL   RF,25                                                            
         LA    RE,MGOXDAY                                                       
         LA    R1,C'1'                                                          
                                                                                
DISP0520 DS    0H                                                               
         LTR   RF,RF                                                            
         BNM   DISP0560            IF DAY BIT IS ON                             
         STC   R1,0(RE)            PUT CORRESPONDING NUMBER THERE               
                                                                                
DISP0560 DS    0H                                                               
         SLL   RF,1                NEXT DAY BIT                                 
         LA    RE,1(RE)            NEXT DAY DISPLAY POSITION                    
         LA    R1,1(R1)            NEXT EBCDIC DISPLAY NUMBER                   
         LA    R0,MGOXDAY+7        CHECK IF WE'VE LOOKED AT ALL 7 DAYS          
         CR    RE,R0                                                            
         BNH   DISP0520                                                         
                                                                                
DISP0600 DS    0H                                                               
         OI    MGOXDAYH+6,X'80'    XMIT IT                                      
         DROP  R6                                                               
         EJECT                                                                  
*              DISPLAY EFFECTIVE DATES                                          
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R7,WORK2            OUTPUT                                       
         MVI   WORK2,C' '                                                       
*                                                                               
         MVC   WORK2+1(L'WORK2-1),WORK2                                         
         LA    R8,WORK2+60         OUTPUT END                                   
*                                                                               
         LA    R9,WORK2+120                                                     
DISP0640 LA    R3,WORK+20                                                       
*                                  DISPLAY DATES                                
         GOTO1 DATCON,DMCB,(3,2(R6)),(4,(R3))                                   
*                                  START DATE                                   
         CLC   2(3,R6),5(R6)                                                    
         BNE   *+12                                                             
         LA    R3,5(R3)                                                         
         B     DISP0680                                                         
*                                                                               
         MVI   5(R3),C'-'                                                       
*                                                                               
         GOTO1 (RF),(R1),(3,5(R6)),(4,6(R3))                                    
*                                  END DATE                                     
         LA    R3,11(R3)                                                        
*                                                                               
DISP0680 TM    8(R6),X'40'         ALT WEEKS?                                   
         BZ    *+12                                                             
         MVI   0(R3),C'A'                                                       
         LA    R3,1(R3)                                                         
*                                                                               
*                                  DISPLAY NPW IF NOT = RBUYNW                  
         CLC   RBUYNW,9(R6)                                                     
         BE    DISP0720                                                         
         MVI   0(R3),C'('                                                       
         EDIT  (1,9(R6)),(3,1(R3)),ALIGN=LEFT                                   
         CLI   9(R6),0                                                          
         BNE   *+8                                                              
         MVI   1(R3),C'0'                                                       
         LA    R3,2(R3)                                                         
         MVI   2(R3),C' '                                                       
         CLI   0(R3),C' '                                                       
         BE    *+12                                                             
         LA    R3,1(R3)                                                         
         B     *-12                                                             
         MVI   0(R3),C')'                                                       
         LA    R3,1(R3)                                                         
DISP0720 LA    RE,WORK+20                                                       
         SR    R3,RE               GET ELEM DISPLAY LEN                         
         LR    RF,R7                                                            
         AR    RF,R3               OUTPUT PTR                                   
*                                  ROOM IN FIRST LINE?                          
         CR    RF,R8               WORK2+60                                     
         BNH   DISP0760            NO  - START AT SECOND LINE                   
*                                                                               
         LA    R8,500(R8)          ELIM. FIRST TEST                             
         LA    R7,WORK2+60         START 2D LINE                                
         CLI   WORK2+60,C'*'                                                    
         BNE   DISP0800                                                         
         LA    R7,1(R7)                                                         
         B     DISP0800                                                         
*                                                                               
DISP0760 CR    RF,R9               WORK2+120                                    
         BH    DISP0840            DOESN'T FIT                                  
*                                                                               
DISP0800 BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),WORK+20                                                  
         MVC   WORK+20(20),MYSPACES                                             
         LA    R7,1(R3,R7)         OUTPUT PTR                                   
         BAS   RE,NEXTEL                                                        
         BNE   DISP1040                                                         
*                                                                               
         MVI   0(R7),C'*'                                                       
         LA    R7,1(R7)                                                         
         B     DISP0640                                                         
*                                  DATES DON'T FIT  - TRY TO                    
*                                     COMPRESS  - DISPLAY WEEKS AS              
*                                     END DATE                                  
DISP0840 LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   WORK2,C' '                                                       
         MVC   WORK2+1(L'WORK2-1),WORK2                                         
         LA    R3,WORK2                                                         
*                                                                               
DISP0880 GOTO1 DATCON,(R1),(3,2(R6)),(4,(R3))                                   
         LA    R7,5                                                             
         CLI   3(R3),C'0'          DAY =JAN01?                                  
         BNE   DISP0920                                                         
         MVC   3(1,R3),4(R3)       COMPRESS TO JAN1                             
         MVI   4(R3),C' '                                                       
         BCTR  R7,R0                                                            
DISP0920 AR    R3,R7                                                            
*                                                                               
         CLI   10(R6),1            1 WEEK?                                      
         BE    DISP0960                                                         
         MVI   0(R3),C'-'                                                       
         LA    R3,1(R3)                                                         
*                                  DISPLAY WEEKS                                
         EDIT  (1,10(R6)),(2,(R3)),ALIGN=LEFT                                   
         CLI   10(R6),9                                                         
         BH    *+16                                                             
         MVI   1(R3),C'W'                                                       
         LA    R3,2(R3)                                                         
         B     *+12                                                             
         MVI   2(R3),C'W'                                                       
         LA    R3,3(R3)                                                         
         TM    8(R6),X'40'         ALT WEEKS?                                   
         BZ    *+12                                                             
         MVI   0(R3),C'A'                                                       
         LA    R3,1(R3)                                                         
*                                  NUMBER PER WEEK                              
DISP0960 CLC   RBUYNW,9(R6)        SAME FOR BUY?                                
         BE    DISP1000                                                         
*                                  DISPLAY NPW                                  
         MVI   0(R3),C'('                                                       
         EDIT  (1,9(R6)),(3,1(R3)),ALIGN=LEFT                                   
         CLI   9(R6),0                                                          
         BNE   *+8                                                              
         MVI   1(R3),C'0'                                                       
         LA    R3,2(R3)                                                         
         MVI   2(R3),C' '                                                       
         CLI   0(R3),C' '                                                       
         BE    *+12                                                             
         LA    R3,1(R3)                                                         
         B     *-12                                                             
*                                                                               
         MVI   0(R3),C')'                                                       
         LA    R3,1(R3)                                                         
*                                  GET NEXT ELEM                                
DISP1000 DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   DISP1040                                                         
         MVI   0(R3),C'*'                                                       
         LA    R3,1(R3)                                                         
         B     DISP0880                                                         
DISP1040 MVC   MGODTES,WORK2       DATES                                        
         CLI   WORK2+60,C' '       ANTHING IN SECOND DATE AREA?                 
         BE    DISP1080            NO                                           
         MVC   MGODTE2,WORK2+60    MOVE 2ND LINE                                
         EJECT                                                                  
*                                                                               
*                                  NPW-RATE                                     
DISP1080 DS    0H                                                               
         EDIT  (1,RBUYNW),(3,MGONPW),ALIGN=LEFT                                 
*                                  NUMBER PER WEEK                              
         STC   R0,MGONPWH+5        NEW OUTPUT LENGTH.  THIS ACTUALLY            
*                                  BECOMES INPUT FOR REMAINING                  
*                                  COMBO CONTRACTS                              
         CLI   RBUYNW,0                                                         
         BNE   DISP1200                                                         
DISP1120 MVI   MGONPW,C'0'                                                      
         MVI   MGONPWH+5,1         ADJUST LENGTH FOR REST OF COMBO              
*                                                                               
DISP1200 DS    0H                                                               
         EDIT  (4,RBUYCOS),(9,MGORATE),2,ALIGN=LEFT,FLOAT=-                     
*                                  RATE                                         
*                                                                               
*        END OF DISPLAY OF ORIGINAL BUYLINE                                     
*                                                                               
*                                                                               
         EJECT                                                                  
DISP1500 EQU   *                   ADDITIONAL CHECK FOR SELECTING BONUS         
*                                                                               
*        DISPLAY OF MAKEGOOD OFFER                                              
*                                                                               
*        NOTE:  DON'T SET PREVALID BITS.  IF DISPLAYED, ASSUME THAT             
*              CHANGE IS MADE, WHICH REQUIRES REVALIDATION AND                  
*              RESTRUCTURING OF OFFER.                                          
*                                                                               
*        FOUT  CONBACTH,MYSPACES,8                                              
*        OI    CONBACTH+4,X'20'                                                 
         OI    CONCNUMH+4,X'20'                                                 
*                                                                               
         GOTO1 VFOUTBLK,DMCB,MGODAYSH,MGOLAST,1                                 
*                                                                               
         LA    R4,MGODAYSH         SET PROTECTED BITS ON ALL FIELDS             
         LA    R5,MGOMOPTH            IN 'ORIGINAL ORDER'                       
         SR    R3,R3                                                            
DISP1560 EQU   *                                                                
         OI    1(R4),X'20'         PROTECTED BIT                                
         FOUT  (R4)                                                             
         IC    R3,0(R4)            FIELD LEN                                    
         LA    R4,0(R3,R4)         NEXT FIELD                                   
         CR    R4,R5                                                            
         BL    DISP1560                                                         
*                                                                               
DISP1600 EQU   *                   ADDITIONAL CHECK FOR SELECTING BONUS         
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
*                                                                               
         TM    TWAMKGFG,X'80'      'SELECT' ACTION?                             
         BNO   DISP1620            NO                                           
         MVI   ADDORSEL,C'S'       YES                                          
         B     DISP1630                                                         
*                                                                               
DISP1620 EQU   *                                                                
         TM    TWAMKGFG,X'40'      'ADD' ACTION?                                
         BNO   DISP1630            NO                                           
         MVI   ADDORSEL,C'A'       YES                                          
         B     DISP1640                                                         
         DROP  RF                                                               
*                                                                               
DISP1630 EQU   *                                                                
         CLI   ADDORSEL,C'S'       'SELECT' ACTION?                             
         BNE   DISP1640            NO                                           
         MVC   CONCACT(3),=C'MGC'  SET ACTION TO 'CHANGE'                       
         MVC   LASTCACT(3),=C'MGC' SET LAST ACTION ALSO                         
         B     DISP1680                                                         
DISP1640 EQU   *                                                                
         MVC   CONCACT(3),=C'MGO'  SET ACTION BACK                              
         MVC   LASTCACT(3),=C'MGO' SET LAST ACTION ALSO                         
*                                  'ADD' OR 'NEW' GETS 'MGO'                    
         CLI   TWAACCS,C'$'        FOR ACTION MGO                               
         BE    DISP1680                                                         
         MVI   MGOMPAD,C'Y'        STATION DEFAULTS Y TO ASSUME OK              
*        OI    MGOMPADH+6,X'80'                                                 
         MVI   MGOMAOK,0                                                        
*        OI    MGOMAOKH+6,X'80'    AND REP DEFAULTS TO PLS ADVISE               
*                                                                               
DISP1680 EQU   *                                                                
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         TM    TWAMKGFG,X'08'      BONUS??                                      
         BZ    DISP1690                                                         
         MVC   MGOMMSD(5),=C'BONUS'                                             
         MVI   MGOMRAT,C'0'                                                     
         OI    MGOMDYSH+6,X'40'    FORCE CURSOR HERE                            
         OI    CONCNUMH+4,X'20'                                                 
         DROP  RF                                                               
*                                                                               
DISP1690 EQU   *                                                                
         LA    RF,CONCACTH                                                      
         OI    6(RF),X'80'         SET FIELD TO 'TRANSMIT'                      
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         MVC   DUB(2),=H'120'      SET MSG:  NEW OFFER GROUP                    
         OC    TWAMKGD2,TWAMKGD2   DETAIL SELECTED?                             
         BZ    DISP1720            NO  - DON'T DISPLAY DETAILS                  
         DROP  RF                                                               
*                                                                               
         GOTO1 =A(DISPNEW),DMCB,(RC),RR=Y                                       
*                                  DISPLAY CONTRACT HEADER INFO                 
         CLI   TWAACCS,C'$'        STATION?                                     
         BE    DISP1720            YES - LEAVE SCREEN                           
*                                  INPUT IS ALLOWED FROM BOTH SIDES             
****>>>  GOTO1 =A(REPSIDE),DMCB,(RC),(RA),RR=Y                                  
DISP1720 EQU   *                                                                
         MVC   DUB(2),=H'191'      BUY DELETED, DO MGX                          
*                                                                               
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         CLC   TWAMKGDS,=F'-2'                                                  
         BNE   DISP1750                                                         
         OI    CONCACTH+6,X'40'    FORCE CURSOR TO CONACT                       
         B     DISPX                                                            
         DROP  RF                                                               
*                                                                               
DISP1750 EQU   *                                                                
         MVC   DUB(2),=H'119'      SET MSG:  NEW OFFER GROUP                    
         CLI   ADDORSEL,C'A'       'ADD' ACTION?                                
         BE    DISP1760            YES                                          
         MVC   DUB(2),=H'118'      NO  - SET MSG:  CHANGING GROUP               
DISP1760 EQU   *                                                                
         OI    MGODCOMH+4,X'20'    SET COMMENT PREVALID BITS                    
         OI    MGODCM2H+4,X'20'    SET PREVALID                                 
         OI    MGODCM3H+4,X'20'                                                 
         TM    MGOMOPTH+1,X'20'    IF PROTECTED, USER IS NOT OFFERER            
         BZ    DISP1770            AND NO INPUT IS EXPECTED, SO                 
         OI    CONCACTH+6,X'40'    FORCE CURSOR TO CONACT                       
         B     DISPX                                                            
DISP1770 DS    0H                                                               
         OI    MGOMOPTH+6,X'40'    FORCE CURSOR TO SELECT MG BUYLINE            
*                                                                               
DISPX    DS    0H                                                               
         MVC   MGOLAST+1(2),=X'0101' RETRANSMIT SCREEN                          
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
* FOR ACTION MGO ONLY:                                                          
* FOR A DARE BRAND CONTRACT, CHECK IF ALL BRANDS IN THIS GROUP OF               
* CONTRACTS ARE CONFIRMED. MGO IS NOT ALLOWED IF NOT                            
** UPDATED TO IGNORE AGENCY OFFICE                                              
*                                                                               
BRANDCHK NTR1                                                                   
         CLC   =C'MGO',CONACT                                                   
         BE    BCHK10                                                           
         CLC   =C'MGO',BUYACT                                                   
         BNE   BCHKXIT                                                          
*                                                                               
BCHK10   DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'        IS THIS A DARE ORDER?                        
         BAS   RE,GETEL                                                         
         BNE   BCHKXIT             NO, EXIT                                     
         USING RCONDREL,R6                                                      
         CLI   RCONDRLN,RCONDL2Q                                                
         BL    BCHKXIT                                                          
         TM    RCONDRF2,X'40'      IS THIS A BRAND?                             
         BZ    BCHKXIT             NO, EXIT                                     
         DROP  R6                                                               
*                                                                               
         XC    IOAREA(32),IOAREA                                                
         MVI   RAGK2TYP,RAGK2TYQ                                                
         MVC   RAGK2AGY,RCONKAGY                                                
         MVC   RAGK2AOF,RCONKAOF                                                
         MVC   RAGK2REP,RCONKREP                                                
         MVC   KEY,IOAREA                                                       
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RAGY2KEY),KEYSAVE                                          
         BNE   BCHKERR                                                          
         GOTO1 VGETREC,DMCB,IOAREA                                              
                                                                                
         LA    R5,KEY                                                           
         USING RDARKEY,R5                                                       
         XC    KEY,KEY                                                          
         MVI   RDARKTYP,X'51'                                                   
         MVC   RDARKREP,REPALPHA                                                
         MVC   RDARKSTA(5),RCONKSTA                                             
         CLI   RDARKSTA+4,C' '                                                  
         BNE   *+8                                                              
         MVI   RDARKSTA+4,C'T'     MUST SPECIFY IF TV                           
         OC    RDARKSTA,MYSPACES                                                
         OC    RAGY2DAR,RAGY2DAR   NULL EQUIVALENCY CODE?                       
         BZ    BCHK29                                                           
         MVC   RDARKAGY,RAGY2DAR   EQUIVALENCY CODE                             
                                                                                
         LA    R6,RCONREC                                                       
         USING RCONDREL,R6                                                      
         MVI   ELCODE,X'1D'        GET DARE ELEMENT                             
         BAS   RE,GETEL                                                         
         BNE   BCHKERR                                                          
                                                                                
         LA    R4,RAGY2DAR         THERE ARE MAX 4 AGENCY ASSIGNMENT            
         LA    R3,4                COMBINATIONS WE NEED TO CHECK                
         B     BCHK20                                                           
*                                                                               
PRVKEY   USING RDARKEY,KEYSAVE                                                  
BCHK18   CLC   RDARKAOF,PRVKEY.RDARKAOF    SAME OFFICE?                         
         DROP  PRVKEY                                                           
         BNE   BCHK19                NO -- DON'T INCREMENT                      
         XR    R0,R0                                                            
         ICM   R0,3,RDARKAOF                                                    
         AHI   R0,1                   INCREMENT AGENCY OFFICE FIELD             
         STCM  R0,3,RDARKAOF                                                    
BCHK19   XC    RDARKORD(7),RDARKORD  CLEAR FELDS AFTER AGENCY OFFICE            
*                                                                               
BCHK20   DS    0H                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(RDARKAOF-RDARKEY),KEYSAVE  SAME KEY?                         
         BNE   BCHK21                                                           
         XC    RDARKORD(7),RDARKORD  CLEAR FELDS AFTER AGENCY OFFICE            
         MVC   RDARKORD,RCONDRLK     MOVE IN ORDER #                            
*                                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(RDARKAOF-RDARKEY),KEYSAVE  SAME KEY?                         
         BNE   BCHK21                                                           
         CLC   RDARKORD,RCONDRLK     SAME ORDER #?                              
         DROP  R6                                                               
*                                                                               
         BNE   BCHK18                NO -- SKIP READ                            
         CLI   RDARKRT,X'10'         AGENCY HEADER?                             
         BE    BCHK30                                                           
         B     BCHK29                                                           
*                                                                               
BCHK21   CLI   RAGY2FXL,RAGY2FLQ   ONLY NEWER AGENCY RECORDS                    
         BL    BCHKERR             HAS MULTI-DARE AGENCY ASSIGNMENTS            
         MVC   KEY,KEYSAVE                                                      
*                                                                               
BCHK22   LA    R4,5(R4)                                                         
         OC    0(3,R4),0(R4)         NULL EQUIVALENCY CODE?                     
         BZ    BCHK29                YES                                        
         CLC   RDARKAGY,0(R4)        SAME EQUIVALENCY CODE?                     
         BNE   *+12                                                             
         BCT   R3,BCHK22             CHECK NEXT EQUIVALENCY CODE                
         B     BCHK29                                                           
*                                                                               
         MVC   RDARKAGY,0(R4)      EQUIVALENCY CODE                             
         XC    RDARKAOF(9),RDARKAOF  CLEAR FIELDS AFTER AGENCY CODE             
         BCT   R3,BCHK20                                                        
*                                  SPECIAL FOR SELTEL                           
BCHK29   CLC   =C'SZ',REPALPHA                                                  
         BNE   BCHKERR                                                          
         CLC   =C'1342  ',RAGK2AGY AND AGENCY 1342 ONLY                         
         BNE   BCHKERR                                                          
         MVC   RDARKAGY(5),=C'ED2DE'                                            
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
         BNE   BCHKERR                                                          
         DROP  R5                                                               
*                                                                               
BCHK30   DS    0H                                                               
         L     R6,AIO4                                                          
         GOTO1 VGETREC,DMCB,(R6)   USE IO4                                      
         USING RDARREC,R6                                                       
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(RDARKORD-RDARKEY),0(R6)                                      
KEYD     USING RDARKEY,KEY                                                      
*                                                                               
         MVI   ELCODE,X'0B'                                                     
         BAS   RE,GETEL                                                         
         BNE   BCHKERR                                                          
         USING RDARBREM,R6                                                      
         MVC   KEYD.RDARKORD,RDARBRVN                                           
         MVI   KEYD.RDARKRT,X'10'                                               
         MVI   KEYD.RDARKTYP,X'41'                                              
         DROP  KEYD,R6                                                          
*                                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
         BNE   BCHKERR                                                          
*                                                                               
         GOTO1 VGETREC,DMCB,(R6)   USE IO4                                      
         USING RDARREC,R6                                                       
         TM    RDARMISC,X'02'                                                   
         BO    BCHKXIT                                                          
*                                                                               
BCHKERR  DS    0H                                                               
         XC    CONCACT,CONCACT                                                  
         LA    R3,695                                                           
         B     ERROR                                                            
*                                                                               
BCHKXIT  DS    0H                                                               
         B     EXXMOD                                                           
         DROP  R6                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
         EJECT                                                                  
       ++INCLUDE REGENDAR                                                       
         EJECT                                                                  
         ORG   CONCNUMX+8                                                       
       ++INCLUDE RECNTE5D                                                       
         EJECT                                                                  
*                                                                               
*   FOXFIX: ROUTINE TO READ PETRY MAKEGOOD OFFERS FOR THIS CONTRACT             
*           AND ADD THEM UNDER FOX FOR STATIONS:                                
*           WNYW, KTTV, WFLD, WTXF AND WFXT.                                    
*                                                                               
*           IF THE MAKEGOOD OFFERS ALREADY EXIST IN THE FOX FILE THEN           
*           WE SHOULD EXIT. WE DON'T WANT TO OVERWRITE                          
*           ANY NEW OR PREVIOUSLY ADDED AND SUBSEQUENTLY CHANGED                
*           MAKEGOOD OFFERS!!                                                   
*                                                                               
T80229   CSECT                                                                  
         DS    0F                                                               
FOXFIX   NMOD1 0,*FOXX*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         USING GENOLD,RC                                                        
         L     RA,4(R1)            RESET A(TWASPACE)                            
         USING TWAD,RA                                                          
         LR    R8,RA                                                            
         AH    R8,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R8                                                       
*                                                                               
         CLC   =C'FN',REPALPHA                                                  
         BNE   FOXXIT                                                           
         CLC   RCONKCON,=X'02512334'                                            
         BH    FOXXIT                                                           
         CLC   =C'WNYW',RCONKSTA                                                
         BE    FOX10                                                            
         CLC   =C'KTTV',RCONKSTA                                                
         BE    FOX10                                                            
         CLC   =C'WFLD',RCONKSTA                                                
         BE    FOX10                                                            
         CLC   =C'WTXF',RCONKSTA                                                
         BE    FOX10                                                            
         CLC   =C'WFXT',RCONKSTA                                                
         BNE   FOXXIT                                                           
*                                                                               
FOX10    DS    0H                                                               
         XC    RMKGREC(32),RMKGREC                                              
         MVI   RMKGKTYP,X'11'                                                   
         MVC   RMKGKREP,=C'PV'     INSERT PETRY POWER CODE                      
         MVC   RMKGKOFF,RCONKOFF   INSERT OFFICE CODE                           
         MVC   RMKGKSTA,RCONKSTA   INSERT STATION CALL LETTERS                  
         MVC   RMKGKCON,TWACNUM                                                 
*                                                                               
         MVC   KEY,RMKGREC         LOAD KEY FROM RECORD                         
         MVI   UPDATE,C'N'         SET UPDATE TO NO                             
         GOTO1 VHIGH                                                            
*                                                                               
FOX20    DS    0H                                                               
         CLC   KEY(RMKGKGRP-RMKGKEY),KEYSAVE                                    
         BNE   FOXXIT              EXIT IF NO RECORD FOUND OR DONE              
*                                  FOUND ONE, SEE IF THIS EXISTS                
         MVI   UPDATE,C'N'         SET UPDATE TO NO                             
         GOTO1 VGETREC,DMCB,RMKGREC                                             
*                                  SAME KEY - THROUGH CON#                      
         MVC   KEY+6(2),=C'FN'     IN THE FOX FILE                              
         MVI   UPDATE,C'N'         SET UPDATE TO NO                             
         OI    DMINBTS,X'08'       READ FOR DELETED RECORDS                     
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(L'RMKGKEY),KEYSAVE                                           
         BE    FOXXIT              EXIT IF RECORD FOUND                         
*                                                                               
         MVC   RMKGKREP,=C'FN'     ADD RECORD AS FOX                            
         GOTO1 VADDREC,DMCB,RMKGREC                                             
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                HOUSTON, WE'VE GOT A PROBLEM                 
*                                                                               
         MVC   KEY(L'RMKGKEY),RMKGKEY                                           
         MVC   KEY+6(2),=C'PV'     IN THE FOX FILE                              
*                                  GET NEXT PETRY MAKEGOOD OFFER                
         MVI   UPDATE,C'N'         SET UPDATE TO NO                             
         GOTO1 VHIGH                                                            
         MVI   UPDATE,C'N'         SET UPDATE TO NO                             
         GOTO1 VSEQ                                                             
         B     FOX20                                                            
*                                                                               
FOXXIT   DS    0H                                                               
         XMOD1                                                                  
         DROP  R8                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
*   ROUTINE TO RETRIEVE/DISPLAY MAKEGOOD OFFER RECORDS                          
*                                                                               
T80229   CSECT                                                                  
DISPNEW  NMOD1 0,*DNEW*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
****>>>  LA    R2,CONBNUMH                                                      
*                                                                               
         LR    R7,RA                                                            
         AH    R7,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R7                                                       
         MVI   CHOYSCTR,0          SET CHOICE COUNTER TO ZERO                   
         XC    TWAMKGDT,TWAMKGDT   CLEAR GROUP CODE                             
         OC    TWAMKGD2,TWAMKGD2   ANY RECORD SELECTED?                         
         BZ    DNEW1600            REQUEST FOR NEW:  NO DISPLAY                 
         TM    TWAMKGFG,X'80'      'SELECT' ACTION?                             
         BNO   DNEW0160            NO                                           
         MVI   ADDORSEL,C'S'       YES                                          
         B     DNEW0200                                                         
DNEW0160 EQU   *                                                                
         TM    TWAMKGFG,X'40'      'ADD' ACTION?                                
         BNO   DNEW0200            NO                                           
         MVI   ADDORSEL,C'A'       YES                                          
DNEW0200 EQU   *                                                                
*                                                                               
         XC    KEY,KEY             RECORD SELECTED: LOAD D/A                    
         MVC   KEY+28(4),TWAMKGD2                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
         GOTO1 VMOVEREC,DMCB,IOAREA,RMKGREC                                     
*                                  MOVE SELECTED RECORD TO RMKGREC              
         GOTO1 GRPCOMNT            DISPLAY GROUP COMMENT                        
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(27),RMKGKEY     LOAD KEY AGAIN                               
         GOTO1 VHIGH               ESTABLISH READ SEQ'ABILITY                   
         CLI   RMKGKRTY,0          BASIC 1ST OF SET?                            
         BE    DNEW0210            YES - PROCEED                                
         ZIC   RF,RMKGKRTY         CHECK IF 1ST OF SET                          
         SLL   RF,28               DROP ADD/CHOICE BIT                          
         SRL   RF,28               RESTORE RECORD #                             
         CH    RF,=H'1'            1ST RECORD?                                  
         BNH   DNEW0210            YES                                          
         MVI   KEY+RMKGKRTY-RMKGKEY,0                                           
*                                  NO  - CLEAR RECORD COUNTER BYTE              
         GOTO1 VHIGH                                                            
         CLC   KEY(26),KEYSAVE     RECORD FOUND?                                
         BE    *+6                 YES                                          
         DC    H'0'                NO  - ERROR                                  
*                                  YES - RETRIEVE THE RECORD                    
         GOTO1 VGETREC,DMCB,IOAREA                                              
         GOTO1 VMOVEREC,DMCB,IOAREA,RMKGREC                                     
DNEW0210 EQU   *                                                                
         XC    OFFRDISP,OFFRDISP                                                
         MVC   SAVMGKEY,KEY        SAVE 27 CHARS OF KEY                         
         MVC   TWAMKGDT,RMKGKGRP   SAVE GROUP CODE                              
         MVC   TWAMKGML,RMKGKLIN   SAVE MAKEGOOD LINE NUMBER                    
         DROP  R7                                                               
* GROUP CODE                                                                    
         MVC   MGOMDTE,RMKGKGR1                                                 
* OFFER NUMBER                                                                  
         EDIT  RMKGKLIN,(2,MGOMOFF)                                             
*                                                                               
         CLI   ADDORSEL,C'A'       'ADD' ACTION?                                
         BNE   DNEW0260            NO  - DISPLAY OFFER                          
         CLI   REDISPIT,1          YES - REQUEST FOR REDISPLAY?                 
         BNE   DNEW1700            NO  - DON'T DISPLAY ANYTHING ELSE            
         MVI   REDISPIT,0          YES - CLEAR THE FLAG                         
*                                                                               
DNEW0260 EQU   *                   SPECIFY ALL OR CHOICE                        
         ZIC   RF,RMKGKRTY                                                      
         SRL   RF,4                                                             
         LTR   RF,RF                                                            
         BZ    *+8                                                              
         MVI   MGOMOPT,C'C'                                                     
*                                                                               
         CLC   =C'APPLIED',MGOSTAT IF APPLIED, PROTECT EVERYTHING               
         BNE   DNEW0265            FOR BOTH OFFERER/OFFEREE                     
         LA    R4,MGOMOPTH                                                      
         B     DNEW0285                                                         
*                                                                               
DNEW0265 DS    0H                                                               
         CLC   =C'DARECAND',MGOSTAT  DARE MAKEGOOD WAS CANCELLED                
         BE    DNEW0285            PROTECT EVERYTHING, ONLY ACTION IS           
*                                    MGX                                        
IOMGH    USING RMKGREC,IOAREA                                                   
*                                                                               
         CLI   TWAACCS,C'$'        CHECK IF OFFER IN WIP                        
         BE    DNEW0270                                                         
         TM    IOMGH.RMKGSFG2,RMGF2STQ+RMGF2WPQ                                 
         BO    DNEW0280                                                         
         CLI   MGOCREA,C'R'                                                     
         BE    DNEW0300                                                         
         TM    IOMGH.RMKGSCST,RMKGSRCQ+RMKGSRJQ                                 
         BNZ   DNEW0280                                                         
         B     DNEW0275                                                         
*                                                                               
DNEW0270 DS    0H                                                               
         TM    IOMGH.RMKGSFG2,RMGF2RPQ+RMGF2WPQ                                 
         BO    DNEW0280                                                         
         CLI   MGOCREA,C'S'                                                     
         BE    DNEW0300                                                         
         TM    IOMGH.RMKGSCST,RMKGSRCQ+RMKGSRJQ                                 
         BNZ   DNEW0280                                                         
         DROP  IOMGH                                                            
*                                                                               
DNEW0275 DS    0H                                                               
         MVI   MGOMOPTH+5,1                                                     
         CLI   MGOMOPT,C'A'        DON'T PROTECT IF CHOICE                      
         BNE   DNEW0300                                                         
         OI    MGOMOPTH+1,X'20'                                                 
         B     DNEW0300                                                         
*                                  IF OPPOSITE SIDE IS LOOKING AT OFFER         
*                                  ASK FOR MAKEGOOD BUY SELECTION               
DNEW0280 DS    0H                                                               
         LA    R4,MGOMOPTH         SET PROTECTED BITS ON ALL FIELDS             
         CLI   MGOMOPT,C'C'        CHANGE PROMPT FOR CHOICE                     
         BNE   DNEW0285                                                         
         MVC   MGOQOPT,=C'Select Makegood Buyline:'                             
         XC    MGOMOPT,MGOMOPT                                                  
*        LA    R4,MGOMPADH         SET PROTECTED BITS ON ALL FIELDS             
*                                  EXCEPT FOR CHOICE OPTION                     
DNEW0285 DS    0H                                                               
         LA    R5,MGOODE3H                                                      
*                                                                               
DNEW0290 EQU   *                                                                
         OI    1(R4),X'20'         PROTECTED BIT                                
         ZIC   RF,0(R4)            FIELD LEN                                    
         AR    R4,RF               NEXT FIELD                                   
         CR    R4,R5                                                            
         BNH   DNEW0290                                                         
*                                                                               
DNEW0300 EQU   *                   CHANGE BACK TO REJECTED                      
         CLC   =C'DARECAND',MGOSTAT                                             
         BNE   DNEW0305            WE USED THIS AS A FLAG TO PROTECT            
         XC    MGOSTAT,MGOSTAT     FIELDS IF MGX WAS THE ONLY VALID             
         MVC   MGOSTAT(8),=C'REJECTED'   ACTION                                 
*                                                                               
DNEW0305 EQU   *                                                                
         LA    R6,RMKGREC          GET MG REFERENCE ELEMENT                     
*                                                                               
         TM    RMKGRTS,X'20'       BONUS?                                       
         BZ    DNEW0308                                                         
         MVC   MGOMMSD(5),=C'BONUS'                                             
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         OI    TWAMKGFG,X'08'      BONUS                                        
         DROP  RF                                                               
*                                                                               
         B     DNEW0355                                                         
*                                                                               
DNEW0308 EQU   *                                                                
         MVI   ELCOD2,X'05'                                                     
         BAS   RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         USING RMKGMGEL,R6                                                      
         XC    WORK2,WORK2                                                      
         LA    R7,WORK2            SET A(MISSED DATE/SPOT)                      
DNEW0310 EQU   *                                                                
         GOTO1 DATCON,DMCB,(X'83',RMKGMGD1),(4,(R7))                            
         ZIC   RF,DMCB+4           GET LENGTH OF DATE                           
         AR    R7,RF               BUMP TO NEXT SPACE                           
*                                                                               
         OC    RMKGMGD2,RMKGMGD2                                                
         BZ    DNEW0315                                                         
         MVI   0(R7),C'-'                                                       
         AHI   R7,1                                                             
         GOTO1 DATCON,DMCB,(X'83',RMKGMGD2),(4,(R7))                            
         ZIC   RF,DMCB+4           GET LENGTH OF DATE                           
         AR    R7,RF               BUMP TO NEXT SPACE                           
*                                                                               
DNEW0315 EQU   *                                                                
         CLI   RMKGMGSP,1          ONE SPOT MISSED (DEFAULT)?                   
         BE    DNEW0340            YES - DON'T SHOW IT                          
         MVI   0(R7),C'('          NO  - INSERT LEFT PAREN                      
         LA    R7,1(R7)            BUMP PAST SEPARATOR                          
         EDIT  RMKGMGSP,(3,0(R7)),ALIGN=LEFT                                    
         LTR   R0,R0               ANY SIGNIFICANT VALUE?                       
         BNZ   DNEW0320            YES -                                        
         MVI   0(R7),C'0'          NO  - INSERT A ZERO                          
         LA    R7,1(R7)            MOVE PAST POSITION                           
         B     DNEW0330                                                         
DNEW0320 EQU   *                                                                
         AR    R7,R0               YES - ADD # OF SIGNIFICANT CHARS             
DNEW0330 EQU   *                                                                
         MVI   0(R7),C')'          INSERT RIGHT PAREN                           
         LA    R7,1(R7)                                                         
DNEW0340 EQU   *                                                                
         BAS   RE,NXTEL2           ANOTHER X'05' ELEMENT?                       
         BNE   DNEW0350            NO  - FINISHED                               
         MVI   0(R7),C','          YES - INSERT A SEPARATOR                     
         LA    R7,1(R7)            PASS THE SEPARATOR                           
         B     DNEW0310            GO BACK AND PROCESS IT                       
*                                                                               
         DROP  R6                                                               
*                                                                               
DNEW0350 EQU   *                                                                
         MVC   MGOMMSD,WORK2                                                    
*                                                                               
DNEW0355 EQU   *                                                                
         LA    R6,RMKGREC          GET CONTROL DESCRIPTION ELEMENT              
         MVI   ELCOD2,X'10'                                                     
         BAS   RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         LA    RF,RMKGCDDS-RMKGCDCD                                             
*                                  L(ELEMENT W/O DESCRIPTION)                   
         ZIC   RE,1(R6)            TOTAL LENGTH OF ELEMENT                      
         SR    RE,RF               CALC L(DESCRIPTION)                          
         BZ    DNEW0370            NO DESCRIPT - DON'T MOVE ANYTHING            
         BCTR  RE,0                MINUS 1 FOR EX STATEMENT                     
         EX    RE,DNEW0360         MOVE DESCRIPTION BY LENGTH                   
         B     DNEW0370                                                         
*                                                                               
DNEW0360 MVC   MGOMDES(0),RMKGCDDS-RMKGCDCD(R6)                                 
*                                                                               
DNEW0370 EQU   *                                                                
*                                  GET STATUS CONTROL ELEMENT                   
         ZIC   RF,CHOYSCTR         INCREMENT CHOICE COUNTER                     
         LA    RF,1(RF)                                                         
         STC   RF,CHOYSCTR         SAVE IT BACK                                 
*                                  GET STATUS CONTROL ELEMENT                   
         LA    R3,MGOMSTRH         DEFAULT NORMAL INTENSITY                     
         A     R3,OFFRDISP                                                      
         MVI   8(R3),C' '          DEFAULT NOT SELECTED (FOR 'OR')              
         NI    1(R3),X'FF'-X'08'                                                
         OI    6(R3),X'80'         SET TRANSMIT BIT                             
*                                                                               
         LA    R6,RMKGREC                                                       
         MVI   ELCOD2,X'20'                                                     
         BAS   RE,GETEL2                                                        
         BNE   DNEW0380            SHOULD REALLY ABORT...                       
         BE    *+6                                                              
         DC    H'0'                MUST BE PRESENT                              
         USING RMKGSTEL,R6                                                      
         TM    RMKGSTCH,X'01'      CHOSEN AS PARTICIPATING IN 'OR'?             
         BNO   DNEW0380            NO                                           
*                                                                               
         DROP  R6                                                               
*                                                                               
         ZIC   R6,CHOYSCTR         YES - INSERT CHOICE INTO DISPLAY             
         LA    R3,MGOMOPTH                                                      
         EDIT  (R6),(1,8(R3))                                                   
         OI    6(R3),X'80'         SET TRANSMIT BIT                             
         LA    R3,MGOMSTRH         SET FLAG AGAINST LINE #                      
         A     R3,OFFRDISP         ADD OFFSET TO CURRENT DISPLAY LINE           
         MVI   8(R3),C'*'          INSERT 'CHOSEN' INDICATOR                    
         OI    1(R3),X'08'         SET TO HI-INTENSITY                          
         OI    6(R3),X'80'         SET TRANSMIT BIT                             
DNEW0380 EQU   *                                                                
         LA    R3,MGOMDYSH         SET A(DAYS FIELD HEADER)                     
         A     R3,OFFRDISP         ADD OFFSET TO CURRENT DISPLAY LINE           
         OI    6(R3),X'80'         TURN ON TRANSMIT BIT                         
         LA    R3,8(R3)            POINT TO DATA ON LINE                        
*                                                                               
         LA    R6,RMKGREC                                                       
         TM    RMKGRTS,X'10'       PREEMPT?                                     
         BZ    DNEW0385                                                         
         MVC   0(7,R3),=C'PREEMPT'                                              
         B     DNEW1240                                                         
*                                                                               
DNEW0385 EQU   *                                                                
         LA    R7,MGOMTMSH         SET A(TIMES FIELD HEADER)                    
         A     R7,OFFRDISP                                                      
         OI    6(R7),X'80'         TURN ON TRANSMIT BIT                         
         LA    R7,8(R7)            POINT TO DATA ON LINE                        
         SR    R8,R8                                                            
         IC    R8,MGOMDYSH                                                      
         A     R8,OFFRDISP                                                      
         LA    R8,MGOMDYSH-1(R8)   DAY FIELD END-1                              
         SR    R9,R9                                                            
         IC    R9,MGOMTMSH                                                      
         A     R9,OFFRDISP                                                      
         LA    R9,MGOMTMSH-1(R9)   TIME FIELD END-1                             
*                                                                               
         LA    R6,RMKGREC                                                       
         MVI   ELCOD2,X'02'                                                     
         BAS   RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DNEW0390 DS    0H                                                               
         MVC   WORK,MYSPACE2                                                    
*                                  DISPLAY DAY-TIME FIELDS                      
         GOTO1 =V(OUTDAY),DMCB,3(R6),2(R6),WORK,RR=Y                            
         LA    RE,WORK                                                          
         SR    R4,R4                                                            
         CLI   0(RE),C' '                                                       
         BE    *+16                                                             
         LA    R4,1(R4)                                                         
         LA    RE,1(RE)                                                         
         B     *-16                                                             
*                                  ADD DAY LENGTH FIELD                         
         LA    RE,0(R4,R3)                                                      
         CR    RE,R8               ROOM LEFT?                                   
         BH    DNEW0400                                                         
         STC   R4,BYTE                                                          
         BCTR  R4,R0                                                            
         EX    R4,*+8              MOVE DAY                                     
         B     *+10                                                             
         MVC   0(0,R3),WORK                                                     
*                                                                               
*                                  TIME                                         
         MVI   WORK,C' '                                                        
         MVC   WORK+1(L'WORK-1),WORK                                            
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A11'                                           
         GOTO1 CALLOV,DMCB,0       GET ADDRESS OF UNTIME                        
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),4(R6),WORK                                             
         LA    RE,WORK             GET TIME LEN                                 
         SR    R2,R2                                                            
         CLI   0(RE),C' '                                                       
         BE    *+16                                                             
         LA    R2,1(R2)                                                         
         LA    RE,1(RE)                                                         
         B     *-16                                                             
         LA    RE,0(R2,R7)                                                      
         CR    RE,R9               ROOM LEFT?                                   
         BH    DNEW0400                                                         
         STC   R2,BYTE2                                                         
         BCTR  R2,R0                                                            
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),WORK                                                     
         BAS   RE,NXTEL2                                                        
         BNE   DNEW0400                                                         
         ZIC   R2,BYTE             DAY LENGTH                                   
         LA    R3,0(R2,R3)         NEXT DAY                                     
         ZIC   R2,BYTE2            TIME LARGER                                  
         LA    R7,0(R2,R7)         NEXT TIME                                    
         MVI   0(R3),C'*'          FIELD SEPARATOR                              
         MVI   0(R7),C'*'                                                       
         LA    R3,1(R3)                                                         
         LA    R7,1(R7)                                                         
         B     DNEW0390                                                         
         EJECT                                                                  
*                                  LENGTH                                       
DNEW0400 DS    0H                                                               
         MVC   HALF,RMKGDUR                                                     
         NI    HALF,X'7F'                                                       
         LA    R7,MGOMLEN          SET A(LENGTH FIELD)                          
         A     R7,OFFRDISP         ADD OFFSET TO CURRENT SCREEN LINE            
         EDIT  (2,HALF),(3,(R7)),ALIGN=LEFT                                     
         LA    R6,MGOMLENH                                                      
         A     R6,OFFRDISP         ADD OFFSET                                   
         OI    6(R6),X'80'         TURN ON TRANSMIT BIT                         
         STC   R0,5(R6)            NEW OUTPUT LENGTH.  THIS ACTUALLY            
*                                  BECOMES INPUT FOR REMAINING                  
*                                  COMBO CONTRACTS                              
         LR    RE,R0                                                            
         AR    RE,R7                                                            
         TM    RMKGDUR,X'80'       MINUTES?                                     
         BZ    DNEW0440                                                         
         MVI   0(RE),C'M'                                                       
*                                                                               
         ZIC   RF,5(R6)            ADJUST FOR NEW LENGTH                        
         LA    RF,1(RF)                                                         
         STC   RF,5(R6)                                                         
         EJECT                                                                  
*                                  DISPLAY CROSS DAY                            
DNEW0440 DS    0H                  CHECK PROFILE, OR IF DDS, DISPLAY            
         XC    MGOXDAY,MGOXDAY     CLEAR                                        
         LR    RF,RA               CROSS DAYS                                   
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BE    DNEW0480                                                         
                                                                                
         TM    PROFILES+CNTXDAYB,CNTXDAYA                                       
         BZ    DNEW0600            PROFILE SET TO USE THIS?                     
                                                                                
DNEW0480 DS    0H                                                               
         CLI   TWAECON,C'B'        IS IT EC/BIAS?                               
         BNE   DNEW0600                                                         
         DROP  RF                                                               
                                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCOD2,X'13'        EC BIAS ELEMENT                              
         BAS   RE,GETEL2                                                        
         BNE   DNEW0600                                                         
         USING RCONCCEL,R6                                                      
                                                                                
         MVC   MGOXDAY,=7C'.'      CLEAR                                        
         ZIC   RF,RCONCCCD         CROSS DAY BYTE                               
         SLL   RF,25                                                            
         LA    RE,MGOXDAY                                                       
         LA    R1,C'1'                                                          
                                                                                
DNEW0520 DS    0H                                                               
         LTR   RF,RF                                                            
         BNM   DNEW0560            IF DAY BIT IS ON                             
         STC   R1,0(RE)            PUT CORRESPONDING NUMBER THERE               
                                                                                
DNEW0560 DS    0H                                                               
         SLL   RF,1                NEXT DAY BIT                                 
         LA    RE,1(RE)            NEXT DAY DISPLAY POSITION                    
         LA    R1,1(R1)            NEXT EBCDIC DISPLAY NUMBER                   
         LA    R0,MGOXDAY+7        CHECK IF WE'VE LOOKED AT ALL 7 DAYS          
         CR    RE,R0                                                            
         BNH   DNEW0520                                                         
                                                                                
DNEW0600 DS    0H                                                               
         OI    MGOXDAYH+6,X'80'    XMIT IT                                      
         DROP  R6                                                               
         EJECT                                                                  
*              DISPLAY EFFECTIVE DATES                                          
         LA    R6,RMKGREC                                                       
         MVI   ELCOD2,X'03'                                                     
         BAS   RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R7,WORK2            OUTPUT                                       
         MVI   WORK2,C' '                                                       
*                                                                               
         MVC   WORK2+1(L'WORK2-1),WORK2                                         
         LA    R8,WORK2+60         OUTPUT END                                   
*                                                                               
         LA    R9,WORK2+120                                                     
DNEW0640 LA    R3,WORK+20                                                       
*                                  DISPLAY DATES                                
         GOTO1 DATCON,DMCB,(3,2(R6)),(4,(R3))                                   
*                                  START DATE                                   
         CLC   2(3,R6),5(R6)                                                    
         BNE   *+12                                                             
         LA    R3,5(R3)                                                         
         B     DNEW0680                                                         
*                                                                               
         MVI   5(R3),C'-'                                                       
*                                                                               
         GOTO1 (RF),(R1),(3,5(R6)),(4,6(R3))                                    
*                                  END DATE                                     
         LA    R3,11(R3)                                                        
*                                                                               
DNEW0680 TM    8(R6),X'40'         ALT WEEKS?                                   
         BZ    *+12                                                             
         MVI   0(R3),C'A'                                                       
         LA    R3,1(R3)                                                         
*                                                                               
DNEW0720 LA    RE,WORK+20                                                       
         SR    R3,RE               GET ELEM DISPLAY LEN                         
         LR    RF,R7                                                            
         AR    RF,R3               OUTPUT PTR                                   
*                                  ROOM IN FIRST LINE?                          
         CR    RF,R8               WORK2+60                                     
         BNH   DNEW0760            NO  - START AT SECOND LINE                   
*                                                                               
         LA    R8,500(R8)          ELIM. FIRST TEST                             
         LA    R7,WORK2+60         START 2D LINE                                
         CLI   WORK2+60,C'*'                                                    
         BNE   DNEW0800                                                         
         LA    R7,1(R7)                                                         
         B     DNEW0800                                                         
*                                                                               
DNEW0760 CR    RF,R9               WORK2+120                                    
         BH    DNEW0840            DOESN'T FIT                                  
*                                                                               
DNEW0800 BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),WORK+20                                                  
         MVC   WORK+20(20),MYSPACE2                                             
         LA    R7,1(R3,R7)         OUTPUT PTR                                   
         BAS   RE,NXTEL2                                                        
         BNE   DNEW1040                                                         
*                                                                               
         MVI   0(R7),C'*'                                                       
         LA    R7,1(R7)                                                         
         B     DNEW0640                                                         
*                                  DATES DON'T FIT  - TRY TO                    
*                                     COMPRESS  - DISPLAY WEEKS AS              
*                                     END DATE                                  
DNEW0840 LA    R6,RMKGREC                                                       
         MVI   ELCOD2,X'03'                                                     
         BAS   RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   WORK2,C' '                                                       
         MVC   WORK2+1(L'WORK2-1),WORK2                                         
         LA    R3,WORK2                                                         
*                                                                               
DNEW0880 GOTO1 DATCON,(R1),(3,2(R6)),(4,(R3))                                   
         LA    R7,5                                                             
         CLI   3(R3),C'0'          DAY =JAN01?                                  
         BNE   DNEW0920                                                         
         MVC   3(1,R3),4(R3)       COMPRESS TO JAN1                             
         MVI   4(R3),C' '                                                       
         BCTR  R7,R0                                                            
DNEW0920 AR    R3,R7                                                            
*                                                                               
         CLI   10(R6),1            1 WEEK?                                      
         BE    DNEW0960                                                         
         MVI   0(R3),C'-'                                                       
         LA    R3,1(R3)                                                         
*                                  DISPLAY WEEKS                                
         EDIT  (1,10(R6)),(2,(R3)),ALIGN=LEFT                                   
         CLI   10(R6),9                                                         
         BH    *+16                                                             
         MVI   1(R3),C'W'                                                       
         LA    R3,2(R3)                                                         
         B     *+12                                                             
         MVI   2(R3),C'W'                                                       
         LA    R3,3(R3)                                                         
         TM    8(R6),X'40'         ALT WEEKS?                                   
         BZ    *+12                                                             
         MVI   0(R3),C'A'                                                       
         LA    R3,1(R3)                                                         
*                                                                               
DNEW0960 EQU   *                                                                
*                                  GET NEXT ELEM                                
DNEW1000 DS    0H                                                               
         BAS   RE,NXTEL2                                                        
         BNE   DNEW1040                                                         
         MVI   0(R3),C'*'                                                       
         LA    R3,1(R3)                                                         
         B     DNEW0880                                                         
DNEW1040 EQU   *                                                                
         LA    R7,MGOMDTS          SET A(DATES FIELD)                           
         A     R7,OFFRDISP         ADD OFFSET TO SCREEN LINE                    
         MVC   0(24,R7),WORK2      INSERT DATES ONTO SCREEN                     
         LA    R7,MGOMDTSH         SET A(DATES HEADER FIELD)                    
         A     R7,OFFRDISP         ADD OFFSET TO SCREEN LINE                    
         OI    6(R7),X'80'         SET TRANSMIT BIT                             
*                                                                               
         LA    R7,MGOMSPT          SET A(#SPOTS FIELD)                          
         A     R7,OFFRDISP         ADD OFFSET TO SCREEN LINE                    
         EDIT  RMKGNW,(2,(R7)),ALIGN=LEFT,ZERO=NOBLANK                          
*                                  INSERT # SPOTS ONTO SCREEN                   
         LA    R7,MGOMSPTH         SET A(#SPOTS HEADER FIELD)                   
         A     R7,OFFRDISP         ADD OFFSET TO SCREEN LINE                    
         OI    6(R7),X'80'         SET TRANSMIT BIT                             
*                                                                               
DNEW1200 DS    0H                                                               
         LA    R7,MGOMRAT                                                       
         A     R7,OFFRDISP         ADD OFFSET TO SCREEN LINE                    
         EDIT  (4,RMKGCOS),(9,(R7)),2,ALIGN=LEFT,FLOAT=-                        
*                                                                               
         LA    R7,MGOMRATH         SET A(RATE HEADER FIELD)                     
         A     R7,OFFRDISP         ADD OFFSET TO SCREEN LINE                    
         OI    6(R7),X'80'         SET TRANSMIT BIT                             
DNEW1240 DS    0H                                                               
         LA    R6,RMKGREC          FIND PROGRAM NAME ELEMENT                    
         MVI   ELCOD2,X'21'                                                     
         BAS   RE,GETEL2                                                        
         BNE   DNEW1250            NOT FOUND                                    
         LA    R7,MGOOPGN                                                       
         A     R7,OFFRDISP         ADD OFFSET TO SCREEN LINE                    
         ZIC   RF,1(R6)            DERIVE COMMENT LENGTH                        
         SH    RF,=H'3'            SUBTRACT 2 FOR CONTROL, 1 FOR EX             
*MN                                                                             
         CLI   1(R6),20                                                         
         BL    *+8                                                              
         LA    RF,16                                                            
*MN                                                                             
         EX    RF,DNEW1260         MOVE TO SCREEN BY LENGTH                     
         B     DNEW1250                                                         
DNEW1250 DS    0H                                                               
         LA    R6,RMKGREC          FIND DETAIL COMMENT                          
         MVI   ELCOD2,X'11'                                                     
         BAS   RE,GETEL2                                                        
         BNE   DNEW1300            NOT FOUND                                    
         LA    R7,MGODCOM                                                       
         A     R7,OFFRDISP         ADD OFFSET TO SCREEN LINE                    
         ZIC   RF,1(R6)            DERIVE COMMENT LENGTH                        
         SH    RF,=H'2'            SUBTRACT 2 FOR CONTROL, 1 FOR EX             
         CHI   RF,L'MGODCOM                                                     
         BL    *+8                                                              
         LHI   RF,L'MGODCOM                                                     
*                                                                               
         SHI   RF,1                                                             
         EX    RF,DNEW1260         MOVE TO SCREEN BY LENGTH                     
         B     DNEW1300                                                         
DNEW1300 DS    0H                                                               
         LA    R6,RMKGREC          FIND DEMO VALUE ELEMENT                      
         MVI   ELCOD2,X'0E'                                                     
         BAS   RE,GETEL2                                                        
         BNE   DNEW1600            NOT FOUND                                    
*                                                                               
         USING RMKGDMEL,R6                                                      
         LA    R0,RMKGDMCV         R0->FIRST DEMO                               
         GOTOR LOOPDEM             LOOP UNTIL WE FIND A PRIMARY                 
         BE    *+6                                                              
         LR    R6,R0               USE FIRST IF THERE IS NO PRIMARY             
         DROP  R6                                                               
*                                                                               
         USING RMKGDMCV,R6                                                      
         LA    R7,MGOODEM                                                       
         A     R7,OFFRDISP         ADD OFFSET TO SCREEN LINE                    
         CLC   RMKGDMDM,=F'-1'     NO DEMO INPUT                                
         BE    DNEW1600                                                         
         ZAP   WORK(5),=P'0'                                                    
         MVO   WORK(5),RMKGDMDM                                                 
         EDIT  (P5,WORK),(8,0(R7)),1,ALIGN=LEFT,ZERO=NOBLANK                    
         DROP  R6                                                               
         B     DNEW1600                                                         
DNEW1260 EQU   *                                                                
         MVC   0(0,R7),2(R6)                                                    
*                                                                               
*                                                                               
DNEW1600 EQU   *                                                                
         L     RF,OFFRDISP         BUMP SCREEN DISPLACEMENT VALUE               
         LA    RF,MGOMDY2H-MGOMDYSH(RF)                                         
         ST    RF,OFFRDISP         PUT IT BACK                                  
         LA    RE,MGOLAST          CHECK IF PAST END OF SCREEN                  
         CR    RF,RE                                                            
         BNL   DNEW1700            PASSED END - END IT HERE!                    
         GOTO1 VSEQ                READ NEXT RECORD                             
         CLC   KEY(26),SAVMGKEY    SAME SET OF MG RECORDS?                      
         BNE   DNEW1700            YES - GO BACK AND ADD TO SCREEN              
         GOTO1 VGETREC,DMCB,IOAREA                                              
         GOTO1 VMOVEREC,DMCB,IOAREA,RMKGREC                                     
         B     DNEW0370            YES - GO BACK AND ADD TO SCREEN              
*                                     CAN ONLY BE 3 IN SET                      
DNEW1700 EQU   *                                                                
         XIT1                      EXIT SUBROUTINE                              
         EJECT                                                                  
*                                                                               
*   GRPCOMNT:  RETRIEVE GROUP COMMENT RECORD, INSERT COMMENT INTO               
*        SCREEN.                                                                
*                                                                               
GRPCOMNT NTR1                                                                   
         LA    R2,MGOGCOMH         SET A(GROUP COMMENT FIELD)                   
         XC    KEY,KEY                                                          
         MVC   KEY(27),RMKGREC     INSERT KEY OF SELECTED REC                   
         XC    KEY+21(6),KEY+21    CLEAR LOW ORDER OF KEY                       
         GOTO1 VHIGH               READ KEY                                     
         CLC   KEY(27),KEYSAVE     GROUP COMMENT RECORD FOUND?                  
         BNE   GCOMX               NO                                           
*                                                                               
* DDS SHOW D/A                                                                  
*                                                                               
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BNE   GCOM0005                                                         
         GOTO1 HEXOUT,DMCB,KEY+28,MGODKDA,4,=C'TOG'                             
*                                                                               
GCOM0005 DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   TWAMKGDH,KEY+28     SAVE OFF HEADER D/A                          
         DROP  RF                                                               
*                                                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                  DON'T MOVE THE RECORD TO RMKGREC:            
         LA    R6,IOAREA                                                        
         USING RMKGREC,R6                                                       
*&&DO                                                                           
         CLI   TWAACCS,C'$'        CHECK IF USER IS ALSO THE OFFERER            
         BE    GCOM0006                                                         
         CLI   MGOCREA,C'R'                                                     
         BE    GCOM0009                                                         
*                                                                               
GCOM0006 EQU   *                                                                
         CLI   MGOCREA,C'R'                                                     
         BNE   GCOM0009                                                         
*                                                                               
         LA    R4,MGOMPADH         SET PROTECTED BITS ON ALL FIELDS             
         LA    R5,MGODCM3H         IF USER IS NOT THE OFFERER                   
*                                                                               
GCOM0008 EQU   *                                                                
         OI    1(R4),X'20'         PROTECTED BIT                                
         ZIC   R3,0(R4)            FIELD LEN                                    
         LA    R4,0(R3,R4)         NEXT FIELD                                   
         CR    R4,R5                                                            
         BNH   GCOM0008                                                         
*&&                                                                             
*                                  THIS IS SET FOR OTHER PURPOSES               
* DISPLAY STATUS                                                                
*                                                                               
GCOM0009 EQU   *                                                                
         XC    MGOSTAT,MGOSTAT                                                  
         MVC   MGOSTAT(3),=C'NEW'                                               
* APPLIED ?                                                                     
         TM    RMKGSCST,RMKGSAPQ                                                
         BZ    GCOM009A                                                         
         MVC   MGOSTAT(7),=C'APPLIED'                                           
         TM    RMKGSFG3,RMGF3SAQ                                                
         BZ    GCOM0020                                                         
         TM    RMKGSFG3,RMGF3ARQ                                                
         BO    GCOM0020                                                         
         MVC   MGOSTAT(7),=C'SELFAPP'                                           
         DROP  R6                                                               
*                                                                               
         LA    R3,RCONREC                                                       
         USING RCONREC,R3                                                       
         LA    R6,IOAREA                                                        
         USING RMKGREC,R6                                                       
         CLC   RMKGAPMN,RCONMOD                                                 
         BNL   GCOM0020                                                         
         MVI   MGOSTAT+7,C'*'                                                   
         B     GCOM0020                                                         
         DROP  R3                                                               
* RECALLED ?                                                                    
GCOM009A EQU   *                                                                
         TM    RMKGSCST,RMKGSRCQ                                                
         BZ    *+14                                                             
         MVC   MGOSTAT(8),=C'RECALLED'                                          
         B     GCOM0020                                                         
* REJECTED ?                                                                    
         TM    RMKGSCST,RMKGSRJQ                                                
         BZ    GCOM0010                                                         
         MVC   MGOSTAT(8),=C'REJECTED'                                          
         TM    RMKGSFG1,RMGF1MCN   SET IF DARE AND CANCELLED TO AGY             
         BZ    GCOM0020                                                         
         MVC   MGOSTAT(8),=C'DARECAND'                                          
         B     GCOM0020                                                         
* REVISED ?                                                                     
GCOM0010 DS    0H                                                               
         TM    RMKGSCST,RMKGSRVQ                                                
         BZ    GCOM0015                                                         
         MVC   MGOSTAT(7),=C'REVISED'                                           
         B     GCOM0020                                                         
*                                                                               
GCOM0015 DS    0H                                                               
         TM    RMKGSCST,RMKGSCNQ                                                
         BZ    GCOM0020                                                         
         MVC   MGOSTAT(6),=C'MGXSEL'                                            
*                                                                               
GCOM0020 DS    0H                                                               
*        OI    6(R2),X'80'         SET FIELD TO TRANSMIT                        
*                                                                               
GCOM0040 DS    0H                  CREATED BY REP/STATION                       
         MVC   MGOCREA,=C'Rep'                                                  
         TM    RMKGSCST,RMKGSCRQ                                                
         BO    *+10                                                             
         MVC   MGOCREA,=C'Sta'                                                  
                                                                                
         TM    RMKGSCST,RMKGSPAQ                                                
         BZ    GCOM0050                                                         
         MVI   MGOMPAD,C'Y'                                                     
         MVI   MGOMAOK,0                                                        
         B     GCOM0060                                                         
                                                                                
GCOM0050 DS    0H                                                               
         MVI   MGOMPAD,0                                                        
         MVI   MGOMAOK,C'Y'                                                     
*                                                                               
* EXPIRATION DATE                                                               
*                                                                               
GCOM0060 DS    0H                                                               
         GOTO1 DATCON,DMCB,(2,RMKGSEXD),(5,MGOEXPD)                             
*                                                                               
* WIP STATUS                                                                    
*                                                                               
         XC    MGORSWP,MGORSWP                                                  
         USING RMKGREC,R6                                                       
         MVC   MGOUPBY,=C'Rep'                                                  
         TM    RMKGSFG2,RMGF2RPQ                                                
         BO    GCOM0070                                                         
         MVC   MGOUPBY,=C'Sta'                                                  
*                                                                               
GCOM0070 DS    0H                                                               
         TM    RMKGSFG2,RMGF2WPQ                                                
         BZ    GCOM0080                                                         
         DROP  R6                                                               
*                                                                               
         TM    RCONMODR+1,X'40'    DON'T SHOW WIP FOR GRAPHNET                  
         BO    GCOM0080                                                         
*                                                                               
         USING RMKGREC,R6                                                       
         TM    RMKGSCST,RMKGSAPQ   DON'T SHOW WIP IF APPLIED                    
         BO    GCOM0080                                                         
         MVC   MGORSWP(3),=C'WIP'                                               
*                                                                               
* LAST ACTIVITY DATE/TIME                                                       
*                                                                               
GCOM0080 DS    0H                                                               
         GOTO1 DATCON,DMCB,(2,RMKGSLAD),(5,MGOUPDT)                             
         GOTO1 HEXOUT,DMCB,RMKGSLAT,MGOUPTM+1,2,=C'TOG'                         
         MVC   MGOUPTM(2),MGOUPTM+1                                             
         MVI   MGOUPTM+2,C':'                                                   
*                                                                               
* CREATION DATE/TIME                                                            
*                                                                               
         GOTO1 DATCON,DMCB,(2,RMKGSCRD),(5,MGOOFDT)                             
         GOTO1 HEXOUT,DMCB,RMKGSCRT,MGOOFTM+1,2,=C'TOG'                         
         MVC   MGOOFTM(2),MGOOFTM+1                                             
         MVI   MGOOFTM+2,C':'                                                   
         DROP  R6                                                               
                                                                                
         LA    R6,IOAREA                                                        
         MVI   ELCOD2,X'10'                                                     
         BAS   RE,GETEL2                                                        
         BNE   GCOMX                                                            
         USING RMKGGCEM,R6                                                      
         ZIC   RF,RMKGGCLN         GET LENGTH OF ELEMENT                        
         SH    RF,=H'3'            MINUS 2 FOR CONTROL, 1 FOR EX                
         EX    RF,*+8              MOVE COMMENT BY LENGTH                       
         B     *+10                                                             
         MVC   8(0,R2),RMKGGCCM    MOVE COMMENT BY LENGTH                       
         DROP  R6                                                               
*                                                                               
GCOMX    EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
GETEL2   EQU   *                                                                
         LA    R6,34(R6)           PASS THE KEY                                 
GETEL2A  EQU   *                                                                
         CLI   0(R6),0                                                          
         BE    GETEL2X                                                          
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLC   ELCOD2,0(R6)                                                     
         BER   RE                  RETURN WITH CC =                             
         B     GETEL2A                                                          
GETEL2X  LTR   RE,RE                                                            
         BR    RE                  RETURN WITH CC NOT =                         
*                                                                               
*                                                                               
NXTEL2   SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
FSTEL2   CLI   0(R6),0                                                          
         BE    NXTEL2X                                                          
         CLC   0(1,R6),ELCOD2                                                   
         BNE   NXTEL2                                                           
         BR    RE                                                               
*                                                                               
NXTEL2X  LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                               
*                                                                               
ELCOD2   DS    CL1                                                              
MYSPACE2 DC    60C' '                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
******************************************************************              
*   DISPCON:   DISPLAY CONTRACT PORTION OF SCREEN FROM CONTRACT                 
*        RECORD.  THIS CODE HAS BEEN LIFTED FROM THE RECNT20 MOD.               
*                                                                               
T80229   CSECT                                                                  
         DS    0F                                                               
DISPCON  NMOD1 0,*DCON*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         USING GENOLD,RC                                                        
         L     RA,4(R1)            RESET A(TWASPACE)                            
         USING TWAD,RA                                                          
         LR    R8,RA                                                            
         AH    R8,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R8                                                       
*                                                                               
*              BUILD AGENCY                                                     
         XC    IOAREA(32),IOAREA                                                
         MVI   RAGYKTYP,10         AGENCY REC TYPE                              
         MVC   RAGYKAGY,RCONKAGY   AGY                                          
         MVC   RAGYKAOF,RCONKAOF   OFFICE                                       
         MVC   RAGYKREP,REPALPHA                                                
*                                                                               
         MVC   KEY,IOAREA                                                       
         MVC   RAGYNAM1,WAGYEXP                                                 
*                                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   DDIS0120                                                         
*                                                                               
DDIS0040 GOTO1 VGETREC,DMCB,IOAREA                                              
         MVC   TWAAGNM1,RAGYNAM1                                                
         MVC   TWAAGNM2,RAGYNAM2                                                
***>     MVC   TWAAGAD1,RAGYADD1                                                
***>     MVC   TWAAGAD2,RAGYADD2                                                
         MVC   TWAAGSTT,RAGYSTAT                                                
         MVC   TWAAGZIP,RAGYZIP                                                 
         MVC   TWAAEASY,RAGYPRO2   PROFILE FOR EASYLINK AGY COPY                
         MVC   TWAARISK,RAGYRISK   AGENCY CREDIT RISK                           
         MVC   TWAALIAB,RAGYLIAB   AGENCY LIABILITY POSITION                    
*                                                                               
DDIS0060 MVC   MGOAGY(4),RAGYKAGY  AGENCY                                       
         CLC   RAGYKAOF,MYSPACE3   OFFICE?                                      
         BE    DDIS0080                                                         
         LA    RE,MGOAGY                                                        
         MVI   MGOAGY+4,C' '                                                    
         CLI   0(RE),C' '                                                       
         BE    *+12                                                             
         LA    RE,1(RE)                                                         
         B     *-12                                                             
         MVI   0(RE),C'-'                                                       
         MVC   1(2,RE),RAGYKAOF    AGENCY OFFICE                                
DDIS0080 DS    0H                                                               
         MVC   MGOAGYN,RAGYNAM1                                                 
*                                                                               
*                                                                               
DDIS0120 DS    0H                                                               
*              ADVERTISER                                                       
DDIS0180 XC    IOAREA(32),IOAREA                                                
         MVI   RADVKTYP,8                                                       
         MVC   RADVKADV,RCONKADV   ADVERTISER                                   
         MVC   RADVKREP,REPALPHA                                                
*                                                                               
         MVC   KEY,IOAREA                                                       
         MVC   RADVNAME,WADVEXP                                                 
         OC    WADVEXP,WADVEXP                                                  
         BNZ   DDIS0220                                                         
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   DDIS0240                                                         
*                                                                               
DDIS0200 GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
DDIS0220 MVC   MGOADV(4),RADVKADV                                               
         MVC   MGOADVN,RADVNAME                                                 
         EJECT                                                                  
*              STATION                                                          
DDIS0240 XC    IOAREA(32),IOAREA   BUILD STATION KEY                            
         MVI   RSTAKTYP,2                                                       
         MVC   RSTAKREP,REPALPHA                                                
         MVC   RSTAKSTA,RCONKSTA                                                
         MVC   RSTAMKT,WSTAEXP                                                  
         OC    WSTAEXP,WSTAEXP                                                  
         BNZ   DDIS0300                                                         
*                                                                               
         MVC   KEY,IOAREA                                                       
         GOTO1 VREAD                                                            
*                                                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
                                                                                
DDIS0300 DS    0H                                                               
         MVC   MGOSTAM,RSTAMKT                                                  
*                                                                               
         MVC   WORK(4),RSTAKSTA                                                 
*                                                                               
         MVC   WORK+4(3),=C'-FM'                                                
         CLI   RSTAKSTA+4,C'F'                                                  
         BE    DDIS0320                                                         
*                                                                               
         MVI   WORK+5,C'A'                                                      
         CLI   RSTAKSTA+4,C'A'                                                  
         BE    DDIS0320                                                         
*                                                                               
         MVI   WORK+5,C'C'                                                      
         CLI   RSTAKSTA+4,C'C'                                                  
         BE    DDIS0320                                                         
*                                                                               
         MVC   WORK+5(2),=C'L '                                                 
         CLI   RSTAKSTA+4,C'L'                                                  
         BE    DDIS0320                                                         
*                                                                               
         MVC   WORK+5(2),=C'TV'                                                 
DDIS0320 CLI   WORK+3,C' '                                                      
         BNE   *+14                                                             
         MVC   WORK+3(3),WORK+4                                                 
         MVI   WORK+6,C' '                                                      
*                                                                               
         MVC   MGOSTA(7),WORK      STATION                                      
*                                                                               
DDIS0380 CLC   RCONPRD,MYSPACE3    PRODUCT CODE?                                
         BE    DDIS0440                                                         
* GET PRODUCT RECORD                                                            
         XC    IOAREA(32),IOAREA                                                
         MVI   RPRDKTYP,9                                                       
         MVC   RPRDKADV,RCONKADV                                                
         MVC   RPRDKPRD,RCONPRD                                                 
         MVC   RPRDKREP,REPALPHA                                                
         MVC   KEY,IOAREA                                                       
         MVC   RPRDNAME,WPRDEXP                                                 
         OC    WPRDEXP,WPRDEXP     PRODUCT LOOKED UP ALREADY?                   
         BZ    DDIS0400                                                         
         MVC   MGOPRD(2),=C'C='                                                 
         MVC   MGOPRD+2(3),RCONPRD                                              
         MVI   MGOPRD+5,0                                                       
         MVC   MGOPRD+6(14),RPRDNAME                                            
         B     DDIS0460                                                         
*                                                                               
DDIS0400 GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   DDIS0460                                                         
*                                                                               
DDIS0420 GOTO1 VGETREC,DMCB,IOAREA                                              
         MVC   TWAPRDNM,RPRDNAME                                                
         MVC   MGOPRD(2),=C'C='                                                 
         MVC   MGOPRD+2(3),RCONPRD                                              
         MVI   MGOPRD+5,0                                                       
         MVC   MGOPRD+6(14),RPRDNAME                                            
         B     DDIS0460                                                         
*              FIND PRODUCT ELEMENT                                             
DDIS0440 DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCOD3,X'05'                                                     
         BAS   RE,GETEL3                                                        
         BE    *+6                                                              
         DC    H'0'                NO X'05'                                     
         MVC   MGOPRD,2(R6)        PRODUCT EXPANSION                            
         EJECT                                                                  
*              GET SALESMAN                                                     
DDIS0460 XC    IOAREA(32),IOAREA                                                
         MVI   RSALKTYP,6          KEY TYPE - SALESMAN RECORD                   
         MVC   RSALKREP,REPALPHA   REP                                          
         MVC   RSALKSAL,RCONSAL    SALESMAN CODE                                
         MVC   KEY,IOAREA                                                       
         MVC   RSALNAME,WSALEXP                                                 
         OC    WSALEXP,WSALEXP                                                  
         BNZ   DDIS0480                                                         
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCOD3,X'9F'                                                     
         BAS   RE,GETEL3                                                        
         BNE   DDIS0480                                                         
         MVC   TWASALAS,22(R6)     SALES ASSISTANT                              
*                                                                               
DDIS0480 DC    0H'0'                                                            
         MVC   MGOSAL(3),RSALKSAL  SALESMAN CODE                                
         MVC   MGOSALN,RSALNAME                                                 
*                                                                               
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),REPALPHA                                                
         GOTO1 (RFKFLT,VREPFACS),DMCB,(X'80',RCONREC),MGOODTS,0,DUB             
*                                                                               
* SET LENGTH OF DATES FOR RE-INPUT (ADDR)                                       
         MVI   MGOODTSH+5,17                                                    
*                                                                               
         XMOD1                                                                  
*                                                                               
         EJECT                                                                  
*                                                                               
GETEL3   EQU   *                                                                
         LA    R6,34(R6)           PASS THE KEY                                 
GETEL3A  EQU   *                                                                
         CLI   0(R6),0                                                          
         BE    GETEL3X                                                          
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLC   ELCOD3,0(R6)                                                     
         BER   RE                  RETURN WITH CC =                             
         B     GETEL3A                                                          
GETEL3X  LTR   RE,RE                                                            
         BR    RE                  RETURN WITH CC NOT =                         
*                                                                               
*                                                                               
NXTEL3   SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
FSTEL3   CLI   0(R6),0                                                          
         BE    NXTEL3X                                                          
         CLC   0(1,R6),ELCOD3                                                   
         BNE   NXTEL3                                                           
         BR    RE                                                               
*                                                                               
NXTEL3X  LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                               
*                                                                               
ELCOD3   DS    CL1                                                              
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
*   REPSIDE:   TURN ON PROTECT BITS ON ALL SCREEN FIELDS BUT DETAIL             
*        COMMENT.  ACCESS TO THIS MODULE IS CONTROLLED FROM RECNT2B.            
*                                                                               
T80229   CSECT                                                                  
         DS    0F                                                               
REPSIDE  NMOD1 0,*RSID*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         USING GENOLD,RC                                                        
         L     RA,4(R1)            RESET A(TWASPACE)                            
         USING TWAD,RA                                                          
         LR    R8,RA                                                            
         AH    R8,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R8                                                       
         LA    R2,FIELDTBL         SET A(SCREEN FIELD TABLE)                    
RSID0040 EQU   *                                                                
         CLI   0(R2),X'FF'         TABLE DELIMITER REACHED?                     
         BE    RSID0200            YES - RESTRICTED FIELDS PROTECTED            
*                                     FROM REP SIDE                             
         TM    4(R2),X'80'         FIELD ALLOWS CHANGE?                         
         BO    RSID0120            YES - GO TO NEXT FIELD                       
         MVC   DUB(4),0(R2)        NO  - GET A(FIELD HDR)                       
         L     R3,DUB                                                           
         LA    RF,TWAD             SET ADDRESSABILITY                           
         AR    R3,RF                                                            
         OI    1(R3),X'20'         SET FIELD TO 'PROTECTED'                     
         OI    6(R3),X'80'         SET FIELD TO 'TRANSMIT'                      
         TM    4(R2),X'01'         FIELD OCCURS AS SET OF 3?                    
         BNO   RSID0120            NO                                           
         LA    RF,2                YES - SET OTHER OCCURRENCES                  
RSID0080 EQU   *                                                                
         LA    R3,MGOMDY2H-MGOMDYSH(R3)                                         
*                                  BUMP TO NEXT OCCURRENCE OF FIELD             
         OI    1(R3),X'20'         SET FIELD TO 'PROTECTED'                     
         OI    6(R3),X'80'         SET FIELD TO 'TRANSMIT'                      
         BCT   RF,RSID0080         LOOP THROUGH SET                             
RSID0120 EQU   *                                                                
         LA    R2,FIELDTB2-FIELDTBL(R2)                                         
*                                  BUMP TO NEXT ENTRY                           
         B     RSID0040            GO BACK FOR NEXT FIELD                       
RSID0200 EQU   *                                                                
         DROP  R8                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   CONTRACT SCREEN FIELD TABLE CONTAINS ADDRESS OF EACH SCREEN                 
*        FIELD HEADER, AND A FLAG BYTE.  IF FIELD CAN BE CHANGED,               
*        FLAG BYTE IS SET TO X'80'.   IF FIELD OCCURS AS A SET,                 
*        FLAG BYTE IS SET TO X'01'.                                             
*                                                                               
FIELDTBL DC    AL4(MGOMOPTH-TWAD),X'00'                                         
FIELDTB2 DC    AL4(MGOGCOMH-TWAD),X'00'                                         
         DC    AL4(MGOMMSDH-TWAD),X'00'                                         
         DC    AL4(MGOMDESH-TWAD),X'00'                                         
*                                                                               
*   NOTE:  FROM THIS POINT ON, THE FOLLOWING FIELDS OCCUR AS A SET              
*        THREE TIMES.  SETS ARE INTERVALED BY THE VALUE OF                      
*        MGOMDY2H-MGOMDYSH.                                                     
*                                                                               
         DC    AL4(MGOMDYSH-TWAD),X'01'                                         
         DC    AL4(MGOMTMSH-TWAD),X'01'                                         
         DC    AL4(MGOMDTSH-TWAD),X'01'                                         
         DC    AL4(MGOMRATH-TWAD),X'01'                                         
         DC    AL4(MGOMLENH-TWAD),X'01'                                         
         DC    AL4(MGODCOMH-TWAD),X'81'                                         
         DC    X'FF'                                                            
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
* CHECK STATION PROFILES TO SEE IF WE NEED TO DISPLAY DEMO OR NOT               
*                                                                               
CKSTPROF NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   TWAACCS,C'$'                                                     
         BNE   CKSTPRY                                                          
*                                                                               
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
*                                                                               
         TM    PROFILES+CNTDEMOB,CNTDEMOA                                       
         BO    CKST0100            DISALLOW STATION TO SEE DEMO                 
*                                                                               
         TM    TWASTAOB,X'02'                                                   
         BO    CKSTPRN             DO NOT PROCESS DEMO                          
         B     CKSTPRY                                                          
CKST0100 DS    0H                                                               
         TM    TWASTAOB,X'02'                                                   
         BZ    CKSTPRN                                                          
         DROP  RF                                                               
*                                                                               
CKSTPRY  SR    RC,RC                                                            
CKSTPRN  LTR   RC,RC                                                            
CKSTPRX  DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
DISDEM   NTR1  BASE=*,LABEL=*                                                   
         LA    R2,MGODEMLH                                                      
*                                                                               
         LR    R5,RA                                                            
         AH    R5,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R5                                                       
         XC    TWADEM,TWADEM                                                    
         DROP  R5                                                               
*                                                                               
         GOTOR PICKDEM             READ 0D, 0E ELT                              
         BE    DISD0402                                                         
*                                                                               
DISD0010 DS    0H                  READ FROM PENDING IF NO 0D, 0E               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'DD'        NO CON DEMO, LOOK UP PENDING                 
         BRAS  RE,GETEL4                                                        
         BNE   DISD0300                                                         
         CLI   1(R6),18            OLD DEMO FORMAT                              
         BE    DISD0300            YES, SKIP                                    
         ZIC   R1,1(R6)            IF ALL SPACES SKIP                           
         SHI   R1,3                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   2(0,R6),MYSPACE3                                                 
         BE    DISD0300                                                         
*                                                                               
DISD0040 DS    0H                                                               
         USING RCONDDEL,R6                                                      
         ZIC   R5,1(R6)                                                         
         CHI   R5,2                                                             
         BNH   DISD0300                                                         
         AR    R5,R6                                                            
         LA    R4,RCONDDCT         FIND PRIMARY DEMO                            
DISD0135 DS    0H                                                               
         CLI   0(R4),C'('          USER DEFINED DEMO?                           
         BE    DISD0138                                                         
         TM    0(R4),X'40'                                                      
         BO    DISD0140                                                         
DISD0138 DS    0H                                                               
         LA    R4,L'RCONDDCT(R4)                                                
         CR    R4,R5                                                            
         BL    DISD0135                                                         
         LA    R4,2(R6)            USE FIRST ONE IF NO PRIMARY FOUND            
*                                                                               
DISD0140 DS    0H                                                               
         XC    WORK(30),WORK                                                    
         MVC   WORK(L'RCONDDCT),0(R4)                                           
         B     DISD0450                                                         
         DROP  R6                                                               
*                                                                               
*                                                                               
DISD0300 DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'12'                                                     
         BRAS  RE,GETEL4                                                        
         BNE   DISD0335                                                         
         USING RSARXEL,R6                                                       
         MVC   WORK(3),RSARXDEM    DEFAULT                                      
         MVC   WORK+3(4),=F'-1'                                                 
         LA    R3,RSARXDEM                                                      
         LA    R4,8                                                             
*                                                                               
DISD0315 DS    0H                                                               
         TM    0(R3),X'40'         PRIMARY?                                     
         BO    DISD0320                                                         
         AHI   R3,3                                                             
         BCT   R4,DISD0315                                                      
         OC    WORK(3),WORK        IF THERE IS NO DEMO                          
         BZ    DISD0335                                                         
         B     DISD0450                                                         
*                                                                               
DISD0320 DS    0H                                                               
         MVC   WORK(3),0(R3)       PRIMARY FOUND                                
         B     DISD0450                                                         
*                                                                               
DISD0335 DS    0H                                                               
         MVC   8(7,R2),=C'NO DEMO'                                              
         B     DISD0415            PROTECT SCREEN                               
*                                                                               
*                                                                               
DISD0402 DS    0H                   R6->DEMO CAT TO BE USED                     
         XC    WORK(30),WORK                                                    
         USING RBUYDMCV,R6                                                      
         OC    RBUYDMCT,RBUYDMCT   HANDLE  NULL                                 
         BZ    DISD0335                                                         
*                                                                               
         MVC   WORK(L'TWADEM),RBUYDMCT                                          
DISD0403 DS    0H                                                               
         LR    R5,RA                                                            
         AH    R5,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R5                                                       
         MVC   TWADEM,WORK          SAVE FOR MODULE 2A                          
         GOTO1 =A(CVTDEM),RR=Y                                                  
         DROP  R6                                                               
         DROP  R5                                                               
*                                                                               
DISD0413 DS    0H                                                               
         MVC   MGODMO1,8(R2)                                                    
         MVC   MGODMO2,MGODMO1                                                  
         MVC   MGODMO3,MGODMO1                                                  
*                                                                               
* DISPLAY DEMO VALUE FOR THIS PARTICULAR BUYLINE                                
*                                                                               
*                                                                               
DISD0414 DS    0H                                                               
         USING RBUYDMCV,R6                                                      
         CLC   RBUYDMDM,=F'-1'     NO DEMO INPUT                                
         BE    DISDEMX                                                          
         ZAP   WORK(5),=P'0'                                                    
         MVO   WORK(5),RBUYDMDM                                                 
         EDIT  (P5,WORK),(L'MGODEM1,MGODEM1),1,ALIGN=LEFT,ZERO=NOBLANK          
         B     DISDEMX                                                          
*                                  LENGTH                                       
DISD0415 DS    0H                                                               
         OI    MGOODEMH+1,X'20'     NO DEMO CAT, MARK DEM INPUT PROT            
         OI    MGOODE2H+1,X'20'                                                 
         OI    MGOODE3H+1,X'20'                                                 
         B     DISDEMX                                                          
DISD0450 DS    0H                                                               
         LR    R5,RA                                                            
         AH    R5,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R5                                                       
         MVC   WORK+3(4),=F'-1'                                                 
         MVC   TWADEM,WORK                                                      
         GOTO1 =A(CVTDEM),RR=YES                                                
         MVC   MGODMO1,8(R2)                                                    
         MVC   MGODMO2,MGODMO1                                                  
         MVC   MGODMO3,MGODMO1                                                  
         DROP  R5                                                               
         B     DISDEMX                                                          
DISDEMX  XIT1                                                                   
MYSPACE3 DC    60C' '                                                           
GETEL4   EQU   *                                                                
         LA    R6,34(R6)                                                        
GETEL4A  EQU   *                                                                
         CLI   0(R6),0                                                          
         BE    GETEL4X                                                          
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLC   ELCODE,0(R6)                                                     
         BER   RE                  RETURN WITH CC =                             
         B     GETEL4A                                                          
GETEL4X  LTR   RE,RE                                                            
         BR    RE                  RETURN WITH CC NOT =                         
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
************************************************************                    
* CONVERT DEMO                                                                  
* WORK -> DEMO TO BE CONVERTED                                                  
*                                                                               
************************************************************                    
CVTDEM   NTR1  BASE=*,LABEL=*                                                   
         L     R6,0(R1)                                                         
         CLI   WORK,C'('            USER DEFINED DEMOS                          
         BE    CVTD0100                                                         
         CLI   WORK+1,C'T'          FUDGE FOR DEMOCON                           
         BNE   *+8                                                              
         MVI   WORK+1,C'I'                                                      
         B     CVTD0110                                                         
CVTD0100 DS    0H                                                               
         MVC   8(3,R2),WORK                                                     
         B     CVTDX               MOVE STRAIGHT TO SCR                         
*                                                                               
CVTD0110 DS    0H                                                               
         L     R4,AIO3                                                          
         USING DBLOCKD,R4                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
*                                                                               
* THIS IS REALLY A CALL TO DEMOCON (NOT DEMCON)                                 
         GOTO1 DEMCON,DMCB,(1,WORK),(2,8(R2)),(0,DBLOCKD)                       
         DROP  R4                                                               
CVTDX    XIT1                                                                   
************************************************************                    
* DETERMINE WHICH DEMO TO USE IN 0E AND 0D                                      
* IF THE LOGIC IS CHANGED, THEN SHOULD CHANGE THE SAME ROUNTINE                 
* IN MODULE RECNT3A, RECNT2A                                                    
************************************************************                    
PICKDEM  NTR1  BASE=*,LABEL=*                                                   
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'0E'                                                     
         BAS   RE,GETEL5                                                        
         BNE   NO0EELT                                                          
*                                                                               
HAS0EELT DS    0H                                                               
         LR    R7,R6                                                            
         LA    R7,2(R7)            R7->FIRST DEMO IN 0E ELT                     
*                                                                               
         GOTOR LOOPDEM                                                          
         BE    *+10                                                             
         LR    R6,R7               NO PRIMARY DEM, USE THE FIRST                
         B     PICKDYES                                                         
*                                                                               
         USING RBUYRDCV,R6                                                      
         LR    R5,R6               R5->PRIMARY IN 0E                            
         CLC   RBUYRDDM,=F'-1'                                                  
         BNE   PICKDYES            PRIMARY <> -1, USE IT                        
         DROP  R6                                                               
*                                                                               
*  BELOW DEALS WITH SITUATION WHEN PRIMARY DEMO VALUE IN 0E = -1                
         LA    R6,RBUYREC          PRIMARY IN 0E = -1,LOOK FOR 0D               
         MVI   ELCODE,X'0D'                                                     
         BAS   RE,GETEL5                                                        
         BNE   PICK0010                                                         
         CLI   1(R6),18                                                         
         BE    PICK0010            IGNORE OLD FORMATE DEMO                      
         CLI   1(R6),34                                                         
         BNE   PICK0020                                                         
PICK0010 DS    0H                                                               
         LR    R6,R5               NO 0D, USE THE PRIMARY IN 0E                 
         B     PICKDYES                                                         
*                                                                               
PICK0020 DS    0H                                                               
         GOTOR LOOPDEM                                                          
         BE    *+10                                                             
         LR    R6,R5               0D HAS NO PRIMARY, USE PRIMARY IN 0E         
         B     PICKDYES                                                         
*                                                                               
         USING RBUYDMCV,R6                                                      
         CLC   RBUYDMDM,=F'-1'     IF 0D'S PRIMARY HAS A VALUE OF -1            
         BNE   PICKDYES            OTHERWISE USE PRIMARY IN 0D                  
         LR    R6,R5               THEN USE THE PRIMARY IN 0E                   
         B     PICKDYES                                                         
         DROP  R6                                                               
*                                                                               
*                                                                               
NO0EELT  DS    0H                                                               
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'0D'                                                     
         BAS   RE,GETEL5           NO DEM CAT                                   
         BNE   PICKDNO             PROTECT INPUT FIELD                          
*                                                                               
         CLI   1(R6),18                                                         
         BE    PICKDNO             IGNORE OLD FORMATE DEMO                      
         CLI   1(R6),34                                                         
         BE    PICKDNO                                                          
*                                                                               
         LR    R8,R6               THERE IS A 0D ELT                            
         LA    R8,2(R8)            R8->FIRST DEMO IN 0D ELT                     
*                                                                               
         GOTOR LOOPDEM                                                          
         BE    PICKDYES                                                         
         LR    R6,R8               USE THE FIRST DEMO IN 0D                     
PICKDYES SR    RC,RC                                                            
PICKDNO  LTR   RC,RC                                                            
PICKDEMX XIT1  REGS=(R6)                                                        
*                                                                               
*                                                                               
GETEL5   EQU   *                                                                
         LA    R6,34(R6)                                                        
GETEL5A  EQU   *                                                                
         CLI   0(R6),0                                                          
         BE    GETEL5X                                                          
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLC   ELCODE,0(R6)                                                     
         BER   RE                  RETURN WITH CC =                             
         B     GETEL5A                                                          
GETEL5X  LTR   RE,RE                                                            
         BR    RE                  RETURN WITH CC NOT =                         
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
***************************************                                         
* LOOP UNTIL WE FIND A PRIMARY DEMO                                             
***************************************                                         
LOOPDEM  NTR1  BASE=*,LABEL=*       R6->ELT                                     
         ZIC   R5,1(R6)                                                         
         AR    R5,R6                <BOUNDRY>                                   
         LA    R6,2(R6)                                                         
LOOPD10  DS    0H                                                               
         CLI   0(R6),C'('          USER DEFINED DEMO?                           
         BE    LOOPD30             YES, SKIP                                    
         TM    0(R6),X'40'          PRIMARY DEM?                                
         BO    LOOPDYES                                                         
LOOPD30  DS    0H                                                               
         LA    R6,L'RBUYDMCV(R6)                                                
         CR    R6,R5                                                            
         BNL   LOOPDNO                                                          
         B     LOOPD10                                                          
LOOPDYES SR    RC,RC                                                            
LOOPDNO  LTR   RC,RC                                                            
LOOPDEMX XIT1  REGS=(R6)                                                        
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'048RECNT29   02/25/15'                                      
         END                                                                    
