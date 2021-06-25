*          DATA SET SCHTP03A   AT LEVEL 052 AS OF 05/01/02                      
*PHASE T31303A,+0                                                               
         TITLE 'NETPAK PAY PROGRAM - BROWSE OVERLAY - T31303'                   
T31303   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**BRPY**,RA,RR=RE                                              
         L     R9,0(R1)                                                         
         USING PAYWRKD,R9                                                       
         USING TWAD,R8                                                          
         L     R7,AOVWORK                                                       
         USING TEMPD,R7                                                         
         L     R6,ASPOOLA                                                       
         USING SPOOLD,R6                                                        
         ST    RE,MYRELO                                                        
         ST    R1,MYPARM                                                        
         SPACE                                                                  
         CLI   PAYDLRS,0           IF PRODUCT DISPLAYED                         
         BE    *+8                                                              
         MVI   NBSPLOPT,X'C0'      - SET SPLIT PRODUCT OPTION                   
         SPACE                                                                  
         TM    MODE,FIRST          TEST FOR FIRST TIME FOR SCREEN               
         BO    *+16                                                             
         TM    SVLMODE,EOF         TEST FOR EOF ON LAST TRANSACTION             
         BZ    *+8                                                              
         OI    MODE,DISPLAY        YES-FORCE A DISPLAY                          
         CLI   ACTION,LIST                                                      
         BE    LS100                                                            
         CLI   ACTION,DIS                                                       
         BE    LS100                                                            
         TM    MODE,DISPLAY+FIRST  TEST FOR FORCED RE-DISPLAY                   
         BNZ   LS100                                                            
         CLI   ACTION,CHA          TEST FOR ACTION CHANGE                       
         BE    CH100                                                            
         DC    H'0'                                                             
         EJECT                                                                  
* INITIAL LIST - SET UP LINE COLUMNS INFORMATION                                
*                                                                               
LS100    LA    R2,CLINFO                                                        
         CLI   TOTOPT,YES                                                       
         BE    LS120                                                            
*                                                                               
         MVC   0(8,R2),=C'DTTMPREB'                                             
         CLI   SEQUENCE,C'D'                                                    
         BE    *+10                                                             
         MVC   0(6,R2),=C'PRDTTM'                                               
         LA    R2,8(R2)                                                         
*                                                                               
LS120    MVC   0(4,R2),=C'PGPN'                                                 
         TM    CLEARST+1,TMPAI     TEST FOR PAID OPTION                         
         BZ    *+10                NO                                           
         MVC   0(4,R2),=C'CGCN'                                                 
*                                                                               
         CLI   TOTOPT,YES                                                       
         BNE   LS140                                                            
         CLI   SCREEN,X'FB'                                                     
         BNE   LS300                                                            
         B     LS200                                                            
*                                                                               
LS140    CLI   PAYDLRS,0                                                        
         BE    LS160                                                            
         CLI   PAYDLRS,C'G'                                                     
         BE    *+10                                                             
         MVC   0(2,R2),2(R2)                                                    
         MVC   2(2,R2),=C'PD'                                                   
*                                                                               
LS160    MVC   4(2,R2),=C'FL'                                                   
         CLI   SCREEN,X'FB'                                                     
         BNE   LS300                                                            
         MVC   6(2,R2),=C'CL'                                                   
         EJECT                                                                  
* LIST UNIT RECORDS ON SCREEN FILTERS & COMMERCIAL SCREEN                       
*                                                                               
LS200    GOTO1 VCLEARF,DMCB,(1,BRCHED2H),BRCLAST                                
         GOTO1 (RF),(R1),BRCDFFH,BRCLAST                                        
         LA    R2,BRCHED1H                                                      
         BAS   RE,SETHDRS1         SET UP HEADERS LINE 1                        
*                                                                               
         LA    R2,BRCHED2H                                                      
         BAS   RE,SETHDRS2         SET UP HEADERS LINE 2                        
*                                                                               
         LA    R2,BRCLIN1H                                                      
         B     LS500                                                            
         SPACE 3                                                                
* LIST UNIT RECORDS ON SCREEN FILTERS ONLY SCREEN                               
*                                                                               
LS300    GOTO1 VCLEARF,DMCB,(1,BROHED2H),BROLAST                                
         GOTO1 (RF),(R1),BRODFFH,BROLAST                                        
         LA    R2,BROHED1H                                                      
         BAS   RE,SETHDRS1         SET UP HEADERS LINE 1                        
*                                                                               
         LA    R2,BROHED2H                                                      
         BAS   RE,SETHDRS2         SET UP HEADERS LINE 2                        
*                                                                               
         LA    R2,BROLIN1H                                                      
         USING LINED,R2                                                         
         EJECT                                                                  
LS500    LA    R3,LINES            R3 IS A COUNTER OF SCREEN LINES              
         MVI   SVKEYS,0                                                         
         LA    R4,SVKEYTAB         R4 POINTS TO SAVE KEY TABLE                  
         LR    RE,R4               CLEAR TABLE                                  
         LA    RF,SVKEYTL                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         CLI   NBMODE,NBREQLST     TEST FOR END-OF-FILE                         
         BE    LS620               YES-JUST DISPLAY TOTALS                      
         TM    MODE,DISPLAY+FIRST  TEST FOR FORCED RE-DISPLAY                   
         BZ    LS520                                                            
         XC    SVGROSS(8),SVGROSS  CLEAR BUCKETS FOR FIRST TIME                 
         B     LS600                                                            
*                                                                               
LS520    BAS   RE,RESTBLK          GET SAVED NETBLOCK                           
         MVI   NBFUNCT,NBFRDHI                                                  
         MVC   NBAIO,AIOAREA1      REFRESH ADDRESSES FOR CONTINUED READ         
         MVC   NBACOM,ACOMFACS                                                  
         CLI   PAYDLRS,0                                                        
         BE    *+8                                                              
         MVI   NBFUNCT,NBFRESTR                                                 
*                                                                               
         SPACE 1                                                                
LS600    BAS   RE,SAVBLOCK                                                      
         GOTO1 VNETIO,DMCB,NETBLOCK                                             
         CLI   NBERROR,NBGOOD                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBPROCUN                                                  
         BE    LS700                                                            
         CLI   NBMODE,NBREQLST     TEST FOR END-OF-FILE                         
         BNE   LS600               NO-GET NEXT RECORD                           
*                                                                               
LS620    LA    R5,LINGROSS                                                      
         MVC   LINTOT(6),=C'TOTALS' OUTPUT DISPLAY LINE                         
         CLI   PAYDLRS,C'N'                                                     
         BE    LS640                                                            
         SHI   R5,1                                                             
         L     R0,SVGROSS                                                       
         EDIT  (R0),(12,0(R5)),2,ZERO=NOBLANK,MINUS=YES                         
         AHI   R5,1                                                             
         LA    R5,LINNET                                                        
*                                  END-OF-FILE TOTALS                           
LS640    CLI   PAYDLRS,C'G'                                                     
         BE    ED100                                                            
         SHI   R5,2                                                             
         L     R0,SVNET                                                         
         EDIT  (R0),(12,0(R5)),2,ZERO=NOBLANK,MINUS=YES                         
         AHI   R5,2                                                             
         B     ED100               NOW PUT OUT MESSAGE                          
*                                                                               
         SPACE                                                                  
LS700    GOTO1 VFILREC                                                          
         BNE   LS600               SKIP THIS RECORD                             
*                                                                               
         CH    R3,=H'2'                                                         
         BNE   LS720                                                            
         CLI   NBPRD2,0                                                         
         BE    LS740                                                            
*        CLC   NBPRD2,NBSPLPRN     LINE 2 = 2ND PRODUCT NO PIGGY                
         CLI   NBSPLTYP,C'S'       LINE 2 = 2ND PRODUCT NO PIGGY                
         BE    LS740               - NEXT LINE                                  
         MVI   PIGGYOK,C'Y'                                                     
         B     LS760                                                            
*                                                                               
LS720    CH    R3,=H'1'                                                         
         BNE   LS740                                                            
         CLI   NBPRD2,0                                                         
         BE    LS740               KEEP SPLIT PROD ON SAME SCREEN               
         CLI   PIGGYOK,C'Y'                                                     
         BNE   ED100                                                            
*                                                                               
LS740    MVI   PIGGYOK,C'N'                                                     
LS760    TM    CLEARST+1,TMPAI     TEST FOR PAID OPTION                         
         BZ    LS780               NO                                           
         TM    NBUNITST,X'40'      TEST FOR PRE-EMPTED UNIT                     
         BZ    LS800                                                            
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'12',NBAIO),0                       
         CLI   12(R1),0            TEST FOR PREVIOUS PAYING                     
         BNE   LS600               NO-SKIP THE UNIT                             
         B     LS800                                                            
*                                                                               
LS780    XC    BLOCK,BLOCK                                                      
         LA    R5,BLOCK                                                         
         USING PAYBLKD,R5                                                       
         MVI   PAYFUNCT,PAYTOTS                                                 
         GOTO1 VGETPAY,DMCB,PAYWRKD,PAYBLKD                                     
         TM    PAYSTAT,PAYABLE     TEST IF PAYABLE                              
         BO    LS800               YES                                          
         TM    NBUNITST,X'42'      TEST FOR PRE-EMPT/MISSING                    
         BNZ   LS600               SKIP PRE-EMPT IF NOT PAYABLE                 
         CLI   PAYRSN,0            TEST FOR UNPAYABLE REASON                    
         BE    LS800               NONE                                         
         CLI   TOTOPT,YES          TEST FOR TOTALS ONLY                         
         BE    LS820               JUST GET DOLLARS                             
         GOTO1 VGETRSN,DMCB,PAYBLKD,LINGROSS                                    
*                                                                               
LS800    CLI   TOTOPT,YES          TEST FOR TOTALS ONLY                         
         BNE   LS840               YES-GET DOLLARS AND RETURN                   
LS820    LA    R5,LINGROSS                                                      
         BAS   RE,DISLINE          OUTPUT SCREEN FIELDS                         
         B     LS600                                                            
*                                                                               
LS840    MVI   RECSW,YES           SET SWITCH FOR QUALIFIED RECORD              
         L     RE,NBAIO            POINT TO RECORD                              
         USING NURECD,R4           COVER KEY TABLE WITH DSECT                   
         MVC   NUKEY,0(RE)         EXTRACT MAJOR KEY                            
         LA    RE,NBKEY            POINT TO NETBLOCK KEY AREA                   
         USING NUFILD,RE           COVER KEY TABLE WITH DSECT                   
         MVC   NUDA,NUDA-NUKEY(RE) EXTRACT DISK ADDRESS                         
         DROP  RE                                                               
         SPACE                                                                  
         LA    R5,8(R2)                                                         
         BAS   RE,DISLINE          OUTPUT SCREEN FIELDS                         
         MVC   LSTNUDA,NUDA        SAVE FOR SPLIT TEST                          
         DROP  R4                                                               
         SPACE                                                                  
         ZIC   R0,0(R2)            NEXT SCREEN LINE                             
         AR    R2,R0                                                            
         LA    R4,KEYLEN(R4)       NEXT KEY POSITION                            
         ZIC   R1,SVKEYS                                                        
         LA    R1,1(R1)            INCREMENT COUNT OF KEYS                      
         STC   R1,SVKEYS                                                        
         BCT   R3,LS600                                                         
         BAS   RE,SAVBLOCK                                                      
         B     ED100               OUTPUT MESSAGE                               
         DROP  R2                                                               
         EJECT                                                                  
********************************************************                        
*   ----- EDIT SCREEN AND MAKE CHANGES TO UNITS -----  *                        
********************************************************                        
CH100    CLI   SCREEN,X'FB'                                                     
         BNE   CH200                                                            
         SPACE 3                                                                
* CHANGE FILTERS AND COMMERCIAL SCREEN                                          
*                                                                               
         LA    R2,BRCDFFH                                                       
         BAS   RE,CHKFLT           SET UP DEFAULT FILTER                        
         BE    *+10                                                             
         MVC   SVDFFLT,8(R2)                                                    
*                                                                               
         LA    R2,BRCDFCH                                                       
         BAS   RE,CHKCML           SET UP DEFAULT COMMERCIAL                    
         BE    *+10                                                             
         MVC   SVDFCML,8(R2)                                                    
*                                                                               
         LA    R2,BRCFIL1H                                                      
         B     CH300                                                            
         SPACE 3                                                                
* CHANGE FILTERS ONLY SCREEN                                                    
*                                                                               
CH200    LA    R2,BRODFFH                                                       
         BAS   RE,CHKFLT           SET UP DEFAULT FILTER                        
         BE    *+10                                                             
         MVC   SVDFFLT,8(R2)                                                    
*                                                                               
         LA    R2,BRODAT1H                                                      
         SPACE 3                                                                
CH300    ZIC   R3,SVKEYS           COUNTER OF LINES ON SCREEN                   
         LA    R4,SVKEYTAB         R4 POINTS TO SAVED KEYS                      
         ST    R2,FADDR                                                         
         SPACE 1                                                                
* EDIT EACH LINE'S FILTERS FIELD IN A LOOP                                      
*                                                                               
CH320    XC    NBKEY,NBKEY                                                      
         MVC   NBKEY(KEYLEN),0(R4) EXTRACT KEY/DISK ADDRESS                     
         MVI   NBFUNCT,NBFGET                                                   
         GOTO1 VNETIO,DMCB,NETBLOCK                                             
         CLI   NBERROR,NBGOOD                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*        CLC   0(14,R4),=XL14'0433B0A3C65534C1C2C340F8E2E4'                     
*        BNE   *+6                                                              
*        DC    H'0'                                                             
*                                                                               
         GOTO1 VHELLO,DMCB,(C'D',UNTFILE),(X'08',NBAIO),(1,=C'P')               
         SR    R5,R5                                                            
         ICM   R5,1,SVDFFLT                                                     
         BZ    *+20                                                             
         STC   R5,8(R2)                                                         
         MVI   5(R2),1                                                          
         OI    6(R2),X'80'         SEND FIELD BACK                              
         B     *+12                                                             
*                                                                               
         BAS   RE,CHKFLT           TEST FOR ANY INPUT                           
         BE    CH400               NONE-FILTERS WERE DELETED                    
*                                                                               
         XC    WORK,WORK                                                        
         LA    R5,WORK                                                          
         USING NUFILD,R5                                                        
         MVI   NUFILEL,X'08'                                                    
         MVI   NUFILLEN,NUFILELN   ELEMENT LENGTH                               
         MVI   NUFILSCM,C'P'       PAY PROGRAM SCHEME                           
         MVC   NUFILTER,SPACES                                                  
         MVC   NUFILTER(1),8(R2)                                                
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),NBAIO,(R5),0                          
         CLI   12(R1),0                                                         
         BE    CH400                                                            
         MVI   FERN,TOOLARGE                                                    
         CLI   12(R1),5            TEST FOR RECORD OVERFLOW                     
         BE    CHERR                                                            
         DC    H'0'                                                             
         DROP  R5                                                               
         SPACE 1                                                                
CH400    CLI   SCREEN,X'FB'                                                     
         BNE   CH500                                                            
         ZIC   R0,0(R2)            NEXT SCREEN LINE                             
         AR    R2,R0                                                            
*                                                                               
         OC    SVDFCML,SVDFCML                                                  
         BNZ   CH420                                                            
         CLI   NBPRD2,0            IF PIGGYBACK PRD'S MAKE SURE BOTH -          
         BE    CH420               - HAVE A COMMERCIAL CODE                     
         CLC   0(KEYLEN,R4),KEYLEN(R4)                                          
         BNE   CH420                                                            
         MVI   FERN,PBTWOCML                                                    
         ST    R2,FADDR                                                         
         LA    RE,BRCCML2H-BRCCML1H(R2)                                         
         CLI   5(R2),0                                                          
         BE    *+16                                                             
         CLI   5(RE),0                                                          
         BE    ERROR                                                            
         B     CH420                                                            
*                                                                               
         CLI   5(RE),0                                                          
         BNE   ERROR                                                            
*                                                                               
CH420    LA    R5,NBSPLPRN                                                      
         GOTO1 VHELLO,DMCB,(C'D',UNTFILE),(X'24',NBAIO),(1,0(R5))               
         OC    SVDFCML,SVDFCML                                                  
         BZ    CH440                                                            
         MVC   8(8,R2),SVDFCML                                                  
         MVI   5(R2),8                                                          
         OI    6(R2),X'80'         SEND FIELD BACK                              
         B     *+12                                                             
*                                                                               
CH440    BAS   RE,CHKCML           TEST FOR ANY INPUT                           
         BE    CH500                                                            
*                                                                               
         XC    WORK,WORK                                                        
         LA    R5,WORK                                                          
         USING NUPCELD,R5                                                       
         MVI   NUPCEID,X'24'                                                    
         MVI   NUPCLEN,NUPCELN     ELEMENT LENGTH                               
         MVC   NUPCPRD,NBSPLPRN    PRODUCT NAME                                 
         MVC   NUPCCML,8(R2)                                                    
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),NBAIO,(R5),0                          
         CLI   12(R1),0                                                         
         BE    CH500                                                            
         MVI   FERN,TOOLARGE                                                    
         CLI   12(R1),5            TEST FOR RECORD OVERFLOW                     
         BE    CHERR                                                            
         DC    H'0'                                                             
*                                                                               
         SPACE 1                                                                
CH500    XC    KEY,KEY                                                          
         MVC   KEY(KEYLEN),0(R4)                                                
         MVC   NDXDA,NUDA-NUKEY(R4) SET DISK ADDRESS                            
         GOTO1 AIO,DMCB,UPDATE+UNT+FILE+GET,AIOAREA2                            
         L     RE,AIOAREA2                                                      
         L     RF,NBAIO                                                         
         CLC   0(20,RE),0(RF)                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   0(20,R4),0(RF)                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,(R1),UNT+FILE+PUT,NBAIO                                      
         SPACE 1                                                                
CH600    ZIC   R0,0(R2)            NEXT SCREEN LINE                             
         AR    R2,R0                                                            
         ZIC   R0,0(R2)            NEXT SCREEN LINE                             
         AR    R2,R0                                                            
         CLC   0(KEYLEN,R4),KEYLEN(R4)                                          
         BE    *+8                                                              
         MVI   NBSPLPRN,0           RESET SPLIT PROD NUMBER                     
         LA    R4,KEYLEN(R4)       NEXT KEY                                     
         BCT   R3,CH320                                                         
         B     ED100               ALL DONE                                     
         SPACE 1                                                                
* ERROR IN FILTERS FIELD EDIT                                                   
*                                                                               
CHERR    ZIC   R0,0(R2)            NEXT SCREEN LINE                             
         AR    R2,R0                                                            
         CLI   0(R2),50            NEXT DATA FIELD                              
         BH    CHERR                                                            
         BCT   R3,*+8                                                           
         B     ERROR                                                            
*                                                                               
         ZIC   R0,0(R2)            NEXT SCREEN LINE                             
         AR    R2,R0                                                            
         TM    4(R2),X'80'         TEST IF FIELD SENT THIS TIME                 
         BZ    *+8                 NO                                           
         OI    6(R2),X'81'         XMIT BACK MODIFIED NEXT FOR RE-EDIT          
         CLI   SCREEN,X'FB'                                                     
         BNE   CHERR                                                            
*                                                                               
         ZIC   R0,0(R2)            NEXT SCREEN LINE                             
         AR    R2,R0                                                            
         TM    4(R2),X'80'         TEST IF FIELD SENT THIS TIME                 
         BZ    *+8                 NO                                           
         OI    6(R2),X'81'         XMIT BACK MODIFIED NEXT FOR RE-EDIT          
         B     CHERR                                                            
         EJECT                                                                  
* SET MESSAGE, CURSOR, AND ACTION                                               
*                                                                               
ED100    CLI   ACTION,CHA          TEST FOR ACTION CHANGE                       
         BNE   ED120               NO                                           
         TM    MODE,FIRST+DISPLAY  TEST FOR FORCED DISPLAY                      
         BNZ   ED120               YES                                          
         MVC   PAYMSG(23),=C'** CHANGES COMPLETED **'                           
         MVC   PAYACT,SPACES                                                    
         MVC   PAYACT(3),=C'DIS'   SET ACTION TO DISPLAY                        
         LA    R2,PAYACTH          CURSOR TO ACTION FIELD                       
         ST    R2,FADDR            FADDR                                        
         B     EDEXT                                                            
         SPACE 1                                                                
ED120    MVI   MORESW,YES                                                       
         CLI   NBMODE,NBPROCUN     TEST IF UNIT RETURNED                        
         BE    *+8                                                              
         MVI   MORESW,NO           AT END-OF-FILE                               
         CLI   RECSW,YES           TEST FOR ANY RECORDS ON SCREEN               
         BE    ED180               YES                                          
*                                  AT END-OF-FILE                               
ED140    LA    RE,SVDATA           CLEAR ENTIRE SAVE AREA                       
         LA    RF,SVDATAL                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         MVI   SVLMODE,EOF         SET LAST MODE IS EOF                         
*                                                                               
         CLI   TOTOPT,YES          TEST FOR TOTALS ONLY                         
         BNE   *+14                                                             
         MVC   PAYMSG(L'TOTMSG),TOTMSG                                          
         B     ED160                                                            
*                                                                               
         MVC   PAYMSG(L'NODATA),NODATA                                          
         TM    MODE,FIRST+DISPLAY  TEST FOR START OF READ                       
         BNZ   *+10                                                             
         MVC   PAYMSG(L'NOMORE),NOMORE                                          
*                                                                               
ED160    LA    R2,PAYACTH          SET CURSOR TO ACTION                         
         OI    6(R2),X'01'         ACTION MODIFIED NEXT TIME                    
         ST    R2,FADDR                                                         
         B     EDEXT                                                            
         SPACE                                                                  
ED180    CLI   ACTION,LIST         TEST FOR ACTION LIST                         
         BNE   ED200               NO                                           
         MVC   PAYMSG(L'LISTMSG),LISTMSG                                        
         CLI   MORESW,YES          TEST FOR MORE TO COME                        
         BNE   *+14                                                             
         MVC   PAYMSG+L'LISTMSG+1(14),=C'(MORE TO COME)'                        
         B     *+10                                                             
         MVC   PAYMSG+L'LISTMSG+1(20),=C'- ENTER NEXT REQUEST'                  
         B     ED240                                                            
         SPACE 1                                                                
ED200    LA    RF,PAYMSG                                                        
         MVC   PAYMSG(21),=C'** UNITS DISPLAYED **' SET DISPLAY MSG             
         LA    RF,19(RF)                                                        
         CLI   MATCHSW,X'FF'                                                    
         BNE   *+18                                                             
         MVC   PAYMSG(35),=C'** WARNING UNMATCHED UNITS EXIST **'               
         LA    RF,14(RF)                                                        
         MVI   MATCHSW,0                                                        
         CLI   ACTION,DIS          TEST FOR ACTION DISPLAY                      
         BE    ED220               YES                                          
         TM    MODE,FIRST+DISPLAY  TEST FOR FORCED DISPLAY                      
         BZ    ED220               NO                                           
         MVC   0(22,RF),=C'- NOW ENTER CHANGES **'                              
         B     ED240                                                            
*                                                                               
ED220    MVC   PAYACT,SPACES                                                    
         MVC   PAYACT(3),=C'CHA'   SET ACTION TO CHANGE                         
         OI    PAYACTH+6,X'80'     XMIT BACK                                    
         B     ED240                                                            
*                                                                               
ED240    LA    R2,PAYACTH          SET CURSOR TO ACTION                         
         CLI   ACTION,LIST         FOR LIST                                     
         BE    ED260                                                            
         LA    R2,BRODFFH          FOR OTHERS SET IT TO FIRST DATA FLD          
         CLI   SCREEN,X'FB'                                                     
         BNE   ED260                                                            
         LA    R2,BRCDFFH          FOR OTHERS SET IT TO FIRST DATA FLD          
ED260    ST    R2,FADDR                                                         
         MVI   SVLMODE,PROCESS     SET TO CONTINUE PROCESSING                   
         SPACE 1                                                                
EDEXT    B     EXXMOD                                                           
         EJECT                                                                  
* SUB-ROUTINE TO RESTORE NETBLOCK                                               
*                                                                               
RESTBLK  ST    RE,SAVEREG                                                       
         LA    RE,NETBLOCK                                                      
         LA    RF,NEBLOCKL                                                      
         LR    R1,RF                                                            
         LA    R0,SVNBLOCK                                                      
         MVCL  RE,R0                                                            
         L     RE,SAVEREG                                                       
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO SAVE NETBLOCK                                                  
*                                                                               
SAVBLOCK ST    RE,SAVEREG                                                       
         LA    RE,SVNBLOCK                                                      
         LA    RF,NEBLOCKL                                                      
         LR    R1,RF                                                            
         LA    R0,NETBLOCK                                                      
         MVCL  RE,R0                                                            
         L     RE,SAVEREG                                                       
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO SET LITERALS FOR HEADLINE 1                                    
*                                                                               
         USING LINED,R2                                                         
SETHDRS1 MVC   LINGROSS(22),=C'----PAYABLE TOTALS----'                          
         TM    CLEARST+1,TMPAI     TEST FOR PAID OPTION                         
         BZ    *+10                NO                                           
         MVC   LINGROSS+4(7),=C'CLEARED'                                        
         OI    6(R2),X'80'         SEND BACK FIRST HEADLINE                     
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO SET LITERALS FOR HEADLINE 1                                    
*                                                                               
SETHDRS2 MVC   8(L'DATHED1,R2),DATHED1                                          
         CLI   SEQUENCE,C'D'       TEST FOR DATE SEQUENCE                       
         BE    *+10                YES                                          
         MVC   8(L'PROGHED1,R2),PROGHED1                                        
*                                                                               
         MVC   LINEST(5),=C'EST B'                                              
*                                                                               
         MVC   LINGROSS+3(5),=C'GROSS'                                          
         CLI   PAYDLRS,C'N'                                                     
         BNE   *+10                                                             
         MVC   LINGROSS+3(5),=C' NET '                                          
*                                                                               
         MVC   LINNET+4(3),=C'NET'                                              
         CLI   PAYDLRS,0                                                        
         BE    *+10                                                             
         MVC   LINNET(7),=C'PRODUCT'                                            
*                                                                               
         OI    6(R2),X'80'         SEND BACK SECOND HEADLINE                    
         BR    RE                                                               
         DROP  R2                                                               
         EJECT                                                                  
**********************************************                                  
*   --------  DISPLAY SUBROUTINES --------   *                                  
**********************************************                                  
* SUB-ROUTINE TO CONTROL DISPLAYING OF OUTPUT LINE                              
*   R5 = START ADDRESS                                                          
*   R2 = START LINE SCREEN FIELD (INPUT)                                        
*        LAST SCREEN FIELD ON LINE (OUTPUT)                                     
*                                                                               
DISLINE  NTR1                                                                   
         LA    R3,CLINFO           FIELDS TO OUTPUT                             
*                                                                               
DL100    OC    0(2,R3),0(R3)                                                    
         BZ    DLEXT                                                            
         LA    RE,OPRTNTBL                                                      
*                                                                               
DL120    CLC   0(2,R3),0(RE)       MATCH ON CODE                                
         BE    DL200                                                            
         LA    RE,4(RE)                                                         
         OC    0(2,RE),0(RE)                                                    
         BNZ   DL120                                                            
         DC    H'0'                                                             
*                                                                               
DL200    SR    RF,RF               GOTO SUB RTN                                 
         ICM   RF,3,2(RE)                                                       
         AR    RF,RB                                                            
         BASR  RE,RF                                                            
         LA    R3,2(R3)                                                         
         B     DL100                                                            
*                                                                               
DLEXT    XIT1  REGS=(R2)                                                        
         SPACE 2                                                                
* SUB-ROUTINE TO DISPLAY DATE/SUB-LINE IN DISPAR                                
*                                                                               
DATOUT   ST    RE,SAVEREG2         SECOND LEVEL RETURN POINT                    
         CLC   LSTNUDA,NUDA-NUKEY(R4) SPLIT PRODUCT                             
         BE    DAT100                                                           
         GOTO1 VDATCON,DMCB,(2,NBACTDAT),(4,0(R5))                              
         CLI   NBACTSUB,1                                                       
         BE    DAT100                                                           
         MVI   5(R5),DASH                                                       
         ZIC   R0,NBACTSUB                                                      
         EDIT  (R0),(3,6(R5)),ALIGN=LEFT                                        
DAT100   LA    R5,10(R5)                                                        
*                                                                               
         L     RE,SAVEREG2                                                      
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO OUTPUT START TIME INTO DUB                                     
*                                                                               
TIMOUT   ST    RE,SAVEREG2         SAVE RETURN POINT                            
         CLC   LSTNUDA,NUDA-NUKEY(R4) SPLIT PRODUCT                             
         BE    TIM100                                                           
         XC    FULL,FULL                                                        
         MVC   FULL(2),NBTIME      GET START TIME                               
         MVC   DUB,SPACES                                                       
         GOTO1 VUNTIME,DMCB,FULL,DUB                                            
         MVC   0(5,R5),DUB                                                      
TIM100   LA    R5,7(R5)                                                         
*                                                                               
         L     RE,SAVEREG2                                                      
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE FOR DISPLAY OF PROGRAM NAME                                       
*                                                                               
PRGOUT   CLC   LSTNUDA,NUDA-NUKEY(R4) SPLIT PRODUCT                             
         BE    PRG100                                                           
         MVC   0(16,R5),NBPROGNM                                                
PRG100   LA    R5,17(R5)                                                        
*                                                                               
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE FOR DISPLAY OF ESTIMATE # & BILLING INDICATOR                     
*                                                                               
ESBOUT   ST    RE,SAVEREG2         SECOND LEVEL RETURN POINT                    
         CLC   LSTNUDA,NUDA-NUKEY(R4) SPLIT PRODUCT                             
         BE    ESB100                                                           
         ZIC   R0,NBACTEST                                                      
         EDIT  (R0),(3,0(R5))                                                   
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'10',NBAIO),0                       
         CLI   12(R1),0                                                         
         BNE   *+8                                                              
         MVI   5(R5),STAR          NOTE BILLING ON UNIT                         
ESB100   LA    R5,6(R5)                                                         
*                                                                               
         L     RE,SAVEREG2                                                      
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE FOR DISPLAY OF CLEAR DOLLARS  GROSS                               
*                                                                               
CGRSOUT  ST    RE,SAVEREG2         SECOND LEVEL RETURN POINT                    
         BAS   RE,CLRSPEC                                                       
         L     R0,NBPAYTGR                                                      
         A     R0,NBPAYIGR         FIND COMBINED CLEARED GROSS                  
         A     R0,CLSPECG                                                       
         L     RE,SVGROSS                                                       
         AR    RE,R0               UPDATE TOTAL GROSS                           
         ST    RE,SVGROSS                                                       
         BAS   RE,EDITDLR          OUTPUT GROSS DOLLARS                         
*                                                                               
         L     RE,SAVEREG2                                                      
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE FOR DISPLAY OF CLEAR DOLLARS NET                                  
*                                                                               
CNETOUT  ST    RE,SAVEREG2         SECOND LEVEL RETURN POINT                    
         BAS   RE,CLRSPEC                                                       
         L     R0,NBPAYTNT                                                      
         A     R0,NBPAYINT                                                      
         A     R0,CLSPECN                                                       
         L     RE,SVNET                                                         
         AR    RE,R0               UPDATE TOTAL NET                             
         ST    RE,SVNET                                                         
         BAS   RE,EDITDLR          OUTPUT GROSS DOLLARS                         
*                                                                               
         L     RE,SAVEREG2                                                      
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE FOR DISPLAY OF CLEAR DOLLARS GROSS                                
*                                                                               
PGRSOUT  ST    RE,SAVEREG2         SECOND LEVEL RETURN POINT                    
         LA    RF,BLOCK                                                         
         USING PAYBLKD,RF                                                       
         CLI   PAYRSN,0            TEST FOR UNPAYABLE REASON                    
         BE    *+12                NONE                                         
         LA    R5,12(R5)                                                        
         B     PGEXT                                                            
*                                                                               
         L     R0,PAYGROSS                                                      
         L     RE,SVGROSS                                                       
         AR    RE,R0               UPDATE TOTAL GROSS                           
         ST    RE,SVGROSS                                                       
         BAS   RE,EDITDLR          OUTPUT GROSS DOLLARS                         
*                                                                               
PGEXT    L     RE,SAVEREG2                                                      
         BR    RE                                                               
         DROP  RF                                                               
         SPACE 2                                                                
* SUB-ROUTINE FOR DISPLAY OF PAID DOLLARS NET                                   
*                                                                               
PNETOUT  ST    RE,SAVEREG2         SECOND LEVEL RETURN POINT                    
         LA    RF,BLOCK                                                         
         USING PAYBLKD,RF                                                       
         CLI   PAYRSN,0            TEST FOR UNPAYABLE REASON                    
         CLI   PAYRSN,0            TEST FOR UNPAYABLE REASON                    
         BE    *+12                NONE                                         
         LA    R5,12(R5)                                                        
         B     PNEXT                                                            
*                                                                               
         L     R0,PAYNT                                                         
         L     RE,SVNET                                                         
         AR    RE,R0               UPDATE TOTAL NET                             
         ST    RE,SVNET                                                         
         BAS   RE,EDITDLR          OUTPUT GROSS DOLLARS                         
*                                                                               
PNEXT    L     RE,SAVEREG2                                                      
         BR    RE                                                               
         DROP  RF                                                               
         SPACE 2                                                                
* SUB-ROUTINE FOR DISPLAY OF PRODUCTS                                           
*                                                                               
PRODOUT  ST    RE,SAVEREG2         SECOND LEVEL RETURN POINT                    
         CLI   NBSPLPRN,0                                                       
         BNE   PT100                                                            
         MVC   0(7,R5),=C'UNALLOC'                                              
         B     PT220                                                            
*                                                                               
PT100    LA    R0,220                                                           
         L     RE,ACLIREC                                                       
         USING CLTHDRD,RE                                                       
         LA    R1,CLIST            POINT TO PRODUCT LIST                        
PT120    OC    0(4,R1),0(R1)       TEST FOR END-OF-LIST                         
         BZ    PTDUMP              YES                                          
         CLC   NBSPLPRN,3(R1)                                                   
         BE    PT200                                                            
         LA    R1,4(R1)                                                         
         BCT   R0,PT120                                                         
PTDUMP   DC    H'0'                                                             
*                                                                               
PT200    MVC   1(3,R5),0(R1)                                                    
PT220    LA    R5,12(R5)           NEXT OUTPUT POSITION                         
*                                                                               
PTEXT    L     RE,SAVEREG2                                                      
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE FOR DISPLAY OF FILTERS                                            
*                                                                               
FILTOUT  ST    RE,SAVEREG2         SECOND LEVEL RETURN POINT                    
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               POINT TO FILTER FIELD                        
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'08',NBAIO),(1,=C'P')               
         CLI   12(R1),0            TEST FOR FILTERS ELEMENT                     
         BNE   FLT100              NO                                           
         L     RE,12(R1)                                                        
         USING NUFILD,RE                                                        
         MVC   8(1,R2),NUFILTER                                                 
*                                                                               
FLT100   L     RE,SAVEREG2                                                      
         BR    RE                                                               
         DROP  RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE FOR DISPLAY OF COMMERCIAL FILTERS                                 
*                                                                               
COMLOUT  ST    RE,SAVEREG2         SECOND LEVEL RETURN POINT                    
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               POINT TO COMMERCIAL FIELD                    
         XC    DMCB(8),DMCB        0 FOR SECOND PARM                            
CML100   GOTO1 MYGETEL,DMCB,(X'24',NBAIO)                                       
         BZ    CMLEXT              NOT FOUND                                    
         L     RE,4(R1)                                                         
         USING NUPCELD,RE                                                       
         CLC   NBSPLPRN,NUPCPRD                                                 
         BNE   CML100                                                           
         MVC   8(8,R2),NUPCCML                                                  
*                                                                               
CMLEXT   L     RE,SAVEREG2                                                      
         BR    RE                                                               
         DROP  RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO OUTPUT DOLLAR FIELD                                            
*                                                                               
EDITDLR  CLI   TOTOPT,YES                                                       
         BER   RE                                                               
         EDIT  (R0),(11,0(R5)),2,ZERO=NOBLANK,MINUS=YES                         
         LA    R5,12(R5)           NEXT OUTPUT POSITION                         
         BR    RE                                                               
         EJECT                                                                  
**********************************************                                  
*   --------  CHANGE SUBROUTINES --------    *                                  
**********************************************                                  
* SUB-ROUTINE TO SAVE DEFAULT FILTER FIELD                                      
*                                                                               
CHKFLT   ST    RE,SAVEREG                                                       
         ST    R2,FADDR                                                         
         XC    FLAST,FLAST                                                      
         XC    FTERM,FTERM                                                      
         GOTO1 AFVAL                                                            
         CLI   FLDH+5,0                                                         
         BE    DCEXT                                                            
         MVI   FERN,INVERR                                                      
         TM    FLDH+4,X'0C'        TEST FOR ALPHA OR NUMERIC                    
         BZ    ERROR                                                            
*                                                                               
DCEXT    L     RE,SAVEREG                                                       
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO SAVE DEFAULT COMMERICAL FIELD                                  
*                                                                               
CHKCML   ST    RE,SAVEREG                                                       
         ST    R2,FADDR                                                         
         XC    FLAST,FLAST                                                      
         XC    FTERM,FTERM                                                      
         GOTO1 AFVAL                                                            
         CLI   FLDH+5,0            INPUT                                        
         BE    SCEXT                                                            
         MVI   FERN,INVERR                                                      
         CLI   FLDH+5,8            LENGTH MUST BE 8                             
         BNE   ERROR                                                            
         LA    RE,FLD                                                           
         LA    R0,4                CHECK 4 ALPHA CHAR                           
*                                                                               
SC100    CLI   0(RE),X'C1'                                                      
         BL    ERROR                                                            
         CLI   0(RE),X'C9'                                                      
         BNH   SC120                                                            
         CLI   0(RE),X'D1'                                                      
         BL    ERROR                                                            
         CLI   0(RE),X'D9'                                                      
         BNH   SC120                                                            
         CLI   0(RE),X'E2'                                                      
         BL    ERROR                                                            
         CLI   0(RE),X'E9'                                                      
         BH    ERROR                                                            
*                                                                               
SC120    LA    RE,1(RE)                                                         
         BCT   R0,SC100                                                         
*                                                                               
         LA    R0,4                CHECK 4 ALPHA CHAR                           
*                                                                               
SC200    CLI   0(RE),X'F0'                                                      
         BL    ERROR                                                            
         CLI   0(RE),X'F9'                                                      
         BH    ERROR                                                            
         LA    RE,1(RE)                                                         
         BCT   R0,SC200                                                         
         LTR   RE,RE                                                            
*                                                                               
SCEXT    L     RE,SAVEREG                                                       
         BR    RE                                                               
         EJECT                                                                  
**********************************************                                  
*   --------  GENERAL SUBROUTINES --------   *                                  
**********************************************                                  
* MY GETEL EXPECTS PARMS SET AS FOLLOWS:                                        
* PARM1 BYTE 0   = ELEMNET CODE                                                 
*       BYTE 1-3 = A(RECORD)                                                    
* PARM2          = LAST ELEMENT FOUND OR 0 TO INDICATE FIRST RECORD             
*                                                                               
MYGETEL  NTR1                                                                   
         SR    R2,R2                                                            
         ICM   R2,15,4(R1)                                                      
         BNZ   MG100                                                            
         ICM   R2,7,1(R1)                                                       
         LA    R2,27(R2)                                                        
         B     MG120                                                            
*                                                                               
MG100    ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
MG120    CLI   0(R2),0                                                          
         BE    EXXMOD                                                           
         CLC   0(1,R1),0(R2)       ELEMENT EQUAL                                
         BNE   MG100                                                            
         ST    R2,4(R1)            RETURN ADDRESS                               
         LTR   R2,R2                                                            
         B     EXXMOD                                                           
         SPACE 2                                                                
* CLRSPEC GETS THE NET AND GROSS CLEARED SPECIAL CHARGES                        
CLRSPEC  NTR1                                                                   
         XC    CLSPECN,CLSPECN                                                  
         XC    CLSPECG,CLSPECG                                                  
*                                                                               
         USING NUPAYD,RE                                                        
         XC    DMCB(8),DMCB        0 FOR SECOND PARM                            
         GOTO1 MYGETEL,DMCB,(X'12',NBAIO)                                       
         BZ    CLRSPEX             NOT FOUND                                    
         L     RE,4(R1)                                                         
         B     CLRSP060                                                         
*                                                                               
CLRSP040 ZIC   RF,NUPAYLEN                                                      
         AR    RE,RF                                                            
         CLI   NUPAYEL,X'12'                                                    
         BNE   CLRSPEX                                                          
*                                                                               
CLRSP060 CLI   NUPAYTYP,C'T'                                                    
         BE    CLRSP040                                                         
         CLI   NUPAYTYP,C'I'                                                    
         BE    CLRSP040                                                         
*                                                                               
         L     RF,CLSPECN                                                       
         A     RF,NUPAYNET                                                      
         ST    RF,CLSPECN                                                       
*                                                                               
         L     RF,CLSPECG                                                       
         A     RF,NUPAYGRS                                                      
         ST    RF,CLSPECG                                                       
*                                                                               
         B     CLRSP040                                                         
*                                                                               
CLRSPEX  B     EXXMOD                                                           
         DROP  RE                                                               
         SPACE 2                                                                
* MODULE AND ROUTINE EXIT                                                       
*                                                                               
EXXMOD   XMOD1 1                                                                
         SPACE 2                                                                
* ERROR EXIT                                                                    
*                                                                               
ERROR    GOTO1 VERROR                                                           
         EJECT                                                                  
* TABLE OF ADDRESS FOR OUTPUT ROUTINES                                          
OPRTNTBL DC    C'DT',AL2(DATOUT-T31303)                                         
         DC    C'TM',AL2(TIMOUT-T31303)                                         
         DC    C'PR',AL2(PRGOUT-T31303)                                         
         DC    C'EB',AL2(ESBOUT-T31303)                                         
         DC    C'PG',AL2(PGRSOUT-T31303)                                        
         DC    C'PN',AL2(PNETOUT-T31303)                                        
         DC    C'CG',AL2(CGRSOUT-T31303)                                        
         DC    C'CN',AL2(CNETOUT-T31303)                                        
         DC    C'PD',AL2(PRODOUT-T31303)                                        
         DC    C'FL',AL2(FILTOUT-T31303)                                        
         DC    C'CL',AL2(COMLOUT-T31303)                                        
         DC    X'00'                                                            
* PATCH AREA                                                                    
*                                                                               
         DS    0H                                                               
PATCH    DC    XL32'00'                                                         
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
NOMORE   DC    C'** NO MORE UNITS TO DISPLAY - ENTER NEXT REQUEST **'           
NODATA   DC    C'** NO UNITS TO DISPLAY - CHECK KEY FIELDS/OPTIONS **'          
TOTMSG   DC    C'** TOTALS DISPLAYED **'                                        
LISTMSG  DC    C'** UNITS LISTED **'                                            
DATHED1  DC    C'DATE      TIME   PROGRAM NAME'                                 
PROGHED1 DC    C'PROGRAM NAME     DATE      TIME'                               
PROGHDL  EQU   *-PROGHED1                                                       
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         DS    0F                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE NEPAYWRK                                                       
         EJECT                                                                  
* BROWSE SCREEN FILTERS ONLY                                                    
*                                                                               
         ORG   PAYLAST                                                          
       ++INCLUDE NEPAYFDD                                                       
         EJECT                                                                  
* BROWSE SCREEN FILTERS & COMMERCIALS                                           
*                                                                               
         ORG   PAYLAST                                                          
       ++INCLUDE NEPAYFBAD !!!!!!!!!!!!!!!                                      
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
LINELEN  EQU   BROLIN2H-BROLIN1H                                                
DATH     EQU   BRODAT1H-BROLIN1H   DISPLACEMENT TO DATA FIELD HEADER            
DAT      EQU   BRODAT1-BROLIN1H    DISPLACEMENT TO DATA FIELD                   
LINES    EQU   (BROLAST-BROLIN1H)/LINELEN                                       
KEYLEN   EQU   L'NUKEY+L'NUKSTAT+L'NUDA UNIT KEY LENGTH                         
         SPACE 2                                                                
* OVERLAY SAVE AREA                                                             
*                                                                               
         ORG   LASTX                                                            
SVDATA   DS    0C                                                               
SVLMODE  DS    X                                                                
SVKEYS   DS    X                                                                
SVGROSS  DS    F                   PAYABLE GROSS FOR READ                       
SVNET    DS    F                   PAYABLE NET FOR READ                         
SVNBLOCK DS    XL(NEBLOCKL)                                                     
SVKEYTAB DS    (LINES)CL(KEYLEN)                                                
SVKEYTL  EQU   *-SVKEYTAB          LENGTH OF KEY TABLE                          
SVDATAL  EQU   *-SVDATA                                                         
         EJECT                                                                  
* DSECT TO COVER LOCAL WORKING STORAGE                                          
*                                                                               
TEMPD    DSECT                                                                  
MYRELO   DS    A                                                                
MYPARM   DS    A                                                                
SAVEREG  DS    A                                                                
SAVEREG2 DS    A                   SECOND LEVEL REGISTER SAVE                   
RECSW    DS    C                                                                
MORESW   DS    C                                                                
ALLSW    DS    C                                                                
DISPAR   DS    CL16                COMMON OUTPUT AREA                           
CLINFO   DS    CL18                OUTPUT COLUMNS                               
SVDFCML  DS    CL8                 DEFAULT COMMERCIAL                           
SVDFFLT  DS    C                   DEFAULT FILTER                               
PIGGYOK  DS    CL1                 CHECK SCREEN LINE ON PIGGY                   
LSTNUDA  DS    A                   USED FOR SPLIT PRODUCT COMPARE               
CLSPECN  DS    F                   CLEARED SPECIAL CHARGES NET                  
CLSPECG  DS    F                   CLEARED SPECIAL CHARGES GROSS                
         DS    0D                                                               
BLOCK    DS    CL256                                                            
         SPACE 2                                                                
* OVERLAY EQUATES                                                               
*                                                                               
EOF      EQU   X'01'                                                            
PROCESS  EQU   X'02'                                                            
         SPACE 2                                                                
* DSECT TO COVER SCREEN LINE                                                    
*                                                                               
LINED    DSECT                                                                  
         DS    CL8                 PROTECTED FIELD HEADER                       
LINDATE  DS    CL9                                                              
         DS    C                                                                
LINTIME  DS    CL5                                                              
         DS    CL2                                                              
LINPRGN  DS    CL16                                                             
         DS    CL1                                                              
         ORG   LINDATE             FIRST 3 FIELDS VARY FOR PROG. SEQ.           
PLINPRGN DS    CL16                                                             
         DS    C                                                                
PLINDATE DS    CL9                                                              
         DS    C                                                                
PLINTIME DS    CL5                                                              
         DS    CL2                                                              
         ORG                                                                    
LINEST   DS    CL3                                                              
         DS    C                                                                
LINBILL  DS    C                                                                
         DS    C                                                                
LINGROSS DS    CL12                                                             
         DS    C                                                                
LINNET   DS    CL12                                                             
LINTOT   EQU   LINGROSS-8          POSITION FOR TOTALS LABEL                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'052SCHTP03A  05/01/02'                                      
         END                                                                    
