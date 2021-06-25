*          DATA SET SPCSO06    AT LEVEL 240 AS OF 09/12/03                      
*PHASE T21806A,*                                                                
         TITLE 'T21806 - CHILD SPOT ALLOCATION REPORT'                          
T21806   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 NUMRCHGS*RCHGTABL+1,T21806,RR=R3                                 
         LR    R2,RC                                                            
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R2,RCHGTAB                                                       
         ST    R3,APRELO                                                        
*                                                                               
         MVI   TRADEOPT,C'N'       IF ACTION 'TREPORT'                          
         CLI   CONACT,C'T'                                                      
         BNE   *+8                                                              
         MVI   TRADEOPT,C'Y'       SET OPTION TO PRINT TRADE DOLLARS            
*                                                                               
         CLI   MYOVNUM,X'06'       CHECK FIRST TIME FOR THIS OVERLAY            
         BE    GOAHEAD                                                          
         NI    REPMEDH+4,X'DF'     FORCE VALIDATION OF KEY FIELDS               
GOAHEAD  MVI   MYOVNUM,X'06'       STORE OVERLAY NUMBER                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PR                                                               
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
*                                                                               
VK       LA    R2,REPMEDH          VALIDATE MEDIA FIELD                         
         TM    4(R2),X'20'                                                      
         BO    VKCLT                                                            
         NI    REPCLTH+4,X'DF'                                                  
         NI    REPSTAH+4,X'DF'                                                  
         NI    REPESTH+4,X'DF'                                                  
         GOTO1 ANY                                                              
         GOTO1 VALIMED                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
VKCLT    LA    R2,REPCLTH          VALIDATE CLIENT FIELD                        
         TM    4(R2),X'20'                                                      
         BO    VKSTA                                                            
         NI    REPSTAH+4,X'DF'                                                  
         NI    REPESTH+4,X'DF'                                                  
         GOTO1 ANY                                                              
         GOTO1 VALICLT                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
VKSTA    LA    R2,REPSTAH          VALIDATE STATION FIELD                       
         TM    4(R2),X'20'                                                      
         BO    VKEST                                                            
         NI    REPESTH+4,X'DF'                                                  
         GOTO1 ANY                                                              
         TM    WHEN,X'40'          IF NOT 'NOW'                                 
         BNZ   *+14                                                             
         CLC   8(3,R2),=C'ALL'     THEN 'ALL' FOR ALL MARKETS                   
         BE    VKALL                                                            
         TM    4(R2),X'08'         NUMERIC MEANS MARKET NUMBER                  
         BO    VKMKT                                                            
         GOTO1 VALISTA             OTHERWISE VALIDATE STATION                   
         OI    4(R2),X'20'                                                      
         B     VKSTAX                                                           
*                                                                               
VKMKT    GOTO1 VALIMKT             VALIDATE MARKET                              
         B     VKSTAX                                                           
*                                                                               
VKALL    XC    BMKTSTA,BMKTSTA     ALL MARKETS/STATIONS                         
*                                                                               
VKSTAX   MVC   SVMKTSTA,BMKTSTA    SAVE REQUESTED MKT/STA                       
*                                                                               
VKEST    LA    R2,REPESTH          VALIDATE ESTIMATE FIELD                      
         TM    4(R2),X'20'                                                      
         BO    VKX                                                              
         GOTO1 ANY                                                              
         GOTO1 VALIMEST                                                         
         OI    4(R2),X'20'                                                      
*                                                                               
VKX      B     XIT                                                              
         EJECT                                                                  
* PRINT REPORT                                                                  
*                                                                               
PR       L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         L     R1,=A(HOOK)         SET UP HEADHOOK AND SPECS                    
         A     R1,APRELO                                                        
         ST    R1,HEADHOOK                                                      
         L     R1,=A(HEDSPECS)                                                  
         A     R1,APRELO                                                        
         ST    R1,SPECS                                                         
*                                                                               
         XC    BMKTSTA,BMKTSTA     INITIALIZE MKTSTA TO ZEROS                   
         XC    NTPMKT,NTPMKT       INITIALIZE CURRENT MARKET                    
*                                                                               
PR5      BAS   RE,NXMKTSTA         GET NEXT MARKET/STATION                      
         OC    BMKTSTA,BMKTSTA                                                  
         BZ    PRX                 DONE IF NO MORE                              
*                                                                               
         CLC   BMKT,NTPMKT         IF NEW MARKET                                
         BE    PR7                                                              
         GOTO1 CMPNTP              THEN COMPUTE NTP PERCENTAGE                  
         MVC   NTPMKT,BMKT         SAVE CURRENT MARKET                          
*                                                                               
PR7      GOTO1 CLRACC              CLEAR ACCUMULATORS                           
*                                                                               
         L     RE,RCHGTAB          INITIALIZE RATE CHANGE TABLE                 
         L     RF,=A(NUMRCHGS*RCHGTABL+1)                                       
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         BAS   RE,CONPRNT          PRINT CONTRACT INFORMATION                   
         BE    *+8                 SKIP HEADLINE COMMENTS IF TRUE               
         BAS   RE,HEADPRNT         PRINT HEADLINE COMMENTS                      
*                                                                               
         XC    HALFNUM,HALFNUM     INITIALIZE TO FIRST HALF OF YEAR             
         XC    TTOTAL,TTOTAL       INITIALIZE TOTAL TRADE DOLLARS               
*                                                                               
PR10     MVC   P1(L'HALFHEAD),HALFHEAD      MOVE HEADINGS TO PLINES             
         MVC   P2(L'HALFUNDR),HALFUNDR                                          
         MVI   P3,0                                                             
         LA    R4,P1+L'HALFHEAD             POINT PAST HEADINGS                 
         LA    R5,P2+L'HALFUNDR                                                 
         B     PR15                                                             
*                                                                               
HALFHEAD DC    C'DAY     TIME        PROGRAM        COST        '               
HALFUNDR DC    C'---     ----        -------        ----        '               
DASHES   DC    C'--------------'                                                
*                                                                               
PR15     OC    HALFNUM,HALFNUM     IF FIRST HALF OF YEAR                        
         BNZ   PR20                                                             
*                                                                               
         MVC   THISDATE,QSTART     POINT TO BEGINNING OF TABLES                 
         LA    R2,WPMTAB                                                        
         LA    R3,MONAMES                                                       
         B     PR30                                                             
*                                                                               
PR20     MVC   THISDATE,HALFDATE   ELSE POINT TO THE MIDDLE OF TABLES           
         L     R2,HALFWPM                                                       
         L     R3,HALFMON                                                       
*                                                                               
PR30     LA    R6,6                SET COUNTER FOR MONTHS IN A 1/2 YEAR         
*                                                                               
PR40     CLI   0(R2),4             TEST FOUR WEEKS THIS MONTH                   
         BNE   PR50                                                             
         MVC   0(11,R4),DASHES     MOVE IN FOUR WEEK HEADING                    
         MVC   4(3,R4),0(R3)                                                    
         LA    R4,13(R4)                                                        
         B     PR60                                                             
*                                                                               
PR50     MVC   0(14,R4),DASHES     MOVE IN FIVE WEEK HEADING                    
         MVC   6(3,R4),0(R3)                                                    
         LA    R4,16(R4)                                                        
*                                                                               
PR60     ZIC   R0,0(R2)            SET COUNTER FOR WEEKS THIS MONTH             
*                                                                               
PR70     MVC   0(2,R5),THISDATE+4       MOVE IN DATE                            
         LA    R5,3(R5)                                                         
         GOTO1 ADDAY,DMCB,THISDATE,THISDATE,7                                   
         BCT   R0,PR70             LOOP UNTIL END OF MONTH                      
*                                                                               
         LA    R5,1(R5)            BUMP LINE POINTER                            
         LA    R2,1(R2)                 WEEKS PER MONTH POINTER                 
         LA    R3,3(R3)                 MONTH NAMES POINTER                     
         BCT   R6,PR40             LOOP UNTIL NO MORE MONTHS                    
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)     PRINT IT OUT                                 
*                                                                               
         OC    HALFNUM,HALFNUM     IF JUST FINISHED FIRST HALF OF YEAR          
         BNZ   PR100                                                            
         MVC   HALFDATE,THISDATE   THEN SAVE POINTERS TO MIDDLE OF YEAR         
         ST    R2,HALFWPM                                                       
         ST    R3,HALFMON                                                       
         EJECT                                                                  
PR100    MVI   COSTFLAG,C'T'       DISPLAY TRADE SPOTS THEN CASH SPOTS          
         MVI   WEEKTOTS,C'N'       SUPPRESS WEEKLY TOTALS IF ALL ZEROS          
*                                                                               
PR110    LA    R7,KEY              BUILD PROGRAM RECORD KEY                     
         USING CSOKEY,R7                                                        
         XC    KEY,KEY                                                          
         MVI   CSOKTYPE,CSOKTYPQ                                                
         MVI   CSOKSTYP,CSOKSTPQ                                                
         MVC   CSOKAM,BAGYMD                                                    
         MVC   CSOKCLT,BCLT                                                     
         MVC   CSOKMKT(5),BMKTSTA                                               
         MVC   CSOKEST,BMEST                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         DROP  R7                                                               
*                                                                               
PR120    CLC   KEY(11),KEYSAVE     TEST NO MORE REFERENCES                      
         BNE   PR300                                                            
*                                                                               
         MVI   RDUPDATE,C'N'       GET RECORD                                   
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO              LOOP THROUGH WEEKLY ELEMENTS TO              
         MVI   ELCODE,WKCODEQ          DETERMINE IF NO SPOTS                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CSOWKEL,R6                                                       
*                                                                               
PR125    CLI   COSTFLAG,C'T'       TRADE SPOTS OR CASH SPOTS                    
         BNE   PR126                                                            
         OC    WKTSPOTS,WKTSPOTS                                                
         BNZ   PR128               OK - ACCEPT THIS PROGRAM                     
         B     PR127                                                            
*                                                                               
PR126    OC    WKCSPOTS,WKCSPOTS                                                
         BNZ   PR128               OK - ACCEPT THIS PROGRAM                     
*                                                                               
PR127    BAS   RE,NEXTEL                                                        
         BE    PR125                                                            
         B     PR270               NO SPOTS - SKIP THIS PROGRAM                 
         DROP  R6                                                               
*                                                                               
PR128    L     R6,AIO              USE DESC. ELEMENT FOR DAY,TIME,ETC.          
         MVI   ELCODE,DSCCODEQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CSODSCEL,R6                                                      
*                                                                               
         CLI   DSCWGT,0            IF NOT TRADE ONLY RECORD                     
         BE    *+8                                                              
         BAS   RE,ADDRCHG          THEN ADD RATES TO RATE CHANGE TABLE          
*                                                                               
         LA    R3,P1               MOVE IN DAY,TIME,ETC.                        
         USING PLIND,R3                                                         
         GOTO1 UNDAY,DMCB,DSCDAY,PLINDAY                                        
         GOTO1 UNTIME,DMCB,DSCTIME,PLINTIME                                     
         MVC   PLINPROG,DSCPROG                                                 
         LA    R4,PLINCOST         POINT TO COST FIELD                          
         DROP  R3,R6                                                            
         EJECT                                                                  
         MVI   ELCODE,WKCODEQ                                                   
         OC    HALFNUM,HALFNUM     IF FIRST HALF OF YEAR                        
         BNZ   PR130                                                            
*                                                                               
         L     R6,AIO              POINT TO FIRST ELEMENT                       
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CSOWKEL,R6                                                       
         ST    R6,FIRSTWK          SAVE POINTER TO FIRST ELEMENT                
*                                                                               
         LA    R2,WPMTAB           POINT TO BEGINNING OF TABLES                 
         LA    R7,ACCTAB                                                        
         USING ACCTABD,R7                                                       
         B     PR140                                                            
*                                                                               
PR130    L     R6,HALFWK           ELSE POINT TO MIDDLE OF TABLES               
         L     R2,HALFWPM                                                       
         L     R7,HALFACC                                                       
*                                                                               
PR140    LA    R3,6                SET COUNTER FOR MONTHS IN 1/2 YEAR           
         CLI   COSTFLAG,C'T'                                                    
         BNE   PR145               IF TRADE SPOTS THEN MOVE IN 'T'              
         MVI   0(R4),C'T'                                                       
         LA    R4,1(R4)                                                         
         CLI   TRADEOPT,C'Y'       DISPLAY RATES IF TRADE OPTION                
         BNE   PR150                                                            
*                                                                               
PR145    MVC   FULL,WKCOST         ELSE MOVE IN FIRST RATE                      
         EDIT  (4,FULL),(5,0(R4)),ALIGN=LEFT,ZERO=NOBLANK                       
         AR    R4,R0                                                            
         MVI   CHGCOST,0                                                        
*                                                                               
PR150    ZIC   R5,0(R2)            SET COUNTER FOR WEEKS THIS MONTH             
*                                                                               
PR160    CLI   COSTFLAG,C'T'       IF LOOKING AT TRADE SPOTS                    
         BNE   PR170                                                            
*                                  THEN ADD IN TRADE DOLLARS                    
         SR    RF,RF                                                            
         ICM   RF,3,WKTSPOTS                                                    
         ICM   R0,15,WKCOST                                                     
         MR    RE,R0                                                            
         L     R0,TTOTAL           ADD FULL VALUE TO TRADE TOTAL                
         AR    R0,RF                                                            
         ST    R0,TTOTAL                                                        
         M     RE,NTPPERC                                                       
         A     RF,=F'5000'                                                      
         D     RE,=F'10000'                                                     
         ICM   RE,15,ACCTWO                                                     
         AR    RE,RF                                                            
         STCM  RE,15,ACCTWO                                                     
         CLI   TRADEOPT,C'Y'       DISPLAY RATES IF TRADE DOLLARS               
         BE    PR175                                                            
         B     PR190                                                            
*                                  ELSE ADD IN CASH DOLLARS                     
PR170    SR    RF,RF                                                            
         ICM   RF,3,WKCSPOTS                                                    
         ICM   R0,15,WKCOST                                                     
         MR    RE,R0                                                            
         ICM   RE,15,ACCONE                                                     
         AR    RE,RF                                                            
         STCM  RE,15,ACCONE                                                     
*                                                                               
         LTR   RF,RF               IF NON-ZERO DOLLARS                          
         BZ    *+8                                                              
         MVI   WEEKTOTS,C'Y'       THEN PRINT WEEKLY TOTALS                     
*                                                                               
PR175    CLC   WKCOST,FULL         TEST CHANGE IN RATE                          
         BE    PR190                                                            
         MVC   FULL,WKCOST         SAVE NEW RATE                                
         CLI   CHGCOST,1                                                        
         BH    PR190               SKIP OUTPUT IF ALREADY CHANGED TWICE         
         BL    PR180                                                            
         MVC   0(2,R4),=C'**'      ** IF ALREADY CHANGED ONCE                   
         MVI   CHGCOST,2                                                        
         B     PR190                                                            
*                                                                               
PR180    MVI   0(R4),C'/'          OTHERWISE MOVE IN SECOND RATE                
         LA    R4,1(R4)                                                         
         EDIT  (4,FULL),(5,0(R4)),ALIGN=LEFT,ZERO=NOBLANK                       
         AR    R4,R0                                                            
         MVI   CHGCOST,1                                                        
*                                                                               
PR190    LA    R7,ACCTABL(R7)                                                   
         BAS   RE,NEXTEL                                                        
         BCT   R5,PR160            REPEAT UNTIL NO MORE WEEKS                   
*                                                                               
         LA    R2,1(R2)                                                         
         BCT   R3,PR150            REPEAT UNTIL NO MORE MONTHS                  
*                                                                               
         OC    HALFNUM,HALFNUM     IF JUST FINISHED FIRST HALF OF YEAR          
         BNZ   PR200                                                            
*                                                                               
         ST    R6,HALFWK           THEN SAVE POINTERS TO MIDDLE OF YEAR         
         ST    R7,HALFACC                                                       
         B     PR200                                                            
         EJECT                                                                  
PR200    OC    HALFNUM,HALFNUM     MOVE IN TRADE/CASH SPOTS                     
         BNZ   PR210                                                            
*                                                                               
         L     R6,FIRSTWK          POINTERS FOR FIRST HALF OF YEAR              
         LA    R2,WPMTAB                                                        
         B     PR220                                                            
*                                                                               
PR210    L     R6,HALFWK           POINTERS FOR SECOND HALF OF YEAR             
         L     R2,HALFWPM                                                       
*                                                                               
PR220    LA    R4,P1+L'HALFHEAD                                                 
         LA    R3,6                SET COUNTER TO SIX MONTHS                    
*                                                                               
PR230    ZIC   R5,0(R2)            SET COUNTER FOR WEEKS THIS MONTH             
*                                                                               
PR240    CLI   COSTFLAG,C'T'       IF TRADE SPOTS                               
         BNE   PR250                                                            
*                                  THEN MOVE IN TRADE SPOTS                     
         EDIT  (2,WKTSPOTS),(2,0(R4)),ZERO=NOBLANK                              
         B     PR260                                                            
*                                  ELSE MOVE IN CASH SPOTS                      
PR250    EDIT  (2,WKCSPOTS),(2,0(R4)),ZERO=NOBLANK                              
*                                                                               
PR260    LA    R4,3(R4)                                                         
         BAS   RE,NEXTEL                                                        
         BCT   R5,PR240            LOOP UNTIL NO MORE WEEKS                     
*                                                                               
         LA    R4,1(R4)                                                         
         LA    R2,1(R2)                                                         
         BCT   R3,PR230            LOOP UNTIL NO MORE MONTHS                    
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)     PRINT IT OUT                                 
         MVI   LINE,1              RESET LINE COUNTER TO 1                      
*                                                                               
PR270    MVI   RDUPDATE,C'N'       READ NEXT KEY                                
         GOTO1 SEQ                                                              
         B     PR120                                                            
*                                                                               
PR300    GOTO1 SPOOL,DMCB,(R8)                                                  
         CLI   COSTFLAG,C'T'       IF JUST LOOKED AT TRADE SPOTS                
         BNE   PR400                                                            
         MVI   COSTFLAG,C'C'       THEN SET FLAG TO LOOK AT CASH SPOTS          
         B     PR110                   AND DO IT AGAIN                          
         EJECT                                                                  
PR400    CLI   WEEKTOTS,C'Y'       IF WEEKLY TOTALS ARE NON-ZERO                
         BNE   PR490                                                            
*                                                                               
         OC    HALFNUM,HALFNUM     THEN PRINT WEEKLY DOLLARS                    
         BNZ   PR410                                                            
*                                                                               
         LA    R7,ACCTAB           FIRST HALF                                   
         LA    R2,WPMTAB                                                        
         B     PR420                                                            
*                                                                               
PR410    L     R7,HALFACC          SECOND HALF                                  
         L     R2,HALFWPM                                                       
*                                                                               
PR420    LA    R6,6                SET COUNTER TO SIX MONTHS                    
         MVI   BYTE,0                                                           
         LA    R3,P1+L'HALFHEAD-3       POINT TO PRINT LINES                    
         LA    R4,P2+L'HALFHEAD+0                                               
         MVI   P3,0                                                             
         MVC   P1+L'HALFHEAD-21(14),=C'WEEKLY DOLLARS'                          
*                                                                               
PR430    ZIC   R5,0(R2)            SET COUNTER FOR WEEKS THIS MONTH             
*                                                                               
PR440    CLI   BYTE,0              TEST TOP OR BOTTOM WEEK                      
         BNE   PR460                                                            
*                                  OUTPUT TOP WEEK                              
PR450    EDIT  (4,ACCONE),(5,0(R3)),ALIGN=RIGHT,ZERO=NOBLANK                    
         LA    R3,6(R3)                                                         
         MVI   BYTE,1                                                           
         B     PR470                                                            
*                                  OUTPUT BOTTOM WEEK                           
PR460    EDIT  (4,ACCONE),(5,0(R4)),ALIGN=RIGHT,ZERO=NOBLANK                    
         LA    R4,6(R4)                                                         
         MVI   BYTE,0                                                           
*                                                                               
PR470    LA    R7,ACCTABL(R7)                                                   
         BCT   R5,PR440            REPEAT UNTIL NO MORE WEEKS                   
*                                                                               
         LA    R2,1(R2)            NEXT MONTH                                   
         LA    R3,1(R3)                                                         
         LA    R4,1(R4)                                                         
         BCT   R6,PR430            REPEAT UNTIL NO MORE MONTHS                  
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)     WRITE OUT PRINT LINES                        
*                                                                               
PR490    L     RF,HALFNUM          DO NEXT HALF OF YEAR                         
         LA    RF,1(RF)                                                         
         ST    RF,HALFNUM                                                       
         C     RF,=F'1'                                                         
         BNH   PR10                                                             
         B     PR500               DONE SECOND HALF                             
*                                                                               
PR500    BAS   RE,FOOTPRNT         PRINT FOOTLINE COMMENTS                      
         MVI   FORCEHED,C'Y'                                                    
*                                  PRINT RATE CHANGE TABLE                      
         GOTO1 =A(PRNTRCHG),RR=APRELO                                           
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         B     PR5                 LOOP BACK FOR NEXT MKTSTA                    
*                                                                               
PRX      B     XIT                                                              
         EJECT                                                                  
* GET NEXT MKT/STA TO BE PROCESSED FOR MULTI-STATION REQUESTS.                  
*                                                                               
NXMKTSTA NTR1                                                                   
         LA    R6,KEY              BUILD KEY UP TO CLIENT                       
         USING CSOKEY,R6                                                        
         XC    KEY,KEY                                                          
         MVI   CSOKTYPE,CSOKTYPQ                                                
         MVI   CSOKSTYP,CSOKSTPQ                                                
         MVC   CSOKAM,BAGYMD                                                    
         MVC   CSOKCLT,BCLT                                                     
*                                                                               
         OC    BMKTSTA,BMKTSTA     IF FIRST TIME                                
         BNZ   NM10                                                             
         MVC   CSOKMKT(5),SVMKTSTA    SET MKTSTA TO START                       
         B     NM15                                                             
*                                                                               
NM10     OC    SVSTA,SVSTA         ELSE IF MKTSTA SPECIFIC                      
         BZ    NM12                                                             
         XC    BMKTSTA,BMKTSTA     THEN RETURN END OF STATIONS                  
         B     NMX                                                              
*                                                                               
NM12     MVC   CSOKMKT(5),BMKTSTA  ELSE SET MKTSTA TO CURRENT                   
         MVC   CSOKEST(3),=3X'FF'  SET EST HIGH TO GET NEXT STA                 
*                                                                               
NM15     MVI   RDUPDATE,C'N'       START KEY SEARCH                             
         GOTO1 HIGH                                                             
*                                                                               
NM20     CLC   KEY(5),KEYSAVE      IF CLIENT CHANGES                            
         BE    NM30                                                             
         XC    BMKTSTA,BMKTSTA     THEN RETURN END OF STATIONS                  
         B     NMX                                                              
*                                                                               
NM30     CLC   CSOKMKT(5),BMKTSTA  IF MKTSTA CHANGED                            
         BE    NM90                                                             
         OC    CSOKSTA,CSOKSTA     AND NOT STATION PERC REC                     
         BE    NM90                                                             
*                                                                               
NM40     OC    SVMKT,SVMKT         AND MKT MATCHES                              
         BZ    NM50                                                             
         CLC   CSOKMKT,SVMKT                                                    
         BNE   NM90                                                             
*                                                                               
NM50     OC    SVSTA,SVSTA         AND STA MATCHES                              
         BZ    NM60                                                             
         CLC   CSOKSTA,SVSTA                                                    
         BNE   NM90                                                             
*                                                                               
NM60     CLC   CSOKEST,BMEST       AND ESTIMATE MATCHES                         
         BE    NM100               THEN RETURN MKTSTA                           
*                                                                               
NM90     MVI   RDUPDATE,C'N'       ELSE READ NEXT KEY                           
         GOTO1 SEQ                                                              
         B     NM20                AND LOOP BACK                                
*                                                                               
NM100    MVC   BMKTSTA,CSOKMKT     RETURN MKTSTA                                
*                                                                               
         XC    FAKEFLD,FAKEFLD                                                  
         LA    R2,FAKEFLD                                                       
         MVI   0(R2),13                                                         
         GOTO1 MSUNPK,DMCB,CSOKMKT,FULL,8(R2)                                   
         MVI   5(R2),5                                                          
         CLI   12(R2),C' '                                                      
         BH    NM110                                                            
         MVI   5(R2),4                                                          
         CLI   11(R2),C' '                                                      
         BH    NM110                                                            
         MVI   5(R2),3                                                          
*                                                                               
NM110    MVC   SAVEMKST,BMKTSTA    NEVER ALLOW VALISTA TO                       
         GOTO1 VALISTA             RESET BMKTSTA - NO MATTER WHAT               
         MVC   BMKTSTA,SAVEMKST                                                 
*                                                                               
NMX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* PRINT CONTRACT INFORMATION BEFORE HEADLINE COMMENTS                           
*                                                                               
CONPRNT  NTR1                                                                   
         LA    R5,KEY              BUILD KEY OF CONTRACT RECORD                 
         USING CNTKEY,R5                                                        
         XC    KEY,KEY                                                          
         MVI   CNTKTYPE,CNTKTYPQ                                                
         MVI   CNTKSTYP,CNTKSTPQ                                                
         MVC   CNTKAM,BAGYMD                                                    
         MVC   CNTKCLT,BCLT                                                     
         MVC   CNTKMKT(5),BMKTSTA                                               
         GOTO1 DATCON,DMCB,QSTART,(2,CNTKDATE)                                  
*                                                                               
         GOTO1 HIGH                READ FOR KEY                                 
         CLC   KEY(13),KEYSAVE                                                  
         BNE   CPNO                DONE IF REC NOT FOUND                        
*                                                                               
         GOTO1 GETREC              READ RECORD                                  
*                                                                               
         L     R6,AIO              R6 = A(FIRST CONTRACT ELEMENT)               
         MVI   ELCODE,COCODEQ                                                   
         BAS   RE,GETEL                                                         
         BNE   CPNO                RETURN IF NONE FOUND                         
         USING COELEM,R6                                                        
*                                  FIRST PRINT LINE CONTAINS HEADING            
         MVC   P(17),=C'STATION RECEIVES:'                                      
*                                                                               
*                                  FOR EACH CONTRACT ELEM...                    
CP10     LA    R4,P+18             FILL IN NUMBER OF SEGMENTS                   
         EDIT  (1,COSEG),(3,(R4)),ALIGN=LEFT,ZERO=NOBLANK                       
         AR    R4,R0                                                            
         LA    R4,1(R4)                                                         
*                                                                               
         BAS   RE,SHOPRNT          FILL IN SHOW INFO                            
*                                                                               
         CLI   CODAYS,0            IF NO DAYS/TIMES                             
         BNE   CP20                                                             
         MVC   0(3,R4),=C'TBD'     FILL IN 'TDB'                                
         LA    R4,4(R4)                                                         
         B     CP30                                                             
*                                  ELSE FILL IN DAYS                            
CP20     GOTO1 UNDAY,DMCB,CODAYS,0(R4)                                          
         LA    R4,7(R4)                                                         
         CLI   0(R4),C' '                                                       
         BH    *+10                                                             
         BCTR  R4,0                                                             
         B     *-10                                                             
         LA    R4,2(R4)                                                         
*                                  FILL IN TIMES                                
         GOTO1 UNTIME,DMCB,COTIMES,0(R4)                                        
         LA    R4,10(R4)                                                        
         CLI   0(R4),C' '                                                       
         BH    *+10                                                             
         BCTR  R4,0                                                             
         B     *-10                                                             
         LA    R4,2(R4)                                                         
*                                  FILL IN FROM/TO DATES                        
CP30     GOTO1  DATCON,DMCB,(3,COFTD),(5,0(R4))                                 
         MVI    8(R4),C'-'                                                      
         LA     R4,9(R4)                                                        
         GOTO1  DATCON,DMCB,(3,COLTD),(5,0(R4))                                 
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)     WRITE OUT PRINT LINE                         
*                                                                               
         MVI   ELCODE,COCODEQ      RESTORE ELEMENT CODE                         
         BAS   RE,NEXTEL           GET NEXT CONTRACT ELEMENT                    
         BE    CP10                LOOP BACK IF MORE CONTRACT ELEMS             
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)     PRINT BLANK LINE                             
*                                                                               
CPYES    SR    RC,RC               FOUND CONTRACT RECORD                        
CPNO     LTR   RC,RC               FOUND NO CONTRACT RECORD                     
CPX      B     XIT                                                              
         EJECT                                                                  
* MOVE SHOW RECORD DETAILS INTO PRINT LINE                                      
*                                                                               
SHOPRNT  NTR1                                                                   
         LA    R5,KEY              BUILD KEY FOR SHOW RECORD                    
         USING SHOKEY,R5                                                        
         XC    KEY,KEY                                                          
         MVI   SHOKTYPE,SHOKTYPQ                                                
         MVI   SHOKSTYP,SHOKSTPQ                                                
         MVC   SHOKCODE,COSHOW                                                  
*                                                                               
         GOTO1 HIGH                READ FOR SHOW KEY                            
         CLC   KEY(13),KEYSAVE                                                  
         BNE   SPX                 DONE IF NONE FOUND                           
*                                                                               
         MVC   AIO,AIO2            READ SHOW RECORD                             
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
         L     R6,AIO2             R6 = A(SHOW ELEMENT)                         
         MVI   ELCODE,SHCODEQ                                                   
         BAS   RE,GETEL                                                         
         BNE   SPX                 DONE IF NONE FOUND                           
         USING SHELEM,R6                                                        
*                                  FILL IN SHOW DURATION                        
         EDIT  (1,SHDUR),(3,0(R4)),ALIGN=LEFT,ZERO=NOBLANK                      
         AR    R4,R0                                                            
         LA    R4,1(R4)                                                         
         MVC   0(27,R4),=C'MINUTE SEGMENTS PER WEEK OF'                         
         LA    R4,28(R4)                                                        
*                                                                               
         MVC   0(30,R4),SHNAME     FILL IN SHOW NAME                            
         LA    R4,29(R4)                                                        
         CLI   0(R4),C' '                                                       
         BH    *+10                                                             
         BCTR  R4,0                                                             
         B     *-10                                                             
         LA    R4,2(R4)                                                         
*                                                                               
SPX      XIT1  REGS=(R4)           RETAIN R4 FOR CALLING ROUTINE                
         DROP  R5,R6                                                            
         EJECT                                                                  
* PRINT HEADLINE COMMENTS ROUTINE                                               
*                                                                               
HEADPRNT NTR1                                                                   
         L     R6,AIO2             MARK IOAREA IN CASE RECORD NOT FOUND         
         MVI   0(R6),0                                                          
*                                                                               
         LA    R6,KEY              BUILD KEY                                    
         USING COMKEY,R6                                                        
         XC    KEY,KEY                                                          
         MVI   COMKTYPE,COMKTYPQ   REPORT COMMENTS RECORD TYPE                  
         MVI   COMKSTYP,COMKSTPQ   RECORD SUB-TYPE                              
         MVC   COMKAM,BAGYMD                                                    
         MVC   COMKCLT,BCLT                                                     
         MVC   COMKMKT(5),BMKTSTA                                               
         MVC   COMKEST,BMEST                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         DROP  R6                                                               
*                                                                               
         CLC   KEY(11),KEYSAVE     TEST COMMENTS EXISTS                         
         BNE   XIT                                                              
*                                                                               
         MVC   AIO,AIO2            READ THEM INTO IO2                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
         L     R6,AIO2             GET HEADLINE COMMENTS                        
         MVI   ELCODE,HEDCODEQ                                                  
         BAS   RE,GETEL                                                         
         BNE   XIT                 NO HEADLINE COMMENTS                         
         USING COMHEDEL,R6                                                      
*                                                                               
         MVC   P1(18),=C'STATION RECEIVES :'                                    
*                                                                               
HP10     MVC   P1+19(L'HEDTEXT),HEDTEXT                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
         BAS   RE,NEXTEL                                                        
         BE    HP10                KEEP GOING UNTIL NO MORE HEADLINES           
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)     PRINT BLANK LINE                             
         B     XIT                                                              
         EJECT                                                                  
FOOTPRNT NTR1                                                                   
         LA    R7,ACCTAB                                                        
*                                  TOTAL CASH DOLLARS IN FIRST FOOTLINE         
         GOTO1 TOTFULL,DMCB,ACCONE,ACCTABL                                      
         OC    0(4,R1),0(R1)                                                    
         BZ    FP3                                                              
         MVC   P1+106(16),=C' CASH DOLLARS  $'                                  
         EDIT  (4,0(R1)),(10,P1+122),ALIGN=LEFT,COMMAS=YES,ZERO=NOBLANK         
*                                  TRADE DOLLARS IN SECOND FOOTLINE             
FP3      CLI   TRADEOPT,C'Y'                                                    
         BNE   FP5                 SKIP IF OPTION NOT SELECTED                  
         MVC   P2+106(16),=C'TRADE DOLLARS  $'                                  
         L     R1,TTOTAL                                                        
         M     R0,NTPPERC          APPLY NTP% NOW                               
         A     R1,=F'5000'                                                      
         D     R0,=F'10000'                                                     
         EDIT  (R1),(10,P2+122),ALIGN=LEFT,COMMAS=YES,ZERO=NOBLANK              
*                                                                               
FP5      L     R6,AIO2             FIND FOOTLINE ELEMENTS IN IO2                
         CLI   0(R6),0                                                          
         BE    FP30                NO COMMENTS RECORD                           
*                                                                               
         MVI   ELCODE,FOTCODEQ                                                  
         BAS   RE,GETEL                                                         
         BNE   FP30                NO FIRST FOOTLINE                            
         USING COMFOTEL,R6                                                      
*                                                                               
         MVC   P1(L'FOTTEXT),FOTTEXT                                            
         BAS   RE,NEXTEL                                                        
         BNE   FP30                NO SECOND FOOTLINE                           
         MVC   P2(L'FOTTEXT),FOTTEXT                                            
         B     FP20                                                             
*                                  THIRD FOOTLINES AND BEYOND                   
FP10     MVC   P1(L'FOTTEXT),FOTTEXT                                            
*                                                                               
FP20     GOTO1 SPOOL,DMCB,(R8)                                                  
         BAS   RE,NEXTEL                                                        
         BE    FP10                KEEP GOING UNTIL NO MORE FOOTLINES           
         B     XIT                                                              
*                                                                               
FP30     GOTO1 SPOOL,DMCB,(R8)     PRINT DOLLAR TOTALS AND EXIT                 
         B     XIT                                                              
         EJECT                                                                  
* ADD PROGRAM RECORD'S RATES TO RATE CHANGE TABLE                               
*                                                                               
ADDRCHG  NTR1                                                                   
*                                                                               
         L     R6,AIO              R6 = A(DESCRIPTION ELEMENT)                  
         MVI   ELCODE,DSCCODEQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CSODSCEL,R6                                                      
*                                                                               
         L     R5,RCHGTAB          R5 = A(RATE CHANGE TABLE)                    
         SR    RF,RF                                                            
         USING RCHGTABD,R5                                                      
*                                                                               
ARC10    CLI   0(R5),0             WHILE NOT END OF TABLE                       
         BE    ARC30                                                            
*                                                                               
         CLC   RCDAYS,DSCDAY       IF TABLE ENTRY ALREADY FOUND                 
         BNE   ARC20                                                            
         CLC   RCTIMES,DSCTIME                                                  
         BNE   ARC20                                                            
         CLC   RCREF,KEY+CSOKREF-CSOKEY                                         
         BE    ARCX                THEN EXIT WITHOUT ADDING IT                  
*                                                                               
ARC20    LA    R5,RCHGTABL(R5)     BUMP R5 TO NEXT ENTRY                        
         LA    RF,1(RF)                                                         
         B     ARC10                                                            
*                                                                               
ARC30    C     RF,=A(NUMRCHGS)     MAKE SURE WE HAVEN'T GONE PAST END           
         BNL   ARCX                                                             
*                                                                               
         MVC   RCDAYS,DSCDAY       COPY DAYS/TIMES/REF TO TABLE ENTRY           
         MVC   RCTIMES,DSCTIME                                                  
         MVC   RCREF,KEY+CSOKREF-CSOKEY                                         
*                                                                               
         L     R6,AIO              R6 = A(FIRST WEEKLY ELEMENT)                 
         MVI   ELCODE,WKCODEQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CSOWKEL,R6                                                       
*                                                                               
         LA    R4,RCRATES          R4 = A(FIRST WEEK'S RATE)                    
*                                                                               
ARC50    MVC   0(4,R4),WKCOST      MOVE THIS WEEK'S RATE TO TABLE ENTRY         
*                                                                               
         LA    R4,4(R4)            R4 = A(NEXT RATE)                            
         BAS   RE,NEXTEL           R6 = A(NEXT WEEKLY ELEMENT)                  
         BE    ARC50               CONTINUE UNTIL END OF ELEMS                  
*                                                                               
ARCX     B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
* GETEL AND ERROR EXITS                                                         
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
EXIT     OI    REPMEDH+6,X'40'     SET CURSOR                                   
         LA    R2,CONHEADH                                                      
         XC    CONHEAD,CONHEAD                                                  
         MVC   8(14,R2),=C'DATA DISPLAYED'                                      
         B     XIT                                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* HEADLINE HOOK ROUTINE                                                         
*                                                                               
HOOK     NTR1  BASE=*,LABEL=*                                                   
*                                  MOVE MASTER EST DESC INTO WORK AREA          
         MVC   HEADWORK(20),QDESC                                               
*                                  FIND LAST NON-SPACE CHAR                     
         LA    RE,HEADWORK+19                                                   
         CLI   0(RE),X'40'                                                      
         BH    *+10                                                             
         BCTR  RE,0                                                             
         B     *-10                                                             
*                                  COMPUTE LENGTH AND CENTER IT                 
         LA    RF,HEADWORK                                                      
         SR    RE,RF                                                            
         SRL   RE,1                                                             
         LA    RF,H3+58                                                         
         SR    RF,RE                                                            
         MVC   0(20,RF),HEADWORK                                                
*                                  PRINT START AND END YEARS                    
         GOTO1 DATCON,DMCB,(0,QSTART),(20,DUB)                                  
         MVC   H4+42(4),DUB                                                     
         MVI   H4+46,C'-'                                                       
         GOTO1 DATCON,DMCB,(0,QEND),(20,DUB)                                    
         MVC   H4+47(4),DUB                                                     
         EDIT  (1,MASTSPLN),(2,H4+53)                                           
*                                                                               
         MVC   H5+49(L'STAPRNT),STAPRNT    PRINT STATION NAME                   
         MVC   H5+58(L'MKTNM),MKTNM        PRINT MARKET NAME                    
*                                                                               
         MVI   H6,0                                                             
         XIT1                                                                   
*                                                                               
HEDSPECS SSPEC H1,44,C'EXHIBIT A TO STANDARD CONTRACT'                          
         SSPEC H2,1,AGYNAME                                                     
         SSPEC H2,43,C'G E N E R A L   M I L L S   I N C'                       
         SSPEC H2,100,PAGE                                                      
         SSPEC H2,110,RUN                                                       
         SSPEC H3,1,AGYADD                                                      
         SSPEC H3,100,REQUESTOR                                                 
         SSPEC H4,53,C':   COMMERCIAL SCHEDULE'                                 
         DC    H'0'                                                             
         EJECT                                                                  
* LOOP THROUGH RATE CHANGE TABLE AND DATE CHANGE TABLE, PRINTING GRID           
*                                                                               
PRNTRCHG NTR1  BASE=*,LABEL=*                                                   
         L     R6,RCHGTAB          IF NO CASH PROGRAM RECORDS THEN DONE         
         CLI   0(R6),0                                                          
         BE    PRCX                                                             
*                                                                               
         BAS   RE,BLDDCHG          BUILD DATE CHANGE TAB FROM RCHGTAB           
*                                                                               
         ZIC   R3,NUMDCENT         COMPUTE COLUMN FOR CENTERING HEADING         
         M     R2,=F'12'                                                        
         LA    R3,24(R3)                                                        
         SRL   R3,1                                                             
*                                                                               
         LA    R3,P(R3)            POINT TO CENTER OF PRINT LINE                
*                                                                               
         MVC   0(10,R3),=C'RATE CHART'          PRINT                           
         GOTO1 SPOOL,DMCB,(R8)                 HEADING...                       
         MVC   0(10,R3),=C'----------'                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         LA    R5,DCHGTAB          R5 = A(FIRST DATE CHG TABLE ENTRY)           
         USING DCHGTABD,R5                                                      
*                                                                               
         MVC   THISDATE,QSTART     INITIALIZE DATE TO BEGIN OF YEAR             
         BAS   RE,FMTSTART         FORMAT BEGIN OF DATE RANGE                   
*                                                                               
         LA    R4,21               R4 = DISPLACEMENT WITHIN PRINT LINE          
*                                                                               
PRC10    CLI   0(R5),0             WHILE NOT END OF DATE CHANGES                
         BE    PRC50                                                            
*                                  CONVERT DATE RANGE START TO YYMMDD           
         GOTO1 DATCON,DMCB,(2,DCDATE),(0,THISDATE)                              
*                                                                               
         BAS   RE,FMTEND           FINISH FORMATTING PREV DATE RANGE            
         LA    R4,12(R4)           BUMP DISP TO NEXT DATE RANGE                 
         BAS   RE,FMTSTART         BEGIN FORMATTING CURRENT DATE RNG            
*                                                                               
         LA    R5,DCHGTABL(R5)     BUMP TO NEXT DCHGTAB ENTRY                   
         B     PRC10               LOOP BACK                                    
*                                                                               
PRC50    MVC   THISDATE,QEND       SET DATE TO END OF YEAR + 1                  
         GOTO1 ADDAY,DMCB,THISDATE,THISDATE,1                                   
         BAS   RE,FMTEND           FINISH FORMATTING PREV DATE RANGE            
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)     PRINT IT OUT                                 
*                                                                               
         L     R6,RCHGTAB          R6 = A(FIRST RATE TABLE ENTRY)               
         USING RCHGTABD,R6                                                      
*                                                                               
PRC60    CLI   0(R6),0             WHILE NOT END OF RATE TABLE                  
         BE    PRC100                                                           
*                                                                               
         GOTO1 UNDAY,DMCB,RCDAYS,P                                              
         GOTO1 UNTIME,DMCB,RCTIMES,P+8                                          
         LA    R4,P+22                                                          
         EDIT  (4,RCRATES),(6,0(R4)),ALIGN=RIGHT,ZERO=NOBLANK                   
         LA    R4,P+34                                                          
*                                                                               
         LA    R5,DCHGTAB          R5 = A(FIRST DATE CHG TABLE ENTRY)           
*                                                                               
PRC70    CLI   0(R5),0             WHILE NOT END OF DATE CHG TABLE              
         BE    PRC90                                                            
*                                                                               
         ICM   R2,15,DCDISP        MOVE THIS WEEK'S RATE TO PRINT LINE          
         LA    R2,RCRATES(R2)                                                   
         EDIT  (4,0(R2)),(6,0(R4)),ALIGN=RIGHT,ZERO=NOBLANK                     
         LA    R4,12(R4)                                                        
         LA    R5,DCHGTABL(R5)                                                  
         B     PRC70                                                            
*                                                                               
PRC90    GOTO1 SPOOL,DMCB,(R8)     PRINT IT OUT                                 
*                                                                               
         LA    R6,RCHGTABL(R6)     BUMP R6 TO NEXT PROGRAM                      
         B     PRC60               AND LOOP BACK                                
*                                                                               
PRC100   DS    0H                                                               
*                                                                               
PRCX     XIT1                                                                   
         DROP  R5,R6                                                            
*                                                                               
* FORMAT FIRST HALF OF DATE RANGE INTO MYWORK.                                  
*                                                                               
FMTSTART NTR1  BASE=*,LABEL=*                                                   
         MVC   MYWORK(11),=CL11' '                                              
         LA    R2,MYWORK                                                        
         GOTO1 DATCON,DMCB,THISDATE,(3,NEXTDATE)                                
         EDIT  (1,NEXTDATE+1),(2,(R2)),ALIGN=LEFT                               
         AR    R2,R0                                                            
         MVI   0(R2),C'/'                                                       
         LA    R2,1(R2)                                                         
         EDIT  (1,NEXTDATE+2),(2,(R2)),ALIGN=LEFT                               
         AR    R2,R0                                                            
         MVI   0(R2),C'-'                                                       
         LA    R2,1(R2)                                                         
         LA    RF,MYWORK                                                        
         SR    R2,RF                                                            
         ST    R2,FULL                                                          
         XIT1                                                                   
*                                                                               
* FORMAT SECOND HALF OF DATE RANGE INTO MYWORK.  FROM THE LENGTH                
* OF MYWORK AND THE DIPLACEMENT WITHIN THE PRINT LINE PASSED IN                 
* R4 MOVE THE DATE RANGE AND DASHES INTO PRINT LINES 1 AND 2.                   
*                                                                               
FMTEND   NTR1  BASE=*,LABEL=*                                                   
         LA    R3,1                                                             
         LNR   R3,R3                                                            
         GOTO1 ADDAY,DMCB,THISDATE,LASTDATE,(R3)                                
         GOTO1 DATCON,DMCB,LASTDATE,(3,NEXTDATE)                                
         LA    R2,MYWORK                                                        
         A     R2,FULL                                                          
         EDIT  (1,NEXTDATE+1),(2,(R2)),ALIGN=LEFT                               
         AR    R2,R0                                                            
         MVI   0(R2),C'/'                                                       
         LA    R2,1(R2)                                                         
         EDIT  (1,NEXTDATE+2),(2,(R2)),ALIGN=LEFT                               
         AR    R2,R0                                                            
         LA    RF,MYWORK                                                        
         SR    R2,RF                                                            
         ST    R2,FULL             STORE LENGTH OF DATE RANGE IN FULL           
*                                                                               
         LA    R3,11               CENTER WITHIN 11 BYTE AREA                   
         S     R3,FULL                                                          
         SRL   R3,1                                                             
         AR    R3,R4               R3 = DISPLACEMENT TO CENTERED SPOT           
*                                                                               
         LA    R2,P1               MOVE DATE RANGE TO P1                        
         AR    R2,R3                                                            
         MVC   0(11,R2),MYWORK                                                  
*                                                                               
         LA    R2,P2               MOVE DASHES TO P2                            
         AR    R2,R3                                                            
         L     RF,FULL                                                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),=C'-----------'                                          
         XIT1                                                                   
         EJECT                                                                  
* LOOP THROUGH RATE CHANGE TABLE AND BUILD DATE CHANGE TABLE.                   
*                                                                               
BLDDCHG  NTR1  BASE=*,LABEL=*                                                   
         LA    R5,DCHGTAB          R5 = A(FIRST DATE CHG TABLE ENTRY)           
         XC    DCHGTAB,DCHGTAB                                                  
         SR    R4,R4               R4 = NUMBER OF TABLE ENTRIES                 
         USING DCHGTABD,R5                                                      
*                                                                               
         MVC   THISDATE,QSTART     START CURRENT AT SECOND WEEK                 
         GOTO1 ADDAY,DMCB,THISDATE,THISDATE,7                                   
         MVC   CURDISP,=F'4'                                                    
*                                                                               
         XC    PREVDISP,PREVDISP   START PREV AT FIRST WEEK                     
*                                                                               
BDC10    L     R6,RCHGTAB          R6 = A(FIRST RATE TABLE ENTRY)               
         USING RCHGTABD,R6                                                      
*                                                                               
BDC20    L     RF,CURDISP          COMPARE CURRENT WEEK'S RATE WITH             
         LA    RF,RCRATES(RF)           PREVIOUS FOR THIS PROGRAM               
         ICM   RE,15,0(RF)                                                      
         L     RF,PREVDISP                                                      
         LA    RF,RCRATES(RF)                                                   
         ICM   RF,15,0(RF)                                                      
         CR    RE,RF                                                            
         BE    BDC30                                                            
*                                  RATE CHANGED THIS WEEK - SAVE INFO           
         GOTO1 DATCON,DMCB,THISDATE,(2,DCDATE)                                  
         MVC   DCDISP,CURDISP                                                   
*                                                                               
         LA    R5,DCHGTABL(R5)     BUMP TO NEXT DATE CHANGE ENTRY               
         MVC   PREVDISP,CURDISP    SET THIS WEEK TO PREV                        
         LA    R4,1(R4)            BUMP NUMBER OF TABLE ENTRIES                 
         B     BDC90               BUMP TO NEXT WEEK                            
*                                                                               
BDC30    LA    R6,RCHGTABL(R6)     BUMP R6 TO NEXT PROGRAM                      
         CLI   0(R6),0                                                          
         BNE   BDC20               REPEAT UNTIL END OF PROGRAMS                 
*                                                                               
BDC90    L     RF,CURDISP          BUMP CURRENT DISPLACEMENT AND DATE           
         LA    RF,4(RF)                                                         
         ST    RF,CURDISP                                                       
         GOTO1 ADDAY,DMCB,THISDATE,THISDATE,7                                   
         CLC   THISDATE,QEND                                                    
         BL    BDC10               REPEAT UNTIL END OF YEAR                     
*                                                                               
         STC   R4,NUMDCENT         SAVE NUMBER OF TABLE ENTRIES                 
*                                                                               
BDCX     XIT1                                                                   
         DROP  R5,R6                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPCSOFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPCSOF6D                                                       
         EJECT                                                                  
       ++INCLUDE SPGENCSO                                                       
         EJECT                                                                  
       ++INCLUDE SPCSOWORKD                                                     
         EJECT                                                                  
* WORK AREA                                                                     
*                                                                               
         ORG   SYSSPARE                                                         
         DS    0F                                                               
APRELO   DS    A                                                                
RCHGTAB  DS    A                   NMOD AREA FOR RATE CHANGE TABLE              
DCHGTAB  DS    XL(NUMDCHGS*DCHGTABL+1)   DATE CHANGE TABLE                      
NUMDCENT DS    XL1                 NUMBER OF DATE CHANGE ENTRIES                
CURDISP  DS    F                                                                
PREVDISP DS    F                                                                
*                                                                               
HALFNUM  DS    F                   HALF OF YEAR (0=FIRST, 1=SECOND)             
HALFDATE DS    CL6                 START DATE OF THIS HALF OF YEAR              
HALFWPM  DS    A                   A(WEEKS/MONTH FOR THIS HALF)                 
HALFMON  DS    A                   A(MONTH NAMES FOR THIS HALF)                 
HALFACC  DS    A                   A(ACCUMULATORS FOR THIS HALF)                
TRADEOPT DS    C                   REPORT OPTION (Y=TRADE, N=REGULAR)           
COSTFLAG DS    C                   C=CASH, T=TRADE SPOTS PRINTING               
CHGCOST  DS    C                   FLAGS IF COST HAS CHANGED THIS WEEK          
FIRSTWK  DS    A                   A(ELEMENTS FOR FIRST HALF OF YEAR)           
HALFWK   DS    A                   A(ELEMENTS FOR SECOND HALF)                  
HEADWORK DS    CL80                WORK AREA TO PRINT HEADING                   
TTOTAL   DS    F                   TOTAL TRADE DOLLARS                          
WEEKTOTS DS    C                   FLAGS IF WEEKLY TOTALS NON-ZERO              
SVMKTSTA DS    0XL5                                                             
SVMKT    DS    XL2                 REQUESTED MARKET OR ZERO FOR ALL             
SVSTA    DS    XL3                 REQUESTED STATION OF ZERO FOR ALL            
FAKEFLD  DS    XL20                FAKE TWA FIELD FOR STATION VAL               
NTPMKT   DS    XL2                 MKTNUM LAST TIME NTPPERC WAS CALCED          
SAVEMKST DS    XL5                 PRESERVE MKT/STA THRU VALISTA CALL           
*                                                                               
PLIND    DSECT                                                                  
PLINDAY  DS    CL7                                                              
         DS    C                                                                
PLINTIME DS    CL11                                                             
         DS    C                                                                
PLINPROG DS    CL14                                                             
         DS    C                                                                
PLINCOST DS    CL12                                                             
*                                                                               
RCHGTABD DSECT                                                                  
RCDAYS   DS    XL1                                                              
RCTIMES  DS    XL4                                                              
RCREF    DS    XL1                                                              
RCRATES  DS    XL212                                                            
RCHGTABL EQU   *-RCHGTABD                                                       
NUMRCHGS EQU   20                                                               
*                                                                               
DCHGTABD DSECT                                                                  
DCDATE   DS    XL2                                                              
DCDISP   DS    XL4                                                              
DCHGTABL EQU   *-DCHGTABD                                                       
NUMDCHGS EQU   20                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'240SPCSO06   09/12/03'                                      
         END                                                                    
