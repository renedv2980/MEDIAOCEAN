*          DATA SET SPCSO01    AT LEVEL 066 AS OF 05/07/03                      
*PHASE T21801A,*                                                                
         TITLE 'T21801 - CHILD SPOT PROGRAM RECORD MAINTENANCE'                 
T21801   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21801                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         USING T21801,RB,R7                                                     
         LA    R7,2048(RB)                                                      
         LA    R7,2048(R7)                                                      
*                                                                               
         CLI   MYOVNUM,X'01'       CHECK FIRST TIME FOR THIS OVERLAY            
         BE    GOAHEAD                                                          
         NI    PROMEDH+4,X'DF'     FORCE VALIDATION OF KEY FIELDS               
GOAHEAD  MVI   MYOVNUM,X'01'       STORE OVERLAY NUMBER                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,XRECPUT                                                     
         BE    DR                                                               
         CLI   MODE,XRECADD                                                     
         BE    DR                                                               
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
*                                                                               
VK       LA    R2,PROMEDH          VALIDATE MEDIA FIELD                         
         TM    4(R2),X'20'                                                      
         BO    VKCLT                                                            
         NI    PROCLTH+4,X'DF'                                                  
         GOTO1 ANY                                                              
         GOTO1 VALIMED                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
VKCLT    LA    R2,PROCLTH          VALIDATE CLIENT FIELD                        
         TM    4(R2),X'20'                                                      
         BO    VKSTA                                                            
         NI    PROSTAH+4,X'DF'                                                  
         GOTO1 ANY                                                              
         GOTO1 VALICLT                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
VKSTA    LA    R2,PROSTAH          VALIDATE STATION FIELD                       
         TM    4(R2),X'20'                                                      
         BO    VKEST                                                            
         NI    PROESTH+4,X'DF'                                                  
         GOTO1 ANY                                                              
         GOTO1 VALISTA                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
VKEST    LA    R2,PROESTH          VALIDATE ESTIMATE FIELD                      
         TM    4(R2),X'20'                                                      
         BO    VKESTX                                                           
         NI    PROREFH+4,X'DF'                                                  
         GOTO1 ANY                                                              
         GOTO1 VALIMEST                                                         
         OI    4(R2),X'20'                                                      
VKESTX   DS    0H                                                               
*                                                                               
VKREF    LA    R2,PROREFH          VALIDATE REFERENCE FIELD                     
         XC    BREF,BREF                                                        
         CLI   ACTNUM,ACTADD                                                    
         BE    VKX                 IGNORE IF ACTION ADD                         
         CLI   ACTNUM,ACTLIST                                                   
         BNE   VKREF10             OR IF ACTION LIST AND FIELD EMPTY            
         CLI   5(R2),0                                                          
         BE    VKX                                                              
VKREF10  GOTO1 ANY                                                              
         GOTO1 VALIREF                                                          
*                                                                               
VKX      LA    R6,KEY              BUILD KEY                                    
         USING CSOKEY,R6                                                        
         XC    KEY,KEY                                                          
         MVI   CSOKTYPE,CSOKTYPQ   CSO RECORD TYPE                              
         MVI   CSOKSTYP,CSOKSTPQ   CSO RECORD SUB-TYPE                          
         MVC   CSOKAM,BAGYMD                                                    
         MVC   CSOKCLT,BCLT                                                     
         MVC   CSOKMKT(5),BMKTSTA                                               
         MVC   CSOKEST,BMEST                                                    
         MVC   CSOKREF,BREF                                                     
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* VALIDATE RECORD                                                               
*                                                                               
VR       BAS   RE,TESTPREV         IF NO FIELDS MODIFIED                        
         CLI   PREVFLAG,0                                                       
         BNE   VR2                                                              
         CLI   ACTNUM,ACTSEL                                                    
         BNE   ERRNOINP            ERROR NO INPUT IF NOT SEL ACTION             
         B     XIT                 ELSE EXIT                                    
*                                                                               
VR2      CLI   ACTNUM,ACTADD       TEST IF ADD RECORD                           
         BNE   VR20                                                             
*                                                                               
         LA    R2,PRODAYH          VALIDATE DAY FIELD                           
         GOTO1 ANY                                                              
         ZIC   R5,5(R2)                                                         
         GOTO1 DAYVAL,DMCB,((R5),PRODAY),HALF,MYWORK                            
         CLI   HALF,0                                                           
         BE    INVERR                                                           
*                                                                               
         LA    R2,PROTIMEH         VALIDATE TIME FIELD                          
         GOTO1 ANY                                                              
         ZIC   R5,5(R2)                                                         
         GOTO1 TIMVAL,DMCB,((R5),PROTIME),MYWORK                                
         CLI   0(R1),X'FF'                                                      
         BE    INVERR                                                           
         CLC   MYWORK+2(2),=C'CC'                                               
         BE    INVERR                                                           
*                                                                               
         MVC   AIO,AIO2                                                         
         MVI   BYTE,0                                                           
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
*                                                                               
VR5      LA    R5,KEY              FIND NEXT AVAILABLE REFERENCE NUMBER         
         USING CSOKEY,R5                                                        
         CLC   KEY(11),KEYSAVE                                                  
         BNE   VR6                                                              
         MVC   BYTE,CSOKREF        SAVE LAST USED REFERNECE NUMBER              
         DROP  R5                                                               
*                                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,DSCCODEQ                                                  
         BAS   RE,GETEL                                                         
         BNE   VR5A                                                             
         USING CSODSCEL,R6         DESCRIPTION ELEMENT                          
         CLC   DSCDAY,HALF         SAME DAY?                                    
         BNE   VR5A                NO                                           
         CLC   DSCTIME,MYWORK      SAME TIME?                                   
         BE    ERRADD              YES, CANNOT ADD SAME DATE/TIME               
         DROP  R6                                                               
*                                                                               
VR5A     OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
         GOTO1 SEQ                                                              
         NI    DMINBTS,X'F7'                                                    
         B     VR5                                                              
*                                                                               
VR6      MVC   AIO,AIO1                                                         
         L     R6,AIO                                                           
         USING CSORECD,R6                                                       
         ZIC   R1,BYTE             INCREMENT LAST USED REFERENCE NUMBER         
         LA    R1,1(R1)            AND INSERT INTO KEY                          
         STC   R1,CSOKREF                                                       
         MVC   CSOLEN,DATADISP     INSERT RECORD LENGTH                         
         MVC   CSOAGYA,AGENCY      AND ALPHA AGENCY CODE                        
*                                                                               
         MVC   BREF,CSOKREF        SAVE REF IN GLOBAL STORAGE                   
*                                                                               
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM           ADD DESCRIPTION ELEMENT                      
         USING CSODSCEL,R6                                                      
         MVI   DSCCODE,DSCCODEQ                                                 
         MVI   DSCLEN,DSCLENQ                                                   
         GOTO1 ADDELEM                                                          
*                                                                               
         XC    ELEM,ELEM           ADD WEEKLY ELEMENTS                          
         USING CSOWKEL,R6                                                       
         MVI   WKCODE,WKCODEQ                                                   
         MVI   WKLEN,WKLENQ                                                     
         MVC   THISDATE,QSTART                                                  
*                                                                               
VR10     GOTO1 DATCON,DMCB,(0,THISDATE),(2,WKDATE)                              
         GOTO1 ADDELEM                                                          
         GOTO1 ADDAY,DMCB,THISDATE,THISDATE,F'7'                                
         CLC   THISDATE,QEND                                                    
         BNH   VR10                                                             
*                                                                               
VR20     BAS   RE,CHKCOPY          CHECK IF COPY OPTION REQUESTED               
         BE    XIT                                                              
*                                                                               
         L     R6,AIO              R6 = A(DESCRIPTION ELEMENT)                  
         MVI   ELCODE,DSCCODEQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CSODSCEL,R6                                                      
*                                                                               
         LA    R2,PROPROGH         VALIDATE PROGRAM FIELD                       
         GOTO1 ANY                                                              
         TM    4(R2),X'20'         IF NEW DATA IS INPUT                         
         BO    *+12                                                             
         CLI   5(R2),14            LENGTH MUST NOT EXCEED 14 CHARS              
         BH    INVERR                                                           
         MVC   DSCPROG(L'PROPROG),PROPROG                                       
         OC    DSCPROG,=CL20' '                                                 
*                                                                               
         LA    R2,PRODAYH          VALIDATE DAY FIELD                           
         GOTO1 ANY                                                              
         ZIC   R5,5(R2)                                                         
         GOTO1 DAYVAL,DMCB,((R5),PRODAY),BYTE,MYWORK                            
         CLI   BYTE,0                                                           
         BE    INVERR                                                           
         MVC   DSCDAY,BYTE                                                      
*                                                                               
         LA    R2,PROTIMEH         VALIDATE TIME FIELD                          
         GOTO1 ANY                                                              
         ZIC   R5,5(R2)                                                         
         GOTO1 TIMVAL,DMCB,((R5),PROTIME),MYWORK                                
         CLI   0(R1),X'FF'                                                      
         BE    INVERR                                                           
         CLC   MYWORK+2(2),=C'CC'                                               
         BE    INVERR                                                           
         MVC   DSCTIME,MYWORK                                                   
*                                                                               
         LA    R2,PROWGTH          VALIDATE WEIGHT FIELD                        
         MVI   DSCWGT,0                                                         
         CLI   TRADONLY,C'T'       TRADE ONLY ALWAYS 0                          
         BE    VR21                                                             
         MVI   DSCWGT,1            CASH/TRADE DEFAULT TO 1                      
         CLI   5(R2),0                                                          
         BE    VR21                                                             
         TM    4(R2),X'08'                                                      
         BZ    INVERR                                                           
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R3,DUB                                                           
         C     R3,=F'99'                                                        
         BH    INVERR                                                           
         STC   R3,DSCWGT                                                        
*                                                                               
VR21     LA    R2,PROADJH          VALIDATE ADJACENCY FIELD                     
         GOTO1 ANY                                                              
         CLI   5(R2),1                                                          
         BNE   INVERR                                                           
         MVC   DSCADJ,PROADJ                                                    
*                                                                               
         LA    R2,PRODPTH          VALIDATE DAYPART FIELD                       
         GOTO1 ANY                                                              
         CLI   5(R2),1                                                          
         BNE   INVERR                                                           
         LA    R3,SVMENU                                                        
VR25     CLI   0(R3),0                                                          
         BE    INVERR                                                           
         CLC   0(1,R3),PRODPT                                                   
         BE    VR25A                                                            
         LA    R3,1(R3)                                                         
         B     VR25                                                             
VR25A    MVC   DSCDPT,0(R3)                                                     
*                                                                               
         LA    R2,PROMMXH          VALIDATE MIN/MAX FIELD                       
         MVI   DSCMIN,0            DEFAULT TO 0/0                               
         MVI   DSCMAX,0                                                         
         CLI   5(R2),0                                                          
         BE    VR50                                                             
         GOTO1 SCANNER,DMCB,(R2),SCANOUT,C',=,/'                                
         CLI   4(R1),1                                                          
         BNE   INVERR                                                           
         CLI   SCANOUT,0                                                        
         BE    VR30                                                             
         TM    SCANOUT+2,X'80'     LEFT FIELD CONTAINS MIN                      
         BZ    INVERR                                                           
         MVC   DSCMIN,SCANOUT+7                                                 
         CLI   SCANOUT+1,0                                                      
         BE    VR50                                                             
VR30     TM    SCANOUT+3,X'80'     RIGHT FIELD CONTAINS MAX                     
         BZ    INVERR                                                           
         MVC   DSCMAX,SCANOUT+11                                                
*                                                                               
VR50     CLI   DSCWGT,0            IF WEIGHT IS ZERO THEN CLEAR OUT             
         BNE   VR90                    ALL CASH SPOTS                           
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,WKCODEQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CSOWKEL,R6                                                       
*                                                                               
         MVC   WKCSPOTS,=X'0000'                                                
         BAS   RE,NEXTEL                                                        
         BE    *-10                                                             
*                                                                               
VR90     B     VR100                                                            
         DROP  R6                                                               
         EJECT                                                                  
VR100    LA    R2,PRORATEH         VALIDATE RATE FIELD                          
         GOTO1 ANY                                                              
         L     R6,AIO              AND PLACE RATES IN WEEKLY ELEMENTS           
         MVI   ELCODE,WKCODEQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CSOWKEL,R6                                                       
         GOTO1 DATCON,DMCB,(2,WKDATE),(0,THISDATE)                              
*                                                                               
         LA    R5,SCANOUT          BREAK SCREEN FIELD INTO SCAN FIELDS          
         GOTO1 SCANNER,DMCB,(R2),SCANOUT,0                                      
         CLI   4(R1),0                                                          
         BE    INVERR                                                           
         ZIC   R3,4(R1)                                                         
         ST    R3,FULL             SAVE NUMBER OF SCAN FIELDS IN FULL           
         CLI   1(R5),0                                                          
         BNE   FLDERR              FIRST FIELD HAS NO SECOND HALF               
         TM    2(R5),X'80'                                                      
         BZ    FLDERR                                                           
         MVC   NORMRATE,4(R5)      USE FIRST FIELD AS NORMAL RATE               
         MVC   THISRATE,NORMRATE                                                
         LA    R5,32(R5)           BUMP TO NEXT FIELD                           
         LA    R3,2                R3 HOLDS SCAN FIELD NUMBER                   
*                                                                               
VR110    C     R3,FULL             IF NO MORE FIELDS LEFT                       
         BNH   VR120                                                            
         MVC   NEXTDATE,=X'FFFFFFFFFFFF'    SET NEXT DATE TO INFINITY           
         B     VR130                                                            
*                                  ELSE VALIDATE NEXT FIELD                     
VR120    GOTO1 DATVAL,DMCB,(1,12(R5)),NEXTDATE                                  
         OC    DMCB(4),DMCB                                                     
         BZ    FLDERR                                                           
         CLC   NEXTDATE+2(4),QSTART+2    FIGURE OUT WHICH YEAR TO PUT           
         BL    *+14                           INTO NEXT DATE                    
         MVC   NEXTDATE(2),QSTART                                               
         B     *+10                                                             
         MVC   NEXTDATE(2),QEND                                                 
*                                                                               
         CLI   1(R5),0             IF NO RATE SPECIFIED THEN SET                
         BNE   *+14                     NEXTRATE TO NORMAL RATE                 
         MVC   NEXTRATE,NORMRATE                                                
         B     VR130                                                            
         TM    3(R5),X'80'                                                      
         BZ    FLDERR                                                           
         MVC   NEXTRATE,8(R5)      ELSE SET NEXTRATE FROM SECOND HALF           
*                                                                               
VR130    CLC   THISDATE,NEXTDATE   INSERT THISRATE INTO THIS WEEKLY             
         BH    FLDERR                 ELEMENT UNTIL THISDATE = NEXTDATE         
         BE    VR150                                                            
         MVC   WKCOST,THISRATE                                                  
         BAS   RE,NEXTEL                                                        
         BNE   VR190               NO MORE WEEKS LEFT                           
         GOTO1 DATCON,DMCB,(2,WKDATE),(0,THISDATE)                              
         B     VR130               LOOP THROUGH WEEKLY ELEMENTS                 
*                                                                               
VR150    MVC   THISRATE,NEXTRATE   SET THISRATE TO NEXTRATE AND                 
         LA    R5,32(R5)               ADVANCE TO NEXT SCAN FIELD               
         LA    R3,1(R3)                                                         
         B     VR110                                                            
*                                                                               
VR190    C     R3,FULL             NO SCAN FIELDS SHOULD BE LEFT                
         BNH   FLDERR                                                           
         EJECT                                                                  
VR200    LA    R2,PROTRDH          VALIDATE TRADE FIELD                         
         CLI   5(R2),0                                                          
         BE    VR300                                                            
         L     R6,AIO              AND PLACE TRADES IN WEEKLY ELEMENTS          
         MVI   ELCODE,WKCODEQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CSOWKEL,R6                                                       
         GOTO1 DATCON,DMCB,(2,WKDATE),(0,THISDATE)                              
*                                                                               
         LA    R5,SCANOUT          BREAK SCREEN FIELD INTO SCAN FIELDS          
         GOTO1 SCANNER,DMCB,(R2),SCANOUT,0                                      
         CLI   4(R1),0                                                          
         BE    INVERR                                                           
         ZIC   R3,4(R1)                                                         
         ST    R3,FULL             SAVE NUMBER OF SCAN FIELDS IN FULL           
*                                                                               
         BAS   RE,TESTGOL          IF SPECIAL PROPORTION TO GOALS REQ           
         BE    VR300               THEN DONE                                    
*                                                                               
         CLI   1(R5),0             IF FIRST FIELD HAS SECOND HALF               
         BE    VR205                                                            
         MVC   NORMTRD,=H'100'     DATE IN FIRST SCAN FIELD INDICATES           
         LA    R5,SCANOUT              THAT ONLY SOME WEEKS CHANGE              
         LA    R3,1                                                             
         B     VR206                                                            
*                                                                               
VR205    TM    2(R5),X'80'         OTHERWISE NORMAL TRADES MUST BE              
         BZ    FLDERR                  VALID NUMERIC                            
         MVC   NORMTRD,6(R5)                                                    
         LA    R5,SCANOUT+32       START WITH SECOND SCAN FIELD                 
         LA    R3,2                                                             
*                                                                               
VR206    MVC   THISTRD,NORMTRD                                                  
*                                                                               
VR210    C     R3,FULL             IF NO MORE FIELDS LEFT                       
         BNH   VR220                                                            
         MVC   NEXTDATE,=X'FFFFFFFFFFFF'    SET NEXT DATE TO INFINITY           
         B     VR230                                                            
*                                  ELSE VALIDATE NEXT FIELD                     
VR220    GOTO1 DATVAL,DMCB,(1,12(R5)),NEXTDATE                                  
         OC    DMCB(4),DMCB                                                     
         BZ    FLDERR                                                           
         CLC   NEXTDATE+2(4),QSTART+2    FIGURE OUT WHICH YEAR TO PUT           
         BL    *+14                           INTO NEXT DATE                    
         MVC   NEXTDATE(2),QSTART                                               
         B     *+10                                                             
         MVC   NEXTDATE(2),QEND                                                 
*                                                                               
         CLI   1(R5),0             IF NO TRADES SPECIFIED THEN SET              
         BNE   *+14                     NEXTTRD TO NORMAL TRADES                
         MVC   NEXTTRD,NORMTRD                                                  
         B     VR230                                                            
         TM    3(R5),X'80'                                                      
         BZ    FLDERR                                                           
         MVC   NEXTTRD,10(R5)      ELSE SET NEXTTRD FROM SECOND HALF            
*                                                                               
VR230    CLC   THISDATE,NEXTDATE   INSERT THISTRD INTO THIS WEEKLY              
         BH    FLDERR                 ELEMENT UNTIL THISDATE = NEXTDATE         
         BE    VR250                                                            
         CLC   THISTRD,=H'100'     UNLESS '*' OPTION HAS BEEN USED              
         BE    *+10                                                             
         MVC   WKTSPOTS,THISTRD                                                 
         BAS   RE,NEXTEL                                                        
         BNE   VR290               NO MORE WEEKS LEFT                           
         GOTO1 DATCON,DMCB,(2,WKDATE),(0,THISDATE)                              
         B     VR230               LOOP THROUGH WEEKLY ELEMENTS                 
*                                                                               
VR250    MVC   THISTRD,NEXTTRD     SET THISTRD TO NEXTTRD AND                   
         LA    R5,32(R5)               ADVANCE TO NEXT SCAN FIELD               
         LA    R3,1(R3)                                                         
         B     VR210                                                            
*                                                                               
VR290    C     R3,FULL             NO SCAN FIELDS SHOULD BE LEFT                
         BNH   FLDERR                                                           
*                                                                               
VR300    B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE LOOKS IN THE SCANNER BLOCK FOR A SPECIAL REQUEST TO              
* PROPORTION TRADE VALUE ACCORDING TO GOAL DOLLAR DISTRIBUTION                  
* ACCROSS WEEKS.  THE SYNTAX IS, 'G=(TOTAL NUMBER OF SPOTS)'.                   
* THE ROUTINE MUST FIRST CALCULATE THE WEIGHT OF EACH WEEK WITH THE             
* FOLLOWING FORMULA:                                                            
*                                                                               
*       WEIGHT = GOAL DOLLARS/COST OF TRADE SPOT                                
*                                                                               
* THEN, FOR EACH WEEK THE ROUTINE WILL USE THE FOLLOWING FORMULA:               
*                                                                               
*       TRADE SPOTS = (TOTAL NUMBER OF SPOTS) * (WEIGHT FOR THE WEEK)           
*                     -----------------------------------------                 
*                     (TOTAL WEIGHT FOR THE YEAR)                               
*                                                                               
TESTGOL  NTR1                                                                   
         C     R3,=F'1'            SCANNER BLOCK MUST HAVE ONE ENTRY            
         BNE   TGNO                                                             
         CLI   0(R5),1             WITH LEFT HAND SIDE                          
         BNE   TGNO                                                             
         CLI   12(R5),C'G'         EQUAL TO 'G'                                 
         BNE   TGNO                                                             
         TM    3(R5),X'80'         AND RIGHT HAND SIDE NUMERIC                  
         BZ    TGNO                                                             
*                                                                               
         SR    R2,R2               ACCUMULATE ALLOCATED SPOTS IN R2             
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 CLRACC              COMPUTE NTP PERCENTAGE                       
         GOTO1 CMPNTP                                                           
         GOTO1 CLRACC              COMPUTE GOAL DOLLARS FOR MARKET              
         GOTO1 CMPMGOL                                                          
*                                                                               
         CLI   ACTNUM,ACTCHA       IF ACTION CHANGE THEN RESTORE                
         BNE   TG5                     READ SEQUENCE                            
         L     RF,AIO1                                                          
         MVC   KEY(13),0(RF)                                                    
         GOTO1 READ                                                             
         GOTO1 GETREC                                                           
*                                                                               
TG5      MVC   AIO,AIO1            RESTORE AIO AND ELCODE                       
         MVI   ELCODE,WKCODEQ                                                   
*                                                                               
         LR    R3,R6               SAVE A(FIRST WEEKLY ELEM) FOR LATER          
         LA    R4,ACCTAB           POINT TO FIRST WEEKLY ACCUM                  
         USING ACCTABD,R4                                                       
*                                                                               
TG10     SR    R0,R0               COMPUTE WEIGHT FOR THIS WEEK =               
         SR    R1,R1                   GOAL / COST                              
         ICM   R1,15,ACCONE                                                     
         ICM   RF,15,WKCOST                                                     
         M     RE,NTPPERC                                                       
         A     RF,=A(5000)                                                      
         D     RE,=A(10000)                                                     
         LTR   RF,RF               (SET TO ZERO IF COST IS ZERO)                
         BZ    TG15                                                             
         LR    RE,RF                                                            
         SRL   RE,1                                                             
         AR    R1,RE                                                            
         DR    R0,RF                                                            
TG15     STCM  R1,15,ACCTWO        SAVE WEIGHT BACK TO ACCUM TWO                
*                                                                               
         LA    R4,ACCTABL(R4)      BUMP TO NEXT ACCUM AND WEEKLY ELEM           
         BAS   RE,NEXTEL                                                        
         BE    TG10                REPEAT UNTIL END OF WEEKLY ELEMS             
*                                                                               
         LA    R4,ACCTAB           POINT BACK TO FIRST WEEK FOR ACCUMS          
         LR    R6,R3                   AND WEEKLY ELEMENTS                      
*                                                                               
*                                  TOTAL ACCUMS TO GET TOTAL WEIGHT             
         GOTO1 TOTFULL,DMCB,ACCTWO,ACCTABL                                      
         MVC   FULL,0(R1)                                                       
         OC    FULL,FULL                                                        
         BZ    ERRTSTG                                                          
*                                                                               
TG20     SR    R0,R0               COMPUTE TRADE SPOTS FOR THIS WEEK =          
         ICM   R1,15,8(R5)             TOTAL SPOTS * WEIGHT FOR WEEK /          
         ICM   RE,15,ACCTWO            TOTAL WEIGHT FOR YEAR                    
         MR    R0,RE                                                            
         L     RE,FULL                                                          
         SRL   RE,1                                                             
         AR    R1,RE                                                            
         D     R0,FULL                                                          
         STCM  R1,3,WKTSPOTS       SAVE SPOTS IN WEEKLY ELEMENT                 
*                                                                               
         AR    R2,R1               ACCUMULATE ALLOCATED SPOTS IN R2             
*                                                                               
         LA    R4,ACCTABL(R4)      BUMP TO NEXT ACCUM AND WEEKLY ELEM           
         BAS   RE,NEXTEL                                                        
         BE    TG20                REPEAT UNTIL END OF WEEKLY ELEMS             
*                                                                               
* THIS SECTION OF CODE MAKES SURE WE HAVEN'T UNDER OR OVER ALLOCED              
*                                                                               
         L     RF,8(R5)            COMPUTE NUMBER OF SPOTS LEFT TO              
         SR    RF,R2                   ALLOCATE                                 
         LR    R2,RF                                                            
*                                                                               
         LA    R4,ACCTAB           POINT BACK TO FIRST WEEK FOR ACCUMS          
         LR    R6,R3                   AND WEEKLY ELEMENTS                      
*                                                                               
TG30     C     R2,=F'0'            WHILE THERE ARE SPOTS TO BE ALLOCED          
         BNH   TG50                                                             
         OC    ACCTWO,ACCTWO       IF THERE ARE GOALS FOR THIS WEEK             
         BZ    TG40                                                             
         ICM   R1,3,WKTSPOTS       THEN ADD ONE SPOT TO THE WEEK                
         LA    R1,1(R1)                                                         
         STCM  R1,3,WKTSPOTS                                                    
         BCTR  R2,0                DECREMENT SPOTS LEFT                         
*                                                                               
TG40     LA    R4,ACCTABL(R4)      BUMP TO NEXT ACCUM AND WEEKLY ELEM           
         BAS   RE,NEXTEL                                                        
         BE    TG30                                                             
*                                                                               
TG50     LCR   R2,R2               COMPUTE NUMBER OVER ALLOCATED                
*                                                                               
         LA    R4,ACCTAB           POINT BACK TO FIRST WEEK FOR ACCUMS          
         LR    R6,R3                   AND WEEKLY ELEMENTS                      
*                                                                               
TG60     C     R2,=F'0'            WHILE THERE ARE SPOTS TO BE REMOVED          
         BNH   TGYES                                                            
         OC    ACCTWO,ACCTWO       IF THERE ARE GOALS FOR THIS WEEK             
         BZ    TG70                                                             
         ICM   R1,3,WKTSPOTS       THEN SUB ONE SPOT FROM THE WEEK              
         BZ    TG70                (IF THERE IS ONE TO SUBTRACT)                
         BCTR  R1,0                                                             
         STCM  R1,3,WKTSPOTS                                                    
         BCTR  R2,0                DECREMENT SPOTS LEFT                         
*                                                                               
TG70     LA    R4,ACCTABL(R4)      BUMP TO NEXT ACCUM AND WEEKLY ELEM           
         BAS   RE,NEXTEL                                                        
         BE    TG60                                                             
*                                                                               
TGYES    SR    RC,RC               RETURN 'EQ' CONDITION CODE                   
TGNO     LTR   RC,RC               RETURN 'NEQ' CONDITION CODE                  
         B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE LOOKS IN THE OPTION FIELD FOR THE OPTION 'COPY=EST/REF'.         
* IF THE OPTION IS THERE THEN THE SPECIFIED EST/REF PAIR WILL BE COPIED         
* TO THE EST/REF PAIR SPECIFED IN THE KEY FIELDS.  ALL RATES IN THE             
* DESTINATION RECORD WILL BE HALF THAT OF THE SOURCE RECORD.  THE               
* ROUTINE RETURNS 'YES' IF THE OPTION IS REQUESTED AND 'NO' OTHERWISE.          
*                                                                               
CHKCOPY  NTR1                                                                   
         LA    R2,PROOPTH          DONE IF BLANK OPTIONS FIELD                  
         CLI   5(R2),0                                                          
         BE    NO                                                               
*                                                                               
         LA    R5,SCANOUT          SCAN FIELD INTO SCANOUT                      
         GOTO1 SCANNER,DMCB,(R2),(R5),C',==/'                                   
         CLI   4(R1),2             MUST HAVE 2 BLOCKS                           
         BNE   INVERR                                                           
         CLI   0(R5),4             FIRST BLOCK MUST SAY 'COPY'                  
         BNE   INVERR                                                           
         CLC   12(4,R5),=C'COPY'                                                
         BNE   INVERR                                                           
         CLI   1(R5),0             AND HAVE NO SECOND HALF                      
         BNE   INVERR                                                           
*                                                                               
         LA    R5,32(R5)           BUMP TO SECOND BLOCK                         
         TM    2(R5),X'80'         BOTH HALVES MUST BE NUMERIC                  
         BZ    INVERR                                                           
         CLC   4(4,R5),=F'256'     AND < 256                                    
         BNL   INVERR                                                           
         TM    3(R5),X'80'                                                      
         BZ    INVERR                                                           
         CLC   8(4,R5),=F'256'                                                  
         BNL   INVERR                                                           
*                                                                               
         LA    R4,KEY              R4 = A(KEY)                                  
         USING CSOKEY,R4                                                        
         L     R6,AIO              R6 = A(IOAREA)                               
*                                                                               
         MVC   CSOKEY,0(R6)        READ FOR SOURCE RECORD                       
         MVC   CSOKEST,7(R5)                                                    
         MVC   CSOKREF,11(R5)                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   ERRRNF              ERROR IF NOT FOUND                           
*                                                                               
         GOTO1 GETREC              READ SOURCE RECORD                           
*                                                                               
         MVC   CSOKEST,BMEST       CHANGE KEY TO DEST RECORD                    
         MVC   CSOKREF,BREF                                                     
         MVC   0(13,R6),0(R4)      COPY KEY TO IOAREA                           
*                                                                               
         L     R6,AIO              R6 = A(FIRST WEEKLY ELEMENT)                 
         MVI   ELCODE,WKCODEQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CSOWKEL,R6                                                       
*                                                                               
CP10     ICM   RF,15,WKCOST        HALF THE COST IN ELEMENT (ROUND UP)          
         LA    RF,1(RF)                                                         
         SRL   RF,1                                                             
         STCM  RF,15,WKCOST                                                     
*                                                                               
         BAS   RE,NEXTEL           REPEAT UNTIL NO MORE ELEMENTS                
         BE    CP10                                                             
*                                                                               
         BAS   RE,COPYDEM          COPY CORRESPONDING DEMO OVERRIDES            
*                                                                               
         CLI   ACTNUM,ACTCHA       IF ACTION IS CHANGE                          
         BNE   CP20                                                             
*                                                                               
         GOTO1 HIGH                REESTABLISH GETREC/PUTREC SEQUENCE           
         MVC   AIO,AIO2                BY READING OLD RECORD INTO VOID          
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
CP20     XC    PROOPT,PROOPT       CLEAR OUT OPTIONS FIELD                      
         OI    PROOPTH+6,X'80'                                                  
*                                                                               
         B     YES                 RETURN 'YES'                                 
         DROP  R4                                                               
         EJECT                                                                  
* THIS ROUTINE COPIES THE DEMO OVERRIDES FOR THE SAME EST/REF AS WAS            
* SPECIFIED FOR THE COPY OF THE PROGRAM RECORD.                                 
*                                                                               
COPYDEM  NTR1                                                                   
         LA    R4,KEY              BUILD KEY OF SOURCE RECORD                   
         USING DEMKEY,R4                                                        
         XC    KEY,KEY                                                          
         MVI   DEMKTYPE,DEMKTYPQ   DEM RECORD TYPE                              
         MVI   DEMKSTYP,DEMKSTPQ   DEM RECORD SUB-TYPE                          
         MVC   DEMKAM,BAGYMD                                                    
         MVC   DEMKCLT,BCLT                                                     
         MVC   DEMKMKT(5),BMKTSTA                                               
         MVC   DEMKEST,7(R5)                                                    
         MVC   DEMKREF,11(R5)                                                   
*                                                                               
         GOTO1 HIGH                READ FOR SOURCE RECORD                       
         CLC   KEY(13),KEYSAVE                                                  
         BNE   CDX                 RETURN IF NOT FOUND                          
*                                                                               
         MVC   AIO,AIO2            READ SOURCE RECORD                           
         GOTO1 GETREC                                                           
*                                                                               
         MVC   DEMKEST,BMEST       CHANGE KEY TO DEST RECORD                    
         MVC   DEMKREF,BREF                                                     
*                                                                               
         L     R6,AIO              COPY KEY TO IOAREA                           
         MVC   0(13,R6),0(R4)                                                   
*                                                                               
         OI    DMINBTS,X'08'       READ FOR ALREADY EXISTING DEST REC           
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE     IF RECORD ALREADY EXISTS                     
         BNE   CD20                                                             
*                                                                               
         TM    KEY+13,X'80'        THEN IF IT IS DELETED                        
         BZ    CD10                                                             
         NI    KEY+13,X'7F'        UNDELETE AND WRITE BACK KEY                  
         GOTO1 WRITE                                                            
*                                                                               
CD10     MVC   AIO,AIO3            READ OLD REC INTO VOID                       
         GOTO1 GETREC                                                           
         MVC   AIO,AIO2            WRITE BACK NEW ONE                           
         GOTO1 PUTREC                                                           
         B     CD50                                                             
*                                                                               
CD20     GOTO1 ADDREC              ELSE ADD NEW RECORD                          
*                                                                               
CD50     MVC   AIO,AIO1            RESTORE AIO TO AIO1                          
         NI    DMINBTS,X'F7'                                                    
*                                                                               
CDX      B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* DISPLAY RECORD                                                                
*                                                                               
DR       GOTO1 CLEARF,DMCB,(0,PROPROGH),PROLAST                                 
         BAS   RE,SETPREV                                                       
         L     R6,AIO                                                           
         MVI   ELCODE,DSCCODEQ     DISPLAY DECRIPTION ELEMENT                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CSODSCEL,R6                                                      
         MVC   PROPROG(L'PROPROG),DSCPROG                                       
         OI    PROPROGH+6,X'80'                                                 
         OI    PROPROGH+4,X'20'                                                 
         GOTO1 UNDAY,DMCB,DSCDAY,PRODAY                                         
         OI    PRODAYH+6,X'80'                                                  
         GOTO1 UNTIME,DMCB,DSCTIME,PROTIME                                      
         OI    PROTIMEH+6,X'80'                                                 
         EDIT  (1,DSCWGT),(2,PROWGT),ALIGN=LEFT,ZERO=NOBLANK                    
         OI    PROWGTH+6,X'80'                                                  
         MVC   PROADJ(1),DSCADJ                                                 
         OI    PROADJH+6,X'80'                                                  
         MVC   PRODPT(1),DSCDPT                                                 
         OI    PRODPTH+6,X'80'                                                  
         LA    R3,PROMMX                                                        
         EDIT  (1,DSCMIN),(4,(R3)),ALIGN=LEFT,ZERO=NOBLANK                      
         AR    R3,R0                                                            
         MVI   0(R3),C'/'                                                       
         LA    R3,1(R3)                                                         
         EDIT  (1,DSCMAX),(4,(R3)),ALIGN=LEFT,ZERO=NOBLANK                      
         OI    PROMMXH+6,X'80'                                                  
         EJECT                                                                  
         L     R6,AIO              DIPLAY RATES FROM WEEKLY ELEMENTS            
         MVI   ELCODE,WKCODEQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CSOWKEL,R6                                                       
         MVC   NORMRATE,WKCOST                                                  
         MVC   THISRATE,WKCOST                                                  
         OI    PRORATEH+6,X'80'                                                 
         LA    R3,PRORATE                                                       
         EDIT  (4,WKCOST),(9,(R3)),ALIGN=LEFT,ZERO=NOBLANK                      
         AR    R3,R0                                                            
DR10     BAS   RE,NEXTEL                                                        
         BNE   DR20                                                             
         CLC   WKCOST,THISRATE                                                  
         BE    DR10                                                             
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
*        GOTO1 DATCON,DMCB,(2,WKDATE),(4,(R3))                                  
*        LA    R3,5(R3)                                                         
         GOTO1 DATCON,DMCB,(2,WKDATE),(3,FULL)                                  
         ZIC   R4,FULL+1                                                        
         EDIT  (R4),(2,(R3)),ALIGN=LEFT                                         
         AR    R3,R0                                                            
         MVI   0(R3),C'/'                                                       
         LA    R3,1(R3)                                                         
         ZIC   R4,FULL+2                                                        
         EDIT  (R4),(2,(R3)),ALIGN=LEFT                                         
         AR    R3,R0                                                            
*                                                                               
         MVC   THISRATE,WKCOST                                                  
         CLC   WKCOST,NORMRATE                                                  
         BE    DR10                                                             
         MVI   0(R3),C'='                                                       
         LA    R3,1(R3)                                                         
         EDIT  (4,WKCOST),(9,BLOCK),ALIGN=LEFT,ZERO=NOBLANK                     
         LR    RF,R0                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),BLOCK                                                    
         AR    R3,R0                                                            
         B     DR10                                                             
         EJECT                                                                  
DR20     L     R6,AIO              BUILD ACCUMULATOR TABLE FOR CALENDER         
         MVI   ELCODE,WKCODEQ          BUILDING ROUTINE                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CSOWKEL,R6                                                       
         LA    R5,ACCTAB           POINT TO ACCUMULATOR TABLE                   
         USING ACCTABD,R5                                                       
*                                                                               
DR30     XC    ACCONE(8),ACCONE                                                 
         MVC   ACCONE+2(2),WKTSPOTS                                             
         MVC   ACCTWO+2(2),WKCSPOTS                                             
         BAS   RE,NEXTEL                                                        
         BNE   DR40                                                             
         LA    R5,ACCTABL(R5)                                                   
         B     DR30                                                             
*                                  CALL CALENDER BUILDER                        
DR40     MVC   CALHEADS,=C'  TS  CS'                                            
         LA    R2,PROL1H                                                        
         ST    R2,CALSPTR                                                       
         GOTO1 BLDCAL                                                           
*                                                                               
         L     R6,AIO              DISPLAY REFERENCE NUMBER                     
         USING CSOKEY,R6                                                        
         EDIT  (1,CSOKREF),(3,PROREF),ALIGN=LEFT,ZERO=NOBLANK                   
         OI    PROREFH+6,X'80'                                                  
*                                                                               
         CLI   MODE,XRECPUT        INTERCEPT GENCON'S SELECTION OF NEXT         
         BNE   DRX                     RECORD AND DISPLAY CHANGES TO            
         CLI   ACTNUM,ACTSEL           THIS ONE                                 
         BNE   DRX                                                              
         CLI   PREVFLAG,1                                                       
         BE    ERREXIT                                                          
*                                                                               
DRX      B     XIT                                                              
         EJECT                                                                  
* DISPLAY KEY                                                                   
*                                                                               
DK       L     R6,AIO              RECORD SELECTED                              
         USING CSOKEY,R6                                                        
*                                                                               
         GOTO1 CLEARF,DMCB,(0,PROMEDH),PROOPTH                                  
         MVC   PROMED(L'QMED),QMED                                              
         MVC   PROCLT(L'QCLT),QCLT                                              
         MVC   PROSTA(L'QSTA-1),QSTA                                            
         MVC   PROEST(L'QMEST),QMEST                                            
         EDIT  (1,CSOKREF),(2,PROREF),ALIGN=LEFT                                
*                                                                               
DKX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* LIST RECORDS                                                                  
*                                                                               
LR       LA    R4,KEY                                                           
         USING CSOKEY,R4                                                        
*                                                                               
         OC    KEY(13),KEY         TEST FIRST TIME THROUGH                      
         BNZ   LR10                                                             
         MVI   CSOKTYPE,CSOKTYPQ   RECORD TYPE                                  
         MVI   CSOKSTYP,CSOKSTPQ   RECORD SUB-TYPE                              
         MVC   CSOKAM,BAGYMD       AGY/MED                                      
         MVC   CSOKCLT,BCLT        CLIENT                                       
         MVC   CSOKMKT(5),BMKTSTA  MARKET/STATION                               
         MVC   CSOKEST,BMEST       ESTIMATE                                     
         MVC   CSOKREF,BREF        STARTING REFERENCE NUMBER                    
         MVC   SAVEKEY,KEY                                                      
*                                                                               
LR10     MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                FIRST RECORD                                 
         B     LR30                                                             
*                                                                               
LR20     LA    R4,KEY              NEXT RECORD                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
*                                                                               
LR30     CLC   KEY(11),SAVEKEY     TEST SAME TYPE/AGY/MED                       
         BNE   LRX                                                              
*                                                                               
         XC    LISTAR,LISTAR       FILL IN LIST LINE                            
         EDIT  (1,CSOKREF),(2,LSTREF),ALIGN=RIGHT,FILL=0                        
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,DSCCODEQ     DESCRIPTION ELEMENT                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         USING CSODSCEL,R6                                                      
         MVC   LSTPROG,DSCPROG     PROGRAM NAME                                 
         GOTO1 UNDAY,DMCB,DSCDAY,LSTDAY      PROGRAM DAYS                       
         GOTO1 UNTIME,DMCB,DSCTIME,LSTTIME   PROGRAM TIMES                      
         DROP  R6                                                               
*                                                                               
         GOTO1 LISTMON             DISPLAY LINE                                 
         B     LR20                                                             
*                                                                               
LRX      B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* SET ALL UNPROTECTED FIELDS TO PREVIOUSLY VALID                                
*                                                                               
SETPREV  NTR1                                                                   
         LA    R2,CONSERVH         POINT TO FIRST UNPROTECTED FIELD             
*                                                                               
SP10     CLI   0(R2),0             TEST END OF SCREEN                           
         BE    XIT                                                              
         TM    1(R2),X'20'         SKIP PROTECTED FIELDS                        
         BO    *+8                                                              
         OI    4(R2),X'20'         SET VALID BIT                                
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               BUMP TO NEXT FIELD                           
         B     SP10                                                             
         SPACE 1                                                                
* TEST IF ANY UNPROTECTED FIELDS ARE NOT PREVIOUSLY VALID                       
* PREVFLAG WILL BE EQUAL TO '1' IF ANY FIELD HAS CHANGED                        
*                                                                               
TESTPREV NTR1                                                                   
         MVI   PREVFLAG,0          SET FLAG FOR NO CHANGED FIELD FOUND          
         LA    R2,CONSERVH         POINT TO FIRST UNPROTECTED FIELD             
*                                                                               
TP10     CLI   0(R2),0             IF END OF SCREEN RETURN                      
         BE    XIT                                                              
         TM    1(R2),X'20'         SKIP PROTECTED FIELDS                        
         BO    TP20                                                             
         TM    4(R2),X'20'         TEST VALID BIT                               
         BO    TP20                                                             
         MVI   PREVFLAG,1          SET FLAG FOR CHANGED FIELD FOUND             
         B     XIT                                                              
*                                                                               
TP20     ZIC   R0,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R0                                                            
         B     TP10                                                             
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
ERRRNF   MVI   ERROR,NOTFOUND                                                   
         B     TRAPERR                                                          
*                                                                               
ERRNOINP XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(35),=C'ERROR - INVALID OR NO DATA RECEIVED'              
         OI    CONSERVH+6,X'81'    SET MODIFIED AND TRANSMIT                    
         LA    R2,PROMEDH                                                       
         GOTO1 ERREX2                                                           
*                                                                               
ERREXIT  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(35),=C'RECORD CHANGED - HIT ENTER FOR NEXT'              
         OI    CONSERVH+6,X'81'    SET MODIFIED AND TRANSMIT                    
         LA    R2,PROMEDH                                                       
         GOTO1 ERREX2                                                           
*                                                                               
ERRTSTG  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(25),=C'INSUFFICIENT GOAL DOLLARS'                        
         OI    CONSERVH+6,X'81'    SET MODIFIED AND TRANSMIT                    
         LA    R2,PROTRDH                                                       
         GOTO1 ERREX2                                                           
*                                                                               
ERRADD   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(40),=C'THIS DAY AND TIME ALREADY EXIST FOR REF#'         
         EDIT  BYTE,(3,CONHEAD+41),ALIGN=LEFT                                   
         OI    CONSERVH+6,X'81'    SET MODIFIED AND TRANSMIT                    
         LA    R2,PRODAYH                                                       
         GOTO1 ERREX2                                                           
*                                                                               
FLDERR   OI    6(R2),X'40'         SET CURSOR                                   
         LA    R2,CONHEADH                                                      
         XC    CONHEAD,CONHEAD                                                  
         MVC   8(23,R2),=C'** ERROR IN FIELD    **'                             
         EDIT  (R3),(2,26(R2)),ALIGN=LEFT                                       
         GOTO1 ERREX2                                                           
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
DASHES   DC    C'------------------'                                            
         EJECT                                                                  
         LTORG                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPCSOFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPCSOF1D                                                       
         EJECT                                                                  
       ++INCLUDE SPCSOWORKD                                                     
         EJECT                                                                  
* WORK AREA                                                                     
*                                                                               
         ORG   SYSSPARE                                                         
NORMRATE DS    F                                                                
THISRATE DS    F                                                                
NEXTRATE DS    F                                                                
NORMTRD  DS    H                                                                
THISTRD  DS    H                                                                
NEXTTRD  DS    H                                                                
PREVFLAG DS    X                                                                
         PRINT OFF                                                              
       ++INCLUDE SPGENDMN                                                       
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE SPGENCSO                                                       
         EJECT                                                                  
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
         DS    C                                                                
LSTREF   DS    CL2                                                              
         DS    CL4                                                              
LSTPROG  DS    CL19                                                             
         DS    CL4                                                              
LSTDAY   DS    CL6                                                              
         DS    CL4                                                              
LSTTIME  DS    CL6                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'066SPCSO01   05/07/03'                                      
         END                                                                    
