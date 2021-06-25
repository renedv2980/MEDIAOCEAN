*          DATA SET SPCSO0A    AT LEVEL 061 AS OF 05/01/02                      
*PHASE T2180AA                                                                  
         TITLE 'T2180A - CHILD SPOT PROGRAM ADJUST'                             
T2180A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T2180A                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         LA    RE,REFTAB                                                        
         A     RE,REFDISP                                                       
         ST    RE,REFPTR                                                        
         LA    RE,REFTAB                                                        
         A     RE,NEXTDISP                                                      
         ST    RE,NEXTREF                                                       
*                                                                               
         CLI   MYOVNUM,X'0A'       CHECK FIRST TIME FOR THIS OVERLAY            
         BE    GOAHEAD                                                          
         NI    ADJMEDH+4,X'DF'     FORCE VALIDATION OF KEY FIELDS               
GOAHEAD  MVI   MYOVNUM,X'0A'       STORE OVERLAY NUMBER                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE AND DISPLAY REQUESTS                
         BE    VK                                                               
         DC    H'0'                                                             
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
*                                                                               
VK       MVI   KEYCHANG,C'N'                                                    
*                                                                               
         LA    R2,ADJMEDH          VALIDATE MEDIA FIELD                         
         TM    4(R2),X'20'                                                      
         BO    VKCLT                                                            
         NI    ADJCLTH+4,X'DF'                                                  
         NI    ADJSTAH+4,X'DF'                                                  
         NI    ADJESTH+4,X'DF'                                                  
         MVI   KEYCHANG,C'Y'                                                    
         GOTO1 ANY                                                              
         GOTO1 VALIMED                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
VKCLT    LA    R2,ADJCLTH          VALIDATE CLIENT FIELD                        
         TM    4(R2),X'20'                                                      
         BO    VKSTA                                                            
         NI    ADJSTAH+4,X'DF'                                                  
         NI    ADJESTH+4,X'DF'                                                  
         MVI   KEYCHANG,C'Y'                                                    
         GOTO1 ANY                                                              
         GOTO1 VALICLT                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
VKSTA    LA    R2,ADJSTAH          VALIDATE STATION FIELD                       
         TM    4(R2),X'20'                                                      
         BO    VKEST                                                            
         NI    ADJESTH+4,X'DF'                                                  
         MVI   KEYCHANG,C'Y'                                                    
         GOTO1 ANY                                                              
         GOTO1 VALISTA                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
VKEST    LA    R2,ADJESTH          VALIDATE ESTIMATE FIELD                      
         TM    4(R2),X'20'                                                      
         BO    VKX                                                              
         MVI   KEYCHANG,C'Y'                                                    
         GOTO1 ANY                                                              
         GOTO1 VALIMEST                                                         
         OI    4(R2),X'20'                                                      
*                                                                               
VKX      CLI   KEYCHANG,C'Y'       IF KEY HAS CHANGED BUILD REFTAB              
         BNE   VR                                                               
         EJECT                                                                  
BUILDREF LA    R4,KEY              READ RECORDS AND BUILD REFERNCE TAB          
         USING CSOKEY,R4                                                        
         MVI   CSOKTYPE,CSOKTYPQ                                                
         MVI   CSOKSTYP,CSOKSTPQ                                                
         MVC   CSOKAM,BAGYMD                                                    
         MVC   CSOKCLT,BCLT                                                     
         MVC   CSOKMKT(5),BMKTSTA                                               
         MVC   CSOKEST,BMEST                                                    
         LA    R5,REFTAB                                                        
         USING REFTABD,R5                                                       
         ST    R5,REFPTR                                                        
         XC    REFDISP,REFDISP                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
BR10     CLC   KEY(11),KEYSAVE     LOOP THROUGH REFERENCE NUMBERS               
         BNE   BRX                                                              
         MVC   REFNUM,CSOKREF      SAVE REFERENCE NUMBER                        
         LR    R1,R4                                                            
         AH    R1,LKEY                                                          
         AH    R1,LSTATUS                                                       
         MVC   REFDISKA,0(R1)      DISK ADDRESS                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,DSCCODEQ     USE                                          
         BAS   RE,GETEL                DESCRIPTION                              
         BE    *+6                          ELEMENT                             
         DC    H'0'                              FOR ...                        
         USING CSODSCEL,R6                                                      
         MVC   REFPROG,DSCPROG     PROGRAM NAME,                                
         MVC   REFDAY,DSCDAY           DAY CODE,                                
         MVC   REFTIME,DSCTIME         TIME CODE                                
*                                                                               
         LA    R5,REFTABL(R5)      BUMP TO NEXT REFTAB ENTRY                    
*                                                                               
         MVI   RDUPDATE,C'N'       READ NEXT KEY                                
         GOTO1 SEQ                                                              
         B     BR10                                                             
*                                                                               
BRX      MVI   0(R5),0             MARK END OF TABLE                            
         B     DR                                                               
         DROP  R5                                                               
         EJECT                                                                  
VR       MVI   RECCHANG,C'N'       VALIDATE RECORD FIELDS                       
         XC    TC,TC                                                            
         MVI   EQUALS,C'N'                                                      
*                                                                               
         LA    R2,ADJTCH           VALIDATE TRADE/SPOT TRANSFER FIELD           
         CLI   5(R2),0                                                          
         BNE   VR10                                                             
         CLI   ADJFROMH+5,0        MUST BE EMPTY                                
         BNE   MISSERR                                                          
         CLI   ADJTOH+5,0                                                       
         BNE   MISSERR                                                          
         CLI   ADJDATSH+5,0                                                     
         BNE   MISSERR                                                          
         B     VR200                                                            
*                                                                               
VR10     CLI   5(R2),1             MUST BE 'T' OR 'C'                           
         BNE   INVERR                                                           
         CLI   8(R2),C'T'                                                       
         BE    VR20                                                             
         CLI   8(R2),C'C'                                                       
         BNE   INVERR                                                           
*                                                                               
VR20     MVC   TC,8(R2)            SAVE T/C SELECTION                           
         MVI   RECCHANG,C'Y'                                                    
*                                                                               
         LA    R2,ADJFROMH         VALDATE FROM FIELD                           
         GOTO1 ANY                                                              
         LA    R3,8(R2)                                                         
         BAS   RE,VALNUM                                                        
         BAS   RE,VALREF                                                        
         ST    RF,FROMREF                                                       
         STC   R1,FROM             SAVE FROM VALUE                              
*                                                                               
         LA    R2,ADJTOH           VALIDATE TO FIELD                            
         GOTO1 ANY                                                              
         LA    R3,8(R2)                                                         
         CLI   0(R3),C'='          IF SPECIAL '=' COMMAND                       
         BNE   VR30                                                             
         MVI   EQUALS,C'Y'         THEN SET FLAG FOR EQUALS                     
         LA    R3,1(R3)                                                         
         BAS   RE,VALNUM           AND VALIDATE THE NUMBER ONLY                 
         B     VR40                                                             
*                                                                               
VR30     BAS   RE,VALNUM           ELSE VALIDATE THE NUMBER AS A                
         BAS   RE,VALREF               REFERENCE NUMBER ALSO                    
         ST    RF,TOREF                                                         
*                                                                               
VR40     STC   R1,TO               SAVE TO VALUE                                
         B     VR50                                                             
         EJECT                                                                  
VALNUM   CLI   0(R3),C'0'          VALIDATE 1 OR 2 DIGIT NUMBER                 
         BL    INVERR                                                           
         CLI   0(R3),C'9'                                                       
         BH    INVERR                                                           
         CLI   1(R3),X'40'                                                      
         BH    VALNUM10                                                         
         PACK  DUB,0(1,R3)         1 DIGIT NUMBER                               
         B     VALNUM20                                                         
*                                                                               
VALNUM10 CLI   1(R3),C'0'                                                       
         BL    INVERR                                                           
         CLI   1(R3),C'9'                                                       
         BH    INVERR                                                           
         CLI   2(R3),X'40'                                                      
         BH    INVERR                                                           
         PACK  DUB,0(2,R3)         2 DIGIT NUMBER                               
*                                                                               
VALNUM20 CVB   R1,DUB              CONVERT INTO R1                              
         BR    RE                                                               
         SPACE 3                                                                
VALREF   LA    RF,REFTAB           VALIDATE R1 IS A LEGAL REFERENCE #           
*                                                                               
VALREF10 CLI   0(RF),0             ERROR IF END OF LIST BEFORE MATCH            
         BE    INVERR                                                           
         ZIC   R0,0(RF)                                                         
         CR    R0,R1                                                            
         BER   RE                  MATCH FOUND                                  
         LA    RF,REFTABL(RF)                                                   
         B     VALREF10            TRY AGAIN                                    
         EJECT                                                                  
VR50     XC    THISDATE,QSTART     VALIDATE DATE FIELD                          
         MVC   NEXTDATE,QEND                                                    
         LA    R2,ADJDATSH                                                      
         CLI   5(R2),0                                                          
         BE    VR70                                                             
         LA    R5,SCANOUT                                                       
         GOTO1 SCANNER,DMCB,(R2),SCANOUT,C',=,-'                                
         CLI   4(R1),1                                                          
         BNE   INVERR                                                           
*                                                                               
         CLI   0(R5),0             FIRST DATE                                   
         BE    VR60                                                             
         GOTO1 DATVAL,DMCB,(1,12(R5)),THISDATE                                  
         OC    DMCB(4),DMCB                                                     
         BZ    INVERR                                                           
         CLC   THISDATE+2(4),QSTART+2                                           
         BL    *+14                                                             
         MVC   THISDATE(2),QSTART                                               
         B     *+10                                                             
         MVC   THISDATE(2),QEND                                                 
*                                                                               
VR60     CLI   1(R5),0             SECOND DATE                                  
         BE    VR70                                                             
         GOTO1 DATVAL,DMCB,(1,22(R5)),NEXTDATE                                  
         OC    DMCB(4),DMCB                                                     
         BZ    INVERR                                                           
         CLC   NEXTDATE+2(4),QSTART+2                                           
         BL    *+14                                                             
         MVC   NEXTDATE(2),QSTART                                               
         B     *+10                                                             
         MVC   NEXTDATE(2),QEND                                                 
*                                                                               
VR70     CLC   THISDATE,NEXTDATE     FIRST DATE MUST COME BEFORE SECOND         
         BNL   INVERR                                                           
         EJECT                                                                  
VR100    L     R5,FROMREF                                                       
         USING REFTABD,R5                                                       
         MVI   ELCODE,WKCODEQ                                                   
         CLI   EQUALS,C'Y'         IF EQUALS IS USED                            
         BNE   VR110                                                            
*                                  THEN GET 'FROM' REC FOR UPDATE               
         GOTO1 DATAMGR,DMCB,(X'80',=C'GETREC'),=C'SPTFIL',REFDISKA,AIO1X        
               ,DMWORK                                                          
         L     R6,AIO1                                                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     VR130                                                            
*                                  ELSE GET 'FROM' REC FOR READ ONLY            
VR110    GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFIL',REFDISKA,AIO2,DMWORK          
         L     R6,AIO2                                                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    R7,R6               R7 = A(FROM REC'S FIRST WEEK ELEM)           
         L     R5,TOREF                                                         
*                                  AND GET 'TO' REC FOR UPDATE                  
         GOTO1 DATAMGR,DMCB,(X'80',=C'GETREC'),=C'SPTFIL',REFDISKA,AIO1X        
               ,DMWORK                                                          
         L     R6,AIO1                                                          
         BAS   RE,GETEL                                                         
         BE    *+6                 R6 = A(TO REC'S FIRST WEEK ELEM)             
         DC    H'0'                                                             
         USING CSOWKEL,R6                                                       
*                                                                               
VR130    GOTO1 DATCON,DMCB,(2,WKDATE),(0,LASTDATE)                              
         CLC   LASTDATE,THISDATE                                                
         BNL   VR140                                                            
         LA    R7,WKLENQ(R7)       BUMP R7 AND R6                               
         BAS   RE,NEXTEL                                                        
         BE    VR130                                                            
         DC    H'0'                                                             
*                                  WHILE WEEK IS BEFORE FINAL WEEK              
VR140    GOTO1 DATCON,DMCB,(2,WKDATE),(0,LASTDATE)                              
         CLC   LASTDATE,NEXTDATE                                                
         BH    VR190                                                            
         CLI   EQUALS,C'Y'         IF EQUALS SIGN SELECTED                      
         BNE   VR150                                                            
         ZIC   R0,TO               GET SPOTS FROM VARIABLE TO                   
         B     VR160                                                            
         DROP  R6                                                               
         USING CSOWKEL,R7                                                       
*                                                                               
VR150    CLI   TC,C'T'             ELSE IF TRADE SPOTS SELECTED                 
         BNE   *+12                                                             
         LA    RF,WKTSPOTS         GET TRADE SPOTS FROM 'FROM' REC              
         B     *+8                                                              
         LA    RF,WKCSPOTS         ELSE GET CASH SPOTS                          
         SR    R0,R0                                                            
         ICM   R0,3,0(RF)                                                       
         XC    0(2,RF),0(RF)       ERASE SPOTS FROM 'FROM' RECORD               
         DROP  R7                                                               
         USING CSOWKEL,R6                                                       
*                                                                               
VR160    CLI   TC,C'T'             IF TRADE SELECTED                            
         BNE   *+12                                                             
         LA    RF,WKTSPOTS         POINT TO TRADE SPOTS                         
         B     *+8                                                              
         LA    RF,WKCSPOTS         ELSE POINT TO CASH SPOTS                     
         DROP  R6                                                               
*                                                                               
         CLI   EQUALS,C'N'         IF EQUALS SIGN NOT SELECTED                  
         BNE   VR170                                                            
         SR    R1,R1                                                            
         ICM   R1,3,0(RF)                                                       
         AR    R0,R1               ADD OLD SPOTS TO NEW SPOTS                   
*                                                                               
VR170    STCM  R0,3,0(RF)          UPDATE RECORD WITH NEW SPOTS                 
         LA    R7,WKLENQ(R7)                                                    
         BAS   RE,NEXTEL           BUMP TO NEXT WEEK ELEMENT                    
         BE    VR140                                                            
*                                                                               
VR190    CLI   EQUALS,C'Y'         IF EQUALS SELECTED                           
         BNE   *+12                                                             
         L     R5,FROMREF          POINT TO FROM DISK ADDRESS                   
         B     *+8                                                              
         L     R5,TOREF            ELSE POINT TO TO DISK ADDRESS                
*                                  WRITE BACK UPDATED RECORD                    
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'SPTFIL',REFDISKA,AIO1,DMWORK          
         CLI   EQUALS,C'N'         IF EQUALS NOT SELECTED                       
         BNE   VR200                                                            
*                                                                               
         L     R5,FROMREF          THEN GET 'FROM' REC FOR UPDATE               
         GOTO1 DATAMGR,DMCB,(X'80',=C'GETREC'),=C'SPTFIL',REFDISKA,AIO3X        
               ,DMWORK                                                          
*                                  WRITE BACK UPDATED RECORD                    
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'SPTFIL',REFDISKA,AIO2,DMWORK          
         DROP  R5                                                               
         EJECT                                                                  
VR200    LA    R3,ADJPROGH         VALIDATE PROGRAMS FIELDS                     
         L     R5,REFPTR                                                        
         USING REFTABD,R5                                                       
*                                                                               
VR210    LR    R2,R3                                                            
*                                                                               
VR220    TM    4(R2),X'20'         TEST CHANGE OF FIELD                         
         BZ    VR230                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         TM    1(R2),X'20'                                                      
         BZ    VR220                                                            
         B     VR290               NO CHANGE THEN GO TO NEXT LINE               
*                                                                               
VR230    MVI   RECCHANG,C'Y'                                                    
*                                  READ RECORD AND UPDATE IT                    
         GOTO1 DATAMGR,DMCB,(X'80',=C'GETREC'),=C'SPTFIL',REFDISKA,AIO,X        
               DMWORK                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,DSCCODEQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CSODSCEL,R6                                                      
*                                                                               
         LR    R2,R3               VALIDATE PROGRAM NAME                        
         MVC   DSCPROG,8(R2)                                                    
         OC    DSCPROG,=CL20' '                                                 
         MVC   REFPROG,DSCPROG                                                  
*                                                                               
         ZIC   R0,0(R2)            VALIDATE DATE                                
         AR    R2,R0                                                            
         ZIC   R4,5(R2)                                                         
         GOTO1 DAYVAL,DMCB,((R4),8(R2)),BYTE,MYWORK                             
         CLI   BYTE,0                                                           
         BE    INVERR                                                           
         MVC   DSCDAY,BYTE                                                      
         MVC   REFDAY,DSCDAY                                                    
*                                                                               
         ZIC   R0,0(R2)            VALIDATE TIME                                
         AR    R2,R0                                                            
         ZIC   R4,5(R2)                                                         
         GOTO1 TIMVAL,DMCB,((R4),8(R2)),MYWORK                                  
         CLI   0(R1),X'FF'                                                      
         BE    INVERR                                                           
         MVC   DSCTIME,MYWORK                                                   
         MVC   REFTIME,DSCTIME                                                  
*                                  WRITE BACK UPDATED RECORD                    
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'SPTFIL',REFDISKA,AIO,DMWORK           
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
VR290    LA    R5,REFTABL(R5)      NEXT REFERENCE                               
         CLI   REFNUM,0                                                         
         BE    VR300               TEST END OF REFTAB OR NEXTREF                
         C     R5,NEXTREF                                                       
         BNL   VR300               END OF LOOP                                  
*                                                                               
         ZIC   R0,0(R2)            BUMP TO NEXT LINE                            
         AR    R2,R0                                                            
         LR    R3,R2                                                            
         B     VR220                                                            
*                                                                               
VR300    CLI   RECCHANG,C'N'       IF RECORD FIELDS DON'T CHANGE                
         BNE   DR                                                               
         MVC   REFPTR,NEXTREF      POINT TO NEXT GROUP OF REFERENCES            
         MVC   REFDISP,NEXTDISP                                                 
         L     RE,REFPTR                                                        
         CLI   0(RE),0             IF THE NEXT GROUP IS PAST THE END            
         BNE   DR                                                               
         LA    RE,REFTAB           POINT BACK TO FIRST GROUP                    
         ST    RE,REFPTR                                                        
         XC    REFDISP,REFDISP                                                  
         B     DR                                                               
         EJECT                                                                  
* DISPLAY CURRENT GROUP OF REFERENCES                                           
*                                                                               
DR       GOTO1 CLEARF,DMCB,(0,ADJTCH),ADJLAST                                   
         GOTO1 CLEARF,DMCB,(1,ADJREFH),ADJLAST                                  
         MVC   ADJHEAD(L'HEADING),HEADING                                       
         OI    ADJHEADH+6,X'80'                                                 
*                                                                               
         L     R5,REFPTR                                                        
         LA    R2,ADJREFH                                                       
*                                                                               
DR10     EDIT  (1,REFNUM),(2,8(R2)),FILL=0                                      
         OI    6(R2),X'80'                                                      
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         MVC   8(20,R2),REFPROG                                                 
         OI    4(R2),X'20'                                                      
         OI    6(R2),X'80'                                                      
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         GOTO1 UNDAY,DMCB,REFDAY,8(R2)                                          
         OI    4(R2),X'20'                                                      
         OI    6(R2),X'80'                                                      
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         GOTO1 UNTIME,DMCB,REFTIME,8(R2)                                        
         OI    4(R2),X'20'                                                      
         OI    6(R2),X'80'                                                      
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    DRX                                                              
*                                                                               
         LA    R5,REFTABL(R5)                                                   
         CLI   0(R5),0                                                          
         BNE   DR10                                                             
*                                                                               
DRX      ST    R5,NEXTREF                                                       
         LA    RE,REFTAB                                                        
         SR    R5,RE                                                            
         ST    R5,NEXTDISP                                                      
         B     EXIT                                                             
*                                                                               
HEADING  DC    C'REF     PROGRAM NAME       DAY       TIME'                     
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
EXIT     OI    ADJTCH+6,X'40'    SET CURSOR                                     
         XC    CONHEAD,CONHEAD                                                  
         OI    CONSERVH+6,X'81'    SET MODIFIED AND XMIT BIT                    
         LA    R2,ADJTCH                                                        
*                                                                               
         CLI   KEYCHANG,C'Y'       IF KEY HAS CHANGED                           
         BNE   EXIT10              THEN GIVE 'ENTER CHANGES' MESSAGE            
         MVC   CONHEAD(30),=C'DATA DISPLAYED - ENTER CHANGES'                   
         B     EXITX                                                            
*                                  ELSE GIVE 'CHANGES ACCEPTED' MESSAGE         
EXIT10   MVC   CONHEAD(37),=C'CHANGES ACCEPTED - ENTER NEXT REQUEST'            
*                                                                               
EXITX    GOTO1 ERREX2                                                           
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
       ++INCLUDE SPCSOFAD                                                       
         EJECT                                                                  
       ++INCLUDE SPCSOWORKD                                                     
         EJECT                                                                  
* WORK AREA                                                                     
*                                                                               
         ORG   SYSSPARE                                                         
KEYCHANG DS    C                                                                
TC       DS    C                                                                
FROM     DS    X                                                                
TO       DS    X                                                                
EQUALS   DS    C                                                                
RECCHANG DS    C                                                                
NUMREFS  DS    F                                                                
REFDISP  DS    F                                                                
NEXTDISP DS    F                                                                
REFPTR   DS    A                                                                
NEXTREF  DS    A                                                                
FROMREF  DS    A                                                                
TOREF    DS    A                                                                
REFTAB   DS    30CL(REFTABL)                                                    
         EJECT                                                                  
       ++INCLUDE SPGENCSO                                                       
         EJECT                                                                  
REFTABD  DSECT                                                                  
REFNUM   DS    XL1                 REFERENCE NUMBER                             
REFDISKA DS    CL4                 DISK ADDRESS                                 
REFPROG  DS    CL20                PROGRAM NAME                                 
REFDAY   DS    XL1                 DAY CODE                                     
REFTIME  DS    XL4                 TIME CODE                                    
REFTABL  EQU   *-REFTABD                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'061SPCSO0A   05/01/02'                                      
         END                                                                    
