*          DATA SET SPCSO0D    AT LEVEL 056 AS OF 05/01/02                      
*PHASE T2180DA                                                                  
         TITLE 'T2180D - NTP CALCULATION RECORD MAINTENANCE'                    
T2180D   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T2180D                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         CLI   MYOVNUM,X'0D'       CHECK FIRST TIME FOR THIS OVERLAY            
         BE    GOAHEAD                                                          
         NI    REQMEDH+4,X'DF'     FORCE VALIDATION OF KEY FIELDS               
GOAHEAD  MVI   MYOVNUM,X'0D'       STORE OVERLAY NUMBER                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,XRECPUT                                                     
         BE    DR                                                               
         CLI   MODE,XRECADD                                                     
         BE    DR                                                               
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
*                                                                               
VK       LA    R2,REQMEDH          VALIDATE MEDIA FIELD                         
         TM    4(R2),X'20'                                                      
         BO    VKCLT                                                            
         NI    REQCLTH+4,X'DF'                                                  
         GOTO1 ANY                                                              
         GOTO1 VALIMED                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
VKCLT    LA    R2,REQCLTH          VALIDATE CLIENT FIELD                        
         TM    4(R2),X'20'                                                      
         BO    VKMKT                                                            
         NI    REQMKTH+4,X'DF'                                                  
         GOTO1 ANY                                                              
         GOTO1 VALICLT                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
VKMKT    LA    R2,REQMKTH          VALIDATE MARKET FIELD                        
         TM    4(R2),X'20'                                                      
         BO    VKEST                                                            
         NI    REQESTH+4,X'DF'                                                  
         GOTO1 ANY                                                              
         GOTO1 VALIMKT                                                          
         XC    BSTA,BSTA                                                        
         OI    4(R2),X'20'                                                      
*                                                                               
VKEST    LA    R2,REQESTH          VALIDATE :30 ESTIMATE FIELD                  
         TM    4(R2),X'20'                                                      
         BO    VKX                                                              
         GOTO1 ANY                                                              
         GOTO1 VALIMEST                                                         
*                                                                               
         CLI   MASTSPLN,30         MUST BE :30 ESTIMATE                         
         BNE   ERREST                                                           
*                                                                               
         MVC   BMEST30,BMEST       SAVE :30 ESTIMATE NUMBER                     
         MVC   SVSUBS30,SVSUBS     AND SUB ESTIMATE LIST                        
         OI    4(R2),X'20'                                                      
*                                                                               
         GOTO1 CLRACC              INITIALIZE ACCUMULATOR TABLE                 
*                                                                               
         MVC   ACCNUM,=F'0'        COMPUTE :30 GOALS                            
         GOTO1 CMPGOL                                                           
*                                                                               
         LA    R4,MESTLST          FIND FIRST :60 CASH ESTIMATE                 
         USING MESTLSTD,R4                                                      
         SR    R5,R5                                                            
*                                                                               
VKE10    CLC   MESTNUM,BMEST30     FIRST FIND :30 ESTIMATE                      
         BE    VKE20                                                            
         LA    R4,MESTLSTL(R4)                                                  
         LA    R5,1(R5)                                                         
         B     VKE10                                                            
*                                                                               
VKE20    CLI   MESTSPLN,60         THEN FIND FIRST :60 EST                      
         BE    VKE30                                                            
         LA    R4,MESTLSTL(R4)                                                  
         B     VKE20                                                            
*                                                                               
VKE30    LTR   R5,R5               THEN BUMP TO MATCHING :60 EST                
         BZ    VKE40                                                            
         LA    R4,MESTLSTL(R4)                                                  
         BCT   R5,*-4                                                           
*                                                                               
VKE40    MVC   BMEST60,MESTNUM     SAVE :60 ESTIMATE NUMBER                     
         MVC   SVSUBS60,MESTSUBS   AND SUB ESTIMATE LIST                        
*                                                                               
         MVC   BMEST,BMEST60       COMPUTE :60 GOALS                            
         MVC   SVSUBS,SVSUBS60                                                  
         MVC   ACCNUM,=F'1'                                                     
         GOTO1 CMPGOL                                                           
*                                                                               
         LA    R5,ACCTAB           COMPUTE TOTALS FOR :30 AND :60 GOALS         
         USING ACCTABD,R5                                                       
         GOTO1 TOTFULL,DMCB,ACCONE,ACCTABL     TOTAL 30S                        
         MVC   TOTAL30,0(R1)                                                    
         GOTO1 TOTFULL,DMCB,ACCTWO,ACCTABL     TOTAL 60S                        
         MVC   TOTAL60,0(R1)                                                    
*                                                                               
         L     R0,TOTAL30          COMPUTE TOTAL GOAL DOLLARS                   
         A     R0,TOTAL60                                                       
         ST    R0,TOTALGOL                                                      
         LTR   R0,R0                                                            
         BZ    ERRZERO             ERROR IF ZERO                                
*                                                                               
         L     RF,TOTAL30          :30 % = (10000 * TOTAL 30S) /                
         M     RE,=F'20000'                (TOTAL GOAL)                         
         D     RE,TOTALGOL                                                      
         LA    RF,1(RF)            ROUND TO NEAREST 1/100 %                     
         SRL   RF,1                                                             
         ST    RF,PERC30                                                        
*                                                                               
         L     RE,=F'10000'        :60 % = 100 % - :30 %                        
         SR    RE,RF                                                            
         ST    RE,PERC60                                                        
*                                  DISPLAY GOAL DOLLARS                         
         XC    REQTGL,REQTGL                                                    
         OI    REQTGLH+6,X'80'                                                  
         GOTO1 CLEARF,DMCB,(1,REQGL30H),REQGLXH                                 
*                                                                               
         LA    R2,REQTGLH          DISPLAY TOTAL GOAL DOLLARS                   
         EDIT  (4,TOTALGOL),(10,8(R2)),ALIGN=LEFT,ZERO=NOBLANK                  
*                                                                               
         LA    R2,REQGL30H         DISPLAY :30 GOAL DOLLARS                     
         EDIT  (4,TOTAL30),(10,8(R2)),ALIGN=LEFT,ZERO=NOBLANK                   
         LA    R3,8(R2)                                                         
         AR    R3,R0                                                            
*                                                                               
         MVI   1(R3),C'('          DISPLAY :30 %                                
         EDIT  (4,PERC30),(6,2(R3)),2,ALIGN=LEFT,ZERO=NOBLANK                   
         LA    R3,2(R3)                                                         
         AR    R3,R0                                                            
         MVI   0(R3),C'%'                                                       
         MVI   1(R3),C')'                                                       
*                                                                               
         MVC   REQGL3X,REQGL30     ECHO TO NEXT FIELD                           
*                                                                               
         LA    R2,REQGL60H         DISPLAY :60 GOAL DOLLARS                     
         EDIT  (4,TOTAL60),(10,8(R2)),ALIGN=LEFT,ZERO=NOBLANK                   
         LA    R3,8(R2)                                                         
         AR    R3,R0                                                            
*                                                                               
         MVI   1(R3),C'('          DISPLAY :60 %                                
         EDIT  (4,PERC60),(6,2(R3)),2,ALIGN=LEFT,ZERO=NOBLANK                   
         LA    R3,2(R3)                                                         
         AR    R3,R0                                                            
         MVI   0(R3),C'%'                                                       
         MVI   1(R3),C')'                                                       
*                                                                               
VKX      LA    R6,KEY              BUILD KEY                                    
         USING NTPKEY,R6                                                        
         XC    KEY,KEY                                                          
         MVI   NTPKTYPE,NTPKTYPQ   NTP RECORD TYPE                              
         MVI   NTPKSTYP,NTPKSTPQ   NTP RECORD SUB-TYPE                          
         MVC   NTPKAM,BAGYMD                                                    
         MVC   NTPKCLT,BCLT                                                     
         MVC   NTPKMKT,BMKT                                                     
         MVC   NTPKEST,BMEST30                                                  
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* VALIDATE RECORD                                                               
*                                                                               
VR       CLI   ACTNUM,ACTADD       IF ACTION ADD                                
         BNE   VR10                                                             
*                                                                               
         L     R6,AIO              THEN BUILD RECORD                            
         USING NTPRECD,R6                                                       
         XC    0(30,R6),0(R6)                                                   
         MVC   0(13,R6),KEY        INSERT KEY                                   
         MVC   NTPLEN,DATADISP     AND RECORD LENGTH                            
         MVC   NTPAGYA,AGENCY      AND ALPHA AGENCY CODE                        
         B     VR20                                                             
*                                                                               
VR10     MVI   ELCODE,NTCODEQ      ELSE REMOVE OLD ELEMENTS                     
         GOTO1 REMELEM                                                          
*                                                                               
VR20     LA    R6,ELEM             INITIALIZE ELEMENT                           
         USING NTELEM,R6                                                        
         MVI   NTCODE,NTCODEQ                                                   
         MVI   NTLEN,NTLENQ                                                     
*                                                                               
         LA    R2,REQL1H           POINT TO FIRST LINE                          
         GOTO1 ANY                                                              
*                                                                               
         SR    R7,R7               R7 = SEQUENCE NUMBER                         
*                                                                               
VR30     LA    R3,REQDSHH          WHILE NOT END OF LINES                       
         CR    R2,R3                                                            
         BE    VR100                                                            
*                                                                               
         CLI   5(R2),0             IF DEAL NAME FIELD EMPTY                     
         BNE   VR35                                                             
         ZIC   R0,0(R2)            THEN AMOUNT MUST ALSO BE EMPTY               
         AR    R2,R0                                                            
         CLI   5(R2),0                                                          
         BNE   INVERR                                                           
         B     VR90                SKIP TO NEXT LINE                            
*                                                                               
VR35     MVC   NTDEAL,8(R2)        ELSE SAVE DEAL NAME IN ELEMENT               
*                                                                               
         ZIC   R0,0(R2)            BUMP TO AMOUNT FIELD                         
         AR    R2,R0                                                            
         GOTO1 ANY                                                              
*                                                                               
         NI    NTSTAT,X'7F'        SET SUPPRESS PERCENTAGE BIT IF '*'           
         ZIC   R3,5(R2)                COMES AFTER AMOUNT                       
         LA    R3,8(R2,R3)                                                      
         BCTR  R3,0                                                             
         CLI   0(R3),C'*'                                                       
         BNE   VR40                                                             
         OI    NTSTAT,X'80'                                                     
         BCTR  R3,0                                                             
*                                                                               
VR40     LA    RF,8(R2)            R3 = LENGTH OF AMOUNT - 1                    
         SR    R3,RF                                                            
*                                                                               
         LA    R4,8(R2)            TEST ALL BYTES IN AMOUNT ARE NUMERIC         
         LA    R5,1(R3)                                                         
*                                                                               
VR50     CLI   0(R4),C'0'          TEST BYTE IS NOT NUMERIC                     
         BL    ERRNUM                                                           
         CLI   0(R4),C'9'                                                       
         BH    ERRNUM                                                           
*                                                                               
         LA    R4,1(R4)            BUMP TO NEXT BYTE AND TRY AGAIN              
         BCT   R5,VR50                                                          
*                                                                               
         EX    R3,*+8              PACK AMOUNT INTO ELEMENT                     
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R0,DUB                                                           
         STCM  R0,15,NTAMNT                                                     
*                                                                               
         STC   R7,NTSEQ            SAVE SEQUNCE NUMBER IN ELEMENT               
         GOTO1 ADDELEM             ADD ELEMENT                                  
         LA    R7,1(R7)            INCREMENT SEQUENCE NUMBER                    
*                                                                               
VR90     LA    RF,3                BUMP TO NEXT LINE                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   RF,*-8                                                           
         B     VR30                                                             
*                                                                               
VR100    DS    0H                                                               
*                                                                               
VRX      B     XIT                                                              
         EJECT                                                                  
* DISPLAY RECORD                                                                
*                                                                               
DR       GOTO1 CLEARF,DMCB,(0,REQL1H),REQDSHH                                   
         GOTO1 CLEARF,DMCB,(1,REQL1H),REQDSHH                                   
         GOTO1 CLEARF,DMCB,(1,REQTNTPH),REQTNTXH                                
         GOTO1 CLEARF,DMCB,(1,REQTAVCH),REQLAST                                 
*                                                                               
         XC    ACCUMTOT,ACCUMTOT   CLEAR ACCUMULATORS                           
         XC    ACCUM30,ACCUM30                                                  
         XC    ACCUM60,ACCUM60                                                  
*                                                                               
         L     R6,AIO              POINT R6 TO FIRST ELEMENT                    
         MVI   ELCODE,NTCODEQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,REQL1H           POINT R2 TO FIRST DISPLAY LINE               
*                                                                               
DR10     MVC   8(16,R2),NTDEAL     DISPLAY DEAL NAME                            
*                                                                               
         ZIC   R0,0(R2)            DISPLAY NTP AMOUNT                           
         AR    R2,R0                                                            
         EDIT  (4,NTAMNT),(10,8(R2)),ALIGN=LEFT,ZERO=NOBLANK                    
         LA    R3,8(R2)                                                         
         AR    R3,R0                                                            
*                                                                               
         TM    NTSTAT,X'80'        DISPLAY '*' IF SUPPRESS PERCENTAGE           
         BZ    *+8                     BIT IS SET IN STAT                       
         MVI   0(R3),C'*'                                                       
*                                                                               
         ZIC   R0,0(R2)            DISPLAY :30 AMOUNT                           
         AR    R2,R0                                                            
         ICM   R5,15,NTAMNT        DON'T APPLY PERCENTAGE IF SUPPRESS           
         TM    NTSTAT,X'80'            PERCENTAGE BIT SET                       
         BO    DR20                                                             
*                                                                               
         M     R4,PERC30           OTHERWISE APPLY PERCENTAGE AND ROUND         
         AR    R5,R5                                                            
         D     R4,=F'10000'                                                     
         LA    R5,1(R5)                                                         
         SRL   R5,1                                                             
*                                                                               
DR20     EDIT  (R5),(10,8(R2)),ALIGN=LEFT,ZERO=NOBLANK                          
*                                                                               
         ZIC   R0,0(R2)            DISPLAY :60 AMOUNT                           
         AR    R2,R0                                                            
         ICM   R4,15,NTAMNT                                                     
         SR    R4,R5                                                            
         EDIT  (R4),(10,8(R2)),ALIGN=LEFT,ZERO=NOBLANK                          
*                                                                               
         ZIC   R0,0(R2)            BUMP TO NEXT DISPLAY LINE                    
         AR    R2,R0                                                            
*                                                                               
         L     R0,ACCUMTOT         UPDATE TOTAL ACCUMULATOR                     
         ICM   R1,15,NTAMNT                                                     
         AR    R0,R1                                                            
         ST    R0,ACCUMTOT                                                      
*                                                                               
         L     R0,ACCUM30          UPDATE :30 ACCUMULATOR                       
         AR    R0,R5                                                            
         ST    R0,ACCUM30                                                       
*                                                                               
         L     R0,ACCUM60          UPDATE :60 ACCUMULATOR                       
         AR    R0,R4                                                            
         ST    R0,ACCUM60                                                       
*                                                                               
         BAS   RE,NEXTEL                                                        
         BE    DR10                                                             
*                                                                               
DR100    LA    R2,REQTNTPH         DISPLAY TOTAL NTP                            
         EDIT  (4,ACCUMTOT),(10,8(R2)),ALIGN=LEFT,ZERO=NOBLANK                  
*                                                                               
         ZIC   R0,0(R2)            DISPLAY :30 NTP                              
         AR    R2,R0                                                            
         EDIT  (4,ACCUM30),(10,8(R2)),ALIGN=LEFT,ZERO=NOBLANK                   
*                                                                               
         ZIC   R0,0(R2)            DISPLAY :60 NTP                              
         AR    R2,R0                                                            
         EDIT  (4,ACCUM60),(10,8(R2)),ALIGN=LEFT,ZERO=NOBLANK                   
*                                                                               
         LA    R2,REQTAVCH         DISPLAY OLD :30 AVAILABLE CASH               
         L     R4,TOTAL30                                                       
         S     R4,ACCUMTOT                                                      
         EDIT  (R4),(10,8(R2)),ALIGN=LEFT,ZERO=NOBLANK                          
*                                                                               
         ZIC   R0,0(R2)            DISPLAY NEW :30 AVAILABLE CASH               
         AR    R2,R0                                                            
         L     R4,TOTAL30                                                       
         S     R4,ACCUM30                                                       
         EDIT  (R4),(10,8(R2)),ALIGN=LEFT,ZERO=NOBLANK                          
*                                                                               
         ZIC   R0,0(R2)            DISPLAY :60 AVAILABLE CASH                   
         AR    R2,R0                                                            
         L     R4,TOTAL60                                                       
         S     R4,ACCUM60                                                       
         EDIT  (R4),(10,8(R2)),ALIGN=LEFT,ZERO=NOBLANK                          
*                                                                               
DRX      B     XIT                                                              
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
ERREST   MVI   ERROR,INVEST                                                     
         B     TRAPERR                                                          
*                                                                               
ERRNUM   MVI   ERROR,NOTNUM                                                     
         B     TRAPERR                                                          
*                                                                               
ERRZERO  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(23),=C'ERROR - NO GOAL DOLLARS'                          
         OI    CONSERVH+6,X'81'    SET MODIFIED AND TRANSMIT                    
         LA    R2,REQMEDH                                                       
         GOTO1 ERREX2                                                           
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPCSOFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPCSOFDD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE SPCSOWORKD                                                     
         EJECT                                                                  
* WORK AREA                                                                     
*                                                                               
         ORG   SYSSPARE                                                         
BMEST30  DS    X                   :30 ESTIMATE NUMBER                          
BMEST60  DS    X                   :60 ESTIMATE NUMBER                          
SVSUBS30 DS    XL(L'MESTSUBS)      :30 EST SUB ESTIMATE LIST                    
SVSUBS60 DS    XL(L'MESTSUBS)      :60 EST SUB ESTIMATE LIST                    
TOTAL30  DS    F                   TOTAL :30 GOAL DOLLARS                       
TOTAL60  DS    F                   TOTAL :60 GOAL DOLLARS                       
TOTALGOL DS    F                   TOTAL GOAL DOLLARS                           
ACCUM30  DS    F                   TOTAL :30 NTP DOLLARS                        
ACCUM60  DS    F                   TOTAL :60 NTP DOLLARS                        
ACCUMTOT DS    F                   TOTAL NTP DOLLARS                            
PERC30   DS    F                   % OF :30 / (:30 + :60)                       
PERC60   DS    F                   % OF :60 / (:30 + :60)                       
         EJECT                                                                  
       ++INCLUDE SPGENCSO                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'056SPCSO0D   05/01/02'                                      
         END                                                                    
