*          DATA SET SPLFM32    AT LEVEL 022 AS OF 12/18/09                      
*PHASE T21932A,+0                                                               
         TITLE 'T21932 - AGENCY HEADER'                                         
*============================================================*                  
*               AGENCY PROFILE                               *                  
*                                                            *                  
* +0      RATING SERVICE                                     *                  
* +1      CLIENT REGIONS                                     *                  
* +2      BILLING PCTG                                       *                  
* +3            "                                            *                  
* +4      EXTENDED DEMOS                                     *                  
* +5      BUY PERIOD OTO'S                                   *                  
* +6      -S AUTHOR REQUIRED                                 *                  
* +7      CANADIAN                                           *                  
* +8      OLD POOL TSHEETS                                   *                  
* +9      BILLING REQUIRED                                   *                  
* +10     CREDIT BUY LIMIT                                   *                  
* +11     - BRAND POL RADIO                                  *                  
* +12     MKGDS IN MISSED MNTH                               *                  
* +13     OFFICE REQUIRED                                    *                  
* +14     BUYER/BILLER                                       *                  
* +15     SPECIAL OTO CODE                                   *                  
* +16     BILLING                                            *                  
* +17     AGENCY ALPHA                                       *                  
* +18           "                                            *                  
* +19     AGENCY HEX                                         *                  
*============================================================*                  
         EJECT                                                                  
T21932   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21932,RR=R9                                                   
         ST    R9,RELO                                                          
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T219FFD,RA                                                       
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         USING AGYHDRD,R8                                                       
* CHANGE ACTION NO LONGER ALLOWED                                               
*                                                                               
         CLI   SVACT,C'C'                                                       
         BNE   MAIN20                                                           
         LA    R2,LFMACTH                                                       
         MVI   ERRCD,INVERR                                                     
         BE    LFMERR                                                           
*                                                                               
MAIN20   CLI   SVFMTSW,0                                                        
         BE    FMT                                                              
         B     EDT                                                              
*                                                                               
EXXMOD   XMOD1 1                                                                
         EJECT                                                                  
FMT      DS    0H                                                               
         MVC   KEY,SVKEY                                                        
         GOTO1 GETREC                                                           
*                                                                               
         LA    R2,AGYSNMEH                                                      
         FOUT  (R2),AGYNAME,33                                                  
         OI    4(R2),II1C                                                       
*                                                                               
         LA    R2,AGYSADDH                                                      
         FOUT  (R2),AGYADDR,33                                                  
         OI    4(R2),II1C                                                       
*                                                                               
         LA    R2,AGYSIDH                                                       
         FOUT  (R2),AGYID,3                                                     
         OI    4(R2),II1C                                                       
         LA    R2,AGYALPHH                                                      
         FOUT  (R2),AGYPROF+17,2                                                
         OI    4(R2),II1C                                                       
         LA    R2,AGYHEXH                                                       
         FOUT  (R2),AGYPROF+19,1                                                
         OI    4(R2),II1C                                                       
*                                                                               
         LA    R2,AGYSACCH                                                      
         FOUT  (R2),AGYACCT,18                                                  
         OI    4(R2),II1C                                                       
* FORMAT FREEFORM OPTIONS                                                       
         XC    SCRNFLAG,SCRNFLAG   INITIALIZE THE FLAG                          
         LA    R2,AGYOPTSH                                                      
         XC    8(L'AGYOPTS,R2),8(R2)                                            
         FOUT  (R2)                                                             
         LA    R4,8(R2)                                                         
         TM    AGYFLAG1,X'80'                                                   
         BZ    FMT2A                                                            
*                                                                               
         LA    R0,7                LENGTH OF OUTPUT DATA                        
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   FMT2A               NO - SO CONTINUE                             
*                                                                               
         MVC   0(7,R4),=C'ADDS=Y,'                                              
         LA    R4,7(R4)                                                         
FMT2A    TM    AGYFLAG1,X'40'                                                   
         BZ    FMT2B                                                            
*                                                                               
         LA    R0,10               LENGTH OF OUTPUT DATA                        
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   FMT2B               NO - SO CONTINUE                             
*                                                                               
         MVC   0(10,R4),=C'MEDNAME=Y,'                                          
         LA    R4,10(R4)                                                        
FMT2B    TM    AGYFLAG1,X'20'                                                   
         BZ    FMT2C                                                            
*                                                                               
         LA    R0,6                LENGTH OF OUTPUT DATA                        
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   FMT2C               NO - SO CONTINUE                             
*                                                                               
         MVC   0(6,R4),=C'CTA=Y,'                                               
         LA    R4,6(R4)                                                         
FMT2C    CLC   AGYCTAGY,=C'  '                                                  
         BNH   FMT2D                                                            
*                                                                               
         LA    R0,10               LENGTH OF OUTPUT DATA                        
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   FMT2D               NO - SO CONTINUE                             
*                                                                               
         MVC   0(10,R4),=C'CTFILE=XX,'                                          
         MVC   7(2,R4),AGYCTAGY                                                 
         LA    R4,10(R4)                                                        
FMT2D    TM    AGYFLAG1,X'10'                                                   
         BZ    FMT2E                                                            
*                                                                               
         LA    R0,8                LENGTH OF OUTPUT DATA                        
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   FMT2E               NO - SO CONTINUE                             
*                                                                               
         MVC   0(8,R4),=C'OFF=HEX,'                                             
         LA    R4,8(R4)                                                         
FMT2E    TM    AGYFLAG1,AGYTESTQ   X'08'                                        
         BZ    FMT2F                                                            
*                                                                               
         LA    R0,7                LENGTH OF OUTPUT DATA                        
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   FMT2F               NO - SO CONTINUE                             
*                                                                               
         MVC   0(7,R4),=C'TEST=Y,'                                              
         LA    R4,7(R4)                                                         
*                                                                               
FMT2F    TM    AGYFLAG1,AGYPRDQ    TEST DDS BILLING BY PRODUCT                  
         BZ    FMT2G                                                            
*                                                                               
         LA    R0,9                LENGTH OF OUTPUT DATA                        
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   FMT2G               NO - SO CONTINUE                             
*                                                                               
         MVC   0(8,R4),=C'DDSB=PRD'                                             
         MVI   8(R4),C','                                                       
         LA    R4,9(R4)                                                         
*                                                                               
FMT2G    TM    AGYFLAG2,AGYFLAG2_PW                                             
         BZ    FMT2K                                                            
         MVC   0(3,R4),=C'PW,'                                                  
         LA    R4,3(R4)                                                         
*                                                                               
FMT2K    LA    R6,REC+24                                                        
         MVI   ELCODE,X'71'                                                     
         USING AGYEXTEL,R6                                                      
         BAS   RE,NEXTEL                                                        
         BNE   FMT2L                                                            
         OC    AGYLOCK,AGYLOCK                                                  
         BZ    FMT2L                                                            
*                                                                               
         LA    R0,14               LENGTH OF OUTPUT DATA                        
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   FMT2L               NO - SO CONTINUE                             
*                                                                               
         MVC   0(5,R4),=C'LOCK='                                                
         GOTO1 VDATCON,DMCB,(3,AGYLOCK),(5,5(R4))                               
         MVI   13(R4),C','                                                      
         LA    R4,14(R4)                                                        
         DROP  R6                                                               
*                                                                               
FMT2L    EQU   *                                                                
*                                                                               
         TM    AGYFLAG1,AGYTRDQ    TRADE AGENCY?                                
         BZ    FMT2M               NO - SO CONTINUE                             
*                                                                               
         LA    R0,4                LENGTH OF OUTPUT DATA                        
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   FMT2M               NO - SO CONTINUE                             
*                                                                               
         MVC   0(4,R4),=C'TRD,'    ELSE - MOVE OUT LITERAL                      
         LA    R4,4(R4)            INC FIELD POSITION                           
*                                                                               
FMT2M    EQU   *                                                                
*                                                                               
         TM    AGYFLAG1,AGYCOS2Q   COST FACTOR REQUIRED?                        
         BZ    FMT2N               NO - SO CONTINUE                             
*                                                                               
         LA    R0,4                LENGTH OF OUTPUT DATA                        
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   FMT2N               NO - SO CONTINUE                             
*                                                                               
         MVC   0(5,R4),=C'COS2,'   ELSE - MOVE OUT LITERAL                      
         LA    R4,5(R4)            INC FIELD POSITION                           
*                                                                               
FMT2N    EQU   *                                                                
*                                                                               
         CLI   SCRNFLAG,0          DID EVERYTHING FIT?                          
         BE    FMT2X               YES - SO CONTINUE                            
*                                                                               
         MVI   0(R1),C'*'          ELSE - MOVE OUT 'DIDN'T FIT' FLAG            
         LA    R1,1(R1)            INC A(FIELD POSITION)                        
*                                                                               
FMT2X    BCTR  R4,0                                                             
         CLI   0(R4),C','                                                       
         BNE   FMT10                                                            
         MVI   0(R4),0             REMOVE LAST COMMA                            
*                                                                               
FMT10    LA    R2,AGYRATSH                                                      
         FOUT  (R2),AGYPROF,1                                                   
         OI    4(R2),II1C                                                       
         LA    R2,AGYCLRGH                                                      
         FOUT  (R2),AGYPROF+1,1                                                 
         OI    4(R2),II1C                                                       
         LA    R2,AGYBLPCH                                                      
         FOUT  (R2),AGYPROF+2,2                                                 
         OI    4(R2),II1C                                                       
         LA    R2,AGYEXDMH                                                      
         FOUT  (R2),AGYPROF+4,1                                                 
         OI    4(R2),II1C                                                       
         LA    R2,AGYBOTOH                                                      
         FOUT  (R2),AGYPROF+5,1                                                 
         OI    4(R2),II1C                                                       
         LA    R2,AGYCANH                                                       
         FOUT  (R2),AGYPROF+7,1                                                 
         OI    4(R2),II1C                                                       
         LA    R2,AGYTMSHH                                                      
         FOUT  (R2),AGYPROF+8,1                                                 
         OI    4(R2),II1C                                                       
         LA    R2,AGYBILLH                                                      
         FOUT  (R2),AGYPROF+16,1                                                
         LA    R2,AGYCBLH                                                       
         FOUT  (R2),AGYPROF+10,1                                                
         OI    4(R2),II1C                                                       
*                                                                               
         CLI   AGYPROF+9,C'Y'                                                   
         BE    FMTBID                                                           
         CLI   AGYPROF+9,C'A'                                                   
         BE    FMTBID                                                           
         MVI   AGYPROF+9,C'N'                                                   
FMTBID   FOUT  AGYBIRQH,AGYPROF+9,1                                             
         OI    AGYBIRQH+4,II1C                                                  
*                                                                               
         CLI   AGYPROF+11,C'Y'                                                  
         BE    *+8                                                              
         MVI   AGYPROF+11,C'0'                                                  
         FOUT  AGYRPOLH,AGYPROF+11,1                                            
**NOP    CLI   AGYPROF+15,C'Y'                                                  
**NOP    BE    *+8                                                              
**NOP    MVI   AGYPROF+15,C'N'                                                  
         FOUT  AGYXOTOH,AGYPROF+15,1                                            
         CLI   AGYPROF+12,C'Y'                                                  
         BE    *+8                                                              
         MVI   AGYPROF+12,C'N'                                                  
         FOUT  AGYMGDMH,AGYPROF+12,1                                            
         CLI   AGYPROF+6,C'Y'                                                   
         BE    *+8                                                              
         MVI   AGYPROF+6,C'N'                                                   
         FOUT  AGYSAUTH,AGYPROF+6,1                                             
         CLI   AGYPROF+13,C'Y'                                                  
         BE    *+8                                                              
         MVI   AGYPROF+13,C'N'                                                  
         FOUT  AGYOFRQH,AGYPROF+13,1                                            
         CLI   AGYPROF+14,C'Y'                                                  
         BE    *+8                                                              
         MVI   AGYPROF+14,C'N'                                                  
         FOUT  AGYBYBLH,AGYPROF+14,1                                            
*                                                                               
         LA    R2,AGYACOCH        ACC OFFICE CODE                               
         FOUT  (R2),AGYOFC2,1                                                   
         OI    4(R2),II1C                                                       
*                                                                               
FMTID    DS    0H                                                               
         XC    AGYTTLE,AGYTTLE     CLEAR FIELD                                  
         FOUT  AGYTTLEH                                                         
         LA    R2,AGYTTLEH                                                      
         XC    AGYTTLE,AGYTTLE                                                  
         LA    R6,REC+24                                                        
         MVI   ELCODE,X'70'                                                     
         USING AGYIDEL,R6                                                       
         BAS   RE,NEXTEL                                                        
         BNE   FMTRFPID                                                         
         MVC   AGYTTLE,AGYTITLE                                                 
*                                                                               
FMTRFPID XC    AGYRFPI,AGYRFPI                                                  
         FOUT  AGYRFPIH                                                         
         LA    R6,REC+24                                                        
         MVI   ELCODE,X'71'                                                     
         USING AGYEXTEL,R6                                                      
         BAS   RE,NEXTEL                                                        
         BNE   FMTACC1                                                          
         OC    AGYPRNID,AGYPRNID                                                
         BZ    FMTACC1                                                          
*                                                                               
         MVC   WORK(2),AGYPRNID    PASS TO DISPID                               
         BRAS  RE,DISPID           DISPLAY ID                                   
*                                                                               
         USING AGYACCEL,R6                                                      
FMTACC1  LA    R2,AGYACCH          ACC AGY CODE LIST                            
         XC    AGYACC,AGYACC                                                    
         OI    4(R2),II1C                                                       
         OI    6(R2),X'80'                                                      
         LA    R6,REC+24           TRY TO FIND OLD ELEM                         
         MVI   ELCODE,X'03'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   FMTACCX             NOT FOUND                                    
         LA    R2,8(R2)                                                         
         LA    R3,AGYACCAG                                                      
         LA    R1,8                                                             
         CLI   0(R3),C' '                                                       
         BNH   FMTACCX                                                          
FMTACC   MVC   0(2,R2),0(R3)                                                    
         LA    R3,2(R3)                                                         
         CLI   0(R3),C' '                                                       
         BNH   FMTACCX                                                          
         LA    R2,2(R2)                                                         
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
         BCT   R1,FMTACC                                                        
FMTACCX  DS    0H                                                               
         DROP  R6                                                               
*                                                                               
FMTID2   FOUT  (R2)                                                             
         LA    R2,AGYMEDH                                                       
         USING AGYMEDEL,R6                                                      
         LA    R6,REC+24                                                        
         MVI   ELCODE,X'02'                                                     
*                                                                               
FMT0     BAS   RE,NEXTEL                                                        
         BNE   FMT1                                                             
         FOUT  (R2),AGYMEDCD,1                                                  
         OI    4(R2),II1C                                                       
         ZIC   R3,0(R2)                                                         
         AR    R2,R3                                                            
         FOUT  (R2),AGYMEDEX,10                                                 
         OI    4(R2),II1C                                                       
         ZIC   R3,0(R2)                                                         
         AR    R2,R3                                                            
         FOUT  (R2),AGYVENEX,7                                                  
         OI    4(R2),II1C                                                       
         ZIC   R3,0(R2)                                                         
         AR    R2,R3                                                            
         B     FMT0                                                             
*                                                                               
FMT1     LA    R5,8                                                             
         ZIC   R3,0(R2)                                                         
         SR    R3,R5                                                            
         FOUT  (R2),SPACES,(R3)                                                 
         ZIC   R3,0(R2)                                                         
         LTR   R3,R3                                                            
         BZ    FMT2                                                             
         AR    R2,R3                                                            
         B     FMT1                                                             
*                                                                               
FMT2     B     EXXMOD                                                           
         EJECT                                                                  
EDT      DS    0H                                                               
         CLI   SVACT,C'A'                                                       
         BNE   EDT0                                                             
         MVC   AGYLEN,=AL2(150)                                                 
         MVC   AGYEL(2),=X'017E'                                                
         XC    AGYPROF,AGYPROF                                                  
         B     EDT1                                                             
EDT0     MVC   KEY,SVKEY                                                        
         GOTO1 GETREC                                                           
*                                                                               
EDT1     LA    R2,AGYSNMEH                                                      
         CLI   SVACT,C'A'                                                       
         BE    *+12                                                             
         TM    4(R2),II1C                                                       
         BO    EDT2                                                             
         BAS   RE,ANYD                                                          
         MVC   AGYNAME,8(R2)                                                    
*                                                                               
EDT2     LA    R2,AGYSADDH                                                      
         CLI   SVACT,C'A'                                                       
         BE    *+12                                                             
         TM    4(R2),II1C                                                       
         BO    EDT3                                                             
         BAS   RE,ANYD                                                          
         MVC   AGYADDR,8(R2)                                                    
*                                                                               
EDT3     LA    R2,AGYSIDH                                                       
         CLI   SVACT,C'A'                                                       
         BE    *+12                                                             
         TM    4(R2),II1C                                                       
         BO    EDT4                                                             
         BAS   RE,ANYD                                                          
         MVI   ERRCD,INVERR                                                     
         CLI   5(R2),2                                                          
         BL    LFMERR                                                           
         MVC   AGYID,8(R2)                                                      
*                                                                               
EDT4     LA    R2,AGYALPHH                                                      
         CLI   SVACT,C'A'                                                       
         BE    *+12                                                             
         TM    4(R2),II1C                                                       
         BO    EDT5                                                             
         CLI   SVACT,C'A'                                                       
         BE    EDT4A                                                            
         MVI   ERRCD,NOCHGERR                                                   
         FOUT  (R2),AGYPROF+17,2                                                
         OI    4(R2),II1C                                                       
         B     LFMERR                                                           
EDT4A    BAS   RE,ANYD                                                          
         MVI   ERRCD,INVERR                                                     
         CLI   8(R2),X'40'                                                      
         BE    LFMERR                                                           
         CLI   9(R2),X'40'                                                      
         BE    LFMERR                                                           
         MVC   AGYPROF+17(2),8(R2)                                              
*                                                                               
EDT5     LA    R2,AGYHEXH                                                       
         TM    4(R2),II1C                                                       
         BO    EDT5A                                                            
         CLI   SVACT,C'A'                                                       
         BE    EDT5A                                                            
         MVI   ERRCD,NOCHGERR                                                   
         FOUT  (R2),AGYPROF+19,1                                                
         OI    4(R2),II1C                                                       
         B     LFMERR                                                           
*                                                                               
EDT5A    LA    R3,AGYTABL                                                       
         MVI   ERRCD,INVERR                                                     
         BAS   RE,TABLOOK2                                                      
         MVC   BYTE2,BYTE                                                       
*                                                                               
         CLI   8(R2),C'0'                                                       
         BNE   EDT5F                                                            
         CLI   SVKEY+1,C'Z'         SPECIAL DDS AGYHEADERS                      
         BE    EDT5F                                                            
         B     LFMERR               ELSE 0 IS INVALID                           
*                                                                               
EDT5F    MVC   AGYPROF+19(1),8(R2)                                              
*                                                                               
EDT6     LA    R2,AGYSACCH                                                      
         CLI   SVACT,C'A'                                                       
         BE    *+12                                                             
         TM    4(R2),II1C                                                       
         BO    EDT7                                                             
         BAS   RE,ANYD                                                          
         MVC   AGYACCT,8(R2)                                                    
*                                                                               
EDT7     EQU   *                                                                
*                                                                               
         CLI   SCRNFLAG,0          DID EVERYTHING FIT ON DISPLAY?               
         BE    EDT7A               YES - SO CONTINUE                            
*                                                                               
         MVC   AGYOPTS,SPACES      ELSE - BLANK OUT THE FIELD                   
         FOUT  AGYOPTSH,=C'OPTIONS CANNOT BE CHANGED - CONTACT DDS',39          
         B     EDT7B               AND SKIP THE OPTIONS                         
*                                                                               
EDT7A    EQU   *                                                                
*                                                                               
         GOTO1 =A(EDOPTS),RR=RELO                                               
         BNE   LFMERR                                                           
*                                                                               
EDT7B    EQU   *                                                                
*                                                                               
         LA    R2,AGYRATSH                                                      
         CLI   SVACT,C'A'                                                       
         BE    *+12                                                             
         TM    4(R2),II1C                                                       
         BO    EDT8                                                             
         LA    R3,RATSTABL                                                      
         BAS   RE,TABLOOK1                                                      
         MVC   AGYPROF(1),8(R2)                                                 
*                                                                               
EDT8     LA    R2,AGYCLRGH                                                      
         CLI   SVACT,C'A'                                                       
         BE    *+12                                                             
         TM    4(R2),II1C                                                       
         BO    EDT9                                                             
         LA    R3,CLRGTABL                                                      
         BAS   RE,TABLOOK1                                                      
         MVC   AGYPROF+1(1),8(R2)                                               
*                                                                               
EDT9     LA    R2,AGYBLPCH                                                      
         CLI   SVACT,C'A'                                                       
         BE    *+12                                                             
         TM    4(R2),II1C                                                       
         BO    EDT10                                                            
         MVI   ERRCD,NOTNUM                                                     
         TM    4(R2),X'08'                                                      
         BZ    LFMERR                                                           
         CLI   5(R2),2                                                          
         BNE   LFMERR                                                           
         MVC   AGYPROF+2(2),8(R2)                                               
*                                                                               
EDT10    LA    R2,AGYEXDMH                                                      
         CLI   SVACT,C'A'                                                       
         BE    *+12                                                             
         TM    4(R2),II1C                                                       
         BO    EDT11                                                            
         LA    R3,EXDMTABL                                                      
         BAS   RE,TABLOOK1                                                      
         MVC   AGYPROF+4(1),8(R2)                                               
*                                                                               
EDT11    LA    R2,AGYBOTOH                                                      
         CLI   SVACT,C'A'                                                       
         BE    *+12                                                             
         TM    4(R2),II1C                                                       
         BO    EDT12                                                            
         LA    R3,BOTOTABL                                                      
         BAS   RE,TABLOOK1                                                      
         MVC   AGYPROF+5(1),8(R2)                                               
*                                                                               
EDT12    LA    R2,AGYCANH                                                       
         CLI   SVACT,C'A'                                                       
         BE    *+12                                                             
         TM    4(R2),II1C                                                       
         BO    EDT13                                                            
         CLI   SVACT,C'A'                                                       
         BE    EDT12A                                                           
         MVI   ERRCD,NOCHGERR                                                   
         CLI   AGYPROF+7,C'C'                                                   
         BNE   EDT12A                                                           
         FOUT  (R2),AGYPROF+7,1                                                 
         OI    4(R2),II1C                                                       
         B     LFMERR                                                           
EDT12A   LA    R3,CANTABL                                                       
         BAS   RE,TABLOOK1                                                      
         MVC   AGYPROF+7(1),8(R2)                                               
         OI    4(R2),II1C                                                       
*                                                                               
EDT13    LA    R2,AGYTMSHH                                                      
         CLI   SVACT,C'A'                                                       
         BE    *+12                                                             
         TM    4(R2),II1C                                                       
         BO    EDT14                                                            
         LA    R3,NWTATABL                                                      
         BAS   RE,TABLOOK1                                                      
         MVC   AGYPROF+8(1),8(R2)                                               
*                                                                               
EDT14    LA    R2,AGYBILLH                                                      
         CLI   SVACT,C'A'                                                       
         BE    *+12                                                             
         TM    4(R2),II1C                                                       
         BO    EDT14E                                                           
         LA    R3,BILLTABL                                                      
         BAS   RE,TABLOOK1                                                      
         MVC   AGYPROF+16(1),8(R2)                                              
EDT14E   DS    0H                                                               
         LA    R2,AGYBIRQH                                                      
         CLI   SVACT,C'A'                                                       
         BE    *+12                                                             
         TM    4(R2),II1C                                                       
         BO    EDT14F                                                           
         LA    R3,BIDTABL                                                       
         BAS   RE,TABLOOK1                                                      
         MVC   AGYPROF+9(1),8(R2)                                               
*                                                                               
EDT14F   MVI   ERRCD,NOTNUM                                                     
         MVI   AGYPROF+10,C'0'                                                  
         LA    R2,AGYCBLH                                                       
         CLI   5(R2),0                                                          
         BE    *+18                                                             
         TM    4(R2),X'08'                                                      
         BZ    LFMERR                                                           
         MVC   AGYPROF+10(1),8(R2)                                              
*                                                                               
EDT14H   DS    0H                                                               
         LA    R2,AGYRPOLH                                                      
         MVI   AGYPROF+11,C'0'                                                  
         CLI   5(R2),0                                                          
         BE    EDT14H6                                                          
         MVI   ERRCD,INVERR                                                     
         CLI   8(R2),C'Y'                                                       
         BE    EDT14H2                                                          
         CLI   8(R2),C'0'                                                       
         BNE   LFMERR                                                           
EDT14H2  MVC   AGYPROF+11(1),8(R2)                                              
*                                                                               
EDT14H6  DS    0H                                                               
         LA    R2,AGYXOTOH                                                      
         MVI   AGYPROF+15,C'N'                                                  
         CLI   5(R2),0                                                          
         BE    EDT14H9                                                          
         MVI   ERRCD,INVERR                                                     
         CLI   8(R2),C' '          ALLOW ANY CHARACTER                          
         BE    LFMERR                                                           
EDT14H8  MVC   AGYPROF+15(1),8(R2)                                              
*                                                                               
EDT14H9  DS    0H                                                               
         LA    R2,AGYMGDMH                                                      
         MVI   AGYPROF+12,C'N'                                                  
         CLI   5(R2),0                                                          
         BE    EDT14HE                                                          
         MVI   ERRCD,INVERR                                                     
         CLI   8(R2),C'Y'                                                       
         BE    EDT14HA                                                          
         CLI   8(R2),C'N'                                                       
         BNE   LFMERR                                                           
EDT14HA  MVC   AGYPROF+12(1),8(R2)                                              
*                                                                               
EDT14HE  DS    0H                  -S AUTH REQUIRED                             
         LA    R2,AGYSAUTH                                                      
         MVI   AGYPROF+6,C'N'                                                   
         CLI   5(R2),0                                                          
         BE    EDT14HG                                                          
         MVI   ERRCD,INVERR                                                     
         CLI   8(R2),C'Y'                                                       
         BE    EDT14HF                                                          
         CLI   8(R2),C'N'                                                       
         BNE   LFMERR                                                           
EDT14HF  MVC   AGYPROF+6(1),8(R2)                                               
*                                                                               
EDT14HG  DS    0H                                                               
         LA    R2,AGYOFRQH                                                      
         MVI   AGYPROF+13,C'N'                                                  
         CLI   5(R2),0                                                          
         BE    EDT14I                                                           
         MVI   ERRCD,INVERR                                                     
         CLI   8(R2),C'Y'                                                       
         BE    EDT14HH                                                          
         CLI   8(R2),C'N'                                                       
         BNE   LFMERR                                                           
EDT14HH  MVC   AGYPROF+13(1),8(R2)                                              
*                                                                               
EDT14I   DS    0H                                                               
         LA    R2,AGYBYBLH                                                      
         MVI   AGYPROF+14,C'N'                                                  
         CLI   5(R2),0                                                          
         BE    EDT14IO                                                          
         MVI   ERRCD,INVERR                                                     
         CLI   8(R2),C'Y'                                                       
         BE    EDT14II                                                          
         CLI   8(R2),C'N'                                                       
         BNE   LFMERR                                                           
EDT14II  MVC   AGYPROF+14(1),8(R2)                                              
*                                                                               
EDT14IO  DS    0H                 ACC OFFICE CODE                               
         LA    R2,AGYACOCH                                                      
         MVI   AGYOFC2,C'N'                                                     
         CLI   8(R2),C'Y'                                                       
         BNE   EDT14J                                                           
         MVI   AGYOFC2,C'Y'                                                     
*                                                                               
EDT14J   DS    0H                                                               
         LA    R2,AGYTTLEH                                                      
         XC    ELEM(30),ELEM                                                    
         MVC   ELEM(2),=X'700C'                                                 
         LA    R6,ELEM                                                          
         USING AGYIDEL,R6                                                       
         CLI   5(R2),0             NO INPUT                                     
         BE    EDT14K                                                           
         MVC   AGYTITLE,AGYTTLE                                                 
         OC    AGYTITLE,SPACES                                                  
         B     EDT14M              GO UPDATE REC                                
*                                                                               
EDT14K   MVI   ERRCD,MSSNGERR                                                   
         CLI   AGYPROF+9,C'Y'                                                   
         BE    LFMERR              ID TITLE REQUIRED                            
         CLI   AGYPROF+9,C'A'                                                   
         BE    LFMERR                                                           
         MVI   ELEM,0              SET FOR NO ELEM TO ADD                       
*                                                                               
EDT14M   DS    0H                                                               
         LA    R6,REC+24           TRY TO FIND OLD ELEM                         
         MVI   ELCODE,X'70'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   EDT14P              NOT FOUND                                    
         CLI   ELEM,0              SEE IF I NEED TO DELETE IT                   
         BE    EDT14N              YES                                          
         MVC   0(12,R6),ELEM       NO -  CAN SWITCH ELEMS                       
         B     EDT14Q                                                           
*                                                                               
EDT14N   DS    0H                                                               
         GOTO1 VRECUP,DMCB,(0,REC),0(R6),0                                      
         B     EDT14Q                                                           
*                                                                               
EDT14P   DS    0H                                                               
         CLI   ELEM,0              SEE IF I NEED TO ADD ELEM                    
         BE    EDT14Q              NO                                           
         GOTO1 VRECUP,DMCB,(0,REC),ELEM,0(R6)                                   
*                                                                               
EDT14Q   DS    0H                                                               
         XC    WORK(2),WORK                                                     
         MVI   ERRCD,INVERR                                                     
         LA    R2,AGYRFPIH         RFP ID                                       
         CLI   5(R2),0                                                          
         BE    EDT14T                                                           
*                                                                               
         MVC   WORK+2(10),8(R2)                                                 
         BRAS  RE,VALID            VALIDATE ID                                  
         BNE   LFMERR                                                           
*                                                                               
         MVC   KEY,SVKEY           RESET KEY                                    
         CLI   SVACT,C'A'                                                       
         BE    EDT14T                                                           
         LA    R1,REC2                                                          
         ST    R1,AREC                                                          
         GOTO1 GETREC              RESTORE GETREC BEFORE PUTREC                 
         LA    R1,REC                                                           
         ST    R1,AREC                                                          
*                                                                               
         USING AGYEXTEL,R6                                                      
EDT14T   LA    R6,REC+24           IS ELEM ALREADY ON RECORD?                   
         MVI   ELCODE,X'71'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   EDT14U              NOT FOUND                                    
         MVC   AGYPRNID,WORK       PRINCIPAL ID FOR RFP                         
         B     EDT14X                                                           
*                                                                               
EDT14U   OC    WORK(2),WORK        DO WE EVEN NEED TO ADD ELEM                  
         BZ    EDT14X              NO                                           
         LR    R1,R6               WHERE TO ADD ELEM                            
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVC   ELEM(2),=X'7120'                                                 
         MVC   AGYPRNID,WORK                                                    
         LR    R6,R1                                                            
         GOTO1 VRECUP,DMCB,(0,REC),ELEM,0(R6)                                   
         DROP  R6                                                               
*                                                                               
EDT14X   DS    0H                                                               
         LA    R1,REC                                                           
         MVC   HALF,AGYLEN                                                      
         LH    R2,HALF                                                          
         AR    R1,R2                                                            
         MVI   0(R1),0             ZERO END OF RECORD                           
*                                                                               
EDTACC   DS    0H                                                               
         LA    R2,AGYACCH                                                       
         XC    ELEM(30),ELEM                                                    
         MVC   ELEM(2),=X'031A'                                                 
         LA    R6,ELEM                                                          
         USING AGYACCEL,R6                                                      
         CLI   5(R2),0             NO INPUT                                     
         BE    EDTACC1                                                          
         LA    R3,AGYACC                                                        
         LA    R4,AGYACCAG                                                      
EDTACCLP CLI   0(R3),C' '                                                       
         BNH   EDTACC2             GO UPDATE RECORD                             
         MVC   0(2,R4),0(R3)                                                    
         LA    R3,3(R3)                                                         
         LA    R4,2(R4)                                                         
         B     EDTACCLP                                                         
*                                                                               
EDTACC1  MVI   ERRCD,MSSNGERR                                                   
         MVI   ELEM,0              SET FOR NO ELEM TO ADD                       
*                                                                               
EDTACC2  DS    0H                                                               
         LA    R6,REC+24           TRY TO FIND OLD ELEM                         
         MVI   ELCODE,X'03'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   EDTACC4             NOT FOUND                                    
         CLI   ELEM,0              SEE IF I NEED TO DELETE IT                   
         BE    EDTACC3             YES                                          
         MVC   0(26,R6),ELEM       NO -  CAN SWITCH ELEMS                       
         B     EDTACCX                                                          
*                                                                               
EDTACC3  DS    0H                                                               
         GOTO1 VRECUP,DMCB,(0,REC),0(R6),0                                      
         B     EDTACCX                                                          
*                                                                               
EDTACC4  DS    0H                                                               
         CLI   ELEM,0              SEE IF I NEED TO ADD ELEM                    
         BE    EDTACCX             NO                                           
         LA    R6,REC+24           GET ADDRESS OF WHERE TO PUT 03               
         MVI   ELCODE,X'02'                                                     
         BAS   RE,NEXTEL                                                        
EDTACC4A CLI   0(R6),X'02'                                                      
         BNE   EDTACC5                                                          
         ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     EDTACC4A                                                         
EDTACC5  GOTO1 VRECUP,DMCB,(0,REC),ELEM,0(R6)                                   
*                                                                               
EDTACCX  DS    0H                                                               
*                                                                               
         USING AGYMEDEL,R6                                                      
EDT15    LA    R6,REC+24                                                        
         LA    R2,AGYMEDH                                                       
         MVI   ELCODE,X'02'                                                     
         XC    HALF,HALF                                                        
EDT15A   CLI   5(R2),0                                                          
         BE    ENDCHK              NO ENTRY FOR THIS SLOT                       
         CLI   SVACT,C'A'                                                       
         BE    EDT15A5                                                          
         TM    4(R2),II1C                                                       
         BO    EDT15C              EXISTING MEDIA                               
EDT15A5  BAS   RE,NEXTEL                                                        
         BE    EDT15B              EXISTING MEDIA                               
         LR    R7,R6               SAVE LAST PTR                                
         LA    R6,ELEM                                                          
         XC    ELEM(34),ELEM                                                    
         LA    R3,MEDTABL                                                       
         MVC   AGYMEDEL(2),=X'0222'                                             
         MVI   ERRCD,INVERR                                                     
         BAS   RE,TABLOOK2         CHECK MEDIA CODE                             
         MVC   AGYMEDBT,BYTE       CREATE AGY/MEDIA CODE                        
         OC    AGYMEDBT,BYTE2                                                   
         MVC   AGYMEDCD,8(R2)                                                   
         BAS   R9,EDTMD            EDIT MEDIA DESC                              
         BAS   R9,EDTVD            DO MED TYPE AND VENDOR                       
         LR    R6,R7               RESTORE SPACE                                
         GOTO1 VRECUP,DMCB,(0,REC),ELEM,0(R6)                                   
         MVI   AGYMEDEL+34,X'00'                                                
         B     EDT18                                                            
*                                                                               
EDT15B   MVI   ERRCD,NOCHGERR                                                   
         FOUT  (R2),AGYMEDCD,1                                                  
         OI    4(R2),II1C                                                       
         B     LFMERR                                                           
*                                                                               
EDT15C   BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                WE HAD THIS MEDIA TO PUT OUT                 
        BAS   R9,EDTMD             EDIT MEDIA DESC                              
        BAS   R9,EDTVD                                                          
*                                                                               
EDT18    ZIC   R3,HALF                                                          
         LA    R3,1(R3)            COUNT MEDIA                                  
         STC   R3,HALF                                                          
         CLI   HALF,5             LAST MEDIA - ARE WE DONE                      
         BE    OUTPUT                                                           
         ZIC   R3,0(R2)                                                         
         AR    R2,R3               ADVANCE SCREEN PTR                           
         B     EDT15A                                                           
*                                                                               
         EJECT                                                                  
ENDCHK   MVI   ERRCD,MSSNGERR                                                   
         CLI   HALF,1             MUST BE AT LEAST 1                            
         BL    LFMERR                                                           
         LR    R5,R2                                                            
ENDCHK2  ZIC   R3,0(R5)            ADVANCE SCREEN                               
         AR    R5,R3                                                            
         CLI   5(R5),0             ANYTHING THERE                               
         BNE   LFMERR                                                           
         ZIC   R3,0(R5)            CHECK VENDOR SLOT                            
         AR    R5,R3                                                            
         CLI   5(R5),0                                                          
         BNE   LFMERR                                                           
         ZIC   R3,HALF                                                          
         LA    R3,1(R3)            GET NEXT MEDIA LINE                          
         STC   R3,HALF                                                          
         CLI   HALF,5             FINISHED                                      
         BE    OUTPUT                                                           
         ZIC   R3,0(R5)                                                         
         AR    R5,R3                                                            
         CLI   5(R5),0             IS ANYTHING THERE                            
         BNE   LFMERR                                                           
         B     ENDCHK2             LOOP TILL ALL MEDIA CHKED                    
         EJECT                                                                  
OUTPUT   MVC   KEY,SVKEY                                                        
         ST    R8,AREC                                                          
         CLI   SVACT,C'A'                                                       
         BNE   OUT1                                                             
         MVC   REC(13),SVKEY                                                    
*                                                                               
         GOTO1 ADDREC                                                           
         B     EXXMOD                                                           
*                                                                               
OUT1     GOTO1 PUTREC                                                           
         B     EXXMOD                                                           
*                                                                               
LFMERR   GOTO1 ERROR                                                            
         EJECT                                                                  
EDTMD    ZIC   R3,0(R2)            MEDIA DESC.                                  
         AR    R2,R3                                                            
         CLI   SVACT,C'A'                                                       
         BE    *+12                                                             
         TM    4(R2),II1C                                                       
         BO    EDTMD2                                                           
         BAS   RE,ANYD                                                          
         MVC   AGYMEDEX,8(R2)                                                   
EDTMD2   BR    R9                                                               
         SPACE 2                                                                
EDTVD    ZIC   R3,0(R2)            VENDOR DESC.                                 
         AR    R2,R3                                                            
         CLI   SVACT,C'A'                                                       
         BE    *+12                                                             
         TM    4(R2),II1C                                                       
         BO    EDTVD2                                                           
         BAS   RE,ANYD                                                          
         MVC   AGYVENEX,8(R2)                                                   
EDTVD2   BR    R9                                                               
         SPACE 2                                                                
NEXTEL   CLI   0(R6),0             END                                          
         BE    NEXTELX                                                          
         ZIC   R0,1(R6)                                                         
         LTR   R0,R0                                                            
         BNZ   *+6                 SAFETY CATCH                                 
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLC   ELCODE,0(R6)        RIGHT ELEMENT                                
         BER   RE                                                               
         B     NEXTEL                                                           
NEXTELX  LTR   RE,RE                                                            
         BR    RE                  NOT FOUND EXIT                               
         SPACE 2                                                                
ANYD     MVI   ERRCD,MSSNGERR                                                   
         CLI   5(R2),0                                                          
         BE    LFMERR                                                           
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
* THIS ROUTINE CHECKS TO MAKE SURE THE CURRENT OPTION WILL FIT IN THE           
* OPTIONS FIELD FOR DISPLAY.                                                    
*                                                                               
* I/P:   R0 = L(OF CURRENT OPTION LITERAL)                                      
*        R4 = A(CURRENT POSITION IN OPTIONS FIELD)                              
*                                                                               
* NOTE: THIS ROUTINE DOESN'T SAVE/RESTORE REGISTERS -- BE CAREFUL!!             
*                                                                               
CHECKOPT EQU   *                                                                
*                                                                               
         AR    R0,R4               A(NEW FIELD POSITION)                        
         LA    R3,AGYOPTS          A(SCREEN FIELD)                              
         LA    R3,L'AGYOPTS-1(R3)  A(LAST POSTITION IN FIELD)                   
         CR    R0,R3               WILL CURRENT OPTION FIT IN FIELD?            
         BH    CKOPERR             NO - SO ERROR                                
*                                                                               
         XR    R0,R0               SET GOOD CC                                  
*                                                                               
CKOPEXIT EQU   *                                                                
*                                                                               
         BR    RE                  RETURN TO CALLER                             
*                                                                               
CKOPERR  EQU   *                                                                
*                                                                               
         ZIC   R3,SCRNFLAG         SAVE OLD COUNT OF MISSED OPTS                
         LA    R3,1(R3)            INC COUNT                                    
         STC   R3,SCRNFLAG         STORE IT                                     
         LTR   RC,RC               SET ERROR CC                                 
         B     CKOPEXIT            AND RETURN                                   
         EJECT                                                                  
TABLOOK1 DS    0H                                                               
         MVI   ERRCD,INVERR                                                     
TAB1A    CLI   0(R3),0             TABLE END                                    
         BE    LFMERR                                                           
         CLC   0(1,R3),8(R2)                                                    
         BER   RE                                                               
         LA    R3,1(R3)                                                         
         B     TAB1A                                                            
*                                                                               
RATSTABL DC    C'012',X'00'        RATING SERVICE                               
CLRGTABL DC    C'0RC',X'00'        CLIENT REGIONS                               
EXDMTABL DC    C'0E',X'00'         EXTENDED DEMOS                               
BOTOTABL DC    C'01',X'00'         BUY PERIOD OTO'S                             
CANTABL  DC    C'0C',X'00'         CANADIAN SPOTPAK                             
NWTATABL DC    C'0Y',X'00'         OLD POOL TIMESHEETS                          
BILLTABL DC    C'0123',X'00'       BILLING                                      
BIDTABL  DC    C'NAY',X'00'        BUY ID REQUIRED                              
         SPACE 2                                                                
TABLOOK2 DS    0H                  CALLER SETS ERROR CODE                       
TAB2A    CLI   0(R3),0                                                          
         BE    LFMERR              TABLE END                                    
         CLC   0(1,R3),8(R2)                                                    
         BE    TAB2B                                                            
         LA    R3,2(R3)                                                         
         B     TAB2A                                                            
TAB2B    MVC   BYTE,1(R3)          RETURN HEX CODE                              
         BR    RE                                                               
*                                                                               
*                                                                               
* NOTE F0 ONLY FOR AGENCIES THAT START WITH 'Z' - DDS SPECIALS                  
*                                                                               
AGYTABL  DC    X'F010F110F220F330F440F550F660F770F880F990C1A0C2B0C3C0C4X        
               D0C5E0C6F00000'                                                  
MEDTABL  DC    X'E301D902D503E704C3080000'                                      
RELO     DC    F'0'                                                             
         EJECT                                                                  
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*=========================================================*                     
* VALIDATE ID IN WORK+2(10) AND RETURN ID NUMBER IN WORK(2)                     
*=========================================================*                     
         SPACE 1                                                                
VALID    NTR1  BASE=*,LABEL=*                                                   
         XC    ELEM,ELEM           VALIDATE ID AND GET ID NUMBER                
         LA    R3,ELEM                                                          
         USING CTIREC,R3                                                        
         MVI   CTIKTYP,CTIKTYPQ    C'I'                                         
         MVC   CTIKID,WORK+2       ID                                           
         OC    CTIKID,SPACES                                                    
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',ELEM,REC2                    
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   ELEM(L'CTIKEY),REC2                                              
         BNE   VIDERR                                                           
         LA    R3,REC2                                                          
         LA    R3,CTIDATA          FIRST ELEM                                   
         DROP  R3                                                               
*                                                                               
VID10    CLI   0(R3),0                                                          
         BE    VIDERR              CAN'T FIND ID NUMBER                         
         CLI   0(R3),X'02'                                                      
         BE    VID20                                                            
         ZIC   R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     VID10                                                            
VID20    MVC   WORK(2),2(R3)       SAVE ID NUMBER                               
         SR    RC,RC                                                            
VIDERR   LTR   RC,RC                                                            
         XIT1                                                                   
         LTORG                                                                  
*=========================================================*                     
* DISPLAY PRINCIPAL ID FOR RFP FROM ID NUMBER IN WORK(2)  *                     
*=========================================================*                     
         SPACE 1                                                                
DISPID   NTR1  BASE=*,LABEL=*                                                   
         XC    ELEM,ELEM           GET ID NAME FROM NUMBER                      
         LA    R3,ELEM                                                          
         USING CTIREC,R3                                                        
         MVI   CTIKTYP,CTIKTYPQ    C'I'                                         
         MVC   CTIKNUM,WORK                                                     
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',ELEM,REC2                    
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   ELEM(L'CTIKEY),REC2                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY,SVKEY                                                        
         LA    R3,REC2                                                          
         LA    R3,CTIDATA          FIRST ELEM                                   
         DROP  R3                                                               
*                                                                               
DID10    CLI   0(R3),0                                                          
         BNE   *+6                 CAN'T FIND ID NAME                           
         DC    H'0'                                                             
         CLI   0(R3),X'02'                                                      
         BE    DID20                                                            
         ZIC   R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     DID10                                                            
DID20    MVC   AGYRFPI,2(R3)       DISPLAY ID                                   
         XIT1                                                                   
         LTORG                                                                  
*=========================================================*                     
* EDIT OPTIONS FIELD                                      *                     
*=========================================================*                     
         SPACE 1                                                                
         DS    0H                                                               
EDOPTS   NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'*EDOPTS*'                                                    
*                                                                               
         MVI   BYTE,0                                                           
         MVI   AGYFLAG1,0                                                       
         MVI   AGYFLAG2,0                                                       
         XC    AGYCTAGY,AGYCTAGY                                                
         LA    R2,AGYOPTSH                                                      
         CLI   5(R2),0                                                          
         BE    EDOPT100                                                         
         GOTO1 VSCANNER,DMCB,(R2),(12,REC2)                                     
         MVI   ERRCD,INVERR                                                     
         CLI   DMCB+4,0                                                         
         BE    NO                                                               
*                                                                               
         LA    R5,REC2-32                                                       
         USING SCAND,R5                                                         
         SR    R6,R6               CLEAR COUNTER                                
*                                                                               
EDOPT2   LA    R5,32(R5)           NEXT OPTION                                  
         CLI   0(R5),0             TEST NO MORE                                 
         BE    EDOPT100                                                         
         LA    R6,1(R6)            BUMP OPTION COUNTER                          
         CLI   FLD1LEN,2                                                        
         BL    EDOPTERR                                                         
         ZIC   RE,FLD1LEN                                                       
         BCTR  RE,0                                                             
         EX    RE,TESTADDS                                                      
         BE    EDOPT10                                                          
         EX    RE,TESTMEDN                                                      
         BE    EDOPT20                                                          
         EX    RE,TESTCTA                                                       
         BE    EDOPT30                                                          
         EX    RE,TESTCTF                                                       
         BE    EDOPT40                                                          
         EX    RE,TESTLOCK                                                      
         BE    EDOPT50                                                          
         EX    RE,TESTOFF                                                       
         BE    EDOPT60                                                          
         EX    RE,TESTTEST                                                      
         BE    EDOPT70                                                          
         EX    RE,TESTDDSB                                                      
         BE    EDOPT75                                                          
*                                                                               
         EX    RE,TESTTRAD         TRADE AGENCY?                                
         BE    EDOPT80             YES - SO GO PROCESS IT                       
*                                                                               
         EX    RE,TESTCOS2         COST FACTOR REQUIRED?                        
         BE    EDOPT82             YES - SO GO PROCESS IT                       
*                                                                               
         EX    RE,TESTPW                                                        
         BE    EDOPT84                                                          
*                                                                               
         EX    RE,TESTDIY          DIY TRADE AGENCY?                            
         BE    EDOPT86             YES - SO GO PROCESS IT                       
*                                                                               
EDOPTERR MVC   LFMMSG(31),=C'** ERROR ** OPTION XX NOT VALID'                   
         CVD   R6,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LFMMSG+19(2),DUB                                                 
         FOUT  LFMMSGH                                                          
         MVI   ERRAREA,X'FF'                                                    
         GOTO1 ERROR                                                            
*                                                                               
TESTADDS CLC   FLD1(0),=C'ADDS'                                                 
TESTMEDN CLC   FLD1(0),=C'MEDNAME'                                              
TESTCTA  CLC   FLD1(0),=C'CTA'                                                  
TESTCTF  CLC   FLD1(0),=C'CTFILE'                                               
TESTLOCK CLC   FLD1(0),=C'LOCK'                                                 
TESTOFF  CLC   FLD1(0),=C'OFF'                                                  
TESTTEST CLC   FLD1(0),=C'TEST'                                                 
TESTDDSB CLC   FLD1(0),=C'DDSB'                                                 
TESTTRAD CLC   FLD1(0),=C'TRD'     TRADE AGENCY?                                
TESTCOS2 CLC   FLD1(0),=C'COS2'    COST FACTOR REQUIRED?                        
TESTPW   CLC   FLD1(0),=C'PW'                                                   
TESTDIY  CLC   FLD1(0),=C'DIY'                                                  
*                                                                               
EDOPT10  CLI   FLD2LEN,1                                                        
         BL    EDOPTERR                                                         
         BAS   RE,TESTYES          DARE=Y/N                                     
         BNE   *+12                                                             
         OI    AGYFLAG1,X'80'                                                   
         B     EDOPT2                                                           
         BAS   RE,TESTNO                                                        
         BE    EDOPT2                                                           
         B     EDOPTERR                                                         
*                                                                               
EDOPT20  DS    0H                  MEDNAME=Y/N                                  
         BAS   RE,TESTYES                                                       
         BNE   *+12                                                             
         OI    AGYFLAG1,X'40'                                                   
         B     EDOPT2                                                           
         BAS   RE,TESTNO                                                        
         BE    EDOPT2                                                           
         B     EDOPTERR                                                         
*                                                                               
EDOPT30  DS    0H                  CTA=Y/N                                      
         BAS   RE,TESTYES                                                       
         BNE   *+12                                                             
         OI    AGYFLAG1,X'20'                                                   
         B     EDOPT2                                                           
         BAS   RE,TESTNO                                                        
         BE    EDOPT2                                                           
         B     EDOPTERR                                                         
*                                                                               
EDOPT40  DS    0H                  CTFILE=XX                                    
         CLI   FLD2LEN,2                                                        
         BNE   EDOPTERR                                                         
         MVC   AGYCTAGY,FLD2                                                    
         B     EDOPT2                                                           
*                                                                               
EDOPT50  DS    0H                  LOCK=DATE                                    
         OI    BYTE,X'80'                                                       
         GOTO1 VDATVAL,DMCB,(0,FLD2),(0,WORK)                                   
         GOTO1 VGETBRD,DMCB,(1,WORK),WORK+6,VGETDAY,VADDAY                      
         CLI   0(R1),X'FF'                                                      
         BE    EDOPTERR                                                         
         GOTO1 VDATCON,DMCB,(0,WORK+12),(3,WORK)                                
         LR    R4,R6                                                            
         USING AGYEXTEL,R6                                                      
         LA    R6,ELEM                                                          
         XC    ELEM(32),ELEM                                                    
         MVC   ELEM(2),=X'7120'                                                 
         MVC   AGYLOCK,WORK                                                     
         LA    R6,REC+24                                                        
         MVI   ELCODE,X'71'                                                     
         BAS   RE,NEXTEL2                                                       
         BE    EDOPT55                                                          
         GOTO1 VRECUP,DMCB,(0,REC),ELEM,0(R6)                                   
         B     EDOPT57                                                          
EDOPT55  MVC   AGYLOCK,WORK                                                     
EDOPT57  LR    R6,R4                                                            
         DROP  R6                                                               
         B     EDOPT2                                                           
*                                                                               
EDOPT60  DS    0H                  OFF=HEX                                      
         CLC   =C'HEX',FLD2                                                     
         BNE   EDOPTERR                                                         
         OI    AGYFLAG1,X'10'                                                   
         B     EDOPT2                                                           
*                                                                               
EDOPT70  BAS   RE,TESTYES          TEST=Y/N                                     
         BNE   *+12                                                             
         OI    AGYFLAG1,AGYTESTQ   X'08'                                        
         B     EDOPT2                                                           
         BAS   RE,TESTNO                                                        
         BE    EDOPT2                                                           
         B     EDOPTERR                                                         
*                                                                               
EDOPT75  CLI   FLD2LEN,3                                                        
         BNE   EDOPTERR                                                         
         CLC   =C'CLT',FLD2                                                     
         BE    EDOPT2                                                           
         CLC   =C'PRD',FLD2                                                     
         BNE   EDOPTERR                                                         
         OI    AGYFLAG1,AGYPRDQ                                                 
         B     EDOPT2                                                           
*                                                                               
EDOPT80  EQU   *                                                                
*                                                                               
         OI    AGYFLAG1,AGYTRDQ    SET TRADE BIT                                
         B     EDOPT2              AND LOOP BACK                                
*                                                                               
EDOPT82  EQU   *                                                                
         OI    AGYFLAG1,AGYCOS2Q   SET COST FACTOR BIT                          
         B     EDOPT2              AND LOOP BACK                                
*                                                                               
EDOPT84  EQU   *                                                                
         OI    AGYFLAG2,AGYFLAG2_PW   SET PW AGENCY FLAG                        
         B     EDOPT2                 AND LOOP BACK                             
*                                                                               
EDOPT86  EQU   *                                                                
***NOP   OI    AGYFLAG2,AGYFLAG2_DIY  NOT AT AGY LEVEL ANY MORE!!!              
         B     EDOPT2                 AND LOOP BACK                             
*                                                                               
EDOPT100 DS    0H                                                               
         TM    BYTE,X'80'                                                       
         BO    EDOPTX                                                           
         USING AGYEXTEL,R6                                                      
         LA    R6,REC+24                                                        
         MVI   ELCODE,X'71'                                                     
         BAS   RE,NEXTEL2                                                       
         BNE   EDOPTX                                                           
         XC    AGYLOCK,AGYLOCK                                                  
         B     EDOPTX                                                           
         DROP  R6                                                               
*                                                                               
TESTYES  CLI   FLD2LEN,3                                                        
         BH    EDOPTERR                                                         
         ZIC   RF,FLD2LEN                                                       
         BCTR  RF,0                                                             
         EX    RF,CLCYES                                                        
         BR    RE                                                               
CLCYES   CLC   FLD2(0),=C'YES'                                                  
*                                                                               
TESTNO   CLI   FLD2LEN,2                                                        
         BH    EDOPTERR                                                         
         ZIC   RF,FLD2LEN                                                       
         BCTR  RF,0                                                             
         EX    RF,CLCNO                                                         
         BR    RE                                                               
CLCNO    CLC   FLD2(0),=C'NO'                                                   
*                                                                               
EDOPTX   DS    0H                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
         XIT1                                                                   
         EJECT                                                                  
*=========================================================*                     
* NEXTEL                                                  *                     
*=========================================================*                     
*                                                                               
NEXTEL2  CLI   0(R6),0             END                                          
         BE    NEXTL2NX                                                         
         ZIC   R0,1(R6)                                                         
         LTR   R0,R0                                                            
         BNZ   *+6                 SAFETY CATCH                                 
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLC   ELCODE,0(R6)        RIGHT ELEMENT                                
         BNE   NEXTEL2                                                          
*                                                                               
NEXTL2EX CR    RE,RE                                                            
         BR    RE                                                               
*                                                                               
NEXTL2NX LTR   RE,RE               NOT FOUND EXIT                               
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
SCAND    DSECT                                                                  
*         DSECT TO COVER SCANNER LINES                                          
FLD1LEN  DS    CL1                                                              
FLD2LEN  DS    CL1                                                              
FLD1VAL  DS    CL1                                                              
FLD2VAL  DS    CL1                                                              
FLD1B    DS    CL4                                                              
FLD2B    DS    CL4                                                              
FLD1     DS    CL10                                                             
FLD2     DS    CL10                                                             
*                                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
       ++INCLUDE SPLFMWRK                                                       
         EJECT                                                                  
         ORG   LFMTABH                                                          
       ++INCLUDE SPLFMD2D                                                       
         SPACE 2                                                                
         ORG   SVAPPL                                                           
SCRNFLAG DS    X                   'DID ALL OPTIONS FIT ON SCREEN' FLAG         
         EJECT                                                                  
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
*                                                                               
       ++INCLUDE CTGENFILE                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022SPLFM32   12/18/09'                                      
         END                                                                    
