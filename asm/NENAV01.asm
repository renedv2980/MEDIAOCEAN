*          DATA SET NENAV01    AT LEVEL 159 AS OF 01/27/21                      
*PROCESS USING(WARN(15))                                                        
*PHASE T31801B                                                                  
*                                                                               
*===============================================================*               
* SOME BASICS --                                                *               
* SVREASON IS 0 ON THE FIRST PASS, NON-ZERO SUBSEQUENTLY        *               
* SVRCVEL IS NON-ZERO ON FIRST PASS ONLY                        *               
* SVOLDRCV IS SAVE AREA FOR SVRCVEL BETWEEN PASSES              *               
*===============================================================*               
*                                                                               
T31801   TITLE 'NENAV01 - STEWARD/MATCHMAKER - INIT OVERLAY'                    
T31801   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,NENV01**,R8,RR=R2                                              
         LA    R7,2048(R8)                                                      
         LA    R7,2048(R7)                                                      
         USING T31801+8192,R7                                                   
*                                                                               
         LR    RC,R1                                                            
         USING WORKD,RC                                                         
*                                                                               
         ST    R2,MYRELO                                                        
         L     R0,=A(PFXTAB)                                                    
         A     R0,MYRELO                                                        
         ST    R0,APFXTAB                                                       
*                                                                               
         L     RA,ATWA                                                          
         USING TWAD,RA                                                          
*                                                                               
         BRAS  RE,GTALPHID          GET USER ID ALPHA                           
*                                                                               
         GOTO1 VALIMED                                                          
* ROUTE THE ELEMENTS TO THE RIGHT ROUTINES                                      
         CLC   SVRCVEL,=X'0010'                                                 
         BE    ROUT020                                                          
         CLC   SVRCVEL,=X'0059'                                                 
         BE    ROUT040                                                          
         CLC   SVRCVEL,=X'0080'                                                 
         BE    ROUT060                                                          
         CLC   SVRCVEL,=X'0040'                                                 
         BE    ROUT220                                                          
         CLC   SVRCVEL,=X'0044'                                                 
         BE    ROUT320                                                          
         CLC   SVRCVEL,=X'005C'                                                 
         BE    ROUT380                                                          
         CLC   SVRCVEL,=X'0060'       WIZARD UNIT DATA                          
         BE    ROUT420                                                          
         CLC   SVRCVEL,=X'0056'                                                 
         BE    ROUT480                                                          
         CLC   SVRCVEL,=X'0142'                                                 
         BE    ROUT540                                                          
         CLC   SVRCVEL,=X'0144'                                                 
         BE    ROUT550                                                          
         CLC   SVRCVEL,=X'0145'                                                 
         BE    ROUT560                                                          
         SPACE 3                                                                
*                                                                               
*  INITIAL DOWNLOAD ROUTINES STEWARD                                            
*                                                                               
*--SEND DEMO, NAD PREFIX, DAY, LENGTH INFO                                      
ROUT020  BAS   RE,PASDEF                                                        
*--SEND DAYPART INFO                                                            
         BAS   RE,PASDYPT                                                       
*--SEND REASON CODE INFO                                                        
         BAS   RE,PASRSN                                                        
*--SEND NETWORK, MEDIA TYPE                                                     
         BAS   RE,PASNTWK                                                       
*--SEND SERVER INFORMATION                                                      
         BAS   RE,PASSRVR                                                       
*--INITIALIZE SECRET                                                            
         BAS   RE,BLDSECRT                                                      
*--SEND SECURITY INFORMATION                                                    
         CLC   VERSION,=X'02000000'    VERSION 2.00.00.00 AND HIGHER            
         BL    *+8                                                              
         BAS   RE,PASSEC                                                        
*--SEND COMSCORE WEB SERVICE SECURITY                                           
         BAS   RE,PASCSSEC                                                      
*--SEND COMSCORE STREAMS/VIEWING TYPE                                           
         BAS   RE,PASCSVT                                                       
*--SEND COMSCORE SOURCES                                                        
         BAS   RE,PASCSS                                                        
*--SEND UNIVERSE CUT OFF DATES                                                  
         BAS   RE,PASUCOD                                                       
*--SEND NETWORK/MARKET INFO                                                     
         BRAS  RE,PASMKT                                                        
*--SEND PROGRAM INFO (CPROG)                                                    
         BRAS  RE,PGMINFO                                                       
         B     EXIT                                                             
         SPACE 3                                                                
*                                                                               
*  INITIAL DOWNLOAD ROUTINES (CABLE MATCHMAKERS)                                
*                                                                               
*--SEND REASON CODES                                                            
ROUT040  GOTO1 VALIMED                                                          
*--SEND MAXIMUM UNIT TRANSFER NUMBER                                            
         CLC   VERSION,=X'01030000'    VERSION 1.03.00.00 AND HIGHER            
         BL    *+8                                                              
         BAS   RE,PASDEF                                                        
*--SEND DAYPART INFO                                                            
         BAS   RE,PASDYPT                                                       
*--SEND REASON CODE INFO                                                        
         BAS   RE,PASRSN                                                        
*--SEND NETWORK, MEDIA TYPE                                                     
         BAS   RE,PASNTWK                                                       
*--SEND AGENCY STATUS                                                           
         BAS   RE,PASAGYST                                                      
*--INITIALIZE SECRET                                                            
         BAS   RE,BLDSECRT                                                      
         B     EXIT                                                             
         SPACE 3                                                                
*                                                                               
*  INITIAL DOWNLOAD ROUTINES (FRONTRUNNER)                                      
*                                                                               
*--INITIALIZE SECRET                                                            
ROUT060  BAS   RE,BLDSECRT                                                      
*--SEND DEMO, NAD PREFIX, DAY, LENGTH INFO                                      
         BAS   RE,PASDEF                                                        
*--SEND DAYPART INFO                                                            
         BAS   RE,PASDYPT                                                       
*--SEND REASON CODE INFO                                                        
         BAS   RE,PASRSN                                                        
*--SEND NETWORK, MEDIA TYPE                                                     
         BAS   RE,PASNTWK                                                       
*--SEND SECURITY INFORMATION                                                    
         BAS   RE,PASSEC                                                        
*--SEND COMSCORE WEB SERVICE SECURITY                                           
         BAS   RE,PASCSSEC                                                      
*--SEND COMSCORE STREAMS/VIEWING TYPE                                           
         BAS   RE,PASCSVT                                                       
*--SEND COMSCORE SOURCES                                                        
         BAS   RE,PASCSS                                                        
*--SEND UNIVERSE CUT OFF DATES                                                  
         BAS   RE,PASUCOD                                                       
         B     EXIT                                                             
         SPACE 3                                                                
*                                                                               
*  PROGRAM SEARCH ROUTINES                                                      
*                                                                               
*--GET AGENCY PROFILES                                                          
ROUT220  BAS   RE,PRGSERCH                                                      
         B     EXIT                                                             
         SPACE 3                                                                
*                                                                               
*  PROGRAM DETAILS ROUTINES                                                     
*                                                                               
*--GET AGENCY PROFILES                                                          
ROUT320  BAS   RE,PRGDET                                                        
         B     EXIT                                                             
         SPACE 3                                                                
*                                                                               
*  PACKAGE DETAILS ROUTINES                                                     
*                                                                               
*--GET AGENCY PROFILES                                                          
ROUT380  BAS   RE,PACKDET                                                       
         BAS   RE,VALREP                                                        
         B     EXIT                                                             
         SPACE 3                                                                
*--GET WIZARD UNIT DATA                                                         
*ROUT420  GOTO1 VALIMED                                                         
ROUT420  GOTO1 VALIMED                                                          
         BAS   RE,WIZDATA                                                       
         B     EXIT                                                             
         SPACE 3                                                                
*                                                                               
*  IGNORE FEATURES FOR INVOICE                                                  
*                                                                               
ROUT480  BRAS  RE,PROCINV                                                       
         B     EXIT                                                             
         SPACE 3                                                                
*                                                                               
*  DOWNLOAD BARRULE RECORDS                                                     
*                                                                               
ROUT540  BRAS  RE,DWNBARR                                                       
         B     EXIT                                                             
         SPACE 3                                                                
*                                                                               
*  DOWNLOAD LIMIT RECORDS                                                       
*                                                                               
ROUT550  BRAS  RE,DWNLIMIT                                                      
         B     EXIT                                                             
*                                                                               
*  DOWNLOAD PROGRAM INFO (CPROG)                                                
*                                                                               
ROUT560  BRAS  RE,PGMINFO                                                       
         B     EXIT                                                             
         SPACE 3                                                                
EXITN    LHI   RE,1                                                             
         J     EXITCC                                                           
EXITY    SR    RE,RE                                                            
EXITCC   LTR   RE,RE                                                            
EXIT     XIT1                                                                   
         EJECT                                                                  
*  ROUTINE SETS UP SECRET BLOCK FOR FUTURE SECURITY CALLS                       
BLDSECRT NTR1                                                                   
*                                                                               
* INITIALIZE SECRET                                                             
*                                                                               
         L     RE,ATWA                                                          
         AH    RE,=Y(SVSECRET-TWAD)                                             
         ST    RE,ASECBLK                                                       
*                                                                               
         SPACE 1                                                                
         OC    TWASAGN,TWASAGN          ON NEW SECURITY                         
         BNZ   *+14                                                             
         OC    TWAACCS(2),TWAACCS       OR HAVE LIMIT ACCESS                    
         BZ    BLDSECEX                                                         
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSECRET-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,('SECPINIT',ASECBLK),0                                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
BLDSECEX B     EXIT                                                             
         EJECT                                                                  
*  ROUTINE BUILDS DEFAULT INFORMATION FOR STEWARD                               
*  IT PASSES NAD PREFIXES, DEMO AGE BREAKS, DAY, LENGTHS                        
PASDEF   NTR1                                                                   
         CLC   SVRCVEL,=X'0059'     CHECK FOR MATCHMAKER                        
         BE    PASDF40                                                          
*                                                                               
         LHI   R1,X'16'                                                         
         BAS   RE,SENDH                                                         
         CLC   SVRCVEL,=X'0059'     CHECK FOR MATCHMAKER                        
         BE    PASDF170                                                         
*                                                                               
*  PASS NAD PREFIXES                                                            
*                                                                               
         L     R4,APFXTAB                                                       
         LA    R3,PREFIXES                                                      
*                                                                               
PASDF20  LHI   R1,X'01'                                                         
         BAS   RE,SENDD                                                         
         LA    R4,1(R4)                                                         
         LHI   R1,X'02'                                                         
         BAS   RE,SENDD                                                         
         LA    R4,7(R4)                                                         
         BCT   R3,PASDF20                                                       
         SPACE 2                                                                
*                                                                               
*  PASS DEMO AGE BREAKS                                                         
*                                                                               
PASDF40  LHI   R1,X'12'                                                         
         BAS   RE,SENDH                                                         
         CLC   SVRCVEL,=X'0059'     CHECK FOR MATCHMAKER                        
         BE    PASDF170                                                         
*                                                                               
         L     RE,AIO3                                                          
         LA    RF,1                                                             
         LA    R2,255                                                           
*                                                                               
PASDF60  MVC   0(2,RE),=X'00C8'                                                 
         STCM  RF,1,2(RE)                                                       
*                                                                               
* DEMOBAD IS A TABLE THAT HAS ALL THE CATEGORIES                                
* THAT SHOULD NOT BE PASSED TO TH PC                                            
*                                                                               
         L     R3,=A(DEMOBAD)                                                   
         A     R3,MYRELO                                                        
         LA    R4,DMBNUM                                                        
PASDF70  CLC   2(1,RE),0(R3)        CHECK TO BYPASS THIS CATEGORY               
         BE    PASDF80                                                          
         LA    R3,1(R3)                                                         
         BCT   R4,PASDF70                                                       
*                                                                               
         LA    RE,3(RE)                                                         
PASDF80  LA    RF,1(RF)                                                         
         BCT   R2,PASDF60                                                       
*                                                                               
         LA    R4,BLOCK            INIT  DBLOCK                                 
         USING DBLOCKD,R4                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'NAD'                                                   
         MVI   DBSELMED,C'N'                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
*                                                                               
         XC    WORK,WORK                                                        
         L     R2,AIO3                                                          
         LA    R3,1000(R2)                                                      
         LA    R2,3(R2)                                                         
         MVC   0(7,R3),=CL7'HHOMES '                                            
         LA    R3,7(R3)                                                         
         SPACE                                                                  
         PRINT GEN                                                              
         GOTO1 VDEMOCON,DMCB,(254,(R2)),(2,(R3)),(C'S',DBLOCK),WORK             
         PRINT NOGEN                                                            
*                                                                               
*  PASS AGE BREAKS                                                              
*                                                                               
         L     R6,AIO3              INTERNAL DEMO FORMAT                        
         LA    R2,1001(R6)          7 CHARACTER DEMO FORMAT                     
         LA    R6,2(R6)             POINT TO AGE BREAK NUMBER                   
         LA    R3,207               255-BAD CATEGORIES=207                      
*                                                                               
PASDF100 CLI   0(R6),0              END OF DEMOS                                
         BE    PASDF130                                                         
         LHI   R1,X'03'                                                         
         CLI   0(R2),C'*'           INVALID NAME                                
         BE    PASDF120                                                         
         LR    R4,R2                                                            
         BAS   RE,SENDD                                                         
         CLC   SVRCVEL,=X'0080'     NO DAYS FOR FRONTRUMNNER                    
         BNE   PASDF120                                                         
         LHI   R1,X'0A'                                                         
         LR    R4,R6                PASS DEMO NUMBER                            
         BAS   RE,SENDD                                                         
PASDF120 LA    R2,7(R2)                                                         
         LA    R6,3(R6)                                                         
         BCT   R3,PASDF100                                                      
         SPACE 2                                                                
*                                                                               
*  PASS DAYS                                                                    
*                                                                               
PASDF130 CLC   SVRCVEL,=X'0080'     NO DAYS FOR FRONTRUMNNER                    
         BE    PASDF150                                                         
*                                                                               
         L     R4,=A(DAYTAB)        DAY TABLE                                   
         A     R4,MYRELO                                                        
         LA    R3,DAYNUM                                                        
*                                                                               
PASDF140 LHI   R1,X'04'                                                         
         BAS   RE,SENDD                                                         
         LA    R4,4(R4)                                                         
         BCT   R3,PASDF140                                                      
         SPACE 2                                                                
*                                                                               
*  PASS LENGTHS                                                                 
*                                                                               
PASDF150 L     R4,=A(LENTAB)        LENGTH TABLE                                
         A     R4,MYRELO                                                        
         LA    R3,LENNUM                                                        
*                                                                               
PASDF160 LHI   R1,X'05'                                                         
         BAS   RE,SENDD                                                         
         LA    R4,1(R4)                                                         
         BCT   R3,PASDF160                                                      
         SPACE 2                                                                
*                                                                               
*    *** IMPORTANT THIS NUMBER IS THE MAXIMUM ***                               
*    *** NUMBER OF RECORDS PASSED FROM THE PC ***                               
*    *** TO THE MAINFRAME IN ONE TRANSACTION  ***                               
*                                                                               
PASDF170 MVI   BYTE,100            MAX UNITS PASS FOR STEWARD/MATCMKR           
         CLC   SVRCVEL,=X'0080'                                                 
         BNE   *+8                                                              
         MVI   BYTE,240            MAX UNITS PASS FOR FRONTRUNNER               
         LHI   R1,X'06'                                                         
         LA    R4,BYTE                                                          
         BAS   RE,SENDD                                                         
*                                                                               
         CLC   SVRCVEL,=X'0059'     CHECK FOR MATCHMAKER                        
         BNE   PASDF175                                                         
         MVI   BYTE,15             MAX MG'S ALLOWED FOR MATCHMKR                
         LHI   R1,X'10'                                                         
         LA    R4,BYTE                                                          
         BAS   RE,SENDD                                                         
         B     PASDFEX                                                          
*                                                                               
PASDF175 CLC   SVRCVEL,=X'0080'                                                 
         BNE   PASDF180                                                         
         CLC   VERSION,=X'02000000'    VERSION 2.00.00.00 AND HIGHER            
         BL    PASDF300                                                         
         MVC   HALF,=H'2000'       MAX UNITS IN A 95 ELEM FOR FRNTRNNR          
         LHI   R1,X'0D'                                                         
         LA    R4,HALF                                                          
         BAS   RE,SENDD                                                         
         B     PASDF300                                                         
*                                                                               
PASDF180 MVI   BYTE,200                                                         
         LHI   R1,X'07'             PRODUCT ONLY MAX                            
         LA    R4,BYTE                                                          
         BAS   RE,SENDD                                                         
*                                                                               
         MVI   BYTE,75                                                          
         LHI   R1,X'0F'             SPLIT RECORD MAX                            
         LA    R4,BYTE                                                          
         BAS   RE,SENDD                                                         
*                                                                               
*    *** READ THE CONTROL FILE RECORD TO CONVERT ***                            
*    *** THE USERID NUMBER TO THE AGENCY SIGN-ON ***                            
*                                                                               
*    *** TABLE CONTAINS A LIST AGENCY SIGN-ONS   ***                            
*    *** THAT HAVE LIMITED STEWARD CAPABILITIES. ***                            
*    *** THEY ARE ONLY ALLOWED TO DO PRODUCT     ***                            
*    *** ALLOCATION.                             ***                            
*                                                                               
*                                                                               
         L     RE,=A(ORIGTAB)                                                   
         A     RE,MYRELO                                                        
*                                                                               
PASDF220 CLI   0(RE),X'FF'                                                      
         BE    PASDF300                                                         
         ZIC   R1,0(RE)                                                         
         EX    R1,AGYCOMP                                                       
         BE    PASDF250                                                         
         LA    RE,9(RE)                                                         
         B     PASDF220                                                         
*                                                                               
PASDF250 LHI   R1,X'08'                                                         
         MVI   BYTE,C'Y'                                                        
         LA    R4,BYTE                                                          
         BAS   RE,SENDD                                                         
*                                                                               
*                                                                               
*  PASS SWEEP DATES (JUST FOR FRONTRUNNER)                                      
*                                                                               
PASDF300 L     RF,ACOMFACS                                                      
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,SWEEPTBL  GET A(SWEEP TABLE)                           
         ICM   R6,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                BAD TABLEID PASSED                           
         L     R3,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
*                                                                               
         USING SWPTABLD,R6                                                      
PASDF310 CLI   0(R6),0                                                          
         BE    PASDF350                                                         
         CLC   SWPTFMS,=CL3'TTN'                                                
         BNE   PASDF320                                                         
         TM    SWPFLAGS,SWPFLAGS_MAJOR_SWEEP                                    
         BNZ   PASDF330                                                         
*                                                                               
*  GET NEXT TABLE ENTRY                                                         
PASDF320 AR    R6,R3                                                            
         B     PASDF310                                                         
*                                                                               
* DETERMINE END DATE OF THIS SWEEP                                              
*                                  SET END OF THIS SWEEP                        
PASDF330 GOTO1 VDATCON,DMCB,(10,SWPTST),WORK                                    
*                                                                               
         GOTO1 VADDAY,DMCB,WORK,WORK+6,27                                       
         CLI   SWPTWKS,3                                                        
         BNE   PASDF340                                                         
         GOTO1 VADDAY,DMCB,WORK,WORK+6,20                                       
PASDF340 GOTO1 VDATCON,DMCB,WORK,(2,FULL)                                       
         GOTO1 VDATCON,DMCB,WORK+6,(2,FULL+2)                                   
*  PASS INFO TO THE PC                                                          
         LA    R4,FULL                                                          
         LHI   R1,X'0B'                                                         
         BAS   RE,SENDD                                                         
         LA    R4,2(R4)                                                         
         LHI   R1,X'0C'                                                         
         BAS   RE,SENDD                                                         
         LA    R4,2(R4)                                                         
         B     PASDF320             GET NEXT DATE                               
         DROP  R6                                                               
***************                                                                 
****PASDF300 CLC   SVRCVEL,=X'0080'                                             
****         BNE   PASDF350                                                     
*                                                                               
****         L     R4,=A(SWEEPTAB)                                              
****         A     R4,MYRELO                                                    
****PASDF320 CLI   0(R4),0                                                      
****         BE    PASDF350                                                     
****         LHI   R1,X'0B'                                                     
****         BAS   RE,SENDD                                                     
****         LA    R4,2(R4)                                                     
****         LHI   R1,X'0C'                                                     
****         BAS   RE,SENDD                                                     
****         LA    R4,2(R4)                                                     
****         B     PASDF320                                                     
*                                                                               
*  PASS THE AGENCY FLAG 2 (DEMO PRECISSION)                                     
*                                                                               
PASDF350 LA    R4,SVAGYFL2                                                      
         LHI   R1,X'0E'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
*  SPECIAL CHARGE INFORMATION (STEWARD ONLY)                                    
*                                                                               
PASDF400 CLC   SVRCVEL,=X'0010'                                                 
         BNE   PASDF430                                                         
         L     R4,=A(SPECHTAB)      SPECIAL CHARGE TABLE                        
         A     R4,MYRELO                                                        
         LA    R3,SPECHNM                                                       
*                                                                               
PASDF410 LHI   R1,X'11'                                                         
         BAS   RE,SENDD                                                         
         LA    R4,1(R4)                                                         
         LHI   R1,X'12'                                                         
         BAS   RE,SENDD                                                         
         LA    R4,2(R4)                                                         
         LHI   R1,X'13'                                                         
         BAS   RE,SENDD                                                         
         LA    R4,20(R4)                                                        
         LHI   R1,X'14'                                                         
         BAS   RE,SENDD                                                         
         LA    R4,1(R4)                                                         
         BCT   R3,PASDF410                                                      
         SPACE 2                                                                
*  PASS SPECIAL CHARGE SCREEN CONTROL PROFILE (B1S)                             
         XC    KEY,KEY                                                          
         MVC   KEY(4),=C'SB1S'                                                  
         NI    KEY,X'BF'           LOWER CASE                                   
         MVC   KEY+4(2),QAGY          AGENCY                                    
         GOTO1 VGETPROF,DMCB,KEY,WORK2,VDATAMGR                                 
*                                                                               
         LA    R4,WORK2+4          PASS SCREEN CONTROL PROFILE                  
         LHI   R1,X'15'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
*  HISTORY RECORD INFORMATION (STEWARD ONLY)                                    
*                                                                               
PASDF430 CLC   SVRCVEL,=X'0010'                                                 
         BNE   PASDF500                                                         
         L     R4,=A(HISTTAB)       SPECIAL CHARGE TABLE                        
         A     R4,MYRELO                                                        
         LA    R3,HISTNM                                                        
*                                                                               
PASDF440 LHI   R1,X'16'                                                         
         BAS   RE,SENDD                                                         
         LA    R4,1(R4)                                                         
         LHI   R1,X'17'                                                         
         BAS   RE,SENDD                                                         
         LA    R4,14(R4)                                                        
         BCT   R3,PASDF440                                                      
         SPACE 2                                                                
*  PASS SPECIAL CHARGE SCREEN CONTROL PROFILE (B1S)                             
         XC    KEY,KEY                                                          
         MVC   KEY(4),=C'SB1S'                                                  
         NI    KEY,X'BF'           LOWER CASE                                   
         MVC   KEY+4(2),QAGY          AGENCY                                    
         GOTO1 VGETPROF,DMCB,KEY,WORK2,VDATAMGR                                 
*                                                                               
         LA    R4,WORK2+4          PASS SCREEN CONTROL PROFILE                  
         LHI   R1,X'15'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
*                                                                               
* PASS THE REP AND AGENCY CODES TO STEWARD (STEWARD ONLY)                       
PASDF500 CLC   SVRCVEL,=X'0010'                                                 
         BNE   PASDF600                                                         
*                                                                               
* READ THE REP RECORDS                                                          
         MVC   AIO,AIO3                                                         
         LA    R3,KEY                                                           
         USING REPRECD,R3                                                       
         XC    KEY,KEY                                                          
         MVI   REBKTYPE,C'B'                                                    
         MVC   REBKAGY,QAGY                                                     
*                                                                               
         GOTO1 AIOCALL,DMCB,STA+FIL+HIGH,AIO                                    
         B     PASDF540                                                         
*                                                                               
PASDF520 GOTO1 AIOCALL,DMCB,STA+FIL+SEQ,AIO                                     
PASDF540 CLC   KEY(3),KEYSAVE                                                   
         BNE   PASDF560                                                         
*                                                                               
         LA    R4,REBKREP          PASS REP CODE                                
         LHI   R1,X'18'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,REBKNAME         PASS REP NAME                                
         LHI   R1,X'19'                                                         
         BAS   RE,SENDD                                                         
         B     PASDF520                                                         
         DROP  R3                                                               
*                                                                               
*  PASS 2 BYTE AGENCY CODE                                                      
PASDF560 LA    R4,QAGY                                                          
         LHI   R1,X'1A'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
PASDF600 DS    0H                                                               
*                                                                               
PASDFEX  B     EXIT                                                             
AGYCOMP  CLC   1(0,RE),ALPHID                                                   
*                                                                               
         SPACE 3                                                                
*  ROUTINE BUILDS DEFAULT INFORMATION FOR STEWARD                               
*  IT PASSES DAYPART TABLE                                                      
PASDYPT  NTR1                                                                   
         LHI   R1,X'13'                                                         
         BAS   RE,SENDH                                                         
*                                                                               
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING NDPTHDR,R3                                                       
*                                                                               
         MVC   NDPTKEY(2),=XL2'0D07'                                            
         MVC   NDPTAGM,BAGYMD                                                   
         GOTO1 AIOCALL,DMCB,UNT+DIR+HIGH                                        
         B     PASDY40                                                          
PASDY20  GOTO1 AIOCALL,DMCB,UNT+DIR+SEQ                                         
*                                                                               
PASDY40  CLC   KEY(3),KEYSAVE      CHECK UP TILL AGENCY                         
         BNE   EXIT                                                             
*                                                                               
***      CLC   SVRCVEL,=X'0080'    FRONTRUNNER USES CLI SPECIFIC DPTS           
***      BE    PASDY60                                                          
         CLC   SVRCVEL,=X'0010'    FRNTRNR/MMKR USES CLI SPECIFIC DPTS          
         BNE   PASDY60                                                          
         CLI   NDPTCLT,0           CHECK AGENCY LEVEL                           
         BNE   EXIT                                                             
*                                                                               
*                                                                               
**PASDY60  CLC   SVRCVEL,=X'0080'  FOLLOWING MAPS FOR FRONTRUNNER ONLY          
**         BNE   PASDY80                                                        
PASDY60  CLC   SVRCVEL,=X'0010'    FOLLOWING MAPS FOR FRONTR/MMKR ONLY          
         BE   PASDY80                                                           
*                                                                               
         CLI   NDPTCLT,0            PASS THE CLIENT                             
         BE    PASDY80                                                          
         XC    BYTE,BYTE                                                        
         TM    NDPTCNTL,NDPTAAN                                                 
         BZ    *+8                                                              
         MVI   BYTE,C'Y'                                                        
         GOTO1 VCLUNPK,DMCB,(BYTE,NDPTCLT),FULL                                 
         LA    R4,FULL             CLIENT                                       
         LHI   R1,X'04'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
PASDY80  LA    R4,NDPTDPTA         DAYPART CODE                                 
         LHI   R1,X'01'                                                         
         BAS   RE,SENDD                                                         
         LA    R4,NDPTDES          DAYPART NAME                                 
         LHI   R1,X'02'                                                         
         BAS   RE,SENDD                                                         
         LA    R4,NDPTDPTE         DAYPART EQUATE                               
         LHI   R1,X'03'                                                         
         BAS   RE,SENDD                                                         
         B     PASDY20                                                          
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
*  ROUTINE BUILDS DEFAULT INFORMATION FOR STEWARD                               
*                                                                               
PASAGYST NTR1                                                                   
         MVC   AIO,AIO1                                                         
         LA    R3,KEY                                                           
         USING CTIREC,R3                                                        
         XC    KEY,KEY                                                          
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKNUM,TWAUSRID                                                 
*                                                                               
         LHI   R1,X'17'                                                         
         BAS   RE,SENDH                                                         
         MVI   BYTE,0                                                           
*                                                                               
         GOTO1 AIOCALL,DMCB,CTL+FIL+HIGH,AIO                                    
         L     R3,AIO                                                           
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AIO                                                           
         GOTO1 VHELLO,DMCB,(C'G',CTFILE),(X'02',(R3)),0                         
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                 MUST BE THERE                               
         L     R6,12(R1)                                                        
*                                                                               
         L     RE,=A(PENDTAB)                                                   
         A     RE,MYRELO                                                        
*                                                                               
S10      CLI   0(RE),X'FF'                                                      
         BE    S40                                                              
         ZIC   R1,0(RE)                                                         
         EX    R1,AGYCOMP                                                       
         BE    S30                                                              
         LA    RE,9(RE)                                                         
         B     S10                                                              
S30      MVI   BYTE,C'P'                                                        
*                                                                               
S40      L     RE,=A(APPRTAB)                                                   
         A     RE,MYRELO                                                        
*                                                                               
S60      CLI   0(RE),X'FF'                                                      
         BE    S100                                                             
         ZIC   R1,0(RE)                                                         
         EX    R1,AGYCOMP                                                       
         BE    S80                                                              
         LA    RE,9(RE)                                                         
         B     S60                                                              
S80      MVI   BYTE,C'A'                                                        
*                                                                               
S100     CLI   BYTE,0                                                           
         BE    EXIT                                                             
         LHI   R1,X'01'                                                         
         LA    R4,BYTE                                                          
         BAS   RE,SENDD                                                         
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*  ROUTINE BUILDS DEFAULT INFORMATION FOR STEWARD                               
*  IT PASSES REASON CODES                                                       
PASRSN   NTR1                                                                   
*                                                                               
         MVC   AIO,AIO3                                                         
         LA    R3,KEY                                                           
         USING RSNRECD,R3                                                       
         XC    KEY,KEY                                                          
         MVC   RSNKTYPE,=XL2'0D77'                                              
         MVC   RSNKAGY,QAGY                                                     
         MVI   RSNKMED,C'N'                                                     
*                                                                               
         GOTO1 AIOCALL,DMCB,SPT+DIR+HIGH                                        
         CLC   KEY(5),KEYSAVE                                                   
         BNE   PASRS100                                                         
         LHI   R1,X'14'                                                         
         BAS   RE,SENDH                                                         
         B     PASRS50                                                          
PASRS40  GOTO1 AIOCALL,DMCB,SPT+DIR+SEQ                                         
         CLC   KEY(5),KEYSAVE                                                   
         BNE   PASRS100                                                         
*                                                                               
PASRS50  GOTO1 AIOCALL,DMCB,SPT+FIL+GET,AIO                                     
         L     R3,AIO                                                           
*                                                                               
         LHI   R1,X'01'                                                         
         LA    R4,RSNKCODE                                                      
         BAS   RE,SENDD                                                         
*                                                                               
         LHI   R1,X'02'                                                         
         LA    R4,RSNTEXT                                                       
         BAS   RE,SENDD                                                         
         B     PASRS40                                                          
         DROP  R3                                                               
         SPACE 3                                                                
*                                                                               
*  PASS PROGRAM PTYPE INFO FOR FRONTRUNNER                                      
*                                                                               
PASRS100 CLC   SVRCVEL,=X'0080'                                                 
         BE    PASRS120                                                         
         CLC   SVRCVEL,=X'0010'                                                 
         BNE   EXIT                                                             
PASRS120 MVC   AIO,AIO3                                                         
         LA    R3,KEY                                                           
         USING PTYRECD,R3                                                       
         XC    KEY,KEY                                                          
         MVC   PTYKTYPE,=XL2'0D54'                                              
         MVC   PTYKAGY,QAGY                                                     
*                                                                               
         GOTO1 AIOCALL,DMCB,SPT+DIR+HIGH                                        
         CLC   KEY(4),KEYSAVE                                                   
         BNE   EXIT                                                             
         LHI   R1,X'18'                                                         
         BAS   RE,SENDH                                                         
         B     PASRS150                                                         
PASRS140 GOTO1 AIOCALL,DMCB,SPT+DIR+SEQ                                         
         CLC   KEY(4),KEYSAVE                                                   
         BNE   EXIT                                                             
*                                                                               
PASRS150 GOTO1 AIOCALL,DMCB,SPT+FIL+GET,AIO                                     
         L     R3,AIO                                                           
*                                                                               
         LHI   R1,X'01'                                                         
         LA    R4,PTYKCODE                                                      
         BAS   RE,SENDD                                                         
*                                                                               
         LHI   R1,X'02'                                                         
         LA    R4,PTYTEXT                                                       
         BAS   RE,SENDD                                                         
         B     PASRS140                                                         
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
*  PASS COMSCORE WEB SECURITY                                                   
*                                                                               
PASCSSEC NTR1                                                                   
         BRAS  RE,PASCSSE          COMSCORE WEB SECURITY                        
*                                                                               
         L     R2,AIO4                                                          
         USING CSWSTABD,R2                                                      
*                                                                               
         CLI   CSWTOK,0                                                         
         JE    EXIT                                                             
         CLI   CSWRSCD,0                                                        
         JE    EXIT                                                             
         CLI   CSWRSME,0                                                        
         JE    EXIT                                                             
         CLI   CSWMASH,0                                                        
         JE    EXIT                                                             
*                                                                               
         LHI   R1,X'20'                                                         
         BAS   RE,SENDH                                                         
*                                                                               
         LHI   R1,X'01'                                                         
         LA    R4,CSWTOK           TOKEN                                        
         BAS   RE,SENDD                                                         
*                                                                               
         LHI   R1,X'02'                                                         
         LA    R4,CSWRSCD          RATING SOURCE CONNECTION DATA                
         BAS   RE,SENDD                                                         
*                                                                               
         LHI   R1,X'03'                                                         
         LA    R4,CSWRSME          RATING SOURCE METHOD ENDPOINTS               
         BAS   RE,SENDD                                                         
*                                                                               
         LHI   R1,X'04'                                                         
         LA    R4,CSWMASH          MASHERY KEY                                  
         BAS   RE,SENDD                                                         
*                                                                               
         LHI   R1,X'05'                                                         
         LA    R4,BYTE                                                          
         BAS   RE,SENDD                                                         
         B     EXIT                                                             
         DROP  R2                                                               
*                                                                               
*  PASS COMSCORE STREAMS/VIEWING TYPES                                          
*                                                                               
PASCSVT  NTR1                                                                   
         LHI   R1,X'21'                                                         
         BAS   RE,SENDH                                                         
*                                                                               
         USING CSVTABD,R2                                                       
         LA    R2,CSVTAB                                                        
PCSVT10  CLI   0(R2),X'FF'                                                      
         JE    PCSVTX                                                           
         LHI   R1,X'01'                                                         
         LA    R4,CSVTYPE          STREAM TYPE                                  
         BAS   RE,SENDD                                                         
*                                                                               
         LHI   R1,X'02'                                                         
         LA    R4,CSVDESC          DESCRIPTION                                  
         BAS   RE,SENDD                                                         
*                                                                               
         LHI   R1,X'03'                                                         
         LA    R4,CSVTYPEC         STREAM FOR COMSCORE REQUEST                  
         BAS   RE,SENDD                                                         
         AHI   R2,CSVTABL                                                       
         J     PCSVT10                                                          
         DROP  R2                                                               
*                                                                               
PCSVTX   J     EXIT                                                             
*                                                                               
CSVTAB   DS    0CL(CSVTABL)                                                     
         DC    CL2'RL',CL15'LIVE',CL8'LIVE'                                     
         DC    CL2'RC',CL15'LIVE COMMERCIAL',CL8'AD_LIVE'                       
         DC    CL2'R3',CL15'LIVE + 3',CL8'C3'                                   
         DC    CL2'R7',CL15'LIVE + 7',CL8'C7'                                   
         DC    X'FF'                                                            
*                                                                               
*  PASS COMSCORE SOURCES                                                        
*                                                                               
PASCSS   NTR1                                                                   
         LHI   R1,X'22'                                                         
         BAS   RE,SENDH                                                         
*                                                                               
         USING CSSTABD,R2                                                       
         LA    R2,CSSTAB                                                        
PCSS10   CLI   0(R2),X'FF'                                                      
         JE    PCSSX                                                            
*                                                                               
         LHI   R1,X'01'                                                         
         LA    R4,CSSRCE                                                        
         BAS   RE,SENDD                                                         
         AHI   R2,CSSTABL                                                       
         J     PCSS10                                                           
         DROP  R2                                                               
*                                                                               
PCSSX    J     EXIT                                                             
*                                                                               
CSSTAB   DS    0CL(CSSTABL)                                                     
         DC    CL20'PAV'                                                        
         DC    X'FF'                                                            
*                                                                               
*  PASS UNIVERSE CUT OFF DATES                                                  
*                                                                               
PASUCOD  NTR1                                                                   
         LHI   R1,X'23'                                                         
         BAS   RE,SENDH                                                         
*                                                                               
         L     RF,ACOMFACS                                                      
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,UNIVYRS   GET A(UNIVERSE YEARS)                        
         ICM   R6,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     R3,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
*                                                                               
PUCOD10  CLI   0(R6),X'FF'         STATION FOUND IN TABLE?                      
         BE    PASUCODX             BYPASS THE STATION                          
*                                                                               
         LHI   R1,X'01'            YEAR                                         
         LA    R4,0(R6)                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LHI   R1,X'02'            DATE                                         
         LA    R4,2(R6)                                                         
         BAS   RE,SENDD                                                         
*                                                                               
PUCOD20  AR    R6,R3                                                            
         B     PUCOD10                                                          
*                                                                               
PASUCODX J     EXIT                                                             
*                                                                               
*  SECURITY BIT ROUTINE FOR FRONTRUNNER, STEWARD, CABLE MATCHMAKER              
*   FRONTRUNNER MAP 1, X'80' = DEMO CHANGES NOT ALLOWED                         
PASSEC   NTR1                                                                   
*                                                                               
*  FRONTRUNNER SECURITY LOGIC                                                   
*                                                                               
****     CLC   SVRCVEL,=X'0080'                                                 
****     BNE   PASSEC70                                                         
*                                                                               
         LHI   R1,X'19'                                                         
         BAS   RE,SENDH                                                         
         MVI   BYTE,0                                                           
*                                                                               
*                                  UNIVERSAL BIT SETTINGS                       
*                                                                               
         CLC   QAGY,=CL2'H9'       STARCOMM                                     
         BNE   PASSECDU                                                         
         OI    BYTE,X'80'          RESTRICT DEMO CHANGES                        
         OI    BYTE,X'02'          X'02'=FREEZE ASSIGNED COST                   
*                                                                               
*  THE FOLLOWING USER ID ARE ALLOWED                                            
*  TO OVERRIDE DEMOS IN THE H9 AGENCY                                           
*                                                                               
         LA    RE,H9DEMTB                                                       
*                                                                               
PASH9050 CLI   0(RE),X'FF'                                                      
         BE    PASSECDU                                                         
         ZIC   R1,0(RE)                                                         
         EX    R1,AGYCOMP                                                       
         BE    PASH9080                                                         
         LA    RE,9(RE)                                                         
         B     PASH9050                                                         
*                                                                               
PASH9080 NI    BYTE,X'7F'          TURN OFF RESTRICT DEMO CHANGES               
*                                                                               
PASSECDU CLC   QAGY,=CL2'DU'       MEDIAVEST                                    
         BNE   PASSECUB                                                         
*****    OI    BYTE,X'40'          X'40'=REASON CODE REQUIRED ON PKGE           
         OI    BYTE,X'20'          X'20'=PROGRAM DAYPART CANT CHANGE            
         OI    BYTE,X'10'          X'10'=PACKAGE 2 NAME REQUIRED                
         OI    BYTE,X'02'          X'02'=FREEZE ASSIGNED COST                   
         OI    BYTE,X'01'          X'01'=CREATE UPLOAD LOG                      
*                                                                               
PASSECUB CLC   QAGY,=CL2'UB'       CARAT                                        
         BNE   PASSECWI                                                         
         OI    BYTE,X'08'          X'08'=ALLOW TVQ DEMO INFORMATION             
*                                                                               
PASSECWI CLC   QAGY,=CL2'WI'       WESTERN                                      
         BNE   PASSEC70                                                         
         OI    BYTE,X'04'          X'04'=ALLOW OPTICOM DEMO INFORMATION         
*                                                                               
PASSEC70 LA    R4,BYTE                                                          
         LHI   R1,X'01'                                                         
         BAS   RE,SENDD                                                         
* SEND SENCOND AGENCY LEVEL PROFILE FROM FOR FRONTRRUNNER ONLY                  
         CLC   SVRCVEL,=X'0080'                                                 
         BNE   EXIT                                                             
         CLC   VERSION,=X'01060000'    VERSION 1.05.01.11 AND HIGHER            
         BL    EXIT                                                             
         XC    KEY,KEY             GET USER PROFILE INTO NBUSE                  
         MVC   KEY(4),=C'S0N0'                                                  
         MVC   KEY+2(2),=CL2'FR'              APPLICATION PROFILE               
         MVC   KEY+4(2),QAGY                                                    
         GOTO1 VGETPROF,DMCB,KEY,WORK2,VDATAMGR    N0 PROFILE                   
         LA    R4,WORK2+1                                                       
         LHI   R1,X'02'                                                         
         BAS   RE,SENDD                                                         
         B     EXIT                                                             
*                                                                               
H9DEMTB  DC    XL1'04',CL8'LAPIZ   '                                            
         DC    XL1'03',CL8'BRSA    '                                            
         DC    X'FF'                                                            
         EJECT                                                                  
*  ROUTINE BUILDS DEFAULT INFORMATION FOR STEWARD                               
*  IT PASSES NETWORK CODES                                                      
PASNTWK  NTR1                                                                   
         LHI   R1,X'15'                                                         
         BAS   RE,SENDH                                                         
         CLC   SVRCVEL,=X'0059'                                                 
         BE    PASNT10                                                          
         CLC   SVRCVEL,=X'0080'                                                 
         BNE   PASNT55                                                          
*                                                                               
* CABLE MATCHMAKER AND FRONTRUNNER READ                                         
* THE STATION FILE PASS ALL CABLE NETWORK                                       
PASNT10  MVC   AIO,AIO3                                                         
         LA    R3,KEY                                                           
         USING STARECD,R3                                                       
         XC    KEY,KEY                                                          
         MVI   STAKTYPE,C'S'                                                    
         MVI   STAKMED,C'N'                                                     
*                                                                               
         GOTO1 AIOCALL,DMCB,STA+FIL+HIGH,AIO                                    
         CLC   KEY(2),KEYSAVE                                                   
         BE    PASNT50                                                          
         B     PASNT94                                                          
PASNT40  GOTO1 AIOCALL,DMCB,STA+FIL+SEQ,AIO                                     
         CLC   KEY(2),KEYSAVE                                                   
         BNE   PASNT94                                                          
*                                                                               
PASNT50  L     R3,AIO                                                           
         CLC   STAKAGY,QAGY                                                     
         BNE   PASNT40                                                          
         CLC   STAKCLT,=CL3'000'                                                
         BNE   PASNT40                                                          
         CLC   SVRCVEL,=X'0080'                                                 
         BE    PASNT53                                                          
         CLI   STYPE,C'C'   FOR NOW ONLY PASS CABLE CABLE MATCHMAKER            
         BNE   PASNT40                                                          
*                                                                               
PASNT53  LHI   R1,X'01'                                                         
         LA    R4,STAKCALL                                                      
         BAS   RE,SENDD                                                         
****     B     PASNT40                                                          
*                                                                               
****     CLC   SVRCVEL,=X'0080'                                                 
****     BNE   PASNT40                                                          
         LHI   R1,X'02'                                                         
         LA    R4,STYPE                                                         
         BAS   RE,SENDD                                                         
         CLC   SVRCVEL,=X'0080'        ONLY FOR FRONTRUNNER                     
         BNE   PASNT40                                                          
         CLC   VERSION,=X'01060000'    VERSION 1.05.01.11 AND HIGHER            
         BL    PASNT40                                                          
         LHI   R1,X'03'                                                         
         LA    R4,SPTYPE                                                        
         BAS   RE,SENDD                                                         
         B     PASNT40                                                          
         SPACE 2                                                                
*                                                                               
*  PASS THE NTI STAION INFO                                                     
*                                                                               
*  PASS THE NETWORK STATION INFO                                                
PASNT55  LA    R4,NETTAB                                                        
PASNT58  CLI   0(R4),X'FF'                                                      
         BE    PASNT60                                                          
         LHI   R1,X'01'                                                         
         BAS   RE,SENDD                                                         
         LA    R4,5(R4)                                                         
         B     PASNT58                                                          
*                                                                               
*  PASS THE CABLE STATION INFO                                                  
*                                                                               
PASNT60  LA    R6,KEY              READ DIR TO GET CABLE STATIONS               
         USING PRKEY,R6                                                         
         XC    KEY,KEY             INIT                                         
         MVC   PRCODE(3),=C'PCN'                                                
*                                                                               
PASNT62  MVI   PRSTAT+4,X'FF'      FORCE NEXT STATION                           
         XC    PRKMKT(10),PRKMKT                                                
         GOTO1 AIOCALL,DMCB,NTI+DIR+HIGH                                        
         B     PASNT66                                                          
*                                                                               
PASNT63  MVI   PRSTAT+4,X'FF'      FORCE NEXT STATION                           
         XC    PRKMKT(10),PRKMKT                                                
         GOTO1 AIOCALL,DMCB,NTI+DIR+HIGH                                        
         B     PASNT66                                                          
*                                                                               
PASNT64  GOTO1 AIOCALL,DMCB,NTI+DIR+SEQ                                         
*                                                                               
PASNT66  CLC   KEY(3),KEYSAVE                                                   
         BNE   PASNT74                                                          
*                                                                               
         TM    PRSTAT,X'F0'        IS THIS NUMERIC?                             
         BNO   PASNT64             NO, SKIP IT                                  
*                                                                               
         PACK  DUB,PRSTAT(4)                                                    
         CVB   R1,DUB                                                           
         STH   R1,HALF                                                          
         XC    DUB,DUB             PREPARE DUB FOR FOR                          
         MVI   DUB+4,C'C'           STATION CALL LETTERS                        
*                                                                               
         L     RF,ACOMFACS                                                      
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,NECABCLL  GET A(NET CABLE STATIONS TABLE)              
         ICM   RE,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RF,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
*****    LA    RE,ALLCAB                                                        
*                                                                               
PASNT68  CLI   0(RE),X'FF'         STATION FOUND IN TABLE?                      
******   BE    PASNT70              NOPE, PUT OUT NUMERIC STATION               
         BE    PASNT63              BYPASS THE STATION                          
         CLC   HALF,4(RE)                                                       
         BE    PASNT69                                                          
         AR    RE,RF                NEXT ENTRY                                  
         B     PASNT68                                                          
PASNT69  MVC   DUB(4),0(RE)        TRANSFER ALPHABETIC STATION LTRS             
         B     PASNT72                                                          
*                                                                               
PASNT70  MVC   DUB(4),PRSTAT       TRANSFER NUMERIC STATION                     
*                                                                               
PASNT72  MVI   DUB+4,C'C'                                                       
*                                                                               
         LA    R4,DUB                                                           
         LHI   R1,X'01'                                                         
         BAS   RE,SENDD                                                         
         B     PASNT63             GET NEXT MARKET                              
*                                                                               
PASNT74  B     PASNT80                                                          
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*  PASS THE CABLE STATION INFO                                                  
*                                                                               
PASNT80  LA    R6,KEY              READ DIR TO GET CABLE STATIONS               
         USING PRKEY,R6                                                         
         XC    KEY,KEY             INIT                                         
         MVC   PRCODE(3),=C'PNN'                                                
*                                                                               
PASNT82  MVI   PRSTAT+4,X'FF'      FORCE NEXT STATION                           
         XC    PRKMKT(10),PRKMKT                                                
         GOTO1 AIOCALL,DMCB,NTI+DIR+HIGH                                        
         CLC   KEY(3),KEYSAVE                                                   
         BNE   PASNT94                                                          
*                                                                               
         CLI   PRSTAT+4,C'S'       IS THIS SYNDICATOR                           
         BNE   PASNT82             NO, READ NEXT STATION                        
*                                                                               
         LA    R4,PRSTAT                                                        
         LHI   R1,X'01'                                                         
         BAS   RE,SENDD                                                         
         B     PASNT82             GET NEXT MARKET                              
*                                                                               
PASNT94  B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* NETWORK STATION TABLE                                                         
*                                                                               
*          DATA SET NEPOD01    AT LEVEL 059 AS OF 08/28/00                      
NETTAB   DC    C'ABC N'                                                         
         DC    C'CBS N'                                                         
         DC    C'FOX N'                                                         
         DC    C'NBC N'                                                         
         DC    C'PAR N'                                                         
         DC    C'PAX N'                                                         
         DC    C'WB  N'                                                         
         DC    X'FF'                                                            
         EJECT                                                                  
*        DC    C'AEN ',AL2(6218)                                                
*        DC    C'AMC ',AL2(6593)                                                
*        DC    C'APL ',AL2(6605)                                                
*        DC    C'BET ',AL2(6200)                                                
*        DC    C'BRVO',AL2(6526)                                                
*        DC    C'CMD ',AL2(7133)                                                
*        DC    C'CMT ',AL2(6647)                                                
*        DC    C'CMAX',AL2(6513)                                                
*        DC    C'CNBC',AL2(6521)                                                
*        DC    C'CNN ',AL2(6202)                                                
*        DC    C'CRT ',AL2(7183)                                                
*        DC    C'DISC',AL2(6530)                                                
*        DC    C'DSNY',AL2(6522)                                                
*        DC    C'ENT ',AL2(6384)                                                
*        DC    C'ESPN',AL2(6204)                                                
*        DC    C'ESP2',AL2(7287)                                                
*        DC    C'FAM ',AL2(6201)                                                
*        DC    C'FOOD',AL2(7304)   YUM YUM                                      
*        DC    C'FX  ',AL2(7328)   FOX CABLE                                    
*        DC    C'FXNC',AL2(7401)   FOX NEWS CHANNEL                             
*        DC    C'GAME',AL2(7099)                                                
*        DC    C'HBO ',AL2(6510)                                                
*        DC    C'HGTV',AL2(8920)   HOME AND GARDEN                              
*        DC    C'HLN ',AL2(6199)                                                
*        DC    C'LIF ',AL2(6196)                                                
*        DC    C'MTV ',AL2(6198)                                                
*        DC    C'MSNB',AL2(7801)                                                
*        DC    C'NICK',AL2(6212)                                                
*        DC    C'ODSY',AL2(6485)                                                
*        DC    C'PREV',AL2(6477)                                                
*        DC    C'SCIF',AL2(7235)                                                
*        DC    C'SHOW',AL2(6511)                                                
*        DC    C'TBS ',AL2(5830)                                                
*        DC    C'TLC ',AL2(6635)                                                
*        DC    C'TMC ',AL2(6514)                                                
*        DC    C'TNN ',AL2(6221)                                                
*        DC    C'TNT ',AL2(6390)                                                
*        DC    C'TOON',AL2(7241)                                                
*        DC    C'TRVL',AL2(6678) TRAVEL                                         
*        DC    C'TVL ',AL2(7838) TV LAND                                        
*        DC    C'TWC ',AL2(6523)                                                
*        DC    C'USA ',AL2(6217)                                                
*        DC    C'VH1 ',AL2(6546)                                                
*        DC    C'WGNC',AL2(6788)                                                
*        DC    C'XHIS',AL2(8908)                                                
*        DC    C'TDSN',AL2(7611)                                                
*        DC    X'FFFF'                                                          
         EJECT                                                                  
*                                                                               
*  PASS SERVER INFORMATION                                                      
*  CHECK FACPACK AND PASS MATCHING SERVER                                       
*                                                                               
PASSRVR  NTR1                                                                   
         CLC   VERSION,=X'05020024'    VERSION 5.02.00.36 AND HIGHER            
         BL    PASRVREX                                                         
         L     R3,ACOMFACS                                                      
         USING COMFACSD,R3                                                      
         ICM   RF,15,CSWITCH                                                    
         MVC   DMCB(4),=X'FEFFFFFF'                                             
         GOTO1 (RF),DMCB                                                        
         L     R3,0(R1)                                                         
         USING SYSFACD,R3                                                       
         ICM   R6,15,VSSB                                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         USING SSBD,R6                                                          
         MVC   SVSYSQ,SSBDSPAC     SAVE SYSTEM                                  
         DROP  R6                                                               
*                                                                               
         LHI   R1,X'11'                                                         
         BAS   RE,SENDH                                                         
*                                                                               
* GET UNIVERSAL RECORD FOR SERVER REQUEST VALUES                                
         BRAS  RE,GETSDR                                                        
*                                                                               
         L     R6,AIO3                                                          
         USING SDRRECD,R6                                                       
         LA    R6,SDRRFRST         POINT TO START OF DATA                       
PASRVR10 CLI   0(R6),0                                                          
         BE    PASRVR20                                                         
         USING SDELD,R6                                                         
*                                                                               
* 10/17 - WE ARE MOVING SDR RECORDS TO A DDLINK CALL BUT                        
* KEEPING THIS HERE TO NOT BREAK ANYTHING. - SCHT                               
*                                                                               
         CLI   SDEEL,X'31'         ELEMMENT 50 OR GREATER SKIP                  
         JH    PASRVR18                                                         
*                                                                               
         XC    WORK,WORK                                                        
         CLI   SDEEL,SDOPTELQ      TEST OPTIMIZER AUTHORIZATION ELEMENT         
         BNE   PASRVR14                                                         
*                                                                               
         MVI   WORK,C'Y'           GIVE AUTHORIZATION TO ALL AGENCIES           
         B     PASRVR16                                                         
***************************************************************                 
         MVI   WORK,C'N'                                                        
         ZIC   R1,SDELEN                                                        
         SHI   R1,3                                                             
*                                                                               
         LA    RF,SDEDATA          POINT TO AGENCY LIST                         
PASRVR12 CLC   QAGY,0(RF)          TEST FOUND AGENCY                            
         BNE   *+12                                                             
         MVI   WORK,C'Y'                                                        
         B     PASRVR16                                                         
*                                                                               
         AHI   RF,2                BUMP TO NEXT AGENCY IN LIST                  
         SHI   R1,1                                                             
         BCT   R1,PASRVR12                                                      
         B     PASRVR16            NOT AUTHORIZED FOR OPTIMIZER                 
*                                                                               
PASRVR14 ZIC   R1,SDELEN                                                        
         SHI   R1,4                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),SDEDATA                                                  
***************************************************************                 
PASRVR16 LA    R4,WORK                                                          
         ZIC   R1,SDEEL             ELEMENT ID = MAP CODE #                     
         BAS   RE,SENDD                                                         
*                                                                               
PASRVR18 ZIC   RF,SDELEN                                                        
         AR    R6,RF                                                            
         B     PASRVR10                                                         
         DROP  R3,R6                                                            
*                                                                               
*  PASS SERVER SECURITY                                                         
*                                                                               
PASRVR20 L     R3,ACOMFACS                                                      
         USING COMFACSD,R3                                                      
         ICM   RF,15,CSWITCH                                                    
         MVC   DMCB(4),=X'FFFFFFFF'                                             
         GOTO1 (RF),DMCB                                                        
         L     R6,DMCB                                                          
         JNZ   *+6                                                              
         DC    H'0'                                                             
         USING UTLD,R6                                                          
         MVI   BYTE,C'N'                                                        
         TM    TSTATB,X'E0'                                                     
         BZ    *+8                                                              
         MVI   BYTE,C'Y'                                                        
         LA    R4,BYTE                                                          
         SR    R1,R1                                                            
         LHI   R1,X'0F'             PASSWORD                                    
         BAS   RE,SENDD                                                         
*                                                                               
PASRVREX B     EXIT                                                             
         DROP  R3,R6                                                            
         EJECT                                                                  
*                                                                               
NETSDRQ  EQU   3                   NET SYSTEM EQUATE #                          
SDOPTELQ EQU   X'0B'               OPITIMIZER ACCESS ELEMENT                    
ADVSDRQ  EQU   0                   ADV SYSTEM                                   
TSTSDRQ  EQU   X'80'               TST SYSTEM                                   
CSCSDRQ  EQU   X'88'               CSC SYSTEM                                   
FQASDRQ  EQU   X'84'               FQA SYSTEM                                   
*                                                                               
*  ROUTINE READS PROGRAM RECORDS USING FILTERS FROM THE X'40' ELEMENT           
*                                                                               
PRGSERCH NTR1                                                                   
         MVC   AIO,AIO3                                                         
         LA    R3,KEY                                                           
         MVI   DAYLOOP,0            SET LOOP TO FIRST DAY                       
         USING NPGRECD,R3                                                       
         XC    KEY,KEY                                                          
         LA    R5,4                 LENGTH OF KEY COMPARE                       
*******  MVC   NPGKTYP,=XL2'0DA0'                                               
         MVC   NPGKTYP,=XL2'0D20'                                               
         MVC   NPGKAM,BAGYMD                                                    
         MVC   NPGKNET,BMKT                                                     
         MVC   HOLDTIME,SVTIME     SET UP FIRST TIME PASS                       
         MVC   DAYSW,SVDAYFLT      SET SWITCH TO FIRST DAY                      
         MVC   DAYFILT,SVDAYDEM    SET FILTER TO FIRST DAY                      
*******  CLI   SVDAYFLT,C'Y'       WAS DAY TO BE FILTERED                       
*******  BNE   PRGSR100                                                         
*******  LA    R5,1(R5)                                                         
*******  MVC   NPGKDAY,SVDAY                                                    
*                                                                               
PRGSR100 STCM  R5,1,SVLENGTH                                                    
         GOTO1 AIOCALL,DMCB,SPT+DIR+HIGH                                        
         B     PRGSR160                                                         
PRGSR120 LA    R3,KEY                                                           
         GOTO1 AIOCALL,DMCB,SPT+DIR+SEQ                                         
PRGSR160 ZIC   R5,SVLENGTH                                                      
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         BNE   PRGSR700                                                         
*                                                                               
***         OC    HOLDTIME,HOLDTIME    WAS TIME TO BE FILTERED                  
***         BZ    PRGSR200                                                      
***         MVC   FULL,NPGKTIME                                                 
*                                                                               
*  ADD 2400 TO TIMES LESS THEN 600                                              
*                                                                               
***         SR    RE,RE                                                         
***         LH    RE,FULL                                                       
***         CH    RE,=H'0559'                                                   
***         BH    PRGSR165                                                      
***         AH    RE,=H'2400'                                                   
***         STH   RE,FULL                                                       
***PRGSR165 SR    RE,RE                                                         
***         LH    RE,FULL+2                                                     
***         CH    RE,=H'0559'                                                   
***         BH    PRGSR170                                                      
***         AH    RE,=H'2400'                                                   
***         STH   RE,FULL+2                                                     
***PRGSR170 CLC   HOLDTIME(2),FULL+2   IS START TIME > RECORD END TIME          
***         BH    PRGSR120                                                      
***         CLC   HOLDTIME+2(2),FULL   IS END TIME < RECORD START TIME          
***         BL    PRGSR120                                                      
*                                                                               
*  DATE FILTER                                                                  
*                                                                               
PRGSR200 OC    SVSDATE(4),SVSDATE    DATE INPUTTED                              
         BZ    PRGSR230                                                         
         OC    SVEDATE,SVEDATE       JUST START DATE INPUTTED                   
         BZ    PRGSR220                                                         
         CLC   SVSDATE,NPGKEND      IS START DATE > PROGRAM DATE                
         BH    PRGSR120                                                         
*                                                                               
         MVC   HALF,SVEDATE                                                     
         CLI   SVFRSW,C'M'          MATCHMAKER?                                 
         BNE   PRGSR210                                                         
         CLC   VERSION,=X'02010005'    VERSION 2.01.00.05 AND HIGHER            
         JL    PRGSR205                                                         
         CLI   SVMEDTYP,C'C'       CABLE?                                       
         JNE   PRGSR230            NO - IGNORE END DATE (SPEC-27997)            
PRGSR205 CLC   SVEDATE,=X'F19F'     DEFAULT 12/31/20 END DATE?                  
         JNE   *+10                                                             
         MVC   HALF,=X'FF9F'        USE 12/31/27 END DATE                       
*                                                                               
PRGSR210 CLC   HALF,NPGKEND         IS END DATE < PROGRAM DATE                  
         BL    PRGSR120                                                         
         B     PRGSR230                                                         
*                                                                               
PRGSR220 CLC   SVSDATE,NPGKEND      IS DATE = PROGRAM DATE                      
         BNE   PRGSR120                                                         
*                                                                               
PRGSR230 L     R3,AIO                                                           
         GOTO1 AIOCALL,DMCB,SPT+FIL+GET,AIO                                     
*                                                                               
         CLI   SVPRNAD,X'40'        NAD CODE FILTER REQUEST                     
         BH    PRGSR250                                                         
         CLI   SVPRTIER,X'40'       TIER FILTER REQUEST                         
         BH    PRGSR250                                                         
         CLI   SVPRCONT,X'40'       CONTENT FILTER REQUEST                      
         BH    PRGSR250                                                         
         CLI   SVPRTYPE,X'40'       PROGRAM TYPE FILTER REQUEST                 
         BH    PRGSR250                                                         
         CLI   SVPRNEW,X'40'        NEW/RETURNING FILTER REQUEST                
         BH    PRGSR250                                                         
         B     PRGSR300                                                         
PRGSR250 GOTO1 VHELLO,DMCB,(C'G',SPTFILE),(X'03',(R3)),0                        
         CLI   12(R1),0                                                         
         BNE   PRGSR120                                                         
         L     R6,12(R1)                                                        
         USING NPGEL03,R6                                                       
         CLI   SVPRNAD,X'40'        NAD CODE FILTER REQUEST                     
         BNH   *+14                                                             
         CLC   SVPRNAD,NPGNADDM                                                 
         BNE   PRGSR120                                                         
         CLI   SVPRNEW,C'U'         NEW/RETURNING UNFILLED CHECK                
         BNE   *+16                                                             
         CLI   NPPRNEW,X'40'                                                    
         BH    PRGSR120                                                         
         B     *+22                                                             
         CLI   SVPRNEW,X'40'        NEW/RETURNING FILTER REQUEST                
         BNH   *+14                                                             
         CLC   SVPRNEW,NPPRNEW                                                  
         BNE   PRGSR120                                                         
         CLI   SVPRTIER,X'40'       TIER FILTER REQUEST                         
         BNH   *+14                                                             
         CLC   SVPRTIER,NPTIER                                                  
         BNE   PRGSR120                                                         
         CLI   SVPRCONT,X'40'       CONTENT FILTER REQUEST                      
         BNH   *+14                                                             
         CLC   SVPRCONT,NPPRGRAT                                                
         BNE   PRGSR120                                                         
         CLI   SVPRTYPE,X'40'       PROGRAM TYPE FILTER REQUEST                 
         BNH   *+14                                                             
         CLC   SVPRTYPE(2),NPPRGTYP                                             
         BNE   PRGSR120                                                         
         CLI   SVPRTYPE+2,X'40'     SUB PROGRAM TYPE FILTER REQUEST             
         BNH   *+14                                                             
         CLC   SVPRTYPE+2(4),NPPRGSTP                                           
         BNE   PRGSR120                                                         
         DROP  R6                                                               
*                                                                               
PRGSR300 CLI   SV2DAYPT,X'40'       2 CAHRACTER DAYPART FILTER                  
         BNH   PRGSR320                                                         
         GOTO1 VHELLO,DMCB,(C'G',SPTFILE),(X'93',(R3)),0                        
         CLI   12(R1),0                                                         
         BE    *+8                                                              
         B     PRGSR120                                                         
         L     R6,12(R1)                                                        
         USING NPGEL93,R6                                                       
         CLC   SV2DAYPT,NPG2DYPA                                                
         BNE   PRGSR120                                                         
         DROP  R6                                                               
*                                                                               
PRGSR320 GOTO1 VHELLO,DMCB,(C'G',SPTFILE),(X'92',(R3)),0                        
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,12(R1)                                                        
         USING NPGEL92,R6                                                       
*                                                                               
         CLI   SVROT,0              ROTATION FILTER REQUESTED                   
         BE    PRGSR325                                                         
         MVC   BYTE,NPGROT                                                      
         OC    BYTE,SVROT                                                       
         CLC   BYTE,NPGROT                                                      
         BNE   PRGSR120                                                         
*                                                                               
PRGSR325 CLI   SVPRFILT,X'40'       PROGRAM FILTER FILTER REQUESTED             
         BNH   PRGSR330                                                         
         CLI   SVPRFILT,C'*'                                                    
         BE    *+14                                                             
         CLC   SVPRFILT(1),NPGFILT                                              
         BNE   PRGSR120                                                         
         CLI   SVPRFILT+1,C'*'                                                  
         BE    *+14                                                             
         CLC   SVPRFILT+1(1),NPGFILT+1                                          
         BNE   PRGSR120                                                         
         CLI   SVPRFILT+2,C'*'                                                  
         BE    *+14                                                             
         CLC   SVPRFILT+2(1),NPGFILT+2                                          
         BNE   PRGSR120                                                         
*                                                                               
PRGSR330 OC    SVNTI,SVNTI          NTI CODE FILTER REQUESTED                   
         BZ    PRGSR340                                                         
         CLC   SVNTI,NPGPPNO                                                    
         BNE   PRGSR120                                                         
*                                                                               
PRGSR340 OC    HOLDTIME,HOLDTIME    WAS TIME TO BE FILTERED                     
         BZ    PRGSR360                                                         
         MVC   FULL,NPGTIME                                                     
*                                                                               
*  ADD 2400 TO TIMES LESS THEN 600                                              
*                                                                               
         SR    RE,RE                                                            
         LH    RE,FULL                                                          
         CH    RE,=H'0559'                                                      
         BH    PRGSR345                                                         
         AH    RE,=H'2400'                                                      
         STH   RE,FULL                                                          
PRGSR345 SR    RE,RE                                                            
         LH    RE,FULL+2                                                        
         CH    RE,=H'0559'                                                      
         BH    PRGSR350                                                         
         AH    RE,=H'2400'                                                      
         STH   RE,FULL+2                                                        
PRGSR350 CLC   HOLDTIME(2),FULL+2   IS START TIME > RECORD END TIME             
         BH    PRGSR120                                                         
         CLC   HOLDTIME+2(2),FULL   IS END TIME < RECORD START TIME             
         BL    PRGSR120                                                         
*                                                                               
PRGSR360 CLI   DAYSW,C'Y'           WAS DAY TO BE FILTERED                      
         BNE   PRGSR400                                                         
         CLC   NPGDAY,DAYFILT                                                   
         BNE   PRGSR120                                                         
*                                                                               
*  PASS THE RECORD TO THE PC                                                    
*                                                                               
PRGSR400 LHI   R1,X'42'                                                         
         BAS   RE,SENDH                                                         
*  OUTPUT THE DAY                                                               
         XC    DMCB(4),DMCB                                                     
         MVC   DMCB+4(3),=X'D9000A'                                             
         MVI   DMCB+7,QUNDAY                                                    
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     R4,DMCB                                                          
         XC    DMCB(24),DMCB                                                    
         SPACE                                                                  
         XC    WORK(4),WORK                                                     
         GOTO1 (R4),DMCB,NPGDAY,WORK       UNDAY                                
         LA    R4,WORK                                                          
         LHI   R1,X'01'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
***         LA    R4,DAYTBL                                                     
***PRGSR420 CLI   0(R4),X'FF'                                                   
***         BNE   *+6                                                           
***         DC    H'0'                                                          
***         CLC   NPGRDAY,0(R4)                                                 
***         BE    PRGSR460                                                      
***         LA    R4,5(R4)                                                      
***         B     PRGSR420                                                      
***PRGSR460 LA    R4,1(R4)                                                      
***         LHI   R1,X'01'                                                      
***         BAS   RE,SENDD                                                      
*                                                                               
*  OUTPUT THE TIME                                                              
         XC    WORK,WORK                                                        
         GOTO1 VUNTIME,DMCB,NPGTIME,WORK                                        
         LHI   R1,X'02'                                                         
         LA    R4,WORK                                                          
         BAS   RE,SENDD                                                         
*                                                                               
*  OUTPUT THE PROGRAM NAME                                                      
         LHI   R1,X'03'                                                         
         LA    R4,NPGNAME                                                       
         BAS   RE,SENDD                                                         
*                                                                               
         MVC   PGMCODE,NPGKPROG     PROGRAM CODE                                
         MVC   PGMENDDT,NPGKEND     END DATE                                    
         MVC   PGMNTI,NPGPPNO       NTI CODE                                    
         MVC   PGMSTAT,NPGSTAT      STATUS                                      
         MVC   PGMSHARE,NPGSHARE    SHARE                                       
         MVC   PGMROT,NPGROT        ROTATOR                                     
         DROP  R6                                                               
*                                                                               
*  OUTPUT THE PROGRAM CODE                                                      
PRGSR480 LHI   R1,X'04'                                                         
         LA    R4,PGMCODE                                                       
         BAS   RE,SENDD                                                         
*                                                                               
*  OUTPUT THE START DATE                                                        
         GOTO1 VHELLO,DMCB,(C'G',SPTFILE),(X'93',(R3)),0                        
         CLI   12(R1),0                                                         
         BNE   PRGSR520                                                         
         L     R6,12(R1)                                                        
         USING NPGEL93,R6                                                       
         OC    NPG2STD,NPG2STD                                                  
         BZ    PRGSR500             NEXT RECORD                                 
         LHI   R1,X'05'                                                         
         LA    R4,NPG2STD                                                       
         BAS   RE,SENDD                                                         
PRGSR500 CLI   SVFRSW,C'F'          IS THIS FRONTRUNNER                         
         BNE   PRGSR520                                                         
*****    LHI   R1,X'0F'                                                         
*****    LA    R4,NPG2DYPA                                                      
*****    BAS   RE,SENDD                                                         
         DROP  R6                                                               
*                                                                               
*  OUTPUT THE END DATE                                                          
PRGSR520 LHI   R1,X'06'                                                         
         LA    R4,PGMENDDT                                                      
         BAS   RE,SENDD                                                         
*                                                                               
*  OUTPUT THE NTI CODE                                                          
         LHI   R1,X'07'                                                         
         LA    R4,PGMNTI                                                        
         BAS   RE,SENDD                                                         
*                                                                               
*  OUTPUT THE RATING/SHARE                                                      
         LHI   R1,X'08'             SET AS A RATING                             
         TM    PGMSTAT,X'80'        IS VALUE A RATING                           
         BO    *+8                                                              
         LHI   R1,X'09'             SET AS A SHARE                              
         LA    R4,PGMSHARE                                                      
         BAS   RE,SENDD                                                         
*                                                                               
*  OUTPUT THE ROTATION (MATCHMAKER ONLY)                                        
         CLI   SVFRSW,C'F'          IS THIS FRONTRUNNER                         
         BE    *+12                                                             
         CLI   SVFRSW,C'M'          IS THIS MATCHMAKER                          
         BNE   PRGSR530                                                         
         LHI   R1,X'0E'                                                         
         LA    R4,PGMROT                                                        
         BAS   RE,SENDD                                                         
*                                                                               
*  OUTPUT THE NEW/TIER/PROGRAMTYPE/CONTENT FRONTRUNNER ONLY                     
PRGSR530 CLI   SVFRSW,C'F'          IS THIS FRONTRUNNER                         
         BNE   PRGSR600                                                         
         GOTO1 VHELLO,DMCB,(C'G',SPTFILE),(X'03',(R3)),0                        
         CLI   12(R1),0                                                         
         BNE   PRGSR600                                                         
         L     R6,12(R1)                                                        
         USING NPGEL03,R6                                                       
         OC    NPPRNEW,NPPRNEW                                                  
         BZ    PRGSR540             NEW PROGRAM                                 
         LHI   R1,X'0A'                                                         
         LA    R4,NPPRNEW                                                       
         BAS   RE,SENDD                                                         
PRGSR540 OC    NPPRGTYP,NPPRGTYP                                                
         BZ    PRGSR550             PROGRAM TYPE                                
         MVC   DUB(2),NPPRGTYP                                                  
         MVC   DUB+2(4),NPPRGSTP                                                
         OC    DUB(6),SPACES                                                    
         LHI   R1,X'0B'                                                         
         LA    R4,DUB                                                           
         BAS   RE,SENDD                                                         
PRGSR550 OC    NPPRGRAT,NPPRGRAT                                                
         BZ    PRGSR560             CONTENT                                     
         LHI   R1,X'0C'                                                         
         LA    R4,NPPRGRAT                                                      
         BAS   RE,SENDD                                                         
PRGSR560 OC    NPTIER,NPTIER                                                    
         BZ    PRGSR600             CONTENT                                     
         LHI   R1,X'0D'                                                         
         LA    R4,NPTIER                                                        
         BAS   RE,SENDD                                                         
*                                                                               
*  OUTPUT THE PRECISSION FACTOR                                                 
PRGSR600 CLI   SVFRSW,C'M'          IS THIS MATCHMAKER                          
         BE    PRGSR120                                                         
         CLI   SVFRSW,C'F'          IS THIS FRONTRUNNER                         
         BE    PRGSR120                                                         
         GOTO1 VHELLO,DMCB,(C'G',SPTFILE),(X'5D',(R3)),0                        
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                 MUST BE THERE                               
         L     R6,12(R1)                                                        
         MVI   BYTE,1               1 DECIMAL INDICATOR                         
         CLC   5(2,R6),=XL2'5901'   2 DECIMAL PRECISSION                        
         BL    *+8                                                              
         MVI   BYTE,2               2 DECIMAL INDICATOR                         
         LA    R4,BYTE                                                          
         LHI   R1,X'10'                                                         
******   BAS   RE,SENDD             TEMP                                        
         B     PRGSR120                                                         
*                                                                               
PRGSR700 OC    SVTIME2,SVTIME2     WAS 2ND TIME INPUTTED                        
         BNZ   PRGSR720                                                         
         CLI   SVDY2FLT,C'Y'       WAS 2ND DAY INPUTED                          
         BNE   PRGSREX                                                          
PRGSR720 CLI   DAYLOOP,X'FF'        CHECK IF DAY2 ALREADY DONE                  
         BE    PRGSREX                                                          
         MVI   DAYLOOP,X'FF'        SET LOOP INDICATOR TO SECOND DAY            
*                                                                               
*  SET KEY FOR SECOND PASS                                                      
*                                                                               
         MVC   HOLDTIME,SVTIME2     SET UP TIME2 COMPARE                        
         XC    KEY,KEY                                                          
         LA    R5,4                 LENGTH OF KEY COMPARE                       
****     MVC   NPGKTYP,=XL2'0DA0'                                               
         MVC   NPGKTYP,=XL2'0D20'                                               
         MVC   NPGKAM,BAGYMD                                                    
         MVC   NPGKNET,BMKT                                                     
         XC    DAYSW,DAYSW         UNSET THE DAY SWITCH                         
         XC    DAYFILT,DAYFILT     UNSET THE DAY FILTER                         
         CLI   SVDY2FLT,C'Y'       WAS DAY TO BE FILTERED                       
         BNE   PRGSR100                                                         
         MVC   DAYSW,SVDY2FLT      SET SWITCH TO CHECK DAY                      
         MVC   DAYFILT,SVDY2DEM    DAY 2 FILTER                                 
****     LA    R5,1(R5)                                                         
****     MVC   NPGKDAY,SVDAY2                                                   
         B     PRGSR100                                                         
*                                                                               
PRGSREX  B     EXIT                                                             
         DROP  R3,R6                                                            
*                                                                               
DAYTBL   DC    X'01',CL4'MON '                                                  
         DC    X'02',CL4'TUE '                                                  
         DC    X'03',CL4'WED '                                                  
         DC    X'04',CL4'THU '                                                  
         DC    X'05',CL4'FRI '                                                  
         DC    X'06',CL4'SAT '                                                  
         DC    X'07',CL4'SUN '                                                  
         DC    X'00',CL4'M-F '                                                  
         DC    X'08',CL4'M-SU'                                                  
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
*  ROUTINE READS A PROGRAM RECORD                                               
*                                                                               
PRGDET   NTR1                                                                   
         MVC   AIO,AIO3                                                         
         LA    R3,KEY                                                           
         USING NPGRECD,R3                                                       
         XC    KEY,KEY                                                          
         LA    R5,4                 LENGTH OF KEY COMPARE                       
         MVC   NPGKTYP,=XL2'0D20'                                               
         MVC   NPGKAM,BAGYMD                                                    
         MVC   NPGKNET,SVMKT                                                    
         MVC   NPGKPROG,SVPRGCD                                                 
         MVC   NPGKEND,SVSDATE                                                  
*                                                                               
         GOTO1 AIOCALL,DMCB,SPT+DIR+HIGH                                        
         CLC   KEY(11),KEYSAVE                                                  
         BNE   PRGDTEXT                                                         
*                                                                               
         L     R3,AIO                                                           
         GOTO1 AIOCALL,DMCB,SPT+FIL+GET,AIO                                     
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',SPTFILE),(X'92',(R3)),0                        
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,12(R1)                                                        
         USING NPGEL92,R6                                                       
*                                                                               
*  PASS THE RECORD TO THE PC                                                    
*                                                                               
         LHI   R1,X'46'                                                         
         BAS   RE,SENDH                                                         
*  OUTPUT THE DAY                                                               
         LA    R4,DAYTBL                                                        
PRGDT420 CLI   0(R4),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   NPGRDAY,0(R4)                                                    
         BE    PRGDT460                                                         
         LA    R4,5(R4)                                                         
         B     PRGDT420                                                         
PRGDT460 LA    R4,1(R4)                                                         
         LHI   R1,X'01'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
*  OUTPUT THE TIME                                                              
         XC    WORK,WORK                                                        
         GOTO1 VUNTIME,DMCB,NPGTIME,WORK                                        
         LHI   R1,X'02'                                                         
         LA    R4,WORK                                                          
         BAS   RE,SENDD                                                         
*                                                                               
*  OUTPUT THE PROGRAM NAME                                                      
         LHI   R1,X'03'                                                         
         LA    R4,NPGNAME                                                       
         BAS   RE,SENDD                                                         
*                                                                               
*  OUTPUT THE NTI CODE                                                          
         LHI   R1,X'04'                                                         
         LA    R4,NPGPPNO                                                       
         BAS   RE,SENDD                                                         
*                                                                               
PRGDTEXT B     EXIT                                                             
         DROP  R3,R6                                                            
         EJECT                                                                  
*                                                                               
*  ROUTINE READS A PACKAGE RECORD                                               
*                                                                               
PACKDET  NTR1                                                                   
         XC    ERROR,ERROR                                                      
         MVC   AIO,AIO3                                                         
         LA    R3,KEY                                                           
         USING NPRECD,R3                                                        
         XC    KEY,KEY                                                          
         LA    R5,4                 LENGTH OF KEY COMPARE                       
         MVI   NPKTYPE,X'02'                                                    
         MVC   NPKAM,BAGYMD                                                     
         MVC   NPKCLT,BCLT                                                      
         MVC   NPKNET,BNET                                                      
         MVC   NPKEST,BEST                                                      
         MVC   NPKPACK,BPKG                                                     
*                                                                               
         GOTO1 AIOCALL,DMCB,UNT+DIR+HIGH                                        
         CLC   KEY(20),KEYSAVE                                                  
         BE    PACKD50                                                          
         MVI   ERROR+1,PAKERR                                                   
         XC    ERRORMSG,ERRORMSG                                                
         GOTO1 SENDMSG                                                          
         B     PACKDEX                                                          
*                                                                               
PACKD50  L     R3,AIO                                                           
         GOTO1 AIOCALL,DMCB,UNT+FIL+GET,AIO                                     
*                                                                               
         L     R3,AIO                                                           
*                                                                               
         MVI   BYTE,C'N'                                                        
         TM    NPAKSTAT,X'02'                                                   
         BZ    *+8                                                              
         MVI   BYTE,C'Y'                                                        
*                                                                               
*  PASS THE RECORD TO THE PC                                                    
*                                                                               
PACKD100 LHI   R1,X'5D'                                                         
         BAS   RE,SENDH                                                         
*                                                                               
*  OUTPUT THE AUDIT STATUS                                                      
         LA    R4,BYTE                                                          
         LHI   R1,X'01'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         CLI   SVFRSW,C'Y'                                                      
         BNE   PACKDEX                                                          
         LA    R4,NPAKSTAT                                                      
         LHI   R1,X'02'                                                         
         BAS   RE,SENDD                                                         
         B     PACKDEX                                                          
*                                                                               
PACKDEX  B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
*  ROUTINE VALIDATES THE REP CODE                                               
*                                                                               
VALREP   NTR1                                                                   
         CLI   QMMREP,X'40'           CHECK INPUT                               
         BNH   VALREPEX                                                         
*                                                                               
         XC    ERROR,ERROR                                                      
         MVC   AIO,AIO3                                                         
         LA    R3,KEY                                                           
         USING REPREC,R3                                                        
         XC    KEY,KEY                                                          
         MVI   REPKTYPE,C'R'                                                    
         MVI   REPKMED,C'N'                                                     
         MVC   REPKREP,QMMREP                                                   
         MVC   REPKAGY,QAGY                                                     
         XC    QMMREP,QMMREP          CLEAR REP                                 
*                                                                               
         GOTO1 AIOCALL,DMCB,STA+FIL+HIGH,AIO                                    
         CLC   KEY(7),KEYSAVE                                                   
         BE    VALREPEX                                                         
*                                                                               
         MVI   ERROR+1,REPERR                                                   
         XC    ERRORMSG,ERRORMSG                                                
         GOTO1 SENDMSG                                                          
*                                                                               
VALREPEX B     EXIT                                                             
         DROP  R3                                                               
*                                                                               
*  ROUTINE GETS NTI PROGRAM NAME                                                
*                                                                               
NTIPROG  NTR1                                                                   
*                                                                               
*  PASS THE RECORD TO THE PC                                                    
*                                                                               
*                                                                               
*  OUTPUT THE NTI PROGRAM NAME                                                  
*                                                                               
         LHI   R1,X'55'                                                         
         BAS   RE,SENDH                                                         
         MVC   DUB(8),=C'PROGNAME'                                              
         LA    R4,DUB                                                           
         LHI   R1,X'01'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
*  OUTPUT THE NTI CODE                                                          
*                                                                               
         MVC   DUB(6),=C'JAN/88'                                                
         LA    R4,DUB                                                           
         LHI   R1,X'02'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
NTIPROGX B     EXIT                                                             
         EJECT                                                                  
*=================================================================*             
* ON ENTRY R1 CONTAINS HEADER CODE                                *             
*=================================================================*             
         SPACE 1                                                                
SENDH    LR    R0,RE                                                            
         GOTO1 GETHDR              GET HEADER ADDRESS                           
         GOTO1 ASETELEM,DMCB,AFABLK,HDRADDR,0,0                                 
         SR    R5,R5               CLEAR LENGTH OVERRIDE                        
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 1                                                                
*===============================================================*               
* PARMS ARE FABLK,MAP_TABLE_ENTRY,A(DATA),OVRD_LEN              *               
* ON ENTRY R1 CONTAINS DATA ITEM NUMBER WITHIN CURRENT ELEMENT  *               
*===============================================================*               
         SPACE 1                                                                
SENDD    LR    R0,RE                                                            
         GOTO1 GETDATA             GET DATA ITEM                                
         GOTO1 AADDDATA,DMCB,AFABLK,DATADDR,(R4),(R5)                           
         SR    R5,R5               CLEAR OVERRIDE LENGTH                        
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 1                                                                
*                                                                               
         EJECT                                                                  
*  ROUTINE BUILDS DEFAULT WIZARD UNIT DATA                                      
*                                                                               
*  R2 -> WIZCLIENT TABLE                                                        
*  R4 -> WIZ CLIENT                                                             
*  R5 -> LENGTH OF DATA                                                         
*                                                                               
*                                                                               
*                                                                               
WIZDATA  NTR1                                                                   
         LA    R2,SVWIZCLT               TABLE OF CLIENTS                       
         USING WIZCLTD,R2                                                       
         CLI   0(R2),0             IS LIST EMPTY?                               
         BE    WIZDATAX            YES                                          
*                                                                               
         CLC   WIZCLT,=CL3'ALL'    ALL CLIENT REQUEST                           
         BE    WIZDTA20                                                         
         LHI   R1,X'62'            NO-SEND HEADER                               
         BAS   RE,SENDH                                                         
         LA    R3,L'SVWIZCLT/LNWIZCLT    MAX NUMBER OF CLIENTS                  
WIZDTA5  MVC   CURRCLT,WIZBCLT           THIS CLIENT                            
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),WIZBCLT          GET CURRENT CLIENT CODE                
         GOTO1 AIOCALL,DMCB,SPT+DIR+HIGH                                        
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIOCALL,DMCB,SPT+FIL+GET,AIO1                                    
*                                                                               
         L     R3,AIO1                                                          
         BAS   RE,WIZIT                  SEND CLT/PRD/EST                       
         LA    R2,LNWIZCLT(R2)           BUMP TO NEXT CLIENT                    
         CLI   0(R2),0                                                          
         BE    WIZDATAX                                                         
         LHI   R1,X'62'            SEND NEW HEADER WITH EACH CLIENT             
         BAS   RE,SENDH                                                         
         BCT   R3,WIZDTA5                                                       
         B     WIZDATAX                                                         
*                                                                               
*  "ALL" CLIENT LOGIC"                                                          
*                                                                               
WIZDTA20 DS    0H                                                               
         MVI   ALLCLSW,C'Y'                                                     
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
WIZDTA30 GOTO1 AIOCALL,DMCB,SPT+DIR+HIGH                                        
         CLC   KEY(2),KEYSAVE                                                   
         BNE   WIZDATAX                                                         
         CLC   KEY+4(9),KEYSAVE+4                                               
         BNE   WIZDATAX                                                         
*                                                                               
         LA    R3,KEY                                                           
         USING CLTHDR,R3                                                        
*        GOTO1 AIOCALL,DMCB,SPT+FIL+GET,AIO1                                    
         MVC   BCLT,CKEYCLT       VALICLT NEEDS QCLT                            
         OI    WIZSTAT,X'05'                                                    
         GOTO1 VALICLT            GETS CLT REC INOT SVCLTREC                    
         L     R3,AIO                                                           
         MVC   CURRCLT,CKEYCLT           THIS CLIENT                            
         TM    WIZSTAT,X'02'                                                    
         BZ    WIZDTA40                                                         
         NI    WIZSTAT,X'FD'      RESET WIZSTAT                                 
         CLC   ERROR,=AL2(55)     SECURITY ERROR                                
         BE    WIZDTA50           GET NEXT CLIENT                               
         GOTO1 SENDMSG            ELSE SEND MESSAGE                             
*                                                                               
WIZDTA40 LHI   R1,X'62'            NO-SEND HEADER                               
         BAS   RE,SENDH                                                         
         BAS   RE,WIZIT                                                         
WIZDTA50 XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         SR    RE,RE                                                            
         MVC   HALF,CURRCLT                                                     
         LH    RE,HALF                                                          
         LA    RE,1(RE)                                                         
         STCM  RE,3,HALF                                                        
         MVC   KEY+2(2),HALF             GET NEXT CLIENT                        
         B     WIZDTA30                                                         
*                                                                               
WIZDATAX XIT1                                                                   
         SPACE 2                                                                
*                                                                               
* EXPECTS R2->SVWIZ CLIENT LIST                                                 
*                                                                               
WIZIT    NTR1                                                                   
         CLI   SVFRSW,C'F'          CHECK FRONTRUNNER                           
         BE    WIZD38                                                           
*                                                                               
         USING CLTHDR,R3                                                        
* PASS CLIENT CODE                                                              
         GOTO1 VCLUNPK,DMCB,(CPROF+6,CKEYCLT),WIZCLT                            
         LHI   R1,X'01'                                                         
         LA    R4,WIZCLT                                                        
         SR    R5,R5               CLEAR LENGTH OVERRIDE                        
         BAS   RE,SENDD                                                         
* PASS CLIENT NAME                                                              
         LA    R5,20                                                            
         LA    R6,CNAME                                                         
         LA    R6,19(R6)           POINT R6 TO END OF CLIENT NAME               
WIZD20   CLI   0(R6),X'40'                                                      
         BH    WIZD30                                                           
         BCTR  R6,0                                                             
         BCTR  R5,0                                                             
         LTR   R5,R5                                                            
         BNZ   WIZD20                                                           
         DC    H'0'                NO PRISONERS                                 
WIZD30   LHI   R1,X'02'                                                         
         LA    R4,CNAME                                                         
         BAS   RE,SENDD            R5 HAS LENGTH                                
* PASS OFFICE CODE                                                              
         LHI   R1,X'03'                                                         
         SR    R5,R5               CLEAR LENGTH OVERRIDE                        
         LA    R4,COFFICE                                                       
         BAS   RE,SENDD                                                         
* PASS ECOST COST TYPE                                                          
         LHI   R1,X'13'                                                         
         LA    R4,KEY                                                           
         MVI   KEY,0                                                            
         TM    COPT3,COP3T                                                      
         BZ    *+8                                                              
         MVI   KEY,C'T'                                                         
         TM    COPT3,COP3TI                                                     
         BZ    *+8                                                              
         MVI   KEY,C'I'                                                         
         TM    COPT4,COP4TIS                                                    
         BZ    *+8                                                              
         MVI   KEY,C'S'                                                         
         BAS   RE,SENDD                                                         
* PASS ROTATION START DAY                                                       
         LHI   R1,X'2A'                                                         
         LA    R4,CSCJROT                                                       
         BAS   RE,SENDD                                                         
* PASS EDI CLIENT AND USER ID                                                   
         BRAS  RE,PASSEDI                                                       
*                                                                               
         DROP  R3                                                               
* PASS N0,N1,N2 PROFILES                                                        
         XC    WORK2(64),WORK2                                                  
         XC    KEY,KEY             GET USER PROFILE INTO NBUSE                  
         MVC   KEY(4),=C'S0N0'                                                  
         MVC   KEY+4(2),QAGY                                                    
         MVI   KEY+6,C'N'                                                       
         MVC   KEY+7(3),WIZCLT                                                  
         MVI   KEY+10,C'*'                                                      
         MVC   KEY+11(1),WIZCLTO                                                
         GOTO1 VGETPROF,DMCB,KEY,WORK2,VDATAMGR    N0 PROFILE                   
         MVI   KEY+3,C'1'                     GET N1 PROFILE                    
         GOTO1 VGETPROF,DMCB,KEY,WORK2+16,VDATAMGR                              
         MVI   KEY+3,C'2'                     GET N2 PROFILE                    
         GOTO1 VGETPROF,DMCB,KEY,WORK2+32,VDATAMGR                              
         MVC   KEY+2(2),=CL2'MM'              APPLICATION PROFILE               
         CLI   SVFRSW,C'M'          IS MATCHMAKER RUNNING                       
         BE    WIZD35                                                           
         MVC   KEY+2(2),=CL2'SW'              APPLICATION PROFILE               
         CLI   SVFRSW,C'Y'          IS STEWARD RUNNING                          
         BE    WIZD35                                                           
         MVC   KEY+2(2),=CL2'FR'              APPLICATION PROFILE               
WIZD35   GOTO1 VGETPROF,DMCB,KEY,WORK2+48,VDATAMGR                              
         LHI   R6,X'04'                                                         
         LA    R3,WORK2                                                         
         BAS   RE,SENDPRF                                                       
         LHI   R6,X'05'                                                         
         LA    R3,WORK2+16                                                      
         BAS   RE,SENDPRF                                                       
         LHI   R6,X'06'                                                         
         LA    R3,WORK2+32                                                      
         BAS   RE,SENDPRF                                                       
         CLI   SVFRSW,X'40'                                                     
         BNH   WIZD38                                                           
         LHI   R6,X'12'             APPLICATION PROFILE                         
         LA    R3,WORK2+48                                                      
         BAS   RE,SENDPRF                                                       
         B     WIZD38                                                           
*                                                                               
*  PASS PRODUCT GROUP RECORD INFO (STEWARD ONLY)                                
*                                                                               
WIZD38   CLI   SVFRSW,C'Y'          IS STEWARD RUNNING                          
         BNE   WIZD40                                                           
         CLC   VERSION,=X'05000009'    VERSION 5.00.00.09 AND HIGHER            
         BL    WIZD40                                                           
         BAS   RE,GETPGRPS          GET PRODUCT GROUP INFO (STEWARD)            
*                                                                               
* NOW DEAL WITH PRODUCTS                                                        
* EITHER PASS ONLY THOSE IN SVWIZPRD LIST OR ALL ON CLIENT RECORD               
*                                                                               
WIZD40   DS    0H                 GET CLIENT RECORD                             
         CLI   ALLCLSW,C'Y'       ALL CLIENT DID THIS CALL ALREADY              
         BE    WIZD41                                                           
         MVC   QCLT,WIZCLT        VALICLT NEEDS QCLT                            
         GOTO1 VALICLT            GETS CLT REC INOT SVCLTREC                    
WIZD41   L     R3,AIO                                                           
         USING CLTHDR,R3                                                        
*                                                                               
* CHANGE REGARDLESS OF REQUEST ALWAY PASS ALL THE PRODUCTS.                     
*                                                                               
****     CLC   QPRD,=C'POL'        ..IF POL                                     
****     BE    WIZD44                                                           
****     CLI   SVWIZPRD,0          ..IF BLANK                                   
****     BNE   WIZD60                                                           
         MVC   QPRD,=C'POL'                                                     
*                                                                               
WIZD44   XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING PRDHDR,R6                                                        
         MVC   PLSTTYPE(2),=XL2'0DF1'                                           
         MVC   PLSTAM,CKEYAM                                                    
         MVC   PLSTCLT,CKEYCLT                                                  
         GOTO1 AIOCALL,DMCB,SPT+DIR+HIGH                                        
         B     WIZD46                                                           
WIZD45   GOTO1 AIOCALL,DMCB,SPT+DIR+SEQ                                         
*                                                                               
WIZD46   CLC   KEY(5),KEYSAVE      CHECK UP TILL CLIENT                         
         BNE   WIZD70              GO CHECK ESTIMATES                           
         GOTO1 AIOCALL,DMCB,SPT+FIL+GET,AIO2                                    
*                                                                               
         L     R6,AIO2                                                          
         MVC   QPRD,PKEYPRD                                                     
         MVC   SVPNAME,PNAME                                                    
         CLI   SVFRSW,C'F'                                                      
         BNE   WIZD46A                                                          
         BAS   RE,FMTPRFR          PASS PRODUCTS WITH RELAX FILM CODE           
         OC    WORK(3),WORK                                                     
         BNZ   WIZD46B                                                          
         B     WIZD46M                                                          
WIZD46A  BAS   RE,FMTPROD          GET NAME/FORMAT OUTPUT                       
WIZD46B  LA    R4,WORK             CODE/NAME IN WORK/R5->LENGTH                 
         LHI   R1,X'07'                                                         
         BAS   RE,SENDD                                                         
         LA    R4,PLOCK            PASS LOCK STATUS                             
         LHI   R1,X'2B'                                                         
         BAS   RE,SENDD                                                         
* PASS GROUP INFORMATION                                                        
         CLI   SVFRSW,C'Y'             GROUP INFO ONLY FOR STEWARD              
         BNE   WIZD46G                                                          
         CLC   VERSION,=X'05000009'    VERSION 5.00.00.09 AND HIGHER            
         BL    WIZD46G                                                          
         MVC   WORK(9),PGRP1                                                    
         MVC   WORK+9(6),PGRP4                                                  
         MVC   WORK+15(15),PGRP6                                                
         MVI   WORK+30,X'FF'                                                    
         LA    R6,WORK                                                          
WIZD46D  CLI   0(R6),X'FF'                                                      
         BE    WIZD46G                                                          
         CLI   0(R6),X'40'                                                      
         BNH   WIZD46F                                                          
         OC    1(2,R6),1(R6)                                                    
         BZ    WIZD46F                                                          
         LA    R4,0(R6)                                                         
         LA    R1,X'1C'                                                         
         BAS   RE,SENDD             SEND GROUP ID                               
         OI    2(R6),X'0C'          SET SIGN FOR PACK                           
         MVC   WORK2(2),1(R6)                                                   
         EDIT  (P2,WORK2),(3,WORK2+2),FILL=0,ZERO=NOBLANK,ALIGN=LEFT,  X        
               WRK=WORK+40                                                      
         LA    R4,WORK2+2                                                       
         LHI   R1,X'1D'                                                         
         BAS   RE,SENDD             SEND GROUP NUMBER (SCHEME)                  
WIZD46F  LA    R6,3(R6)                                                         
         B     WIZD46D                                                          
WIZD46G  L     R6,AIO2              RESET PRODUCT RECORD POINTER                
WIZD46M  B     WIZD45              BUMP TO NEXT PROD                            
         DROP  R6                                                               
*                                  ..PASS ALL PRODUCTS IN CLIST                 
***WIZD44   CLI   CLIST,0             ANY PRODUCTS FOR CLIENT                   
***         BE    WIZD100                                                       
***         LA    R6,CLIST                                                      
***         LA    R3,220              MAX PRODUCTS                              
***WIZD45   CLI   0(R6),0             NO MORE PRODS?                            
***         BE    WIZD70                                                        
***         MVC   QPRD,0(R6)          THIS PROD CODE                            
***         CLI   SVFRSW,C'F'                                                   
***         BNE   WIZD45A                                                       
***         BAS   RE,FMTPRFR          PASS PRODUCTS WITH RELAX FILM CDE         
***         OC    WORK(3),WORK                                                  
***         BNZ   WIZD45B                                                       
***         B     WIZD45C                                                       
***WIZD45A  BAS   RE,FMTPROD          GET NAME/FORMAT OUTPUT                    
***WIZD45B  LA    R4,WORK             CODE/NAME IN WORK/R5->LENGTH              
***         LHI   R1,X'07'                                                      
***         BAS   RE,SENDD                                                      
***WIZD45C  LA    R6,4(R6)            BUMP TO NEXT PROD                         
***         BCT   R3,WIZD45                                                     
**** CHECK SECOND PRODUCT TABLE                                                 
***         LR    R3,RA                                                         
***         AHI   R3,(SVCLTREC-TWAD)   RESET CLIENT POINTER                     
***         LA    R6,CLIST2                                                     
***         LA    R3,35               MAX PRODUCTS                              
***WIZD48   CLI   0(R6),0             NO MORE PRODS?                            
***         BE    WIZD70                                                        
***         MVC   QPRD,0(R6)          THIS PROD CODE                            
***         CLI   SVFRSW,C'F'                                                   
***         BNE   WIZD48A                                                       
***         BAS   RE,FMTPRFR          PASS PRODUCTS WITH RELAX FILM CDE         
***         OC    WORK(3),WORK                                                  
***         BNZ   WIZD48B                                                       
***         B     WIZD48C                                                       
***WIZD48A  BAS   RE,FMTPROD          GET NAME/FORMAT OUTPUT                    
***WIZD48B  LA    R4,WORK             CODE/NAME IN WORK/R5->LENGTH              
***         LHI   R1,X'07'                                                      
***         BAS   RE,SENDD                                                      
***WIZD48C  LA    R6,4(R6)            BUMP TO NEXT PROD                         
***         BCT   R3,WIZD48                                                     
***         B     WIZD70                                                        
*                                                                               
* STEP THROUGH PRODUCTS IN LIST AND SEND THOSE VALID FOR CLIENT                 
***WIZD60   LA    R6,SVWIZPRD                                                   
***         LA    R3,SVWZPRD#         NUMBER OF PRODS                           
***WIZD65   BAS   RE,PRODMTCH         FIND MATCHING PRODUCT                     
***         BNE   WIZD66              (RETURNS R4-> PROD)                       
***         MVC   QPRD,0(R4)                                                    
***         BAS   RE,FMTPROD          GET NAME/FORMAT OUTPUT                    
***         LA    R4,WORK             WORK HAS CODE/NAME,R5->LENGTH             
***         LHI   R1,X'07'                                                      
***         BAS   RE,SENDD                                                      
***WIZD66   LA    R6,3(R6)                                                      
***         CLI   0(R6),0             EOF?                                      
***         BE    WIZD70                                                        
***         BCT   R3,WIZD65                                                     
***         B     WIZD70              GO DO ESTIMATES                           
***         DROP  R3                                                            
*                                                                               
***FMTPROD  NTR1                    FORMAT PRODUCT CODE/NAME                    
***         MVC   AIO,AIO2                                                      
***         GOTO1 VALIPRD                                                       
*                                                                               
*  PASS FLIGHT RECORD INFORMATION (STEWARD ONLY)                                
*                                                                               
WIZD70   DS    0H                                                               
         CLI   SVFRSW,C'Y'          IS STEWARD RUNNING                          
         BNE   WIZD71                                                           
         CLC   VERSION,=X'05000016'    VERSION 5.00.00.22 AND HIGHER            
         BL    WIZD71                                                           
         BAS   RE,GETFLITS          GET FLIGHT RECORDS                          
*                                                                               
*  PASS BARRLUES RECORD IF REQUESTED                                            
*                                                                               
WIZD71   DS    0H                                                               
         TM    WIZSTAT,X'08'        IS BARRULES RECORDS REQUESTED               
         BZ    WIZD73                                                           
         OC    BARCLI,BARCLI                                                    
         BNZ   *+8                                                              
         BRAS  RE,GETBRRLS          PASS AGENCY LAVE RECORD                     
         MVC   BARCLI,CURRCLT                                                   
         BRAS  RE,GETBRRLS          PASS CLIENT LEVEL RECORD                    
*                                                                               
*  PASS FEED RECORD IF REQUESTED                                                
*                                                                               
WIZD73   DS    0H                                                               
         CLI   SVFRSW,C'Y'          IS STEWARD RUNNING                          
         BNE   WIZD74                                                           
         OC    FEEDCLI,FEEDCLI                                                  
         BNZ   *+8                                                              
         BRAS  RE,GETFEEDS          PASS DEFAULT FEED RECORDS                   
         MVC   FEEDCLI,CURRCLT                                                  
         BRAS  RE,GETFEEDS          PASS CLIENT LEVEL RECORD                    
* NOW DEAL WITH ESTIMATES                                                       
* EITHER SEND ALL OR FROM REQUESTED LIST                                        
WIZD74   DS    0H                                                               
         CLI   SVFRSW,C'F'          IF FRONTRUNNER EXIT                         
         BE    WIZD200                                                          
         MVI   WIZEST,0            CLEAR EST FLAG                               
         LA    R6,SVWIZEST         POINT TO EST TABLE                           
         CLI   0(R6),0             RETURN ALL ESTIMATES?                        
         BNE   WIZD86              NO                                           
* FIRST DO POL ESTIMATE LOOP                                                    
         LA    R6,1                YES                                          
         LA    RF,255                                                           
WIZD75   STC   R6,CURREST                                                       
         BAS   RE,SENDEST                                                       
         LA    R6,1(R6)                                                         
         BCT   RF,WIZD75                                                        
         B     WIZD90                                                           
*                                                                               
*  REQUESTED ESTIMATE LOGIC                                                     
WIZD86   EQU   *                   USE TABLE OF REQUESTED EST                   
         LA    RF,20                                                            
WIZD87   MVC   CURREST,0(R6)       SET ESTIMATE                                 
         BAS   RE,SENDEST                                                       
         LA    R6,1(R6)            BUMP TO NEXT ESTIMATE                        
         CLI   0(R6),0                                                          
         BE    WIZD90                                                           
         BCT   RF,WIZD87                                                        
WIZD90   CLC   VERSION,=X'03010001'    VERSION 3.01.00.01 AND HIGHER            
         BNH   *+8                                                              
         BAS   RE,SNDEPST          PASS DETAIL ESTIMATE INFO                    
****     CLI   WIZEST,0            RETURN NO DATA IF NO MATCH                   
****     BNE   WIZD100                                                          
****     MVC   ERROR,=AL2(237)                                                  
****     GOTO1 SENDMSG                                                          
         EJECT                                                                  
* NOW DEAL WITH CLIENT LEVEL DAYPARTS                                           
WIZD100  CLC   VERSION,=X'01000061'    VERSION 1.00.00.97 AND HIGHER            
         BL    WIZD200                                                          
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING NDPTHDR,R6                                                       
*                                                                               
         MVC   NDPTKEY(2),=XL2'0D07'                                            
         MVC   NDPTAGM,BAGYMD                                                   
         MVC   NDPTCLT,CURRCLT                                                  
         GOTO1 AIOCALL,DMCB,UNT+DIR+HIGH                                        
         B     WIZD140                                                          
WIZD120  GOTO1 AIOCALL,DMCB,UNT+DIR+SEQ                                         
*                                                                               
WIZD140  CLC   KEY(5),KEYSAVE      CHECK UP TILL CLIENT                         
         BNE   WIZD200                                                          
*                                                                               
         LA    R4,NDPTDPTA         DAYPART CODE                                 
         LHI   R1,X'0E'                                                         
         BAS   RE,SENDD                                                         
         LA    R4,NDPTDES          DAYPART NAME                                 
         LHI   R1,X'0F'                                                         
         BAS   RE,SENDD                                                         
         LA    R4,NDPTDPTE         DAYPART MAP EQUATE                           
         LHI   R1,X'10'                                                         
         BAS   RE,SENDD                                                         
         B     WIZD120                                                          
*                                                                               
WIZD200  XIT1                                                                   
         DROP  R3,R6                                                            
         EJECT                                                                  
*                                                                               
* R3->PROFILE STRING                                                            
* R1->MAP CODE                                                                  
SENDPRF  NTR1                                                                   
         LA    R4,WORK             OUTPUT AREA                                  
         LA    R2,16               # OF PROFILES                                
SENDP10  LA    R5,1                                                             
         CLI   0(R3),X'40'         SEND BLANKS                                  
         BE    SENDP15                                                          
         CLI   0(R3),C'*'          SEND ASTERISK                                
         BE    SENDP15                                                          
         CLI   0(R3),C'A'          SEND ALPHAS                                  
         BNL   SENDP15                                                          
* ASSUME NUMERIC IF LESS THAN C'A'                                              
         EDIT  (B1,0(R3)),(2,WORK+20),ALIGN=LEFT,ZERO=NOBLANK                   
         MVC   WORK(3),WORK+20                                                  
         LR    R5,R0               R0 = # OF SIGNIFICANT CHARACTERS             
         B     SENDP20                                                          
*                                                                               
SENDP15  MVC   WORK(1),0(R3)                                                    
*                                                                               
SENDP20  LR    R1,R6             R1 GETS TRASHED BY SENDD                       
         BAS   RE,SENDD                                                         
         LA    R3,1(R3)                                                         
         BCT   R2,SENDP10                                                       
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
* PRODUCT FORMAT ROUTINE                                                        
*                                                                               
FMTPROD  NTR1                    FORMAT PRODUCT CODE/NAME                       
         MVC   WORK(3),QPRD                                                     
         MVC   WORK+3(20),SVPNAME                                               
         LA    R5,23               DROP END BLANKS                              
         LA    R4,WORK                                                          
         LA    R4,22(R4)                                                        
FMTPRD5  CLI   0(R4),X'40'                                                      
         BH    FMTPRDX                                                          
         BCTR  R4,0                                                             
         BCTR  R5,0                                                             
         CHI   R5,3                                                             
         BNE   FMTPRD5                                                          
FMTPRDX  XIT1  REGS=(R5)           PASS BACK LENGTH                             
*                                                                               
* SPECIAL FRONTRUNNER PRODUCT FORMAT                                            
* PASS PRODUCTS WHO HAVE RELAX FILM CODE SET                                    
***FMTPRFR  NTR1                    FORMAT PRODUCT CODE/NAME                    
***         LA    R5,3                                                          
***         MVC   AIO,AIO2                                                      
***         GOTO1 VALIPRD                                                       
FMTPRFR  NTR1                    FORMAT PRODUCT CODE/NAME                       
         LA    R5,3                                                             
         L     RE,AIO2                                                          
         USING PRDHDR,RE                                                        
         XC    WORK(3),WORK                                                     
         TM    POPT1,POPT1_RFC                                                  
         BZ    FMTPRFX                                                          
         MVC   WORK(3),QPRD                                                     
         DROP  RE                                                               
FMTPRFX  XIT1  REGS=(R5)           PASS BACK LENGTH                             
*                                                                               
*                                                                               
* EXPECTS R6 -> TO PRODUCT TO BE MATCHED                                        
* EXPECTS CLIENT RECORD IN AIO1                                                 
PRODMTCH NTR1                                                                   
         L     R5,AIO4                                                          
         USING CLTHDR,R5                                                        
         LA    R4,CLIST                                                         
         LA    R3,220                                                           
PRODM10  CLC   0(3,R4),0(R6)                                                    
         BNE   PRODM12                                                          
* ADD PRODUCT TO CURRPRDS LIST                                                  
         LA    R1,CURRPRDS         PRODS VALID FOR CURR CLIENT                  
PRODM11  CLI   0(R1),X'FF'                                                      
         BE    PRODM11B                                                         
         CLI   0(R1),0                                                          
         BE    PRODM11B                                                         
         LA    R1,3(R1)                                                         
         B     PRODM11                                                          
PRODM11B MVC   0(3,R1),0(R4)       ADD PROD TO LIST                             
         MVI   3(R1),X'FF'         SET EOF                                      
         SR    RE,RE               SET CC EQUAL                                 
         B     PRODMX                                                           
*                                                                               
PRODM12  LA    R4,4(R4)                                                         
         BCT   R3,PRODM10                                                       
*                                                                               
* CHECK SECOND PRODUCT TABLE                                                    
         LA    RE,CLIST2                                                        
         CR    R4,RE                ARE WE LOOKING AT CLIST2                    
         BH    PRODMX               ALREADY CHECKED CLIST2 EXIT                 
         LA    R4,CLIST2                                                        
         LA    R3,35                                                            
         B     PRODM10                                                          
*                                                                               
PRODMX   LTR   RE,RE                                                            
         XIT1  REGS=(R4)                                                        
         DROP  R5                                                               
         EJECT                                                                  
*                                                                               
* NOW DEAL WITH PRODUCT GROUPS                                                  
* SEND PRODUCT GROUP INFO (STEWARD ONLY)                                        
*                                                                               
GETPGRPS NTR1                                                                   
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING PRGRECD,R6                                                       
         MVC   PRGKTYP(2),=XL2'0D01'                                            
         MVC   PRGKAGMD,BAGYMD                                                  
         MVC   PRGKCLT,CURRCLT                                                  
         GOTO1 AIOCALL,DMCB,SPT+DIR+HIGH                                        
         B     GETPG20                                                          
GETPG15  GOTO1 AIOCALL,DMCB,SPT+DIR+SEQ                                         
*                                                                               
GETPG20  CLC   KEY(5),KEYSAVE      CHECK UP TILL CLIENT                         
         BNE   GETPGEX             EXIT ROUTINE                                 
         GOTO1 AIOCALL,DMCB,SPT+FIL+GET,AIO2                                    
*                                                                               
         L     R6,AIO2                                                          
         OC    PRGKGRP,PRGKGRP      CHECK LEVEL OF GROUP                        
         BNZ   GETPG80                                                          
*                                                                               
* PROCESS TITLE LEVEL INFO                                                      
         LA    R4,PRGKID            SEND THE GROUP ID                           
         LA    R1,X'17'                                                         
         BAS   RE,SENDD             SEND GROUP ID                               
*                                                                               
         USING PRGEL01,R3                                                       
         GOTO1 VHELLO,DMCB,(C'G',SPTFILE),(X'01',(R6)),0                        
         CLI   12(R1),0                                                         
         BNE   GETPG15                                                          
         L     R3,12(R1)                                                        
*                                                                               
         CLI   PRGBK1LN,0                                                       
         BE    GETPG40                                                          
         LA    R4,PRGBK1                                                        
         LHI   R1,X'18'                                                         
         BAS   RE,SENDD             SEND TITLE 1                                
         LA    R4,PRGBK1LN                                                      
         LHI   R1,X'19'                                                         
         BAS   RE,SENDD             SEND TITLE 1 LENGTH                         
GETPG40  CLI   PRGBK2LN,0                                                       
         BE    GETPG41                                                          
         LA    R4,PRGBK2                                                        
         LHI   R1,X'18'                                                         
         BAS   RE,SENDD             SEND TITLE 2                                
         LA    R4,PRGBK2LN                                                      
         LHI   R1,X'19'                                                         
         BAS   RE,SENDD             SEND TITLE 2 LENGTH                         
GETPG41  CLI   PRGBK3LN,0                                                       
         BE    GETPG15              GET NET RECORD                              
         LA    R4,PRGBK3                                                        
         LHI   R1,X'18'                                                         
         BAS   RE,SENDD             SEND TITLE 3                                
         LA    R4,PRGBK3LN                                                      
         LHI   R1,X'19'                                                         
         BAS   RE,SENDD             SEND TITLE 3 LENGTH                         
         B     GETPG15              GET NEXT RECORD                             
         DROP  R3                                                               
*                                                                               
* PROCESS BREAK LEVEL GROUP                                                     
*                                                                               
GETPG80  MVC   WORK2(2),PRGKGRP                                                 
         OI    WORK2+1,X'0C'         SET UP AS PACKED NUMBER                    
         EDIT  (P2,WORK2),(3,WORK2+2),FILL=0,ZERO=NOBLANK,ALIGN=LEFT            
         LA    R4,WORK2+2                                                       
         LHI   R1,X'1A'                                                         
         BAS   RE,SENDD             SEND GROUP NUMBER (SCHEME)                  
*                                                                               
         USING PRGEL10,R3                                                       
         GOTO1 VHELLO,DMCB,(C'G',SPTFILE),(X'10',(R6)),0                        
         CLI   12(R1),0                                                         
         BNE   GETPG15                                                          
         L     R3,12(R1)                                                        
*                                                                               
         CLI   PRGNAM1,0                                                        
         BE    GETPG90                                                          
         LA    R4,PRGNAM1                                                       
         LHI   R1,X'1B'                                                         
         BAS   RE,SENDD             SEND TITLE 1                                
GETPG90  CLI   PRGNAM2,0                                                        
         BE    GETPG91                                                          
         LA    R4,PRGNAM2                                                       
         LHI   R1,X'1B'                                                         
         BAS   RE,SENDD             SEND TITLE 2 LENGTH                         
GETPG91  CLI   PRGNAM3,0                                                        
         BE    GETPG15              GET NET RECORD                              
         LA    R4,PRGNAM3                                                       
         LHI   R1,X'1B'                                                         
         BAS   RE,SENDD             SEND TITLE 3                                
         B     GETPG15              GET NEXT RECORD                             
         DROP  R3                                                               
*                                                                               
GETPGEX  XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* NOW DEAL WITH FLIGHT RECORDS (STEWARD ONLY)                                   
*                                                                               
GETFLITS NTR1                                                                   
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING WFLIGHTD,R6                                                      
         MVC   WP1TYP(2),=XL2'0E8C'                                             
         MVC   WP1AM,BAGYMD                                                     
         MVC   WP1CLT,CURRCLT                                                   
         GOTO1 AIOCALL,DMCB,XSP+DIR+HIGH                                        
         B     GETFL20                                                          
GETFL15  GOTO1 AIOCALL,DMCB,XSP+DIR+SEQ                                         
*                                                                               
GETFL20  CLC   KEY(5),KEYSAVE      CHECK UP TILL CLIENT                         
         BNE   GETFLEX             EXIT ROUTINE                                 
         OC    WP1WFID,WP1WFID                                                  
         BZ    GETFL15                                                          
         TM    WFKCNTRL,WFKINACT    TEST FOR INACTIVE                           
         BO    GETFL15                                                          
*                                                                               
         LA    R4,WP1PRD                                                        
         LHI   R1,X'1F'                                                         
         BAS   RE,SENDD             FLIGHT PRODUCT                              
         LA    R4,WP1FTYPE                                                      
         LHI   R1,X'20'                                                         
         BAS   RE,SENDD             FLIGHT TYPE                                 
         LA    R4,WP1DPT                                                        
         LHI   R1,X'21'                                                         
         BAS   RE,SENDD             FLIGHT DAYPART                              
         LA    R4,WP1STD                                                        
         LHI   R1,X'22'                                                         
         BAS   RE,SENDD             FLIGHT START DATE                           
         LA    R4,WP1END                                                        
         LHI   R1,X'23'                                                         
         BAS   RE,SENDD             FLIGHT END DATE                             
         LA    R4,WP1WFID                                                       
         LHI   R1,X'24'                                                         
         BAS   RE,SENDD             FLIGHT NUMBER                               
         LA    R4,WP1IFLT                                                       
         LHI   R1,X'25'                                                         
         BAS   RE,SENDD             FLIGHT INTERNAL FLIGHT NUMBER               
*                                                                               
         B     GETFL15                                                          
*                                                                               
GETFLEX  XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*  SEND ESTIMAE INFORMATION                                                     
*                                                                               
SENDEST  NTR1                                                                   
         CLC   QPRD,=C'POL'                                                     
         BNE   *+12                                                             
SENDE00  BAS   RE,SENDE04                                                       
         B     SENDEX                                                           
                                                                                
* NOT POL - CHECK THAT AT LEAST ONE PRODUCT SATISFIES                           
*           DATE RESTRICTIONS                                                   
         LA    R2,CURRPRDS                                                      
SENDE01  CLI   0(R2),X'FF'         EOF?                                         
         BE    SENDEX                                                           
         CLI   0(R2),0             EOF?                                         
         BE    SENDEX                                                           
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),CURRCLT                                                 
         MVC   KEY+4(3),0(R2)      PRODUCT                                      
         MVC   KEY+7(1),CURREST    ESTIMATE                                     
         GOTO1 AIOCALL,DMCB,SPT+DIR+HIGH                                        
         CLC   KEY(13),KEYSAVE                                                  
         BNE   SENDE02                                                          
         L     R3,AIO1                                                          
         GOTO1 AIOCALL,DMCB,SPT+FIL+GET,AIO1                                    
         USING ESTHDR,R3                                                        
         CLI   SVSDATE,0           ANY DATE RESTRICTIONS?                       
         BE    SENDE00                                                          
         GOTO1 VDATCON,DMCB,ESTART,(2,FULL)                                     
         GOTO1 VDATCON,DMCB,EEND,(2,FULL+2)                                     
         CLC   SVEDATE,FULL        CHECK DATE RESTRICTIONS                      
         BL    SENDE02                                                          
         CLC   SVSDATE,FULL+2                                                   
         BH    *+8                                                              
         B     SENDE00             OK- SEND DATA                                
SENDE02  LA    R2,3(R2)            BUMP TO NEXT PROD                            
         B     SENDE01                                                          
*                                                                               
*                                                                               
* SEND POL ESTIMATE DATA FOR CURRENT CLI/ESTIMATE                               
SENDE04  NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),CURRCLT                                                 
         MVC   KEY+4(3),=C'POL'    PRODUCT                                      
         MVC   KEY+7(1),CURREST    ESTIMATE                                     
         GOTO1 AIOCALL,DMCB,SPT+DIR+HIGH                                        
         CLC   KEY(13),KEYSAVE                                                  
         BNE   SENDEX                                                           
         L     R3,AIO1                                                          
         GOTO1 AIOCALL,DMCB,SPT+FIL+GET,AIO1                                    
         USING ESTHDR,R3                                                        
         CLI   SVSDATE,0           ANY DATE RESTRICTIONS?                       
         BE    SENDE05                                                          
         GOTO1 VDATCON,DMCB,ESTART,(2,FULL)                                     
         GOTO1 VDATCON,DMCB,EEND,(2,FULL+2)                                     
         CLC   SVEDATE,FULL        CHECK DATE RESTRICTIONS                      
         BL    SENDEX                                                           
         CLC   SVSDATE,FULL+2                                                   
         BH    SENDEX                                                           
*                                                                               
SENDE05  DS    0H                                                               
         MVI   WIZEST,1            SET EST FLAG                                 
         EDIT  (B1,EKEYEST),(3,WORK2),ALIGN=LEFT  SEND EST #                    
         LA    R5,3                                                             
         CLI   WORK2+2,X'40'       DROP BLANKS                                  
         BH    SENDE07                                                          
         BCTR  R5,0                                                             
         CLI   WORK2+1,X'40'                                                    
         BH    SENDE07                                                          
         BCTR  R5,0                                                             
SENDE07  LA    R4,WORK2                                                         
         LHI   R1,X'08'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R6,EDESC            SEND EST DESCRIPTION                         
         LA    R6,19(R6)                                                        
         LA    R5,20                                                            
SENDE10  CLI   0(R6),X'40'                                                      
         BH    SENDE20                                                          
         BCTR  R5,0                                                             
         BCTR  R6,0                                                             
         CHI   R5,X'01'                                                         
         BNE   SENDE10                                                          
SENDE20  LHI   R1,X'09'                                                         
         LA    R4,EDESC            SEND EST DESCRIPTION                         
         BAS   RE,SENDD                                                         
                                                                                
         L     R2,AIO2                                                          
         USING DEMOWRK,R2                                                       
         MVC   ESTDEMO1,EDEMLST                                                 
         MVC   ESTDEMO2,EDEMLST1                                                
         MVC   ESTDEMO3,EDEMLST2                                                
         XC    ESTDEMON,ESTDEMON                                                
****     LA    R2,WORK+20          SEND DEMO LIST                               
****     XC    WORK,WORK                                                        
****     MVC   0(L'EDEMLST,R2),EDEMLST    20 DEMOS                              
****     LA    R2,L'EDEMLST(R2)           BUMP                                  
****     MVC   0(3,R2),EDEM21             21ST DEMO                             
****     LA    R2,WORK+20                 REPOSITION R2                         
*                                                                               
         LA    R4,BLOCK            INIT  DBLOCK                                 
         USING DBLOCKD,R4                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'NAD'                                                   
         MVI   DBSELMED,C'N'                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         XC    ESTDEMRT(250),ESTDEMRT            11 POS DEMO NAMES              
         XC    ESTDEMRT+250(200),ESTDEMRT                                       
                                                                                
         LA    R5,EUSRNMS                                                       
         GOTO1 VDEMOCON,DMCB,(50,(R2)),(13,ESTDEMRT),(C'S',DBLOCK),(R5)*        
               ,ENONTDMS                                                        
******   GOTO1 VDEMOCON,DMCB,(21,(R2)),(13,WORK2),(C'S',DBLOCK),(R5)            
                                                                                
*******  LA    R2,WORK+20          R2->DEMO LIST INPUT                          
         LA    R6,ESTDEMRT         R6->DEMO DESCRIPTION OUTPUT                  
SENDE25  CLI   1(R2),0             LAST DEMO?                                   
         BNH   SENDE30                                                          
         CLC   VERSION,=X'03000005'    VERSION 3.00.00.06 AND HIGHER            
         BH    SENDE28             ALLOW NADS, AND USER DEMOS                   
         CLI   0(R2),0             CHECK FOR NAD                                
         BNE   SENDE29             SKIP IT                                      
         CLI   1(R2),33            CHECK FOR USER                               
         BE    SENDE29             SKIP IT                                      
SENDE28  CLI   2(R2),1             HOMES?                                       
         BNE   *+12                                                             
         CLI   0(R2),0             REGULAR HOMES?                               
         BE    SENDE29             SKIP IT                                      
                                                                                
*****    BAS   RE,CHKUSER          CHECK USER DEMOS                             
         BRAS  RE,FMTDEMO          FORMAT DESCRIPTION                           
*                                                                               
         CLI   WORK,10             MAX LENGTH OF DEMO                           
         BNH   *+8                                                              
         MVI   WORK,10                                                          
*                                                                               
         ZIC   R5,WORK             WORK=LENGTH                                  
         LA    R4,WORK+1           WORK+1=DEMO DESCRIPTION                      
         LHI   R1,X'0A'                                                         
         BAS   RE,SENDD                                                         
SENDE29  LA    R2,3(R2)            NEXT DEMO IN BLOCK                           
         LA    R6,11(R6)           NEXT OUPUT DEMO DESCRIPTION                  
         B     SENDE25                                                          
         DROP  R2                                                               
* SEND INDICATOR IF HOMES IS THE TARGET DEMO POL ESTIMATES ONLY                 
SENDE30  CLC   VERSION,=X'05000009'    VERSION 5.00.00.09 AND HIGHER            
         BL    SENDE35                                                          
         CLC   EKEYPRD,=CL3'POL'                                                
         BNE   SENDE35                                                          
         CLI   EDEMLST,0            CHECK FOR NAD                               
         BNE   SENDE35                                                          
         CLI   EDEMLST+2,1          CHECK FOR HOMES                             
         BNE   SENDE35                                                          
         SR    R5,R5                                                            
         MVI   WORK,C'Y'                                                        
         LA    R4,WORK                                                          
         LHI   R1,X'1E'                                                         
         BAS   RE,SENDD             SEND HOMES IS TARGET DEMO INDICATOR         
*                                                                               
SENDE35  DS    0H                                                               
         SR    R5,R5                                                            
         XC    WORK,WORK                                                        
         GOTO1 VDATCON,DMCB,ESTART,(2,WORK)                                     
         LA    R4,WORK                                                          
         LHI   R1,X'0B'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         GOTO1 VDATCON,DMCB,EEND,(2,WORK)                                       
         LA    R4,WORK                                                          
         LHI   R1,X'0C'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         MVI   WORK,C'Y'           ESTIMATE LOCKED?                             
         TM    ECNTRL,X'08'                                                     
         BO    *+8                                                              
         MVI   WORK,C'N'                                                        
         LA    R4,WORK                                                          
         LHI   R1,X'0D'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
SENDEX   XIT1                                                                   
         DROP  R3,R4                                                            
                                                                                
         EJECT                                                                  
*                                                                               
* PASS ALL PRODUCT ESTIMATE DETAIL INFO TO STEWARD                              
*                                                                               
SNDPEST  NTR1                                                                   
         XC    HOLDPROD,HOLDPROD   CLEAR BYPASS SWITCH                          
         MVC   ESTPROD,CURRPRDS                                                 
SNDP005  XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),CURRCLT                                                 
         CLC   QPRD,=C'POL'                                                     
         BE    *+16                                                             
         MVC   KEY+4(3),ESTPROD    PRODUCT                                      
         MVC   KEY+7(1),CURREST    ESTIMATE                                     
         GOTO1 AIOCALL,DMCB,SPT+DIR+HIGH                                        
         B     SNDP020                                                          
SNDP010  GOTO1 AIOCALL,DMCB,SPT+DIR+SEQ                                         
SNDP020  CLC   KEY(4),KEYSAVE                                                   
         BNE   SNDPEX                                                           
         OC    KEY+8(5),KEY+8       CHECK ESTIMATE RECORD                       
         BNZ   SNDP010                                                          
         CLC   KEY+4(3),=CL3'POL'   BYPASS POL                                  
         BE    SNDP010                                                          
*                                                                               
         CLC   QPRD,=C'POL'                                                     
         BE    SNDP060                                                          
* CHECK ESTIMATE RECORD AGAINST PRODUCT LIST                                    
         CLC   ESTPROD,KEY+4                                                    
         BE    SNDP060                                                          
* BUMP TO NEXT PRODUCT IN THE TABLE                                             
         LA    RE,CURRPRDS                                                      
         LA    RF,10                                                            
SNDP030  CLC   ESTPROD,0(RE)        COMPARE TO CURRENT PRODUCT                  
         BE    SNDP035                                                          
         LA    RE,3(RE)                                                         
         BCT   RF,SNDP030                                                       
         B     SNDPEX               EXIT. ALL PRODUCTS PROCESSED                
SNDP035  LA    RE,3(RE)                                                         
         CLI   0(RE),X'FF'                                                      
         BE    SNDPEX               EXIT, ALL PRODUCTS PROCESSED                
         MVC   ESTPROD,0(RE)        GET NEXT PRODUCT                            
         B     SNDP005              READ ESTIMATES                              
* CHECK TO SEE IF ESTIMATE NUMBER MATCH                                         
SNDP060  CLC   KEY+7(1),CURREST                                                 
         BNE   SNDP010                                                          
*                                                                               
         GOTO1 AIOCALL,DMCB,SPT+FIL+GET,AIO1                                    
         L     R3,AIO1                                                          
         USING ESTHDR,R3                                                        
*                                                                               
*  DATE FILTER                                                                  
         CLI   SVSDATE,0           ANY DATE RESTRICTIONS?                       
         BE    SNDP100                                                          
         GOTO1 VDATCON,DMCB,ESTART,(2,FULL)                                     
         GOTO1 VDATCON,DMCB,EEND,(2,FULL+2)                                     
         CLC   SVEDATE,FULL        CHECK DATE RESTRICTIONS                      
         BL    SNDP010                                                          
         CLC   SVSDATE,FULL+2                                                   
         BH    SNDP010                                                          
*                                                                               
* PRODUCT NAME (ONLY ONCE)                                                      
*                                                                               
SNDP100  CLC   HOLDPROD,KEY+4                                                   
         BE    SNDP140                                                          
         MVC   HOLDPROD,KEY+4                                                   
         LHI   R1,X'14'                                                         
         LA    R4,HOLDPROD         SEND PRODUCT                                 
         BAS   RE,SENDD                                                         
*                                                                               
* ESTIMATE NUMBER                                                               
*                                                                               
SNDP140  LHI   R1,X'15'                                                         
         LA    R4,KEY+7            SEND EST NUMBER                              
         BAS   RE,SENDD                                                         
*                                                                               
* TARGET DEMO (OWNERSHIP REPORT ONLY)                                           
*                                                                               
         CLI   SVBRNDOW,C'Y'       PASS TARGET ONLY FOR BRAND OWNRSHIP          
         BNE   SNDP300                                                          
*                                                                               
         LA    R2,WORK+20          SEND DEMO LIST                               
         XC    WORK,WORK                                                        
         MVC   0(3,R2),EDEMLST     1 DEMO DEFAULT                               
         CLC   VERSION,=X'06000032'    VERSION 6.00.00.50                       
         BL    *+10                                                             
         MVC   0(9,R2),EDEMLST     3 DEMOS                                      
*                                                                               
         LA    R4,BLOCK            INIT  DBLOCK                                 
         USING DBLOCKD,R4                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'NAD'                                                   
         MVI   DBSELMED,C'N'                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         XC    WORK2(250),WORK2    OUTPUT FOR 11 CHAR DEMO NAMES                
                                                                                
         LA    R5,EUSRNMS                                                       
         GOTO1 VDEMOCON,DMCB,(1,(R2)),(13,WORK2),(C'S',DBLOCK),(R5)             
         CLC   VERSION,=X'06000032'    VERSION 6.00.00.50                       
         BL    SNDP145                                                          
         GOTO1 VDEMOCON,DMCB,(3,(R2)),(13,WORK2),(C'S',DBLOCK),(R5)             
*                                                                               
SNDP145  LA    R2,WORK+20          R2->DEMO LIST INPUT                          
         LA    R6,WORK2            R6->DEMO DESCRIPTION OUTPUT                  
*                                                                               
SNDP150  CLI   0(R6),0                                                          
         BE    SNDP300                                                          
         BRAS  RE,FMTDEMO          FORMAT DESCRIPTION                           
         ZIC   R5,WORK             WORK=LENGTH                                  
         LA    R4,WORK+1           WORK+1=DEMO DESCRIPTION                      
         LHI   R1,X'16'                                                         
         BAS   RE,SENDD                                                         
         AHI   R6,11                                                            
         B     SNDP150                                                          
*                                                                               
SNDP300  B     SNDP010                                                          
*                                                                               
SNDPEX   XIT1                                                                   
         DROP  R3,R4                                                            
                                                                                
         EJECT                                                                  
*                                                                               
* PASS ALL PRODUCT ESTIMATE DETAIL INFO TO STEWARD                              
*                                                                               
SNDEPST  NTR1                                                                   
         XC    HOLDPROD,HOLDPROD   CLEAR BYPASS SWITCH                          
         MVC   ESTPROD,CURRPRDS                                                 
SNDE005  XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),CURRCLT                                                 
         GOTO1 AIOCALL,DMCB,SPT+DIR+HIGH                                        
         B     SNDE020                                                          
SNDE010  GOTO1 AIOCALL,DMCB,SPT+DIR+SEQ                                         
SNDE020  CLC   KEY(4),KEYSAVE                                                   
         BNE   SNDEEX                                                           
         OC    KEY+8(5),KEY+8       CHECK ESTIMATE RECORD                       
         BNZ   SNDE010                                                          
         CLI   KEY+7,0              CHECK ESTIMATE RECORD                       
         BZ    SNDE010                                                          
         CLC   KEY+4(3),=CL3'POL'   BYPASS POL                                  
         BE    SNDE010                                                          
*                                                                               
         CLC   QPRD,=C'POL'         CHECK PRODUCT FILTER                        
         BE    SNDE060                                                          
* CHECK ESTIMATE RECORD AGAINS PRODUCT TABLE                                    
         LA    RE,CURRPRDS                                                      
         LA    RF,10                                                            
SNDE030  CLI   0(RE),X'FF'          END OF TABLE                                
         BE    SNDE010              BYPASS                                      
         CLC   0(4,RE),KEY+4        CHECK PRODUCT MATCH                         
         BE    SNDE060                                                          
         LA    RE,3(RE)                                                         
         BCT   RF,SNDE030                                                       
         B     SNDE010              NO MATCH, BYPASS                            
* CHECK ESTIMATE RECORD AGAINS ESTIMATE TABLE                                   
SNDE060  CLI   SVWIZEST,0           CHECK ESTIMATE FILTER                       
         BE    SNDE080                                                          
*                                                                               
         LA    RE,SVWIZEST                                                      
         LA    RF,20                                                            
SNDE070  CLC   0(1,RE),KEY+7        CHECK ESTIMATE                              
         BE    SNDE080                                                          
         LA    RE,1(RE)                                                         
         BCT   RF,SNDE070                                                       
         B     SNDE010              NO MATCH, BYPASS                            
*                                                                               
SNDE080  GOTO1 AIOCALL,DMCB,SPT+FIL+GET,AIO1                                    
         L     R3,AIO1                                                          
         USING ESTHDR,R3                                                        
*                                                                               
*  DATE FILTER                                                                  
         CLI   SVSDATE,0           ANY DATE RESTRICTIONS?                       
         BE    SNDE100                                                          
         GOTO1 VDATCON,DMCB,ESTART,(2,FULL)                                     
         GOTO1 VDATCON,DMCB,EEND,(2,FULL+2)                                     
         CLC   SVEDATE,FULL        CHECK DATE RESTRICTIONS                      
         BL    SNDE010                                                          
         CLC   SVSDATE,FULL+2                                                   
         BH    SNDE010                                                          
*                                                                               
* PRODUCT NAME (ONLY ONCE)                                                      
*                                                                               
SNDE100  CLC   HOLDPROD,KEY+4                                                   
         BE    SNDE140                                                          
         MVC   HOLDPROD,KEY+4                                                   
         LHI   R1,X'14'                                                         
         LA    R4,HOLDPROD         SEND PRODUCT                                 
         BAS   RE,SENDD                                                         
*                                                                               
* ESTIMATE NUMBER                                                               
*                                                                               
SNDE140  LHI   R1,X'15'                                                         
         LA    R4,KEY+7            SEND EST NUMBER                              
         BAS   RE,SENDD                                                         
*                                                                               
* TARGET DEMO (OWNERSHIP REPORT ONLY)                                           
*                                                                               
         CLI   SVBRNDOW,C'Y'       PASS TARGET ONLY FOR BRAND OWNRSHIP          
         BNE   SNDE300                                                          
*                                                                               
         LA    R2,WORK+20          SEND DEMO LIST                               
         XC    WORK,WORK                                                        
         MVC   0(3,R2),EDEMLST     1 DEMO DEFAULT                               
         CLC   VERSION,=X'06000032'    VERSION 6.00.00.50                       
         BL    *+10                                                             
         MVC   0(9,R2),EDEMLST     3 DEMOS                                      
*                                                                               
         LA    R4,BLOCK            INIT  DBLOCK                                 
         USING DBLOCKD,R4                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'NAD'                                                   
         MVI   DBSELMED,C'N'                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         XC    WORK2(250),WORK2    OUTPUT FOR 11 CHAR DEMO NAMES                
                                                                                
         LA    R5,EUSRNMS                                                       
         GOTO1 VDEMOCON,DMCB,(1,(R2)),(13,WORK2),(C'S',DBLOCK),(R5)             
         CLC   VERSION,=X'06000032'    VERSION 6.00.00.50                       
         BL    SNDE145                                                          
         GOTO1 VDEMOCON,DMCB,(3,(R2)),(13,WORK2),(C'S',DBLOCK),(R5)             
*                                                                               
SNDE145  LA    R2,WORK+20          R2->DEMO LIST INPUT                          
         LA    R6,WORK2            R6->DEMO DESCRIPTION OUTPUT                  
*                                                                               
SNDE150  CLI   0(R6),0                                                          
         BE    SNDE300                                                          
         BRAS  RE,FMTDEMO          FORMAT DESCRIPTION                           
         ZIC   R5,WORK             WORK=LENGTH                                  
         LA    R4,WORK+1           WORK+1=DEMO DESCRIPTION                      
         LHI   R1,X'16'                                                         
         BAS   RE,SENDD                                                         
         AHI   R6,11                                                            
         B     SNDE150                                                          
*                                                                               
SNDE300  B     SNDE010                                                          
*                                                                               
SNDEEX   XIT1                                                                   
         DROP  R3,R4                                                            
                                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
*         CHKUSER                                                     *         
***********************************************************************         
* ROUTINE TO CHECK AND OUTPUT USER DEMOS                              *         
* ... R2 POINTS TO 3 BYTE DEMO ... R6 POINTS TO WORK2 OUTPUT FIELD    *         
***********************************************************************         
CHKUSER  NTR1                                                                   
         CLC   VERSION,=X'03000006'    VERSION 3.00.00.06 AND HIGHER            
         BL    CKUSREX                                                          
         USING ESTHDR,R3                                                        
         CLI   1(R2),33             CHECK USER DEMO                             
         BNE   CKUSREX                                                          
*                                                                               
         MVC   0(11,R6),SPACES      CLEAR OUTPUT                                
         ZIC   RE,2(R1)             USER NUMBER                                 
         BCTR  RE,0                                                             
         LA    RF,EUSRNMS                                                       
*                                                                               
CKUSR30  MVC   0(7,R6),0(RF)                                                    
         BCT   RE,CKUSR30                                                       
*                                                                               
CKUSREX  XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
                                                                                
*                                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
SPTFILE  DC    CL8'SPTFILE'                                                     
UNTFILE  DC    CL8'UNTFILE'                                                     
CTFILE   DC    CL8'CTFILE'                                                      
XSPFILE  DC    CL8'XSPFILE'                                                     
GNDDIR   DC    CL8'GENDIR '                                                     
GNDFILE  DC    CL8'GENFILE'                                                     
*                                                                               
*                                                                               
PENDTAB  DC    XL1'4',CL8'STWP    '                                             
         DC    XL1'6',CL8'GMMPTR  '                                             
         DC    XL1'6',CL8'GMMVLA  '                                             
         DC    XL1'5',CL8'DDSLA   '                                             
         DC    XL1'5',CL8'GMMLL   '                                             
         DC    XL1'7',CL8'GMMLLNS '                                             
         DC    XL1'5',CL8'SJGMP   '                                             
         DC    XL1'4',CL8'SMGD    '                                             
         DC    X'FF'                                                            
*                                                                               
* AGENCIES CAN ONLY SET APPROVALS                                               
*                                                                               
APPRTAB  DC    XL1'4',CL8'STWM    '                                             
         DC    XL1'5',CL8'GMMNY   '                                             
         DC    XL1'5',CL8'SJGMM   '                                             
         DC    X'FF'                                                            
         EJECT                                                                  
ORIGTAB  DC    XL1'04',CL8'GMMCH   '         AGENCY GMMCH ID=9144               
         DC    XL1'05',CL8'FDMDET  '                                            
         DC    XL1'05',CL8'FDMYRC  '                                            
         DC    XL1'04',CL8'FDMOM   '                                            
         DC    XL1'04',CL8'FDMVO   '                                            
         DC    XL1'05',CL8'FDMWBD  '                                            
         DC    XL1'04',CL8'FDMGS   '                                            
         DC    XL1'05',CL8'FDMGSC  '                                            
         DC    XL1'05',CL8'MFIMED  '                                            
         DC    XL1'03',CL8'MCMV    '                                            
         DC    XL1'03',CL8'SMGD    '                                            
         DC    XL1'04',CL8'DFZLA   '                                            
         DC    XL1'03',CL8'OMDLAKE '                                            
         DC    XL1'05',CL8'PHDDEP  '                                            
         DC    X'FF'                                                            
         EJECT                                                                  
LENTAB   DS    0CL1                                                             
         DC    X'05'                 5 SEC                                      
         DC    X'07'                 7 SEC                                      
         DC    X'0A'                10 SEC                                      
         DC    X'0F'                15 SEC                                      
         DC    X'14'                20 SEC                                      
         DC    X'1E'                30 SEC                                      
         DC    X'23'                35 SEC                                      
         DC    X'2D'                45 SEC                                      
         DC    X'3C'                60 SEC                                      
         DC    X'4B'                75 SEC                                      
         DC    X'5A'                90 SEC                                      
         DC    X'69'                105 SEC                                     
         DC    X'78'                120 SEC                                     
         DC    X'96'                150 SEC                                     
         DC    X'F0'                240 SEC                                     
LENNUM   EQU   (*-LENTAB)/L'LENTAB                                              
         EJECT                                                                  
SPECHTAB DS    0CL24                                                            
         DC    CL1'A',CL2'AD',CL20'ADMIN               ',CL1'N'                 
         DC    CL1'C',CL2'BC',CL20'BARTER ACTUAL       ',CL1'Y'                 
         DC    CL1'F',CL2'BA',CL20'BARTER ASSIGNED     ',CL1'Y'                 
         DC    CL1'B',CL2'BO',CL20'BLACKOUT            ',CL1'N'                 
         DC    CL1'S',CL2'CS',CL20'COPY SPLIT          ',CL1'N'                 
         DC    CL1'U',CL2'CI',CL20'CUT-IN              ',CL1'N'                 
         DC    CL1'D',CL2'DA',CL20'DELIVERY ADJUSTMENT ',CL1'N'                 
         DC    CL1'Q',CL2'EC',CL20'EARNED COST ADJ     ',CL1'Y'                 
         DC    CL1'L',CL2'LC',CL20'LATE CHARGE         ',CL1'N'                 
         DC    CL1'O',CL2'OT',CL20'OTHER CHARGE        ',CL1'N'                 
         DC    CL1'E',CL2'SE',CL20'SECTIONAL           ',CL1'N'                 
         DC    CL1'X',CL2'TX',CL20'TAX                 ',CL1'N'                 
         DC    CL1'M',CL2'TC',CL20'TRADE CREDIT        ',CL1'N'                 
         DC    CL1'V',CL2'TS',CL20'TRADE SAVINGS       ',CL1'N'                 
SPECHNM  EQU   (*-SPECHTAB)/L'SPECHTAB                                          
         EJECT                                                                  
HISTTAB  DS    0CL15                                                            
         DC    CL1'B',CL14'BRAND         '                                      
         DC    CL1'L',CL14'LENGTH        '                                      
         DC    CL1'A',CL14'ACTUAL COST   '                                      
         DC    CL1'D',CL14'DATE          '                                      
         DC    CL1'T',CL14'TIME          '                                      
         DC    CL1'N',CL14'PROGRAM NAME  '                                      
         DC    CL1'R',CL14'ROTATION      '                                      
         DC    CL1'G',CL14'MAKEGOOD      '                                      
         DC    CL1'M',CL14'MISSED        '                                      
         DC    CL1'P',CL14'PREEMPT       '                                      
         DC    CL1'C',CL14'COMMENT       '                                      
HISTNM   EQU   (*-HISTTAB)/L'HISTTAB                                            
         EJECT                                                                  
DEMOBAD  DS    0CL1                                                             
         DC    AL1(02)                                                          
         DC    AL1(03)                                                          
         DC    AL1(04)                                                          
*********DC    AL1(05)                                                          
*********DC    AL1(06)                                                          
*********DC    AL1(07)                                                          
         DC    AL1(08)                                                          
         DC    AL1(09)                                                          
         DC    AL1(10)                                                          
         DC    AL1(11)                                                          
         DC    AL1(12)                                                          
         DC    AL1(13)                                                          
         DC    AL1(14)                                                          
         DC    AL1(15)                                                          
         DC    AL1(16)                                                          
         DC    AL1(17)                                                          
***      DC    AL1(18)                                                          
         DC    AL1(19)                                                          
         DC    AL1(63)                                                          
         DC    AL1(64)                                                          
****     DC    AL1(65)                                                          
         DC    AL1(66)                                                          
         DC    AL1(163)                                                         
         DC    AL1(164)                                                         
         DC    AL1(165)                                                         
         DC    AL1(166)                                                         
         DC    AL1(171)                                                         
         DC    AL1(178)                                                         
         DC    AL1(195)                                                         
         DC    AL1(196)                                                         
         DC    AL1(198)                                                         
         DC    AL1(199)                                                         
         DC    AL1(207)                                                         
         DC    AL1(208)                                                         
         DC    AL1(209)                                                         
         DC    AL1(227)                                                         
         DC    AL1(228)                                                         
         DC    AL1(229)                                                         
         DC    AL1(230)                                                         
         DC    AL1(231)                                                         
         DC    AL1(232)                                                         
         DC    AL1(233)                                                         
         DC    AL1(234)                                                         
         DC    AL1(235)                                                         
         DC    AL1(236)                                                         
         DC    AL1(240)                                                         
         DC    AL1(241)                                                         
         DC    AL1(242)                                                         
         DC    AL1(243)                                                         
DMBNUM   EQU   (*-DEMOBAD)/L'DEMOBAD                                            
*                                                                               
*  TABLE HAS THE SWEEP DATES FOR A GIVEN YEAR                                   
*  THIS TABLE WILL REQUIRE UPDATING ON A YEARLY BASIS                           
*                                                                               
SWEEPTAB DC    XL4'CB61CB7C'       NOV01/01-NOV28/01                            
         DC    XL4'CC3FCC5B'       JAN31/02-FEB27/02                            
         DC    XL4'CC99CCB6'       APR25/02-MAY22/02                            
         DC    XL4'CCEBCD07'       JUL11/02-AUG07/02                            
         DC    XL4'CD5FCD7B'       OCT31/02-NOV27/02                            
         DC    XL4'CE3ECE5A'       JAN30/03-FEB26/03                            
         DC    XL4'CE98CEB5'       APR24/03-MAY21/03                            
         DC    XL4'CEEACF06'       JUL10/03-AUG06/03                            
         DC    XL4'CF5ECF7A'       OCT30/03-NOV26/03                            
         DC    XL4'D045D063'       FEB05/04-MAR03/04                            
         DC    XL4'D09DD0BA'       APR29/04-MAY26/04                            
         DC    XL4'D0E8D104'       JUL08/04-AUG04/04                            
         DC    XL4'D164D181'       NOV04/04-DEC01/04                            
         DC    XL4'D243D262'       FEB03/05-MAR02/05                            
         DC    XL4'D29CD2B9'       APR28/05-MAY25/05                            
         DC    XL4'D2DED2FB'       JUN30/05-JUL27/05                            
         DC    XL4'D363D37E'       NOV03/05-NOV30/05                            
         DC    XL4'D442D461'       FEB02/06-MAR01/06                            
         DC    XL4'D49BD4B8'       APR27/06-MAY24/06                            
         DC    XL4'D4DDD4FA'       JUN29/06-JUL26/06                            
         DC    XL4'D562D57D'       NOV02/06-NOV29/06                            
         DC    XL4'D641D65C'       FEB01/07-FEB28/07                            
         DC    XL4'D69AD6B7'       APR26/07-MAY23/07                            
         DC    XL4'D6E5D701'       JUL05/07-AUG01/07                            
         DC    XL4'D761D77C'       NOV01/07-NOV28/07                            
         DC    XL4'D83FD85B'       JAN31/08-FEB27/08                            
         DC    XL4'D898D8B5'       APR24/08-MAY21/08                            
         DC    XL4'D8E3D8FE'       JUL03/08-JUL30/08                            
         DC    XL4'D95ED97A'       OCT30/08-NOV26/08                            
         DC    XL4'DA65DA81'       MAR05/09-APR01/09                            
         DC    XL4'DA9EDABB'       APR30/09-MAY27/09                            
         DC    XL4'DAE2DAFD'       JUL02/09-JUL29/09                            
         DC    XL4'DB5DDB79'       OCT29/09-NOV25/09                            
         DC    XL4'00000000'       EMERGENCY SPARE                              
         DC    X'00'                                                            
*                                                                               
DAYTAB   DS    0CL4                                                             
         DC    CL4'MON '                                                        
         DC    CL4'TUE '                                                        
         DC    CL4'WED '                                                        
         DC    CL4'THU '                                                        
         DC    CL4'FRI '                                                        
         DC    CL4'SAT '                                                        
         DC    CL4'SUN '                                                        
         DC    CL4'M-F '                                                        
         DC    CL4'M-SU'                                                        
DAYNUM   EQU   (*-DAYTAB)/L'DAYTAB                                              
*                                                                               
ALLCAB   DS    0CL6                                                             
*****  ++INCLUDE DECABSEQU                                                      
         EJECT                                                                  
         LTORG                                                                  
       ++INCLUDE DENADCATS                                                      
*                                                                               
*  READ CONTROL FILE GET THE ID RECORD AND STORE                                
*  ALPHA ID IN LOCAL STORAGE                                                    
*                                                                               
GTALPHID NTR1  BASE=*,LABEL=*                                                   
         MVC   AIO,AIO3                                                         
         LA    R3,KEY                                                           
         USING CTIREC,R3                                                        
         XC    KEY,KEY                                                          
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKNUM,TWAUSRID                                                 
*                                                                               
         GOTO1 AIOCALL,DMCB,CTL+FIL+HIGH,AIO                                    
         L     R3,AIO                                                           
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AIO                                                           
         GOTO1 VHELLO,DMCB,(C'G',CTFILE),(X'02',(R3)),0                         
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                 MUST BE THERE                               
         L     R6,12(R1)                                                        
         MVC   ALPHID(10),2(R6)                                                 
         J     EXIT                                                             
         LTORG                                                                  
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
*  PASS COMSCORE WEB SERVICE SECURITY                                           
*                                                                               
PASCSSE  NTR1  BASE=*,LABEL=*                                                   
         L     RF,AIO4                                                          
         USING CSWSTABD,RF                                                      
         XC    CSWTOK,CSWTOK                                                    
         XC    CSWRSCD,CSWRSCD                                                  
         XC    CSWRSME,CSWRSME                                                  
         XC    CSWMASH,CSWMASH                                                  
*                                                                               
         GOTOR VGETFACT,DMCB,0                                                  
         L     R1,DMCB                                                          
         USING FACTSD,R1                                                        
*                                                                               
         LA    R2,KEY                                                           
         USING TOKKEY,R2                                                        
         XC    KEY,KEY                                                          
         MVI   TOKKMIN,TOKKMINQ      C'K'                                       
         MVI   TOKKTYP,TOKKRTRK      X'01' - RENTRAK RECORD                     
         MVC   TOKKAAGY,QAGY         AGENCY ALPHA CODE                          
         MVC   TOKKSAGY,FATAGYSC     SECURITY AGENCY CODE                       
         MVI   TOKKSYS,X'03'         NET SYSTEM                                 
         DROP  R1                                                               
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTOR VDATAMGR,DMCB,(0,=C'DMRDHI '),=C'GENDIR ',KEY,KEY                
         CLC   KEY(L'TOKKEY),KEYSAVE                                            
         BNE   PCSSECX                                                          
*                                                                               
*        LHI   R1,X'20'                                                         
*        BAS   RE,SENDH                                                         
*                                                                               
         MVC   AIO,AIO3                                                         
         GOTOR VDATAMGR,DMCB,=C'GETREC',=C'GENFIL ',KEY+36,AIO,WORK             
*                                                                               
         L     R2,AIO3                                                          
         AHI   R2,TOKFIRST             R4=A(1ST ELEMENT)                        
         L     RF,AIO4                                                          
         USING CSWSTABD,RF                                                      
*                                                                               
PCSSEC10 CLI   0(R2),0                 ANY ELEMENTS?                            
         JE    PCSSEC30                                                         
         CLI   0(R2),RTAUTELQ          X'0A' - RENTRAK AUTHOR ELEM?             
         JE    PCSSEC20                                                         
         LLC   R0,1(R2)                CHECK THE NEXT ELEMENT                   
         AR    R2,R0                                                            
         J     PCSSEC10                                                         
*                                                                               
         USING RTAUTHD,R2                                                       
PCSSEC20 CLC   RTAUTID,SPACES          LICENSE ID BETTER BE > SPACES            
         JNH   PCSSEC30                                                         
         L     RF,AIO4                                                          
         MVC   0(L'RTAUTID,RF),RTAUTID     LICENSE ID                           
         MVI   L'RTAUTID(RF),C'|'                                               
         MVC   L'RTAUTID+1(L'RTAUTSEC,RF),RTAUTSEC   SECURITY ID                
         DROP  R2                                                               
*                                                                               
PCSSEC30 GOTOR VGETFACT,DMCB,(X'80',0),F#SSBD                                   
         L     R1,0(R1)                                                         
         MVC   BYTE,F@SSYSFL-F@SSBD(R1)   SYSTEM                                
*                                                                               
         LA    R2,KEY                                                           
         USING SDRRECD,R2                                                       
         XC    KEY,KEY                                                          
         MVI   SDRKMIN,SDRKMINQ    X'5D'                                        
         MVC   SDRKFFL,BYTE                                                     
         MVI   SDRKSYS,X'03'       NET                                          
         MVC   SDRKAPP,=X'0034'    AUDIENCE ESTIMATOR (FAXPEQUS)                
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTOR VDATAMGR,DMCB,(0,=C'DMRDHI '),=C'GENDIR ',KEY,KEY                
         B     PCSSEC45                                                         
*                                                                               
PCSSEC40 MVC   KEYSAVE,KEY                                                      
         GOTOR VDATAMGR,DMCB,(0,=C'DMRSEQ '),=C'GENDIR ',KEY,KEY                
PCSSEC45 CLC   KEY(L'SDRKEY),KEYSAVE                                            
         BNE   PCSSECX                                                          
*                                                                               
         GOTOR VDATAMGR,DMCB,=C'GETREC',=C'GENFIL ',KEY+36,AIO,WORK             
*                                                                               
         USING SDRRECD,R2                                                       
         L     R2,AIO3                                                          
         LA    R2,SDRRFRST                                                      
PCSSEC50 CLI   0(R2),0                                                          
         BE    PCSSECX                                                          
*                                                                               
         USING SDELD,R2                                                         
         CLI   SDEEL,X'02'         RATING SOURCE CONNECTION DATA                
         JNE   PCSSEC55                                                         
         L     RF,AIO4                                                          
         ZIC   R1,SDELEN                                                        
         SHI   R1,SDELEN1Q                                                      
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CSWRSCD(0),SDEDATA                                               
*                                                                               
*        LHI   R1,X'02'                                                         
*        LA    R4,WORK2                                                         
*        BAS   RE,SENDD                                                         
         J     PCSSEC70                                                         
*                                                                               
PCSSEC55 CLI   SDEEL,X'03'         RATING SOURCE METHOD ENDPOINTS               
         JNE   PCSSEC60                                                         
         L     RF,AIO4                                                          
         ZIC   R1,SDELEN                                                        
         SHI   R1,SDELEN1Q                                                      
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CSWRSME(0),SDEDATA                                               
*                                                                               
*        LHI   R1,X'03'                                                         
*        LA    R4,WORK2                                                         
*        BAS   RE,SENDD                                                         
         J     PCSSEC70                                                         
*                                                                               
PCSSEC60 CLI   SDEEL,X'04'         MASHERY KEY FOR RENTRAK API                  
         JNE   PCSSEC65                                                         
         L     RF,AIO4                                                          
         ZIC   R1,SDELEN                                                        
         SHI   R1,SDELEN1Q                                                      
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CSWMASH(0),SDEDATA                                               
*                                                                               
*        LHI   R1,X'04'                                                         
*        LA    R4,WORK2                                                         
*        BAS   RE,SENDD                                                         
         J     PCSSEC70                                                         
*                                                                               
PCSSEC65 CLI   SDEEL,X'05'         NUMBER OF DAYS                               
         JNE   PCSSEC70                                                         
         XC    BYTE,BYTE                                                        
         ZIC   R1,SDELEN                                                        
         SHI   R1,SDELEN1Q                                                      
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BYTE(0),SDEDATA                                                  
*                                                                               
*        LHI   R1,X'05'                                                         
*        LA    R4,BYTE                                                          
*        BAS   RE,SENDD                                                         
         J     PCSSEC70                                                         
*                                                                               
PCSSEC70 ZIC   RF,SDELEN                                                        
         AR    R2,RF                                                            
         B     PCSSEC50                                                         
         DROP  RF                                                               
*                                                                               
PCSSECX  J     EXIT                                                             
         LTORG                                                                  
***********************************************************************         
* CALL DDLINK TO GET SELF-DEFINING RECORD                             *         
*                                                                               
* NOTE:  AIO3 WILL BE CLOBBERED TO READ THE GENERIC SDR                         
*        AIO4 WILL BE CLOBBERED TO READ THE AGENCY SPECIFIC SDR                 
***********************************************************************         
GETSDR   NTR1  BASE=*,LABEL=*                                                   
         GOTO1 RDSDRREC,DMCB,AIO3,0                                             
*                                                                               
         L     RF,ACOMFACS         LOAD FACILITIES OVERLAYS                     
         L     RF,CRECUP-COMFACSD(RF)                                           
         ST    RF,VRECUP                                                        
*****                                                                           
* AGENCY ALPHA SDR OVERRIDE                                                     
*****                                                                           
         L     R2,AIO4                                                          
         XC    0(L'SDRKEY,R2),0(R2)  CLEAR OUT KEY IN IOAREA                    
*                                                                               
         XC    WORK,WORK             SETUP THE SDR FREEFORM KEY                 
         L     R1,ATWA                                                          
         MVC   WORK(L'TWAAGY),TWAAGY-TWATASK(R1)  AGENCY ALPHA                  
         GOTO1 RDSDRREC,DMCB,AIO4,WORK  USING AIO4 NOW                          
*                                                                               
         L     RE,AIO3               NEED THIS CHECK AS GETSDR DOES A           
         L     RF,AIO4                 DEFAULT READ BY CLEARING SDRKFFL         
         CLC   0(SDRKKEY-SDRKEY,RE),0(RF)                                       
         JNE   GTSDRX                                                           
*                                                                               
         BRAS  RE,SDROVRW                                                       
*                                                                               
GTSDRX   J     EXITCC                                                           
**********************                                                          
* READ THE SDR RECORD                                                           
*                                                                               
* ON ENTRY:    PARAM 1            A(IO AREA)                                    
*              PARAM 2            PROGRAM DEFINED FREE FORM IF PROVIDED         
**********************                                                          
RDSDRREC NTR1  LABEL=*                                                          
         L     RE,0(R1)           GET A(IOAREA) TO USE                          
         ST    RE,AIO                                                           
         L     RF,4(R1)           RF CONTAINS A(FREE FORM)                      
*                                                                               
         LA    R3,KEY                                                           
         USING SDRRECD,R3                                                       
         XC    KEY,KEY                                                          
         MVI   SDRKMAJ,SDRKMAJQ    X'00'                                        
         MVI   SDRKMIN,SDRKMINQ    X'5D'                                        
*                                                                               
         MVI   SDRKFFL,TSTSDRQ     DEFAULT TO TST                               
         CLI   SVSYSQ,C'A'         ADV                                          
         JNE   *+8                                                              
         MVI   SDRKFFL,ADVSDRQ                                                  
         CLI   SVSYSQ,C'C'         CSC                                          
         JNE   *+8                                                              
         MVI   SDRKFFL,CSCSDRQ                                                  
         CLI   SVSYSQ,C'Q'         FQA                                          
         JNE   *+8                                                              
         MVI   SDRKFFL,FQASDRQ                                                  
*                                                                               
         MVI   SDRKSYS,NETSDRQ     NET                                          
         MVI   SDRKAPP+1,XPSTWRDQ  STEWARD                                      
*                                                                               
         LTR   RF,RF               ANY FREE FORM?                               
         JZ    RDSDR10                                                          
         MVC   SDRKKEY,0(RF)       USE THE FREE FORM KEY                        
*                                                                               
RDSDR10  MVC   KEYSAVE,KEY                                                      
         GOTOR VDATAMGR,DMCB,=C'DMRDHI',GNDDIR,KEY,KEY,0                        
         CLC   KEY(L'SDRKEY),KEYSAVE  SAME KEY?                                 
         JNE   RDSDRNO                                                          
*                                                                               
         L     R3,AIO                                                           
         GOTOR VDATAMGR,DMCB,=C'GETREC',GNDFILE,KEY+36,AIO,DMWORK               
RDSDRYES J     EXITY                                                            
*                                                                               
RDSDRNO  J     EXITN                                                            
         DROP  R3                                                               
**********************                                                          
* OVERWRITE VALUES IN GENERIC SDR THAT WE FOUND IN OUR SPECIFIC SDR             
**********************                                                          
SDROVRW  NTR1  LABEL=*                                                          
         L     R2,AIO4             LOOK AT THE AGENCY SPECIFIC SDR              
         USING SDRRECD,R2                                                       
         OC    0(2,R2),0(R2)       MAKE SURE WE HAVE A RECORD                   
         JZ    SDROVX              OTHERWISE JUST USE GENERIC SDR               
         LA    R2,SDRRFRST                                                      
SDROV010 CLI   0(R2),0             ANY MORE ELEMENT IN AGY SPECIFIC?            
         JE    SDROVX                                                           
*                                                                               
         L     R6,AIO3                                                          
         LA    R6,SDRRFRST-SDRKEY(R6)  R6 = 1ST ELEMENT IN GENERIC              
SDROV020 CLI   0(R6),0             CAN'T FIND A MATCHING ELEM?                  
         JE    SDROV040            NO, JUST ADD IT TO GENERIC                   
         CLC   0(1,R6),0(R2)                                                    
         JE    SDROV030                                                         
         LLC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         J     SDROV020                                                         
*                                                                               
* DELETE THE ELEM IN GENERIC                                                    
SDROV030 GOTO1 VRECUP,DMCB,(X'FE',AIO3),0(R6),0(R6),=X'002A002007D0'            
*                                                                               
* ADD ELEM IN GENERIC                                                           
SDROV040 GOTO1 VRECUP,DMCB,(X'FE',AIO3),0(R2),0(R6),=X'002A002007D0'            
*                                                                               
         LLC   R1,1(R2)                                                         
         AR    R2,R1                                                            
         J     SDROV010                                                         
*                                                                               
SDROVX   J     EXITCC                                                           
         DROP  R2                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*  ROUTINE READS AND UPDATES INVOICE RECORD                                     
*                                                                               
PROCINV  NTR1  BASE=*,LABEL=*                                                   
         MVI   INVFLG,0                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(4),=C'S0I2'                                                  
         MVC   KEY+4(2),QAGY                                                    
         MVI   KEY+6,C'N'                                                       
         GOTO1 VCLUNPK,DMCB,BCLT,QCLT                                           
         MVC   KEY+7(L'QCLT),QCLT                                               
                                                                                
         GOTO1 VGETPROF,DMCB,KEY,PROFI2,VDATAMGR    I2 PROFILE                  
*                                                                               
         XC    SVINVSTD,SVINVSTD                                                
*                                                                               
         GOTO1 VDATCON,DMCB,(2,SVSDATE),(0,WORK)                                
         GOTO1 VDATCON,DMCB,(X'32',SVSDATE),(0,WORK+6),0                        
         GOTO1 VDATCON,DMCB,(X'32',SVSDATE),(0,WORK+12),1                       
         GOTO1 VGETBROD,DMCB,(1,WORK),WORK2+6,VGETDAY,VADDAY                    
         CLI   0(RF),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
* FIRST READ CALENDAR INVOICE, IF DNE, THEN TRY BROADCAST                       
*                                                                               
         MVC   AIO,AIO3                                                         
         LA    R3,KEY                                                           
         USING SNVKEYD,R3                                                       
         XC    KEY,KEY                                                          
         MVI   SNVKTYPE,SNVKTYPQ         X'0E'                                  
         MVI   SNVKSUB,SNVKSUBQ          X'03'                                  
         MVC   SNVKAM,BAGYMD             AGY/MED                                
         MVC   SNVKCLT,BCLT              CLIENT                                 
         MVC   SNVKSTA,BSTAP             STATION                                
*                                                                               
         GOTO1 VDATCON,DMCB,(0,WORK+12),(3,DUB)                                 
         MVI   DUB+2,X'01'               FIRST TRY CALENDAR                     
         GOTO1 VDATCON,DMCB,(3,DUB),(2,DUB+4)                                   
         XC    DUB+4(2),=X'FFFF'                                                
*                                                                               
         MVC   SNVKMOS,DUB+4             COMPRESSED DATE                        
         MVC   SNVKINV,SVINV             INVOICE #                              
*                                                                               
         GOTO1 AIOCALL,DMCB,XSP+DIR+HIGH+UPDATE                                 
         CLC   KEY(SNVKMINK-SNVKEY),KEYSAVE                                     
         JE    PROCI04                                                          
*                                                                               
PROCI02  MVC   AIO,AIO3                  TRY BROADCAST                          
         LA    R3,KEY                                                           
         USING SNVKEYD,R3                                                       
         XC    KEY,KEY                                                          
         MVI   SNVKTYPE,SNVKTYPQ         X'0E'                                  
         MVI   SNVKSUB,SNVKSUBQ          X'03'                                  
         MVC   SNVKAM,BAGYMD             AGY/MED                                
         MVC   SNVKCLT,BCLT              CLIENT                                 
         MVC   SNVKSTA,BSTAP             STATION                                
*                                                                               
         GOTO1 VDATCON,DMCB,(0,WORK2+12),(3,DUB)                                
         MVI   DUB+2,X'01'               DEFAULT TO BRD MOS                     
         GOTO1 VDATCON,DMCB,(3,DUB),(2,DUB+4)                                   
         XC    DUB+4(2),=X'FFFF'                                                
*                                                                               
         MVC   SNVKMOS,DUB+4             COMPRESSED DATE                        
         MVC   SNVKINV,SVINV             INVOICE #                              
*                                                                               
         GOTO1 AIOCALL,DMCB,XSP+DIR+HIGH+UPDATE                                 
         OI    INVFLG,IFLGBRD                                                   
         J     PROCI04                                                          
*                                                                               
PROCISEQ GOTO1 AIOCALL,DMCB,XSP+DIR+SEQ+UPDATE                                  
*                                                                               
PROCI04  CLC   KEY(SNVKMINK-SNVKEY),KEYSAVE                                     
         BE    PROCI06                                                          
         TM    INVFLG,IFLGBRD                                                   
         JZ    PROCI02                                                          
         DC    H'0'                                                             
*                                                                               
PROCI06  L     R3,AIO                                                           
         GOTO1 AIOCALL,DMCB,XSP+FIL+GET+UPDATE,AIO                              
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',XSPFILE),(X'10',(R3)),0                        
         CLI   12(R1),0                                                         
         BE    PROCI08                                                          
         OC    SVINVSTD,SVINVSTD                                                
         BNZ   PROCI10                                                          
         DC    H'0'                                                             
*                                                                               
PROCI08  L     R6,12(R1)                                                        
         USING SNVHDELD,R6                                                      
         MVC   SVINVSTD,SNVHDSDT                                                
*                                                                               
PROCI10  GOTO1 VDATCON,DMCB,(2,SVINVSTD),(0,WORK+6)                             
         GOTO1 VDATCON,DMCB,(2,SVSDATE),(0,WORK)                                
         GOTO1 VPERVERT,DMCB,WORK+6,WORK                                        
         ZIC   RF,DMCB+9                                                        
         SHI   RF,1                                                             
         STC   RF,BYTE              DAY OFFSET FROM PERIOD START                
*                                                                               
         L     R3,AIO                                                           
         GOTO1 VHELLO,DMCB,(C'G',XSPFILE),(X'40',(R3)),0                        
         CLI   12(R1),0                                                         
         BNE   PROCISEQ                                                         
         L     R6,12(R1)                                                        
         USING SNVIDELD,R6                                                      
*                                                                               
PROCI20  CLI   0(R6),SNVIDELQ                                                   
         BNE   PROCISEQ                                                         
         CLC   SNVIDDAY,BYTE        FOUND MATCH ON DAY?                         
         BNE   PROCI30                                                          
*                                                                               
         MVC   FULL,SNVIDCST        SET COST FROM INVOICE                       
         TM    SNVIDCTL,SNVIDNGQ    NEGATIVE COST?                              
         JZ    PROCI22                                                          
         SR    R0,R0                                                            
         ICM   R1,15,SNVIDCST                                                   
         M     R0,=F'-1'             NEGATE IT TO FIND MATCH                    
         ST    R1,FULL                                                          
*                                                                               
PROCI22  CLC   FULL,SVACTCST        FOUND MATCH ON COST?                        
         BNE   PROCI30                                                          
*                                                                               
         SR    R0,R0                                                            
         ZICM  R1,SNVIDTIM,2                                                    
         D     R0,=F'60'                                                        
         MH    R1,=H'100'                                                       
         AR    R1,R0                                                            
         AH    R1,=H'600'                                                       
         CH    R1,=H'2400'                                                      
         BL    *+8                                                              
         SH    R1,=H'2400'                                                      
         STH   R1,HALF                                                          
*                                                                               
         ZICM  R1,SVTIME,2                                                      
         CH    R1,=H'2400'                                                      
         BL    *+8                                                              
         SH    R1,=H'2400'                                                      
         STH   R1,WORK                                                          
*                                                                               
         CLC   HALF,WORK            FOUND MATCH ON TIME?                        
         BNE   PROCI30                                                          
*                                                                               
         NI    SNVIDCTL,X'FF'-(SNVIDIFQ+SNVIDICQ+SNVIDITQ+SNVIDSIQ)             
         NI    SNVIDCT2,X'FF'-(SNVIDISQ+SNVIDIIQ)                               
*                                                                               
         TM    SVIGNORE,SVIFILMQ    IGNORE FILM?                                
         BZ    *+8                                                              
         OI    SNVIDCTL,SNVIDIFQ                                                
         TM    SVIGNORE,SVISEPQ     IGNORE SEPERATION?                          
         BZ    *+8                                                              
         OI    SNVIDCTL,SNVIDSIQ                                                
         TM    SVIGNORE,SVICOSTQ    IGNORE COST?                                
         BZ    *+8                                                              
         OI    SNVIDCTL,SNVIDICQ                                                
         TM    SVIGNORE,SVITIMEQ    IGNORE TIME?                                
         BZ    *+8                                                              
         OI    SNVIDCTL,SNVIDITQ                                                
         TM    SVIGNORE,SVILENQ     IGNORE LENGTH?                              
         BZ    *+8                                                              
         OI    SNVIDCT2,SNVIDISQ                                                
         TM    SVIGNORE,SVIICSTQ    IGNORE INTEGRATION?                         
         BZ    *+8                                                              
         OI    SNVIDCT2,SNVIDIIQ                                                
*                                                                               
         GOTO1 VGETFACT,DMCB,(X'80',FULL),F#TPERS                               
*                                                                               
         L     R3,AIO                                                           
         GOTO1 VHELLO,DMCB,(C'G',XSPFILE),(X'F1',(R3)),0                        
         CLI   12(R1),0                                                         
         BNE   PROCI25                                                          
         L     R6,12(R1)                                                        
         USING ACTVD,R6                                                         
         GOTO1 VDATCON,DMCB,(5,0),(3,ACTVCHDT)                                  
         MVC   ACTVCHID,FULL                                                    
*                                                                               
PROCI25  GOTO1 AIOCALL,DMCB,XSP+FIL+PUT+UPDATE,AIO                              
         B     PROCINVX                                                         
*                                                                               
PROCI30  ZIC   RF,1(R6)                                                         
         AR    R6,RF                                                            
         B     PROCI20                                                          
*                                                                               
PROCINVX J     EXIT                                                             
         DROP  R3,R6                                                            
*                                                                               
*F#TPERS  EQU   0007,2,C'Y'         PASSWORD NUMBER     XL2                     
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*         FMTDEMO                                                     *         
***********************************************************************         
* ROUTINE TO FORMAT DEMOS ... R6 POINTS TO 10 CHARACTER DESCRIPTION   *         
* ... R2 POINTS TO 3 BYTE DEMO ... WORK(1) RETURNS LENGTH ...         *         
* WORK+1 RETURNS DESCRIPTION                                          *         
***********************************************************************         
FMTDEMO  NTR1  BASE=*,LABEL=*                                                   
         USING ESTHDR,R3                                                        
         MVC   WORK(11),SPACES       INITIALIZE WORK                            
         MVI   WORK,0                                                           
         CLI   0(R6),C' '            IF NO DEMO TO FORMAT ... EXIT              
         BNH   FMTDEMOX                                                         
*                                                                               
         LA    R1,11                                                            
         LA    R4,10(R6)                                                        
FMTD5    CLI   0(R4),C' '            SCAN BACKWARDS FOR NON-SPACE               
         BH    FMTD10                                                           
         BCTR  R4,0                                                             
         BCT   R1,FMTD5                                                         
*                                                                               
FMTD10   STC   R1,WORK               LENGTH OF DEMO INTO WORK                   
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK+1(0),0(R6)       DEMO DESCRIPTION INTO WORK+1               
*                                                                               
         CLI   1(R2),X'21'           IF DOING A USER DEMO, INSERT               
         BNE   FMTD20                USER DEMO HEADER                           
FMTD15   MVC   WORK+11(7),WORK+1                                                
         MVC   WORK+1(3),=C'U /'                                                
         MVC   WORK+4(7),WORK+11                                                
         ZIC   R0,2(R2)              USER NAME NUMBER                           
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+2(1),DUB+7(1)                                               
         IC    R1,WORK               UPDATE LENGTH                              
         AHI   R1,3                                                             
         STC   R1,WORK                                                          
         B     FMTDEMOX                                                         
*                                                                               
FMTD20   CLC   WORK+1(7),EWGTNM                                                 
         BNE   FMTDEMOX                                                         
         MVC   WORK+10(7),WORK+1     IF DEMO MATCHES WEIGHTED DEMO              
         MVC   WORK+1(2),=C'W/'      INSERT HEADER                              
         MVC   WORK+3(7),WORK+10                                                
         IC    R1,WORK               UPDATE LENGTH                              
         AHI   R1,2                                                             
         STC   R1,WORK                                                          
FMTDEMOX J     EXIT                                                             
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* NOW DEAL WITH BARRULES RECORDS                                                
*                                                                               
GETBRRLS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING BRULESD,R6                                                       
         MVC   BRULKTYP(2),=XL2'0E70'                                           
         MVC   BRULAGY,BAGYMD                                                   
         MVC   BRULCLT,BARCLI                                                   
         GOTO1 AIOCALL,DMCB,SPT+DIR+HIGH                                        
         B     GETBR20                                                          
GETBR15  GOTO1 AIOCALL,DMCB,SPT+DIR+SEQ                                         
*                                                                               
GETBR20  CLC   KEY(5),KEYSAVE      CHECK UP TILL CLIENT                         
         BNE   GETBREX             EXIT ROUTINE                                 
*                                                                               
         GOTO1 AIOCALL,DMCB,SPT+FIL+GET,AIO1                                    
         L     R6,AIO1                                                          
*                                                                               
         LA    R4,BR01UAD                                                       
         LHI   R1,X'32'                                                         
         BRAS  RE,SENDD             ASSIGNED DOLLAR SWITCH                      
         LA    R4,BR01RSD                                                       
         LHI   R1,X'33'                                                         
         BRAS  RE,SENDD             ROUND SPLIT DOLLARS                         
         LA    R4,BR01NTI                                                       
         LHI   R1,X'34'                                                         
         BRAS  RE,SENDD             NETWORK TIME RELAXATION INTERVAL            
         LA    R4,BR01CTI                                                       
         LHI   R1,X'35'                                                         
         BRAS  RE,SENDD             CABLE TIME RELAXATION INTERVAL              
         LA    R4,BR01MDS                                                       
         LHI   R1,X'36'                                                         
         BRAS  RE,SENDD             ALLOW MAXIMUM DOUBLE SPOTTING               
         LA    R4,BR01DWP                                                       
         LHI   R1,X'37'                                                         
         BRAS  RE,SENDD             DOLLAR WEIGHT PCTG                          
         LA    R4,BR01TBL                                                       
         LHI   R1,X'38'                                                         
         BRAS  RE,SENDD             TOTAL BUDGET LOWER RANGE                    
         LA    R4,BR01TBU                                                       
         LHI   R1,X'39'                                                         
         BRAS  RE,SENDD             TOTAL BUDGET UPPER RANGE                    
         LA    R4,BR01GBL                                                       
         LHI   R1,X'3A'                                                         
         BRAS  RE,SENDD             GOAL BUDGET LOWER RANGE                     
         LA    R4,BR01GBU                                                       
         LHI   R1,X'3B'                                                         
         BRAS  RE,SENDD             GOAL BUDGET UPPER RANGE                     
         LA    R4,BR01WBL                                                       
         LHI   R1,X'3C'                                                         
         BRAS  RE,SENDD             WEEKLY BUDGET LOWER RANGE                   
         LA    R4,BR01WBU                                                       
         LHI   R1,X'3D'                                                         
         BRAS  RE,SENDD             WEEKLY BUDGET UPPER RANGE                   
         LA    R4,BR01IZU                                                       
         LHI   R1,X'3E'                                                         
         BRAS  RE,SENDD             INCLUDE $0. UNITS                           
*                                                                               
         CLI   BR01LN,BR01LNQ                                                   
         BE    GETBR50                                                          
         LA    R4,BR01NTP                                                       
         LHI   R1,X'46'                                                         
         BRAS  RE,SENDD            NETWORK PCTG                                 
         LA    R4,BR01PGP                                                       
         LHI   R1,X'47'                                                         
         BRAS  RE,SENDD            PROGRAM PCTG                                 
*                                                                               
* GET DEMO INFO ELEMENTS                                                        
GETBR50  GOTO1 VHELLO,DMCB,(C'G',SPTFILE),(X'02',(R6)),0                        
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                 ELEMENT SHOULD BE THERE                     
         L     R3,12(R1)                                                        
         USING BDEMD,R3                                                         
         B     GETBR200                                                         
*                                                                               
*  GET NEXT ELEMENT                                                             
GETBR150 ZIC   RE,BDEMLN                                                        
         AR    R3,RE                                                            
         CLI   0(R3),X'02'                                                      
         BNE   GETBREX                                                          
*                                                                               
GETBR200 LA    R4,BDEMTWP                                                       
         LHI   R1,X'3F'                                                         
         BRAS  RE,SENDD             DEMO TARGET WEIGHT                          
         LA    R4,BDEMTDL                                                       
         LHI   R1,X'40'                                                         
         BRAS  RE,SENDD             TOTAL DEMO LOWER                            
         LA    R4,BDEMTDU                                                       
         LHI   R1,X'41'                                                         
         BRAS  RE,SENDD             TOTAL DEMO UPPER                            
         LA    R4,BDEMBGL                                                       
         LHI   R1,X'42'                                                         
         BRAS  RE,SENDD             BRAND DEMO GOAL LOWER                       
         LA    R4,BDEMBGU                                                       
         LHI   R1,X'43'                                                         
         BRAS  RE,SENDD             BRAND DEMO GOAL UPPER                       
         LA    R4,BDEMWBL                                                       
         LHI   R1,X'44'                                                         
         BRAS  RE,SENDD             WEEKELY DEMO BRAND LOWER                    
         LA    R4,BDEMWBU                                                       
         LHI   R1,X'45'                                                         
         BRAS  RE,SENDD             WEEKLY DEMO BRAND UPPER                     
         B     GETBR150                                                         
*                                                                               
GETBREX  J     EXIT                                                             
         DROP  R3,R6                                                            
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* NOW DEAL WITH THE FEED RECORDS                                                
*                                                                               
GETFEEDS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING FEEDKEY,R6                                                       
         MVC   FDPSKID(2),=XL2'0AAB'                                            
         MVC   FDPSKAM,BAGYMD                                                   
         MVC   FDPSKCLT,FEEDCLI                                                 
         GOTO1 AIOCALL,DMCB,SPT+DIR+HIGH                                        
         B     GETFD20                                                          
GETFD15  GOTO1 AIOCALL,DMCB,SPT+DIR+SEQ                                         
*                                                                               
GETFD20  CLC   KEY(5),KEYSAVE      CHECK UP TILL CLIENT                         
         BNE   GETFDEX             EXIT ROUTINE                                 
*                                                                               
         GOTO1 AIOCALL,DMCB,SPT+FIL+GET,AIO1                                    
         L     R6,AIO1                                                          
*                                                                               
         OC    FEEDCLI,FEEDCLI                                                  
         BNZ   GETFD40                                                          
         MVI   BYTE,C'Y'                                                        
         LA    R4,BYTE                                                          
         LHI   R1,X'26'                                                         
         BRAS  RE,SENDD             AGENCY LEVEL FEED                           
GETFD40  LA    R4,FEEDKNET                                                      
         LHI   R1,X'27'                                                         
         BRAS  RE,SENDD             FEED NETWORK CODE                           
         LA    R4,FEEDKFD                                                       
         LHI   R1,X'28'                                                         
         BRAS  RE,SENDD             FEED CODE                                   
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',SPTFILE),(X'20',(R6)),0                        
         CLI   12(R1),0                                                         
         BNE   GETFD15                                                          
         L     R3,12(R1)                                                        
FEL      USING FEEDELEM,R3                                                      
*                                                                               
         LA    R4,FEL.FEEDELDS                                                  
         LHI   R1,X'29'                                                         
         BRAS  RE,SENDD             FEED DESCRIPTION                            
*                                                                               
         B     GETFD15              GET NEXT RECORD                             
*                                                                               
GETFDEX  J     EXIT                                                             
         DROP  FEL,R6                                                           
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* DOWNLOAD ALL THE BA RULES RECORDS 141 REQUEST                                 
*                                                                               
DWNBARR  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING BRULESD,R6                                                       
         MVC   BRULKTYP(2),=XL2'0E70'                                           
         MVC   BRULAGY,BAGYMD                                                   
         GOTO1 AIOCALL,DMCB,SPT+DIR+HIGH+PASSDEL                                
         B     DWNBR20                                                          
DWNBR15  GOTO1 AIOCALL,DMCB,SPT+DIR+SEQ+PASSDEL                                 
*                                                                               
DWNBR20  CLC   KEY(BRULCLT-BRULKEY),KEYSAVE  CHECK UP TILL AGENCY               
         BNE   DWNBREX             EXIT ROUTINE                                 
*                                                                               
         GOTO1 AIOCALL,DMCB,SPT+FIL+GET+PASSDEL,AIO1                            
         L     R6,AIO1                                                          
*                                                                               
         LHI   R1,X'141'                                                        
         BAS   RE,SENDH                                                         
*                                                                               
                                                                                
         MVI   BYTE,C'A'                                                        
         TM    BRULRSTA,X'80'                                                   
         BZ    *+8                                                              
         MVI   BYTE,C'D'                                                        
         LA    R4,BYTE                                                          
         LHI   R1,X'01'                                                         
         BRAS  RE,SENDD             RECORD DELTE STATUS                         
*                                                                               
         MVC   FULL(3),=CL3'***'                                                
         OC    BRULCLT,BRULCLT                                                  
         BZ    DWNBR30                                                          
         GOTO1 VCLUNPK,DMCB,BRULCLT,FULL                                        
DWNBR30  LA    R4,FULL             CLIENT                                       
         LHI   R1,X'03'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,BR01UAD                                                       
         LHI   R1,X'04'                                                         
         BRAS  RE,SENDD            USE ASSIGNED DOLLARS                         
         LA    R4,BR01RSD                                                       
         LHI   R1,X'05'                                                         
         BRAS  RE,SENDD            ROUND SPLI DOLLARS                           
         LA    R4,BR01IZU                                                       
         LHI   R1,X'06'                                                         
         BRAS  RE,SENDD            USE ZERO UNITS                               
         LA    R4,BR01DWP                                                       
         LHI   R1,X'07'                                                         
         BRAS  RE,SENDD            DOLLAR WEGHT PCTG                            
         LA    R4,BR01TBL                                                       
         LHI   R1,X'08'                                                         
         BRAS  RE,SENDD            TOTAL BUDGET LOWER                           
         LA    R4,BR01TBU                                                       
         LHI   R1,X'09'                                                         
         BRAS  RE,SENDD            TOTAL BUDGET UPPER                           
         LA    R4,BR01GBL                                                       
         LHI   R1,X'0A'                                                         
         BRAS  RE,SENDD            GOAL BUDGET LOWER                            
         LA    R4,BR01GBU                                                       
         LHI   R1,X'0B'                                                         
         BRAS  RE,SENDD            GOAL BUDGET UPPER                            
         LA    R4,BR01WBL                                                       
         LHI   R1,X'0C'                                                         
         BRAS  RE,SENDD            WEEKLY BUDGET LOWER                          
         LA    R4,BR01WBU                                                       
         LHI   R1,X'0D'                                                         
         BRAS  RE,SENDD            WEEKLY BUDGET UPPER                          
         LA    R4,BR01NTI                                                       
         LHI   R1,X'0E'                                                         
         BRAS  RE,SENDD            NETWORK SEPERATION MINUTES                   
         LA    R4,BR01CTI                                                       
         LHI   R1,X'0F'                                                         
         BRAS  RE,SENDD            CABLE SEPERATION MINUTES                     
         LA    R4,BR01MDS                                                       
         LHI   R1,X'10'                                                         
         BRAS  RE,SENDD            MAX DOUBLE SPOTTING ALLOWED                  
*                                                                               
         CLI   BR01LN,BR01LNQ                                                   
         BE    DWNBR40                                                          
         LA    R4,BR01NTP                                                       
         LHI   R1,X'26'                                                         
         BRAS  RE,SENDD            NETWORK PCTG                                 
         LA    R4,BR01PGP                                                       
         LHI   R1,X'27'                                                         
         BRAS  RE,SENDD            PROGRAM PCTG                                 
*                                                                               
*                                                                               
* GET DEMO INFO ELEMENTS                                                        
DWNBR40  GOTO1 VHELLO,DMCB,(C'G',SPTFILE),(X'02',(R6)),0                        
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                 ELEMENT SHOULD BE THERE                     
         L     R3,12(R1)                                                        
         USING BDEMD,R3                                                         
*                                                                               
         LA    R4,BDEMTWP                                                       
         LHI   R1,X'11'                                                         
         BRAS  RE,SENDD             DEMO TARGET WEIGHT                          
         LA    R4,BDEMTDL                                                       
         LHI   R1,X'12'                                                         
         BRAS  RE,SENDD             TOTAL DEMO LOWER                            
         LA    R4,BDEMTDU                                                       
         LHI   R1,X'13'                                                         
         BRAS  RE,SENDD             TOTAL DEMO UPPER                            
         LA    R4,BDEMBGL                                                       
         LHI   R1,X'14'                                                         
         BRAS  RE,SENDD             BRAND DEMO GOAL LOWER                       
         LA    R4,BDEMBGU                                                       
         LHI   R1,X'15'                                                         
         BRAS  RE,SENDD             BRAND DEMO GOAL UPPER                       
         LA    R4,BDEMWBL                                                       
         LHI   R1,X'16'                                                         
         BRAS  RE,SENDD             WEEKELY DEMO BRAND LOWER                    
         LA    R4,BDEMWBU                                                       
         LHI   R1,X'17'                                                         
         BRAS  RE,SENDD             WEEKLY DEMO BRAND UPPER                     
*                                                                               
*  GET NEXT ELEMENT                                                             
         ZIC   RE,BDEMLN                                                        
         AR    R3,RE                                                            
         CLI   0(R3),X'02'                                                      
         BNE   DWNBREX                                                          
*                                                                               
         LA    R4,BDEMTWP                                                       
         LHI   R1,X'18'                                                         
         BRAS  RE,SENDD             DEMO TARGET WEIGHT                          
         LA    R4,BDEMTDL                                                       
         LHI   R1,X'19'                                                         
         BRAS  RE,SENDD             TOTAL DEMO LOWER                            
         LA    R4,BDEMTDU                                                       
         LHI   R1,X'1A'                                                         
         BRAS  RE,SENDD             TOTAL DEMO UPPER                            
         LA    R4,BDEMBGL                                                       
         LHI   R1,X'1B'                                                         
         BRAS  RE,SENDD             BRAND DEMO GOAL LOWER                       
         LA    R4,BDEMBGU                                                       
         LHI   R1,X'1C'                                                         
         BRAS  RE,SENDD             BRAND DEMO GOAL UPPER                       
         LA    R4,BDEMWBL                                                       
         LHI   R1,X'1D'                                                         
         BRAS  RE,SENDD             WEEKELY DEMO BRAND LOWER                    
         LA    R4,BDEMWBU                                                       
         LHI   R1,X'1E'                                                         
         BRAS  RE,SENDD             WEEKLY DEMO BRAND UPPER                     
*                                                                               
*  GET NEXT ELEMENT                                                             
         ZIC   RE,BDEMLN                                                        
         AR    R3,RE                                                            
         CLI   0(R3),X'02'                                                      
         BNE   DWNBREX                                                          
*                                                                               
         LA    R4,BDEMTWP                                                       
         LHI   R1,X'1F'                                                         
         BRAS  RE,SENDD             DEMO TARGET WEIGHT                          
         LA    R4,BDEMTDL                                                       
         LHI   R1,X'20'                                                         
         BRAS  RE,SENDD             TOTAL DEMO LOWER                            
         LA    R4,BDEMTDU                                                       
         LHI   R1,X'21'                                                         
         BRAS  RE,SENDD             TOTAL DEMO UPPER                            
         LA    R4,BDEMBGL                                                       
         LHI   R1,X'22'                                                         
         BRAS  RE,SENDD             BRAND DEMO GOAL LOWER                       
         LA    R4,BDEMBGU                                                       
         LHI   R1,X'23'                                                         
         BRAS  RE,SENDD             BRAND DEMO GOAL UPPER                       
         LA    R4,BDEMWBL                                                       
         LHI   R1,X'24'                                                         
         BRAS  RE,SENDD             WEEKELY DEMO BRAND LOWER                    
         LA    R4,BDEMWBU                                                       
         LHI   R1,X'25'                                                         
         BRAS  RE,SENDD             WEEKLY DEMO BRAND UPPER                     
*                                                                               
         B     DWNBR15                                                          
*                                                                               
DWNBREX  J     EXIT                                                             
         DROP  R3,R6                                                            
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* PASS NETWORK/MARKET INFO                                                      
*                                                                               
PASMKT   NTR1  BASE=*,LABEL=*                                                   
         LHI   R1,X'147'                                                        
         BAS   RE,SENDH                                                         
*                                                                               
* READ THE MASTER RECORDS                                                       
         MVC   AIO,AIO3                                                         
         LA    R3,KEY                                                           
         USING STARECD,R3                                                       
         XC    KEY,KEY                                                          
         MVI   STAKTYPE,C'S'                                                    
         MVI   STAKMED,C'N'                                                     
*                                                                               
         GOTO1 AIOCALL,DMCB,STA+FIL+HIGH,AIO                                    
         B     PMKT15                                                           
PMKT10   GOTO1 AIOCALL,DMCB,STA+FIL+SEQ,AIO                                     
PMKT15   CLC   KEY(2),KEYSAVE                                                   
         BNE   PASMKTX                                                          
*                                                                               
         L     R3,AIO                                                           
         CLC   STAKAGY,QAGY                                                     
         BNE   PMKT10                                                           
         CLC   =C'000',STAKCLT                                                  
         JNE   PMKT10                                                           
*                                                                               
         LA    R4,STAKCALL         NETWORK                                      
         LHI   R1,X'01'                                                         
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R4,SMKT             MARKET                                       
         LHI   R1,X'02'                                                         
         BRAS  RE,SENDD                                                         
*                                                                               
         CLI   STAKLEN+1,STACRLNQ                                               
         JL    PMKT20                                                           
         CLI   SCSSTA,0                                                         
         JE    PMKT20                                                           
*                                                                               
         LA    R4,SCSSTA           COMSCORE NETWORK NUMBER                      
         LHI   R1,X'03'                                                         
         BRAS  RE,SENDD                                                         
*                                                                               
PMKT20   B     PMKT10                                                           
*                                                                               
PASMKTX  J     EXIT                                                             
         LTORG                                                                  
*                                                                               
* DOWNLOAD PROGRAM INFO                                                         
*                                                                               
PGMINFO  NTR1  BASE=*,LABEL=*                                                   
         MVC   AIO,AIO3                                                         
         LA    R3,KEY                                                           
         USING NPGRECD,R3                                                       
         XC    KEY,KEY                                                          
         LA    R5,4                 LENGTH OF KEY COMPARE                       
         MVC   NPGKTYP,=XL2'0D20'                                               
         MVC   NPGKAM,BAGYMD                                                    
         MVC   NPGKNET,BMKT                                                     
         GOTO1 AIOCALL,DMCB,SPT+DIR+HIGH                                        
         B     PINFO20                                                          
*                                                                               
PINFO10  LA    R3,KEY                                                           
         GOTO1 AIOCALL,DMCB,SPT+DIR+SEQ                                         
*                                                                               
PINFO20  CLC   KEY(3),KEYSAVE                                                   
         BNE   PINFOX                                                           
*                                                                               
         OC    BMKT,BMKT           ANY NETWORK ENTERED?                         
         JZ    *+14                                                             
         CLC   NPGKNET,BMKT                                                     
         JNE   PINFO10                                                          
*                                                                               
         OC    SVSDATE(4),SVSDATE    DATE INPUTTED                              
         BZ    PINFO40                                                          
         OC    SVEDATE,SVEDATE       JUST START DATE INPUTTED                   
         BZ    PINFO30                                                          
         CLC   SVSDATE,NPGKEND       IS START DATE > PROGRAM END?               
         BH    PINFO10                                                          
*                                                                               
         CLC   SVEDATE,NPGKEND      IS END DATE < PROGRAM DATE                  
         BL    PINFO10                                                          
         B     PINFO40                                                          
*                                                                               
PINFO30  CLC   SVSDATE,NPGKEND      IS DATE = PROGRAM DATE                      
         BNE   PINFO10                                                          
*                                                                               
PINFO40  L     R3,AIO                                                           
         GOTO1 AIOCALL,DMCB,SPT+FIL+GET,AIO                                     
*                                                                               
         XC    TMPSER#,TMPSER#                                                  
         GOTO1 VHELLO,DMCB,(C'G',SPTFILE),(X'03',(R3)),0                        
         CLI   12(R1),0                                                         
         BNE   PINFO50                                                          
         L     R6,12(R1)                                                        
         USING NPGEL03,R6                                                       
         CLI   NPGLEN3,NPG3LNQ2                                                 
         JL    PINFO50                                                          
         MVC   TMPSER#,NPGCSN      COMSCORE SERIES #                            
*                                                                               
PINFO50  L     R3,AIO                                                           
         GOTO1 VHELLO,DMCB,(C'G',SPTFILE),(X'DE',(R3)),0                        
         CLI   12(R1),0                                                         
         BE    PINFO55                                                          
         OC    TMPSER#,TMPSER#     IS THERE A SERIES #?                         
         JZ    PINFO10                                                          
         J     *+8                 YES - RETURN IT                              
PINFO55  L     R6,12(R1)           THERE'S OVERRIDES TOO                        
*                                                                               
         LHI   R1,X'146'                                                        
         BAS   RE,SENDH                                                         
*                                                                               
         LA    R4,NPGKNET          MARKET NUMBER                                
         LHI   R1,X'01'                                                         
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R4,NPGKPROG         PROGRAM CODE                                 
         LHI   R1,X'02'                                                         
         BRAS  RE,SENDD                                                         
*                                                                               
         GOTO1 VDATCON,DMCB,(2,NPGKEND),(5,DUB)                                 
         LA    R4,DUB                                                           
         LHI   R1,X'03'                                                         
         BRAS  RE,SENDD                                                         
*                                                                               
         OC    TMPSER#,TMPSER#     ANY COMSCORE SERIES #?                       
         JZ    PINFO60                                                          
         LA    R4,TMPSER#                                                       
         LHI   R1,X'04'                                                         
         BRAS  RE,SENDD                                                         
*                                                                               
         USING NPGCELDD,R6                                                      
PINFO60  CLI   0(R6),NPGCELQ                                                    
         JNE   PINFO10                                                          
*                                                                               
         LA    R4,NPGCCAT                                                       
         LHI   R1,X'05'                                                         
*        BRAS  RE,SENDD                                                         
*                                                                               
         LA    R4,NPGCAMT                                                       
         LHI   R1,X'06'                                                         
*        BRAS  RE,SENDD                                                         
*                                                                               
         ZIC   RF,1(R6)                                                         
         AR    R6,RF                                                            
         J     PINFO60                                                          
*                                                                               
PINFOX   J     EXIT                                                             
         LTORG                                                                  
TMPSER#  DS    CL10                                                             
*                                                                               
* DOWNLOAD ALL THE BA RULES RECORDS 141 REQUEST                                 
*                                                                               
DWNLIMIT NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING PXCRECD,R6                                                       
         MVC   PXCKTYP,=XL2'0D70'                                               
         MVC   PXCKAGM,BAGYMD                                                   
         MVC   PXCKCLT,SVCLI                                                    
         GOTO1 AIOCALL,DMCB,SPT+DIR+HIGH+PASSDEL                                
         B     DWNLM20                                                          
DWNLM15  LA    R6,KEY                                                           
         GOTO1 AIOCALL,DMCB,SPT+DIR+SEQ+PASSDEL                                 
*                                                                               
                                                                                
DWNLM20  CLC   KEY(PXCKSTA-PXCKEY),KEYSAVE  CHECK UP TILL CLIENT                
         BNE   DWNLMEX             EXIT ROUTINE                                 
         TM    PXCKSTAT,PXCKLIMQ   CHECK FOR LIMIT RECORD                       
         BZ    DWNLM15                                                          
*  CHECK FOR ESTIMATE PRODUCT LEVEL FILTERS                                     
         CLI   QPRD,X'40'                                                       
         BNH   *+14                                                             
         CLC   PXCKPRD,QPRD                                                     
         BNE   DWNLM15                                                          
*                                                                               
         CLI   PXCKEST,0           CHECK ALL ESTIMATE                           
         BE    DWNLM30                                                          
         CLI   BEST,0                                                           
         BNH   DWNLM30                                                          
         CLC   PXCKEST,BEST                                                     
         BNE   DWNLM15                                                          
*                                                                               
DWNLM30  GOTO1 AIOCALL,DMCB,SPT+FIL+GET+PASSDEL,AIO1                            
         L     R6,AIO1                                                          
*                                                                               
         LHI   R1,X'143'                                                        
         BAS   RE,SENDH                                                         
*                                                                               
*  PASS RECORD STATUS                                                           
         MVI   BYTE,C'A'                                                        
         TM    PXCCNTL,X'80'                                                    
         BZ    *+8                                                              
         MVI   BYTE,C'D'                                                        
         LA    R4,BYTE                                                          
         LHI   R1,X'01'                                                         
         BRAS  RE,SENDD                                                         
*                                                                               
*  UNPACK THE STATION                                                           
         MVC   WORK2+15(4),=CL4'ALL '                                           
         CLC   PXCKSTA,=XL3'FFFFFF'                                             
         BE    DWNLM40                                                          
         XC    DMCB(4),DMCB                                                     
         MVC   DMCB+4(3),=X'D9000A'                                             
         MVI   DMCB+7,QMSUNPK                                                   
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     R4,DMCB                                                          
         XC    DMCB(24),DMCB                                                    
         MVC   WORK2(2),=XL2'0001'                                              
         MVC   WORK2+2(3),PXCKSTA                                               
         GOTO1 (R4),DMCB,WORK2,WORK2+10,WORK2+15  MSUNPK                        
*                                                                               
DWNLM40  LA    R4,WORK2+15          STATION                                     
         LHI   R1,X'02'                                                         
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R4,PXCKPRD           PRODUCT                                     
         LHI   R1,X'03'                                                         
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R4,PXCKEST           ESTIMATE                                    
         LHI   R1,X'04'                                                         
         BRAS  RE,SENDD                                                         
*                                                                               
*                                                                               
* GET DEMO INFO ELEMENTS                                                        
         GOTO1 VHELLO,DMCB,(C'G',SPTFILE),(X'01',(R6)),0                        
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                 ELEMENT SHOULD BE THERE                     
         L     R3,12(R1)                                                        
*                                                                               
*  INCLUDE EXCLUDE STATUS                                                       
DWNLM50  ZIC   RE,1(R3)                                                         
         AR    R3,RE                                                            
         CLI   0(R3),0                                                          
         BE    DWNLM15                                                          
         MVI   BYTE,C'+'                                                        
         CLI   0(R3),X'06'                                                      
         BE    DWNLM70                                                          
         MVI   BYTE,C'-'                                                        
         CLI   0(R3),X'07'                                                      
         BH    DWNLM15 '                                                        
DWNLM70  LA    R4,BYTE                                                          
         LHI   R1,X'05'                                                         
         BRAS  RE,SENDD                                                         
*                                                                               
*  NETWORK LIMIT                                                                
         USING L07ELD,R3                                                        
         TM    L07STAT,X'80'                                                    
         BZ    DWNLM100                                                         
         MVI   BYTE,C'N'           STATUS                                       
         LA    R4,BYTE                                                          
         LHI   R1,X'06'                                                         
         BAS   RE,SENDD                                                         
         LA    R4,L07LIM           NETWORK                                      
         LHI   R1,X'07'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
*  DAY LIMIT                                                                    
DWNLM100 TM    L07STAT,X'40'                                                    
         BZ    DWNLM120                                                         
         MVI   BYTE,C'D'           STATUS                                       
         LA    R4,BYTE                                                          
         LHI   R1,X'06'                                                         
         BAS   RE,SENDD                                                         
         LA    R4,L07LIM           DAY                                          
         LHI   R1,X'08'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
*  TIME LIMIT                                                                   
DWNLM120 TM    L07STAT,X'20'                                                    
         BZ    DWNLM140                                                         
         MVI   BYTE,C'T'           STATUS                                       
         LA    R4,BYTE                                                          
         LHI   R1,X'06'                                                         
         BAS   RE,SENDD                                                         
         XC    WORK2(20),WORK2                                                  
         GOTO1 VUNTIME,DMCB,L07LIM,WORK2                                        
         LA    R4,WORK2            TIME                                         
         LHI   R1,X'09'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
*  PROGRAM NAME LIMIT                                                           
DWNLM140 TM    L07STAT,X'10'                                                    
         BZ    DWNLM160                                                         
         MVI   BYTE,C'P'           STATUS                                       
         LA    R4,BYTE                                                          
         LHI   R1,X'06'                                                         
         BAS   RE,SENDD                                                         
         LA    R4,L07LIM           PROGRAM NAME                                 
         LHI   R1,X'0A'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
*  DAY/TIME LIMIT                                                               
DWNLM160 TM    L07STAT,X'08'                                                    
         BZ    DWNLM180                                                         
         MVI   BYTE,C'Y'           STATUS                                       
         LA    R4,BYTE                                                          
         LHI   R1,X'06'                                                         
         BAS   RE,SENDD                                                         
         LA    R4,L07LIM           DAY                                          
         LHI   R1,X'08'                                                         
         BAS   RE,SENDD                                                         
         XC    WORK2(20),WORK2                                                  
         GOTO1 VUNTIME,DMCB,L07LIM+1,WORK2                                      
         LA    R4,WORK2            TIME                                         
         LHI   R1,X'09'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
*  START END DATES                                                              
DWNLM180 OC    L07STDTE,L07STDTE                                                
         BZ    DWNLM200                                                         
         LA    R4,L07STDTE                                                      
         LHI   R1,X'0B'                                                         
         BRAS  RE,SENDD                                                         
DWNLM200 OC    L07ENDTE,L07ENDTE                                                
         BZ    DWNLM50                                                          
         LA    R4,L07ENDTE                                                      
         LHI   R1,X'0C'                                                         
         BRAS  RE,SENDD                                                         
         B     DWNLM50                                                          
*                                                                               
DWNLMEX  J     EXIT                                                             
         DROP  R3,R6                                                            
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*  ROUTINE TO PASS EDI FROM CLIENT RECORD IF IT EXISTS                          
*                                                                               
PASSEDI  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R6,AIO1                                                          
         USING CLTHDR,R6                                                        
*                                                                               
         LHI   R1,72                                                            
         LA    R4,BYTE                                                          
         TM    CINDS1,CIN1NEDI     NEDI OPTION?                                 
         JZ    *+12                                                             
         MVI   BYTE,C'Y'                                                        
         BRAS  RE,SENDD                                                         
*                                                                               
         CLI   CLEDICLT,X'40'                                                   
         BNH   PASSEDIX                                                         
*                                                                               
         LHI   R1,X'2C'                                                         
         LA    R4,CLEDICLT                                                      
         BRAS  RE,SENDD                                                         
*                                                                               
         MVC   AIO,AIO2                                                         
         LA    R3,KEY                                                           
         USING CTIREC,R3                                                        
         XC    KEY,KEY                                                          
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKNUM,CLEDIUID                                                 
         DROP  R6                                                               
*                                                                               
*  PASS EDI USER ID NUMBER                                                      
         GOTO1 AIOCALL,DMCB,CTL+FIL+HIGH,AIO                                    
         L     R3,AIO                                                           
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AIO                                                           
         GOTO1 VHELLO,DMCB,(C'G',CTFILE),(X'02',(R3)),0                         
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                 MUST BE THERE                               
         L     R6,12(R1)                                                        
         USING CTDSCD,R6                                                        
*                                                                               
*  FIND FIRST BLANK SPACE TO CALCULATE LENGTH OF FIELD                          
         LA    RE,10                                                            
         LA    RF,CTDSC                                                         
         SR    R5,R5                                                            
PASEDI50 CLI   0(RF),X'40'                                                      
         BNH   PASEDI60                                                         
         LA    RF,1(RF)                                                         
         LA    R5,1(R5)                                                         
         BCT   RE,PASEDI50                                                      
*                                                                               
PASEDI60 LHI   R1,X'2D'                                                         
         LA    R4,CTDSC                                                         
         BRAS  RE,SENDD                                                         
*                                                                               
PASSEDIX J     EXIT                                                             
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE NENAVWRK                                                       
WORKD    DSECT                                                                  
         ORG   OVWORK                                                           
NDAYS    DS    F                                                                
BUYDSTR  DS    A                                                                
BUYDEND  DS    A                                                                
SPOTNUM  DS    H                                                                
INVSDATE DS    CL6                                                              
INVHDPRD DS    X                   INVOICE HEADER PRD                           
INVHDPR2 DS    X                                                                
INVHDEST DS    X                                                                
RELAFDAY DS    X                                                                
RELAFTIM DS    XL2                                                              
BARCLI   DS    XL2                 BARRULES CLIENT                              
FEEDCLI  DS    XL2                 FEED RECORD CLIENT                           
SVBDELEM DS    CL67                                                             
ALDATE   DS    XL2                 ALLOC DATE                                   
LADATE   DS    XL2                 LAST ALLOC DATE                              
V10301   DS    CL4                                                              
EDSAVE   DS    XL17                                                             
ALLCLSW  DS    CL1                  ALL CLIENT SWITCH                           
DAYLOOP  DS    CL1                  DAY LOOP CHECK FOR PROGRAM SEARCH           
DAYSW    DS    CL1                  DAY SWITCH FOR PROG SEARCH Y=DAY            
DAYFILT  DS    CL1                  CURRENT FILTER CHECK FOR PROG SERCH         
HOLDTIME DS    CL4                  CURRENT TIME FOR PROGRAM SEARCH             
ALPHID   DS    CL10                 USER ID ALPHA                               
HOLDPROD DS    CL3                                                              
*                                                                               
SVINVSTD DS    XL2                 INVOICE PERIOD START DATE                    
*                                                                               
SVSYSQ   DS    CL1                 SYSTEM                                       
*                                                                               
INVFLG   DS    XL1                                                              
IFLGBRD  EQU   X'01'                BROADCAST DATES                             
*                                                                               
PGMCODE  DS    CL6                  PROGRAM CODE                                
PGMENDDT DS    XL2                  PROGRAM END DATE                            
PGMNTI   DS    XL2                  PROGRAM NTI CODE                            
PGMSTAT  DS    XL1                  PROGRAM STATUS                              
PGMSHARE DS    XL2                  PGOGRAM SHARE                               
PGMROT   DS    XL1                  PROGRAM ROTATOR                             
*                                                                               
PROFI2   DS    XL16                                                             
*                                                                               
MYRELO   DS    F                                                                
APFXTAB  DS    F                                                                
*                                                                               
ESTCLT   DS    CL2                                                              
ESTPROD  DS    CL3                                                              
ESTESTN  DS    CL1                                                              
*                                                                               
         DS    0D                                                               
BUYDATA1 DS    CL78                                                             
BUYDATA2 DS    CL78                                                             
BUYDATA3 DS    CL78                                                             
BUYDATA4 DS    CL78                                                             
BUYDATAL EQU   *-BUYDATA1                                                       
BUYDATAX EQU   *                                                                
         ORG                                                                    
*                                                                               
DEMOWRK  DSECT                                                                  
ESTDEMO1 DS    XL60                 FIRST 20 ESTIMATE DEMOS                     
ESTDEMO2 DS    XL60                 SECOND 20 ESTIMATE DEMOS                    
ESTDEMO3 DS    XL30                 LAST 10 ESTIMATE DEMOS                      
ESTDEMON DS    XL3                  END OF TABLE INDICATOR                      
ESTDEMRT DS    XL550                EXPANDED DEMO LIST                          
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
REPRECD  DSECT                                                                  
       ++INCLUDE SPGENREP                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NEGENPACK                                                      
       ++INCLUDE NEGENDPT                                                       
       ++INCLUDE SPGENPROG                                                      
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENSNV                                                       
       ++INCLUDE SPGENPTYP                                                      
       ++INCLUDE SPGENPRG                                                       
       ++INCLUDE SPGENREAS                                                      
       ++INCLUDE SPTRNFEED                                                      
       ++INCLUDE SPGENWBFLT                                                     
       ++INCLUDE SPGENBARU                                                      
       ++INCLUDE SPGENPXC                                                       
       ++INCLUDE DEDEMFILE                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DEDEMTABD                                                      
       ++INCLUDE NESYSFAC                                                       
       ++INCLUDE DDACTIVD                                                       
       ++INCLUDE FASSB                                                          
       ++INCLUDE FAUTL                                                          
       ++INCLUDE GEGENSDR                                                       
       ++INCLUDE FAXPEQUS                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE GEGENTOK                                                       
*                                                                               
* COMSCORE TABLES                                                               
*                                                                               
CSSTABD  DSECT                                                                  
CSSRCE   DS    CL20                                                             
CSSTABL  EQU   *-CSSTABD                                                        
*                                                                               
CSVTABD  DSECT                                                                  
CSVTYPE  DS    CL2                                                              
CSVDESC  DS    CL15                                                             
CSVTYPEC DS    CL8                                                              
CSVTABL  EQU   *-CSVTABD                                                        
*                                                                               
CSWSTABD DSECT                                                                  
CSWTOK   DS    CL200               TOKEN                                        
CSWRSCD  DS    CL200               RATING SOURCE CONNECTION DATA                
CSWRSME  DS    CL200               RATING SOURCE METHOD END POINTS              
CSWMASH  DS    CL200               MASHERY KEYS                                 
CSWSTABL EQU   *-CSWTOK                                                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'159NENAV01   01/27/21'                                      
         END                                                                    
