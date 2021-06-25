*          DATA SET NEWRI33T   AT LEVEL 002 AS OF 05/01/02                      
*          DATA SET NEWRI33    AT LEVEL 001 AS OF 09/27/95                      
*          DATA SET NEWRI20    AT LEVEL 012 AS OF 09/26/95                      
*PHASE T32030A,+0                                                               
*INCLUDE NETSPB                                                                 
*INCLUDE NETCOM                                                                 
*INCLUDE MOBILE                                                                 
*INCLUDE GETBROAD                                                               
*INCLUDE DYNALLOC                                                               
*INCLUDE MSPACK                                                                 
*INCLUDE SPBVAL                                                                 
*INCLUDE NEPACC                                                                 
         TITLE 'T32030 - NETWORK WRITER CONTROL'                                
******************************************************************              
T32030   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T32030**                                                       
         USING T32030,RB,R5                                                     
         LA    R5,2048(RB)                                                      
         LA    R5,2048(R5)                                                      
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T320FFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R1,ANETWS1          PASS CLIENT RECORD IN W/S AREA 1             
         ST    R1,NBACLI                                                        
         L     R7,ANETWS2                                                       
         A     R7,=F'500'                                                       
         USING WRITERD,R7                                                       
         ST    R7,NBADEM           1ST PART IS NET DEMO BLOCK                   
         LA    R1,NDDEMBLK                                                      
         ST    R1,NDADMBLK                                                      
         LA    R1,RAWDATA          (EXTENSION FOR RAW DEMOS)                    
         ST    R1,NDARAWEX                                                      
         LA    R1,DBLOCK                                                        
         ST    R1,NDADBLCK                                                      
         LA    R1,DATEAREA                                                      
         ST    R1,NDADATES                                                      
                                                                                
                                                                                
         CLI   MODE,VALKEY                                                      
         BE    VKEY                                                             
         CLI   MODE,DISPREC                                                     
         BE    DISP                                                             
         CLI   MODE,VALREC                                                      
         BE    AE2                                                              
         B     XIT                                                              
*                                                                               
SOONERR  LA    R2,CONWHENH                                                      
         MVI   ERROR,INVPRINT                                                   
         B     EDERR                                                            
         EJECT                                                                  
* VALIDATE KEY                                                                  
*                                                                               
VKEY     CLI   ACTNUM,ACTLIST      TEST ACTION=LIST                             
         BE    VKEY0                                                            
         CLI   ACTNUM,ACTDIS       ACTION=DISPLAY                               
         BNE   VKEYX                                                            
*                                                                               
VKEY0    BAS   RE,RDPROF           CHK WR PROFILE                               
         CLI   WORK+18,C'Y'        TEST FILTER REQUIRED                         
         BNE   VKEYX                                                            
*                                                                               
         L     R2,EFHTAG           YES-LOCATE FILTER FIELD                      
         SR    R0,R0                                                            
         LA    RE,3                                                             
         CLI   ACTNUM,ACTLIST      TEST ACTION=LIST                             
         BE    *+8                  YES                                         
         LA    RE,6                 NO - FILTER FIELD DIFFERENT!                
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   RE,*-6                                                           
*                                                                               
         CLI   5(R2),4             TEST AT LEAST 4 FILTER CHARACTERS            
         BL    FILTERR                                                          
         ZIC   RE,5(R2)                                                         
         LA    R1,8(R2)            CHARACTERS * AND - NOT ALLOWED               
*                                                                               
VKEY2    CLI   0(R1),C'*'                                                       
         BE    FILTERR                                                          
         CLI   0(R1),C'-'                                                       
         BE    FILTERR                                                          
         LA    R1,1(R1)                                                         
         BCT   RE,VKEY2                                                         
VKEY3    MVC   FILTSV,8(R2)        SAVE VALID FILTER                            
*                                                                               
VKEYX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
         SPACE                                                                  
RDPROF   NTR1                      YES-READ AGENCY'S WR PROFILE                 
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0WR'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         GOTO1 GETPROF,DMCB,WORK,WORK+16,DATAMGR                                
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* DISPLAY RECORD                                                                
*                                                                               
DISP     BAS   RE,RDPROF           READ AGENCY'S WR PROFILE                     
         CLI   WORK+18,C'Y'        TEST FILTER REQUIRED                         
         BNE   DISPX                                                            
         CLI   TWAOFFC,C'*'        IF DDS                                       
         BE    DISPX                                                            
         LA    R2,SPLFLTH          YES-CHECK IT                                 
         CLC   FILTSV,SPLFLT       DOES IT MATCH                                
         BE    DISPX               YES                                          
         MVC   SPLFLT,FILTSV       NO-RETURN REQUESTED FILTER                   
         OI    SPLFLTH+6,X'80'                                                  
         LA    R1,SPLCLIH            -CLEAR ALL UPROTECTED FIELDS               
         SR    R0,R0                                                            
         B     DISP4                                                            
*                                                                               
DISP2    ICM   R0,1,0(R1)                                                       
         BZ    DISPX                 - AND EXIT                                 
         AR    R1,R0                                                            
*                                                                               
DISP4    TM    1(R1),X'20'                                                      
         BO    DISP2                                                            
         ZIC   RE,0(R1)                                                         
         SH    RE,=H'9'                                                         
         BM    DISP2                                                            
         TM    1(R1),X'02'                                                      
         BZ    *+12                                                             
         SH    RE,=H'8'                                                         
         BM    DISP2                                                            
         EX    RE,*+4                                                           
         XC    8(0,R1),8(R1)                                                    
         OI    6(R1),X'80'                                                      
         B     DISP2                                                            
*                                                                               
DISPX    B     XIT                                                              
         EJECT                                                                  
* VALIDATE RECORD                                                               
*              INITIALIZE                                                       
         SPACE 3                                                                
AE2      MVI   NBQINIT,0           DO ALL VALIDATIONS EACH TIME                 
         MVI   LSTRD,0                                                          
         MVI   BHRD,0                                                           
         XC    BILLFLT,BILLFLT                                                  
         MVI   NDANYREC,C'N'                                                    
         CLI   SPLRECPH+5,0                                                     
         BE    *+8                                                              
         MVI   NDANYREC,C'Y'                                                    
         SPACE                                                                  
*                                                                               
         CLI   SPLCLI,C'@'         ..IF AGY LIST READ                           
         BNE   AE2A                                                             
         CLI   OFFLINE,C'Y'        ..AND ONLINE                                 
         BE    SKIPTWA                                                          
         CLI   TWAOFFC,C'*'        ..UNLESS DDS TERMINAL                        
         BE    *+12                                                             
         CLI   TWAWHEN,2           ..SOON IS VERBOTEN                           
         BE    SOONERR                                                          
SKIPTWA  MVC   LSTCLT,SPLCLI       .SAVE REQUEST @NNN                           
         OI    NDCROSS,X'01'       .SET GENERAL CROSS AGY READ                  
*                                                                               
         CLC   LSTCLT,=C'@GI2'                                                  
         BE    GI2RES                                                           
         CLC   LSTCLT,=C'@TMS'                                                  
         BE    GI2RES                                                           
         CLC   LSTCLT,=C'@LEX'                                                  
         BE    GI2RES                                                           
         CLC   LSTCLT,=C'@RES'                                                  
         BNE   NOTGI2                                                           
GI2RES   OI    NDCROSS,X'10'       X'10'=@GI2/@RES/LEX/TMS                      
         B     ACS03                                                            
*                                                                               
NOTGI2   OI    NDCROSS,X'02'       .SET CROSS AGY READ KEYWORD CHECK            
         CLC   LSTCLT,=C'@YAL'          @YAL=NW=X'02'                           
         BE    ACS03                                                            
         CLC   LSTCLT,=C'@ATT'          @ATT=AT=X'02'                           
         BE    ACS03                                                            
         CLC   LSTCLT,=C'@BCS'          @BCS=AT=X'02'                           
         BE    ACS03                                                            
         CLC   =C'@FC',LSTCLT           @FC=FCNY=X'02'                          
         BE    ACS03                                                            
         CLC   LSTCLT,=C'@CHR'          CHR=X'04'                               
         BNE   ACS03                                                            
         NI    NDCROSS,X'FF'-X'02'                                              
         OI    NDCROSS,X'04'       .SET CROSS AGY READ KEYWORD CHECK            
         B     ACS03                                                            
ACS03    CLI   OFFLINE,C'Y'        .AND IF ON-LINE                              
         BE    AE2A                                                             
         LA    R4,RELO                                                          
         S     R4,RELO                                                          
         L     R2,=A(AGYTABL)      .CHECK AUTHORIZED ACCESS                     
         AR    R2,R4                                                            
ACS05    CLC   NBSELAGY,0(R2)                                                   
         BNE   ACS10                                                            
         CLC   SPLCLI+1(3),4(R2)                                                
         BE    ACS20                                                            
ACS10    LA    R2,12(R2)                                                        
         CLI   0(R2),X'FF'                                                      
         BE    AUTHERR             .AUTHORIZATION ERROR                         
         B     ACS05                                                            
ACS20    MVC   NBSELCLI,=C'ALL'    .OK/ FUDGE ON-LINE CLI VALIDATION            
         B     AE4D                                                             
         SPACE                                                                  
*                                                                               
AE2A     CLI   OFFLINE,C'Y'                                                     
         BNE   AE3                                                              
         CLI   SPLCLI,C'@'         READ AGY LIST                                
         BNE   AE2B                                                             
         BAS   RE,LSTRDS                                                        
         B     AE3                                                              
AE2B     CLC   SPLCLI(3),=C'***'   ALLOW FOR DDS REQUEST                        
         BNE   AE3                                                              
         XC    NBSELAGY,NBSELAGY                                                
         XC    NBEFFAGY,NBEFFAGY                                                
         MVC   SPLCLI(3),=C'ALL'                                                
         EJECT                                                                  
         SPACE 3                                                                
AE3      DS    0H                                                               
         CLC   =C'BH',SPLFLAV       CHK BILL HEADER READ                        
         BNE   AE3D                                                             
         MVI   BHRD,1                                                           
         CLI   SPLFLAV+2,C'O'                                                   
         BNE   *+8                                                              
         MVI   BHRD,2              ONLY BILL HEADER READ                        
         CLI   OFFLINE,C'Y'                                                     
         BNE   AE3D                                                             
         L     R3,=A(MYIO)         CLEAR BLOCK FOR BILLS                        
         A     R3,=F'2000'                                                      
         USING BHBLOCK,R3                                                       
         LR    RE,R3                                                            
         LA    RF,BHBLENE                                                       
         XCEF                                                                   
         MVC   BHBLOCK,=C'**BBLK**'      BILL HEADER BLOCKS IN DUMP             
         MVC   BHDATA,=C'**BKEY**'                                              
         MVC   BHDOLS,=C'**BDOL**'                                              
         SPACE                                                                  
AE3D     CLC   CONREC(2),=CL2'PW'   CHECK FOR PUP WRITER                        
         BNE   AE4                                                              
         OI    NDCOLIND,X'20'       SET PUP INDICATOR                           
         MVI   FTERMFLG,0          (REQUIRED FIELDS)                            
         MVI   SPLFLAVH+5,1                                                     
         MVI   SPLPROH+5,3                                                      
         MVI   SPLESTH+5,3                                                      
         SPACE 1                                                                
*              EDIT CLI/PRD/EST/NET/DPT/PACKAGE                                 
*                                                                               
AE4      DS    0H                  CLIENT VALIDATION                            
         LA    R2,SPLCLIH                                                       
         NETGO NVCLIALL,DMCB,0                                                  
*                                                                               
         CLI   LSTRD,1             IF 1ST TIME XAGY READ                        
         BNE   *+10                                                             
         MVC   SPLCLI(4),LSTCLT    RESET @NNN TO SCREEN FIELD                   
*                                                                               
         MVI   NBDODEMS,C'N'       PRESET FOR NO DEMOS                          
         CLI   BHRD,0              IF BILLHEAD READ                             
         BE    AE4D                                                             
         CLI   OFFLINE,C'Y'                                                     
         BNE   AE4D                                                             
         MVC   BHAGYMD,NBACTAM     SET AGY/MD OF CLIENT REC                     
         CLC   =C'ALL',NBSELCLI                                                 
         BE    AE4D                                                             
         MVC   BHSELCLI,NBEFFCLI   SET CLIENT                                   
         SPACE 1                                                                
*                                                                               
AE4D     DS    0H                                                               
*        XC    WORK,WORK           READ WR PROFILE                              
*        MVC   WORK(4),=C'S0WR'                                                 
*        MVC   WORK+4(2),AGENCY                                                 
*        MVC   WORK+6(1),NBSELMED                                               
*        CLI   NBSELOFF,X'40'                                                   
*        BNE   WR012                                                            
*        CLC   NBSELCLI,=C'ALL'                                                 
*        BE    WR014                                                            
*        MVC   WORK+7(3),NBSELCLI                                               
*        MVI   WORK+10,C'*'                                                     
*        L     R1,NBAIO                                                         
*        USING CLTHDR,R1                                                        
*        MVC   WORK+11(1),COFFICE                                               
*        B     WR014                                                            
*        DROP  R1                                                               
*                                                                               
WR012    DS    0H                                                               
*        MVI   WORK+10,C'*'                                                     
*        MVC   WORK+11(1),NBSELOFF                                              
                                                                                
*WR014    GOTO1 GETPROF,DMCB,WORK,WORK+20,DATAMGR                               
                                                                                
*         CLI   WORK+22,C'Y'       TEST FILTER REQUIRED                         
*         BNE   WR016                                                           
*         LA    R2,SPLFLTH          YES-                                        
*         GOTO1 ANY                                                             
*         CLI   5(R2),4             AT LEAST 4 CHARACTERS                       
*         BNE   FILTERR                                                         
*                                                                               
WR016    DS    0H                                                               
         TM    NDCOLIND,X'20'      TEST FOR PUP REQUEST                         
         BNO   WR017                                                            
         MVC   SPLPRO(3),=CL3'ALL'                                              
         MVC   SPLEST(3),=CL3'ALL'                                              
         MVI   SPLPROH+5,3                                                      
         MVI   SPLESTH+5,3                                                      
         SPACE 1                                                                
*                                  PRODUCT & PRODUCT GROUP                      
WR017    LA    R2,SPLPROH                                                       
         NETGO NVPRDALL,DMCB,0                                                  
         CLC   =C'PEN',SPLCLI                                                   
         BNE   WR017A                                                           
         CLC   =C'BD',NBSELAGY                                                  
         BNE   WR017A                                                           
* - GET FULL AGENCY ID                                                          
         GOTO1 =A(OVERFLW),DMCB,(RC),6,RR=YES                                   
         BNE   EDERR                                                            
*                                                                               
WR017A   CLI   BHRD,0              CHK BILL HEADER READ                         
         BE    WR017D                                                           
         CLI   OFFLINE,C'Y'                                                     
         BNE   WR017D                                                           
         CLC   =C'ALL',NBSELPRD                                                 
         BE    WR017D                                                           
         CLC   =C'POL',NBSELPRD                                                 
         BE    WR017D                                                           
         MVC   BHSELPRD,NBSELPRD                                                
         SPACE 1                                                                
*                                  ESTIMATE VALIDATION                          
WR017D   LA    R2,SPLESTH                                                       
         NETGO NVESTRNG,DMCB,0,NDDEMBLK                                         
         CLI   BHRD,0              CHK BILL HEADER READ                         
         BE    WR017E                                                           
         CLI   OFFLINE,C'Y'                                                     
         BNE   WR017E                                                           
         MVC   BHSELEST,NBSELEST                                                
         MVC   BHSELESE,NBSELESE                                                
         SPACE 1                                                                
WR017E   MVI   FTERMFLG,1          (OPTIONAL FIELDS)                            
         SPACE 1                                                                
*                                  NETWORK VALIDATION                           
         DS    0H                                                               
         XC    BINMKT,BINMKT                                                    
         LA    R2,SPLNETH                                                       
         NETGO NVNETALL,DMCB,BINMKT                                             
         CLI   BHRD,0              CHK BILL HEADER READ                         
         BE    WR20                                                             
         CLI   OFFLINE,C'Y'                                                     
         BNE   WR20                                                             
         CLC   =C'ALL,',SPLNET     MEDIA FILTER                                 
         BNE   *+10                                                             
         MVC   BHSELMED,SPLNET+4                                                
         CLC   =C'ALL',NBSELNET                                                 
         BE    *+10                                                             
         MVC   BHSELNET,NBSELNET                                                
         SPACE 1                                                                
*                                  DAYPART VALIDATION                           
WR20     LA    R2,SPLDPTH                                                       
         NETGO NVDPTALL,DMCB,0                                                  
         OI    SPLDPTH+6,X'80'                                                  
         CLI   BHRD,0                                                           
         BE    WR30                                                             
         CLI   OFFLINE,C'Y'                                                     
         BNE   WR30                                                             
         MVC   BHSELDPT,NBSELDP                                                 
         SPACE 1                                                                
*                                  PACKAGE VALIDATION                           
WR30     LA    R2,SPLPAKH                                                       
         TM    NDCOLIND,X'20'                                                   
         BNO   VPAK20                                                           
         XC    SPLPRO,SPLPRO                                                    
         XC    SPLEST,SPLEST                                                    
         MVI   SPLPROH+5,0                                                      
         MVI   SPLESTH+5,0                                                      
         GOTO1 NDVALPLN            FOR PUP REQUEST VALIDATE PLAN                
*-IF NO INPUT IN DATE FIELDS DERIVE THE DATES FROM THE                          
*-PLAN RECORD.NDVALPLN STORES THE DATES IN WORK+20.                             
         OC    WORK+20(3),WORK+20  WAS DATES GENERATED                          
         BZ    VPAK30                                                           
         CLI   SPLRSTRH+5,0        WAS DATE INPUTTED                            
         BNE   VPAK15                                                           
         MVI   SPLRSTRH+5,7                                                     
         OC    SPLRSTRH+6,X'80'    TRANSMIT THE FIELD                           
         MVC   SPLRSTR(7),WORK+20                                               
VPAK15   CLI   SPLRENDH+5,0        WAS END DATE INPUTTED                        
         BNE   VPAK30                                                           
         MVI   SPLRENDH+5,8                                                     
         OC    SPLRENDH+6,X'80'    TRANSMIT THE FIELD                           
         MVC   SPLREND(8),WORK+27                                               
         B     VPAK30                                                           
         SPACE 2                                                                
VPAK20   NETGO NVPAKLOK,DMCB,0                                                  
         SPACE 1                                                                
VPAK30   LA    R2,SPLFLAVH         FLAVOR                                       
         GOTO1 ANY                                                              
         MVC   NDFLAVOR,WORK                                                    
         CLC   =C'BH',NDFLAVOR     IF BH READ                                   
         BNE   *+10                                                             
         MVC   NDFLAVOR,=C'E '     MAKE IT E FLAVOR                             
         CLI   NDFLAVOR+1,C'G'     IF GOALS ONLY REQUESTED                      
         BNE   VPK32                                                            
         MVI   NDFLAVOR+1,X'40'    GET IT OUT OF NDFLAVOR                       
         MVI   SPLFLAV+2,C'G'   SET IT IN SPLFLAV+2                             
         MVI   SPLFLAV+1,X'40'                                                  
         MVI   WORK+1,X'40'                                                     
VPK32    LA    R3,FLAVLIST                                                      
         BAS   RE,LISTVAL                                                       
         SPACE 1                                                                
         LA    R2,SPLOPTH          OPTIONS                                      
         GOTO1 NDVALOPT                                                         
         SPACE 1                                                                
         LA    R2,SPLOTHH          OTHERS                                       
         GOTO1 NDVALOTH                                                         
         SPACE 1                                                                
         TM    NDDOWNL,X'80'       IF WE ARE DOWNLOADING                        
         BNO   VREC2                                                            
         CLI   CONOUT,C' '            AND OUTPUT TYPE NOT REQUESTED             
         BH    VREC2                                                            
         MVC   CONOUT(8),=CL8'DOWN'   DEFAULT TO OUTPUT OF 'DOWN'               
         OI    CONOUTH+6,X'80'                                                  
         MVI   CONOUTH+4,4                                                      
         MVC   TWAOUT,CONOUT                                                    
         SPACE 1                                                                
VREC2    LA    R2,SPLRSTRH         RUN START                                    
         NETGO NVSTRDAT,DMCB                                                    
         LA    R2,SPLRENDH         AND START                                    
         NETGO NVENDDAT,DMCB                                                    
         BAS   RE,BDLIST           BUILD DATE LISTS                             
         CLI   BHRD,0              CHK BILLHEADER READ                          
         BE    VREC2D                                                           
         CLI   OFFLINE,C'Y'                                                     
         BNE   VREC2D                                                           
         MVC   BHSELSTR,NBSELSTR                                                
         MVC   BHSELEND,NBSELEND                                                
         SPACE 1                                                                
VREC2D   LA    R2,SPLFILTH         ACCOUNTING FILTERS                           
         NETGO NVFILT,DMCB                                                      
         SPACE 1                                                                
         LA    R2,SPLALLUH         ALL UNITS?                                   
         CLI   SPLFLAV,C'E'         ..IN E FLAVOR                               
         BNE   ALLU5                                                            
         CLC   SPLFLAV+1(2),=2X'40'                                             
         BH    ALLU5                                                            
         CLI   SPLALLU,X'40'         ..SET TO Y IF BLANK                        
         BH    ALLU5                                                            
         MVI   SPLALLU,C'Y'                                                     
         MVI   SPLALLUH+5,1                                                     
         OI    SPLALLUH+6,X'80'                                                 
ALLU5    NETGO NVGETFLD,DMCB                                                    
         SPACE 1                                                                
         XC    NDPERCNT,NDPERCNT   OPTIONAL PERCENTAGE OVERRIDE                 
         LA    R2,SPLPCTH                                                       
         NETGO NVGETFLD,DMCB                                                    
         BZ    ED4                                                              
         MVC   NDPERCNT,=C'COST'   WE'LL PASS THROUGHT THE WORD COST            
         CLC   FLD(4),=C'COST'     IF IT IS INPUT                               
         BE    ED2                                                              
         LR    R3,R1               (R1 IS LENGTH OF FIELD)                      
         GOTO1 CASHVAL,DMCB,FLD,(R3)                                            
         CLI   DMCB,X'FF'          CHECK FOR ERROR                              
         BE    EDERR                                                            
         MVC   NDPERCNT,DMCB+4     SAVE VALUE IN NDPERCNT                       
         B     ED4                                                              
         SPACE 1                                                                
ED2      MVC   HALF,=H'644'        COST NOT ALLOWED CLI=ALL                     
         CLC   SPLCLI(3),=C'ALL'                                                
         BE    ERR2                                                             
         MVC   HALF,=H'645'        COST NOT ALLOWED WITH PROD=POL               
         CLC   SPLPRO(3),=C'POL'                                                
         BE    ERR2                                                             
         SPACE 1                                                                
ED4      LA    R2,SPLDEMH          DEMOS                                        
         MVI   ERROR,INVALID                                                    
         MVI   NDNDEMOS,12         MAX 12                                       
         NETGO NVDEM,DMCB,DBLOCK,NDDEMBLK                                       
         CLC   LSTCLT,=C'@CHR'     ..IF CHR CROSS-AGY READ                      
         BNE   ED4D                                                             
         CLI   SPLDEMH+5,0         ..DEMOS MUST BE INPUT                        
         BE    EDERR                                                            
         LA    R4,RELO                                                          
         S     R4,RELO                                                          
         L     R3,=A(MYIO)                                                      
         AR    R3,R4                                                            
         GOTO1 SCANNER,DMCB,SPLDEMH,(4,0(R3)),0                                 
         ZIC   R4,4(R1)                                                         
         LTR   R4,R4                                                            
         BZ    EDERR                                                            
         C     R4,=F'3'            MAX OF 3 DEMOS                               
         BH    EDERR                                                            
ED4B     CLC   =C'M2554',12(R3)    ONLY VALID CHR CROSS-AGY DEMOS               
         BE    ED4C                                                             
         CLC   =C'A2554',12(R3)                                                 
         BE    ED4C                                                             
         CLC   =C'HOMES',12(R3)                                                 
         BNE   EDERR                                                            
ED4C     LA    R3,32(R3)                                                        
         BCT   R4,ED4B                                                          
         XC    0(100,R3),0(R3)     CLEAR SCAN BLOCK                             
         SPACE 1                                                                
ED4D     LA    R2,SPLTITLH         TITLE                                        
         GOTO1 NDVALTIT                                                         
         EJECT                                                                  
*              NOW HANDLE THE DRONE BIT                                         
         SPACE 3                                                                
         CLI   OFFLINE,C'Y'                                                     
         BE    ED5                                                              
         LA    R1,SVDRON           ONLINE BUFF                                  
         ST    R1,DRSTBUF                                                       
         A     R1,=F'1024'                                                      
         ST    R1,DRENDBUF                                                      
         B     ED6                                                              
ED5      L     R1,NDADPG           DRONE NEEDS A DPG BUFFER                     
         ST    R1,DRSTBUF                                                       
         A     R1,=F'24000'                                                     
         BCTR  R1,0                                                             
         ST    R1,DRENDBUF                                                      
         SPACE 1                                                                
ED6      MVI   NDRPTYPE,C'D'       DO THE DETAILS FIRST                         
         BAS   RE,DODRONE                                                       
         LA    R4,RELO                                                          
         S     R4,RELO                                                          
         GOTO1 =A(OVERFLW),DMCB,(RC),2,RR=R4    HICOM4                          
         MVI   NDRPTYPE,C'R'       THEN, IF REQUESTED, RECAPS                   
         CLI   NDANYREC,C'Y'                                                    
         BNE   EDEND                                                            
         CLI   OFFLINE,C'Y'                                                     
         BNE   *+10                                                             
         MVC   DRSTBUF,DRCURBUF    FOR SECOND REPORT, RESET START               
*                                  TO CURRENT DRONE POINTER                     
         XC    NDPRGFLT,NDPRGFLT   THIS CALL WILL REBUILD THE LIST              
         BAS   RE,DODRONE                                                       
         CLI   NDRCPOPT,C'Y'       IF GRAND RECAP WAS REQUESTED...              
         BNE   EDEND                                                            
*                                  GENERATE A GRAND RECAP                       
         XC    NDPRGFLT,NDPRGFLT   THIS CALL WILL REBUILD THE LIST              
         MVC   DRSTBUF,DRCURBUF                                                 
         BAS   RE,GRRECAP                                                       
         B     EDEND                                                            
RELO     DC    A(*)                                                             
         SPACE 3                                                                
GRRECAP  NTR1                                                                   
         GOTO1 NDINIDRO                                                         
         LA    R2,2                                                             
         BAS   RE,PUTLIT                                                        
         B     DODRONE4                                                         
DODRONE  NTR1                                                                   
         MVI   NDPUPREQ,0                                                       
         GOTO1 NDINIDRO                                                         
         LA    R2,1                                                             
         BAS   RE,PUTLIT                                                        
         LA    R2,SPLLEFTH         LEFT HEADERS                                 
         GOTO1 NDVALLFT                                                         
         LA    R2,SPLRGHTH         RIGHT HEADERS                                
         GOTO1 NDVALRGT                                                         
         CLI   NDANYREC,C'Y'       IF THERE IS ALSO A RECAP                     
         BNE   DODRONE2                                                         
         CLI   OFFLINE,C'Y'           AND WE ARE OFF LINE                       
         BNE   DODRONE2                                                         
         XC    WORK,WORK           NEED TO POP IN RECORD NUMBER                 
         MVI   WORK+5,6                                                         
         MVC   WORK+8(6),=C'RECORD'                                             
         LA    R1,WORK             THIS IS SO THAT THE DETAILS WILL             
         ST    R1,DRACCFLD         INTERLEAVE WITH THE RECAPS                   
         OI    DRFLAGS,DREXPDIC                                                 
         MVI   DRACTION,DRROW                                                   
         GOTO1 NDDRONE,DMCB,DRGEN                                               
         NI    DRFLAGO,X'7F'                                                    
         MVI   DRACTION,DRGENROW                                                
         GOTO1 NDDRONE,DMCB,DRGEN                                               
         SPACE 1                                                                
DODRONE2 CLI   NDRPTYPE,C'D'       FOR DETAILS,                                 
         BNE   DODRONE4                                                         
         LA    R2,SPLMIDH          MIDLINE                                      
         GOTO1 NDVALMID                                                         
         SPACE 1                                                                
DODRONE4 LA    R2,SPLDETSH         DETAILS                                      
         CLI   NDRPTYPE,C'R'                                                    
         BNE   *+8                                                              
         LA    R2,SPLRECPH         OR RECAPS                                    
         GOTO1 ANY                 (MUST BE AT LEAST 1)                         
         GOTO1 NDVALROW            COLUMNS                                      
         LA    R2,SPLCOLSH                                                      
         GOTO1 NDVALCOL                                                         
         B     XIT                                                              
         EJECT                                                                  
*              PUT A LITERAL INTO DRONE                                         
         SPACE 3                                                                
*              INPUT               R2=CHARACTER TO BE INSERTED                  
         SPACE 1                                                                
PUTLIT   NTR1                                                                   
         CLI   OFFLINE,C'Y'                                                     
         BNE   XIT                                                              
         L     R1,DRCURBUF                                                      
         MVC   0(2,R1),=X'2002'           IN                                    
         LA    R1,2(R1)                                                         
         MVC   0(5,R1),=X'2205E70001'     T=X                                   
         LA    R1,5(R1)                                                         
         MVC   0(3,R1),=X'230301'         L=1                                   
         LA    R1,3(R1)                                                         
         MVC   0(4,R1),=X'87040200'       LIT=X'??'                             
         STC   R2,3(R1)                                                         
         LA    R1,4(R1)                                                         
         ST    R1,DRCURBUF                                                      
         B     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE NENETGOALD                                                     
       ++INCLUDE NENETPUPD                                                      
         EJECT                                                                  
*                                                                               
* AUTHORIZED AGENCY TABLE - AGY / REQUEST ID / ADDRESS OF LIST                  
         DS    0F                                                               
AGYTABL  DC    CL4'AT',CL4'ATT',A(ATTLIST)                                      
         DC    CL4'AT',CL4'BCS',A(BCSLIST)                                      
         DC    CL4'AT',CL4'YAL',A(YALLIST)                                      
         DC    CL4'NW',CL4'YAL',A(YALLIST)                                      
         DC    CL4'BJ',CL4'CHR',A(CHRLIST)                                      
         DC    CL4'BD',CL4'CHR',A(CHRLIST)                                      
         DC    CL4'CE',CL4'CHR',A(CHRLIST)                                      
         DC    CL4'FC',CL4'FCB',A(FCBLIST)                                      
         DC    CL4'FC',CL4'FC1',A(FC1LIST)                                      
         DC    CL4'FC',CL4'FC2',A(FC2LIST)                                      
*                                                                               
         DC    CL4'TH',CL4'RES',A(RESLIST)                                      
         DC    CL4'TH',CL4'GI2',A(GI2LIST)                                      
         DC    CL4'TH',CL4'LEX',A(LEXLIST)                                      
         DC    CL4'TH',CL4'TMS',A(TMSLIST)                                      
*                                                                               
         DC    CL4'DF',CL4'RES',A(RESLIST)                                      
         DC    CL4'DF',CL4'GI2',A(GI2LIST)                                      
         DC    CL4'DF',CL4'LEX',A(LEXLIST)                                      
         DC    CL4'DF',CL4'TMS',A(TMSLIST)                                      
         DC    X'FF'                                                            
         SPACE 2                                                                
*                                                                               
*                                                                               
         DS    0H                                                               
       ++INCLUDE NETAGYLST                                                      
*                                                                               
NETBLKSV DS    CL2000                                                           
         EJECT                                                                  
*                                                                               
*                                                                               
         DS    0D                                                               
*                                                                               
* - DO NOT USE R5 - DRIVER CALLS A HOOK IN 1ST NMOD AND THIS                    
*                   IS DEPENDENT ON R5                                          
*                                                                               
OVERFLW  NMOD1 0,*NE20OV*,R8                                                    
         L     RC,0(R1)                                                         
         L     RF,4(R1)                                                         
         SLL   RF,2                                                             
         B     BRANCHTB(RF)                                                     
EXIT     XIT1                                                                   
*                                                                               
BRANCHTB DS    0H                                                               
         B     SPARE0           0                                               
         B     FLTCCML          1=FILTER COMMERCIAL CLASS                       
         B     HICOM4           2=COMMENTS                                      
         B     XAGYRD           3=CROSS AGY READ                                
         B     XINITB           4=INIT NET BLOCK                                
         B     GETCHKS          5=GET CHECKING INFO                             
         B     GETAGYID         6=GET FULL AGY ID                               
*                                                                               
SPARE0   DC    H'0'                                                             
         SPACE                                                                  
*                                                                               
GETEL2   AH    (R4),DATADISP                                                    
*                                                                               
FIRST2   CLI   0((R4)),0                                                        
         BNE   *+10                                                             
         CLI   0((R4)),1                                                        
         BR    RE                                                               
         CLI   ELCODE,0                                                         
         BCR   8,RE                                                             
         CLC   ELCODE,0((R4))                                                   
         BCR   8,RE                                                             
NEXT2    SR    RF,RF                                                            
         IC    RF,1((R4))                                                       
         LTR   RF,RF                                                            
         BNZ   *+10                                                             
         CLI   1((R4)),1                                                        
         BR    RE                                                               
         AR    (R4),RF                                                          
         B     FIRST2                                                           
         EJECT                                                                  
*                                                                               
TESTMASK NTR1                                                                   
         LA    R2,NBESTMSK                                                      
         SR    R0,R0                                                            
         SLDL  R0,29                                                            
         SRL   R1,29                                                            
         AR    R2,R0                                                            
         LA    R1,BITLIST(R1)                                                   
         MVC   BITTEST,0(R2)                                                    
         NC    BITTEST,0(R1)                                                    
         CLI   BITTEST,0                                                        
         XIT1                                                                   
*        BE    NO                                                               
*        B     YES                                                              
         SPACE 1                                                                
BITLIST  DC    X'8040201008040201'                                              
BITTEST  DS    CL1                                                              
         EJECT                                                                  
                                                                                
* INPUT:   WORKINV=MONTH(2BYTES)+INVNO(4BYTES)                                  
*          WORKYMD=YYMMDD OF BILLING RUN DATE                                   
*                                                                               
* OUTPUT:  3BYTE 1(Y/M) + 2(INVNO)                                              
GETBYMN  NTR1                                                                   
         PACK  DUB,WORKINV+2(4)   .GET BINARY VALUE OF INVNO                    
         CVB   R1,DUB                                                           
         STCM  R1,3,WORK+1                                                      
* - CONVERT BILLING RUN MONTH TO BINARY                                         
         PACK  DUB,WORKINV(2)     .GET BINARY VALUE OF MONTH                    
         CVB   R1,DUB                                                           
         STCM  R1,1,WORK                                                        
* GET YEAR INTO ZONE OF MONTH BYTE                                              
         PACK  BYTE,WORKYMD+1(1)   .SWITCH FN TO NF  (YYMMDD)                   
         NI    BYTE,X'F0'          .MAKE IT N0                                  
         OC    WORK(1),BYTE        .SET IT INTO FIRSTHALF OF MONTH BYTE         
         B     XIT                                                              
                                                                                
                                                                                
* - ROUTINE GETS MONTH OF SERVICE OF UNIT DATE                                  
GETMOS   DS    0H                                                               
         L     RF,=A(PERLIST)                                                   
GETM4    DS    0H                                                               
         CLC   NBACTDAT,2(RF)      TEST DATE VS START                           
         BE    GETM8                                                            
         BNL   *+6                                                              
         DC    H'0'                TIME TO LENGTHEN PERLIST                     
         CLC   NBACTDAT,4(RF)                                                   
         BNH   GETM8                                                            
         LA    RF,6(RF)            NEXT ENTRY                                   
         B     GETM4               NOTE- LIST ENDS IN FF                        
GETM8    DS    0H                                                               
         MVC   BYMOS,0(RF)         SET YR/MOS                                   
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
* - READS SPOT00 PROFILE TO SET UP DATE LIST                                    
GOMOBILE NTR1                                                                   
                                                                                
* - HAVE WE ALREADY SET UP DATE LIST                                            
         CLC   NBCLICOD,PROFILSV                                                
         BE    XIT                                                              
         MVC   PROFILSV(3),NBCLICOD                                             
                                                                                
* - NEED TO PASS THESE ADDS TO MOBILE                                           
* - NOT ENOUGH ROOM TO LINK THEM IN                                             
         LA    R2,MOBILADS                 ADDRESSES FOR MOBILE                 
         XC    0(4,R2),0(R2)               (GETBROAD) LINKED                    
         MVC   4(4,R2),ADDAY                                                    
         MVC   8(4,R2),GETDAY                                                   
         MVC   12(4,R2),DATCON                                                  
*                                                                               
         XC    WORK(30),WORK                                                    
         MVC   WORK(2),=C'SP'                                                   
         MVC   WORK+2(2),=C'B3'           GET B3 PROFILE                        
         MVC   WORK+4(2),NBEFFAGY         AGENCY ALPHA                          
         MVC   WORK+6(1),NBSELMFL         ..IF FILTERING                        
         CLI   NBSELMFL,0                                                       
         BNE   *+10                                                             
         MVC   WORK+6(1),NBSELMED         ..ELSE SET IT TO NETWORK              
         MVC   WORK+7(3),NBCLICOD         CLIENT                                
         CLI   NBEFFOFF,X'40'                                                   
         BH    *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),NBEFFOFF        OFFICE                                
         L     R2,DATAMGR                                                       
         L     R3,ATWA                                                          
         XC    DMCB(16),DMCB                                                    
         GOTO1 GETPROF,DMCB,(0,WORK),MYWORK2,(0,(R2))                           
*                                                                               
         XC    WORK(30),WORK                                                    
         MVC   WORK(2),=C'SP'                                                   
         MVC   WORK+2(2),=C'00'                                                 
         MVC   WORK+4(2),NBEFFAGY         AGENCY ALPHA                          
         MVC   WORK+6(1),NBSELMFL         ..MEDIA FILTER                        
         CLI   NBSELMFL,0                                                       
         BNE   *+10                                                             
         MVC   WORK+6(1),NBSELMED         ..ELSE SET IT TO NETWORK              
         MVC   WORK+7(3),NBCLICOD         CLIENT                                
         CLI   NBEFFOFF,X'40'                                                   
         BH    *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),NBEFFOFF        OFFICE                                
         L     R2,DATAMGR                                                       
         L     R3,ATWA                                                          
         XC    DMCB(16),DMCB                                                    
         GOTO1 GETPROF,DMCB,(0,WORK),MYWORK,(0,(R2))                            
* - SET B3 PROFILE VALUES INTO 00 DATE AREAS                                    
         MVC   MYWORK+2(1),MYWORK2                                              
         MVC   MYWORK+6(3),MYWORK2+1                                            
         IC    R0,MYWORK+2       DATE CONTROL                                   
* - SET (NBSELSTR - 1 YEAR)  TO DUB FOR BROAD MOBILE DATELIST                   
         L     R4,=F'-450'                                                      
         GOTO1 ADDAY,DMCB,NBSELSTR,MYWORK2,(R4)                                 
         MVC   MYWORK2+6(6),NBSELEND                                            
                                                                                
* - BUILD LONG LIST OF DATE PAIRS                                               
         L     R2,=A(MYIO)                                                      
         LA    R4,MYWORK         PASS ADDRESS OF 00 PROFILE                     
         GOTO1 =V(MOBILE),DMCB,(208,MYWORK2),((R0),(R2)),MOBILADS,(R4)          
                                                                                
*                                  FIND FIRST PERIOD OF A NEW YEAR              
SETD4    DS    0H                                                               
         BAS   RE,CKNEWYR          TEST NEW YEAR                                
         BZ    SETD6               YES                                          
         CLI   5(R2),0             IF ZERO WE GET LOOP IN SETD8                 
         BE    SETD6                                                            
         LA    R2,4(R2)                                                         
         B     SETD4                                                            
*                                  BUILD  A LIST OF YM, START-END               
SETD6    DS    0H                                                               
         L     R3,=A(PERLIST)                                                   
SETD7    DS    0H                                                               
         ZIC   R0,2(R2)                                                         
         SRL   R0,1                                                             
         STC   R0,BYTE             YEAR                                         
         SR    R4,R4               FOR PER SEQUENCE WITHIN YR                   
SETD8    DS    0H                                                               
         LA    R4,1(R4)                                                         
         MVC   0(1,R3),BYTE        YEAR                                         
         STC   R4,1(R3)            MONTH                                        
         MVC   2(4,R3),0(R2)       START-END OF PER                             
         LA    R3,6(R3)                                                         
         LA    R2,4(R2)                                                         
         CLI   0(R2),X'FF'                                                      
         BE    SETD12              EOL                                          
         BAS   RE,CKNEWYR          TEST NEW YEAR                                
         BZ    SETD7               YES                                          
         B     SETD8                                                            
*                                                                               
SETD12   DS    0H                                                               
*                                                                               
SETDATEX DS    0H                                                               
         B     XIT                                                              
         SPACE 1                                                                
*                                  FIND START OF NEW YEAR                       
*                                  1) A PERIOD THAT SPANS YEAR CHANGE           
*                                     AND BEGINS NO FURTHER AWAY                
*                                     FROM 12/31 THAN IT ENDS                   
*                             OR   2) A PERIOD THAT STARTS BEFORE 1/14          
*                                                                               
CKNEWYR  DS    0H                                                               
         MVC   DUB(4),0(R2)                                                     
         NI    DUB,X'01'           STRIP YEAR                                   
         CLC   DUB(2),NEWYRLO                                                   
         BL    CKNYYES                                                          
*                                                                               
         CLC   DUB(2),PDDEC                                                     
         BNH   CKNYNO                                                           
*                                                                               
         NI    DUB+2,X'01'                                                      
         CLC   DUB+2(2),PDDEC                                                   
         BH    CKNYNO                                                           
*                                                                               
         NI    DUB+1,X'1F'         ISOLATE DAY                                  
         ZIC   RF,DUB+1                                                         
         LA    R0,30                                                            
         SR    R0,RF                                                            
         BNP   CKNYYES             STARTS ON 30TH OR 31ST                       
         STC   R0,DUB+4                                                         
*                                                                               
         NI    DUB+3,X'1F'         ISOLATE DAY                                  
         CLC   DUB+4(1),DUB+3                                                   
         BNH   CKNYYES                                                          
*                                                                               
CKNYNO   DS    0H                                                               
         LTR   RE,RE                                                            
         BR    RE                                                               
CKNYYES  DS    0H                                                               
         SR    R0,R0                                                            
         BR    RE                                                               
*                                                                               
*                                                                               
NEWYRLO  DC    X'002E'             JAN14                                        
PDDEC    DC    X'0180'             DEC00                                        
*                                                                               
MOBILADS DS    4F                  ADDRESSES PASSED TO MOBILE                   
         EJECT                                                                  
* - CLIENT GROUP FILTERING FOR BILL RECORDS                                     
*   KEY HAS BILLING HEADER RECORD                                               
CGRPFILT NTR1                                                                   
         L     R4,ANETWS1          CLIENT HEADER SITS IN ANETWS1                
         USING CLTHDR,R4                                                        
         CLC   CKEYCLT,KEY+2       ...DO WE NEED NEW CLIENT HEADER              
         BE    VCL2                                                             
         MVC   MYKEY,KEY           ...YES/SAVE CURRENT KEY                      
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),MYKEY+1     AGY/MED + CLIENT                            
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FILENAME,=C'SPTFILE '                                            
         MVC   AIO,ANETWS1         CLIENT REC SITS IN ANETWS1                   
         GOTO1 GETREC                                                           
         L     R1,=A(MYIO)        RESET AIO AREA FOR BHREAD                     
         ST    R1,AIO                                                           
         XC    KEY,KEY            RESET SEQ READ FOR BILL HEADER RECORD         
         MVC   KEY,MYKEY                                                        
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
* - CHECK CLIENT GROUP FILTERING                                                
VCL2     LA    R0,5                                                             
         LA    RF,CGRP1                                                         
         CLC   NBSELCGR(1),0(RF)       CHECK SCHEME LETTER                      
         BE    VCL5                                                             
VCL4     LA    RF,3(RF)                                                         
         BCT   R0,*-14                                                          
         B     VCLNO                                                            
VCL5     UNPK  DUB(5),1(3,RF)      UNPK PWOS                                    
         LA    R3,DUB                                                           
         LA    RE,NBSELCGR+1                                                    
         LA    R1,4                                                             
VCL6     CLI   0(RE),X'C1'         IF LETTER OR NUMBER                          
         BL    VCL7                                                             
         CLC   0(1,RE),0(R3)       MUST MATCH                                   
         BNE   VCL4                IF NO MATCH,TEST AGAINST NXT CGRP            
VCL7     LA    RE,1(RE)                                                         
         LA    R3,1(R3)                                                         
         BCT   R1,VCL6                                                          
         MVC   NBACTCGR(2),1(RF)   SET CLIENT GROUP CODE  PWOS                  
VCLYES   SR    RE,RE               CLIENT PASSED TESTS                          
VCLNO    LTR   RE,RE                                                            
VCLX     B     EXIT                                                             
*                                                                               
MYKEY    DS    CL13                                                             
         EJECT                                                                  
* - FILTER BILLING HEADER                                                       
FILTBH   NTR1                                                                   
         LA    R4,KEY                                                           
         USING BKEY,R4                                                          
         CLI   BHSELCLI,0                                                       
         BE    *+14                                                             
         CLC   BKEYCLT,BHSELCLI                                                 
         BNE   FBX                                                              
         CLI   BHSELPRD,0                                                       
         BE    *+14                                                             
         CLC   BKEYPRD,BHSELPRD                                                 
         BNE   FBX                                                              
         CLI   BHSELEST,0                                                       
         BE    *+14                                                             
         CLC   BKEYEST,BHSELEST                                                 
         BNE   FBX                                                              
         CLI   BHSELSTB,0          YEAR/MONTH OF SERVICE                        
         BE    FB10                                                             
         CLC   BKEYYSRV(2),BHSELSTB                                             
         BL    FBNO                                                             
FB10     CLI   BHSELENB,0                                                       
         BE    FBYE                                                             
         CLC   BKEYYSRV(2),BHSELENB                                             
         BH    FBNO                                                             
FBYE     SR    R4,R4                                                            
*                                                                               
FBNO     LTR   R4,R4                                                            
*                                                                               
FB20     DS    0H                                                               
*                                                                               
FBX      B     EXIT                                                             
*                                                                               
* - WORK AREA FOR RDBELEM                                                       
         DS    0D                                                               
MYDM     DS    CL96                                                             
ABELEM   DS    F                                                                
SVKEY    DS    CL13                                                             
PROFILSV DS    CL10                                                             
BYMOS    DS    CL2                 BILLING YR/MOS FROM GETMOS                   
RUNDATE  DS    0CL6                                                             
RUNDYY   DS    CL2                                                              
RUNDMM   DS    CL2                                                              
RUNDDD   DS    CL2                                                              
INVNUMBR DS    CL6         CURRENT INV NUMBER MONTH(2) + NUMBER                 
WORKINV  DS    CL6                 TEMP INVOICE WORK AREA                       
WORKYMD  DS    CL6                 TEMP YYMMDD WORK AREA                        
CURRBINV DS    CL3                 TEMP CL1(Y/M) + CL2(INVNO)                   
MYWORK   DS    CL100                                                            
MYWORK2  DS    CL100                                                            
         EJECT                                                                  
*                                                                               
*  - COMMERCIAL CLASS FILTERING                                                 
*                                                                               
FLTCCML  DS    0H                  NOT NEEDED IN EDIT MODULE                    
         B     EXIT                                                             
         EJECT                                                                  
HICOM4   DS    0H            IF HIGHCOM IN HEADER, SET NDHIGHCM BYTE            
         LA    R2,SPLLEFTH                                                      
         LA    R4,8               4=MAX NUMB OF FIELDS IN EACH HEADER           
         MVI   NDHIGHCM,0                                                       
HIC10    CLI   5(R2),0                                                          
         BE    HIC12                                                            
         CLC   =C'HIGHCOM4',8(R2)                                               
         BE    HIC20                                                            
HIC12    ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BCTR  R4,0                                                             
         C     R4,=F'0'                                                         
         BNH   HICX                                                             
         C     R4,=F'4'            IF HICOM NOT IN LEFT HEADER                  
         BNE   HIC10                                                            
         LA    R2,SPLRGHTH         TRY RIGHT HEADER                             
         B     HIC10                                                            
*                                                                               
HIC20    DS    0H                                                               
         LA    R2,SPLLEFTH         YES/HICOM-SET APPROPRIATE BITS               
         LA    R4,8                4=MAX NUMB FIELDS IN EACH HEADER             
HIC30    CLI   5(R2),0                                                          
         BE    HIC40                                                            
         CLC   =C'CLI',8(R2)                                                    
         BNE   *+12                                                             
         OI    NDHIGHCM,X'01'                                                   
         B     HIC40                                                            
         CLC   =C'PRO',8(R2)                                                    
         BNE   *+12                                                             
         OI    NDHIGHCM,X'02'                                                   
         B     HIC40                                                            
         CLC   =C'EST',8(R2)                                                    
         BNE   *+12                                                             
         OI    NDHIGHCM,X'04'                                                   
         B     HIC40                                                            
         CLC   =C'NET',8(R2)                                                    
         BNE   *+12                                                             
         OI    NDHIGHCM,X'08'                                                   
         B     HIC40                                                            
         CLC   =C'DPT',8(R2)                                                    
         BNE   *+12                                                             
         OI    NDHIGHCM,X'10'                                                   
         B     HIC40                                                            
         CLC   =C'K',8(R2)                                                      
         BNE   HIC40                                                            
         OI    NDHIGHCM,X'20'                                                   
HIC40    ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BCTR  R4,0                                                             
         C     R4,=F'0'                                                         
         BNH   HICX                                                             
         C     R4,=F'4'                                                         
         BNE   HIC30                                                            
         LA    R2,SPLRGHTH         NOW TRY RIGHT HEADER                         
         B     HIC30                                                            
*                                                                               
HICX     B     EXIT                                                             
*                                                                               
SCANBLK  DS    CL420                                                            
         EJECT                                                                  
*                                                                               
         USING BHBLOCK,R3                                                       
GETPRD1  NTR1                  USES BHACTPRD AND RETURNS                        
         L     R1,ANETWS1      1 BYTE PROD IN WORK                              
         USING CLTHDR,R1                                                        
         LA    R1,CLIST                                                         
         LA    R2,220                                                           
GTP5     CLC   BHACTPRD,0(R1)                                                   
         BE    GOTPRD                                                           
         LA    R1,4(R1)                                                         
         BCT   R2,GTP5                                                          
         MVC   WORK(1),0                                                        
         B     *+10                                                             
GOTPRD   MVC   WORK(1),3(R1)                                                    
         B     EXIT                                                             
         DROP  R1,R3                                                            
                                                                                
*                                                                               
GETPRD3  NTR1                  RETURNS 3 BYTE PROD IN WORK                      
         L     R1,ANETWS1                                                       
         USING CLTHDR,R1                                                        
         LA    R1,CLIST                                                         
         LA    R2,220                                                           
GP5      CLC   WORK(1),3(R1)                                                    
         BE    GOTPRD3                                                          
         LA    R1,4(R1)                                                         
         BCT   R2,GP5                                                           
         MVC   WORK(3),=C'***'                                                  
         B     *+10                                                             
GOTPRD3  MVC   WORK(3),0(R1)                                                    
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
*  - CROSS AGENCY READ                                                          
*                                                                               
XAGYRD   DS    0H                  NOT NEEDED IN EDIT MODULE                    
         B     EXIT                                                             
         EJECT                                                                  
* READ UNIT PAYING ELEMENT AND GET CORRESPONDING CLEARANCE STATUS REC           
* THEN GO TO DRIVER WITH CHECK NUMBER                                           
* ROUTINE THUS CALLS DRIVER MULTIPLE TIMES FOR EACH UNIT                        
GETCHKS  DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* - GET FULL AGENCY ID                                                          
GETAGYID DS    0H                                                               
         L     RA,ATWA                                                          
         USING T320FFD,RA                                                       
         LA    R1,RELO2                                                         
         S     R1,RELO2                                                         
         L     R4,=A(MYIO)         YES-READ CONTROL ID REC FOR AGY ID           
         AR    R4,R1                                                            
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         MVC   CTIKNUM,T320FFD+10                                               
         GOTO1 DATAMGR,DMCB,(0,=CL8'DMREAD'),=CL8'CTFILE',(R4),(R4)             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    RE,CTIDATA                                                       
WRTRYAGN CLI   0(RE),X'02'                                                      
         BE    WRGOTIT                                                          
         ZIC   RF,1(RE)                                                         
         AR    RE,RF                                                            
         CLI   0(RE),0                                                          
         BNE   WRTRYAGN                                                         
         DC    H'0'                                                             
WRGOTIT  MVC   WORK(7),2(RE)       SET AGY ID IN WORK                           
         DROP  R4                                                               
         LA    R4,PENTABLE                                                      
WRGOT5   CLC   WORK(5),0(R4)       IS IT RESTRICTED AGY ID                      
         BE    WRGOT10             YES/CHECK IT                                 
         LA    R4,12(R4)                                                        
         CLI   0(R4),X'FF'                                                      
         BE    WRGYES              EOF/OK                                       
         B     WRGOT5                                                           
*                                                                               
WRGOT10  CLC   7(4,R4),SPLPRO      CHECK PRODUCT                                
         BE    WRGYES                                                           
         LA    R4,12(R4)                                                        
         CLI   0(R4),X'FF'                                                      
         BE    WRERR                                                            
         CLC   WORK(5),0(R4)       STILL SAME AGY ID                            
         BE    WRGYES              YES                                          
WRERR    MVI   ERROR,INVALID       NO MATCH                                     
         LTR   RE,RE                                                            
         B     EXIT                                                             
*                                                                               
WRGYES   B     EXIT                ASSUME COND CODE =                           
         DROP  RA                                                               
*                                                                               
PENTABLE DS    0H                                                               
         DC    CL7'BDPEA',C'V=3**'                                              
         DC    CL7'BDPEB',C'V=1**'                                              
         DC    CL7'BDPEB',C'V=2**'                                              
         DC    CL7'BDPEC',C'V=4**'                                              
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
XINITB   DS    0H                                                               
         B     EXIT                                                             
*                                                                               
*                                                                               
RELO2    DC    A(*)                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         EJECT                                                                  
         SPACE 1                                                                
         DS    0D                                                               
         DC    CL8'**MYIO**'                                                    
MYIO     DC    3000X'00'                                                        
         DC    CL8'*NETLIST'                                                    
NETLIST  DC    4000X'00'                                                        
         DC    CL8'PRODLIST'                                                    
PRODLIST DC    1600X'00'                                                        
         DC    CL8'*SPLBILL'                                                    
SPLBILL  DC    1000X'00'           SPLIT BILLING BLOCK                          
         DC    CL8'*STALIST'                                                    
STALIST  DC    2000X'00'                                                        
         DC    CL8'*GRPPRODS'      PRODUCTS FOR PRODUCT GROUPS                  
GRPPLIST DC    2560X'00'           10GROUPS X 256 PRODUCTS                      
*                                                                               
PLANIO   DS    CL2000              I/O AREA FOR PLAN RECORD                     
*                                                                               
PERLIST  DS    XL(15*13*6+1)       15YRS X 13 MNTHS X 6                         
*                                  MOS(2) + START(2) + END(2)                   
UDEFLST  DS    CL250               UDEF PROD/EST DATA                           
*                                  MOS(2) + START(2) + END(2)                   
GLRGXTN  DS    CL160               GLARG EXTENSION                              
         EJECT                                                                  
*              STORAGE FOR WRITER                                               
         SPACE 3                                                                
WRITERD  DSECT                                                                  
*        INCLUDE NETDEMOD                                                       
*        INCLUDE DEDBLOCK                                                       
       ++INCLUDE NETDEMOD                                                       
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE NENETRAWEX                                                     
         SPACE 1                                                                
PERTYPE  DS    CL3                                                              
NWEEKS   DS    F                                                                
NMONTHS  DS    F                                                                
NQUARTS  DS    F                                                                
DATEAREA DS    XL420               105 WEEKS                                    
         DS    XL100               25 MONTHS                                    
         DS    XL32                8 QUARTERS                                   
         DS    XL56                14 DAYS                                      
SVNBACT  DS    F                                                                
SVNBASS  DS    F                                                                
SVNBINT  DS    F                                                                
ABHRD    DS    F                   ADDRESS OF T32082 MODULE                     
MYHALF   DS    CL2                                                              
SVDRON   DS    CL1024              SAVE AREA FOR DRONE COMPUTATION              
*                                                                               
AAGYCLT  DS    F                  .CROSS AGENCY READ SAVE AREA                  
CURRLST  DS    CL5                                                              
PREVLST  DS    CL5                                                              
LSTRD    DS    CL1                .CLIENT/AGY READ COUNT                        
LSTRQAGY DS    CL2                .REQUEST AGENCY IN CROSS-AGY READ             
LSTCLT   DS    CL4                .SAVE @NNN                                    
*                                                                               
STAKEYSV DS    CL15                STATION KEY SAVE FOR CHQ READ                
MKTSV    DS    CL4                 SAVE MKT NUMBER FOR CHQ READ                 
BILLFLT  DS    CL4                 SAVE BILL FILTER DATES IN BHRD               
*                                                                               
BHRD     DS    CL1                 BILL HEADER READ                             
BHDTYPF  DS    CL1                 BILL HEADER READ DATE TYPE FILTER            
BHMANFLG DS    CL1                 BILL HEADER MANUAL BILL ONLY                 
BHBFLG   DS    CL2                 BILL HEADER B1,B2 ETC FLAG                   
CLTSAV   DS    CL3                 3 BYTE CLIENT CODE IN BH READ                
KEY2     DS    CL25                                                             
COMPLEN  DS    CL1                 COMPARE LENGTH OF KEY                        
PAKWORK  DS    PL12                                                             
BINMKT   DS    XL2                 2 BYTE MARKET NUMBER                         
TARGID   DS    CL8                                                              
TARGIDS  DS    CL336               TABLE OF COML IDS FROM UNIT                  
TARGLENE EQU   *-TARGIDS                                                        
CIDTBL   DS    CL240               (CML ID CL8 + CML CLASS CL4 ) X 20           
BILLDFLT DS    CL40                BILLING DATE COLUMN FILTER TABLE             
*                                  CL4(START/END) X 10 MAX COLUMN               
*                                                                               
         SPACE                                                                  
**AGYENTD  DSECT          AGENCY ENTRY LIST DSECT                               
*AEAGY    DS    CL2                 AGENCY                                      
*AECLT    DS    CL3                 CLIENT                                      
*AEPRD    DS    CL3                 PRODUCT                                     
*AEEST    DS    XL1                 ESTIMATE                                    
*         DS    XL1                 SPARE                                       
*AELENE   EQU   *-AEAGY                                                         
         SPACE                                                                  
AGYENTD  DSECT          AGENCY ENTRY LIST DSECT                                 
AEAGY    DS    CL2                 AGENCY                                       
AECLT    DS    CL3                 CLIENT                                       
         DS    CL15                SPARE                                        
AELENE   EQU   *-AEAGY                                                          
         EJECT                                                                  
         SPACE 3                                                                
*        INCLUDE NEGENINCLS                                                     
*        INCLUDE NEDATELSTD                                                     
*        INCLUDE DRGLOBAL                                                       
*        INCLUDE SPGENPRD                                                       
*        INCLUDE NECOMBLOK                                                      
*        INCLUDE NEGENCOM                                                       
         PRINT OFF                                                              
       ++INCLUDE NEGENINCLS                                                     
       ++INCLUDE NEDATELSTD                                                     
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE NECOMBLOK                                                      
       ++INCLUDE NEGENCOM                                                       
         EJECT                                                                  
       ++INCLUDE SPLTBLKD                                                       
         EJECT                                                                  
*              BASE SCREEN DSECT                                                
         PRINT ON                                                               
         SPACE 3                                                                
       ++INCLUDE NEWRIFFD                                                       
         EJECT                                                                  
*              OVERLAY SCREEN                                                   
         SPACE 3                                                                
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRIE0D                                                       
FILTSV   DS    CL4                 SAVE WR FILTER                               
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDTWADCOND                                                     
       ++INCLUDE DDFAXINFOD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDCNTRL                                                        
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NEGENPACK                                                      
       ++INCLUDE SPTRCMML                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENCLRST                                                     
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPGENBILL                                                      
       ++INCLUDE SPBVALD                                                        
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE BHBLOCKD                                                       
       ++INCLUDE NEGBLOCKD                                                      
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002NEWRI33T  05/01/02'                                      
         END                                                                    
