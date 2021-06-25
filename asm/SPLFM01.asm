*          DATA SET SPLFM01    AT LEVEL 034 AS OF 11/20/12                      
*PHASE T21901A                                                                  
*INCLUDE MEDGET                                                                 
*INCLUDE TIMVAL                                                                 
         TITLE 'T21901 - SPOTPAK LFM - KEY VALIDATION'                          
T21901   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21901,RR=R9                                                   
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     R3,4(R1)                                                         
         USING T219FFD,R3                                                       
         LA    RA,T21901+4095                                                   
         LA    RA,1(RA)                                                         
         USING T21901+4096,RA     ***** SECOND BASE REG=RA                      
*                                                                               
         LA    R7,4095(RA)                                                      
         LA    R7,1(R7)                                                         
         USING T21901+8192,R7    ***** THIRD BASE REG=R7                        
         MVI   HALF2,C'N'           INITIALIZE YEAR SWITCH                      
*                                                                               
         XC    FLEN,FLEN                                                        
         LA    R2,LFMKEYH                                                       
         LA    R4,8(R2)                                                         
         ST    R4,FADDR                                                         
         XC    SVKEYDTA,SVKEYDTA                                                
*                                                                               
         SR    R8,R8                                                            
         LA    R9,KEYTABX-KEYTAB                                                
         D     R8,=F'4'             R9 GETS BCT LIMIT                           
         SPACE                                                                  
         LA    R8,KEYTAB                                                        
KTB1     CLC   SVREC,0(R8)                                                      
         BE    KTB2                                                             
         LA    R8,4(R8)                                                         
         BCT   R9,KTB1                                                          
         DC    H'0'                                                             
         SPACE                                                                  
KTB2     L     RE,0(R8)                                                         
         A     RE,RELO                                                          
         BR    RE                                                               
*                                                                               
EXXMOD   XMOD1 1                                                                
         EJECT                                                                  
CLT      MVI   ERRCD,1                                                          
         BAS   R9,GETMED                                                        
         MVC   LFMKEXP(10),WORK+1                                               
         FOUT  LFMKEXPH                                                         
         MVC   SVKEY+1(1),SVAGYMD                                               
*                                                                               
         MVI   ERRCD,2                                                          
         BAS   R9,GETCLT                                                        
         MVC   SVKEY+2(2),SVCLT                                                 
*                                                                               
         BAS   R9,CHKADV                                                        
*                                                                               
         CLI   SVSCRN,X'F1'        TEST CLTHDR SCREEN                           
         BNE   CLT2                                                             
*                                                                               
         CLC   SVCTAGY,=C'  '      DOING CODE COORDINATION ?                    
         BNH   *+8                 NO                                           
         BAS   RE,CHKCTCLT                                                      
*                                                                               
         TM    SVAGYFL1,X'40'      IF SPECIAL MEDIA NAMES                       
         BNO   CLT2                                                             
         NI    CLTMDTLH+1,X'F4'    TURN TO NORMAL INTENSITY                     
         OI    CLTMDTLH+6,X'80'                                                 
         NI    CLTMDNMH+1,X'DF'    UNPROTECT NAME FIELD                         
         OI    CLTMDNMH+6,X'80'                                                 
*                                                                               
CLT2     CLI   SVREC,X'11'         TEST CLTHDR                                  
         BE    CHKSPT                                                           
         CLI   SVREC,X'31'         TEST CLT2                                    
         BNE   CLT3                                                             
         CLI   SVACT,C'A'          NOT VALID FOR ACTION ADD                     
         BNE   CHKSPT                                                           
         LA    R2,LFMACTH                                                       
         B     ERRINV                                                           
*                                                                               
CLT3     BAS   R9,RDCLT                                                         
         USING CLTHDRD,R8                                                       
         MVC   LFMKEXP+11(20),CNAME                                             
         MVC   SVCLPROF,CPROF                                                   
         MVC   SVCLEX,CEXTRA                                                    
         MVC   SVCLDLY,CDAILY                                                   
         MVC   SVCLOP1,COPT1                                                    
         MVC   SVCLOP2,COPT2                                                    
         MVC   SVCLTPW,CPWPCT                                                   
         MVC   SVCLTPOL,CPOLONLY                                                
         MVC   SVCCOST2,CCOST2                                                  
         DROP  R8                                                               
*                                                                               
         CLI   SVREC,X'54'         TEST CGRP LIST                               
         BE    CHKSPT              VALIDATE CLIENT CODE ONLY                    
*                                                                               
         CLI   SVREC,X'36'         TEST TALENT FACTOR REC                       
         BE    TAL                                                              
*                                                                               
         MVI   ERRCD,3                                                          
         BAS   R9,GETPRD                                                        
         CLI   SVREC,X'12'         PRDHDR                                       
         BNE   CLT5                                                             
         CLI   SVACT,C'A'                                                       
         BNE   CLT5                                                             
         CLC   SVEBCPRD,=C'ZZZ'    DON'T ALLOW ADD OF PRD ZZZ                   
         BNE   *+8                                                              
         B     BADKEY                                                           
*                                                                               
         CLC   SVAALPHA,=C'CK'     CHECK IF COKEAT                              
         BNE   CLT5                                                             
         CLC   SVEBCCLT,=C'CC '    DON'T ALLOW ADD OF PRODUCT TO                
         BE    CLT5                CLIENTS OTHER THAN CC                        
         B     PRDESTCC                                                         
*                                                                               
CLT5     MVC   SVKEY+4(3),SVEBCPRD                                              
         CLI   SVSCRN,X'F2'        TEST PRDHDR SCREEN                           
         BNE   CLT5A                                                            
* PRODUCT SCREEN NEEDS USER FIELDS                                              
         MVC   ELEM(L'SVP1USER),SVP1USER      DISPLAY 1ST DESC LINE             
         LA    R1,PRDDSC1H                                                      
         LA    R6,PRDUSR1H                                                      
         BAS   R9,FMTUSR                                                        
*                                                                               
         MVC   ELEM(L'SVP2USER),SVP2USER      DISPLAY 2ND DESC LINE             
         LA    R1,PRDDSC2H                                                      
         LA    R6,PRDUSR2H                                                      
         BAS   R9,FMTUSR                                                        
*                                                                               
CLT5A    CLI   SVREC,X'12'         TEST PRDHDR                                  
         BE    CHKSPT                                                           
*                                                                               
         CLC   SVAALPHA,=C'CK'     CHECK IF COKEAT                              
         BNE   CLT5AA                                                           
         CLC   SVEBCCLT,=C'CC '    DON'T ALLOW ADD OF PRODUCT TO                
         BE    CLT5AA              CLIENTS OTHER THAN CC                        
         CLI   SVREC,X'13'         ESTHDR                                       
         BNE   CLT5AA                                                           
         CLI   SVACT,C'A'                                                       
         BNE   CLT5AA                                                           
         B     PRDESTCC                                                         
*                                                                               
CLT5AA   BAS   R9,RDPRD                                                         
         USING PRDHDRD,R8                                                       
         MVC   LFMKEXP+32(20),PNAME                                             
         MVC   SVPRD,PCODE+1                                                    
         DROP  R8                                                               
*                                                                               
         MVI   ERRCD,NEWERR                                                     
         MVC   NERRCD,=AL2(NOTRDCPY)                                            
         CLC   SVOVLY(2),=X'1CFC'  TEST EST COPY                                
         BNE   *+12                                                             
         CLI   SVEBCPRD+2,C'#'     TEST TRADE PRODUCT                           
         BE    KEYERR                                                           
*                                                                               
         MVI   ERRCD,4                                                          
         BAS   R9,GETNUM                                                        
         STC   R0,SVKEY+7                                                       
         STC   R0,SVEST                                                         
*                                                                               
         CP    DUB,=P'255'         EST CANNOT EXCEED 255                        
         BH    BADKEY                                                           
*                                                                               
         CLI   SVSCRN,X'F3'        TEST ESTIMATE SCREEN                         
         BNE   CLT5B                                                            
*                                                                               
         MVC   ELEM(L'SVE1USER),SVE1USER      DISPLAY 1ST DESC LINE             
         LA    R1,ESTDSC1H                                                      
         LA    R6,ESTUSR1H                                                      
         BAS   R9,FMTUSR                                                        
*                                                                               
         MVC   ELEM(L'SVE2USER),SVE2USER      DISPLAY 2ND DESC LINE             
         LA    R1,ESTDSC2H                                                      
         LA    R6,ESTUSR2H                                                      
         BAS   R9,FMTUSR                                                        
*                                                                               
CLT5B    CLI   SVREC,X'53'         TEST ESTDOLS                                 
         BNE   CLT6                                                             
         CLI   SVACT,C'A'                                                       
         BNE   CHKSPT                                                           
         LA    R2,LFMACTH                                                       
         MVI   ERRCD,INVERR                                                     
         B     KEYERR                                                           
*                                                                               
CLT6     CLI   SVREC,X'48'         TEST EST INTERFACE REC                       
         BNE   CLT7                NO                                           
         CLC   LFMREC(2),=C'GF'    TEST GFEST REC                               
         BE    CLTGF                                                            
         CLC   LFMREC(2),=C'PG'    TEST PGEST REC                               
         BE    CLT8                YES - ALLOW EST=0                            
*                                                                               
CLT7     CP    DUB,=P'0'                                                        
         BNH   BADKEY                                                           
         B     CLT8                                                             
*                                                                               
CLTGF    CLI   SVPRD,X'FF'         IF PRODUCT=POL, EST <> 0                     
         BNE   CLTGF2                ELSE EST=0                                 
         CLI   SVEST,0                                                          
         BE    BADKEY                                                           
         B     CLT8                                                             
*                                                                               
CLTGF2   CLI   SVEST,0                                                          
         BNE   BADKEY                                                           
*                                                                               
CLT8     CLI   SVREC,X'1F'                                                      
         BNE   SDRX                                                             
* STATION DESCRIPTOR COMMENTS                                                   
         MVI   ERRCD,5                                                          
         BAS   R9,GETSTA                                                        
*                                                                               
         XC    SVKEY,SVKEY                                                      
         MVC   SVKEY(2),=X'0D0C'     SET COMMENT REC KEY                        
         MVC   SVKEY+2(1),SVAGYMD                                               
         MVI   SVKEY+3,C'S'        SET SUB-TYPE                                 
         MVC   SVKEY+4(2),SVCLT                                                 
         MVC   SVKEY+8(1),SVPRD                                                 
         MVC   SVKEY+9(1),SVEST                                                 
         GOTO1 VMSPACK,DMCB,=C'0000',SVSTA,DUB                                  
         MVC   SVKEY+10(3),DUB+2     MOVE STATION ONLY                          
         B     CHKSPT                                                           
         EJECT                                                                  
*        PUT OUT USER DESCRIPTION FIELDS                                        
*             R1   = A(DESC FIELD).                                             
*             R6   = A(INPUT FIELD).                                            
*             DESC = PRODUCT DESCRIPTION.                                       
*                                                                               
FMTUSR   DS    0H                                                               
         OI    1(R6),X'20'              PROTECT INPUT FIELD                     
         MVC   8(L'SVP1USER,R1),ELEM    CLEAR ANY PREVIOUS DESC FIELDS          
         CLC   SPACES(L'SVP1USER),ELEM  NEED TO SHOW DESCRIPTION                
         BL    FMTUSR10                                                         
         LR    R0,R1                    SAVE C(R1) AROUND.                      
         ZIC   R1,0(R6)                 R1=L(HEADER)+L(INPUT FIELD).            
         SH    R1,=H'8'                 R1=L(INPUT FIELD).                      
         TM    1(R6),X'02'              CHECK FOR EXTENDED HEADER.              
         BZ    *+8                                                              
         SH    R1,=H'8'                 SUBTRACT L(X-HEADER).                   
         BCTR  R1,0                                                             
         EX    R1,*+10                                                          
         LR    R1,R0                                                            
         B     FMTX                                                             
         XC    8(0,R6),8(R6)            CLEAR ANY GARBAGE.                      
FMTUSR10 NI    1(R6),X'FF'-X'20'        UNPROTECT INPUT FIELD                   
*                                                                               
FMTX     OI    6(R1),X'80'              TRANSMIT FIELD                          
         OI    6(R6),X'80'                                                      
         BR    R9                                                               
         EJECT                                                                  
SDRX     DS    0H                                                               
         CLI   SVREC,X'46'                                                      
         BE    PXC                                                              
         CLI   SVREC,X'47'                                                      
         BNE   SPLIT                                                            
*                                                                               
PXC      MVC   KEY(13),SVKEY                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    PXC1                                                             
         MVI   ERRCD,NOESTERR                                                   
         B     KEYERR                                                           
         SPACE                                                                  
PXC1     DS    0H                                                               
         CLI   SVREC,X'47'                                                      
         BE    SHR                                                              
         MVI   ERRCD,5             * * PRODUCT EXCLUSION RECS * *               
         BAS   R9,GETSTA                                                        
         CLC   SVSTA,SPACES        TEST 'ALL'                                   
         BE    PXC14                                                            
*                                                                               
         LA    R9,REC                                                           
         ST    R9,AREC                                                          
         MVC   KEY+9(3),SVEBCCLT                                                
PXC10    MVI   KEY,C'S'                                                         
         MVC   KEY+1(1),SVEBCMED                                                
         MVC   KEY+2(5),SVSTA                                                   
         MVC   KEY+7(2),AGYALPHA                                                
         MVC   COMMAND,=C'DMRDHI'                                               
         GOTO1 STA                                                              
         CLC   REC(15),KEY                                                      
         BE    PXC15                                                            
         CLC   KEY+9(3),=C'000'       SEE IF I WAS TRYING FOR DEFAULT           
         BE    PXC13               YES - SEND NOT FOUND MSG                     
         MVC   KEY(17),ZEROS       TRY FOR DEFAULT STATION                      
         B     PXC10                                                            
*                                                                               
PXC13    MVI   ERRCD,NOSTAERR                                                   
         B     KEYERR                                                           
*                                                                               
PXC14    XC    SVKEY,SVKEY             IF STA = ALL                             
         MVC   SVKEY+5(3),=3X'FF'                                               
         B     PXC20                                                            
*                                                                               
PXC15    XC    SVKEY,SVKEY                                                      
         MVC   DUB(5),SVSTA                                                     
         CLI   DUB+4,C'C'          CHANGE COMBINED TO T                         
         BNE   *+8                                                              
         MVI   DUB+4,C'T'          BECAUSE MSPACK CAN'T HANDLE                  
         GOTO1 VMSPACK,DMCB,=C'0000',DUB,SVKEY+3                                
*                                                                               
PXC20    MVC   SVKEY(2),=X'0D70'                                                
         MVC   SVKEY+2(1),SVAGYMD                                               
         MVC   SVKEY+3(2),SVCLT                                                 
         MVC   SVKEY+8(3),SVEBCPRD                                              
         MVC   SVKEY+11(1),SVEST                                                
         B     CHKSPT                                                           
         SPACE                                                                  
SHR      DS    0H                  * * SHARE RECS * *                           
         MVC   SVKEY(2),=X'0D71'                                                
         MVC   SVKEY+2(1),SVAGYMD                                               
         MVC   SVKEY+3(2),SVCLT                                                 
         MVC   SVKEY+8(3),SVEBCPRD                                              
         MVC   SVKEY+11(1),SVEST                                                
         B     CHKSPT                                                           
         EJECT                                                                  
SPLIT    DS    0H                                                               
         CLI   SVREC,X'48'         PGEST REC                                    
         BE    PGEST00                                                          
         CLI   SVREC,X'28'         DPT MENU SPLIT REC                           
         BNE   EST                                                              
         MVC   KEY(13),SVKEY                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    SPLIT4                                                           
         MVI   ERRCD,NOESTERR                                                   
         B     KEYERR                                                           
*                                                                               
SPLIT4   L     R8,AREC                                                          
         USING ESTHDRD,R8                                                       
         GOTO1 GETREC                                                           
         MVC   SVAEDATS(1),EDAYMENU    SAVE DPT MENU IN ADV EST DATES           
         MVC   WORK(13),SVKEY                                                   
         MVC   SVKEY(2),=X'0D40'                                                
         MVC   SVKEY+2(11),WORK+1                                               
         B     CHKSPT                                                           
         SPACE 2                                                                
TAL      CLI   LFMKEYH+5,6                                                      
         BNL   *+12                                                             
         MVI   ERRCD,3                                                          
         B     BADKEY                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D27'                                                  
         MVC   KEY+2(3),SVKEY+1    BUILD PARTIAL KEY A-M/C                      
*                                                                               
         BAS   R9,GETNUM                                                        
         MVI   ERRCD,3                                                          
         CLI   0(R4),C'4'          VALID TALENT FACTOR GROUP CODES              
         BH    BADKEY              ARE BETWEEN ZERO AND FOUR                    
         CLI   0(R4),C'0'                                                       
         BL    BADKEY                                                           
         MVN   KEY+11(1),0(R4)                                                  
*                                                                               
         CLI   SVEBCMED,C'N'       TEST MEDIA N                                 
         BNE   CHKSPTX                                                          
*                                                                               
* REDO KEY FOR NETWORK                                                          
*                                                                               
K        USING TALKEY,KEY                                                       
*                                                                               
         MVC   SVSTA,SPACES                                                     
* GET NETWORK CALL LETTERS                                                      
         BAS   RE,FLDVAL                                                        
         MVI   ERRCD,4                                                          
         CLI   FLEN+1,3                                                         
         BL    BADKEY                                                           
         CLI   FLEN+1,4                                                         
         BH    BADKEY                                                           
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   SVSTA(0),0(R4)                                                   
         MVI   SVSTA+4,C'N'                                                     
*                                                                               
         MVC   K.NTALKNET,SVSTA                                                 
         MVC   SVKEY,KEY           SAVE KEY NOW SO CAN USE IT                   
         DROP  K                                                                
                                                                                
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(14),KEY                                                    
*                                                                               
         MVI   KEY,C'S'                                                         
         MVC   KEY+1(1),SVEBCMED                                                
         MVC   KEY+2(5),SVSTA                                                   
         MVC   KEY+7(2),AGYALPHA                                                
         MVC   COMMAND,=C'DMRDHI'                                               
         LA    RE,REC                                                           
         ST    RE,AREC                                                          
         GOTO1 STA                                                              
         CLC   REC(15),KEY                                                      
         BNE   BADKEY                                                           
         B     CHKSPTX2                                                         
         SPACE                                                                  
RATS     DS    0H               ** ESTIMATED BOOK DEMO RECS **                  
         XC    SVKEY,SVKEY                                                      
         MVI   ERRCD,1                                                          
         BAS   R9,GETMED                                                        
         MVC   LFMKEXP(10),WORK+1                                               
         FOUT  LFMKEXPH                                                         
         MVC   SVKEY+2(1),SVAGYMD                                               
         SPACE                                                                  
         BAS   RE,FLDVAL                                                        
         CLI   FLEN+1,3                                                         
         BNE   BADKEY                                                           
         MVI   DUB,X'50'                                                        
         CLC   0(3,R4),=C'ARB'                                                  
         BE    RATSX                                                            
         MVI   DUB,X'51'                                                        
         CLC   0(3,R4),=C'NSI'                                                  
         BE    RATSX                                                            
         B     BADKEY                                                           
*                                                                               
RATSX    MVI   SVKEY,X'0D'                                                      
         MVC   SVKEY+1(1),DUB                                                   
         B     CHKSPT                                                           
         EJECT                                                                  
* ESTHDR MUST EXIST BEFORE BUILDING PGEST REC                                   
*                                                                               
PGEST00  DS    0H                                                               
         MVC   KEY(13),SVKEY                                                    
         MVI   KEY+7,0                                                          
         GOTO1 HIGH                                                             
*                                                                               
         CLI   SVKEY+7,0                                                        
         BE    PGEST10                                                          
PGEST05  CLC   KEY(13),KEYSAVE                                                  
         BE    PGEST10             REBUILD SVKEY OF THE PGEST REC               
         MVI   ERRCD,NOESTERR                                                   
         B     KEYERR                                                           
*                                                                               
PGEST10  DS    0H                                                               
         MVC   KEY(13),SVKEY                                                    
         LA    R4,SVKEY                                                         
         USING PGESTD,R4                                                        
*                                                                               
         XC    0(PGKLENQ,R4),0(R4)                                              
         MVI   PGKRID,PGKNDIRQ                                                  
         MVI   PGKSID,PGKNDISQ                                                  
         MVC   PGKAM(7),KEY+1                                                   
         B     CHKSPT                                                           
         DROP  R4                                                               
*                                                                               
* BRAND POOL CLIENTS MUST HAVE POL ESTHDR OPEN BEFORE BRANDS                    
*                                                                               
EST      CLI   SVACT,C'A'          TEST ADD                                     
         BNE   EST2                                                             
         CLI   SVCLTPOL,C'Y'       TEST ONLY POL BUYING                         
         BE    EST1                                                             
         CLI   SVCLPROF+0,C'0'     TEST BRAND POOL CLT                          
         BE    EST2                                                             
*                                                                               
EST1     CLI   SVPRD,X'FF'         TEST THIS IS POL HDR                         
         BE    EST2                                                             
         MVC   KEY(13),SVKEY       POL ESTHDR MUST BE OPEN                      
         MVC   KEY+4(3),=C'POL'                                                 
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BE    EST2                                                             
         MVI   ERRCD,NOPOLEST                                                   
         B     KEYERR                                                           
*                                                                               
* DO NOT ALLOW BRAND EST ADD IF POL IS MASTER OR SUB                            
*                                                                               
EST2     CLI   SVACT,C'A'                                                       
         BNE   EST4                                                             
         CLI   SVPRD,X'FF'                                                      
         BE    EST4                                                             
         MVC   KEY(13),SVKEY                                                    
         MVC   KEY+4(3),=C'POL'                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    EST2A                                                            
         CLI   SVEBCMED,C'N'       TEST NETWORK                                 
         BE    *+12                                                             
         CLI   SVCLTPOL,C'Y'                                                    
         BNE   EST4                                                             
         MVI   ERRCD,NOPOLEST                                                   
         B     KEYERR                                                           
EST2A    GOTO1 GETREC                                                           
         L     R8,AREC                                                          
         USING ESTHDRD,R8                                                       
         MVI   ERRCD,MSTREST                                                    
         CLI   SVREC,X'13'         CHK REGULAR EST                              
         BNE   EST3                                                             
*                                                                               
         CLI   EMSTRIND,0                                                       
         BNE   KEYERR                                                           
         B     EST6                                                             
*                                                                               
EST3     CLI   SVREC,X'1B'                                                      
         BNE   EST4                                                             
         CLI   EMSTRIND,0          POL MUST BE A MASTER OR SLAVE                
         BE    KEYERR                                                           
         B     EST6                                                             
*                                                                               
*                                                                               
EST4     CLI   SVREC,X'1A'         TEST SUB-EST                                 
         BNE   EST6                                                             
         CLI   SVPRD,X'FF'         PRD MUST BE POL                              
         BE    ESTX                                                             
         MVI   ERRCD,3                                                          
         B     BADKEY                                                           
EST6     DS    0H                                                               
         XC    SVAEDATS,SVAEDATS                                                
         OC    SVADVDA,SVADVDA     SEE IF DOING ADV                             
         BZ    EST10                                                            
         SR    R0,R0                                                            
         IC    R0,SVKEY+7          SEE IF DOING DECADE EST                      
         SRDA  R0,32                                                            
         D     R0,=F'10'                                                        
         LTR   R0,R0                                                            
         BNZ   ESTX                MUST BE DIVISIBLE BY 10                      
         XC    KEY,KEY                                                          
         MVC   KEY(13),SVKEY                                                    
         NI    KEY+1,X'0F'                                                      
         OC    KEY+1(1),SVADVAGY                                                
         CLC   SVKEY(13),KEY       SEE IF DOING ADV                             
         BE    ESTX                YES                                          
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    EST6B                                                            
         MVI   ERRCD,NOADVEST      ADV EST NOT ON FILE                          
         B     KEYERR                                                           
*                                                                               
EST6B    L     R8,AREC                                                          
         GOTO1 GETREC                                                           
         MVC   SVAEDATS,ESTART     SAVE ADV EST DATES                           
         B     ESTX                                                             
*                                                                               
EST10    OC    SVCLTPW,SVCLTPW     TEST PW CLIENT                               
         BZ    ESTX                NO                                           
* POL EST MUST BE OPEN FIRST FOR PW CLIENTS                                     
         CLI   SVPRD,X'FF'         TEST THIS IS POL (THANKS SAM)                
         BE    ESTX                                                             
         XC    KEY,KEY                                                          
         MVC   KEY(8),SVKEY                                                     
         MVC   KEY+4(3),=C'POL'                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    EST12                                                            
         MVI   ERRCD,NOPOLEST                                                   
         B     KEYERR                                                           
EST12    DS    0H                                                               
         GOTO1 GETREC                                                           
         L     R8,AREC                                                          
         USING ESTHDRD,R8                                                       
         MVC   SVPOLPW,EPWPCT                                                   
*                                                                               
ESTX     B     CHKSPT                                                           
         EJECT                                                                  
COM      MVC   SVKEY(2),=X'0D0C'                                                
         MVC   SVKEY+3(1),LFMREC   SET COM TYPE                                 
         CLC   =C'A3',LFMREC       TEST A3 COMMENT                              
         BNE   *+8                                                              
         MVI   SVKEY+3,C'3'        YES - COMMENT TYPE IS 3                      
*                                                                               
         MVI   ERRCD,1                                                          
         BAS   R9,GETMED                                                        
         MVC   SVKEY+2(1),SVAGYMD                                               
         MVC   LFMKEXP(10),WORK+1                                               
         FOUT  LFMKEXPH                                                         
         L     R4,FADDR                                                         
         AH    R4,FLEN                                                          
         CLI   0(R4),C','                                                       
         BNE   BADKEY                                                           
*                                                                               
         MVI   ERRCD,2                                                          
* CHECK FOR OFFICE CODE                                                         
         BAS   RE,FLDVAL                                                        
         CLI   FLEN+1,2                                                         
         BNE   COM4                                                             
         CLI   0(R4),C'*'                                                       
         BNE   COM4                                                             
         CLI   1(R4),C'A'                                                       
         BL    BADKEY                                                           
         CLI   1(R4),C'Z'                                                       
         BNH   COM2                                                             
         CLI   1(R4),C'0'                                                       
         BL    BADKEY                                                           
         CLI   1(R4),C'9'                                                       
         BH    BADKEY                                                           
*                                                                               
COM2     MVC   SVCLT,0(R4)         OFFICE CODE IS VALID                         
         MVC   SVKEY+4(2),SVCLT                                                 
         MVC   SVEBCCLT(2),SVCLT                                                
         MVI   SVEBCCLT+2,C' '                                                  
         MVC   LFMKEXP+11(14),=C'** OFFICE X **'                                
         MVC   LFMKEXP+21(1),SVCLT+1                                            
*                                                                               
COM3     ZIC   R0,LFMKEYH+5        TOTAL FIELD LENGTH                           
         SH    R0,=H'2'            FOR MEDIA AND ,                              
         CH    R0,FLEN                                                          
         BE    COMX                                                             
         MVI   ERRCD,3                                                          
         B     BADKEY              NO MORE FIELDS ALLOWED                       
*                                                                               
*                                                                               
COM4     CLI   FLEN+1,3                                                         
         BH    BADKEY                                                           
         CLC   =C'ALL',0(R4)                                                    
         BNE   COM5                                                             
         B     COM3                NO MORE FLDS ALLOWED                         
*                                                                               
COM5     XC    FLEN,FLEN           SET TO RE-EDIT                               
         BAS   R9,GETCLT                                                        
         MVC   SVKEY+4(2),SVCLT                                                 
*                                                                               
         BAS   R9,RDCLT                                                         
         USING CLTHDRD,R8                                                       
         MVC   LFMKEXP+11(20),CNAME                                             
         DROP  R8                                                               
*                                                                               
         L     R4,FADDR                                                         
         AH    R4,FLEN                                                          
         CLI   0(R4),C','                                                       
         BNE   COMX                                                             
*                                                                               
         CLI   LFMREC,C'N'         CHECK NV LETTER TEXT                         
         BE    COMX                                                             
*                                                                               
         MVI   ERRCD,3                                                          
         BAS   RE,FLDVAL                                                        
         CLI   FLEN+1,3                                                         
         BNE   *+14                                                             
         CLC   =C'ALL',0(R4)                                                    
         BE    COM10                                                            
*                                                                               
         CLC   =C'PGR=',0(R4)                                                   
         BNE   COM7                                                             
         CLI   FLEN+1,5                                                         
         BL    BADKEY                                                           
         LA    R4,4(R4)            BUMP PAST PGR=                               
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         MVI   HALF,C'P'                                                        
         BAS   R9,GETGRP                                                        
         MVC   SVKEY+6(3),FULL                                                  
         CLI   HALF+1,0                                                         
         BE    COM6C                                                            
*                                                                               
         OC    FULL+1(2),FULL+1    TEST GROUP NUM ENTERED                       
         BZ    BADKEY                                                           
* NOW CHECK THAT NUMBER OF INPUT DIGITS = BREAK 1 DIGITS                        
         MVC   WORK(20),SVKEY                                                   
         MVC   SVKEY+3(3),WORK+4       ALTER SVKEY FOR RDPGRDEF                 
*                                                                               
         BAS   RE,RDPGRDEF                                                      
         CLC   KEY(13),KEYSAVE     SEE IF FOUND                                 
        BNE   BADKEY               NOT FOUND                                    
         MVC   SVKEY(20),WORK          RESTORE SVKEY                            
         CLC   SVBKLNS+0(1),HALF+1                                              
         BNE   BADKEY                                                           
* NOW READ PRDGRP REC TO EXTRACT NAME                                           
COM6C    XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D01'                                                  
         MVC   KEY+2(1),SVKEY+2           A/M                                   
         MVC   KEY+3(5),SVKEY+4        CLT/PGRP                                 
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(6),KEYSAVE      TY/A-M/CLT/PGRPID                            
         BNE   BADKEY                                                           
         CLI   HALF+1,0            TEST ADDING DEFAULT                          
         BNE   *+14                NO                                           
         MVC   LFMKEXP+32(13),=C'** DEFAULT **'                                 
         B     COM6E                                                            
         IC    RE,SVBKLNS+0        GET PRDGRP BREAK 1 DIGITS                    
         BCTR  RE,0                                                             
         UNPK  DUB,KEY+6(3)        UNPACK RESULT OF HI                          
         UNPK  WORK(8),SVKEY+7(3)  UNPACK OUR DATA                              
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   DUB+3(0),WORK+3  **EXECUTED**                                    
         BNE   BADKEY              SHOULD AGREE FOR BREAK 1 DIGITS              
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R8,AREC                                                          
         USING PRGRECD,R8                                                       
         LA    R6,PRGEL                                                         
         USING PRGEL10,R6                                                       
         MVC   LFMKEXP+32(20),PRGNAM1                                           
         DROP  R6,R8                                                            
COM6E    DS    0H                                                               
         L     R4,FADDR                                                         
         AH    R4,FLEN                                                          
         CLI   0(R4),0                                                          
         BE    COMX                                                             
         B     COM10               ELSE CHECK FOR EST                           
*                                                                               
COM7     DS    0H                                                               
         XC    FLEN,FLEN           SET TO RE-EDIT                               
         BAS   R9,GETPRD                                                        
*                                                                               
         BAS   R9,RDPRD                                                         
         USING PRDHDRD,R8                                                       
         MVC   LFMKEXP+32(20),PNAME                                             
         MVC   SVKEY+8(1),PCODE+1                                               
         DROP  R8                                                               
*                                                                               
COM10    L     R4,FADDR                                                         
         AH    R4,FLEN                                                          
         CLI   0(R4),C','                                                       
         BNE   COMX                                                             
* EDIT ESTIMATE NUMBER                                                          
         MVI   ERRCD,4                                                          
         BAS   RE,FLDVAL                                                        
         CLI   FLEN+1,3                                                         
         BNE   *+14                                                             
         CLC   =C'ALL',0(R4)                                                    
         BE    COM20                                                            
*                                                                               
         XC    FLEN,FLEN                                                        
         BAS   R9,GETNUM                                                        
         CP    DUB,=P'0'                                                        
         BNH   BADKEY                                                           
         CP    DUB,=P'255'                                                      
         BH    BADKEY                                                           
         STC   R0,SVKEY+9                                                       
         EJECT                                                                  
COM20    L     R4,FADDR                                                         
         AH    R4,FLEN                                                          
         CLI   0(R4),C','                                                       
         BNE   COMX                                                             
         CLI   LFMREC,C'B'         MARKET ALLOWED FOR                           
         BE    COM21               BILLING COMMENT                              
         CLI   LFMREC,C'M'         AND REQUIRED FOR                             
         BNE   COMX                MEDIA COMMENT                                
*                                                                               
* EDIT MARKET                                                                   
COM21    DS    0H                                                               
         MVI   ERRCD,5                                                          
         BAS   R9,GETNUM                                                        
         CP    DUB,=P'0'                                                        
         BL    BADKEY                                                           
         STH   R0,HALF                                                          
         MVC   SVKEY+11(2),HALF                                                 
         LTR   R0,R0                                                            
         BZ    COMX                                                             
*                                                                               
         MVC   KEY(17),ZEROS                                                    
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),SVEBCMED                                                
         UNPK  KEY+2(4),DUB                                                     
         MVC   KEY+6(2),AGYALPHA                                                
         MVC   COMMAND,=C'DMRDHI'                                               
         LA    RE,REC                                                           
         ST    RE,AREC                                                          
         GOTO1 STA                                                              
         CLC   REC(15),KEY                                                      
         BNE   BADKEY                                                           
*                                                                               
         XC    LFMKEXP,LFMKEXP                                                  
         MVC   LFMKEXP(20),REC+18  MOVE MARKET NAME                             
         B     COMX                                                             
         SPACE 2                                                                
COMX     CLI   LFMREC,C'M'         FOR MEDIA COMMENT,                           
         BNE   CHKSPT                                                           
         MVI   ERRCD,2                                                          
         OC    SVKEY+4(2),SVKEY+4  TEST FOR CLIENT                              
         BZ    BADKEY                                                           
         CLI   SVCLT,C'*'          TEST NOT CLIENT OFFICE                       
         BE    BADKEY                                                           
         MVI   ERRCD,3                                                          
         CLI   SVKEY+8,0           TEST FOR PRODUCT                             
         BE    BADKEY                                                           
         CLI   SVKEY+6,0           TEST NOT PRODUCT GROUP                       
         BNE   BADKEY                                                           
         CLI   SVKEY+8,X'FF'       TREAT 'POL' AS 'ALL'                         
         BNE   *+8                                                              
         MVI   SVKEY+8,0                                                        
         MVI   ERRCD,4                                                          
         CLI   SVKEY+9,0           TEST FOR ESTIMATE                            
         BE    BADKEY                                                           
         MVI   ERRCD,5                                                          
******** OC    SVKEY+11(2),SVKEY+11     TEST FOR MARKET                         
******** BZ    BADKEY              ***** MARKET 0 IS OK NOW ******              
         B     CHKSPT                                                           
         EJECT                                                                  
PGRP     MVC   SVKEY(2),=X'0D01'                                                
         MVI   ERRCD,1                                                          
         BAS   R9,GETMED                                                        
         MVC   LFMKEXP(10),WORK+1                                               
         FOUT  LFMKEXPH                                                         
         MVC   SVKEY+2(1),SVAGYMD                                               
*                                                                               
         MVI   ERRCD,2                                                          
         BAS   R9,GETCLT                                                        
         MVC   SVKEY+3(2),SVCLT                                                 
         BAS   R9,RDCLT                                                         
         USING CLTHDRD,R8                                                       
         MVC   LFMKEXP+11(20),CNAME                                             
         DROP  R8                                                               
*                                                                               
         MVI   ERRCD,3                                                          
         MVI   HALF,C'P'           INDICATE PRDGRP                              
         BAS   R9,GETGRP                                                        
         MVC   SVKEY+5(3),FULL                                                  
*                                                                               
         CLI   FULL,C'V'           ID MUST BE V-Z                               
         BL    BADKEY                                                           
         CLI   FULL,C'Z'                                                        
         BH    BADKEY                                                           
         CLI   SVREC,X'14'         TEST PGRDEF                                  
         BNE   PGRP10                                                           
* PGRDEF MUST SPECIFY ID/OMIT NUMBER                                            
         CLI   HALF+1,0            TEST NO DIGITS ENTERED                       
         BNE   BADKEY                                                           
         B     PGRPX                                                            
* PGROUP MUST SPECIFY ID AND NUMBER                                             
PGRP10    OC    FULL+1(2),FULL+1                                                
         BZ    BADKEY                                                           
         BAS   RE,RDPGRDEF         TEST RIGHT NUMBER OF DIGITS INPUT            
         BNE   BADKEY                                                           
*                                                                               
PGRPX    DS    0H                                                               
         B     CHKSPT                                                           
         EJECT                                                                  
MGRP     MVC   SVKEY(2),=X'0D02'                                                
         CLI   SVREC,X'18'         TEST CSO PMGR RECORD                         
         BNE   *+10                                                             
         MVC   SVKEY(2),=X'0D07'                                                
         MVI   ERRCD,1                                                          
         BAS   R9,GETMED                                                        
         MVC   SVKEY+2(1),SVAGYMD                                               
         MVC   LFMKEXP(10),WORK+1                                               
         FOUT  LFMKEXPH                                                         
* EDIT OFFICE,CLIENT, OR 'ALL'                                                  
         MVI   ERRCD,2                                                          
         BAS   RE,FLDVAL                                                        
         CLI   FLEN+1,3                                                         
         BNE   MGRP10                                                           
         CLC   =C'ALL',0(R4)                                                    
         BNE   MGRP10                                                           
* PRDGRP MUST BE 'ALL' IF CLIENT 'ALL'                                          
         MVI   ERRCD,3                                                          
         BAS   RE,FLDVAL                                                        
         CLI   FLEN+1,3                                                         
         BNE   BADKEY                                                           
         CLC   =C'ALL',0(R4)                                                    
         BNE   BADKEY                                                           
         XC    FLEN,FLEN           SET TO RE-EDIT                               
         B     MGRP11                                                           
         EJECT                                                                  
MGRP10   CLI   SVREC,X'16'         TEST MGRDEF                                  
         BE    MGRP10X             YES                                          
         CLI   SVREC,X'18'         TEST CSO PMGR                                
         BE    MGRP10X                                                          
* FOR MGROUP CHECK FOR OFFICE                                                   
         CLI   FLEN+1,2                                                         
         BNE   MGRP10X                                                          
         CLI   0(R4),C'*'                                                       
         BNE   MGRP10X                                                          
         CLI   1(R4),C'A'                                                       
         BL    MGRP10X                                                          
         CLI   1(R4),C'Z'                                                       
         BL    MGRP10A                                                          
         CLI   1(R4),C'0'                                                       
         BL    MGRP10X                                                          
         CLI   1(R4),C'9'                                                       
         BH    MGRP10X                                                          
* VALID OFFICE CODE WAS ENTERED                                                 
MGRP10A  MVC   SVCLT,0(R4)                                                      
         MVC   SVKEY+3(2),SVCLT                                                 
         MVC   SVEBCCLT(2),SVCLT                                                
         MVI   SVEBCCLT+2,C' '                                                  
         MVC   LFMKEXP+11(14),=C'** OFFICE X **'                                
         MVC   LFMKEXP+21(1),SVCLT+1                                            
         B     MGRP20                                                           
*                                                                               
MGRP10X  XC    FLEN,FLEN           SET TO RE-EDIT                               
         BAS   R9,GETCLT                                                        
         MVC   SVKEY+3(2),SVCLT                                                 
         BAS   R9,RDCLT                                                         
         USING CLTHDRD,R8                                                       
         MVC   LFMKEXP+11(20),CNAME                                             
         DROP  R8                                                               
         EJECT                                                                  
MGRP11   CLI   SVREC,X'16'         TEST MGRDEF                                  
         BNE   MGRP20               NO - MUST BE MGROUP                         
* MGRDEF - PRDGRP SHOULD BE ALL                                                 
         MVI   ERRCD,3                                                          
         BAS   RE,FLDVAL                                                        
         CLI   FLEN+1,3                                                         
         BNE   BADKEY                                                           
         CLC   =C'ALL',0(R4)                                                    
         BNE   BADKEY                                                           
* MGRDEF - GET MKTGRP ID                                                        
         MVI   ERRCD,4                                                          
         MVI   HALF,C'M'           INDICATE MKTGRP                              
         BAS   R9,GETGRP                                                        
         MVC   SVKEY+8(3),FULL                                                  
         CLI   SVKEY+8,C'N'        ACCEPT ANY ALPHA                             
         BE    BADKEY              EXCEPT N OR Y                                
         CLI   SVKEY+8,C'Y'                                                     
         BE    BADKEY                                                           
         CLI   SVKEY+8,C'Z'                                                     
         BH    BADKEY                                                           
         CLI   HALF+1,0            TEST ID ONLY ENTERED                         
         BNE   BADKEY                                                           
* FOR ID A-F, CLT SHOULD BE PRESENT. ELSE NOT.                                  
         MVI   ERRCD,2                                                          
         CLI   SVKEY+8,C'F'                                                     
         BH    MGRP12                                                           
         OC    SVKEY+3(2),SVKEY+3                                               
         BZ    BADKEY                                                           
         B     MGRPX                                                            
*                                                                               
MGRP12   OC    SVKEY+3(2),SVKEY+3                                               
         BNZ   BADKEY                                                           
         B     MGRPX                                                            
         EJECT                                                                  
* MGROUP - EDIT PRDGRP OR 'ALL'                                                 
*                                                                               
MGRP20   DS    0H                                                               
         MVI   ERRCD,3                                                          
         CLI   SVREC,X'18'         TEST CSO PMGR                                
         BE    MGRP30                                                           
*                                                                               
         BAS   RE,FLDVAL                                                        
         CLI   FLEN+1,3                                                         
         BNE   MGRP22                                                           
         CLC   =C'ALL',0(R4)                                                    
         BE    MGRP40                                                           
*                                                                               
MGRP22   CLI   SVEBCCLT,C'*'       TEST OFFICE                                  
         BE    BADKEY              YES - PRDGRP MUST BE ALL                     
*                                                                               
         XC    FLEN,FLEN           SET TO RE-EDIT                               
         MVI   HALF,C'P'           INDICATE PRDGRP                              
         BAS   R9,GETGRP                                                        
         MVC   SVKEY+5(3),FULL                                                  
         CLI   HALF+1,0            TEST NO DIGITS ENTERED                       
         BE    MGRP23              NONE - MUST BE DEFAULT REC                   
         OC    FULL+1(2),FULL+1    TEST GROUP NUM ENTERED                       
         BZ    BADKEY                                                           
* NOW CHECK THAT NUMBER OF INPUT DIGITS = BREAK 1 DIGITS                        
         BAS   RE,RDPGRDEF                                                      
         CLC   KEY(13),KEYSAVE                                                  
         BNE   BADKEY              NOT FOUND                                    
         CLC   SVBKLNS+0(1),HALF+1                                              
         BNE   BADKEY                                                           
* NOW READ PRDGRP REC TO EXTRACT NAME                                           
MGRP23   XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D01'                                                  
         MVC   KEY+2(6),SVKEY+2    A-M/CLT/PRDGRP                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(6),KEYSAVE      TY/A-M/CLT/PGRPID                            
         BNE   BADKEY                                                           
         CLI   HALF+1,0            TEST ADDING DEFAULT                          
         BNE   *+14                NO                                           
         MVC   LFMKEXP+31(13),=C'** DEFAULT **'                                 
         B     MGRP40                                                           
         IC    RE,SVBKLNS+0        GET PRDGRP BREAK 1 DIGITS                    
         BCTR  RE,0                                                             
         UNPK  DUB,KEY+6(3)        UNPACK RESULT OF HI                          
         UNPK  WORK(8),SVKEY+6(3)  UNPACK OUR DATA                              
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   DUB+3(0),WORK+3  **EXECUTED**                                    
         BNE   BADKEY              SHOULD AGREE FOR BREAK 1 DIGITS              
*                                                                               
MGRP24   GOTO1 GETREC                                                           
*                                                                               
         L     R8,AREC                                                          
         USING PRGRECD,R8                                                       
         LA    R6,PRGEL                                                         
         USING PRGEL10,R6                                                       
         MVC   LFMKEXP+31(20),PRGNAM1                                           
         B     MGRP40                                                           
         DROP  R6,R8                                                            
         SPACE 2                                                                
* CSO PMGR RECORDS - EDIT PRODUCT CODE                                          
*                                                                               
MGRP30   BAS   R9,GETPRD                                                        
         MVC   SVKEY+5(3),SVEBCPRD                                              
         BAS   R9,RDPRD                                                         
         EJECT                                                                  
* EDIT MKTGRP                                                                   
*                                                                               
MGRP40   MVI   ERRCD,4                                                          
         MVI   HALF,C'M'           INDICATE MKTGRP                              
         BAS   R9,GETGRP                                                        
         MVC   SVKEY+8(3),FULL                                                  
         CLI   SVKEY+8,C'N'        ACCEPT ANY ALPHA                             
         BE    BADKEY              EXCEPT N OR Y                                
         CLI   SVKEY+8,C'Y'                                                     
         BE    BADKEY                                                           
         CLI   SVKEY+8,C'Z'                                                     
         BH    BADKEY                                                           
         OC    FULL+1(2),FULL+1    NUMBER MUST BE PRESENT                       
         BZ    BADKEY                                                           
         BAS   RE,RDMGRDEF         TEST RIGHT NUMBER OF DIGITS INPUT            
         BNE   BADKEY                                                           
* FOR ID A-F, CLT MUST BE PRESENT                                               
         MVI   ERRCD,2                                                          
         CLI   SVKEY+8,C'F'                                                     
         BH    MGRP27                                                           
         OC    SVKEY+3(2),SVKEY+3                                               
         BZ    BADKEY                                                           
         B     MGRP41                                                           
*                                                                               
MGRP27   OC    SVKEY+3(2),SVKEY+3                                               
         BZ    MGRP41                                                           
* FOR ID G-K, IF CLT ENTERED IT MUST BE IN EXCEPTION LIST                       
         L     R8,AREC                                                          
         USING MKGRECD,R8                                                       
         LA    R6,MKGEL                                                         
         MVI   ELCODE,2                                                         
         BAS   RE,NEXTEL                                                        
         BNE   BADKEY                                                           
         CLC   2(3,R6),SVEBCCLT                                                 
         BNE   *-14                                                             
         B     MGRP41                                                           
         EJECT                                                                  
* CHECK PRDGRP PRESENT IF REQUIRED                                              
*                                                                               
MGRP41   CLI   SVREC,X'18'         TEST CSO PMGR                                
         BE    MGRP45                                                           
*                                                                               
         MVI   ERRCD,3                                                          
         L     R8,AREC                                                          
         USING MKGRECD,R8                                                       
         LA    R6,MKGEL                                                         
         USING MKGEL01,R6                                                       
         CLI   MKGPGA,0            TEST MKTGRP BY PRDGRP                        
         BNE   MGRP43              YES                                          
         OC    SVKEY+5(3),SVKEY+5  NO - PRDGRP MUST BE 'ALL'                    
         BNZ   BADKEY                                                           
         B     MGRP45                                                           
*                                                                               
MGRP43   OC    SVKEY+5(3),SVKEY+5                                               
         BZ    BADKEY                                                           
         OC    SVKEY+6(2),SVKEY+6  TEST ADDING TO DEFAULT                       
         BZ    MGRP45                                                           
* CHECK PRDGRP IN EXCEPTION LIST                                                
         MVI   ELCODE,2                                                         
         BAS   RE,NEXTEL                                                        
         BNE   BADKEY                                                           
         CLC   SVKEY+5(3),2(R6)                                                 
         BNE   *-14                                                             
         DROP  R6,R8                                                            
*                                                                               
* MGROUP MAY HAVE STARTING MKT NUM FOR DISPLAY                                  
*                                                                               
MGRP45   DS    0H                                                               
         L     R4,FADDR                                                         
         AH    R4,FLEN                                                          
         CLI   0(R4),C','                                                       
         BNE   MGRPX                                                            
* EDIT MKT NUM                                                                  
         MVI   ERRCD,5                                                          
         BAS   R9,GETNUM                                                        
         STH   R0,SVMKT                                                         
*                                                                               
MGRPX    B     CHKSPT                                                           
         EJECT                                                                  
ADV      MVI   SVKEY,5                                                          
         MVI   ERRCD,1                                                          
         BAS   R9,GETCLT                                                        
         MVC   SVKEY+1(2),SVCLT                                                 
         B     CHKSPT                                                           
         SPACE 2                                                                
AGY      MVI   SVKEY,6                                                          
         MVI   ERRCD,1                                                          
         BAS   R9,GETAGY                                                        
         MVC   SVKEY+1(2),DUB                                                   
         B     CHKSPT                                                           
         SPACE 2                                                                
RPT      MVI   SVKEY,7                                                          
         MVI   ERRCD,1                                                          
         BAS   R9,GETNUM                                                        
         CP    DUB,=P'0'                                                        
         BNH   BADKEY                                                           
         CP    DUB,=P'99'                                                       
         BH    BADKEY                                                           
         UNPK  SVKEY+1(2),DUB                                                   
*                                                                               
         MVI   ERRCD,2                                                          
         BAS   R9,GETAGY                                                        
         MVC   SVKEY+3(2),DUB                                                   
*                                                                               
         AR    R4,R5               POINT TO STOP CHAR                           
         CLI   0(R4),C' '                                                       
         BNH   RPTX                                                             
*                                                                               
         MVI   ERRCD,3                                                          
         BAS   R9,GETMED                                                        
         MVC   SVKEY+5(1),SVEBCMED                                              
*                                                                               
         AR    R4,R5                                                            
         CLI   0(R4),C' '                                                       
         BNH   RPTX                                                             
*                                                                               
         MVI   ERRCD,4                                                          
         BAS   R9,GETCLT                                                        
         MVC   SVKEY+6(2),SVCLT                                                 
*                                                                               
RPTX     B     CHKSPT                                                           
         EJECT                                                                  
DPT      MVI   SVKEY,8                                                          
         MVI   ERRCD,1                                                          
         BAS   R9,GETAGY                                                        
         MVC   SVKEY+1(2),DUB                                                   
*                                                                               
         MVI   ERRCD,2                                                          
         BAS   R9,GETMED                                                        
         MVC   SVKEY+3(1),SVEBCMED                                              
*                                                                               
         MVI   ERRCD,3                                                          
         BAS   RE,FLDVAL                                                        
         CLI   FLEN+1,1                                                         
         BNE   BADKEY                                                           
         CLI   0(R4),C'A'                                                       
         BL    BADKEY                                                           
         CLI   0(R4),C'Z'                                                       
         BNH   DPTX                                                             
         CLI   0(R4),C'0'                                                       
         BL    BADKEY                                                           
         CLI   0(R4),C'9'                                                       
         BH    BADKEY                                                           
DPTX     MVC   SVKEY+4(1),0(R4)                                                 
         B     CHKSPT                                                           
         EJECT                                                                  
EQU      MVI   SVKEY,9                                                          
         MVI   ERRCD,1                                                          
         BAS   R9,GETAGY                                                        
         MVC   SVKEY+1(2),DUB                                                   
         MVI   ERRCD,2                                                          
         BAS   R9,GETMED                                                        
         MVC   SVKEY+3(1),SVEBCMED                                              
*                                                                               
         AR    R4,R5                                                            
         CLI   0(R4),C' '                                                       
         BNH   EQUX                                                             
*                                                                               
         MVI   ERRCD,3                                                          
         BAS   R9,GETCLT                                                        
         MVC   SVKEY+4(2),SVCLT                                                 
*                                                                               
EQUX     B     CHKSPT                                                           
         EJECT                                                                  
DEM      DS    0H                                                               
         XC    LFMMSG,LFMMSG                                                    
         MVC   LFMMSG(41),=C'** ERROR ** RECORDS NOW SUPPORTED IN $SFM'         
         FOUT  LFMMSGH                                                          
         MVI   ERRAREA,X'FF'                                                    
         OI    6(R2),X'40'         POSITION CURSOR                              
         B     EXXMOD                                                           
*                                                                               
         MVI   SVEBCMED,C'N'                                                    
         BAS   R9,GETMED1                                                       
         MVC   SVKEY(2),=X'0D17'                                                
         MVC   SVKEY+2(1),SVAGYMD                                               
         MVI   ERRCD,1                                                          
         BAS   R9,GETNWK                                                        
         MVC   SVNTWK,DUB                                                       
         MVC   FULL,DUB            SAVE ACTUAL NETWORK CODE                     
*                                                                               
         BAS   R9,RDNWK            NOTE - NETWORK DA SAVED IN PRDDA             
         L     R8,AREC                                                          
         USING NDEFRECD,R8                                                      
         GOTO1 GETREC                                                           
         LA    R6,NDEFEL                                                        
         MVI   ELCODE,X'02'        FIND NETWORK SEQUENCE NUMBER                 
         USING NDEFEL02,R6                                                      
         BAS   RE,GETEL                                                         
         BNE   BADKEY                                                           
         MVC   SVKEY+3(1),NDEFNET                                               
*                                                                               
         MVI   ERRCD,2                                                          
         BAS   R9,GETPGM                                                        
         MVC   SVKEY+7(4),DUB                                                   
         BAS   R9,RDPGM                                                         
         USING NPGMRECD,R8                                                      
         MVC   LFMKEXP(17),NPGMPGM    DISPLAY PROGRAM NAME                      
         FOUT  LFMKEXPH                                                         
         DROP  R8                                                               
*                                                                               
         MVI   ERRCD,3                                                          
         BAS   R9,GETRATS                                                       
         MVC   SVKEY+11(1),DUB                                                  
         CLI   SVREC,X'2B'         DEMO DEFINITION                              
         BNE   DEM2                                                             
         MVI   ERRCD,4             CHK FOR CLIENT                               
         BAS   RE,FLDVAL                                                        
         LTR   R5,R5               MISSING                                      
         BNZ   DEM01                                                            
         LA    R4,1(R4)                                                         
         ST    R4,FADDR                                                         
         B     DEM1                                                             
*                                                                               
DEM01    CLC   =C'ALL',0(R4)       ALLOW 'ALL'                                  
         BE    DEM1                                                             
         BAS   R9,GETCLT2          GET CLIENT                                   
         MVC   SVKEY+4(2),SVCLT                                                 
         BAS   R9,RDCLT                                                         
         USING CLTHDRD,R8                                                       
         MVC   LFMKEXP+20(20),CNAME    PUT OUT CLIENT NAME                      
         DROP  R8                                                               
*                                                                               
DEM1     BAS   RE,FLDVAL                                                        
         LTR   R5,R5               MISSING                                      
         BZ    DEMX                                                             
         MVI   ERRCD,5             CHK FOR OPTIONAL SEQ NUMBER                  
         XC    FLEN,FLEN           RE-EDIT                                      
         BAS   R9,GETNUM                                                        
         C     R0,=F'1'            MUST BE 0 OR 1                               
         BH    BADKEY                                                           
         STC   R0,SVKEY+12                                                      
*                                                                               
DEMX     B     CHKSPT                                                           
*                                                                               
DEM2     MVI   ERRCD,4                                                          
         BAS   RE,FLDVAL                                                        
         BAS   RE,GETDEM           3 BYTE DEMO RETURNED IN DUB                  
         AR    R4,R5                                                            
         CLI   0(R4),C' '                                                       
         BNH   DEM3                                                             
         BAS   RE,FLDVAL                                                        
         LTR   R5,R5               TEST CLIENT OMITTED                          
         BZ    DEM2A                                                            
         XC    FLEN,FLEN           RE-EDIT                                      
         MVI   ERRCD,5                                                          
         BAS   R9,GETCLT                                                        
         MVC   SVKEY+4(2),SVCLT                                                 
* NEED TO CHECK FOR CLIENT SPECIFIC NETWORK                                     
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D11'                                                  
         MVC   KEY+2(2),AGYALPHA                                                
         MVC   KEY+4(4),SVNTWK                                                  
         MVC   KEY+8(2),SVCLT                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   *+10                                                             
         MVC   SVPRDDA,KEY+14        SAVE IN PRDDA                              
*                                                                               
DEM2A    MVI   ERRCD,4                                                          
*                                                                               
DEM3     BAS   R9,RDDEMD           READ DEFINITION REC                          
*                                  DEMO MUST BE IN DEMOLIST                     
         USING DOVRECD,R8                                                       
DEM4     MVI   ERRCD,4                                                          
         ZIC   R6,DOVEL01+1        ELEM LENGTH                                  
         SH    R6,=H'12'           SUBTRACT DATE FLDS + ELEM ID + LEN           
         LTR   R6,R6                                                            
         BNP   BADKEY              MEANS DEMD ELEM HAD NO DEMOS                 
         LR    R1,R8                                                            
         SR    R8,R8                                                            
         LR    R9,R6                                                            
         D     R8,=F'3'             R9 GETS BCT LIMIT                           
         LR    R6,R9                                                            
         LR    R8,R1                                                            
         SPACE                                                                  
         LA    R5,DOVDLSTC                                                      
         SR    R4,R4                                                            
DEM5     MVC   WORK(3),0(R5)                                                    
         NI    WORK,X'7F'          SET OFF HIGH ORDER BIT                       
         CLC   DUB(3),WORK         NOW MATCH DEMOS                              
         BE    DEM10               FOUND - OK                                   
         LA    R5,3(R5)                                                         
         LA    R4,1(R4)                                                         
         BCT   R6,DEM5                                                          
         CLI   DOVKSEQ,0                                                        
         BNE   BADKEY              NOT FOUND                                    
         MVI   SVKEY+12,1          TRY FOR SEQ RECORD                           
         XC    KEY,KEY                                                          
         MVC   KEY(13),SVKEY                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   BADKEY              NO SEQ RECORD FOUND - BAD DEMO               
         B     DEM3                GO READ SEQ RECORD                           
*                                                                               
DEM10    STC   R4,SVEST      SAVE DISPLACEMENT IN SVEST                         
         OC    SVCLT,SVCLT                                                      
         BZ    DEMXX                                                            
         MVC   ELEM(L'KEY),KEY  SAVE KEY TO RE-GET DEMO                         
         BAS   R9,RDCLT                                                         
         USING CLTHDRD,R8                                                       
         MVC   LFMKEXP+30(20),CNAME    PUT OUT CLIENT NAME                      
         DROP  R8                                                               
         MVC   SVKEY,ELEM          RESTORE DEMO READ                            
         BAS   R9,RDDEMD           READ DEFINITION REC                          
*                                                                               
DEMXX    B     CHKSPTX                                                          
*                                  I.E. 3= THIRD DEMO IN DOVDSLTC               
         EJECT                                                                  
MENU     DC    0H'0'                                                            
         MVC   SVKEY(2),=X'0D26'                                                
         MVI   ERRCD,1                                                          
         BAS   R9,GETMED                                                        
         MVC   SVKEY+2(1),SVAGYMD  A-M                                          
         MVC   LFMKEXP(10),WORK+1                                               
         FOUT  LFMKEXPH                                                         
         SPACE 1                                                                
         MVI   ERRCD,2                                                          
         BAS   R9,GETPGM                                                        
         MVC   SVKEY+3(4),DUB      MENU CODE (1 TO 4 CHAR)                      
         B     CHKSPT                                                           
         SPACE 2                                                                
FLIGHT   DC    0H'0'                                                            
         MVC   SVKEY(2),=XL2'0D0D' TYPE                                         
         SPACE 1                                                                
         MVI   ERRCD,1                                                          
         BAS   R9,GETMED                                                        
         MVC   LFMKEXP(10),WORK+1                                               
         FOUT  LFMKEXPH                                                         
         MVC   SVKEY+2(1),SVAGYMD  A-M                                          
         SPACE 1                                                                
         MVI   ERRCD,2                                                          
         BAS   R9,GETCLT                                                        
         BAS   R9,RDCLT                                                         
         USING CLTHDRD,R8                                                       
         MVC   LFMKEXP+11(20),CNAME                                             
         MVC   SVCLPROF,CPROF                                                   
         MVC   SVCLEX,CEXTRA                                                    
         DROP  R8                                                               
         MVC   SVKEY+3(2),SVCLT    CLIENT                                       
         SPACE 1                                                                
         CLI   SVREC,X'1E'         TEST USER DEM                                
         BE    USRD                                                             
         SPACE 1                                                                
         MVI   ERRCD,3                                                          
         BAS   R9,GETPRD                                                        
         BAS   R9,RDPRD                                                         
         CLC   =C'POL',SVEBCPRD                                                 
         BE    FLTERR              PRODUCT MAY NOT BE POOL                      
         USING PRDHDRD,R8                                                       
         MVC   LFMKEXP+32(20),PNAME                                             
         DROP  R8                                                               
         MVC   SVKEY+5(3),SVEBCPRD PRODUCT                                      
         SPACE 1                                                                
*                                                                               
         MVI   ERRCD,4                                                          
         MVI   HALF2,C'Y'          TEST FOR YEAR                                
         BAS   R9,GETNUM                                                        
         STC   R0,SVKEY+8          YEAR                                         
         MVI   HALF2,C'N'                                                       
         MVI   SVKEY+9,0           ESTIMATE (CURRENTLY NOT USED)                
FLT15    XC    SVKEY+10(3),SVKEY+10                                             
         B     CHKSPT                                                           
         SPACE 1                                                                
FLTERR   XC    LFMMSG,LFMMSG                                                    
         MVC   LFMMSG(35),=C'*** ERROR * PRODUCT MAY NOT BE POOL'               
         FOUT  LFMMSGH                                                          
         MVI   ERRAREA,X'FF'                                                    
         B     EXXMOD                                                           
         SPACE 2                                                                
USRD     DS    0H                                                               
         MVI   SVKEY+1,X'28'       CORRECT KEY                                  
         B     CHKSPT                                                           
         EJECT                                                                  
NUNI     MVC   SVKEY(2),=X'0D22'                                                
         MVI   SVEBCMED,C'N'                                                    
         BAS   R9,GETMED1                                                       
         MVC   SVKEY+2(2),AGYALPHA                                              
         MVI   ERRCD,1                                                          
         BAS   RE,FLDVAL                                                        
         LTR   R5,R5               TEST DATA                                    
         BZ    BADKEY                                                           
         TM    FVAL,X'08'          TEST NUMERIC                                 
         BZ    NUNI10                                                           
         CLI   FLEN+1,4            MAX LEN 4                                    
         BH    BADKEY                                                           
         EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  SVKEY+11(3),0(0,R4) PWOS                                         
         OC    SVKEY+11(2),SVKEY+11                                             
         BZ    BADKEY                                                           
         MVI   SVKEY+13,0                                                       
         MVI   SVKEY+10,X'01'                                                   
         CLI   SVACT,C'S'          DDS FORMAT                                   
         BE    CHKSPTX2            NO FILE READING                              
         B     CHKSPT                                                           
*                                                                               
NUNI10   DS    0H                                                               
         MVI   ERRCD,1                                                          
         XC    FLEN,FLEN           SET TO RE-EDIT FIELD                         
         LA    R4,8(R2)                                                         
         ST    R4,FADDR                                                         
         BAS   R9,GETDAY                                                        
         MVC   SVKEY+11(2),DUB                                                  
         CLI   SVACT,C'S'          DDS FORMAT                                   
         BE    CHKSPTX2            NO FILE READING                              
         B     CHKSPT                                                           
         SPACE 2                                                                
NPGM     MVC   SVKEY(2),=X'0D20'                                                
         MVI   SVEBCMED,C'N'                                                    
         BAS   R9,GETMED1                                                       
         MVC   SVKEY+2(1),SVAGYMD                                               
         MVI   ERRCD,1                                                          
         BAS   R9,GETNWK                                                        
         MVC   SVSTA(4),DUB        READ NTWK STATION REC                        
         MVI   SVSTA+4,C'N'                                                     
         BAS   R9,READSTA                                                       
         PACK  DUB,REC+18(4)       GET MKT NUMBER                               
         CVB   R0,DUB                                                           
         STH   R0,SVMKT                                                         
         MVC   SVKEY+3(2),SVMKT                                                 
         MVI   ERRCD,2                                                          
         BAS   R9,GETNPGM                                                       
         MVC   SVKEY+5(6),DUB                                                   
         MVI   ERRCD,3                                                          
         BAS   R9,GETDAY           END DATE                                     
         MVC   SVKEY+11(2),DUB                                                  
*                                                                               
NPGMX    B     CHKSPT                                                           
         SPACE 2                                                                
NHUT     MVC   SVKEY(2),=X'0D50'                                                
         MVI   SVEBCMED,C'N'                                                    
         BAS   R9,GETMED1                                                       
         MVC   SVKEY+2(1),SVAGYMD                                               
         MVI   ERRCD,1                                                          
         BAS   R9,GETNSCH          GET NETWRK SCHEME                            
         MVC   SVKEY+3(1),DUB                                                   
         MVI   ERRCD,2                                                          
         BAS   R9,GETDOW           GET DAY OF WEEK                              
         MVC   SVKEY+4(1),DUB                                                   
         MVI   ERRCD,3                                                          
         BAS   R9,GETTIME                                                       
         MVC   SVKEY+5(2),DUB                                                   
         MVI   ERRCD,4                                                          
         BAS   R9,GETNUM                                                        
         LR    R9,R0               MOVE YEAR INTO R9                            
**       CP    DUB,=P'99'                                                       
**       BH    BADKEY                                                           
**       STC   R0,SVKEY+7          R0 HAS BINARY YEAR                           
         CP    DUB,=P'50'          CHK Y2K                                      
         BH    *+8                                                              
         LA    R9,100(R9)          CARRY 00+ AS 100+                            
         STC   R9,SVKEY+7          R0 HAS BINARY YEAR                           
         CLI   SVACT,C'S'          DDS FORMAT                                   
         BE    CHKSPTX2            NO FILE READING                              
         B     CHKSPT                                                           
         SPACE 2                                                                
NHHUT    MVC   SVKEY(2),=X'0D52'       HOLI-HUT                                 
         MVI   SVEBCMED,C'N'                                                    
         BAS   R9,GETMED1                                                       
         MVC   SVKEY+2(1),SVAGYMD                                               
         MVI   ERRCD,1                                                          
         BAS   R9,GETDAY                                                        
         MVC   SVKEY+3(2),DUB                                                   
*              CHECK FOR OPTIONAL SCHEME CODE                                   
         MVI   ERRCD,2                                                          
         BAS   RE,FLDVAL                                                        
         LTR   R5,R5                                                            
         BZ    NHHUTX                                                           
         XC    FLEN,FLEN                                                        
         BAS   R9,GETNSCH                                                       
         MVC   SVKEY+5(1),DUB                                                   
NHHUTX   B     CHKSPT                                                           
         SPACE 2                                                                
NWK      XC    LFMMSG,LFMMSG                                                    
         MVC   LFMMSG(41),=C'** ERROR ** RECORDS NOW SUPPORTED IN $SFM'         
         FOUT  LFMMSGH                                                          
         MVI   ERRAREA,X'FF'                                                    
         OI    6(R2),X'40'         POSITION CURSOR                              
         B     EXXMOD                                                           
*                                                                               
         MVI   SVEBCMED,C'T'                                                    
         BAS   R9,GETMED1                                                       
         MVC   SVKEY(2),=X'0D11'                                                
         MVC   SVKEY+2(2),AGYALPHA                                              
         MVI   ERRCD,1                                                          
         BAS   R9,GETNWK                                                        
         TM    FVAL,X'04'          TEST ALPHA                                   
         BZ    BADKEY              CANADIAN NETWORKS MUST BE ALPHA              
         MVC   SVKEY+4(4),DUB                                                   
*              CHECK FOR OPTIONAL CLIENT CODE                                   
         MVI   ERRCD,2                                                          
         BAS   RE,FLDVAL                                                        
         LTR   R5,R5                                                            
         BZ    NWK10                                                            
         XC    FLEN,FLEN                                                        
         BAS   R9,GETCLT                                                        
         BAS   R9,RDCLT                                                         
         MVC   SVKEY+08(2),SVCLT                                                
* EDIT OPTIONAL ESTIMATE NUMBER                                                 
         MVI   ERRCD,3                                                          
         BAS   RE,FLDVAL                                                        
         LTR   R5,R5                                                            
         BZ    NWK10                                                            
         XC    FLEN,FLEN                                                        
         BAS   R9,GETNUM                                                        
         STC   R0,SVKEY+10                                                      
*                                                                               
NWK10    DS    0H                                                               
         CLI   SVACT,C'A'          TEST ADD                                     
         BNE   NWKX                                                             
* NEED TO MAKE SURE THERE IS A STATION MASTER REC FOR THIS NETWORK              
         XC    KEY,KEY                                                          
         MVI   KEY,C'S'                                                         
         MVI   KEY+1,C'N'                                                       
         MVC   KEY+2(4),SVKEY+4    MOVE CALL LETTERS                            
         MVI   KEY+6,C'N'                                                       
         MVC   KEY+7(2),AGYALPHA                                                
         MVC   COMMAND,=C'DMRDHI'                                               
         GOTO1 STA                                                              
         CLC   KEY(9),REC                                                       
         BE    NWKX                                                             
         MVI   ERRCD,NOSTAERR                                                   
         B     KEYERR                                                           
*                                                                               
NWKX     B     CHKSPT                                                           
*                                                                               
         SPACE 2                                                                
PGM      MVI   SVEBCMED,C'N'                                                    
         BAS   R9,GETMED1                                                       
         MVC   SVKEY(2),=X'0D12'                                                
         MVC   SVKEY+2(2),AGYALPHA                                              
         MVI   ERRCD,1                                                          
         BAS   R9,GETNWK                                                        
         MVC   SVKEY+4(4),DUB                                                   
         BAS   R9,RDNWK                                                         
         MVI   ERRCD,2                                                          
         BAS   R9,GETPGM                                                        
         MVC   SVKEY+8(4),DUB                                                   
         B     CHKSPT                                                           
         EJECT                                                                  
SPIL     MVC   SVKEY(2),=X'0D13'                                                
         MVC   SVKEY+2(2),AGYALPHA                                              
         MVI   ERRCD,1                                                          
         BAS   R9,GETRATS          RATING SERVICE                               
         MVC   SVKEY+4(1),DUB                                                   
         MVI   ERRCD,2                                                          
         BAS   R9,GETSTA                                                        
         CLI   SVSTA+4,C'T'        MUST BE TV                                   
         BNE   BADKEY                                                           
         MVC   KEY(17),ZEROS                                                    
*              CHECK FOR OPTIONAL CLIENT CODE                                   
         MVI   ERRCD,3                                                          
         BAS   RE,FLDVAL                                                        
         LTR   R5,R5                                                            
         BZ    SPIL10                                                           
         XC    FLEN,FLEN                                                        
         BAS   R9,GETCLT                                                        
         BAS   R9,RDCLT                                                         
         MVC   SVKEY+10(2),SVCLT                                                
         MVC   KEY+9(3),SVEBCCLT                                                
SPIL10   MVI   KEY,C'S'                                                         
         MVC   KEY+1(1),SVEBCMED                                                
         MVC   KEY+2(5),SVSTA                                                   
         MVC   KEY+7(2),AGYALPHA                                                
SPIL12   MVC   COMMAND,=C'DMRDHI'                                               
         GOTO1 STA                                                              
         CLC   REC(15),KEY                                                      
         BE    SPIL15                                                           
         CLC   KEY+9(3),=C'000'       SEE IF I WAS TRYING FOR DEFAULT           
         BE    SPIL13              YES - SEND NOT FOUND MSG                     
         MVC   KEY(17),ZEROS       TRY FOR DEFAULT STATION                      
         B     SPIL10                                                           
*                                                                               
SPIL13   MVI   ERRCD,NOSTAERR                                                   
         B     KEYERR                                                           
*                                                                               
SPIL15   DS    0H                                                               
         PACK  DUB,REC+18(4)       SAVE MKT FROM STA MASTER                     
         CVB   R0,DUB                                                           
         STH   R0,SVMKT                                                         
         MVC   SVKEY+5(4),SVSTA                                                 
*                                                                               
SPIL17   DS    0H                                                               
         CLI   SVACT,C'A'                                                       
         BNE   SPILX                                                            
         MVI   ERRCD,141           RTGSVC-STA MUST  EXIST BEFORE CLT            
         OC    SVKEY+10(2),SVKEY+10                                             
         BZ    SPILX                                                            
         XC    KEY,KEY                                                          
         MVC   KEY(10),SVKEY                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(10),SVKEY                                                    
         BNE   KEYERR                                                           
*                                                                               
SPILX    B     CHKSPT                                                           
         EJECT                                                                  
XSPIL    MVC   SVKEY(2),=X'0D23'                                                
         MVC   SVKEY+2(2),AGYALPHA                                              
         MVI   ERRCD,1                                                          
         BAS   R9,GETRATS          RATING SERVICE                               
         MVC   SVKEY+4(1),DUB                                                   
         MVI   ERRCD,2                                                          
         BAS   R9,GETSTA                                                        
         CLI   SVSTA+4,C'T'        MUST BE TV                                   
         BNE   BADKEY                                                           
         MVC   KEY(17),ZEROS                                                    
*              CHECK FOR OPTIONAL CLIENT CODE                                   
         MVI   ERRCD,3                                                          
         BAS   RE,FLDVAL                                                        
         LTR   R5,R5                                                            
         BZ    XSPIL10                                                          
         XC    FLEN,FLEN                                                        
         BAS   R9,GETCLT                                                        
         BAS   R9,RDCLT                                                         
         MVC   SVKEY+10(2),SVCLT                                                
         MVC   KEY+9(3),SVEBCCLT                                                
XSPIL10  MVI   KEY,C'S'                                                         
         MVC   KEY+1(1),SVEBCMED                                                
         MVC   KEY+2(5),SVSTA                                                   
         MVC   KEY+7(2),AGYALPHA                                                
         MVC   COMMAND,=C'DMRDHI'                                               
         GOTO1 STA                                                              
         CLC   REC(15),KEY                                                      
         BE    XSPIL30                                                          
         CLC   KEY+9(3),=C'000'       SEE IF I WAS TRYING FOR DEFAULT           
         BE    XSPIL20             YES - SEND NOT FOUND MSG                     
         MVC   KEY(17),ZEROS       TRY FOR DEFAULT STATION                      
         B     XSPIL10                                                          
*                                                                               
XSPIL20  MVI   ERRCD,NOSTAERR                                                   
         B     KEYERR                                                           
*                                                                               
XSPIL30  DS    0H                                                               
         PACK  DUB,REC+18(4)       SAVE MKT FROM STA MASTER                     
         CVB   R0,DUB                                                           
         STH   R0,SVMKT                                                         
         MVC   SVKEY+5(4),SVSTA                                                 
*                                                                               
XSPILX   B     CHKSPT                                                           
         EJECT                                                                  
CHKSPT   DS    0H                                                               
* ALL KEY DATA SHOULD BE PROCESSED                                              
         MVI   ERRCD,KEYLNERR                                                   
         L     R4,FADDR                                                         
         AH    R4,FLEN                                                          
         CLI   0(R4),C' '                                                       
         BH    KEYERR                                                           
         MVC   KEY,SVKEY                                                        
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         CLI   SVREC,X'1A'         TEST POL SUBEST                              
         BE    CHKSPT1             YES - MUST ALWAYS BE ON FILE                 
         CLI   SVACT,C'A'          TEST ADD                                     
         BE    CHKSPT2                                                          
*                                                                               
* ACTION IS NOT ADD                                                             
*                                                                               
CHKSPT1  MVI   ERRCD,NOFNDERR                                                   
         CLC   KEY(13),KEYSAVE                                                  
         BNE   KEYERR                                                           
*                                                                               
CHKSPT1E MVI   ERRCD,CLSDERR                                                    
         TM    KEY+13,X'C0'        TEST CLOSED OUT                              
         BO    KEYERR                                                           
         CLI   SVACT,C'R'          ACTION NOT RESTORE                           
         BE    CHKSPT1F                                                         
         MVI   ERRCD,DELERR                                                     
         TM    KEY+13,X'80'        TEST DELETED                                 
         BO    KEYERR                                                           
         B     CHKSPTX                                                          
*                                                                               
CHKSPT1F DS    0H                  ACTION = RESTORE                             
         TM    KEY+13,X'80'                                                     
         BO    CHKSPTX                                                          
         LA    R2,LFMKEYH                                                       
         OI    6(R2),X'40'         POSITION CURSOR                              
         MVC   LFMMSG(18),=C'RECORD NOT DELETED'                                
         FOUT  LFMMSGH                                                          
         MVI   ERRAREA,X'FF'                                                    
         B     EXXMOD                                                           
         EJECT                                                                  
* ACTION IS ADD                                                                 
*                                                                               
CHKSPT2  CLC   KEY(13),KEYSAVE     TEST DUP                                     
         BNE   CHKSPTX2                                                         
         TM    KEY+13,X'80'                                                     
         BZ    CHKSPT4                                                          
         CLI   SVREC,X'1D'           DELETED OK FOR COMMENTS                    
         BE    CHKSPTX                                                          
         CLI   SVREC,X'1C'                                                      
         BE    CHKSPTX                                                          
CHKSPT4  MVI   ERRCD,DUPERR                                                     
         TM    KEY+13,X'80'                                                     
         BZ    KEYERR                                                           
         MVI   ERRCD,DELERR                                                     
         B     KEYERR                                                           
CHKSPTX  MVC   SVKEY,KEY           SAVE KEY/CNTL/DA                             
CHKSPTX2 OI    LFMKEYH+4,X'20'     SET KEY VALID                                
         B     EXXMOD                                                           
         EJECT                                                                  
MAS      MVC   SVKEY(17),ZEROS                                                  
         MVI   SVKEY,C'S'                                                       
         MVI   ERRCD,1                                                          
         BAS   R9,GETSTA                                                        
         MVC   SVKEY+1(1),SVEBCMED                                              
         MVC   SVKEY+2(5),SVSTA                                                 
         MVC   SVKEY+7(2),AGYALPHA                                              
* CHECK FOR OPTIONAL CLIENT CODE                                                
         MVI   ERRCD,3                                                          
         BAS   RE,FLDVAL                                                        
         LTR   R5,R5                                                            
         BZ    MAS10                                                            
         XC    FLEN,FLEN           SET TO EDIT AGAIN                            
         BAS   R9,GETCLT                                                        
         BAS   R9,RDCLT                                                         
         MVC   KEY,SVKEY           FOR CLT EXCEPTIONS - FIRST BE                
         MVC   COMMAND,=C'DMRDHI'           SURE REGULAR STATION                
         GOTO1 STA                          IS ON FILE                          
         MVI   ERRCD,NOFNDERR                                                   
         CLC   KEY(15),REC                                                      
         BNE   KEYERR                                                           
*                                                                               
         MVC   SVKEY+9(3),SVEBCCLT                                              
* SPECIAL TESTS FOR BM                                                          
MAS10    CLC   =C'DD',AGYALPHA                                                  
         BE    MASBM                                                            
         B     MAS20                                                            
* DD STATIONS MUST BE ON BOC FIRST                                              
*                                                                               
MASBM    CLI   SVACT,C'A'          TEST ADD                                     
         BNE   MASX                                                             
         MVC   KEY,SVKEY                                                        
         MVC   KEY+7(2),=C'BO'                                                  
         MVC   KEY+9(3),ZEROS                                                   
         MVC   COMMAND,=C'DMRDHI'                                               
         GOTO1 STA                                                              
*                                                                               
         MVI   ERRCD,NOADVSTA                                                   
         CLC   KEY(15),REC                                                      
         BNE   KEYERR                                                           
         EJECT                                                                  
MAS20    DS    0H                                                               
*                                                                               
MASX     B     CHKSTA                                                           
         SPACE 2                                                                
ADDR     MVC   SVKEY,ZEROS                                                      
         MVI   SVKEY,C'A'                                                       
* FIRST CHECK FOR 'BOX' - REMITTANCE ADDRESS REC                                
         MVI   ERRCD,1                                                          
         BAS   RE,FLDVAL                                                        
         CLI   FLEN+1,4                                                         
         BNE   ADDR2                                                            
         CLC   =C'BOX',0(R4)                                                    
         BNE   ADDR2                                                            
         MVI   SVKEY+1,C'T'        FORCE MEDIA TO T                             
         MVI   SVEBCMED,C'T'                                                    
         MVC   SVKEY+2(4),0(R4)                                                 
         MVI   SVKEY+6,C'T'                                                     
         MVC   SVKEY+7(2),AGYALPHA                                              
         B     CHKSTA                                                           
         SPACE 2                                                                
ADDR2    XC    FLEN,FLEN           SET TO RE-EDIT                               
         MVI   ERRCD,1                                                          
         BAS   R9,GETSTA                                                        
*                                                                               
         MVC   SVKEY+1(1),SVEBCMED                                              
         MVC   SVKEY+2(5),SVSTA                                                 
         MVC   SVKEY+7(2),AGYALPHA                                              
*                                                                               
         B     CHKSTA                                                           
         EJECT                                                                  
SPREP    MVC   SVKEY,ZEROS                                                      
         MVI   SVKEY,C'R'                                                       
*                                                                               
         MVI   ERRCD,1                                                          
         BAS   R9,GETMED                                                        
         MVC   SVKEY+1(1),SVEBCMED                                              
*                                                                               
         MVI   ERRCD,2                                                          
         BAS   R9,GETNUM                                                        
         CP    DUB,=P'999'                                                      
         BH    BADKEY                                                           
         UNPK  SVKEY+2(3),DUB                                                   
         MVC   SVKEY+5(2),AGYALPHA                                              
         B     CHKSTA                                                           
         SPACE 2                                                                
REP      MVC   SVKEY,ZEROS                                                      
         MVI   SVKEY,C'R'                                                       
*                                                                               
         MVI   ERRCD,1                                                          
         BAS   R9,GETMED                                                        
         MVC   SVKEY+1(1),SVEBCMED                                              
* ALLOW REP TO BE ANY 3 CHARS BUT ALL                                           
         MVI   ERRCD,2                                                          
         BAS   RE,FLDVAL                                                        
         CH    R5,=H'3'                                                         
         BNE   BADKEY                                                           
         CLC   =C'ALL',0(R4)                                                    
         BE    BADKEY                                                           
         MVC   SVKEY+2(3),0(R4)                                                 
         MVC   SVKEY+5(2),AGYALPHA                                              
         CLC   SVCTAGY,=C'  '      DOING CODE COORDINATION ?                    
         BNH   CHKSTA              NO                                           
         CLI   SVEBCMED,C'N'       TEST NETWORK                                 
         BE    REP10                                                            
*NOP====================                                                        
         B     CHKSTA                                                           
*NOP====================                                                        
REP10    BAS   RE,CHKCTREP                                                      
         B     CHKSTA                                                           
         SPACE 2                                                                
MKT      MVC   SVKEY,ZEROS                                                      
         MVI   SVKEY,C'M'                                                       
         MVI   ERRCD,1                                                          
         BAS   R9,GETMED                                                        
         MVC   SVKEY+1(1),SVEBCMED                                              
         MVI   ERRCD,2                                                          
         BAS   R9,GETNUM                                                        
         CLI   SVAPROF+7,C'C'          SEE IF CANADIAN AGY                      
         BNE   MKT5                                                             
         CLI   SVEBCMED,C'T'       ALLOW MKT=0000 FOR CANADIAN TV               
         BE    MKT7                                                             
         CLI   SVEBCMED,C'N'                                                    
         BNE   MKT3                                                             
         CP    DUB,=P'0'           MUST BE MKT 0000 FOR NETWORK                 
         BNE   BADKEY                                                           
         B     MKT7                                                             
*                                                                               
MKT3     CLI   SVEBCMED,C'C'                                                    
         BNE   MKT5                                                             
         MVI   ERRCD,1             MEDIA CANNOT BE C FOR CANADA                 
         B     BADKEY                                                           
*                                                                               
MKT5     CP    DUB,=P'0'                                                        
         BNH   BADKEY                                                           
MKT7     CLC   2(2,RA),=X'0001'    FOR MASTER TERMINAL                          
         BE    MKT10               ALLOW ALL MKTS                               
         CP    DUB,=P'9998'        ELSE ALLOW 9998                              
         BE    MKT10               (SPECIAL FOR SPOT BILLING)                   
         CP    DUB,=P'9999'        BUT NO OTHERS OVER 9999                      
         BH    BADKEY                                                           
MKT10    UNPK  SVKEY+2(4),DUB                                                   
         MVC   SVKEY+6(2),AGYALPHA                                              
         B     CHKSTA                                                           
         EJECT                                                                  
CHKSTA   DS    0H                                                               
* ALL KEY DATA SHOULD BE PROCESSED                                              
         MVI   ERRCD,KEYLNERR                                                   
         L     R4,FADDR                                                         
         AH    R4,FLEN                                                          
         CLI   0(R4),C' '                                                       
         BH    KEYERR                                                           
*                                                                               
         MVC   KEY(17),SVKEY                                                    
         OI    DMINBTS,X'08'       PASS DELETES                                 
         MVC   COMMAND,=C'DMRDHI'                                               
         GOTO1 STA                                                              
         NI    DMINBTS,X'F7'                                                    
         CLI   SVACT,C'A'                                                       
         BE    CHKSTA2                                                          
* ACTION IS NOT ADD                                                             
         MVI   ERRCD,NOFNDERR                                                   
         CLC   REC(15),SVKEY                                                    
         BNE   KEYERR                                                           
         MVI   ERRCD,DELERR                                                     
         TM    REC+17,X'80'        TEST DELETED                                 
         BO    KEYERR                                                           
         B     CHKSTAX                                                          
* ACTION IS ADD                                                                 
CHKSTA2  CLC   REC(15),SVKEY                                                    
         BNE   EXXMOD                                                           
         MVI   ERRCD,DUPERR                                                     
         TM    REC+17,X'80'                                                     
         BZ    KEYERR                                                           
         MVI   ERRCD,DELERR                                                     
         B     KEYERR                                                           
*                                                                               
CHKSTAX  MVC   SVKEY(18),REC       SAVE KEY/CNTL                                
*                                                                               
CHKSTAXX DS    0H                                                               
         OI    LFMKEYH+4,X'20'     SET KEY VALID                                
         B     EXXMOD                                                           
         EJECT                                                                  
GETMED   BAS   RE,FLDVAL                                                        
         CLI   FLEN+1,1                                                         
         BNE   BADKEY                                                           
         MVC   SVEBCMED,0(R4)                                                   
GETMED1  GOTO1 =V(MEDGET),DMCB,(SVEBCMED,AGYALPHA),VDATAMGR,WORK,      X        
               RR=RELO                                                          
         ORG   *-2                                                              
         CLI   SVREC,X'35'         EQUHDR ALLOWS MEDIA 'Z' FOR TV               
         BNE   GETMED2                                                          
         CLI   SVEBCMED,C'Z'                                                    
         BNE   GETMED2                                                          
         MVI   0(R1),C'T'                                                       
*                                                                               
GETMED2  BASR  RE,RF               CALL MEDGET                                  
         CLI   8(R1),X'FF'                                                      
         BE    BADKEY                                                           
         CLI   SVAPROF+7,C'C'      CANADIAN AGY                                 
         BNE   GETMEDX             NO NETWORK OR COMBINED                       
         CLI   SVREC,X'2E'         SHOWS                                        
         BE    GETMEDX             YES - BYPASS CHKS                            
         TM    SVLOCK,X'40'        TEST MEDIA C OR N ALLOWED                    
         BO    GETMEDX             YES                                          
         CLI   SVACT,C'A'          SEE IF ADD                                   
         BE    GETMED5                                                          
         CLI   SVREC,X'13'         OR ESTS TO ALLOW FOR AUTH $                  
         BE    GETMEDX             CHANGES                                      
         CLI   SVREC,X'53'         OR ESTD TO ALLOW FOR AUTH $                  
         BE    GETMEDX             CHANGES                                      
GETMED5  CLI   SVEBCMED,C'N'                                                    
         BE    BADKEY                                                           
         CLI   SVEBCMED,C'C'                                                    
         BE    BADKEY                                                           
*                                                                               
GETMEDX  MVC   SVAGYMD,WORK                                                     
         BR    R9                                                               
         SPACE 2                                                                
GETCLT   BAS   RE,FLDVAL                                                        
         LTR   R5,R5                                                            
         BZ    BADKEY                                                           
GETCLT2  CLI   FLEN+1,3                                                         
         BH    BADKEY                                                           
         MVC   SVEBCCLT,SPACES                                                  
         BCTR  R5,0                                                             
         EX    R5,MVCLT                                                         
*                                                                               
* CLPACK                                                                        
         MVC   DMCB+4(4),=X'D9000A14'                                           
         GOTO1 VCALLOV,DMCB,0                                                   
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),SVEBCCLT,SVCLT                                         
         CLI   DMCB,0                                                           
         BNE   BADKEY                                                           
*                                                                               
         CLC   =C'ALL',SVEBCCLT                                                 
         BE    BADKEY                                                           
         CLI   T219FFD+6,C'*'      TEST OFFICE LOCKOUT                          
         BER   R9                  YES                                          
         CLI   T219FFD+6,C'$'      TEST OFFICE LIST LOCKOUT                     
         BER   R9                  YES                                          
         MVI   ERRCD,SCRTYERR                                                   
         OC    T219FFD+6(2),T219FFD+6                                           
         BZ    *+14                                                             
         CLC   SVCLT,T219FFD+6                                                  
         BNE   KEYERR                                                           
         BR    R9                                                               
MVCLT    MVC   SVEBCCLT(0),0(R4)                                                
         EJECT                                                                  
GETPRD   BAS   RE,FLDVAL                                                        
*-------------------------------------------------------------                  
         CLI   SVREC,X'12'         PRDHDR                                       
         BNE   GETPRD4                                                          
*                                                                               
         CLI   0(R4),C'#'          PRODUCT NUMBER GIVEN?                        
         BNE   GETPRD4             NO                                           
         CLI   FLEN+1,3                                                         
         BNE   BADKEY                                                           
*                                                                               
         L     R8,VCOMFACS                                                      
         USING COMFACSD,R8                                                      
         GOTO1 CHEXIN,DMCB,1(R4),BYTE,2                                         
         DROP  R8                                                               
         OC    DMCB+16(4),DMCB+16  INVALID HEX NUMBER?                          
         BZ    BADKEY              YES                                          
*                                                                               
         LA    R8,REC                                                           
         USING CLTHDRD,R8                                                       
         LA    R8,CLIST                                                         
         DROP  R8                                                               
*                                                                               
GETPRD2  CLI   0(R8),0             END OF PRD TABLE?                            
         BE    BADKEY              YES, BAD PRD                                 
         CLC   BYTE,3(R8)          PRD FOUND?                                   
         BE    *+12                YES                                          
         LA    R8,4(R8)                                                         
         B     GETPRD2                                                          
*                                                                               
         MVC   0(3,R4),0(R8)       MOVE PRD NAME TO KEY                         
         OI    LFMKEYH+6,X'80'      TRANSMIT                                    
*                                                                               
         CLI   2(R4),C' '          IS PRD NAME TWO CHARACTERS?                  
         BNE   *+10                NO                                           
         MVI   FLEN+1,2            LENGTH GOES FROM 2 TO 3                      
         BCTR  R5,0                                                             
*----------------------------------------------------------------               
GETPRD4  CLI   SVREC,X'46'           PRODUCT EXCLUSION REC                      
         BNE   GETPRD5                                                          
         CLC   =C'POL',0(R4)                                                    
         BE    BADKEY                                                           
*                                                                               
GETPRD5  CLI   0(R4),C'A'                                                       
         BL    BADKEY                                                           
         CLI   0(R4),C'Z'                                                       
         BH    BADKEY                                                           
*                                                                               
         CLI   FLEN+1,2                                                         
         BL    BADKEY                                                           
         CLI   FLEN+1,3                                                         
         BH    BADKEY                                                           
*                                                                               
         ZIC   R0,FLEN+1                                                        
         BCTR  R0,0                                                             
         LA    R1,1(R4)            POINT TO SECOND CHAR OF PRD                  
*                                                                               
GETPRD7  CLI   0(R1),C'A'                                                       
         BL    BADKEY                                                           
         CLI   0(R1),C'Z'                                                       
         BNH   GETPRD9                                                          
         CLI   0(R1),C'0'                                                       
         BL    BADKEY                                                           
         CLI   0(R1),C'9'                                                       
         BH    BADKEY                                                           
GETPRD9  LA    R1,1(R1)                                                         
         CLI   0(R1),C'#'          FOR TRADE PRODUCTS                           
         BE    *+8                                                              
         BCT   R0,GETPRD7                                                       
*                                                                               
         MVC   SVEBCPRD,SPACES                                                  
         BCTR  R5,0                                                             
         EX    R5,MVPRD                                                         
         CLC   =C'ALL',SVEBCPRD                                                 
         BE    BADKEY                                                           
         CLC   =C'NO',SVEBCPRD                                                  
         BE    BADKEY                                                           
         BR    R9                                                               
MVPRD    MVC   SVEBCPRD(0),0(R4)                                                
         SPACE 2                                                                
GETNUM   BAS   RE,FLDVAL                                                        
*                                                                               
         CLI   SVREC,X'46'         PRODUCT EXCLUSION REC                        
         BNE   GETNUM5                                                          
         CLC   =C'ALL',0(R4)                                                    
         BNE   GETNUM5                                                          
         MVI   SVEST,X'00'                                                      
         B     PXC                                                              
*                                                                               
GETNUM5  LTR   R5,R5               TEST DATA                                    
         BZ    BADKEY                                                           
         TM    FVAL,X'08'          TEST NUMERIC                                 
         BZ    BADKEY                                                           
         LA    RF,4                THE MAX LENGTH = 4                           
         CLI   HALF2,C'Y'           UNLESS WE ARE GETTING THE YEAR              
         BNE   *+8                                                              
         LA    RF,2                                                             
         EX    RF,MAXLEN           TEST FOR MAX LENGTH                          
         BH    BADKEY                                                           
         BCTR  R5,0                                                             
         EX    R5,NUMPACK                                                       
         CVB   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         BR    R9                                                               
         SPACE 1                                                                
MAXLEN   CLI   FLEN+1,0                                                         
NUMPACK  PACK  DUB,0(0,R4)                                                      
         EJECT                                                                  
GETSTA   MVC   SVSTA,SPACES                                                     
*                                                                               
         BAS   RE,FLDVAL                                                        
*                                                                               
         CLI   SVREC,X'46'         PROD EXCLUSION REC                           
         BNE   GETSTA2                                                          
         CLC   =C'ALL',0(R4)       STA=ALL                                      
         BER   R9                                                               
*                                                                               
GETSTA2  CLI   FLEN+1,3                                                         
         BL    BADKEY                                                           
         CLI   FLEN+1,4                                                         
         BH    BADKEY                                                           
         CLI   SVREC,X'21'         MASTER STATION RECORDS                       
         BNE   GETSTA2B                                                         
         ZIC   R1,FLEN+1                                                        
         LTR   R1,R1                                                            
         BNP   BADKEY                                                           
         LR    RE,R4                                                            
*                                                                               
GETSTA2A CLI   0(RE),C'0'          TEST CABLE                                   
         BL    GETSTA2B                                                         
         LA    RE,1(RE)                                                         
         BCT   R1,GETSTA2A                                                      
         B     USESFM                                                           
*                                                                               
GETSTA2B TM    FVAL,X'04'          TEST ALPHA                                   
         BZ    BADKEY                                                           
         LA    RE,0(R4,R5)         POINT TO STOP CHAR                           
         CLI   0(RE),C'-'                                                       
         BE    GETSTA8                                                          
*                                                                               
         CLI   SVREC,X'2F'         TEST SPILL RECORD                            
         BE    GETSTA3                                                          
         CLI   SVREC,X'30'         OR XSPILL RECORD                             
         BNE   GETSTA4             NO - CONTINUE                                
*                                                                               
GETSTA3  BCTR  R5,0                FOR SPILL, FORCE MEDIA = TV                  
         EX    R5,MVSTA                                                         
         MVI   SVEBCMED,C'T'                                                    
         MVI   SVSTA+4,C'T'                                                     
         B     GETSTA12                                                         
*                                                                               
GETSTA4  CLI   SVREC,X'1F'         TEST STAT DESC COMMENT                       
         BE    GETSTA6                                                          
         CLI   SVREC,X'46'         TEST PRD EXCLUSION                           
         BE    GETSTA6                                                          
         B     BADKEY                                                           
*                                                                               
GETSTA6  BCTR  R5,0                                                             
         EX    R5,MVSTA                                                         
         CLI   SVEBCMED,C'R'       SUB-MEDIA REQD FOR RADIO                     
         BE    BADKEY                                                           
         MVC   SVSTA+4,SVEBCMED                                                 
         BR    R9                                                               
*                                                                               
GETSTA8  BCTR  R5,0                                                             
         EX    R5,MVSTA                                                         
*                                                                               
         MVI   ERRCD,MEDERR                                                     
         BAS   RE,FLDVAL                                                        
         LTR   R5,R5                                                            
         BZ    KEYERR                                                           
         BCTR  R5,0                                                             
*                                                                               
         MVI   SVEBCMED,C'R'                                                    
         EX    R5,STAAM                                                         
         BE    GETSTA10                                                         
         EX    R5,STAFM                                                         
         BE    GETSTA10                                                         
         MVI   SVEBCMED,C'T'                                                    
         EX    R5,STATV                                                         
         BE    GETSTA10                                                         
         CLI   FLEN+1,1                                                         
         BNE   KEYERR                                                           
         CLI   0(R4),C'R'                                                       
         BE    KEYERR              DON'T ALLOW WABC-R                           
         MVC   SVEBCMED,0(R4)                                                   
*                                                                               
GETSTA10 MVC   SVSTA+4(1),0(R4)                                                 
*                                                                               
GETSTA12 GOTO1 =V(MEDGET),DMCB,(SVEBCMED,AGYALPHA),VDATAMGR,WORK,      X        
               RR=RELO                                                          
*                                                                               
         CLI   8(R1),X'FF'                                                      
         BE    KEYERR                                                           
         CLI   SVAPROF+7,C'C'      CANADIAN AGY                                 
         BNE   GETSTAX             NO NETWORK OR COMBINED                       
         CLI   SVEBCMED,C'N'                                                    
         BE    KEYERR                                                           
         CLI   SVEBCMED,C'C'                                                    
         BE    KEYERR                                                           
*                                                                               
GETSTAX  MVC   SVAGYMD,WORK                                                     
         BR    R9                                                               
*                                                                               
MVSTA    MVC   SVSTA(0),0(R4)                                                   
STAAM    CLC   0(0,R4),=C'AM'                                                   
STAFM    CLC   0(0,R4),=C'FM'                                                   
STATV    CLC   0(0,R4),=C'TV'                                                   
         SPACE 2                                                                
GETAGY   BAS   RE,FLDVAL                                                        
         CLI   FLEN+1,2                                                         
         BNE   BADKEY                                                           
         MVC   DUB(2),0(R4)                                                     
         CLC   DUB(2),ZEROS                                                     
         BE    GETAGYX                                                          
         CLI   DUB,C'0'                                                         
         BL    GETAGYY                                                          
         CLI   DUB,C'9'                                                         
         BH    BADKEY                                                           
         B     GETAGYX                                                          
GETAGYY  CLI   DUB,C'A'                                                         
         BL    BADKEY                                                           
         CLI   DUB,C'Z'                                                         
         BH    BADKEY                                                           
GETAGYX  BR    R9                                                               
         SPACE 2                                                                
GETNWK   BAS   RE,FLDVAL                                                        
         CLI   FLEN+1,4                                                         
         BH    BADKEY                                                           
         CLI   FLEN+1,3                                                         
         BL    BADKEY                                                           
         MVC   DUB(4),SPACES                                                    
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   DUB(0),0(R4) *EXECUTED*                                          
         CLI   DUB,C'A'                                                         
         BL    BADKEY                                                           
         BR    R9                                                               
*                                                                               
GETNSCH  BAS   RE,FLDVAL                                                        
         LTR   R5,R5               TEST DATA                                    
         BZ    BADKEY                                                           
         TM    FVAL,X'04'          TEST ALPHA - SCHEME                          
         BZ    BADKEY                                                           
         CLI   FLEN+1,1            MAX LEN 1                                    
         BH    BADKEY                                                           
         MVC   DUB(1),0(R4)                                                     
         BR    R9                                                               
         SPACE 2                                                                
GETDOW   BAS   RE,FLDVAL           EDIT FOR DAY OF WEEK                         
         LTR   R5,R5               TEST DATA                                    
         BZ    BADKEY                                                           
         CLI   FLEN+1,3            MAX LEN 1                                    
         BH    BADKEY                                                           
         LA    R1,DOWTAB                                                        
         LA    R6,7                FOR BCT                                      
         BCTR  R5,0                                                             
GETDOW5  EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R1),0(R4)                                                    
         BE    GETDOW6                                                          
         LA    R1,4(R1)                                                         
         BCT   R6,GETDOW5                                                       
         B     BADKEY                                                           
*                                                                               
GETDOW6  DS    0H                                                               
         CLI   FLEN+1,1                                                         
         BNE   GETDOW8                                                          
         CLI   0(R4),C'M'          NEED TO CHK FOR M-F AND M-S                  
         BNE   BADKEY                                                           
         CLI   1(R4),C'-'          DELIMITER MUST BE -                          
         BNE   BADKEY                                                           
         BAS   RE,FLDVAL                                                        
         CLI   FLEN+1,1                                                         
         BNE   BADKEY                                                           
         MVI   DUB,0               FOR M-F                                      
         CLI   0(R4),C'F'                                                       
         BER   R9                                                               
         MVI   DUB,X'08'           FOR M-S                                      
         CLI   0(R4),C'S'                                                       
         BER   R9                                                               
         B     BADKEY                                                           
*                                                                               
GETDOW8  MVC   DUB(1),3(R1)                                                     
         BR    R9                                                               
*                                                                               
         SPACE 2                                                                
GETTIME  BAS   RE,FLDVAL           EDIT FOR DAY OF WEEK                         
         LTR   R5,R5               TEST DATA                                    
         BZ    BADKEY                                                           
         GOTO1 =V(TIMVAL),DMCB,((R5),0(R4)),DUB,RR=RELO                         
         CLI   DMCB,X'FF'                                                       
         BE    BADKEY                                                           
         CLC   DUB(4),=C'NONE'                                                  
         BE    BADKEY                                                           
         CLC   DUB+2(2),=2X'00'                                                 
         BNE   BADKEY              NO END TIME ALLOWED                          
*        CLC   DUB(2),=X'0000'     FORCE 12A                                    
*        BNE   *+10                                                             
*        MVC   DUB(2),=X'0960'     TO 12M                                       
         BR    R9                                                               
         SPACE 2                                                                
GETPGM   BAS   RE,FLDVAL                                                        
         CLI   FLEN+1,4                                                         
         BH    BADKEY                                                           
         CLI   FLEN+1,0                                                         
         BE    BADKEY                                                           
         MVC   DUB(4),SPACES                                                    
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   DUB(0),0(R4) *EXECUTED*                                          
         BR    R9                                                               
*                                                                               
         SPACE 2                                                                
GETNPGM  BAS   RE,FLDVAL                                                        
         CLI   FLEN+1,6                                                         
         BH    BADKEY                                                           
         CLI   FLEN+1,0                                                         
         BE    BADKEY                                                           
         MVC   DUB(6),SPACES                                                    
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   DUB(0),0(R4)                                                     
         BR    R9                                                               
         SPACE 2                                                                
GETDAY   BAS   RE,FLDVAL                                                        
         GOTO1 VDATVAL,DMCB,(0,0(R4)),WORK,(R5)                                 
         OC    DMCB(4),DMCB                                                     
         BZ    BADKEY                                                           
         GOTO1 VDATCON,DMCB,(0,WORK),(2,DUB)                                    
         BR    R9                                                               
*                                                                               
         SPACE 2                                                                
GETRATS  BAS   RE,FLDVAL                                                        
         LTR   R5,R5                                                            
         BZ    BADKEY                                                           
         CLI   FLEN+1,3                                                         
         BH    BADKEY                                                           
         BCTR  R5,0                SET FOR EX                                   
*                                                                               
         MVI   DUB,C'1'                                                         
         LA    R1,=C'BBM'                                                       
         CLI   SVREC,X'30'         XSPILL REC IS INPUT BY US AGY                
         BE    GETRAT2             BUT SHOULD REFER TO CANADIAN SVC             
         CLI   SVAPROF+7,C'C'      TEST CANADIAN                                
         BE    *++8                                                             
         LA    R1,=C'ARB'                                                       
*                                                                               
GETRAT2  EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R4),0(R1)                                                    
         BER   R9                                                               
*                                                                               
         MVI   DUB,C'0'                                                         
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R4),=C'NSI'                                                  
         BER   R9                                                               
         B     BADKEY                                                           
         SPACE 2                                                                
GETDEM   NTR1                                                                   
         LTR   R5,R5                                                            
         BZ    FNDDERR                                                          
         XC    SVEST,SVEST         USED TO SAVE DISPLACEMENT                    
         XC    WORK(20),WORK       USED TO BUILD DUMMY FLDHDR FOR               
*                                  DEMOVAL                                      
         STC   R5,WORK+7           FLD LEN                                      
         STC   R5,WORK+5           INPUT LEN                                    
         AH    R5,=H'8'                                                         
         STC   R5,WORK             FLD + HDR LEN                                
         SH    R5,=H'9'                                                         
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   WORK+8(0),0(R4)                                                  
         LA    R6,REC2                                                          
         XC    0(250,R6),0(R6)      SET TO DEMAREA                              
         USING DBLOCK,R6                                                        
         MVC   DBCOMFCS,VCOMFACS                                                
         MVI   DBSELMED,C'C'                                                    
         MVC   DBFILE,=C'TPT'                                                   
         MVC   DMCB+4(4),=X'D9000AD9' DEMOVAL                                   
         GOTO1 VCALLOV,DMCB,0                                                   
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(1,WORK),(1,DUB),(C'S',DBLOCK),REC2+500                
         CLI   DMCB+4,0                                                         
         BE    FNDDERR                                                          
         MVC   DBFILE,=C'TP '                                                   
         MVC   DMCB+4(4),=X'D9000AE0' DEMOCON                                   
         GOTO1 VCALLOV,DMCB,0                                                   
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(1,DUB),(2,LFMKEXP+20),(C'S',DBLOCK),REC2+500          
FNDDX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
FNDDERR  MVI   ERRCD,DEMINV                                                     
         B     KEYERR                                                           
         DROP  R6                                                               
         SPACE 2                                                                
GETEL    CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
         CLC   ELCODE,0(R6)                                                     
         BER   RE                                                               
         B     NXT10                                                            
*                                                                               
NEXTEL   CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
*                                                                               
NXT10    SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   ELCODE,0(R6)                                                     
         BER   RE                                                               
         B     NEXTEL                                                           
NEXTELX  LTR   RE,RE               EXIT WITH CC NOT EQ                          
         BR    RE                                                               
         EJECT                                                                  
* EDIT GROUP CODE - FORMAT IS A999                                              
*                                                                               
GETGRP   XC    FULL,FULL                                                        
         MVI   HALF+1,0            RESET DIGITS ENTERED                         
*                                                                               
         BAS   RE,FLDVAL                                                        
         LTR   R5,R5                                                            
         BZ    GETGRPX                                                          
         CLI   0(R4),C'A'                                                       
         BL    BADKEY                                                           
         CLI   0(R4),C'Z'                                                       
         BH    BADKEY                                                           
         MVC   FULL(1),0(R4)       MOVE GROUP ID                                
*                                                                               
         LA    R4,1(R4)                                                         
         BCTR  R5,0                                                             
         LTR   R5,R5                                                            
         BZ    GETGRPX                                                          
*                                                                               
         LA    R0,3                PRDGRP HAS 3 DIGITS MAX                      
         CLI   HALF,C'P'           TEST PRDGRP EDIT                             
         BE    *+8                 YES                                          
         LA    R0,4                MKTGRP HAS 4 DIGITS MAX                      
* MAY NOT ENTER ALL 9'S                                                         
         LR    RE,R0                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R4),=C'9999'  **EXECUTED**                                   
         BE    BADKEY                                                           
*                                                                               
         CR    R5,R0                                                            
         BH    BADKEY                                                           
         STC   R5,HALF+1           RETURN NUMBER OF DIGITS ENTERED              
         STM   R4,R5,WORK                                                       
GETGRP2  CLI   0(R4),C'0'                                                       
         BL    BADKEY                                                           
         CLI   0(R4),C'9'                                                       
         BH    BADKEY                                                           
         LA    R4,1(R4)                                                         
         BCT   R5,GETGRP2                                                       
         LM    R4,R5,WORK                                                       
         XC    WORK,WORK                                                        
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(R4) *EXECUTED*                                         
         PACK  FULL+1(3),WORK(5)   GET DIGITS LEFT ALIGNED                      
*                                                                               
GETGRPX  BR    R9                                                               
         EJECT                                                                  
RDCLT    XC    KEY,KEY                                                          
         MVC   KEY+1(1),SVAGYMD                                                 
         MVC   KEY+2(2),SVCLT                                                   
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         MVI   ERRCD,NOCLTERR                                                   
         CLC   KEY(13),KEYSAVE                                                  
         BNE   KEYERR                                                           
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         USING CLTHDRD,R8                                                       
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ERRCD,SCRTYERR                                                   
         CLI   T219FFD+6,C'*'      TEST OFFICE LOCKOUT                          
         BNE   *+14                                                             
         CLC   T219FFD+7(1),COFFICE                                             
         BNE   KEYERR                                                           
*                                                                               
         CLI   T219FFD+6,C'$'      TEST OFFICE LIST LOCKOUT                     
         BNE   RDCLT10                                                          
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38'  GET OFFICER ADDRESS                      
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         XC    DUB,DUB                                                          
         LA    R1,DUB                                                           
         USING OFFICED,R1                                                       
         MVI   OFCSYS,C'S'         SYSTEM ID                                    
         MVC   OFCAUTH,T219FFD+6   ID AUTH VALUE                                
         MVC   OFCAGY,AGYALPHA                                                  
         MVC   OFCOFC,COFFICE                                                   
         DROP  R1                                                               
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,DUB,VCOMFACS                                           
         CLI   0(R1),0                                                          
         BNE   KEYERR                                                           
*                                                                               
RDCLT10  MVC   SVCLTDA,KEY+14                                                   
         XC    SVP1USER(SVULNQ),SVP1USER                                        
         MVC   SVP1USER(SVULNQ),CPU1  MOVE ALL INFO TO SAVED STORAGE            
         BR    R9                                                               
         DROP  R8                                                               
         SPACE 2                                                                
RDPRD    XC    KEY,KEY                                                          
         MVC   KEY+1(1),SVAGYMD                                                 
         MVC   KEY+2(2),SVCLT                                                   
         MVC   KEY+4(3),SVEBCPRD                                                
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         MVI   ERRCD,NOPRDERR                                                   
         CLC   KEY(13),KEYSAVE                                                  
         BNE   KEYERR                                                           
*                                                                               
         LA    R8,REC2                                                          
         ST    R8,AREC                                                          
         GOTO1 GETREC                                                           
*                                                                               
         MVC   SVPRDDA,KEY+14                                                   
         BR    R9                                                               
         EJECT                                                                  
RDNWK    XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D11'                                                  
         MVC   KEY+2(2),AGYALPHA                                                
         MVC   KEY+4(4),DUB           DUB STILL HAS NETWORK                     
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   NOTONF                                                           
         MVC   SVPRDDA,KEY+14        SAVE IN PRDDA                              
         BR    R9                                                               
         SPACE 2                                                                
RDPGM    XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D12'                                                  
         MVC   KEY+2(2),AGYALPHA                                                
         MVC   KEY+4(4),FULL       NWK                                          
         MVC   KEY+8(4),SVKEY+7    PGM                                          
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   NOTONF                                                           
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         GOTO1 GETREC                                                           
         BR    R9                                                               
         SPACE 2                                                                
RDDEMD   XC    KEY,KEY                                                          
         MVC   KEY(13),SVKEY                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   NOTONF                                                           
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         GOTO1 GETREC                                                           
         BR    R9                                                               
         SPACE 2                                                                
READSTA  MVC   KEY(17),ZEROS                                                    
READSTA2 MVI   KEY,C'S'                                                         
         MVC   KEY+1(1),SVEBCMED                                                
         MVC   KEY+2(5),SVSTA                                                   
         MVC   KEY+7(2),AGYALPHA                                                
         MVC   COMMAND,=C'DMRDHI'                                               
         GOTO1 STA                                                              
         CLC   REC(15),KEY                                                      
         BER   R9                  FOUND                                        
NFNDERR  MVI   ERRCD,NOSTAERR      STATION NOT ON FILE                          
         B     KEYERR                                                           
*                                                                               
NOTONF   MVI   ERRCD,NOFNDERR      RECORD NOT FOUND                             
         B     KEYERR                                                           
         EJECT                                                                  
* CHECK FOR ADVTSR AGENCY THIS CLT                                              
CHKADV   CLI   SVEBCMED,C'T'       TEST SPOT TV                                 
         BNER  R9                                                               
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,5                                                            
         MVC   KEY+1(2),SVCLT                                                   
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(3),KEYSAVE                                                   
         BNE   CHKADVX                                                          
*                                                                               
         LA    R8,REC2                                                          
         ST    R8,AREC                                                          
         GOTO1 GETREC                                                           
*                                                                               
         USING ADVHDRD,R8                                                       
         SR    R0,R0                                                            
         IC    R0,SVAGYMD                                                       
         SRL   R0,4                                                             
         STC   R0,BYTE             ISOLATE AGENCY CODE                          
*                                                                               
         PACK  DUB,ADVAGY                                                       
         CVB   R0,DUB                                                           
         STC   R0,BYTE2            GET ADVTSR AGENCY CODE                       
         CLC   BYTE,BYTE2          IS THIS OUR AGENCY                           
         BE    CHKADV4                                                          
*                                                                               
         LA    R1,ADVLIST                                                       
CHKADV2  CLI   0(R1),0                                                          
         BE    CHKADVX                                                          
         CLC   BYTE,0(R1)                                                       
         BE    CHKADV4                                                          
         LA    R1,1(R1)                                                         
         B     CHKADV2                                                          
CHKADV4  PACK  DUB,ADVAGY                                                       
         CVB   R0,DUB                                                           
         SLL   R0,4                LEFT ALIGN                                   
         STC   R0,SVADVAGY                                                      
         MVC   SVADVDA,KEY+14         SAVE DISK ADDR                            
*                                                                               
CHKADVX  BR    R9                                                               
         EJECT                                                                  
* HALF+1(1) HAS NUMBER OF DIGITS INPUT FOR GROUP                                
*                                                                               
RDPGRDEF NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D01'                                                  
         MVC   KEY+2(4),SVKEY+2    A-M/CLT/PGRPID                               
         B     RDALLDEF                                                         
*                                                                               
RDMGRDEF NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D02'                                                  
         MVC   KEY+2(1),SVKEY+2    A-M                                          
         CLI   SVKEY+8,C'F'                                                     
         BH    *+10                                                             
         MVC   KEY+3(2),SVKEY+3    CLT                                          
         MVC   KEY+8(1),SVKEY+8    MGRPID                                       
*                                                                               
RDALLDEF GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BNE   RDALLX                                                           
*                                                                               
         LA    R8,REC2                                                          
         ST    R8,AREC                                                          
         GOTO1 GETREC                                                           
*                                                                               
         USING PRGRECD,R8          NB- '01' ELEM DSECTS ARE IDENTICAL           
         LA    R6,PRGEL                                                         
         USING PRGEL01,R6                                                       
*                                                                               
         LA    RF,SVBKLNS          SAVE AREA FOR PGRDEF BREAK LENGTHS           
         CLI   KEY+1,1                                                          
         BE    *+8                                                              
         LA    RF,SVBKLNS+3        SAVE AREA FOR MGRDEF BREAK LENGTHS           
         EJECT                                                                  
* SAVE BREAK LENGTHS                                                            
         MVC   0(1,RF),PRGBK1LN                                                 
         MVC   1(1,RF),PRGBK2LN                                                 
         MVC   2(1,RF),PRGBK3LN                                                 
*                                                                               
         SR    RE,RE                                                            
         IC    RE,0(RF)                                                         
         SR    R0,R0                                                            
         IC    R0,1(RF)                                                         
         AR    RE,R0                                                            
         IC    R0,2(RF)                                                         
         AR    RE,R0                                                            
         STC   RE,HALF                                                          
         CLC   HALF(1),HALF+1                                                   
*                                                                               
RDALLX   XIT1                      EXIT WITH CC SET                             
         EJECT                                                                  
* FOR CLIENTS DOING CODE COORDINATION CODE MUST BE ON CTFILE                    
* GO FIND OUT                                                                   
         SPACE 1                                                                
CHKCTCLT NTR1                                                                   
         XC    WORK,WORK                                                        
         LA    R4,WORK                                                          
         USING ZENRECD,R4                                                       
         MVI   ZENKCODE,ZENKCODQ                                                
         MVI   ZENKTYP,ZENCLTQ                                                  
         MVC   ZENKAGY,SVCTAGY                                                  
         MVC   ZENKCLT,SVEBCCLT                                                 
         BAS   R9,CHKCT                                                         
* POSITION TO FIRST UNP FIELD (CLIENT NAME, HOPEFULLY)                          
         BAS   R9,NEXTUF                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R4,REC2             POINT TO RECORD                              
         MVI   5(R2),20            FORCE LENGTH                                 
         OI    6(R2),X'80'         FORCE XMT                                    
         LA    R4,ZENFIRST                                                      
         USING ZENELEM,R4                                                       
         MVC   8(20,R2),ZENCNAME   MOVE ZENITH NAME                             
         B     CHKX                                                             
         DROP  R4                                                               
*                                                                               
CHKCTREP NTR1                                                                   
         XC    WORK,WORK                                                        
         LA    R4,WORK                                                          
         USING ZENRECD,R4                                                       
         MVI   ZENKCODE,ZENKCODQ                                                
         MVI   ZENKTYP,ZENREPQ                                                  
         MVI   ZENKSYS,C'S'        SPOT                                         
         CLI   SVEBCMED,C'N'                                                    
         BNE   *+8                                                              
         MVI   ZENKSYS,C'N'                                                     
         MVC   ZENKMED,SVEBCMED                                                 
         MVC   ZENKAGY,SVCTAGY                                                  
         MVC   ZENKREP,SVKEY+2                                                  
         BAS   R9,CHKCT                                                         
*                                                                               
         LA    R4,REC2             POINT TO RECORD                              
         LA    R4,ZENFIRST                                                      
         USING ZENELEM,R4                                                       
         BAS   R9,NEXTUF                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    6(R2),X'80'                                                      
         OI    1(R2),X'01'         SET MODIFIED FLAG                            
         MVC   8(22,R2),ZENREPNM                                                
         BAS   R9,NEXTUF                                                        
         OI    6(R2),X'80'                                                      
         MVC   8(24,R2),ZENREPAD                                                
         BAS   R9,NEXTUF                                                        
         OI    6(R2),X'80'                                                      
         MVC   8(24,R2),ZENRADR2                                                
         BAS   R9,NEXTUF                                                        
         OI    6(R2),X'80'                                                      
         MVC   8(3,R2),ZENRADR3                                                 
         BAS   R9,NEXTUF                                                        
         OI    6(R2),X'80'                                                      
         MVC   8(5,R2),ZENRADR4                                                 
*                                                                               
CHKX     XIT1                                                                   
         DROP  R4                                                               
*                                                                               
CHKCT    CLI   SVACT,C'A'          TEST ACTION = ADD                            
         BNE   CHKX                NO - FORGET IT                               
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',WORK,REC2                    
         CLC   0(25,R4),REC2                                                    
         BER   R9                                                               
         MVI   ERRCD,NEWERR                                                     
         MVC   NERRCD,=AL2(NOCTCODE)                                            
         B     KEYERR                                                           
*                                                                               
FNDUF    TM    1(R2),X'20'         TEST PROTECTED                               
         BCR   8,R9                NO = EXIT WITH CC EQ                         
NEXTUF   SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNE   FNDUF                                                            
         LTR   R9,R9               SET CC NEQ                                   
         BR    R9                                                               
         EJECT                                                                  
* VALIDATE FIELD FOR ALPHA/NUMERIC.                                             
* FIELDS ARE DELIMITED BY BLANK, COMMA, OR DASH.                                
*  FADDR  HAS PREVIOUS FIELD ADDRESS.                                           
*  FLEN   HAS PREVIOUS FIELD LENGTH.                                            
*  FVAL   HAS VALIDITY BITS (X'04'=VALID ALPHA,X'08'=VALID NUMERIC)             
*                                                                               
* ON EXIT, R4 HAS FIELD ADDRESS, R5 HAS FIELD LENGTH                            
FLDVAL   DS    0H                                                               
         MVI   FVAL,X'0C'          SET ALL VALID                                
         L     R4,FADDR                                                         
         LH    R5,FLEN                                                          
         LTR   R5,R5                                                            
         BZ    *+8                                                              
         LA    R5,1(R5)                                                         
         AR    R4,R5                                                            
         ST    R4,FADDR                                                         
         SR    R0,R0               TEST STILL IN FIELD                          
         IC    R0,0(R2)                                                         
         AR    R0,R2               R0 HAS FLDHDR END+1                          
         CR    R4,R0                                                            
         BNL   BADKEY                                                           
FLDVAL2  CLI   0(R4),C' '                                                       
         BE    FLDVALX                                                          
         CLI   0(R4),0                                                          
         BE    FLDVALX                                                          
         CLI   0(R4),C','                                                       
         BE    FLDVALX                                                          
         CLI   0(R4),C'-'                                                       
         BE    FLDVALX                                                          
         CLI   0(R4),C'A'                                                       
         BL    FLDVAL4                                                          
         CLI   0(R4),C'Z'                                                       
         BNH   FLDVAL6                                                          
FLDVAL4  NI    FVAL,X'FB'          FIELD NOT ALPHA                              
         CLI   0(R4),C'0'                                                       
         BL    FLDVAL6                                                          
         CLI   0(R4),C'9'                                                       
         BNH   FLDVAL8                                                          
FLDVAL6  NI    FVAL,X'F7'          FIELD NOT NUMERIC                            
*                                                                               
FLDVAL8  LA    R4,1(R4)                                                         
         CR    R4,R0               TEST STILL IN FIELD                          
         BE    FLDVALX                                                          
         BNL   BADKEY                                                           
         B     FLDVAL2                                                          
*                                                                               
FLDVALX  LR    R5,R4                                                            
         S     R5,FADDR            END-START GIVES LENGTH                       
         STH   R5,FLEN                                                          
         L     R4,FADDR            POINT TO START                               
         BR    RE                                                               
         EJECT                                                                  
USESFM   DS    0H                                                               
         XC    LFMMSG,LFMMSG                                                    
         MVC   LFMMSG(32),=C'** ERROR * USE $SFM FOR CABLE **'                  
         FOUT  LFMMSGH                                                          
         MVI   ERRAREA,X'FF'                                                    
         OI    6(R2),X'40'         POSITION CURSOR                              
         B     EXXMOD                                                           
*                                                                               
BADKEY   DS    0H                                                               
         MVC   LFMMSG(35),=C'** ERROR * KEY FIELD 9 NOT VALID **'               
         OI    ERRCD,X'F0'                                                      
         MVC   LFMMSG+21(1),ERRCD                                               
BADK5    MVC   ERRAREA,ERRCD       SET ERROR FLAG                               
*                                                                               
         OI    6(R2),X'40'         POSITION CURSOR                              
         B     EXXMOD                                                           
*                                                                               
PRDESTCC MVC   NERRCD,=AL2(ADPRESCC)                                            
         MVI   ERRCD,NEWERR                                                     
KEYERR   GOTO1 ERROR                                                            
*                                                                               
ERRINV   MVI   ERRCD,INVERR                                                     
         GOTO1 ERROR                                                            
*                                                                               
ZEROS    DC    20C'0'                                                           
*                                                                               
DOWTAB   DC    C'MON',X'01'        DAY OF WEEK TABLE                            
         DC    C'TUE',X'02'                                                     
         DC    C'WED',X'03'                                                     
         DC    C'THU',X'04'                                                     
         DC    C'FRI',X'05'                                                     
         DC    C'SAT',X'06'                                                     
         DC    C'SUN',X'07'                                                     
         SPACE 1                                                                
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
         CNOP  2,4                                                              
*                                                                               
KEYTAB   DC    X'11',AL3(CLT)      CLT                                          
         DC    X'12',AL3(CLT)      PRD                                          
         DC    X'13',AL3(CLT)      EST                                          
         DC    X'19',AL3(COM)      COMMENT (FOR MEDIA COMMENT)                  
         DC    X'1A',AL3(CLT)      POL SUB-EST                                  
         DC    X'1B',AL3(CLT)      BRD SUB-EST                                  
         DC    X'1C',AL3(COM)      COMMENT (FOR NVTEXT)                         
         DC    X'1D',AL3(COM)      COMMENT                                      
         DC    X'1F',AL3(CLT)      STATION DESCRIPTOR                           
*                                                                               
         DC    X'1E',AL3(FLIGHT)   USERDEM                                      
         DC    X'21',AL3(MAS)      MAS                                          
         DC    X'22',AL3(ADDR)     ADDR                                         
         DC    X'23',AL3(REP)      REP                                          
         DC    X'24',AL3(MKT)      MKT                                          
         DC    X'25',AL3(SPREP)                                                 
         DC    X'26',AL3(MENU)     DEMO MENU                                    
         DC    X'27',AL3(FLIGHT)   CHILD SPOT FLIGHTS                           
         DC    X'28',AL3(CLT)      DPT MENU SPLIT                               
         DC    X'29',AL3(NUNI)                                                  
         DC    X'2A',AL3(NPGM)     NTWK PROGRAM                                 
         DC    X'2B',AL3(DEM)      DEMO OVERRIDE DEF                            
         DC    X'2C',AL3(DEM)      DEMO OVERRIDE VALUE                          
         DC    X'2D',AL3(NWK)      NETWORK                                      
         DC    X'2E',AL3(PGM)      PROGRAM ID                                   
         DC    X'2F',AL3(SPIL)     SPILL DEF REC                                
         DC    X'30',AL3(XSPIL)    XSPILL DEF REC                               
*                                                                               
         DC    X'31',AL3(CLT)      CL2                                          
*        DC    X'31',AL3(ADV)      ADV                                          
         DC    X'32',AL3(AGY)      AGY                                          
         DC    X'33',AL3(RPT)      PROF                                         
         DC    X'34',AL3(DPT)      DPT                                          
         DC    X'35',AL3(EQU)      EQU                                          
         DC    X'36',AL3(CLT)      TALFAC                                       
         DC    X'38',AL3(RATS)     ESTIMATED BOOK DEMOS                         
*                                                                               
         DC    X'14',AL3(PGRP)     PGRDEF                                       
         DC    X'15',AL3(PGRP)     PGROUP                                       
         DC    X'16',AL3(MGRP)     MGRDEF                                       
         DC    X'17',AL3(MGRP)     MGROUP                                       
         DC    X'18',AL3(MGRP)     PMGR (MGRP BY PRD FOR CSO)                   
*                                                                               
         DC    X'41',AL3(NUNI)     NTWK NEW UNIV                                
         DC    X'42',AL3(NPGM)     NTWK NEW PROGRAM                             
         DC    X'43',AL3(NHUT)     NTWK NEW HUT                                 
         DC    X'44',AL3(NHUT)     NTWK NEW HUT- MONTH DISPLAY                  
         DC    X'45',AL3(NHHUT)    NTWK NEW HOLI-HUT                            
*                                                                               
         DC    X'46',AL3(CLT)      PROD EXCLUSION RECS                          
         DC    X'47',AL3(CLT)      SHARE RECS                                   
         DC    X'48',AL3(CLT)      PGEST RECS                                   
         DC    X'53',AL3(CLT)      ESTDOLS                                      
         DC    X'54',AL3(CLT)      CGRP DIS                                     
KEYTABX  EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE SPGENPGEST                                                     
         EJECT                                                                  
       ++INCLUDE SPLFMWRK                                                       
         EJECT                                                                  
         ORG   LFMTABH                                                          
       ++INCLUDE SPLFMF1D          MUST COME AFTER SPLFMWRK                     
         EJECT                                                                  
         ORG   LFMTABH                                                          
       ++INCLUDE SPLFMF2D          MUST COME AFTER SPLFMWRK                     
         EJECT                                                                  
         ORG   LFMTABH                                                          
       ++INCLUDE SPLFMF3D          MUST COME AFTER SPLFMWRK                     
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
* SPGENCLT                                                                      
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
* SPGENEST                                                                      
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
* SPGENPRD                                                                      
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
* SPGENADV                                                                      
ADVHDRD  DSECT                                                                  
       ++INCLUDE SPGENADV                                                       
         EJECT                                                                  
* SPGENPRG                                                                      
       ++INCLUDE SPGENPRG                                                       
         EJECT                                                                  
* SPGENMKG                                                                      
       ++INCLUDE SPGENMKG                                                       
         EJECT                                                                  
* SPGENREP                                                                      
SPREPD   DSECT                                                                  
       ++INCLUDE SPGENREP                                                       
         EJECT                                                                  
*        DEMO OVERRIDE RECORD                                                   
       ++INCLUDE SPGENNDOV                                                      
         EJECT                                                                  
*        NETWORK DEFINITION RECORD                                              
       ++INCLUDE SPGENNDEF                                                      
         EJECT                                                                  
       ++INCLUDE SPGENNPGM                                                      
         EJECT                                                                  
       ++INCLUDE SPGENTAL                                                       
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
       ++INCLUDE DDOFFICED                                                      
         EJECT                                                                  
       ++INCLUDE CTGENZEN                                                       
 END                                                                            
