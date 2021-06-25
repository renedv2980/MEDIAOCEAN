*          DATA SET PEXCT00    AT LEVEL 002 AS OF 03/07/08                      
*PHASE TE0700A                                                                  
         TITLE 'PEXCT00 - TRANSFER CONTROL PROGRAM'                             
PEXCT    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL WORKX-WORKD,**TST**,RA,RR=R4,CLEAR=YES                           
         USING WORKD,RC                                                         
         ST    R4,RELO                                                          
         MVC   SVPARMS,0(R1)       SAVE S/R PARM LIST                           
         L     R8,ACOMFACS                                                      
         USING COMFACSD,R8         R8=A(COMFACS)                                
         L     R9,ATWA                                                          
         USING PEXCTFFD,R9         R9=A(TWA)                                    
*                                                                               
INIT1    LA    R7,2048(R9)         R7=A(TWA SAVE AREA)                          
         USING SAVED,R7                                                         
         SR    R1,R1               TEST FIRST TIME IN                           
         ICM   R1,3,16(R9)                                                      
         BNZ   INIT1A                                                           
         MVC   FRSTINL,=C'*FRSTIN*'                                             
         MVC   FRSTOUL,=C'*FRSTOU*'                                             
         MVC   THISINL,=C'*THISIN*'                                             
         MVC   THISOUL,=C'*THISOU*'                                             
         MVC   LASTINL,=C'*LASTIN*'                                             
         MVC   LASTOUL,=C'*LASTOU*'                                             
INIT1A   LA    R1,1(R1)                                                         
         STH   R1,16(R9)           BUMP SEQUENCE NUMBER                         
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  XCTSEQ+34(4),DUB    DISPLAY SEQUENCE NUMBER                      
         OI    XCTSEQH+6,X'80'                                                  
*                                                                               
INIT2    LA    R1,XCTACTH          SET INITIAL CURSOR POS                       
         ST    R1,CURSOR                                                        
         OI    XCTACTH+6,X'80'     RETRANSMIT ACT AND OPT FIELDS                
         OI    XCTOPTH+6,X'80'                                                  
*                                                                               
INIT3    MVC   XCTLIN-8(8),=C'XCTLIN  '                                         
         MVC   XCTLOU-8(8),=C'XCTLOUT '                                         
*                                                                               
INIT4    MVC   AGLOBBER,CGLOBBER   EXTRACT GLOBBER ADDRESS                      
         MVI   REASON,0                                                         
*                                                                               
SPEC1    CLC   =C'LOOP',XCTACT     TEST TIME BOUND TRANSACTION                  
         BNE   SPEC1X                                                           
         LHI   R0,F#TBTST                                                       
         LHI   RF,20               20 SECS                                      
         ST    RF,FULL                                                          
         GOTO1 CGETFACT,DMCB,(X'80',FULL),(R0)                                  
         B     *                   INDUCE LOOP                                  
SPEC1X   EQU   *                                                                
*                                                                               
SPEC2    CLC   =C'FALINK',XCTACT   SET SIMULATE FALINK SCREEN                   
         BNE   SPEC2X                                                           
         MVI   18(R9),C'F'                                                      
SPEC2X   EQU   *                                                                
*                                                                               
SPEC3    CLC   =C'FALINX',XCTACT   UNSET FALINK SIMULATION                      
         BNE   SPEC3X                                                           
         MVI   18(R9),0                                                         
SPEC3X   EQU   *                                                                
*                                                                               
SPEC4    CLC   =C'TRSRV',XCTACT    RESERVE SOME TEMPEST                         
         BNE   SPEC4X                                                           
         LA    RF,1                                                             
         GOTO1 CDATAMGR,DMCB,=C'DMRSRV',=C'TEMPEST',((RF),0),0                  
         LHI   R0,F#XTGET                                                       
         GOTO1 CGETFACT,PARM,(X'80',TEMPINF),(R0)                               
         LA    R1,=CL60'TEMPEST XXXXX=                 DMCB+8=XX'               
         MVC   XCTHDR,0(R1)                                                     
         MVC   XCTHDR+8(5),=C'ALLOC'                                            
         GOTO1 CHEXOUT,PARM,TEMPINF,XCTHDR+14,07,=C'TOG'                        
         GOTO1 CHEXOUT,PARM,DMCB+08,XCTHDR+38,01,=C'TOG'                        
         OI    XCTHDRH+6,X'80'                                                  
         B     EXIT                                                             
SPEC4X   EQU   *                                                                
*                                                                               
SPEC5    CLC   =C'TRLSE',XCTACT    RELEASE SOME TEMPEST                         
         BNE   SPEC5X                                                           
         LA    RF,1                                                             
         GOTO1 CDATAMGR,DMCB,=C'DMRLSE',=C'TEMPEST',((RF),0),0                  
         LHI   R0,F#XTGET                                                       
         GOTO1 CGETFACT,PARM,(X'80',TEMPINF),(R0)                               
         LA    R1,=CL60'TEMPEST XXXXX=                 DMCB+8=XX'               
         MVC   XCTHDR,0(R1)                                                     
         MVC   XCTHDR+8(5),=C'ALLOC'                                            
         GOTO1 CHEXOUT,PARM,TEMPINF,XCTHDR+14,07,=C'TOG'                        
         GOTO1 CHEXOUT,PARM,DMCB+08,XCTHDR+38,01,=C'TOG'                        
         OI    XCTHDRH+6,X'80'                                                  
         B     EXIT                                                             
SPEC5X   EQU   *                                                                
*                                                                               
SPEC6    CLC   =C'TREAD',XCTACT    READ A TEMPEST RECORD                        
         BNE   SPEC6X                                                           
         L     RF,ATIA                                                          
         GOTO1 CDATAMGR,DMCB,=C'DMREAD',=C'TEMPEST',(1,0),(RF)                  
         L     RF,ATIA                                                          
         LA    R1,=CL60'TEMPEST XXXXX=                 DMCB+8=XX'               
         MVC   XCTHDR,0(R1)                                                     
         MVC   XCTHDR+8(5),=C'READ '                                            
         MVC   XCTHDR+14(12),0(RF)                                              
         GOTO1 CHEXOUT,PARM,DMCB+08,XCTHDR+38,01,=C'TOG'                        
         OI    XCTHDRH+6,X'80'                                                  
         B     EXIT                                                             
SPEC6X   EQU   *                                                                
*                                                                               
SPEC7    CLC   =C'TWRIT',XCTACT    WRITE A TEMPEST RECORD                       
         BNE   SPEC7X                                                           
         L     RF,ATIA                                                          
         XC    0(256,RF),0(RF)                                                  
         MVC   0(8,RF),=C'PER/XCT '                                             
         MVC   8(4,RF),XCTSEQ+34                                                
         GOTO1 CDATAMGR,DMCB,=C'DMWRT',=C'TEMPEST',(1,0),(RF)                   
         L     RF,ATIA                                                          
         LA    R1,=CL60'TEMPEST XXXXX=                 DMCB+8=XX'               
         MVC   XCTHDR,0(R1)                                                     
         MVC   XCTHDR+8(5),=C'WRITE'                                            
         MVC   XCTHDR+14(12),0(RF)                                              
         GOTO1 CHEXOUT,PARM,DMCB+08,XCTHDR+38,01,=C'TOG'                        
         OI    XCTHDRH+6,X'80'                                                  
         B     EXIT                                                             
SPEC7X   EQU   *                                                                
*                                                                               
SPEC8    CLC   =C'TALLOC',XCTACT   TEST TEMPEST ALLOCATION                      
         BNE   SPEC8X                                                           
         MVI   DMCB+8,0                                                         
         LHI   R0,F#XTGET                                                       
         GOTO1 CGETFACT,PARM,(X'80',TEMPINF),(R0)                               
         LA    R1,=CL60'TEMPEST XXXXX=                 DMCB+8=XX'               
         MVC   XCTHDR,0(R1)                                                     
         MVC   XCTHDR+8(5),=C'ALLOC'                                            
         GOTO1 CHEXOUT,PARM,TEMPINF,XCTHDR+14,07,=C'TOG'                        
         GOTO1 CHEXOUT,PARM,DMCB+08,XCTHDR+38,01,=C'TOG'                        
         OI    XCTHDRH+6,X'80'                                                  
         B     EXIT                                                             
SPEC8X   EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
*TEST IF WE HAVE BEEN CALLED VIA XCTL GLOBAL                          *         
***********************************************************************         
         SPACE 1                                                                
WHY      GOTO1 AGLOBBER,DMCB,=C'GETD',XCTL+2,22,04                              
         TM    DMCB+8,X'10'                                                     
         BO    WHYX                                                             
         GOTO1 AGLOBBER,DMCB,=C'DELE' WE MUST DELETE THE XCTL ELEMENT           
*                                                                               
         BAS   RE,RESHIP           MUST ERASE AND RESHIP ALL FIELDS             
*                                                                               
         LA    R2,XCTL             SAVE INPUT XCTL IN W/S                       
         USING GLVXCTLD,R2                                                      
         MVI   GLVXCODE,4                                                       
         MVI   GLVXLEN,24                                                       
         MVC   XCTLIN,XCTL                                                      
         OC    FRSTIN,FRSTIN                                                    
         BNZ   *+10                                                             
         MVC   FRSTIN,XCTLIN       SAVE FRST INPUT XCTL IN TWA                  
         OC    THISIN,THISIN                                                    
         BZ    *+10                                                             
         MVC   LASTIN,THISIN       SAVE LAST INPUT XCTL IN TWA                  
         MVC   THISIN,XCTLIN       SAVE THIS INPUT XCTL IN TWA                  
*                                                                               
WHY1     CLC   GLVXFRSY,MYSYS      TEST WHAT SORT OF XFR                        
         BNE   WHY1B                                                            
         CLC   GLVXFRPR,MYPRG                                                   
         BNE   WHY1B                                                            
WHY1A    MVI   REASON,1            THIS IS MY ORIGINAL XCTL ELEMENT             
         B     WHY2                                                             
WHY1B    CLC   GLVXTOSY,MYSYS                                                   
         BNE   WHY1D                                                            
         CLC   GLVXTOPR,MYPRG                                                   
         BNE   WHY1D                                                            
WHY1C    MVI   REASON,2            THIS WAS CREATED BY MY PARTNER               
         TM    GLVXFLG1,GLV1RETN   TEST IF RETURN CALL                          
         BZ    WHY2                                                             
         MVI   REASON,3            THIS IS MY PARTNER RETURNING TO ME           
         B     WHY2                                                             
WHY1D    DC    H'0'                DONT KNOW WHAT THE SUCKER IS                 
*                                                                               
WHY2     GOTO1 AGLOBBER,DMCB,=C'GETD',MSG,20,50                                 
         GOTO1 (RF),(R1),=C'DELE'                                               
*                                                                               
WHY3     MVC   XCTCLR(7),=C'XXX/XXX'                                            
         MVC   XCTCLR+0(3),GLVXFRSY                                             
         MVC   XCTCLR+4(3),GLVXFRPR                                             
         OI    XCTCLRH+6,X'80'                                                  
         MVC   XCTMSG(20),MSG      DISPLAY MESSAGED PASSED                      
         OI    XCTMSGH+6,X'80'                                                  
         CLI   REASON,1            TEST IF MY ORIGINAL XCTL EMEMENT             
         BNE   *+12                                                             
         BAS   RE,DUMMY                                                         
         B     OK1                                                              
         CLI   REASON,3            TEST IF RETURN CALL FROM PARTNER             
         BNE   *+12                                                             
         BAS   RE,DUMMY                                                         
         B     OK3                                                              
*                                                                               
WHY4     CLC   MSG(4),=C'TIME'     TEST IF REQUEST FOR TIME                     
         BNE   WHY5                                                             
         THMS                      GET TIME                                     
         ST    R1,FULL                                                          
         OI    FULL+3,X'0F'                                                     
         UNPK  DUB,FULL                                                         
         MVC   MSG(20),=C'TIME IS HHMMSS #NNN '                                 
         MVC   MSG+8(6),DUB+2                                                   
         LH    R1,16(R9)           GET SEQUENCE NUMBER                          
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MSG+16(3),DUB                                                    
         GOTO1 AGLOBBER,DMCB,=C'PUTD',MSG,20,50                                 
         OI    FLAGS,X'01'                                                      
         B     WHYR                                                             
*                                                                               
WHY5     CLC   MSG(4),=C'UPDN'     TEST IF UPDATE=N OR UPDATE=Y                 
         BE    WHY5B                                                            
         CLC   MSG(4),=C'UPDY'                                                  
         BNE   WHY6                                                             
WHY5A    LHI   R0,F#UPDY                                                        
         MVC   MSG(20),=C'UPDATE=YES SET #NNN '                                 
         B     WHY5C                                                            
WHY5B    LHI   R0,F#UPDN                                                        
         MVC   MSG(20),=C'UPDATE=NO SET  #NNN '                                 
WHY5C    GOTO1 CGETFACT,DMCB,(X'80',0),(R0)                                     
         LH    R1,16(R9)           GET SEQUENCE NUMBER                          
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MSG+16(3),DUB                                                    
         GOTO1 AGLOBBER,DMCB,=C'PUTD',MSG,20,50                                 
         OI    FLAGS,X'01'                                                      
         B     WHYR                                                             
*                                                                               
WHY6     CLC   MSG(7),=C'TEMPEST'  TEMPEST=XXXXXX PASSED                        
         BNE   WHY7                                                             
         OC    TEMPINF+1(2),TEMPINF+1  TEST IF I HAVE TEMPEST                   
         BZ    WHY6A                                                            
         TM    TEMPINF,X'80'       TEST IF ITS NOT REALLY MINE                  
         BO    WHY6A                                                            
         MVC   MSG,=CL20'I HAVE SOME ALREADY'                                   
         B     WHY6R                                                            
WHY6A    OC    MSG+9(2),MSG+9      TEST NUMBER OF TRACKS ALLOCATED              
         BNZ   WHY6B                                                            
         MVC   MSG,=CL20'YOU DONT HAVE ANY'                                     
         B     WHY6R                                                            
WHY6B    LHI   R0,F#XTSET                                                       
         GOTO1 CGETFACT,PARM,(X'80',MSG+8),(R0)                                 
         L     RF,ATIA                                                          
         GOTO1 CDATAMGR,DMCB,=C'DMREAD',=C'TEMPEST',(1,0),(RF)                  
         MVC   DUB+0(1),DMCB+8                                                  
         L     RF,ATIA                                                          
         MVI   3(RF),C'\'                                                       
         GOTO1 CDATAMGR,DMCB,=C'DMWRT',=C'TEMPEST',(1,0),(RF)                   
         MVC   DUB+1(1),DMCB+8                                                  
         L     RF,ATIA                                                          
         MVC   MSG,SPACES                                                       
         MVC   MSG(12),0(RF)                                                    
         GOTO1 CHEXOUT,PARM,DUB,MSG+14,02,=C'TOG'                               
WHY6R    GOTO1 AGLOBBER,DMCB,=C'PUTD',MSG,20,50                                 
         OI    FLAGS,X'01'                                                      
         B     WHYR                                                             
*                                                                               
WHY7     CLC   MSG(4),=C'EXIT'     TEST IF EXIT REQUIRED                        
         BNE   WHY8                                                             
         BAS   RE,DUMMY                                                         
         B     OK2                                                              
*                                                                               
WHY8     DS    0H                                                               
*                                                                               
WHYR     LA    R2,XCTLOU           BUILD RETURN                                 
         USING GLVXCTLD,R2                                                      
         MVI   GLVXCODE,4                                                       
         MVI   GLVXLEN,24                                                       
         MVC   GLVXTOSY,XCTLIN+GLVXFRSY-GLVXCTLD                                
         MVC   GLVXTOPR,XCTLIN+GLVXFRPR-GLVXCTLD                                
         MVC   GLVXFRSY,MYSYS                                                   
         MVC   GLVXFRPR,MYPRG                                                   
         OI    GLVXFLG1,GLV1RETN   SET RETURN CALL                              
         TM    FLAGS,X'01'                                                      
         BZ    *+8                                                              
         OI    GLVXFLG1,GLV1RETG   SET RETURN GLOBALS                           
         TM    XCTLIN+GLVXFLG1-GLVXCTLD,GLV1SEPS                                
         BZ    *+8                                                              
         OI    GLVXFLG1,GLV1SEPS   SET SEPERATE SESSION                         
         GOTO1 AGLOBBER,DMCB,=C'PUTD',XCTLOU+2,22,04                            
         B     OK2                                                              
*                                                                               
WHYX     EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
*WE ARE GOING TO CALL OUR PARTNER                                     *         
***********************************************************************         
HELP     CLI   XCTACT,C'?'         TEST HELP                                    
         BE    HELPACT                                                          
         CLI   XCTOPT,C'?'                                                      
         BE    HELPOPT                                                          
         SPACE 1                                                                
CALL     CLI   XCTACTH+5,0         TEST ACTION FIELD                            
         BE    ERR1                                                             
         CLC   XCTACT(4),=C'TEST'  JUST DO SOME STUFF                           
         BE    OK0                                                              
         CLC   XCTACT(4),=C'SAME'  CALLEE IN SAME SESSION AS US                 
         BE    CALL1                                                            
         CLC   XCTACT(3),=C'SEPS'  CALLEE IN SEPERATED SESSION                  
         BE    CALL1                                                            
         CLC   XCTACT(4),=C'CALL'  CALL IS ONTHER WORD FOR SAME                 
         BE    CALL1                                                            
         CLC   XCTACT(4),=C'RETN'  RETURN TO ORIGINAL CALLER                    
         BNE   ERR2                                                             
         OC    FRSTIN,FRSTIN       ONLY VALID IF WE WERE CALLED                 
         BZ    ERR3                .... FROM A SEPERATE SESSION                 
         TM    FRSTIN+GLVXFLG1-GLVXCTLD,GLV1SEPS                                
         BZ    ERR3                                                             
         B     RETN                                                             
*                                                                               
CALL1    LA    R2,XCTL             BUILD XCTL ELEMENT                           
         USING GLVXCTLD,R2                                                      
         MVI   GLVXCODE,4                                                       
         MVI   GLVXLEN,24                                                       
         MVC   GLVXTOSY,HISSYS                                                  
         MVC   GLVXTOPR,HISPRG                                                  
         MVC   GLVXFRSY,MYSYS                                                   
         MVC   GLVXFRPR,MYPRG                                                   
         MVI   GLVXFLG1,0                                                       
*                                                                               
         CLC   XCTACT(3),=C'SEPS'                                               
         BNE   CALL1X                                                           
         OI    GLVXFLG1,GLV1SEPS   SET SEPERATE SESSION                         
         CLC   XCTACT+4(4),=C',DIALOGUE'                                        
         BNE   *+8                                                              
         OI    GLVXFLG1,GLV1SEPD   SET DIALOGUE MODE                            
         CLC   XCTACT+4(4),=C',SINGLE'                                          
         BNE   *+8                                                              
         OI    GLVXFLG1,GLV1SNGL   SET SINGLESHOT MODE (EXTRA SESS)             
         CLI   XCTACT+3,C'S'                                                    
         BE    CALL1X              ANY SEPERATE SESSION                         
*                                                                               
CALL1A   CLI   XCTACT+3,C'A'       NOMINATE SESSION A                           
         BNE   CALL1B                                                           
         OI    GLVXFLG1,GLV1SIDE                                                
         MVI   GLVXSESE,0                                                       
         B     CALL1X                                                           
CALL1B   CLI   XCTACT+3,C'B'       NOMINATE SESSION B                           
         BNE   CALL1C                                                           
         OI    GLVXFLG1,GLV1SIDE                                                
         MVI   GLVXSESE,1                                                       
         B     CALL1X                                                           
CALL1C   CLI   XCTACT+3,C'C'       NOMINATE SESSION C                           
         BNE   CALL1D                                                           
         OI    GLVXFLG1,GLV1SIDE                                                
         MVI   GLVXSESE,2                                                       
         B     CALL1X                                                           
CALL1D   CLI   XCTACT+3,C'D'       NOMINATE SESSION D                           
         BNE   CALL1E                                                           
         OI    GLVXFLG1,GLV1SIDE                                                
         MVI   GLVXSESE,3                                                       
         B     CALL1X                                                           
CALL1E   CLI   XCTACT+3,C'E'       NOMINATE SESSION E                           
         BNE   CALL1F                                                           
         OI    GLVXFLG1,GLV1SIDE                                                
         MVI   GLVXSESE,4                                                       
         B     CALL1X                                                           
CALL1F   CLI   XCTACT+3,C'F'       NOMINATE SESSION F                           
         BNE   CALL1G                                                           
         OI    GLVXFLG1,GLV1SIDE                                                
         MVI   GLVXSESE,5                                                       
         B     CALL1X                                                           
CALL1G   CLI   XCTACT+3,C'G'       NOMINATE SESSION G                           
         BNE   CALL1H                                                           
         OI    GLVXFLG1,GLV1SIDE                                                
         MVI   GLVXSESE,6                                                       
         B     CALL1X                                                           
CALL1H   CLI   XCTACT+3,C'H'       NOMINATE SESSION H                           
         BNE   CALL1R                                                           
         OI    GLVXFLG1,GLV1SIDE                                                
         MVI   GLVXSESE,7                                                       
         B     CALL1X                                                           
CALL1R   CLI   XCTACT+3,C'R'       NOMINATE RESERVED SESSION                    
         BNE   CALL1T                                                           
         OI    GLVXFLG1,GLV1SIDE                                                
         MVI   GLVXSESE,255                                                     
         B     CALL1X                                                           
CALL1T   CLI   XCTACT+3,C'T'       NOMINATE TRANSFER SESSION                    
         BNE   ERR2                                                             
         OI    GLVXFLG1,GLV1SIDE                                                
         MVI   GLVXSESE,254                                                     
         B     CALL1X                                                           
CALL1X   MVC   XCTLOU,XCTL                                                      
         GOTO1 AGLOBBER,DMCB,=C'PUTD',XCTL+2,22,04                              
*                                                                               
         OC    FRSTOU,FRSTOU       SAVE XCTL OUTPUT DATA                        
         BNZ   *+10                                                             
         MVC   FRSTOU,XCTLOU                                                    
         OC    THISOU,THISOU                                                    
         BZ    *+10                                                             
         MVC   LASTOU,THISOU                                                    
         MVC   THISOU,XCTLOU                                                    
*                                                                               
         MVI   XCTACT,C'*'                                                      
         OI    XCTACTH+6,X'80'                                                  
*                                                                               
CALL2    CLI   XCTOPTH+5,0         TEST ANY INPUT IN OPTION                     
         BE    CALL3                                                            
         MVC   MSG,XCTOPT                                                       
CALL2A   CLC   MSG(7),=C'TEMPEST'                                               
         BNE   CALL2X                                                           
         LHI   R0,F#XTGET                                                       
         GOTO1 CGETFACT,PARM,(X'80',TEMPINF),(R0)                               
         XC    MSG,MSG                                                          
         MVC   MSG(7),=C'TEMPEST'                                               
         MVI   MSG+7,C'='                                                       
         MVC   MSG+8(8),TEMPINF                                                 
CALL2X   GOTO1 AGLOBBER,DMCB,=C'PUTD',MSG,20,50                                 
*                                                                               
CALL3    B     OK4                                                              
*                                                                               
CALLX    B     OK1                                                              
         EJECT                                                                  
***********************************************************************         
*WE ARE GOING TO RETURN CONTROL TO OUR ORIGINAL CALLER                *         
***********************************************************************         
         SPACE 1                                                                
RETN     XC    MSG,MSG             TEST TO RETURN ANYTHING                      
         CLI   XCTOPTH+5,0                                                      
         BE    RETN1                                                            
         MVC   MSG,XCTOPT                                                       
         GOTO1 AGLOBBER,DMCB,=C'PUTD',MSG,20,50                                 
*                                                                               
RETN1    LA    R2,XCTL             BUILD XCTL ELEMENT TO RETURN                 
         USING GLVXCTLD,R2                                                      
         MVI   GLVXCODE,4                                                       
         MVI   GLVXLEN,24                                                       
         MVC   GLVXTOSY,HISSYS                                                  
         MVC   GLVXTOPR,HISPRG                                                  
         MVC   GLVXFRSY,MYSYS                                                   
         MVC   GLVXFRPR,MYPRG                                                   
         MVI   GLVXFLG1,GLV1RETN   SET RETURN CALL                              
*                                                                               
         OC    MSG,MSG             TEST IF ANY OPTION INPUT                     
         BZ    *+8                                                              
         OI    GLVXFLG1,GLV1RETG   SET PASSING GLOBALS                          
*                                                                               
         TM    FRSTIN+GLVXFLG1-GLVXCTLD,GLV1SEPS                                
         BZ    *+8                                                              
         OI    GLVXFLG1,GLV1SEPS   SET SEPERATE SESSION                         
*                                                                               
         CLC   XCTACT+4(5),=C',SWAP' TEST IF RETN,SWAP INPUT                    
         BE    *+14                                                             
         OI    GLVXFLG1,GLV1SIDR   SET SESSION ID OF ORIGINAL CALLER            
         MVC   GLVXSESR,FRSTIN+GLVXSESR-GLVXCTLD                                
*                                                                               
         MVC   XCTLOU,XCTL                                                      
         GOTO1 AGLOBBER,DMCB,=C'PUTD',XCTL+2,22,04                              
*                                                                               
         OC    FRSTOU,FRSTOU       SAVE XCTL OUTPUT DATA                        
         BNZ   *+10                                                             
         MVC   FRSTOU,XCTLOU                                                    
         OC    THISOU,THISOU                                                    
         BZ    *+10                                                             
         MVC   LASTOU,THISOU                                                    
         MVC   THISOU,XCTLOU                                                    
         B     OK5                                                              
         EJECT                                                                  
***********************************************************************         
*INFO AND ERROR EXITS                                                 *         
***********************************************************************         
         SPACE 1                                                                
OK0      LA    R1,=CL60'TEST COMPLETE - FIRST XCTL ELEMENT SHOWN'               
         B     OKX                                                              
OK1      LA    R1,=CL60'PER/XCT RETURNED FROM CON/XCT - ORIGINAL'               
         B     OKX                                                              
OK2      LA    R1,=CL60'PER/XCT WAS CALLED BY CON/XCT'                          
         B     OKX                                                              
OK3      LA    R1,=CL60'PER/XCT RETURNED FROM CON/XCT - RETURNED'               
         B     OKX                                                              
OK4      LA    R1,=CL60'PER/XCT CALLED CON/XCT'                                 
         B     OKX                                                              
OK5      LA    R1,=CL60'PER/XCT RETURNING TO CON/XCT'                           
         B     OKX                                                              
OKX      CLI   18(R9),C'F'         TEST FALINK SIMULATION                       
         BE    OKX1                                                             
         MVC   XCTHDR,0(R1)                                                     
         OI    XCTHDRH+6,X'80'                                                  
         B     EXIT          '                                                  
OKX1     MVC   XCTHDR(13),=C'<FALINK> 0 0 '                                     
         MVC   XCTHDR+13(47),0(R1)                                              
         OI    XCTHDRH+6,X'80'                                                  
         B     EXIT                                                             
         SPACE 1                                                                
HELPACT  MVC   XCTHDR,=CL60'TEST/SAME/SEPS/SEPS,SINGLE/RETN/RETN,SWAP'          
         OI    XCTHDRH+6,X'80'                                                  
         LA    R1,XCTACTH                                                       
         ST    R1,CURSOR                                                        
         B     EXIT                                                             
HELPOPT  MVC   XCTHDR,=CL60'EXIT(THIS PROGRAM)/TIME(RETURN TIME)'               
         OI    XCTHDRH+6,X'80'                                                  
         LA    R1,XCTOPTH                                                       
         ST    R1,CURSOR                                                        
         B     EXIT                                                             
         SPACE 1                                                                
ERR1     LA    R1,=CL60'ED/0001 MISSING INPUT FIELD'                            
         B     ERRX                                                             
ERR2     LA    R1,=CL60'ED/0002 INVALID INPUT FIELD'                            
         B     ERRX                                                             
ERR3     LA    R1,=CL60'ED/0003 ONLY VALID IF CALLED FROM A SEPS SESS'          
         B     ERRX                                                             
*                                                                               
ERRX     CLI   18(R9),C'F'         TEST FALINK SIMULATION                       
         BE    ERRX1                                                            
         MVC   XCTHDR,0(R1)                                                     
         OI    XCTHDRH+6,X'80'                                                  
         B     EXIT          '                                                  
ERRX1    MVC   XCTHDR(13),=C'<FALINK> 1 1 '                                     
         MVC   XCTHDR+13(47),0(R1)                                              
         OI    XCTHDRH+6,X'80'                                                  
         B     EXIT                                                             
         SPACE 1                                                                
EXIT     L     R1,CURSOR           SET CURSOR POS                               
         OI    6(R1),X'40'                                                      
*                                                                               
EXIT0    MVC   USERDATA(4),XCTACT                                               
         OC    USERDATA(4),SPACES                                               
         MVI   USERDATA+4,C'#'                                                  
         MVC   USERDATA+5(1),REASON                                             
         OI    USERDATA+5,X'F0'                                                 
         LHI   R0,F#SETUSR                                                      
         GOTO1 CGETFACT,DMCB,(X'80',USERDATA),(R0)                              
*                                                                               
EXIT1    XC    XCTXCTI,XCTXCTI     DISPLAY INPUT XCTL ELEMENT IN HEX            
         OI    XCTXCTIH+6,X'80'                                                 
         MVC   THISIN,XCTLIN                                                    
         OC    XCTLIN,XCTLIN                                                    
         BZ    EXIT2                                                            
         GOTO1 CHEXOUT,DMCB,XCTLIN,XCTXCTI,24,=C'TOG'                           
*                                                                               
EXIT2    XC    XCTXCTO,XCTXCTO     DISPLAY OUTPUT XCTL ELEMENT IN HEX           
         OI    XCTXCTOH+6,X'80'                                                 
         MVC   THISOU,XCTLOU                                                    
         OC    XCTLOU,XCTLOU                                                    
         BZ    EXIT3                                                            
         GOTO1 CHEXOUT,DMCB,XCTLOU,XCTXCTO,24,=C'TOG'                           
*                                                                               
EXIT3    XC    XCTXCFI,XCTXCFI     DISPLAY FIRST XCTL ELEMENT INPUT             
         OI    XCTXCFIH+6,X'80'                                                 
         OC    FRSTIN,FRSTIN                                                    
         BZ    EXITX                                                            
         GOTO1 CHEXOUT,DMCB,FRSTIN,XCTXCFI,24,=C'TOG'                           
*                                                                               
EXITX    XMOD1                                                                  
         SPACE 1                                                                
         EJECT                                                                  
***********************************************************************         
*CLEAR AND RESHIP ALL FIELDS - REQUIRED FOR CERTAIN RETURN CALLS      *         
***********************************************************************         
         SPACE 1                                                                
RESHIP   LA    RF,XCTHDRH          POINT TO FIRST FIELD                         
         SR    R1,R1                                                            
RESHIP1  CLI   0(RF),0             TEST END OF TWA                              
         BE    RESHIP2                                                          
         OI    6(RF),X'80'                                                      
         IC    R1,0(RF)                                                         
         AR    RF,R1                                                            
         B     RESHIP1                                                          
RESHIP2  MVC   0(3,RF),=X'000100'  SET ERASE BEFORE                             
         BR    RE                                                               
         SPACE 2                                                                
DUMMY    BR    RE                  DUMMY SUB ROUTINE                            
         EJECT                                                                  
***********************************************************************         
*CONSTANTS AND LITERALS                                               *         
***********************************************************************         
         SPACE 1                                                                
MYSYS    DC    CL3'PER'                                                         
MYPRG    DC    CL3'XCT'                                                         
*                                                                               
HISSYS   DC    CL3'CON'                                                         
HISPRG   DC    CL3'XCT'                                                         
*                                                                               
SPACES   DC    80C' '                                                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*DSECT TO COVER SAVED STORAGE IN TWA                                  *         
***********************************************************************         
         SPACE 1                                                                
SAVED    DSECT                                                                  
*                                                                               
FRSTINL  DS    CL8                                                              
FRSTIN   DS    XL24                                                             
FRSTOUL  DS    CL8                                                              
FRSTOU   DS    XL24                                                             
*                                                                               
THISINL  DS    CL8                                                              
THISIN   DS    XL24                                                             
THISOUL  DS    CL8                                                              
THISOU   DS    XL24                                                             
*                                                                               
LASTINL  DS    CL8                                                              
LASTIN   DS    XL24                                                             
LASTOUL  DS    CL8                                                              
LASTOU   DS    XL24                                                             
*                                                                               
TEMPINF  DS    XL8                                                              
         SPACE 2                                                                
***********************************************************************         
*DSECT TO COVER WORKING STORAGE                                       *         
***********************************************************************         
         SPACE 1                                                                
WORKD    DSECT                                                                  
REASON   DS    X                                                                
         DS    XL7                                                              
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
BYTE1    DS    X                                                                
RELO     DS    A                                                                
SAVERE   DS    A                                                                
CURSOR   DS    A                                                                
AGLOBBER DS    A                                                                
*                                                                               
SVPARMS  DS    0XL32                                                            
ATIOB    DS    A                                                                
ATWA     DS    A                                                                
ALOCSYS  DS    A                                                                
ATIA     DS    A                                                                
ACOMFACS DS    A                                                                
AXTRA    DS    A                                                                
         DS    A                                                                
         DS    A                                                                
*                                                                               
DMCB     DS    6F                                                               
PARM     DS    6F                                                               
WORK     DS    XL24                                                             
XCTL     DS    XL24                                                             
MSG      DS    CL20                                                             
*                                                                               
         DS    CL8                                                              
XCTLIN   DS    XL24                                                             
*                                                                               
         DS    CL8                                                              
XCTLOU   DS    XL24                                                             
FLAGS    DS    X                                                                
         DS    X                                                                
*                                                                               
USERDATA DS    CL6                                                              
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
*DSECT TO COVER TWA                                                   *         
***********************************************************************         
         SPACE 1                                                                
PEXCTFFD DSECT                                                                  
         DS    CL64                                                             
       ++INCLUDE PEXCTFFD                                                       
         SPACE 1                                                                
*DDCOMFACS                                                                      
*DDGLVXCTLD                                                                     
*FAFACTS                                                                        
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE FAFACTS                                                        
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002PEXCT00   03/07/08'                                      
         END                                                                    
