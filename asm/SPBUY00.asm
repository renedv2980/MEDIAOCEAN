*          DATA SET SPBUY00    AT LEVEL 111 AS OF 02/26/21                      
*PHASE T21100C   <===                                                           
*INCLUDE SPBUYUPL                                                               
*===========================================================*                   
* AUTHORIZATION BIT UUF P2YRSTS ARE OR'D)                   *                   
* X'80' = ?                                                 *                   
* X'40' = READ-ONLY                                         *                   
* X'20' = NO CLEARANCE DATA                                 *                   
*===========================================================*                   
*===========================================================*                   
* NOTES ON TEMPSTR USAGE FOR TSAR                           *                   
* MGA ACTION USES TEMPSTR PAGES 1/2                         *                   
* MGE ACTION USES TEMPSTR PAGE 3                            *                   
*===========================================================*                   
T21100   TITLE 'T21100 - SPOTPAK BUY PROGRAM - BASE'                            
T21100   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 SPBUYWKX-SPBUYWKD,**T21100,RR=(R8)                               
         USING SPBUYWKD,RC                                                      
*                                                                               
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING T21100+4096,R9                                                   
         LR    R2,R1               SAVE PARM ADDRESS FROM MONITOR               
*                                                                               
         C     R8,RELO             TEST RELO INITIALIZED                        
         BE    HAVERELO                                                         
         L     RF,16(R2)                                                        
         L     RF,CPROTOFF-COMFACSD(RF)                                         
         GOTO1 (RF)                                                             
*                                                                               
         ST    R8,RELO             ** INITL ASSUMES R8=RELO                     
*                                                                               
         L     RF,16(R2)                                                        
         L     RF,CPROTON-COMFACSD(RF)                                          
         GOTO1 (RF)                                                             
*                                                                               
HAVERELO ST    RB,BASERB           NOTE THIS IS AT 0(RC)                        
         ST    RD,BASERD           NOTE THIS IS AT 4(RC)                        
* CLEAR WORK AREA                                                               
         LA    R0,8(RC)                                                         
         LR    R1,RD                                                            
         SR    R1,R0                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         ST    R2,VMONPARM                                                      
         BRAS  RE,INITL            R8 MUST CONTAIN RELO VALUE                   
         ST    R9,BASER9           BASER9 MUST BE SAVED AFTER INITL             
*                                                                               
         L     R3,VBUYTWA                                                       
         USING T211FFD,R3                                                       
         L     RA,VBUYSAVE                                                      
         USING BUYSAVE,RA                                                       
*                                                                               
         XC    BUYMSG,BUYMSG                                                    
         FOUT  BUYMSGH                                                          
*                                                                               
         MVC   SLNTAB,SLNTABC                                                   
         MVC   SVBIGMAX,BIGMAX     SET FLAG FOR MAX REC SIZE                    
         MVC   SVMAXSPT,BIGSPTS    AND MAX SPOTS/REC                            
*                                                                               
         XC    GETBLK,GETBLK                                                    
         MVI   GBYACT,GBYINIT                                                   
         MVC   GBYAGY,AGYALPHA                                                  
         MVC   GBYCOMF,VCOMFACS                                                 
*                                                                               
         GOTO1 VGETBUY,GETBLK                                                   
         MVC   SV1OR2,GBY1OR2                                                   
         CLC   AGYALPHA,=C'T1'                                                  
         BE    *+14                                                             
         CLC   AGYALPHA,=C'SJ'                                                  
         BNE   NOT2                                                             
         CLC   QCLT,=C'PG0'                                                     
         BE    *+10                                                             
         CLC   QCLT,=C'PG1'                                                     
         BE    *+10                                                             
         CLC   QCLT,=C'TBL'                                                     
         BNE   NOT2                                                             
         MVI   SV1OR2,2                                                         
NOT2     B     SCRN                                                             
*                                                                               
BIGMAX   DC    C'Y'                PATCH TO N FOR 4K RECORDS                    
BIGSPTS  DC    AL1(208)                                                         
*                                                                               
SCRN     LA    R0,BUYINP4H         SET FOR 4 INPUT LINES                        
*                                                                               
         CLI   SVSCR,X'F0'                                                      
         BL    SCRN2                                                            
         CLI   SVSCR,X'F7'                                                      
         BH    SCRN2                                                            
         LA    R0,BUYINP1H         F1-F6 HAVE ONLY ONE INPUT LINE               
*                                                                               
SCRN2    ST    R0,FLAST                                                         
*                                                                               
         BRAS  RE,TSTLNKIO         TEST SPLINK CALL                             
         BNE   SCRN2A               NO - NOT LINKIO                             
         OI    SVXFRCTL,SVXFR_SDT  SET SPOT DESKTOP MODE                        
         LA    R8,X'39'                                                         
         BAS   RE,GETOVLY                                                       
         B     EXXMOD                                                           
*                                                                               
SCRN2A   CLI   T211FFD+1,C'*'      TEST DDS TERMINAL                            
         BNE   *+14                                                             
         CLC   =C'KEY=',BUYINP1    TEST LOOKUP BUY RECORD                       
         BE    EDITHL                                                           
*                                                                               
         BRAS  RE,TESTGLB                                                       
         TM    BYTE2,X'01'         TEST RETURN CALL FROM MIS                    
         BZ    SCRN4               NO                                           
         BRAS  RE,RSTRGLB          RESTORE BUY GLOBALS                          
*                                                                               
         CLI   SVSCR,X'F7'         TEST BUY/MOVE                                
         BE    SCRN3                                                            
         CLI   SVSCR,X'F8'         TEST SKEVAL                                  
         BNE   SCRN4                                                            
         CLI   SVDSPMOD,C'S'       TEST SORT ACTIVE                             
         BNE   SCRN4                                                            
*                                                                               
SCRN3    BRAS  RE,BACKMSG                                                       
         MVC   BUYMSG(22),0(R1)    SET 'BACK TO SPOT BUY' MESSAGE               
         XIT1                                                                   
*                                                                               
SCRN4    CLI   SVRCLOPT,RCLAUTB    TEST AWAITING RETURN FROM AUTORCL            
         BNE   SCRN6                                                            
*                                                                               
         MVI   SVRCLOPT,0          CLEAR AUTO RECALL                            
         BRAS  RE,RCLMSG           POINT R1 TO MESSAGE                          
         XC    BUYMSG,BUYMSG                                                    
         MVC   BUYMSG(50),0(R1)                                                 
         B     EXXMOD                                                           
*                                                                               
SCRN6    TM    BYTE2,X'40'         TEST TO FORCE HL EDIT                        
         BZ    *+8                                                              
         NI    BUYMDH+4,X'DF'                                                   
*                                                                               
         TM    BYTE2,X'80'         TEST TO CALL SETSTAR                         
         BZ    NOSTAR                                                           
         OI    6(R2),X'40'         SET CURSOR TO CURRENT LINE                   
         BAS   RE,SETSTAR                                                       
         BAS   RE,XMTALL                                                        
         B     EXXMOD                                                           
*                                                                               
NOSTAR   TM    UPSW,UPON           TEST CALL FROM $MAD                          
         BZ    TESTHL                                                           
*                                                                               
         L     RF,=V(SPBUYUPL)     YES-GET A(UPLOAD OVERLAY)                    
         A     RF,RELO                                                          
         ST    RF,AUPLOVLY                                                      
         MVI   UPMODE,UPMINIT      CALL IT FOR INITIALIZATION                   
         GOTO1 (RF),DMCB,(RC)                                                   
         CLI   UPMODE,UPMEND                                                    
         BE    EXXMOD              EXIT IF ERROR FOUND                          
*===========================================================*                   
* TEST FOR HEADLINE CHANGE                                  *                   
*===========================================================*                   
         SPACE 1                                                                
TESTHL   CLC   =C'@#$%',BUYMD                                                   
         BNE   TESTHL2                                                          
         LA    R8,X'33'                                                         
         BAS   RE,GETOVLY                                                       
         B     EXXMOD                                                           
*                                                                               
TESTHL2  MVI   NEWHEADS,C'N'                                                    
         TM    BUYMDH+4,X'20'                                                   
         BZ    EDITHL                                                           
         TM    BUYBUH+4,X'20'                                                   
         BZ    EDITHL                                                           
         TM    BUYCLH+4,X'20'                                                   
         BZ    EDITHL                                                           
         TM    BUYPRH+4,X'20'                                                   
         BZ    EDITHL                                                           
         TM    BUYESH+4,X'20'                                                   
         BZ    EDITHL                                                           
         TM    BUYSTH+4,X'20'                                                   
         BZ    EDITHL                                                           
         TM    BUYOPH+4,X'20'                                                   
         BO    EDITHLX                                                          
*                                                                               
         CLI   SVSCR,X'F2'         TEST MGE RELATED                             
         BL    EDITHL                                                           
         CLI   SVSCR,X'F8'         TEST MGE RELATED                             
         BH    EDITHL                                                           
         B     EDITHL2                                                          
*                                                                               
EDITHL   MVI   SVMGINIT,0          IF HEADLINES CHANGE, RESET                   
         MVI   SVRCLOPT,C' '       RESET PREVIOUS RECALL  OPTION                
         NI    BUYINP1H+4,X'DF'    FIRST INPUT LINE NOT VALID                   
         XC    SVHFLAGS,SVHFLAGS   CLEAR LAST HISTORY FLAGS                     
*                                                                               
EDITHL2  MVI   NEWHEADS,C'Y'       MAKE A NOTE THAT HEADLINES CHANGED           
*                                                                               
         TM    SVXFRCTL,SVXFR_MAK  TEST CALLED BY MATCHMAKER                    
         BNZ   EDITHL10             NO                                          
         LHI   R4,SVRSNEL-BUYSAVE  CLEAR PREVIOUS REASON CODE                   
         AR    R4,RA                                                            
         XC    0(L'SVRSNEL,R4),0(R4)                                            
*                                                                               
         LA    R2,BUYINFH          CLEAR REASON CODE DISPLAY                    
         XC    8(19,R2),8(R2)                                                   
         OI    6(R2),X'80'                                                      
*                                                                               
EDITHL10 XC    SVRCLEND,SVRCLEND   CLEAR LAST RECALL DATA                       
*                                                                               
         LA    R8,1                CALL HEADLINE EDIT                           
         BAS   RE,GETOVLY                                                       
         BE    EDITHLX                                                          
*                                                                               
         CLI   UPNDX,0             UPLOAD ERROR - TEST IT'S SIGNIFICANT         
         BNE   *+12                                                             
         MVI   ERRAREA,0           NO - CONTINUE (PROBABLY MISSING STA)         
         B     EDITHLX                                                          
*                                                                               
         MVI   UPMODE,UPMERR       YES-                                         
         GOTO1 AUPLOVLY,DMCB,(RC)  CALL UPLOAD OVERLAY                          
         B     EXXMOD              AND EXIT                                     
*                                                                               
EDITHLX  LA    R2,BUYINP1H                                                      
         BRAS  RE,TESTMOVE         NNNMOVE IS NEW, MOVE IS OLD                  
         BNE   EDITHLX0            INPUT IS NOT NEW FORMAT                      
         LA    R8,X'37'            THEN CALL THE MOVE OVERLAY                   
*                                                                               
         CLI   SV1OR2,2            TEST 2-BYTE LINE NUMBERS                     
         BNE   ED32                                                             
*                                                                               
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(NOTFOR2)                                               
         GOTO1 ERROR                                                            
         B     EXXMOD                                                           
*                                                                               
EDITHLX0 TM    UPSW,UPON           IF UPLOADING,                                
         BZ    EDITHLX3                                                         
*                                                                               
         XC    SVDEMOS,SVDEMOS     SET THE DEMOS                                
         LHI   R4,UPDEMS-BUYSAVE                                                
         AR    R4,RA                                                            
         MVC   SVDEMOS(L'UPDEMS),0(R4)                                          
*                                                                               
         OC    SVBRDEMS,SVBRDEMS                                                
         BZ    EDITHLX1                                                         
         XC    SVBRDEMS,SVBRDEMS                                                
         MVC   SVBRDEMS(L'UPDEMS),0(R4)                                         
*                                                                               
EDITHLX1 MVI   UPMODE,UPMNEXT      GET NEXT BUY                                 
*                                                                               
EDITHLX2 DS    0H                                                               
         GOTO1 AUPLOVLY,DMCB,(RC)                                               
         CLI   UPMODE,UPMEND       EXIT IF UPLOAD DONE                          
         BE    EXXMOD                                                           
         CLI   UPMODE,UPMNSTA      TEST NEW STATION                             
         BNE   EDITHLX3                                                         
         LA    R8,1                YES-VALIDATE IT                              
         BAS   RE,GETOVLY                                                       
         MVI   UPMODE,UPMNSTA      AND CONTINUE WITH UPLOAD OVERLAY             
         B     EDITHLX2                                                         
*                                                                               
EDITHLX3 LA    R2,BUYINP1H                                                      
         CLI   SVSCR,X'F3'         TEST DARE MGE                                
         BNE   EDHLX3A                                                          
         CLI   PFKEY,12                                                         
         BE    EDHLX3D                                                          
*                                                                               
EDHLX3A  CLI   SVRCLOPT,RCLAUTA    TEST AUTO RECALL PENDING                     
         BNE   EDHLX3C                                                          
         CLI   PFKEY,2                                                          
         BE    *+12                                                             
         CLI   PFKEY,14                                                         
         BNE   EDHLX3B                                                          
* CALL DARE TO RECALL ORDER                                                     
         BRAS  RE,EDSEND                                                        
         B     EXXMOD              EXIT TO DO XFRCTL                            
*                                                                               
EDHLX3B  MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(BADRCLPF)                                              
         CLI   PFKEY,0             DID THEY HIT A PFKEY                         
         BNE   XERR                                                             
*                                                                               
EDHLX3C  CLI   8(R2),C'*'          TEST CONTINUATION OF PREVIOUS RECALL         
         BE    *+8                                                              
         MVI   PFKEY,0             IF NOT, PFKEY CAN'T BE VALID                 
* PF12 AFTER XFRCTL FROM DARE MEANS GO BACK TO DARE                             
         CLI   PFKEY,12                                                         
         BNE   EDHLX3E                                                          
         CLI   SVSCR,X'F2'         CHECK DARE ORBIT                             
         BE    EDHLX3E                                                          
         CLI   SVSCR,X'F4'         CHECK DARE DPT SCREEN                        
         BE    EDHLX3E                                                          
         CLI   SVSCR,X'F5'         CHECK DARE NOTICE SCREEN                     
         BE    EDHLX3E                                                          
         CLI   SVSCR,X'F8'         CHECK SKEVAL SCREEN                          
         BE    EDHLX3E                                                          
EDHLX3D  TM    SVXFRCTL,SVXFR_DARE                                              
         BZ    EDHLX3E                                                          
         BRAS  RE,XFRETN                                                        
         B     EXXMOD                                                           
*                                                                               
* NEED TO LET *MGA/*MGE GO THROUGH TO OVERLAY                                   
*                                                                               
EDHLX3E  CLI   SVMGINIT,C'A'       TEST IN MGA PREVIOUSLY                       
         BE    EDHLX3F                                                          
         CLI   SVMGINIT,C'E'       TEST IN MGE PREVIOUSLY                       
         BE    EDHLX3F                                                          
         B     EDITHLX4            NO                                           
*                                                                               
EDHLX3F  CLC   =C'*MGA',8(R2)                                                   
         BNE   EDHLX3G                                                          
         MVI   SVRCLOPT,0          RESET LAST RECALL OPTION                     
         MVC   BUTRCODE,=C'MGA'                                                 
         LA    R8,X'30'                                                         
         B     ED34                                                             
*                                                                               
EDHLX3G  CLC   =C'*MGE',8(R2)      MAKEGOOD OFFER EVALUATE?                     
         BNE   EDITHLX4                                                         
*                                                                               
         MVC   BUTRCODE,=C'MGE'                                                 
         MVI   SVRCLOPT,0          RESET LAST RECALL OPTION                     
         LA    R8,X'31'            T21131 IS THE OVERLAY                        
         B     ED34                                                             
*                                                                               
EDITHLX4 DS    0H                                                               
         CLI   SVRCLOPT,C'N'       TEST MULTI-LINE RECALL PREV                  
         BE    EDITHLPF                                                         
         CLI   SVRCLOPT,C'S'       TEST SORTED RECALL PREV                      
         BE    EDITHLPF                                                         
         CLI   SVRCLOPT,RCLROT                                                  
         BE    EDITHLPF                                                         
         CLI   SVRCLOPT,RCLPAY                                                  
         BE    EDITHLPF                                                         
         CLI   SVRCLOPT,RCLPAYDT                                                
         BE    EDITHLPF                                                         
         CLI   SVRCLOPT,RCLHIST                                                 
         BE    EDITHLPF                                                         
         B     EDITHLX5            NO                                           
*                                                                               
EDITHLPF CLI   PFKEY,0                                                          
         BE    EDITHLX5                                                         
         LA    R8,3                SEND IT TO RECALL OVERLAY                    
         B     ED34                                                             
*                                                                               
EDITHLX5 CLI   5(R2),0                                                          
         BE    EDITHLX8                                                         
         CLI   8(R2),C'*'          SKIP NOP TR CODES                            
         BNE   EDITTR                                                           
*                                                                               
EDITHLX7 DS    0H                  TEST INPUT IN NEXT FIELD                     
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         C     R2,FLAST                                                         
         BNH   EDITHLX5                                                         
*                                                                               
EDITHLX8 LA    R2,BUYINP1H                                                      
         BRAS  RE,SETSCRN           MAKE SURE HAVE NORMAL SCREEN                
         MVC   BUYMSG(22),=C'ENTER TRANSACTION DATA'                            
         B     EXIT                                                             
*                                                                               
EDITTR   DS    0H                                                               
         CLC   =C'SEND',8(R2)                                                   
         BE    SEND                                                             
         CLC   =C'DARE',8(R2)                                                   
         BE    SEND                                                             
         CLC   =C'OM',8(R2)        ORDER MANAGER                                
         BE    SEND                                                             
*                                                                               
         CLI   T211FFD+1,C'*'      TEST DDS TERMINAL                            
         BNE   EDITTR1             NO                                           
         OC    SVKEY+14(4),SVKEY+14 MAKE SURE HAVE SOMETHING                    
         BZ    EDITTR1                                                          
         CLC   8(3,R2),=C'PFM'                                                  
         BE    CALLXFR                                                          
         CLC   8(3,R2),=C'NFL'                                                  
         BE    CALLXFR                                                          
*                                                                               
EDITTR1  CLC   8(3,R2),=C'MIS'                                                  
         BE    CALLXFR                                                          
*                                                                               
         CLC   8(3,R2),=C'FIS'                                                  
         BE    CALLXFR                                                          
*                                                                               
         CLC   8(3,R2),=C'MAT'                                                  
         BE    CALLXFR                                                          
*                                                                               
         CLC   8(3,R2),=C'PAY'                                                  
         BE    CALLXFR                                                          
*                                                                               
         CLC   8(2,R2),=C'PW'                                                   
         BE    *+14                                                             
         CLC   8(2,R2),=C'C2'                                                   
         BNE   EDITTR1A                                                         
         NI    BUYMDH+4,X'DF'      FORCE HEADLINE EDIT                          
         B     CALLXFR                                                          
*                                                                               
EDITTR1A TM    SVAFLAG1,AGYFCTAQ+AGYTRDQ  TEST TRADE CLIENT                     
         BZ    *+14                                                             
         CLC   8(2,R2),=C'CTA'                                                  
         BE    CALLXFR                                                          
*                                                                               
         TM    SVCOPT1,X'40'       TEST INFOMERCIAL CLIENT                      
         BZ    EDITTR2                                                          
         CLC   8(2,R2),=C'IC'                                                   
         BE    CALLXFR                                                          
         CLC   8(2,R2),=C'IB'                                                   
         BE    CALLXFR                                                          
*                                                                               
EDITTR2  LA    R8,X'21'                                                         
         LA    R0,BUYINP1H                                                      
         CR    R0,R2                                                            
         BNE   EDITTR4                                                          
         MVI   RCLOPT,C'D'                                                      
         CLC   =C'DEM',8(R2)                                                    
         BE    ED30                                                             
         MVI   RCLOPT,C'F'         DARE FLIGHT DISPLAY                          
         CLC   =C'DDATES',8(R2)    DISPLAY DARE FLIGHT DATES                    
         BE    ED30                                                             
         MVI   RCLOPT,C'R'         DARE ORDER DISPLAY                           
*****                                                                           
         CLC   =C'DD/C',8(R2)                                                   
         BE    EDITTR3                                                          
         CLC   =C'DD/T',8(R2)                                                   
         BNE   *+12                                                             
         MVI   RCLOPT,C'T'         DARE ORDER DISPLAY FOR TRADE                 
         B     EDITTR3                                                          
*****                                                                           
         CLC   =C'DD',8(R2)                                                     
         BNE   EDITTR4                                                          
EDITTR3  BRAS  RE,EDITDD                                                        
         CLI   ERRCD,0                                                          
         BE    ED30                                                             
         B     XERR                                                             
*                                                                               
EDITTR4  MVI   RCLOPT,0                                                         
         LA    R8,3                RECALL OVLY                                  
         CLC   =C'LI=',8(R2)                                                    
         BE    ED30                                                             
         CLI   8(R2),C'0'          TREAT NUMERIC INPUT AS LINE RECALL           
         BL    *+14                                                             
         MVC   BUTRCODE,=C'LI='                                                 
         B     ED34                                                             
* ACTION IS NOT DISPLAY - CAN THEY DO IT                                        
         MVI   ERRCD,NOFNACCS                                                   
         TM    UPSW,UPON           TEST UPLOAD                                  
         BO    EDITTR6             YES - ALLOW ACCESS                           
         CLI   T211FFD+1,C'*'      TEST DDS TERMINAL                            
         BE    EDITTR6             YES - ALLOW ACCESS                           
         TM    T211FFD+12,X'40'    TEST READ-ONLY ACCESS                        
         BO    EDERR2                                                           
EDITTR6  CLC   =C'MGA',8(R2)                                                    
         BE    ED20                                                             
         CLC   =C'MGE',8(R2)       REQUESTED MGE?                               
         BE    ED24                                                             
         CLC   =C'O,-(',8(R2)      TEST DARE MAKEGOOD PENDING                   
         BE    EDITTR8             YES - LET IT GO                              
*                                                                               
         MVI   ERRCD,ESTLOCK                                                    
         TM    SVECNTRL,X'08'      TEST ESTIMATE LOCKED                         
         BO    EDERR2                                                           
         MVI   ERRCD,NEWERRS                                                    
         LHI   R0,PWLOCK                                                        
         OC    SVECOST2,SVECOST2                                                
         BZ    *+8                                                              
         LHI   R0,C2LOCK                                                        
         STH   R0,NERRCD                                                        
         TM    SVPWFLG,X'C0'       TEST PW BUY LOCK                             
         BNZ   EDERR2                                                           
* CHECK LIMITED CODES FOR CANAD NTWK                                            
EDITTR8  CLI   SVAPROF+7,C'C'                                                   
         BNE   ED12                                                             
         CLI   BUYMD,C'N'                                                       
         BNE   ED12                                                             
         OC    SVNDEF(16),SVNDEF                                                
         BZ    ED4                                                              
* CHECK FOR CUT-IN LIST FUNCTIONS                                               
         LA    R8,X'15'                                                         
         CLC   =C'CUT',8(R2)                                                    
         BE    ED30                                                             
         CLC   =C'ZCUT',8(R2)                                                   
         BE    ED30                                                             
         LA    R8,X'14'                                                         
         CLC   =C'CSET',8(R2)                                                   
         BE    ED30                                                             
         CLC   =C'CCLR',8(R2)                                                   
         BE    ED30                                                             
         CLC   =C'CDIS',8(R2)                                                   
         BNE   ED4                                                              
         LA    R8,X'22'                                                         
         MVI   RCLOPT,RCLCLST                                                   
         MVC   SVRCLOPT,RCLOPT                                                  
         B     ED30                                                             
*                                                                               
ED4      MVI   ERRCD,NOSTATNS                                                   
         CLI   SVNRGN,C'*'         MAY NOT BUY IN RGN '*'                       
         BE    EDERR                                                            
         LA    RE,CNTWKBUY                                                      
         OC    SVNDEF(16),SVNDEF   TEST EXP OR BUY                              
         BNZ   *+8                                                              
         LA    RE,CNTWKEXP                                                      
         LA    RF,8                                                             
*                                                                               
         CLC   0(1,RE),8(R2)                                                    
         BE    ED12                                                             
         LA    RE,1(RE)                                                         
         BCT   RF,*-14                                                          
                                                                                
* CHECK FOR SPECIAL COMMANDS TO DIS/CLR/SET NETWORK DEF                         
                                                                                
         LA    R0,BUYINP1H                                                      
         CR    R0,R2                                                            
         BNE   EDERR                                                            
         CLC   =C'NCLR',8(R2)                                                   
         BNE   ED10                                                             
ED9X     NI    BUYSTH+4,X'DF'      FORCE STATION EDIT                           
         MVC   8(4,R2),=C'NDIS'    AND SET COMMAND TO DISPLAY                   
         XC    SVOLDNET,SVOLDNET   FORGET EVERYTHING ABOUT NETWORK              
         B     EDITHL                                                           
*                                                                               
ED10     LA    R8,X'22'                                                         
         CLC   =C'NDIS',8(R2)                                                   
         BNE   ED11                                                             
         MVI   RCLOPT,RCLNET                                                    
         MVI   SVRCLOPT,RCLNET                                                  
         B     ED30                                                             
*                                                                               
ED11     LA    R8,X'14'                                                         
         CLC   =C'NSET',8(R2)                                                   
         BE    ED30                                                             
         CLC   =C'NXPLOD',8(R2)                                                 
         BE    ED30                                                             
*                                                                               
         LA    R8,3                                                             
         CLC   =C'NLIS',8(R2)                                                   
         BNE   EDERR                                                            
         MVI   SVRCLOPT,RCLNLST    USE BECAUSE 03 CLEARS RCLOPT                 
         B     ED30                                                             
*                                                                               
CNTWKEXP DC    CL8'ACIO  '         CODES FOR EXPLODED BUY                       
CNTWKBUY DC    CL8'ABCDIOS'        CODES FOR NETWORK BUY                        
*                                                                               
ED12     LA    R8,6                                                             
         CLI   8(R2),C'D'          DELETE                                       
         BE    ED30                                                             
*                                                                               
         LA    R8,X'15'                                                         
         OC    SVNDEF(16),SVNDEF   TEST CANAD NTWK                              
         BNZ   *+8                 YES                                          
         LA    R8,5                                                             
         CLI   8(R2),C'A'          ALLO                                         
         BE    ED30                                                             
         CLI   8(R2),C'O'          OTO                                          
         BE    ED30                                                             
         CLC   =C'C,M=',8(R2)      M=                                           
         BE    ED30                                                             
         LA    R8,X'55'                                                         
         CLC   8(4,R2),=C'COMP'    COMPLETE                                     
         BE    ED30                                                             
         CLI   8(R2),C'I'          INVOICE                                      
         BE    ED30                                                             
         CLI   8(R2),C'F'          FILM ALLOC                                   
         BE    ED30                                                             
         CLC   =C'RSV',8(R2)                                                    
         BE    ED30                                                             
*                                                                               
         CLC   =C'DF',AGYALPHA     LET DFS MAKE BUYS UNDER NON-POL              
         BE    *+14                                                             
         CLC   =C'NON-POL',BUYBU                                                
         BE    ED16                                                             
*                                                                               
         LA    R8,X'14'                                                         
         OC    SVNDEF(16),SVNDEF   TEST CANAD NTWK                              
         BNZ   ED14                                                             
*                                                                               
         LA    R8,X'34'            NETPAK BUY                                   
         OC    SVNETYM,SVNETYM     TEST NETPAK USER                             
         BNZ   ED14                                                             
*                                                                               
         LA    R8,4                                                             
ED14     CLI   8(R2),C'B'          BUY                                          
         BE    ED15                                                             
         CLC   =C'COPY',8(R2)                                                   
         BE    ED15                                                             
         CLC   =C'MOVE',8(R2)                                                   
         BNE   ED16                                                             
*                                                                               
ED15     CLI   SVPOLONL,C'Y'       TEST POL BUYS ONLY                           
         BNE   ED30                NO                                           
         CLI   SVKEY+3,X'FF'                                                    
         BE    ED30                                                             
         B     ED30          ***** NOP ABEA/MHER 2/20/97                        
         MVI   ERRCD,NOTPOL                                                     
         B     EDERR2                                                           
*                                                                               
ED16     LA    R8,8                                                             
         CLC   =C'LOCK=',8(R2)     TEST LCI LOCKIN INPUT                        
         BE    ED30                                                             
         CLC   =C'C,DEM',8(R2)     TEST DEMOS                                   
         BE    ED30                                                             
         CLC   =C'C,SPI',8(R2)     TEST SPILL                                   
         BE    ED30                                                             
         CLC   =C'C,PBD',8(R2)     OR POST BUY DEMEOS                           
         BE    ED30                                                             
         CLI   5(R2),1                                                          
         BNE   ED17                                                             
         CLI   8(R2),C'C'          DEMOS CAN BE CHANGED IN DISPLAY AREA         
         BNE   ED17                                                             
         CLI   SVRCLOPT,RCLDEM     IF THEY'VE JUST BEEN RECALLED                
         BE    ED30                                                             
         CLI   SVRCLOPT,RCLDEMS                                                 
         BE    ED30                                                             
         CLI   SVRCLOPT,RCLPDEM    POST BUY DEMOS TOO STUPID                    
         BE    ED30                                                             
         CLI   SVRCLOPT,RCLPDEMX                                                
         BE    ED30                                                             
*                                                                               
ED17     LA    R8,7                                                             
         CLI   8(R2),C'C'          CHANGE                                       
         BE    ED30                                                             
         CLI   8(R2),C'S'          SKED                                         
         BE    ED30                                                             
         CLC   =C'RC=',8(R2)       REASON CODE                                  
         BE    ED30                                                             
*                                                                               
ED20     LA    R8,X'30'                                                         
         CLC   =C'MGA',8(R2)                                                    
         BNE   EDERR                                                            
         B     ED26                                                             
*                                                                               
ED24     LA    R8,X'31'                                                         
         CLC   =C'MGE',8(R2)                                                    
         BNE   EDERR                                                            
*                                                                               
ED26     MVI   SVRCLOPT,0          CLEAR LAST RECALL ACTION                     
         XC    SVSTCODE,SVSTCODE                                                
         CLI   5(R2),3                                                          
         BE    ED30                                                             
         ZIC   R1,5(R2)                                                         
         AHI   R1,-5               SKIP 'MGA/'                                  
         CHI   R1,1                ONLY MOVE IN CODE                            
         BNH   *+8                                                              
         LA    R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SVSTCODE(0),12(R2)                                               
         OC    SVSTCODE,SPACES                                                  
         EJECT                                                                  
ED30     DS    0H                                                               
         MVC   BUTRCODE,8(R2)      SAVE TRANSACTION CODE                        
*                                                                               
ED32     L     RE,ASVDARE                                                       
         MVI   SVDRFLG2-SVDARED(RE),0    RESET NO DARE TEST FLAG                
         MVI   ADBLOCK,0           RESET UPGRADE FLAG                           
*                                                                               
ED34     DS    0H                                                               
         STC   R8,RESTOROV         SET LAST OVLY CALLED FROM HERE               
*                                                                               
         BRAS  RE,SETSCRN          MAKE SURE RIGHT SCREEN LOADED                
*                                                                               
         XC    BUYMSG,BUYMSG                                                    
         XC    BUSTART(BUHUTADJ-BUSTART),BUSTART                                
         XC    BUREF(BUDEM-BUREF),BUREF                                         
         XC    PRDLIST,PRDLIST                                                  
         TM    UPSW,UPON+UPCHA     TEST UPLOADING AND CHANGING THE BUY          
         BO    ED34A               YES-LEAVE REC ALONE (IT HAS THE BUY)         
         TM    WRKRUPSW,WRKRUPSW_NOIO  TEST WORKER UPLOAD                       
         BO    ED34A                                                            
* CTSFB  (CLEAR THE BUYREC AREA)                                                
         LA    R0,REC                                                           
         LHI   R1,REC2-REC         MAX RECORD LENGTH                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
ED34A    BAS   RE,GETOVLY                                                       
         BE    ED34B                                                            
         MVI   UPMODE,UPMERR       UPLOAD ERROR                                 
         GOTO1 AUPLOVLY,DMCB,(RC)                                               
         B     EDITHLX1            BRANCH TO NEXT BUY TO UPLOAD                 
*                                                                               
* MOVE * TO FIRST CHAR OF INPUT LINE IF NOT ALREADY THERE                       
*                                                                               
ED34B    CLI   SVSCR,X'F0'                 WE'RE IN MGA DISPLAY                 
         BE    *+12                                                             
         CLI   SVSCR,X'F3'                                                      
         BNE   *+8                                                              
         LA    R2,BUYINP1H                                                      
         CLI   8(R2),C'*'                                                       
         BE    *+8                                                              
         BAS   RE,SETSTAR                                                       
*                                                                               
         CHI   R8,3                TEST RECALL                                  
         BE    ED54                                                             
* TEST TO CALL ADDS INTERFACE                                                   
         TM    SVAFLAG1,X'80'      TEST ADDS INTERFACE ACTIVE                   
         BZ    ED35                                                             
         CLI   SVUPDATE,X'FF'      TEST ALREADY CALLED                          
         BE    ED35                                                             
         TM    SVUPDATE,X'C0'      TEST REASON TO CALL                          
         BZ    ED35                NO                                           
         GOTO1 CALLADDS,C'C'                                                    
         MVI   SVUPDATE,X'FF'      SET TO NOT CALL AGAIN                        
*                                                                               
* TEST TO CALL REQUEST MODULE                                                   
ED35     CLC   BUTRCODE,=C'COMP'   TIM NOTES ONLY TESTS 3 BYTES                 
         BE    ED54                                                             
         CLC   =C'SPOT',BUYBU                                                   
         BE    ED54                                                             
         CLC   =C'TEST',BUYBU                                                   
         BE    ED54                                                             
         CLI   BUYREC,X'10'        TEST BUY RECORD IN 'REC'                     
         BNH   ED54                NO - SKIP REQUEST (THANKS ABS)               
*                                                                               
         TM    VCALLBAS,X'80'      TEST IN CALLBASE MODE                        
         BZ    ED35A               NO                                           
         TM    WRKRUPSW,WRKRUPSW_ISDRMG   TEST DARE MAKEGOOD                    
         BZ    ED54                       NO - EXIT                             
         LHI   R4,SVB0PROF-BUYSAVE        CHECK B0 PROFILE                      
         AR    R4,RA                                                            
         CLI   0(R4),C'Y'                 TEST DO T/A REQUEST                   
         BNE   ED54                                                             
*                                                                               
ED35A    CLI   SVAPROF+7,C'C'      TEST CANAD                                   
         BNE   *+12                NO                                           
         CLI   BUYMD,C'N'          TEST NTWK                                    
         BE    EDITCN                                                           
         CLI   SVPRD,X'FF'                                                      
         BNE   ED38                                                             
         EJECT                                                                  
* POOL                                                                          
         CLI   SVCPROF,C'0'        TEST BRD POOL                                
         BNE   ED36                YES                                          
         CLI   SVQPRDS,0                                                        
         BNE   ED54                                                             
         CLI   SVCPROF+12,C'Y'     TEST TRUE POL BY BRD                         
         BNE   ERQ                                                              
         OC    QLIST,QLIST         TEST ANY NEW PRDS                            
         BZ    ED54                NO - CONTINUE                                
         B     ERQX                ELSE CALL REQ OVLY                           
         SPACE 1                                                                
* BRAND POL *                                                                   
         SPACE 1                                                                
ED36     OC    BDMASPRD,BDMASPRD                                                
         BZ    ED54                                                             
         CLC   SVQPRDS,BDMASPRD    TEST SAME MASPRDS                            
         BE    ED54                NO                                           
         MVC   SVQPRDS,BDMASPRD                                                 
         B     ERQ                                                              
* NON-POL                                                                       
ED38     CLI   BDTIME,0            TEST PIGGYBACK                               
         BNE   ED50                YES                                          
         CLI   SVQPRDS,0                                                        
         BNE   ED54                                                             
         B     ERQ                                                              
         EJECT                                                                  
* PIGGYBACK - FIND PBELEM                                                       
*                                                                               
ED50     LA    R6,BDELEM                                                        
*                                                                               
ED52     ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    DCH0                                                             
         CLI   0(R6),4                                                          
         BNE   ED52                                                             
*                                                                               
         CLC   SVQPRDS+1(1),2(R6)                                               
         BE    ED54                                                             
         MVC   SVQPRDS+1(1),2(R6)                                               
         B     ERQ                                                              
*                                                                               
ED54     ZIC   R0,0(R2)            ELSE CHECK FOR MORE TR CODES                 
         AR    R2,R0                                                            
         C     R2,FLAST                                                         
         BH    EDX                                                              
         CLI   5(R2),0                                                          
         BE    EDX                                                              
         CLI   8(R2),C'*'                                                       
         BE    ED54                                                             
         B     EDITTR                                                           
*                                                                               
EDX      TM    UPSW,UPON           IF UPLOADING,                                
         BO    EDITHLX1            BRANCH TO NEXT BUY TO UPLOAD                 
*                                                                               
         TM    SVXFRCTL,SVXFR_MAK   TEST CALLED BY MATCHMAKER                   
         BZ    EDX2                                                             
         BRAS  RE,XFRETN                                                        
         B     EXXMOD                                                           
*                                                                               
EDX2     LA    R2,BUYINP1H                                                      
         CLI   PFKEYLEN,0          TEST NEED TO ADD PFKEY LINE TO TWA           
         BE    EXIT                NO - DONE                                    
         BRAS  RE,SETPFKEY                                                      
         B     EXIT                                                             
         SPACE 2                                                                
ERQ      CLI   SVQPRDS,0                                                        
         BNE   *+10                                                             
         MVC   SVQPRDS(1),SVPRD                                                 
ERQX     LA    R8,X'25'                                                         
         BAS   RE,GETOVLY                                                       
         B     ED54                                                             
*                                                                               
* CANAD NTWK                                                                    
*                                                                               
EDITCN   CLC   SVQPRDS(1),SVLIN    TEST SAME LINE NUM                           
         BE    ED54                YES - INGORE                                 
         MVC   SVQPRDS(1),SVLIN                                                 
         B     ERQX                                                             
         EJECT                                                                  
*                                                                               
*        GET CORRECT OVERLAY                                                    
*                                                                               
GETOVLY  LR    R0,RE                                                            
         MVC   OLDSVKEY,SVKEY      SAVE CURRENT BUYLINE                         
         GOTO1 VCALLOV,DMCB,((R8),0),VTWA                                       
         CLI   4(R1),X'FF'                                                      
         BE    DCH0                                                             
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(RC),(RA)                                              
*                                                                               
GETOVLYX LR    RE,R0                                                            
*                                                                               
         CLI   ERRAREA,0           TEST ERROR IN OVERLAY                        
         BCR   8,RE                NO-RETURN WITH CC EQ                         
         TM    UPSW,UPON           YES-TEST UPLOADING                           
         BZ    EXXMOD              NO-EXIT THE PROGRAM                          
         LTR   RE,RE               YES-RETURN WITH CC NE                        
         BR    RE                                                               
         SPACE 2                                                                
EXIT     OI    6(R2),X'40'         POSITION CURSOR                              
*                                                                               
EXXMOD   DS    0H                                                               
         BRAS  RE,GFACT                                                         
         MVC   SVBUTRCD,BUTRCODE   SAVE LAST TRANSACTION CODE                   
                                                                                
*==============================================================                 
* TRAP TO FIND WHY SVKEY AND DISK ADDRESS AT SVKEY+14 HAVE                      
* DIFFERENT LINE NUMBERS FOR CANADIAN NETWORK                                   
*==============================================================                 
                                                                                
         CLI   BUYMD,C'N'                                                       
         BNE   EXX10                                                            
         BRAS  RE,CANCHK                                                        
*                                                                               
EXX10    TM    VCALLBAS,X'80'      TEST IN CALLBASE MODE                        
         BZ    EXX30                                                            
*                                                                               
         TM    SVXFRCTL,SVXFR_SDT  TEST SPOT DESKTOP MODE                       
         BNZ   EXX20                YES                                         
         NI    VCALLBAS,X'7F'      RESET MODE FLAG                              
         L     RD,CALLBSRD                                                      
         XC    CALLBSRD,CALLBSRD                                                
         B     EXX30                                                            
*                                                                               
EXX20    OC    CALLBSRD,CALLBSRD   ARE WE ON AN INTERVENING CALL?               
         BZ    EXX22                NO                                          
         L     RD,CALLBSRD          YES - GO BACK TO IT!                        
         XC    CALLBSRD,CALLBSRD                                                
         B     EXX30                                                            
*                                                                               
EXX22    GOTO1 VCALLOV,DMCB,(X'39',0),0    RE-LOAD DESKTOP CONTROLLER           
         CLI   4(R1),X'FF'                                                      
         JE    *+2                                                              
         ICM   RD,15,BY39RD        GET RD OF T21139                             
         JZ    *+2                                                              
         XC    BY39RD,BY39RD                                                    
         NI    VCALLBAS,X'7F'      RESET MODE FLAG                              
*                                                                               
EXX30    XIT1                                                                   
*                                                                               
SETSTAR  DS    0H                                                               
         TM    DRMGFLG,DRMG_NOSTAR      TEST TO SUPPRESS '*' FOR ABS            
         BO    SETSTARX                                                         
         MVC   ELEM(L'BUYINP1),8(R2)                                            
         MVI   8(R2),C'*'                                                       
         MVC   9(L'BUYINP1-1,R2),ELEM                                           
SETSTARX DS    0H                                                               
         FOUT  (R2)                                                             
         BR    RE                                                               
*                                                                               
* SET X'000101' AT END OF TWA TO TRANSMIT ALL FIELDS                            
*                                                                               
XMTALL   LA    R2,BUYMSGH                                                       
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNE   *-10                                                             
         MVI   1(R2),1             SET TO TRANSMIT ALL                          
         MVI   2(R2),1                                                          
         BR    RE                                                               
*                                                                               
EDERR    MVI   ERRCD,INVERR                                                     
*                                                                               
EDERR2   TM    UPSW,UPON           IS IT A FUCKING UPLOAD                       
         JNZ   *+2                 BETTER TO DIE HERE THAN IN HEXIN !           
*                                                                               
XERR     GOTO1 ERROR                                                            
         B     EXXMOD                                                           
         EJECT                                                                  
*=======================================================*                       
* CALL ADDS INTERFACE TO 'SEND' REPORTS VIA SOON        *                       
*=======================================================*                       
         SPACE 1                                                                
SEND     DS    0H                                                               
         MVC   BUTRCODE,8(R2)                                                   
         CLC   =C'OM',8(R2)        ORGASMS NEED TO BE CHANGED                   
         BNE   *+8                                                              
         MVI   BUTRCODE,C'D'       TO LOOK LIKE DARE                            
*                                                                               
         BRAS  RE,EDSEND                                                        
         BNZ   XERR                                                             
         CLI   BUTRCODE,C'D'       TEST DARE SEND                               
         BE    EXXMOD              YES - EXIT                                   
*                                                                               
SEND2    DS    0H                                                               
         GOTO1 CALLADDS,C'S'                                                    
         CLI   ERRAREA,0           TEST MESSAGE PRESENT                         
         BNE   EXXMOD                                                           
*                                                                               
         XC    BUYMSG,BUYMSG                                                    
         MVC   BUYMSG(38),=C'REPORT XXX,1234 WILL BE PROCESSED SOON'            
         L     R4,AREC5            POINT TO SPADINTD                            
         USING SPADINTD,R4                                                      
         MVC   BUYMSG+7(3),ADSRPTID                                             
         SR    R0,R0                                                            
         ICM   R0,3,ADSRPTNO       IF ZERO, NO DATA SENT                        
         BZ    SEND4                                                            
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  BUYMSG+11(4),DUB                                                 
         MVI   SVUPDATE,0          RE-ENABLE UPDATE INTERFACE                   
         B     EXXMOD                                                           
         DROP  R4                                                               
*                                                                               
SEND4    XC    BUYMSG,BUYMSG                                                    
         MVC   BUYMSG(32),=C'** ERROR ** NO ADDS DATA TO SEND'                  
         B     EXXMOD                                                           
*                                                                               
***D50   DS    0H                                                               
***      B     XERR                                                             
         EJECT                                                                  
CALLXFR  DS    0H                                                               
         BRAS  RE,BLDXFR                                                        
         MVC   BUYMSG(22),=C'** BACK TO SPOT BUY **'                            
         BAS   RE,SETSTAR          MOVE * IN FRONT OF INPUT                     
         LA    R2,BUYINP1H                                                      
         B     EXIT                                                             
         SPACE 2                                                                
*============================================================*                  
* PROVIDE LINKAGE FOR SPBUY31 TO CALL SPBUY32 OVER ITSELF    *                  
* AND THEN RESTORE SPBUY31                                   *                  
* NOTE THAT DMCB HAS PARMS FOR SPBUY32 CALL                  *                  
*============================================================*                  
         SPACE 1                                                                
CALLB32  NTR1  BASE=BASERB                                                      
*                                                                               
         MVI   DUB,X'32'                                                        
         BRAS  RE,BUOVLY           GET 32 INTO STORAGE                          
*                                                                               
         L     RF,DUB                                                           
         GOTO1 (RF),DMCB           CALL THE 32 WITH PARMS FROM 31               
*                                                                               
         MVI   DUB,X'31'                                                        
         BRAS  RE,BUOVLY           GET 31 INTO STORAGE                          
         XIT1                                                                   
                                                                                
*============================================================*                  
* PROVIDE LINKAGE FOR SPBUY31 TO CALL SPBUY38 OVER ITSELF    *                  
* AND THEN RESTORE SPBUY31                                   *                  
* NOTE THAT DMCB HAS PARMS FOR SPBUY38 CALL                  *                  
*============================================================*                  
         SPACE 1                                                                
CALLB38  NTR1  BASE=BASERB                                                      
*                                                                               
         MVI   DUB,X'38'                                                        
         BRAS  RE,BUOVLY           GET 32 INTO STORAGE                          
*                                                                               
         L     RF,DUB                                                           
         GOTO1 (RF),DMCB           CALL THE 32 WITH PARMS FROM 31               
*                                                                               
         MVI   DUB,X'31'                                                        
         BRAS  RE,BUOVLY           GET 31 INTO STORAGE                          
         XIT1                                                                   
                                                                                
*============================================================*                  
* PROVIDE LINKAGE FOR MGE PROCESSING TO RE-ENTER BUY PROGRAM *                  
* AFTER BUILDING INPUT LINES. EXIT IS ALWAYS BACK TO CALLER  *                  
*============================================================*                  
         SPACE 1                                                                
CALLBASE NTR1  BASE=BASERB                                                      
         B     *+12                                                             
         DC    CL8'CALLBASE'                                                    
         OI    VCALLBAS,X'80'      SET FLAG THAT CALLBASE ACTIVE                
*                                                                               
         CLC   0(8,R1),=C'*T21139*'                                             
         BNE   *+12                                                             
         STCM  RD,15,BY39RD                                                     
         B     *+8                                                              
         ST    RD,CALLBSRD                                                      
*                                                                               
         L     R9,BASER9                                                        
         L     R3,VBUYTWA                                                       
         L     RA,VBUYSAVE                                                      
*                                                                               
         XC    BMGEDATA,BMGEDATA                                                
         XC    BLDROW,BLDROW       RESET OUTPUT POINTERS                        
         XC    PRVDSPLN,PRVDSPLN                                                
         LA    RE,BUYOUTH                                                       
         XC    0(3,RE),0(RE)                                                    
         L     RF,=A(TESTHL)                                                    
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         EJECT                                                                  
*                                                                               
LENTWA   DC    H'14336'                                                         
*                                                                               
RELO     DC    A(0)                                                             
*                                                                               
DCH0     DC    H'0'                                                             
*                                                                               
SLNTABC  DS    0H                                                               
       ++INCLUDE SPSLNTAB                                                       
         DC    5X'00'                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
SPCOMMON NTR1  BASE=BASERB                                                      
*                                                                               
         L     R9,BASER9           RESTORE SECOND BASE REG AS WELL              
         L     R3,VBUYTWA                                                       
         L     RA,VBUYSAVE                                                      
         SRL   RF,24                                                            
         B     SPCOMTAB(RF)                                                     
*                                                                               
SPCOMTAB B     SPERROR             X'00'                                        
         B     SPANY               X'04'                                        
         B     SPMOVE              X'08'                                        
         B     SPPACK              X'0C'                                        
         B     SPREAD              X'10'                                        
         B     SPSEQ               X'14'                                        
         B     SPHIGH              X'18'                                        
         B     SPADD               X'1C'                                        
         B     SPDIR               X'20'                                        
         B     SPRDSTA             X'24'                                        
         B     SPSTA               X'28'                                        
         B     SPGETREC            X'2C'                                        
         B     SPPUTREC            X'30'                                        
         B     SPADDREC            X'34'                                        
         B     SPFIL               X'3C'                                        
SPCOMUSR DC    5AL4(0)   ** USER ROUTINES 10 - 14 ORIGIN HERE **                
         DC    9AL4(0)   ** USER ROUTINES 01 - 09 ORIGIN HERE **                
SPCOMCNT EQU   (*-SPCOMTAB)/4      NUMBER OF ENTRIES                            
         SPACE 1                                                                
* USER ROUTINES 15-18 ARE NOT INCLUDED IN SPCOMCNT BECAUSE THE *                
* ADDRESS CONSTANTS FOR THEM ARE SEPARATELY INITIALIZED        *                
         SPACE 1                                                                
         DC    4AL4(0)   ** USER ROUTINES 15 - 18 ORIGIN HERE **                
         SPACE 2                                                                
SPCOMXIT XIT1                                                                   
         EJECT                                                                  
*================================================================*              
* ERRAREA = X'FE' TO CAUSE DC H'0',C'$ABEND'                     *              
* WITH NORMAL ERROR PROCESSING                                   *              
* ERRAREA= X'FF' IF PRESET MESSAGE IS PRESENT                    *              
*================================================================*              
         SPACE 1                                                                
SPERROR  OI    6(R2),X'40'           POSITION CURSOR                            
         TM    VCALLBAS,X'80'        TEST IN 'CALLBASE' MODE                    
         BZ    SPERR02               NO                                         
* IF IN DTOP MODE AND SOMEONE WANTS A $ABEND, JUST DIE                          
         TM    SVXFRCTL,SVXFR_SDT  TEST SPOT DESKTOP MODE                       
         BZ    *+12                 NO                                          
         CLI   ERRAREA,X'FE'       $ABEND?                                      
         JE    *+2                  YES, DIE! NO ABEND FOR SBTK                 
* RETURN SPECIAL ERRORS IF ENTERED VIA CALLBASE BUT NOT IF CALLED BY OM         
         TM    SVXFRCTL,SVXFR_DARE   TEST CALL BY OM?                           
         BO    SPERR01               YES, SKIP SPECIAL ERROR                    
*                                                                               
         CLI   ERRAREA,0             TEST FOR PRESET MESSAGE                    
         BE    *+14                  NO                                         
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(DARMGERR)   CAN'T REALLY DEAL WITH IT                  
*                                                                               
SPERR01  MVC   BMGEERR+1(1),ERRCD                                               
         CLI   ERRCD,NEWERRS                                                    
         BNE   *+10                                                             
         MVC   BMGEERR,NERRCD                                                   
         B     EXX10                                                            
*                                                                               
*&&DO                                                                           
* THIS CODE SHOULD BE INSTALLED, BUT SOMETHING IS SETTING SVXFR_SDT             
* WHEN IT CLEARLY ISN'T IN DT MODE.                                             
SPERR02  TM    SVXFRCTL,SVXFR_SDT  TEST SPOT DESKTOP MODE                       
         BZ    *+6                  NO                                          
         DC    H'0'                DIE UNTIL I FIND A WAY BACK TO BY39          
*&&                                                                             
*                                                                               
SPERR02  NI    VCALLBAS,X'7F'        RESET MODE FLAG                            
         L     R4,ERRAREA                                                       
         CLI   ERRAREA,X'FE'       ABEND?                                       
         BE    *+12                                                             
         CLI   ERRAREA,0           TEST FOR PRESET MESSAGE                      
         BNE   SPERR20                                                          
         CLI   ERRCD,NEWERRS       IS THIS A 2 CHAR ERROR                       
         BNE   SPERR10                                                          
*                                                                               
         XC    WORK,WORK           DEFINE CONTROL BLOCK                         
         LA    R1,WORK                                                          
         USING GETTXTD,R1                                                       
         MVI   GTMTYP,GTMERR       SET MESSAGE TYPE TO ERRO                     
         MVC   GTMSGNO,NERRCD      AND MESSAGE NUMBER                           
         MVI   GTMSYS,3            AND MESSAGE SYSTEM                           
         LA    RE,ERRTEXT          AREA FOR APPENDED ERR TEXT                   
         CLI   0(RE),C' '          TEST IF THERE IS ANY TEXT                    
         BL    *+12                                                             
         ST    RE,GTATXT-1                                                      
         MVI   GTLTXT,L'ERRTEXT                                                 
         DROP  R1                                                               
         L     RF,VCOMFACS                                                      
         L     RF,CGETTXT-COMFACSD(RF)                                          
         GOTO1 (RF),WORK           PUT OUT SYSTEM MESSAGE                       
         XC    ERRTEXT,ERRTEXT                                                  
         B     SPERR12                                                          
         EJECT                                                                  
*                                                                               
SPERR10  DS    0H                  READ SYSTEM 3 MESSAGES                       
         GOTO1 VGETMSG,DMCB+12,(ERRCD,8(R4)),(X'FF',DMCB),(3,0)                 
*                                                                               
SPERR12  FOUT  (R4)                                                             
         CLI   ERRAREA,0                                                        
         BNE   *+8                                                              
         MVI   ERRAREA,X'FF'                                                    
*                                                                               
SPERR20  CLI   ERRNDX,0            TEST CALLER WANTS CURSOR POSITIONING         
         BE    SPERRORX                                                         
         OI    6(R2),X'80'         MAKE SURE ERROR FIELD IS XMITD               
         L     R1,VTIOB                                                         
         USING TIOBD,R1                                                         
         LR    RF,R2                                                            
         S     RF,VTWA                                                          
         STCM  RF,3,TIOBCURD       DISPLACEMENT TO ERROR FIELD                  
         MVC   TIOBCURI,ERRNDX     INDEX INTO ERROR FIELD FOR CURSOR            
         OI    TIOBINDS,TIOBSETC   ACTIVATE CURSOR POSITIONING                  
         DROP  R1                                                               
*                                                                               
SPERRORX DS    0H                                                               
         TM    SVXFRCTL,SVXFR_MAK  TEST CALLED BY MATCHMAKER                    
         BZ    SPERRX2                                                          
         BRAS  RE,XFRETN                                                        
         B     SPERRX4                                                          
*                                                                               
SPERRX2  CLI   ERRAREA,X'FE'       TEST ABEND                                   
         BNE   SPERRX4              NO                                          
         TM    SVXFRCTL,SVXFR_SDT+SVXFR_MAK  IF SBTK OR MM THEN                 
         JNZ   *+2                     NO $ABEND FOR SPOT DESKTOP OR MM         
         DC    H'0',C'$ABEND'                                                   
*                                                                               
SPERRX4  L     RD,BASERD           RETURN TO * BASE *                           
         LM    RE,RC,12(RD)                                                     
         BR    RE                                                               
         EJECT                                                                  
SPANY    CLI   5(R2),0                                                          
         BNE   ANY2                                                             
         MVI   ERRCD,1                                                          
         B     SPERROR                                                          
*                                                                               
ANY2     TM    4(R2),X'10'                                                      
         BZ    SPCOMXIT                                                         
         MVI   ERRCD,3                                                          
         SPACE 2                                                                
SPPACK   BRAS  RE,SUBRPACK                                                      
         XIT1  REGS=(R0,R1)                                                     
*                                                                               
SPMOVE   BRAS  RE,SUBRMOVE                                                      
         B     SPCOMXIT                                                         
         EJECT                                                                  
SPREAD   MVC   COMMAND,=C'DMREAD'                                               
         MVI   GBYACT,C'R'                                                      
         B     SPDIR                                                            
SPSEQ    MVC   COMMAND,=C'DMRSEQ'                                               
         MVI   GBYACT,C'S'                                                      
         B     SPDIR                                                            
SPHIGH   MVC   COMMAND,=C'DMRDHI'                                               
         MVI   GBYACT,C'H'                                                      
         MVC   KEYSAVE,KEY                                                      
         B     SPDIR                                                            
SPADD    MVC   COMMAND,=C'DMADD'                                                
         MVI   GBYACT,C'B'                                                      
                                                                                
* SPWRITE ENTERS WITH A DIR CALL AND A W IN GBYACT *                            
                                                                                
SPDIR    CLI   KEY,X'10'           TEST BUYREC                                  
         BH    SPDIR2              YES                                          
         MVI   GBYACT,0            NO GETBUY CALL REQUIRED                      
         B     SPDIR6                                                           
*                                                                               
SPDIR2   CLI   LOCKFLAG,C'Y'       LOCK TESTED ALREADY                          
         BE    SPDIR4                                                           
         MVI   LOCKFLAG,C'Y'                                                    
         BRAS  RE,TSTLOCK                                                       
         BE    SPDIR4                                                           
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(DATALOCK)                                              
         B     SPERROR                                                          
*                                                                               
SPDIR4   XC    GETBLK+1(L'GETBLK-1),GETBLK+1                                    
         MVC   GBYDMIN,DMINBTS                                                  
         MVC   GBYDMOUT,DMOUTBTS                                                
         LA    RE,KEY                                                           
         ST    RE,GBYKEYIN                                                      
         ST    RE,GBYKEYOT                                                      
         MVC   GBYCOMF,VCOMFACS                                                 
         MVC   GBY1OR2,SV1OR2                                                   
*                                                                               
         GOTO1 VGETBUY,GETBLK                                                   
*                                                                               
         MVC   SVBUYDA,KEY+(BUYKDA-BUYREC)                                      
         B     SPDIRX                                                           
*                                                                               
SPDIR6   GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'SPTDIR',KEY,KEY               
*                                                                               
SPDIRX   TM    8(R1),X'FD'         TEST ALL BUT DELETED                         
         BZ    SPDIRX2             NO ERROR                                     
         CLI   COMMAND+2,C'R'      TEST READ COMMAND                            
         JNE   *+2                 FORCE RECOVERY ON ADD/WRITE ERROR            
*                                                                               
SPDIRX2  MVC   BYTE,DMOUTBTS                                                    
         NC    BYTE,8(R1)                                                       
         BZ    SPCOMXIT                                                         
* DATAMGR ERROR HAS OCCURRED                                                    
         TM    SVXFRCTL,SVXFR_SDT  IF HERE FROM SPOT DESKTOP                    
         BNZ   *+8                  LEAVE THE DAMN ERROR ALONE!                 
         MVI   ERRCD,0                                                          
         B     SPERROR                                                          
*                                                                               
SPRDSTA  MVC   COMMAND,=C'DMREAD'                                               
*                                                                               
SPSTA    GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'STATION',KEY,AREC             
         B     SPDIRX                                                           
*                                                                               
SPGETREC MVC   COMMAND,=C'GETREC'                                               
         MVI   GBYACT,C'G'                                                      
         CLI   KEY,X'10'           TEST BUYREC (KEY MUST BE VALID)              
         BH    *+8                                                              
         MVI   GBYACT,0                                                         
         B     SPFIL                                                            
*                                                                               
SPPUTREC MVC   COMMAND,=C'PUTREC'                                               
         B     *+10                                                             
SPADDREC MVC   COMMAND,=C'ADDREC'                                               
         BRAS  RE,TESTFIL                                                       
         BNE   SPERROR                                                          
         CLI   UPDFLAG,C'Y'                                                     
         BNE   SPCOMXIT                                                         
*                                                                               
SPFIL    CLI   GBYACT,0            TEST BUYREC I/O                              
         BE    SPFIL2                                                           
         XC    GETBLK+1(L'GETBLK-1),GETBLK+1                                    
         MVC   GBYDMIN,DMINBTS                                                  
         MVC   GBYDMOUT,DMOUTBTS                                                
         LA    RE,KEY+14                                                        
         ST    RE,GBYDA                                                         
         MVC   GBYIOA,AREC                                                      
         LA    RE,DMWORK                                                        
         ST    RE,GBYDMWRK                                                      
         MVC   GBYPRDL,DMCB+20                                                  
         MVC   GBYCOMF,VCOMFACS                                                 
         MVC   GBY1OR2,SV1OR2                                                   
*                                                                               
         GOTO1 VGETBUY,GETBLK                                                   
         B     SPFIL4                                                           
*                                                                               
SPFIL2   GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'SPTFILE',KEY+14,AREC,X        
               DMWORK                                                           
*                                                                               
SPFIL4   CLI   8(R1),0             ANY ERRORS?                                  
         BNE   SPFIL10             YES, DON'T SAVE ANY D/A'S                    
*                                                                               
         L     RE,AREC                                                          
         USING BUYREC,RE                                                        
         CLI   BUYKAM,X'10'        TEST BUYREC                                  
         BL    SPFIL10                                                          
         TM    BUYRCNTL,BUYRDEL    TEST DELETED (X'80')                         
         BNZ   SPFIL10                                                          
         DROP  RE                                                               
         MVC   SVBUYDA,DMWORK+4                                                 
*                                                                               
         CLC   =C'GETREC',COMMAND  ONLY SAVE PUTREC/ADDREC                      
         BE    SPFIL10                                                          
         ICM   RE,15,SVDATABP      NEXT AVAIL ENTRY IN DA TABLE                 
         BZ    SPFIL10                                                          
*                                                                               
         OC    0(L'BUYKDA,RE),0(RE) IS THIS OUR FIRST ENTRY?                    
         BZ    SPFIL5                YES                                        
*                                                                               
         CLC   SVBUYDA,0(RE)       MATCH PREVIOUS ENTRY?                        
         BE    SPFIL10              YES                                         
         AHI   RE,L'BUYKDA          NO, BUMP TO NEXT AVAIL ENTRY                
         ST    RE,SVDATABP                                                      
         CLC   =X'FFFFFFFF',0(RE)  END OF TABLE?                                
         JE    *+2                 YES, CHECK TABLE SIZE!                       
*                                                                               
SPFIL5   MVC   0(L'BUYKDA,RE),SVBUYDA                                           
*                                                                               
SPFIL10  TM    8(R1),X'D0'         TEST EOF OR ERROR                            
         BZ    SPDIRX2                                                          
         DC    H'0'                FORCE RECOVERY ON ADD/WRITE ERRORS           
         EJECT                                                                  
*                                                                               
         EJECT                                                                  
** BRANCH ADDRESSES FOR SPCOMUSR **                                             
         ORG   SPCOMUSR                                                         
*                                                                               
         B     BUGETSPL            ** USER 10 **                                
         B     BUBLDDEM            ** USER 11 **                                
         B     BUBLDEL             ** USER 12 **                                
         B     BUMVREC             ** USER 13 **                                
         B     BUTSTGLS            ** USER 14 **                                
*                                                                               
         B     BUFLDVAL            ** USER 1 **                                 
         B     BUSTAPAK            ** USER 2 **                                 
         B     BUDMLKUP            ** USER 3 **                                 
         B     BUCALLED            ** USER 4 **                                 
         B     BUCALLDS            ** USER 5 **                                 
         B     BUCALLCH            ** USER 6 **                                 
         B     BUCHGDT             ** USER 7 **                                 
         B     BUDEMUP             ** USER 8 **                                 
         B     BURECUP             ** USER 9 **                                 
*                                                                               
         B     BLDQLIST            ** USER 15 **                                
         B     BUGOADDS            ** USER 16 **                                
         B     BUSECRET            ** USER 17 **                                
         B     BUCHKSLN            ** USER 18 **                                
         ORG                                                                    
         SPACE 2                                                                
* ON ENTRY                                                                      
*  R2         = A(FIELD HEADER)                                                 
*  FADDR      = A(START OF PREVIOUS STRING)                                     
*  FLAST      = A(LAST FIELD HDR FOR CONT'D INPUT)                              
*  FLEN       = LEN OF PREVIOUS STRING (OR ZERO FIRST TIME)                     
*  FSTOPS(6)  = STOP CHARACTERS (X'00' ENDS LIST)                               
*                                                                               
* ON EXIT                                                                       
*  R2         = POINTS TO CURRENT UNP FLDHDR                                    
*  R3         = UNCHANGED                                                       
*  R4         = A(STRING)                                                       
*  R5         = LEN OF STRING                                                   
*  FSTOP      = STOP CHARACTER                                                  
*  FVAL       = VALIDITY BITS (X'04'=ALPHA, X'08'=NUM)                          
*                                                                               
* LEADING BLANKS ARE IGNORED. FSTOP=X'FF' FOR NO MORE DATA                      
* FLEN=0 CAUSES RE-EDIT OF PREVIOUS FIELD.                                      
*                                                                               
BUFLDVAL DS    0H                  ** USER 1 **                                 
         MVI   FVAL,0              UNSET VALIDITY BITS                          
         L     R4,FADDR                                                         
         LH    R5,FLEN                                                          
         LTR   R5,R5                                                            
         BZ    *+8                                                              
         LA    R5,1(R5)                                                         
         AR    R4,R5                                                            
         ST    R4,FADDR                                                         
* GET END OF DATA ADDRESS                                                       
         SR    R0,R0                                                            
         IC    R0,5(R2)            INPUT LENGTH                                 
         AR    R0,R2                                                            
         AHI   R0,8                                                             
         CR    R4,R0               TEST STILL IN FIELD                          
         BL    FLDVAL20                                                         
FLDVAL2  MVI   FSTOP,X'FF'         NO-SET NO MORE DATA                          
         L     R4,FADDR            BACK UP TO LAST CHAR                         
         BCTR  R4,0                                                             
         SR    R5,R5                                                            
         CLI   0(R4),C'/'          TEST LAST CHAR A SLASH                       
         BE    FLDVAL10             YES                                         
         CLI   0(R4),C','          TEST LAST CHAR A COMMA                       
         BNE   FLDVALX6             NO                                          
                                                                                
* ADVANCE R2 TO NEXT UNP FLDHDR *                                               
* REMEMBER THAT EVEN THOUGH WE EXIT WITH R2 RESET, THE 05 OR 15                 
* PROGRAM MAY NOT BE THE PROGRAM THAT CALLED FLDVAL!                            
                                                                                
FLDVAL10 CLI   BUTRCODE,C'B'       TEST CONT'D ALLOWED (NEW BUY ONLY)           
         BE    FLDVAL12             YES                                         
         TM    SVXFRCTL,SVXFR_SDT+SVXFR_MAK  DESKTOP OR MATCHMAKER?             
         BZ    FLDVALX6             NO                                          
         CLI   BUTRCODE,C'O'       DESKTOP CAN CONTINUE OTO'S                   
         BE    FLDVAL12                                                         
         CLI   BUTRCODE,C'A'       AND ALLOCATIONS                              
         BNE   FLDVALX6                                                         
*                                                                               
FLDVAL12 CLI   ABUYINPH,0          TEST ADVANCED ALREADY                        
         BNE   FLDVAL16                                                         
         ST    R2,ABUYINPH         SAVE ORIGINAL FLDHDR ADDR                    
         MVI   ABUYINPH,X'80'      SET FLAG                                     
*                                                                               
FLDVAL16 LLC   R0,0(R2)                                                         
         AR    R2,R0               ADVANCE TO NEXT FIELD                        
         TM    1(R2),X'20'         TEST PROTECTED                               
         BO    FLDVAL16            YES - SKIP                                   
         CLI   8(R2),C'*'          TEST SKIP                                    
         BE    FLDVAL16            YES                                          
         CLI   5(R2),0             TEST INPUT                                   
         BNE   FLDVAL18            YES                                          
         SR    R2,R0               NO - RESTORE POINTER                         
         B     FLDVALX6                                                         
*                                                                               
FLDVAL18 LA    R4,8(R2)            POINT TO FIRST INPUT                         
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         B     BUFLDVAL            AND SCAN DATA                                
*                                                                               
FLDVAL20 CLI   0(R4),C' '                                                       
         BE    *+12                                                             
         CLI   0(R4),0                                                          
         BNE   FLDVAL22                                                         
         LA    R4,1(R4)                                                         
         CR    R4,R0                                                            
         BL    FLDVAL20                                                         
         B     FLDVAL2             GO SET NO MORE DATA                          
*                                                                               
FLDVAL22 MVI   FVAL,X'0C'          SET VALIDITY BITS                            
         ST    R4,FADDR            SET CURRENT ADDR AS FIELD START              
*                                                                               
FLDVAL30 LA    R5,FSTOPS                                                        
         LA    R6,6                                                             
FLDVAL40 CLI   0(R5),0             TEST NO MORE STOP CHARS                      
         BE    FLDVAL50                                                         
         CLC   0(1,R4),0(R5)       MATCH STOP CHAR                              
         BNE   *+14                NO                                           
         MVC   FSTOP,0(R5)                                                      
         B     FLDVALX                                                          
         LA    R5,1(R5)            NEXT STOP CHAR                               
         BCT   R6,FLDVAL40                                                      
         EJECT                                                                  
FLDVAL50 CLI   0(R4),C'A'                                                       
         BL    *+12                                                             
         CLI   0(R4),C'Z'                                                       
         BNH   *+8                                                              
         NI    FVAL,X'FB'          NOT ALPHA                                    
         CLI   0(R4),C'0'                                                       
         BL    *+12                                                             
         CLI   0(R4),C'9'                                                       
         BNH   *+8                                                              
         NI    FVAL,X'F7'          NOT NUMERIC                                  
*                                                                               
FLDVAL60 LA    R4,1(R4)            NEXT CHAR                                    
         CR    R4,R0               TEST STILL IN FIELD                          
         BL    FLDVAL30            YES                                          
         MVI   FSTOP,0             INDICATE NO STOP CHAR                        
*                                                                               
FLDVALX  LR    R5,R4                                                            
         S     R5,FADDR            END-START GIVES LENGTH                       
         BNZ   *+8                                                              
         MVI   FVAL,0              RESET VALIDITY BITS IF LEN=0                 
         STH   R5,FLEN                                                          
         LTR   R5,R5                                                            
         BZ    FLDVALXX                                                         
         BCTR  R4,0                POINT TO LAST DATA CHAR                      
* ELIMINATE TRAILING BLANKS                                                     
FLDVALX2 CLI   0(R4),C' '                                                       
         BE    FLDVALX4                                                         
         CLI   0(R4),0                                                          
         BNE   FLDVALX6                                                         
FLDVALX4 BCTR  R4,0                BACK UP TO PREVIOUS DATA CHAR                
         BCT   R5,FLDVALX2          AND ADJUST LENGTH                           
*                                                                               
FLDVALX6 STH   R5,FLEN                                                          
         L     R4,FADDR                                                         
         LTR   R5,R5                                                            
         BZ    FLDVALXX                                                         
         TM    FVAL,X'08'          TEST NUMERIC                                 
         BZ    FLDVALXX            NO                                           
         L     RE,FADDR                                                         
         LH    RF,FLEN                                                          
         BCTR  RF,0                                                             
         EX    RF,*+8              PACK VALID NUM FIELD INTO DUB                
         B     *+10                                                             
         PACK  DUB,0(0,R4)  **EXECUTED**                                        
*                                                                               
FLDVALXX XIT1  REGS=(R2,R5)                                                     
         EJECT                                                                  
*-------------------------------------------------------*                       
* PROVIDE LINKAGE TO STAPAK MODULE                      *                       
*                                                       *                       
* FOR MSPACK FUNCTION (HOB OF P1 = P)                   *                       
* R1 POINTS TO A(MKT)/A(STA)/A(MKTSTA)                  *                       
*                                                       *                       
* FOR MSUNPK FUNCTION (HOB OF P1 = U)                   *                       
* R1 POINTS TO A(MKTSTA)/A(MKT)/A(STA)                  *                       
* ALSO, P3 = X'80' FOR     8 BYTE OUTPUT FROM MSUNPK    *                       
*-------------------------------------------------------*                       
         SPACE 1                                                                
BUSTAPAK DS    0H                  *** USER 2 ***                               
         L     R4,=A(STAWORK-SPBUYWKD)                                          
         AR    R4,RC               POINT TO STAWORK                             
         XC    0(L'STAWORK,R4),0(R4)                                            
         USING STAPACKD,R4                                                      
*                                                                               
         MVC   STAPACT,0(R1)                                                    
         MVC   STAPAGY,AGYALPHA                                                 
         MVC   STAPCTRY,SVAPROF+7                                               
         MVC   STAPMED,BUYMD                                                    
         MVC   STAPACOM,VCOMFACS                                                
*                                                                               
         CLI   0(R1),C'P'                                                       
         BNE   BUSTAP10                                                         
* MSPACK CALL                                                                   
         LM    R5,R7,0(R1)         GET A(MKT)/A(STA)/A(MKTSTA)                  
         MVC   STAPQMKT,0(R5)      MARKET                                       
         MVC   STAPQSTA(8),0(R6)   STATION                                      
         GOTO1 VSTAPACK,(R4)                                                    
*                                                                               
         MVI   ERRCD,CBLNETER                                                   
         CLI   STAPERR,QSTP_INVCBL                                              
         BE    SPERROR                                                          
*                                                                               
         CLI   STAPERR,0                                                        
         BNE   BUSTPERR                                                         
*                                                                               
         MVC   0(5,R7),STAPMKST    RETURN RESULT                                
         MVC   SVSTVRSN,STAPVRSN   SAVE VERSION                                 
         B     SPCOMXIT                                                         
*                                                                               
BUSTAP10 CLI   0(R1),C'U'                                                       
         JNE   *+2                                                              
* MSUNPK CALL                                                                   
         LM    R5,R7,0(R1)         GET A(MKTSTA)/A(MKT)/A(STA)                  
         MVC   STAPMKST,0(R5)                                                   
*                                                                               
         GOTO1 VSTAPACK,(R4)                                                    
*                                                                               
         CLI   STAPERR,0                                                        
         BE    BUSTAP12                                                         
BUSTPERR LA    R2,BUYSTH                                                        
         NI    4(R2),X'DF'         SET STATION INVALID                          
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(BADSTA)                                                
         B     SPERROR                                                          
*                                                                               
BUSTAP12 MVC   0(4,R6),STAPQMKT                                                 
         MVC   0(5,R7),STAPQSTA    MOVE 5 CHARS FOR COMPAT                      
         LTR   R7,R7               X'80' MEANS MOVE 8 CHARS                     
         BNM   *+10                                                             
         MVC   0(8,R7),STAPQSTA                                                 
         B     SPCOMXIT                                                         
         DROP  R4                                                               
         EJECT                                                                  
*================================================================               
* PROVIDE DEMO LOOK UP LINKAGE - USER PARAMS ARE IN DMCB                        
*================================================================               
                                                                                
BUDMLKUP DS    0H                  ** USER 3 **                                 
         CLI   BUYMD,C'X'          FUGGEDABOUDIT                                
         BE    SPCOMXIT                                                         
         BRAS  RE,DMLKUP                                                        
         B     SPCOMXIT                                                         
         EJECT                                                                  
BUCALLED MVI   DUB,X'11'            ** USER 4 **                                
         CLI   EDTVAL,PGMEDT                                                    
         BE    BUCALLE2                                                         
         CLI   EDTVAL,ADJEDT                                                    
         BE    BUCALLE2                                                         
         CLI   EDTVAL,REFEDT                                                    
         BE    BUCALLE2                                                         
         CLI   EDTVAL,PCDEDT                                                    
         BE    BUCALLE2                                                         
         CLI   EDTVAL,DSKEDT                                                    
         BE    BUCALLE2                                                         
*                                                                               
         MVI   DUB,X'10'                                                        
*                                                                               
BUCALLE2 DS    0H                                                               
         BAS   RE,BUOVLY                                                        
         L     RF,DUB                                                           
         GOTO1 (RF),DMCB,(RC)                                                   
         XIT1  REGS=(R2)           R2 MAY BE UPDATED BY FLDVAL                  
         EJECT                                                                  
BUCALLDS DS    0H                  ** USER 5 **                                 
         CLI   RCLOPT,35           TEST VALUE BEYOND TABLE                      
         BH    BUDS2                                                            
         ZIC   RE,RCLOPT                                                        
         LA    RE,RCLTABLE(RE)                                                  
         TM    0(RE),X'80'         TEST GO DIRECT TO SECOND OVLY                
         BO    BUDS4                                                            
*                                                                               
BUDS2    MVI   DUB,X'20'                                                        
         CLI   RCLOPT,C'S'         TEST SORT ACTIVE                             
         BNE   BUDS2X                                                           
         CLI   SVDSPMOD,0          TEST SKED MODE                               
         BE    BUDS2X              NO                                           
         MVI   DUB,X'23'           THEN GO DIRECTLY TO SPBUY23                  
         B     BUDS8                AND EXIT                                    
*                                                                               
BUDS2X   BAS   RE,BUOVLY                                                        
         L     RF,DUB                                                           
         GOTO1 (RF),DMCB,(RC)                                                   
*                                                                               
* TEST NEED SECOND DISPLY MODULE                                                
*                                                                               
BUDS4    CLI   RCLOPT,35           TEST VALUE BEYOND TABLE                      
         BH    SPCOMXIT            YES - IGNORE                                 
         ZIC   RE,RCLOPT                                                        
         IC    RE,RCLTABLE(RE)                                                  
         LTR   RE,RE                                                            
         BZ    SPCOMXIT                                                         
*                                                                               
BUDS6    STC   RE,DUB                                                           
*                                                                               
BUDS8    NI    DUB,X'7F'           DROP HOB                                     
         BAS   RE,BUOVLY                                                        
         L     RF,DUB                                                           
         GOTO1 (RF),DMCB,(RC)                                                   
         B     SPCOMXIT                                                         
         SPACE 1                                                                
* BYTES GIVE OVERLAY NUMBER OF ADDITIONAL DISPLAY LOGIC                         
* X'80' MEANS DO NOT CALL SPBUY20 FIRST                                         
         SPACE 1                                                                
RCLTABLE DS    0F                                                               
         DC    X'00000000'  00     .../ROT/PAY/PAYX                             
         DC    X'21222222'  04     INV/REF/COM/DEM                              
         DC    X'22222222'  08     ACTV/ORB/SPL/STA                             
         DC    X'21212121'  0C     HUT/PCD/INT/INTX                             
         DC    X'21212100'  10     FLM/NTCD/HMS/SCH                             
         DC    X'00212222'  14     .../RSV/DEMS/PD                              
         DC    X'220021A2'  18     PDX/XCH/DROT/NET=DIS                         
         DC    X'2100A2A2'  1C     DT/CUT/CLST/NLST                             
         DC    X'00A2A2A3'  20     HIST/.../LKIN/SKVL                           
* REMEMBER TO CHANGE TEST ABOVE IF YOU ADD ANOTHER ENTRY HERE                   
* CLI  RCLOPT ... (TEST VALUE BEYOND TABLE)                                     
         SPACE 2                                                                
BUCALLCH MVC   DUB(1),BUCHGOV      ** USER 6 **                                 
         BAS   RE,BUOVLY                                                        
         L     RF,DUB                                                           
         GOTO1 (RF),DMCB,(RC)                                                   
         B     SPCOMXIT                                                         
         EJECT                                                                  
BUOVLY   LR    R0,RE              *** BASELESS SUBROUTINE ***                   
         CLC   BUYOVLY(1),DUB                                                   
         JNE   *+12                                                             
         MVC   DUB(4),BUYOVLY                                                   
         BR    RE                                                               
*                                                                               
         XC    DUB+1(3),DUB+1                                                   
         MVC   DUB+4(4),VBUYTWA                                                 
         GOTO1 VCALLOV,DUB                                                      
         CLI   4(R1),X'FF'                                                      
         JE    *+2                                                              
         MVC   BUYOVLY,DUB                                                      
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
BUCHGDT  DS    0H                  ** USER 7 **                                 
         GOTO1 VDATCON,DMCB,(5,0),(3,FULL)                                      
         OC    BDWHY,BUWHY                                                      
         OC    BDWHY2,BUWHY2                                                    
         OC    BDWHY3,BUWHY3                                                    
         CLC   BDCHG,FULL                                                       
         BE    SPCOMXIT                                                         
         MVC   BDWHY,BUWHY                                                      
         MVC   BDWHY2,BUWHY2                                                    
         MVC   BDWHY3,BUWHY3                                                    
         MVC   BDCHG,FULL                                                       
         B     SPCOMXIT                                                         
         EJECT                                                                  
*=================================================================              
* THIS ROUTINE IS CALLED WHEN THE UPGRADE VALUES HAVE BEEN EDITED               
* AND SPDEMUPD HAS BEEN BUILT IN DBLOCK ALREADY                                 
* IF DMLKUP IS CALLED, THE VALUES ARE EXTRACTED FROM THE 62 ELEM                
*=================================================================              
                                                                                
BUDEMUP  DS    0H                  ********  USER 8   ********                  
         CLI   BUYMD,C'T'          SPOT TV?                                     
         JE    DEMUP1                OR                                         
         CLI   BUYMD,C'R'          CANADIAN RADIO?                              
         JNE   SPCOMXIT                                                         
         CLI   SVAPROF+7,C'C'                                                   
         JNE   SPCOMXIT                                                         
*                                                                               
DEMUP1   CLI   BDDAYPT,C'@'        TEST INTERNET                                
         BE    SPCOMXIT                                                         
         XC    SVSPLMKT,SVSPLMKT                                                
         L     R1,=A(STAWORK-SPBUYWKD)                                          
         AR    R1,RC               POINT TO STAWORK                             
         LAY   RE,BUUPGD                                                        
         MVC   0(L'UPINPUT,R1),0(RE)  SAVE UPGRADE INPUT TEXT                   
         MVI   BUDEMSW,0           SET DEMO ELEMENT NOT BUILT YET               
         XC    BUDEMS,BUDEMS       CLEAR DEMO VALUES                            
*                                                                               
DEMUP2   L     R8,ADBLOCK                                                       
         USING SPDEMUPD,R8                                                      
         MVC   SPUPUID,T211FFD+10  MOVE USERID                                  
         MVC   SPUPLPM,SVLPMDAT    SET LPM START DATE                           
         MVC   SPUPMALF,SVMKTMKT   SET MKT ALPHA                                
*                                                                               
         LAY   RF,SV00APRF+6                                                    
         CLI   0(RF),C'Y'          TEST 2-DECIMAL IMPS                          
         JNE   DEMUP2A                                                          
         OI    SPUPOPT2,SPOP2IPR   REQUEST 2-DEC IMPS AND                       
         J     DEMUP2B              2-DEC RTG THEN                              
*                                                                               
DEMUP2A  CLI   SV00PROF+9,C'Y'     TEST 2-DECIMAL RATINGS                       
         JNE   DEMUP2C                                                          
DEMUP2B  OI    SPUPOPTS,SPOP2DEC   REQUEST 2-DEC RTG THEN                       
*                                                                               
DEMUP2C  MVC   SPUPSTA,QSTA                                                     
         XC    SPUPSYSE,SPUPSYSE                                                
*                                                                               
         CLI   SVKEY+6,X'E8'       TEST CABLE                                   
         BL    *+16                                                             
         MVC   SPUPSYSE,QSTA       SET SYSCODE                                  
         MVC   SPUPSTA,QCBLNET     AND CABLE NETWORK                            
*                                                                               
DEMUP3   OC    SPUPSPL,SPUPSPL     DON'T DO THIS FOR SPILL                      
         BNZ   DEMUP6                                                           
         CLI   SPUPBTYP,0          TEST PROJ HAS BOOKTYPE                       
         BH    DEMUP4               YES                                         
*                                                                               
         MVC   SPUPBTYP,BUBKTYPE                                                
         CLI   BUBKTYPE,0          USER ENTERED "C,BT="                         
         BNE   DEMUP4              ALWAYS WINS                                  
         MVC   SPUPBTYP,SVOPTBT                                                 
         CLI   SVOPTBT,0           USER ENTERED BT= IN OPTIONS                  
         BNE   DEMUP4              ALWAYS WINS                                  
*                                                                               
         MVI   ELCDLO,X'24'        CHECK LKUP ELEM                              
         MVI   ELCDHI,X'24'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   DEMUP3A                                                          
         MVC   SPUPBTYP,2(R6)                                                   
         CLI   2(R6),0             TEST BT IN LKUP OVERRIDE ELEM?               
         BH    DEMUP6                                                           
*                                                                               
DEMUP3A  MVC   SPUPBTYP,SVEBKTYP                                                
         CLI   SVEBKTYP,0          TEST EST BOOKTYPE                            
         BH    DEMUP4                                                           
         MVC   SPUPBTYP,SVBKTYPE   NO THEN STATION/MARKET BOOKTYPE              
         CLI   SVBKTYPE,0          TEST EST BOOKTYPE                            
         BH    DEMUP4                                                           
         B     DEMUP6                                                           
                                                                                
DEMUP4   MVI   ELCDLO,X'24'        UPDATE LKUP ELEM                             
         MVI   ELCDHI,X'24'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   DEMUP4X                                                          
         MVC   ELEM+2(1),SPUPBTYP  UPDATE THE ELEMENT                           
         B     DEMUP6                                                           
*                                                                               
DEMUP4X  XC    ELEM,ELEM                                                        
         MVC   ELEM(2),=X'2406'                                                 
         MVC   ELEM+2(1),SPUPBTYP                                               
*                                                                               
         GOTO1 RECUP,DMCB,BUYREC,ELEM,(R6)                                      
                                                                                
DEMUP6   MVI   BUDLUSW,C'N'        INHIBIT X'24' UPDATE BY BLDDEM               
         GOTO1 VBLDDEM             BUILD CURRENT DEMO ELEMENT                   
         MVI   BUDEMSW,0           AND INDICATE FIRST TIME FOR BELOW            
*                                                                               
         MVC   SPUPAREC,AREC2                                                   
         MVC   SPUPAFAC,VCOMFACS                                                
         MVC   SPUPAGY,AGYALPHA                                                 
*                                                                               
         MVC   SPUPMED,BUYMD                                                    
         CLI   SVAPROF+7,C'C'                                                   
         BNE   DEMUP8                                                           
         CLI   BUYMD,C'T'                                                       
         BNE   DEMUP8                                                           
         MVI   SPUPMED,C'C'        FOR CANADA, SPOTTV=C                         
*                                                                               
DEMUP8   MVC   SPUPCLI,QCLT                                                     
*                                                                               
         OC    SPUPSPL,SPUPSPL                                                  
         BNZ   DEMUP10                                                          
*                                                                               
         MVI   SPUPSRC,C'N'        FORCE RTGSVC TO NSI FOR US                   
*                                                                               
         MVI   ELCDLO,X'24'        CHECK FOR DEMO LOOKUP ELEMENT                
         MVI   ELCDHI,X'24'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   DEMUP10                                                          
*                                                                               
         USING DLUELEM,R6                                                       
         CLI   DLUBKTYP,0                                                       
         BNE   *+10                                                             
         MVC   SPUPBTYP,DLUBKTYP                                                
*                                                                               
         CLI   DLUBAMKT,C'A'                                                    
         BL    *+10                                                             
         MVC   SPUPMALF,DLUBAMKT                                                
*                                                                               
         CLI   DLUBSTOV,C'A'                                                    
         BL    *+14                                                             
         MVC   SPUPSTA,DLUBSTOV                                                 
         MVI   SPUPSTA+4,C'T'                                                   
*                                                                               
         TM    DLUBFLGS,X'01'      TEST BBM FOR CANADA                          
         BZ    *+8                                                              
         MVI   SPUPSRC,C'A'                                                     
*                                                                               
         TM    DLUBFLGS,X'02'      TEST NSI FOR CANADA                          
         BZ    *+8                                                              
         MVI   SPUPSRC,C'N'                                                     
                                                                                
*                                                                               
DEMUP10  CLI   SV1WPROF+5,C'I'     NSI/USTV EXTENDED PRECISION                  
         BNE   *+8                                                              
         OI    SPUPOPTS,X'08'      SET EXTENDED PRECISION                       
*                                                                               
         CLI   SV1WPROF+7,C'Y'     NSI/USTV NORMALIZED IMPS                     
         BNE   *+8                                                              
         OI    SPUPOPTS,SPOPNORM   SET NORMALIZED IMPS                          
*                                                                               
         XC    ELEM,ELEM           MOVE DEMO LIST AND SET EOL FLAG              
         MVC   ELEM(60),SVDEMOS                                                 
         LA    RE,ELEM                                                          
         CLI   1(RE),0                                                          
         BE    *+12                                                             
         LA    RE,3(RE)                                                         
         B     *-12                                                             
         MVI   0(RE),X'FF'                                                      
*                                                                               
         XC    BUDEMS(240),BUDEMS  CLEAR OLD VALUES - THANKS TIM                
*                                                                               
         MVC   DUB,SPUPTYPE        SAVE SPUPTYPE, DEMUP CLOBBERS IT             
*                                                                               
         GOTO1 VCALLOV,DMCB,0,X'D9000A22'                                       
         CLI   4(R1),X'FF'                                                      
         JE    *+2                                                              
         L     RF,0(R1)            GET DEMUP ADDRESS                            
         GOTO1 (RF),(R1),ADBLOCK,ELEM,BUDEMS                                    
*                                                                               
         MVC   SPUPTYPE(8),DUB     RESTORE SPUPTYPE                             
*                                                                               
         OC    SPUPSPL,SPUPSPL     IF DOING SPILL                               
         BNZ   DEMUP12             IGNORE ERRORS                                
*                                                                               
         CLI   SPUPMED,C'C'        TEST CANADA                                  
         BE    DEMUP12             YES - IGNORE ERRORS FOR NOW                  
*                                                                               
         MVI   ERRCD,NOUPDEMS                                                   
         CLI   0(R1),X'FF'         NO DEMOS FOUND?                              
         BE    DEMUP12             YES, ALLOW IF ALL OVERRIDES/COMSCORE         
         MVI   ERRCD,NOBOOK                               -HWON 9/15/16         
         CLI   0(R1),X'80'         NO BOOK FOUND?                               
         BE    DEMUP12             YES, ALLOW IF ALL OVERRIDES/COMSCORE         
         MVI   ERRCD,NORTGSVC                                                   
         CLI   0(R1),X'FE'         NO H/P/T?                                    
         BE    DEMUP12             YES, ALLOW IF ALL OVERRIDES/COMSCORE         
         CLI   0(R1),0                                                          
         BNE   SPERROR                                                          
         MVI   ERRCD,0             RESET IT, SO WE DONT FAIL TEST LATER         
*                                                         -HWON 9/15/16         
* INSERT UPGRADE VALUES IN DEMO ELEMENT                                         
*                                                                               
         USING NDELEM,R6                                                        
DEMUP12  LA    R6,BDELEM           FIND DEMO ELEM                               
         MVI   ELCDLO,X'02'                                                     
         MVI   ELCDHI,X'02'                                                     
         BRAS  RE,NEXTEL                                                        
         JNE   *+2                                                              
         OC    SPUPSPL,SPUPSPL     TEST DOING SPILL                             
         BNZ   DEMUP13             YES                                          
         MVC   NDPROG,SPUPPRG      MOVE PROGRAM NAME                            
         B     DEMUP13B                                                         
*                                                                               
DEMUP13  MVI   ELCDLO,X'03'                                                     
         MVI   ELCDHI,X'03'                                                     
DEMUP13A BRAS  RE,NEXTEL                                                        
         JNE   *+2                                                              
         CLC   SPUPSPL,NDRSMKT     MATCH PREVIOUS                               
         BNE   DEMUP13A                                                         
*                                                                               
DEMUP13B MVC   NDBOOK,SPUPFBK      SET SHARE BOOK SOURCE                        
         DROP  R6                                                               
*                                                                               
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AHI   R0,-24                                                           
         BNP   SPCOMXIT                                                         
*                                  IF DARE MG AND SKIP UPG ERROR                
         TM    WRKRUPSW,WRKRUPSW_NODMER+WRKRUPSW_ISDRMG                         
         BO    DEMUP20             YES - DON'T BOTHER SETTING                   
*                                                                               
         SRL   R0,3                                                             
         LA    R6,24(R6)                                                        
*                                                                               
DEMUP14  CLI   2(R6),0             TEST NONT DEMO CATEGORY                      
         BE    DEMUP15              YES, SKIP IT, DOESN'T REQ. VALUE            
*                                                         -HWON 4/12/18         
         TM    4(R6),X'80'         TEST OVERRIDE PRESENT                        
         BO    DEMUP15             -YES, SKIP SETTING LOOKUP VALUE              
*                                                                               
         CLI   ERRCD,0             -NO, DID WE HAVE A LOOKUP ERROR?             
         BNE   SPERROR              -YES, ERROR, NO LOOKUP VALUE TO SET         
*                                                         -HWON 9/15/16         
         BAS   RE,GETVAL                                                        
*                                                                               
DEMUP15  LA    R6,8(R6)                                                         
         BCT   R0,DEMUP14                                                       
         B     DEMUP20                                                          
*                                                                               
GETVAL   NTR1                                                                   
         LA    RE,ELEM             POINT TO DEMO CODES                          
         LA    RF,BUDEMS           POINT TO DEMO VALUES                         
*                                                                               
GETVAL2  CLC   0(3,R6),0(RE)       MATCH DEMO                                   
         BE    GETVAL4                                                          
         LA    RE,3(RE)            NEXT CODE                                    
         LA    RF,4(RF)            NEXT VALUE                                   
         CLI   0(RE),X'FF'                                                      
         BNE   GETVAL2                                                          
         B     SPCOMXIT                                                         
*                                                                               
GETVAL4  MVI   3(R6),100           FORCE SVI                                    
         MVC   4(4,R6),0(RF)       SET DEMO VALUE                               
         B     SPCOMXIT                                                         
         EJECT                                                                  
*===========================================================                    
* CHECK FOR SPILL DEMO ELEMENTS                                                 
*===========================================================                    
                                                                                
DEMUP20  MVI   ELCDLO,X'03'                                                     
         MVI   ELCDHI,X'03'                                                     
         LA    R6,BDELEM                                                        
*                                                                               
         USING NDELEM,R6                                                        
DEMUP22  BRAS  RE,NEXTEL                                                        
         BNE   DEMUP30                                                          
         OC    SVSPLMKT,SVSPLMKT                                                
         BZ    DEMUP24                                                          
         CLC   SVSPLMKT,NDAGYMKT   MATCH PREVIOUS                               
         BNE   DEMUP22                                                          
         BRAS  RE,NEXTEL           TRY FOR ANOTHER                              
         BNE   DEMUP30                                                          
*                                                                               
DEMUP24  MVC   SVSPLMKT,NDAGYMKT   SET AGENCY MKT FOR BLDDEM                    
         MVC   SPUPSPL,NDRSMKT     SET SPILL R/S MKT                            
         MVC   SPUPMALF,NDMKTALF   SET SPILL MKT ALPHA                          
         DROP  R6                                                               
*                                                                               
         CLI   SPUPTYPE,C'R'       TEST RATING UPGRADE                          
         BNE   DEMUP26                                                          
         MVC   BUDEMS+240(8),SPUPTYPE   SAVE OLD UPGRADE EXPRESSION             
         XC    SPUPTYPE,SPUPTYPE                                                
         MVC   SPUPTYPE(4),=X'04000064' FORCE IX/100                            
*                                                                               
DEMUP26  B     DEMUP2                                                           
         EJECT                                                                  
* BUILD AND INSERT UPGRADE DESCRIPTION ELEMENT *                                
         SPACE 1                                                                
DEMUP30  XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING UPELEM,R6                                                        
         MVI   ELEM,X'62'                                                       
         MVI   ELEM+1,UPELEMLN                                                  
         MVC   UPFILE,SPUPFIL                                                   
         MVC   UPSRC,SPUPSRC                                                    
         MVC   UPPUR,SPUPPUR                                                    
         MVC   UPFBK,SPUPFBK                                                    
         MVC   UPFBKLST,SPUPFBKL                                                
         MVC   UPDAYTIM,SPUPUDAY                                                
         MVC   UP2YRP,SPUP2YRP                                                  
         MVC   UP2YRS,SPUP2YRR                                                  
         CLC   QSTA,SPUPSTA        TEST OVRD STAT = ACTL STAT                   
         BE    *+10                                                             
         MVC   UPSTA,SPUPSTA       IF NOT, SAVE STATION OVERRIDE                
         MVC   UPTYPE,SPUPTYPE                                                  
         L     R1,=A(STAWORK-SPBUYWKD)                                          
         AR    R1,RC               POINT TO STAWORK                             
         MVC   UPINPUT,0(R1)       UPGRADE INPUT TEXT SAVED HERE                
*                                                                               
         MVI   ELCDLO,X'62'                                                     
         MVI   ELCDHI,X'62'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   DEMUP40                                                          
         BRAS  RE,ELEMDEL                                                       
*                                                                               
DEMUP40  BRAS  RE,ELEMADD                                                       
         XC    SVSPLMKT,SVSPLMKT   CLEAR BEFORE EXIT!                           
         B     SPCOMXIT                                                         
         DROP  R6                                                               
         EJECT                                                                  
* INTERCEPT CALLS TO RECUP TO MAKE SURE                                         
* RECSIZE WILL NOT EXCEED MAX.                                                  
*                                                                               
BURECUP  DS    0H                  ** USER 9 **                                 
         L     R8,0(R1)            A(REC)                                       
         SR    R0,R0                                                            
         ICM   R0,3,13(R8)                                                      
         C     R8,4(R1)            IF A(REC) GT P2, MUST BE INSERTION           
         BH    BURECUP2                                                         
         AR    R8,R0                                                            
         C     R8,4(R1)            IF A(RECX) GT P2, MUST BE DELETION           
         BH    BURECUP4                                                         
* INSERTION                                                                     
BURECUP2 L     RE,4(R1)            A(NEW ELEM)                                  
         ZIC   RF,1(RE)            GET LEN                                      
         AR    R0,RF                                                            
         LHI   RF,3972                                                          
         CLI   SVBIGMAX,C'Y'                                                    
         BNE   *+8                                                              
         LHI   RF,5972                                                          
         CR    R0,RF               COMPARE TO MAX LEN                           
         BNH   BURECUP4                                                         
         MVI   ERRCD,MAXSZERR                                                   
         B     SPERROR                                                          
*                                                                               
BURECUP4 GOTO1 SVRECUP,(R1)                                                     
         L     R8,0(R1)            A(REC)                                       
         SR    R0,R0                                                            
         ICM   R0,3,13(R8)                                                      
         AR    R8,R0                                                            
         XC    0(2,R8),0(R8)       MAKE SURE 2 BYTES AFTER REC = 0000           
         B     SPCOMXIT                                                         
         EJECT                                                                  
BUGETSPL DS    0H                  ** USER 10 **                                
         BRAS  RE,GETSPL                                                        
         B     SPCOMXIT                                                         
         SPACE 1                                                                
BUBLDDEM DS    0H                  ** USER 11 **                                
         CLI   BUDEMSW,0           TEST DONE PREVIOUSLY                         
         BNE   SPCOMXIT                                                         
         MVI   BUDEMSW,X'DD'                                                    
         BRAS  RE,BLDDEM                                                        
         B     SPCOMXIT                                                         
         EJECT                                                                  
* CREATE PROTOTYPE ELEMENT - N.B. USES BUELPRD *                                
         SPACE 1                                                                
BUBLDEL  DS    0H                   ** USER 12 **                               
         XC    ELEM,ELEM                                                        
         MVI   ELEM,6                                                           
         MVI   ELEM+1,10                                                        
         CLI   BDTIME,0            TEST P/B                                     
         BE    *+8                                                              
         MVI   ELEM+1,12                                                        
         MVC   ELEM+7(1),BDNOWK                                                 
         CLI   BUYKEY+3,X'FF'                                                   
         BNE   BLDELX                                                           
* POL                                                                           
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'0B'                                                       
         MVI   ELEM+1,10                                                        
         LA    R1,SVPOLNPW                                                      
         CLI   BUTRCODE,C'B'       NEW BUY                                      
         BE    *+8                                                              
         LA    R1,BDSTAT                                                        
         TM    0(R1),X'80'                                                      
         BNO   *+16                                                             
         IC    R0,BDNOWK                                                        
         SLL   R0,2                USE 6 BITS FOR NPW                           
         STC   R0,ELEM+7                                                        
         CLI   BUELPRD,0                                                        
         BE    BLDELX                                                           
         MVI   ELEM+1,14                                                        
         MVC   ELEM+10(1),BUELPRD                                               
         MVC   ELEM+11(1),BDSEC                                                 
*                                                                               
         CLI   BUELPR2,0           TEST P/B                                     
         BE    BLDELX              NO                                           
*                                                                               
         MVI   ELEM+1,18                                                        
         MVC   ELEM+6(1),BUELPRSW                                               
         MVC   ELEM+14(1),BUELPR2                                               
*                                                                               
         CLI   BUELSEC,0           TEST UNEQ SPLIT ENTERED                      
         BE    BLDEL2              NO                                           
         MVC   ELEM+11(1),BUELSEC                                               
         MVC   ELEM+15(1),BUELSEC2                                              
         B     BLDELX                                                           
*                                                                               
BLDEL2   MVI   ERRCD,ODDSECS                                                    
         TM    BDSEC,X'01'         TEST SLN IS ODD                              
         BO    SPERROR                                                          
*                                                                               
         MVI   ERRCD,SLNERR                                                     
         SR    R0,R0                                                            
         IC    R0,BDSEC                                                         
         SRL   R0,1                                                             
         LTR   R0,R0                                                            
         BZ    SPERROR                                                          
         STC   R0,ELEM+11                                                       
         STC   R0,ELEM+15                                                       
*                                                                               
         STC   R0,BYTE                                                          
         BRAS  RE,CHKSLN                                                        
         BE    SPERROR             EQ MEANS INVALID SLN                         
         EJECT                                                                  
* FIND AN ALLOCATED ELEM TO CHECK MASPRD IS EVEN SPLIT                          
*                                                                               
BLDEL4   MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0B'                                                     
         LA    R6,BDELEM                                                        
BLDEL6   BRAS  RE,NEXTEL                                                        
         BNE   BLDELX                                                           
         CLI   1(R6),18                                                         
         BNE   BLDEL6                                                           
         CLC   ELEM+10(1),10(R6)                                                
         BNE   BLDEL6                                                           
         CLC   ELEM+14(1),14(R6)                                                
         BNE   BLDEL6                                                           
         MVC   ELEM+11(1),11(R6)                                                
         MVC   ELEM+15(1),15(R6)                                                
*                                                                               
BLDELX   B     SPCOMXIT                                                         
         EJECT                                                                  
BUTSTGLS DS    0H                  ***** USER 14 *****                          
         TM    SVOPT2,X'20'        TEST OPTION TO IGNORE GOALS                  
         BO    SPCOMXIT                                                         
         CLI   SVCXTRA+8,C'Y'      TEST OPTION ON AT ALL                        
         BE    *+12                                                             
         CLI   SVCXTRA+8,C'P'      TEST PG GOALS                                
         BNE   SPCOMXIT                                                         
         TM    WRKRUPSW,WRKRUPSW_ISDRMG                                         
         BNZ   SPCOMXIT            COMING FROM DARE DON'T CHECK GOALS           
* CALL NTR'D ROUTINE                                                            
         BRAS  RE,BUTSTG                                                        
         B     SPCOMXIT                                                         
         EJECT                                                                  
*============================================================*                  
* SUBROUTINE TO BUILD LIST OF PRODUCTS APPEARING IN BUYREC   *                  
* FOR BRAND REQUESTS ON TRUE POL BUYING                      *                  
*============================================================*                  
         SPACE 1                                                                
BLDQLIST DS    0H                  ** USER 15 **                                
         CLI   SVCPROF+0,C'0'      TEST TRUE POL BUYING                         
         BNE   SPCOMXIT                                                         
         CLI   SVCPROF+12,C'Y'     TEST T/A BY PRODUCT ENABLED                  
         BNE   SPCOMXIT                                                         
         CLI   BDMASPRD,0          TEST MASPRD PRESENT                          
         BE    BLDQ2               NO                                           
         ZIC   R4,BDMASPRD                                                      
         BAS   RE,SETQBIT                                                       
         SR    R4,R4                                                            
         ICM   R4,1,BDMASPRD+1                                                  
         BZ    *+8                                                              
         BAS   RE,SETQBIT                                                       
         B     SPCOMXIT                                                         
*                                                                               
BLDQ2    LA    R6,BDELEM                                                        
         MVI   ELCDLO,11                                                        
         MVI   ELCDHI,13                                                        
*                                                                               
BLDQ4    BRAS  RE,NEXTEL                                                        
         BNE   SPCOMXIT                                                         
         CLI   1(R6),10            TEST ALLOCATED                               
         BNH   BLDQ4                                                            
         ZIC   R4,10(R6)                                                        
         BAS   RE,SETQBIT                                                       
         CLI   1(R6),14                                                         
         BNH   BLDQ4                                                            
         ZIC   R4,14(R6)                                                        
         BAS   RE,SETQBIT                                                       
         B     BLDQ4                                                            
*                                                                               
SETQBIT  BCTR  R4,0                                                             
         SRDL  R4,3                DIVIDE BY 8 (REMAINDER IN R5)                
         LA    R4,QLIST(R4)        POINT TO TABLE BYTE                          
         SRL   R5,29               RIGHT ALIGN                                  
         IC    R5,QBITS(R5)                                                     
         STC   R5,BYTE                                                          
         OC    0(1,R4),BYTE        'OR' INTO APPROPRIATE BYTE                   
         BR    RE                                                               
*                                                                               
QBITS    DC    X'8040201008040201'                                              
         EJECT                                                                  
BUGOADDS DS    0H                  ** USER 16 **                                
         L     R4,AREC5            CLEAR BLOCK                                  
         XC    0(256,R4),0(R4)                                                  
         USING SPADINTD,R4                                                      
         STC   R1,ADACTN                                                        
         DROP  R4                                                               
*                                                                               
         LR    R1,R4                                                            
         BRAS  RE,BUADDS                                                        
         B     SPCOMXIT                                                         
         SPACE 2                                                                
BUSECRET DS    0H                  ** USER 17 **                                
         L     R3,VBUYTWA                                                       
         USING T211FFD,R3                                                       
         OC    TWASAGN,TWASAGN     NOT ON NEW SECURITY                          
         BZ    BSX                                                              
         L     RF,VCOMFACS                                                      
         L     RF,CSECRET-COMFACSD(RF)                                          
         LHI   R0,1024                                                          
         GOTO1 (RF),DMCB,('SECPRACT',ASECBLK),(R0)                              
         BE    SPCOMXIT                                                         
         MVI   ERRCD,NOFNACCS                                                   
         GOTO1 ERROR                                                            
*                                                                               
BSX      B     SPCOMXIT                                                         
         EJECT                                                                  
*============================================================*                  
* SUBROUTINE TO VALIDATE SLN IN BYTE                         *                  
*============================================================*                  
         SPACE 1                                                                
BUCHKSLN BRAS  RE,CHKSLN                                                        
         BNE   SPCOMXIT                                                         
         B     SPERROR                                                          
         SPACE 1                                                                
*============================================================*                  
* SUBROUTINE TO MOVE RECORD IN DUB TO RECORD IN DUB+4        *                  
*============================================================*                  
         SPACE 1                                                                
BUMVREC  DS    0H                  ** USER 13 **                                
         L     RE,DUB              FROM RECORD                                  
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)                                                      
         L     R0,DUB+4            TO RECORD                                    
         LA    R1,2(RF)            SET TO LEN TO 2 MORE                         
         MVCL  R0,RE                                                            
         B     SPCOMXIT                                                         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
SUBRPACK NTR1  BASE=*,LABEL=*                                                   
         SR    R1,R1                                                            
         SR    R0,R0                                                            
         ZAP   DUB,=P'0'                                                        
         IC    R1,5(R2)                                                         
         LTR   R1,R1               EXIT ON ZERO LENGTH                          
         BZ    PACKX                                                            
         TM    4(R2),X'08'         OR NON-NUMERIC                               
         BZ    PACKX                                                            
         BCTR  R1,0                                                             
         EX    R1,*+12                                                          
         CVB   R0,DUB                                                           
         B     PACKX                                                            
         PACK  DUB,8(0,R2)   * EXECUTED *                                       
*                                                                               
PACKX    XIT1  REGS=(R0,R1)                                                     
         LTORG                                                                  
*                                                                               
SUBRMOVE NTR1  BASE=*,LABEL=*                                                   
         MVI   WORK,C' '                                                        
         MVC   WORK+1(L'WORK-1),WORK                                            
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BZ    SUBRMOVX                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),8(R2)                                                    
SUBRMOVX XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*=========================================================                      
* SET UP AND CALL SPGETDEM (T00A20) OR DEMUPGD                                  
*=========================================================                      
                                                                                
DMLKUP   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLC   =C'MC',AGYALPHA                                                  
         BNE   DMLK2                                                            
         MVI   ELCDLO,X'FE'        LOOK FOR CONVERSION ELEMENT                  
         MVI   ELCDHI,X'FE'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   DMLK2                                                            
*                                                                               
         MVI   ELCDLO,2                                                         
         MVI   ELCDHI,2                                                         
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         JNE   *+2                 ORIG NDEMEL MUST BE PRESENT!                 
         OC    2(2,R6),2(R6)       TEST BOOK PRESENT                            
         BNZ   DMLK2               YES - THEN GET DEMOS                         
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AHI   R0,-24                                                           
         BNP   DMLK2                                                            
         SRL   R0,3                                                             
*                                                                               
         LA    R1,24(R6)                                                        
         OI    4(R1),X'80'         TEST OVRD                                    
         LA    R1,8(R1)                                                         
         BCT   R0,*-8                                                           
*                                                                               
DMLK2    XC    SVSPLMKT,SVSPLMKT                                                
*                                                                               
         MVI   ELCDLO,X'62'        SEARCH FOR UPGRADE ELEMENT                   
         MVI   ELCDHI,X'62'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   DMLK20                                                           
*                                                                               
         USING UPELEM,R6                                                        
         L     R8,ADBLOCK                                                       
         XC    0(256,R8),0(R8)                                                  
         USING SPDEMUPD,R8                                                      
         MVC   SPUPDAY,BDDAY                                                    
         MVC   SPUPTIM,BDPROG                                                   
         MVI   SPUPFIL,C'T'                                                     
         MVC   SPUPTYPE(L'UPTYPE),UPTYPE     MOVE UPGRADE TYPE !                
         MVC   SPUPFBK(2),UPFBK                                                 
         MVC   SPUPFBKL,UPFBKLST                                                
         MVC   SPUP2YRP,UP2YRP                                                  
         MVC   SPUP2YRR,UP2YRS                                                  
         L     R1,=A(STAWORK-SPBUYWKD)                                          
         AR    R1,RC                   POINT TO STAWORK                         
         MVC   0(L'UPINPUT,R1),UPINPUT SAVE TEXT SO CAN REBUILD ELEM            
         MVC   SPUPBTYP,BUBKTYPE   USE BOOKTYPE IF INPUT THIS TIME              
         CLI   SPUPBTYP,0                                                       
         BE    DMLK4                                                            
         MVI   ELCDLO,X'24'                                                     
         MVI   ELCDHI,X'24'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   *+10                                                             
         MVC   SPUPBTYP,(DLUBKTYP-DLUELEM)(R6)                                  
*                                                                               
DMLK4    GOTO1 DEMUPGD                                                          
         B     DMLKX                                                            
         DROP  R6,R8                                                            
*                                                                               
DMLK20   MVI   ELCDLO,2            FIND DEMO ELEM                               
         MVI   ELCDHI,2                                                         
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         JNE   *+2                 MUST BE PRESENT                              
*                                                                               
         CLI   SVAPROF+7,C'C'      TEST CANAD                                   
         BNE   DMLK24                                                           
         MVI   BYTE,1                                                           
         CLI   BUYMD,C'N'                                                       
         BE    DMLK26                                                           
*                                                                               
DMLK24   MVC   BYTE,SVAGYMD                                                     
         NI    BYTE,X'0F'                                                       
         CLI   BYTE,1              TEST SPOT TV                                 
         BE    DMLK26                                                           
         CLI   SV1WPROF+6,C'Y'     TEST DO RADIO LOOKUP                         
         BNE   DMLKX                                                            
*                                                                               
DMLK26   MVC   BYTE2,SVHUTADJ                                                   
         CLI   SVCXTRA+5,C'Y'      TEST US SPILL                                
         BE    DMLK28                                                           
         CLI   SVAPROF+7,C'C'      TEST CANADIAN                                
         BNE   *+8                                                              
*                                                                               
DMLK28   O     R6,=X'01000000'     SET TO LOOK UP SPILL                         
*                                                                               
         OC    2(2,R6),2(R6)       TEST BOOK IN ELEMENT                         
         BNZ   *+10                                                             
         MVC   2(2,R6),SVBOOK                                                   
*                                                                               
         OC    SVOPTBK,SVOPTBK                                                  
         BZ    *+10                                                             
         MVC   2(2,R6),SVOPTBK                                                  
         CLI   SVOPTHUT,0                                                       
         BE    *+10                                                             
         MVC   BYTE2,SVOPTHUT                                                   
*                                                                               
         OC    BUBOOK,BUBOOK                                                    
         BZ    *+10                                                             
         MVC   2(2,R6),BUBOOK                                                   
         CLI   BUHUTADJ,0                                                       
         BE    *+10                                                             
         MVC   BYTE2,BUHUTADJ                                                   
         MVC   4(20,R6),SVWGTLST   MOVE WEIGHTS                                 
*                                                                               
DMLK30   XC    WORK,WORK                                                        
         LA    R0,SV1WPROF                                                      
         ST    R0,WORK                                                          
         MVI   WORK,X'FF'                                                       
         MVC   WORK+4(4),VBUYTWA                                                
         MVC   WORK+8(4),VCOMFACS                                               
         MVC   WORK+12(4),VDATAMGR                                              
         MVC   WORK+16(4),VCALLOV                                               
*                                                                               
*   WE'RE GOING TO USE  WORK+24  FOR 3 FULL WORDS TO CALL CALLOV                
*                                                                               
         MVC   BYTE,SVCPROF+3      SET RATING SVC BYTE                          
*                                                                               
DMLK32   NI    BYTE,X'0F'                                                       
*                                                                               
         CLI   SVAPROF+7,C'C'                                                   
         BNE   DMLK40                                                           
         CLI   SVCXTRA,C'U'        TEST US CLIENT                               
         BE    DMLK40                                                           
         OI    BYTE,X'80'          SET CANADIAN IND                             
*                                                                               
DMLK40   MVC   WORK+28(4),=X'D9000A20'                                          
         GOTO1 VCALLOV,WORK+24                                                  
         CLI   4(R1),X'FF'                                                      
         JE    *+2                                                              
         MVC   DUB(4),WORK+24                                                   
         XC    WORK+24(12),WORK+24  DONE WITH THIS                              
*                                                                               
         L     RE,AREC2            PASS A WORK AREA IN DM6                      
         LA    RE,1600(RE)                                                      
         ST    RE,DMCB+20                                                       
*                                                                               
         LA    RE,100(RE)          AREA FOR EXTENDED PARAMS                     
         ST    RE,DMCB+8                                                        
*                                                                               
         MVC   DMCB+8(1),BYTE      MOVE PREVIOUS VALUE                          
         OI    DMCB+8,X'40'        AND SET FLAG FOR PARAM                       
*                                                                               
         USING GETDEMD,RE                                                       
*                                                                               
         XC    0(32,RE),0(RE)                                                   
         MVC   GTDMHUT,BYTE2       MOVE HUT BYTE                                
         MVC   GTDMKALF,SVMKTMKT   PASS ORIGINATING MARKET ALPHA ID             
         MVC   GTDMUID,T211FFD+10  MOVE USERID                                  
         CLI   SVNTI,C' '                                                       
         BNH   *+10                                                             
         MVC   GTDMNTI,SVNTI                                                    
*                                                                               
         LAY   RF,SV00APRF+6                                                    
         CLI   0(RF),C'Y'          TEST 2-DECIMAL IMPS                          
         JNE   DMLK44                                                           
         OI    GTDMFLAG,X'01'      REQUEST 2-DEC IMPS AND                       
         J     DMLK44A              AND 2-DEC RTGS THEN                         
*                                                                               
DMLK44   CLI   SV00PROF+9,C'Y'     TEST 2-DECIMAL RTGS                          
         JNE   DMLK50                                                           
DMLK44A  OI    GTDMFLAG,X'40'      REQUEST 2-DEC RTGS THEN                      
         DROP  RE                                                               
*                                                                               
DMLK50   L     RF,DUB              T00A20 ADDRESS (SPGETDEME)                   
         GOTO1 (RF),DMCB,BUYREC,(R6),,AREC2,WORK                                
         XC    DMCB+20(4),DMCB+20  CLEAR ADDRESS ON RETURN                      
* CHECK FOR ERRORS                                                              
         OC    SVNDEF(16),SVNDEF   TEST CANAD NTWK                              
         BZ    *+8                                                              
         MVI   0(R1),0             *****  SUPPRESS ERROR  *****                 
         CLI   0(R1),0                                                          
         BE    DMLK70                                                           
*                                                                               
         TM    SVESTFL1,X'10'      TEST NO DEMOS ANTICIPATED                    
         BO    DMLK70                                                           
*                                                                               
         MVI   ERRCD,NORTGSVC                                                   
         CLI   0(R1),X'11'                                                      
         BE    DMLK55              OK IF HAVE OVERRIDES                         
         MVI   ERRCD,BADRTGSV                                                   
         CLI   0(R1),X'41'                                                      
         BE    DMLK55              OK IF HAVE OVERRIDES                         
         MVI   ERRCD,NOBOOK                                                     
         CLI   0(R1),X'80'         TEST E-O-F                                   
         BE    DMLK55              YES - MISSING BOOK ERROR                     
         CLI   0(R1),X'45'         TEST MISSING BOOK                            
         BE    DMLK55                                                           
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(RTGBKNA)                                             
         CLI   0(R1),2                                                          
         BE    DMLK55                                                           
         MVI   ERRAREA,X'FE'       SET ABEND/UNDO                               
         MVC   NERRCD,=AL2(LKUPERR)                                             
         B     DMLKERR                                                          
         SPACE 1                                                                
* MISSING BOOK OK IF ALL DEMOS HAVE OVERRIDES *                                 
         SPACE 1                                                                
DMLK55   CLI   SVCXTRA+4,C'E'      TEST FLIGHTS BY EST                          
         BE    DMLK70              YES - EXIT AND IGNORE ERROR                  
*TEST UPLD OR DARE MAKEGOOD/IGNORE DEM ERRS                                     
         TM    UPSW,UPON           TEST UPLOAD                                  
         BO    DMLK70                                                           
         TM    WRKRUPSW,WRKRUPSW_NODMER+WRKRUPSW_ISDRMG                         
         BNZ   DMLK70              YES - SO IGNORE IT                           
         CLI   SVAPROF+7,C'C'                                                   
         BE    DMLK70                                                           
         CLI   SVKEY+6,X'E8'       TEST SPOT CABLE                              
         BNL   DMLK70              YES - IGNORE                                 
*                                                                               
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AHI   R0,-24                                                           
         BNP   DMLK70                                                           
         LA    R1,24(R6)                                                        
*                                                                               
DMLK60   SRL   R0,3                                                             
DMLK61   CLI   2(R1),0             TEST NONT DEMO CATEGORY                      
         BE    DEMLK65              YES, DON'T BOTHER TEST                      
*                                                                               
************************************************                                
* IF SPGETDEME RETURNS AN ERROR AND THE BRAND ESTIMATE HAS LESS DEMO            
* CATEGORIES THAN THE POL ESTIMATE, BUY PGM WILL ALWAYS RETURN AN ERROR         
* PER MEL, INSTEAD OF ADDING CODE, THE WORKAROUND IS TO EITHER                  
* 1)ADD THE BUY UNDER THE POL BRAND OR                                          
* 2)ADD ALL DEMOS TO THE BRAND ESTIMATE                                         
*  OR                                                                           
* 3)ADD CODE AND CHECK BUDEMS+24                                                
************************************************    -HWON 01/04/2017            
         TM    4(R1),X'80'                                                      
         BZ    DMLKERR                                                          
DEMLK65  LA    R1,8(R1)                                                         
         BCT   R0,DMLK61                                                        
*                                                                               
DMLK70   OC    SVDEMMIN,SVDEMMIN   TEST MIN TGT SPECIFIED                       
         BZ    DMLK80              NO                                           
         TM    SVOPT2,X'10'        TEST OVERRIDE MIN                            
         BO    DMLK80                                                           
* FIND FIRST BRAND DEMO IN DEMO ELEM *                                          
         LA    RE,SVDEMLST         POINT TO NORMAL DEMO LIST                    
         OC    SVBRDEMS,SVBRDEMS   TEST BRAND POL                               
         BZ    *+8                 NO                                           
         LA    RE,SVBRDEMS         POINT TO FIRST BRAND DEMO                    
*                                                                               
         LA    R1,24(R6)                                                        
         ZIC   R0,1(R6)                                                         
         AHI   R0,-24                                                           
         SRL   R0,3                                                             
*                                                                               
DMLK72   CLC   0(3,RE),0(R1)                                                    
         BE    DMLK74                                                           
         LA    R1,3(R1)                                                         
         BCT   R0,DMLK72                                                        
         DC    H'0'                                                             
*                                                                               
DMLK74   MVI   ERRCD,DEM2LTTL                                                   
         TM    4(R1),X'80'         TEST BUYER OVRD                              
         BO    DMLK80              YES - MIN DOES NOT APPLY                     
         CLC   6(2,R1),SVDEMMIN    COMPARE ACTUAL TO MIN                        
         BL    DMLKERR                                                          
*                                                                               
DMLK80   MVC   DUB(1),RESTOROV     SET OVLY NUM TO BE RESTORED                  
         BRAS  RE,BUOVLY           RESTORE CALLING OVERLAY                      
*                                                                               
DMLKX    XIT1                                                                   
*                                                                               
DMLKERR  GOTO1 ERROR                                                            
         LTORG                                                                  
         EJECT                                                                  
*===================================================*                           
* READ SPILL REC AND ADD SPILL DEMO EL FOR EACH MKT *                           
*                                                   *                           
*    ** NOTE **   R7 HAS A(5 BYTE MKT-STA)          *                           
*===================================================*                           
         SPACE 1                                                                
GETSPL   NTR1  BASE=*,LABEL=*                                                   
         L     RA,VBUYSAVE                                                      
         USING BUYSAVE,RA                                                       
         L     R3,VBUYTWA                                                       
         USING T211FFD,R3                                                       
         GOTO1 STAPACK,DMCB,(C'U',(R7)),WORK,WORK+4                             
*                                                                               
         LHI   RE,SVB0PROF-BUYSAVE                                              
         AR    RE,RA                                                            
         CLI   5(RE),C'Y'          TEST COPY FROM PREVIOUS BUY                  
         BNE   GETSPL01                                                         
         CLI   BUTRCODE,C'B'       TEST NEW BUY                                 
         BE    GPRVSPL                                                          
         CLC   =C'COPY',BUTRCODE   OR COPY                                      
         BE    GPRVSPL                                                          
*                                                                               
GETSPL01 XC    KEY,KEY             BUILD KEY FOR SPILL DEFINITION REC           
         MVC   KEY(2),=X'0D13'                                                  
         MVC   KEY+2(2),AGYALPHA                                                
         MVC   KEY+4(1),SVCPROF+3  AGENCY RATING SERVICE                        
         MVC   KEY+5(5),WORK+4     STATION CALL LETTERS                         
         CLI   BUYMD,C'R'                                                       
         BE    *+8                 MEDIA T/N HAVE 0 IN KEY+9 !!!                
         MVI   KEY+9,0                                                          
*                                                                               
GETSPL04 DS    0H                                                               
         MVC   KEY+10(2),SVCLT     CHECK FOR CLT EXCEPTION REC                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     AND USE IT IF FOUND                          
         BE    GETSPL06                                                         
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(10),KEYSAVE     RESTORE WITHOUT CLIENT                       
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     READ FOR DEFAULT SPILL RECORD                
         BNE   GETSPLX             EXIT WITH CC NOT =                           
*                                                                               
GETSPL06 L     R0,AREC                                                          
         LA    R8,SPLLWRK                                                       
         USING SDEFRECD,R8                                                      
         ST    R8,AREC                                                          
         GOTO1 GETREC                                                           
         ST    R0,AREC                                                          
         CLC   13(2,R8),=H'256'                                                 
         JH    *+2                                                              
         LA    R7,SDEFEL                                                        
         USING SDEFEL05,R7                                                      
         DROP  R8                                                               
* CREATE SPILL DEMO EL IN ELEM                                                  
         LA    R6,BDELEM                                                        
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),2                                                          
         JNE   *+2                                                              
         MVC   ELEM(24),0(R6)                                                   
         MVI   ELEM,3              SET SPILL DEMEL CODE                         
         MVI   ELEM+1,24           RESET LENGTH                                 
* MOVE ALL RATINGS TO SPILL ELEM                                                
         ZIC   R0,1(R6)                                                         
         AHI   R0,-24                                                           
         BZ    GETSPL30                                                         
         SRL   R0,3                SET FOR BCT                                  
         LA    R1,ELEM+24                                                       
         LA    R6,24(R6)           POINT TO FIRST DEMO                          
*                                                                               
GETSPL10 DS    0H                                                               
         CLI   BUYMD,C'R'          RADIO SAVES SPILL FOR ALL DEMOS              
         BE    GETSPL25                                                         
         CLI   1(R6),C'R'          TRAD DEMO 'R'ATING?                          
         BE    GETSPL25             YES, SAVE IT                                
         CLI   1(R6),C'E'          TRAD DEMO 'E'XTEND RATING?                   
         BE    GETSPL25             YES, SAVE IT                                
*                                                                               
****************************************                  -HWON 9/26/16         
         CLI   2(R6),0             TEST NONT DEMO?                              
         BNE   GETSPL20             NO                                          
         LLC   RE,1(R6)             YES, GET NONT DEMO SEQNUM                   
         BCTR  RE,0                                                             
         SLL   RE,3                X 8                                          
         AHI   RE,SVNTDMS-BUYSAVE                                               
         AR    RE,RA               RE=A(NON-TRAD SHORT NAME)                    
         CLI   0(RE),C'R'          NON-TRAD 'R'ATING?                           
         BE    GETSPL25             YES, SAVE IT                                
         CLI   0(RE),C'E'          NON-TRAD 'E'XTENDED RATING?                  
         BE    GETSPL25             YES, SAVE IT                                
         B     GETSPL27            OTHERWISE, SKIP IT                           
*                                                                               
GETSPL20 CLI   1(R6),X'21'         TEST USER DEMO                               
         BNE   GETSPL27                                                         
         ZIC   RE,2(R6)            GET DEMO NUMBER                              
         BCTR  RE,0                                                             
         MHI   RE,7                                                             
         LA    RE,SVUSRNMS(RE)     RE=A(USE-DEF NAME)                           
         CLI   0(RE),C'R'          USER-DEF 'R'ATING?                           
         BE    GETSPL25             YES, SAVE IT                                
         CLI   0(RE),C'E'          USER-DEF 'E'XTENDED RATING?                  
         BNE   GETSPL27             NO, SKIP IT                                 
****************************************                  -HWON 9/26/16         
*                                                                               
GETSPL25 XC    0(8,R1),0(R1)                                                    
         MVC   0(3,R1),0(R6)       MOVE DEMO DESC                               
         LA    R1,8(R1)                                                         
         ZIC   RE,ELEM+1           BUMP ELEMENT LENGTH                          
         LA    RE,8(RE)                                                         
         STC   RE,ELEM+1                                                        
*                                                                               
GETSPL27 LA    R6,8(R6)                                                         
         BCT   R0,GETSPL10                                                      
         SPACE 1                                                                
* ADD SPILL DEMEL FOR EACH SPILL MKT *                                          
         SPACE 1                                                                
GETSPL30 CLI   0(R7),5             SKIP IF NOT A SPILL MARKET ELEMENT           
         BNE   GETSPL50                                                         
         CLC   BUYREC+4(2),SDEFAMKT TEST SPILL MKT EQ ACTUAL MKT                
         BE    GETSPL50             YES - SKIP                                  
         TM    8(R7),X'80'          TEST '*' FEATURE                            
         BO    GETSPL50             YES SKIP                                    
*                                                                               
E        USING NDELEM,ELEM                                                      
         MVC   E.NDAGYMKT,SDEFAMKT                                              
         MVC   E.NDRSMKT,SDEFRMKT   NEED THIS FOR US STUPIDO !                  
         MVC   E.NDBKTYPE,SDEFBKTY                                              
         MVC   E.NDRTGSVC,SDEFRSVC                                              
         MVC   E.NDMKTALF,SDEFALPH                                              
*                                                                               
         MVC   E.NDSTA,SVDMLST0                                                 
         CLI   SDEFRSVC,C'1'        TEST BBM LOOKUP                             
         BNE   *+10                                                             
         MVC   E.NDSTA,SVDMLST1                                                 
*                                                                               
         OC    SVNDEF(16),SVNDEF                                                
         BZ    GETSPL40                                                         
*                                                                               
         SR    RF,RF               FOR NETWORK, GET TABLE ENTRY                 
         ICM   RF,1,SVNDINDX                                                    
         BZ    GETSPL40                                                         
         BCTR  RF,0                                                             
         MHI   RF,L'SVNDDEM                                                     
         LA    RF,SVNDDEM(RF)                                                   
         USING SVNDDEMD,RF                                                      
         MVC   E.NDSTA,SVNDDST0                                                 
         CLI   SDEFRSVC,C'1'        TEST BBM LOOKUP                             
         BNE   *+10                                                             
         MVC   E.NDSTA,SVNDDST1                                                 
         DROP  E,R7,RF                                                          
*                                                                               
GETSPL40 LA    R6,BDELEM                                                        
         ZIC   R0,1(R6)            ELEMENT LENGTH                               
         AR    R6,R0               BUMP TO THE SECOND ELEMENT                   
         CLI   0(R6),2             IT HAS TO BE A 02 ELEMENT                    
         JNE   *+2                                                              
*                                                                               
GETSPL41 IC    R0,1(R6)                                                         
         AR    R6,R0               INSERT AFTER 02 ELEMENT                      
         CLI   0(R6),X'22'         TEST POST BUY DEMEL                          
         BE    GETSPL41                                                         
         CLI   0(R6),X'23'                                                      
         BE    GETSPL41                                                         
*                                                                               
GETSPL42 CLI   0(R6),3             03 ELEMENT?                                  
         BNE   GETSPL45                                                         
*                                                                               
         CLC   ELEM+4(2),4(R6)     AGENCY MARKET NUMBER                         
         BE    GETSPL47            DON'T ADD IF ALREADY IN RECORD               
         B     GETSPL41                                                         
*                                                                               
GETSPL45 BRAS  RE,ELEMADD                                                       
*                                                                               
GETSPL47 MVC   4(20,R6),ELEM+4     FIX LOOKUP DETAILS                           
*                                                                               
GETSPL50 ZIC   R0,1(R7)            BUMP TO NEXT SPILL MARKET                    
         AR    R7,R0                                                            
         CLI   0(R7),0             DONE AT END OF MARKETS                       
         BNE   GETSPL30                                                         
         EJECT                                                                  
*=====================================================================          
* DELETE ANY ELEMENTS FOR MARKETS NO LONGER IN SPILL DEF RECORD                 
*=====================================================================          
         SPACE 1                                                                
GTSDEL   DS    0H                                                               
*                                                                               
*        DELETE SPILL DEMO ELEMENTS FOR MKTS NOT IN SPILL LIST                  
*                                                                               
         LA    R8,SPLLWRK          POINT TO  SPILL DEFINITION RECORD            
         USING SDEFRECD,R8         ESTABLISH SPILL DEFINITION RECORD            
*                                                                               
         SR    R0,R0                                                            
*                                                                               
         LA    R6,BDELEM           POINT TO FIRST ELEMENT IN BUY                
         USING NDELEM,R6           ESTABLISH AS DEMO ELEMENT                    
*                                                                               
GTSDEL1L DS    0H                                                               
*                                                                               
         CLI   NDCODE,0            DONE AT END OF BUY RECORD                    
         BE    GTSDEL1D                                                         
*                                                                               
         CLI   NDCODE,3            SKIP IF NOT A SPILL DEMO ELEMENT             
         BNE   GTSDEL1C                                                         
*                                                                               
         LA    R7,SDEFEL           POINT TO FIRST ELM IN SPILL DEF REC          
         USING SDEFEL05,R7         ESTABLISH AS SPILL MARKET ELEMENT            
*                                                                               
GTSDEL2L DS    0H                                                               
*                                                                               
         CLI   SDEFEL05,0          DONE IF END OF RECORD REACHED                
         BE    GTSDEL2D                                                         
*                                                                               
         CLI   SDEFEL05,5          SKIP IF NOT A SPILL MARKET ELEMENT           
         BNE   GTSDEL2C                                                         
*                                                                               
         CLC   SDEFAMKT,NDPROG     MATCH AGENCY MARKETS                         
         BE    GTSDEL2F                                                         
*                                                                               
GTSDEL2C DS    0H                                                               
*                                                                               
         SR    R0,R0                                                            
         IC    R0,SDEFEL05+1       BUMP TO NEXT SPILL MKT ELEM                  
         AR    R7,R0                                                            
         B     GTSDEL2L                                                         
*                                                                               
GTSDEL2D DS    0H                  DEMO ELEM MKT NOT IN SPILL MKTS LIST         
*                                  DELETE ELEMENT FROM BUY                      
         BRAS  RE,ELEMDEL                                                       
*                                                                               
         CLI   0(R6),X'23'         TEST NEXT IS POSTBUY                         
         BE    GTSDEL2D            YES - DELETE !                               
*                                                                               
         B     GTSDEL1L            NO NEED TO BUMP TO NEXT ELEMENT              
*                                                                               
GTSDEL2F DS    0H                  MKT IN SPILL LIST                            
*                                                                               
GTSDEL1C DS    0H                                                               
*                                                                               
         SR    R0,R0                                                            
         IC    R0,NDLEN            BUMP TO NEXT SPILL DEMO ELEMENT              
         AR    R6,R0                                                            
         B     GTSDEL1L                                                         
*                                                                               
GTSDEL1D DS    0H                                                               
*                                                                               
         B     GETSPLX             EXIT WITH CC =                               
         DROP  R7,R8                                                            
         LTORG                                                                  
         EJECT                                                                  
*==============================================================                 
* IF THERE IS A PREVIOUS BUYLINE FOR THE SAME DAYPART, USE THE                  
* SPILL LIST FROM IT.                                                           
*==============================================================                 
         SPACE 1                                                                
GPRVSPL  CLC   SVSPLSTA,BUYKEY+6   TEST SAME STATION                            
         BNE   GPRVSP10                                                         
         CLC   SVSPLCLT,BUYKEY+1   TEST SAME CLIENT                             
         BNE   GPRVSP10                                                         
         CLC   SVSPLDPT,BDDAYPT    TEST SAME DAYPART                            
         BNE   GPRVSP10                                                         
         LHI   R4,SVSPLLST-BUYSAVE                                              
         AR    R4,RA                                                            
         CLI   0(R4),X'FF'         TEST ANY SPILL MARKETS                       
         BE    GETSPLX             NO                                           
         B     GPRVSP50                                                         
*                                                                               
GPRVSP10 XC    SVSPLKEY,SVSPLKEY   CLEAR PREVIOUS                               
*                                                                               
         MVI   DMINBTS,0           DO NOT READ DELETED RECORDS!                 
         XC    KEY,KEY                                                          
         MVC   KEY(10),SVKEY       A-M/CLT/PRD/MKT/STA/EST                      
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE     ANY PREVIOUS LINE NUMBER                     
         BNE   GETSPL01            IF NO PREVIOUS, TRY OTHER METHODS            
         B     GPRVSP30            ELSE READ THIS RECORD                        
*                                                                               
GPRVSP20 MVC   KEYSAVE,KEY         SAVE PREVIOUS KEY                            
         GOTO1 SEQ                                                              
*                                                                               
GPRVSP25 CLC   KEY(10),KEYSAVE     SAME A-M/CLT/PRD/MKT/STA/EST                 
         BE    GPRVSP30                                                         
* NO MATCH FOUND - USE LAST LINE READ                                           
         MVC   KEY,KEYSAVE         RESTORE LAST LINE FOUND                      
         GOTO1 HIGH                AND USE IT                                   
         L     R0,AREC             SAVE CURRENT AREC                            
         L     R6,AREC2                                                         
         ST    R6,AREC                                                          
         GOTO1 GETREC                                                           
         ST    R0,AREC                                                          
         B     GPRVSP40                                                         
*                                                                               
GPRVSP30 L     R0,AREC             SAVE CURRENT AREC                            
         L     R6,AREC2                                                         
         ST    R6,AREC                                                          
         GOTO1 GETREC                                                           
         ST    R0,AREC             RESTORE AREC                                 
*                                                                               
         LA    RE,BDDAYPT-BUYREC(R6)                                            
         CLC   BDDAYPT,0(RE)       SAME DAYPART                                 
         BNE   GPRVSP20                                                         
         SPACE 1                                                                
*==============================================================                 
* SAVE THE AGENCY/RTGSVC MARKET NUMBERS IN SVSPLLST                             
*==============================================================                 
         SPACE 1                                                                
GPRVSP40 MVC   SVSPLSTA,BUYKEY+6   SAVE STATION                                 
         MVC   SVSPLCLT,BUYKEY+1   SAVE CLIENT                                  
         MVC   SVSPLDPT,BDDAYPT    SAVE DAYPART                                 
*                                                                               
         LHI   R4,SVSPLLST-BUYSAVE                                              
         AR    R4,RA                                                            
         MVI   0(R4),X'FF'                                                      
*                                                                               
         L     R6,AREC2                                                         
         LA    R6,BDELEM-BUYREC(R6)                                             
*                                                                               
GPRVSP45 SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    GPRVSP50                                                         
         CLI   0(R6),3                                                          
         BNE   GPRVSP45                                                         
*                                                                               
         MVC   0(2,R4),4(R6)       MOVE AGENCY MARKET                           
         MVC   2(2,R4),6(R6)       MOVE RTGSVC MARKET                           
         MVC   4(1,R4),7(R6)       BOOKTYPE OVERRIDE                            
         MVC   5(3,R4),17(R6)      ALPHA MARKET                                 
*                                                                               
         LA    R4,L'SVSPLLST(R4)                                                
         MVI   0(R4),X'FF'         SET NEW EOL FLAG                             
         B     GPRVSP45                                                         
         SPACE 1                                                                
*==============================================================                 
* ADD A SPILL ELEMENT FOR EACH MARKET IN SVSPILL LIST                           
*==============================================================                 
         SPACE 1                                                                
GPRVSP50 LHI   R4,SVSPLLST-BUYSAVE                                              
         AR    R4,RA                                                            
         CLI   0(R4),X'FF'         TEST NO MKTS IN LIST                         
         BE    GETSPLX                                                          
         LA    R6,BDELEM                                                        
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),2             FIND 02 DEMO ELEM                            
         JNE   *+2                                                              
*                                                                               
         MVC   ELEM(24),0(R6)                                                   
         MVI   ELEM,X'03'          SET SPILL DEMEL CODE                         
         MVI   ELEM+1,24           RESET LENGTH                                 
* MOVE DEMOS TO SPILL ELEM (RATINGS ONLY FOR TV)                                
         IC    R0,1(R6)                                                         
         AHI   R0,-24                                                           
         BZ    GETSPLX                                                          
         SRL   R0,3                GIVES DEMO COUNT                             
         LA    R1,ELEM+24                                                       
         LA    R6,24(R6)           POINT TO FIRST DEMO                          
*                                                                               
GPRVSP60 CLI   BUYMD,C'R'          TEST RADIO                                   
         BE    GPRVSP80            DOES RATINGS AND IMPS                        
         CLI   1(R6),C'R'                                                       
         BE    GPRVSP80                                                         
         CLI   1(R6),C'E'                                                       
         BNE   GPRVSP85                                                         
*                                                                               
****************************************                  -HWON 9/26/16         
         CLI   2(R6),0             TEST NONT DEMO?                              
         BNE   GPRVSP70             NO                                          
         LLC   RE,1(R6)             YES, GET NONT DEMO SEQNUM                   
         BCTR  RE,0                                                             
         SLL   RE,3                X 8                                          
         AHI   RE,SVNTDMS-BUYSAVE                                               
         AR    RE,RA               RE=A(NON-TRAD SHORT NAME)                    
         CLI   0(RE),C'R'          NON-TRAD 'R'ATING?                           
         BE    GPRVSP80             YES, SAVE IT                                
         CLI   0(RE),C'E'          NON-TRAD 'E'XTENDED RATING?                  
         BE    GPRVSP80             YES, SAVE IT                                
         B     GPRVSP85            OTHERWISE, SKIP IT                           
*                                                                               
GPRVSP70 CLI   1(R6),X'21'         TEST USER DEMO                               
         BNE   GPRVSP85                                                         
         ZIC   RE,2(R6)            GET DEMO NUMBER                              
         BCTR  RE,0                                                             
         MHI   RE,7                                                             
         LA    RE,SVUSRNMS(RE)     RE=A(USE-DEF NAME)                           
         CLI   0(RE),C'R'          USER-DEF 'R'ATING?                           
         BE    GPRVSP80             YES, SAVE IT                                
         CLI   0(RE),C'E'          USER-DEF 'E'XTENDED RATING?                  
         BNE   GPRVSP85             NO, SKIP IT                                 
****************************************                  -HWON 9/26/16         
*                                                                               
GPRVSP80 XC    0(8,R1),0(R1)                                                    
         MVC   0(3,R1),0(R6)       MOVE DEMO CODE                               
         LA    R1,8(R1)                                                         
         SR    RE,RE                                                            
         IC    RE,ELEM+1           BUMP ELEMENT LENGTH                          
         LA    RE,8(RE)                                                         
         STC   RE,ELEM+1                                                        
*                                                                               
GPRVSP85 LA    R6,8(R6)                                                         
         BCT   R0,GPRVSP60                                                      
*                                                                               
         LA    R6,BDELEM           FIND INSERTION POINT IN NEW BUY              
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),2                                                          
         JNE   *+2                                                              
         IC    R0,1(R6)                                                         
         AR    R6,R0               INSERT AFTER 02 ELEM                         
*                                                                               
         LHI   R4,SVSPLLST-BUYSAVE                                              
         AR    R4,RA                                                            
*                                                                               
GPRVSP90 MVC   ELEM+4(2),0(R4)                                                  
         MVC   ELEM+6(2),2(R4)                                                  
         MVC   ELEM+7(1),4(R4)     BOOKTYPE                                     
         MVC   ELEM+17(3),5(R4)    ALPHA MKT                                    
*                                                                               
         BRAS  RE,ELEMADD                                                       
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0               INSERT NEXT AFTER THIS ONE                   
*                                                                               
         LA    R4,L'SVSPLLST(R4)                                                
         CLI   0(R4),X'FF'                                                      
         BNE   GPRVSP90                                                         
*                                                                               
GETSPLX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*=============================================================*                 
* THIS ROUTINE CREATES NEW DEMO ELEM FROM ESTHDR DEMOS        *                 
* AND INSERTS OLD DEMO VALUES FROM EXISTING DEMO ELEM         *                 
* POL SEQUENCE IS USED FOR BRD POL BY BRAND                   *                 
* ALSO CREATES NEW POST BUY DEMO ELEM IF ONE EXISTED          *                 
* ALSO CREATE DEMO LOOK-UP OVERRIDE ELEMENT IF NEEDED         *                 
*                                                             *                 
*  ON EXIT:   BUDLUSW  WILL BEL CLEARED                       *                 
*                                                             *                 
*=============================================================*                 
                                                                                
BLDDEM   NTR1  BASE=*,LABEL=*       ** USER 11 **                               
         L     RA,VBUYSAVE                                                      
         USING BUYSAVE,RA                                                       
         L     R3,VBUYTWA                                                       
         USING T211FFD,R3                                                       
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R1,ELEM+24                                                       
         LA    R4,SVDEMOS                                                       
*                                                                               
BLDDEM3  CLI   1(R4),0                                                          
         BE    BLDDEM4                                                          
         CLI   SVCXTRA+5,C'Y'      TEST US SPILL                                
         BE    BLDDEM3A                                                         
         CLI   SVAPROF+7,C'C'      TEST CANAD                                   
         BNE   BLDDEM3D                                                         
*                                                                               
BLDDEM3A OC    SVSPLMKT,SVSPLMKT   TEST SPILL                                   
         BZ    BLDDEM3D                                                         
         CLI   BUYMD,C'R'          RADIO GETS SPILL FOR ALL DEMOS               
         BE    BLDDEM3D                                                         
         CLI   1(R4),C'R'          TEST RATING                                  
         BE    BLDDEM3D                                                         
         CLI   1(R4),C'E'          TEST EXTENDED RATING                         
         BE    BLDDEM3D                                                         
         CLI   1(R4),X'21'         TEST USER DEMO                               
         BE    BLDDEM3B             YES                                         
         CLI   2(R4),0             TEST NONT DEMO                               
         BE    BLDDEM3C             YES                                         
         B     BLDDEM3E             NONE OF THE ABOVE, SKIP                     
*                                                                               
BLDDEM3B LLC   RE,2(R4)            GET USER-DEF DEMO NUMBER                     
         BCTR  RE,0                                                             
         MHI   RE,7                                                             
         LA    RE,SVUSRNMS(RE)     RE=A(USE-DEF NAME)                           
         CLI   0(RE),C'R'          USER-DEF 'R'ATING?                           
         BE    BLDDEM3D             YES, SAVE IT                                
         CLI   0(RE),C'E'          USER-DEF 'E'XTENDED RATING?                  
         BNE   BLDDEM3E             YES, SAVE IT                                
         B     BLDDEM3D             OTHER, SKIP IT                              
*                                                                               
BLDDEM3C LLC   RE,1(R4)            GET NONT DEMO SEQNUM                         
         BCTR  RE,0                                                             
         SLL   RE,3                X 8                                          
         AHI   RE,SVNTDMS-BUYSAVE                                               
         AR    RE,RA               RE=A(NON-TRAD SHORT NAME)                    
         CLI   0(RE),C'R'          NON-TRAD 'R'ATING?                           
         BE    BLDDEM3D             YES, SAVE IT                                
         CLI   0(RE),C'E'          NON-TRAD 'E'XTENDED RATING?                  
         BNE   BLDDEM3E             NO, SKIP IT                                 
*                                                                               
BLDDEM3D MVC   0(3,R1),0(R4)                                                    
         LA    R1,8(R1)                                                         
*                                                                               
BLDDEM3E LA    R4,3(R4)                                                         
         B     BLDDEM3                                                          
*                                                                               
BLDDEM4  LA    R0,ELEM                                                          
         SR    R1,R0                                                            
         STC   R1,ELEM+1           SET LEN                                      
         MVI   ELEM,X'02'          AND ELEM CODE                                
*                                                                               
         SR    R8,R8               CLEAR R8 TO DEFAULT NO PBELEM                
*                                                                               
         CLI   SVCXTRA+5,C'Y'      TEST US SPILL                                
         BE    BLDDEM4A                                                         
         CLI   SVAPROF+7,C'C'      TEST CANAD                                   
         BNE   BLDDEM4B                                                         
*                                                                               
BLDDEM4A OC    SVSPLMKT,SVSPLMKT                                                
         BNZ   BLDDEM5                                                          
*                                                                               
BLDDEM4B MVI   ELCDLO,2                                                         
         MVI   ELCDHI,2                                                         
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL           HAVE AN ORIG DEMO ELEM?                      
         BNE   BLDDEM37             NO, JUMP AROUND ALL DEMEXT REF'S            
*                                                         -HWON 9/26/16         
         B     BLDDEM10                                                         
         EJECT                                                                  
BLDDEM5  MVI   ELCDLO,3                                                         
         MVI   ELCDHI,3                                                         
         MVI   ELEM,X'03'                                                       
         LA    R6,BDELEM                                                        
*                                                                               
BLDDEM5A BRAS  RE,NEXTEL           HAVE A SPILL DEMO ELEM?                      
         BNE   BLDDEM37             NO, JUMP AROUND ALL DEMEXT REF'S            
*                                                         -HWON 9/26/16         
         CLC   SVSPLMKT,4(R6)      SAME AGENCY MARKET NUMBER                    
         BNE   BLDDEM5A                                                         
*                                                                               
*===========================================================                    
* IF PREVIOUS DEMO ELEMENT FOUND,                                               
* CALL SPDEMEXT TO EXTRACT THE EXISTING DEMO VALUES                             
* BUT ADD NONT DEMO EL IF ANY NONT DEMOS                                        
*===========================================================                    
*                                                                               
BLDDEM10 CLI   0(R6),2             TEST 02 ELEM                                 
         JNE   BLDDEM20            NO                                           
         LHI   RF,SVNTDMS-BUYSAVE  NON-TRAD INDEX DEMO LIST                     
         AR    RF,RA                                                            
         OC    0(L'SVNTDMS,RF),0(RF) TEST ANY NON-TRAD DEMOS                    
         JZ    *+8                                                              
         BRAS  RE,BLDNTDEM         ADD NON-TRAD DEMEL IF NEEDED                 
*                                                                               
BLDDEM20 L     R1,ABLOCK                                                        
         XC    0(64,R1),0(R1)                                                   
         USING DEMEXTD,R1                                                       
*                                                                               
         ST    R6,DXDEMEL          SAVE DEMEL ADDRESS                           
         LA    R0,SVDEMOS                                                       
         ST    R0,DXESTDEM                                                      
         LA    R0,SVUSRNMS                                                      
         ST    R0,DXUSRNMS                                                      
         LHI   R0,SVNTDMS-BUYSAVE  NON-TRAD DEMO NAMES LIST                     
         AR    R0,RA                                                            
         ST    R0,DXNTNMS                                                       
*                                                                               
         L     R1,ABLOCK                                                        
         GOTO1 VSPDEMEXT,(R1)                                                   
*                                                                               
         ICM   R4,15,DXDEMTAB       GET A(DEMO TABLE ON RETURN)                 
         JZ    BLDDEM35                                                         
         USING DXDEMTABD,R4                                                     
* ALWAYS MOVE TO ELEM IN CASE THERE ARE NO DEMOS                                
         L     R6,DXDEMEL                                                       
         MVC   ELEM(24),0(R6)      MOVE CD/LEN/PRGM DESC/BOOK/ETC               
         MVI   ELEM+1,24           RESET LEN                                    
*                                                                               
         OC    0(3,R4),0(R4)       TEST ANY DEMOS IN TABLE                      
         JZ    BLDDEM35            NO                                           
                                                                                
*=================================================                              
* MOVE DEMO VALUES FOR 02/03 DEMEL                                              
*=================================================                              
                                                                                
         LA    R6,ELEM+24          POINT TO FIRST DEMO SLOT                     
*                                                                               
         XC    WORK,WORK           BUILD NEW POST BUY ELEM IN WORK              
         ICM   R8,15,DXPBDEL       GET A(POST BUY DEMEL) IF ANY                 
         JZ    BLDDEM26                                                         
*                                                                               
         LA    R7,WORK             R7 = A(POST BUY ELEM)                        
         CLI   ELEM,2              TEST ORIGINATING                             
         BNE   BLDDEM24             NO                                          
         MVI   0(R7),X'22'         BUILD PB'ORIG DEMO ELEM                      
         LA    R7,PDEMO-PDELEM(R7) SET R7 TO FIRST DEMO POSN                    
         B     BLDDEM26                                                         
*                                                                               
BLDDEM24 MVC   0(SDEMO-SDELEM,R7),0(R8)  BUILD PB'SPILL DEMO ELEM               
         LA    R7,SDEMO-SDELEM(R7)       SET R7 TO FIRST DEMO POSN              
                                                                                
*=================================================================              
* MOVE DEMO VALUES FROM TABLE TO NEW DEMO ELEMENTS                              
* R4 POINTS TO DEMXTAB                                                          
* R6 POINTS TO FIRST DEMO IN NEW 02/03 ELEM IN ELEM                             
* R7 POINTS TO FIRST DEMO IN NEW 22/23 ELEM IN WORK                             
*=================================================================              
*                                                                               
BLDDEM26 DS    0H                                                               
         CLI   ELEM,2              TEST ORIGINATING                             
         JE    BLDDEM30            YES, PROCESS ALL DEMOS                       
         CLI   BUYMD,C'R'          TEST RADIO                                   
         JE    BLDDEM30            YES -  PROCESS ALL DEMOS                     
*                                                                               
         LA    RF,DXDEMCD+1        RF = A(RATING TYPE FOR TRAD DEMO)            
         CLI   DXDEMCD+2,0         TEST NONT DEMO                               
         JNE   *+8                 NO                                           
         LA    RF,DXDEMNM          RF = A(RATING TYPE FOR NON-TRAD)             
*                                                                               
         CLI   0(RF),C'R'          TEST DEMO IS A RATING                        
         JE    BLDDEM30             YES                                         
         CLI   0(RF),C'E'          OR EXTENDED                                  
         JNE   BLDDEM34             NEITHER, SKIP IT                            
*                                                                               
BLDDEM30 MVC   0(3,R6),DXDEMCD     TABLE DEMO CODE                              
                                                                                
         L     R0,DXDEMVAL         GET VALUE FROM TABLE                         
         SR    RF,RF                                                            
         ICM   RF,12,DXDEMFLG                                                   
         N     RF,=X'C0000000'     DROP ALL BUT OVRD/2 DEC                      
         OR    R0,RF                                                            
         STCM  R0,15,4(R6)         SET VALUE IN ELEMENT                         
         MVI   3(R6),100           SET SVI                                      
*                                                                               
         LTR   R8,R8               TEST POST-BUY DEMEL                          
         JZ    BLDDEM32             NO                                          
         L     R0,DXPOSTVAL        GET THE POST-BUY VALUE                       
         STCM  R0,7,0(R7)          SET VALUE IN ELEMENT                         
         TM    DXPOSTFLG,X'80'     CHECK POST-BUY FLAG                          
         JZ    *+8                                                              
         OI    0(R7),X'80'                                                      
*                                                                               
         LA    R7,3(R7)            NEXT DEMO IN PB-ORIG/PB-SPILL                
*                                                                               
BLDDEM32 LA    R6,8(R6)            NEXT DEMO IN ORIG/SPILL                      
*                                                                               
BLDDEM34 AHI   R4,DXDEMTABL                                                     
         OC    0(3,R4),0(R4)       TEST MORE DEMOS IN TABLE                     
         JNZ   BLDDEM26                                                         
         DROP  R4                                                               
* UPDATE ELEMENT LENGTHS                                                        
*                                                                               
         LA    RE,ELEM                                                          
         SR    R6,RE                                                            
         STC   R6,ELEM+1           SET ELEM LENGTH IN ELEM                      
*                                                                               
         LTR   R8,R8               TEST POST-BUY DEMEL                          
         JZ    BLDDEM35                                                         
         LA    RE,WORK                                                          
         SR    R7,RE               SET ELEM LENGTH IN WORK                      
         STC   R7,WORK+1                                                        
*                                                                               
* IF YOU DELETE 02/03 FIRST, THEN DXPBDEL WOULD BE INCORRECT                    
* SO YOU MUST DELETE 22/23 ELEM FIRST THEN DELETE 02/03                         
*                                                                               
BLDDEM35 ICM   R6,15,DXPBDEL       HAVE 22/23 ELEM?                             
         BZ    *+8                                                              
         BRAS  RE,ELEMDEL          DELETE 22/23 ELEM FIRST                      
*                                                                               
BLDDEM36 L     R6,DXDEMEL                                                       
         BRAS  RE,ELEMDEL          THEN DELETE 02/03 ELEM                       
         B     BLDDEM38                                                         
         DROP  R1                                                               
*                                                                               
* IF WE GET HERE, IT MEANS WE DID NOT FIND A 02/03 ELEM                         
* ADD NEW AFTER BDELEM (OR AFTER 02 FOR SPILL) *                                
*                                                                               
BLDDEM37 LA    R6,BDELEM           GET ADDRESS OF FIRST ELEM                    
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0               BUMP TO NEXT                                 
         CLI   0(R6),2             CAN WE ADD 02/03 HERE?                       
         BE    *-10                NO, KEEP LOOKING                             
*                                                                               
BLDDEM38 DS    0H                                                               
         BRAS  RE,ELEMADD                                                       
*                                                                               
BLDDEM39 LTR   R8,R8               TEST POST BUY DEMO ELEMENT                   
         JZ    BLDDEM40                                                         
         SR    R0,R0               CAN'T RELY R8 HAVING CORRECT ADDRESS         
         IC    R0,1(R6)            SO ADD AFTER 02/03 WE JUST ADDED             
         AR    R6,R0                                                            
* !!! NOTE !!! NEW ELEMENT IS IN WORK NOT ELEM                                  
         GOTO1 VRECUP,DMCB,BUYREC,WORK,(R6)                                     
         EJECT                                                                  
*=============================================================                  
* ADD DEMO LOOK-UP OVERRIDE ELEMENT IF NEEDED                                   
*=============================================================                  
                                                                                
BLDDEM40 DS    0H                                                               
         CLI   BUDLUSW,C'N'        TEST DO NOT REBUILD DLUELEM                  
         BE    BLDDEMX                                                          
         CLI   BUBKTYPE,0          USER ENTERED BT=                             
         BNE   BLDDEMX             ALWAYS WINS                                  
*                                                                               
         OC    SVNDEF(16),SVNDEF   TEST CANAD NETWORK                           
         BZ    *+12                                                             
         CLI   SVNDINDX,0          C,LOOK DOESN'T USE CURRENT NET               
         BE    BLDDEMX             SO CAN'T REBUILD DLUEL                       
*                                                                               
         MVI   ELCDLO,X'24'                                                     
         MVI   ELCDHI,X'24'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   *+8                                                              
         BRAS  RE,ELEMDEL                                                       
                                                                                
E        USING DLUELEM,ELEM                                                     
                                                                                
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'24'                                                       
         MVI   ELEM+1,6                                                         
         MVC   E.DLUBKTYP,BUBKTYPE                                              
         CLI   BUBKTYPE,0          USER ENTERED BT=                             
         BNE   BLDDEM42            ALWAYS WINS!                                 
         MVC   E.DLUBKTYP,SVOPTBT                                               
         CLI   SVOPTBT,0           USER ENTERED BT= IN OPTIONS                  
         BNE   BLDDEM42            ALWAYS WINS                                  
         MVC   E.DLUBKTYP,SVEBKTYP                                              
         CLI   SVEBKTYP,0          TEST EST BOOKTYPE                            
         BNE   BLDDEM42                                                         
         MVC   ELEM+2(1),SVBKTYPE  NO, THEN STATION/MARKET BOOKTYPE             
*                                                                               
BLDDEM42 CLI   BUYMD,C'R'          TEST RADIO                                   
         BNE   BLDDEM43                                                         
         CLI   SV1WPROF+6,C'Y'     TEST RADIO BUY LOOKUPS                       
         BNE   BLDDEMX                                                          
* INSERT MKTALPHA CODE IN DEMO LKUP ELEM                                        
         MVC   E.DLUBAMKT,SVMKTMKT                                              
         CLI   SVSTAMKT,C' '                                                    
         BNH   *+10                                                             
         MVC   E.DLUBAMKT,SVSTAMKT                                              
         B     BLDDEM50                                                         
*                                                                               
BLDDEM43 CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BNE   BLDDEM45            NO                                           
         MVI   ELEM+1,11           CORRECT ELEMENT LEN                          
         CLI   SVNDINDX,X'FF'      TEST MARKET 0 LOOKUP                         
         JE    BLDDEM44                                                         
         OC    SVNDEF(16),SVNDEF   TEST CANADIAN NETWORK                        
         BNZ   BLDDEM46                                                         
         CLI   BUYMD,C'T'          SO IF SELECT TV                              
         BE    *+12                                                             
         CLI   BUYMD,C'N'          OR LOCAL NETWORK                             
         BNE   BLDDEM50                                                         
*                                                                               
BLDDEM44 MVC   E.DLUBAMKT,SVMKTMKT  SET MARKET                                  
         MVC   E.DLUBSTOV,SVDMLST0  AND OVERRIDE STATION (IF ANY)               
         TM    SVDMLFLG,X'01'      TEST BBM LOOKUP                              
         BZ    *+10                                                             
         MVC   E.DLUBSTOV,SVDMLST1                                              
         MVC   E.DLUBFLGS,SVDMLFLG  SET IMPS/RTG SVC FLAG                       
*                                                                               
BLDDEM45 CLI   E.DLUBKTYP,0         IF BOOKTYPE THERE                           
         BH    BLDDEM50                                                         
         CLC   E.DLUBAMKT(8),SPACES  OR ANY OTHER DATA THERE                    
         BH    BLDDEM50                                                         
         B     BLDDEMX                                                          
*                                                                               
BLDDEM46 CLI   SVNDINDX,X'FF'      SPECIAL VALUE FOR NATIONAL                   
         JNE   BLDDEM47                                                         
         CLC   =C'NWK',SVMKTMKT                                                 
         JNE   BLDDEMX                                                          
         CLI   SV1WPROF+10,C'B'    TEST LOOK UP NATIONAL RTGS                   
         JNE   BLDDEMX                                                          
*                                                                               
         MVC   E.DLUBAMKT,SVMKTMKT                                              
         MVC   E.DLUBFLGS,SVDMLFLG                                              
         MVC   E.DLUBKTYP,SVDFLTBT                                              
         J     BLDDEM50                                                         
*                                                                               
BLDDEM47 SR    RF,RF               THIS FOR NET AT NET LEVEL                    
         IC    RF,SVNDINDX                                                      
         BCTR  RF,0                                                             
         MHI   RF,L'SVNDDEM                                                     
         LA    RF,SVNDDEM(RF)                                                   
         USING SVNDDEMD,RF                                                      
         MVC   E.DLUBAMKT,SVNDDMKT  OVERRIDE MARKET                             
         MVC   E.DLUBFLGS,SVNDDFLG  SET IMPS/RTGSVC                             
         MVC   E.DLUBSTOV,SVNDDST0                                              
         TM    SVNDDFLG,X'01'                                                   
         BZ    *+10                                                             
         MVC   E.DLUBSTOV,SVNDDST1                                              
*                                                                               
         CLI   SVNETBTS,X'01'       TEST CABLE                                  
         BNE   BLDDEM49                                                         
         CLI   SVNDDMKT,C'A'        TEST ALPHA MKT FROM CBLDEF                  
         BL    BLDDEMX              IF NONE, NO DLU ELEM                        
         MVC   E.DLUBSTOV,SVDMLST0                                              
         TM    SVNDDFLG,X'01'       TEST BBM LOOKUP                             
         BZ    *+10                                                             
         MVC   E.DLUBSTOV,SVDMLST1                                              
*                                                                               
BLDDEM49 CLI   E.DLUBKTYP,0        IF BOOKTYPE THERE                            
         BH    BLDDEM50                                                         
         CLC   E.DLUBAMKT(8),SPACES   OR ANY OTHER DATA THERE                   
         BNH   BLDDEMX                                                          
         DROP  E,RF                                                             
*                                                                               
BLDDEM50 BRAS  RE,ELEMADD                                                       
*                                                                               
BLDDEMX  MVI   BUDLUSW,0                                                        
         XIT1                                                                   
         EJECT                                                                  
ELEMDEL  NTR1                                                                   
         GOTO1 VRECUP,DMCB,BUYREC,(R6)                                          
         XIT1                                                                   
*                                                                               
ELEMADD  NTR1                                                                   
         GOTO1 VRECUP,DMCB,BUYREC,ELEM,(R6)                                     
         XIT1                                                                   
         EJECT                                                                  
*=============================================================                  
* BUILD X'50' ELEMENT WITH NON-TRADITIONAL NAMES                                
*=============================================================                  
                                                                                
BLDNTDEM NTR1  BASE=*,LABEL=*                                                   
         XC    ELEM,ELEM                                                        
         MVI   ELEM,NTDELCDQ       X'50'                                        
*                                                                               
         LA    RE,ELEM+2           POINT TO FIRST SLOT                          
*                                                                               
         LHI   RF,SVNTDMS-BUYSAVE  NON-TRAD INDEX DEMO LIST                     
         AR    RF,RA                                                            
*                                                                               
BLDNT2   MVC   0(8,RE),0(RF)       MOVE FIRST DEMO                              
         LA    RE,9(RE)            NEXT SLOT IN ELEM                            
         LA    RF,8(RF)            NEXT SLOT IN LIST                            
         CLI   0(RF),0             TEST ANY MORE VALUES                         
         JNE   BLDNT2                                                           
         LA    R0,ELEM                                                          
         SR    RE,R0               GIVES ELEM LEN                               
         STC   RE,ELEM+1           SET LENGTH                                   
                                                                                
* FIND EXISTING ELEMENT AND COMPARE                                             
                                                                                
         LA    R6,BDELEM                                                        
BLDNT4   LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         JE    BLDNT6                                                           
         CLI   0(R6),X'50'                                                      
         JNE   BLDNT4                                                           
         LLC   RE,1(R6)            GET ELEMENT LENGTH                           
         BCTR  RE,0                                                             
         EX    RE,SAMEX50                                                       
         JE    BLDNTX                                                           
*                                                                               
* IF SPOT DESKTOP MODE, LET SPBUY39 HANDLE UPDATING THE NONT DEMO ELEM.         
* THIS IS BECAUSE SPBUY39 WOULD HAVE PREVIOUSLY SET FLAG BITS AND               
* DELETING THE ELEM, WOULD RESULT IN LOSING ANY BITS PREVIOUSLY SET             
*                                                         -HWON 9/26/16         
         TM    SVXFRCTL,SVXFR_SDT  TEST SPOT DESKTOP MODE                       
         BNZ   BLDNT9               YES, SKIP IT                                
*                                                                               
         BRAS  RE,ELEMDEL                                                       
*                                                                               
BLDNT6   BRAS  RE,ELEMADD                                                       
*                                                                               
BLDNT9   OI    BDSTAT2,BDSNTDMS                                                 
BLDNTX   XIT1                                                                   
*                                                                               
SAMEX50  CLC   0(0,R6),ELEM  *EXECUTED*                                         
         LTORG                                                                  
         EJECT                                                                  
*============================================================*                  
* SUBROUTINE TO CALL ADDS INTERFACE --> SPADINT (T00A5E)     *                  
* ON ENTRY R1 HAS A(ADDS BLOCK)                              *                  
*============================================================*                  
         SPACE 1                                                                
BUADDS   NTR1  BASE=*,LABEL=*                                                   
         L     RA,VBUYSAVE                                                      
         USING BUYSAVE,RA                                                       
         L     R3,VBUYTWA                                                       
         USING T211FFD,R3                                                       
*                                                                               
         LR    R4,R1               GET ADDRESS OF BLOCK                         
         USING SPADINTD,R4                                                      
*                                                                               
         CLI   ADACTN,C'C'         TEST CHANGE                                  
         BE    BUADDS4                                                          
         CLI   ADACTN,C'S'         TEST SEND                                    
         JNE   *+2                                                              
         CLI   BUYMD,C'T'                                                       
         BNE   BUADDS2                                                          
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(NOTVADDS)                                            
         GOTO1 ERROR                                                            
*                                                                               
BUADDS2  OC    SVRSMKT,SVRSMKT                                                  
         BNZ   BUADDS4                                                          
         CLI   BUYMD,C'R'          RTG SVC MKT NOT REQUIRED FOR RADIO           
         BE    BUADDS4                                                          
         LA    RE,5                SET ERROR MESSAGE NUMBER                     
         B     BUADDS12                                                         
*                                                                               
BUADDS4  MVC   ADACOMFC,VCOMFACS   SET A(COMFACS)                               
         ST    R3,ADATWA           SET A(TWA)                                   
         MVC   ADQAGYMD,SVAGYMD                                                 
         MVC   ADQCLT,SVCLT                                                     
         MVC   ADQPRD,SVSNDP1                                                   
         MVC   ADQPRD2,SVSNDP2                                                  
         MVC   ADQAPRD,SVSNDAP1                                                 
         MVC   ADQAPRD2,SVSNDAP2                                                
         MVC   ADQEST,SVEST                                                     
         MVC   ADQMKT(5),SVMKT                                                  
         MVC   ADQAGY,AGYALPHA                                                  
         MVC   ADQMED,BUYMD                                                     
         MVC   ADUSRID,T211FFD+10                                               
         MVC   ADQUEST,BUYBU       MOVE BUYER'S WHOLE NAME                      
         MVC   ADQSTART,SVSNDST                                                 
         MVC   ADQEND,SVSNDEND                                                  
*                                                                               
         MVI   ADRTGSVC,C'N'                                                    
         CLI   SVCPROF+3,C'0'                                                   
         BE    *+8                                                              
         MVI   ADRTGSVC,C'A'                                                    
         MVC   ADRSMKT,SVRSMKT                                                  
*                                                                               
         GOTO1 VCALLOV,DMCB,0,X'D9000A5E'                                       
         CLI   4(R1),X'FF'                                                      
         JE    *+2                                                              
         L     RF,0(R1)                                                         
*                                                                               
         GOTO1 (RF),DMCB,(R4)                                                   
         TM    ADERRS,ADERREQ      TEST REQ ERROR                               
         BZ    BUADDS10                                                         
         MVC   BUYMSG(18),=C'ADDS REQUEST ERROR'                                
         MVI   ERRAREA,X'FF'                                                    
         GOTO1 ERROR                                                            
*                                                                               
BUADDS10 TM    ADERRS,ADERDM       TEST DMGR ERROR                              
         BZ    BUADDS20                                                         
*                                                                               
         ZIC   RE,ADRECTYP                                                      
*                                                                               
BUADDS12 SLL   RE,4                X 16                                         
         LA    RE,ADDSRECS-16(RE)                                               
         XC    BUYMSG,BUYMSG                                                    
         MVC   BUYMSG(27),=C'UNABLE TO SEND - DATA ERROR'                       
         MVC   BUYMSG+28(16),0(RE)                                              
         MVI   ERRAREA,X'FF'                                                    
         GOTO1 ERROR                                                            
*                                                                               
BUADDS20 CLI  ADERRS,0                                                          
         JNE   *+2                                                              
*                                                                               
BUADDSX  XIT1                                                                   
*                                                                               
ADDSRECS DS    0CL8                                                             
         DC    CL16'STATUS RECORD'                                              
         DC    CL16'STATION RECORD'                                             
         DC    CL16'DIRECTORY ADDR'                                             
         DC    CL16'ID RECORD     '                                             
         DC    CL16'NO RTG SVC MKT'                                             
         LTORG                                                                  
         EJECT                                                                  
*=======================================================*                       
* DAILY SCHEDULING FUNCTION.                            *                       
* CONSTRUCT A TABLE FOR THE ENTIRE BUY PERIOD           *                       
* OF ACTUAL/DESIRED SPOTS PER DAY                       *                       
* BUTRCODE = C'X' TO BUILD TABLE ONLY (FOR DISPLAY)     *                       
*=======================================================*                       
         SPACE 1                                                                
DSKED    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         OI    BDSTAT2,X'80'       SET DAILY SKED FLAG                          
         GOTO1 VDATCON,DMCB,(3,BDSTART),WORK                                    
         GOTO1 (RF),(R1),(3,BDEND),WORK+12  SAVE YYMMDD END DATE                
*                                                                               
         MVI   ERRCD,CANTSKED                                                   
         CLI   BDWKIND,C'O'                                                     
         BNE   DSKERR                                                           
* MAKE SURE BUY IS A ROTATOR                                                    
         SR    RE,RE                                                            
         IC    RE,BDSEDAY                                                       
         SRDL  RE,4                                                             
         SRL   RF,28                                                            
         CR    RE,RF                                                            
         BE    DSKERR                                                           
*                                                                               
DSK2     MVI   ERRCD,SKEDERR3                                                   
         CLI   BDWKS,14            MAX 14 WEEKS TOTAL DURATION                  
         BH    DSKERR                                                           
*                                                                               
         L     R4,AREC5                                                         
         USING SKEDTAB,R4                                                       
*                                                                               
         LR    R0,R4               SET 'TO' ADDR                                
         LHI   R1,2000             SET 'TO' LEN                                 
         SR    RE,RE               SET 'FROM' ADDR = 0                          
         SR    RF,RF               SET 'FROM' LEN = 0                           
         MVCL  R0,RE                                                            
*                                                                               
         SR    R0,R0               CLEAR COUNTER                                
DSK4     BCTR  R0,0                DECREMENT COUNTER                            
         GOTO1 VDATCON,DMCB,WORK,(2,SKEDDATE)                                   
*                                                                               
         LA    R4,SKEDNEXT                                                      
         GOTO1 VADDAY,DMCB,WORK,WORK+6,F'1'                                     
         MVC   WORK(6),WORK+6                                                   
         CLC   WORK(6),WORK+12     CURRENT DATE TO BUY END DATE                 
         BNH   DSK4                                                             
         LPR   R0,R0                                                            
         AHI   R0,6                ADD 6 TO ROUND UP                            
         SRDA  R0,32                                                            
         D     R0,=F'7'                                                         
         STC   R1,BDWKS            MAKE SURE BDWKS SET FOR DISPLAY              
         EJECT                                                                  
* NOW WORK OUT WHICH DAYS MAY BE SCHEDULED *                                    
         SPACE 1                                                                
DSK6     XC    WORK,WORK                                                        
         LA    R0,7                                                             
         LA    R1,WORK                                                          
         LA    RE,X'40'            X'40'=MONDAY, X'01'=SUNDAY                   
*                                                                               
DSK10    EX    RE,*+8                                                           
         B     *+8                                                              
         TM    BDDAY,0  **EXECUTED**                                            
         BZ    *+8                                                              
         MVI   0(R1),C'Y'          SET FLAG                                     
         SRL   RE,1                SHIFT DAY                                    
         LA    R1,1(R1)                                                         
         BCT   R0,DSK10                                                         
         MVC   WORK+7(7),WORK      COPY FIRST WEEK TO SECOND                    
* NOW SHIFT THE FLAGS SO THE START DATE DAY IS FIRST                            
         ZIC   RE,BDSEDAY                                                       
         SRL   RE,4                                                             
         LA    RE,WORK-1(RE)                                                    
         MVC   WORK(7),0(RE)                                                    
         SPACE 1                                                                
* NOW SET FLAGS IN TABLE FOR DAYS THAT MAY NOT BE SCHEDULED *                   
         SPACE 1                                                                
         GOTO1 VDATCON,DMCB,(3,BDEND),(2,HALF)                                  
         L     R4,AREC5                                                         
*                                                                               
DSK12    LA    RE,WORK                                                          
         LA    RF,7                                                             
*                                                                               
DSK14    OC    SKEDDATE,SKEDDATE   TEST E-O-T                                   
         BZ    DSK16                                                            
         CLI   0(RE),C'Y'          TEST SPOTS ALLOWED                           
         BE    *+8                                                              
         OI    SKEDFLAG,X'80'      SET 'NO SKED' FLAG                           
*                                                                               
         CLC   SKEDDATE,HALF       TEST PAST BUY END DATE                       
         BNH   *+8                                                              
         OI    SKEDFLAG,X'80'      SET FLAG                                     
*                                                                               
         LA    R4,SKEDNEXT                                                      
         LA    RE,1(RE)                                                         
         BCT   RF,DSK14                                                         
         B     DSK12                                                            
         EJECT                                                                  
*==========================================================*                    
* COUNT CURRENT SPOTS AND POST TO DATE TABLE               *                    
*==========================================================*                    
         SPACE 1                                                                
DSK16    LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0D'                                                     
*                                                                               
         L     R4,AREC5            SET INITIAL TABLE POINTER                    
*                                                                               
DSK20    BRAS  RE,NEXTEL                                                        
         BNE   DSK30                                                            
*                                                                               
DSK22    CLC   2(2,R6),SKEDDATE    MATCH DATE                                   
         BE    DSK24                                                            
         LA    R4,SKEDNEXT                                                      
         OC    SKEDDATE,SKEDDATE                                                
         BNZ   DSK22                                                            
         CLI   0(R6),X'0B'         TEST REGEL                                   
         BNE   DSK20               NO - MAY BE OTO OUTSIDE BUY DATES            
         GOTO1 VDATCON,DMCB,(2,2(R6)),(4,ERRTEXT)                               
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(BADSKED)                                               
         B     DSKERR                                                           
*                                                                               
DSK24    MVI   ERRCD,DSKEDNG       SET CAN'T DSKED                              
         TM    SKEDFLAG,X'80'      TEST NO SKED ALLOWED                         
         BO    DSKERR                                                           
         LA    R1,SKEDPAID                                                      
         OC    4(2,R6),4(R6)       TEST PAID                                    
         BNZ   *+8                                                              
         LA    R1,SKEDUNPD                                                      
         ZIC   RE,0(R1)                                                         
         TM    6(R6),X'C0'         TEST MINUS OR MINUSSED                       
         BNZ   DSK20                                                            
         LA    RE,1(RE)                                                         
         STC   RE,0(R1)                                                         
*                                                                               
         IC    RE,SKEDOLD          BUMP TOTAL SPOTS                             
         LA    RE,1(RE)                                                         
         STC   RE,SKEDOLD                                                       
         B     DSK20                                                            
         EJECT                                                                  
*=========================================================*                     
* POST REQUESTED SPOTS INTO TABLE                         *                     
* BUDSKED CONTAINS EBCDIC NUMBER OF SPOTS FOR EACH DAY    *                     
* IF BUDSKED = 7X'00' THEN NO CHANGE WAS REQUESTED        *                     
*=========================================================*                     
         SPACE 1                                                                
DSK30    CLI   BUTRCODE,C'X'       TEST 'BUILD THE TABLE' CALL                  
         BE    DSKX                YES - DONE                                   
*                                                                               
         L     R4,AREC5            POINT TO TABLE                               
         LA    R5,BUDSKED          POINT TO SCHEDULE DATA                       
*                                                                               
DSK32    LA    R0,7                                                             
         OC    0(7,R5),0(R5)       ANY DATA FOR THIS WEEK ?                     
         BNZ   DSK36                                                            
*                                                                               
DSK34    MVC   SKEDNEW,SKEDOLD     NO DATA - SET NEW=OLD                        
         LA    R4,SKEDNEXT                                                      
         LA    R5,1(R5)                                                         
         BCT   R0,DSK34                                                         
         B     DSK40                                                            
*                                                                               
DSK36    MVC   SKEDNEW,0(R5)       MOVE SPOTS TO TABLE                          
         NI    SKEDNEW,X'0F'       DROP ZONE BITS                               
         CLI   SKEDNEW,0           TEST SPOTS SCHEDULED THIS DATE               
         BE    DSK38               NO - IGNORE                                  
         TM    SKEDFLAG,X'80'      TEST NO SPOTS ALLOWED                        
         BO    DSKDTER1                                                         
*                                                                               
DSK38    LA    R4,SKEDNEXT                                                      
         LA    R5,1(R5)                                                         
         BCT   R0,DSK36                                                         
*                                                                               
DSK40    OC    SKEDDATE,SKEDDATE   TEST MORE DATES IN TABLE                     
         BNZ   DSK32                                                            
         EJECT                                                                  
*==================================================*                            
* NOW CHANGE THE NUMBER OF SPOTS IN THE BUY RECORD *                            
*==================================================*                            
         SPACE 1                                                                
         L     R4,AREC5                                                         
         USING SKEDTABD,R4                                                      
*                                                                               
DSK50    DS    0H                                                               
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0D'                                                     
         LA    R6,BDELEM                                                        
         XC    BUDEMS,BUDEMS       CLEAR SPACE FOR ELEMENT TABLE                
         LA    R7,BUDEMS+4         LEAVE A 0 ENTRY AT TOP OF TABLE              
*                                                                               
         ZIC   R8,SKEDNEW          GET NEW SPOTS                                
         ZIC   R0,SKEDOLD          GET OLD SPOTS                                
         SR    R8,R0               TEST ANY DIFFERENCE                          
         BZ    DSK80               NO                                           
         BP    DSK60               MORE NEW SPOTS - GO ADD THEM                 
         SPACE 1                                                                
* NEW SPOTS LESS THAN OLD *                                                     
         SPACE 1                                                                
         LPR   R8,R8               MAKE NUMBER OF SPOTS POSITIVE                
DSK52    BRAS  RE,NEXTEL                                                        
         BNE   DSK54                                                            
         CLC   2(2,R6),SKEDDATE    RIGHT DATE                                   
         BL    DSK52                                                            
         BH    DSK54                                                            
         TM    6(R6),X'C0'         TEST MINUS OR MINUSSED                       
         BNZ   DSK52               YES - IGNORE                                 
         ST    R6,0(R7)            SAVE ELEMENT ADDRESS                         
         OC    4(2,R6),4(R6)       TEST PAID                                    
         BZ    *+8                                                              
         OI    0(R7),X'80'         SET PAID FLAG                                
         ZIC   RE,1(R6)                                                         
         AR    RE,R6                                                            
         CLI   0(RE),X'10'         TEST MATCHED                                 
         BNE   *+8                                                              
         OI    0(R7),X'40'         SET MATCHED FLAG                             
         LA    R7,4(R7)            BUMP TABLE POINTER                           
         B     DSK52                                                            
         EJECT                                                                  
DSK54    DS    0H                                                               
         AHI   R7,-4               BACK UP ONE ENTRY                            
         OC    0(4,R7),0(R7)       ANY MORE ELEMENTS                            
         BZ    DSKDTER2                                                         
         CLI   0(R7),0             TEST PAID/MATCHED                            
         BNE   DSK54               YES - SKIP                                   
         L     R6,0(R7)            POINT TO THE ELEMENT                         
DSK56    DS    0H                                                               
         BRAS  RE,ELEMDEL     DELETE THE ELEMENT                                
         CLI   0(R6),X'10'    TEST NEXT ELEMENT RELATED                         
         BL    DSK58          NO                                                
         CLI   0(R6),X'19'                                                      
         BL    DSK56                                                            
*                                                                               
DSK58    BCT   R8,DSK54            GO DELETE NEXT                               
         B     DSK80                                                            
         EJECT                                                                  
* NEW SPOTS MORE THAN OLD *                                                     
         SPACE 1                                                                
DSK60    DS    0H                                                               
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(CLTFRZN)                                               
         TM    SVCOPT2,X'08'       TEST CLIENT FROZEN                           
         BO    DSKERR                                                           
         MVC   BUELPRD(2),BDMASPRD                                              
         GOTO1 VBLDEL              BUILD NEW ELEMENT IN ELEM                    
         MVC   ELEM+2(2),SKEDDATE                                               
*                                                                               
         GOTO1 TESTGLS,DMCB,ELEM   TEST GOALS PRESENT IF NEEDED                 
         SPACE 1                                                                
* FIND INSERTION POINT *                                                        
         SPACE 1                                                                
DSK61    MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0D'                                                     
         SPACE 1                                                                
* FIND LAST DEMO OR POST/BUY DEMO ELEMENT *                                     
         SPACE 1                                                                
         LA    R6,BDELEM                                                        
DSK62    ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),X'02'                                                      
         BE    DSK62X                                                           
         CLI   0(R6),X'22'                                                      
         BE    DSK62X                                                           
         CLI   0(R6),X'03'                                                      
         BE    DSK62X                                                           
         CLI   0(R6),X'23'                                                      
         BNE   DSK64                                                            
DSK62X   LR    R7,R6                                                            
         B     DSK62                                                            
*                                                                               
DSK64    BRAS  RE,NEXTEL2                                                       
         BNE   DSK68                                                            
*                                                                               
DSK66    CLC   SKEDDATE,2(R6)                                                   
         BL    DSK68                                                            
         LR    R7,R6               SAVE ADDRESS OF PREVIOUS ELEMENT             
         BRAS  RE,NEXTEL                                                        
         BE    DSK66                                                            
*                                                                               
DSK68    ZIC   R6,1(R7)            GET LENGTH OF PREVIOUS ELEMENT               
         AR    R6,R7               ADD AFTER IT                                 
DSK70    DS    0H                                                               
         BRAS  RE,ELEMADD                                                       
         BCT   R8,DSK70                                                         
         EJECT                                                                  
DSK80    LA    R4,SKEDNEXT         NEXT TABLE ENTRY                             
         OC    SKEDDATE,SKEDDATE   TEST MORE DATA                               
         BNZ   DSK50                                                            
*                                                                               
         MVI   BUWHY3,X'80'        SET SKED CHANGE FLAG                         
         GOTO1 SETCHGDT                                                         
*                                                                               
DSK82    GOTO1 PUTREC                                                           
*                                                                               
         MVI   RCLOPT,RCLDSK                                                    
         GOTO1 CALLDSP                                                          
*                                                                               
DSKX     DS    0H                                                               
         XIT1                                                                   
*                                                                               
DSKDTER1 LA    R1,LDSKMSG1                                                      
         B     DSKDTERR                                                         
DSKDTER2 LA    R1,LDSKMSG2                                                      
*                                                                               
DSKDTERR MVC   BUYMSG(10),=C'* ERROR *'                                         
         ZIC   R5,0(R1)                                                         
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   BUYMSG+11(0),1(R1)                                               
         LA    R5,BUYMSG+13(R5)                                                 
         GOTO1 VDATCON,DMCB,(2,SKEDDATE),(4,(R5))                               
         MVI   ERRAREA,X'FF'       SET FLAG FOR MESSAGE PRESENT                 
*                                                                               
DSKERR   GOTO1 ERROR                                                            
*                                                                               
LDSKMSG1 DC    AL1(L'DSKMSG1)                                                   
DSKMSG1  DC    C'CAN''T SCHEDULE SPOTS FOR'                                     
LDSKMSG2 DC    AL1(L'DSKMSG2)                                                   
DSKMSG2  DC    C'SCHEDULED SPOTS LESS THAN ACTUAL FOR'                          
         EJECT                                                                  
*==============================================================*                
* CREATE OR UPDATE ACTIVITY ELEMENT                            *                
*==============================================================*                
         SPACE 1                                                                
SETACTV  NTR1  BASE=*,LABEL=*                                                   
         GOTO1 VDATCON,DMCB,(5,0),(3,SVTODAYB)                                  
* CREATE OR UPDATE ACTIVITY ELEMENT                                             
         L     R6,AREC                                                          
         CLI   0(R6),X'10'                                                      
         BNH   SETACTVX                                                         
         LA    R6,24(R6)                                                        
         SR    R0,R0                                                            
SETACTV2 IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),X'99'                                                      
         BE    SETACTV4                                                         
         CLI   0(R6),0                                                          
         BNE   SETACTV2                                                         
* CREATE NEW ELEM                                                               
         MVC   DUB(2),=X'990C'     DUB IS AVAILABLE                             
         GOTO1 VRECUP,DMCB,AREC,DUB,(R6)                                        
         XC    2(10,R6),2(R6)                                                   
         MVC   2(2,R6),SVPASSWD                                                 
         MVC   4(3,R6),SVTODAYB                                                 
         B     SETACTVX                                                         
* UPDATE EXISTING ELEM                                                          
SETACTV4 MVC   7(2,R6),SVPASSWD                                                 
         MVC   9(3,R6),SVTODAYB                                                 
*                                                                               
SETACTVX XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*==============================================================*                
* CREATE OR UPDATE ACTIVITY ELEMENT                                             
* CHANGE OVERLAY SETS ELEMENT CODE IN SVRSNEL IF THIS CHANGE                    
*  REQUIRES A REASON CODE. THE ELCODE IS RESET AT THE END OF                    
*  THIS ROUTINE.                                                                
*==============================================================*                
         SPACE 1                                                                
SETRSN   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R6,AREC             REMOVE RSNCOD ELEM FROM RECORD               
         CLI   0(R6),X'10'         TEST BUYREC                                  
         BNH   SETRSNX                                                          
         LA    R6,24(R6)                                                        
         SR    R0,R0                                                            
SETRSN2  IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),X'90'                                                      
         BE    SETRSN4                                                          
         CLI   0(R6),0                                                          
         BNE   SETRSN2                                                          
         B     SETRSN10                                                         
*                                                                               
SETRSN4  GOTO1 VRECUP,DMCB,AREC,(R6)  DELETE EXISTING ELEMENT                   
*                                                                               
SETRSN10 LHI   R4,SVRSNEL-BUYSAVE                                               
         AR    R4,RA                                                            
         CLI   0(R4),0             MAY NOT BE PRESENT !                         
         BE    SETRSNX                                                          
* INSERT ELEMENT AT EOF                                                         
         L     R6,AREC                                                          
         LA    R6,24(R6)                                                        
         SR    R0,R0                                                            
SETRSN12 IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   SETRSN12                                                         
         GOTO1 VRECUP,DMCB,AREC,(R4),(R6)                                       
*                                                                               
SETRSNX  LHI   R4,SVRSNFLD-BUYSAVE                                              
         AR    R4,RA                                                            
         MVI   0(R4),0             CLEAR FIELD ID (NOT ELCODE)                  
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*===============================================================*               
* EDIT ROUTINES FOR 'SEND=XXX,PR1-PR2',DATES                    *               
* R2 POINTS TO FLDHDR ON ENTRY                                  *               
* EXIT WITH CC EQ IF NO ERRORS                                  *               
*===============================================================*               
         SPACE 1                                                                
EDSEND   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   AREC,AREC1                                                       
         L     R4,AREC             POINT TO SCANNER TABLE                       
         XC    0(256,R4),0(R4)                                                  
*                                                                               
         CLI   SVRCLOPT,RCLAUTA    TEST AUTO RECALL                             
         BE    EDS70                                                            
         MVI   ERRCD,INVERR                                                     
         CLI   BUTRCODE,C'D'       TEST DARE SEND                               
         BE    *+12                                                             
         TM    SVAFLAG1,X'80'      TEST ADDS INTFC ENABLED                      
         BZ    EDSNEQ                                                           
         L     RF,VCOMFACS                                                      
         L     RF,CSCANNER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(R2),(5,(R4))                                          
*                                                                               
         CLI   1(R4),0             TEST SENDER ENTERED                          
         BE    EDS8                NO                                           
         CLI   1(R4),3             TEST TOO LONG                                
         BH    EDSNEQ                                                           
         MVC   SVSENDER,22(R4)                                                  
*                                                                               
         CLI   BUTRCODE,C'D'       TEST DARE SEND                               
         BNE   EDS8                                                             
         MVI   BYTE2,0                                                          
         L     R2,ASVDARE          IF NO FLIGHT                                 
         USING SVDARED,R2                                                       
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(INVFLIGT)                                              
         CLI   0(R4),6             DARE## ?                                     
         BH    EDSNEQ                                                           
         CLC   =C'OM',12(R4)       CHECKING OM##?                               
         BE    EDS1                                                             
         CLI   0(R4),4                                                          
         BH    EDS3                                                             
         B     EDS2                                                             
*                                                                               
EDS1     CLI   0(R4),4             OM## ?                                       
         BH    EDSNEQ                                                           
         CLI   0(R4),2                                                          
         BH    EDS3                                                             
*                                                                               
EDS2     MVC   NERRCD,=Y(FLTREQD)                                               
         CLI   SVDRFLAG,C'Y'       NO FLIGHT ENTERED BUT                        
         BE    EDSNEQ              FLIGHT REQUIRED                              
         B     EDS8                                                             
*                                                                               
EDS3     MVC   NERRCD,=Y(INVFLIGT)                                              
         XC    WORK(10),WORK                                                    
         ZIC   R6,0(R4)                                                         
         CLC   =C'OM',12(R4)                                                    
         BE    EDS3A                                                            
         AHI   R6,-5               ISOLATE FLIGHT NUMBER                        
         LA    RE,16(R4)                                                        
         B     EDS3B                                                            
*                                                                               
EDS3A    AHI   R6,-3               ISOLATE FLIGHT NUMBER                        
         LA    RE,14(R4)                                                        
*                                                                               
EDS3B    EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(RE)                                                    
         LA    R7,WORK                                                          
EDS4     CLI   0(R7),X'F0'         VALID NUMERIC                                
         BL    EDSNEQ                                                           
         CLI   0(R7),X'F9'                                                      
         BH    EDSNEQ                                                           
         LA    R7,1(R7)                                                         
         CLI   0(R7),0                                                          
         BNE   EDS4                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         PACK  DUB,WORK(0)                                                      
         CVB   R6,DUB                                                           
         CHI   R6,16               MUST BE 0-16                                 
         BH    EDSNEQ                                                           
         STC   R6,BYTE2            SAVE FLIGHT NUMBER                           
*                                                                               
         CLI   BYTE2,0             USER ENTERED FLIGHT 00?                      
         BE    EDS8                YES                                          
*                                                                               
         BCTR  R6,0                DOES FLIGHT EXIST                            
         MHI   R6,4                                                             
         LA    R7,SVDRSTR1         START OF FLIGHTS                             
         AR    R7,R6                                                            
         OC    0(4,R7),0(R7)                                                    
         BZ    EDSNEQ              INVALID FLIGHT NUMBER                        
         DROP  R2                                                               
*                                                                               
EDS8     MVC   SVSNDP1,SVPOLPRD      PRESET PRODUCT CODES                       
         CLI   SVSNDP1,0                                                        
         BNE   *+10                                                             
         MVC   SVSNDP1,SVPRD                                                    
         MVC   SVSNDAP1,QPRD                                                    
         MVI   SVSNDP2,0                                                        
         XC    SVSNDAP2,SVSNDAP2                                                
*                                                                               
         GOTO1 (RF),(R1),,,C',=,-'   EDIT FOR PIGGYBACKS                        
         L     R4,AREC                                                          
         LA    R4,32(R4)           POINT TO SECOND ENTRY                        
         MVI   ERRCD,PRDERR                                                     
         CLI   0(R4),0             TEST PRD PRESENT                             
         BNE   EDS10               YES - CONTINUE                               
         CLI   BUTRCODE,C'D'       TEST DARE                                    
         BE    EDS16               YES - SKIP TESTS                             
         CLI   SVSNDP1,X'FF'       IF PRD NOT POL                               
         BNE   EDS16               CONTINUE                                     
         CLI   SVCXTRA+13,C'Y'     TEST PRD REQ'D                               
         BE    EDSNEQ              YES - ERROR                                  
         B     EDS16               ELSE CHECK FOR PERIOD                        
*                                                                               
EDS10    CLI   BUTRCODE,C'D'       TEST DARE SEND                               
         BE    *+12                                                             
         CLI   SVCXTRA+13,C'Y'     TEST PRD REQ'D                               
         BNE   EDSNEQ              NO - ERROR                                   
         CLI   0(R4),3             TEST LENGTH                                  
         BH    EDSNEQ                                                           
         LA    R1,12(R4)                                                        
         BAS   RE,EDSPRD                                                        
         MVC   SVSNDAP1(4),0(RF)   MOVE PRODUCT CODE/NUMBER                     
*                                                                               
         CLI   1(R4),0             TEST FOR SECOND PRD                          
         BE    EDS16               NO                                           
         MVI   ERRCD,INVPTNR                                                    
         CLI   1(R4),3                                                          
         BH    EDSNEQ                                                           
         LA    R1,22(R4)                                                        
         BAS   RE,EDSPRD                                                        
         MVC   SVSNDAP2(4),0(RF)   MOVE PRODUCT CODE/NUMBER                     
*                                                                               
EDS16    LA    R4,32(R4)                                                        
         CLI   BUTRCODE,C'D'       TEST DARE SEND                               
         BNE   EDS18                                                            
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(DAREFMT)                                               
         OC    0(2,R4),0(R4)       TEST FOR MORE INPUT                          
         BNZ   EDSNEQ              SHOULD NOT BE ANY MORE                       
         B     EDS30                                                            
                                                                                
EDS18    XC    SVSNDST,SVSNDST                                                  
         XC    SVSNDEND,SVSNDEND                                                
         LA    R2,WORK             INPUT MUST = DATES                           
         MVC   WORK,SPACES                                                      
         OC    0(2,R4),0(R4)       TEST FOR MORE INPUT                          
         BNZ   EDS20                                                            
         GOTO1 VDATCON,DMCB,(2,SVSTARTP),(5,WORK)                               
         MVI   WORK+8,C'-'                                                      
         GOTO1 VDATCON,DMCB,(2,SVENDP),(5,WORK+9)                               
         MVI   BYTE,17                                                          
         MVI   ERRCD,NEEDPER                                                    
         BAS   RE,CKPER            CHECK PERIOD                                 
         BNE   EDSNEQ                                                           
         B     EDS30                                                            
*                                                                               
EDS20    MVI   ERRCD,PERERR                                                     
         ZIC   R1,0(R4)            L'1ST DATE                                   
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),12(R4)                                                   
         AR    R2,R1                                                            
         ZIC   R1,1(R4)            L'2ND DATE                                   
         LTR   R1,R1                                                            
         BZ    EDSNEQ                                                           
         MVI   1(R2),C'-'                                                       
         LA    R2,2(R2)            BUMP R3 TO POINT TO NEXT FIELD               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),22(R4)                                                   
         AR    R2,R1                                                            
         LA    R2,1(R2)                                                         
         LA    R1,WORK                                                          
         SR    R2,R1               R3 = L'TOTAL INPUT                           
         STC   R2,BYTE                                                          
*                                                                               
         LA    R4,32(R4)           THERE CAN'T BE ANY MORE INPUT                
         OC    0(2,R4),0(R4)                                                    
         BNZ   EDSNEQ                                                           
         MVI   ERRCD,PDTOOBIG                                                   
         BAS   RE,CKPER            CHECK PERIOD                                 
         BNE   EDSNEQ                                                           
         USING PERVALD,R4                                                       
*                                                                               
         MVI   ERRCD,NOTINEST                                                   
         CLC   PVALCSTA,SVSTARTP    ENSURE SEND DATE IN EST DATES               
         BL    EDSNEQ                                                           
         CLC   PVALCSTA,SVENDP                                                  
         BH    EDSNEQ                                                           
         CLC   PVALCEND,SVSTARTP                                                
         BL    EDSNEQ                                                           
         CLC   PVALCEND,SVENDP                                                  
         BH    EDSNEQ                                                           
*                                                                               
         MVC   SVSNDST,PVALCSTA    COMPRESSED SAVE START & END DATES            
         MVC   SVSNDEND,PVALCEND                                                
         DROP  R4                                                               
         B     EDS30                                                            
*                                                                               
EDS30    DS    0H                                                               
         CLI   BUTRCODE,C'D'       TEST DARE SEND                               
         BE    EDS60                                                            
         SPACE 1                                                                
*==============================================================*                
* NOW WE NEED TO FIND OUT IF THERE ARE ANY BUYS ON THE FILE    *                
* FOR THIS BRAND OR BRANDS                                     *                
*==============================================================*                
         SPACE 1                                                                
         CLI   SVPRD,X'FF'         FOR POL BUYING ONLY                          
         BNE   EDSEQ                                                            
         MVI   ERRCD,NORECS                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(13),SVKEY                                                    
         MVC   KEY+3(1),SVSNDP1    SET TO READ PASSIVE POINTERS                 
         GOTO1 HIGH                                                             
         B     EDS42                                                            
*                                                                               
EDS40    DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
EDS42    CLC   KEY(10),KEYSAVE     A-M/CLT/PRD/MKT/STA/EST                      
         BNE   EDSNEQ                                                           
         CLI   SVSNDP1,X'FF'                                                    
         BE    EDSXIT                                                           
*                                                                               
* NEED TO READ BUY RECORD TO CHECK PRODUCT ALLOCATIONS                          
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         LA    R6,BDELEM                                                        
         SR    R0,R0                                                            
*                                                                               
EDS50    ICM   R0,1,1(R6)                                                       
         JZ    *+2                                                              
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    EDS40                                                            
*                                                                               
         CLI   SVSNDP2,0           TEST P/B SEND                                
         BNE   EDS52                                                            
         CLI   1(R6),14            TEST SINGLE PRD SPOT                         
         BNE   EDS50                                                            
         CLC   SVSNDP1,10(R6)      MATCH PRD                                    
         BE    EDSXIT                                                           
         B     EDS50                                                            
*                                                                               
EDS52    CLI   1(R6),18            TEST P/B SPOT                                
         BNE   EDS50                                                            
         CLC   SVSNDP1,10(R6)      MATCH PRD 1                                  
         BNE   EDS54                                                            
         CLC   SVSNDP2,14(R6)      MATCH PRD 2                                  
         BE    EDSXIT              GOT A MATCH !                                
         B     EDS50               ELSE CONTINUE                                
*                                                                               
EDS54    CLC   SVSNDP1,14(R6)      MATCH PRD 2                                  
         BNE   EDS50                                                            
         CLC   SVSNDP2,10(R6)      MATCH PRD 1                                  
         BE    EDSXIT                                                           
         B     EDS50                                                            
         EJECT                                                                  
*=============================================================*                 
* DARE SEND. FORMAT IS DARE,BYR,(PR1-(PR2))                   *                 
*=============================================================*                 
         SPACE 1                                                                
EDS60    DS    0H                                                               
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(NODARBYR)                                              
         CLI   SVSENDER,C' '                                                    
         BNH   EDSNEQ                                                           
         GOTO1 VGLOBBER,DMCB,=C'PUTD',SVSENDER,3,GLVSPBYR                       
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
*                                                                               
         MVC   NERRCD,=Y(NODARPRD)                                              
         CLI   SVSNDP1,X'FF'       TEST PRD = POL                               
         BNE   EDS62                                                            
         CLI   SVPRD,X'FF'         TEST HDLN PRD = POL                          
         BNE   EDSNEQ              NO - ERROR                                   
         CLI   SVCPROF+0,C'0'      TEST BRAND POL CLIENT                        
         BE    EDS64               NO                                           
         B     EDSNEQ              YES - ERROR                                  
*                                                                               
EDS62    CLI   SVPOLPRD,0          TEST BRAND POL                               
         BE    EDS64               NO                                           
         MVC   NERRCD,=Y(INVDARPR)                                              
         CLC   SVPOLPRD,SVSNDP1    IF SO, HDLN PRD MUST BE FIRST                
         BNE   EDSNEQ                                                           
*                                                                               
EDS64    XC    ELEM,ELEM           BUILD DARE PRODUCT GLOBAL                    
         MVC   ELEM(3),SVSNDAP1                                                 
         MVC   ELEM+3(3),SVSNDAP2                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',ELEM,6,GLVDRPRD                           
*                                                                               
         L     RE,ASVDARE                                                       
         CLI   SVDRFLAG-SVDARED(RE),C'Y'       FLIGHT REQUIRED?                 
         BNE   EDS80                                                            
         GOTO1 VGLOBBER,DMCB,=C'PUTD',BYTE2,1,GLVSPEFL    FLIGHT                
         B     EDS80                                                            
         EJECT                                                                  
*================================================================               
* FOR DARE AUTO RECALL, USE GLVBUY1 FOR                                         
*                       A-M/CLT/PRD/MKT/STA/EST (10)                            
*                       PRD1/PRD2/FLT  (3) SAVED IN TSTDARE                     
*================================================================               
         SPACE 1                                                                
EDS70    XC    ELEM,ELEM                                                        
         MVC   ELEM(10),SVKEY                                                   
         MVC   ELEM+10(3),SVDARRCL                                              
*                                                                               
         MVI   SVRCLOPT,RCLAUTB    SET AWAITING RETURN FROM WHOA                
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',ELEM,13,GLVBUY1                           
         XC    ELEM,ELEM                                                        
         MVI   ELEM,C'R'           SET RECALL FLAG                              
         MVC   ELEM+1(1),SVDCORT   SET CASH/TRADE LOCKED FLAG                   
         GOTO1 (RF),(R1),,,8,GLVSPOPT                                           
*                                                                               
* SEND WHAT WE CAN, IT STILL NEEDS DARE BUYER AND PRODUCT                       
*                                                                               
         GOTO1 (RF),(R1),=C'PUTF',BUYMDH,,GLVSPMD                               
         GOTO1 (RF),(R1),=C'PUTF',BUYCLH,,GLVSPCLT                              
         GOTO1 (RF),(R1),=C'PUTF',BUYESH,,GLVSPEST                              
         GOTO1 (RF),(R1),=C'PUTF',BUYSTH,,GLVSPSTA                              
*                                                                               
         L     RE,ASVDARE                                                       
         CLI   SVDRFLAG-SVDARED(RE),C'Y'  FLIGHT REQUIRED?                      
         BNE   EDS80                       NO, DON'T PASS FLIGHT                
*                                          YES, THIS WORKS FOR FLT0 TOO         
         GOTO1 VGLOBBER,DMCB,=C'PUTD',SVDARRCL+2,1,GLVSPEFL                     
*                                                                               
EDS80    XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'SPO'                                                 
         MVC   GLVXFRPR,=C'BUY'                                                 
         MVC   GLVXTOSY,=C'SPO'                                                 
         MVC   GLVXTOPR,=C'OM '                                                 
         OI    GLVXFLG1,GLV1SEPS                                                
         DROP  R1                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',ELEM,GLVXLENQ,GLVXCTL                     
         B     EDSEQ                                                            
*                                                                               
EDSNEQ   LTR   RB,RB               SET CC NEQ                                   
         B     EDSXIT                                                           
*                                                                               
EDSEQ    CR    RB,RB               SET CC EQ                                    
*                                                                               
EDSXIT   XIT1                                                                   
         SPACE 2                                                                
EDSPRD   LA    RF,ALLPRDS          ALLOW ALL PRDS                               
         CLC   ALLPRDS(3),0(R1)                                                 
         BER   RE                                                               
*                                                                               
         L     RF,ASVCLIST                                                      
EDSPRD2  CLI   3(RF),X'FF'         NEVER ALLOW PRD=POL                          
         BE    EDSPRD4                                                          
         CLC   0(3,RF),0(R1)       MATCH LIST TO INPUT                          
         BER   RE                                                               
EDSPRD4  LA    RF,4(RF)                                                         
         CLI   0(RF),C'A'                                                       
         BNL   EDSPRD2                                                          
         B     EDSNEQ                                                           
ALLPRDS  DC    C'***',X'FF'        ALL PRODUCTS/POL                             
         SPACE                                                                  
*                                                                               
CKPER    NTR1                                                                   
         USING PERVALD,R4                                                       
*                                                                               
         L     RF,VCOMFACS                                                      
         L     RF,CPERVAL-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(BYTE,WORK),(R4)                                       
         CLI   4(R1),0                                                          
         BNE   CKNO                                                             
         CLC   PVALNWKS,=X'001C'   MAXIMUM N'WEEKS  = 28                        
         BH    CKNO                                                             
*                                                                               
CKYES    SR    RC,RC                                                            
CKNO     LTR   RC,RC                                                            
         B     EDSXIT                                                           
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*=========================================================*                     
* CALL SFM WITH IC OR IB FUNCTION. USER RETURNS WITH SWAP *                     
*=========================================================*                     
         SPACE 1                                                                
BLDXFR   NTR1  BASE=*,LABEL=*                                                   
         CLC   =C'PFM',8(R2)                                                    
         BE    BLDXFPF                                                          
         CLC   =C'NFL',8(R2)                                                    
         BE    BLDXFPF                                                          
         XC    BUTRCODE,BUTRCODE                                                
         ZIC   RE,5(R2)                                                         
         CHI   RE,3                DO NOT USE MORE THAN 3 CHARS                 
         BNH   *+8                                                              
         LA    RE,3                                                             
         LR    R0,RE                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   BUTRCODE(0),8(R2)                                                
* PAY AND MATCH ALLOW MONTH/YEAR INPUT                                          
         CLC   =C'MAT',BUTRCODE                                                 
         BE    BLDXFR4                                                          
         CLC   =C'PAY',BUTRCODE                                                 
         BE    BLDXFR4                                                          
*                                                                               
         CLC   =C'MIS',BUTRCODE                                                 
         BE    BLDXFR10                                                         
*                                                                               
         CLC   =C'FIS',BUTRCODE                                                 
         BE    BLDXFR10                                                         
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',BUTRCODE,(R0),GLVXREC                     
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
*                                                                               
         MVC   ELEM(3),=C'CHA'                                                  
         CLC   =C'C2',BUTRCODE                                                  
         BE    BLDXFR2                                                          
         CLC   =C'PW',BUTRCODE                                                  
         BNE   BLDXFR2X                                                         
BLDXFR2  MVC   ELEM(3),=C'MAI'                                                  
*                                                                               
BLDXFR2X CLC   =C'CT',BUTRCODE     CTA MAINT                                    
         BNE   *+10                                                             
         MVC   ELEM(3),=C'USA'                                                  
*                                                                               
         GOTO1 (RF),(R1),,ELEM,3,GLVXACT                                        
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
         CLC   =C'CT',BUTRCODE    TEST CTA/MAINT                                
         BNE   BLDXFR10                                                         
         MVC   ELEM(5),SVCTACON                                                 
         GOTO1 (RF),(R1),,ELEM,5,GLVSPCON                                       
         B     BLDXFR10                                                         
*                                                                               
BLDXFR4  CLI   5(R2),4             TEST DATE INPUT                              
         BNH   BLDXFR10            NO                                           
         ZIC   RE,5(R2)            GET INPUT LENGTH                             
         AHI   RE,-4               ADJUST FOR MAT, OR MAT=                      
         LR    R0,RE               SAVE LENGTH                                  
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),12(R2)                                                   
         GOTO1 VGLOBBER,DMCB,=C'PUTD',ELEM,(R0),GLVSPPER                        
         EJECT                                                                  
* BUILD TRANSFER CONTROL ELEMENT                                                
*                                                                               
BLDXFR10 XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'SPO'                                                 
         MVC   GLVXFRPR,=C'BUY'                                                 
         MVC   GLVXTOSY,=C'SPO'                                                 
         MVC   GLVXTOPR,=C'SFM'                                                 
         OI    GLVXFLG1,GLV1SEPS                                                
*                                                                               
         CLC   =C'MIS',BUTRCODE                                                 
         BNE   *+10                                                             
         MVC   GLVXTOPR,=C'MIS'                                                 
*                                                                               
         CLC   =C'FIS',BUTRCODE                                                 
         BNE   *+10                                                             
         MVC   GLVXTOPR,=C'FIS'                                                 
*                                                                               
         CLC   =C'MAT',BUTRCODE                                                 
         BNE   *+10                                                             
         MVC   GLVXTOPR,=C'MAT'                                                 
*                                                                               
         CLC   =C'PAY',BUTRCODE                                                 
         BNE   *+10                                                             
         MVC   GLVXTOPR,=C'PAY'                                                 
*                                                                               
         DROP  R1                                                               
*                                                                               
BLDXFR20 DS    0H                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',ELEM,GLVXLENQ,GLVXCTL                     
         XIT1                                                                   
         SPACE 2                                                                
BLDXFPF  DS    0H                                                               
         XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
         USING GLPFMFIL,R1                                                      
         MVC   GLPFMFIL(6),=C'SPTFIL'                                           
         MVC   GLPFMDA,SVKEY+14                                                 
         GOTO1 VGLOBBER,DMCB,=C'PUTD',ELEM,54,GLPFMCDQ                          
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'SPO'                                                 
         MVC   GLVXFRPR,=C'BUY'                                                 
         MVC   GLVXTOSY,=C'SPO'                                                 
         MVC   GLVXTOPR,8(R2)      MOVE PFM/NFL                                 
         OI    GLVXFLG1,GLV1SEPS                                                
         B     BLDXFR20                                                         
         LTORG                                                                  
         EJECT                                                                  
*=================================================================*             
* TEST FOR TRANSFER CONTROL ELEMENT FROM DARE OR $MAD                           
* SET BYTE2 = X'80' IF NEED TO CALL SETSTAR/XMTALL                              
*           = X'40' IF HEADLINE CHANGED BY SPT/DARE                             
*           = X'20' IF BY SPT/DARE AND FOR MGEACC                               
*           = X'10' IF TRADE ORDER MG                                           
*           = X'08' IF CASH ORDER MG THAT HAS A TRADE COUNTERPART               
*           = X'01' RETURN CALL FROM SPOT/MIS - DISPLAY AND EXIT                
* SET UPSW TO UPON IF $MAD CALL DETECTED                                        
*=================================================================*             
         SPACE 1                                                                
         DS    0D                                                               
TESTGLB  NTR1  BASE=*,LABEL=*                                                   
         MVI   BYTE2,0             RESET EXIT FLAG                              
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'GETD',ELEM,GLVXLENQ,GLVXCTL                     
         TM    DMCB+8,X'10'        GLOBAL NOT FOUND? (2ND HIT OF ENTER)         
         BNZ   GLBX                YES, DON'T CLOBBER SVXFRSID                  
*                                                                               
         LA    R1,ELEM-2           FESA                                         
         USING GLVXCTLD,R1                                                      
         CLC   GLVXFRSY(6),=C'SPOMIS'  RETURN FROM MIS TO BUYMOVE?              
         BNE   GLB00                                                            
         TM    GLVXFLG1,GLV1RETN   RETURN CALL?                                 
         BZ    GLB00               NO                                           
* YES                                                                           
         GOTO1 VGLOBBER,DMCB,=C'DELE'                                           
* USED TO FORCE XMTALL HERE BUT ALAN'S FIX SHOULD FIX IT                        
         OI    BYTE2,X'01'                                                      
         B     GLBX                                                             
*                                                                               
GLB00    TM    GLVXFLG1,GLV1RETN   RETURN CALL?                                 
         BO    *+10                YES                                          
         MVC   SVXFRSID,GLVXSESR   SAVE CALLER/CALLEE SESSION IDS               
         DROP  R1                                                               
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'GETD',ELEM,12,GLVNOTE                           
         CLI   DMCB+8,0            IGNORE ERRORS                                
         BNE   GLB02                                                            
* TEST IT'S FROM WALTER                                                         
         CLC   ELEM(6),=C'SPODAR'                                               
         BE    *+14                                                             
         CLC   ELEM(6),=C'SPOOM '                                               
         BNE   GLB02                                                            
         OI    SVXFRCTL,SVXFR_DARE                                              
*                                                                               
         TM    ELEM+6,X'48'        TEST THESE FLAGS?                            
         BZ    GLB01                                                            
*                                                                               
         TM    ELEM+6,X'40'        TEST MGEACC                                  
         BZ    *+8                                                              
         OI    BYTE2,X'20'                                                      
*                                                                               
         TM    ELEM+6,X'08'        SELF APPLY?                                  
         BZ    *+8                                                              
         OI    BYTE2,X'04'                                                      
*                                                                               
         NI    ELEM+6,X'FF'-X'48'  ONCE AND ONLY ONCE !                         
         GOTO1 VGLOBBER,DMCB,=C'PUTD'  AND WRITE IT BACK                        
*                                                                               
GLB01    TM    ELEM+6,X'20'        TEST TRADE ORDER                             
         BZ    *+8                                                              
         OI    BYTE2,X'10'                                                      
         TM    ELEM+6,X'10'        TEST CASH ORDER THAT HAS TRADE METHD         
         BZ    *+8                                                              
         OI    BYTE2,X'08'                                                      
*                                                                               
         TM    ELEM+6,X'80'        TEST HEADLINE CHANGE                         
         BZ    GLB02                                                            
         GOTO1 (RF),(R1),=C'DELE'  DELETE THE NOTE                              
         OI    BYTE2,X'40'         FORCE HEADLINE EDIT                          
*                                                                               
GLB02    GOTO1 VGLOBBER,DMCB,=C'GETD',ELEM,GLVXLENQ,GLVXCTL                     
         CLI   DMCB+8,0            IGNORE ERRORS                                
         BNE   GLBX                                                             
* DELETE XFR CONTROL ELEMENT IMMEDIATELY                                        
         GOTO1 (RF),(R1),=C'DELE'                                               
*                                                                               
         LA    R1,ELEM             TRANSFERRED CONTROL FROM SPOT/DARE?          
         USING GLVXFRSY,R1                                                      
*                                                                               
         CLC   GLVXFRSY(6),=C'SPOMAK'  TEST FROM MATCHMAKER                     
         BE    GLBMAK                                                           
                                                                                
         CLC   GLVXFRSY,=C'SPO'                                                 
         BNE   GLB80                                                            
         CLC   GLVXFRPR,=C'DAR'                                                 
         BE    *+14                                                             
         CLC   GLVXFRPR,=C'OM '                                                 
         BNE   GLB80               NO                                           
*                                                                               
         TM    GLVXFLG1,GLV1RETN   RETURN CALL?                                 
         BO    GLB20               YES, RETURNING FROM SPOT/DARE                
         GOTO1 (RF),DMCB,=C'GETF',BUYMDH,,GLVSPMD                               
         GOTO1 (RF),(R1),=C'GETF',BUYBUH,,GLVSPBYR                              
         GOTO1 (RF),(R1),=C'GETF',BUYCLH,,GLVSPCLT                              
         GOTO1 (RF),(R1),=C'GETF',BUYESH,,GLVSPEST                              
         GOTO1 (RF),(R1),=C'GETF',BUYSTH,,GLVSPSTA                              
*                                                                               
         XC    DUB,DUB                                                          
         GOTO1 (RF),(R1),=C'GETD',DUB,8,GLVSPPR2                                
         TM    DMCB+8,X'10'        GLOBAL NOT FOUND                             
         BNZ   GLB03                                                            
         MVC   BUYPR(3),=C'POL'                                                 
         MVI   BUYPRH+5,3                                                       
         MVC   BUYOP(6),=C'NOGOAL'                                              
         MVI   BUYOPH+5,6                                                       
         GOTO1 (RF),DMCB,=C'DELE',,,GLVSPPR2                                    
         B     GLB05                                                            
*                                                                               
GLB03    GOTO1 (RF),DMCB,=C'GETF',BUYPRH,,GLVSPPRD                              
         CLC   BUYPR(3),=C'POL'                                                 
         BNE   GLB05                                                            
         MVC   BUYOP(6),=C'NOGOAL'                                              
         MVI   BUYOPH+5,6                                                       
*                                                                               
GLB05    XC    BUYINP1,BUYINP1     SET UP INPUT LINE (MGE=AA,PR1-PR2)           
         MVC   BUYINP1(3),=C'MGE'                                               
         LA    R2,BUYINP1+3                                                     
*                                                                               
         TM    BYTE2,X'20'         MGEACC?                                      
         BZ    *+14                                                             
         MVC   0(3,R2),=C'ZZT'     USER'S RESPONSE TO ON-HOLD                   
         LA    R2,3(R2)                                                         
*                                                                               
         TM    BYTE2,X'04'         MGESAP?                                      
         BZ    *+14                                                             
         MVC   0(3,R2),=C'SAP'     MAKEGOOD SELF APPLY                          
         LA    R2,3(R2)                                                         
*                                                                               
         GOTO1 (RF),DMCB,=C'GETD',DDFLIGHT,1,GLVSPEFL                           
         CLI   8(R1),X'10'         FLIGHT NOT FOUND?                            
         BE    GLB06                                                            
         EDIT  (B1,DDFLIGHT),(2,0(R2)),FILL=0                                   
         LA    R2,2(R2)                                                         
*                                                                               
GLB06    MVI   0(R2),C'='                                                       
         LA    R2,1(R2)                                                         
         GOTO1 (RF),DMCB,=C'GETD',0(R2),3,GLVSPMKG                              
*                                                                               
         CLI   8(R1),X'10'         MAKEGOOD GROUP NOT FOUND?                    
         BNE   GLB07               IT WAS FOUND, SO MGE=                        
         XC    BUYINP1,BUYINP1     NOT FOUND, ASSUME FROM ORDER/LIST            
         MVC   BUYINP1(2),=C'1N'                                                
         MVI   BUYINP1H+5,2                                                     
         OI    BUYINP1H+6,X'80'    AND TRANSMIT                                 
         B     GLBX                                                             
*                                                                               
GLB07    LA    R2,2(R2)                                                         
         CLI   0(R2),C' '                                                       
         BNH   *+8                                                              
         LA    R2,1(R2)            R2=A(1ST CHAR AFTER GROUP CODE)              
*                                                                               
         TM    BYTE2,X'10'         TRADE?                                       
         BZ    GLB08                                                            
         MVC   0(2,R2),=C'/T'                                                   
         LA    R2,2(R2)                                                         
*                                                                               
GLB08    TM    BYTE2,X'08'         CASH THAT HAS A TRADE COUNTERPART?           
         BZ    GLB09                                                            
         MVC   0(2,R2),=C'/C'                                                   
         LA    R2,2(R2)                                                         
*                                                                               
GLB09    CLC   BUYPR(3),=C'POL'    POL ESTIMATE?                                
         BNE   GLB16               NO, WE'RE USING PRODUCT FROM KEY             
*                                                                               
GLB10    MVI   0(R2),C','                                                       
         LA    R2,1(R2)            MGE=AA,PR1-PR2                               
         GOTO1 (RF),DMCB,=C'GETD',(R2),7,GLVDRPRD                               
         TM    DMCB+8,X'10'                                                     
         BNZ   GLB11                                                            
         LA    R2,7(R2)                                                         
         B     GLB12                                                            
*                                                                               
GLB11    GOTO1 (RF),DMCB,=C'GETD',(R2),3,GLVSPPRD                               
         LA    R2,3(R2)                                                         
         CLI   0(R2),X'40'                                                      
         BH    *+8                                                              
         BCT   R2,*-8                                                           
*                                                                               
         CLI   DUB,0                                                            
         BE    GLB12                                                            
         MVI   1(R2),C'-'                                                       
         MVC   2(3,R2),DUB                                                      
         LA    R2,5(R2)                                                         
GLB12    CLI   0(R2),C' '                                                       
         BH    GLB15                                                            
         BCT   R2,GLB12                                                         
GLB15    LA    R2,1(R2)            CAN'T USE BUYINP1-1 AT GLB16                 
*                                                                               
GLB16    LA    R0,BUYINP1          CALCULATE LENGTH OF INPUT LINE               
         SR    R2,R0                                                            
         STC   R2,BUYINP1H+5                                                    
         B     GLBX                GOT CALLED BY SPOT/DARE                      
*                                                                               
GLB20    CLI   SVRCLOPT,RCLAUTB    TEST RETURN FROM AUTO RECALL                 
         BE    GLB50                                                            
*                                                                               
         GOTO1 (RF),DMCB,=C'GETD',ELEM,60,GLVSPORD                              
         CLI   8(R1),0                                                          
         BNE   GLB80                                                            
         GOTO1 (RF),DMCB,=C'DELE',,,GLVSPORD                                    
         MVC   BUYMSG(60),ELEM      SHOW MESSAGE TO USER                        
*                                                                               
* INSERT '*' IN FRONT OF INPUT                                                  
*                                                                               
         LA    R2,BUYINP1H                                                      
GLB30    CLI   8(R2),C'*'                                                       
         BNE   GLB40                                                            
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     GLB30                                                            
*                                                                               
GLB40    OI    BYTE2,X'80'         SET TO CALL SETSTAR                          
*                                                                               
* LASTLY, ORDER IS NOW PENDING SO ADD PRD(S) AND FLT TO TABLE                   
*                                                                               
         MVI   ELEM,0                                                           
         GOTO1 VGLOBBER,DMCB,=C'GETD',ELEM,,GLVSPEFL       FLIGHT #             
*                                                                               
         L     RE,ASVDARE                                                       
         USING SVDARED,RE                                                       
         LA    RE,SVDRTBL                                                       
         DROP  RE                                                               
         L     RF,=A(((SVDAREX-SVDARE-72)/3)-2) SET FOR NUM ENTS - 2            
*                                                                               
         OC    0(3,RE),0(RE)       GET TO END YET ?                             
         BZ    *+14                                                             
         LA    RE,3(RE)                                                         
         BCT   RF,*-14                                                          
         DC    H'0'                                                             
         MVC   0(1,RE),SVSNDP1                                                  
         MVC   1(1,RE),SVSNDP2                                                  
         MVC   2(1,RE),ELEM        FLIGHT NUMBER                                
         CLC   SVSNDP1,SVSNDP2                                                  
         BL    GLBX                                                             
         MVC   0(1,RE),SVSNDP2                                                  
         MVC   1(1,RE),SVSNDP1                                                  
         B     GLBX                                                             
*                                                                               
* RETURN FROM AUTO RECALL                                                       
*                                                                               
GLB50    ICM   R1,15,SVDARENT      GET SAVED DSPL TO LOCK                       
         A     R1,ASVDARE                                                       
         LA    RE,X'7F'            SET TO UNLOCK CASH                           
         TM    SVDCORT,X'40'       TEST TRADE                                   
         BZ    *+8                                                              
         LA    RE,X'BF'                                                         
         EX    RE,*+8              RESET LOCK FLAG                              
         B     *+8                                                              
         NI    2(R1),0  *EXECUTED*                                              
         B     GLBX                                                             
*                                                                               
GLB80    CLC   GLVXFRPR,=C'MAD'    DID WE GET CALLED BY $MAD?                   
         BNE   GLBX                                                             
         MVI   UPSW,UPON           YES                                          
*                                                                               
GLBX     XIT1  REGS=(R2)                                                        
         EJECT                                                                  
*=================================================================*             
* PROCESS GLOBBER CALLS FROM MATCHMAKER                           *             
*=================================================================*             
         SPACE 1                                                                
GLBMAK   DS    0H                                                               
         OI    SVXFRCTL,SVXFR_MAK                                               
         XC    ELEM,ELEM                                                        
         GOTO1 VGLOBBER,DMCB,=C'GETD',ELEM,MAKDATAL,GLVSMSG  BUY DATA           
         GOTO1 (RF),(R1),=C'DELE',,,GLVSMSG                                     
*                                                                               
G        USING MAKDATAD,ELEM                                                    
*                                                                               
         LA    R2,BUYMDH                                                        
         LA    R5,G.MAKMED                                                      
         LA    R6,L'MAKMED                                                      
         BRAS  RE,SETIN                                                         
*                                                                               
         LA    R2,BUYBUH                                                        
         LA    R5,G.MAKBUYER                                                    
         LA    R6,L'MAKBUYER                                                    
         BRAS  RE,SETIN                                                         
*                                                                               
         LA    R2,BUYCLH                                                        
         LA    R5,G.MAKCLT                                                      
         LA    R6,L'MAKCLT                                                      
         BRAS  RE,SETIN                                                         
*                                                                               
         LA    R2,BUYPRH                                                        
         LA    R5,G.MAKPRD                                                      
         LA    R6,L'MAKPRD                                                      
         BRAS  RE,SETIN                                                         
*                                                                               
         LA    R2,BUYESH                                                        
         LA    R5,G.MAKEST                                                      
         LA    R6,L'MAKEST                                                      
         BRAS  RE,SETIN                                                         
*                                                                               
         LA    R2,BUYSTH                                                        
         LA    R5,G.MAKSTA                                                      
         LA    R6,L'MAKSTA                                                      
         BRAS  RE,SETIN                                                         
*                                                                               
         LHI   R6,SVRSNEL-BUYSAVE                                               
         AR    R6,RA                                                            
         XC    0(69,R6),0(R6)                                                   
*                                                                               
         MVI   0(R6),X'90'         SET ELEM CODE                                
         MVI   1(R6),RCELLENQ      SET ELEM LEN W/O TEXT                        
         MVC   2(6,R6),G.MAKRSNCD  SAVE THE REASON CODE                         
         OC    2(6,R6),SPACES                                                   
         CLC   G.MAKRSNTX,SPACES   IS THERE A COMMENT?                          
         BNH   GLBMAK6              NO                                          
*                                                                               
         LHI   R6,SVRSNTXT-BUYSAVE                                              
         AR    R6,RA                                                            
         MVC   0(L'MAKRSNTX,R6),G.MAKRSNTX                                      
*  COUNT INPUT LENGTH                                                           
         LHI   RE,40                                                            
         CLI   0(R6),0                                                          
         BE    *+12                                                             
         AHI   R6,1                                                             
         BCT   RE,*-12                                                          
*                                                                               
         LHI   RF,40                                                            
         SR    RF,RE                                                            
         AHI   RF,9                8+1+L'TEXT                                   
         LHI   R6,SVRSNEL-BUYSAVE                                               
         AR    R6,RA                                                            
         STC   RF,1(R6)                                                         
*                                                                               
GLBMAK6  LA    R6,ELEM+100                                                      
         CLI   G.MAKOPTNG,C'Y'     SET NOGOAL OPTION?                           
         BNE   *+14                 NO                                          
         MVC   0(7,R6),=C'NOGOAL,'                                              
         AHI   R6,7                                                             
*                                                                               
         CLI   G.MAKOPTND,C'Y'     SET NODEMO OPTION?                           
         BNE   *+14                NO                                           
         MVC   0(7,R6),=C'NODEMO,'                                              
         AHI   R6,7                                                             
*                                                                               
         OC    G.MAKOPTID,G.MAKOPTID   CANADIAN ID= OR PURPOSE COED?            
         BZ    GLBMAK8                  NO                                      
         MVC   0(3,R6),=C'ID='                                                  
         CLI   G.MAKB0PRF,C'Y'     PURPOSE CODES REQUIRED?                      
         BNE   *+14                 NO                                          
         MVC   0(4,R6),=C'PUR='                                                 
         AHI   R6,1                PUR= IS 1 LONGER THAN ID=                    
         AHI   R6,3                                                             
*                                                                               
         MVC   0(12,R6),G.MAKOPTID                                              
         AHI   R6,11                                                            
         CLI   0(R6),C' '                                                       
         BH    *+10                                                             
         BCTR  R6,0                                                             
         B     *-10                                                             
*                                                                               
         MVI   1(R6),C','                                                       
         AHI   R6,2                                                             
*                                                                               
GLBMAK8  OC    G.MAKREP,G.MAKREP   ANY REP CODE?                                
         BZ    GLBMAK10                                                         
         MVC   0(4,R6),=C'REP='                                                 
         MVC   4(3,R6),G.MAKREP                                                 
         MVI   7(R6),C','                                                       
         AHI   R6,8                                                             
*                                                                               
GLBMAK10 BCTR  R6,0                GET RID OF TRAILING COMMA                    
         MVI   0(R6),C' '                                                       
*                                                                               
         LA    R5,ELEM+100                                                      
         SR    R6,R5               SET LENGTH                                   
         BNP   GLBMAK12            NO OPTIONS                                   
         LA    R2,BUYOPH                                                        
         BRAS  RE,SETIN                                                         
         DROP  G                                                                
*                                                                               
GLBMAK12 XC    ELEM,ELEM                                                        
         MVI   SVSPOMAK,0          RESET                                        
         GOTO1 VGLOBBER,DMCB,=C'GETD',ELEM,78,GLVBUY1                           
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
         GOTO1 (RF),(R1),=C'DELE',,,GLVBUY1                                     
*                                                                               
         LA    R2,BUYINP1H                                                      
         LA    R5,ELEM                                                          
         LA    R6,78                                                            
         CLI   0(R5),C'$'          TEST DO-NOT-ADD BUY                          
         BNE   GLBMAK20                                                         
*                                                                               
         OI    SVSPOMAK,SVSPOMAK_NOBUY                                          
         AHI   R5,1                                                             
         BCTR  R6,0                                                             
         CLI   BUYMD,C'N'                                                       
         BNE   GLBMAK20                                                         
         BRAS  RE,SETIN             YES - DON'T SEND ALL COMMAND LINES          
         B     GLBMAKX               ON VALIDATION PASS                         
*                                                                               
GLBMAK20 BRAS  RE,SETIN                                                         
*                                                                               
         XC    ELEM,ELEM                                                        
         GOTO1 (RF),(R1),=C'GETD',ELEM,78,GLVBUY2                               
         CLI   8(R1),0                                                          
         BNE   GLBMAKX                                                          
         GOTO1 (RF),(R1),=C'DELE',,,GLVBUY2                                     
         LA    R2,BUYINP2H                                                      
         LA    R5,ELEM                                                          
         LA    R6,78                                                            
         BRAS  RE,SETIN                                                         
*                                                                               
         XC    ELEM,ELEM                                                        
         GOTO1 (RF),(R1),=C'GETD',ELEM,78,GLVBUY3                               
         CLI   8(R1),0                                                          
         BNE   GLBMAKX                                                          
         GOTO1 (RF),(R1),=C'DELE',,,GLVBUY3                                     
         LA    R2,BUYINP3H                                                      
         LA    R5,ELEM                                                          
         LA    R6,78                                                            
         BRAS  RE,SETIN                                                         
*                                                                               
         XC    ELEM,ELEM                                                        
         GOTO1 (RF),(R1),=C'GETD',ELEM,78,GLVBUY4                               
         CLI   8(R1),0                                                          
         BNE   GLBMAKX                                                          
         GOTO1 (RF),(R1),=C'DELE',,,GLVBUY4                                     
         LA    R2,BUYINP4H                                                      
         LA    R5,ELEM                                                          
         LA    R6,78                                                            
         BRAS  RE,SETIN                                                         
*                                                                               
GLBMAKX  XIT1                                                                   
*                                                                               
         SPACE 1                                                                
* KEEP THIS IN SYNC WITH SAME DSECT IN SPMAK20                                  
MAKDATAD DSECT                                                                  
MAKMED   DS    CL1                                                              
MAKBUYER DS    CL12                                                             
MAKCLT   DS    CL3                                                              
MAKPRD   DS    CL3                                                              
MAKEST   DS    CL3                                                              
MAKSTA   DS    CL8                                                              
MAKPRD2  DS    CL3                                                              
MAKEST2  DS    CL3                                                              
MAKOPTNG DS    CL1                 Y=NOGOAL                                     
MAKB0PRF DS    CL1                 PROF_B0+9 - PURPOSE CODES REQUIRED           
MAKOPTID DS    CL12                CANADIAN ID                                  
MAKREP   DS    CL3                 REP                                          
MAKRSNCD DS    CL6                 REASON CODE                                  
MAKRSNTX DS    CL40                RC TEXT                                      
MAKOPTND DS    CL1                 Y=NODEMO                                     
MAKDATAL EQU   *-MAKDATAD                                                       
*                                                                               
T21100   CSECT                                                                  
         EJECT                                                                  
*===============================================================                
* ROUTINE MOVES DATA TO A TWA FIELD AND SETS APPROPRIATE HEADER                 
* ON ENTRY, R2 = A(FIELD HEADER)                                                
*           R5 = A(DATA FIELD)                                                  
*           R6 = L'DATA FIELD                                                   
*===============================================================                
         SPACE 1                                                                
SETIN    NTR1                                                                   
         CLI   0(R5),0             TEST NO DATA                                 
         BE    SETINX                                                           
*                                                                               
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AHI   R0,-8                                                            
         TM    1(R2),X'02'         TEST FOR EXTENDED HEADER                     
         BZ    *+8                                                              
         AHI   R0,-8                                                            
*                                                                               
         LA    RF,0(R5,R6)         POINT TO END OF DATA FIELD                   
         BCTR  RF,0                BACK UP TO LAST BYTE                         
*                                                                               
SETIN2   CLI   0(RF),C' '          SCAN BACKWARDS FOR NON-SPACE                 
         BH    SETIN4                                                           
         BCTR  RF,0                                                             
         BCT   R6,SETIN2                                                        
         B     SETINX              NO INPUT                                     
*                                                                               
SETIN4   CLM   R6,1,5(R2)          TEST NEW LEN = PREVIOUS INPUT LEN            
         BNE   SETIN10                                                          
         EX    R6,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),0(R5)       TEST DATA IS THE SAME                        
         BE    SETINX                                                           
*                                                                               
SETIN10  LR    RE,R0               DATA HAS CHANGED SO                          
         BCTR  RE,0                CLEAR THE FIELD                              
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
*                                                                               
         STC   R6,5(R2)            INPUT LENGTH                                 
         NI    4(R2),X'DF'         UNSET PREVIOUSLY VALIDATED                   
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),0(R5)       MOVE THE DATA                                
         LA    R6,1(R6)            RESTORE R6                                   
*                                                                               
SETIN12  CLI   0(RF),C'0'          TEST NUMERIC                                 
         BL    SETINX                                                           
         CLI   0(RF),C'9'                                                       
         BH    SETINX                                                           
         BCTR  RF,0                                                             
         BCT   R6,SETIN12                                                       
         OI    4(R2),X'08'         SET VALID NUMERIC                            
*                                                                               
SETINX   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*==========================================================                     
* GLOBBER CALLS FROM MOVE OVERWRITE ORIGINAL BUY GLOBALS                        
* RESTORE THEM NOW                                                              
*==========================================================                     
         SPACE 1                                                                
RSTRGLB  NTR1  BASE=*,LABEL=*                                                   
         GOTO1 VGLOBBER,DMCB,=C'PUTD',QCLT,3,GLVSPCLT                           
*                                                                               
         GOTO1 (RF),(R1),,QPRD,3,GLVSPPRD                                       
*                                                                               
         SR    R0,R0                                                            
         IC    R0,SVEST                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(3),DUB                                                      
         GOTO1 (RF),(R1),,WORK,3,GLVSPEST                                       
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*===========================================================*                   
* ON ENTRY R2 HAS FLDHDR ADDRESS                            *                   
* EDIT FOR DARE DISPLAY                                     *                   
* DD=(PRD1-(PRD2))                                          *                   
*===========================================================*                   
         DS    0D                                                               
EDITDD   NTR1  BASE=*,LABEL=*                                                   
         MVI   ERRCD,0                                                          
         MVC   DDPRD1,SVPRD        SET HEADLINE PRD                             
         CLI   DDPRD1,X'FF'                                                     
         BNE   *+10                                                             
         MVC   DDPRD1,SVPOLPRD                                                  
         MVI   DDPRD2,0                                                         
         MVI   DDSTLIN,0                                                        
         MVI   DDFLIGHT,0                                                       
*                                                                               
         LA    R4,8(R2)                                                         
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         XC    FSTOPS,FSTOPS                                                    
         MVC   FSTOPS(3),=C',=-'                                                
*                                                                               
         GOTO1 FLDVAL              GET 'DD'                                     
         CLC   =C'DD/C',0(R4)                                                   
         BE    *+14                                                             
         CLC   =C'DD/T',0(R4)                                                   
         BNE   EDITDD00                                                         
         SHI   R5,2                TO FAKE OUT    EDITDD01                      
         AHI   R4,2                TO FAKE OUT    EDITDD02                      
         B     EDITDD01                                                         
*                                                                               
EDITDD00 CLC   =C'DD',0(R4)                                                     
         JNE   *+2                                                              
*                                                                               
EDITDD01 CHI   R5,2                ANY FLIGHT NUMBER                            
         BH    EDITDD02            YES                                          
         CLC   =C'DD=*',8(R2)      FLIGHT NOT REQ'D FOR LIST                    
         BE    EDITDD08                                                         
         L     R7,ASVDARE          IS FLIGHT REQD                               
         USING SVDARED,R7                                                       
         CLI   SVDRFLAG,C'Y'                                                    
         BNE   EDITDD08            NOT REQD                                     
         MVI   ERRCD,NEWERRS       FLIGHT REQD                                  
         MVC   NERRCD,=Y(FLTREQDD)                                              
         B     EDITDDX                                                          
         DROP  R7                                                               
*                                                                               
EDITDD02 XC    WORK(10),WORK                                                    
         AHI   R5,-3               ISOLATE FLIGHT NUMBER (2+1)                  
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),2(R4)                                                    
         LA    R7,WORK                                                          
EDITDD04 CLI   0(R7),X'F0'         VALID NUMERIC                                
         BL    EDITDDER                                                         
         CLI   0(R7),X'F9'                                                      
         BH    EDITDDER                                                         
         LA    R7,1(R7)                                                         
         CLI   0(R7),0                                                          
         BNE   EDITDD04                                                         
         EX    R5,*+8              R5 STILL SET FOR EX                          
         B     *+10                                                             
         PACK  DUB,WORK(0)                                                      
         CVB   R5,DUB                                                           
         CHI   R5,16               MUST BE 0-16                                 
         BH    EDITDDER                                                         
         STC   R5,DDFLIGHT         SAVE FLIGHT NUMBER                           
         LTR   R5,R5                                                            
         BNZ   *+8                                                              
         MVI   DDFLIGHT,C'0'       SET FLIGHT 0 AS C'0'                         
*                                                                               
         CLI   DDFLIGHT,0                                                       
         BE    EDITDD08                                                         
*                                                                               
         BCTR  R5,0                DOES FLIGHT EXIST                            
         MHI   R5,4                                                             
         L     R7,ASVDARE                                                       
         USING SVDARED,R7                                                       
*                                                                               
         CLI   DDFLIGHT,C'0'       FILTER FOR FLIGHT 0?                         
         BNE   EDITDD06                                                         
         OC    SVDRFLT0,SVDRFLT0   IS ESTIMATE SETUP WITH FLT0?                 
         BZ    EDITDDER             NO, ERROR                                   
         B     EDITDD08                                                         
*                                                                               
EDITDD06 LA    R7,SVDRSTR1         START OF FLIGHTS                             
         AR    R7,R5               DISP TO FLIGHT DATES                         
         OC    0(4,R7),0(R7)                                                    
         BNZ   EDITDD08                                                         
         DROP  R7                                                               
*                                                                               
EDITDDER MVI   ERRCD,NEWERRS       INVALID FLIGHT NUMBER                        
         MVC   NERRCD,=Y(INVFLIGT)                                              
         B     EDITDDX                                                          
*                                                                               
EDITDD08 CLI   FSTOP,0             END OF EDIT                                  
         BNE   EDITDD10                                                         
         CLC   =C'POL',BUYPR                                                    
         BNE   EDITDDX                                                          
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(DARPRREQ)                                              
         B     EDITDDX                                                          
*                                                                               
EDITDD10 CLI   FSTOP,C','          LINE NUMBER IS NEXT                          
         BE    EDITDD20                                                         
         XC    FSTOPS,FSTOPS                                                    
         MVC   FSTOPS(2),=C',-'                                                 
         GOTO1 FLDVAL              GET PRODUCT                                  
         CLI   0(R4),C'*'          SPECIAL DISPLAY                              
         BNE   EDITDD15                                                         
         MVI   RCLOPT,C'L'         DARE LIST                                    
         MVI   DDPRD1,X'FF'        SET FOR ALL PRD PAIRS                        
         B     EDITDDX                                                          
*                                                                               
EDITDD15 BAS   RE,CHKPRD           GET PRD FROM CLIST                           
         BNE   ERRPRD                                                           
         MVC   DDPRD1,BYTE                                                      
         CLI   FSTOP,0             END OF EDIT                                  
         BE    EDITDDX                                                          
         CLI   FSTOP,C','          NEXT FIELD IS A LINE NUMBER                  
         BE    EDITDD20                                                         
*                                                                               
         XC    FSTOPS,FSTOPS                                                    
         MVI   FSTOPS,C','         GET 2ND PRD                                  
         GOTO1 FLDVAL                                                           
         BAS   RE,CHKPRD           GET PRD FROM CLIST                           
         BNE   ERRPRD                                                           
         MVC   DDPRD2,BYTE                                                      
         CLI   FSTOP,0             DONE                                         
         BE    EDITDDX                                                          
*                                                                               
EDITDD20 XC    FSTOPS,FSTOPS                                                    
         GOTO1 FLDVAL                                                           
         CLI   FLEN+1,3                                                         
         BH    ERRINV                                                           
         CLI   FSTOP,0             END OF EDIT                                  
         BNE   ERRINV                                                           
         LTR   R5,R5                                                            
         BZ    ERRINV                                                           
         TM    FVAL,X'08'          VALID NUMERIC                                
         BZ    ERRINV                                                           
         CHI   R5,4                                                             
         BH    ERRINV                                                           
         CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         BZ    ERRINV                                                           
         CHI   R0,255                                                           
         BH    ERRINV                                                           
         STC   R0,DDSTLIN                                                       
         B     EDITDDX                                                          
*                                                                               
ERRPRD   MVI   ERRCD,PRDERR                                                     
         B     EDITDDX                                                          
*                                                                               
ERRINV   MVI   ERRCD,INVERR                                                     
*                                                                               
EDITDDX  XIT1                                                                   
         EJECT                                                                  
*                                                                               
* CHECK PRODUCT CODE FROM CLIST                                                 
*                                                                               
CHKPRD   DS    0H                                                               
         L     R5,ASVCLIST                                                      
*                                                                               
CP10     CLI   0(R5),0             END OF LIST                                  
         BE    CPNO                                                             
         MVC   BYTE,3(R5)          SET BCODE OF PRD                             
         LH    R1,FLEN                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R5),0(R4)                                                    
         BE    CPYES                                                            
         LA    R5,4(R5)            BUMP TO NEXT PRODUCT                         
         B     CP10                                                             
*                                                                               
CPYES    SR    R5,R5               RETURN CC NEQ                                
CPNO     LTR   R5,R5               RETURN CC NEQ                                
         B     CPX                                                              
*                                                                               
CPX      BR    RE                                                               
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*===================================================================*           
* SUBROUTINE CALLS GETFACT TO SEE IF USER SIGNED ON VIA FLASHPOINT              
* AND SETS SECURITY AGENCY                                                      
* IF SO - SET SUB SCREEN NUMBER IN FATIOB                                       
*===================================================================*           
         SPACE 1                                                                
GFACT    NTR1  BASE=*,LABEL=*                                                   
         XC    DMCB,DMCB                                                        
         L     RF,VCOMFACS                                                      
         L     RF,CGETFACT-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB                                                        
         L     R2,0(R1)                                                         
         USING FACTSD,R2                                                        
         MVC   SVSECAGY,FATAGYSC   SAVE SECURITY AGENCY                         
         NI    SVSTAT,X'FF'-SVSTFLSH                                            
         TM    FATSTAT6,TST6FLSH                                                
         BNO   TFX                                                              
         OI    SVSTAT,SVSTFLSH     SET FLASHPOINT                               
         DROP  R2                                                               
         L     R1,VTIOB                                                         
         USING TIOBD,R1                                                         
         OI    TIOBINDS,TIOBSCRN   SET TIOBAID TO FORCE SCREEN NUMBER           
         MVI   TIOBAID,0                                                        
         XC    TIOBCNT,TIOBCNT                                                  
         ZIC   R2,SVRCLOPT                                                      
         STC   R2,TIOBCNT                                                       
*                                                                               
TFX      XIT1                                                                   
         DROP  R1                                                               
         LTORG                                                                  
         EJECT                                                                  
         DS    0D                  THANKS LISA !!!                              
TSTDARE  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R9,BASER9                                                        
         L     R3,VBUYTWA                                                       
         L     RA,VBUYSAVE                                                      
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB+6(2),ELCDLO     SAVE ELCDLO/ELCDHI                           
*                                                                               
         L     R4,ASVDARE                                                       
         USING SVDARED,R4                                                       
         TM    SVDRFLG2,SVDRFLG2_IGN        CALL TSTDARE?                       
         BNZ   TDX                          NO                                  
         DROP  R4                                                               
*                                                                               
         TM    SVXFRCTL,SVXFR_SDT      TEST SPOT DESKTOP MODE                   
         BZ    *+12                                                             
         TM    SVSPOMAK,SVSPOMAK_NOBUY  AND TEST DO-NOT-ADD                     
         BNZ   TDX                     THIS IS SIMULATE - SKIP DARE             
*                                                                               
         TM    WRKRUPSW,WRKRUPSW_ISDRMG     DARE MG?                            
         BNZ   TDX                          YES, ACCEPT IT ALL                  
*                                                                               
         L     R6,AREC                                                          
         CLI   0(R6),X'10'         MAKE SURE THIS IS A BUY                      
         BL    TDX                                                              
*                                                                               
         LA    R6,BDELEM                                                        
         CLI   BUYREC+3,X'FF'      IS THIS POL                                  
         BE    TD20                                                             
* NON-POL - SEARCH FOR P/B ELEMENT                                              
         MVC   DUB(1),BUYREC+3    NO - SET PRD1 TO BRAND                        
         MVI   ELCDLO,X'04'                                                     
         MVI   ELCDHI,X'04'        GET PIGGYBACK ELEMENT                        
         BRAS  RE,NEXTEL                                                        
         BNE   *+10                                                             
         MVC   DUB+1(1),2(R6)                                                   
*                                                                               
         L     R4,ASVDARE                                                       
         USING SVDARED,R4                                                       
         OC    SVDRELEM,SVDRELEM   INTERESTED IN ELEM AT GIVEN ADDR?            
         BZ    TD10                NO                                           
         L     R6,SVDRELEM         A(BUY ELEM) TO BE CHECKED                    
         B     TD50                                                             
         DROP  R4                                                               
*                                                                               
TD10     MVI   ELCDLO,6                                                         
         MVI   ELCDHI,8                                                         
         LA    R6,BDELEM                                                        
*                                                                               
TD12     BRAS  RE,NEXTEL                                                        
         BNE   TDX                                                              
         B     TD50                                                             
*                                                                               
TD20     L     R4,ASVDARE                                                       
         USING SVDARED,R4                                                       
         OC    SVDRELEM,SVDRELEM   INTERESTED IN ELEM AT GIVEN ADDR?            
         BZ    TD25                NO                                           
         L     R6,SVDRELEM         A(BUY ELEM) TO BE CHECKED                    
         MVI   DUB,X'FF'           SET TO PRD POL                               
         CLI   1(R6),10                                                         
         BNH   TD50                                                             
         MVC   DUB(1),10(R6)                                                    
         CLI   1(R6),14                                                         
         BNH   TD50                                                             
         MVC   DUB+1(1),14(R6)                                                  
         B     TD50                                                             
         DROP  R4                                                               
*                                                                               
TD25     MVI   ELCDLO,X'0B'        MUST USE BUY ELEMENTS                        
         MVI   ELCDHI,X'0D'                                                     
*                                                                               
TD30     LA    R6,BDELEM                                                        
*                                                                               
TD40     XC    DUB(2),DUB                                                       
         BRAS  RE,NEXTEL                                                        
         BNE   TDX                                                              
         MVI   DUB,X'FF'           SET PRD POL                                  
         CLI   1(R6),10            IS THIS SPOT ALLOCATED                       
         BNH   TD50                NO - GET NEXT                                
         MVC   DUB(1),10(R6)                                                    
         CLI   1(R6),14            IS THIS A PIGGYBACK                          
         BNH   TD50                                                             
         MVC   DUB+1(1),14(R6)     YES SET PRD2                                 
*                                                                               
TD50     CLC   DUB(1),DUB+1        SET LOW PRD CODE FIRST                       
         BL    TD60                                                             
         ICM   R0,3,DUB                                                         
         STC   R0,DUB              SET THE SECOND PRD IN DUB                    
         SRL   R0,8                                                             
         STC   R0,DUB+1            SET THE FIRST PRD IN DUB+1                   
*                                                                               
TD60     DS    0H                  SEE IF PAIR IS IN TABLE                      
         MVI   DUB+2,0             SET FLIGHT TO 0                              
         BAS   RE,FLNUM            FIND FLIGHT SEQ NUM                          
* NOTE CAN ALWAYS DO THIS - IT JUST DOESN'T MATTER                              
         MVC   DUB+3(2),=X'00FF'   BUILD POL ENTRY                              
         MVC   DUB+5(1),DUB+2      MOVE FLIGHT                                  
*                                                                               
TD65     L     R4,ASVDARE                                                       
         LA    R4,72(R4)           SKIP FLIGHT TABLE                            
*                                                                               
TD70     CLC   0(2,R4),=X'0000'    ANY MORE ENTRIES ?                           
         BE    TD72                NO                                           
*                                                                               
         MVC   BYTE,2(R4)          EXTRACT THE FLIGHT                           
         NI    BYTE,X'0F'                                                       
*                                                                               
         CLC   DUB(2),0(R4)        MATCH PRD-PRD                                
         BNE   TD70A               YES                                          
         CLC   BYTE,DUB+2                                                       
         BE    TD80                MATCHED ON FLIGHT AS WELL                    
*                                                                               
TD70A    CLC   DUB+3(3),0(R4)      MATCH 00-FF                                  
         BNE   TD70B               YES                                          
         CLC   BYTE,DUB+3+2                                                     
         BE    TD80                MATCHED ON FLIGHT AS WELL                    
*                                                                               
TD70B    LA    R4,3(R4)            TRY NEXT PRD ENTRY                           
         B     TD70                                                             
*                                                                               
TD72     MVC   NERRCD,=Y(CLTFRZN)                                               
         TM    SVCOPT2,X'08'       TEST CLIENT FROZEN                           
         BO    TDERRX                                                           
         OC    SVCLOCK,SVCLOCK     TEST CLT LOCKED                              
         BZ    TD73                                                             
         CLC   2(2,R6),SVCLKSDT    ELEM TO LOCK START DATE                      
         BL    TD73                                                             
         CLC   2(2,R6),SVCLKNDT    ELEM TO LOCK END DATE                        
         BH    TD73                                                             
         B     TD73ERR                                                          
*                                                                               
TD73     OC    SVELOCK,SVELOCK     TEST EST LOCKED                              
         BZ    TD74                                                             
         CLC   2(2,R6),SVELKSDT    ELEM TO LOCK START DATE                      
         BL    TD74                                                             
         CLC   2(2,R6),SVELKNDT    ELEM TO LOCK END DATE                        
         BH    TD74                                                             
TD73ERR  MVC   NERRCD,=Y(FLTLCKD)                                               
         B     TDERRX                                                           
*                                                                               
TD74     L     R4,ASVDARE                                                       
         USING SVDARED,R4                                                       
         OC    SVDRELEM,SVDRELEM   INTERESTED IN ELEM AT GIVEN ADDR?            
         BNZ   TDX                 YES - DONE                                   
         DROP  R4                                                               
*                                                                               
TD75     CLI   BUYREC+3,X'FF'      TEST POL BUY                                 
         BNE   TD12                                                             
         B     TD40                                                             
         EJECT                                                                  
*=============================================================                  
* WHAT KIND OF DARE BUY DO WE HAVE HERE?                                        
*=============================================================                  
         SPACE 1                                                                
TD80     DS    0H                                                               
         MVI   SVDCORT,0           SET NO CASH/TRADE FLAG                       
         CLC   =C'000',SVDARPRF+6  ANY TRADE SPECIAL REP?                       
         BE    TD80A               NONE, ALL LOCKED                             
*                                                                               
         LA    RE,BUYREC                                                        
         SR    RF,RF                                                            
         MVC   WORK(2),BDREP-BUYKEY(RE)                                         
         LHI   RE,VRCPACK-BUYSAVE  ONLY CHECK THAT FIRST 2 DIGITS MATCH         
         AR    RE,RA                                                            
         L     RF,0(RE)                                                         
         GOTO1 (RF),DMCB,(C'U',WORK),WORK+8                                     
*                                                                               
TD80A    CLI   SVDARPRF+14,C'Y'    MULTIPLE TRADE REP CODES?                    
         BE    *+14                                                             
         CLC   WORK+8(3),SVDARPRF+6   NO, REP MATCHES FOR 3 DIGITS?             
         B     *+10                                                             
         CLC   WORK+8(2),SVDARPRF+6   YES, REP MATCHES FOR 2 DIGITS?            
         BE    TD80B                       YES, WE HAVE A TRADE BUYLINE         
         TM    2(R4),X'80'         CASH BUYLINE, CASH LOCKED?                   
         BZ    TD72                              NO, WE'RE OKAY                 
         MVI   SVDCORT,X'80'       SETS FOR CASH LOCKED ERROR                   
         B     TD80E                                                            
*                                                                               
TD80B    TM    2(R4),X'40'         TRADE LOCKED?                                
         BZ    TD72                                                             
         MVI   SVDCORT,X'40'       SETS FOR TRADE LOCKED ERROR                  
*                                                                               
TD80E    TM    UPSW,UPON           NO OPTION IF UPLOADING                       
         BO    TD80F                                                            
         CLI   SVOMPROF,C'Y'       TEST ORGASMIC                                
         BNE   TD80F               NOOOOO - POOR SUCKERS                        
*                                                                               
         MVC   SVDARRCL(3),DUB     SAVE DARE PRDS/FLIGHT                        
         S     R4,ASVDARE          GET DISPLACEMENT TO SVDARE ENTRY             
         STCM  R4,15,SVDARENT      SAVE DSPL TO ENTRY                           
         MVI   SVRCLOPT,RCLAUTA    SET AUTO RECALL PENDING                      
         MVC   NERRCD,=Y(AUTORCL)                                               
         B     TDERRX                                                           
*                                                                               
TD80F    MVC   NERRCD,=Y(DARELOCK)                                              
*                                                                               
         USING SVDARED,R4                                                       
TDERRX   MVI   ERRCD,NEWERRS       YES ERROR                                    
         L     R4,ASVDARE                                                       
         TM    SVDRFLG2,SVDRFLG2_ROE  RETURN TO CALLER ON ERROR?                
         BZ    *+12                    NO - ERR OUT                             
         MVI   SVDRFLG2,SVDRFLG2_ERR  INDICATE ERROR TO CALLER                  
         B     TDXX                                                             
         GOTO1 ERROR                                                            
*                                                                               
TDX      MVC   ELCDLO(2),DUB+6     RESTORE ELCODES !                            
         L     R4,ASVDARE                                                       
         XC    SVDRELEM,SVDRELEM   CLEAR ELEMENT ADDRESS !                      
         NI    SVDRFLG2,X'FF'-SVDRFLG2_ROE   RESET ROE FLAG                     
         DROP  R4                                                               
TDXX     XIT1                                                                   
         SPACE 2                                                                
**********************************************************************          
*   FIND FLIGHT SEQ NUMBER IN FLIGHT TABLE                           *          
*   ON ENTRY R6 POINTS TO BUY ELEMENT                                *          
*   ON EXIT DUB+2 CONTAINS FLIGHT NUMBER                             *          
**********************************************************************          
FLNUM    NTR1                                                                   
*                                                                               
         L     R4,ASVDARE                                                       
*                                                                               
* CHECK IF WE HAVE FLIGHT0, IF SO, CHECK DATE AGAINST FLT-0 ENDDATE             
         OC    SVDRFLT0-SVDARED(L'SVDRFLT0,R4),SVDRFLT0-SVDARED(R4)             
         BZ    FLNUM05                                                          
         CLC   2(2,R6),SVDRFLT0-SVDARED(R4)                                     
         BNH   FLNUMEX                                                          
*                                                                               
FLNUM05  LA    R4,8(R4)            POINT R4 TO START/END DATES                  
         LA    R7,1                FLIGHT SEQ NUM                               
         LA    R5,16               MAX NUM OF FLIGHTS                           
*                                                                               
* HOLES IN THE TABLE ARE IDENTIFIED BY X'FF' IN THE FIRST BYTE                  
* END OF THE TABLE IS IDENTIFIED BY NULLS IN THE FIRST TWO BYTES                
*                                                                               
FLNUM10  CLC   0(2,R4),=X'0000'    NO MORE FLIGHTS?                             
         BE    FLNUMEX                                                          
         CLC   2(2,R6),0(R4)       ELEM DATE TO FLIGHT START                    
         BL    FLNUM40                                                          
         CLC   2(2,R6),2(R4)                                                    
         BH    FLNUM40                                                          
         STC   R7,DUB+2                                                         
         B     FLNUMEX                                                          
*                                                                               
FLNUM40  LA    R4,4(R4)            NEXT FLIGHT                                  
         LA    R7,1(R7)            NEXT SEQ NUM                                 
         BCT   R5,FLNUM10          END OF FLIGHT TABLE?                         
*                                                                               
FLNUMEX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================*         
* CALL SPAUTH TO GENERATE SUPERDESK AUTHORIZATION TABLES IF NOT YET   *         
* BUILT, AND MAKE SURE ALL DATES IN RECORD BEING WRITTEN ARE COVERED  *         
* BY AN AUTHORIZATION.                                                *         
* TABLE CONTAINS DATES OF AUTHORIZATIONS WITH NO STATION LEVEL RECORD *         
* THAT MEANS IF THERE IS NO AUTH FOR A TIME PERIOD, WE IGNORE IT      *         
*=====================================================================*         
         SPACE 1                                                                
TSTSDE   NTR1  BASE=*,LABEL=*                                                   
         SPACE 1                                                                
*=================================================================*             
* SAVE CONTENTS OF AREC4/AREC5 IN 31BIT STORAGE WHILE IN TSTSDE   *             
*=================================================================*             
         SPACE 1                                                                
         OC    VSPAUTH,VSPAUTH                                                  
         BNZ   TSTSDE0                                                          
         MVC   DMCB+4(3),=X'D9000A'                                             
         MVI   DMCB+7,QSPAUTH                                                   
         GOTO1 VCALLOV,DMCB,0                                                   
         CLI   4(R1),X'FF'                                                      
         JE    *+2                                                              
         MVC   VSPAUTH,0(R1)                                                    
*                                                                               
TSTSDE0  L     RF,VCOMFACS                                                      
         L     RF,CWSSVR-COMFACSD(RF)                                           
* BUILD CALL TO W/S SAVE                                                        
         LHI   R1,SPAUTHWK-BUYSAVE                                              
         AR    R1,RA                                                            
         XC    0(L'SPAUTHWK,R1),0(R1)                                           
         USING FAWSSVRD,R1                                                      
*                                                                               
         MVC   FAWSTOKN,=CL4'REC4'                                              
         MVI   FAWSACTN,FAWSASVE                                                
         LHI   R0,REC5X-REC4       LEN OF REC4 + REC5                           
         STH   R0,FAWSLEN                                                       
         MVC   FAWSADR,AREC4                                                    
         GOTO1 (RF),(R1)                                                        
         CLI   FAWSRTN,0                                                        
         JNE   *+2                                                              
* BUILD SPAUTH PARAM BLOCK                                                      
         XC    0(L'SPAUTHWK,R1),0(R1)                                           
         USING SPAUTHD,R1                                                       
*                                                                               
         MVC   SPAIO,AREC4        USE REC4/REC5 AS 4K IOAREA                    
*                                                                               
         MVC   SPACOM,VCOMFACS                                                  
         L     RE,AREC                                                          
         MVC   SPAKAM(1),0(RE)     A-M                                          
         MVC   SPAKCLT,1(RE)       CLT                                          
*                                                                               
         MVC   SPAKPRD,SVPRD       USE HEADLINE PRD                             
         CLI   SPAKPRD,X'FF'       TEST PRD=POL                                 
         BNE   TSTSDE2             NO - USE IT                                  
         CLI   SVCPROF+0,C'0'      TEST TRUE POL CLIENT                         
         BE    TSTSDE2                                                          
         MVC   SPAKPRD(2),BDMASPRD-BUYREC(RE)  USE MASPRDS                      
*                                                                               
TSTSDE2  CLI   SPAKPRD,0           MAKE SURE HAVE GOOD PRD                      
         BNE   *+8                                                              
         MVI   SPAKPRD,X'FF'       IF NO MASPRD USE POL                         
*******  DC    H'0'                DON'T DIE HERE ANYMORE!                      
*                                                                               
         MVC   SPAKEST,9(RE)                                                    
         MVC   SPAKMKT(5),4(RE)   MKT/STA                                       
*                                                                               
         CLI   SPAKPRD+1,0         TEST PIGGYBACK                               
         BE    TSTSDE4             NO                                           
* NEED TO MAKE SURE PRODUCTS IN ALPHA SEQ                                       
         LA    RF,SPAKPRD          TRANSLATE PRD1                               
         BAS   RE,FINDPRD                                                       
         MVC   DUB(3),0(RF)        SAVE QPRD1                                   
*                                                                               
         LA    RF,SPAKPRD+1        TRANSLATE PRD2                               
         BAS   RE,FINDPRD                                                       
         MVC   DUB+3(3),0(RF)      SAVE QPRD2                                   
*                                                                               
         CLC   DUB(3),DUB+3        PRODUCTS IN ALPHA SEQ                        
         BL    TSTSDE4                                                          
*                                                                               
         L     RE,AREC                                                          
         MVC   SPAKPRD(1),BDMASPRD+1-BUYREC(RE)                                 
         MVC   SPAKPRD+1(1),BDMASPRD-BUYREC(RE)                                 
*                                                                               
TSTSDE4  LHI   R4,SVSDEDTS-BUYSAVE                                              
         AR    R4,RA                                                            
         ST    R4,SPAFATBL               SET DATE TABLE ADDRESS                 
*                                                                               
         LHI   RE,SVSDEKEY-BUYSAVE       POINT TO TABLE KEY                     
         AR    RE,RA                                                            
         CLC   SPAKEY,0(RE)              TEST KEYS AGREE                        
         BE    TSTSDE10                                                         
         MVC   0(L'SPAKEY,RE),SPAKEY     SAVE NEW KEY                           
*                                                                               
         OI    SPAFLAG,SPAFTBL     BUILD STA AUTH TABLE                         
*                                                                               
         GOTO1 VSPAUTH,(R1)        BUILD DATE TABLE                             
*                                                                               
* COMPARE DATES IN BIGGLESWORTH TABLE TO DATES IN BUY RECORD                    
*                                                                               
TSTSDE10 L     RE,AREC                                                          
         MVI   ELCDLO,6                                                         
         MVI   ELCDHI,13                                                        
         LA    R6,BDELEM-BUYREC(RE)                                             
*                                                                               
TSTSDE12 BAS   RE,NEXTEL                                                        
         BNE   TSTSDEX                                                          
*                                                                               
         LR    RE,R4               POINT TO START OF TABLE                      
         CLC   0(2,RE),=X'FFFF'    TEST EOT                                     
         BE    TSTSDEX                                                          
*                                                                               
TSTSDE20 CLC   2(2,R6),0(RE)       SPOT TO AUTH START                           
         BL    TSTSDE22                                                         
         CLC   2(2,R6),2(RE)       SPOT TO AUTH END                             
         BNH   TSTSDE24            IN PERIOD, NEXT SPOT                         
*                                                                               
TSTSDE22 AHI   RE,4                                                             
         CLC   0(2,RE),=X'FFFF'    TEST EOT                                     
         BE    TSTSDE12                                                         
         B     TSTSDE20                                                         
*                                                                               
* NEED TO ADD STA LEVEL AUTH - SWING BIGGLESWORTH INTO ACTION                   
*                                                                               
TSTSDE24 MVI   SPAFLAG,SPAFBUY+SPAFUPT                                          
         TM    UPSW,UPON           TEST SPOT BUY UPLOAD                         
         BZ    *+8                                                              
         OI    SPAFLAG,SPAFUPL     TELL SONIA                                   
*                                                                               
         MVC   SPASDTE,2(R6)       SET ELEMENT DATE AS START                    
         MVC   SPAEDTE,2(R6)       AND END                                      
         GOTO1 VSPAUTH,(R1)                                                     
         CLI   SPAERR,0                                                         
         BE    TSTSDE12                                                         
         DC    H'0'                                                             
*                                                                               
TSTSDEX  L     RF,VCOMFACS         BUILD CALL TO W/S SAVE                       
         L     RF,CWSSVR-COMFACSD(RF)                                           
         USING FAWSSVRD,R1                                                      
*                                                                               
         MVC   FAWSTOKN,=CL4'REC4'                                              
         MVI   FAWSACTN,FAWSARST                                                
         MVC   FAWSADR,AREC4                                                    
         GOTO1 (RF),(R1)                                                        
         CLI   FAWSRTN,0                                                        
         JNE   *+2                                                              
         XIT1                                                                   
         EJECT                                                                  
*=================================================================*             
* REGISTER USE HERE IS AWKWARD                                    *             
* RF POINTS TO BINARY PRODUCT CODE ON ENTRY                       *             
* AND TO EBCDIC PRODUCT CODE ON EXIT                              *             
*=================================================================*             
         SPACE 1                                                                
FINDPRD  LR    R0,RE               SAVE RETURN REG                              
*                                                                               
         LR    RE,RF               POINT RF TO REQUESTED PRDCODE                
         L     RF,ASVCLIST                                                      
*                                                                               
FINDPRD2 CLC   0(1,RE),3(RF)                                                    
         BE    FINDPRDX                                                         
         LA    RF,4(RF)                                                         
         CLI   0(RF),C'A'                                                       
         BNL   FINDPRD2                                                         
         DC    H'0'                                                             
*                                                                               
FINDPRDX LR    RE,R0               RESTORE RETURN REG                           
         BR    RE                                                               
         LTORG                                                                  
         EJECT                                                                  
*=============================================================*                 
* SWITCH BETWEEN MGA/MGE AND REGULAR BUY SCREENS AS REQUIRED  *                 
* R2 POINT TO CURRENT INPUT LINE (TO SAVE/RESTORE IT)         *                 
* IF SET                                                                        
*=============================================================*                 
         SPACE 1                                                                
SETSCRN  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   SVSCR,X'F8'         TEST SKEVAL SCREEN                           
         BNE   SETSCR02                                                         
         CLI   PFKEY,0             DO NOT CHANGE THE SCREEN                     
         BNE   SETSCRX                                                          
*                                                                               
SETSCR02 XC    ELEM,ELEM                                                        
         ZIC   R1,5(R2)            SAVE CURRENT INPUT LINE IN ELEM              
         STC   R1,ELEM             SAVE REAL LEN (THANKS MEL)                   
         LTR   R1,R1                                                            
         BZ    SETSCR04                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEM+1(0),8(R2)                                                  
*                                                                               
SETSCR04 CLC   =C'MGA',BUTRCODE                                                 
         BE    SETSCR10                                                         
         CLC   =C'MGE',BUTRCODE                                                 
         BE    SETSCR15                                                         
         CLC   =C'*MV',BUTRCODE                                                 
         BE    SETSCR20                                                         
         CLI   SVRCLOPT,RCLSTA     TEST LAST ACT CAN NTWK STA DSPL              
         BNE   SETSCR06                                                         
         OC    8(2,R2),SPACES                                                   
         CLC   =C'C ',8(R2)        DID THEY WANT TO CHANGE IT ?                 
         BE    SETSCR50            YES - LEAVE F1 SCREEN LOADED                 
*                                                                               
SETSCR06 CLI   SVRCLOPT,RCLNET     TEST LAST ACT CAN NTWK DSPLY                 
         BNE   SETSCR07                                                         
         CLC   =C'NSET',8(R2)      ARE THEY CHANGING IT                         
         BE    SETSCR50                                                         
*                                                                               
SETSCR07 CLI   SVRCLOPT,RCLCLST    OR NETWORK CUTIN STATION DSPLY               
         BNE   SETSCR08                                                         
         CLC   =C'CSET',8(R2)                                                   
         BE    SETSCR50                                                         
*                                                                               
SETSCR08 CLI   SVSCR,0             ELSE NEED FE SCREEN                          
         BNE   *+8                                                              
         MVI   SVSCR,X'FF'                                                      
*                                                                               
         CLI   SVSCR,X'FE'         TEST HAD FE/FF                               
         BNL   SETSCR50            YES - DONE                                   
*                                                                               
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(4),=X'D90211FE'                                           
         GOTO1 VCALLOV,DMCB,BUYHL1H                                             
         CLI   4(R1),X'FF'                                                      
         JE    *+2                                                              
         MVI   SVSCR,X'FE'                                                      
         B     SETSCR30                                                         
*                                                                               
* ACTION IS MGA - LOAD F0 SCREEN IF NECESSARY                                   
*                                                                               
SETSCR10 CLI   SVSCR,X'F0'         ARE WE IN THE MGA SCREEN                     
         BE    SETSCR50                                                         
*                                                                               
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(4),=X'D90211F0'                                           
         GOTO1 VCALLOV,DMCB,BUYHL1H                                             
*                                                                               
         MVI   SVSCR,X'F0'                                                      
         MVI   SVMGINIT,0                                                       
         B     SETSCR30                                                         
*                                                                               
* ACTION IS MGE - LOAD F3 SCREEN IF NECESSARY                                   
*                                                                               
SETSCR15 CLI   SVSCR,X'F3'         ARE WE IN THE MGE SCREEN                     
         BE    SETSCR50                                                         
         CLI   NEWHEADS,C'Y'       IF HEADLINES CHANGED                         
         BE    SETSCR17            MAKE SURE F3 GETS LOADED                     
         CLI   SVSCR,X'F2'         MGE ORBIT SCREEN?                            
         BE    SETSCR50                                                         
         CLI   SVSCR,X'F4'         MGE DAYPART SCREEN?                          
         BE    SETSCR50                                                         
         CLI   SVSCR,X'F5'                                                      
         BE    SETSCR50                                                         
         CLI   SVSCR,X'F8'                                                      
         BE    SETSCR50                                                         
*                                                                               
SETSCR17 XC    DMCB,DMCB                                                        
         MVC   DMCB+4(4),=X'D90211F3'                                           
         GOTO1 VCALLOV,DMCB,BUYHL1H                                             
*                                                                               
         MVI   SVSCR,X'F3'                                                      
         MVI   SVMGINIT,0                                                       
         B     SETSCR30                                                         
*                                                                               
SETSCR20 CLI   SVSCR,X'F7'         TEST ALREADY LOADED                          
         BE    SETSCR50                                                         
* LOAD BUY MOVE SCREEEN                                                         
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(4),=X'D90211F7'                                           
         GOTO1 VCALLOV,DMCB,BUYHL1H                                             
         MVI   SVSCR,X'F7'                                                      
         B     SETSCR30                                                         
*                                                                               
SETSCR30 LA    R2,BUYINP1H         RESTORE INPUT DATA                           
         CLI   ELEM,0                                                           
         BE    SETSCR40                                                         
         ZIC   R1,ELEM                                                          
         BCTR  R1,0                THANKS MEL                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),ELEM+1                                                   
*                                                                               
SETSCR40 OI    4(R2),X'80'         FIELD INPUT                                  
         LA    R1,1(R1)                                                         
         STC   R1,5(R2)            SET INPUT LENGTH                             
         OI    6(R2),X'80'         TRANSMIT FIELD                               
*                                                                               
         BAS   RE,SETXALL          SET TWA TO XMT ALL FIELDS                    
*                                                                               
SETSCR50 LA    R0,BUYINP1H                                                      
         ST    R0,FLAST                                                         
         CLI   SVSCR,X'F8'         TEST MOVE SCREEN                             
         BE    SETSCRX                                                          
         CLI   SVSCR,X'F7'         TEST MOVE SCREEN                             
         BE    SETSCRX                                                          
         CLI   SVSCR,X'F3'         TEST MGE SCREEN                              
         BE    SETSCRX                                                          
         CLI   SVSCR,X'F2'         TEST MGE ORBIT                               
         BE    SETSCRX                                                          
         CLI   SVSCR,X'F0'         TEST MGA SCREEN                              
         BE    SETSCRX                                                          
         CLI   SVSCR,X'F1'         TEST CANAD NTWK STA SCREEN                   
         BE    SETSCRX                                                          
*                                                                               
         LA    R0,BUYINP4H                                                      
         ST    R0,FLAST                                                         
SETSCRX  XIT1                                                                   
*                                                                               
SETXALL  LA    R2,BUYMSGH                                                       
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNE   *-10                                                             
         MVI   1(R2),1             SET TO TRANSMIT ALL                          
         MVI   2(R2),1                                                          
         BR    RE                                                               
         EJECT                                                                  
*=================================================================*             
* NOTE R8 CONTAINS RELO VALUE FOR THIS OVERLAY                                  
*=================================================================*             
         SPACE 1                                                                
INITL    NTR1  BASE=*,LABEL=*                                                   
         L     R1,VMONPARM         RESTORE MONITORS PARM POINTER                
*                                                                               
         L     R0,0(R1)                                                         
         ST    R0,VTIOB            A(TRANSLATOR IO BLOCK)                       
         L     R0,12(R1)                                                        
         ST    R0,VTIA             A(TIA)                                       
*                                                                               
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
         LM    R2,R4,0(R1)                                                      
         ST    R3,VTWA                                                          
*                                                                               
         MVC   VDATAMGR(80),0(R4)  FACILITY LIST                                
         L     RF,16(R1)                                                        
         ST    RF,VCOMFACS         ** SAVE COMFACS ADDRESS **                   
         MVC   VGLOBBER,CGLOBBER-COMFACSD(RF)                                   
         LR    RA,R3                                                            
         MVC   AGYALPHA,14(RA)                                                  
         LA    R3,64(R3)           PRESET ERROR MSG ADDRESS                     
         ST    R3,ERRAREA                                                       
         MVI   DMINBTS,X'C0'                                                    
         MVI   DMOUTBTS,X'FD'                                                   
         LA    R0,REC                                                           
         ST    R0,AREC                                                          
* SET UP COMMON FACILITY LINKAGES                                               
         L     R3,=A(SPCOMMON)                                                  
         AR    R3,R8                                                            
         SR    R4,R4                                                            
         LA    R5,ERROR                                                         
         LA    R0,SPCOMCNT                                                      
*                                                                               
INIT10   ST    R3,0(R5)            SET A(SPCOMMON)                              
         STC   R4,0(R5)            SET BRANCH TABLE DSPL                        
         LA    R4,4(R4)            BUMP DSPL                                    
         LA    R5,4(R5)            NEXT SUBR                                    
         BCT   R0,INIT10                                                        
         SPACE 1                                                                
* CODE TO INITIALIZE EXTENDED USER ROUTINES (15-18) *                           
         SPACE 1                                                                
         LA    R5,USER15                                                        
         LA    R0,4                                                             
*                                                                               
INIT12   ST    R3,0(R5)            SET A(SPCOMMON)                              
         STC   R4,0(R5)            SET BRANCH TABLE DSPL                        
         LA    R4,4(R4)            BUMP DSPL                                    
         LA    R5,4(R5)            NEXT SUBR                                    
         BCT   R0,INIT12                                                        
*                                                                               
* GET SOME CORERES MODULE ADDRESSES                                             
*                                                                               
         MVI   DMCB+7,QSTAPACK                                                  
         BAS   RE,INICALLO                                                      
         MVC   VSTAPACK,0(R1)                                                   
         MVI   DMCB+7,QUNTIME                                                   
         BAS   RE,INICALLO                                                      
         MVC   VUNTIME,0(R1)                                                    
         MVI   DMCB+7,QDAYUNPK                                                  
         BAS   RE,INICALLO                                                      
         MVC   VDAYUNPK,0(R1)                                                   
         MVI   DMCB+7,QDEMOVAL                                                  
         BAS   RE,INICALLO                                                      
         MVC   VDEMOVAL,0(R1)                                                   
         MVI   DMCB+7,QGETBROD                                                  
         BAS   RE,INICALLO                                                      
         MVC   VGETBROD,0(R1)                                                   
         MVI   DMCB+7,QMOBILE                                                   
         BAS   RE,INICALLO                                                      
         MVC   VMOBILE,0(R1)                                                    
         MVI   DMCB+7,QTSAR                                                     
         BAS   RE,INICALLO                                                      
         MVC   VTSAR,0(R1)                                                      
         MVI   DMCB+7,QGETBUY                                                   
         BAS   RE,INICALLO                                                      
         MVC   VGETBUY,0(R1)                                                    
         MVI   DMCB+7,QSPDEMEXT                                                 
         BAS   RE,INICALLO                                                      
         MVC   VSPDEMEXT,0(R1)                                                  
*                                                                               
INIT14   L     R3,VBUYTWA                                                       
         USING T211FFD,R3                                                       
         LA    RE,2303(R3)                                                      
         ST    RE,VBUYTWAX                                                      
         OI    BUYSERVH+1,X'01'    SERVICE FIELD ALWAYS MODIFIED                
         OI    BUYSERVH+6,X'80'                                                 
*                                                                               
         LR    RA,R3               POINT RA TO TWA+4200                         
         AHI   RA,NBUYSAVE-T211FFD                                              
         ST    RA,VBUYSAVE                                                      
         USING BUYSAVE,RA                                                       
*                                                                               
         MVI   DMCB+7,QRCPACK                                                   
         BAS   RE,INICALLO                                                      
         LHI   RE,VRCPACK-BUYSAVE                                               
         AR    RE,RA                                                            
         MVC   0(4,RE),0(R1)                                                    
*                                                                               
         MVI   DMCB+7,QSPSLNTB                                                  
         BAS   RE,INICALLO                                                      
         LHI   RE,VSLNTAB-BUYSAVE                                               
         AR    RE,RA                                                            
         MVC   0(4,RE),0(R1)                                                    
*                                                                               
         L     R1,VTIOB                                                         
         USING TIOBD,R1                                                         
         ZIC   R0,TIOBAID                                                       
         CHI   R0,12                                                            
         BNH   *+8                                                              
         AHI   R0,-12                                                           
         STC   R0,PFKEY            SAVE ADJUSTED PFKEY                          
*                                                                               
         LA    R0,SVKEY                                                         
         ST    R0,ADRSVKEY                                                      
*                                                                               
         MVC   SVRECUP,VRECUP      SAVE REAL RECUP ADDRESS                      
         MVC   VRECUP,USER9        AND SUBSTITUTE USER9                         
         EJECT                                                                  
* SET UP ADDITIONAL DSECT ADDRESSABILITY                                        
         LA    RE,REC                                                           
         ST    RE,AREC1                                                         
         AHI   RE,REC2-REC                                                      
         ST    RE,AREC2                                                         
         AHI   RE,REC3-REC2                                                     
         ST    RE,AREC3                                                         
         AHI   RE,REC4-REC3                                                     
         ST    RE,AREC4                                                         
         AHI   RE,REC5-REC4                                                     
         ST    RE,AREC5                                                         
* NOTE A(WSGLTAB) NOT SAVED - ALWAYS REFERENCED BY DSPL INTO SPBUYWK            
         MVI   SVWSGLIN,C'N'       SET TABLE NOT INITLZD FLAG                   
         AHI   RE,BUDBLOCK-REC5                                                 
         ST    RE,ADBLOCK                                                       
         AHI   RE,OVWORK-BUDBLOCK                                               
         ST    RE,AOVWORK                                                       
         L     RE,=A(MGWORK-SPBUYWKD)                                           
         AR    RE,RC                                                            
         ST    RE,AMGWORK                                                       
         SH    RE,=H'8'                                                         
         MVC   0(8,RE),=C'*MGWORK*'                                             
*                                                                               
         L     RE,=A(DSKED)                                                     
         AR    RE,R8                                                            
         ST    RE,VDSKED                                                        
*                                                                               
         L     RE,=A(BLDFLD)                                                    
         AR    RE,R8                                                            
         ST    RE,VBLDFLD                                                       
*                                                                               
         LHI   RE,SVCLIST-BUYSAVE                                               
         AR    RE,RA                                                            
         ST    RE,ASVCLIST                                                      
*                                                                               
         LHI   RE,SVOLDDEF-BUYSAVE                                              
         AR    RE,RA                                                            
         ST    RE,ASVNOLD                                                       
*                                                                               
         LHI   RE,SVDARE-BUYSAVE                                                
         AR    RE,RA                                                            
         ST    RE,ASVDARE                                                       
* RESET AT LEAST SOME SVDARE FIELDS                                             
         MVI   SVDRFLG2-SVDARED(RE),0 RESET TRANSIENT FLAGS                     
         LA    RE,SVDRELEM-SVDARED(RE)                                          
         XC    0(4,RE),0(RE)                                                    
*                                                                               
         XC    SVBUYDA,SVBUYDA                                                  
         XC    SVDATABP,SVDATABP                                                
*                                                                               
         L     RE,=A(TSTDARE)                                                   
         AR    RE,R8                                                            
         ST    RE,ATESTDAR                                                      
*                                                                               
         L     RE,=A(SETSCRN)                                                   
         AR    RE,R8                                                            
         ST    RE,ASETSCRN                                                      
*                                                                               
         L     R4,=A(BLOCK-SPBUYWKD)                                            
         AR    R4,RC               GET ADDRESSABILITY TO WORK AREA              
         ST    R4,ABLOCK                                                        
*                                                                               
         L     RE,=A(CALLBASE)                                                  
         AR    RE,R8                                                            
         ST    RE,VCALLBAS                                                      
*                                                                               
         LHI   RE,SVSECRET-BUYSAVE                                              
         AR    RE,RA                                                            
         ST    RE,ASECBLK                                                       
*                                                                               
         L     RE,=A(CALLB32)                                                   
         AR    RE,R8                                                            
         ST    RE,VGOBUY32                                                      
*                                                                               
         L     RE,=A(CALLB38)                                                   
         AR    RE,R8                                                            
         ST    RE,VGOBUY38                                                      
*                                                                               
         L     RE,=A(EDITBTY)                                                   
         AR    RE,R8                                                            
         LHI   RF,VEDITBTY-BUYSAVE                                              
         AR    RF,RA                                                            
         ST    RE,0(RF)                                                         
*                                                                               
INITX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
INICALLO LR    R0,RE                                                            
         XC    DMCB(7),DMCB                                                     
         MVC   DMCB+4(3),=X'D9000A'                                             
         GOTO1 VCALLOV,DMCB                                                     
         LR    RE,R0                                                            
         CLI   4(R1),X'FF'                                                      
         BNER  RE                                                               
         DC    H'0'                                                             
         LTORG                                                                  
         EJECT                                                                  
*========================================================*                      
* BLDROW HAS START ROW, BLDCOL HAS START COL             *                      
* EACH BLDLIST ENTRY IS 8 BYTES.                         *                      
*  BYTE 0 = NUMBER OF LEADING SPACES                     *                      
*       1 = DATA LENGTH                                  *                      
*       2 = ATTRIBUTE BYTE (PROT/UNP)                    *                      
*       3 = NUMBER OF TRAILING SPACES                    *                      
*       4 = SPARE                                        *                      
*     5-7 = DATA ADDRESS (OR 0)                          *                      
*                                                        *                      
*  ALL FIELDS MUST FIT THIS LINE OR USE NEXT LINE.       *                      
* XL4'00' IS E-O-L                                       *                      
*========================================================*                      
         SPACE 1                                                                
BLDFLD   NTR1  BASE=*,LABEL=*      RC SHOULD STILL POINT TO SPBUYWORK           
*                                                                               
         LA    R2,BUYOUTH                                                       
         CLI   SVSCR,X'F1'                                                      
         BNE   *+8                                                              
         LA    R2,BUYINP1H         CAN NTWK STATION DISPLAY                     
*                                                                               
BLDF6    SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0             TEST E-O-S                                   
         BNE   *-10                                                             
*                                                                               
         CLC   BLDROW,=H'24'                                                    
         BH    BLDFNEQ                                                          
*                                                                               
         OC    BLDROW,BLDROW                                                    
         BNZ   *+14                                                             
         MVC   BLDROW(4),=X'000E0000'                                           
         LA    R2,BUYOUTH                                                       
*                                                                               
         SR    R0,R0                                                            
         SR    RE,RE                                                            
         LA    R5,BLDLIST                                                       
BLDF12   IC    R0,0(R5)                 LEADING SPACES                          
         AR    RE,R0                                                            
         IC    R0,1(R5)            DATA LEN                                     
         AR    RE,R0                                                            
         LA    R0,2                UNPROTECTED FIELDS NEED 2 BYTES 0/H          
         TM    2(R5),X'20'         TEST PROTECTED                               
         BZ    *+6                                                              
         SR    R0,R0                                                            
         AR    RE,R0                                                            
         IC    R0,3(R5)            TRAILING SPACES                              
         AR    RE,R0                                                            
         LA    R5,8(R5)                                                         
         OC    0(4,R5),0(R5)                                                    
         BNE   BLDF12                                                           
*                                                                               
         LH    R0,BLDCOL                                                        
         AR    R0,RE                                                            
         CHI   R0,80                                                            
         BL    BLDF14                                                           
* TRY IGNORING LAST TRAILING SPACES                                             
         AHI   R5,-8                                                            
         ZIC   R6,3(R5)                                                         
         SR    R0,R6                                                            
         CHI   R0,80                                                            
         BNH   BLDF14                                                           
* WILL NOT FIT THIS ROW - TRY NEXT                                              
         XC    BLDCOL,BLDCOL                                                    
         LH    R6,BLDROW                                                        
         LA    R6,1(R6)                                                         
         STH   R6,BLDROW                                                        
         CHI   R6,24                                                            
         BNL   BLDFNEQ                                                          
         CLI   PFKEYLEN,0                                                       
         BE    BLDF14                                                           
         CHI   R6,23           PFKEYS USE ROW 24                                
         BH    BLDFNEQ                                                          
*                                                                               
* MAKE SURE DATA FITS IN TWA. RE HAS DATA LEN                                   
*                                                                               
BLDF14   LA    R5,BLDLIST                                                       
         LA    RE,8(RE)            ADD 8 FOR EACH FLDHDR                        
         LA    R5,8(R5)                                                         
         OC    0(4,R5),0(R5)                                                    
         BNZ   *-14                                                             
         LA    RE,3(RE)            ADD 3 FOR GENCER INDICS                      
         LA    RE,10(RE)           AND 10 MORE FOR TAB FIELD                    
         AR    RE,R2                                                            
         SR    R0,R0               ALLOW ROOM FOR PFKEYS TOO                    
         IC    R0,PFKEYLEN                                                      
         AR    RE,R0                                                            
         C     RE,VBUYTWAX                                                      
         BNL   BLDFNEQ                                                          
*                                                                               
         LA    R5,BLDLIST                                                       
*                                                                               
BLDF16   XC    0(8,R2),0(R2)                                                    
         FOUT  (R2)                                                             
         OI    4(R2),X'20'         SET 'EDITED' FLAG                            
         ZIC   RE,1(R5)            GET FIELD LEN                                
         STC   RE,7(R2)            SET IT                                       
         OI    7(R2),X'80'         SET SPECIAL FLAG FOR XLTR                    
         BCTR  RE,0                                                             
         L     RF,4(R5)            GET DATA ADDR                                
         LTR   RF,RF                                                            
         BNZ   *+8                                                              
         LA    RF,SPACES                                                        
         EX    RE,BLDFMVC                                                       
         EX    RE,BLDFOC                                                        
BLDF18   LA    RE,9(RE)            RESTORE TO LEN+8                             
         STC   RE,0(R2)            SET LEN+8                                    
* BUILD OUTPUT FIELD ADDRESS                                                    
         LH    RE,BLDROW                                                        
         BCTR  RE,0                                                             
         MHI   RE,80                                                            
         AH    RE,BLDCOL                                                        
         ZIC   R0,0(R5)            ADD LEADING SPACES                           
         AR    RE,R0                                                            
         TM    2(R5),X'20'         TEST PROTECTED                               
         BO    *+8                                                              
         LA    RE,1(RE)                                                         
         STH   RE,2(R2)                                                         
         MVC   1(1,R2),2(R5)       SET PRO/UNP IND                              
         TM    2(R5),X'20'         TEST PROT                                    
         BO    *+8                                                              
         OI    1(R2),X'08'         SET HIGH INTENSITY FOR UNP FIELDS            
         ZIC   RE,0(R2)            GET FIELD LEN                                
         AR    R2,RE               POINT TO NEW END OF SCREEN                   
* UPDATE COLUMN                                                                 
         ZIC   RE,0(R5)            LEADING SPACES                               
         ZIC   R0,1(R5)            DATA LENGTH                                  
         AR    RE,R0                                                            
         LA    R0,2                                                             
         TM    2(R5),X'20'         TEST PROTECTED                               
         BZ    *+6                                                              
         SR    R0,R0                                                            
         AR    RE,R0               + OVERHEAD                                   
         IC    R0,3(R5)            TRAILING SPACES                              
         AR    RE,R0                                                            
         AH    RE,BLDCOL           + PREVIOUS COL                               
         STH   RE,BLDCOL                                                        
*                                                                               
         LA    R5,8(R5)                                                         
         OC    0(4,R5),0(R5)                                                    
         BNE   BLDF16                                                           
         MVC   0(3,R2),=X'000000'  SET E-O-S FLAG                               
         CR    RB,RB               SET CC =                                     
         B     *+6                                                              
BLDFNEQ  LTR   RB,RB                                                            
         XIT1  REGS=(R2)                                                        
*                                                                               
BLDFMVC  MVC   8(0,R2),0(RF)                                                    
BLDFOC   OC    8(0,R2),SPACES                                                   
         LTORG                                                                  
         EJECT                                                                  
*==============================================================*                
* TRANSFER CONTROL BACK TO CALLING PROGRAM ON PF12             *                
*==============================================================*                
XFRETN   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R5,ELEM                                                          
         USING GLVXFRSY,R5                                                      
         MVC   GLVXFRSY,=C'SPO'    FROM SPOT/BUY                                
         MVC   GLVXFRPR,=C'BUY'                                                 
         MVC   GLVXTOSY,=C'SPO'    TO                                           
         MVC   GLVXTOPR,=C'OM '                                                 
         TM    SVXFRCTL,SVXFR_DARE                                              
         BO    XFRETN2                                                          
         MVC   GLVXTOPR,=C'MAK'                                                 
         TM    SVXFRCTL,SVXFR_MAK   TEST CALLED BY MATCHMAKER                   
         JNO   *+2                  WHO CALLED ?                                
*                                                                               
XFRETN2  OI    GLVXFLG1,GLV1RETN+GLV1SEPS                                       
         MVC   GLVXSESR,SVXFRSID   SET CALLER/CALLEE SESSION IDS                
         OI    GLVXFLG1,GLV1SIDR+GLV1SIDE                                       
         DROP  R5                                                               
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',ELEM,GLVXLENQ,GLVXCTL                     
*                                                                               
         TM    SVXFRCTL,SVXFR_MAK   TEST CALLED BY MATCHMAKER                   
         BZ    XFRETNX                                                          
* BUY NEEDS TO TELL EVAN STUFF                                                  
         XC    ELEM,ELEM                                                        
         CLI   ERRAREA,0                                                        
         BE    XFRETN4                                                          
         MVC   ELEM+1(1),ERRCD                                                  
         CLI   ERRCD,NEWERRS                                                    
         BNE   *+10                                                             
         MVC   ELEM(2),NERRCD                                                   
         MVI   ELEM+3,3            SYSTEM 3 ERRORS                              
         GOTO1 VGLOBBER,DMCB,=C'PUTD',ELEM,78,GLVBUY1                           
         B     XFRETNX                                                          
*                                                                               
* RETURN LINE NUMBER, MAKEGOOD CODE, BDELEM, & REGELS                           
*                                                                               
XFRETN4  MVC   ELEM+4(2),BUYRLIN     MOVE LINE NUMBER                           
         MVC   ELEM+6(2),BUMGCODE    MOVE MAKEGOOD CODE                         
         MVC   ELEM+8(3),SVMAKEDT    PASS ELEMDT/NUMBER                         
*                                                                               
         MVC   ELEM+11(66),BDELEM    66 BYTES AS OF 2/24/11                     
         GOTO1 VGLOBBER,DMCB,=C'PUTD',ELEM,78,GLVBUY1                           
*                                                                               
         LA    R4,ELEM                                                          
         XC    ELEM,ELEM                                                        
*                                                                               
         L     R6,AREC1                                                         
         AHI   R6,24                                                            
         MVI   ELCDLO,11                                                        
         CLI   BUYKEY+3,X'FF'      TEST POL BUY                                 
         BE    *+8                                                              
         MVI   ELCDLO,6                                                         
         MVC   ELCDHI,ELCDLO                                                    
*                                                                               
XFRETN20 BRAS  RE,NEXTEL                                                        
         BNE   XFRETN22                                                         
         MVC   0(2,R4),2(R6)       MOVE ELEMENT DATE                            
                                                                                
         MVI   2(R4),1             SET NUMBER OF SPOTS                          
         CLI   BUYKEY+3,X'FF'                                                   
         BE    *+10                                                             
         MVC   2(1,R4),6(R6)       MOVE NUMBER OF SPOTS                         
         AHI   R4,3                                                             
         LA    R0,ELEM+230         HAVE ROOM FOR 3 78-BYTE LINES                
         CR    R4,R0                                                            
         BNH   XFRETN20                                                         
*                                                                               
XFRETN22 LA    R4,ELEM                                                          
         GOTO1 VGLOBBER,DMCB,=C'PUTD',(R4),78,GLVBUY2                           
         AHI   R4,78                                                            
         GOTO1 (RF),(R1),,(R4),,GLVBUY3                                         
         AHI   R4,78                                                            
         GOTO1 (RF),(R1),,(R4),,GLVBUY4                                         
*                                                                               
XFRETNX  XIT1                                                                   
         LTORG                                                                  
*                                                                               
XFRDATAD DSECT                                                                  
*                                                                               
XFMAKERR DS    XL2                 ERROR MESSAGE NUMBER                         
XFMAKTYP DS    XL1                 MESSAGE TYPE (0)                             
XFMAKSYS DS    XL1                 OVERRIDE SYSTEM NUMBER                       
*                                                                               
XFMAKLIN DS    XL1                                                              
XFMAKMG  DS    CL2                                                              
         EJECT                                                                  
T21100   CSECT                                                                  
*                                                                               
NEXTEL   CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         JZ    *+2                                                              
         AR    R6,R0                                                            
NEXTEL2  CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         CLC   ELCDLO,0(R6)                                                     
         JH    NEXTEL                                                           
         CLC   ELCDHI,0(R6)                                                     
         JL    NEXTEL                                                           
         CR    RB,RB                                                            
         J     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
         LTORG                                                                  
         EJECT                                                                  
*===========================================================*                   
* SETPFKEY - MOVE PFKEY DATA TO END OF TWA                  *                   
* DISPLAY ROUTINES SET PFKEYLEN/PFKEYADR                    *                   
*===========================================================*                   
         SPACE 1                                                                
SETPFKEY NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         SR    R0,R0                                                            
SETPF2   ICM   R0,1,0(R2)                                                       
         BZ    SETPF4                                                           
         AR    R2,R0                                                            
         B     SETPF2                                                           
*                                                                               
SETPF4   XC    0(8,R2),0(R2)                                                    
         L     RF,PFKEYADR                                                      
         ZIC   RE,PFKEYLEN                                                      
         STC   RE,7(R2)            SET OUTPUT FIELD LEN                         
         OI    7(R2),X'80'         SET FLAG FOR XLTR                            
         BCTR  RE,0                                                             
         EX    RE,SETPFMVC                                                      
         LA    RE,9(RE)            SET TO LEN +8                                
         STC   RE,0(R2)            SET IN BYTE 0 OF FLDHDR                      
         MVI   1(R2),X'20'         SET 'PROTECTED'                              
         MVC   2(2,R2),=AL2((24-1)*80+10)  SET ROW 24/COL 10 ADDRESS            
         AR    R2,RE               POINT TO EOS                                 
         XC    0(3,R2),0(R2)       SET FLAGS                                    
*                                                                               
         XC    PFKEYADR,PFKEYADR   CLEAR PFKEY SAVE DATA                        
         XIT1                                                                   
*                                                                               
SETPFMVC MVC   8(0,R2),2(RF)                                                    
         LTORG                                                                  
         EJECT                                                                  
*============================================================*                  
* SUBROUTINE TO TEST GOALS PRESENT FOR WEEK OF SPOT          *                  
* TABLE IS BUILT IN SPBUY01 WHENEVER HEADLINES ARE EDITED    *                  
* PARAM 1 HAS ADDRESS OF INSERTION ELEMENT                   *                  
*============================================================*                  
         SPACE 1                                                                
BUTSTG   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   SVWSGLIN,C'Y'       TEST TABLE RESTORED FROM XA YET              
         JE    BLDGL1              YES                                          
*                                                                               
         L     R0,0(R1)            SAVE CALLING P1  !!!!                        
         LA    R1,DMCB                                                          
         USING FAWSSVRD,R1                                                      
         MVC   FAWSTOKN,=C'GLDP'                                                
         MVI   FAWSACTN,C'U'       RESTORE FROM UTL XA BUFFER                   
         LHI   RE,WSGLTAB-SPBUYWKD                                              
         AR    RE,RC                                                            
         ST    RE,FAWSADR                                                       
*                                                                               
         L     RF,VCOMFACS                                                      
         L     RF,CWSSVR-COMFACSD(RF)                                           
         GOTO1 (RF),(R1)                                                        
         CLI   FAWSRTN,0                                                        
         JNE   *+2                                                              
         MVI   SVWSGLIN,C'Y'                                                    
         DROP  R1                                                               
         ST    R0,0(R1)            RESTORE CALLING P1 !!!!                      
                                                                                
* STEP 1 FIND THE DAYPART IN SVDPEQTAB *                                        
                                                                                
BLDGL1   XC    DSPAREA(24),DSPAREA                                              
         LA    R8,DSPAREA                                                       
*                                                                               
         CLI   SVCXTRA+8,C'P'      TEST PG GOALS                                
         BE    BLDGL20                                                          
*                                                                               
         LA    RE,SVGLDPEQ                                                      
         LA    R0,L'SVGLDPEQ/2     R0=MAX N'ENTRIES                             
         MVC   BYTE,BDDAYPT                                                     
         ICM   RF,15,AGLDATA       TEST OVERRIDE GOAL DATA                      
         BZ    BLDGL2                                                           
         USING GLDATAD,RF                                                       
         MVC   BYTE,GLDATA_DPT     USE OVERRRIDE DAYPART                        
         DROP  RF                                                               
*                                                                               
BLDGL2   CLC   BYTE,0(RE)                                                       
         BE    BLDGL4                                                           
         LA    RE,2(RE)                                                         
         BCT   R0,BLDGL2                                                        
         MVI   ERRCD,MSSNGDPT                                                   
         GOTO1 ERROR                                                            
*                                                                               
BLDGL4   DS    0H                                                               
         ZIC   RF,1(RE)                                                         
         SRL   RF,4                                                             
         STC   RF,BYTE             SAVE FIRST DIGIT (RIGHT ALIGNED)             
                                                                                
* STEP 2 - FIND ALL OCCURENCES OF FIRST DIGIT *                                 
                                                                                
         LA    R0,L'SVGLDPEQ/2                                                  
         LA    RE,SVGLDPEQ                                                      
*                                                                               
BLDGL6   ZIC   RF,1(RE)                                                         
         SRL   RF,4                                                             
         CLM   RF,1,BYTE                                                        
         BNE   BLDGL12                                                          
*                                                                               
BLDGL10  MVC   0(1,R8),0(RE)       MOVE CODE TO PROCESS LIST                    
         LA    R8,1(R8)                                                         
*                                                                               
BLDGL12  LA    RE,2(RE)                                                         
         CLI   0(RE),0                                                          
         BE    *+8                                                              
         BCT   R0,BLDGL6                                                        
         B     TESTGL2                                                          
*                                                                               
BLDGL20  L     RE,0(R1)            GET PRD FROM NEW ELEMENT                     
         CLI   1(RE),10            TEST UNALL                                   
         BE    TESTGLX                                                          
         MVC   GLHALF(1),10(RE)    MOVE PRODUCT CODE                            
         MVC   GLHALF+1(1),BDSEC                                                
         B     TESTGL10                                                         
                                                                                
* STEP 3 - SEARCH THE TABLE FOR EACH DAYPART IN LIST *                          
                                                                                
TESTGL2  LA    R8,DSPAREA          POINT TO LIST                                
*                                                                               
TESTGL4  MVC   GLHALF(1),0(R8)                                                  
         MVC   GLHALF+1(1),BDSEC                                                
*                                                                               
         ICM   RF,15,AGLDATA       TEST OVERRIDE GOAL DATA                      
         BZ    *+10                                                             
         USING GLDATAD,RF                                                       
         MVC   GLHALF+1(1),GLDATA_SLN  USE OVERRRIDE LEN                        
         DROP  RF                                                               
*                                                                               
TESTGL10 MVI   ERRCD,NOGOALS                                                    
         LHI   R4,WSGLTAB-SPBUYWKD  GET DSPL TO TABLE IN STORAGE                
         AR    R4,RC                POINT TO TABLE                              
         AHI   R4,WSGLDPTS-WSGLTABD POINT TO WEEK DATA                          
*                                                                               
TESTGL20 CLC   GLHALF,0(R4)                                                     
         BE    TESTGL30                                                         
         LA    R4,10(R4)                                                        
         CLI   0(R4),0             TEST END OF TABLE                            
         BNE   TESTGL20                                                         
         B     TESTGL70                                                         
                                                                                
* PARAM1 POINTS TO REGELEM BEING INSERTED                                       
* R4 POINTS TO TABLE ENTRY FOR THIS DPT/SLN OR PRD/SLN FOR PG                   
                                                                                
TESTGL30 MVI   ERRCD,0             SET FLAG WE ARE SEARCHING WEEKS              
*                                                                               
TESTGL40 L     R6,0(R1)            POINT TO NEW ELEMENT                         
         LHI   RF,WSGLTAB-SPBUYWKD GET DSPL TO TABLE                            
         AR    RF,RC               POINT TO TABLE OF SUNDAY DATES               
         SR    RE,RE               CLEAR COUNTER                                
*                                                                               
TESTGL50 CLC   2(2,R6),0(RF)       TEST SPOT IN WEEK                            
         BNH   TESTGL60                                                         
         LA    RF,2(RF)            TABLE ENDS WITH X'FFFF'                      
         BCT   RE,TESTGL50         SO NO TEST FOR END OF TABLE NEEDED           
*                                                                               
TESTGL60 LPR   RE,RE                                                            
         SRDL  RE,3                DIVIDE BY 8                                  
         LA    RE,2(R4,RE)         POINT TO BYTE IN TABLE ENTRY                 
         SRL   RF,29               SHIFT REMAINDER                              
         IC    R0,GLBITTAB(RF)     GET BIT VALUE                                
         STC   R0,BYTE                                                          
         NC    BYTE,0(RE)          TEST BIT ON (W/O CHANGING TABLE)             
         BNZ   TESTGLX                                                          
         EJECT                                                                  
* NO GOALS FOUND FOR THIS DAYPART - TRY NEXT *                                  
                                                                                
TESTGL70 LA    R8,1(R8)            NEXT LIST ENTRY                              
         CLI   0(R8),0                                                          
         BNE   TESTGL4                                                          
                                                                                
TESTGL80 CLI   ERRCD,0             TEST WEEKLY ERROR                            
         BNE   TESTGL84                                                         
                                                                                
* BUILD ERROR MESSAGE FOR WEEK WITHOUT GOALS *                                  
                                                                                
TESTGL82 MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(NOGLWK)                                                
         GOTO1 VDATCON,DMCB,(2,2(R6)),(4,ERRTEXT)                               
*                                                                               
TESTGL84 CLI   SVCXTRA+8,C'P'      TEST PG GOALS                                
         BNE   TESTGL86                                                         
*****                                                                           
         TM    UPSW,UPON           UPLOAD?  IGNORE AS PER  0114544T             
         BNZ   TESTGLX             YES, DON'T CARE THEN                         
*****                                                                           
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(NOPGGOAL)                                              
         TM    SVXFRCTL,SVXFR_MAK  TEST CALLED BY MATCHMAKER                    
         BZ    *+10                NO                                           
         MVC   NERRCD,=Y(PGMGUNAL) GIVE UNALLOCATED CRAP MSG                    
*                                                                               
TESTGL86 GOTO1 ERROR                                                            
*                                                                               
TESTGLX  XIT1                                                                   
*                                                                               
GLBITTAB DC    X'8040201008040201'                                              
         LTORG                                                                  
         EJECT                                                                  
*=================================================================*             
* TEST IF INPUT LINE IS A MOVE REQUEST                            *             
* FORMAT IS 1MOVE OR *1MOVE                                       *             
*=================================================================*             
         SPACE 1                                                                
         DS    0D                                                               
TESTMOVE NTR1  BASE=*,LABEL=*                                                   
         LA    R1,8(R2)                                                         
         CLI   0(R1),C'*'                                                       
         BNE   *+8                                                              
         LA    R1,1(R1)                                                         
*                                                                               
         CLI   0(R1),C'0'          MAKE SURE A NUMBER IS PRESENT                
         BL    TESTMVX                                                          
*                                                                               
         LA    R0,3                                                             
TESTMV2  CLI   0(R1),C'0'                                                       
         BL    TESTMV4                                                          
         CLI   0(R1),C'9'                                                       
         BH    TESTMV4                                                          
         LA    R1,1(R1)                                                         
         BCT   R0,TESTMV2                                                       
*                                                                               
TESTMV4  CLC   0(4,R1),=C'MOVE'                                                 
         BNE   TESTMVX                                                          
         MVC   BUTRCODE,=C'*MV'   SET TRANSACTION CODE                          
*                                                                               
TESTMVX  XIT1                      EXIT WITH CC EQ OR NEQ TO MOVE               
         LTORG                                                                  
         EJECT                                                                  
         DS    0D                                                               
CANCHK   NTR1  BASE=*,LABEL=*                                                   
         OC    SVNDEF(16),SVNDEF                                                
         BZ    CANCHKX                                                          
         OC    SVKEY+14(4),SVKEY+14                                             
         BZ    CANCHKX                                                          
         CLI   BUTRCODE,C'B'       TEST NEW BUY                                 
         BE    CANCHKX                                                          
         CLI   RCLOPT,C'N'         TEST RECALL MULTIPLE                         
         BE    CANCHKX                                                          
         CLI   RCLOPT,C'S'         OR SORTED                                    
         BE    CANCHKX                                                          
* NEED TO TEST IF READ ONLY                                                     
         XC    DUB,DUB                                                          
         LHI   R0,-1                                                            
         ST    R0,DUB                                                           
         L     RF,VCOMFACS                                                      
         L     RF,(CSWITCH-COMFACSD)(RF)                                        
         GOTO1 (RF),DUB                                                         
         L     RF,0(R1)                                                         
         USING UTLD,RF                                                          
         TM    TSTAT5,TST5NONO     TEST NO UPDATE OCCURRED                      
         BZ    CANCHK2                                                          
         DROP  RF                                                               
         XC    SVKEY+14(4),SVKEY+14  SET FLAG NO REC AROUND                     
         B     CANCHKX                                                          
*                                                                               
CANCHK2  MVC   KEY(18),SVKEY                                                    
         LA    RE,REC                                                           
         ST    RE,AREC                                                          
         GOTO1 GETREC                                                           
         CLC   KEY(10),REC                                                      
         JNE   *+2                                                              
         CLC   KEY+11(2),REC+10    SAME LINE NUMBER                             
         JNE   *+2                                                              
CANCHKX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*&&DO                                                                           
*=================================================================*             
* MAKE SURE SPOD SLNS SUM TO A MULTIPLE OF BDSEC                  *             
*=================================================================*             
         SPACE 1                                                                
         DS    0D                                                               
TSTSPOD  NTR1  BASE=*,LABEL=*                                                   
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0C'                                                     
         L     R8,AREC                                                          
         LA    R6,BDELEM-BUYREC(R8)                                             
         XC    ELEMDT,ELEMDT                                                    
         SR    RF,RF                                                            
*                                                                               
TSTSPOD2 BRAS  RE,NEXTEL                                                        
         BNE   TSTSPOD4                                                         
         TM    6(R6),X'80'         TEST MINUS                                   
         BO    TSTSPOD2            YES - IGNORE                                 
         CLC   ELEMDT,2(R6)                                                     
         BE    TSTSPOD6                                                         
         LTR   RF,RF                                                            
         BZ    TSTSPOD6                                                         
*                                                                               
TSTSPOD4 SR    RE,RE                                                            
         SR    R0,R0                                                            
         IC    R0,BDSEC-BUYREC(R8)                                              
         DR    RE,R0                                                            
         LTR   RE,RE               TEST REMAINDER                               
         BNZ   TSTSPDER                                                         
         SR    RF,RF               CLEAR SLN SUM                                
*                                                                               
TSTSPOD6 CLI   0(R6),0             TEST E-O-R                                   
         BE    TSTSPODX            EXIT WITH CC EQ                              
*                                                                               
         CLI   1(R6),14                                                         
         BL    TSTSPOD2                                                         
         JNE   *+2                 SHOULD NEVER BE A PIGGYBACK                  
*                                                                               
         MVC   ELEMDT,2(R6)                                                     
         SR    R0,R0                                                            
         IC    R0,11(R6)                                                        
         AR    RF,R0                                                            
         B     TSTSPOD2                                                         
*                                                                               
TSTSPDER MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(BADSPOD)                                               
         GOTO1 VDATCON,DMCB,(2,ELEMDT),(4,ERRTEXT)                              
         LTR   RB,RB               SET CC NEQ                                   
*                                                                               
TSTSPODX XIT1                                                                   
         LTORG                                                                  
*&&                                                                             
         EJECT                                                                  
*=================================================================*             
* VARIOUS TESTS BEFORE DOING PUTRECS/ADDRECS                                    
* EXIT CC EQU  = CARRY ON (TEST UPDFLAG)                                        
* EXIT CC NEQ  = EXIT WITH ERROR                                                
*=================================================================*             
         SPACE 1                                                                
TESTFIL  NTR1  BASE=*,LABEL=*                                                   
         MVI   GBYACT,0                                                         
         MVI   UPDFLAG,C'Y'        SET TO UPDATE FILE                           
         L     RE,AREC                                                          
         CLI   0(RE),X'10'         TEST BUYREC                                  
         BL    TF10                 NO                                          
*                                                                               
         TM    SVCOPT1,COP1GMI     TEST DFS GMI CLIENT                          
         BZ    *+12                                                             
         BRAS  RE,TSTGMI                                                        
         BNE   TFXNEQ              ERROR EXIT                                   
*                                                                               
         TM    SVXFRCTL,SVXFR_SDT  TEST DESKTOP MODE                            
         BZ    *+8                  NO                                          
         BRAS  RE,ADDDTXEL         ADD DESKTOP XFER EL                          
*                                                                               
         BRAS  RE,BUYVAL           CALL SPBUYVAL                                
*                                                                               
TF10     L     RE,ASVDARE                                                       
         OC    0(2,RE),0(RE)       TEST DARE LOCKOUT ACTIVE                     
         BZ    *+8                                                              
         BRAS  RE,TSTDARE                                                       
*                                                                               
         TM    SVSPOMAK,SVSPOMAK_NOBUY TEST DO-NOT-ADD                          
         BNO   *+12                     NO                                      
         MVI   UPDFLAG,C'N'             SET NO UPDATE                           
         B     TFXEQ                    AND EXIT                                
*                                                                               
         TM    SVESTFL1,EF1SDE    TEST SUPERDESK AUTH ACTIVE THIS EST           
         BZ    TF20                                                             
         L     RE,AREC                                                          
         CLI   0(RE),X'10'        TEST BUYREC                                   
         BL    TF20                                                             
         BRAS  RE,TSTSDE          DO SUPERDESK DATE TESTS                       
*                                                                               
TF20     OC    SVPASSWD,SVPASSWD   TEST PASSWD PROTECT ACTIVE                   
         BZ    *+8                                                              
         BRAS  RE,SETACTV                                                       
*                                                                               
         LHI   RE,SVB0PROF-BUYSAVE                                              
         AR    RE,RA                                                            
         CLI   7(RE),C'0'          TEST REASON CODES REQUIRED                   
         BNH   *+8                                                              
         BRAS  RE,SETRSN                                                        
*                                                                               
         CLC   =C'ADDREC',COMMAND                                               
         BNE   TFPUTREC                                                         
*                                                                               
* TESTS/ACTIONS FOR ADDREC ONLY                                                 
TFADDREC L     RE,AREC             IF ADDING A BUYREC,                          
         CLI   0(RE),X'10'          MAKE SURE DTOP ADD FLAG IS OFF!             
         BL    TFXEQ                                                            
         MVI   GBYACT,C'A'                                                      
         NI    BDSTAT3-BUYREC(RE),X'FF'-BDST3_DSKADD                            
         B     TFXEQ                                                            
*                                                                               
* TESTS/ACTIONS FOR PUTREC ONLY                                                 
TFPUTREC CLC   =C'PUTREC',COMMAND                                               
         BE    *+6                                                              
         DCHO                                                                   
*                                                                               
         L     RE,AREC             IF CHANGING A BUYREC,                        
         CLI   0(RE),X'10'          MAKE SURE DTOP CHANGE FLAG IS OFF           
         BL    TFPUT10                                                          
         MVI   GBYACT,C'P'                                                      
         NI    BDSTAT3-BUYREC(RE),X'FF'-BDST3_DSKCHG                            
*                                                                               
         CLI   LOCKFLAG,C'Y'       LOCK TESTED ALREADY                          
         BE    TFPUT10                                                          
         MVI   LOCKFLAG,C'Y'                                                    
         BRAS  RE,TSTLOCK                                                       
         BE    TFPUT10                                                          
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(DATALOCK)                                              
         B     TFXNEQ              EXIT WITH ERROR                              
*                                                                               
TFPUT10  L     RE,DMCB+20          GET DMCBW6                                   
         LA    RE,0(RE)            CLEAR HOB                                    
         LA    R0,PRDLIST          PRDLIST IS ONLY VALID DMCBW6                 
         CR    R0,RE                                                            
         BE    *+10                                                             
         XC    DMCB+20(4),DMCB+20                                               
         B     TFXEQ                                                            
*                                                                               
TFXEQ    CR    RB,RB               HERE FOR NORMAL EXIT                         
         B     *+6                                                              
TFXNEQ   CR    RB,RD               HERE FOR ERROR EXIT                          
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*=================================================================              
* GMI BUYS AFTER 5/31/04 MUST BE IN ONE BROADCAST MONTH                         
* SINCE THEY DO NOT BUY OOWR'S, ONLY WEEK START DATES ARE TESTED                
*=================================================================              
         SPACE 1                                                                
         DS    0D                                                               
TSTGMI   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R8,AREC                                                          
         CLC   BDSTART-BUYREC(3,R8),=X'68051F'  START AFTER 5/31/04             
         BL    TSTGMI10            NO                                           
*                                                                               
         GOTO1 VCALLOV,DMCB,0,X'D9000A1D'  GET A(GETBROAD)                      
         MVC   WORK(4),0(R1)               SAVE ADDRESS                         
*                                                                               
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0C'                                                     
         L     R8,AREC                                                          
         LA    R6,BDELEM-BUYREC(R8)                                             
         BRAS  RE,NEXTEL                                                        
         BNE   TSTGMI10                                                         
         GOTO1 VDATCON,DMCB,(2,2(R6)),WORK+6  GET 6 BYTE DATE                   
         L     RF,WORK                                                          
         GOTO1 (RF),DMCB,(1,WORK+6),WORK+12,VGETDAY,VADDAY                      
*                                                                               
TSTGMI2  BRAS  RE,NEXTEL                                                        
         BNE   TSTGMI10                                                         
         GOTO1 VDATCON,DMCB,(2,2(R6)),WORK+6  GET 6 BYTE DATE                   
         L     RF,WORK                                                          
         GOTO1 (RF),DMCB,(1,WORK+6),WORK+24,VGETDAY,VADDAY                      
         CLC   WORK+12(12),WORK+24            TEST SAME BDCST MONTH             
         BE    TSTGMI2                                                          
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(NOT1MON)                                               
         LTR   RB,RB                                                            
         B     TSTGMIX                                                          
*                                                                               
TSTGMI10 CR    RB,RB                                                            
*                                                                               
TSTGMIX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*========================================================                       
* UPDATE DESKTOP TRANSFER ELEM                                                  
*  NOTE: ELEM EDITED IN PLACE FOR FEAR OF TRASHING ELEM!                        
*========================================================                       
         SPACE                                                                  
ADDDTXEL NTR1  BASE=*,LABEL=*                                                   
         L     R6,AREC             FIND AND REMOVE OLD TRANSFER ELEM            
         AHI   R6,BDELEM-BUYREC                                                 
         XR    R0,R0                                                            
*                                                                               
ADDDTX10 CLI   0(R6),0                                                          
         BE    ADDDTX20                                                         
         CLI   0(R6),DTXCODEQ                                                   
         BNE   *+12                                                             
         BRAS  RE,ELEMDEL                                                       
         B     ADDDTX20                                                         
*                                                                               
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     ADDDTX10                                                         
                                                                                
ADDDTX20 XC    DUB,DUB             BUILD NEW TRANSFER ELEM                      
         MVI   DUB,DTXCODEQ                                                     
         MVI   DUB+1,DTXSLNQ                                                    
                                                                                
         GOTO1 VRECUP,DMCB,BUYREC,DUB,(R6)                                      
                                                                                
         USING DTXELEM,R6                                                       
         MVI   DTXSTAT,DTXS_OLD    SET OLD SAVE BUY (SPBUY39)                   
         MVC   DTXVER,SVPCVRSN     SAVE PC VERSION NUMBER                       
         THMS  DDSTIME=YES                                                      
         ST    R0,DUB                                                           
         ST    R1,FULL                                                          
         AP    DUB(4),FULL                                                      
*                                                                               
         CP    DUB(4),=P'240000'      PAST MIDNIGHT?                            
         BL    *+10                                                             
         SP    DUB(4),=P'240000'      YES, BUMP TO NEXT DAY AND ADJUST          
         L     R1,DUB                                                           
         SRL   R1,4                GET RID OF SECONDS AND SIGN                  
         STCM  R1,7,DTXTIME                                                     
         DROP  R6                                                               
                                                                                
         XIT1                                                                   
         EJECT                                                                  
*==================================================                             
* CALL SPBUYVAL BEFORE ADDING/UPDATING BUY RECORDS                              
*==================================================                             
BUYVAL   NTR1  BASE=*,LABEL=*,WORK=(R6,8)                                       
         TM    SVOPT1,SVOPT1_NOBYVAL                                            
         BNZ   BUYVX                                                            
         GOTOR VCALLOV,DMCB,0,X'D9000A2A'  GET ADDRESS OF BUYVAL                
         L     RF,0(R1)                                                         
         USING SPBUYVLD,R6                                                      
         XC    SPBYLEN,SPBYLEN                                                  
         MVC   SPBYAREC,AREC                                                    
         MVC   SPBYAFAC,VCOMFACS                                                
                                                                                
         GOTO1 (RF),(R1),(SVCPROF,SPBYLEN)                                      
         CLI   SPBYERR,0                                                        
         BE    *+6                                                              
         DCHO                                                                   
*                                                                               
BUYVX    XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
*=================================================================*             
* TEST DATA LOCKED BY OFFLINE APPLICATION                         *             
* THIS CODE SHOULD BE CHANGED TO CALL LOCKUP WHEN ALL CONVENTIONS *             
* ARE AGREED. LOCKUP/LOCKET DSECTS ARE IDENTICAL                  *             
*=================================================================*             
         SPACE 1                                                                
         DS    0D                                                               
TSTLOCK  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    LKUPKEY,LKUPKEY                                                  
L        USING LKKEYD,LKUPKEY                                                   
*                                                                               
         MVC   L.LOCKAGY,AGYALPHA                                               
         MVC   L.LOCKRTY,=C'BU'    LOCK BUYS                                    
         MVC   L.LOCKMED,BUYMD                                                  
         MVC   L.LOCKCLT,QCLT                                                   
         MVC   L.LOCKSTA,QSTA                                                   
         CLI   L.LOCKSTA,C'0'                                                   
         BL    *+8                                                              
         MVI   L.LOCKSTA+4,C'/'                                                 
         BAS   RE,TSTIT                                                         
* TEST ALLOCATION KEYS                                                          
         XC    LKUPKEY,LKUPKEY                                                  
L        USING LKKEYD,LKUPKEY                                                   
*                                                                               
         MVC   L.LOCKAGY,AGYALPHA                                               
         MVC   L.LOCKRTY,=C'BA'    LOCK BUYS                                    
         MVC   L.LOCKMED,BUYMD                                                  
         MVC   L.LOCKCLT,QCLT                                                   
         SR    R0,R0                                                            
         IC    R0,SVEST                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  L.LOCKEST,DUB                                                    
         MVC   DUB(3),L.LOCKEST                                                 
         BAS   RE,TSTIT                                                         
*                                                                               
         MVC   L.LOCKEST,SPACES                                                 
         BAS   RE,TSTIT                                                         
*                                                                               
         CLI   SVAPROF+7,C'C'      FOR CAN, MAKE SURE MED C NOT LOCKED          
         BNE   TSTLKEQ                                                          
         CLI   BUYMD,C'T'                                                       
         BE    *+12                                                             
         CLI   BUYMD,C'N'                                                       
         BNE   TSTLKEQ                                                          
         MVI   L.LOCKMED,C'C'                                                   
         MVC   L.LOCKEST,DUB                                                    
         BAS   RE,TSTIT                                                         
*                                                                               
         MVC   L.LOCKEST,SPACES                                                 
         BAS   RE,TSTIT                                                         
         B     TSTLKEQ                                                          
         DROP  L                                                                
*                                                                               
TSTIT    LR    R0,RE                                                            
*                                                                               
TSTIT2   L     RF,VCOMFACS                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB,('LKTESTQ',LKUPKEY),VCOMFACS                           
         CLI   4(R1),1             TEST LOCKED                                  
         BE    TSTLKNEQ                                                         
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTIT2                                                           
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
TSTLKEQ  CR    RB,RB                                                            
         B     *+6                                                              
TSTLKNEQ LTR   RB,RB                                                            
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
RCLMSG   BASR  R1,RE               POINT R1 TO MESSAGE                          
         DC    CL50'** ORDER  RECALLED - HIT ENTER TO CONTINUE **'              
BACKMSG  BASR  R1,RE               POINT R1 TO MESSAGE                          
         DC    CL22'** BACK TO SPOT BUY **'                                     
       ++INCLUDE FALOCKUPD                                                      
LKKEYD   DSECT                                                                  
         ORG   LOCKKEY                                                          
LOCKMED  DS    CL1                                                              
LOCKCLT  DS    CL3                                                              
LOCKSTA  DS    CL5                                                              
         ORG   LOCKSTA                                                          
LOCKEST  DS    CL3                                                              
         ORG                                                                    
T21100   CSECT                                                                  
*                                                                               
*=============================================================                  
* TSTLNKIO - INITIAL LINKIO INTERFACE                                           
*  NOTE: AREC5 IS ONLY LIOB FOR INITIAL CALL - THEN LIOB IS                     
*        MOVED TO W/S OF SPBUY39 SO IT STAYS PUT!                               
*=============================================================                  
*                                                                               
TSTLNKIO NTR1  BASE=*,LABEL=*                                                   
         L     RF,VCOMFACS                                                      
         LHI   RE,ALINKIO-BUYSAVE                                               
         AR    RE,RA                                                            
         MVC   0(4,RE),CLINKIO-COMFACSD(RF)                                     
*                                                                               
         L     R4,AREC5                                                         
         USING LIOBD,R4                                                         
         LR    R0,R4                                                            
         LHI   R1,LIOBX-LIOBD      CLEAR PARAM BLOCK AREA TOO                   
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R0,VTIA                                                          
         ST    R0,LIOBABUF         FIRST 14K OF TIA (WRKF SIZE)                 
         AHI   R0,14*1024                                                       
         ST    R0,LIOBAREC         LAST 4K OF TIA                               
         MVC   LIOBACOM,VCOMFACS                                                
         MVI   LIOBMSYS,3          INDICATE MSGSYS NUMBER                       
*                                                                               
         LHI   RF,ALINKIO-BUYSAVE                                               
         AR    RF,RA                                                            
         L     RF,0(RF)                                                         
         GOTOR (RF),DMCB,('LIOAINI',LIOBD)                                      
         BNE   TSTLNKX             NOT LINKIO                                   
         MVC   SVPCVRSN,LIOBPCV1   SAVE THE PC VERSION NUMBER                   
*                                                                               
TSTLNKX  XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*=================================================================              
* VALIDATE SLN IN BYTE AGAINST NEW CORE-RESIDENT PHASE                          
*=================================================================              
                                                                                
CHKSLN   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LHI   RF,VSLNTAB-BUYSAVE                                               
         AR    RF,RA                                                            
         L     R1,0(RF)            POINT TO SLNTAB                              
         LH    RE,0(R1)            GET ENTRY LENGTH                             
         L     RF,2(R1)            GET DSPL TO EOT                              
         AR    RF,R1               RELOCATE EOT ADDRESS                         
         AHI   R1,6                POINT TO FIRST ENTRY                         
*                                                                               
         SR    R0,R0                                                            
         LA    R0,C'T'                                                          
         CLI   BUYMD,C'T'                                                       
         BE    CHKSLN2                                                          
         CLI   BUYMD,C'N'                                                       
         BE    CHKSLN2                                                          
         CLI   BUYMD,C'C'                                                       
         BE    CHKSLN2                                                          
*                                                                               
         LA    R0,C'R'                                                          
         CLI   BUYMD,C'R'                                                       
         BE    CHKSLN2                                                          
         CLI   BUYMD,C'X'                                                       
         JNE   *+2                                                              
*                                                                               
CHKSLN2  CLC   =C'00',0(R1)        FIND DEFAULT ENTRY                           
         BE    CHKSLN4                                                          
         CLC   AGYALPHA,0(R1)      MATCH AGY                                    
         BNE   *+12                                                             
CHKSLN4  CLM   R0,1,2(R1)          MATCH MEDIA                                  
         BE    CHKSLN6                                                          
*                                                                               
         BXLE  R1,RE,CHKSLN2       NEXT ENTRY                                   
         DC    H'0'                                                             
*                                                                               
CHKSLN6  MVI   ERRCD,SLNERR                                                     
         AHI   R1,4                POINT BEYOND TABLE ID                        
         SR    RE,RE                                                            
         IC    RE,BYTE             GET SLN                                      
         AR    RE,RE               X 2                                          
         AR    RE,R1               POINT TO ENTRY                               
         CLI   1(RE),0             TEST SLN VALID                               
         XIT1                      EXIT WITH CC SET                             
         LTORG                                                                  
         EJECT                                                                  
*====================================================================           
* VALIDATE BOOKTYPE  ON ENTRY R4 POINTS TO DATA                                 
*====================================================================           
                                                                                
EDITBTY  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LR    R4,R1               POINT TO BOOKTYPE DATA                       
*                                                                               
         MVI   BUBKTYPE,0          CLEAR RESULT                                 
         CLC   =C'NO',0(R4)                                                     
         BE    EDITBTX                                                          
*                                                                               
EDITBT2  MVC   HALF,0(R4)                                                       
         CLI   HALF+1,C'A'                                                      
         BH    *+8                                                              
         MVI   HALF+1,C' '                                                      
*                                                                               
         L     RF,VCOMFACS                                                      
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,SPBOOKTB  GET A(BOOK TABLE)                            
         ICM   RF,15,0(R1)         A(TABLE)RETURNED IN P1                       
         JZ    *+2                                                              
*                                                                               
         L     R0,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
         USING SPBKTYPD,RF                                                      
*                                                                               
EDITBT4  CLI   0(RF),X'FF'                                                      
         BNE   EDITBT6                                                          
         MVI   ERRCD,INVBKTY                                                    
         GOTO1 ERROR                                                            
*                                                                               
EDITBT6  CLC   HALF,SPBKTYPA                                                    
         BE    *+10                                                             
         AR    RF,R0                                                            
         B     EDITBT4                                                          
*                                                                               
         MVC   BUBKTYPE,SPBKTYPN                                                
EDITBTX  XIT1                                                                   
         DROP  RF                                                               
         LTORG                                                                  
         EJECT                                                                  
*SPBUYWORK                                                                      
       ++INCLUDE SPBUYWORK                                                      
         EJECT                                                                  
       ++INCLUDE SPAUTHD                                                        
         PRINT OFF                                                              
*SPDEMUPD                                                                       
       ++INCLUDE SPDEMUPD                                                       
         PRINT OFF                                                              
*SPGENAGY                                                                       
       ++INCLUDE SPGENAGY                                                       
*SPGENCLT                                                                       
CLTHDRD DSECT                                                                   
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
*SPADINTD                                                                       
       ++INCLUDE SPADINTD                                                       
         EJECT                                                                  
*FACTSD                                                                         
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
*SPGENSDEF                                                                      
       ++INCLUDE SPGENSDEF                                                      
         EJECT                                                                  
*SPGENMTR                                                                       
MTRRECD  DSECT                                                                  
       ++INCLUDE SPGENMTR                                                       
         EJECT                                                                  
*SPGENDBLBK                                                                     
       ++INCLUDE SPGENDBLBK                                                     
         EJECT                                                                  
*DDPERVALD                                                                      
       ++INCLUDE DDPERVALD                                                      
         SPACE 2                                                                
*SPDSTBLK                                                                       
       ++INCLUDE SPDSTBLK                                                       
         SPACE 2                                                                
*SPDBLBOOKD                                                                     
       ++INCLUDE SPDBLBOOKD                                                     
         SPACE 2                                                                
*SPBUYVALD                                                                      
       ++INCLUDE SPBUYVALD                                                      
         EJECT                                                                  
* DDCOREQUS                                                                     
* DDGLOBEQUS                                                                    
* FATIOB                                                                        
* FAUTL                                                                         
         SPACE 1                                                                
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE DDGLPFMD                                                       
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FAUTL                                                          
       ++INCLUDE SPGETDEMD                                                      
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE FAWSSVRD                                                       
LIOBD    DSECT                                                                  
       ++INCLUDE DDLINKIOD                                                      
       ++INCLUDE DEDEMTABD                                                      
       ++INCLUDE SPDEMEXTD                                                      
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'111SPBUY00   02/26/21'                                      
         END                                                                    
