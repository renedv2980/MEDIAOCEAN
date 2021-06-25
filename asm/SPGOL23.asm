*          DATA SET SPGOL23    AT LEVEL 041 AS OF 12/18/17                      
*PHASE T20223B                                                                  
         TITLE 'SPGOL23 - BRDDOL/BRDPCT ADD/DELETE/EXPLODE   T20223'            
T20223   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20223                                                         
         L     RC,0(R1)                                                         
         LA    R9,2048(RC)                                                      
         LA    R9,2048(R9)                                                      
         USING GENOLD,RC,R9                                                     
*                                                                               
         L     RA,4(R1)                                                         
         USING T202FFD,RA                                                       
         EJECT                                                                  
         XC    GKEY,GKEY                                                        
         XC    BDATA,BDATA         CLEAR OLD DATA VALUES                        
* CLEAR ALL MKT NAMES WITH CHANGED MARKETS                                      
         LA    R4,GOLACT1H                                                      
GL1A     LR    R2,R4               SAVE PREVIOUS HDR ADDRESS                    
         SR    R5,R5                                                            
         IC    R5,0(R4)                                                         
         AR    R4,R5                                                            
         CLI   0(R4),0                                                          
         BE    GL1X                                                             
         TM    1(R4),X'20'         TEST PROTECTED (MKTNAME)                     
         BZ    GL1A                NO - SKIP                                    
         TM    4(R2),X'20'         IS MARKET VALID                              
         BO    GL1A                YES - NEXT                                   
         IC    R5,0(R4)            GET LENGTH                                   
         SH    R5,=H'9'            SET FOR EX                                   
         EX    R5,GLOC             IS ANYTHING THERE                            
         BZ    GL1A                NO                                           
         EX    R5,GLXC                                                          
         FOUT  (R4)                                                             
         B     GL1A                                                             
*                                                                               
GLOC     OC    8(0,R4),8(R4)                                                    
GLXC     XC    8(0,R4),8(R4)                                                    
*                                                                               
GL1X     DS    0H                                                               
         LA    R2,GOLACT1H                                                      
*                                                                               
GL2      ST    R2,BLNADDR          SAVE START OF LINE ADDRESS                   
         MVI   ERRCD,MSSNGERR                                                   
         CLI   5(R2),0                                                          
         BE    GLERR                                                            
         MVC   BACT,8(R2)                                                       
*                                                                               
         CLI   8(R2),C'A'          TEST ADD                                     
         BE    *+12                                                             
         CLI   8(R2),C'D'                                                       
         BNE   GL3                                                              
         CLI   SVSCRN,X'FC'        TEST BRDDOL                                  
         BNE   GL3                                                              
* MAKE SURE TIER NUMBER ENTERED                                                 
         MVI   ERRCD,INVERR                                                     
         CLI   9(R2),C'0'                                                       
         BL    GLERR                                                            
         CLI   9(R2),C'9'                                                       
         BH    GLERR                                                            
         CLI   10(R2),C'0'                                                      
         BL    GLERR                                                            
         CLI   10(R2),C'9'                                                      
         BH    GLERR                                                            
         MVC   BTIER,9(R2)         SAVE CURRENT TIER NUMBER                     
         B     GL4                                                              
*                                                                               
GL3      XC    BDOLS,BDOLS         FORCE $ AND PTS NEXT ADD                     
         XC    BPTS,BPTS                                                        
         MVI   BCPPSW,0                                                         
*                                                                               
GL4      DS    0H                                                               
         CLI   8(R2),C'*'                                                       
         BNE   GL6                                                              
*                                                                               
* NO CHANGED DATA FIELDS ALLOWED ON * LINE                                      
*                                                                               
         MVI   ERRCD,CHDTAERR                                                   
         LR    R4,R2                                                            
         SR    R5,R5                                                            
         LA    R0,7                TEST MKT/NAME/DPT/DOLS/PTS/PER               
         B     GL4B                                                             
*                                                                               
GL4A     TM    1(R4),X'20'         TEST PROTECTED                               
         BO    GL4B                YES - SKIP                                   
         TM    4(R4),X'20'         TEST VALIDATED                               
         BZ    GLERR                                                            
*                                                                               
GL4B     IC    R5,0(R4)                                                         
         AR    R4,R5                                                            
         BCT   R0,GL4A                                                          
*                                                                               
         B     GLMKT                                                            
         EJECT                                                                  
GL6      CLI   8(R2),C'A'                                                       
         BE    GLMKT                                                            
         CLI   8(R2),C'D'                                                       
         BE    GLMKT                                                            
         CLI   SVSCRN,X'FC'        TEST BRDDOL                                  
         BNE   *+12                                                             
         CLI   8(R2),C'E'                                                       
         BE    GLMKT                                                            
         MVI   ERRCD,INVERR                                                     
         B     GLERR                                                            
         SPACE 2                                                                
GLMKT    SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         CLI   5(R2),0             TEST FOR DATA                                
         BNE   GLMKT2              OK                                           
         MVI   ERRCD,MSSNGERR                                                   
         OC    BMKT,BMKT           DID WE HAVE A MARKET ABOVE                   
         BZ    GLERR                                                            
* CLEAR DATA IN MKTNAME (IF ANY)                                                
         B     GLMKTX                                                           
*                                                                               
GLMKT2   GOTO1 USER1               'EDTMKT'                                     
*                                                                               
         TM    4(R2),X'20'                                                      
         BO    GLMKTX                                                           
         SR    R4,R4                                                            
         IC    R4,0(R2)                                                         
         AR    R4,R2                                                            
         FOUT  (R4),SVMKTNAM,16                                                 
*                                                                               
GLMKTX   OI    4(R2),X'20'                                                      
         EJECT                                                                  
GLDPT    SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         IC    R0,0(R2)            SKIP MKT NAME                                
         AR    R2,R0                                                            
*                                                                               
         CLI   SVSCRN,X'FB'        TEST BRDPCT                                  
         BNE   GLDPT1                                                           
         MVI   BDPT,C'Z'                                                        
         MVI   ERRCD,SLNERR                                                     
         TM    4(R2),X'08'         TEST NUMERIC                                 
         BZ    GLERR                                                            
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         LTR   R1,R1                                                            
         BZ    GLERR                                                            
         STC   R1,BSLN                                                          
*                                                                               
         GOTO1 USER5               CHKSLN                                       
*                                                                               
GLDPT1   CLI   5(R2),0                                                          
         BNE   GLDPT2                                                           
         CLI   BDPT,0                                                           
         BNZ   GLDPTX                                                           
         MVI   ERRCD,MSSNGERR                                                   
         B     GLERR                                                            
*                                                                               
GLDPT2   GOTO1 USER2               'EDTDPTLN'                                   
*                                                                               
         CLI   SVPRD,X'FF'         TEST CPP                                     
         BE    GLDPT4                                                           
* NOT CPP                                                                       
         MVI   ERRCD,SLNERR                                                     
         CLI   BSLN,0                                                           
         BE    GLERR                                                            
         B     GLDPTX                                                           
* CPP                                                                           
GLDPT4   DS    0H                                                               
         CLI   BSLN,30             ** THIS IS FOR COMPATIBILITY **              
         BE    GLDPTX                                                           
*                                                                               
         MVI   ERRCD,INVERR                                                     
         CLI   BSLN,0              SPTLN MUST BE OMITTED                        
         BNE   GLERR                                                            
         MVI   BSLN,30             FORCE 30 SEC LEN                             
*                                                                               
GLDPTX   OI    4(R2),X'20'                                                      
         EJECT                                                                  
* EDIT BUDGET DOLLARS OR WEEKLY PCT                                             
*                                                                               
GLBUD    ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         CLI   SVSCRN,X'FC'        TEST BRDDOL                                  
         BNE   GLPCT                                                            
* EDIT BRAND DOLLARS IF ACTN=ADD                                                
         CLI   BACT,C'A'                                                        
         BNE   GLBUDX                                                           
         MVI   ERRCD,MSSNGERR                                                   
         CLI   5(R2),0                                                          
         BE    GLERR                                                            
         ZIC   R5,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,(2,8(R2)),(R5)                                     
         MVI   ERRCD,INVERR                                                     
         CLI   0(R1),X'FF'                                                      
         BE    GLERR                                                            
         MVC   BDOLS,4(R1)         BDOLS WILL BE ADJUSTED BY TALFAC             
         MVC   BPTS,4(R1)          BPTS WILL CONTAIN UNADJUSTED DOLS            
         CLC   =C'DF',AGYALPHA                                                  
         BNE   GLBUDX                                                           
         OC    SVTLNT,SVTLNT                                                    
         BNZ   GLBUD10                                                          
* READ NEW TALENT FACTOR                                                        
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D27'                                                  
         MVC   KEY+2(1),SVAGYMD                                                 
         MVC   KEY+3(2),SVCLT                                                   
         MVC   KEY+5(6),SVSTART    ESTIMATE START DATE                          
         GOTO1 HIGH                                                             
         MVI   ERRCD,NOTALERR                                                   
         CLC   KEY(5),KEYSAVE                                                   
         BNE   GLERR                                                            
         MVC   KEY+11(1),SVPTAL                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(5),KEYSAVE                                                   
         BNE   GLERR                                                            
         CLC   SVPTAL,KEY+11                                                    
         BNE   GLERR                                                            
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         GOTO1 GETREC                                                           
         USING TALRECD,R8                                                       
* NOTE - MUL AND DIV FACTORS ARE INVERTED FOR GOALS INPUT                       
         MVC   SVTLNT(2),TAL05MUL+2                                             
         MVC   SVTLNT+2(2),TAL05DIV+2   (10000)                                 
         DROP  R8                                                               
*                                                                               
GLBUD10  SR    R0,R0                                                            
         ICM   R0,3,SVTLNT+2       GET MULT FACTOR (10000)                      
         L     R1,BDOLS                                                         
         AR    R1,R1               X 2                                          
         MR    R0,R0                                                            
         SR    RE,RE                                                            
         ICM   RE,3,SVTLNT                                                      
         DR    R0,RE                                                            
         AH    R1,=H'1'            ROUND                                        
         SRA   R1,1                                                             
         ST    R1,BDOLS                                                         
*                                                                               
GLBUDX   OI    4(R2),X'20'                                                      
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               POINTS (SKIP)                                
         IC    R0,0(R2)                                                         
         AR    R2,R0               PERIOD                                       
         MVI   5(R2),0             FORCE S-E                                    
         B     GLPER2                                                           
         EJECT                                                                  
GLPCT    CLI   BACT,C'A'                                                        
         BNE   GLPCTX                                                           
         MVI   ERRCD,MSSNGERR                                                   
         CLI   5(R2),0                                                          
         BE    GLERR                                                            
         ZIC   R5,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,(2,8(R2)),(R5)                                     
         MVI   ERRCD,INVERR                                                     
         CLI   0(R1),X'FF'                                                      
         BE    GLERR                                                            
         L     R0,4(R1)                                                         
         LTR   R0,R0                                                            
         BZ    GLERR                                                            
         CH    R0,=H'10000'                                                     
         BH    GLERR                                                            
         ST    R0,BDOLS                                                         
*                                                                               
GLPCTX   OI    4(R2),X'20'                                                      
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               POINTS (SKIP)                                
         B     GLPER                                                            
         EJECT                                                                  
GLPER    SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         CLI   5(R2),0                                                          
         BNE   GLPER2                                                           
         OC    BWEEKS,BWEEKS                                                    
         BNZ   GLPERX                                                           
         MVI   ERRCD,MSSNGERR                                                   
         B     GLERR                                                            
*                                                                               
GLPER2   DS    0H                                                               
         GOTO1 USER3               'EDTPER'                                     
*                                                                               
         MVI   BPERSW,0                                                         
         CLC   =C'S-E',8(R2)       TEST PERIOD IS S-E                           
         BNE   *+8                                                              
         MVI   BPERSW,C'E'                                                      
*                                                                               
GLPERX   OI    4(R2),X'20'                                                      
         SPACE 2                                                                
         CLI   BACT,C'*'           TEST NO ACTN THIS LINE                       
         BE    GLNEXT                                                           
         EJECT                                                                  
* BUILD GOAL KEY                                                                
GLKEY    DS    0H                                                               
*                                                                               
         LA    RE,GOALREC          SET RECORD ADDRESS                           
         ST    RE,AREC                                                          
*                                                                               
         XC    GKEY,GKEY                                                        
         MVI   GKEYTYPE,2                                                       
         MVC   GKEYAM,SVAGYMD                                                   
         MVC   GKEYCLT,SVCLT                                                    
         MVC   GKEYPRD,SVPRD                                                    
         MVC   GKEYMKT,BMKT                                                     
         MVC   GKEYEST,SVEST                                                    
         MVC   GKEYDPT,BDPT                                                     
         MVC   GKEYSLN,BSLN                                                     
         MVC   GKEYSEC,BSLN                                                     
*                                                                               
GLKEY2   CLI   SVSCRN,X'FB'        TEST BRDPCT                                  
         BNE   *+8                                                              
         MVI   GKEYAGY,X'40'       GKEYPRD2 ISN'T A PRD                         
         CLI   BACT,C'A'                                                        
         BE    GLADD                                                            
         CLI   BACT,C'E'                                                        
         BE    GLADD                                                            
         CLI   BACT,C'D'                                                        
         BE    GLDEL                                                            
         DC    H'0'                                                             
*                                                                               
GLKEY4   MVC   KEY,GKEY                                                         
         MVI   ERRCD,NODTAERR                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   GLEXPERR                                                         
         GOTO1 GETREC                                                           
         B     GLX                                                              
         EJECT                                                                  
GLADD    OI    DMINBTS,X'08'       PASS DELETES                                 
*                                                                               
         CLI   SVSCRN,X'FB'        TEST BRDPCT                                  
         BNE   GLADD1                                                           
         OC    GOLTG+2(2),GOLTG+2  ELSE USE GOLTG                               
         BNZ   *+12                                                             
         MVI   ERRCD,INVERR                                                     
         B     GLERR                                                            
*                                                                               
         PACK  DUB,GOLTG+2(2)                                                   
         CVB   R1,DUB                                                           
         STCM  R1,1,GKEYPRD2                                                    
GLADD1   MVC   KEY(13),GKEY                                                     
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BNE   GLADD2              BUILD NEW REC                                
*                                                                               
* ALWAYS RE-READ SPTFILE REC TO REBUILD GETREC TABLE                            
*                                                                               
         GOTO1 GETREC                                                           
         BAS   RE,CHKELDT                                                       
*                                                                               
         B     GLADD4                                                           
         EJECT                                                                  
         SPACE 2                                                                
* BUILD NEW RECORD                                                              
*                                                                               
GLADD2   CLI   SVPRD,X'FF'         TEST ADDING CPP GUIDE                        
         BNE   GLADD3                                                           
*                                                                               
         MVI   ERRCD,CPPDTERR                                                   
         CLI   BPERSW,C'E'         TEST ENTIRE ESTIMATE PERIOD                  
         BNE   GLERR                                                            
*                                                                               
GLADD3   LA    R0,GOALREC                                                       
         LHI   R1,REC2-REC                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   GKEY(13),KEYSAVE                                                 
         MVC   GLENGTH,=H'100'                                                  
         MVC   GAGYALPH,AGYALPHA                                                
         MVC   GDELEM(2),=X'204C'                                               
         MVC   GBUYNAME,GOLPLNR                                                 
         SR    R0,R0                                                            
         IC    R0,SVADVAGY                                                      
         SRL   R0,4                RIGHT ALIGN ADVTSR CODE                      
         STC   R0,GADVAGY                                                       
*                                                                               
GLADD4   LA    R8,BWEEKS                                                        
*                                                                               
GLADD6   XC    ELEM,ELEM                                                        
         CLI   SVSCRN,X'FC'        TEST BRDDOL                                  
         BNE   GLADD8              NO                                           
         MVC   ELEM(2),=X'4010'    BRDDOL ELEM                                  
         MVC   ELEM+2(2),0(R8)                                                  
* FIND LAST WEEK                                                                
         LR    RE,R8                                                            
         CLI   2(RE),0                                                          
         BE    *+12                                                             
         LA    RE,2(RE)                                                         
         B     *-12                                                             
         MVC   ELEM+4(2),0(RE)                                                  
*                                                                               
         MVC   ELEM+6(2),BTIER     SET TIER NUMBER                              
         MVC   ELEM+8(4),BDOLS                                                  
         MVC   ELEM+12(4),BPTS     SAVE UNADJUSTED DOLLARS                      
         B     GLADD10                                                          
*                                                                               
GLADD8   MVC   ELEM(2),=X'410C'    BRDPCT ELEM                                  
         MVC   ELEM+2(2),0(R8)                                                  
         MVC   ELEM+4(4),BDOLS                                                  
         EJECT                                                                  
* NOW REMOVE SIMILAR ELEMENTS FROM RECORD                                       
*                                                                               
GLADD10  XC    SVTIERS(SVTIERX-SVTIERS),SVTIERS                                 
         MVC   SVTIERS(2),BTIER    SAVE NEW TIER VALUES                         
         MVC   SVTIERS+2(4),BDOLS                                               
         LA    R6,GDELEM                                                        
*                                                                               
GLADD12  MVC   ELCODE,ELEM                                                      
*                                                                               
         BAS   RE,NEXTEL                                                        
         BNE   GLADD18                                                          
*                                                                               
GLADD14  CLC   2(2,R6),0(R8)       TEST SAME WEEK                               
         BL    GLADD12                                                          
         BH    GLADD18                                                          
*                                                                               
         CLI   SVSCRN,X'FC'        TEST BRDDOL                                  
         BNE   GLADD16                                                          
         CLI   BTIER,X'F0'         TEST ABS OLD                                 
         BL    GLADD16                                                          
         CLC   BTIER,6(R6)         TEST SAME TIER                               
         BE    GLADD16                                                          
         BAS   RE,SVTIER                                                        
         B     GLADD12             GET NEXT                                     
*                                                                               
* DELETE EXISTING ELEMENT                                                       
*                                                                               
GLADD16  GOTO1 VRECUP,DMCB,GOALREC,(R6),0                                       
         BAS   RE,NEXTEL2          MORE ELEMS?                                  
         BE    GLADD14                                                          
*                                                                               
* ADD NEW ELEMENT                                                               
*                                                                               
GLADD18  GOTO1 VRECUP,DMCB,GOALREC,ELEM,(R6)                                    
*                                                                               
         LA    R8,2(R8)            NEXT WEEK                                    
         CLI   0(R8),0                                                          
         BE    GLSET                                                            
         CLI   SVSCRN,X'FC'        TEST BRDDOL                                  
         BE    GLSET                                                            
         BNE   GLADD6                                                           
         EJECT                                                                  
* SET ACTIVITY DATE IN REC                                                      
*                                                                               
GLSET    DS    0H                                                               
         GOTO1 VDATCON,DMCB,(5,0),(2,GACTDATE)                                  
*                                                                               
         OC    GREDATE,GREDATE                                                  
         BNZ   *+10                                                             
         MVC   GREDATE,GACTDATE                                                 
*                                                                               
         TM    GCNTRLS,X'80'       TEST REC IS DELETED                          
         BZ    GLADD30             NO                                           
*                                                                               
         NI    GCNTRLS,X'7F'       RESET 'DELETE'                               
         GOTO1 PUTREC                                                           
*                                                                               
* UNSET DIRECTORY CONTROL BIT TOO                                               
*                                                                               
         NI    KEY+13,X'7F'                                                     
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 DIR                                                              
*                                                                               
         B     GLX                                                              
         SPACE 2                                                                
GLADD30  CLC   KEY(13),KEYSAVE     TEST REC ON FILE                             
         BE    GLADD40             YES                                          
*                                                                               
         GOTO1 ADDREC                                                           
*                                                                               
         B     GLX                                                              
*                                                                               
GLADD40  GOTO1 PUTREC                                                           
*                                                                               
         B     GLX                                                              
         EJECT                                                                  
GLDEL    DS    0H                                                               
         CLI   SVSCRN,X'FB'        TEST BRDPCT                                  
         BNE   GLDEL1                                                           
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(NOTBPBD)                                             
         OC    GOLTG+2(2),GOLTG+2  AND USE GOLTG                                
         BZ    GLERR                                                            
*                                                                               
         PACK  DUB,GOLTG+2(2)                                                   
         CVB   R1,DUB                                                           
         STC   R1,GKEYPRD2                                                      
*                                                                               
GLDEL1   MVC   KEY(13),GKEY                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+12                                                             
         MVI   ERRCD,DELERR                                                     
         B     GLERR                                                            
*                                                                               
GLDEL2   LA    RE,GOALREC                                                       
         ST    RE,AREC                                                          
         GOTO1 GETREC                                                           
         BAS   RE,CHKELDT                                                       
* SET ACTIVITY DATE                                                             
         GOTO1 VDATCON,DMCB,(5,0),(2,GACTDATE)                                  
*                                                                               
         CLI   SVSCRN,X'FC'        TEST BRDDOLS                                 
         BE    GLDEL30                                                          
*                                                                               
GLDEL4   LA    R8,BWEEKS                                                        
*                                                                               
GLDEL6   MVI   ELCODE,X'41'                                                     
         LA    R6,GDELEM                                                        
*                                                                               
         CLC   =C'S-',8(R2)        IF PERIOD INPUT IS 'S-', MISSING             
         BE    GLDEL16              FIRST ELEMENT IS OK                         
*                                                                               
GLDEL10  BAS   RE,NEXTEL                                                        
         BNE   GLDEL14                                                          
*                                                                               
GLDEL12  CLI   BPERSW,C'E'         TEST ENTIRE ESTIMATE PERIOD                  
         BE    GLDEL18             YES - SKIP DATE TEST                         
         CLC   2(2,R6),0(R8)                                                    
         BL    GLDEL10                                                          
         BE    GLDEL18                                                          
*                                                                               
GLDEL14  MVI   ERRCD,NOWKERR                                                    
         B     GLERR                                                            
*                                                                               
GLDEL16  BAS   RE,NEXTEL                                                        
         BNE   GLDEL20                                                          
*                                                                               
GLDEL17  CLI   BPERSW,C'E'         TEST ENTIRE ESTIMATE PERIOD                  
         BE    GLDEL18             YES - SKIP DATE TEST                         
         CLC   2(2,R6),0(R8)                                                    
         BL    GLDEL16                                                          
         BH    GLDEL20                                                          
GLDEL18  DS    0H                                                               
         GOTO1 VRECUP,DMCB,GOALREC,(R6),0                                       
         BAS   RE,NEXTEL2                                                       
         BE    GLDEL17                                                          
*                                                                               
GLDEL20  LA    R8,2(R8)            NEXT WEEK                                    
         CLI   0(R8),0                                                          
         BE    GLDEL40                                                          
         LA    R6,GDELEM                                                        
         B     GLDEL16                                                          
         EJECT                                                                  
*-------------------------------------------------------------*                 
* BRDDOLS - DELETE ELEMENT FOR MATCHING TIER                  *                 
* AND SAVE REMAINING TIER VALUES IN SVTIERS TABLE             *                 
*-------------------------------------------------------------*                 
         SPACE 1                                                                
GLDEL30  DS    0H                                                               
         XC    SVTIERS(SVTIERX-SVTIERS),SVTIERS                                 
         LA    R6,GDELEM                                                        
         MVI   ELCODE,X'40'                                                     
*                                                                               
GLDEL32  BAS   RE,NEXTEL                                                        
         BNE   GLDEL40                                                          
*                                                                               
GLDEL34  CLC   BTIER,6(R6)                                                      
         BE    GLDEL36                                                          
         BAS   RE,SVTIER                                                        
         B     GLDEL32                                                          
*                                                                               
GLDEL36  DS    0H                                                               
         GOTO1 VRECUP,DMCB,GOALREC,(R6),0                                       
         BAS   RE,NEXTEL2                                                       
         BE    GLDEL34                                                          
*                                                                               
GLDEL40  DS    0H                  TEST ANY ELEMENTS LEFT                       
         LA    R6,GDELEM                                                        
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   GLDEL50                                                          
* DELETE RECORD                                                                 
         OI    GCNTRLS,X'80'                                                    
         GOTO1 PUTREC                                                           
         OI    KEY+13,X'80'                                                     
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 DIR                                                              
*                                                                               
GLDEL50  B     GLSET                                                            
         EJECT                                                                  
GLX      DS    0H                                                               
         L     R4,BLNADDR                                                       
         MVI   8(R4),C'*'          INDICATE ACTION COMPLETED                    
         FOUT  (R4)                                                             
         CLI   SVSCRN,X'FC'        TEST BRDDOL                                  
         BE    GLX2                                                             
* DO NOT OVERWRITE TIER IF BRDDOL                                               
         MVC   9(1,R4),BACT                                                     
         MVI   10(R4),C' '                                                      
         B     GLREQX              NO MORE TO DO FOR BRDPCT                     
*                                                                               
GLX2     LA    R7,SVTIERS          POINT TO FIRST ENTRY IN TIER LIST            
         OC    0(6,R7),0(R7)       TEST ANY TIERS LEFT                          
         BZ    GLX9                NO - JUST DELETE ALL GOAL ELEMENTS           
*                                                                               
* NOW READ DEFAULT BRAND PERCENTAGES                                            
*                                                                               
GLX4     XC    KEY,KEY                                                          
         MVI   KEY,2                                                            
         MVC   KEY+1(1),SVAGYMD                                                 
         MVC   KEY+2(2),SVCLT                                                   
         MVC   KEY+4(1),SVPRD                                                   
         MVC   KEY+5(2),=H'9999'                                                
         MVC   KEY+7(1),SVEST                                                   
         MVI   KEY+8,C'Z'                                                       
         MVC   KEY+9(1),BSLN                                                    
         MVC   KEY+10(1),BSLN                                                   
         MVI   KEY+11,X'40'        GKEYPRD2 ISN'T A PRD                         
         PACK  DUB,0(2,R7)         PACK TIER NUMBER                             
         CVB   R0,DUB                                                           
         STC   R0,KEY+12                                                        
*                                                                               
         GOTO1 HIGH                                                             
         MVI   ERRCD,NOPCT999                                                   
         CLC   KEY(13),KEYSAVE                                                  
         BNE   GLEXPERR                                                         
*                                                                               
         MVC   WORK(20),KEY        SAVE KEY                                     
* NOW TRY FOR SPECIFIC BRDPCT                                                   
         MVC   KEY+5(2),BMKT                                                    
         MVI   KEY+11,X'40'        GKEYPRD2 ISN'T A PRD                         
         MVC   KEY+12(1),WORK+12   TIER #                                       
*                                                                               
GLX8     NI    DMINBTS,X'F7'       DONT WANT DELETES HERE STUPID                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     TEST FOUND SPECIFIC                          
         BNE   GLX8A                                                            
* READ SPECIFIC AND USE IT IF IT HAS ANY X'41' ELEMENTS                         
         LA    RE,REC2                                                          
         ST    RE,AREC                                                          
         GOTO1 GETREC                                                           
         LA    R6,REC2+GDELEM-GKEY                                              
         MVI   ELCODE,X'41'                                                     
         BAS   RE,NEXTEL                                                        
         BE    GLX8C                                                            
*                                                                               
GLX8A    MVC   KEY(20),WORK        NO - USE DEFAULT                             
*                                                                               
GLX8B    LA    RE,REC2             SET I/O ADDRESS                              
         ST    RE,AREC                                                          
         GOTO1 GETREC                                                           
*                                                                               
GLX8C    LA    RE,GOALREC          RESTORE I/O ADDRESS                          
         ST    RE,AREC                                                          
* CHECK SUM OF PCT ELEMS                                                        
         MVI   ERRCD,NOSUM100                                                   
         LA    R6,REC2+GDELEM-GKEY                                              
         MVI   ELCODE,X'41'                                                     
         SR    R3,R3                                                            
         BAS   RE,NEXTEL                                                        
         BNE   *+12                                                             
         A     R3,4(R6)                                                         
         B     *-12                                                             
         CH    R3,=H'10000'                                                     
         BNE   GLEXPERR                                                         
         EJECT                                                                  
*--------------------------------------------------------------------*          
* READ SUB-EST GOALRECS IN AND EXPLODE                               *          
*--------------------------------------------------------------------*          
         SPACE 1                                                                
GLX9     OI    DMINBTS,X'08'       PASS DELETES                                 
         LA    R4,SVSUBEST                                                      
*                                                                               
GLX10    MVC   KEY,GKEY                                                         
         MVC   KEY+7(1),0(R4)      MOVE SUB-EST TO KEY                          
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    GLX14                                                            
* BUILD NEW REC                                                                 
         LA    R0,GOALREC                                                       
         LHI   R1,REC2-REC                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   GKEY(13),KEYSAVE                                                 
         MVC   GLENGTH,=H'100'                                                  
         MVC   GAGYALPH,AGYALPHA                                                
         MVC   GDELEM(2),=X'204C'                                               
         MVC   GBUYNAME(5),=C'GLEXP'                                            
         B     GLX20                                                            
*                                                                               
GLX14    DS    0H                                                               
         GOTO1 GETREC                                                           
* DELETE ALL X'21' ELEMENTS FOR FIRST TIER ONLY                                 
         LA    R0,SVTIERS                                                       
         CR    R0,R7               TEST FIRST TIER                              
         BNE   GLX20                                                            
*                                                                               
         MVI   ELCODE,X'21'                                                     
GLX16    LA    R6,GDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BNE   GLX19                                                            
GLX18    DS    0H                                                               
         GOTO1 VRECUP,DMCB,GOALREC,(R6),0                                       
         BAS   RE,NEXTEL2                                                       
         BE    GLX18                                                            
*                                                                               
GLX19    OC    0(6,R7),0(R7)       TEST ANY TIERS AT ALL                        
         BZ    GLX30               NO - GO DELETE RECORD                        
         EJECT                                                                  
GLX20    LA    R6,REC2+GDELEM-GKEY                                              
         MVI   ELCODE,X'41'        LOOK FOR PERCENT ELEMENTS                    
*                                                                               
GLX22    BAS   RE,NEXTEL                                                        
         BNE   GLX30                                                            
*                                                                               
         CLC   2(2,R6),1(R4)       PCT EL TO SUBEST START                       
         BL    GLX22               PRIOR                                        
         CLC   2(2,R6),3(R4)       PCT EL TO SUBEST END                         
         BH    GLX30                                                            
* CREATE GOAL ELEM                                                              
         XC    ELEM,ELEM                                                        
         MVC   ELEM(2),=X'210C'                                                 
         MVC   ELEM+2(2),2(R6)                                                  
         L     R1,4(R6)                                                         
         ICM   R0,15,2(R7)         GET DOLLARS FOR THIS TIER                    
         MR    R0,R0                                                            
         SLDA  R0,1                                                             
         D     R0,=F'1000000'                                                   
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         MH    R1,=H'100'          GIVES ROUNDED DOLLARS IN PENNIES             
         ST    R1,ELEM+8                                                        
* SEE IF ELEMENT IS IN RECORD - IF NOT ADD IT. IF YES, ADD TO DOLLARS           
         LA    R3,GDELEM                                                        
GLX24    ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),0                                                          
         BE    GLX26                                                            
         CLC   ELEM(4),0(R3)       MATCH ELCODE/LENGTH/DATE                     
         BNE   GLX24                                                            
* GOT A MATCH                                                                   
         ICM   R0,15,8(R3)         ADD DOLLARS TO EXISTING ELEMENT              
         A     R0,ELEM+8                                                        
         STCM  R0,15,8(R3)                                                      
         B     GLX22                                                            
*                                                                               
GLX26    DS    0H                  ADD NEW ELEMENT                              
         GOTO1 VRECUP,DMCB,GOALREC,ELEM,(R3)                                    
         B     GLX22               NEXT PCTEL                                   
         EJECT                                                                  
GLX30    DS    0H                  SET ACTIVITY DATE                            
         GOTO1 VDATCON,DMCB,(5,0),(2,GACTDATE)                                  
         OC    GREDATE,GREDATE                                                  
         BNZ   *+10                                                             
         MVC   GREDATE,GACTDATE                                                 
         LA    R6,GDELEM                                                        
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0             TEST ANY ELEMENTS                            
         BE    GLX38               NO                                           
*                                                                               
         CLC   KEY(13),KEYSAVE     TEST REC ON FILE                             
         BE    GLX32               YES                                          
         GOTO1 ADDREC                                                           
         B     GLX42                                                            
*                                                                               
GLX32    NI    GCNTRLS,X'7F'       UNSET DELETE IF ON                           
         GOTO1 PUTREC                                                           
*                                                                               
         TM    KEY+13,X'80'        TEST DELETED                                 
         BZ    GLX42                                                            
         NI    KEY+13,X'7F'                                                     
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 DIR                                                              
         B     GLX42                                                            
*                                                                               
GLX38    CLC   KEY(13),KEYSAVE     NO ELEMS - TEST REC ON FILE                  
         BNE   GLX42               NO - SKIP                                    
         TM    GCNTRLS,X'80'       TEST ALREADY DELETED                         
         BO    GLX40                                                            
         OI    GCNTRLS,X'80'       SET REC DELETED                              
         GOTO1 PUTREC                                                           
*                                                                               
GLX40    TM    KEY+13,X'80'        TEST KEY ALREADY DELETED                     
         BO    GLX42                                                            
         OI    KEY+13,X'80'        SET KEY DELETED                              
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 DIR                                                              
*                                                                               
GLX42    LA    R4,5(R4)            NEXT SUB-EST                                 
         CLI   0(R4),0                                                          
         BNE   GLX10                                                            
*                                                                               
         LA    R0,SVTIERX                                                       
         CR    R0,R7               DID WE DO LAST TIER                          
         BE    GLREQ               YES                                          
         LA    R7,L'SVTIERS(R7)    BUMP TO NEXT TIER                            
         OC    0(L'SVTIERS,R7),0(R7)     TEST MORE TIERS                        
         BNZ   GLX4                      YES - GO PROCESS                       
         EJECT                                                                  
*---------------------------------------------------------------------*         
* GENERATE REQUEST FOR BRAND MEDIA PLAN                               *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
GLREQ    CLC   =C'SPOT',GOLPLNR                                                 
         BE    GLREQX                                                           
         CLC   =C'TEST',GOLPLNR                                                 
         BE    GLREQX                                                           
*                                                                               
         MVC   SVMKT,BMKT                                                       
         XC    ELEM,ELEM           CLEAR BUILD AREA                             
         LA    R4,ELEM+26                                                       
         MVC   0(80,R4),SPACES                                                  
         MVC   0(2,R4),=C'U9'                                                   
         MVC   2(2,R4),AGYALPHA                                                 
         MVC   4(1,R4),GOLMD                                                    
         MVC   5(3,R4),GOLCL                                                    
         OC    5(3,R4),SPACES                                                   
         MVC   11(3,R4),QPRD                                                    
         MVI   62(R4),C'G'         RECAP OPTION                                 
*                                                                               
         LH    R0,BMKT                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  14(4,R4),DUB                                                     
*                                                                               
         CLC   =C'BM ',QCLT        SO DOES BM                                   
         BE    GLREQ10                                                          
         MVC   14(4,R4),=CL4'ALL'  EVERYONE ELSE DOES ALL                       
*                                                                               
GLREQ10  MVC   23(2,R4),=C'NO'     EVERYONE DOES ALL ESTIMATES, BUT             
         CLC   =C'DF',AGYALPHA     FOR DANCER, DO 1 ESTIMATE                    
         BNE   GLREQ20                                                          
         SPACE 1                                                                
         CLI   SVMSTR,C'S'         IF SUB EST, JUST REQUEST THAT EST.           
         BE    GLREQ16         IF MASTER EST, REQ RANGE OF SUB ESTS.            
         SPACE 1                                                                
         LA    R5,SVSUBEST         LIST OF SUB ESTS. FOR THIS MASTER            
         SR    R0,R0               CONVERT FIRST SUB EST FOR REQ. CARD          
         IC    R0,0(R5)                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  23(3,R4),DUB                                                     
         SPACE 1                                                                
         CLI   5(R5),0          IF ONLY ONE SUB EST, DON'T NEED RANGE           
         BE    GLREQ18                                                          
         SPACE                                                                  
GLREQ12  LA    R5,5(R5)            NOW FIND LAST SUB EST. IN RANGE              
         CLI   5(R5),0                                                          
         BNE   GLREQ12                                                          
         SPACE 1                                                                
         IC    R0,0(R5)            FOUND LAST SUB EST.                          
         CVD   R0,DUB              CONVERT FOR REQUEST CARD                     
         OI    DUB+7,X'0F'                                                      
         UNPK  26(3,R4),DUB                                                     
         B     GLREQ18                                                          
         SPACE 1                                                                
GLREQ16  SR    R0,R0               CONVERT SUB EST. FOR REQUEST CARD            
         IC    R0,SVEST                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  23(3,R4),DUB                                                     
         SPACE 1                                                                
GLREQ18  MVI   62(R4),C'C'         RECAP OPTION FOR DANCER (LIKE M9)            
         SPACE 1                                                                
GLREQ20  MVC   37(12,R4),SVSTART                                                
         MVC   68(12,R4),GOLPLNR                                                
         OC    68(12,R4),SPACES                                                 
*                                                                               
         MVI   ELEM+10,0                                                        
         MVI   ELEM+14,106                                                      
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMADD',=C'REQUEST',ELEM,ELEM                    
*                                                                               
GLREQX   DS    0H                                                               
         EJECT                                                                  
GLNEXT   SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    EXXMOD                                                           
* CHECK FOR NO INPUT FIELDS ON NEXT LINE                                        
         LA    RE,7                                                             
         LR    R4,R2                                                            
*                                                                               
GLN2     TM    1(R4),X'20'         TEST PROTECTED                               
         BO    GLN4                                                             
         CLI   5(R4),0                                                          
         BNE   GL2                 EDIT NEXT LINE                               
*                                                                               
GLN4     IC    R0,0(R4)                                                         
         AR    R4,R0                                                            
         BCT   RE,GLN2                                                          
*                                                                               
* CLEAR PROTECTED FIELDS ON REMAINING LINES                                     
*                                                                               
GLN10    TM    1(R2),X'20'         TEST PROTECTED                               
         BZ    GLN12               NO                                           
         SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         SH    R5,=H'9'                                                         
         EX    R5,*+8                                                           
         B     *+10                                                             
         OC    8(0,R2),8(R2)  ** EXECUTED **                                    
*                                                                               
         BZ    GLN12                                                            
         EX    R5,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)  ** EXECUTED **                                    
*                                                                               
         FOUT  (R2)                                                             
*                                                                               
GLN12    IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNE   GLN10                                                            
         B     EXXMOD                                                           
         EJECT                                                                  
NEXTEL   SR    R0,R0                                                            
         CLI   0(R6),0             TEST E-O-R                                   
         BE    NEXTELX                                                          
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
NEXTEL2  CLC   ELCODE,0(R6)                                                     
         BNE   NEXTEL                                                           
         BR    RE                                                               
NEXTELX  LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                               
TSTNUM   MVI   ERRCD,INVERR                                                     
TSTNUM2  CLI   0(R1),C'0'                                                       
         BL    GLERR                                                            
         CLI   0(R1),C'9'                                                       
         BH    GLERR                                                            
         LA    R1,1(R1)                                                         
         BCT   R0,TSTNUM2                                                       
         BR    RE                                                               
*                                                                               
GLEXPERR L     R4,BLNADDR                                                       
         OI    6(R4),X'40'         POSITION CURSOR                              
*                                                                               
GLERR    GOTO1 ERROR                                                            
*                                                                               
EXXMOD   XMOD1 1                                                                
         EJECT                                                                  
* THIS ROUTINE CHECKS THAT FIRST ELEMENT IS EST START DATE OR                   
* MONDAY AND CHANGES DATE IF NECESSARY.                                         
*                                                                               
CHKELDT  NTR1                                                                   
*                                                                               
         LA    R6,GDELEM                                                        
CHKEL2   ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    CHKELX                                                           
         CLI   0(R6),X'21'                                                      
         BNE   CHKEL2                                                           
* GET 6 BYTE YYMMDD                                                             
         GOTO1 VDATCON,DMCB,(2,2(R6)),WORK                                      
*                                                                               
         CLC   SVSTART,WORK        TEST EST START DATE                          
         BE    CHKELX                                                           
*                                                                               
         GOTO1 VGETDAY,DMCB,WORK,WORK+6                                         
*                                                                               
         CLI   0(R1),1             TEST MONDAY                                  
         BE    CHKELX                                                           
* BACK UP TO PREVIOUS MONDAY OR ESTART                                          
         ZIC   R0,0(R1)                                                         
         BCTR  R0,0                                                             
         LCR   R0,R0                                                            
         GOTO1 VADDAY,DMCB,WORK,WORK+6,(R0)                                     
*                                                                               
         CLC   SVSTART,WORK+6                                                   
         BL    *+10                                                             
         MVC   WORK+6(6),SVSTART                                                
*                                                                               
         GOTO1 VDATCON,DMCB,WORK+6,(2,2(R6))                                    
*                                                                               
CHKELX   XIT1                                                                   
         SPACE 2                                                                
*----------------------------------------------------------------*              
* SAVE TIER AND DOLLARS IN TABLE                                 *              
* ON ENTRY R6 POINTS TO A X'40' ELEMENT                          *              
*----------------------------------------------------------------*              
         SPACE 1                                                                
SVTIER   DS    0H                                                               
         LA    R1,SVTIERS                                                       
         LA    R0,SVTIERN                                                       
*                                                                               
SVTIER2  OC    0(6,R1),0(R1)       SLOT FREE                                    
         BZ    SVTIER4                                                          
         LA    R1,L'SVTIERS(R1)                                                 
         BCT   R0,SVTIER2                                                       
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(TIERMAX)                                             
         B     GLERR                                                            
*                                                                               
SVTIER4  MVC   0(2,R1),6(R6)       SAVE TIER ID                                 
         MVC   2(4,R1),8(R6)       SAVE BUDGET DOLLARS                          
         BR    RE                                                               
         EJECT                                                                  
TALRECD  DSECT                                                                  
*SPGENTAL                                                                       
       ++INCLUDE SPGENTAL                                                       
         PRINT OFF                                                              
       ++INCLUDE SPGOLWRK                                                       
 END                                                                            
