*          DATA SET SPWRI30    AT LEVEL 001 AS OF 12/16/04                      
*PHASE T20430A,*                                                                
*                                                                               
*********************************************************************           
*                                                                   *           
*          SPWRI30 (T20430) - PW RECORD LIST                        *           
*                                                                   *           
*-------------------------------------------------------------------*           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* 02SEP98 01 EFJ -- INITIAL DEVELOPMENT                             *           
*                                                                   *           
*********************************************************************           
         TITLE 'T20430 - SPOTPAK WRITER PW RECORD LIST'                         
T20430   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKL,T20430**,RA,RR=R2                                          
         LR    R5,RC                                                            
         USING WORKD,R5                                                         
         ST    R2,RELO                                                          
         MVC   SAVERD,4(RD)        SAVE A(CALLING PROGRAM'S SAVE AREA)          
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         ST    RC,AGEND                                                         
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
*                                  REPORT CALLING MODE                          
         CLI   RPMODE,RPINIT       INITIALIZATION                               
         BE    INIT                                                             
         CLI   RPMODE,RPVAL        VALIDATION                                   
         BE    VALID                                                            
         CLI   RPMODE,RPINPUT      INPUT                                        
         BE    INPUT                                                            
         CLI   RPMODE,RPDRHOOK     DRIVER HOOK                                  
         BE    DRHOOK                                                           
         B     XIT                                                              
*                                                                               
EQXIT    CR    RB,RB                                                            
         B     XIT                                                              
*                                                                               
NEQXIT   LTR   RB,RB                                                            
XIT      XIT1  ,                                                                
         EJECT                                                                  
* INITIALIZATION                                                                
*                                                                               
INIT     DS    0H                                                               
         OI    SBQSKIP,SBQSKBUY+SBQSKGL+SBQSKBIL                                
         LR    RF,R9                                                            
         AH    RF,=Y(SBQREAD2-SYSD)                                             
         OI    0(RF),SBQRD2PW                                                   
         OI    SBIOFLAG,SBRDPWS                                                 
         OI    SBQPER,SBQPMN+SBQPWK                                             
         MVI   SBQPERLO,1                                                       
         MVI   SBQPERHI,X'FF'                                                   
*                                                                               
         CLI   RPTSCRN,0           TEST USER REPORT SCREEN                      
         BE    INITX                NO                                          
*                                                                               
         MVI   MYFIRSTH,12                                                      
         MVI   WIDTHOPT,C'W'       WIDE PRINTING (165)                          
*                                                                               
INITX    B     XIT                                                              
         SPACE 2                                                                
         EJECT                                                                  
* FURTHER REQUEST VALIDATION                                                    
*                                                                               
VALID    DS    0H                                                               
VALX     B     XIT                                                              
         EJECT                                                                  
INPUT    L     R4,AGLOBAL          ** INPUT ROUTINE **                          
         USING GLOBALD,R4                                                       
         CLI   SBMODE,SBPROCWP     HOOK MODE PASSED FROM SPOTIO                 
         BE    PROCWP                                                           
         B     XIT                                                              
         SPACE 2                                                                
* DRIVER INPUT ROUTINE                                                          
*                                                                               
DRIVIN   LR    R0,RE                                                            
         MVI   GLMODE,GLINPUT                                                   
         BAS   RE,GODRIVER                                                      
*                                                                               
DRIVINX  LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
PROCWP   DS    0H                                                               
         L     R2,SBAIO1                                                        
         USING PWRECD,R2                                                        
         LR    R6,R2                                                            
         MVI   ELCODE,PWDOLCDQ                                                  
         BAS   RE,GETEL                                                         
         BNE   WIPWX                                                            
*                                                                               
WIPW10   ST    R6,SBACURCH                                                      
         BAS   RE,DRIVIN           CALL DRIVER FOR INPUT                        
         BAS   RE,NEXTEL                                                        
         BE    WIPW10                                                           
*                                                                               
WIPWX    B     EQXIT                                                            
         DROP  R2,R4                                                            
         EJECT                                                                  
* DRIVER HOOK                                                                   
*                                                                               
DRHOOK   L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         CLI   GLHOOK,GLINIT       INITIALIZATION                               
         BE    DRVINIT                                                          
         CLI   GLHOOK,GLRESOLV     RESOLVE ADDRESSES                            
         BE    RESOLVE                                                          
         CLI   GLHOOK,GLROUT       EXECUTE ROUTINES                             
         BE    EXEC                                                             
         CLI   GLHOOK,GLINCOMP     INTERNAL COMPUTES                            
         BE    INTCOMP                                                          
         CLI   GLHOOK,GLPRINT      PRINT A LINE                                 
         BE    PRINT                                                            
         CLI   GLHOOK,GLPUTSRT     PUT A RECORD TO SORT                         
         BE    PUTSRT                                                           
         CLI   GLHOOK,GLFIRST      FIRSTS                                       
         BE    FIRSTS                                                           
         CLI   GLHOOK,GLLAST       LASTS                                        
         BE    LASTS                                                            
         CLI   GLHOOK,GLHEAD       HEADHOOK                                     
         BE    HEAD                                                             
*                                                                               
DRHOOKX  B     XIT                                                              
         EJECT                                                                  
* DRIVER INITIALIZATION                                                         
*                                                                               
DRVINIT  DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
* DRIVER HOOK ROUTINE TO RESOLVE ADDRESSES                                      
*                                                                               
RESOLVE  LA    R1,ROUTLIST         SEARCH LIST FOR ROUTINE NAME                 
*                                                                               
RESOLVE2 CLI   0(R1),FF                                                         
         BE    XIT                                                              
         CLC   0(8,R1),GLLABEL                                                  
         BE    *+12                                                             
         LA    R1,12(R1)                                                        
         B     RESOLVE2                                                         
         MVC   GLAROUT,8(R1)       YES-RETURN ADDRESS                           
         B     XIT                                                              
         SPACE 1                                                                
ROUTLIST DS    0F                                                               
         DC    CL8'IPWDOLSP',A(IPWDOLSP)                                        
         DC    CL8'IPWDOLWG',A(IPWDOLWG)                                        
         DC    CL8'IPWDOLWN',A(IPWDOLWN)                                        
         DC    CL8'IPWDOLTX',A(IPWDOLTX)                                        
         DC    CL8'IPWDOLCG',A(IPWDOLCG)                                        
         DC    CL8'IPWDOLCN',A(IPWDOLCN)                                        
         DC    CL8'IPWDOLCT',A(IPWDOLCT)                                        
         DC    CL8'ODOLOUT ',A(DOLOUT)                                          
         DC    CL8'OSPOTS  ',A(OSPOTS)                                          
         DC    CL8'OMKT    ',A(OMKT)                                            
         DC    X'FF'                                                            
         EJECT                                                                  
* INTERNAL COMPUTES HOOK                                                        
*                                                                               
INTCOMP  DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
* PRINT A LINE                                                                  
*                                                                               
PRINT    DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
* DRIVER HOOK FIRSTS                                                            
*                                                                               
FIRSTS   DS    0H                                                               
         CLI   GLARGS,0            TEST LEVEL 0 BREAK                           
         BNE   FIRSTX                                                           
         MVC   TITLE,BLANKS        YES-SET THE APPROPRIATE TITLE                
         MVC   TITLE(14),=C'PW RECORD LIST'                                     
         LA    R2,S49TITH          OVERRIDE TITLE?                              
         CLI   5(R2),0                                                          
         BE    FIRSTC                                                           
         GOTO1 ANY                                                              
         MVC   TITLE,WORK                                                       
FIRSTC   GOTO1 CENTER,DMCB,TITLE,63                                             
FIRSTX   B     XIT                                                              
         SPACE 2                                                                
         EJECT                                                                  
* DRIVER HOOK PUTSRT                                                            
*                                                                               
PUTSRT   DS    0H                                                               
PUTSRTX  B     XIT                                                              
         EJECT                                                                  
* DRIVER HOOK LASTS                                                             
*                                                                               
LASTS    DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
* DRIVER HEADHOOK                                                               
*                                                                               
HEAD     DS    0H                                                               
HDX      B     XIT                                                              
         SPACE 1                                                                
         EJECT                                                                  
* DRIVER HOOK TO EXECUTE ROUTINES                                               
*                                                                               
EXEC     DS    0H                                                               
         L     R2,GLAIFLD          R2=A(INPUT)                                  
         L     R3,GLAOFLD          R3=A(OUTPUT)                                 
         L     RF,GLAROUT          BRANCH TO ROUTINE                            
*                                                                               
         L     R6,SBACURCH                                                      
         USING PWDOLEL,R6                                                       
         BR    RF                                                               
*                                                                               
EXECIX   B     XIT                                                              
EXECIXX  B     XIT                                                              
*                                                                               
EXECOX   B     XIT                                                              
         EJECT                                                                  
* DRIVER INPUT/OUTPUT ROUTINES                                                  
*                                                                               
*                                                                               
IPWDOLSP DS    0H                                                               
         L     RF,PWDOLSPT                                                      
         B     PACKDOL                                                          
*                                                                               
IPWDOLWG DS    0H                                                               
         L     RF,PWDOLWG                                                       
         B     PACKDOL                                                          
*                                                                               
IPWDOLWN DS    0H                                                               
         L     RF,PWDOLWN                                                       
         B     PACKDOL                                                          
*                                                                               
IPWDOLTX DS    0H                                                               
         L     RF,PWDOLTAX                                                      
         B     PACKDOL                                                          
*                                                                               
IPWDOLCG DS    0H                                                               
         L     RF,PWDOLCG                                                       
         B     PACKDOL                                                          
*                                                                               
IPWDOLCN DS    0H                                                               
         L     RF,PWDOLCN                                                       
         B     PACKDOL                                                          
*                                                                               
IPWDOLCT DS    0H                                                               
         L     RF,PWDOLCTX                                                      
         B     PACKDOL                                                          
*                                                                               
         DROP  R6                                                               
*                                                                               
OSPOTS   DS    0H                                                               
         CP    0(8,R2),8(8,R2)     STATION TOTAL = MARKET TOTAL?                
         BE    DOLOUT10             YES                                         
         EDIT  (P8,0(R2)),(11,0(R3)),TRAIL=C'*'                                 
         EDIT  (P8,8(R2)),(11,198(R3)),TRAIL=C'*'                               
         B     EXECOX                                                           
*                                                                               
DOLOUT   DS    0H                                                               
         CP    0(8,R2),8(8,R2)     STATION TOTAL = MARKET TOTAL?                
         BE    DOLOUT10             YES                                         
         EDIT  (P8,0(R2)),(11,0(R3)),2,TRAIL=C'*'                               
         EDIT  (P8,8(R2)),(11,198(R3)),2,TRAIL=C'*'                             
         B     EXECOX                                                           
*                                                                               
DOLOUT10 MVI   GLHOOK,GLEDIT       MKT = STA, LET DRIVER EDIT                   
         B     EXECOX                                                           
*                                                                               
*                                                                               
* INPUT: RF = DOLLARS                                                           
*                                                                               
*                                                                               
PACKDOL  CVD   RF,DUB                                                           
         L     RF,SBAIO1                                                        
         USING PWRECD,RF                                                        
         OC    PWKSTA,PWKSTA       IS THIS A STATION LEVEL REC?                 
         BNZ   PACK10               YES                                         
         DROP  RF                                                               
         ZAP   0(8,R2),DUB          NO - MKT REC                                
         B     PACKX                                                            
*                                                                               
PACK10   ZAP   8(8,R2),DUB                                                      
*                                                                               
PACKX    B     EXECIX                                                           
*                                                                               
         EJECT                                                                  
*                                                                               
OMKT     DS    0H                                                               
         MVC   SBBMKT,0(R2)                                                     
         EDIT  SBBMKT,(4,SBMKT),FILL=0     SET MARKET DETAILS                   
         GOTO1 GETMKTNM                                                         
         MVC   0(4,R3),SBMKT                                                    
         MVC   5(L'SBMKTNM,R3),SBMKTNM                                          
         OC    0(29,R3),BLANKS                                                  
         B     EXECOX                                                           
*                                                                               
         EJECT                                                                  
*                                                                               
* ROUTINE TO HOOK TO CALLING PROGRAM TO CALL DRIVER                             
*                                                                               
GODRIVER NTR1                                                                   
         L     RF,ADRIVER                                                       
         L     RE,SAVERD                                                        
         LM    R0,RC,20(RE)                                                     
         BASR  RE,RF                                                            
         XIT1                      DO NOT CHANGE THIS!!!                        
         EJECT                                                                  
*                                                                               
         GETEL (R6),24,ELCODE                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
* WORKING STORAGE                                                               
*                                                                               
         SPACE 1                                                                
*                                                                               
BLANKS   DC    CL132' '                                                         
XFF      DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'                           
FF       EQU   X'FF'                                                            
*                                                                               
*                                                                               
WORKD    DSECT                                                                  
*                                                                               
SAVERD   DS    A                                                                
RELO     DS    A                                                                
AGEND    DS    A                                                                
*                                                                               
WORKL    EQU   *-WORKD                                                          
*                                                                               
         EJECT                                                                  
* OTHER DSECTS ARE HIDDEN IN HERE                                               
*                                                                               
*SPWRIWORKD                                                                     
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDBIGBOX                                                                       
*DRGLOBAL                                                                       
*DRIVETABLE                                                                     
*DRINTRECD                                                                      
*SPGENAGY                                                                       
*SPGENBUY                                                                       
*SPGENEST                                                                       
*SPGENBILL                                                                      
*SPGENWIPW                                                                      
*SPWRIFFD                                                                       
         PRINT OFF                                                              
       ++INCLUDE SPWRIWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE DRIVETABLE                                                     
       ++INCLUDE DRINTRECD2                                                     
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
BILLHDRD DSECT                                                                  
       ++INCLUDE SPGENBILL                                                      
       ++INCLUDE SPGENWIPW                                                      
       ++INCLUDE SPWRIFFD                                                       
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE SPWRIF1D                                                       
         EJECT                                                                  
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE SPWRIB5D                                                       
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDPERVALD                                                      
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001SPWRI30   12/16/04'                                      
         END                                                                    
