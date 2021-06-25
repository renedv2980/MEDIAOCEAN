*          DATA SET SPWRI17    AT LEVEL 001 AS OF 12/15/04                      
*PHASE T20417A,*                                                                
*                                                                               
*********************************************************************           
*                                                                   *           
*          SPWRI17 (T20417) - BILLED/ORD PERCENTAGE                 *           
*                                                                   *           
*-------------------------------------------------------------------*           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* 14AUG02 01 EFJ -- INITIAL DEVELOPMENT                             *           
*                                                                   *           
*********************************************************************           
         TITLE 'T20417 - SPOTPAK BILLED/ORD PERCENTAGE'                         
T20417   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKL,T20417**                                                   
         LR    R5,RC                                                            
         USING WORKD,R5                                                         
         MVC   SAVERD,4(RD)        SAVE A(CALLING PROGRAM'S SAVE AREA)          
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
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
         J     XIT                                                              
*                                                                               
NEQXIT   LTR   RB,RB                                                            
XIT      XIT1  ,                                                                
         EJECT                                                                  
* INITIALIZATION                                                                
*                                                                               
INIT     DS    0H                                                               
         OI    SBQSKIP,SBQSKBUY+SBQSKGL+SBQSKBIL                                
         OI    SBQREAD,SBQRDBH                                                  
         OI    SBQPER,SBQPMN                                                    
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
         CLI   SBMODE,SBPROCES     HOOK MODE PASSED FROM SPOTIO                 
         BE    PROCES                                                           
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
*------------------------------------------------------------------*            
* ROUTINE TO PROCESS AN ESTIMATE                                   *            
*   WARNING - RF USED FOR BCT!                                     *            
*------------------------------------------------------------------*            
PROCES   DS    0H                                                               
         L     R2,SBAIO1                                                        
         USING ESTHDRD,R2                                                       
         XC    WORK,WORK                                                        
         MVC   WORK(4),GETBROAD                                                 
         MVC   WORK+4(4),ADDAY                                                  
         MVC   WORK+8(4),GETDAY                                                 
         MVC   WORK+12(4),DATCON                                                
         L     R3,SBAIO3           GET THE ESTIMATES'S MONTHS                   
         GOTO1 MOBILE,DMCB,(12,SBESTST),(DATEFORM,(R3)),WORK,SBSPPROF           
         GOTO1 DATCON,DMCB,(2,2(R3)),(3,FULL)                                   
         ZIC   R6,FULL+1           R6=MONTH NUMBER                              
         L     R7,SBADATE          ACCUMULATE ORD DOLLARS FOR EACH              
         L     RF,SBNDATES         REQUESTED DATE PERIOD                        
*                                                                               
EST2     ZAP   DUB,=P'0'           DUB=DOLLAR ACCUMULATOR                       
*                                                                               
EST4     CLC   0(2,R7),0(R3)       TEST ESTIMATE MONTH WITHIN PERIOD            
         BH    EST6                                                             
         CLC   2(2,R7),2(R3)                                                    
         BL    EST8                                                             
         LR    RE,R6               YES-GET ORD DOLLARS FOR THIS MONTH           
         BCTR  RE,0                                                             
         MHI   RE,6                                                             
         LA    RE,EORD(RE)                                                      
         AP    DUB,0(6,RE)                                                      
*                                                                               
EST6     LA    R3,4(R3)            NEXT ESTIMATE MONTH                          
         CLI   0(R3),X'FF'                                                      
         BE    EST8                                                             
         LA    R6,1(R6)            AUGMENT MONTH NUMBER                         
         CH    R6,=H'13'                                                        
         BL    EST4                                                             
         BH    *+12                                                             
         CLI   DATEFORM,10         13TH MONTH ALLOWED FOR 4-WEEK MONTHS         
         BE    EST4                                                             
         LA    R6,1                                                             
         B     EST4                                                             
*                                                                               
EST8     CP    DUB,=P'0'           TEST ANY ORD DOLLARS                         
         BE    EST10                                                            
         L     RE,SBACHUNK         YES-                                         
         ZAP   4(8,RE),DUB         STORE ORD DOLLARS                            
         MVC   0(4,RE),0(R7)       AND PERIOD START/END                         
         BAS   RE,DRIVIN           CALL DRIVER FOR INPUT                        
*                                                                               
EST10    CLI   0(R3),X'FF'         TEST END OF ESTIMATE MONTHS                  
         BE    ESTX                                                             
         LA    R7,4(R7)            NO-NEXT PERIOD                               
         BCT   RF,EST2                                                          
*                                                                               
ESTX     J     EQXIT                                                            
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
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
         DC    CL8'IORDDOL ',A(IORDDOL)                                         
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
         MVC   TITLE(25),=C'LESS THAN 98% BILLED LIST'                          
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
         BR    RF                                                               
*                                                                               
EXECIX   J     XIT                                                              
*                                                                               
EXECOX   J     XIT                                                              
         EJECT                                                                  
* DRIVER INPUT/OUTPUT ROUTINES                                                  
*                                                                               
*                                                                               
IORDDOL  DS    0H                                                               
         CLI   SBMODE,SBPROCES                                                  
         JNE   EXECIX                                                           
         L     R4,SBACHUNK                                                      
         ZAP   0(8,R2),4(8,R4)     STORE ORD DOLLARS                            
         J     EXECIX                                                           
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
*SPGENEST                                                                       
*SPGENBILL                                                                      
*SPWRIFFD                                                                       
         PRINT OFF                                                              
       ++INCLUDE SPWRIWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE DRIVETABLE                                                     
       ++INCLUDE DRINTRECD2                                                     
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
BILLHDRD DSECT                                                                  
       ++INCLUDE SPGENBILL                                                      
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
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001SPWRI17   12/15/04'                                      
         END                                                                    
