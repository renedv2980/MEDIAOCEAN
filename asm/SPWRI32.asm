*          DATA SET SPWRI32    AT LEVEL 004 AS OF 12/16/04                      
*PHASE T20432A,*                                                                
*INCLUDE FINDOUT                                                                
*                                                                               
*********************************************************************           
*                                                                   *           
*          SPWRI32 (T20432) - SAATCHI RETAIL EXPENDITURE REPORT     *           
*                                                                   *           
*-------------------------------------------------------------------*           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* 19MAR02 04 EFJ -- SUPPORT BILLFORM OPTION                         *           
*                -- SET EST OPTIONS                                 *           
* 14MAR02 03 EFJ -- PRINT DETAIL/SUMMARY ON HEADLINES               *           
* 11FEB02 02 EFJ -- DON'T DO COVAIL CALL ONLINE!                    *           
*                -- ADD COVAIL FREE CALL                            *           
* 22JAN02 01 EFJ -- INITIAL ENTRY                                   *           
*                                                                   *           
*                                                                   *           
*********************************************************************           
         TITLE 'T20432 - RETAIL EXPENDITURE REPORT'                             
T20432   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKL,T20432**,RR=R2                                             
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
*                                                                               
         L     RA,=A(GDAREA)                                                    
         USING GDSECT,RA                                                        
*                                  REPORT CALLING MODE                          
         CLI   RPMODE,RPINIT       INITIALIZATION                               
         BE    INIT                                                             
         CLI   RPMODE,RPVAL        VALIDATION                                   
         BE    VALID                                                            
         CLI   RPMODE,RPINPUT      INPUT                                        
         BE    INPUT                                                            
         CLI   RPMODE,RPDRHOOK     DRIVER HOOK                                  
         BE    DRHOOK                                                           
         CLI   RPMODE,RPFINAL                                                   
         BE    FINAL                                                            
         J     XIT                                                              
*                                                                               
EQXIT    CR    RB,RB                                                            
         J     XIT                                                              
*                                                                               
NEQXIT   LTR   RB,RB                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
* INITIALIZATION                                                                
*                                                                               
INIT     DS    0H                                                               
         OI    SBQSKIP,SBQSKGL                                                  
         OI    SBQPER,SBQPMN                                                    
         MVI   SBQPERLO,1                                                       
         MVI   SBQPERHI,X'FF'                                                   
         OI    SBQPIND,SBQPOLES                                                 
         MVI   SBQSEPES,C'Y'                                                    
         OI    SBEUDEF,SBEUPRD1+SBEUEST2                                        
         OI    DATAIND7,DIRTLSCH                                                
*                                                                               
         CLI   RPTSCRN,0           TEST USER REPORT SCREEN                      
         BE    INITX                NO                                          
*                                                                               
         LA    R2,REXDSBH                                                       
         GOTO1 ANY                                                              
         CLI   WORK,C'D'                                                        
         BE    INIT10                                                           
         CLI   WORK,C'S'                                                        
         BE    INIT10                                                           
         CLI   WORK,C'B'                                                        
         BE    INIT10                                                           
         MVI   ERROR,INVALID                                                    
         GOTO1 CURSERR                                                          
*                                                                               
INIT10   MVC   SUMOPT,WORK                                                      
         LA    R2,REXBFH                                                        
         GOTO1 ANY                                                              
         CLI   WORK,C'Y'                                                        
         BE    INIT20                                                           
         CLI   WORK,C'N'                                                        
         BE    INIT20                                                           
         MVI   ERROR,INVALID                                                    
         GOTO1 CURSERR                                                          
*                                                                               
INIT20   MVC   BFOPT,WORK                                                       
         MVI   MYFIRSTH,13                                                      
         MVI   WIDTHOPT,C'W'       WIDE PRINTING (165)                          
*                                                                               
* GET STORAGE FOR FINDOUT TABLES                                                
         CLI   OFFLINE,C'Y'        TEST OFFLINE                                 
         BNE   INITX                NO                                          
         GOTO1 COVAIL,DMCB,C'GET',560000,560000    560K                         
         ICM   RE,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RE,AFOTABS                                                       
*                                                                               
INITX    J     XIT                                                              
         SPACE 2                                                                
         EJECT                                                                  
* FURTHER REQUEST VALIDATION                                                    
*                                                                               
VALID    DS    0H                                                               
VALX     J     XIT                                                              
         EJECT                                                                  
* FINAL CALL (END OF PREP)                                                      
*                                                                               
FINAL    L     R3,AFOTABS          FREE UP FINDOUT TABLE                        
         L     R5,=F'560000'                                                    
         GOTO1 COVAIL,DMCB,C'FREE',(R3),(R5)                                    
FINX     J     XIT                                                              
         EJECT                                                                  
*                                                                               
INPUT    L     R4,AGLOBAL          ** INPUT ROUTINE **                          
         USING GLOBALD,R4                                                       
         CLI   SBMODE,SBPROCCL     CLIENT FIRST                                 
         BE    CLIENT                                                           
         CLI   SBMODE,SBPROCMK     MARKET FIRST                                 
         BE    MARKET                                                           
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* CLIENT FIRST                                                        *         
***********************************************************************         
         SPACE 1                                                                
CLIENT   XC    WORK,WORK           GET B9 PROFILE                               
         MVC   WORK(4),=C'S0B9'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),SBQMED                                                 
         MVC   WORK+7(3),SBCLT                                                  
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SBCOFF                                                
         GOTO1 GETPROF,DMCB,WORK,B9PROF,DATAMGR                                 
*                                                                               
         MVC   WORK+2(2),=C'F0'    F0 PROFILE                                   
         GOTO1 (RF),(R1),WORK,F0PROF,DATAMGR                                    
*                                                                               
         MVC   WORK(4),=C'SB9A'                                                 
         NI    WORK,X'BF'                                                       
         GOTO1 (RF),(R1),WORK,B9APROF                                           
*                                                                               
         LA    R0,GDSECT           INITIALIZE FINDOUT DSECT                     
         LHI   R1,GDSECTL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   GDCOMP(3),B9COMP    COMPANY/UNIT/LEDGER                          
         MVC   GDMGRLEN,B9MGRLEN   MARKET GROUP LENGTH                          
         CLI   B9PROF+14,C'*'      TEST PSEUDO MGR (ACCT PREFIX)                
         BNH   *+8                                                              
         MVI   GDMGRLEN,1                                                       
         CLI   B9MKTS,C'Y'         BY MARKET?                                   
         BNE   *+8                                                              
         MVI   GDMKTLEN,4          YES-MARKET LENGTH = 4                        
         MVC   GDOUTLEN,B9OUTLET   OUTLET CODE LENGTH                           
         MVC   GDACOMS,SBCOMFAC                                                 
         MVC   GDATABS,AFOTABS                                                  
         L     RF,TWAMASTC                                                      
         MVC   GDAUTL,MCUTL-MASTD(RF) A(UTL)                                    
         MVC   GDACCSYS,MCS2SENO-MASTD(RF) ACCOUNTIMG SYSTEM SE NUM             
         CLI   B9APROF+8,0                                                      
         BE    *+10                                                             
         MVC   GDACCSYS,B9APROF+8                                               
*                                                                               
CLTX     J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* MARKET FIRST                                                        *         
***********************************************************************         
         SPACE 1                                                                
MARKET   CLI   GDMGRLEN,0          NEED TO SET MARKET GROUP ?                   
         BE    MKT2                                                             
         MVC   GDMGR,BLANKS        YES-                                         
         CLC   SBBMGR,=X'9999'     UNKNOWN MARKET GROUP                         
         BE    MKT2                                                             
         UNPK  DUB(5),SBBMGR(3)                                                 
         ZIC   RF,GDMGRLEN         N'REQUIRED MKT GRP DIGITS                    
         ZIC   RE,SBMGR3LN                                                      
         SR    RE,RF                                                            
         BNM   *+6                                                              
         SR    RE,RE                                                            
         LA    RE,DUB(RE)          USE LAST N POSITIONS                         
         BCTR  RF,0                                                             
         EX    RF,*+4              MOVE MARKET GROUP TO FINDOUT DSECT           
         MVC   GDMGR(0),0(RE)                                                   
*                                                                               
MKT2     MVC   GDSCHM,BLANKS       CLEAR SCHEME CODE                            
         CLI   GDMKTLEN,0          MIGHT NEED MARKET                            
         BE    *+10                                                             
         MVC   GDMKT,SBMKT                                                      
         MVI   GDOUTNO,0           FIRST TIME FOR FINDOUT                       
         GOTO1 =V(FINDOUT),DMCB,GDSECT                                          
*                                                                               
MKTX     J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DRIVER ROUTINES                                                     *         
***********************************************************************         
         SPACE 1                                                                
* DRIVER INPUT ROUTINE                                                          
*                                                                               
DRIVIN   LR    R0,RE                                                            
         MVI   GLMODE,GLINPUT                                                   
         BAS   RE,GODRIVER                                                      
*                                                                               
DRIVINX  LR    RE,R0                                                            
         BR    RE                                                               
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
DRHOOKX  J     XIT                                                              
         EJECT                                                                  
* DRIVER INITIALIZATION                                                         
*                                                                               
DRVINIT  MVC   GLOPTS+2(1),SUMOPT SUMMARY/DETAIL/BOTH                           
         MVC   GLOPTS+3(1),BFOPT   BILLFORM                                     
         J     XIT                                                              
         EJECT                                                                  
* DRIVER HOOK ROUTINE TO RESOLVE ADDRESSES                                      
*                                                                               
RESOLVE  LA    R1,ROUTLIST         SEARCH LIST FOR ROUTINE NAME                 
*                                                                               
RESOLVE2 CLI   0(R1),FF                                                         
         JE    XIT                                                              
         CLC   0(8,R1),GLLABEL                                                  
         BE    *+12                                                             
         LA    R1,12(R1)                                                        
         B     RESOLVE2                                                         
         MVC   GLAROUT,8(R1)       YES-RETURN ADDRESS                           
         J     XIT                                                              
         SPACE 1                                                                
ROUTLIST DS    0F                                                               
         DC    CL8'OMKT    ',A(OMKT)                                            
         DC    CL8'ICOUNT  ',A(ICOUNT)                                          
         DC    CL8'OCOUNT  ',A(OCOUNT)                                          
         DC    CL8'IASSPCT ',A(IASSPCT)                                         
         DC    CL8'OASSPCT ',A(OASSPCT)                                         
         DC    CL8'ILEXPCT ',A(ILEXPCT)                                         
         DC    CL8'OLEXPCT ',A(OLEXPCT)                                         
         DC    CL8'HASS    ',A(HASS)                                            
         DC    CL8'HLEX    ',A(HLEX)                                            
         DC    X'FF'                                                            
         EJECT                                                                  
* INTERNAL COMPUTES HOOK                                                        
*                                                                               
INTCOMP  DS    0H                                                               
         J     XIT                                                              
         EJECT                                                                  
* PRINT A LINE                                                                  
*                                                                               
PRINT    DS    0H                                                               
         J     XIT                                                              
         EJECT                                                                  
* DRIVER HOOK FIRSTS                                                            
*                                                                               
FIRSTS   DS    0H                                                               
         CLI   GLARGS,0            TEST LEVEL 0 BREAK                           
         BNE   FIRSTX                                                           
         MVC   TITLE,BLANKS        YES-SET THE APPROPRIATE TITLE                
*                                                                               
         XR    R1,R1               OVERRIDE TITLE?                              
         ICM   R1,1,REXTITH+5                                                   
         BZ    FIRST10                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     FIRST20                                                          
         MVC   TITLE(0),REXTIT                                                  
*                                                                               
FIRST10  MVC   TITLE(25),=C'RETAIL EXPENDITURE REPORT'                          
         LHI   R1,24                                                            
*                                                                               
FIRST20  LA    R1,TITLE(R1)                                                     
         AHI   R1,1                                                             
         CLI   GLRECNO,1                                                        
         BNE   *+14                                                             
         MVC   0(9,R1),=C' - DETAIL'                                            
         B     FIRSTC                                                           
         CLI   GLRECNO,2                                                        
         BNE   *+10                                                             
         MVC   0(10,R1),=C' - SUMMARY'                                          
*                                                                               
FIRSTC   GOTO1 CENTER,DMCB,TITLE,63                                             
*                                                                               
FIRSTX   J     XIT                                                              
         SPACE 2                                                                
         EJECT                                                                  
* DRIVER HOOK PUTSRT                                                            
*                                                                               
PUTSRT   J     XIT                                                              
         EJECT                                                                  
* DRIVER HOOK LASTS                                                             
*                                                                               
LASTS    J     XIT                                                              
         EJECT                                                                  
* DRIVER HEADHOOK                                                               
*                                                                               
HEAD     J     XIT                                                              
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
         BR    RF                                                               
         DC    H'0'                                                             
*                                                                               
EXECX    J     XIT                                                              
*                                                                               
         EJECT                                                                  
* DRIVER INPUT/OUTPUT ROUTINES                                                  
*                                                                               
ICOUNT   MVI   3(R2),1                                                          
         J     XIT                                                              
*                                                                               
OCOUNT   MVC   RECCNT,0(R2)                                                     
         SR    R0,R0                                                            
         L     R1,ASSPCT                                                        
         D     R0,RECCNT                                                        
         ST    R1,ASSPCT                                                        
         SR    R0,R0                                                            
         L     R1,LEXPCT                                                        
         D     R0,RECCNT                                                        
         ST    R1,LEXPCT                                                        
         J     XIT                                                              
*                                                                               
OMKT     DS    0H                                                               
         MVC   SBBMKT,0(R2)                                                     
         EDIT  SBBMKT,(4,SBMKT),FILL=0     SET MARKET DETAILS                   
         GOTO1 GETMKTNM                                                         
         MVC   0(4,R3),SBMKT                                                    
         MVC   5(L'SBMKTNM,R3),SBMKTNM                                          
         OC    0(29,R3),BLANKS                                                  
         J     EXECX                                                            
*                                                                               
HLEX     MVC   0(11,R3),=C'LEXUS SHARE'                                         
         EDIT  LEXPCT,(6,12(R3)),2,TRAIL=C'%'                                   
         J     EXECX                                                            
*                                                                               
HASS     MVC   0(11,R3),=C'ASSOC SHARE'                                         
         EDIT  ASSPCT,(6,12(R3)),2,TRAIL=C'%'                                   
         J     EXECX                                                            
*                                                                               
ILEXPCT  MVI   GDOUTNO,1                                                        
         BRAS  RE,CALLFIND                                                      
         MVC   0(4,R2),GDSHRP                                                   
         J     EXECX                                                            
*                                                                               
OLEXPCT  MVC   LEXPCT,0(R2)                                                     
         J     EXECX                                                            
*                                                                               
IASSPCT  MVI   GDOUTNO,2                                                        
         BRAS  RE,CALLFIND                                                      
         MVC   0(4,R2),GDSHRP                                                   
         J     EXECX                                                            
*                                                                               
OASSPCT  MVC   ASSPCT,0(R2)                                                     
         J     EXECX                                                            
*                                                                               
CALLFIND LR    R0,RE                                                            
         MVC   GDSCHM,SBRTLSCH                                                  
         CLI   GDSCHM,C'*'                                                      
         JNE   *+10                                                             
         MVC   GDSCHM,BLANKS                                                    
         MVC   GDPRD,SBBPRD                                                     
         MVC   GDEST,SBBEST                                                     
         GOTO1 =V(FINDOUT),DMCB,GDAREA                                          
CFX      LR    RE,R0                                                            
         BR    RE                                                               
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
SUMOPT   DS    C                   SUMMRAY OPTION (D/S/B)                       
BFOPT    DS    C                   BILLFORM OPTION (Y/N)                        
ASSPCT   DS    F                                                                
LEXPCT   DS    F                                                                
RECCNT   DS    F                                                                
*                                                                               
B9PROF   DS    0CL16                                                            
B9COMP   DS    CL1                                                              
B9UNIT   DS    CL1                                                              
B9LEDGER DS    CL1                                                              
B9MGRLEN DS    CL1                                                              
B9MKTS   DS    CL1                                                              
B9OUTLET DS    CL1                                                              
         DS    CL4                                                              
B9MEDCAL DS    CL1                                                              
         DS    CL5                                                              
*                                                                               
B9APROF  DS    CL16                                                             
*                                                                               
F0PROF   DS    0CL16                                                            
         DS    CL1                                                              
F0POLSCM DS    CL1                                                              
         DS    CL14                                                             
*                                                                               
AFOTABS  DS    F                                                                
*                                                                               
GDAREA   DS    (GDSECTL)C          FINDOUT WORK AREA                            
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
*SPRTLBLK                                                                       
*SPWRIFFD                                                                       
         PRINT OFF                                                              
       ++INCLUDE SPWRIWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE DRIVETABLE                                                     
       ++INCLUDE DRINTRECD2                                                     
       ++INCLUDE DDMASTD                                                        
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
BILLHDRD DSECT                                                                  
       ++INCLUDE SPGENBILL                                                      
       ++INCLUDE SPRTLBLK                                                       
       ++INCLUDE SPWRIFFD                                                       
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE SPWRIF1D                                                       
         EJECT                                                                  
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE SPWRIB6D                                                       
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDTWADCOND                                                     
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004SPWRI32   12/16/04'                                      
         END                                                                    
