*          DATA SET SPWRI0A    AT LEVEL 009 AS OF 12/15/04                      
*PHASE T2040AA,*                                                                
*                                                                               
*********************************************************************           
*                                                                   *           
*          SPWRI0A (T2040A) - FORMAT LIST                           *           
*                                                                   *           
*-------------------------------------------------------------------*           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* 09NOV98 01 EFJ -- INITIAL DEVELOPMENT                             *           
*                                                                   *           
*********************************************************************           
         TITLE 'T2040A - SPOTPAK WRITER FORMAT'                                 
T2040A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKL,T2040A**,RR=R2                                             
         LR    R7,RC                                                            
         USING WORKD,R7                                                         
         ST    R2,RELO                                                          
         MVC   SAVERD,4(RD)        SAVE A(CALLING PROGRAM'S SAVE AREA)          
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         ST    RC,AGEND                                                         
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
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
         MVI   SBQSKIP,X'FF'       READ NO RECORDS                              
         MVI   SBQREAD,0                                                        
         LR    RF,R9                                                            
         AH    RF,=Y(SBQREAD2-SYSD)                                             
         MVI   0(RF),0                                                          
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
*                                                                               
         MVC   FILENAME,=CL8'CTFILE'                                            
         MVI   USEIO,C'Y'                                                       
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING CT01RECD,R2                                                      
         MVI   CT01TYPE,CT01TYPQ                                                
         MVC   CT01AGID,SBQAGY                                                  
         MVC   CT01SYS(3),=X'020401'                                            
         MVC   KEYSAVE,KEY                                                      
         L     R2,AIO                                                           
         GOTO1 HIGH                                                             
*                                                                               
INP20    CLC   KEYSAVE(CT01NAME-CT01TYPE),CT01KEY                               
         BNE   XIT                                                              
         BAS   RE,DRIVIN           CALL DRIVER FOR INPUT                        
         GOTO1 SEQ                                                              
         B     INP20                                                            
         DROP  R2                                                               
*                                                                               
         EJECT                                                                  
* DRIVER INPUT ROUTINE                                                          
*                                                                               
DRIVIN   LR    R0,RE                                                            
         MVI   GLMODE,GLINPUT                                                   
         BAS   RE,GODRIVER                                                      
*                                                                               
DRIVINX  LR    RE,R0                                                            
         BR    RE                                                               
         DROP  R4                                                               
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
         DC    CL8'INAME   ',A(INAME)                                           
         DC    CL8'IDESC   ',A(IDESC)                                           
         DC    CL8'IFILT   ',A(IFILT)                                           
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
         MVC   TITLE(11),=C'FORMAT LIST'                                        
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
         L     R5,AIO                                                           
         USING CT01RECD,R5                                                      
         BR    RF                                                               
*                                                                               
EXECIX   B     XIT                                                              
*                                                                               
EXECOX   B     XIT                                                              
         EJECT                                                                  
* DRIVER INPUT/OUTPUT ROUTINES                                                  
*                                                                               
*                                                                               
INAME    DS    0H                                                               
         MVC   0(8,R2),CT01NAME                                                 
         B     EXECIX                                                           
*                                                                               
IDESC    DS    0H                                                               
         LR    R6,R5                                                            
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BNE   EXECIX                                                           
         USING CT01DSCD,R6                                                      
         XR    RE,RE                                                            
         IC    RE,CT01DLEN                                                      
         AHI   RE,-3                                                            
         EX    RE,*+8                                                           
         B     EXECIX                                                           
         MVC   0(0,R2),CT01DESC                                                 
         DROP  R6                                                               
*                                                                               
IFILT    DS    0H                                                               
         LR    R6,R5                                                            
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   EXECIX                                                           
         USING CT01FLTD,R6                                                      
         MVC   0(4,R2),CT01FILT                                                 
         B     EXECIX                                                           
         DROP  R6                                                               
*                                                                               
         DROP  R5                                                               
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
         GETEL (R6),28,ELCODE                                                   
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
       ++INCLUDE CTGENPGREC                                                     
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
**PAN#1  DC    CL21'009SPWRI0A   12/15/04'                                      
         END                                                                    
