*          DATA SET CTSFM14A   AT LEVEL 135 AS OF 05/22/00                      
*PHASE TA0A14A                                                                  
*INCLUDE REGENDRB                                                               
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE: TA0A14 - DARE AGENCY ROUTING MAINT/LIST                     *         
*                                                                     *         
*  COMMENTS: MAINTAINS DARE AGENCY ROUTING RECORDS                    *         
*            3 CHAR CODE (IN KEY) IS BLANK PADDED                     *         
*            10 CHAR NAME, AND PREFIX ARE BOTH BLANK PADDED ALSO      *         
*                                                                     *         
*  CALLED FROM: SFM CONTROLLER (TA0A00), WHICH CALLS                  *         
*               DDGENCON (T00A30) WHICH CALLS THIS.                   *         
*                                                                     *         
*  CALLS TO:    DATAMGR                                               *         
*                                                                     *         
*  INPUTS: SCREENS CTSFMBA (TA0ABA) -- MAINTENANCE                    *         
*                  CTSFMBB (TA0ABB) -- LIST                           *         
*                                                                     *         
*  OUTPUTS: UPDATED DARE AGENCY ROUTING RECORDS                       *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R0 - WORK                                                  *         
*          R1 - WORK                                                  *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR    *         
*          R3 - GETEL REGISTER                                        *         
*          R4 - WORK                                                  *         
*          R5 - WORK                                                  *         
*          R6 - WORK                                                  *         
*          R7 - 2ND BASE                                              *         
*          R8 - SPOOLD                                                *         
*          R9 - SYSD                                                  *         
*          RA - TWA                                                   *         
*          RB - FIRST BASE                                            *         
*          RC - GEND                                                  *         
*          RD - SYSTEM                                                *         
*          RE - SYSTEM                                                *         
*          RF - SYSTEM                                                *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
         TITLE 'TA0A14 DARE AGENCY ROUTING RECORDS'                             
TA0A14   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*TA0A14*                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,LISTRECS       ONLINE LIST RECORDS                          
         BE    LR                                                               
*        CLI   MODE,PRINTREP                                                    
*        BE    PR                                                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
VK       NTR1                                                                   
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING AGRKEYD,R4                                                       
*                                                                               
         CLI   ACTNUM,ACTLIST      SKIP TESTS IF LIST                           
         BNE   *+22                                                             
         OC    SFLMEDA,SFLMEDA                                                  
         BNZ   VK50                                                             
         LA    R2,SFMMEDAH                                                      
         B     VK20                                                             
*                                                                               
         XC    REP,REP          CLEAR REP CODE FOR REGENDRB                     
         LA    R2,SFMREP1H                                                      
         LA    R5,REPENTRY                                                      
VK2      GOTO1 =V(REGENDRB),DMCB,(C'T',REP),(X'80',REPENTRY),          *        
               ACOMFACS,RR=YES                                                  
*                                                                               
         TM    8(R1),X'80'      IF NO RECORD FOUND, END LOOP                    
         BNZ   VK5                                                              
         TM    13(R5),X'40'     IS THIS A TEST REP?                             
         BZ    VK2A               NO, PROCESS                                   
         TM    8(R1),X'40'        YES, CHECK "NO MORE RECORDS" FLAG             
         BZ    VK2                IF FLAG OFF, REPEAT LOOP                      
         B     VK5                ELSE, END LOOP                                
*                                                                               
VK2A     MVC   8(10,R2),3(R5)           MOVE IN REP NAME                        
         OI    6(R2),X'80'              TRANSMIT                                
         LA    R2,SFMREP2H-SFMREP1H(R2) POINT TO NEXT SCREEN FIELD              
         TM    8(R1),X'40'              CHECK "NO MORE RECORDS" FLAG            
         BNZ   VK5                      IF ON, END LOOP                         
         LA    R6,SFMLRPLH              ELSE, CHECK FIELD POINTER               
         CR    R2,R6                       FOR VALID POSITION                   
         BNH   VK2                         AND REPEAT LOOP IF OKAY              
*                                                                               
VK5      LA    R2,SFMOVR1H-SFMREP1H(R2)                                         
         LA    R6,SFMLOVLH                                                      
VK6      CR    R2,R6                                                            
         BH    VK8                                                              
         OI    1(R2),X'20'                                                      
         OI    6(R2),X'80'                                                      
         LA    R2,SFMOVR2H-SFMOVR1H(R2)                                         
         B     VK6                                                              
*                                                                               
VK8      LA    R2,SFMTRP1H                                                      
         XC    REP,REP                                                          
*                                                                               
VK10     GOTO1 =V(REGENDRB),DMCB,(C'T',REP),(X'80',REPENTRY),          *        
               ACOMFACS,RR=YES                                                  
         TM    8(R1),X'80'              IF NO RECORD FOUND,                     
         BNZ   VK15                     END LOOP                                
         TM    13(R5),X'40'                                                     
         BNZ   VK12                                                             
         TM    8(R1),X'40'                                                      
         BZ    VK10                                                             
         B     VK15                                                             
VK12     MVC   8(10,R2),3(R5)                                                   
         OI    6(R2),X'80'                                                      
         LA    R2,SFMREP2H-SFMREP1H(R2)                                         
         TM    8(R1),X'40'                                                      
         BNZ   VK15                                                             
         LA    R6,SFMREPLH                                                      
         CR    R2,R6                                                            
         BNH   VK10                                                             
*                                                                               
VK15     LA    R2,SFMOVR1H-SFMREP1H(R2)                                         
         LA    R6,SFMOVRLH                                                      
VK16     CR    R2,R6                                                            
         BH    VK18                                                             
         OI    1(R2),X'20'                                                      
         OI    6(R2),X'80'                                                      
         LA    R2,SFMOVR2H-SFMOVR1H(R2)                                         
         B     VK16                                                             
*                                                                               
VK18     LA    R2,SFMMEDAH                                                      
         CLI   5(R2),1             IS THE MEDIA 1 CHAR?                         
         BNE   VK20                                                             
         CLI   8(R2),C'T'          IS THE MEDIA T?                              
         BE    VK30                                                             
*                                                                               
VK20     MVC   GERROR,=AL2(300)                                                 
         B     SFMERROR                                                         
*                                                                               
VK30     LA    R2,SFMAGRTH                                                      
         CLI   5(R2),5             IS THE CODE 5 CHARS?                         
         BNE   VK40                                                             
         CLI   SFMAGRT+2,C' '      3RD CHAR SHLD BE A BLANK                     
         BE    VK50                                                             
*                                                                               
VK40     MVC   GERROR,=AL2(2)                                                   
         B     SFMERROR                                                         
*                                                                               
VK50     MVI   AGRKSYS,AGRKSYSQ    BUILD KEY FOR MAINT...                       
         MVI   AGRKTYP,AGRKTYPQ                                                 
         MVC   AGRKMEDA,SFMMEDA                                                 
         MVC   AGRKAGRT,SFMAGRT                                                 
         CLI   ACTNUM,ACTLIST                                                   
         BNE   EXIT                                                             
*                                                                               
         MVC   AGRKMEDA,SFLMEDA    ...OR FOR LIST                               
         OC    SFLAGRT,SFLAGRT                                                  
         BZ    EXIT                                                             
         MVC   AGRKAGRT,SFLAGRT                                                 
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
VR       NTR1                                                                   
         L     R4,AIO                                                           
         USING AGRKEYD,R4                                                       
         MVI   ELCODE,AGROVRCQ     IS THERE AN AGY RT ELEMT?                    
         GOTO1 REMELEM                                                          
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING AGROVRD,R3          AGY RTING  ELEMENT DSECT                     
         MVI   AGROVRC,AGROVRCQ                                                 
         MVI   AGROVRLN,AGROVRLQ                                                
*                                                                               
         LA    R2,SFMOVR1H                                                      
         LA    R6,SFMTOR1H                                                      
         XC    REP,REP                                                          
         LA    R5,REPENTRY         DARE REP ENTRY FIELD                         
VR10     GOTO1 =V(REGENDRB),PARAS,(C'T',REP),(X'80',REPENTRY),         *        
               ACOMFACS,RR=YES                                                  
         TM    PARAS+8,X'80'      IF NO RECORD FOUND, END LOOP                  
         BNZ   VR100                                                            
         TM    13(R5),X'40'                                                     
         BNZ   VR50                                                             
         LA    R0,SFMLRPLH                                                      
         CR    R2,R0               ...CHECK FOR END OF SCREEN                   
         BH    VR100                                                            
*                                                                               
VR30     CLI   8(R2),C'A'          IF NO OVERRIDE                               
         BNL   VR35                                                             
         LA    R2,SFMOVR2H-SFMOVR1H(R2)                                         
         TM    PARAS+8,X'40'       WAS THIS THE LAST REP?                       
         BNZ   VR100               THEN END                                     
         B     VR10                THEN THERE SHLDN'T BE AN ELEMENT             
*                                                                               
VR35     CLI   5(R2),2                                                          
         BNE   VR45                                                             
         CLC   SFMAGRT+3(2),8(R2)  OVRIDE & RTING CODE SHLD BE DIFF             
         BE    VRERR                                                            
*                                                                               
VR45     MVC   AGROVRCR,0(R5)                                                   
         MVC   AGROVROF,8(R2)      BUILD OFFICE OVERRIDE OF ELEMENT             
         B     VR65                                                             
*                                                                               
VRERR    MVC   GERROR,=AL2(305)                                                 
         B     SFMERROR                                                         
*                                                                               
VR50     LA    R0,SFMREPLH                                                      
         CR    R6,R0                                                            
         BH    VR100                                                            
*                                                                               
VR55     CLI   8(R6),C'A'                                                       
         BNL   VR58                                                             
         LA    R6,SFMOVR2H-SFMOVR1H(R6)                                         
         TM    PARAS+8,X'40'       WAS THIS THE LAST REP?                       
         BNZ   VR100               THEN END                                     
         B     VR10                                                             
*                                                                               
VR58     CLI   5(R6),2                                                          
         BE    *+10                                                             
         LR    R2,R6                                                            
         B     VRERR                                                            
         CLC   SFMAGRT+3(2),8(R6)                                               
         BNE   VR60                                                             
         LR    R2,R6                                                            
         B     VRERR                                                            
*                                                                               
VR60     MVC   AGROVRCR,0(R5)      BUILD REP PART OF ELEMENT                    
         MVC   AGROVROF,8(R6)      BUILD OFFICE OVERRIDE OF ELEMENT             
*                                                                               
VR65     GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    13(R5),X'40'                                                     
         BZ    *+12                                                             
         LA    R6,SFMOVR2H-SFMOVR1H(R6)                                         
         B     *+8                                                              
         LA    R2,SFMOVR2H-SFMOVR1H(R2)   PT R2 TO NEXT FIELD                   
         TM    PARAS+8,X'40'       WAS THIS THE LAST REP?                       
         BNZ   VR100               THEN END                                     
         B     VR10                                                             
*                                                                               
VR100    B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
DR       NTR1                                                                   
         L     R4,AIO              CHECK KEY SAME AS RECORD                     
         USING AGRKEYD,R4                                                       
         TWAXC SFMREP1H                                                         
*                                                                               
         CLC   KEY(AGRKLENQ),AGRKEY                                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,AIO                                                           
         USING AGROVRD,R3                                                       
         MVI   ELCODE,AGROVRCQ                                                  
         BAS   RE,GETEL                                                         
         BNE   EXIT                GET OUT IF NO MORE ELEMENTS                  
*                                                                               
DR5      XC    TEMP,TEMP                                                        
         LA    R5,REPENTRY                                                      
DR10     GOTO1 =V(REGENDRB),DMCB,(C'T',AGROVRCR),REPENTRY,ACOMFACS,    *        
               RR=YES                                                           
         TM    8(R1),X'80'      IF NO RECORD FOUND, END LOOP                    
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    13(R5),X'40'                                                     
         BZ    DR20                LIVE                                         
         B     DR50                TEST                                         
*                                                                               
DR20     MVC   TEMP,3(R5)                                                       
*                                                                               
         LA    R0,SFMLRPLH                                                      
         LA    R2,SFMREP1H                                                      
DR30     CLC   TEMP,8(R2)                                                       
         BE    DR40                                                             
         LA    R2,SFMREP2H-SFMREP1H(R2)                                         
         CR    R2,R0                                                            
         BNH   DR30                                                             
         DC    H'0'                ELEMT W/OUT VALID CODE                       
*                                                                               
DR40     LA    R2,SFMOVR1H-SFMREP1H(R2)                                         
         MVC   8(2,R2),AGROVROF                                                 
         OI    6(R2),X'80'                                                      
         B     DR100                                                            
*                                                                               
DR50     MVC   TEMP,3(R5)                                                       
         LA    R6,SFMTRP1H                                                      
DR60     CLC   TEMP,8(R6)                                                       
         BE    DR70                                                             
         LA    R6,SFMREP2H-SFMREP1H(R6)                                         
         CLC   0(R6),SFMREPLH                                                   
         BNH   DR60                                                             
         DC    H'0'                                                             
*                                                                               
DR70     LA    R6,SFMOVR1H-SFMREP1H(R6)                                         
         MVC   8(2,R6),AGROVROF                                                 
         OI    6(R6),X'80'                                                      
*                                                                               
DR100    BAS   RE,NEXTEL                                                        
         BE    DR5                                                              
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*   LIST RECORDS                                                                
*                                                                               
LR       NTR1                                                                   
         LA    R4,KEY                                                           
         USING AGRKEYD,R4                                                       
*                                                                               
         OC    KEY(L'AGRKSYS+L'AGRKTYP),KEY                                     
         BNZ   LR10                                                             
*                                                                               
         MVI   AGRKSYS,AGRKSYSQ    BUILD SEARCH KEY USE INPUT                   
         MVI   AGRKTYP,AGRKTYPQ                                                 
         LA    R2,SFLMEDAH                                                      
         CLI   SFLMEDAH+5,1        IS THE MEDIA 1 CHAR?                         
         BNE   VK20                                                             
         CLI   SFLMEDA,C'T'        IS THE MEDIA T?                              
         BNE   VK20                                                             
         MVC   AGRKMEDA,SFLMEDA    BUILD SEARCH KEY                             
         OC    SFLAGRT,SFLAGRT                                                  
         BZ    *+10                                                             
         MVC   AGRKAGRT,SFLAGRT                                                 
         MVC   SAVEKEY,KEY                                                      
*                                                                               
LR10     GOTO1 HIGH                                                             
*                                                                               
LR20     CLC   KEY(L'AGRKSYS+L'AGRKTYP),SAVEKEY                                 
         BNE   EXIT                                                             
*                                                                               
         XC    LISTAR,LISTAR                                                    
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
*                                                                               
LR30     MVC   LSTMEDIA,AGRKMEDA                                                
         MVC   LSTAGYRT,AGRKAGRT                                                
         GOTO1 LISTMON             DISPLAY RECORD                               
*                                                                               
LR50     GOTO1 SEQ                 GET NEXT RECORD AND CHECK FOR MATCH          
         LA    R4,KEY                                                           
         B     LR20                                                             
         EJECT                                                                  
*                                                                               
* DISPLAY KEY / IN SELECT                                                       
*                                                                               
DK       NTR1                                                                   
*                                                                               
         LA    R2,SFMREP1H                                                      
         LA    R5,REPENTRY                                                      
         XC    REP,REP                                                          
DK10     GOTO1 =V(REGENDRB),DMCB,(C'T',REP),(X'80',REPENTRY),          *        
               ACOMFACS,RR=YES                                                  
         TM    8(R1),X'80'      IF NO RECORD FOUND, END LOOP                    
         BNZ   DK15                                                             
         TM    13(R5),X'40'                                                     
         BZ    DK13                                                             
         TM    8(R1),X'40'      WAS THIS LAST REP?                              
         BNZ   DK15             IF SO, END LOOP                                 
         B     DK10             ELSE, GET NEXT REP                              
DK13     MVC   8(10,R2),3(R5)                                                   
         OI    6(R2),X'80'                                                      
         LA    R2,SFMREP2H-SFMREP1H(R2)                                         
         TM    8(R1),X'40'      WAS THIS LAST REP?                              
         BZ    DK10             IF NOT, REPEAT  LOOP                            
*                                                                               
DK15     LA    R2,SFMOVR1H-SFMREP1H(R2)                                         
         LA    R6,SFMLOVLH                                                      
DK16     CR    R2,R6                                                            
         BH    DK20                                                             
         OI    1(R2),X'20'                                                      
         OI    6(R2),X'80'                                                      
         LA    R2,SFMOVR2H-SFMOVR1H(R2)                                         
         B     DK16                                                             
*                                                                               
DK20     LA    R2,SFMTRP1H                                                      
         LA    R5,REPENTRY                                                      
         XC    REP,REP                                                          
DK30     GOTO1 =V(REGENDRB),DMCB,(C'T',REP),(X'80',REPENTRY),          *        
               ACOMFACS,RR=YES                                                  
         TM    8(R1),X'80'      IF NO RECORD FOUND, END LOOP                    
         BNZ   DK35                                                             
         TM    13(R5),X'40'                                                     
         BNZ   DK33                                                             
         TM    8(R1),X'40'      WAS THIS LAST REP?                              
         BNZ   DK35             YES->END LOOP                                   
         B     DK30             NO ->REPEAT LOOP                                
*                                                                               
DK33     MVC   8(10,R2),3(R5)                                                   
         OI    6(R2),X'80'                                                      
         LA    R2,SFMREP2H-SFMREP1H(R2)                                         
         TM    8(R1),X'40'      WAS THIS LAST REP?                              
         BZ    DK30             NO -> REPEAT LOOP                               
*                                                                               
DK35     LA    R2,SFMOVR1H-SFMREP1H(R2)                                         
         LA    R6,SFMOVRLH                                                      
DK36     CR    R2,R6                                                            
         BH    DK40                                                             
         OI    1(R2),X'20'                                                      
         OI    6(R2),X'80'                                                      
         LA    R2,SFMOVR2H-SFMOVR1H(R2)                                         
         B     DK36                                                             
*                                                                               
DK40     L     R4,AIO                                                           
         USING AGRKEYD,R4                                                       
*                                                                               
         MVC   SFMMEDA,AGRKMEDA    RECORD TO SCREEN                             
         OI    SFMMEDAH+6,X'80'                                                 
         MVC   SFMAGRT,AGRKAGRT                                                 
         OI    SFMAGRTH+6,X'80'                                                 
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
SFMERROR GOTO1 SFMERR                                                           
*                                                                               
RELO     DS    A                                                                
*                                                                               
INVALCMB DC    CL60'INVALID: OVERRIDE SAME AS ROUTING CODE'                     
*                                                                               
         GETEL  R3,DATADISP,ELCODE                                              
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE CTGENAGRD                                                      
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE CTGENDEPT                                                      
         EJECT                                                                  
         PRINT ON                                                               
       ++INCLUDE CTSFMFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMBAD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMBBD                                                       
*                                                                               
*                                                                               
         EJECT                                                                  
       ++INCLUDE CTSFMWORKD                                                     
         SPACE 5                                                                
         EJECT                                                                  
* PUT MY STORAGE DSECT HERE IF NEEDED                                           
*                                                                               
         ORG   SYSSPARE                                                         
*                                                                               
SAVEKEY  DS    CL32                                                             
TEMP     DS    CL10                                                             
REP      DS    CL3                                                              
REPENTRY DS    CL25                                                             
*                                                                               
         EJECT                                                                  
* ONLINE LIST (1 LINE)                                                          
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
         DS    CL3                                                              
LSTMEDIA DS    CL1                                                              
         DS    CL9                                                              
LSTAGYRT DS    CL5                                                              
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'135CTSFM14A  05/22/00'                                      
         END                                                                    
