*          DATA SET REBAS03    AT LEVEL 127 AS OF 05/01/02                      
*PHASE T82403A,*                                                                
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE: T82403 - REP FILE STATION COMPETITIVE RECORD                *         
*                                                                     *         
*  CALLED FROM: REP CONTROLLER (T82400)                               *         
*                                                                     *         
*  INPUTS: SCREENS REBASB2  (T824B2) -- MAINTENANCE                   *         
*          SCREENS SPSFMD2  (T824D2) -- LIST                          *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R0 - WORK                                                  *         
*          R1 - WORK                                                  *         
*          R2 - POINTER TO SCREEN FLDH                                *         
*          R3 - WORK                                                  *         
*          R4 - STATION RECORD                                        *         
*          R5 - WORK                                                  *         
*          R6 - GETEL REGISTER                                        *         
*          R7 - SECOND BASE REGISTER                                  *         
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
*                                                                               
         TITLE 'T82403 STATION COMPETITIVE MAINTENANCE'                         
T82403   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T82403*,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         ST    R3,RELO                                                          
*                                                                               
NOCHGERR EQU   123                                                              
DUPSTERR EQU   125                 DUP STA OR AFFILIATE                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BNE   MAIN10                                                           
         GOTO1 =A(VK),DMCB,(RC),(RA),RR=RELO                                    
         BNZ   TRAPERR                                                          
         B     EXIT                                                             
MAIN10   EQU   *                                                                
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BNE   MAIN20                                                           
         GOTO1 =A(VR),DMCB,(RC),(RA),RR=RELO                                    
         BNZ   TRAPERR                                                          
         B     EXIT                                                             
MAIN20   EQU   *                                                                
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BNE   MAIN30                                                           
         GOTO1 =A(DR),DMCB,(RC),(RA),RR=RELO                                    
         BNZ   TRAPERR                                                          
         B     EXIT                                                             
MAIN30   EQU   *                                                                
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BNE   MAIN40                                                           
         GOTO1 =A(DK),DMCB,(RC),(RA),RR=RELO                                    
         BNZ   TRAPERR                                                          
         B     EXIT                                                             
MAIN40   EQU   *                                                                
         CLI   MODE,LISTRECS       ONLINE LIST RECORDS                          
         BNE   EXIT                                                             
         GOTO1 =A(LR),DMCB,(RC),(RA),RR=RELO                                    
         BNZ   TRAPERR                                                          
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
TRAPERR  GOTO1 MYERROR                                                          
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
*                  VALIDATE KEY ROUTINE                              *          
**********************************************************************          
VK       NMOD1 0,*VALKEY*                                                       
         L     RC,0(R1)                                                         
         L     RA,4(R1)            TWA                                          
*                                                                               
         CLI   ACTNUM,ACTLIST      ONLINE LIST RECORDS                          
         BE    VKGOOD                                                           
*                                                                               
         LA    R2,FSCSTAH                                                       
         GOTO1 ANY                 REQUIRED                                     
*                                                                               
* VALIDATE STATION CALL LETTERS                                                 
*                                                                               
         XC    BLOCK(256),BLOCK                                                 
         MVC   WORK(50),SPACES                                                  
         GOTO1 SCANNER,DMCB,(R2),(2,BLOCK),C',=,-'                              
         MVC   RERROR,=AL2(INVSTA)                                              
         LA    R5,BLOCK                                                         
*                                                                               
         CLI   0(R5),3                                                          
         BL    VKBAD                                                            
         CLI   0(R5),4                                                          
         BH    VKBAD                                                            
         TM    2(R5),X'40'         TEST ALPHA                                   
         BZ    VKBAD                                                            
         MVC   WORK(4),12(R5)      SAVE CALL LETTERS                            
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,1(R5)          DEFAULT = TV                                 
         BZ    VK100               YES                                          
         BCTR  RE,0                                                             
         EX    RE,STATV            TV LEAVE BLANK                               
         BE    VK100                                                            
         MVI   WORK+4,C'L'         L = LO POWER                                 
         EX    RE,STALP                                                         
         BE    VK100                                                            
         MVI   WORK+4,C'A'         AM = A                                       
         EX    RE,STAAM                                                         
         BE    VK100                                                            
         MVI   WORK+4,C'F'         FM = F                                       
         EX    RE,STAFM                                                         
         BE    VK100                                                            
         MVI   WORK+4,C'C'         CM = C                                       
         EX    RE,STACM                                                         
         BE    VK100                                                            
         MVI   WORK+4,C' '         MAY BE SATELLITE STATION                     
         EX    RE,STA1                                                          
         BNE   VK50                                                             
         MVI   WORK+4,C'1'                                                      
         B     VK100                                                            
VK50     EX    RE,STA2                                                          
         BNE   VKBAD                                                            
         MVI   WORK+4,C'2'                                                      
*                                                                               
VK100    EQU   *                                                                
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RSTAKEY,R4                                                       
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,AGENCY     REP                                          
         MVC   RSTAKSTA(4),WORK    STATION                                      
         OC    RSTAKSTA,SPACES                                                  
         CLI   WORK+4,C'1'         DON'T FILL FOR SATELLITES                    
         BE    VKGOOD                                                           
         CLI   WORK+4,C'2'                                                      
         BE    VKGOOD                                                           
         MVC   RSTAKSTA+4(1),WORK+4                                             
VKGOOD   EQU   *                                                                
         SR    R0,R0                                                            
         B     VKEX                                                             
VKBAD    EQU   *                                                                
         LA    R0,1                                                             
VKEX     EQU   *                                                                
         LTR   R0,R0               SET THE CONDITION CODE                       
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
STATV    CLC   22(0,R5),=C'TV'                                                  
STAAM    CLC   22(0,R5),=C'AM'                                                  
STAFM    CLC   22(0,R5),=C'FM'                                                  
STACM    CLC   22(0,R5),=C'CM'                                                  
STALP    CLC   22(0,R5),=C'L'                                                   
STA1     CLC   22(0,R5),=C'1'                                                   
STA2     CLC   22(0,R5),=C'2'                                                   
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
*                  DISPLAY RECORD                                    *          
**********************************************************************          
DR       NMOD1 0,*DISREC*                                                       
         L     RC,0(R1)                                                         
         L     RA,4(R1)                                                         
*                                                                               
         L     R4,AIO                                                           
         USING STATD,R4                                                         
*                                                                               
         BAS   RE,CLEAR            CLEAR FIELDS                                 
*                                                                               
         LA    R2,FSCCPPH                                                       
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   DRGOOD                                                           
*                                                                               
DR20     EQU   *                                                                
         GOTO1 =A(NEXTUF),DMCB,(R2),RR=RELO  GET NEXT UNPR. FIELD               
         CLI   0(R2),9                                                          
         BE    DRGOOD                                                           
         LA    R0,FSCLSTH                                                       
         CR    R0,R2                                                            
         BE    DRGOOD                                                           
*                                                                               
         FOUT  (R2),SPACES,11                                                   
         SPACE 1                                                                
         USING RSTAMKEL,R6                                                      
         MVC   8(4,R2),RSTAMKST    CALL LETTERS                                 
         LA    R5,11(R2)                                                        
         CLI   0(R5),C' '                                                       
         BE    *+8                                                              
         LA    R5,1(R5)                                                         
         SPACE 1                                                                
         MVI   0(R5),C'-'                                                       
         MVC   1(1,R5),RSTAMKST+4  BAND                                         
         CLI   1(R5),C' '                                                       
         BNE   *+8                                                              
         MVI   1(R5),C'T'                                                       
         LA    R5,2(R5)                                                         
         CLC   RSTAMKAF(3),SPACES                                               
         BE    DR30                                                             
         MVI   0(R5),C'='                                                       
         MVC   1(3,R5),RSTAMKAF    AFFILIATION                                  
         DROP  R6                                                               
DR30     EQU   *                                                                
         BAS   RE,NEXTEL                                                        
         BE    DR20                                                             
*                                                                               
DRGOOD   DS    0H                                                               
         SR    R0,R0                                                            
         B     DREX                                                             
DRBAD    LA    R0,1                                                             
DREX     EQU   *                                                                
         LTR   R0,R0               SETS THE CONDITION CODE                      
         XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
*                  CLEAR FIELDS ROUTINE                              *          
**********************************************************************          
CLEAR    NTR1                                                                   
         LA    R2,FSCCMPH          FIRST FIELD                                  
         LA    R5,FSCLST           LAST FIELD                                   
*                                                                               
CR20     EQU   *                                                                
         CR    R2,R5               ALL FIELDS HAVE BEEN CLEARED?                
         BNL   CREX                YES                                          
         TM    1(R2),X'20'         PROTECTED FIELD?                             
         BO    CR50                YES - NEXT FIELD                             
         ZIC   R3,5(R2)            LENGTH                                       
         LTR   R3,R3                                                            
         BZ    CR50                NEXT FIELD                                   
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         OI    6(R2),X'80'                                                      
CR50     EQU   *                                                                
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     CR20                                                             
CREX     EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
**********************************************************************          
         GETEL R6,DATADISP,ELCODE                                               
**********************************************************************          
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
*                  VALIDATE RECORD                                   *          
**********************************************************************          
VR       NMOD1 0,*VALREC*                                                       
         L     RC,0(R1)                                                         
         L     RA,4(R1)            TWA                                          
*                                                                               
VRE10    EQU   *                   GET RECORD AND DELETE ALL                    
         OI    GENSTAT2,RETEQSEL                                                
*                                                                               
         L     R6,AIO                                                           
         MVC   MYKEY(L'MYKEY),0(R6) SAVE THE KEY                                
*                                                                               
         MVI   ELCODE,X'02'                                                     
         GOTO1 REMELEM                                                          
*                                                                               
         MVI   BYTE4,X'00'         COUNT COMPETING STATIONS                     
*                                                                               
         LA    R2,FSCCPPH                                                       
*                                                                               
VRE20    EQU   *                                                                
         GOTO1 =A(NEXTUF),DMCB,(R2),RR=RELO  GET NEXT UNPR. FIELD               
         CLI   0(R2),9                                                          
         BE    VRE50                                                            
         LA    R0,FSCLSTH                                                       
         CR    R0,R2                                                            
         BE    VRE50                                                            
         CLI   5(R2),0                                                          
         BE    VRE20                                                            
*                                                                               
         L     R6,AIO                                                           
         LA    R4,ELEM                                                          
*                                                                               
         XC    ELEM,ELEM                                                        
         MVC   0(2,R4),=X'020F'                                                 
         MVC   2(8,R4),SPACES                                                   
*                                                                               
         LA    R1,2(R4)                                                         
         LA    R3,8(R2)                                                         
         LA    R5,4                                                             
         BAS   RE,TESTAN           TEST AND MOVE FIELD                          
         BNZ   VRERR2                                                           
         TM    BYTE,X'04'          ALPHA                                        
         BZ    VRERR2                                                           
         CH    R5,=H'3'            AT LEAST 3 CHARS                             
         BL    VRERR2                                                           
         CLI   0(R3),C'-'          CHECK FOR - TO NAME BAND                     
         BNE   VRERR2                                                           
         MVC   6(1,R4),1(R3)       BAND                                         
         CLC   1(2,R3),=C'A='                                                   
         BE    VRE30                                                            
         CLC   1(2,R3),=C'F='                                                   
         BE    VRE30                                                            
         CLC   1(2,R3),=C'C='      COMBO STATION                                
         BE    VRE30                                                            
         CLI   1(R3),C'A'                                                       
         BE    VRE30                                                            
         CLI   1(R3),C'F'                                                       
         BE    VRE30                                                            
         CLI   1(R3),C'C'          COMBO STATION                                
         BE    VRE30                                                            
         CLI   1(R3),C'N'          RADIO NETWORK                                
         BE    VRE30                                                            
         CLC   1(2,R3),=C'T='                                                   
         BNE   VRERR2                                                           
         MVI   6(R4),C' '                                                       
*                                                                               
VRE30    LA    R3,3(R3)                                                         
         MVC   7(3,R4),0(R3)       AFFILIATION                                  
         OC    7(3,R4),=CL3' '                                                  
         CLC   7(3,R4),=CL3' '                                                  
         BNE   VRE40               IF RADIO, ANY INPUT OR BLANK IS OK           
         CLI   26(R6),C' '         IF TV, MUST PUT IN SOMETHING                 
         BE    VRERR2                                                           
         CLI   26(R6),C'L'         IF TV, MUST PUT IN SOMETHING                 
         BE    VRERR2                                                           
VRE40    BAS   RE,DUPCK                                                         
         BNZ   VRBAD                                                            
*                                                                               
         GOTO1 ADDELEM                                                          
         B     VRE20                                                            
*                                                                               
VRE50    EQU   *                                                                
         L     R6,AIO                                                           
*                                                                               
         CLI   26(R6),C'L'         DO NOT CHECK RADIO                           
         BE    VRE60               FOR DELETED STATIONS                         
         CLI   26(R6),C' '         DO NOT CHECK RADIO                           
         BNE   VRGOOD              FOR DELETED STATIONS                         
VRE60    EQU   *                                                                
         MVC   KEY,0(R6)                                                        
         GOTO1 HIGH                                                             
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC              GET OLD REC IN REC2                          
         MVC   AIO,AIO1                                                         
*                                                                               
         L     R6,AIO2                                                          
         LA    R4,34(R6)                                                        
*                                                                               
VRE70    ZIC   R5,1(R4)                                                         
         LTR   R5,R5                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R4,R5                                                            
         CLI   0(R4),0                                                          
         BE    VRGOOD              OK TO PUTREC                                 
         CLI   0(R4),2                                                          
         BNE   VRE70                                                            
*                                                                               
         GOTO1 =A(NEXTUF),DMCB,(R2),RR=RELO  GET NEXT UNPR. FIELD               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL2                                                        
         B     VRE85                                                            
*                                                                               
VRE80    EQU   *                                                                
         BAS   RE,NEXTEL2                                                       
VRE85    EQU   *                                                                
         BNE   VRERR6                                                           
* R4 - AIO2, R6 - AIO                                                           
         CLC   2(5,R6),2(R4)       IS OLD COMPETING ON NEW REC                  
         BNE   VRE80                                                            
         B     VRE70               GET NEXT  OLD                                
*                                                                               
VRGOOD   EQU   *                                                                
         GOTO1 =A(DR),DMCB,(RC),(RA),RR=RELO                                    
         MVC   KEY(L'MYKEY),MYKEY                                               
         GOTO1 HIGH                AVOID PUTREC DRAMA                           
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
         SR    R0,R0                                                            
         B     VREX                                                             
*                                                                               
VRBAD    LA    R0,1                                                             
*                                                                               
VREX     EQU   *                                                                
         LTR   R0,R0               SET THE CONDITION CODE                       
         XIT1  REGS=(R2)           IN CASE OF ERROR MESSAGE                     
         EJECT                                                                  
********************************************************************            
*              CHECK FOR DUPLICATES                                *            
********************************************************************            
DUPCK    NTR1                                                                   
         L     R6,AIO1                                                          
*                                                                               
         LA    R4,ELEM                                                          
         CLC   2(5,R4),22(R6)   SAME STATION AND BAND                           
         BE    DUPERR1                                                          
*                                                                               
DUP10    EQU   *                                                                
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL2                                                        
         B     DUP25                                                            
*                                                                               
DUP20    BAS   RE,NEXTEL2                                                       
*                                                                               
DUP25    EQU   *                                                                
         BNE   DUP30                                                            
         CLC   2(5,R4),2(R6)                                                    
         BE    DUPERR1                                                          
         B     DUP20                                                            
*                                                                               
DUP30    EQU   *                                                                
         L     R6,AIO1                                                          
         CLI   26(R6),C'L'        KEY IS LOW POWER TV                           
         BE    DUP40                                                            
         CLI   26(R6),C' '        KEY IS TV                                     
         BNE   *+16                                                             
         CLI   6(R4),C' '         COMPETING MUST BE TV                          
         BNE   DUPERR2                                                          
         B     DUP40                                                            
         CLI   6(R4),C' '         KEY IS NOT TV                                 
         BE    DUPERR2             COMPETING IS TV                              
*                                                                               
DUP40    EQU   *                                                                
         ZIC   R0,BYTE4            COUNT TOTAL COMPETING                        
         AH    R0,=H'1'                                                         
         STC   R0,BYTE4                                                         
         CLI   26(R6),C'L'        LOW POWER TV                                  
         BE    DUP41                                                            
         CLI   26(R6),C' '        TV                                            
         BNE   DUPGOOD                                                          
DUP41    EQU   *                                                                
         CH    R0,=H'11'           MAX 11 COMPETITIVES FOR TV                   
         BH    DUPERR2                                                          
*                                                                               
         CLC   7(3,R4),=C'ABC'   AND COUNT 'IND' COMPETING                      
         BE    DUPGOOD                                                          
         CLC   7(3,R4),=C'CBS'                                                  
         BE    DUPGOOD                                                          
         CLC   7(3,R4),=C'NBC'                                                  
         BE    DUPGOOD                                                          
         CLI   26(R6),C'L'        LOW POWER TV                                  
         BE    DUP42                                                            
         CLI   26(R6),C' '        TV                                            
         BNE   *+12                                                             
DUP42    EQU   *                                                                
*                                                                               
DUPGOOD  SR    R0,R0                                                            
DUPEX    LTR   R0,R0                                                            
         XIT1                                                                   
DUPBAD   LA    R0,1                                                             
         B     DUPEX                                                            
*                                                                               
DUPERR1  EQU   *                                                                
         MVC   RERROR,=AL2(DUPSTERR)                                            
         B     DUPBAD                                                           
*                                                                               
DUPERR2  EQU   *                                                                
         MVC   RERROR,=AL2(INVERR)                                              
         B     DUPBAD                                                           
*                                                                               
         EJECT                                                                  
*********************************************************************           
*          DATA SET RELFM14    AT LEVEL 140 AS OF 03/17/97                      
* VALIDATE FIELD IS ALPHA/NUMERIC                                               
* FIELD IS DELIMITED BY BLANK,COMMA,OR DASH                                     
* R3 HAS FIELD ADDRESS. ON EXIT HAS STOP CHAR ADDRESS                           
* R5 HAS MAX LENGTH.    ON EXIT HAS CHAR COUNT.                                 
* R1 HAS 'TO' ADDRESS.  ON EXIT HAS NEXT CHAR ADDRESS                           
*********************************************************************           
TESTAN   MVI   BYTE,X'0C'          SET VALID A (X'04') AND N (X'08')            
         LA    R0,1(R5)            GET MAX LEN+1                                
TESTAN1  CLI   0(R3),C' '                                                       
         BE    TESTANX                                                          
         CLI   0(R3),0                                                          
         BE    TESTANX                                                          
         CLI   0(R3),C','                                                       
         BE    TESTANX                                                          
         CLI   0(R3),C'-'                                                       
         BE    TESTANX                                                          
         CLI   0(R3),C'A'                                                       
         BL    TESTAN2                                                          
         CLI   0(R3),C'Z'                                                       
         BNH   TESTAN4                                                          
TESTAN2  NI    BYTE,X'08'          FIELD NOT ALPHA                              
         CLI   0(R3),C'0'                                                       
         BL    TESTAN4                                                          
         CLI   0(R3),C'9'                                                       
         BNH   TESTAN6                                                          
TESTAN4  NI    BYTE,X'04'          FIELD NOT NUMERIC                            
TESTAN6  MVC   0(1,R1),0(R3)                                                    
         LA    R1,1(R1)                                                         
         LA    R3,1(R3)                                                         
         BCT   R0,TESTAN1                                                       
         B     TESTBAD                                                          
TESTANX  BCTR  R0,0                ADJUST COUNT                                 
         SR    R5,R0               GIVES CHARACTER COUNT                        
*                                                                               
TESTGOOD SR    R0,R0                                                            
TESTEX   EQU   *                                                                
         LTR   R0,R0                                                            
         BR    RE                                                               
TESTBAD  LA    R0,1                                                             
         B     TESTEX                                                           
         EJECT                                                                  
*                                                                               
*******************************************************************             
GETEL2   AH    R6,DATADISP                                                      
FIRSTEL2 CLI   0(R6),0                                                          
         BNE   *+10                                                             
         CLI   0(R6),1                                                          
         BR    RE                                                               
         CLI   ELCODE,0                                                         
         BCR   8,RE                                                             
         CLC   ELCODE,0(R6)                                                     
         BCR   8,RE                                                             
NEXTEL2  SR    RF,RF                                                            
         IC    RF,1(R6)                                                         
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,RF                                                            
         B     FIRSTEL2                                                         
         EJECT                                                                  
*                                                                               
VRERR2   EQU   *                                                                
         MVC   RERROR,=AL2(INVERR)                                              
         B     VRBAD                                                            
*                                                                               
VRERR4   EQU   *                                                                
         MVC   RERROR,=AL2(DUPSTERR)                                            
         B     VRBAD                                                            
*                                                                               
VRERR6   EQU   *                                                                
         MVC   RERROR,=AL2(NOCHGERR)                                            
         B     VRBAD                                                            
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
*                  LIST RECORDS                                      *          
**********************************************************************          
LR       NMOD1 0,*LSTREC*                                                       
         L     RC,0(R1)                                                         
         L     RA,4(R1)            TWA                                          
*                                                                               
         OC    KEY,KEY             FIRST TIME THROUGH?                          
         BNZ   LR15                NO                                           
*                                                                               
         LA    R4,KEY                                                           
         USING STATD,R4                                                         
         MVI   RSTAKTYP,X'02'      STATION RECORD TYPE                          
         MVC   RSTAKREP(2),AGENCY  REP CODE                                     
*                                                                               
         LA    R2,LSCSTATH                                                      
         CLI   5(R2),0                                                          
         BE    LR10                                                             
*                                                                               
         XC    BLOCK(256),BLOCK                                                 
         MVC   WORK(50),SPACES                                                  
         GOTO1 SCANNER,DMCB,(R2),(2,BLOCK),C',=,-'                              
         MVC   RERROR,=AL2(INVSTA)                                              
         LA    R5,BLOCK                                                         
*                                                                               
         CLI   0(R5),4                                                          
         BH    LRBAD                                                            
         TM    2(R5),X'40'         TEST ALPHA                                   
         BZ    LRBAD                                                            
         MVC   RSTAKSTA(4),12(R5)  MOVE CALL LETTERS INTO THE KEY               
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,1(R5)          DEFAULT = TV                                 
         BZ    LR10                YES                                          
         BCTR  RE,0                                                             
         EX    RE,STATV2           TV LEAVE BLANK                               
         BE    LR10                                                             
         MVI   RSTAKSTA+4,C'L'         L = LO POWER                             
         EX    RE,STALP2                                                        
         BE    LR10                                                             
         MVI   RSTAKSTA+4,C'A'         AM = A                                   
         EX    RE,STAAM2                                                        
         BE    LR10                                                             
         MVI   RSTAKSTA+4,C'F'         FM = F                                   
         EX    RE,STAFM2                                                        
         BE    LR10                                                             
         MVI   RSTAKSTA+4,C'C'         CM = C                                   
         EX    RE,STACM2                                                        
         BE    LR10                                                             
         MVI   RSTAKSTA+4,C' '         MAY BE SATELLITE STATION                 
         EX    RE,STA1V2                                                        
         BNE   LR05                                                             
         MVI   RSTAKSTA+4,C'1'                                                  
         B     LR10                                                             
LR05     EX    RE,STA2V2                                                        
         BNE   LRBAD                                                            
         MVI   RSTAKSTA+4,C'2'                                                  
*                                                                               
LR10     EQU   *                                                                
         OC    RSTAKSTA,SPACES                                                  
         DROP  R4                                                               
LR15     EQU   *                                                                
         GOTO1 HIGH                                                             
*                                                                               
LR20     EQU   *                                                                
         CLC   KEY(22),KEYSAVE     ALL RECORDS HAVE BEEN DISPLAYED?             
         BNE   LRGOOD              YES                                          
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         XC    LISTAR,LISTAR                                                    
*                                                                               
         L     R4,AIO                                                           
         USING STATD,R4                                                         
         MVC   LISTSTA(4),RSTAKSTA                                              
         CLI   RSTAKSTA+4,C' '                                                  
         BE    LR25                                                             
         MVI   LISTSTA+4,C'-'                                                   
         MVC   LISTSTA+5(1),RSTAKSTA+4                                          
*                                                                               
LR25     EQU   *                                                                
         MVC   LISTMKT(L'RSTAMKT),RSTAMKT                                       
         DROP  R4                                                               
         GOTO1 LISTMON                                                          
         MVC   KEYSAVE(27),KEY                                                  
         GOTO1 SEQ                                                              
         B     LR20                                                             
*                                                                               
LRGOOD   EQU   *                                                                
         SR    R0,R0                                                            
         B     LREX                                                             
*                                                                               
LRBAD    LA    R0,1                                                             
*                                                                               
LREX     EQU   *                                                                
         LTR   R0,R0               SET THE CONDITION CODE                       
         XIT1  REGS=(R2)                                                        
         EJECT                                                                  
*                                                                               
STATV2   CLC   22(0,R5),=C'TV'                                                  
STAAM2   CLC   22(0,R5),=C'AM'                                                  
STAFM2   CLC   22(0,R5),=C'FM'                                                  
STACM2   CLC   22(0,R5),=C'CM'                                                  
STALP2   CLC   22(0,R5),=C'L'                                                   
STA1V2   CLC   22(0,R5),=C'1'                                                   
STA2V2   CLC   22(0,R5),=C'2'                                                   
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
*                  DISPLAY KEY                                       *          
**********************************************************************          
DK       NMOD1 0,*DISKEY*                                                       
         L     RC,0(R1)                                                         
         L     RA,4(R1)            TWA                                          
*                                                                               
         L     R4,AIO                                                           
         USING STATD,R4                                                         
         MVC   FSCSTA(L'FSCSTA),SPACES                                          
         MVC   FSCSTA(4),RSTAKSTA                                               
         CLI   RSTAKSTA+4,C' '                                                  
         BE    DKGOOD                                                           
         CLI   FSCSTA+3,C' '                                                    
         BNE   DK10                                                             
         MVI   FSCSTA+3,C'-'                                                    
         MVC   FSCSTA+4(1),RSTAKSTA+4                                           
         B     *+14                                                             
DK10     EQU   *                                                                
         MVI   FSCSTA+4,C'-'                                                    
         MVC   FSCSTA+5(1),RSTAKSTA+4                                           
*                                                                               
DKGOOD   EQU   *                                                                
         OI    FSCSTAH+6,X'80'                                                  
         SR    R0,R0                                                            
         B     DKEX                                                             
*                                                                               
DKBAD    LA    R0,1                                                             
*                                                                               
DKEX     EQU   *                                                                
         LTR   R0,R0               SET THE CONDITION CODE                       
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
*****************************************************************               
NEXTUF   NMOD1 0,*NEXTUF*                                                       
         L     R2,0(R1)                                                         
NEXTUF1  EQU   *                                                                
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    NEXTEX                                                           
         CLI   0(R2),9                                                          
         BE    NEXTUF2                                                          
         TM    1(R2),X'20'         TEST PROTECTED                               
         BO    NEXTUF1                                                          
NEXTUF2  CLI   9(R2),0             CHECK FOR LAST                               
         BNE   NEXTUF4                                                          
         CR    R2,R2               IF LAST, SET CC=                             
         B     NEXTEX                                                           
NEXTUF4  LTR   R2,R2               NOT LAST, SET CC NOT=                        
*                                                                               
NEXTEX   XIT1  REGS=(R2)                                                        
         EJECT                                                                  
*                                                                               
RELO     DS    A                                                                
BYTE4    DS    X                                                                
MYKEY    DS    CL27                                                             
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDFLDIND          EQUATED INDICATORS FOR FOUT                  
       ++INCLUDE REBASINC                                                       
       ++INCLUDE REBASTWA                                                       
         SPACE 2                                                                
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE REBASB2D          MAINTENANCE SCREEN                           
         ORG   CONTAGH                                                          
       ++INCLUDE REBASD2D          LIST SCREEN                                  
         EJECT                                                                  
*                                                                               
STATD    DSECT                                                                  
       ++INCLUDE REGENSTA                                                       
*      ++INCLUDE CTGENFILE                                                      
       ++INCLUDE FATWA                                                          
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE REBASWORKD                                                     
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LISTSTA  DS    CL6                                                              
         DS    CL3                                                              
LISTMKT  DS    CL20                                                             
         DS    CL2                                                              
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'127REBAS03   05/01/02'                                      
         END                                                                    
