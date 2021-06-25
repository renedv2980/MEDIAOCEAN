*          DATA SET CTSFM34    AT LEVEL 051 AS OF 02/26/20                      
*PROCESS USING(WARN(15))                                                        
*PHASE TA0A34E                                                                  
*INCLUDE NSIWEEK                                                                
*INCLUDE NETWEEK                                                                
*INCLUDE NETUNBK                                                                
*                                                                               
***********************************************************************         
*                     (DCON)                                          *         
*  TITLE: TA0A34 - DEMO CONTROL RECORDS MAINTENANCE / LIST / REPORT   *         
*                                                                     *         
*  CALLED FROM: SFM CONTROLLER (TA0A00), WHICH CALLS                  *         
*               DDGENCON (T00A30) WHICH CALLS THIS.                   *         
*                                                                     *         
*  CALLS TO:    DATAMGR                                               *         
*                                                                     *         
*  INPUTS: SCREENS CTSFM92 (TA0A92) -- MAINTENANCE                    *         
*                  CTSFM91 (TA0A91) -- LIST/PRINTED REPORT            *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R0 - WORK                                                  *         
*          R1 - WORK                                                  *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR    *         
*          R3 - WORK                                                  *         
*          R4 - WORK                                                  *         
*          R5 - WORK                                                  *         
*          R6 - USED FOR GETEL ELEMENT DSECT POINTER                  *         
*          R7 - SECOND BASE                                           *         
*          R8 - WORK (AND SPOOLD IN LISTRECS)                         *         
*          R9 - SYSD                                                  *         
*          RA - TWA                                                   *         
*          RB - FIRST BASE                                            *         
*          RC - GEND                                                  *         
*          RD - SYSTEM                                                *         
*          RE - SYSTEM                                                *         
*          RF - SYSTEM                                                *         
*                                                                     *         
***********************************************************************         
         TITLE 'TA0A34 DEMO CONTROL RECORDS MAINTENANCE /LIST'                  
TA0A34   CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL 0,TA0A34**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
*                                                                               
         BAS   RE,SETUP                                                         
*                                                                               
         L     RF,ACOMFACS         PERMIT MAXIMUM I/O'S TO BE EXCEEDED          
         ICM   RF,15,CGETFACT-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,(X'80',0),F#MIOST                                      
*                                                                               
         MVC   DMCB+4(4),=X'D9000A1D'   GETBROAD                                
         GOTO1 CALLOV,DMCB,0                                                    
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   AGETBROD,DMCB                                                    
*                                                                               
         CLC   =C'++',TWAAGY       DEMO SIGN-IN GETS FULL ACCESS                
         BE    FULLACCS            ALL OTHERS:                                  
*                                                                               
         CLI   ACTNUM,ACTLIST      ON LIST SCREEN...                            
         BNE   CHKMAINT                                                         
         MVC   DCOAGYC,TWAAGY      ...PLUG IN AGENCY CODE FILTER                
         OI    DCOAGYCH+4,X'04'    FORCE VALID ALPHA                            
         OI    DCOAGYCH+6,X'80'    XMIT                                         
         MVI   DCOAGYCH+5,2        INPUT LENGTH = 2                             
         B     FULLACCS                                                         
*                                                                               
CHKMAINT DS    0H                                                               
         CLI   ACTNUM,ACTADD        ADD                                         
         BE    NOACCER                                                          
         CLI   ACTNUM,ACTCHA        CHANGE                                      
         BE    NOACCER                                                          
         CLI   ACTNUM,ACTDEL        DELETE                                      
         BE    NOACCER                                                          
         CLI   ACTNUM,ACTREST       RESTORE                                     
         BE    NOACCER                                                          
*                                                                               
FULLACCS DS    0H                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       ONLINE LIST RECORDS                          
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    LR                                                               
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
NOACCER  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOACCERM),NOACCERM                                     
         GOTO1 ERREX2                                                           
NOACCERM DC    C'!! YOU ARE NOT AUTHORIZED TO PERFORM UPDATIVE ACTIONS'         
         EJECT                                                                  
********************************************************************            
*                      VALIDATE KEY                                             
********************************************************************            
*                                                                               
VK       DS    0H                                                               
*** REQUIRE PASSWORD FOR UPDATIVE ACTIONS                                       
         GOTOR CHKPSWD                                                          
*                                                                               
         LA    R5,KEY              START BUILDING KEY                           
         USING CTRREC,R5                                                        
         XC    KEY,KEY                                                          
         MVI   CTRKTYP,CTRKTEQU                                                 
*                                                                               
*** VALIDATE SOURCE                                                             
         MVI   SVSOURCE,0                                                       
         LA    R2,DCOSRCH                                                       
         CLI   5(R2),0             INPUT SOURCE REQUIRED                        
         BNE   *+16                                                             
         CLI   ACTNUM,ACTLIST      EXCEPT FOR LIST                              
         BE    VK40                                                             
         B     MISSERR                                                          
*                                                                               
         CLI   5(R2),1             - SOURCE NAME -                              
         BE    VK15                                                             
         LARL  RE,SRCTAB                                                        
         USING SRCTABD,RE                                                       
VK10     CLC   =X'FFFF',0(RE)                                                   
         BE    INVERR                                                           
         LLC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SRCNAME(0),8(R2)                                                 
         BE    VK30                                                             
         LA    RE,SRCTABLQ(RE)                                                  
         B     VK10                                                             
*                                                                               
VK15     LARL  RE,SRCTAB           - 1-CHAR SOURCE CODE -                       
VK20     CLC   =X'FFFF',SRCNAME                                                 
         BE    INVERR                                                           
         CLC   SRCCODE,8(R2)                                                    
         BE    VK30                                                             
         LA    RE,SRCTABLQ(RE)                                                  
         B     VK20                                                             
*                                                                               
VK30     DS    0H                                                               
         MVC   CTRKSRC,SRCCODE                                                  
         MVC   SVSOURCE,CTRKSRC                                                 
         DROP  RE                                                               
*                                                                               
VK40     DS    0H                                                               
*                                                                               
*** VALIDATE SUB-FILE/MEDIA                                                     
         LA    R2,DCOSUBFH                                                      
         CLI   5(R2),0             INPUT SUB-FILE IS REQUIRED                   
         BNE   *+16                                                             
         CLI   ACTNUM,ACTLIST      EXCEPT FOR LIST                              
         BE    VK85                                                             
         B     MISSERR                                                          
*                                                                               
         CLI   DCOSRCH+5,0         SOURCE MUST ALSO BE THERE (ON LIST)          
         BNE   *+12                                                             
         LA    R2,DCOSRCH          PUT CURSOR ON SOURCE FIELD                   
         B     MISSERR                                                          
*                                                                               
         CLI   5(R2),1             - SUBFILE(MEDIA) NAME -                      
         BE    VK55                                                             
         LARL  RE,SUBFTAB                                                       
         USING SUBFTABD,RE                                                      
VK50     CLC   =X'FFFF',0(RE)      SEARCH SUB-FILE TABLE                        
         BE    INVERR                                                           
         LLC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SUBFNAME(0),8(R2)                                                
         BE    VK70                                                             
         LA    RE,SUBFTBLQ(RE)                                                  
         B     VK50                                                             
*                                                                               
VK55     LARL  RE,SUBFTAB          - 1-CHAR MEDIA CODE -                        
VK60     CLC   =X'FFFF',0(RE)                                                   
         BE    INVERR                                                           
         CLC   SUBFCODE,8(R2)                                                   
         BE    VK70                                                             
         LA    RE,SUBFTBLQ(RE)                                                  
         B     VK60                                                             
*                                                                               
VK70     DS    0H                                                               
         MVC   CTRKMED,SUBFCODE                                                 
         MVC   SVSUBF,CTRKMED                                                   
         DROP  RE                                                               
*                                                                               
         CLI   DCOSRCH+5,0                                                      
         BE    VK85                                                             
*                                                                               
*** MAKE SURE THE SOURCE/SUB-FILE COMBO IS VALID                                
         MVC   HALF(1),CTRKSRC                                                  
         MVC   HALF+1(1),CTRKMED                                                
         LARL  RE,SRCMEDT                                                       
         USING SRCMEDTD,RE                                                      
VK80     CLI   0(RE),X'FF'                                                      
         BE    INVERR                                                           
         CLC   HALF,SRCMEDSM                                                    
         BE    *+12                                                             
         LA    RE,SRCMEDTQ(RE)                                                  
         B     VK80                                                             
*                                                                               
         MVC   DCOSRC,SRCMEDSN     DISPLAY FULL SOURCE NAME                     
         OI    DCOSRCH+6,X'80'                                                  
         MVC   DCOSUBF,SRCMEDMN    DISPLAY FULL SUB-FILE NAME                   
         OI    DCOSUBFH+6,X'80'                                                 
         DROP  RE                                                               
*                                                                               
VK85     DS    0H                                                               
         MVI   CTRKCODE,X'00'                                                   
         XC    CTRKCLI,CTRKCLI                                                  
         XC    CTRKBOOK,CTRKBOOK                                                
         XC    CTRKAGY,CTRKAGY                                                  
*                                                                               
*** VALIDATE AGENCY CODE OR USER-ID                                             
         CLI   ACTNUM,ACTLIST      VALIDATE THIS AT MODE LISTRECS               
         BE    VK130                                                            
*                                                                               
         XC    DCNAGYC,DCNAGYC     CLEAR AGENCY CODE FIELD                      
         OI    DCNAGYCH+6,X'80'                                                 
*                                                                               
         LA    R2,DCNAUIDH         AGENCY/USER-ID COMBO FIELD                   
         CLI   5(R2),0             ALL AGENCY DEFAULT?                          
         BNE   VK90                                                             
         CLC   =C'++',TWAAGY       YES: IS USERID "DEMO" ?                      
         BNE   INVERR              NO: UNAUTHORIZED                             
         MVC   CTRKAGY,=X'FFFF'                                                 
         MVC   SVAPHAGY,CTRKAGY                                                 
         B     VK130                                                            
*                                                                               
VK90     DS    0H                                                               
         CLI   5(R2),2             ALPHA AGENCY CODES ARE 2-CHARACTER           
         BNE   VK100                                                            
*                                                                               
         CLC   =C'++',TWAAGY       IS USERID "DEMO" ?                           
         BE    *+14                YES: ANY AGENCY CODE IS VALID                
         CLC   DCNAUID(2),TWAAGY   NO: CAN ONLY DISPLAY YOUR OWN RECS           
         BNE   INVERR                                                           
*                                                                               
*&&DO                                                                           
* THIS CODE WAS DISABLED BY DEIS IN FEB/2010. THE CODE DOES NOTHING             
* OTHER THAN VALIDATE AN ALPHA AGENCY CODE BY READING THE ACCESS                
* RECORD FROM THE CONTROL FILE. SOMETIMES WE MIGHT WANT TO DELETE               
* DCON RECORDS FOR AGENCIES THAT HAVE BEEN REMOVED FROM THE CONTROL             
* FILE. I.E., THEY DON'T HAVE AN ACCESS RECORD ANY LONGER. SO RATHER            
* THAN STOP US FROM DELETING THE DCON RECORDS, WE JUST ASSUME THAT THE          
* USER IS SPECIFYING A VALID ALPHA AGENCY CODE. IF AN ALPHA AGENCY              
* CODE IS ENTERED WHICH WAS NEVER VALID TO BEGIN WITH, WE PRESUMABLY            
* WON'T FIND ANY DCON RECORDS. SO THIS CHANGE SEEMS SAFE.                       
*                                                                               
         MVC   HALF,DCNAUID        VALIDATE AGENCY CODE                         
         BRAS  RE,VALAGY                                                        
         BNE   INVERR                                                           
*&&                                                                             
*                                                                               
         MVC   CTRKAGY,DCNAUID     IT'S VALID                                   
         MVC   SVAPHAGY,CTRKAGY                                                 
         B     VK130                                                            
*                                                                               
VK100    DS    0H                                                               
*** VALIDATE USER ID                                                            
         TM    4(R2),X'08'         NUMERIC?                                     
         BNO   VK110               NO: ALPHA USERID PROVIDED                    
*                                                                               
         XC    DUB,DUB                                                          
         LLC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R0,DUB                                                           
         STCM  R0,3,CTRKAGY                                                     
         MVC   SVBINAGY,CTRKAGY                                                 
         GOTOR VALBUSER                                                         
         BE    VK120                                                            
         CLI   ACTNUM,ACTDEL                                                    
         BNE   INVERR                                                           
         B     VK120                                                            
*                                                                               
VK110    DS    0H                                                               
         GOTOR VALAUSER            VALIDATE ALPHA USERID                        
         BE    VK120                                                            
         CLI   ACTNUM,ACTDEL                                                    
         BNE   INVERR                                                           
*                                                                               
VK120    DS    0H                                                               
         CLC   =C'++',TWAAGY       IS USERID "DEMO" ?                           
         BE    *+14                YES: ANY USERID IS VALID                     
         CLC   SVAPHAGY,TWAAGY     NO: CAN ONLY DISPLAY YOUR OWN RECS           
         BNE   INVERR                                                           
*                                                                               
         MVC   DCNAUID,SVALPUID    ALWAYS DISPLAY ALPHA USERID...               
         OI    DCNAUIDH+6,X'80'    ...AS OPPOSED TO NUMERIC                     
         MVC   DCNAGYC,=C'(  )'    ENCLOSE AGENCY CODE IN PARENTHESES           
         MVC   DCNAGYC+1(2),SVAPHAGY  DISPLAY AGENCY CODE                       
         MVC   CTRKAGY,SVBINAGY    USERID NUMBER GOES INTO KEY                  
*                                                                               
VK130    DS    0H                                                               
*** VALIDATE BOOK TYPE                                                          
         MVI   CTRKCODE,X'FF'      DEFAULT INTERNAL VALUE                       
         LA    R2,DCNBTYPH                                                      
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BNE   *+18                                                             
         LA    R2,DCOBTYPH         YES: FILTER ONLY DEFAULT BOOKTYPE?           
         CLC   DCOBTYP,=C'**'                                                   
         BE    VK160               YES                                          
*                                                                               
         CLI   5(R2),0             USE DEFAULT BOOKTYPE?                        
         BE    VK150               YES                                          
*                                                                               
         L     RF,ACOMFACS         VALIDATE BOOKTYPE                            
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,SPBOOKTB  GET A(BOOKTYPE TABLE)                        
         ICM   RF,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                BAD TABLEID PASSED                           
         L     R0,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
*                                                                               
         MVC   HALF,8(R2)                                                       
         CLI   5(R2),1             IF 1-CHARACTER BOOKTYPE ENTERED...           
         BNE   VK140                                                            
         MVI   HALF+1,C' '         ...PAD IT WITH A BLANK                       
*                                                                               
VK140    DS    0H                                                               
         USING SPBKTYPD,RF                                                      
         CLI   0(RF),X'FF'         EOT?                                         
         BE    INVERR                                                           
*                                                                               
         CLC   SPBKTYPA,HALF       IS BOOKTYPE IN TABLE?                        
         BE    *+10                                                             
         AR    RF,R0               NO: TRY NEXT                                 
         B     VK140                                                            
*                                                                               
         MVC   CTRKCODE,SPBKTYPN   SAVE INTERNAL BOOKTYPE                       
         DROP  RF                                                               
*                                                                               
VK150    DS    0H                                                               
         MVC   SVBKTYPE,CTRKCODE                                                
         OI    6(R2),X'80'                                                      
*                                                                               
VK160    DS    0H                                                               
*** VALIDATE CLIENT CODE                                                        
         CLI   ACTNUM,ACTLIST      NO CLIENT FIELD IN LIST                      
         BE    VK180                                                            
*                                                                               
         MVC   CTRKCLI,=X'FFFFFF'                                               
         LA    R2,DCNCLIH                                                       
         CLI   5(R2),0                                                          
         BE    VK170                                                            
         CLC   SVAPHAGY,=X'FFFF'                                                
         BE    INVERR                                                           
         CLI   5(R2),2                                                          
         BL    INVERR                                                           
         MVC   CTRKCLI,=C'   '     PAD WITH SPACES                              
         LLC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CTRKCLI(0),DCNCLI                                                
         OI    6(R2),X'80'                                                      
VK170    DS    0H                                                               
*                                                                               
VK180    CLI   ACTNUM,ACTLIST      NO BOOK FIELD IN LIST                        
         BNE   VK190                                                            
*                                                                               
         LA    R2,DCOOPTNH         JUST LOOK AT THE OPTIONS                     
         CLI   5(R2),0                                                          
         BE    *+12                                                             
         CLI   8(R2),C'Y'                                                       
         BE    *+8                                                              
         MVI   8(R2),C'N'                                                       
         OI    6(R2),X'80'                                                      
         MVC   BKOPTN,8(R2)                                                     
         B     VK250                                                            
*                                                                               
*** VALIDATE BOOK FIELD                                                         
VK190    LA    R2,DCNBOOKH                                                      
         CLI   5(R2),0             IF BOOK =  BLANK                             
         BNE   VK200                AND                                         
         CLI   ACTNUM,ACTDIS       ACTION = DISPLAY                             
         BE    VK230                GET LATEST BOOK                             
         B     MISSERR              OTHERWISE DON'T ALLOW BLANK BOOK            
*                                                                               
VK200    XC    WORK,WORK                                                        
         LLC   R1,DCNBOOKH+5       BOOK NOT= BLANK                              
         STC   R1,WORK+2                                                        
         BCTR  R1,0                                                             
         LR    R0,R1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DUB(0),DCNBOOK                                                   
         MVC   WORK(1),CTRKSRC                                                  
         MVC   WORK+1(1),CTRKMED                                                
         GOTOR DTTOWK                                                           
         BNZ   BOOKERR                                                          
         MVC   CTRKBOOK,MYHALF                                                  
         XC    CTRKBOOK,=X'FFFF'                                                
*                                                                               
         GOTOR WKTODT                                                           
         LLC   R1,DUB2             REFORMAT BOOK INPUT,                         
         BCTR  R1,0                                                             
         CR    R0,R1               IF NECESSARY                                 
         BNE   VK210                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   DUB(0),8(R2)                                                     
         BE    VK220                                                            
VK210    XC    DCNBOOK,DCNBOOK     IT'S NECESSARY                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DCNBOOK(0),DUB                                                   
VK220    OI    6(R2),X'80'                                                      
         EJECT                                                                  
VK230    CLI   ACTNUM,ACTADD       DON'T SEARCH FOR RECD IF                     
         BE    VK250               ADD OR RESTORE                               
         CLI   ACTNUM,ACTREST                                                   
         BE    VK250                                                            
*                                                                               
         MVC   WORK(2),CTRKBOOK                                                 
         MVC   MYKEY,KEY           SAVE KEY                                     
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(23),KEYSAVE                                                  
         BNE   BOOKERR                                                          
         CLC   CTRKBOOK,KEYSAVE+CTRKBOOK-CTRKEY                                 
         BE    VK240               MATCH FOUND                                  
         CLI   ACTNUM,ACTCHA       NOT VALID FOR CHANGE                         
         BE    BOOKERR                                                          
         CLI   ACTNUM,ACTDEL                                                    
         BE    BOOKERR                                                          
*                                                                               
VK240    MVC   WORK(2),CTRKSRC     IF DISPLAY,                                  
         MVC   MYHALF,CTRKBOOK     REPLACE GIVEN BOOK WITH REAL BOOK            
         XC    MYHALF,=X'FFFF'                                                  
         XC    DCNBOOK,DCNBOOK                                                  
         GOTOR WKTODT                                                           
         LLC   R1,DUB2             DUB2(1) = L(OUTPUT DATE)                     
         BCTR  R1,0                                                             
         EX    R1,*+8              DUB = OUTPUT DATE                            
         B     *+10                                                             
         MVC   DCNBOOK(0),DUB                                                   
         OI    DCNBOOKH+6,X'80'                                                 
         MVC   WORK(2),CTRKBOOK                                                 
*                                                                               
         MVC   KEY,MYKEY           RESTORE KEY                                  
         MVC   CTRKBOOK,WORK                                                    
*                                                                               
VK250    DS    0H                                                               
         MVC   SAVEKEY,KEY                                                      
         J     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
*****************************************************************               
*              VALIDATE RECORD                                                  
*****************************************************************               
*                                                                               
VR       DS    0H                                                               
*** REQUIRE PASSWORD FOR UPDATIVE ACTIONS                                       
         GOTOR CHKPSWD                                                          
*                                                                               
         L     R6,AIO              START BUILDING RECORD                        
         USING CTRREC,R6                                                        
*                                                                               
*** BUILD VALID/INVALID MARKET ELEMENT                                          
         MVI   ELCODE,CTRMECDQ                                                  
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R2,DCNLTYPH         MARKET LIST FILTER TYPE                      
         CLI   DCNLTYPH+5,0                                                     
         BE    MISSERR             MUST BE PROVIDED                             
*                                                                               
         CLI   DCNLTYP,C'A'        ALL MARKETS AUTHORIZED?                      
         BE    VR200               CORRECT: NO MARKET LIST FILTER               
*                                                                               
         MVI   BYTE,CTRMTPV        ASSUME MARKET LIST IS VALID                  
         CLI   DCNLTYP,C'N'        NO MARKETS AUTHORIZED?                       
         BE    VR10                CORRECT: WILL HARD-CODE MARKET 999           
         CLI   DCNLTYP,C'V'        LIST OF MARKETS IS VALID?                    
         BE    VR10                CORRECT: TAKE IT FROM SCREEN                 
*                                                                               
         MVI   BYTE,CTRMTPI        ASSUME MARKET LIST IS INVALID                
         CLI   DCNLTYP,C'I'        LIST OF MARKETS IS INVALID?                  
         BNE   INVERR              NO: UNKNOWN VALUE                            
*                                                                               
VR10     DS    0H                                                               
         LA    R4,ELEM                                                          
         XC    ELEM,ELEM                                                        
         USING CTRMD,R4                                                         
         LA    R3,CTRMMKTS         DON'T CHANGE R3-POINTS TO WHERE TO           
         MVI   CTRMECDE,CTRMECDQ   STORE MARKET IN ELEM                         
         MVI   CTRMLEN,CTRMFXLQ                                                 
         MVC   CTRMTYPE,BYTE       LIST TYPE (VALID OR INVALID)                 
*                                                                               
         CLI   DCNLTYP,C'N'        NO MARKETS AUTHORIZED?                       
         BNE   VR20                CORRECT: HARD-CODE MARKET 999 FOR            
*                                   COMPATIBILITY WITH PREEXISTING APPS         
         MVI   CTRMLEN,CTRMFXLQ+2  OVERHEAD PLUS ONE MARKET (999)               
         MVC   CTRMMKTS,=AL2(999)  THIS HAS THE EFFECT OF ALLOWING ONLY         
*                                   MARKET 999, WHICH DOESN'T EXIST!            
         B     VR180               ADD THE ELEMENT                              
*                                                                               
VR20     DS    0H                                                               
         LA    R5,DCNMKTSH         BUILDING MARKET ELEMENT                      
         LA    R2,DCNMKTXH         DON'T CHANGE. R2=A(LAST LINE)                
         CLI   5(R5),0             R5 POINTS TO FIRST MKT LINE                  
         BNE   VR30                                                             
         GOTOR NXTMKLIN                                                         
         BNE   VR180               NO MARKETS INPUT                             
*                                                                               
VR30     LA    RE,SCANTAB                                                       
         LA    RF,SCANTABQ                                                      
         XCEF                                                                   
         GOTO1 SCANNER,DMCB,(0,(R5)),('MAXSCAN',SCANTAB)                        
         CLI   4(R1),0                                                          
         BE    INVERR                                                           
*                                                                               
         MVC   NLINES,4(R1)                                                     
         MVI   FNDX,1                                                           
         LA    R8,SCANTAB          R8 =A(SCAN TABLE ENTRY)                      
*                                                                               
VR40     CLC   FNDX,NLINES                                                      
         BH    VR170               GO TO NEXT LINE OF MARKETS                   
*                                                                               
         CLI   1(R8),0                                                          
         BNE   INVERR                                                           
*                                                                               
*  IF NUMERIC JUST PUT IN STRAIGHT INTO THE ELEMENT                             
*  IF NOT READ THE ALPHA MARKET RECORD TO SEE IF THE ALPHA EXISTS               
         XC    FNDMKT,FNDMKT                                                    
         TM    2(R8),X'80'         LEFT-HAND-SIDE NUMERIC?                      
         BO    VR80                YES                                          
*                                                                               
         CLI   CTRKMED,C'N'        GET MARKET NUMBER FROM ALPHA FOR NET         
         BE    VR50                                                             
         CLI   CTRKMED,C'W'        HISPANIC WEEKLY IS ALSO NET                  
         BNE   VR60                                                             
         CLI   CTRKSRC,C'H'                                                     
         BNE   VR60                                                             
*                                                                               
VR50     GOTOR VALNET                                                           
         B     VR80                                                             
*                                                                               
VR60     DS    0H                  FOR OTHERS                                   
         CLI   0(R8),3             ALPHAMARKET CAN'T BE > 3 CHARACTERS          
         BH    MKTERR                                                           
*                                                                               
         GOTOR VALALPH             GET MARKET NUMBER FROM ALPHA                 
*                                                                               
VR80     DS    0H                  NUMERIC MARKET                               
         ST    R5,ACURFORC                                                      
         OC    4(4,R8),4(R8)                                                    
         BZ    MKTERR                                                           
         CLC   4(4,R8),=F'9999'                                                 
         BH    MKTERR                                                           
*                                                                               
*  UPDATE ELEM                                                                  
*   BEFORE WE UPDATE THE ELEM MAKE SURE THE MARKET CURRENTLY                    
*   VALIDATED IS NOT ALREADY INCLUDED                                           
*                                                                               
         LA    RE,CTRMMKTS                                                      
VR90     CR    RE,R3                                                            
         BE    VR160                                                            
         CLC   0(2,RE),6(R8)                                                    
         BE    MKTERD                                                           
         LA    RE,2(RE)                                                         
         B     VR90                                                             
*                                                                               
VR160    MVC   0(L'CTRMMKTS,R3),6(R8)                                           
         LA    R3,L'CTRMMKTS(R3)                                                
*                                                                               
         LLC   R1,CTRMLEN                                                       
         LA    R1,2(R1)                                                         
         STC   R1,CTRMLEN                                                       
*                                                                               
         LLC   R1,FNDX                                                          
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
         LA    R8,L'SCANTAB(R8)                                                 
         B     VR40                                                             
*                                                                               
VR170    GOTOR NXTMKLIN                                                         
         BE    VR30                                                             
*                                                                               
VR180    DS    0H                                                               
         LA    RE,CTRMMKTS                                                      
         CLI   CTRMLEN,CTRMFXLQ    ANY MARKETS ENTERED?                         
         BH    VR190               YES: ADD THE ELEMENT                         
         CLI   DCNLTYP,C'V'        LIST OF VALID MARKETS EXPECTED?              
         BE    *+12                YES: ERROR                                   
         CLI   DCNLTYP,C'I'        LIST OF INVALID MARKETS EXPECTED?            
         BNE   VR200               NO: DON'T ADD ELEMENT                        
         LA    R2,DCNMKTSH         PUT CURSOR ON 1ST MARKET LIST FIELD          
         B     MISSERR             GENERATE ERROR MESSAGE                       
*                                                                               
VR190    DS    0H                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
VR200    DS    0H                                                               
         DROP  R4                                                               
*                                                                               
*** VALIDATE LOOKUP OPTIONS                                                     
         MVI   ELCODE,CTROECDQ     X'03' ELEMENT                                
         GOTO1 REMELEM                                                          
*                                                                               
         XC    ELEM,ELEM                                                        
E        USING CTROD,ELEM                                                       
         MVI   E.CTROECDE,CTROECDQ ELEMENT CODE                                 
         MVI   E.CTROLEN,CTROELNQ  ELEMENT LENGTH                               
*                                                                               
         LA    R2,DCNFASTH         STORE FIRST AND LAST LOOKUP FIELDS           
         LA    R5,DCNLSTXH                                                      
VR210    CLI   5(R2),0                                                          
         BE    VR290               BLANK FIELD                                  
*                                                                               
         LARL  RF,PARMTAB          INPUT IN LOOKUP FIELD                        
         USING PARMTABD,RF                                                      
VR240    CLI   0(RF),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         SR    R8,R8                                                            
         ICM   R8,3,PARMDISP                                                    
         LR    R1,R2                                                            
         LA    R0,DCNSRCH                                                       
         SR    R1,R0               DISPLACEMENT FROM FIRST FIELD                
         CR    R8,R1                                                            
         BNE   VR280                                                            
*                                                                               
         ICM   R4,15,PARMAKWD      FIELD WAS FOUND IN TABLE                     
         A     R4,RELO             POSITION IN KEYTAB                           
         USING KEYTABD,R4                                                       
VR250    CLI   0(R4),0                                                          
         BE    INVLOKE             OPTION NOT VALID                             
         LLC   R1,5(R2)            LENGTH OF INPUT OPTION                       
*                                                                               
         LA    R8,8(R2)                                                         
VR260    LTR   R1,R1                                                            
         BZ    INVLOKE                                                          
         CLI   0(R8),C' '          SKIP SPACES AND '=' SIGNS                    
         BE    *+12                                                             
         CLI   0(R8),C'='                                                       
         BNE   *+14                                                             
         BCTR  R1,0                DECREMENT OPTION LENGTH                      
         LA    R8,1(R8)                                                         
         B     VR260                                                            
*                                                                               
         BCTR  R1,0                FIND OPTION IN KEYTAB                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R8),KTKEYWRD                                                 
         BNE   VR270                                                            
*                                                                               
         LLC   RE,KTCNTDSP         OPTION FOUND                                 
*                                  AFFECTS CTROOPT1, CTROOPT2...                
*                                  ...CTROOPT3, AND CTROOPT4                    
         LA    R0,E.CTROOPT1                                                    
         AR    RE,R0                                                            
         OC    0(1,RE),KTORFLAG                                                 
         B     VR290                                                            
*                                                                               
VR270    LA    R4,KEYTABLQ(R4)     TRY NEXT OPTION                              
         B     VR250                                                            
         DROP  R4                                                               
*                                                                               
VR280    LA    RF,PARMTBLQ(RF)     TRY NEXT PARAMETER                           
         B     VR240                                                            
         DROP  RF                                                               
*                                                                               
VR290    LLC   RE,0(R2)            NEXT LOOKUP FIELD                            
         AR    R2,RE                                                            
         CR    R2,R5                                                            
         BE    VR300               END OF LOOKUP FIELDS                         
*                                                                               
         TM    1(R2),X'20'                                                      
         BO    VR290               SKIP PROTECTED FIELDS                        
         B     VR210                                                            
*                                                                               
VR300    DS    0H                                                               
         GOTO1 ADDELEM                                                          
         DROP  E                                                                
*                                                                               
VRX      DS    0H                                                               
         DROP  R6                                                               
         EJECT                                                                  
****************************************************************                
*                    DISPLAY RECORD                                             
****************************************************************                
*                                                                               
DR       DS    0H                                                               
         GOTOR CLRSCRN                                                          
         L     R6,AIO                                                           
         MVI   ELCODE,CTRMECDQ     X'04' ELEMENT                                
*                                                                               
         MVI   BYTE,0              FLAG TO SIGNAL ELEMENT LIST FOUND            
         BRAS  RE,GETEL                                                         
         USING CTRMD,R6                                                         
         BNE   DR100                                                            
*                                                                               
         MVI   DCNLTYP,C'I'        ASSUME MARKET LIST IS INVALID                
         MVC   DCNLTYD,=CL31'LIST CONTAINS *INVALID* MARKETS'                   
         CLI   CTRMTYPE,CTRMTPI    INVALID LIST?                                
         BE    DR20                YES                                          
         CLI   CTRMTYPE,CTRMTPV    VALID LIST?                                  
         BE    *+6                                                              
         DC    H'0'                INVALID LIST TYPE VALUE                      
*                                                                               
         MVI   DCNLTYP,C'V'        ASSUME THERE'S A REAL MARKET LIST            
         MVC   DCNLTYD,=CL31'LIST CONTAINS *VALID* MARKETS'                     
         CLI   CTRMLEN,CTRMFXLQ+2  IS THERE ONLY ONE MARKET IN LIST?            
         BNE   DR20                                                             
         CLC   CTRMMKTS,=AL2(999)  IS IT MARKET 999 ?                           
         BNE   DR20                                                             
*                                                                               
         MVI   DCNLTYP,C'N'        YES: NO MARKETS ARE VALID                    
         MVC   DCNLTYD,=CL31'*NO* MARKETS VALID'                                
         B     DR120                                                            
*                                                                               
DR20     MVI   BYTE,C'Y'           SIGNAL VALID/INVALID ELEMENT FOUND           
*                                                                               
         LA    R5,CTRMMKTS         SORT MARKETS ALPHABETICALLY                  
         LLC   R1,CTRMLEN                                                       
         SHI   R1,CTRMFXLQ                                                      
         LR    R0,R1               L'MARKET NUMBER ARRAY                        
         SRL   R0,1                # OF MARKETS=LENGTH/2                        
         LR    R2,R0               SAVE # OF MARKETS                            
*                                                                               
         L     R4,AIO3             BUILD TRANSLATED ARRAY IN AIO3               
DR25     DS    0H                                                               
         XC    DMCB(24),DMCB                                                    
         ST    R5,DMCB             A(MARKET NUMBER)                             
         ST    R4,DMCB+4           A(ALPHA MKT DESTINATION)                     
         ST    R6,DMCB+8           A(BEGINNING OF ELEMENT)                      
         GOTOR GETALPHA,DMCB                                                    
         LA    R5,L'CTRMMKTS(R5)   BUMP TO NEXT MARKET                          
         LA    R4,L'NETALPH(R4)    BUMP TO NEXT OUTPUT POSITION                 
         BCT   R0,DR25                                                          
         DROP  R6                                                               
*                                                                               
         GOTO1 XSORT,DMCB,AIO3,(R2),L'NETALPH,L'NETALPH,0                       
*                                                                               
         L     R5,AIO3             A(SORTED ALPHA MARKETS)                      
         LA    R4,DCNMKTS          BEGINNING OF MARKET LIST                     
         LA    R3,DCNMKTSH                                                      
*                                                                               
DR30     LLC   RE,0(R3)            CHECK IF ENOUGH ROOM ON LINE                 
         AR    RE,R3                                                            
         SHI   RE,L'NETALPH        LENGTH OF ALPHA MARKET                       
         CR    R4,RE                                                            
         BL    DR40                                                             
         LR    RF,R4                                                            
         SHI   RF,1                BEFORE WE TRANSMIT LINE GET RID OF           
         MVI   0(RF),0             LAST COMMA AT END OF LINE                    
         OI    6(R3),X'80'                                                      
         AHI   RE,L'NETALPH        LENGTH OF ALPHA MARKET                       
         LR    R3,RE                                                            
         LR    R4,RE                                                            
*                                                                               
DR40     DS    0H                                                               
         CR    R4,R3               NEW LINE                                     
         BNE   *+16                                                             
         TM    1(R4),X'20'         PROTECTED FIELD?                             
         BO    DR50                YES                                          
         LA    R4,8(R4)            PAST HEADER                                  
*                                                                               
         CLI   0(R5),C'0'          IS THIS A NUMERIC MARKET CODE?               
         BL    DR45                NO                                           
         MVC   ALPHAMKT,0(R5)      YES: SUPPRESS LEADING ZEROES                 
         EDIT  (C6,ALPHAMKT),(L'NETALPH,0(R5)),ALIGN=LEFT                       
*                                                                               
DR45     DS    0H                                                               
         MVC   0(L'NETALPH,R4),0(R5) MOVE IN A MARKET CODE                      
         CLI   0(R4),C' '                                                       
         BNH   *+12                                                             
         LA    R4,1(R4)                                                         
         B     *-12                                                             
*                                                                               
         MVI   0(R4),C','                                                       
         LA    R4,1(R4)                                                         
*                                                                               
         LA    R5,L'NETALPH(R5)                                                 
         BCT   R2,DR30             ANY MORE MARKETS TO DISPLAY? YES.            
*                                                                               
         LR    RF,R4               CURRENT DISPLAY POSITION                     
         SHI   RF,1                BACK UP ONE CHARACTER                        
         MVI   0(RF),0             REMOVE COMMA AT END OF LINE                  
         OI    6(R3),X'80'         XMIT LINE                                    
         B     DR100                                                            
*                                                                               
DR50     DS    0H                  BUMP TO NEXT UNPROTECTED FIELD               
         LLC   RE,0(R3)                                                         
         AR    R4,RE                                                            
         LR    R3,R4                                                            
         LA    R4,8(R4)                                                         
         B     DR30                                                             
*                                                                               
DR100    CLI   BYTE,C'Y'           ANY VALID/INVALID ELEMENT FOUND?             
         BE    *+18                YES                                          
         MVI   DCNLTYP,C'A'        NO: ALL MARKETS ARE AUTHORIZED               
         MVC   DCNLTYD,=CL31'*ALL* MARKETS VALID'                               
         B     DR120                                                            
*                                                                               
         LR    RF,R4               CURRENT DISPLAY POSITION                     
         SHI   RF,1                BACK UP ONE CHARACTER                        
         MVI   0(RF),0             REMOVE LAST COMMA ON LINE                    
         OI    6(R3),X'80'         XMIT FIELD                                   
*                                                                               
DR120    DS    0H                                                               
         OI    DCNLTYPH+6,X'80'    XMIT MARKET LIST TYPE FIELD                  
         OI    DCNLTYDH+6,X'80'    XMIT MARKET LIST TYPE DESC. FIELD            
*                                                                               
         MVI   ELCODE,CTROECDQ     DEMO CONTROL ELEMENT                         
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   DR300                                                            
*                                                                               
*** DISPLAY LOOKUP OPTIONS                                                      
         USING CTROD,R6                                                         
         LARL  R3,PARMTAB                                                       
         USING PARMTABD,R3                                                      
DR130    CLI   0(R3),0             TEST E-O-T                                   
         BE    DR300                                                            
*                                                                               
         ICM   R8,15,PARMAKWD                                                   
         A     R8,RELO             R8=A(KEYWORD TABLE)                          
         USING KEYTABD,R8                                                       
*                                                                               
DR140    CLI   0(R8),0             TEST E-O-T                                   
         BE    DR160                                                            
         LLC   RE,KTCNTDSP                                                      
         LA    RE,CTROOPT1(RE)     RE=A(CONTROL BYTE IN ELEMENT)                
         LLC   RF,KTORFLAG                                                      
         EX    RF,*+8              RF=OR MASK                                   
         B     *+8                                                              
         TM    0(RE),0             TEST IF BIT ON                               
         BZ    DR150                                                            
*                                                                               
         SR    R2,R2                                                            
         ICM   R2,3,PARMDISP                                                    
         LA    R1,DCNSRCH                                                       
         AR    R2,R1                                                            
         MVI   8(R2),C'='                                                       
         MVC   9(8,R2),KTKEYWRD                                                 
         OI    6(R2),X'80'                                                      
         B     DR160                                                            
*                                                                               
DR150    LA    R8,KEYTABLQ(R8)     BUMP TO NEXT KEY TABLE ENTRY                 
         B     DR140                                                            
         DROP  R8                                                               
*                                                                               
DR160    LA    R3,PARMTBLQ(R3)     BUMP TO NEXT PARM TABLE ENTRY                
         B     DR130                                                            
         DROP  R3                                                               
         DROP  R6                                                               
*                                                                               
DR300    DS    0H                  DISPLAY LAST ACTIVE DATE                     
         MVI   ELCODE,X'F1'        SFM ACTIVITY ELEMENT                         
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   DR310                                                            
         USING ACTVD,R6                                                         
         LA    R3,ACTVCHDT                                                      
         DROP  R6                                                               
         B     DR320                                                            
*                                                                               
DR310    MVI   ELCODE,X'01'        LFM ACTIVITY ELEMENT                         
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   DR350                                                            
         LA    R3,2(R6)                                                         
*                                                                               
DR320    MVC   CONHEAD(34),=C'RECORD DISPLAYED - LAST ACTIVE ON '               
         CLI   ACTNUM,ACTCHA                                                    
         BNE   *+10                                                             
         MVC   CONHEAD(19),=C'RECORD CHANGED -   '                              
         GOTO1 DATCON,DMCB,(3,0(R3)),(8,CONHEAD+34)                             
         OI    CONHEADH+6,X'80'                                                 
         OI    GENSTAT2,USMYOK     USE THIS MESSAGE INSTEAD                     
*                                                                               
DR350    J     EXIT                                                             
         EJECT                                                                  
****************************************************************                
*                    DISPLAY KEY                                                
****************************************************************                
*                                                                               
DK       DS    0H                                                               
         L     R6,AIO                                                           
         USING CTRREC,R6                                                        
*                                                                               
         LARL  RE,SRCMEDT                                                       
         USING SRCMEDTD,RE                                                      
         MVC   HALF(1),CTRKSRC                                                  
         MVC   HALF+1(1),CTRKMED                                                
DK10     CLI   0(RE),0                                                          
         BNE   *+6                                                              
         DC    H'0'                BAD RECD ON FILE!                            
         CLC   HALF,SRCMEDSM                                                    
         BE    *+12                                                             
         LA    RE,SRCMEDTQ(RE)                                                  
         B     DK10                                                             
*                                                                               
         MVC   DCNSRC,SRCMEDSN     DISPLAY SOURCE                               
         OI    DCNSRCH+6,X'80'                                                  
         MVC   DCNSUBF,SRCMEDMN    DISPLAY MEDIA                                
         OI    DCNSUBFH+6,X'80'                                                 
         DROP  RE                                                               
*                                                                               
         XC    DCNAGYC,DCNAGYC     AGENCY CODE (PROTECTED)                      
         XC    DCNAUID,DCNAUID     AGENCY CODE / USER-ID COMBO                  
         OI    DCNAGYCH+6,X'80'                                                 
         OI    DCNAUIDH+6,X'80'                                                 
*                                                                               
         CLC   CTRKAGY,=X'FFFF'    DEFAULT AGENCY?                              
         BE    DK20                YES                                          
         CLC   CTRKAGY,=C'AA'      AGENCY CODE IN KEY?                          
         BL    *+14                                                             
         MVC   DCNAUID(2),CTRKAGY  YES: DISPLAY IT                              
         B     DK20                                                             
*                                                                               
         MVC   SVBINAGY,CTRKAGY    IT'S A USERID                                
         GOTOR VALBUSER                                                         
*********BE    *+6                                                              
*********DC    H'0'                                                             
         EDIT  CTRKAGY,(L'DCNAUID,DCNAUID),ALIGN=LEFT                           
         CLC   SVALPUID,=CL10'**********' IF WE HAVE AN ALPHA USERID...         
         BE    DK20                                                             
         MVC   DCNAUID,SVALPUID           ...THEN DISPLAY IT                    
         MVC   DCNAGYC,=C'(  )'    ENCLOSE AGENCY CODE IN PARENTHESES           
         MVC   DCNAGYC+1(2),SVAPHAGY ...AND DISPLAY IT (PROTECTED)              
*                                                                               
DK20     DS    0H                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(1),CTRKSRC                                                  
         MVC   WORK+1(1),CTRKMED                                                
         MVC   MYHALF,CTRKBOOK                                                  
         XC    MYHALF,=X'FFFF'                                                  
         GOTOR WKTODT                                                           
         LLC   RE,DUB2                                                          
         XC    DCNBOOK,DCNBOOK                                                  
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   DCNBOOK(0),DUB                                                   
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         USING CTRREC,R6                                                        
         MVC   KEY,SAVEKEY                                                      
         XC    SAVEKEY,SAVEKEY                                                  
         OI    DCNBOOKH+6,X'80'                                                 
*                                                                               
         XC    DCNBTYP,DCNBTYP                                                  
         OI    DCNBTYPH+6,X'80'                                                 
         CLI   CTRKCODE,X'FF'      DEFAULT BOOKTYPE?                            
         BE    DK40                YES                                          
*                                                                               
         L     RF,ACOMFACS         LOOK UP BOOKTYPE                             
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,SPBOOKTB  GET A(BOOKTYPE TABLE)                        
         ICM   RF,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                BAD TABLEID PASSED                           
         L     R0,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
*                                                                               
DK30     DS    0H                                                               
         USING SPBKTYPD,RF                                                      
         CLI   0(RF),X'FF'         EOT?                                         
         BNE   *+6                                                              
         DC    H'0'                INVALID BOOKTYPE IN RECORD                   
*                                                                               
         CLC   SPBKTYPN,CTRKCODE   MATCH ON BOOKTYPE?                           
         BE    *+10                                                             
         AR    RF,R0               NO: TRY NEXT                                 
         B     DK30                                                             
*                                                                               
         MVC   DCNBTYP,SPBKTYPA                                                 
         OI    DCNBTYPH+6,X'80'                                                 
         DROP  RF                                                               
*                                                                               
DK40     DS    0H                                                               
         XC    DCNCLI,DCNCLI                                                    
         OI    DCNCLIH+6,X'80'                                                  
         CLC   CTRKCLI,=X'FFFFFF'                                               
         BE    *+14                                                             
         MVC   DCNCLI,CTRKCLI                                                   
         OI    DCNCLIH+6,X'80'                                                  
*                                                                               
         J     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*            LIST RECORD                                                        
***********************************************************************         
*                                                                               
LR       L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         LA    R4,KEY                                                           
         USING CTRREC,R4                                                        
         OC    KEY,KEY             DISPLAY FROM SELECT?                         
         BNZ   LR10                READ RECORD                                  
*                                                                               
         XC    KEY,KEY                                                          
         MVI   CTRKTYP,CTRKTEQU    'R' RECORDS                                  
         MVC   CTRKSRC,SVSOURCE    IF PROVIDED, START W/ SOURCE FILTER          
*                                                                               
LR10     XC    PREVKEY,PREVKEY                                                  
*                                                                               
LR30     DS    0H                                                               
         GOTO1 HIGH                                                             
*                                                                               
LR50     DS    0H                                                               
         CLC   KEY(CTRKSRC-CTRKEY),KEYSAVE    DCON RECORD?                      
         BNE   LRX                 NO: WE'RE DONE                               
*                                                                               
         CLI   DCOSRCH+5,0         SOURCE FILTER PROVIDED?                      
         BE    LR80                NO (THEREFORE NO SUBFIELD EITHER)            
         CLC   CTRKSRC,SVSOURCE    YES: ONLY LIST RECS FOR THIS SOURCE          
         BNE   LRX                                                              
*                                                                               
         CLI   DCOSUBFH+5,0        SUBFIELD FILTER PROVIDED?                    
         BE    LR80                NO                                           
         CLC   CTRKMED,SVSUBF      MATCH?                                       
         BE    LR80                YES                                          
*                                                                               
         LLC   R0,CTRKMED          NO: BUMP UP MEDIA BY BINARY 1...             
         AHI   R0,1                                                             
         STC   R0,CTRKMED                                                       
         XC    CTRKAGY(L'CTRKAGY+L'CTRKCODE+L'CTRKCLI+L'CTRKBOOK),CTRKA+        
               GY                  ...AND CLEAR REMAINDER OF KEY                
         B     LR30                TRY READING AGAIN                            
*                                                                               
LR80     DS    0H                                                               
         CLI   BKOPTN,C'Y'         IF OPTION IS OFF                             
         BE    LR100               PRINT ONLY LATEST BOOK                       
         CLC   PREVKEY(CTRKBOOK-CTRKEY),KEY                                     
         BE    LR300                                                            
         MVC   PREVKEY(CTRKBOOK-CTRKEY),KEY                                     
*                                                                               
LR100    DS    0H                                                               
         MVC   HALF(1),CTRKSRC     CHECK MATCH ON SOURCE/MEDIA NAME             
         MVC   HALF+1(1),CTRKMED                                                
         LARL  RE,SRCMEDT                                                       
         USING SRCMEDTD,RE                                                      
LR110    CLI   0(RE),0                                                          
         BNE   *+6                                                              
         DC    H'0'                BAD RECORD                                   
*                                                                               
         CLC   HALF,SRCMEDSM                                                    
         BE    *+12                                                             
         LA    RE,SRCMEDTQ(RE)                                                  
         B     LR110                                                            
*                                                                               
         MVC   RECSNAME,SRCMEDSN                                                
         MVC   RECMNAME,SRCMEDMN                                                
         DROP  RE                                                               
*                                                                               
         LLC   R1,DCOSRCH+5                                                     
         CHI   R1,1                                                             
         BNH   LR120               SOURCE CODE (NOT NAME) OR NONE               
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   DCOSRC(0),RECSNAME                                               
         BNE   LR300               DIFF SOURCE NAME. SKIP IT                    
*                                                                               
LR120    LLC   R1,DCOSUBFH+5                                                    
         CHI   R1,1                                                             
         BNH   LR130               MEDIA CODE (NOT NAME) OR NONE                
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   DCOSUBF(0),RECMNAME                                              
         BNE   LR300               DIFF SOURCE NAME. SKIP IT                    
*                                                                               
LR130    DS    0H                                                               
         CLI   DCOAGYCH+5,0        FILTER ON AGENCY CODE?                       
         BE    LR150               NO                                           
         CLC   DCOAGYC,=C'**'      YES: SHOW DEFAULT AGENCY ONLY?               
         BNE   *+18                                                             
         CLC   CTRKAGY,=X'FFFF'    YES                                          
         BE    LR150                                                            
         B     LR300                                                            
*                                                                               
         CLC   CTRKAGY,=C'AA'      ALPHA?                                       
         BNL   LR140                                                            
         MVC   SVBINAGY,CTRKAGY                                                 
         GOTOR VALBUSER            NO                                           
*********BE    *+6                                                              
*********DC    H'0'                                                             
         CLC   DCOAGYC,SVAPHAGY                                                 
         BNE   LR300                                                            
         B     LR150                                                            
LR140    CLC   CTRKAGY,DCOAGYC     YES                                          
         BNE   LR300                                                            
*                                                                               
LR150    CLI   DCOBTYPH+5,0        ANY BOOKTYPE FILTER?                         
         BE    LR160               NO                                           
*                                                                               
         CLC   DCOBTYP,=C'**'      FILTER ON DEFAULT BOOKTYPE?                  
         BNE   *+16                                                             
         CLI   CTRKCODE,X'FF'      YES                                          
         BNE   LR300                                                            
         B     LR160                                                            
*                                                                               
         CLC   CTRKCODE,SVBKTYPE   MATCH ON INTERNAL BOOKTYPE CODE              
         BNE   LR300                                                            
*                                                                               
LR160    DS    0H                                                               
         CLI   DCOUIDH+5,0         ANY USERID FILTER?                           
         BE    LR190               NO                                           
*                                                                               
         CLI   DCOUIDH+5,2         FILTER ON DEFAULT USERID?                    
         BNE   LR170                                                            
         CLC   =C'**',DCOUID                                                    
         BNE   LR170                                                            
         CLC   CTRKAGY,DCOAGYC     YES                                          
         BNE   LR300                                                            
         B     LR190                                                            
*                                                                               
LR170    DS    0H                                                               
         TM    DCOUIDH+4,X'08'                                                  
         BNO   LR180                                                            
         XC    DUB,DUB             FILTER ON NUMERIC USER ID                    
         LLC   R1,DCOUIDH+5                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,DCOUID(0)                                                    
         CVB   R0,DUB                                                           
         STCM  R0,3,DUB                                                         
         CLC   CTRKAGY,DUB                                                      
         BNE   LR300                                                            
         B     LR190                                                            
*                                                                               
LR180    CLC   CTRKAGY,=X'FFFF'    FILTER ON ALPHA USER ID                      
         BE    LR300                                                            
         CLC   CTRKAGY,=C'AA'                                                   
         BNL   LR300                                                            
         MVC   SVBINAGY,CTRKAGY    RECORD IS FOR AN USER ID                     
         GOTOR VALBUSER                                                         
*********BE    *+6                                                              
*********DC    H'0'                BUGGY CODE. RECD IN LIST IS VALID            
         LLC   R1,DCOUIDH+5                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   DCOUID(0),SVALPUID                                               
         BNE   LR300                                                            
*                                                                               
LR190    CLI   DCOMFILH+5,0                                                     
         BE    LR200                                                            
         OI    DCOMFILH+6,X'80'                                                 
         GOTOR GETMFIL             SVBMKT  BINARY MKT AFTER CALL                
         GOTO1 HIGH                RESTORE SEQUENCE FOR READSQ                  
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR MKFILTER                                                         
         BNE   LR300                                                            
LR200    DS    0H                                                               
*                                                                               
         CLI   DCOLFILH+5,0                                                     
         BE    LR210                                                            
         OI    DCOLFILH+6,X'80'                                                 
         GOTOR GETLFIL                                                          
         GOTOR LKFILTER                                                         
         BNE   LR300                                                            
LR210    DS    0H                                                               
*                                                                               
         MVC   LISTAR,SPACES       CLEAR LIST LINE                              
         CLC   CTRKAGY,=C'AA'      ALPHA?                                       
         BNL   LR220               YES                                          
         MVC   SVBINAGY,CTRKAGY                                                 
         GOTOR VALBUSER            NO. GET ALPHA AGENCY AND USER ID             
*********BE    *+6                                                              
*********DC    H'0'                                                             
         MVC   LSTAGY,SVAPHAGY                                                  
         MVC   LSTUID,SVALPUID                                                  
         B     *+10                                                             
LR220    MVC   LSTAGY,CTRKAGY                                                   
         MVC   LSTSRC,RECSNAME                                                  
         MVC   LSTSUBF,RECMNAME                                                 
*                                                                               
         MVC   HALF,CTRKBOOK                                                    
         XC    HALF,=X'FFFF'                                                    
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(1),CTRKSRC                                                  
         MVC   WORK+1(1),CTRKMED                                                
         MVC   MYHALF,CTRKBOOK                                                  
         XC    MYHALF,=X'FFFF'                                                  
         GOTOR WKTODT                                                           
         LLC   RE,DUB2                                                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   LSTBOOK(0),DUB                                                   
*                                                                               
         CLI   CTRKCODE,X'FF'      DEFAULT BOOKTYPE?                            
         BE    LR240               YES                                          
*                                                                               
         L     RF,ACOMFACS         LOOK UP BOOKTYPE                             
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,SPBOOKTB  GET A(BOOKTYPE TABLE)                        
         ICM   RF,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                BAD TABLEID PASSED                           
         L     R0,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
*                                                                               
LR230    DS    0H                                                               
         USING SPBKTYPD,RF                                                      
         CLI   0(RF),X'FF'         EOT?                                         
         BNE   *+6                                                              
         DC    H'0'                INVALID BOOKTYPE IN RECORD                   
*                                                                               
         CLC   SPBKTYPN,CTRKCODE   MATCH ON BOOKTYPE?                           
         BE    *+10                                                             
         AR    RF,R0               NO: TRY NEXT                                 
         B     LR230                                                            
*                                                                               
         MVC   LSTBTYP,SPBKTYPA    ALPHA BOOKTYPE                               
         DROP  RF                                                               
*                                                                               
LR240    DS    0H                                                               
         MVC   LSTCLI,CTRKCLI      CLIENT                                       
         DROP  R4                                                               
*                                                                               
         L     R6,AIO              A(DCON RECORD)                               
         MVI   ELCODE,CTRMECDQ     X'04' ELEMENT                                
         BRAS  RE,GETEL                                                         
         BE    *+14                                                             
         MVC   LSTMKTAU,=CL8'ALL'  NONE: ALL MARKETS ARE AUTHORIZED             
         B     LR250                                                            
*                                                                               
         USING CTRMD,R6                                                         
         MVC   LSTMKTAU,=CL8'INVALID' ASSUME MARKET LIST IS INVALID             
         CLI   CTRMTYPE,CTRMTPI    INVALID LIST?                                
         BE    LR250               YES                                          
         CLI   CTRMTYPE,CTRMTPV    VALID LIST?                                  
         BE    *+6                                                              
         DC    H'0'                INVALID LIST TYPE VALUE                      
*                                                                               
         MVC   LSTMKTAU,=CL8'VALID' ASSUME THERE'S A REAL MARKET LIST           
         CLI   CTRMLEN,CTRMFXLQ+2  IS THERE ONLY ONE MARKET IN LIST?            
         BNE   LR250                                                            
         CLC   CTRMMKTS,=AL2(999)  IS IT MARKET 999 ?                           
         BNE   LR250                                                            
*                                                                               
         MVC   LSTMKTAU,=CL8'NONE' YES: NO MARKETS ARE VALID                    
         DROP  R6                                                               
*                                                                               
LR250    DS    0H                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   LR270                                                            
*                                                                               
*** BUILD REPORT LINE                                                           
*                                                                               
         LA    R5,HEDSPECS                                                      
         ST    R5,SPECS                                                         
*                                                                               
         MVC   P,SPACES                                                         
         MVC   PRTSRC,LSTSRC                                                    
         MVC   PRTSUBF,LSTSUBF                                                  
         MVC   PRTAGY,LSTAGY                                                    
         MVC   PRTBOOK,LSTBOOK                                                  
         MVC   PRTBTYP,LSTBTYP                                                  
         MVC   PRTCLI,LSTCLI                                                    
         MVC   PRTUID,LSTUID                                                    
         MVC   PRTMKTAU,LSTMKTAU                                                
         GOTO1 SPOOL,DMCB,SPOOLD                                                
*                                                                               
*  PRINT VALID/INVALID MARKETS                                                  
         LA    R2,DCODETLH                                                      
         CLI   5(R2),0                                                          
         BE    LRRP300                                                          
         CLI   8(R2),C'Y'                                                       
         BNE   LRRP300                                                          
*                                                                               
         MVI   BYTE,CTRMTPV                                                     
LRRP100  MVI   TITLEQ,0            FLAG FOR TITLE                               
         LA    R4,P+28             R4 POINTS TO DESTINATION OF NEXT             
         L     R6,AIO               MARKET ON PRINT LINE                        
         MVI   ELCODE,CTRMECDQ     X'04' ELEMENT                                
         BRAS  RE,GETEL                                                         
         BNE   LRRP180                                                          
         USING CTRMD,R6                                                         
         CLC   CTRMTYPE,BYTE                                                    
         BNE   LRRP150                                                          
         CLI   TITLEQ,C'Y'                                                      
         BE    LRRP110                                                          
         CLI   BYTE,CTRMTPV                                                     
         BNE   *+14                                                             
         MVC   P+10(17),=C'VALID MARKETS   :'                                   
         B     *+10                                                             
         MVC   P+10(17),=C'INVALID MARKETS :'                                   
         MVI   TITLEQ,C'Y'                                                      
LRRP110  LA    R5,CTRMMKTS                                                      
LRRP120  LA    RE,P                                                             
         AHI   RE,L'P-10                                                        
         CR    R4,RE               END OF LINE?                                 
         BL    LRRP130                                                          
         GOTO1 SPOOL,DMCB,SPOOLD                                                
         MVC   P,SPACES                                                         
         LA    R4,P+28                                                          
LRRP130  LR    R1,R5               R5 POINTS TO NEXT MARKET                     
         CLC   0(2,R1),=H'999'                                                  
         BNE   LRRP140                                                          
         MVC   P+28(10),=C'** NONE **'                                          
         GOTO1 SPOOL,DMCB,SPOOLD                                                
         B     LRRP200                                                          
LRRP140  XC    DMCB(24),DMCB                                                    
         ST    R1,DMCB             A(MARKET NUMBER)                             
         ST    R4,DMCB+4           A(ALPHA MKT DESTINATION)                     
         ST    R6,DMCB+8           A(BEGINNING OF ELEMENT)                      
         GOTOR GETALPHA,DMCB                                                    
         GOTO1 HIGH                RESTORE SEQUENCE                             
         CLC   KEY(L'CTRKEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   0(R4),C'0'          IS THIS A NUMERIC MARKET CODE?               
         BL    LRRP145             NO                                           
         MVC   ALPHAMKT,0(R4)      YES: SUPPRESS LEADING ZEROES                 
         EDIT  (C6,ALPHAMKT),(L'NETALPH,0(R4)),ALIGN=LEFT                       
*                                                                               
LRRP145  DS    0H                                                               
         CLI   0(R4),C' '                                                       
         BNH   *+12                                                             
         LA    R4,1(R4)                                                         
         B     *-12                                                             
*                                                                               
         MVI   0(R4),C','                                                       
         LA    R4,1(R4)                                                         
*                                                                               
         LA    R5,L'CTRMMKTS(R5)                                                
         LLC   RE,CTRMLEN                                                       
         AR    RE,R6                                                            
         CR    R5,RE               ANY MORE MARKETS IN ELEMENT?                 
         BL    LRRP120                                                          
         DROP  R6                                                               
*                                                                               
LRRP150  DS    0H                                                               
         LA    RE,P+28                                                          
         CR    R4,RE                                                            
         BE    LRRP160             NO MARKETS TO DISPLAY                        
         BCTR  R4,0                                                             
         MVI   0(R4),C' '          DELETE LAST COMMA                            
         GOTO1 SPOOL,DMCB,SPOOLD                                                
*                                                                               
LRRP160  CLI   BYTE,CTRMTPV                                                     
         BNE   LRRP200                                                          
         MVI   BYTE,CTRMTPI                                                     
         B     LRRP100                                                          
*                                                                               
LRRP180  MVC   P+10(17),=C'VALID MARKETS   :'                                   
         MVC   P+28(9),=C'** ALL **'                                            
         GOTO1 SPOOL,DMCB,SPOOLD                                                
*                                                                               
*  PRINT LOOKUP OPTIONS                                                         
LRRP200  MVC   P+10(17),=C'LOOKUP OPTIONS  :'                                   
         LA    R4,P+28             POINT TO WHERE TO PRINT NEXT OPTION          
         L     R6,AIO                                                           
         MVI   ELCODE,CTROECDQ     DEMO CONTROL ELEMENT                         
         BRAS  RE,GETEL                                                         
         BNE   LRRP290                                                          
*                                                                               
         USING CTROD,R6                                                         
         LARL  R3,PARMTAB                                                       
         USING PARMTABD,R3                                                      
LRRP230  CLI   0(R3),0             TEST E-O-T                                   
         BE    LRRP280                                                          
         ICM   R2,15,PARMAKWD                                                   
         A     R2,RELO             R2=A(KEYWORD TABLE)                          
         USING KEYTABD,R2                                                       
*                                                                               
LRRP240  CLI   0(R2),0             TEST E-O-T                                   
         BE    LRRP270                                                          
         LLC   RE,KTCNTDSP                                                      
         LA    RE,CTROOPT1(RE)     RE=A(CONTROL BYTE IN ELEMENT)                
         LLC   RF,KTORFLAG                                                      
         EX    RF,*+8              RF=OR MASK                                   
         B     *+8                                                              
         TM    0(RE),0             TEST IF BIT ON                               
         BZ    LRRP260                                                          
*                                                                               
         LA    RE,P                                                             
         AHI   RE,L'P-18                                                        
         CR    R4,RE               END OF LINE?                                 
         BL    LRRP250                                                          
         GOTO1 SPOOL,DMCB,SPOOLD                                                
         MVC   P,SPACES                                                         
         LA    R4,P+28                                                          
*                                                                               
LRRP250  MVC   0(8,R4),PARMNAME                                                 
         CLI   0(R4),C' '                                                       
         BNH   *+12                                                             
         LA    R4,1(R4)                                                         
         B     *-12                                                             
         MVI   0(R4),C'='                                                       
         LA    R4,1(R4)                                                         
         MVC   0(8,R4),KTKEYWRD                                                 
         CLI   0(R4),C' '                                                       
         BNH   *+12                                                             
         LA    R4,1(R4)                                                         
         B     *-12                                                             
         MVI   0(R4),C','                                                       
         LA    R4,1(R4)                                                         
         B     LRRP270                                                          
*                                                                               
LRRP260  LA    R2,KEYTABLQ(R2)     BUMP TO NEXT KEY TABLE ENTRY                 
         B     LRRP240                                                          
         DROP  R2                                                               
*                                                                               
LRRP270  LA    R3,PARMTBLQ(R3)     BUMP TO NEXT PARM TABLE ENTRY                
         B     LRRP230                                                          
         DROP  R3                                                               
*                                                                               
LRRP280  DS    0H                                                               
         LA    RE,P+28                                                          
         CR    R4,RE                                                            
         BE    LRRP290             NO OPTIONS TO DISPLAY                        
         DROP  R6                                                               
*                                                                               
         BCTR  R4,0                                                             
         MVI   0(R4),C' '          DELETE LAST COMMA                            
         GOTO1 SPOOL,DMCB,SPOOLD                                                
*                                                                               
LRRP290  MVC   P,SPACES                                                         
         GOTO1 SPOOL,DMCB,SPOOLD                                                
         B     LR300                                                            
*                                                                               
LRRP300  MVI   8(R2),C'N'                                                       
         OI    6(R2),X'80'                                                      
         B     LR300                                                            
*** END OF REPORT LINES                                                         
*                                                                               
LR270    GOTO1 LISTMON             SEND LINE TO SCREEN                          
*                                                                               
         LA    R2,DCODETLH                                                      
         MVI   8(R2),C'N'          NO DETAILS FOR LIST                          
         OI    6(R2),X'80'                                                      
*                                                                               
LR300    DS    0H                                                               
         GOTO1 HIGH                RESTORE SEQUENCE                             
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 SEQ                 NEXT DCON RECORD                             
         LA    R4,KEY              POINT R4 BACK TO KEY                         
         B     LR50                                                             
*                                                                               
LRX      DS    0H                                                               
         J     EXIT                                                             
         DROP  R8                                                               
         EJECT                                                                  
***********************************************************************         
*        VARIOUS SUBROUTINES                                                    
***********************************************************************         
*                                                                               
SETUP    NTR1                                                                   
         MVI   NLISTS,11           NO OF LINES ON LIST SCREEN                   
         OI    CONSERVH+1,X'01'    MODIFY SERVICE REQUEST                       
         OI    CONSERVH+6,X'80'    TRANSMIT TO GET CONTROL                      
         OI    GENSTAT2,DISTHSPG                                                
         OI    GENSTAT4,CONFDEL    CONFIRM DELETES                              
         OI    GENSTAT4,NODELLST   NO DEL ALLOWED FROM LIST                     
         OI    GENSTAT5,NOCHGLST   NO CHANGES FROM LIST                         
*                                                                               
         L     R3,ACOMFACS         A(COMFACS)                                   
         USING COMFACSD,R3                                                      
         CLC   CDEFINE,=F'0'       IS A(DEFINE) SET IN COMFACS?                 
         BNE   SETUP50             YES                                          
*                                                                               
         GOTO1 CCALLOV,DMCB,0,X'D9000A26'  NO: GET A(DEFINE)                    
         CLI   DMCB+4,X'FF'        WAS A(DEFINE) RETURNED FROM CALLOV?          
         BNE   *+6                                                              
         DC    H'0'                NO                                           
         MVC   CDEFINE,DMCB        SAVE A(DEFINE) IN COMFACS                    
*                                                                               
SETUP50  DS    0H                                                               
         MVC   VDEFINE,CDEFINE     SAVE A(DEFINE) IN WORKING STORAGE            
         DROP  R3                                                               
*                                                                               
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        TABLES AND CONSTANTS                                                   
***********************************************************************         
*                                                                               
         DS    0H                                                               
NETTBL   DS    0XL8                NETWORK MKTS                                 
NETMKT   DS    0XL2                2BYTE MKT #, 6BYTE ALPHA MKT NAME            
NETALPH  DS    0XL6                                                             
*NTI                                                                            
         DC    AL2(001),C'NTI   '  GRANTS ACCESS TO BOTH NTI + NTINSS           
         DC    AL2(002),C'NTIAGG'  NTI AGGR DATA (05 RECDS)                     
         DC    AL2(003),C'NSS   '  NEW NSS ACCESS (2007)                        
         DC    AL2(004),C'NCAA  '  ACESS TO NCAA SPECIAL TAPE                   
         DC    AL2(210),C'NTIC  '    N/A ANY LONGER                             
         DC    AL2(211),C'NTIA  '    N/A ANY LONGER                             
         DC    AL2(221),C'DAY   '  NTI DAILY DATA                               
         DC    AL2(222),C'DAYAGG'  NTI AGGR DAILY DATA (05 RECDS)               
         DC    AL2(215),C'WB1   '  WB TCAR                                      
         DC    AL2(216),C'TCAR  '  MAIN TCAR                                    
         DC    AL2(217),C'ACMWB '  ACMWB                                        
*COMMERCIAL AVERAGE (ACM)                                                       
         DC    AL2(100),C'ACMN  '  COMMERCIAL AVERAGE NTI                       
         DC    AL2(101),C'ACMS  '  COMMERCIAL AVERAGE NSS                       
         DC    AL2(102),C'ACMC  '  COMMERCIAL AVERAGE CABLE                     
*                                                                               
         DC    AL2(103),C'AMRLD '  ALL MINUTE RESPONDENT LEVEL DATA             
*CABLE                                                                          
         DC    AL2(300),C'CABLE '  <--POST 9337 REGULAR CABLE                   
         DC    AL2(321),C'CNAD  '  CABLE NAD                                    
         DC    AL2(010),C'CNN   '                                               
         DC    AL2(011),C'WTBS  '                                               
         DC    AL2(012),C'USA   '                                               
         DC    AL2(013),C'CBN   '                                               
         DC    AL2(014),C'ESPN  '                                               
         DC    AL2(015),C'MTV   '                                               
         DC    AL2(016),C'NICK  '                                               
         DC    AL2(017),C'AEN   '                                               
         DC    AL2(018),C'LFTM  '                                               
*NAD                                                                            
         DC    AL2(200),C'NAD   '  NAD MONTHLY                                  
         DC    AL2(201),C'NAW   '  NAD WEEKLY                                   
         DC    AL2(202),C'NADPC '  MONTHLY OPTIONAL PC DATA                     
         DC    AL2(203),C'NAWPC '  WKLY OPTIONAL PC DATA                        
         DC    AL2(205),C'FSTNAD'  FAST NAD WEEKLY DATA                         
         DC    AL2(207),C'MOVIE '  NAD WEEKLY MOVIEGOER                         
         DC    AL2(400),C'NADS  '  MONTHLY NAD-NSS                              
         DC    AL2(401),C'NAWS  '  WKLY NAD-NSS                                 
         DC    AL2(402),C'NADSPC'  MONTHLY NAD-NSS OPTIONAL PC DATA             
         DC    AL2(403),C'NAWSPC'  WKLY NAD-NSS OPTIONAL PC DATA                
*NHTI                                                                           
         DC    AL2(500),C'NHTI  '                                               
         DC    AL2(501),C'NHTGMN'  NHTI GENERAL MARKETS                         
         DC    AL2(502),C'NHTAFF'  NHTI AFFILIATES                              
         DC    AL2(503),C'FSTNHT'  FAST WEEKLY NHTI                             
*NHT CABLE                                                                      
         DC    AL2(510),C'NHTGAL'  <--CABLE NHTI GALAVISION                     
         DC    AL2(511),C'NHTCBL'  <--CABLE NHTI                                
*                                                                               
* NEW SETTINGS FOR THE HISPANIC DATA FROM NATIONAL SAMPLE                       
         DC    AL2(512),C'HPM   '  NTIH                                         
         DC    AL2(513),C'HPMGMN'  NTIH GENERAL MARKETS                         
         DC    AL2(514),C'HPMAFF'  NTIH AFFILIATES                              
         DC    AL2(515),C'FSTHPM'  FAST WEEKLY NTIH                             
         DC    AL2(516),C'HPMGAL'  <--CABLE NTIH GALAVISION                     
         DC    AL2(517),C'HPMCBL'  <--CABLE NTIH                                
*                                                                               
* NEW SETTINGS FOR TEMP SOURCE OOH/OOHC                                         
         DC    AL2(518),C'OOHN  '  <--ACCESS TO OOH NTI FILE                    
         DC    AL2(519),C'OOHC  '  <--ACCESS TO OOH NHI FILE                    
*                                                                               
         DC    X'FFFF',XL6'FF'                                                  
NETTBLQ  EQU   (*-NETTBL)/L'NETTBL NUMBER OF NET TABLE ENTRIES                  
         EJECT                                                                  
*  TABLE OF VALID SOURCE/MEDIA COMBOS                                           
*  2002 - CHANGED FORMAT OF ALL BOOKS TO WEEKLY                                 
*  ANYTHING PRIOR TO 2002 WILL KEEP THE OLD FORMATS - SEE SMTAB                 
         DS    0H                                                               
SMTAB02  DS    0CL(SMTABL)                                                      
         DC    C'AC',C'W',VL4(NSIWEEK),VL4(NSIWEEK),AL1(1),AL1(SAT)             
         DC    C'NC',C'W',VL4(NSIWEEK),VL4(NSIWEEK),AL1(1),AL1(SAT)             
         DC    C'AR',C'W',VL4(NSIWEEK),VL4(NSIWEEK),AL1(1),AL1(SAT)             
         DC    C'RR',C'W',VL4(NSIWEEK),VL4(NSIWEEK),AL1(1),AL1(SAT)             
         DC    C'MR',C'W',VL4(NSIWEEK),VL4(NSIWEEK),AL1(1),AL1(SAT)             
         DC    C'NR',C'W',VL4(NSIWEEK),VL4(NSIWEEK),AL1(1),AL1(SAT)             
         DC    C'AT',C'W',VL4(NSIWEEK),VL4(NSIWEEK),AL1(1),AL1(SAT)             
         DC    C'NT',C'W',VL4(NSIWEEK),VL4(NSIWEEK),AL1(1),AL1(SAT)             
         DC    C'MT',C'W',VL4(NSIWEEK),VL4(NSIWEEK),AL1(1),AL1(SAT)             
         DC    C'ST',C'W',VL4(NSIWEEK),VL4(NSIWEEK),AL1(1),AL1(SAT)             
         DC    C'NA',C'W',VL4(NSIWEEK),VL4(NSIWEEK),AL1(1),AL1(SAT)             
         DC    C'NN',C'W',VL4(NETWEEK),VL4(NETUNBK),AL1(2),AL1(0)               
         DC    C'NB',C'W',VL4(NETWEEK),VL4(NETUNBK),AL1(2),AL1(0)               
         DC    C'NW',C'W',VL4(NSIWEEK),VL4(NSIWEEK),AL1(1),AL1(SAT)             
         DC    C'HN',C'W',VL4(NETWEEK),VL4(NETUNBK),AL1(2),AL1(0)               
         DC    C'HW',C'W',VL4(NETWEEK),VL4(NETUNBK),AL1(2),AL1(0)               
         DC    C'SR',C'W',VL4(NSIWEEK),VL4(NSIWEEK),AL1(1),AL1(SAT)             
         DC    C'NU',C'W',VL4(NSIWEEK),VL4(NSIWEEK),AL1(1),AL1(SAT)             
         DC    C'NO',C'W',VL4(NSIWEEK),VL4(NSIWEEK),AL1(1),AL1(MON)             
         DC    C'FT',C'W',VL4(NSIWEEK),VL4(NSIWEEK),AL1(1),AL1(SAT)             
         DC    C'AU',C'W',VL4(NSIWEEK),VL4(NSIWEEK),AL1(1),AL1(SAT)             
         DC    C'TR',C'W',VL4(NSIWEEK),VL4(NSIWEEK),AL1(1),AL1(SAT)             
         DC    C'VT',C'W',VL4(NSIWEEK),VL4(NSIWEEK),AL1(1),AL1(SAT)             
         DC    C'IR',C'W',VL4(NSIWEEK),VL4(NSIWEEK),AL1(1),AL1(SAT)             
         DC    C'CT',C'W',VL4(NSIWEEK),VL4(NSIWEEK),AL1(1),AL1(SAT)             
         DC    X'00'                                                            
*                                                                               
* TABLE OF VALID SOURCE/MEDIA COMBOS - PRIOR TO 2002                            
         DS    0H                                                               
SMTAB    DS    0CL(SMTABL)                                                      
         DC    C'AC',C'M',AL4(0),AL4(0),AL1(0),AL1(0)                           
         DC    C'NC',C'M',AL4(0),AL4(0),AL1(0),AL1(0)                           
         DC    C'AR',C'M',AL4(0),AL4(0),AL1(0),AL1(0)                           
         DC    C'RR',C'M',AL4(0),AL4(0),AL1(0),AL1(0)                           
         DC    C'MR',C'M',AL4(0),AL4(0),AL1(0),AL1(0)                           
         DC    C'NR',C'M',AL4(0),AL4(0),AL1(0),AL1(0)                           
         DC    C'AT',C'M',AL4(0),AL4(0),AL1(0),AL1(0)                           
         DC    C'NT',C'M',AL4(0),AL4(0),AL1(0),AL1(0)                           
         DC    C'MT',C'M',AL4(0),AL4(0),AL1(0),AL1(0)                           
         DC    C'ST',C'M',AL4(0),AL4(0),AL1(0),AL1(0)                           
         DC    C'NA',C'M',AL4(0),AL4(0),AL1(0),AL1(0)                           
         DC    C'NN',C'W',VL4(NETWEEK),VL4(NETUNBK),AL1(2),AL1(0)               
         DC    C'NB',C'W',VL4(NETWEEK),VL4(NETUNBK),AL1(2),AL1(0)               
         DC    C'NW',C'W',VL4(NSIWEEK),VL4(NSIWEEK),AL1(1),AL1(SAT)             
         DC    C'HN',C'M',AL4(0),AL4(0),AL1(0),AL1(0)                           
         DC    C'HW',C'W',VL4(NETWEEK),VL4(NETUNBK),AL1(2),AL1(0)               
         DC    C'SR',C'W',VL4(NSIWEEK),VL4(NSIWEEK),AL1(1),AL1(SAT)             
         DC    C'NU',C'M',AL4(0),AL4(0),AL1(0),AL1(0)                           
         DC    C'NO',C'M',AL4(0),AL4(0),AL1(0),AL1(0)                           
         DC    C'FT',C'M',AL4(0),AL4(0),AL1(0),AL1(0)                           
         DC    C'AU',C'M',AL4(0),AL4(0),AL1(0),AL1(0)                           
         DC    C'VT',C'M',AL4(0),AL4(0),AL1(0),AL1(0)                           
         DC    C'IR',C'M',AL4(0),AL4(0),AL1(0),AL1(0)   IHEART                  
         DC    C'CT',C'M',AL4(0),AL4(0),AL1(0),AL1(0)   NCM                     
         DC    X'00'                                                            
         EJECT                                                                  
SMTABD   DSECT                                                                  
SMSM     DS    CL2                 SOURCE/MEDIA                                 
SMWM     DS    C                   WEEKLY OR MONTHLY                            
SMADW    DS    AL4                 A(DATE TO WEEK CNVRSN) OR NULLS              
SMAWD    DS    AL4                 A(WEEK TO DATE CNVRSN) OR NULLS              
SMPNUM   DS    AL1                 NTH PARM WHICH HAS A(RETURNED DATE)          
SMWKST   DS    AL1                 WEEK START (MON OR SAT) FOR NSIWEEK          
SAT      EQU   0                                                                
MON      EQU   1                                                                
SMTABL   EQU   *-SMTABD                                                         
*                                                                               
PARMTABD DSECT                                                                  
PARMNAME DS    CL8                 PARAMETER NAME                               
PARMAKWD DS    AL4                 A(KEYWORD LIST)                              
PARMDISP DS    AL2                 DISPLACEMENT TO TWA FIELD                    
PARMTBLQ EQU   *-PARMTABD                                                       
         SPACE 2                                                                
TA0A34   CSECT                                                                  
*                                                                               
* TABLE OF VALID PARAMETERS                                                     
PARMTAB  DS    0H                                                               
         DC    C'FAST    ',AL4(KEYFAST),AL2(DCNFASTH-DCNSRCH)                   
         DC    C'P+S2    ',AL4(KEYPPS2),AL2(DCNPS2H-DCNSRCH)                    
         DC    C'PROG    ',AL4(KEYPROG),AL2(DCNPROGH-DCNSRCH)                   
         DC    C'XQHR    ',AL4(KEYXQHR),AL2(DCNXQHRH-DCNSRCH)                   
         DC    C'TPT     ',AL4(KEYTPT),AL2(DCNTPTH-DCNSRCH)                     
         DC    C'PAV     ',AL4(KEYPAV),AL2(DCNPAVH-DCNSRCH)                     
         DC    C'NTI     ',AL4(KEYNTI),AL2(DCNNTIH-DCNSRCH)                     
         DC    C'MPA     ',AL4(KEYMPA),AL2(DCNMPAH-DCNSRCH)                     
         DC    C'DPT     ',AL4(KEYDPT),AL2(DCNDPTH-DCNSRCH)                     
         DC    C'RDP     ',AL4(KEYRDP),AL2(DCNRDPH-DCNSRCH)                     
         DC    C'JUL211  ',AL4(KEY211),AL2(DCNJULH-DCNSRCH)                     
         DC    C'SHARES  ',AL4(KEYSHR),AL2(DCNSHRSH-DCNSRCH)                    
         DC    C'XSPILL  ',AL4(KEYXSPL),AL2(DCNXSPLH-DCNSRCH)                   
         DC    C'PUTS    ',AL4(KEYPUTS),AL2(DCNPUTSH-DCNSRCH)                   
         DC    C'ADI     ',AL4(KEYADIS),AL2(DCNADIH-DCNSRCH)                    
         DC    C'S7MIN   ',AL4(KEYS7MIN),AL2(DCNS7MIH-DCNSRCH)                  
         DC    C'E7MIN   ',AL4(KEYE7MIN),AL2(DCNE7MIH-DCNSRCH)                  
         DC    C'SPDOM   ',AL4(KEYSPDOM),AL2(DCNSPDMH-DCNSRCH)                  
         DC    X'00'                                                            
         EJECT                                                                  
KEYTABD  DSECT                                                                  
KTKEYWRD DS    CL8                 KEYWORD                                      
KTCNTDSP DS    AL1                 DISPLACEMENT TO CONTROL BYTE                 
KTORFLAG DS    AL1                 LOGICAL "OR" FLAG POSITION                   
KEYTABLQ EQU   *-KEYTABD                                                        
         SPACE 2                                                                
TA0A34   CSECT                                                                  
*                                                                               
* TABLE OF VALID KEYWORDS                                                       
*                                                                               
KEYFAST  DC    C'YES     ',AL1(0),X'40'                                         
         DC    C'NO      ',AL1(0),X'00'                                         
         DC    X'00'                                                            
KEYPPS2  DC    C'YES     ',AL1(0),X'20'                                         
         DC    C'NO      ',AL1(0),X'00'                                         
         DC    X'00'                                                            
KEYPROG  DC    C'2WEEK   ',AL1(0),X'10'                                         
         DC    C'4WEEK   ',AL1(0),X'08'                                         
         DC    C'DEFAULT ',AL1(0),X'00'                                         
         DC    X'00'                                                            
KEYXQHR  DC    C'YES     ',AL1(0),X'04'                                         
         DC    C'NO      ',AL1(0),X'00'                                         
         DC    X'00'                                                            
KEY211   DC    C'YES     ',AL1(0),X'02'                                         
         DC    C'NO      ',AL1(0),X'00'                                         
         DC    X'00'                                                            
KEYSHR   DC    C'TOTAL   ',AL1(0),X'01'                                         
         DC    C'QHR     ',AL1(0),X'00'                                         
         DC    X'00'                                                            
KEYTPT   DC    C'YES     ',AL1(1),X'80'                                         
         DC    C'NO      ',AL1(1),X'00'                                         
         DC    X'00'                                                            
KEYPAV   DC    C'YES     ',AL1(1),X'40'                                         
         DC    C'NO      ',AL1(1),X'00'                                         
         DC    X'00'                                                            
KEYNTI   DC    C'YES     ',AL1(1),X'20'                                         
         DC    C'NO      ',AL1(1),X'00'                                         
         DC    X'00'                                                            
KEYMPA   DC    C'YES     ',AL1(1),X'10'                                         
         DC    C'NO      ',AL1(1),X'00'                                         
         DC    X'00'                                                            
KEYDPT   DC    C'YES     ',AL1(1),X'08'                                         
         DC    C'NO      ',AL1(1),X'00'                                         
         DC    X'00'                                                            
KEYRDP   DC    C'YES     ',AL1(1),X'04'                                         
         DC    C'NO      ',AL1(1),X'00'                                         
         DC    X'00'                                                            
KEYXSPL  DC    C'YES     ',AL1(2),X'80'                                         
         DC    C'NO      ',AL1(2),X'00'                                         
         DC    X'00'                                                            
KEYPUTS  DC    C'2YEAR   ',AL1(2),X'40'                                         
         DC    C'1YEAR   ',AL1(2),X'00'                                         
         DC    X'00'                                                            
KEYADIS  DC    C'YES     ',AL1(2),X'20'                                         
         DC    C'NO      ',AL1(2),X'00'                                         
         DC    X'00'                                                            
KEYS7MIN DC    C'YES     ',AL1(2),X'10'                                         
         DC    C'NO      ',AL1(2),X'00'                                         
         DC    X'00'                                                            
KEYE7MIN DC    C'YES     ',AL1(2),X'08'                                         
         DC    C'NO      ',AL1(2),X'00'                                         
         DC    X'00'                                                            
KEYSPDOM DC    C'YES     ',AL1(2),X'04'                                         
         DC    C'NO      ',AL1(2),X'00'                                         
         DC    X'00'                                                            
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 2                                                                
* ERROR MESSAGES                                                                
         SPACE 2                                                                
INVLOKE  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVLOKEM),INVLOKEM                                     
         OI    6(R2),X'40'                                                      
         GOTO1 ERREX2                                                           
INVLOKEM DC    C'* ERROR * INVALID LOOKUP OPTION *'                             
*                                                                               
INVFMKT  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVFMKTM),INVFMKTM                                     
         OI    6(R2),X'40'                                                      
         GOTO1 ERREX2                                                           
INVFMKTM DC    C'* ERROR * INVALID FILTER KEYWORD *'                            
*                                                                               
BOOKERR  GOTOR CLRSCRN                                                          
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BOOKERRM),BOOKERRM                                     
         OI    6(R2),X'40'                                                      
         GOTO1 ERREX2                                                           
BOOKERRM DC    C'* ERROR * INVALID BOOK *'                                      
*                                                                               
MKTERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'MKTERRM),MKTERRM                                       
         EDIT  FNDX,(2,CONHEAD+L'MKTERRM+1),ALIGN=LEFT                          
         GOTO1 ERREX2                                                           
MKTERRM  DC    C'* ERROR * INVALID INPUT FIELD - FIELD#'                        
*                                                                               
MKTERD   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'MKTERDM),MKTERDM                                       
         EDIT  FNDX,(2,CONHEAD+L'MKTERDM+1),ALIGN=LEFT                          
         GOTO1 ERREX2                                                           
MKTERDM  DC    C'* ERROR * DUPLICATE MARKET ENTRY - FIELD#'                     
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         OI    6(R2),X'40'                                                      
         GOTO1 ERREX                                                            
         J     EXIT                                                             
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         GOTO1 ERREX                                                            
         J     EXIT                                                             
         EJECT                                                                  
* HEAD SPECS                                                                    
*                                                                               
HEDSPECS DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,58,REQUESTOR                                                  
         SSPEC H2,58,REPORT                                                     
         SSPEC H2,71,PAGE                                                       
*                                                                               
         SSPEC H1,30,C'DCON REPORT'                                             
         SSPEC H2,30,C'-----------'                                             
*                                                                               
         SSPEC H4,2,C'SOURCE'                                                   
         SSPEC H5,2,C'------'                                                   
         SSPEC H4,11,C'MEDIA'                                                   
         SSPEC H5,11,C'------'                                                  
         SSPEC H4,19,C'AGENCY'                                                  
         SSPEC H5,19,C'------'                                                  
         SSPEC H4,29,C'BOOK'                                                    
         SSPEC H5,27,C'--------'                                                
         SSPEC H4,37,C'BOOKTYPE'                                                
         SSPEC H5,37,C'--------'                                                
         SSPEC H4,48,C'CLIENT'                                                  
         SSPEC H5,48,C'------'                                                  
         SSPEC H4,56,C'USER-ID'                                                 
         SSPEC H5,56,C'-------'                                                 
         SSPEC H4,68,C'MKTAUTH'                                                 
         SSPEC H5,68,C'-------'                                                 
         DC    X'00'                                                            
*                                                                               
* LTORG                                                                         
         LTORG                                                                  
         SPACE 3                                                                
         DROP  RB,R7                                                            
         EJECT                                                                  
***********************************************************************         
* GETALPHA - GET ALPHA CODE FOR MKT                                             
*  DMCB       0(R8) = MKT #  (2BYTE)                                            
*  DMCB+4     0(R4) = DESTINATION FOR ALPHA MKT                                 
*  DMCB+8     0(R5) = BEGINING OF ELEMENT                                       
***********************************************************************         
*                                                                               
GETALPHA NTR1  BASE=(*,GETALPHX),LABEL=*                                        
*                                                                               
         L     R8,DMCB                                                          
         L     R4,DMCB+4                                                        
         L     R5,DMCB+8                                                        
*                                                                               
         XC    0(L'NETALPH,R4),0(R4) CLEAR OUTPUT AREA                          
*                                                                               
         CLI   2(R5),CTRMTPI       ALPHA MKT FOR VAL/INV LISTS ONLY             
         BE    *+12                NO ALPHA MKTS FOR MKT OVR LIST               
         CLI   2(R5),CTRMTPV                                                    
         BNE   GETALP50            MKT OVR - JUST EDIT OUTPUT MKT #             
*                                                                               
         L     RE,AIO                                                           
         USING CTRREC,RE                                                        
         CLI   CTRKMED,C'N'        FOR NETWORK,GET ALPHA NETTBL                 
         BE    GETALP3                                                          
         CLI   CTRKMED,C'W'        HISP WKLY IS NET ALSO                        
         BNE   GETALP20                                                         
         CLI   CTRKSRC,C'H'                                                     
         BNE   GETALP20                                                         
*                                                                               
GETALP3  LARL  R1,NETTBL           NETWORK ALPHA CODE                           
GETALP5  CLI   0(R1),X'FF'                                                      
         BE    GETALP50            NOT DEFINED, JUST OUTPUT MKT #               
         CLC   0(2,R8),0(R1)       MATCH ON MKT #?                              
         BE    *+12                                                             
         LA    R1,L'NETTBL(R1)                                                  
         B     GETALP5                                                          
         MVC   0(L'NETALPH,R4),2(R1) MOVE IN ALPHA MKT NAME                     
         B     GETALPX                                                          
*                                                                               
GETALP20 DS    0H                  CALL DEFINE FOR ALPHA MKT                    
         XC    BLOCK(256),BLOCK    FIELD 'BLOCK' USED FOR DBLOCK                
         USING DEDBLOCK,R1                                                      
         LA    R1,BLOCK                                                         
         MVC   DBINTMED,CTRKMED    MEDIA                                        
         MVC   DBACTSRC,CTRKSRC    SOURCE                                       
         MVC   DBSELRMK,0(R8)      NUMERIC MARKET CODE                          
         MVC   DBBTYPE,CTRKCODE    BOOKTYPE (OR X'FF')                          
         DROP  RE                                                               
         L     RF,ACOMFACS                                                      
         ST    RF,DBCOMFCS         A(COMFACS)                                   
         DROP  R1                                                               
*                                                                               
*                                  CONVERT NUMERIC TO ALPHA MARKET, BUT         
*                                  ...DON'T DEFAULT TO BOOKTYPE X'FF'           
         GOTO1 VDEFINE,DMCB,=C'NAXMKT',BLOCK,0(R4)                              
         CLC   0(3,R4),=C'   '     ALPHA MARKET WAS RETURNED?                   
         BH    GETALPX             YES                                          
*                                                                               
GETALP50 DS    0H                                                               
         SR    R0,R0               RETURN ZERO-PADDED MARKET NUMBER             
         ICM   R0,3,0(R8)                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(L'NETALPH,R4),DUB                                              
*                                                                               
GETALPX  DS    0H                                                               
         J     EXIT                                                             
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 3                                                                
GETALPHX EQU   *                                                                
         EJECT                                                                  
******************************************************************              
* DTTOWK - DATE TO WEEK CONVERSION                                              
*        AT ENTRY,                                                              
*          DUB       = DATE                                                     
*          WORK(2)   = SOURCE/MEDIA                                             
*          WORK+2(1) = L(INPUT DATE)                                            
*        AT EXIT,                                                               
*          CC = 0 IF VALID                                                      
*             <>0 IF INVALID                                                    
*          MYHALF =  CORRESPONDING BOOK DATE, IF VALID                          
******************************************************************              
*                                                                               
DTTOWK   NTR1  BASE=(*,DTTOWKX),LABEL=*                                         
*                                                                               
         MVI   DUB2,0              SET INPUT BITS                               
         OI    DUB2,PVINSGLO       SINGLE DATE ONLY IS VALID                    
         OI    DUB2,PVINSGLS       SINGLE DATE RETURNED AS SINGLE               
         GOTO1 PERVAL,DMCB,(WORK+2,DUB),(DUB2,PERVOUT)                          
         TM    4(R1),PVRCINV1      DATE ONE INVALID?                            
         BO    DTWNO               YEP, ERROR                                   
*                                                                               
         LA    RE,PERVOUT          RE-->PERVAL OUTPUT BLOCK                     
         USING PERVALD,RE                                                       
         TM    PVALASSM,PVALASM+PVALASY    ASSUMED MONTH OR YEAR?               
         BNZ   DTWNO               YEP, ERROR                                   
*                                                                               
*********CLI   PVALBSTA,YR_2002                                                 
*********BNL   DTW02                                                            
         CLC   PVALBSTA,=X'650C1D' DEC29/01                                     
         BNL   DTW02                                                            
         LARL  R2,SMTAB            PRIOR TO 2002                                
         B     DTW10                                                            
DTW02    LARL  R2,SMTAB02          2002 AND LATER                               
*                                                                               
         USING SMTABD,R2                                                        
DTW10    CLI   0(R2),0             IF AT END OF TABLE,                          
         BNE   *+6                                                              
         DC    H'0'                DIE!                                         
         CLC   SMSM,WORK                                                        
         BE    *+12                                                             
         LA    R2,SMTABL(R2)                                                    
         B     DTW10                                                            
         ICM   R1,15,SMADW                                                      
         BZ    *+8                                                              
         A     R1,RELO                                                          
         ST    R1,ADTWK            GET A(DATE TO WEEK CONVRSN)                  
*                                                                               
         OC    ADTWK,ADTWK                                                      
         BNZ   *+12                                                             
         TM    PVALASSM,PVALASD    VALIDATE FOR M/Y                             
         BZ    DTWNO               SO EXIT IF DAY NOT ASSUMED                   
*                                                                               
         MVC   MYDATE,PVALESTA     GET YYMMDD                                   
         DROP  RE                                                               
*                                                                               
         OC    ADTWK,ADTWK                                                      
         BNZ   DTW20               NEED TO VALIDATE SPECIALLY                   
*                                                                               
* MMMYY BOOK  - MONTHLY BOOKS (PRIOR TO 2002 ONLY)                              
         GOTO1 DATCON,DMCB,(0,MYDATE),(3,MYHALF)                                
         B     DTWYES              EXIT WITH GOOD CC                            
*                                                                               
* MMMDD/YY BOOK  - WEEKLY BOOKS                                                 
DTW20    DS    0H                                                               
         MVC   DMCB+4(4),GETDAY                                                 
         L     R0,=V(NSIWEEK)                                                   
         A     R0,RELO                                                          
         C     R0,ADTWK            IF ADTWK POINTS TO NSIWEEK,                  
         BNE   *+16                                                             
         MVC   DMCB+12(4),DATCON   SET 4TH PARM TO A(DATCON)                    
         MVC   DMCB+4(1),SMWKST    WEEK START (MON OR SAT) FOR NSIWEEK          
*                                                                               
         GOTO1 ADTWK,DMCB,MYDATE,,ADDAY                                         
*                                                                               
         MVC   MYHALF(1),4(R1)     YEAR                                         
         MVC   MYHALF+1(1),0(R1)   WEEK NO.                                     
         L     R0,=V(NETWEEK)                                                   
         A     R0,RELO                                                          
         C     R0,ADTWK            IF ADTWK POINTS TO NETWEEK,                  
         BNE   *+10                                                             
         MVC   MYHALF+1(1),8(R1)   WEEK NO.                                     
         B     DTWYES                                                           
*                                                                               
DTWNO    LA    RF,1                                                             
         B     *+6                                                              
DTWYES   SR    RF,RF                                                            
         LTR   RF,RF               RETURN WITH CONDITION CODE SET               
         J     EXIT                                                             
         DROP  R2                                                               
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 3                                                                
DTTOWKX  EQU   *                                                                
         EJECT                                                                  
*******************************************************************             
* WKTODT - WEEK TO DATE CONVERSION                                              
*                                                                               
*     AT ENTRY,                                                                 
*        WORK(2) = SOURCE/MEDIA                                                 
*        MYHALF = YEAR/WEEK                                                     
*     AT EXIT,                                                                  
*        DUB    = DATE IN MMM/YY OR MMMDD/YY FORMAT                             
*******************************************************************             
*                                                                               
WKTODT   NTR1  BASE=(*,WKTODTXX),LABEL=*                                        
         CLI   MYHALF,YR_2002                                                   
         BNL   *+14                                                             
         LARL  R2,SMTAB            PRIOR TO 2002                                
         B     WTD10                                                            
         LARL  R2,SMTAB02          2002 AND AFTER                               
*                                                                               
         USING SMTABD,R2                                                        
WTD10    CLI   0(R2),0             IF END-OF-TABLE,                             
         BNE   *+6                                                              
         DC    H'0'                DIE!                                         
         CLC   SMSM,WORK                                                        
         BE    *+12                                                             
         LA    R2,SMTABL(R2)                                                    
         B     WTD10                                                            
         ICM   R1,15,SMAWD                                                      
         BZ    *+8                                                              
         A     R1,RELO                                                          
         ST    R1,AWKDT            A(WEEK TO DATE CONVRSN)                      
*                                                                               
         XC    MYDATE,MYDATE                                                    
         OC    AWKDT,AWKDT         NEED SPECIAL CONVERSION?                     
         BNZ   WTD20               YES                                          
*                                                                               
* MMM/YY BOOK                                                                   
         GOTO1 DATCON,DMCB,(3,MYHALF),(0,MYDATE)                                
         OC    MYDATE,=C'000000'                                                
         MVI   DUB2,9                                                           
         B     WKTODTX             CONVERT TO MMM/YY AND EXIT                   
*                                                                               
* MMMDD/YY BOOK                                                                 
WTD20    LA    R4,MYHALF                                                        
         CLI   SMPNUM,2            SEE WHICH DATE TO WEEK CONV TO USE           
         BE    WTD20A                                                           
*                                                                               
         MVI   DUB2,C'D'           USE NSIWEEK                                  
         MVC   DMCB+4(4),GETDAY                                                 
         MVC   DMCB+4(1),SMWKST    WEEK START (MON OR SAT) FOR NSIWEEK          
         L     R6,ADDAY                                                         
         L     R3,DATCON                                                        
         SR    R5,R5                                                            
         B     WTD25                                                            
*                                                                               
WTD20A   MVI   DUB2,C'W'           USE NETUNBK                                  
         LA    RE,MYDATE                                                        
         ST    RE,DMCB+4                                                        
         L     R6,GETDAY                                                        
         L     R3,ADDAY                                                         
         L     R5,AGETBROD                                                      
*                                                                               
WTD25    GOTO1 AWKDT,DMCB,(DUB2,(R4)),,(R6),(R3),(R5)                           
         LLC   RE,SMPNUM           R0=PARM # OF A(RETURNED DATE),               
         BCTR  RE,0                 LESS ONE,                                   
         SLL   RE,2                 TIMES 4                                     
         AR    R1,RE                                                            
         L     R1,0(R1)            R1-->DATE                                    
         MVC   MYDATE,0(R1)        MYDATE = RETURNED DATE (YYMMDD)              
         MVI   DUB2,8                                                           
*                                                                               
WKTODTX  GOTO1 DATCON,DMCB,(X'80',MYDATE),(DUB2,DUB),0                          
         MVC   DUB2(1),4(R1)       L(OUTPUT DATE) IN DUB2                       
         J     EXIT                                                             
         DROP  R2                                                               
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 3                                                                
WKTODTXX EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
* VALNET - VALIDATE NETWORK MARKET NUMBER AGAINST NETTBL.                       
*             FNDMKT = MKT # TO VALIDATE                                        
*             R8     - PTS TO SCANTABLE ENTRY 12(R8)=ALPHA INPUT                
***********************************************************************         
*                                                                               
VALNET   NTR1  BASE=*,LABEL=*                                                   
         LARL  R1,NETTBL                                                        
         CLI   0(R8),6             MAX #CHAR FOR MKT NAME                       
         BH    VALNET8             INVALID INPUT                                
         LLC   RE,0(R8)            L'INPUT                                      
         BCTR  RE,0                                                             
         MVC   ALPHAMKT,=CL6' '                                                 
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ALPHAMKT(0),12(R8)                                               
*                                                                               
VALNET5  CLI   0(R1),X'FF'                                                      
         BE    VALNET8                                                          
         CLC   FNDMKT,0(R1)        MATCH ON MKT #?                              
         BE    VALNETX                                                          
         CLC   ALPHAMKT,2(R1)      MATCH ON (FULL) MKT NAME?                    
         BE    *+12                                                             
         LA    R1,L'NETTBL(R1)                                                  
         B     VALNET5                                                          
*                                                                               
         MVC   FNDMKT,0(R1)        SAVE MKT NUMBER                              
         MVC   6(2,R8),FNDMKT                                                   
         B     VALNETX                                                          
*                                                                               
VALNET8  XC    FNDMKT,FNDMKT       MKT # NOT FOUND -- CC=ZERO                   
*                                                                               
VALNETX  OC    FNDMKT,FNDMKT       SET CONDITION CODE                           
         J     EXIT                                                             
         SPACE 3                                                                
         DROP  RB                                                               
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALALPH - VALIDATE MARKET ALPHA NAME FROM ALPHA MARKET RECORDS                
*        R8     - PTS TO SCANTABLE ENTRY 12(R8)=ALPHA INPUT                     
*        UPON RETURN, 6(R8) WILL HOLD THE MKT # IF FOUND, 0'S OTHERWISE         
***********************************************************************         
*                                                                               
VALALPH  NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO3             READ ALPHA RECORD INTO BUFF2                 
         USING CTDMREC,R6                                                       
         L     R4,AIO                                                           
         USING CTRREC,R4                                                        
         XC    0(25,R6),0(R6)      CLEAR KEY FIELD AREA                         
         MVI   CTDMKTYP,CTDMKTEQ                                                
         MVI   CTDMKTY2,CTDMKT2E                                                
         MVC   CTDMKMED,CTRKMED                                                 
         MVC   CTDMKSRC,CTRKSRC                                                 
         MVC   CTDMKBKT,CTRKCODE   BOOKTYPE                                     
         MVC   CTDMKMKT,12(R8)                                                  
         XC    4(4,R8),4(R8)       CLR NUMERIC PART OF SCAN ENTRY               
         MVC   WORK,0(R6)                                                       
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE ',WORK,(R6)                    
         CLC   WORK(23),0(R6)                                                   
         BNE   *+10                                                             
         MVC   6(2,R8),CTDMKNUM    GET MARKET NUMBER                            
*                                                                               
         J     EXIT                                                             
         SPACE 3                                                                
         DROP  R6                                                               
         DROP  R4                                                               
         DROP  RB                                                               
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
****************************************************************                
*  ROUTINE TO DETERMINE IF THIS AGENCY CONTROL RECORD HAS THE FILTER            
* MKTS IN IT.  MFILTYES = OVERALL GOOD (HIT ON ALL MARKETS)                     
*              MFILTNO = NOT GOOD (NO HIT ON AT LEAST ONE MARKET)               
* MKFILTAB HOLDS THE MARKETS TO FILTER BY, AS GIVEN BY GETMFIL                  
****************************************************************                
*                                                                               
MKFILTER NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R3,MKFILTAB         TABLE OF INPUT FILTERS                       
         USING SVMKFILD,R3                                                      
*                                                                               
MFILT10  CLC   =X'FFFF',0(R3)      END OF MARKET FILTERS TABLE                  
         BE    MFILTYES            ALL YES, SO EXIT WITH YES                    
*                                                                               
         MVI   ELEMQ,0             STORE MARKET TYPE FROM ELEMENT               
*                                  V=VALID,I=INVALID                            
         L     R6,AIO                                                           
         XC    ELEM,ELEM                                                        
         MVI   ELCODE,CTRMECDQ     X'04' ELEMENT                                
         USING CTRMD,R6                                                         
         BRAS  RE,GETEL                                                         
         BE    *+16                                                             
         CLI   VALMKTQ,C'Y'        NO '04' ELEMENTS FOUND.                      
         BE    MFILT60             LOOKING FOR VALID MARKET                     
         BNE   MFILTNO             LOOKING FOR INVALID MARKET                   
*                                                                               
         LA    R5,CTRMMKTS         '04' ELEM FOUND                              
         B     MFILT50                                                          
*                                                                               
MFILT30  DS    0H                                                               
         CLI   VALMKTQ,C'Y'        **SEARCHING FOR A VALID MARKET               
         BNE   *+16                                                             
         CLI   ELEMQ,C'V'                                                       
         BE    MFILTNO             NO MATCH FOUND IN VALID MARKETS              
         B     MFILT60        (YES)NO MATCH FOUND IN INVALID MARKETS            
*                                                                               
         CLI   ELEMQ,C'V'          **SEARCHING FOR AN INVALID MKT               
         BE    MFILT60        (YES)NO MATCH FOUND IN VALID MARKETS              
         B     MFILTNO             NO MATCH FOUND IN INVALID MARKETS            
*                                                                               
MFILT40  LA    R5,L'CTRMMKTS(R5)   LOOK AT NEXT MARKET IN ELEMENT               
         LLC   RE,CTRMLEN                                                       
         AR    RE,R6                                                            
         CR    R5,RE                                                            
         BNL   MFILT30                                                          
*                                                                               
MFILT50  DS    0H                                                               
         CLI   VALMKTQ,C'Y'                                                     
         BE    MFILT52                                                          
         CLI   CTRMTYPE,CTRMTPV                                                 
         BE    MFILT53B                                                         
         CLI   CTRMTYPE,CTRMTPI                                                 
         BNE   MFILT30                                                          
         MVI   ELEMQ,C'I'          ELEM WITH INVALID MARKETS FOUND              
         B     MFILT53                                                          
MFILT52  CLI   CTRMTYPE,CTRMTPI                                                 
         BE    MFILT53A                                                         
         CLI   CTRMTYPE,CTRMTPV                                                 
         BNE   MFILT30                                                          
         MVI   ELEMQ,C'V'          ELEM WITH VALID MARKETS FOUND                
         DROP  R6                                                               
*                                                                               
MFILT53  DS    0H                  *LOOKING FOR VALID MKT IN VALID MKT          
         CLC   SVBMKT,0(R5)          ELEM - OR -                                
         BE    MFILT60              LOOKING FOR INVALID MKT IN INVALID          
         BNE   MFILT40               MKT ELEM                                   
*                                                                               
MFILT53A DS    0H                  *LOOKING FOR VALID MARKET IN ELEMENT         
*                                     WITH INVALID MARKETS                      
         CLI   ELEMQ,C'V'                                                       
         BE    MFILT30             IF VALID MKT ELEMS PRESENT,SKIP INVS         
         MVI   ELEMQ,C'I'                                                       
         CLC   SVBMKT,0(R5)                                                     
         BE    MFILTNO                                                          
         BNE   MFILT40                                                          
*                                                                               
MFILT53B DS    0H                  *LOOKING FOR INVALID MARKET IN ELEM          
*                                     WITH VALID MARKETS                        
         MVI   ELEMQ,C'V'                                                       
         CLC   SVBMKT,0(R5)                                                     
         BE    MFILTNO                                                          
         BNE   MFILT40                                                          
MFILT60  LA    R3,L'MKFILTAB(R3)   GET NEXT MARKET TO FILTER BY                 
         B     MFILT10                                                          
*                                                                               
MFILTYES DS    0H                                                               
         CR    RB,RB                                                            
         B     MKFILTRX                                                         
*                                                                               
MFILTNO  DS    0H                                                               
         CR    RB,RE                                                            
*                                                                               
MKFILTRX J     EXIT                                                             
         SPACE 3                                                                
         DROP RB                                                                
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
********************************************************************            
*  ROUTINE TO DETERMINE IF THIS AGENCY CONTROL RECORD HAS THE FILTER            
* LOOKUP OPTIONS IN IT.                                                         
*              MFILTYES = OVERALL GOOD (HIT ON ALL OPTIONS)                     
*              MFILTNO = NOT GOOD (NO HIT ON AT LEAST ONE OPTION)               
* LKFILTAB HOLDS THE OPTIONS TO FILTER BY, AS GIVEN BY GETLKFIL                 
********************************************************************            
*                                                                               
LKFILTER NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,DCOLFILH                                                      
         LA    R3,LKFILTAB                                                      
         USING SVLKFILD,R3                                                      
*                                                                               
LFILT05  CLC   =X'FFFF',0(R3)      END OF LOOKUP FILTERS TABLE                  
         BE    LFILTYES            ALL YES, SO EXIT WITH YES                    
*                                                                               
         L     R6,AIO                                                           
         XC    ELEM,ELEM                                                        
         MVI   ELCODE,CTROECDQ     DEMO CONTROL ELEMENT                         
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   LFILTNO                                                          
*                                                                               
         USING CTROD,R6                                                         
         ICM   R8,15,SVAKEYLK                                                   
         USING KEYTABD,R8                                                       
LFILT20  CLI   0(R8),0             TEST E-O-T                                   
         BE    LFILTNO                                                          
         CLC   SVOPTN,KTKEYWRD                                                  
         BNE   LFILT50                                                          
*                                                                               
         LLC   RE,KTCNTDSP                                                      
         LA    RE,CTROOPT1(RE)     RE=A(CONTROL BYTE IN ELEMENT)                
         LLC   RF,KTORFLAG                                                      
         EX    RF,*+8              RF=OR MASK                                   
         B     *+8                                                              
         TM    0(RE),0             TEST IF BIT ON                               
         BO    LFILT60                                                          
LFILT50  LA    R8,KEYTABLQ(R8)                                                  
         B     LFILT20                                                          
         DROP  R8                                                               
*                                                                               
LFILT60  LA    R3,L'LKFILTAB(R3)                                                
         B     LFILT05                                                          
         DROP  R6                                                               
*                                                                               
LFILTYES DS    0H                                                               
         CR    RB,RB                                                            
         B     LKFILTRX                                                         
*                                                                               
LFILTNO  DS    0H                                                               
         CR    RB,RE                                                            
*                                                                               
LKFILTRX J     EXIT                                                             
         SPACE 3                                                                
         DROP  R3                                                               
         DROP  RB                                                               
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
*VALBUSER-                                                                      
* READS ID RECORD AND GETS AGENCY ALPHA CODE FROM ID NUMBER                     
* INPUT: SVBINAGY SET TO BINARY USER ID                                         
* OUTPUT:SVAPHAGY SET TO TWO BYTE AGENCY CODE                                   
*        SVALPUID SET TO ALPHA USER ID                                          
*  CC SET TO EQUAL IF VALID ID                                                  
*  CC SET TO NOT EQUAL IF INVALID ID                                            
**********************************************************************          
*                                                                               
VALBUSER NTR1  BASE=*,LABEL=*                                                   
         MVC   SVAPHAGY,=C'**'     FOR INVALID USER ID                          
         MVC   SVALPUID,=CL10'**********'                                       
*                                                                               
         MVC   SVAIO,AIO                                                        
         MVC   AIO,AIO3                                                         
         MVC   MYKEY,KEY           SAVE CONTENTS OF KEY                         
         MVC   MYKEYSV,KEYSAVE                                                  
         L     RE,AIO              READ ALPHA RECORD INTO AIO3                  
         USING CTIREC,RE                                                        
         XC    0(25,RE),0(RE)      CLEAR KEY FIELD AREA                         
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,SVBINAGY                                                 
         MVC   KEY,0(RE)                                                        
         DROP  RE                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BNE   VALBNO                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,CTAGYELQ      AGENCY ALPHA ID ELEMENT '06'                
         BRAS  RE,GETEL                                                         
         BNE   VALBNO                                                           
         USING CTAGYD,R6                                                        
         MVC   SVAPHAGY,CTAGYID                                                 
         DROP  R6                                                               
*                                                                               
         MVC   SVALPUID,=CL10' '                                                
         L     R6,AIO                                                           
         MVI   ELCODE,CTDSCELQ      USER ID NAME ELEMENT '02'                   
         BRAS  RE,GETEL                                                         
         BNE   VALBYES                                                          
         USING CTDSCD,R6                                                        
         MVC   SVALPUID,CTDSC                                                   
*                                                                               
VALBYES  DS    0H                                                               
         CR    RB,RB                                                            
         B     VALBUSRX                                                         
*                                                                               
VALBNO   DS    0H                                                               
         SR    R0,R0                                                            
         CR    RB,R0                                                            
*                                                                               
VALBUSRX MVC   AIO,SVAIO                                                        
         MVC   KEY,MYKEY                                                        
         MVC   KEYSAVE,MYKEYSV                                                  
         J     EXIT                                                             
         SPACE 3                                                                
         DROP  R6                                                               
         DROP  RB                                                               
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
*VALAUSER-                                                                      
* READS ID RECORD AND AGENCY ID NUMBER FROM ALPHA USERID                        
* INPUT: R2 --> TWA FIELD HEADER WITH ALPHA USERID                              
* OUTPUT:SVAPHAGY SET TO TWO BYTE AGENCY CODE                                   
*        SVALPUID SET TO ALPHA USER ID                                          
*        SVBINAGY SET TO BINARY USER ID                                         
*  CC SET TO EQUAL IF VALID ID                                                  
*  CC SET TO NOT EQUAL IF INVALID ID                                            
**********************************************************************          
*                                                                               
VALAUSER NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   SVAPHAGY,=C'**'     FOR INVALID USER ID                          
         MVC   SVALPUID,=CL10'**********'                                       
         XC    SVBINAGY,SVBINAGY                                                
*                                                                               
         MVC   SVAIO,AIO                                                        
         MVC   AIO,AIO3                                                         
         MVC   MYKEY,KEY           SAVE CONTENTS OF KEY                         
         MVC   MYKEYSV,KEYSAVE                                                  
         L     RE,AIO              READ ID RECORD INTO AIO3                     
         USING CTIREC,RE                                                        
         XC    0(25,RE),0(RE)      CLEAR KEY FIELD AREA                         
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,=CL10' '     BLANK-PADDED USERID                          
         LLC   RF,5(R2)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CTIKID(0),8(R2)     ALPHA USERID FROM TWA FIELD                  
         MVC   KEY,0(RE)                                                        
         DROP  RE                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BNE   VALANO                                                           
*                                                                               
         MVC   SVALPUID,KEY+(CTIKID-CTIREC)  ALPHA USERID                       
         L     R6,AIO                                                           
         MVI   ELCODE,CTAGYELQ      AGENCY ALPHA ID ELEMENT '06'                
         BRAS  RE,GETEL                                                         
         BNE   VALANO                                                           
         USING CTAGYD,R6                                                        
         MVC   SVAPHAGY,CTAGYID                                                 
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,CTDSCELQ      USER ID NAME/NUMBER ELEMENT '02'            
         BRAS  RE,GETEL                                                         
         BNE   VALAYES                                                          
         USING CTDSCD,R6                                                        
         MVC   SVBINAGY,CTDSC       USERID NUMBER                               
*                                                                               
VALAYES  DS    0H                                                               
         CR    RB,RB                                                            
         B     VALAUSRX                                                         
*                                                                               
VALANO   DS    0H                                                               
         SR    R0,R0                                                            
         CR    RB,R0                                                            
*                                                                               
VALAUSRX MVC   AIO,SVAIO                                                        
         MVC   KEY,MYKEY                                                        
         MVC   KEYSAVE,MYKEYSV                                                  
         J     EXIT                                                             
         SPACE 3                                                                
         DROP  R6                                                               
         DROP  RB                                                               
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*&&DO                                                                           
**********************************************************************          
*VALAGY-                                                                        
* READS AGENCY ACCESS RECORD                                                    
* INPUT: FIELD 'HALF' CONTAINS 2-CHARACTER AGENCY CODE                          
*  CC SET TO EQUAL IF VALID ID                                                  
*  CC SET TO NOT EQUAL IF INVALID ID                                            
**********************************************************************          
*                                                                               
VALAGY   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   SVAIO,AIO                                                        
         MVC   AIO,AIO3                                                         
         MVC   MYKEY,KEY           SAVE CONTENTS OF KEY                         
         MVC   MYKEYSV,KEYSAVE                                                  
         L     RE,AIO              READ ID RECORD INTO AIO3                     
         USING CT5REC,RE                                                        
         XC    0(25,RE),0(RE)      CLEAR KEY FIELD AREA                         
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,HALF       AGENCY CODE                                  
         MVC   KEY,0(RE)                                                        
         DROP  RE                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BNE   VALAGNO                                                          
*                                                                               
         CR    RB,RB                                                            
         B     VALAGX                                                           
*                                                                               
VALAGNO  DS    0H                                                               
         SR    R0,R0                                                            
         CR    RB,R0                                                            
*                                                                               
VALAGX   MVC   AIO,SVAIO                                                        
         MVC   KEY,MYKEY                                                        
         MVC   KEYSAVE,MYKEYSV                                                  
         J     EXIT                                                             
         SPACE 3                                                                
         DROP  RB                                                               
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*&&                                                                             
**************************************************************                  
* NXTMKLIN - SUBROUTINE TO GET NEXT MKT INPUT LINE                              
*   AT ENTRY: R5 POINTS TO PREVIOUS FIELD                                       
*             R2 POINTS TO LAST LINE                                            
*   AT EXIT:  R5 POINTS TO NEXT NON-EMPTY FIELD                                 
*             CC=NE IF NO MORE NON-EMPTY FIELDS                                 
**************************************************************                  
*                                                                               
NXTMKLIN NTR1  BASE=*,LABEL=*                                                   
NXTM1    LLC   RE,0(R5)                                                         
         AR    R5,RE                                                            
         CR    R5,R2                                                            
         BH    NXTMNO                                                           
         TM    1(R5),X'20'                                                      
         BO    NXTM1                                                            
         CLI   5(R5),0                                                          
         BE    NXTM1                                                            
*                                                                               
         CR    RB,RB                                                            
         B     NXTMX                                                            
*                                                                               
NXTMNO   SR    R0,R0                                                            
         CR    R0,RB                                                            
*                                                                               
NXTMX    XIT1  REGS=(R5)                                                        
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* CHKPSWD - SUBROUTINE TO CHECK PASSWORD FOR UPDATIVE ACTIONS                   
**********************************************************************          
*                                                                               
CHKPSWD  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RF,ACOMFACS         GET FACPAK ID FROM GETFACT                   
         ICM   RF,15,CGETFACT-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,0                                                      
         L     RF,DMCB                                                          
         USING FACTSD,RF                                                        
         CLI   FASYSID,1           FACTST?                                      
         BE    CHKPSWDX            YES: NO PASSWORD REQUIRED                    
         DROP  RF                                                               
*                                                                               
         CLI   ACTNUM,ACTADD        REQUEST PASSWORD FOR UPDATIVE ACTNS         
         BE    CH200                                                            
         CLI   ACTNUM,ACTCHA                                                    
         BE    CH200                                                            
         CLI   ACTNUM,ACTDEL                                                    
         BE    CH200                                                            
         CLI   ACTNUM,ACTREST                                                   
         BE    CH200                                                            
         B     CHKPSWDX                                                         
*                                                                               
CH200    LA    R2,DCNPSWDH                                                      
         CLI   5(R2),0                                                          
         BE    MISPWER                                                          
         CLI   5(R2),6                                                          
         BNE   INVPWER                                                          
         GOTO1 DATCON,DMCB,(5,0),(20,DUB)     YYMMDD                            
         LA    R1,DUB+7                                                         
         LA    RE,DUB2                                                          
         LA    RF,6                                                             
         MVC   0(1,RE),0(R1)       REVERSE DATE INTO DUB2                       
         LA    RE,1(RE)                                                         
         BCTR  R1,0                                                             
         BCT   RF,*-12                                                          
         PACK  DUB,DUB2(6)                                                      
         ZAP   DUB2,=P'999999'                                                  
         SP    DUB2,DUB            AND GET 9'S COMPLEMENT                       
         UNPK  DUB(6),DUB2                                                      
         OI    DUB+5,X'F0'                                                      
         CLC   DUB(6),8(R2)        TEST IF PASSWORD VALID                       
         BNE   INVPWER                                                          
CHKPSWDX J     EXIT                                                             
*                                                                               
INVPWER  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVPWERM),INVPWERM                                     
         OI    6(R2),X'40'                                                      
         GOTO1 ERREX2                                                           
INVPWERM DC    C'* ERROR * INVALID PASSWORD *'                                  
*                                                                               
MISPWER  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'MISPWERM),MISPWERM                                     
         OI    6(R2),X'40'                                                      
         GOTO1 ERREX2                                                           
MISPWERM DC    C'* ERROR * PASSWORD IS REQUIRED FOR UPDATIVE ACTIONS *'         
         SPACE 3                                                                
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GETMFIL - SUBROUTINE TO GET INPUT MARKET FILTERS FROM SCREEN AND              
*           STORE THEM IN MKFILTAB - SEE DSECT SVMKFILD                         
***********************************************************************         
*                                                                               
GETMFIL  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,DCOMFILH                                                      
         LA    R3,MKFILTAB                                                      
         USING SVMKFILD,R3                                                      
         GOTO1 SCANNER,DMCB,(0,DCOMFILH),('MAXSCAN',SCANTAB)                    
         LLC   R0,DMCB+4                                                        
         CHI   R0,0                MISSING OR INVALID DATA                      
         BE    INVERR2                                                          
         USING SCANBLKD,R4                                                      
         LA    R4,SCANTAB                                                       
         MVI   FNDX,1                                                           
*                                                                               
GETMFL05 CLC   =C'VALID',SC1STFLD  VALID=MARKET                                 
         BNE   *+12                                                             
         MVI   VALMKTQ,C'Y'                                                     
         B     GETMFL40                                                         
*                                                                               
         CLC   =C'INVALID',SC1STFLD  INVALID=MARKET                             
         BNE   *+12                                                             
         MVI   VALMKTQ,C'N'                                                     
         B     GETMFL40                                                         
*                                                                               
         CLC   =X'404040',SC2NDFLD                                              
         BNE   INVFMKT2                                                         
         MVI   VALMKTQ,C'Y'        JUST MARKET(LIKE VALID=MKT)                  
         MVC   SC2NDFLD,SC1STFLD                                                
         MVC   SC2NDLEN,SC1STLEN                                                
GETMFL40 DS    0H                                                               
*                                                                               
         CLC   =X'404040',SC2NDFLD                                              
         BE    INVFMKT2                                                         
         CLC   =X'F0F0F0',SC2NDFLD                                              
         BH    GETMFL50                                                         
         LLC   RE,SC2NDLEN         NUMERIC MARKET INPUT                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,SC2NDFLD(0)                                                  
         CVB   RE,DUB                                                           
         STCM  RE,3,SVBMKT                                                      
         B     GETMFL60            NEXT MARKET FILTER                           
GETMFL50 DS    0H                                                               
*                                                                               
         XC    DMCB(24),DMCB       ALPHA MARKET INPUT                           
         OC    DMCB(6),=X'404040404040'                                         
         LLC   RE,SC2NDLEN                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   DMCB(0),SC2NDFLD                                                 
*                                                                               
         MVC   DMCB+12(1),SC2NDLEN MOVE IN LEGTH OF ALPHAMKT                    
         GOTOR GETBMKT,DMCB                                                     
         MVC   SVBMKT,DMCB                                                      
         OC    SVBMKT,SVBMKT                                                    
         BZ    MKTERR2                                                          
*                                                                               
GETMFL60 LA    R4,L'SCANTAB(R4)                                                 
         LA    R3,L'MKFILTAB(R3)                                                
         LLC   R1,FNDX             UPDATE INDEX NO OF MARKET                    
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
         BCT   R0,GETMFL05                                                      
*                                                                               
GETMFILX DS    0H                                                               
         MVC   0(2,R3),=X'FFFF'                                                 
         DROP  R3                                                               
         DROP  R4                                                               
         J     EXIT                                                             
*                                                                               
MKTERR2  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'MKTERRM2),MKTERRM2                                     
         EDIT  FNDX,(2,CONHEAD+L'MKTERRM2+1),ALIGN=LEFT                         
         GOTO1 ERREX2                                                           
MKTERRM2 DC    C'* ERROR * INVALID INPUT FIELD - FIELD#'                        
*                                                                               
INVFMKT2 XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVFMK2M),INVFMK2M                                     
         OI    6(R2),X'40'                                                      
         GOTO1 ERREX2                                                           
INVFMK2M DC    C'* ERROR * INVALID MARKET FILTER KEYWORD *'                     
*                                                                               
INVALMK  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVALMKM),INVALMKM                                     
         OI    6(R2),X'40'                                                      
         GOTO1 ERREX2                                                           
INVALMKM DC    C'* ERROR * INVALID ALPHA MARKET *'                              
*                                                                               
INVERR2  MVI   ERROR,INVALID                                                    
         OI    6(R2),X'40'                                                      
         GOTO1 ERREX                                                            
         J     EXIT                                                             
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GETLFIL - SUBROUTINE TO GET INPUT LOOKUP FILTERS FROM SCREEN AND              
*           STORE THEM IN LKFILTAB - SEE DSECT SVLKFILD                         
***********************************************************************         
*                                                                               
GETLFIL  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,DCOLFILH                                                      
         LA    R5,LKFILTAB                                                      
         USING SVLKFILD,R5                                                      
*                                                                               
         GOTO1 SCANNER,DMCB,(0,DCOLFILH),('MAXSCAN',SCANTAB)                    
         LLC   R0,DMCB+4                                                        
         CHI   R0,0                MISSING OR INVALID DATA                      
         BE    INVERR3                                                          
         USING SCANBLKD,R4                                                      
         LA    R4,SCANTAB                                                       
*                                                                               
GETLFL30 CLC   =X'404040',SC2NDFLD                                              
         BE    INVFLKT2                                                         
*                                                                               
         LARL  R3,PARMTAB                                                       
         USING PARMTABD,R3                                                      
         LLC   R1,SC1STLEN                                                      
         BCTR  R1,0                                                             
GETLFL60 CLI   0(R3),0             TEST E-O-T                                   
         BE    INVFLKT2                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SC1STFLD(0),PARMNAME                                             
         BE    *+12                                                             
         LA    R3,PARMTBLQ(R3)                                                  
         B     GETLFL60                                                         
*                                                                               
         ICM   R8,15,PARMAKWD                                                   
         A     R8,RELO             R8=A(KEYWORD TABLE)                          
         STCM  R8,15,SVAKEYLK                                                   
         USING KEYTABD,R8                                                       
         DROP  R3                                                               
*                                                                               
         LLC   R1,SC2NDLEN                                                      
         BCTR  R1,0                                                             
GETLFL70 CLI   0(R8),0                                                          
         BE    INVOPTN                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SC2NDFLD(0),KTKEYWRD                                             
         BE    *+12                                                             
         LA    R8,KEYTABLQ(R8)                                                  
         B     GETLFL70                                                         
         DROP  R8                                                               
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SVOPTN,SC2NDFLD                                                  
*                                                                               
         LA    R4,L'SCANTAB(R4)                                                 
         LA    R5,L'LKFILTAB(R5)                                                
         BCT   R0,GETLFL30                                                      
*                                                                               
GETLFILX MVC   0(2,R5),=X'FFFF'                                                 
         J     EXIT                                                             
         DROP  R5                                                               
         DROP  R4                                                               
*                                                                               
INVFLKT2 XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVFLK2M),INVFLK2M                                     
         OI    6(R2),X'40'                                                      
         GOTO1 ERREX2                                                           
INVFLK2M DC    C'* ERROR * INVALID LOOKUP FILTER KEYWORD *'                     
*                                                                               
INVOPTN  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVOPTNM),INVOPTNM                                     
         OI    6(R2),X'40'                                                      
         GOTO1 ERREX2                                                           
INVOPTNM DC    C'* ERROR * INVALID LOOKUP OPTION *'                             
*                                                                               
INVERR3  MVI   ERROR,INVALID                                                    
         OI    6(R2),X'40'                                                      
         GOTO1 ERREX                                                            
*                                                                               
         J     EXIT                                                             
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
****************************************************************                
* GETBMKT - GETS THE BINARY MKT GIVEN ALPHAMKT , MED, SRC                       
* ENTRY = DMCB HAS ALPHAMKT                                                     
* EXIT = VALID DMCB HAS 2 BYTES BINARY MKT                                      
*      = INVALID DMCB =0000                                                     
*******************************************************************             
*                                                                               
GETBMKT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   SVAIO,AIO                                                        
         MVC   AIO,AIO2                                                         
         LA    R6,KEY                                                           
         MVC   MYKEY,KEY                                                        
         USING CTDMREC,R6                                                       
         LA    RE,MYKEY            AIO POINTS TO ORGINAL RECORD                 
         USING CTRREC,RE                                                        
*                                                                               
         CLI   CTRKMED,C'N'        NET LOOK AT NETTBL                           
         BE    GETB300                                                          
         CLI   CTRKMED,C'W'        HISP WKLY IS NET ALSO                        
         BNE   *+12                                                             
         CLI   CTRKSRC,C'H'                                                     
         BE    GETB300                                                          
         DROP  RE                                                               
*                                                                               
         XC    0(25,R6),0(R6)      CLEAR KEY FIELD AREA                         
         MVI   CTDMKTYP,CTDMKTEQ                                                
         MVI   CTDMKTY2,CTDMKT2E                                                
         MVC   CTDMKMED,CTRKMED-CTRREC(RE)                                      
         MVC   CTDMKSRC,CTRKSRC-CTRREC(RE)                                      
         MVC   CTDMKBKT,CTRKCODE-CTRREC(RE)                                     
         OC    CTDMKBKT,CTDMKBKT                                                
         BNZ   *+8                                                              
         MVI   CTDMKBKT,X'FF'                                                   
         MVC   CTDMKMKT,DMCB                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(CTDMKNUM-CTDMREC),KEYSAVE                                    
         BNE   GETB500             NO MATCH                                     
         XC    DMCB,DMCB                                                        
         MVC   DMCB(L'CTDMKNUM),CTDMKNUM                                        
         DROP  R6                                                               
         B     GETBX                                                            
*                                                                               
* FOR NET LOOK AT TABLE FOR MARKETS                                             
GETB300  DS    0H                                                               
         LARL  R1,NETTBL                                                        
GETB350  CLI   0(R1),X'FF'                                                      
         BE    GETB500             NOT DEFINED, JUST OUTPUT MKT #               
         CLC   DMCB(6),2(R1)       MATCH ON MKT NAME? 6-CHAR PADDED             
         BE    *+12                                                             
         LA    R1,L'NETTBL(R1)                                                  
         B     GETB350                                                          
         MVC   DMCB,0(R1)          MOVE IN BINARY MARKET                        
         B     GETBX                                                            
*                                                                               
GETB500  DS    0H                                                               
         XC    DMCB,DMCB                                                        
*                                                                               
GETBX    MVC   KEY,MYKEY                                                        
         MVC   AIO,SVAIO                                                        
         J     EXIT                                                             
         SPACE 2                                                                
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
********************************************************************            
* CLRSCRN - SUBROUTINE TO CLEAR SCREEN FIELDS                                   
********************************************************************            
*                                                                               
CLRSCRN  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    DCNLTYD,DCNLTYD     MARKET LIST TYPE DESCRIPTION FIELD           
         OI    DCNLTYDH+6,X'80'                                                 
*                                                                               
         LA    R2,DCNLTYPH         FIRST FIELD TO CLEAR                         
         LA    R3,DCNLSTXH         END OF TWA TAG FIELD                         
*                                                                               
CLRS20   DS    0H                                                               
         CR    R2,R3                                                            
         BE    CLRSX                                                            
*                                                                               
         LLC   RE,0(R2)                                                         
         TM    1(R2),X'20'         DON'T CLEAR PROTECTED FIELDS                 
         BO    CLR40                                                            
*                                                                               
         LR    R0,RE               SAVE A(FIELD)                                
         SHI   RE,8                8 BYTE HEADER                                
         TM    1(R2),X'02'                                                      
         BNO   *+8                                                              
         SHI   RE,8                SUBRACT 8 MORE FOR FIELD EXTENSION           
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         OI    6(R2),X'80'                                                      
         LR    RE,R0               RESTORE A(FIELD)                             
CLR40    AR    R2,RE                                                            
         B     CLRS20                                                           
*                                                                               
CLRSX    DS    0H                                                               
*                                                                               
         J     EXIT                                                             
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
SRCTABD  DSECT                                                                  
SRCNAME  DS    CL8                 SOURCE NAME                                  
SRCCODE  DS    C                   SOURCE CODE                                  
SRCTABLQ EQU   *-SRCTABD                                                        
         SPACE 2                                                                
TA0A34   CSECT                                                                  
*                                                                               
SRCTAB   DS    0H                                                               
         DC    C'NSI     ',C'N'    NSI SPOT                                     
         DC    C'NTI     ',C'N'    NSI NETWORK                                  
         DC    C'NHT     ',C'H'    NSI HISPANIC                                 
         DC    C'ARB     ',C'A'    ARB SPOT                                     
         DC    C'MFX     ',C'M'    MEDIAFAX SPOT                                
         DC    C'BBM     ',C'A'    BBM SPOT                                     
         DC    C'BIRCH   ',C'N'    BIRCH SPOT                                   
         DC    C'SRC     ',C'S'    SRC SPOT                                     
         DC    C'SQAD    ',C'S'    SQAD RADIO                                   
         DC    C'BBRADIO ',C'M'    BBM RADIO                                    
         DC    C'RADAR   ',C'R'    RADAR NETWORK RADIO                          
         DC    C'FUSION  ',C'F'    FUSION                                       
         DC    C'NATIONAL',C'N'    RENTRAK NATIONAL                             
         DC    C'TRITON  ',C'T'    TRITON                                       
         DC    C'VIDEOLOG',C'V'    VIDEOLOGY                                    
         DC    C'IHT     ',C'I'    IHEART                                       
         DC    C'NCM     ',C'C'    NCM                                          
         DC    X'FFFF'                                                          
         EJECT                                                                  
SUBFTABD DSECT                                                                  
SUBFNAME DS    CL8                 SUBFILE NAME                                 
SUBFCODE DS    C                   SUBFILE CODE                                 
SUBFTBLQ EQU   *-SUBFTABD                                                       
         SPACE 2                                                                
TA0A34   CSECT                                                                  
*                                                                               
SUBFTAB  DS    0H                                                               
         DC    CL8'TV      ',C'T'                                               
         DC    CL8'USTV    ',C'T'                                               
         DC    CL8'CANTV   ',C'C'                                               
         DC    CL8'RADIO   ',C'R'                                               
         DC    CL8'NETTV   ',C'N'                                               
         DC    CL8'NAD     ',C'A'                                               
         DC    CL8'CABLE   ',C'B'                                               
         DC    CL8'WEEKLY  ',C'W'                                               
         DC    CL8'COUNTY  ',C'U'  COUNTY COVERAGE                              
         DC    CL8'OVRNIGHT',C'O'  NSI OVERNIGHTS                               
         DC    CL8'RENTRAK ',C'R'  RENTRAK                                      
         DC    XL2'FFFF'                                                        
         EJECT                                                                  
SRCMEDTD DSECT                                                                  
SRCMEDSM DS    CL2                 SOURCE/MEDIA CODE                            
SRCMEDSN DS    CL8                 SOURCE NAME                                  
SRCMEDMN DS    CL8                 MEDIA NAME                                   
SRCMEDTQ EQU   *-SRCMEDTD                                                       
         SPACE 2                                                                
TA0A34   CSECT                                                                  
*                                                                               
SRCMEDT  DS    0H                                                               
* SOURCE CODE + MEDIA CODE + SOURCE NAME + MEDIA NAME                           
* ALL ALLOWED COMBINATIONS OF SOURCE/MEDIA                                      
* NAMES DEPEND ON COMBINATION OF THE TWO                                        
         DC    C'AC',CL8'BBM',CL8'CANTV'                                        
         DC    C'NC',CL8'NSI',CL8'CANTV'                                        
         DC    C'AR',CL8'ARB',CL8'RADIO'                                        
         DC    C'RR',CL8'RADAR',CL8'RADIO'                                      
         DC    C'MR',CL8'BBRADIO',CL8'RADIO'                                    
******   DC    C'NR',CL8'NTI',CL8'RADIO'      - THIS IS DEAD                    
         DC    C'NR',CL8'NAT',CL8'RENTRAK'    - USING NR FOR RENTRAK            
         DC    C'AT',CL8'ARB',CL8'USTV'                                         
         DC    C'NT',CL8'NSI',CL8'USTV'                                         
         DC    C'MT',CL8'MFX',CL8'USTV'       - THIS IS DEAD                    
         DC    C'ST',CL8'SQAD',CL8'USTV'                                        
         DC    C'NA',CL8'NTI',CL8'NAD'                                          
         DC    C'NN',CL8'NTI',CL8'NETTV'                                        
         DC    C'NB',CL8'NTI',CL8'CABLE'                                        
         DC    C'NW',CL8'NSI',CL8'WEEKLY'                                       
         DC    C'HN',CL8'NHT',CL8'NETTV'                                        
         DC    C'HW',CL8'NHT',CL8'WEEKLY'                                       
         DC    C'SR',CL8'SQAD',CL8'RADIO'                                       
         DC    C'NU',CL8'NSI',CL8'COUNTY'                                       
         DC    C'NO',CL8'NSI',CL8'OVRNIGHT'                                     
         DC    C'FT',CL8'FUS',CL8'USTV'                                         
         DC    C'AU',CL8'ARB',CL8'COUNTY'                                       
         DC    C'TR',CL8'TRI',CL8'RADIO'                                        
         DC    C'VT',CL8'VID',CL8'VDEOLOGY'                                     
         DC    C'IR',CL8'IHT',CL8'RADIO'                                        
         DC    C'CT',CL8'NCM',CL8'USTV'                                         
         DC    X'FF'                                                            
         EJECT                                                                  
* ++INCLUDE DDSCANBLKD                                                          
* ++INCLUDE CTGENFILE                                                           
* ++INCLUDE DDSPLWORKD                                                          
* ++INCLUDE DDSPOOLD                                                            
* ++INCLUDE DDPERVALD                                                           
* ++INCLUDE DDMONYREQU                                                          
* ++INCLUDE DEDEMTABD                                                           
* ++INCLUDE DEDBLOCK                                                            
* ++INCLUDE FAFACTS                                                             
* ++INCLUDE DDCOMFACS                                                           
* ++INCLUDE DDACTIVD                                                            
* ++INCLUDE CTSFMFFD                                                            
* ++INCLUDE DDGENTWA                                                            
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         EJECT                                                                  
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDPERVALD                                                      
PERVALDX EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE DDMONYREQU                                                     
         EJECT                                                                  
       ++INCLUDE DEDEMTABD                                                      
         EJECT                                                                  
DEDBLOCK DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE DDACTIVD                                                       
         EJECT                                                                  
       ++INCLUDE CTSFMFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFM91D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFM92D                                                       
         EJECT                                                                  
* ++INCLUDE CTSFMWORKD                                                          
         PRINT OFF                                                              
       ++INCLUDE CTSFMWORKD                                                     
         EJECT                                                                  
         PRINT ON                                                               
********************************************************************            
* TA0A34 SAVED STORAGE DSECT                                                    
********************************************************************            
*                                                                               
         ORG   SYSSPARE                                                         
DUB2     DS    D                                                                
RELO     DS    A                   RELOCATION FACTOR                            
ADTWK    DS    A                                                                
AWKDT    DS    A                                                                
SVAIO    DS    A                                                                
AGETBROD DS    A                   GETBROAD                                     
VDEFINE  DS    V                   DEFINE                                       
MYHALF   DS    H                                                                
TITLEQ   DS    X                                                                
SVSOURCE DS    C                                                                
SVSUBF   DS    C                                                                
RECSNAME DS    CL8                 SOURCE NAME                                  
RECMNAME DS    CL8                 MEDIA NAME                                   
SVUSRID  DS    CL5                                                              
SAVEKEY  DS    XL25                DCON RECORD KEY                              
MYKEY    DS    XL25                                                             
MYKEYSV  DS    XL25                                                             
SVBINAGY DS    CL2                                                              
SVAPHAGY DS    CL2                                                              
SVALPUID DS    CL10                                                             
SVBKTYPE DS    X                   INTERNAL BOOKTYPE CODE                       
ELEMQ    DS    X                                                                
SVKEY    DS    CL(L'KEY)                                                        
PREVKEY  DS    XL(CTRKBOOK-CTRKEY)                                              
NLINES   DS    X                                                                
FNDX     DS    X                                                                
FNDMKT   DS    XL2                                                              
BKOPTN   DS    X                                                                
MYDATE   DS    CL6                                                              
USIDFLAG DS    X                   FLAG TEST IF USER ID ENTERED                 
USIDBIN  EQU   0                   BINARY USER ID ENTERED                       
USIDALP  EQU   1                   ALPHA ID ENTERED                             
ALPHAMKT DS    CL6                                                              
MKFILTAB DS    (MAXSCAN)XL(L'SVBMKT+L'VALMKTQ)                                  
         DS    XL2                                                              
LKFILTAB DS    (MAXSCAN)XL(L'SVAKEYLK+L'SVOPTN)                                 
         DS    XL2                                                              
MAXSCAN  EQU   27                                                               
PERVOUT  DS    CL(PERVALDX-PERVALD)                                             
SCANTAB  DS    (MAXSCAN+1)CL32                                                  
SCANTABQ EQU   *-SCANTAB                                                        
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
********************************************************************            
* LIST LINE DSECT                                                               
********************************************************************            
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTSRC   DS    CL8                                                              
         DS    C                                                                
LSTSUBF  DS    CL8                                                              
         DS    CL2                                                              
LSTAGY   DS    CL2                                                              
         DS    CL4                                                              
LSTBOOK  DS    CL10                                                             
         DS    CL4                                                              
LSTBTYP  DS    CL2                                                              
         DS    CL6                                                              
LSTCLI   DS    CL3                                                              
         DS    CL4                                                              
LSTUID   DS    CL10                                                             
         DS    C                                                                
LSTMKTAU DS    CL8                 MARKET AUTHORIZATION LIST TYPE               
         SPACE 3                                                                
********************************************************************            
* REPORT LINE DSECT                                                             
********************************************************************            
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    C                                                                
PRTSRC   DS    CL8                                                              
         DS    C                                                                
PRTSUBF  DS    CL8                                                              
         DS    CL2                                                              
PRTAGY   DS    CL2                                                              
         DS    CL4                                                              
PRTBOOK  DS    CL10                                                             
         DS    CL4                                                              
PRTBTYP  DS    CL2                                                              
         DS    CL6                                                              
PRTCLI   DS    CL3                                                              
         DS    CL4                                                              
PRTUID   DS    CL10                                                             
         DS    CL2                                                              
PRTMKTAU DS    CL8                 MARKET AUTHORIZATION LIST TYPE               
         EJECT                                                                  
********************************************************************            
* DSECT TO COVER ENTRIES IN MKFILTAB                                            
********************************************************************            
SVMKFILD DSECT                                                                  
SVBMKT   DS    XL2                 BINARY MARKET NUMBER                         
VALMKTQ  DS    X                   Y=LOOKING FOR VALID MARKET                   
*                                  N=LOOKING FOR INVALID MARKET                 
         SPACE 3                                                                
********************************************************************            
* DSECT TO COVER ENTRIES IN LKFILTAB                                            
********************************************************************            
SVLKFILD DSECT                                                                  
SVAKEYLK DS    XL4                 ADDRESS OF KEY TABLE                         
SVOPTN   DS    XL8                 OPTION (YES,4WEEK,ETC...)                    
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'051CTSFM34   02/26/20'                                      
         END                                                                    
