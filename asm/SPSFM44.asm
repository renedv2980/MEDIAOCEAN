*          DATA SET SPSFM44    AT LEVEL 005 AS OF 05/18/07                      
*PHASE T21744A                                                                  
T21744   TITLE 'SPSFM44 - PROFIT WITHIN UNLOCK OVERLAY'                         
***********************************************************************         
*============================= UPDATE LOG ============================*         
*   Date   Lvl User   Description                                     *         
* -------- --- ----   ----------------------------------------------- *         
* Mar15/02 001 MCHO - Changed screens from sp* to sc*                 *         
*                                                                     *         
* Jan17/95 001 GLEE - New program for Profit Within Unlock/Lock       *         
***********************************************************************         
         EJECT                                                                  
T21744   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T21744*,R7,RR=RE                                              
                                                                                
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
                                                                                
         ST    RE,RELO                                                          
         BAS   RE,MYINIT           INITIALIZE VALUES FIRST                      
                                                                                
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
                                                                                
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*========================== INITIALIZE VALUES ========================*         
MYINIT   NTR1                                                                   
         MVI   WORK,XFF-X'40'                                                   
         MVC   WORK+1(L'ACTNAME-1),WORK                                         
         MVC   ACTNAME,CONACT                                                   
         NC    ACTNAME,WORK        GET ACTION NAME IN LOWER CASE                
                                                                                
         MVC   AIO,AIO1            SET AIO TO IO AREA 1                         
         B     XIT                                                              
***********************************************************************         
         TITLE 'SPSFM44 - PROFIT WITHIN UNLOCK OVERLAY (VALKEY)'                
***********************************************************************         
*======================== VALIDATE KEY ROUTINE =======================*         
VK       DS    0H                                                               
         NI    MISCFLG1,XFF-MF1KYCHG                                            
*                                                                               
*--------------------------- VALIDATE MEDIA --------------------------*         
*                                                                               
         LA    R2,PWUMEDH          MEDIA                                        
         BAS   RE,KYCHNGED         DID THIS KEY FIELD CHANGE?                   
         GOTO1 VALIMED                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
*-------------------------- VALIDATE CLIENT --------------------------*         
*                                                                               
         LA    R2,PWUCLTH          CLIENT                                       
         BAS   RE,KYCHNGED         DID THIS KEY FIELD CHANGE?                   
         GOTO1 VALICLT                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
*-------------------------- VALIDATE PRODUCT -------------------------*         
*                                                                               
         LA    R2,PWUPRDH          PRODUCT                                      
         BAS   RE,KYCHNGED         DID THIS KEY FIELD CHANGE?                   
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         GOTO1 VALIPRD                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
*------------------------- VALIDATE ESTIMATE -------------------------*         
*                                                                               
         LA    R2,PWUESTH          ESTIMATE                                     
         BAS   RE,KYCHNGED         DID THIS KEY FIELD CHANGE?                   
         GOTO1 VALIEST                                                          
         OI    4(R2),X'20'                                                      
         EJECT                                                                  
*                                                                               
*----------------------------- EXIT TASKS ----------------------------*         
*                                                                               
         B     XIT                                                              
*                                                                               
*------------------------- TEST CHANGE OF KEY ------------------------*         
*                                                                               
KYCHNGED DS    0H                                                               
         TM    4(R2),X'20'                                                      
         BZ    KYCH10                                                           
         TM    4(R2),X'80'                                                      
         BZR   RE                                                               
KYCH10   OI    MISCFLG1,MF1KYCHG                                                
         BR    RE                                                               
*                                                                               
*----------------------------- BUILD PWKEY ---------------------------*         
*                                                                               
BPWKEY   DS    0H                                                               
         XC    KEY,KEY             SET UP THE PW RECORD KEY                     
PWK      USING PWFKEY,KEY                                                       
         MVC   PWK.PWKTYP,=X'0D7A'  RECORD TYPE                                 
         MVC   PWK.PWKAGMD,BAGYMD   AGENCY/MEDIA                                
         MVC   PWK.PWKCLT,BCLT      CLIENT                                      
         MVC   PWK.PWKPRD,BPRD      PRODUCT                                     
         MVC   PWK.PWKEST,BEST      ESTIMATE                                    
         DROP  PWK                                                              
         BR    RE                                                               
***********************************************************************         
         TITLE 'SPSFM44 - PROFIT WITHIN UNLOCK OVERLAY (VALREC)'                
***********************************************************************         
*========================== VALIDATE RECORD ==========================*         
VR       DS    0H                                                               
                                                                                
         TM    MISCFLG1,MF1KYCHG   DID KEY CHANGE?                              
         BO    DR                   YES, DISPLAY STUFF                          
         CLC   PREVACT,ACTNAME     DID USER CHANGE ACTION?                      
         BNE   DR                   YES, DISPLAY STUFF ALSO                     
                                                                                
         LA    R2,PWUBOKH          CHECK BUY OK FIELD                           
         MVC   FULL,BUYCNT                                                      
         BAS   RE,VRCHK                                                         
         LA    R2,PWUPOKH          CHECK PW  OK FIELD                           
         MVC   FULL,PWCNT                                                       
         BAS   RE,VRCHK                                                         
                                                                                
         BAS   RE,GTRQID           CHECK IF REQUEST ID NEEDED                   
         BAS   RE,GENREQ           GENERATE REQUEST, IF NEEDED                  
                                                                                
         CLC   PWUBOK,OKAY         IF AT LEAST ONE FIELD HAS                    
         BE    VR10                                                             
         CLC   PWUPOK,OKAY          OKAY, THEN UPDATE FILE                      
         BE    VR10                                                             
         OC    REQID,REQID         IF NO FIELDS HAVE OKAY                       
         BZ    VRX                                                              
         B     DR                   GO DISPLAY RECD IF REQID EXIST              
*                                                                               
VR10     MVI   RDUPDATE,C'Y'                                                    
         BAS   RE,BPWKEY           BUILD KEY OF PW RECORD                       
         GOTO1 HIGH                                                             
         B     VR10B                                                            
VR10A    GOTO1 SEQ                                                              
VR10B    CLC   KEY(PKYESTL),KEYSAVE                                             
         BNE   VR100                                                            
                                                                                
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING PWRECD,R6                                                        
         LA    RF,VRLOCK                                                        
         CLC   LOCK,ACTNAME                                                     
         BER   RF                                                               
         LA    RF,VRULCK                                                        
         BR    RF                                                               
*                                                                               
** ACTION=LOCK **                                                               
*                                                                               
VRLOCK   DS    0H                                                               
         MVI   BYTE,PWGNBILQ+PWGNBPLK    BUY LOCK                               
         CLC   PWUBOK,OKAY                                                      
         BNE   VRLOCK10                                                         
         OC    PWGNFLG,BYTE                                                     
*                                                                               
VRLOCK10 MVI   BYTE,PWGNPLKQ+PWGNBPLK    PW  LOCK                               
         CLC   PWUPOK,OKAY                                                      
         BNE   VR20                                                             
         OC    PWGNFLG,BYTE                                                     
         B     VR20                                                             
*                                                                               
** ACTION=UNLOCK **                                                             
*                                                                               
VRULCK   DS    0H                                                               
         MVI   BYTE,XFF-PWGNBILQ   BUY UNLOCK                                   
         CLC   PWUBOK,OKAY                                                      
         BNE   VRULCK10                                                         
         NC    PWGNFLG,BYTE                                                     
*                                                                               
VRULCK10 MVI   BYTE,XFF-PWGNPLKQ   PW  UNLOCK                                   
         CLC   PWUPOK,OKAY                                                      
         BNE   VR20                                                             
         NC    PWGNFLG,BYTE                                                     
         B     VR20                                                             
*                                                                               
         DROP  R6                                                               
VR20     GOTO1 PUTREC                                                           
         B     VR10A                                                            
*                                                                               
VR100    XC    PWUBOK,PWUBOK                                                    
         OI    PWUBOKH+6,X'80'                                                  
         XC    PWUPOK,PWUPOK                                                    
         OI    PWUPOKH+6,X'80'                                                  
         B     DR                                                               
*                                                                               
VRX      MVC   ACURFORC,AFRSTREC                                                
         B     XIT                                                              
         EJECT                                                                  
*-------------------------- CHECK INPUT RTN --------------------------*         
                                                                                
* Checks the input fields for the word 'OK'                                     
*  At entry, R2-->to the field for validation                                   
*            FULL=count of lock/unlock records on file                          
                                                                                
VRCHK    NTR1                                                                   
         CLI   5(R2),0             DID USER TYPE ANYTHING IN?                   
         BE    VRCHKX               NOPE, NO INPUT                              
                                                                                
         OC    FULL,FULL           WERE ANY RECORDS LOCKED/UNLOCKED?            
         BNZ   VRCHK10                                                          
         DC    H'0'                 NO, SO FIELD S/B PROTECTED                  
                                                                                
VRCHK10  CLI   5(R2),1               YES, BAD INPUT IF L(INPUT)=1               
         BE    BINPE                                                            
         CLC   OKAY,8(R2)          DID USER TYPE 'OK'?                          
         BNE   BINPE                NO, ERROR                                   
*                                                                               
VRCHKX   B     XIT                                                              
         EJECT                                                                  
*-------------------------- GET REQUEST ID ---------------------------*         
GTRQID   NTR1                                                                   
                                                                                
         XC    REQID,REQID                                                      
         CLC   LOCK,ACTNAME        REQUEST ID VALID ONLY FOR ACTN=LOCK          
         BNE   GQIX                                                             
                                                                                
         LA    R2,PWUBIDH                                                       
         CLI   5(R2),0             ANY INPUT IN REQUEST ID FIELD?               
         BNE   GQI10                YES, GO STRAIGHT TO PROCESS IT              
         OC    BUYCNT,BUYCNT        NO, BUT ANY BUY LEFT TO LOCK?               
         BZ    GQIX                  NOPE, SO JUST EXIT                         
                                                                                
         DS    0H                  THERE ARE MKTS FOR BUY LOCK                  
         CLC   PWUBOK,OKAY         IS ID NEEDED?                                
         BNE   GQIX                 NOPE, USER IS NOT LOCKING BUY               
*                                                                               
GQI10    DS    0H                  NEED REQUEST ID FOR REPORT                   
         GOTO1 ANY                                                              
         CLI   5(R2),2             MUST BE AT LEAST 2 CHAR                      
         BL    INVLFLD                                                          
                                                                                
         ZIC   R0,5(R2)            R0=L(INPUT)                                  
         LA    R3,WORK             R3-->INPUT DATA                              
GQI20    LA    RF,ALPHANUM         RF-->TABLE OF VALID ALPHANUMERICS            
GQI20A   CLI   0(RF),C'\'          IF AT END OF THIS TABLE,                     
         BE    INVLFLD              THEN INPUT NOT ALPHANUMERIC                 
         CLC   0(1,R3),0(RF)       IF MATCH FOUND                               
         BE    GQI20B               VALIDATE NEXT CHAR OF INPUT                 
         LA    RF,1(RF)             ELSE, KEEP LOOKING FOR MATCH                
         B     GQI20A                                                           
GQI20B   LA    R3,1(R3)            R3-->NEXT CHAR OF INPUT                      
         BCT   R0,GQI20                                                         
                                                                                
         MVC   REQID,WORK          INPUT ID IS VALID, HOLD ONTO IT              
         OC    REQID,REQID                                                      
         BNZ   GQIX                                                             
         DC    H'0'                IT BETTER NOT BE NULLS                       
*                                                                               
GQIX     B     XIT                                                              
         EJECT                                                                  
*------------------------- GENERATE REQUEST --------------------------*         
GENREQ   NTR1                                                                   
                                                                                
         OC    REQID,REQID         ANY REQUEST ID?                              
         BZ    GRQX                 NOPE, DON'T GENERATE REQUEST                
         CLC   REQID,=C'DDS'       IF REQUEST ID IS DDS                         
         BE    GRQX                                                             
         CLC   REQID,=C'XXX'        OR XXX,                                     
         BE    GRQX                 DON'T GENERATE REQUEST                      
                                                                                
         XC    REQHDR,REQHDR                                                    
         MVC   REQUEST,SPACES                                                   
                                                                                
         MVC   REQUEST(2),=C'LK'                                                
         MVC   REQUEST+2(2),TWAAGY                                              
         MVI   REQUEST+4,C'*'                                                   
         MVI   REQUEST+5,C'.'                                                   
                                                                                
         MVC   REQUEST+6(2),=C'SL'       RECORD                                 
         MVI   REQUEST+8,C'.'                                                   
                                                                                
         MVC   REQUEST+9(6),=C'REPORT'   ACTION                                 
         MVI   REQUEST+15,C'.'                                                  
                                                                                
         MVI   REQUEST+16,C'.'                                                  
                                                                                
         MVC   REQUEST+17(3),=C'OV,'     PRINT OPTION                           
         MVC   REQUEST+20(3),REQID                                              
         MVI   REQUEST+23,C'.'                                                  
                                                                                
         MVI   REQUEST+24,C'.'                                                  
         MVI   REQUEST+25,C'.'                                                  
         MVI   REQUEST+26,C'.'                                                  
         MVI   REQUEST+27,C'.'                                                  
         MVI   REQUEST+28,C'.'                                                  
         MVI   REQUEST+29,C'.'                                                  
                                                                                
         MVC   REQUEST+30(1),QMED        MEDIA                                  
         MVI   REQUEST+31,C'.'                                                  
                                                                                
         MVC   REQUEST+32(3),QCLT        CLIENT                                 
         MVI   REQUEST+35,C'.'                                                  
                                                                                
         MVC   REQUEST+36(3),QPRD        PRODUCT                                
         MVI   REQUEST+39,C'.'                                                  
                                                                                
         LA    R2,REQUEST+40             ESTIMATE                               
         ZIC   RF,BEST                                                          
         EDIT  (RF),(3,0(R2)),FILL=0                                            
         MVI   REQUEST+43,C'.'                                                  
                                                                                
         MVC   REQUEST+44(3),=C'ALL'     MARKET                                 
         MVI   REQUEST+47,C'.'                                                  
                                                                                
         MVC   REQUEST+48(3),=C'ALL'     STATION                                
         MVI   REQUEST+51,C'.'                                                  
                                                                                
         MVI   REQUEST+52,C'*'           TERMINATOR                             
                                                                                
         XC    FLD,FLD                                                          
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'REQUEST',FLD,REQHDR                    
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GRQX     B     XIT                                                              
         EJECT                                                                  
*------------------------- MY VALREC ERRORS --------------------------*         
BINPE    MVI   MYERRCD,BINPQ                                                    
         B     MYERROR                                                          
***********************************************************************         
         TITLE 'SPSFM44 - PROFIT WITHIN UNLOCK OVERLAY (DISPREC)'               
***********************************************************************         
*========================== DISPLAY RECORD ===========================*         
DR       DS    0H                                                               
                                                                                
         MVC   PREVACT,ACTNAME     REMEMBER PREVIOUS ACTION                     
         XC    NUMREC,NUMREC       INITIALIZE COUNTERS                          
         XC    BUYCNT,BUYCNT                                                    
         XC    PWCNT,PWCNT                                                      
*                                                                               
** COUNT RECORDS **                                                             
*                                                                               
         BAS   RE,BPWKEY           BUILD KEY OF PW RECORD                       
DR10     GOTO1 HIGH                                                             
         CLC   KEY(PKYESTL),KEYSAVE  MATCH KEY UP TO & INCLUDING EST            
         BNE   DR20                                                             
                                                                                
         LA    R6,KEY                                                           
         USING PWRECD,R6                                                        
         OC    PWKSTA,PWKSTA       BETTER BE A MKT-LEVEL RECD                   
         BZ    *+6                                                              
         DC    H'0'                                                             
         DROP  R6                                                               
                                                                                
         L     R1,NUMREC           UPDATE # OF PW (MKT-LEVEL)                   
         LA    R1,1(R1)                                                         
         ST    R1,NUMREC            RECDS FOR MED/CLT/PRD/EST                   
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING PWRECD,R6                                                        
                                                                                
         XC    HALF,HALF                                                        
         MVC   HALF(1),PWGNFLG     CHECK BUY LOCK/UNLOCK                        
         NI    HALF,PWGNBILQ        TURN OFF ALL OTHER FLAGS                    
         CLC   LOCK,ACTNAME                                                     
         BNE   *+8                                                              
         OI    HALF+1,PWGNBILQ                                                  
         XC    HALF(1),HALF+1                                                   
         BZ    DR10A                                                            
         L     R1,BUYCNT           UPDATE # OF RECDS FOR M/CLT/PRD/EST          
         LA    R1,1(R1)                                                         
         ST    R1,BUYCNT            WHICH ARE BUY LOCKED/UNLOCKED               
                                                                                
DR10A    XC    HALF,HALF                                                        
         MVC   HALF(1),PWGNFLG     CHECK PW LOCK/UNLOCK                         
         NI    HALF,PWGNPLKQ        TURN OFF ALL OTHER FLAGS                    
         CLC   LOCK,ACTNAME                                                     
         BNE   *+8                                                              
         OI    HALF+1,PWGNPLKQ                                                  
         XC    HALF(1),HALF+1                                                   
         BZ    DR15                                                             
         L     R1,PWCNT            UPDATE # OF RECDS FOR M/CLT/PRD/EST          
         LA    R1,1(R1)                                                         
         ST    R1,PWCNT             WHICH ARE PW LOCKED/UNLOCKED                
         DROP  R6                                                               
*                                                                               
DR15     LA    R6,KEY              READ NEXT MKT FOR MED/CLT/PRD/EST            
         USING PWRECD,R6                                                        
         ZICM  R1,PWKMKT,(3)                                                    
         LA    R1,1(R1)                                                         
         STCM  R1,3,PWKMKT                                                      
         XC    PWKSTA(4),PWKSTA                                                 
         DROP  R6                                                               
         B     DR10                                                             
*                                                                               
** DISPLAY CORRECT COMMENTS **                                                  
*                                                                               
DR20     DS    0H                                                               
         TWAXC PWUBFRCH,PWUPRGTH,PROT=Y                                         
                                                                                
         OC    NUMREC,NUMREC       ANY MARKETS OUT THERE?                       
         BNZ   DR30                 YEP                                         
*                                                                               
*** NO MARKETS CASE ***                                                         
*                                                                               
         LA    R2,PWUBCM1          BUY                                          
         MVC   0(L'NOMKTMSG,R2),NOMKTMSG                                        
         LA    R2,L'NOMKTMSG+1(R2)                                              
         MVC   0(L'BUY,R2),BUY                                                  
         LA    R2,L'BUY+1(R2)                                                   
         CLC   LOCK,ACTNAME                                                     
         BE    *+14                                                             
         MVC   0(2,R2),=CL2'un'                                                 
         LA    R2,2(R2)                                                         
         MVC   0(L'LOCK,R2),LOCK                                                
         OI    PWUBOKH+1,X'20'     PROTECT INPUT FIELD                          
         OI    PWUBIDH+1,X'20'        "      "     "                            
                                                                                
         LA    R2,PWUPCM1          PW                                           
         MVC   0(L'NOMKTMSG,R2),NOMKTMSG                                        
         LA    R2,L'NOMKTMSG+1(R2)                                              
         MVC   0(L'PW,R2),PW                                                    
         LA    R2,L'PW+1(R2)                                                    
         CLC   LOCK,ACTNAME                                                     
         BE    *+14                                                             
         MVC   0(2,R2),=CL2'un'                                                 
         LA    R2,2(R2)                                                         
         MVC   0(L'LOCK,R2),LOCK                                                
         OI    PWUPOKH+1,X'20'     PROTECT INPUT FIELD                          
                                                                                
         L     R2,AFRSTKEY                                                      
         B     DRX                                                              
*                                                                               
*** AT LEAST 1 MARKET EXIST ***                                                 
*                                                                               
DR30     DS    0H                                                               
         MVI   BPLURAL,C'N'        DETERMINE PLURALITY                          
         ICM   R1,15,BUYCNT         FOR BUY                                     
         BNZ   *+8                                                              
         L     R1,NUMREC                                                        
         C     R1,=F'1'                                                         
         BE    *+8                 IF COUNT IS ANYTHING OTHER THAN 1,           
         MVI   BPLURAL,C'Y'         MAKE PLURAL                                 
                                                                                
         MVI   PPLURAL,C'N'        DETERMINE PLURALITY                          
         ICM   R1,15,PWCNT          FOR PW                                      
         BNZ   *+8                                                              
         L     R1,NUMREC                                                        
         C     R1,=F'1'                                                         
         BE    *+8                 IF COUNT IS ANYTHING OTHER THAN 1,           
         MVI   PPLURAL,C'Y'         MAKE PLURAL                                 
*                                                                               
**** DISPLAY COUNTERS ****                                                      
*                                                                               
         L     RF,NUMREC                                                        
         ICM   RE,15,BUYCNT        BUY COUNTERS                                 
         BNZ   *+6                                                              
         LR    RE,RF                                                            
         EDIT  (RE),(3,PWUBFRC),ZERO=NOBLANK                                    
         MVC   PWUBOF,OF                                                        
         EDIT  (RF),(3,PWUBTTL),ZERO=NOBLANK                                    
                                                                                
         ICM   RE,15,PWCNT         PW COUNTERS                                  
         BNZ   *+6                                                              
         LR    RE,RF                                                            
         EDIT  (RE),(3,PWUPFRC),ZERO=NOBLANK                                    
         MVC   PWUPOF,OF                                                        
         EDIT  (RF),(3,PWUPTTL),ZERO=NOBLANK                                    
*                                                                               
**** DISPLAY MESSAGES ****                                                      
*                                                                               
         LA    R2,PWUBCM1          THE WORD 'MARKET'                            
         MVC   0(L'MARKET,R2),MARKET                                            
         LA    R2,L'MARKET+1(R2)                                                
                                                                                
         MVC   0(L'IS,R2),IS       THE WORD 'IS' OR 'ARE'                       
         LA    R0,L'IS+1                                                        
         CLI   BPLURAL,C'N'                                                     
         BE    *+14                                                             
         MVC   0(L'ARE#,R2),ARE#                                                
         LA    R0,L'ARE#+1                                                      
         AR    R2,R0                                                            
                                                                                
         MVC   0(L'BUY,R2),BUY     THE WORD 'BUY'                               
         LA    R2,L'BUY+1(R2)                                                   
                                                                                
         SR    R1,R1                                                            
         IC    R1,BEQ+1            GET BRANCH MASK FOR THE                      
         OC    BUYCNT,BUYCNT        CLC INSTRUCTION WHICH FOLLOWS               
         BZ    *+8                                                              
         IC    R1,BNEQ+1                                                        
         CLC   LOCK,ACTNAME        THE WORD 'LOCK' OR 'UNLOCK'                  
         EX    R1,*+4                                                           
         BC    0,*+14               S/B BRANCH EQUAL IF BUYCNT=0                
         MVC   0(2,R2),=CL2'un'                                                 
         LA    R2,2(R2)                                                         
         MVC   0(L'LOCKED,R2),LOCKED                                            
         LA    R2,L'LOCKED+1(R2)                                                
         OI    PWUBOKH+1,X'20'     PROTECT INPUT FIELD                          
         OI    PWUBIDH+1,X'20'        "      "     "                            
                                                                                
         OC    BUYCNT,BUYCNT                                                    
         BZ    DR38                                                             
                                                                                
         LA    R2,PWUBCM2                                                       
         MVC   0(L'TYPEOK,R2),TYPEOK    THE WORDS 'TYPE OK TO'                  
         LA    R2,PWUBCM3                                                       
         CLC   LOCK,ACTNAME                                                     
         BE    *+14                                                             
         MVC   0(2,R2),=CL2'un'         THE WORDS 'LOCK' OR 'UNLOCK'            
         LA    R2,2(R2)                                                         
         MVC   0(L'LOCK,R2),LOCK                                                
         LA    R2,L'LOCK+1(R2)                                                  
         MVC   0(L'BUY,R2),BUY          THE WORD 'BUY'                          
         MVI   PWUBLFT,C'<'                                                     
         MVI   PWUBRGT,C'>'                                                     
         NI    PWUBOKH+1,XFF-X'20'    UNPROTECT INPUT FIELD                     
                                                                                
DR38     DS    0H                                                               
         CLC   LOCK,ACTNAME        REQ ID FIELD FOR                             
         BNE   DR40                                                             
         MVC   PWUBREQ,RQSTID       LOCKING BUY ONLY                            
         MVI   PWUBRQL,C'<'                                                     
         MVI   PWUBRQR,C'>'                                                     
         NI    PWUBIDH+1,XFF-X'20'    UNPROTECT INPUT FIELD                     
*                                                                               
DR40     DS    0H                                                               
         LA    R2,PWUPCM1          THE WORD 'MARKET'                            
         MVC   0(L'MARKET,R2),MARKET                                            
         LA    R2,L'MARKET+1(R2)                                                
                                                                                
         MVC   0(L'IS,R2),IS       THE WORD 'IS' OR 'ARE'                       
         LA    R0,L'IS+1                                                        
         CLI   PPLURAL,C'N'                                                     
         BE    *+14                                                             
         MVC   0(L'ARE#,R2),ARE#                                                
         LA    R0,L'ARE#+1                                                      
         AR    R2,R0                                                            
                                                                                
         MVC   0(L'PW,R2),PW       THE WORD 'BUY'                               
         LA    R2,L'PW+1(R2)                                                    
                                                                                
         SR    R1,R1                                                            
         IC    R1,BEQ+1            GET BRANCH MASK FOR THE                      
         OC    PWCNT,PWCNT          CLC INSTRUCTION WHICH FOLLOWS               
         BZ    *+8                                                              
         IC    R1,BNEQ+1                                                        
         CLC   LOCK,ACTNAME        THE WORD 'LOCK' OR 'UNLOCK'                  
         EX    R1,*+4                                                           
         BC    0,*+14               S/B BRANCH EQUAL IF PWCNT=0                 
         MVC   0(2,R2),=CL2'un'                                                 
         LA    R2,2(R2)                                                         
         MVC   0(L'LOCKED,R2),LOCKED                                            
         LA    R2,L'LOCKED+1(R2)                                                
         OI    PWUPOKH+1,X'20'     PROTECT INPUT FIELD                          
                                                                                
         OC    PWCNT,PWCNT                                                      
         BZ    DR50                                                             
                                                                                
         LA    R2,PWUPCM2                                                       
         MVC   0(L'TYPEOK,R2),TYPEOK    THE WORDS 'TYPE OK TO'                  
         LA    R2,PWUPCM3                                                       
         CLC   LOCK,ACTNAME                                                     
         BE    *+14                                                             
         MVC   0(2,R2),=CL2'un'         THE WORDS 'LOCK' OR 'UNLOCK'            
         LA    R2,2(R2)                                                         
         MVC   0(L'LOCK,R2),LOCK                                                
         LA    R2,L'LOCK+1(R2)                                                  
         MVC   0(L'PW,R2),PW            THE WORD 'PW'                           
         MVI   PWUPLFT,C'<'                                                     
         MVI   PWUPRGT,C'>'                                                     
         NI    PWUPOKH+1,XFF-X'20'    UNPROTECT INPUT FIELD                     
*                                                                               
** SET CURSOR POSITION **                                                       
*                                                                               
DR50     LA    R2,PWUBOKH                                                       
         OC    BUYCNT,BUYCNT                                                    
         BNZ   DRX                                                              
         LA    R2,PWUPOKH                                                       
         OC    PWCNT,PWCNT                                                      
         BNZ   DRX                                                              
         L     R2,AFRSTKEY                                                      
*                                                                               
DRX      ST    R2,ACURFORC                                                      
         B     XIT                                                              
***********************************************************************         
         TITLE 'SPSFM44 - PROFIT WITHIN UNLOCK (MISC STUFF)'                    
***********************************************************************         
*======================== MISCELLANEOUS STUFF ========================*         
                                                                                
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
*                                                                               
MISSFLD  MVI   ERROR,MISSING                                                    
         B     ERREXIT                                                          
*                                                                               
INVLFLD  MVI   ERROR,INVALID                                                    
         B     ERREXIT                                                          
         SPACE 2                                                                
ERREXIT  GOTO1 ERREX                                                            
         B     XIT                                                              
         EJECT                                                                  
*----------------------- MY ROUTINES FOR ERRORS ----------------------*         
                                                                                
MYERROR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(9),=C'**ERROR**'                                         
         LA    R1,CONHEAD+10                                                    
         ZIC   RF,MYERRCD                                                       
         LTR   RF,RF                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         B     MYERR0(RF)                                                       
                                                                                
BINPQ    EQU   ((MYERR1-MYERR0)/4)+1                                            
                                                                                
MYERR0   DS    0H                                                               
MYERR1   B     BINP                                                             
         DC    H'0'                                                             
*                                                                               
BINP     MVC   0(12,R1),=C'Bad input, '''                                       
         LA    R1,12(R1)                                                        
         MVC   0(L'OKAY,R1),OKAY                                                
         LA    R1,L'OKAY(R1)                                                    
         MVC   0(10,R1),=C''' expected'                                         
         B     MSGERR                                                           
*                                                                               
MSGERR   MVI   ERROR,0                                                          
         MVI   MYERRCD,0                                                        
         GOTO1 ERREX2                                                           
***********************************************************************         
         TITLE 'SPSFM44 - PROFIT WITHIN UNLOCK (LTORG && CONST)'                
***********************************************************************         
*======================== LTORG AND CONSTANTS ========================*         
BEQ      BE    *(R0)               THIS CODE SHOULD NEVER BE EXECUTED           
BNEQ     BNE   *(R0)               THIS CODE SHOULD NEVER BE EXECUTED           
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
OKAY     DC    C'OK'                                                            
LOCKED   DC    C'locked'                                                        
LOCK     DC    C'lock'                                                          
BUY      DC    C'buy'                                                           
PW       DC    C'PW'                                                            
DONTINPT DC    C'**'                                                            
NOMKTMSG DC    C'No market for'                                                 
OF       DC    C'of'                                                            
IS       DC    C'is'                                                            
ARE#     DC    C'are'                                                           
MARKET   DC    C'market(s)'                                                     
TYPEOK   DC    C'Type ''OK'' to'                                                
RQSTID   DC    C'Lock Station (Req ID)'                                         
                                                                                
ALPHANUM DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789\'                         
         SPACE 2                                                                
XFF      EQU   X'FF'                                                            
PKYESTL  EQU   PWKMKT-PWFKEY       L(PW KEY UNTIL & INCLUDING EST)              
PKYMKTL  EQU   PWKSTA-PWFKEY       L(PW KEY UNTIL & INCLUDING MKT)              
***********************************************************************         
         DROP  R7,R8,R9,RA,RB,RC                                                
         TITLE 'SPSFM44 - PROFIT WITHIN UNLOCK (SPSFMWORKD)'                    
***********************************************************************         
*============================= SPSFMWORKD ============================*         
       ++INCLUDE SPSFMWORKD                                                     
***********************************************************************         
         TITLE 'SPSFM44 - PROFIT WITHIN UNLOCK (TWA DSECTS)'                    
***********************************************************************         
*================================ TWA ================================*         
                                                                                
*---------------------------- BASE SCREEN ----------------------------*         
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
*-------------------------- PW UNLOCK SCREEN -------------------------*         
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM97D                                                       
         EJECT                                                                  
*---------------------------- STORAGE AREA ---------------------------*         
                                                                                
RELO     DS    F                   RELOCATION FACTOR                            
                                                                                
MY97TWAL EQU   *-CONHEADH                                                       
         DS    0CL(3520-MY97TWAL)  CHECK AGAINST GENCON'S TWA LIMIT             
***********************************************************************         
         TITLE 'SPSFM44 - PROFIT WITHIN UNLOCK OVERLAY'                         
***********************************************************************         
*============================ OTHER DSECTS ===========================*         
                                                                                
* DDGENTWA                                                                      
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FATIOB                                                                        
* DDCOMFACS                                                                     
* DDPERVALD                                                                     
* DDCOREQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE FATIOB                                                         
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*========================== SAVED WORK AREA ==========================*         
                                                                                
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
*                                 **************** MISC ***************         
NUMREC   DS    F                   NUMBER OF PW (MKT-LEVEL) RECORDS             
PWCNT    DS    F                                                                
BUYCNT   DS    F                                                                
                                                                                
FLDH     DS    CL8                                                              
FLD      DS    CL40                                                             
REQID    DS    CL3                 REQUEST ID                                   
                                                                                
MISCFLG1 DS    XL1                                                              
MF1KYCHG EQU   X'80'               KEY CHANGED                                  
                                                                                
BPLURAL  DS    CL1                 Y OR N FOR BUY PLURAL                        
PPLURAL  DS    CL1                 Y OR N FOR PW  PLURAL                        
MYERRCD  DS    XL1                 MY ERROR CODE                                
*                                                                               
ACTNAME  DS    CL(L'CONACT)        ACTION NAME                                  
PREVACT  DS    CL(L'ACTNAME)       PREVIOUS ACTION                              
*                                                                               
REQHDR   DS    CL26                                                             
REQUEST  DS    CL80                                                             
         SPACE 2                                                                
MYSSPREL EQU   *-SYSSPARE                                                       
         DS    0CL(1024-MYSSPREL)  CHECK AGAINST AVAIL SYSSPARE AMT             
***********************************************************************         
         TITLE 'SPSFM44 - PROFIT WITHIN UNLOCK (SPGEN DSECTS)'                  
***********************************************************************         
*============================ SPGEN DSECTS ===========================*         
                                                                                
*----------------------------- SPGENWIPW -----------------------------*         
       ++INCLUDE SPGENWIPW         WESTERN INTL. PROFIT WITHIN REC              
***********************************************************************         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005SPSFM44   05/18/07'                                      
         END                                                                    
