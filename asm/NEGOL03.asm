*          DATA SET NEGOL03    AT LEVEL 008 AS OF 08/09/07                      
*PHASE T31403A                                                                  
         TITLE 'NEGOL03 -  GOAL MAINTENANCE'                                    
         PRINT NOGEN                                                            
T31403   CSECT                                                                  
         NMOD1 0,T31403                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA     BASE SCREEN FOR SYSTEM + THIS PROG            
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         LA    R6,2048(RB)                                                      
         LA    R6,2048(R6)                                                      
         USING T31403,RB,R6                                                     
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         MVI   IOOPT,C'Y'                                                       
*                                                                               
         CLC   =C'DELETE',CONACT                                                
         JE    INVACTN                                                          
         CLC   =C'CHANGE',CONACT                                                
         JE    INVACTN                                                          
         CLC   =C'ADD',CONACT                                                   
         JE    INVACTN                                                          
         CLC   =C'RESTORE',CONACT                                               
         JE    INVACTN                                                          
*                                                                               
         MVI   SYSTFLAG,C'X'                                                    
         GOTO1 VSETXSP                                                          
*                                                                               
         MVC   AIO,AIO1                                                         
         L     R7,AIO                                                           
         USING NGOLRECD,R7                                                      
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
EXIT     XIT1                                                                   
*                                                                               
VK       DS    0H                                                               
         CLI   SYSTFLAG,C'X'       XSPOT FILE?                                  
         BNE   *+12                                                             
         BRAS  RE,XVK                                                           
         J     EXIT                                                             
*                                                                               
         MVI   KEYB4MKT,4                                                       
         MVI   KEYAFMKT,0                                                       
         MVI   NLISTS,14           SET GENCON LIST NUMBER TO 14                 
         XC    SVKEY,SVKEY                                                      
         MVI   SVKEY,2                                                          
*                                                                               
         LA    R2,HSYMEDH          * MEDIA                                      
         GOTO1 VALIMED                                                          
         MVC   GKEYAM,BAGYMD                                                    
         MVC   SVKEY+1(1),BAGYMD                                                
*                                                                               
         LA    R2,HSYCLIH          * CLIENT                                     
         GOTO1 VALIFLD                                                          
         BNZ   VK10                                                             
         MVI   ERROR,INVALID                                                    
         J     INVINPT                                                          
VK10     GOTO1 VALICLT                                                          
         MVC   SVKEY+2(2),BCLT                                                  
         SPACE                                                                  
         MVI   NOPTFLG,0           SET OPTIONAL FLAG                            
         CLI   ACTNUM,ACTLIST      LIST/PRINT OPTIONAL                          
         BNE   *+8                                                              
         MVI   NOPTFLG,1           SET OPTIONAL FLAG                            
*                                                                               
         MVI   BPAKG,0                                                          
         LA    R2,HSYPRDH          * PRODUCT                                    
         GOTO1 VALIFLD                                                          
         BNZ   VK20                                                             
         CLI   NOPTFLG,1                                                        
         BE    VK30                                                             
         MVI   ERROR,INVALID                                                    
         J     INVINPT                                                          
VK20     GOTO1 VALIPRD                                                          
         MVC   SVKEY+4(1),BPRD                                                  
         ZIC   R5,KEYB4MKT                                                      
         LA    R5,1(R5)                                                         
         STC   R5,KEYB4MKT                                                      
*                                                                               
VK30     XC    QNET,QNET                                                        
         MVI   NOPTFLG,1           SET FIELD AS OPTIONAL                        
         LA    R2,HSYNETH          * NETWORK                                    
         GOTO1 VALIFLD                                                          
         BZ    VK40                                                             
*                                                                               
         CLC   8(2,R2),=C'M='      MEDIA TYPE?                                  
         BNE   VK35                                                             
*                                                                               
         CLI   10(R2),C'N'         NETWORK?                                     
         BNE   *+14                                                             
         MVC   SVKEY+5(2),=X'0309'     NETWORK = 0777                           
         B     VK40                                                             
*                                                                               
         CLI   10(R2),C'S'         SYNDICATION?                                 
         BNE   *+14                                                             
         MVC   SVKEY+5(2),=X'0306'     SYNDICATION = 0774                       
         B     VK40                                                             
*                                                                               
         CLI   10(R2),C'C'         CABLE?                                       
         JNE   INVINPT                                                          
         MVC   SVKEY+5(2),=X'0307'     CABLE = 0775                             
         B     VK40                                                             
*                                                                               
VK35     DS    0H                                                               
         GOTO1 VALINTWK                                                         
         MVC   SVKEY+5(2),QNETMKT                                               
         ZIC   R5,KEYB4MKT                                                      
         LA    R5,2(R5)                                                         
         STC   R5,KEYB4MKT                                                      
*                                                                               
VK40     MVI   NOPTFLG,0           RESET THE OPTION FLAG                        
         CLI   ACTNUM,ACTLIST      LIST/PRINT OPTIONAL                          
         BNE   *+8                                                              
         MVI   NOPTFLG,1           SET OPTIONAL FLAG                            
         SPACE                                                                  
         LA    R2,HSYESTH          * ESTIMATE                                   
         GOTO1 VALIFLD                                                          
         BNZ   VK50                                                             
         CLI   NOPTFLG,1                                                        
         BE    VK60                                                             
         MVI   ERROR,INVALID                                                    
         J     INVINPT                                                          
VK50     GOTO1 VALIEST                                                          
         MVC   SVKEY+7(1),BEST                                                  
         ZIC   R5,KEYAFMKT                                                      
         LA    R5,1(R5)                                                         
         STC   R5,KEYAFMKT                                                      
         SPACE                                                                  
VK60     XC    HSYDPTL,HSYDPTL     * DAYPART                                    
         OI    HSYDPTLH+6,X'80'                                                 
         LA    R2,HSYDPTH                                                       
         GOTO1 VALIFLD                                                          
         BNZ   VK70                                                             
         CLI   NOPTFLG,1                                                        
         BE    VK80                                                             
         MVI   ERROR,INVALID                                                    
         J     INVINPT                                                          
VK70     OI    HSYDPT+1,X'40'                                                   
         GOTO1 VALIDPT,DMCB,(0,HSYDPT)                                          
         MVC   QDPT,DPTVALUE                                                    
         MVC   SVKEY+8(1),QDPT                                                  
         MVC   HSYDPTL,DPTNAME                                                  
*                                                                               
         ZIC   R5,KEYAFMKT                                                      
         LA    R5,1(R5)                                                         
         STC   R5,KEYAFMKT                                                      
*                                                                               
VK80     XC    QLEN,QLEN                                                        
         LA    R2,HSYLENH          * SPOT LENGTH                                
         GOTO1 VALIFLD                                                          
         LTR   R0,R0                                                            
         BNZ   VK90                                                             
         CLI   NOPTFLG,1                                                        
         BE    VK100                                                            
         MVI   ERROR,INVALID                                                    
         J     INVINPT                                                          
VK90     STC   R0,SVKEY+9                                                       
         MVC   QLEN,HSYLEN                                                      
         ZIC   R5,KEYAFMKT                                                      
         LA    R5,1(R5)                                                         
         STC   R5,KEYAFMKT                                                      
*                                                                               
VK100    CLI   BPAKG,0                                                          
         BE    VK110                                                            
         OI    SVKEY+11,X'40'                                                   
         MVC   SVKEY+12,BPAKG                                                   
         ZIC   R5,KEYAFMKT                                                      
         LA    R5,3(R5)                                                         
         STC   R5,KEYAFMKT                                                      
*                                                                               
VK110    MVI   NOPTFLG,0                                                        
         OI    SVKEY+11,X'20'      READ THE GOAL HISTORY RECORD                 
*                                                                               
         CLC   LASTACT(2),CONACT                                                
         BE    VK120                                                            
         MVC   HOLDKEY,SVKEY                                                    
         XC    STRTSCRN,STRTSCRN                                                
         XC    ENDSCRN,ENDSCRN                                                  
         MVC   LASTACT,CONACT                                                   
VK120    MVC   KEY,SVKEY                                                        
         GOTO1 VSETSPT                                                          
         GOTO1 HIGH                                                             
         CLI   ACTNUM,ACTLIST                                                   
         BE    EXIT                                                             
         CLI   ACTNUM,ACTSEL                                                    
         BE    EXIT                                                             
         CLC   SVKEY(13),KEY                                                    
         BE    EXIT                                                             
*                                                                               
         CLC   SVKEY(10),KEY                                                    
         BE    VK130                                                            
*                                                                               
         CLC   HSYNET(3),=C'M=N'                                                
         BE    *+12                                                             
         CLI   HSYNETH+5,0         WAS NETWORK INPUTTED                         
         BNE   VK140               YES DON'T TEST SECOND DEFAULT                
*                                                                               
         XC    KEY,KEY                                                          
         MVC   SVKEY+5(2),=XL2'1E61'   TRY MARKET 7777                          
         MVC   KEY,SVKEY                                                        
         GOTO1 VSETSPT                                                          
         GOTO1 HIGH                                                             
         CLC   SVKEY(13),KEY                                                    
         BE    EXIT                                                             
         CLC   SVKEY(10),KEY                                                    
         BNE   VK140                                                            
VK130    OI    KEY+11,X'20'        READ THE GOAL HISTORY RECORD                 
         B     EXIT                                                             
VK140    MVC   KEY,SVKEY                                                        
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
DK       DS    0H                                                               
         CLI   SYSTFLAG,C'X'       XSPOT FILE?                                  
         BNE   *+12                                                             
         BRAS  RE,XDK                                                           
         J     EXIT                                                             
*                                                                               
         MVC   WORK2(48),KEY                                                    
         MVI   HSYMED,C'N'         MEDIA = N                                    
         FOUT  HSYMEDH                                                          
*                                                                               
         MVC   HSYCLI(3),QCLT      DISPLAY CLIENT                               
         FOUT  HSYCLIH                                                          
*                                                                               
         XC    HSYPRD,HSYPRD       DISPLAY PRODUCT                              
         MVC   BPRD,WORK2+4                                                     
         BAS   RE,GETPRD                                                        
         MVC   HSYPRD(3),QPRD                                                   
         TM    WORK2+11,X'40'      WAS PACKAGE INPUTTED                         
         BZ    DK10                                                             
*--MOVE PACKAGE OUT                                                             
         LA    R2,HSYPRD+2                                                      
         CLI   0(R2),X'40'                                                      
         BNH   *+8                                                              
         LA    R2,1(R2)                                                         
         MVI   0(R2),C'*'                                                       
         LA    R2,1(R2)                                                         
         EDIT  (1,WORK2+12),(3,0(R2)),ALIGN=LEFT                                
DK10     FOUT  HSYPRDH                                                          
*                                                                               
         LA    R3,24(R7)                                                        
         CLI   0(R3),X'20'                                                      
         BNE   DK15                                                             
         CLI   22(R3),X'40'                                                     
         BNH   DK15                                                             
         MVC   HSYNET(4),22(R3)                                                 
         B     DK19                                                             
*                                                                               
DK15     CLC   5(2,R7),=X'0309'       DEFAULT TO NETWORK                        
         BNE   *+14                   0777                                      
         MVC   HSYNET(3),=C'M=N'                                                
         B     DK19                                                             
*                                                                               
         CLC   5(2,R7),=X'1E61'       DEFAULT TO NETWORK                        
         BNE   *+14                   7777                                      
         MVC   HSYNET(3),=C'M=N'                                                
         B     DK19                                                             
*                                                                               
         CLC   5(2,R7),=X'0306'       SYNDICATION?                              
         BNE   *+14                                                             
         MVC   HSYNET(3),=C'M=S'                                                
         B     DK19                                                             
*                                                                               
         CLC   5(2,R7),=X'0307'       CABLE?                                    
         BNE   *+10                                                             
         MVC   HSYNET(3),=C'M=C'                                                
*                                                                               
DK19     FOUT  HSYNETH                                                          
*                                                                               
DK20     GOTO1 DISEST,WORK2+7                                                   
         MVC   HSYEST(3),QEST                                                   
         FOUT  HSYESTH                                                          
*                                                                               
         XC    HSYDPT,HSYDPT                                                    
         XC    HSYDPTL,HSYDPTL                                                  
         LA    R2,HSYDPTH                                                       
         GOTO1 VALIDPT,DMCB,(1,WORK2+8)                                         
         MVC   HSYDPT,DPTCODE                                                   
         MVC   HSYDPTL,DPTNAME                                                  
         FOUT  HSYDPTH                                                          
         FOUT  HSYDPTLH                                                         
*                                                                               
         EDIT  (1,WORK2+9),(3,HSYLEN),ALIGN=LEFT                                
         FOUT  HSYLENH                                                          
*                                                                               
         MVC   KEY,WORK2                                                        
         GOTO1 VSETSPT                                                          
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
* DISPLAY THE X'60' (GOAL HISTORY) ELEMENTS                                     
DR       DS    0H                                                               
         CLI   SYSTFLAG,C'X'       XSPOT FILE?                                  
         BNE   *+12                                                             
         BRAS  RE,XDR                                                           
         J     EXIT                                                             
*                                                                               
         CLI   THISLSEL,C'D'       SELECT FOR DELETE                            
         BE    INVACTN                                                          
         CLI   THISLSEL,C'C'       SELECT FOR CHANGE                            
         BE    INVACTN                                                          
         LA    R3,24(R7)           POINT TO FIRST ELEMENT                       
*                                                                               
         LA    R2,HSYLNE1H         CLEAR SCREEN                                 
         LA    RE,HSYTAGH          END OF SCREEN                                
DR10     CR    R2,RE                                                            
         BNL   DR20                                                             
         XC    8(L'HSYLNE1,R2),8(R2)                                            
         FOUT  (R2)                                                             
         ZIC   R0,0(R2)            CHECK IF ANY ROOM LEFT ON SCREEN             
         AR    R2,R0                                                            
         B     DR10                                                             
*                                                                               
DR20     LA    R2,HSYLNE1H         POINT TO FIRST LINE FOR ELEMENT DISP         
         MVI   ELCODE,X'60'                                                     
         CLI   ACTNUM,ACTDIS                                                    
         BE    DR30                                                             
         CLI   ACTNUM,ACTSEL                                                    
         BNE   DR40                                                             
DR30     MVC   STRTSCRN,ENDSCRN                                                 
DR40     XC    ENDSCRN,ENDSCRN                                                  
         BAS   RE,NEXTEL                                                        
         BNE   DR60                                                             
         OC    STRTSCRN,STRTSCRN                                                
         BZ    DR50                                                             
         CLC   2(6,R3),STRTSCRN    COMPARE DATE & TIME FROM ELEMENT             
         BL    DR40                                                             
*                                                                               
         USING GHSTEL,R3                                                        
         USING DLINED,R4                                                        
DR50     LA    R4,8(R2)                                                         
         GOTO1 DATCON,DMCB,(2,GHSTDATE),(8,DDATE)  DISPLAY DATE                 
*                                                                               
         ICM   R1,15,GHSTTIME                                                   
         SRL   R1,12               GET RID OF SECONDS AND SIGN                  
         STH   R1,HALF             AND CHANGE TO 2 BYTE PWOS                    
         GOTO1 HEXOUT,DMCB,HALF,WORK,L'HALF                                     
         MVC   DTIME(2),WORK                                                    
         MVI   DTIME+2,C':'                                                     
         MVC   DTIME+3(2),WORK+2                                                
         FOUT  (R2)                                                             
*                                                                               
         ZIC   R1,GHSTLEN                                                       
         SH    R1,=H'9'             GET LENGTH OF COMMENT                       
         LA    RE,L'PCMNT           COMPARE TO SPACE AVAILABLE ON LINE          
         BCTR  RE,0                                                             
         CR    R1,RE                                                            
         BNH   *+6                                                              
         LR    R1,RE                TOO LARGE, USE LENGTH OF LINE               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PCMNT(0),GHSTRSN                                                 
*                                                                               
         BAS   RE,NEXTEL                                                        
         BNE   DR60                NO MORE ELEMENTS TO DISPLAY                  
*                                                                               
         ZIC   R0,0(R2)            CHECK IF ANY ROOM LEFT ON SCREEN             
         AR    R2,R0                                                            
         LA    RE,HSYTAGH                                                       
         CR    R2,RE                                                            
         BL    DR50                                                             
         MVC   ENDSCRN,2(R3)       NO,SAVE EL INFO TO CONT ON NEXT SCRN         
*                                                                               
DR60     OI    CONSRVH+6,X'81'                                                  
         CLI   ACTNUM,ACTSEL       IF ACTION IS SELECT                          
         BE    DR70                                                             
         CLI   ACTNUM,ACTDIS                                                    
         BNE   EXIT                                                             
DR70     OC    ENDSCRN,ENDSCRN     IS THERE ANOTHER SCREEN TO DISPLAY           
         BZ    EXIT                                                             
         OI    GENSTAT2,RETEQSEL   SET TO RETURN THIS SELECTION                 
         B     EXIT                                                             
         DROP  R3,R4                                                            
         EJECT                                                                  
*                                                                               
LR       DS    0H                                                               
         CLI   SYSTFLAG,C'X'       XSPOT FILE?                                  
         BNE   *+12                                                             
         BRAS  RE,XLR                                                           
         J     EXIT                                                             
*                                                                               
         LA    R7,KEY                                                           
         MVC   AIO,AIO1                                                         
         MVI   NFILE,C'S'          SPOT FILE                                    
         OC    KEY(17),KEY                                                      
         BNZ   LR10                                                             
         MVC   KEY,SVKEY                                                        
*                                                                               
LR10     GOTO1 VSETSPT                                                          
         GOTO1 HIGH                                                             
         B     LR30                                                             
*                                                                               
LR20     GOTO1 VSETSPT                                                          
         GOTO1 SEQ                                                              
LR30     CLC   SVKEY(4),KEY                                                     
         BNE   LREXT                                                            
         TM    KEY+11,X'20'                                                     
         BNO   LR20                                                             
         ZIC   RF,KEYB4MKT                                                      
         BCTR  RF,0                                                             
         EX    RF,FRONTCK                                                       
         BNE   LR20                                                             
*                                                                               
         OC    KEYAFMKT,KEYAFMKT                                                
         BZ    LR40                                                             
         ZIC   RF,KEYAFMKT                                                      
         BCTR  RF,0                                                             
         EX    RF,BACKCK                                                        
         BNE   LR20                                                             
*                                                                               
LR40     DS    0H                                                               
LR50     MVC   SVKEY,KEY                                                        
         GOTO1 GETREC                                                           
*                                                                               
         LA    R5,LISTAR                                                        
         USING LLINED,R5                                                        
         XC    LISTAR,LISTAR                                                    
*                                                                               
         MVC   BPRD,GKEYPRD                                                     
         BAS   RE,GETPRD                                                        
         LA    R2,LRPROD                                                        
         L     R3,AIO                                                           
         LA    R3,24(R3)                                                        
         TM    17(R3),X'80'        CHECK PRIORITY PRODUCT BIT                   
         BZ    LR60                                                             
         MVI   0(R2),C'*'                                                       
         LA    R2,1(R2)                                                         
LR60     MVC   0(3,R2),QPRD                                                     
*--MOVE PACKAGE CODE OUT IF ON THE RECORD                                       
         TM    GKEYAGY,X'40'                                                    
         BZ    LR90                                                             
         LA    RE,3                                                             
LR70     CLI   0(R2),X'40'                                                      
         BE    LR80                                                             
         LA    R2,1(R2)                                                         
         BCT   RE,LR70                                                          
LR80     MVI   0(R2),C'/'                                                       
         EDIT  (1,GKEYPRD2),(3,1(R2)),ALIGN=LEFT                                
*                                                                               
LR90     DS    0H                                                               
         USING GDELEM,R3                                                        
         CLI   GDNETWK,X'40'                                                    
         BNH   LR95                                                             
         MVC   LRNETWK,GDNETWK                                                  
         B     LR100                                                            
         DROP  R3                                                               
*                                                                               
LR95     DS    0H                                                               
         MVI   LRNETWK,C'N'          DEFAULT FOR NETWORK                        
         CLC   5(2,R7),=X'0309'                                                 
         BE    LR100                                                            
*                                                                               
         CLC   5(2,R7),=X'0306'                                                 
         BNE   *+12                                                             
         MVI   LRNETWK,C'S'          SYNDICATION                                
         B     LR100                                                            
*                                                                               
         CLC   5(2,R7),=X'0307'                                                 
         BNE   *+8                                                              
         MVI   LRNETWK,C'C'          CABLE                                      
*                                                                               
LR100    DS    0H                                                               
         XC    LREST,LREST                                                      
         EDIT  (1,GKEYEST),(3,LREST),ALIGN=LEFT                                 
*--MOVE DAYPART OUT TO THE LIST LINE                                            
         MVC   WORK2(1),GKEYDPT                                                 
         MVC   WORK2+1(13),GKEY  SAVE THE KEY                                   
         GOTO1 VALIDPT,DMCB,(1,WORK2)                                           
         MVI   NFILE,C'S'                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(13),WORK2+1                                                  
         GOTO1 HIGH                 REPOSITION THE POINTER                      
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   LRDPT(8),DPTNAME                                                 
*                                                                               
*--MOVE LENGTH OUT TO THE LIST LINE                                             
         XC    LRLEN,LRLEN                                                      
         EDIT  (1,GKEYSLN),(3,LRLEN),ALIGN=LEFT                                 
*                                                                               
         GOTO1 LISTMON                                                          
         B     LR20                GOTO READ SEQ                                
*                                                                               
LREXT    DS    0H                                                               
         B     EXIT                                                             
         DROP  R5                                                               
FRONTCK  CLC   SVKEY(0),KEY                                                     
BACKCK   CLC   SVKEY+7(0),KEY+7                                                 
         EJECT                                                                  
         EJECT                                                                  
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),0                                                          
         BE    NEXTEL2                                                          
         CLC   ELCODE,0(R3)                                                     
         BER   RE                                                               
         B     NEXTEL                                                           
*                                                                               
NEXTEL2  LTR   R3,R3                                                            
         BR    RE                                                               
         SPACE 2                                                                
*                                                                               
GETPRD   NTR1                                                                   
         LA    RE,255                                                           
         LA    RF,SVCLIST                                                       
*--MOVE PRODUCT OUT TO THE LIST LINE                                            
GP300    CLC   3(1,RF),BPRD                                                     
         BE    GP310                                                            
         LA    RF,4(RF)                                                         
         BCT   RE,GP300                                                         
         DC    H'0'                                                             
GP310    MVC   QPRD,0(RF)                                                       
GPEX     XIT1                                                                   
NEXTUN   DS    0H                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0             END OF SCREEN                                
         BER   RE                                                               
         TM    1(R2),X'20'         TEST PROTECTED                               
         BO    NEXTUN                                                           
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINES FOR ELEMENT MAINTENANCE                                          
*                                                                               
DELEL    LR    R0,RE                                                            
         GOTO1 HELLO,DMCB,(C'D',=C'SPTFILE'),(ELCODE,0(R7)),0                   
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
PUTEL    LR    R0,RE                                                            
         GOTO1 HELLO,DMCB,(C'P',=C'SPTFILE'),0(R7),ELEM                         
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
         DROP  R7                                                               
*********************************************************************           
*        VALIDATE KEY                                                           
*********************************************************************           
XVK      NTR1  BASE=*,LABEL=*                                                   
         MVI   NETFILT,0                                                        
         MVI   NLISTS,14           SET GENCON LIST NUMBER TO 14                 
*                                                                               
         XC    SVMED,SVMED                                                      
         XC    SVCLI,SVCLI                                                      
         XC    SVPRD,SVPRD                                                      
         XC    SVMKT,SVMKT                                                      
         XC    SVEST,SVEST                                                      
         XC    SVDPT,SVDPT                                                      
         XC    SVSLN,SVSLN                                                      
         XC    SVSEC,SVSEC                                                      
         XC    SVAGY,SVAGY                                                      
         XC    SVABOVE,SVABOVE                                                  
         XC    SVPRDA,SVPRDA                                                    
*                                                                               
         LA    R2,HSYMEDH           MEDIA                                       
         CLI   8(R2),C'*'           PLANNED?                                    
         BNE   XVK20                                                            
         OI    SVAGY,X'10'                                                      
         MVC   DUMMYH(8),=X'0900000000010000'                                   
         MVC   DUMMYH+8(1),9(R2)                                                
         LA    R2,DUMMYH                                                        
XVK20    GOTO1 VALIMED                                                          
         MVC   SVMED,BAGYMD                                                     
*                                                                               
         LA    R2,HSYCLIH           CLIENT                                      
         CLI   5(R2),0                                                          
         JE    MISSERR                                                          
         GOTO1 VALICLT                                                          
         MVC   SVCLI,BCLT                                                       
*                                                                               
         MVI   BPAKG,0                                                          
*                                                                               
         LA    R2,HSYPRDH           PRODUCT                                     
         CLI   5(R2),0                                                          
         BNE   XVK40                                                            
         CLI   ACTNUM,ACTLIST                                                   
         BE    XVK50                                                            
         J     MISSERR                                                          
*                                                                               
XVK40    DS    0H                                                               
         GOTO1 VALIPRD                                                          
         MVC   SVPRD,BPRD                                                       
         MVC   SVPRDA,QPRD                                                      
*                                                                               
XVK50    DS    0H                                                               
         XC    QNET,QNET                                                        
         LA    R2,HSYNETH          NETWORK                                      
         CLI   5(R2),0                                                          
         BE    XVK80                                                            
*                                                                               
XVK60    DS    0H                                                               
         MVI   NETFILT,C'N'                                                     
         CLC   8(2,R2),=C'M='      MEDIA TYPE?                                  
         BNE   XVK65                                                            
*                                                                               
         CLI   10(R2),C'N'         NETWORK?                                     
         BE    XVK80                                                            
*                                                                               
         MVI   NETFILT,C'S'                                                     
         CLI   10(R2),C'S'         SYNDICATION?                                 
         BNE   *+14                                                             
         MVC   SVMKT,=X'0306'      SYNDICATION = 0774                           
         B     XVK80                                                            
*                                                                               
         MVI   NETFILT,C'C'                                                     
         CLI   10(R2),C'C'         CABLE?                                       
         JNE   INVINPT                                                          
         MVC   SVMKT,=X'0307'      CABLE = 0775                                 
         B     XVK80                                                            
*                                                                               
XVK65    DS    0H                                                               
         GOTO1 VALINTWK                                                         
         MVC   SVMKT,QNETMKT                                                    
         MVI   NETFILT,0                                                        
         B     XVK90                                                            
*                                                                               
XVK80    DS    0H                                                               
         CLI   ACTNUM,ACTLIST                                                   
         BE    XVK90                                                            
         CLI   N2PROF+15,C'Y'      IS NETWORK REQUIRED ON ADDS                  
         JE    MISSERR             YES ERROR                                    
         MVC   SVMKT,=XL2'0309'    MOVE MARKET '777' INTO KEY                   
*                                                                               
XVK90    DS    0H                                                               
         LA    R2,HSYESTH          ESTIMATE                                     
         CLI   5(R2),0                                                          
         BNE   XVK100                                                           
         CLI   ACTNUM,ACTLIST                                                   
         BE    XVK120                                                           
         J     MISSERR                                                          
*                                                                               
XVK100   DS    0H                                                               
         CLI   ACTNUM,ACTLIST                                                   
         BNE   XVK110                                                           
         OC    QPRD,QPRD                                                        
         BNZ   XVK110                                                           
         MVC   QPRD,=C'POL'                                                     
*                                                                               
XVK110   GOTO1 VALIEST                                                          
         MVC   SVEST,BEST                                                       
*                                                                               
         L     R3,AIO                                                           
         USING ESTHDR,R3                                                        
         MVC   MYEDAILY,EDAILY                                                  
         GOTO1 DATCON,DMCB,(0,SVBEGIN),(2,MYESTART)                             
         GOTO1 DATCON,DMCB,(0,SVEND),(2,MYEEND)                                 
         DROP  R3                                                               
*                                                                               
XVK120   DS    0H                                                               
         XC    HSYDPTL,HSYDPTL     DAYPART                                      
         OI    HSYDPTLH+6,X'80'                                                 
         LA    R2,HSYDPTH                                                       
         CLI   5(R2),0                                                          
         BNE   XVK130                                                           
         CLI   ACTNUM,ACTLIST                                                   
         BE    XVK140                                                           
         J     INVINPT                                                          
*                                                                               
XVK130   DS    0H                                                               
         OI    HSYDPT+1,X'40'                                                   
         GOTO1 VALIDPT,DMCB,(0,HSYDPT)                                          
         MVC   QDPT,DPTVALUE                                                    
         MVC   SVDPT,QDPT                                                       
         MVC   HSYDPTL,DPTNAME                                                  
*                                                                               
XVK140   DS    0H                                                               
         XC    QLEN,QLEN                                                        
         MVI   NOPTFLG,1                                                        
         LA    R2,HSYLENH           SPOT LENGTH                                 
         GOTO1 VALIFLD                                                          
         LTR   R0,R0                                                            
         BNZ   XVK150                                                           
         CLI   ACTNUM,ACTLIST                                                   
         BE    XVK160                                                           
         J     INVINPT                                                          
*                                                                               
XVK150   DS    0H                                                               
         STC   R0,SVSLN                                                         
         MVC   QLEN,HSYLEN                                                      
*                                                                               
XVK160   DS    0H                                                               
         MVI   NOPTFLG,0                                                        
         CLI   BPAKG,0                                                          
         BE    XVK170                                                           
         OI    SVAGY,X'40'                                                      
         MVC   SVABOVE,BPAKG                                                    
*                                                                               
XVK170   DS    0H                                                               
*                                                                               
XVK200   DS    0H                                                               
*                                                                               
XVK300   DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    RF,KEY                                                           
         USING NGOLRECD,RF                                                      
*                                                                               
         OI    SVAGY,X'20'         ONLY GET HISTORY RECORDS                     
*                                                                               
         MVI   GXKEYTYP,GKEYTYPQ                                                
         MVC   GXKEYAM,SVMED                                                    
         MVC   GXKEYCLT,SVCLI                                                   
         MVC   GXKEYPRD,SVPRD                                                   
         MVC   GXKEYMKT,SVMKT                                                   
         MVC   GXKEYEST,SVEST                                                   
         MVC   GXKEYDPT,SVDPT                                                   
         MVC   GXKEYSLN,SVSLN                                                   
         MVC   GXKEYSEC,SVSEC                                                   
         MVC   GXKEYAGY,SVAGY                                                   
         MVC   GXKEYAGY+1(1),SVABOVE                                            
         MVC   GXKPRDA,SVPRDA                                                   
         DROP  RF                                                               
*                                                                               
         MVC   SVXKEY,KEY                                                       
*                                                                               
         CLI   ACTNUM,ACTLIST                                                   
         BE    *+12                                                             
         CLI   ACTNUM,ACTSEL                                                    
         BNE   XVK320                                                           
*                                                                               
         GOTO1 VSETXSP                                                          
         GOTO1 HIGH                                                             
         B     XVKSPX                                                           
*                                                                               
XVK320   DS    0H                                                               
         GOTO1 VSETXSP                                                          
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(16),KEYSAVE                                                  
         BE    XVKSPX                                                           
*                                                                               
         MVC   SVMKT,=XL2'1E61'   TRY MARKET 7777                               
*                                                                               
         CLC   HSYNET(3),=C'M=N'                                                
         BNE   *+14                                                             
         MVC   SVMKT,=XL2'0309'   TRY MARKET 0777                               
         B     *+12                                                             
*                                                                               
XVK330   DS    0H                                                               
         CLI   HSYNETH+5,0         WAS NETWORK INPUTTED                         
         BE    XVK335                                                           
         CLC   HSYNET(2),=C'M='                                                 
         BE    XVK340                                                           
         MVC   SVMKT,QNETMKT                                                    
         B     XVK340              YES DON'T TEST SECOND DEFAULT                
*                                                                               
XVK335   DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY,SVXKEY                                                       
         LA    RF,KEY                                                           
         USING NGOLRECD,RF                                                      
         MVC   GXKEYMKT,SVMKT                                                   
         DROP  RF                                                               
*                                                                               
         GOTO1 VSETXSP                                                          
         GOTO1 HIGH                                                             
         CLC   KEY(16),KEYSAVE                                                  
         BE    XVKSPX                                                           
*                                                                               
XVK340   DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY,SVXKEY                                                       
         LA    RF,KEY                                                           
         USING NGOLRECD,RF                                                      
         MVC   GXKEYMKT,SVMKT                                                   
         DROP  RF                                                               
*                                                                               
XVKSPX   DS    0H                                                               
         MVC   SVXKEY,KEY                                                       
*                                                                               
XVKX     DS    0H                                                               
         CLI   ACTNUM,ACTADD                                                    
         JE    EXIT                                                             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(32),SVXKEY           RESTORE XSPOT KEY                       
         GOTO1 VSETXSP                                                          
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(16),KEYSAVE                                                  
         JNE   RECNFND                                                          
         J     EXIT                                                             
MISSERR  DS    0H                                                               
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
INVACTN  DS    0H                                                               
         LA    R2,CONACTH                                                       
         MVI   ERROR,INVACT                                                     
         B     TRAPERR                                                          
*                                                                               
INVINPT  DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
RECNFND  DS    0H                                                               
         MVI   ERROR,NOTFOUND                                                   
         B     TRAPERR                                                          
*                                                                               
RSNCDERR DS    0H                                                               
         MVI   ERROR,RSNCDERQ                                                   
         B     TRAPERR                                                          
*                                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(30),=C'* REASON FOR CHANGE REQUIRED *'                   
         FOUT  CONHEADH                                                         
         GOTO1 ERREX2                                                           
*                                                                               
INVINP   MVI   ERROR,INVALID                                                    
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
         PRINT NOGEN                                                            
         LTORG                                                                  
*****************************************************************               
*                                                                               
*****************************************************************               
XDK      NTR1  BASE=*,LABEL=*                                                   
XDK010   L     R7,AIO                                                           
         USING NGOLRECD,R7                                                      
*                                                                               
         MVC   HOLDXKEY,KEY                                                     
*                                                                               
         MVC   HSYMED(2),=C'N '                                                 
         TM    GXKEYAGY,GXKEYTAR   PLANNED GOAL?                                
         BZ    *+10                                                             
         MVC   HSYMED(2),=C'*N'                                                 
         FOUT  HSYMEDH                                                          
*                                                                               
         MVC   HSYCLI(3),QCLT                                                   
         FOUT  HSYCLIH                                                          
*                                                                               
         XC    HSYPRD,HSYPRD                                                    
         MVC   HSYPRD(3),GXKPRDA                                                
*                                                                               
         TM    GXKEYAGY,X'40'      WAS PACKAGE INPUTTED                         
         BZ    XDK080                                                           
*                                                                               
*--MOVE PACKAGE OUT                                                             
*                                                                               
         LA    R2,HSYPRD+2                                                      
         CLI   0(R2),X'40'                                                      
         BNH   *+8                                                              
         LA    R2,1(R2)                                                         
         MVI   0(R2),C'*'                                                       
         AHI   R2,1                                                             
         EDIT  (1,GXKEYAGY),(3,0(R2)),ALIGN=LEFT                                
*                                                                               
XDK080   FOUT  HSYPRDH                                                          
*                                                                               
         XC    HSYNET,HSYNET                                                    
*                                                                               
         LA    R3,42(R7)                                                        
         CLI   0(R3),X'20'                                                      
         BNE   XDK090                                                           
         CLI   22(R3),X'40'                                                     
         BNH   XDK090                                                           
         MVC   HSYNET(4),22(R3)                                                 
         B     XDK095                                                           
*                                                                               
XDK090   CLC   GXKEYMKT,=X'0309'       DEFAULT TO NETWORK                       
         BNE   *+14                   0777                                      
         MVC   HSYNET(3),=C'M=N'                                                
         B     XDK095                                                           
*                                                                               
         CLC   GXKEYMKT,=X'0306'       SYNDICATION?                             
         BNE   *+14                                                             
         MVC   HSYNET(3),=C'M=S'                                                
         B     XDK095                                                           
*                                                                               
         CLC   GXKEYMKT,=X'0307'       CABLE?                                   
         BNE   *+10                                                             
         MVC   HSYNET(3),=C'M=C'                                                
*                                                                               
XDK095   FOUT  HSYNETH                                                          
*                                                                               
XDK100   DS    0H                                                               
         EDIT  (1,GXKEYEST),(3,HSYEST),ALIGN=LEFT                               
         FOUT  HSYESTH                                                          
*                                                                               
         MVC   QPRD,GXKPRDA                                                     
         GOTO1 DISEST,GXKEYEST                                                  
         L     RE,AIO3                                                          
         USING ESTHDR,RE                                                        
         MVC   MYEDAILY,EDAILY                                                  
         MVC   TUSRNM,EUSRNMS                                                   
         DROP  RE                                                               
*                                                                               
         MVC   AIO,AIO1                                                         
*                                                                               
XDK200   XC    HSYDPTL,HSYDPTL                                                  
         XC    HSYDPT,HSYDPT                                                    
         LA    R2,HSYDPTH                                                       
         GOTO1 VALIDPT,DMCB,(1,GXKEYDPT)                                        
         MVC   HSYDPT,DPTCODE                                                   
         MVC   HSYDPTL,DPTNAME                                                  
XDK250   FOUT  HSYDPTH                                                          
         FOUT  HSYDPTLH                                                         
*                                                                               
         GOTO1 VSETXSP                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(32),HOLDXKEY                                                 
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
*                                                                               
XDK300   EDIT  (1,GXKEYSLN),(3,HSYLEN),ALIGN=LEFT                               
         FOUT  HSYLENH                                                          
*                                                                               
XDK400   MVC   KEY,HOLDXKEY                                                     
         GOTO1 VSETXSP                                                          
         J     EXIT                                                             
         LTORG                                                                  
*************************************************************                   
*                                                                               
*************************************************************                   
XDR      NTR1  BASE=*,LABEL=*                                                   
         L     R7,AIO                                                           
         LA    R3,42(R7)           POINT TO FIRST ELEMENT                       
*                                                                               
         LA    R2,HSYLNE1H         CLEAR SCREEN                                 
         LA    RE,HSYTAGH          END OF SCREEN                                
XDR10    CR    R2,RE                                                            
         BNL   XDR20                                                            
         XC    8(L'HSYLNE1,R2),8(R2)                                            
         FOUT  (R2)                                                             
         ZIC   R0,0(R2)            CHECK IF ANY ROOM LEFT ON SCREEN             
         AR    R2,R0                                                            
         B     XDR10                                                            
*                                                                               
XDR20    LA    R2,HSYLNE1H         POINT TO FIRST LINE FOR ELEMENT DISP         
         MVI   ELCODE,X'60'                                                     
         CLI   ACTNUM,ACTDIS                                                    
         BE    XDR30                                                            
         CLI   ACTNUM,ACTSEL                                                    
         BNE   XDR40                                                            
XDR30    MVC   STRTSCRN,ENDSCRN                                                 
XDR40    XC    ENDSCRN,ENDSCRN                                                  
         BAS   RE,NXTEL                                                         
         BNE   XDR60                                                            
         OC    STRTSCRN,STRTSCRN                                                
         BZ    XDR50                                                            
         CLC   2(6,R3),STRTSCRN    COMPARE DATE & TIME FROM ELEMENT             
         BL    XDR40                                                            
*                                                                               
         USING GHSTEL,R3                                                        
         USING DLINED,R4                                                        
XDR50    LA    R4,8(R2)                                                         
         GOTO1 DATCON,DMCB,(2,GHSTDATE),(8,DDATE)  DISPLAY DATE                 
*                                                                               
         ICM   R1,15,GHSTTIME                                                   
         SRL   R1,12               GET RID OF SECONDS AND SIGN                  
         STH   R1,HALF             AND CHANGE TO 2 BYTE PWOS                    
         GOTO1 HEXOUT,DMCB,HALF,WORK,L'HALF                                     
         MVC   DTIME(2),WORK                                                    
         MVI   DTIME+2,C':'                                                     
         MVC   DTIME+3(2),WORK+2                                                
         FOUT  (R2)                                                             
*                                                                               
         ZIC   R1,GHSTLEN                                                       
         SH    R1,=H'9'             GET LENGTH OF COMMENT                       
         LA    RE,L'PCMNT           COMPARE TO SPACE AVAILABLE ON LINE          
         BCTR  RE,0                                                             
         CR    R1,RE                                                            
         BNH   *+6                                                              
         LR    R1,RE                TOO LARGE, USE LENGTH OF LINE               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PCMNT(0),GHSTRSN                                                 
*                                                                               
         ZIC   RF,1(R3)                                                         
         AR    R3,RF                                                            
         CLI   0(R3),X'60'                                                      
         BNE   XDR60               NO MORE ELEMENTS TO DISPLAY                  
*                                                                               
         ZIC   R0,0(R2)            CHECK IF ANY ROOM LEFT ON SCREEN             
         AR    R2,R0                                                            
         LA    RE,HSYTAGH                                                       
         CR    R2,RE                                                            
         BL    XDR50                                                            
         MVC   ENDSCRN,2(R3)       NO,SAVE EL INFO TO CONT ON NEXT SCRN         
*                                                                               
XDR60    OI    CONSRVH+6,X'81'                                                  
         CLI   ACTNUM,ACTSEL       IF ACTION IS SELECT                          
         BE    XDR70                                                            
         CLI   ACTNUM,ACTDIS                                                    
         JNE   EXIT                                                             
XDR70    OC    ENDSCRN,ENDSCRN     IS THERE ANOTHER SCREEN TO DISPLAY           
         JZ    EXIT                                                             
         OI    GENSTAT2,RETEQSEL   SET TO RETURN THIS SELECTION                 
         J     EXIT                                                             
*                                                                               
NXTEL    DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),0                                                          
         BE    NXTEL2                                                           
         CLC   ELCODE,0(R3)                                                     
         BER   RE                                                               
         B     NXTEL                                                            
*                                                                               
NXTEL2   LTR   R3,R3                                                            
         BR    RE                                                               
         SPACE 2                                                                
         DROP  R3,R4                                                            
*************************************************************                   
*                                                                               
*************************************************************                   
XLR      NTR1  BASE=*,LABEL=*                                                   
         MVC   AIO,AIO1                                                         
         L     R7,AIO                                                           
         USING NGOLRECD,R7                                                      
*                                                                               
         OC    KEY(17),KEY                                                      
         BNZ   *+10                                                             
         MVC   KEY,SVXKEY                                                       
*                                                                               
         GOTO1 VSETXSP                                                          
         GOTO1 HIGH                                                             
         B     XLR30                                                            
*                                                                               
XLR20    DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
XLR30    CLC   KEY(4),KEYSAVE                                                   
         BNE   XLREXT                                                           
         TM    KEY+11,X'20'                                                     
         BNO   XLR20                                                            
         CLI   HSYMED,C'*'         PLANNED?                                     
         BNE   *+12                                                             
         TM    KEY+11,X'10'                                                     
         BZ    XLR20                                                            
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         LA    R5,LISTAR                                                        
         USING LLINED,R5                                                        
         XC    LISTAR,LISTAR                                                    
*                                                                               
         TM    GXKEYAGY,X'10'       PLANNED GOAL?                               
         BZ    *+8                                                              
         MVI   LRPLAN,C'*'                                                      
*                                                                               
         OC    SVPRDA,SVPRDA                                                    
         BZ    *+14                                                             
         CLC   GXKPRDA,SVPRDA       SAME PRODUCT?                               
         BNE   XLR20                                                            
*                                                                               
         OC    SVMKT,SVMKT                                                      
         BZ    *+14                                                             
         CLC   GXKEYMKT,SVMKT                                                   
         BNE   XLR20                                                            
*                                                                               
         OC    SVDPT,SVDPT                                                      
         BZ    *+14                                                             
         CLC   GXKEYDPT,SVDPT                                                   
         BNE   XLR20                                                            
*                                                                               
         OC    SVEST,SVEST                                                      
         BZ    *+14                                                             
         CLC   GXKEYEST,SVEST                                                   
         BNE   XLR20                                                            
*                                                                               
         OC    SVSLN,SVSLN                                                      
         BZ    *+14                                                             
         CLC   GXKEYSLN,SVSLN                                                   
         BNE   XLR20                                                            
*                                                                               
         LA    R2,LRPROD                                                        
         MVC   0(3,R2),GXKPRDA                                                  
*--MOVE PACKAGE CODE OUT IF ON THE RECORD                                       
         TM    GXKEYAGY,X'40'                                                   
         BZ    XLR90                                                            
         LA    RE,3                                                             
XLR70    CLI   0(R2),X'40'                                                      
         BE    XLR80                                                            
         LA    R2,1(R2)                                                         
         BCT   RE,XLR70                                                         
XLR80    MVI   0(R2),C'/'                                                       
         MVC   1(3,R2),GXKPRDA2                                                 
*                                                                               
XLR90    DS    0H                                                               
         LA    R3,42(R7)                                                        
         USING GDELEM,R3                                                        
         CLI   GDNETWK,X'40'                                                    
         BNH   XLR95                                                            
         MVC   LRNETWK,GDNETWK                                                  
         B     XLR100                                                           
         DROP  R3                                                               
*                                                                               
XLR95    DS    0H                                                               
         MVI   LRNETWK,C'N'          DEFAULT FOR NETWORK                        
         CLC   5(2,R7),=X'0309'                                                 
         BE    XLR100                                                           
*                                                                               
         CLC   5(2,R7),=X'0306'                                                 
         BNE   *+12                                                             
         MVI   LRNETWK,C'S'          SYNDICATION                                
         B     XLR100                                                           
*                                                                               
         CLC   5(2,R7),=X'0307'                                                 
         BNE   *+8                                                              
         MVI   LRNETWK,C'C'          CABLE                                      
*                                                                               
XLR100   DS    0H                                                               
         XC    LREST,LREST                                                      
         EDIT  (1,GXKEYEST),(3,LREST),ALIGN=LEFT                                
*                                                                               
*--MOVE DAYPART OUT TO THE LIST LINE                                            
         MVC   SVXKEY,KEY                                                       
         GOTO1 VALIDPT,DMCB,(1,GXKEYDPT)                                        
         GOTO1 VSETXSP                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(32),SVXKEY                                                   
         GOTO1 HIGH                 REPOSITION THE POINTER                      
         GOTO1 GETREC                                                           
*                                                                               
         MVC   LRDPT(8),DPTNAME                                                 
*                                                                               
*--MOVE LENGTH OUT TO THE LIST LINE                                             
         XC    LRLEN,LRLEN                                                      
         EDIT  (1,GKEYSLN),(3,LRLEN),ALIGN=LEFT                                 
*                                                                               
         GOTO1 LISTMON                                                          
*                                                                               
         MVC   SVXKEY,KEY                                                       
         B     XLR20               GOTO READ SEQ                                
*                                                                               
XLREXT   DS    0H                                                               
         J     EXIT                                                             
         DROP  R7,R3                                                            
         EJECT                                                                  
*************************************************************                   
         SPACE 2                                                                
LLINED   DSECT                                                                  
LRPLAN   DS    CL1                                                              
LRPROD   DS    CL7                                                              
         DS    CL1                                                              
LRNETWK  DS    CL4                                                              
         DS    CL1                                                              
LREST    DS    CL3                                                              
         DS    CL1                                                              
LRDPT    DS    CL14                                                             
         DS    CL1                                                              
LRLEN    DS    CL3                                                              
         EJECT                                                                  
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENCLT                                                       
NGOLRECD DSECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDFLDIND                                                       
         EJECT                                                                  
       ++INCLUDE NEGOLFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEGOLF3D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NEGOLF4D                                                       
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
*                                                                               
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
       ++INCLUDE NEGENUSER                                                      
         EJECT                                                                  
       ++INCLUDE NEGOLWORK                                                      
         ORG   SYSSPARE                                                         
*                           *******  T31403 WORK AREA  *******                  
INVINPTQ EQU   241                                                              
ESIZERRQ EQU   242                                                              
CHGADDQ  EQU   243                                                              
INVESTMQ EQU   244                                                              
INVDPTQ  EQU   245                                                              
NOHELPAQ EQU   246                                                              
RSNCDERQ EQU   247                                                              
*                           *******  T31403 WORK AREA  *******                  
WORKAREA DS    0CL500                                                           
MYWORKD  DS    CL20                BASE MODULE WORK AREA                        
QLEN     DS    CL3                                                              
WORK2    DS    CL64                                                             
CHGSW    DS    CL1                 SCREEN CHANGE SWITCH                         
INPSW    DS    CL1                 SCREEN INPUT SWITCH                          
HOLDKEY  DS    CL13                SECONDARY KEY SAVE AREA                      
LASTACT  DS    CL2                 LAST ACTION                                  
KEYB4MKT DS    CL1                 LENGTH OF KEY CHECK FOR LIST                 
KEYAFMKT DS    CL1                 LENGTH OF KEY CHECK FOR LIST                 
*PACKED STORAGE AREA USED TO CALCULATE DOLLARS WHEN TOTAL IS USED               
PACKWORK DS    PL16                WORK AREA                                    
STRTSCRN DS    CL6                 SAVE DATE(4) AND TIME(2) FROM ELEM           
ENDSCRN  DS    CL6                 OF LAST ELEMENT DISPLAYED                    
*                                                                               
NETFILT  DS    XL1                 NETWORK FILTER FLAG (N,C,S)                  
*                                                                               
SVMED    DS    XL1                 MEDIA                                        
SVCLI    DS    XL2                 CLIENT                                       
SVPRD    DS    XL1                 PRODUCT                                      
SVMKT    DS    XL2                 MARKET                                       
SVEST    DS    XL1                 ESTIMATE                                     
SVDPT    DS    XL1                 DAYPART                                      
SVSLN    DS    XL1                 SPOT LENGTH                                  
SVSEC    DS    XL1                 SECONDS                                      
SVAGY    DS    XL1                 AGENCY CODE                                  
SVABOVE  DS    XL1                                                              
SVPRDA   DS    XL3                 PRODUCT ALPHA                                
*                                                                               
SVTGWKC  DS    XL2                 GOAL WEEK FOR TOTALS                         
*                                                                               
SYSTFLAG DS    CL1                 (S)POT, (X)FILE                              
*                                                                               
SVXKEY   DS    XL32                SAVED XFILE KEY                              
HOLDXKEY DS    CL32                XFILE AREA                                   
SVSKEY   DS    XL13                SAVED SPOT KEY                               
*                                                                               
MYEDAILY DS    X                                                                
MYESTART DS    XL2                 COMPRESSED EST START DATE                    
MYEEND   DS    XL2                 COMPRESSED EST END DATE                      
TUSRNM   DS    CL28                USER DEMO NAMES                              
*                                                                               
DUMMYH   DS    XL9                                                              
*                                                                               
DLINED   DSECT                                                                  
DDATE    DS    CL8                                                              
         DS    CL1                                                              
DTIME    DS    CL5                                                              
         DS    CL1                                                              
PCMNT    DS    CL63                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008NEGOL03   08/09/07'                                      
         END                                                                    
