*          DATA SET SPWRI23    AT LEVEL 011 AS OF 10/16/06                      
*PHASE T20423A,*                                                                
*                                                                               
*********************************************************************           
*                                                                   *           
*          SPWRI23 (T20423) - BUY CHECKING REPORT                   *           
*                                                                   *           
*-------------------------------------------------------------------*           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* 17DEC04 09 AKT -- FLAGS ALREADY DEFINED IN SPWRIWORKD             *           
* 06JAN98 08 EFJ -- SKIP IO HOOK FOR PROCGL                         *           
*                -- CLEAR STATBYTE FOR EACH NEW BGR REC             *           
* 05JAN98 07 EFJ -- KEEP AROUND SKIP FLAG FOR NEXT BUY(S)/GOAL(S)   *           
* 12AUG97 06 EFJ -- INCLUDE GOAL RECS TO GET DATA BGR RECS FOR      *           
*                   WHICH THERE ARE GOALS BUT NO BUYS               *           
* 29JAN97 05 EFJ -- COMMENTS ETC. NEED TO BE PRINTED AT EST LEV     *           
*                -- IF NOT PRINTING FLIGHTS, USE STATUS FROM MKT    *           
*                   LEVEL AND LATEST APPROVAL DATE FROM FLIGHTS     *           
* 13JAN97 04 EFJ -- ONLY PRINT RCV DATE AT MKT LEVEL (LIKE COMMENTS)*           
* 08JAN97 03 EFJ -- CHANGES TO SOME OPTIONS                         *           
* 26NOV96 02 EFJ -- SUPPORT UDEF AS ROW                             *           
*                -- CHANGE STATUS R TO H                            *           
*                -- OPTIONS TO SUPPRESS COLUMNS                     *           
*                -- VARIABLE COMMENT OUTPUT LENGTH                  *           
* 28AUG96 01 EFJ -- INITIAL DEVELOPMENT                             *           
*                                                                   *           
*                                                                   *           
*********************************************************************           
         TITLE 'T20423 - SPOTPAK WRITER BUY CHECKING REPORT'                    
T20423   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKL,T20423**,RA,RR=R2                                          
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
         LA    R0,AXTRAN           SET EXTENTION ROUTINES                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         L     R1,=A(T20423X)                                                   
         A     R1,RELO                                                          
         ST    R1,AXTRA(RE)                                                     
         STC   RF,AXTRA(RE)                                                     
         LA    RF,1(RF)                                                         
         LA    RE,4(RE)                                                         
         BCT   R0,*-16                                                          
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
XIT      XIT1  ,                                                                
         EJECT                                                                  
* INITIALIZATION                                                                
*                                                                               
INIT     DS    0H                                                               
*        OI    SBQSKIP,SBQSKGL+SBQSKBIL                                         
         OI    SBQSKIP,SBQSKBIL                                                 
         OI    SBQPER,SBQPWK                                                    
         MVI   SBQPERLO,1                                                       
         MVI   SBQPERHI,X'FF'                                                   
         OI    SBEUDEF,SBEUEST1    SET UDEF EXTRACT FIELD                       
*                                                                               
         MVI   MKTIND,X'FF'        WILL GET PROCBUY IN 23 BEFORE IN 01          
*                                                                               
         CLI   RPTSCRN,0           TEST USER REPORT SCREEN                      
         BE    INITX                NO                                          
*                                                                               
         MVI   COMMSW,C'N'         DEFAULT IS NO COMMENTS                       
         CLI   BYCCMTH+5,0                                                      
         BE    INIT10                                                           
         LA    R2,BYCCMTH                                                       
         CLI   BYCCMT,C'N'                                                      
         BE    INIT10                                                           
         CLI   BYCCMT,C'Y'                                                      
         BNE   EINV                                                             
         MVI   COMMSW,C'Y'                                                      
*                                                                               
INIT10   DS    0H                                                               
         LA    RF,OPTIONS                                                       
         LA    R0,L'OPTIONS                                                     
         LA    R2,BYCBUYH                                                       
         SR    RE,RE                                                            
         MVC   OPTIONS(6),=C'YYYYYY'    OPTIONS 1-6 DEFAULT Y                   
*                                                                               
INIT12   CLI   5(R2),0                                                          
         BE    INIT14                                                           
         CLI   8(R2),C'Y'                                                       
         BE    *+12                                                             
         CLI   8(R2),C'N'                                                       
         BNE   EINV                                                             
         MVC   0(1,RF),8(R2)                                                    
*                                                                               
INIT14   LA    RF,1(RF)                                                         
         IC    RE,0(R2)                                                         
         AR    R2,RE                                                            
         IC    RE,0(R2)                                                         
         AR    R2,RE                                                            
         BCT   R0,INIT12                                                        
*                                                                               
         OI    STATSW,STATALL      DEFAULT TO STATUS ALL                        
         CLI   BYCSTATH+5,0                                                     
         BE    INIT20                                                           
         LA    R2,BYCSTATH                                                      
         NI    STATSW,X'FF'-STATALL                                             
         CLC   =C'AP',BYCSTAT      APPROVED                                     
         BNE   *+12                                                             
         OI    STATSW,STATAPP                                                   
         B     INIT20                                                           
         CLC   =C'RJ',BYCSTAT      REJECTED                                     
         BNE   *+12                                                             
         OI    STATSW,STATREJ                                                   
         B     INIT20                                                           
         CLC   =C'UN',BYCSTAT      UNALLOCATED                                  
         BNE   *+12                                                             
         OI    STATSW,STATUNL                                                   
         B     INIT20                                                           
         CLC   =C'AL',BYCSTAT      ALL                                          
         BNE   EINV                                                             
         OI    STATSW,STATALL                                                   
*                                                                               
INIT20   DS    0H                                                               
         MVI   MYFIRSTH,12                                                      
*                                                                               
INITX    B     XIT                                                              
         SPACE 2                                                                
         EJECT                                                                  
* FURTHER REQUEST VALIDATION                                                    
*                                                                               
VALID    DS    0H                                                               
VALX     B     XIT                                                              
         EJECT                                                                  
* ERROR EXITS AND MESSAGES                                                      
*                                                                               
EINV     MVI   ERROR,INVALID                                                    
         B     CURSOR                                                           
*                                                                               
CURSOR   GOTO1 CURSERR                                                          
*                                                                               
         EJECT                                                                  
INPUT    L     R4,AGLOBAL          ** INPUT ROUTINE **                          
         USING GLOBALD,R4                                                       
         CLI   SBMODE,SBPROCSP     HOOK MODE PASSED FROM SPOTIO                 
         BE    PROCBUY                                                          
         CLI   SBMODE,SBPROCGL                                                  
         BE    PROCBUY                                                          
         B     XIT                                                              
         EJECT                                                                  
* PROCESS BUY RECORD - READ BGR REC                                             
*                                                                               
PROCBUY  DS    0H                                                               
         BAS   RE,SETXSPOT                                                      
*                                                                               
         L     R3,SBAIO1           BUY OR GOAL REC                              
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING BGRKEYD,R2                                                       
         MVI   BGRKTYPE,BGRKTYPQ                                                
         MVI   BGRKSUB,BGRKSUBQ                                                 
*                                                                               
         CLI   SBMODE,SBPROCSP     DO WE HAVE A BUY?                            
         BNE   BUY02                NO - IT MUST BE A GOAL                      
         USING BUYREC,R3                                                        
         MVC   BGRKAM,BUYKAM                                                    
         MVC   BGRKCLT,BUYKCLT                                                  
         MVC   BGRKPRD,BDMASPRD                                                 
         MVC   BGRKEST,BUYKEST                                                  
         MVC   BGRKMKT,BUYMSTA                                                  
         B     BUY04                                                            
         DROP  R3                                                               
*                                                                               
         USING GOALRECD,R3                                                      
BUY02    MVC   BGRKAM,GKEYAM                                                    
         MVC   BGRKCLT,GKEYCLT                                                  
         MVC   BGRKPRD,GKEYPRD                                                  
         MVC   BGRKEST,GKEYEST                                                  
         MVC   BGRKMKT,GKEYMKT                                                  
         DROP  R3                                                               
*                                                                               
BUY04    L     RF,SBAIO3           ALREADY READ THIS REC?                       
         CLC   KEY(L'BGRKMAST),0(RF)                                            
         BNE   *+16                                                             
         TM    MYFLAGS,SKIPBUY      SKIP BUYS FOR THIS BGR REC?                 
         BNZ   BUY08                YES                                         
         B     BUYX                 NO - JUST EXIT                              
*                                                                               
         NI    MYFLAGS,X'FF'-SKIPBUY                                            
         GOTO1 HIGH                                                             
         CLC   KEY(L'BGRKMAST),KEYSAVE                                          
         BE    *+12                                                             
         MVI   FLTTAB,X'FF'        SET NO FLIGHTS                               
         B     BUYX                                                             
*                                                                               
         L     R2,SBAIO3                                                        
         ST    R2,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         XC    RULE,RULE                                                        
         MVI   MISSTAT,0                                                        
         MVI   STATUS,C' '                                                      
         MVI   STATBYTE,0                                                       
         LR    R6,R2                                                            
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   BUY05                                                            
*                                                                               
         USING BGRSTELD,R6                                                      
         MVC   RULE,BGRSTERS                                                    
*                                                                               
         MVC   STATUS,BGRSAST      MASTER STATUS                                
         MVC   BGLRUN,BGRSRDAT                                                  
         MVI   STATBYTE,X'80'      APPROVED                                     
         CLI   BGRSAST,C'A'                                                     
         BE    BUY05                                                            
         MVI   STATBYTE,X'40'      REJECTED                                     
         CLI   BGRSAST,C'R'                                                     
         BE    BUY05                                                            
         MVI   STATBYTE,X'20'      UNEXAMINED                                   
*                                  TREAT ANYTHING ELSE AS UNEXAMINED            
         DROP  R6                                                               
*                                                                               
BUY05    DS    0H                                                               
         LR    R6,R2                                                            
         MVI   ELCODE,X'11'                                                     
         BAS   RE,GETEL                                                         
         BNE   BUY07                                                            
*                                                                               
         USING BGRMSELD,R6                                                      
         MVC   MISSTAT,BGRMSTAT                                                 
         DROP  R6                                                               
*                                                                               
BUY07    BAS   RE,BLDFLT                                                        
*                                                                               
         CLI   STATSW,STATAPP      APPROVED ONLY?                               
         BNE   *+16                                                             
         CLI   STATBYTE,X'80'       YES - EVERYTHING MUST BE APPROVED           
         BE    BUY10                                                            
         B     BUY08                                                            
*                                                                               
         CLI   STATSW,STATREJ      REJECTED                                     
         BNE   *+16                                                             
         TM    STATBYTE,X'40'      PASS IF ANYTHING REJECTED                    
         BNZ   BUY10                                                            
         B     BUY08                                                            
*                                                                               
         CLI   STATSW,STATUNL      UNEVALUATED                                  
         BNE   BUY10                                                            
         TM    STATBYTE,X'20'      PASS IF ANYTHING UNEVALUATED                 
         BNZ   BUY10                                                            
BUY08    MVI   RPMODE,RPSKIOHK     SKIP THIS BUY                                
         OI    MYFLAGS,SKIPBUY                                                  
         B     BUYX                                                             
*                                                                               
* FIND THE BGL COMMENT                                                          
BUY10    CLI   COMMSW,C'Y'                                                      
         BNE   BUY40                                                            
         MVI   BLOCK,C' '                                                       
         MVC   BLOCK+1(249),BLOCK                                               
         LR    R6,R2                                                            
         MVI   ELCODE,X'21'        BGL COMMENT                                  
         BAS   RE,GETEL                                                         
         B     *+8                                                              
BUY20    BAS   RE,NEXTEL                                                        
         BE    BUY30                                                            
         CLI   BGRKMINK,X'21'      BELOW X'21' ELS?                             
         BH    BUY40                NO - NO COMMENTS                            
         GOTO1 SEQ                                                              
         GOTO1 GETREC                                                           
         B     BUY10                                                            
*                                                                               
BUY30    ZIC   R1,1(R6)                                                         
         SH    R1,=H'7'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BLOCK(0),6(R6)        SAVE COMMENT                               
*                                                                               
BUY40    MVI   SBMODE,SBPROCBG                                                  
         BAS   RE,DRIVIN                                                        
         MVI   SBMODE,SBPROCSP                                                  
*                                                                               
BUYX     BAS   RE,SETSPOT                                                       
         CLI   SBMODE,SBPROCGL                                                  
         BNE   XIT                                                              
         MVI   RPMODE,RPSKIOHK                                                  
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
SETXSPOT MVC   SYSDIR,=C'XSPDIR  '                                              
         MVC   SYSFIL,=C'XSPFILE '                                              
         MVC   LKEY,=H'32'                                                      
         MVC   LSTATUS,=H'4'                                                    
         BR    RE                                                               
*                                                                               
SETSPOT  MVC   SYSDIR,=C'SPTDIR  '                                              
         MVC   SYSFIL,=C'SPTFILE '                                              
         MVC   LKEY,=H'13'                                                      
         MVC   LSTATUS,=H'1'                                                    
         BR    RE                                                               
         EJECT                                                                  
BLDFLT   NTR1                                                                   
         XC    FLTTAB,FLTTAB                                                    
         LA    R3,FLTTAB                                                        
         USING FLTABD,R3                                                        
         MVI   INFLIGHT,C'N'                                                    
         MVI   EOFSW,C'N'                                                       
*                                                                               
         USING BGRWSELD,R6                                                      
BF02     LR    R6,R2                                                            
         MVI   ELCODE,X'14'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
BF06     BAS   RE,NEXTEL                                                        
         BE    BF06C                                                            
         CLI   BGRKMINK,X'14'      BELOW X'14' ELS?                             
         BH    BF06A                NO                                          
         GOTO1 SEQ                                                              
         GOTO1 GETREC                                                           
         B     BF02                                                             
*                                                                               
BF06A    MVI   EOFSW,C'Y'          SET AT END OF ELEMS                          
         B     BF20                                                             
*                                                                               
BF06C    DS    0H                                                               
         CLC   BGRWSTRT,SBBQSTP   IS WEEK WITHIN REQ START/END                  
         BL    BF06                                                             
         CLC   BGRWSTRT,SBBQENDP                                                
         BNH   BF06D                                                            
         MVI   EOFSW,C'Y'          SET AT END OF ELEMS                          
         B     BF20                                                             
*                                                                               
BF06D    DS    0H                                                               
         TM    BGRWSBGS,X'40'      ANY GOALS FOR THIS WEEK?                     
         BZ    BF20                NO                                           
*                                                                               
         MVC   FLTEND,BGRWSTRT     YES, SET PROVISIONAL END OF FLIGHT           
*                                                                               
*                                  SET STATUS                                   
         CLI   BGRWSSTA,C'A'       IF THIS (LATEST) WEEK IS NOT APPRVD          
         BNE   BF07                USE ITS STATUS                               
         CLI   FLTBUST,C'R'        IF IT IS APPROVED BUT WE HAD                 
         BE    BF08                EARLIER UNAPPROVED (R OR U)                  
         CLI   FLTBUST,C'U'                                                     
         BE    BF08                IGNORE APPRVD                                
*                                                                               
BF07     DS    0H                                                               
         MVC   FLTBUST,BGRWSSTA    USE LATEST STATUS                            
         CLI   FLTBUST,C' '                                                     
         BH    *+8                                                              
         MVI   FLTBUST,C'U'        DEFAULT TO UNEVALUATED                       
         MVC   FLTCOM,BGRWSCOM     AND COMMENT                                  
*         MVC   FLTPER,BGRWSPER     AND PERSON                                  
         MVC   FLTSTDT,BGRWSDAT    AND STATUS DATE                              
*                                                                               
BF08     DS    0H                                                               
         MVI   BYTE,X'80'          FOR SETTING STATBYTE                         
         CLI   FLTBUST,C'A'                                                     
         BE    BF08D                                                            
         MVI   BYTE,X'40'                                                       
         CLI   FLTBUST,C'R'                                                     
         BE    BF08D                                                            
*                                                                               
         MVI   BYTE,X'00'                                                       
         TM    STATBYTE,X'C0'      IF APPROVED OR REJECTED AT                   
         BNZ   BF08D               MARKET LEVEL, DON'T SET TO U HERE            
*                                                                               
         MVI   BYTE,X'20'                                                       
         CLI   FLTBUST,C'U'                                                     
         BE    BF08D                                                            
         DC    H'0'                                                             
*                                                                               
BF08D    DS    0H                                                               
         OC    STATBYTE,BYTE       'OR UP' STATUS BIT                           
         CLI   INFLIGHT,C'Y'       ARE WE ALREADY IN A FLIGHT?                  
         BE    BF06                YES, NEXT ELEM                               
*                                                                               
         MVI   INFLIGHT,C'Y'       NO, NOW WE ARE                               
         MVC   FLTSTRT,BGRWSTRT    SET START OF FLIGHT                          
         B     BF06                NEXT ELEM                                    
*                                                                               
BF20     DS    0H                  THIS WEEK HAS NO GOALS                       
         CLI   INFLIGHT,C'Y'       ARE WE ALREADY IN A FLIGHT?                  
         BNE   BF22                NO, NEXT ELEM                                
         MVI   INFLIGHT,C'N'       SET OFF IN FLIGHT                            
         LA    R3,FLTABDL(R3)      NEXT TABLE POSITION                          
*                                                                               
BF22     DS    0H                                                               
         CLI   EOFSW,C'Y'                                                       
         BNE   BF06                                                             
*                                                                               
         MVI   0(R3),X'FF'         SET END OF TABLE                             
*                                                                               
BFX      B     XIT                                                              
         DROP  R2,R3,R6                                                         
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
         MVI   ESTLEV,6                                                         
         MVC   GLOPTS+6(L'OPTIONS),OPTIONS                                      
         CLI   GLOPTS+1,0          MARKET GROUPS?                               
         BE    *+8                  NO                                          
         MVI   ESTLEV,7                                                         
         MVC   GLOPTS+2(1),COMMSW  SET COMMENT OPTION                           
         CLI   WIDTHOPT,C'W'                                                    
         BNE   *+8                                                              
         MVI   GLOPTS+3,C'W'       SET REPORT WIDTH (W=WIDE)                    
         MVI   GLOPTS+5,0          DEFAULT ALL                                  
         TM    STATSW,STATALL                                                   
         BO    DRINIT10                                                         
         TM    STATSW,STATAPP                                                   
         BZ    *+12                                                             
         MVI   GLOPTS+5,C'A'                                                    
         B     DRINIT10                                                         
         TM    STATSW,STATREJ                                                   
         BZ    *+12                                                             
         MVI   GLOPTS+5,C'R'                                                    
         B     DRINIT10                                                         
         TM    STATSW,STATUNL                                                   
         BZ    *+12                                                             
         MVI   GLOPTS+5,C'U'                                                    
         B     DRINIT10                                                         
         DC    H'0'                                                             
DRINIT10 DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
* DRIVER HOOK ROUTINE TO RESOLVE ADDRESSES                                      
*                                                                               
RESOLVE  LA    R1,RTNLIST          SEARCH LIST FOR ROUTINE NAME                 
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
RTNLIST  DS    0F                                                               
         DC    CL8'OMKT    ',A(OMKT)                                            
         DC    CL8'IFLT    ',A(IFLIGHT)                                         
         DC    CL8'OFLT    ',A(OFLIGHT)                                         
         DC    CL8'IAPPDAT ',A(IAPPDAT)                                         
         DC    CL8'IBGSTA  ',A(IBGSTA)                                          
         DC    CL8'IRCVDT  ',A(IRCVDT)                                          
         DC    CL8'ORCVDT  ',A(ORCVDT)                                          
         DC    CL8'ISTATUS ',A(ISTATUS)                                         
         DC    CL8'ICOMM   ',A(ICOMM)                                           
         DC    CL8'OCOMM   ',A(OCOMM)                                           
         DC    CL8'IBGERR  ',A(IBGERR)                                          
         DC    CL8'OBGERR  ',A(OBGERR)                                          
         DC    CL8'IBGDUMMY',A(IBGDUMMY)                                        
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
*                                                                               
* USE PRTSW TO SUPPRESS A LINE                                                  
*PRINT    CLI   SUPPRESS,C'Y'       TEST SUPPRESS PRINT                         
*         BNE   XIT                                                             
*         MVI   GLHOOK,GLDONT                                                   
         B     XIT                                                              
         EJECT                                                                  
* DRIVER HOOK FIRSTS                                                            
*                                                                               
FIRSTS   CLI   GLARGS,0            TEST LEVEL 0 BREAK                           
         BNE   FIRSTX                                                           
         MVC   TITLE,BLANKS        YES-SET THE APPROPRIATE TITLE                
         MVC   TITLE(26),=C'BUY CHECKING STATUS REPORT'                         
         LA    R2,BYCTITH          OVERRIDE TITLE?                              
         CLI   5(R2),0                                                          
         BE    FIRSTC                                                           
         GOTO1 ANY                                                              
         MVC   TITLE,WORK                                                       
FIRSTC   GOTO1 CENTER,DMCB,TITLE,63                                             
FIRSTX   B     XIT                                                              
         SPACE 2                                                                
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
EXEC     L     R2,GLAIFLD          R2=A(INPUT)                                  
         L     R3,GLAOFLD          R3=A(OUTPUT)                                 
         L     RF,GLAROUT          BRANCH TO ROUTINE                            
         BR    RF                                                               
*                                                                               
         DROP  R4                                                               
         EJECT                                                                  
* DRIVER INPUT/OUTPUT ROUTINES                                                  
*                                                                               
OMKT     DS    0H                                                               
         MVC   SBBMKT,0(R2)                                                     
         EDIT  SBBMKT,(4,SBMKT),FILL=0     SET MARKET DETAILS                   
         GOTO1 GETMKTNM                                                         
         MVC   0(4,R3),SBMKT                                                    
         MVC   5(L'SBMKTNM,R3),SBMKTNM                                          
         OC    0(29,R3),BLANKS                                                  
         B     XIT                                                              
*                                                                               
IRCVDT   DS    0H                                                               
         CLI   SBMODE,SBPROCBG                                                  
         BE    XIT                                                              
         OC    BGLRUN,BGLRUN                                                    
         BNZ   *+14                                                             
         MVC   0(6,R2),=X'FFFFFFFFFFFF'                                         
         B     XIT                                                              
         MVC   0(6,R2),BGLRUN                                                   
         B     XIT                                                              
         SPACE 2                                                                
*                                                                               
ORCVDT   DS    0H                                                               
         L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         CLI   OPTFLT,C'N'                                                      
         BE    ORCV10                                                           
         TM    GLINDS,GLTOTLIN     IS THIS A TOTAL LINE?                        
         BZ    ORCV10               NO                                          
         CLC   GLLEVEL,ESTLEV      TEST EST TOTAL                               
         BNE   XIT                  NO - DON'T PRINT                            
         MVC   0(L'HOLDRCV,R3),HOLDRCV                                          
         B     XIT                                                              
ORCV10   DS    0H                                                               
         OC    0(6,R2),0(R2)                                                    
         BZ    ORCV20                                                           
         CLC   0(6,R2),=X'FFFFFFFFFFFF'                                         
         BNE   *+14                                                             
         MVC   HOLDRCV,BLANKS                                                   
         B     XIT                                                              
         GOTO1 DATCON,DMCB,(0,0(R2)),(5,HOLDRCV)                                
*                                                                               
ORCV20   CLI   OPTFLT,C'N'                                                      
         BNE   *+16                                                             
         TM    GLINDS,GLTOTLIN                                                  
         BNZ   XIT                                                              
         B     *+12                                                             
*                                                                               
         TM    GLINDS,GLTOTLIN                                                  
         BZ    XIT                                                              
         MVC   0(L'HOLDRCV,R3),HOLDRCV                                          
         B     XIT                                                              
*                                                                               
ISTATUS  DS    0H                                                               
         CLI   SBMODE,SBPROCBG                                                  
         BE    XIT                                                              
         CLI   OPTFLT,C'N'         IF NO FLIGHTS, USE MASTER STATUS             
         BE    IS10                                                             
*                                                                               
         BAS   RE,GETFLT                                                        
         USING FLTABD,R4                                                        
         BNE   XIT                                                              
         CLI   STATUS,C' '                                                      
         BNH   *+12                IF NO MASTER STAT, USE FLIGHT STAT           
*                                                                               
         CLI   FLTBUST,C'U'        IF FLIGHT STATUS IS UNEX,                    
         BE    *+14                                                             
         MVC   0(1,R2),FLTBUST                                                  
         B     ISX                                                              
*                                                                               
IS10     MVC   0(1,R2),STATUS      USE MASTER STATUS                            
*                                                                               
ISX      CLI   0(R2),C'R'          CHANGE STATUS R TO H PER STEVE P.            
         BNE   XIT                                                              
         MVI   0(R2),C'H'                                                       
         B     XIT                                                              
         DROP  R4                                                               
         SPACE 2                                                                
IAPPDAT  DS    0H                                                               
         CLI   SBMODE,SBPROCBG                                                  
         BE    XIT                                                              
         CLI   OPTFLT,C'N'         USE LATEST APP DATE IF NO FLIGHTS            
         BE    APPDAT20                                                         
*                                                                               
         BAS   RE,GETFLT                                                        
         USING FLTABD,R4                                                        
         BNE   XIT                                                              
*                                                                               
*  IF FLIGHT STAT IS UNEX, NO DATE                                              
         TM    STATBYTE,X'C0'                                                   
         BNZ   APPDAT10            IF APP/REJ AT MKT LEV, IGNORE FLIGHT         
         CLI   FLTBUST,C'U'                                                     
         BE    XIT                                                              
*                                                                               
APPDAT10 MVC   0(2,R2),FLTSTDT                                                  
         B     XIT                                                              
*                                                                               
APPDAT20 LA    R4,FLTTAB                                                        
         USING FLTABD,R4                                                        
APPDAT30 CLI   0(R4),X'FF'         NO FLIGHTS IN TABLE                          
         BE    XIT                                                              
         CLC   0(2,R2),FLTSTDT                                                  
         BNL   *+10                                                             
         MVC   0(2,R2),FLTSTDT                                                  
         LA    R4,FLTABDL(R4)                                                   
         B     APPDAT30                                                         
*                                                                               
         DROP  R4                                                               
         SPACE 2                                                                
IBGSTA   DS    0H                                                               
         XC    0(8,R2),0(R2)                                                    
         CLI   SBMODE,SBPROCBG                                                  
         BNE   *+14                                                             
         MVC   0(5,R2),=X'FFFFFFFFFF'                                           
         B     XIT                                                              
         MVC   0(5,R2),SBSTA                                                    
         OC    0(8,R2),BLANKS                                                   
         B     XIT                                                              
         SPACE 2                                                                
IFLIGHT  DS    0H                                                               
         CLI   SBMODE,SBPROCBG                                                  
         BNE   *+14                                                             
         MVC   0(4,R2),=X'FFFFFFFF'                                             
         B     XIT                                                              
         BAS   RE,GETFLT                                                        
         USING FLTABD,R4                                                        
         BE    *+14                                                             
         MVC   0(4,R2),=X'FEFEFEFE'                                             
         B     XIT                                                              
*                                                                               
         MVC   0(4,R2),FLTSTRT      AND FLTEND                                  
         B     XIT                                                              
         DROP  R4                                                               
         SPACE 2                                                                
OFLIGHT  DS    0H                                                               
         CLC   =X'FFFFFFFF',0(R2)                                               
         BE    XIT                                                              
*        BNE   *+12                                                             
*        MVI   PRTSW,C'N'                                                       
*        B     XIT                                                              
         CLC   =X'FEFEFEFE',0(R2)                                               
         BNE   *+14                                                             
         MVC   0(09,R3),=C'NO FLIGHT'                                           
         B     XIT                                                              
         GOTO1 DATCON,DMCB,(X'12',0(R2)),(4,0(R3))                              
         B     XIT                                                              
         SPACE 2                                                                
IBGERR   DS    0H                                                               
         MVC   0(15,R2),BLANKS                                                  
         CLI   SBMODE,SBPROCBG                                                  
         BNE   XIT                                                              
         LA    R0,10                                                            
         LR    R1,R2                                                            
         LA    RF,RULE                                                          
         LA    RE,ERRTAB                                                        
IBGER10  CLI   0(RF),C'Y'                                                       
         BNE   *+10                                                             
         MVC   0(1,R1),0(RE)                                                    
         LA    R1,1(R1)                                                         
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,IBGER10                                                       
*                                                                               
* PICK UP MIS ERRORS                                                            
         LA    R0,5                                                             
         LA    RE,C'1'             ERROR CODES 1 THRU 5                         
         LA    RF,X'80'            ROTATE THRU BITS                             
IBGER20  EX    RF,*+8                                                           
         B     *+8                                                              
         TM    MISSTAT,0                                                        
         BZ    *+8                                                              
         STC   RE,0(R1)                                                         
         LA    RE,1(RE)                                                         
         LA    R1,1(R1)                                                         
         SRA   RF,1                                                             
         BCT   R0,IBGER20                                                       
*                                                                               
         B     XIT                                                              
         SPACE 2                                                                
OBGERR   DS    0H                                                               
         L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         CLI   OPTFLT,C'N'                                                      
         BE    OBGER10                                                          
         TM    GLINDS,GLTOTLIN     IS THIS A TOTAL LINE?                        
         BZ    *+14                 NO                                          
         CLC   GLLEVEL,ESTLEV      TEST EST TOTAL                               
         BNE   XIT                  NO - DON'T PRINT                            
         B     *+12                                                             
*                                                                               
OBGER10  TM    GLINDS,GLTOTLIN                                                  
         BNZ   XIT                                                              
         MVC   0(15,R3),0(R2)                                                   
         B     XIT                                                              
         EJECT                                                                  
IBGDUMMY DS    0H                                                               
         CLI   SBMODE,SBPROCBG                                                  
         BNE   XIT                                                              
         MVI   0(R2),1                                                          
         B     XIT                                                              
         SPACE 2                                                                
ICOMM    DS    0H                                                               
         CLI   SBMODE,SBPROCBG                                                  
         BNE   ICOMM10                                                          
         MVC   0(250,R2),BLOCK                                                  
         B     XIT                                                              
*                                                                               
ICOMM10  DS    0H                                                               
         BAS   RE,GETFLT                                                        
         USING FLTABD,R4                                                        
         BNE   XIT                                                              
*                                                                               
* NEED TO RE-READ FIRST BGL REC & START LOOKING FOR COMMENT FROM THERE          
         BAS   RE,SETXSPOT                                                      
         L     R1,SBAIO1                                                        
         USING BUYREC,R1                                                        
         LR    R0,R2                                                            
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING BGRKEYD,R2                                                       
         MVI   BGRKTYPE,BGRKTYPQ                                                
         MVI   BGRKSUB,BGRKSUBQ                                                 
         MVC   BGRKAM,BUYKAM                                                    
         MVC   BGRKCLT,BUYKCLT                                                  
         MVC   BGRKPRD,BDMASPRD                                                 
         MVC   BGRKEST,BUYKEST                                                  
         MVC   BGRKMKT,BUYMSTA                                                  
         DROP  R1                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'BGRKMAST),KEYSAVE                                          
         BNE   ICX                                                              
         L     R2,SBAIO3                                                        
         ST    R2,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
ICOMM15  LR    R6,R2                                                            
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
ICOMM20  BAS   RE,NEXTEL                                                        
         BE    ICOMM30                                                          
         CLI   BGRKMINK,X'20'      BELOW X'20' FLIGHT COMMENTS?                 
         BH    ICX                  NO - NO COMMENTS                            
         GOTO1 SEQ                                                              
         GOTO1 GETREC                                                           
         B     ICOMM15                                                          
*                                                                               
ICOMM30  CLC   2(1,R6),FLTCOM                                                   
         BNE   ICOMM20                                                          
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'7'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),6(R6)                                                    
*                                                                               
ICX      BAS   RE,SETSPOT                                                       
         B     XIT                                                              
         DROP  R4                                                               
         SPACE 2                                                                
OCOMM    DS    0H                                                               
         L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         CLI   OPTFLT,C'N'                                                      
         BE    OCOMM05                                                          
*                                                                               
         TM    GLINDS,GLTOTLIN     IS THIS A TOTAL LINE?                        
         BZ    *+14                 NO                                          
         CLC   GLLEVEL,ESTLEV      TEST EST TOTAL                               
         BNE   XIT                  NO - DON'T PRINT                            
         B     *+12                                                             
*                                                                               
OCOMM05  TM    GLINDS,GLTOTLIN                                                  
         BNZ   XIT                                                              
         CLI   0(R2),C' '                                                       
         BL    XIT                                                              
         L     R1,GLADTENT                                                      
         USING DROD,R1                                                          
         ZIC   RF,DROLEN           L'OUTPUT FIELD                               
         BCTR  RF,0                                                             
         DROP  R1,R4                                                            
         LA    RE,5                MAX NUMBER OF LINES                          
*                                                                               
OCOMM10  DS    0H                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R2)                                                    
         LA    R2,1(RF,R2)                                                      
         LA    R3,198(R3)          NEXT PRINT LINE                              
         BCT   RE,OCOMM10                                                       
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
* GET FLIGHT DATA FOR THIS CHUNK                                                
* RETURNS R4 = A(FLTTAB ENTRY)                                                  
*                                                                               
GETFLT   LR    R0,RE                                                            
         L     RE,SBACURCH                                                      
         CLI   SBMODE,SBPROCSP                                                  
         BE    *+14                                                             
         MVC   DUB(2),SGDATE-SGLCHNKD(RE)                                       
         B     *+10                                                             
         MVC   DUB(2),SCDATE-SCHUNKD(RE)                                        
         LA    R4,FLTTAB                                                        
         USING FLTABD,R4                                                        
         CLI   0(R4),X'FF'         NO FLIGHTS IN TABLE                          
         BE    NOFLIGHT                                                         
*                                                                               
         CLC   DUB(2),FLTEND                                                    
         BNH   GF10                                                             
         LA    R4,FLTABDL(R4)                                                   
         CLI   0(R4),X'FF'                                                      
         BNE   *-18                                                             
         B     NOFLIGHT                                                         
*                                                                               
GF10     CLC   DUB(2),FLTSTRT                                                   
         BNL   GFX                                                              
*                                                                               
NOFLIGHT LTR   RE,R0                                                            
         BR    RE                                                               
*                                                                               
GFX      LR    RE,R0                                                            
         CR    RE,RE                                                            
         BR    RE                                                               
*                                                                               
         DROP  R4                                                               
         EJECT                                                                  
* ROUTINE TO HOOK TO CALLING PROGRAM TO CALL DRIVER                             
*                                                                               
GODRIVER NTR1                                                                   
         L     RF,ADRIVER                                                       
         L     RE,SAVERD                                                        
         LM    R0,RC,20(RE)                                                     
         BASR  RE,RF                                                            
         XIT1                      DO NOT CHANGE THIS!!!                        
         EJECT                                                                  
         GETEL (R6),42,ELCODE                                                   
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
COMMSW   DS    C                   COMMENT SWITCH                               
*                                                                               
OPTIONS  DS    0CL(OPTX)                                                        
OPTBUY   DS    C                                                                
OPTAPP   DS    C                                                                
OPTRCV   DS    C                                                                
OPTERRS  DS    C                                                                
OPTSTA   DS    C                                                                
OPTFLT   DS    C                                                                
OPTX     EQU   *-OPTBUY                                                         
*                                                                               
HOLDRCV  DS    CL8                                                              
*                                                                               
MYFLAGS  DS    X                                                                
SKIPBUY  EQU   X'80'               SKIP BUYS FOR THIS REC                       
*                                                                               
STATSW   DS    X                                                                
STATAPP  EQU   X'80'               APPROVED ONLY                                
STATREJ  EQU   X'40'               REJECTED ONLY                                
STATUNL  EQU   X'20'               UNEVAL ONLY                                  
STATALL  EQU   STATAPP+STATREJ+STATUNL                                          
STATRCAP EQU   X'10'               RECAP ONLY                                   
*                                                                               
STATUS   DS    C                   CURRENT RECORDS STATUS (A/R/U)               
*                                                                               
INFLIGHT DS    C                                                                
EOFSW    DS    XL1                                                              
STATBYTE DS    XL1                                                              
RULE     DS    CL20                                                             
MISSTAT  DS    X                                                                
BGLRUN   DS    CL6                 BGL RUN DATE                                 
ESTLEV   DS    X                   ESTIMATE LEVEL                               
ERRTAB   DC    C'ABCDEFGHIJ'                                                    
*                                                                               
FLTTAB   DS    XL(FLTABDL*FLTMAX)                                               
*                                                                               
         EJECT                                                                  
* EXTENTION ROUTINES                                                            
*                                                                               
T20423X  NMOD1 0,**423X**,RA                                                    
         L     RC,AGEND                                                         
         USING GEND,RC                                                          
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
*                                                                               
         B     XIT2                                                             
*                                                                               
XIT2     XIT1  ,                                                                
         EJECT                                                                  
CURS     GOTO1 CURSERR                                                          
*                                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
WORKD    DSECT                                                                  
*                                                                               
AXTRA    DS    0F               ** EXTENTION ROUTINE ADDRESSES **               
         DS    A                                                                
AXTRAN   EQU   (*-AXTRA)/L'AXTRA                                                
*                                                                               
SAVERD   DS    A                                                                
RELO     DS    A                                                                
AGEND    DS    A                                                                
*                                                                               
WORKL    EQU   *-WORKD                                                          
*                                                                               
FLTABD   DSECT                                                                  
FLTSTRT  DS    XL2                                                              
FLTEND   DS    XL2                                                              
FLTBUST  DS    CL1                 BUCH STATUS                                  
FLTSTDT  DS    XL2                 STATUS DATE                                  
*FLTPER   DS    XL1                 PERSON  (INTERNAL CODE)                     
FLTCOM   DS    XL1                 COMMENT     "                                
FLTABDL  EQU   *-FLTABD                                                         
FLTMAX   EQU   7                   MAXIMUM FLIGHTS (IN 14 WEEKS)                
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
GOALRECD DSECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
       ++INCLUDE SPGENBGR                                                       
       ++INCLUDE SPWRIFFD                                                       
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE SPWRIF1D                                                       
         EJECT                                                                  
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE SPWRIB3D                                                       
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011SPWRI23   10/16/06'                                      
         END                                                                    
