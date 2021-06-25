*          DATA SET SPWRI21    AT LEVEL 025 AS OF 12/15/04                      
*PHASE T20421A,*                                                                
*                                                                               
*********************************************************************           
*                                                                   *           
*          SPWRI21 (T20421) - IAS REPORT                            *           
*                                                                   *           
*-------------------------------------------------------------------*           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* 11DEC97 14 EFJ -- FILTER BY BATCH DATE                            *           
* 28AUG97 13 EFJ -- DON'T PRINT ALL DETAILS                         *           
* 13AUG97 12 EFJ -- OPTION TO REPORT UNEVAL & REJECTED ONLY (IOW,   *           
*                   ANYTHING NOT APPROVED)                          *           
* 29APR97 11 EFJ -- OPTION TO SUPPRESS RECAP                        *           
* 25FEB97 10 EFJ -- FIRSTREC NOT CLEARED IF NO COMMENTS ON REPORT   *           
* 11NOV96 09 EFJ -- USE BATCH DATE FOR RECEIVE DATE (IF PRESENT)    *           
* 09SEP96 08 EFJ -- SHOW ERRORS FOR ALL STATUS (NOT JUST APPROVED)  *           
* 28AUG96 07 EFJ -- SUPPORT PAID/UNPAID FILTER                      *           
* 18JUN96 06 EFJ -- XSPFIL HAS 4000 BYTE RECS, NOT 2000             *           
* 17JUN96 05 EFJ -- TREAT UNKNOWN AS UNEVAL IN FILTERS              *           
* 12JUN96 04 EFJ -- FIX ERROR CALLS                                 *           
*                -- TREAT MISSING X'13' ELS AS ?                    *           
* 10JUN96 03 EFJ -- TREAT ANYTHING BUT APPROVED & REJECT AS UNEVAL  *           
* 07JUN96 02 EFJ -- ADD MOS ROW                                     *           
*                -- SHORTEN COMMENTS TO 30 CHARS ACROSS (NO WIDE)   *           
*                -- SET CORRECT TITLE IF REPORT IS RECAP ONLY       *           
* 10MAY96 01 EFJ -- INITIAL DEVELOPMENT                             *           
*                                                                   *           
*                                                                   *           
*********************************************************************           
         TITLE 'T20421 - SPOTPAK WRITER INVOICE APPROVAL STATUS REPORT'         
T20421   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKL,T20421,RA,RR=R2                                            
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
         L     R1,=A(T20421X)                                                   
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
         OI    SBQSKIP,SBQSKBUY+SBQSKGL+SBQSKBIL                                
         OI    SBQREAD,SBQRDMSR                                                 
*                                                                               
         CLI   RPTSCRN,0           TEST USER REPORT SCREEN                      
         BE    INITX                NO                                          
*                                                                               
         MVI   COMMSW,C'N'         DEFAULT IS NO COMMENTS                       
         CLI   IASCMTH+5,0                                                      
         BE    INIT10                                                           
         LA    R2,IASCMTH                                                       
         CLI   IASCMT,C'N'                                                      
         BE    INIT10                                                           
         CLI   IASCMT,C'Y'                                                      
         BNE   EINV                                                             
         MVI   COMMSW,C'Y'                                                      
**NOP    MVI   WIDTHOPT,C'W'       FORCE WIDE OPTION                            
*                                                                               
INIT10   DS    0H                                                               
         OI    STATSW,STATALL      DEFAULT TO STATUS ALL                        
         CLI   IASSTATH+5,0                                                     
         BE    INIT20                                                           
         LA    R2,IASSTATH                                                      
         NI    STATSW,X'FF'-STATALL                                             
         CLC   =C'AP',IASSTAT      APPROVED                                     
         BNE   *+12                                                             
         OI    STATSW,STATAPP                                                   
         B     INIT20                                                           
         CLC   =C'NA',IASSTAT      NOT APPROVED (REJ OR UNAL)                   
         BNE   *+12                                                             
         OI    STATSW,STATNA                                                    
         B     INIT20                                                           
         CLC   =C'RJ',IASSTAT      REJECTED                                     
         BNE   *+12                                                             
         OI    STATSW,STATREJ                                                   
         B     INIT20                                                           
         CLC   =C'UN',IASSTAT      UNALLOCATED                                  
         BNE   *+12                                                             
         OI    STATSW,STATUNL                                                   
         B     INIT20                                                           
         CLC   =C'AE',IASSTAT      APPROVED WITH ERROR                          
         BNE   *+12                                                             
         OI    STATSW,STATAE                                                    
         B     INIT20                                                           
         CLC   =C'AL',IASSTAT      ALL                                          
         BNE   EINV                                                             
         OI    STATSW,STATALL                                                   
*                                                                               
INIT20   DS    0H                                                               
         NI    STATSW,X'FF'-STATRCAP                                            
         CLI   IASRCAPH+5,0                                                     
         BE    INIT30                                                           
         CLI   IASRCAP,C'X'                                                     
         BE    INIT30                                                           
         CLI   IASRCAP,C'Y'                                                     
         BE    INIT30                                                           
         CLI   IASRCAP,C'N'                                                     
         BNE   EINV                                                             
*&&DO                                                                           
         LA    R2,IASRCAPH                                                      
         CLI   IASRCAP,C'X'        EXCLUDE RECAP                                
         BNE   *+12                 NO                                          
         OI    STATSW,STATXCAP                                                  
         B     INIT30                                                           
*                                                                               
         CLI   IASRCAP,C'N'        RECAP ONLY?                                  
         BE    INIT30               NO (DEFAULT)                                
         CLI   IASRCAP,C'Y'                                                     
         BNE   EINV                                                             
         OI    STATSW,STATRCAP                                                  
*&&                                                                             
*                                                                               
INIT30   DS    0H                                                               
         MVI   MYFIRSTH,11                                                      
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
         CLI   SBMODE,SBPROCMS     HOOK MODE PASSED FROM SPOTIO                 
         BE    PROCMSR                                                          
         B     XIT                                                              
         EJECT                                                                  
* PROCESS MATCHING STATUS RECORD                                                
*                                                                               
PROCMSR  DS    0H                                                               
         CLI   FIRSTREC,0          FIRST REC OF MINIO SET?                      
         BNE   MSR40                NO                                          
*                                                                               
         L     R6,SBAIO1                                                        
         MVI   ELCODE,X'13'                                                     
         BAS   RE,GETEL2                                                        
         BE    *+12                                                             
         MVI   STATUS,C' '                                                      
         B     *+10                                                             
         MVC   STATUS,MSRICST-MSRICEL(R6)                                       
*                                                                               
         B     MSR1                NOP TEST CODE                                
         CLI   STATUS,C'A'                                                      
         BE    MSR1                                                             
         CLI   STATUS,C'U'                                                      
         BE    MSR1                                                             
         CLI   STATUS,C'R'                                                      
         BE    MSR1                                                             
         DC    H'0'                                                             
MSR1     DS    0H                                                               
*                                                                               
         TM    STATSW,STATALL      PROCESSING ALL RECORDS?                      
         BO    MSR30                YES - NO FILTERING                          
*                                                                               
         TM    STATSW,STATNA       ANYTHING BUT APPROVED?                       
         BNO   *+16                 NO                                          
         CLI   STATUS,C'A'         IS IT APPROVED?                              
         BE    MSR20                YES - SKIP IT                               
         B     MSR30                                                            
*                                                                               
         TM    STATSW,STATAPP      WANT APPROVED ONLY?                          
         BZ    *+12                                                             
         MVI   FULL,C'A'                                                        
         B     MSR10                                                            
         TM    STATSW,STATREJ      WANT REJECTED ONLY?                          
         BZ    *+12                                                             
         MVI   FULL,C'R'                                                        
         B     MSR10                                                            
*                                                                               
         TM    STATSW,STATAE       WANT APPROVED WITH ERROR ONLY?               
         BZ    *+12                                                             
         MVI   FULL,C'E'           STATUS = E                                   
         B     MSR10                                                            
*                                                                               
         CLI   STATUS,C'A'         MUST WANT UNEXAMINED                         
         BE    MSR20                - ACCEPT ANYTHING NOT A/R                   
         CLI   STATUS,C'R'                                                      
         BE    MSR20                                                            
         B     MSR30                                                            
*                                                                               
MSR10    CLC   STATUS,FULL                                                      
         BE    MSR30                                                            
MSR20    OI    SBIOFLAG,SBMINSK    SKIP RECORD SET                              
         B     MSRX                                                             
*                                                                               
MSR30    LA    RE,FIRSTREC                                                      
         CLI   0(RE),0             IS THERE A RECORD ALREADY HERE?              
         BNE   MSR40                YES                                         
         L     R2,SBAIO1            NO - MOVE IN FIRST REOCRD                   
         ZICM  R3,MSRRLEN-MSRKEY(R2),2                                          
         LR    RF,R3                                                            
         MVCL  RE,R2                                                            
         CLI   COMMSW,C'Y'         PROCESSING COMMENTS?                         
         BE    MSR40                YES - GET THEM                              
         OI    SBIOFLAG,SBMINSK     NO - PROC WHAT WE HAVE & SKIP               
         B     MSR80                TO NEXT REC                                 
*                                                                               
MSR40    L     R6,SBAIO1                                                        
         LA    R6,MSRELS-MSRKEYD(R6)                                            
         SR    R0,R0                                                            
MSR50    CLI   0(R6),0                                                          
         BE    MSR60                                                            
         CLI   0(R6),X'A0'                                                      
         BE    MSR70                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     MSR50                                                            
*                                                                               
MSR60    L     R6,SBAIO1                                                        
         USING MSRKEYD,R6                                                       
         CLC   MSRKMINK,XFF                                                     
         BNE   MSRX                GET NEXT MINIO REC                           
         B     MSR80               ELSE NO COMMENTS                             
         DROP  R6                                                               
*                                                                               
MSR70    XC    BLOCK(255),BLOCK                                                 
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'5'                                                         
         STC   R1,BLOCK                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BLOCK+1(0),6(R6)      SAVE COMMENT AND LENGTH                    
*                                                                               
MSR80    LA    R6,FIRSTREC                                                      
         LA    R6,MSRELS-MSRKEYD(R6)                                            
*                                                                               
MSR90    CLI   0(R6),0                                                          
         BE    MSR120                                                           
         CLI   0(R6),X'12'                                                      
         BE    MSR110                                                           
         BH    MSR120                                                           
MSR100   ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     MSR90                                                            
*                                                                               
MSR110   DS    0H                                                               
         USING MSRINELD,R6                                                      
         OC    SBQBATST,SBQBATST   BATCH DATE FILTER?                           
         BZ    MSR112               NO                                          
         CLC   MSRINADT,SBQBATST                                                
         BL    MSR100                                                           
         CLC   MSRINADT,SBQBATEN                                                
         BH    MSR100                                                           
*                                                                               
MSR112   TM    SBEINV,SBEIPAID                                                  
         BZ    *+12                                                             
         TM    MSRINST,MSRINPYQ    PAID?                                        
         BZ    MSR100               NO                                          
*                                                                               
         TM    SBEINV,SBEIUNPD                                                  
         BZ    *+12                                                             
         TM    MSRINST,MSRINPYQ    PAID?                                        
         BNZ   MSR100               YES                                         
         DROP  R6                                                               
*                                                                               
         ST    R6,SBACURCH                                                      
         BAS   RE,DRIVIN                                                        
         B     MSR100                                                           
*                                                                               
MSR120   L     R6,SBAIO1                                                        
         CLI   COMMSW,C'Y'         PROCESSING COMMENTS?                         
         BNE   MSRLAST              NO - ALWAYS CLEAR FIRSTREC                  
         CLC   =X'FFFFFF',MSRKMINK-MSRKEY(R6)                                   
         BNE   MSRX                                                             
*                                                                               
MSRLAST  LA    R0,FIRSTREC         CLEAR PREV RECORD                            
         LA    R1,L'FIRSTREC                                                    
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVI   BLOCK,0             CLEAR COMMENT                                
         MVI   STATUS,0            CLEAR STATUS                                 
*                                                                               
MSRX     B     XIT                                                              
         SPACE 2                                                                
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
*         OI    GLINDS,GLPALDET                                                 
         MVC   GLOPTS+2(1),COMMSW  SET COMMENT OPTION                           
         CLI   WIDTHOPT,C'W'                                                    
         BNE   *+8                                                              
         MVI   GLOPTS+3,C'W'       SET REPORT WIDTH (W=WIDE)                    
         MVC   GLOPTS+4(1),IASRCAP                                              
*&&DO                                                                           
         TM    STATSW,STATRCAP     RECAP ONLY?                                  
         BZ    *+8                                                              
         MVI   GLOPTS+4,C'Y'                                                    
         TM    STATSW,STATXCAP     EXCLUDE RECAP?                               
         BZ    *+8                  NO                                          
         MVI   GLOPTS+4,C'X'                                                    
*&&                                                                             
         MVI   GLOPTS+5,0          DEFAULT ALL                                  
         TM    STATSW,STATALL                                                   
         BO    DRINIT10                                                         
         TM    STATSW,STATNA       ALL BUT APPROVED?                            
         BNO   *+12                                                             
         MVI   GLOPTS+5,C'N'                                                    
         B     DRINIT10                                                         
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
         TM    STATSW,STATAE                                                    
         BZ    *+12                                                             
         MVI   GLOPTS+5,C'E'                                                    
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
         DC    CL8'IMOS    ',A(IMOS)                                            
         DC    CL8'IINVNO  ',A(IINVNO)                                          
         DC    CL8'IINVCST ',A(IINVCST)                                         
         DC    CL8'ISTATUS ',A(ISTATUS)                                         
         DC    CL8'IAPPDAT ',A(IAPPDAT)                                         
         DC    CL8'IRCVDAT ',A(IRCVDAT)                                         
         DC    CL8'IERROR  ',A(IERROR)                                          
         DC    CL8'ICOMM   ',A(ICOMM)                                           
         DC    CL8'OCOMM   ',A(OCOMM)                                           
         DC    CL8'IAPPROV ',A(IAPPROV)                                         
         DC    CL8'IAPPRVI ',A(IAPPRVI)                                         
         DC    CL8'IUNEVAL ',A(IUNEVAL)                                         
         DC    CL8'IUNEVLI ',A(IUNEVLI)                                         
         DC    CL8'IREJECT ',A(IREJECT)                                         
         DC    CL8'IREJCTI ',A(IREJCTI)                                         
         DC    X'FF'                                                            
         EJECT                                                                  
* INTERNAL COMPUTES HOOK                                                        
*                                                                               
INTCOMP  DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
* DRIVER HOOK TO EXECUTE ROUTINES                                               
*                                                                               
EXEC     L     R2,GLAIFLD          R2=A(INPUT)                                  
         L     R3,GLAOFLD          R3=A(OUTPUT)                                 
         L     RF,GLAROUT          BRANCH TO ROUTINE                            
         BR    RF                                                               
         EJECT                                                                  
OMKT     DS    0H                                                               
         MVC   SBBMKT,0(R2)                                                     
         EDIT  SBBMKT,(4,SBMKT),FILL=0     SET MARKET DETAILS                   
         GOTO1 GETMKTNM                                                         
         MVC   0(4,R3),SBMKT                                                    
         MVC   5(L'SBMKTNM,R3),SBMKTNM                                          
         OC    0(29,R3),BLANKS                                                  
         B     XIT                                                              
*                                                                               
IMOS     DS    0H                                                               
         L     R6,SBAIO1                                                        
         MVC   0(L'MSRKMOS,R2),MSRKMOS-MSRKEY(R6)                               
         XC    0(2,R2),XFF                                                      
         B     XIT                                                              
*                                                                               
IINVNO   DS    0H                                                               
         L     R6,SBACURCH                                                      
         MVC   0(10,R2),MSRININO-MSRINELD(R6)                                   
IINVNOX  B     XIT                                                              
         SPACE 2                                                                
*                                                                               
IAPPROV  CLI   STATUS,C'A'                                                      
         BE    IINVCST                                                          
         B     XIT                                                              
*                                                                               
IUNEVAL  CLI   STATUS,C'A'         NOT APPR OR REJ = UNEVAL                     
         BE    XIT                                                              
         CLI   STATUS,C'R'                                                      
         BE    XIT                                                              
         B     IINVCST                                                          
*                                                                               
IREJECT  CLI   STATUS,C'R'                                                      
         BE    IINVCST                                                          
         B     XIT                                                              
*                                                                               
IAPPRVI  CLI   STATUS,C'A'                                                      
         BE    IINVCNT                                                          
         B     XIT                                                              
*                                                                               
IUNEVLI  CLI   STATUS,C'A'         NOT APPR OR REJ = UNEVAL                     
         BE    XIT                                                              
         CLI   STATUS,C'R'                                                      
         BE    XIT                                                              
         B     IINVCNT                                                          
*                                                                               
IREJCTI  CLI   STATUS,C'R'                                                      
         BNE   XIT                                                              
IINVCNT  MVI   3(R2),1                                                          
         B     XIT                                                              
*                                                                               
IINVCST  DS    0H                                                               
         L     R6,SBACURCH                                                      
         ICM   RF,15,MSRINAMT-MSRINELD(R6)                                      
         CVD   RF,DUB                                                           
         ZAP   0(8,R2),DUB                                                      
IINVCSX  B     XIT                                                              
         SPACE 2                                                                
*                                                                               
ISTATUS  MVC   0(1,R2),STATUS                                                   
         B     XIT                                                              
         SPACE 2                                                                
IAPPDAT  DS    0H                                                               
         LA    R6,FIRSTREC                                                      
         MVI   ELCODE,X'13'                                                     
         BAS   RE,GETEL2                                                        
         BNE   XIT                                                              
         MVC   0(6,R2),MSRICDAT-MSRICELD(R6)                                    
         B     XIT                                                              
         SPACE 2                                                                
IRCVDAT  DS    0H                                                               
         L     R6,SBACURCH                                                      
         USING MSRINELD,R6                                                      
         OC    MSRINADT,MSRINADT   IS THERE A DATE ADDED?                       
         BZ    IRCV10               NO - USE RUN DATE                           
         GOTO1 DATCON,DMCB,(2,MSRINADT),(0,0(R2))                               
         B     XIT                                                              
         DROP  R6                                                               
IRCV10   LA    R6,FIRSTREC                                                      
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL2                                                        
         BNE   XIT                                                              
         MVC   0(6,R2),MSRSTDAT-MSRSTELD(R6)                                    
         B     XIT                                                              
         SPACE 2                                                                
IERROR   DS    0H                                                               
*                                                                               
         LA    R6,FIRSTREC                                                      
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL2                                                        
         BNE   XIT                                                              
*                                                                               
         MVC   0(8,R2),SPACES                                                   
         TM    MSRSTBGS-MSRSTELD(R6),MSRBYHI  A: OVER BUDGET                    
         BNO   *+8                                                              
         MVI   0(R2),C'A'                                                       
         TM    MSRSTBGS-MSRSTELD(R6),MSRNOBD  B: NO BUDGET                      
         BNO   *+8                                                              
         MVI   1(R2),C'B'                                                       
         TM    MSRMPERR-MSRSTELD(R6),MSRMPFIN C: INVALID FILM CODE              
         BNO   *+8                                                              
         MVI   2(R2),C'C'                                                       
         TM    MSRMPERR-MSRSTELD(R6),MSRMPFRR D: FILM RELEASE                   
         BNO   *+8                                                              
         MVI   3(R2),C'D'                                                       
         TM    MSRMPERR-MSRSTELD(R6),MSRMPFPR E: FILM INVALID                   
         BNO   *+8                                                              
         MVI   4(R2),C'E'                                                       
         TM    MSRMPERR-MSRSTELD(R6),MSRMPACN F: INVALID AGENCY                 
         BNO   *+8                                                              
         MVI   5(R2),C'F'                                                       
         TM    MSRMPERR-MSRSTELD(R6),MSRMP5IN G: MORE THAN 5 INVOICES           
         BNO   *+8                                                              
         MVI   6(R2),C'G'                                                       
*                                                                               
         B     XIT                                                              
ICOMM    DS    0H                                                               
         ZICM  RE,BLOCK,1                                                       
         BZ    XIT                                                              
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),BLOCK                                                    
         B     XIT                                                              
         SPACE 2                                                                
OCOMM    DS    0H                                                               
         ZICM  RE,0(R2)                                                         
         BZ    XIT                                                              
         BCTR  RE,0                SUBTRACT LENGTH BYTE                         
         LA    R2,1(R2)            FIRST COMMENT CHAR                           
         LA    RF,30               L'OUTPUT FIELD                               
*                                                                               
OCOMM10  CR    RE,RF                                                            
         BNH   OCOMM30                                                          
         MVC   0(30,R3),0(R2)                                                   
         SH    RE,=H'30'                                                        
         LA    R2,30(R2)                                                        
         LA    R3,198(R3)          NEXT PRINT LINE                              
         B     OCOMM10                                                          
*                                                                               
OCOMM30  DS    0H                                                               
         LTR   RE,RE                                                            
         BZ    XIT                                                              
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R2)                                                    
         B     XIT                                                              
         EJECT                                                                  
* PRINT A LINE                                                                  
*                                                                               
PRINT    CLI   SUPPRESS,C'Y'       TEST SUPPRESS PRINT                          
         BNE   XIT                                                              
         MVI   GLHOOK,GLDONT                                                    
         B     XIT                                                              
         EJECT                                                                  
* DRIVER HOOK FIRSTS                                                            
*                                                                               
FIRSTS   CLI   GLARGS,0            TEST LEVEL 0 BREAK                           
         BNE   FIRSTX                                                           
         MVC   TITLE,BLANKS        YES-SET THE APPROPRIATE TITLE                
         CLI   GLRECNO,1                                                        
         BNE   FIRSTS10                                                         
         TM    STATSW,STATRCAP     RECAP ONLY?                                  
         BNZ   FIRSTS20                                                         
         MVC   TITLE(39),=C'INVOICE APPROVAL STATUS REPORT - DETAIL'            
         LA    R2,IASTITH          OVERRIDE TITLE?                              
         CLI   5(R2),0                                                          
         BE    FIRSTC                                                           
         GOTO1 ANY                                                              
         MVC   TITLE,WORK                                                       
         B     FIRSTC                                                           
FIRSTS10 CLI   GLRECNO,2                                                        
         BNE   FIRSTX                                                           
FIRSTS20 MVC   TITLE(38),=C'INVOICE APPROVAL STATUS REPORT - RECAP'             
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
* ROUTINE TO HOOK TO CALLING PROGRAM TO CALL DRIVER                             
*                                                                               
GODRIVER NTR1                                                                   
         L     RF,ADRIVER                                                       
         L     RE,SAVERD                                                        
         LM    R0,RC,20(RE)                                                     
         BASR  RE,RF                                                            
         XIT1                      DO NOT CHANGE THIS!!!                        
         EJECT                                                                  
         GETEL2 (R6),42,ELCODE                                                  
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
* WORKING STORAGE                                                               
*                                                                               
         SPACE 1                                                                
SUPPRESS DS    CL1                                                              
*                                                                               
BLANKS   DC    CL132' '                                                         
XFF      DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'                           
FF       EQU   X'FF'                                                            
*                                                                               
COMMSW   DS    C                   COMMENT SWITCH                               
STATSW   DS    X                                                                
STATAPP  EQU   X'80'               APPROVED ONLY                                
STATREJ  EQU   X'40'               REJECTED ONLY                                
STATUNL  EQU   X'20'               UNEVAL ONLY                                  
STATNA   EQU   STATREJ+STATUNL     NOT APPROVED (REJ OR UNEVAL)                 
STATALL  EQU   STATAPP+STATREJ+STATUNL                                          
STATRCAP EQU   X'10'               RECAP ONLY                                   
STATXCAP EQU   X'08'               EXCLUDE RECAP                                
STATAE   EQU   X'04'               APPROVE WITH ERROR                           
*                                                                               
STATUS   DS    C                   CURRENT RECORDS STATUS (A/R/U)               
*                                                                               
         DS    0F                                                               
         DC    C'FIRSTREC'                                                      
FIRSTREC DS    CL4000              FIRST MSR MINIO REC                          
*                                                                               
         EJECT                                                                  
* EXTENTION ROUTINES                                                            
*                                                                               
T20421X  NMOD1 0,**421X**,RA                                                    
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
         DS    0D                                                               
*                                                                               
WORKL    EQU   *-WORKD                                                          
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
       ++INCLUDE SPGENMSR                                                       
       ++INCLUDE SPWRIFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE SPWRIF1D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPWRID2D                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025SPWRI21   12/15/04'                                      
         END                                                                    
