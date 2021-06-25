*          DATA SET SPOMS19    AT LEVEL 201 AS OF 01/17/07                      
*PHASE T23419A                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        T23419  -- OCOM MAINTENANCE AND LIST                 *         
*                                                                     *         
*  COMMENTS:                                                          *         
*                                                                     *         
*  CALLED FROM:  ADDS CONTROLLER (T23400), WHICH CALLS                *         
*                DDGENCON (T00A30) WHICH CALLS THIS.                  *         
*                                                                     *         
*  INPUTS:       SCREEN SPOMSC1 (MAINT) & SPOMSC2 (LIST)              *         
*                                                                     *         
*  OUTPUTS:                                                           *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- POINTS TO THE OVERLAY STORAGE AREA DSECT       *         
*                R6 -- GETEL REGISTER                                 *         
*                R7 -- SECOND BASE                                    *         
*                R8 -- SPOOL                                          *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- FIRST BASE                                     *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
***********************************************************************         
T23419   TITLE 'SPOMS19 - COMMENTS'                                             
T23419   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T23419*,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R8,SYSSPARE         R5 = A(OVERLAY STORAGE AREA)                 
         USING MYAREAD,R8                                                       
         ST    R3,RELO                                                          
*                                                                               
         BAS   RE,INIT                                                          
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
*                                                                               
         CLI   MODE,XRECADD        ADDED RECORD?                                
         BE    REQ                                                              
*                                                                               
         CLI   MODE,XRECPUT        CHANGED RECORD?                              
         BE    REQ                                                              
*                                                                               
         CLI   MODE,XRECDEL        DELETED RECORD?                              
         BE    REQ                                                              
*                                                                               
         CLI   MODE,XRECREST       RESTORED RECORD?                             
         BE    REQ                                                              
         B     EXIT                                                             
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*                             VK                                      *         
***********************************************************************         
VK       NI    OCOMEDH+4,X'DF'     FORCE VALIDATION OF ALL FIELDS               
         XC    MYFLT,MYFLT         FLIGHT NUMBER (BINARY)                       
         XC    MYCLT,MYCLT         CLIENT FOR LR                                
         XC    MYPRD,MYPRD         PRODUCT FOR LR                               
         XC    MYEST,MYEST         ESTIMATE FOR LR                              
         XC    BPRD,BPRD           PRODUCT CODE                                 
         XC    BCLT,BCLT           CLIENT CODE                                  
         XC    BEST,BEST           ESTIMATE                                     
***                                                                             
* MEDIA FIELD                                                                   
***                                                                             
VK05     LA    R2,OCOMEDH          MEDIA                                        
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED?                        
         BO    VK10                YES                                          
         NI    OCOCLTH+4,X'DF'     FORCE VALIDATION OF NEXT FLD                 
         CLI   5(R2),0                                                          
         BE    ERRMIS                                                           
         GOTO1 VALIMED             VALIDATE MEDIA                               
         OI    4(R2),X'20'         PREVIOUSLY VALIDATED                         
***                                                                             
* CLIENT FIELD                                                                  
***                                                                             
VK10     LA    R2,OCOCLTH          CLIENT FIELD                                 
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED?                        
         BO    VK20                YES                                          
         NI    OCOPRDH+4,X'DF'     FORCE VALIDATION OF NEXT FLD                 
*                                                                               
         CLI   5(R2),0             LIST ACTION + NO CLIENT INPUT                
         BNE   VK15                                                             
         CLI   ACTEQU,ACTLIST      IS IT LIST ACTION?                           
         BNE   VK20                NO...MUST HAVE INPUT                         
*                                                                               
         CLI   OCOPRDH+5,0         PRODUCT FIELD BLANK?                         
         BNE   ERRMTPRD            NO...ERROR                                   
         CLI   OCOESTH+5,0         ESTIMATE FIELD BLANK?                        
         BNE   ERRMTEST            NO...ERROR                                   
         CLI   OCOFLITH+5,0        FLIGHT FIELD BLANK?                          
         BNE   ERRMTFLT            NO...ERROR                                   
         B     VK50                                                             
*                                                                               
VK15     GOTO1 VALICLT                                                          
         OI    4(R2),X'20'         CLIENT PREVIOUSLY VALIDATED                  
         MVC   MYCLT,BCLT          CLIENT CODE FOR LR                           
***                                                                             
* PRODUCT FIELD                                                                 
***                                                                             
VK20     LA    R2,OCOPRDH          PRODUCT FIELD                                
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED?                        
         BO    VK30                YES                                          
         NI    OCOESTH+4,X'DF'     FORCE VALIDATION OF NEXT FLD                 
         CLI   5(R2),0             ANY INPUT?                                   
         BNE   VK25                YES                                          
*                                                                               
         CLI   OCOESTH+5,0         NO PRODUCT...CANNOT HAVE                     
         BNE   ERRMTEST            1) ESTIMATE                                  
         CLI   OCOFLITH+5,0        2) FLIGHT                                    
         BNE   ERRMTFLT                                                         
         CLI   ACTEQU,ACTLIST      IS IT LIST ACTION?                           
         BE    VK50                YES...JUST BUILD KEY                         
         OI    4(R2),X'20'         PRODUCT PREVIOUSLY VALIDATED                 
         B     VK50                BUILD KEY                                    
*                                                                               
VK25     CLC   =C'ALL',8(R2)       ALL PRODUCTS?                                
         BNE   VK26                YES...MUST HAVE ESTIMATE AS WELL             
         CLI   OCOESTH+5,0                                                      
         BE    ERRMTES2                                                         
         B     VK29                                                             
*                                                                               
VK26     GOTO1 VALIPRD             VALIDATE PRODUCT                             
         CLI   BPRD,X'FF'                                                       
         BNE   *+8                                                              
         MVI   BPRD,0                                                           
         MVC   MYPRD+2,BPRD                                                     
         OI    4(R2),X'20'         PRODUCT PREVIOUSLY VALIDATED                 
         CLC   =C'POL',8(R2)                                                    
         BNE   VK30                                                             
VK29     MVC   MYPRD,8(R2)         MOVE IN 'POL' OR 'ALL'                       
         OI    4(R2),X'20'         PRODUCT PREVIOUSLY VALIDATED                 
***                                                                             
* ESTIMATE FIELD                                                                
***                                                                             
VK30     LA    R2,OCOESTH          ESTIMATE FIELD HEADER                        
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED?                        
         BO    VK40                YES                                          
         NI    OCOFLITH+4,X'DF'    FORCE VALIDATION OF NEXT FLD                 
         CLI   5(R2),0             ANY INPUT?                                   
         BNE   VK35                YES...VALIDATE THE ESTIMATE                  
         CLI   OCOFLITH+5,0        IS FLIGHT BLANK?                             
         BNE   ERRMTFLT            NO...ERROR                                   
         B     VK50                BUILD KEY                                    
VK35     GOTO1 VALIEST             VALIDATE ESTIMATE FIELD                      
         BAS   RE,CHKPRD           CLT/PRD/EST != CLT/ALL/EST                   
         OI    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         MVC   MYEST,BEST                                                       
***                                                                             
* FLIGHT FIELD                                                                  
***                                                                             
VK40     LA    R2,OCOFLITH         FLIGHT FIELD HEADER                          
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED?                        
         BO    VK50                YES                                          
         CLI   5(R2),0             ANY INPUT?                                   
         BE    VK50                NO                                           
*                                                                               
         CLC   OCOPRD,=C'ALL'      IF PRODUCT=ALL ; CLIENT!=BRAND               
         BNE   VK41                                                             
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CLTHDRD,R4          READ CLIENT RECORD                           
         MVI   CKEYTYPE,CKEYTYPQ   RECORD TYPE X'00'                            
         MVC   CKEYAM,BAGYMD       AGENCY/MEDIA                                 
         MVC   CKEYCLT,BCLT        CLIENT CODE                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   CPROF,0             BRAND CLIENT?                                
         BNE   ERRBCLT             NO...ERROR                                   
         DROP  R4                                                               
*                                                                               
VK41     TM    4(R2),X'08'         NUMERIC?                                     
         BNO   ERRFLTNU            NO...ERROR                                   
*                                                                               
         ZIC   R1,5(R2)            INPUT LENGTH                                 
         BCTR  R1,0                DECREMENT FOR EXECUTED PACK                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)         ** EXECUTED **                               
         OI    DUB+L'DUB-1,X'0C'   MAKE IT A VALID PACKED NUMBER                
         CVB   R1,DUB                                                           
         CHI   R1,16               FLIGHT CANNOT BE GREATER THAN 16             
         BH    ERRFLT17                                                         
         STC   R1,MYFLT                                                         
         BAS   RE,GETFLTRC         SEE IF FLIGHT RECORD EXISTS                  
*                                                                               
         L     R6,AIO                                                           
         USING DFFLTEL,R6          FLIGHT RECORD                                
         MVI   ELCODE,DFFLTELQ     GET 1ST FLIGHT ELEMENT (X'05')               
         BAS   RE,GETEL            IS THERE EVEN 1 ELEMENT?                     
         BNE   ERRFLTEL            NO...ERROR                                   
                                                                                
VK45     CLC   MYFLT,DFFLTNUM      MATCH?                                       
         BE    VK49                YES                                          
         BAS   RE,NEXTEL           END OF ELEMENTS?                             
         BNE   ERRFLTNM            YES, ERROR...NO FLIGHT # MATCH               
         B     VK45                                                             
         DROP  R6                                                               
VK49     OI    4(R2),X'20'         PREVIOUSLY VALIDATED                         
***                                                                             
* BUILD KEY                                                                     
***                                                                             
VK50     CLI   ACTEQU,ACTLIST      IS IT LIST ACTION?                           
         BE    VK60                YES...JUST BUILD KEY                         
         MVI   PFKEY,6             FORCE PAGE UP                                
         BAS   RE,NEWPAGE                                                       
         MVI   PFKEY,0                                                          
*                                                                               
VK60     LA    R6,KEY              SET UP KEY                                   
         USING COMHDRD,R6          COMMENT RECORD HEADER                        
         XC    KEY,KEY                                                          
         MVC   COMKTYPE,=X'0D0C'   RECORD TYPE                                  
         MVC   COMKAGY,BAGYMD      AGENCY/MEDIA                                 
         MVI   COMCTYPE,C'O'       "O" IS ASSIGNED FOR OCOM                     
         MVC   COMKCLT,MYCLT       CLIENT                                       
         MVC   COMKPRD,MYPRD       PRODUCT (2 BYTES NULLS+1 BYTE PROD)          
         MVC   COMKEST,MYEST       ESTIMATE                                     
         MVC   COMKSTA(1),MYFLT    FLIGHT (X'01-X'10'),NULLS=NO FLIGHT          
*                                                                               
VKX      B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                           CHKPRD                                    *         
*                 CLT/ALL/EST != CLT/PRD/EST                          *         
***********************************************************************         
CHKPRD   NTR1                                                                   
*                                                                               
         LA    R6,KEY              SET UP KEY                                   
         USING COMHDRD,R6          COMMENT RECORD HEADER                        
         XC    KEY,KEY                                                          
         MVC   COMKTYPE,=X'0D0C'   RECORD TYPE                                  
         MVC   COMKAGY,BAGYMD      AGENCY/MEDIA                                 
         MVI   COMCTYPE,C'O'       "O" IS ASSIGNED FOR OCOM                     
         MVC   COMKCLT,BCLT        CLIENT                                       
         GOTO1 HIGH                                                             
         B     CPRD09A                                                          
*                                                                               
CPRD09   GOTO1 SEQ                                                              
*                                                                               
CPRD09A  CLI   DMCB+8,0            IS THERE AN ERROR?                           
         BE    *+6                 NO                                           
         DC    H'0'                SHOULD NEVER BE AN ERROR ON SEQ              
*                                                                               
         CLC   KEY(COMKCLT-COMHDRD),KEYSAVE  MATCH UP TO CLIENT?                
         BNE   CPRDX               NO...DONE                                    
         OC    KEY+6(4),KEY+6      PRODUCT & ESTIMATE FOUND?                    
         BZ    CPRD09              NO                                           
*                                                                               
         CLC   =C'ALL',OCOPRD      ALL PRODUCTS ON SCREEN?                      
         BNE   CPRD11              NO                                           
         CLC   =C'ALL',COMKPRD     ALL PRODUCTS IN REC FOUND?                   
         BE    CPRD09              YES                                          
         CLC   COMKEST,BEST        MATCH ON ESTIMATES?                          
         BE    ERRALLPR            YES...ERROR                                  
         B     CPRD09                                                           
*                                                                               
CPRD11   CLC   =C'ALL',COMKPRD     ALL PRODUCTS IN REC FOUND?                   
         BNE   CPRD09              NO                                           
         CLC   COMKEST,BEST        MATCH ON ESTIMATES?                          
         BE    ERRALLPR            YES...ERROR                                  
         B     CPRD09                                                           
         DROP  R6                                                               
*                                                                               
CPRDX    B     EXIT                                                             
***********************************************************************         
*                              VR                                     *         
***********************************************************************         
VR       DS    0H                                                               
*                                                                               
         L     R0,AIO2             CLEAR TO BUILD EXISTING COMMENTS             
         LHI   R1,LIOS                                                          
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R3,AIO2             BUILD 10 COMMENT LINES HERE                  
         XR    R1,R1               COMMENT LINE COUNTER                         
         XR    R2,R2                                                            
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
VR00     BAS   RE,NEXTEL            HAVE A COMMENT ELEMENT?                     
         BNE   VR05                 NO                                          
         AHI   R1,1                 YES - INCRIMENT COM LINE COUNTER            
*                                                                               
         CLI   OCOLHD+5,C'1'        UPDATING LINES 11-20?                       
         BNE   VR01                 NO                                          
         CHI   R1,10                PAST LINE 10?                               
         BH    VR06                 YES, ALREADY SAVED FIRST 10 LINES           
         B     VR02                                                             
*                                                                               
VR01     CHI   R1,10                PAST 10TH COMMENT LINE?                     
         BNH   VR00                 NO, KEEP GETTING THE ELEMENTS               
*                                                                               
VR02     IC    R2,1(R6)             GET ELEMENT LENGTH                          
         BCTR  R2,0                 -1 FOR EX                                   
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R6)        MOVE THE ELEMENT TO AIO2                    
         LA    R3,1(R2,R3)          NEXT ELEMENT SLOT                           
         B     VR00                                                             
*                                                                               
VR05     CLI   OCOLHD+5,C'1'        UPDATING LINES 11-20?                       
         BNE   VR06                 NO                                          
         LA    R2,OCOLN1H           ARE LINES 11-20 BLANK?                      
         LA    R4,10                                                            
*                                                                               
VR05A    CLI   5(R2),0              ANY INPUT                                   
         BNE   VR05B                YES                                         
         BAS   RE,NXTUN                                                         
         BCT   R4,VR05A                                                         
         B     VR06                                                             
*                                                                               
VR05B    LA    R2,10                MUST HAVE 10 COM LINES                      
         SR    R2,R1                SUBTRACT FROM WHAT EXISTS                   
         BNP   VR06                 MUST BE > 0                                 
*                                                                               
         MVC   0(3,R3),=X'050300'   FILL BLANK COM LINES                        
         AHI   R3,3                                                             
         BCT   R2,*-10                                                          
*                                                                               
VR06     L     R6,AIO                                                           
         MVI   ELCODE,X'05'        REMOVE ALL COMMENT ELEMENTS                  
         GOTO1 REMELEM                                                          
*                                                                               
         CLI   OCOLHD+5,C'1'       UPDATING LINES 11-20?                        
         BNE   VR09                NO                                           
         XC    ELEM,ELEM           YES - ADD THE FIRST 10 ELEMENTS              
         L     R3,AIO2             FIRST 10 COMMENTS SAVED HERE                 
         XR    R2,R2                                                            
         IC    R2,1(R3)            ELEMENT LENGTH                               
         BCTR  R2,0                -1 FOR EX                                    
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),0(R3)       MOVE 1ST ELEMENT TO ELEM                     
         LA    R3,1(R2,R3)         BUMP TO NEXT SAVED COMMENT IN AIO2           
*                                                                               
         L     R6,AIO              FIRST COMMENT ELEMENT ADDED                  
         GOTO1 ADDELEM                                                          
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BNE   VR09                                                             
         IC    R2,1(R6)                                                         
         AR    R6,R2                                                            
*                                                                               
         BAS   RE,ADDCOM           ADD COMMENT LINES 1-10                       
*                                                                               
VR09     BAS   RE,CNTCOMLN         COUNT NUMBER OF COMMENT LINES                
         CLI   NUMLNS,0            ANY COMMENT LINES TO ADD?                    
         BE    VRX                 NO                                           
         MVI   ELSADDED,0          INITIALIZE ELEMENTS ADDED                    
*                                                                               
         L     R6,AIO                                                           
         LA    R2,OCOLN1H          LINE 1 FROM OCOM SCREEN                      
         LA    R1,10               MAX NUMBER OF LINES                          
*                                                                               
VR10     CLI   5(R2),0             ANY INPUT ON THIS LINE?                      
         BE    VR30                NO                                           
*                                                                               
VR20     XC    ELEM,ELEM                                                        
         MVI   ELEM,X'05'                                                       
         ZIC   R4,5(R2)            GET LENGTH OF INPUT                          
         LA    R3,2(R4)            L'ELEMENT (DATA LEN+EL-CD+EL-LEN)            
         STC   R3,ELEM+1           STORE L'ELEMENT                              
         BCTR  R4,0                SUBTRACT FOR EXECUTED MVC                    
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   ELEM+2(0),8(R2)     **EXECUTED**                                 
         ZIC   R4,ELSADDED         INCRIMENT # OF ELEMENTS ADDED                
         AHI   R4,1                                                             
         STC   R4,ELSADDED                                                      
         B     VR40                                                             
*                                                                               
VR30     CLC   ELSADDED,NUMLNS     IF ALREADY ADDED ALL ELEMENTS                
         BE    VRX                 DON'T ADD MORE                               
         XC    ELEM,ELEM           BLANK LINE ELEMENT                           
         MVI   ELEM,X'05'                                                       
         MVC   ELEM+1(2),=X'0300'                                               
*                                                                               
VR40     L     R6,AIO                                                           
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL            DID WE ADD FIRST COMMENT ELEMENT?            
         BE    VR41                YES                                          
*                                                                               
         L     R6,AIO              FIRST COMMENT ELEMENT ADDED                  
         GOTO1 ADDELEM                                                          
         B     VR46                                                             
*                                                                               
VR41     ZIC   R4,1(R6)            BUMP UNTIL LAST X'05' ELEMENT                
         AR    R6,R4                                                            
         CLI   0(R6),X'05'                                                      
         BE    VR41                                                             
*                                                                               
VR45     GOTO1 RECUP,DMCB,(0,AIO),ELEM,(R6)                                     
VR46     C     R2,CMNTADD          LAST LINE TO ADD?                            
         BE    VRX                 YES                                          
*                                                                               
VR50     BAS   RE,NXTUN            SKIP TO NEXT UNPROTECTED LINE                
         BCT   R1,VR10                                                          
*                                                                               
VRX      CLI   OCOLHD+5,C'1'       UPDATING LINES 11-20?                        
         BE    VRXIT               YES, ALL DONE                                
         L     R3,AIO2             NEED TO ADD LINES 11-20 BACK                 
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL            DID WE ADD FIRST COMMENT ELEMENT?            
         BNE   VRXIT               NO, RECORD HAS NO COMMENT ELEMENTS           
*                                                                               
         XR    R2,R2                                                            
         IC    R2,1(R6)            BUMP UNTIL LAST X'05' ELEMENT                
         AR    R6,R2                                                            
         CLI   0(R6),X'05'                                                      
         BE    *-10                                                             
*                                                                               
         BAS   RE,ADDCOM           ADD COMMENT LINES 11-20                      
*                                                                               
VRXIT    CLI   PFKEY,0             WAS A PF KEY HIT                             
         BE    *+8                                                              
         BAS   RE,CHKPF            YES CHECK IT                                 
         B     DR                                                               
         EJECT                                                                  
***********************************************************************         
*     ADD SAVED COMMENT ELEMS FROM AIO2 TO AIO1                       *         
***********************************************************************         
ADDCOM   NTR1                                                                   
*                                                                               
AC10     CLI   0(R3),0             END OF ELEMENTS?                             
         BE    ACX                 YES                                          
*                                                                               
         XC    ELEM,ELEM           BUILD COMMENT ELEM HERE                      
         IC    R2,1(R3)            ELEMENT LENGTH                               
         BCTR  R2,0                -1 FOR EX                                    
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),0(R3)       MOVE ELEMENT TO ELEM                         
*                                                                               
         GOTO1 RECUP,DMCB,(0,AIO),ELEM,(R6)                                     
*                                                                               
         LA    R3,1(R2,R3)         BUMP TO NEXT SAVED COMMENT IN AIO2           
         IC    R2,1(R6)            BUMP PAST LAST X'05' ELEMENT IN AIO          
         AR    R6,R2                                                            
         B     AC10                                                             
*                                                                               
ACX      B     EXIT                                                             
*                                                                               
***********************************************************************         
*                               DR                                    *         
***********************************************************************         
DR       DS    0H                                                               
*                                                                               
         TWAXC OCOLN1H             CLEAR SCREEN                                 
*                                                                               
         CLI   PFKEY,5             PAGE DOWN?                                   
         BE    *+12                YES                                          
         CLI   PFKEY,6             PAGE UP?                                     
         BNE   *+8                 NO                                           
         BAS   RE,NEWPAGE                                                       
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL            ANY COMMENT LINES?                           
         BNE   DR20                NO                                           
*                                                                               
         CLI   OCOLHD+5,C'1'       DISPLAYING COMMENT LINES 11-20?              
         BNE   DR15                NO                                           
         LA    R2,10               BUMP THROUGH 10 MORE COMMENT ELEMS           
*                                                                               
DR10     BAS   RE,NEXTEL           HAVE 11TH COMMENT LINE?                      
         BNE   DR20                NOPE                                         
         BCT   R2,DR10             FALL THROUGH BCT = 11TH COMMENT LINE         
*                                                                               
DR15     LA    R2,OCOLN1H          FIRST LINE OF COMMENT INPUT                  
         BAS   RE,PRNTOUT          PRINT COMMENT LINES OUT                      
*                                                                               
DR20     BAS   RE,COMMENTS         DISPLAY OTHER COMMENT LEVELS                 
*                                                                               
         CLI   ACTNUM,ACTADD       IS THIS AN ADD?                              
         BE    DRX                 YES F1 ELEMENT NOT PRESENT YET               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'F1'        ACTIVITY ELEMENT                             
         BAS   RE,GETEL            MUST BE THERE                                
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACTVD,R6                                                         
         XC    WORK,WORK                                                        
         LA    R3,WORK                                                          
         USING AUTHD,R3                                                         
         MVC   SECRAGY,SVSECAGY    SECURITY AGENCY                              
         MVC   PASSWD,ACTVADID     PERSON WHO CREATED REC (2 BYTE ID)           
         BAS   RE,AUTH                                                          
         MVC   OCOCRTR,PRSNLID     PERSON WHO CREATED REC (NAME)                
         GOTO1 DATCON,DMCB,(3,ACTVADDT),(5,OCOCDTE)                             
*                                                                               
         XC    WORK,WORK                                                        
         LA    R3,WORK                                                          
         MVC   SECRAGY,SVSECAGY    SECURITY AGENCY                              
         MVC   PASSWD,ACTVCHID     PERSON WHO CHANGED REC (2 BYTE ID)           
         BAS   RE,AUTH                                                          
         MVC   OCOCWHO,PRSNLID     PERSON WHO CHANGED REC (NAME)                
         GOTO1 DATCON,DMCB,(3,ACTVCHDT),(5,OCOADTE)                             
         DROP  R3,R6                                                            
*                                                                               
DRX      B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*                            AUTH                                     *         
***********************************************************************         
AUTH     NTR1                                                                   
*                                                                               
         MVC   SAVEKEY,KEY                                                      
         MVC   AIO,AIO2                                                         
*                                                                               
         LA    R4,WORK             SECURITY BLOCK                               
         USING AUTHD,R4                                                         
         XC    PRSNLID,PRSNLID                                                  
         OC    PASSWD,PASSWD       CHANGED BY FIELD                             
         BZ    AUTHX                                                            
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING CT0REC,R6                                                        
         MVI   CT0KTYP,CT0KTEQU    RECORD TYPE '0'                              
         MVC   CT0KAGY,SECRAGY     GET SECURITY AGENCY                          
         CLC   SECRAGY,BLANKS                                                   
         BH    *+10                                                             
         MVC   CT0KAGY,AGENCY                                                   
         MVC   CT0KNUM,PASSWD      PERSON AUTH NUMBER                           
*                                                                               
         L     R6,AIO                                                           
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',KEY,(R6)                      
         CLC   CT0KEY,KEY                                                       
         BNE   AUTHX                                                            
         LA    RE,28(R6)                                                        
*                                                                               
AUTH10   DS    0H                                                               
         CLC   =X'C30A',0(RE)      NEW SECURITY - PERSON ELEMENT                
         BE    AUTH20                                                           
         ZIC   R0,1(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),0                                                          
         BNE   AUTH10                                                           
         B     AUTHX                                                            
*                                                                               
AUTH20   MVC   PRSNLID,2(RE)       PERSONAL ID                                  
AUTHX    MVC   AIO,AIO1                                                         
         MVC   KEY,SAVEKEY                                                      
         B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
*                              DK                                     *         
***********************************************************************         
DK       DS    0H                                                               
*                                                                               
         TWAXC OCOMEDH,OCOFLITH    CLEAR KEY FILEDS AND TRANSMIT                
         L     R6,AIO                                                           
         USING COMHDRD,R6                                                       
         MVC   WKAGY,COMKAGY       SAVE KEY'S COMPONENTS                        
         MVC   WKCTYPE,COMCTYPE                                                 
         MVC   WKCLT,COMKCLT                                                    
         MVC   WKPRD,COMKPRD                                                    
         MVC   WKEST,COMKEST                                                    
         MVC   WKFLT,COMKFLT                                                    
***                                                                             
* DISPLAY MEDIA                                                                 
***                                                                             
         ZIC   RE,COMKAGY                                                       
         SLL   RE,28                                                            
         SRL   RE,28               ISOLATE MEDIA                                
         IC    RE,MEDTAB-1(RE)                                                  
         STC   RE,OCOMED                                                        
***                                                                             
* DISPLAY CLIENT                                                                
***                                                                             
*                                                                               
         MVC   SVKEY,KEY                                                        
         OC    MYCLT,MYCLT         CLIENT LIST FILTER?                          
         BNZ   *+8                                                              
         BAS   RE,GETCLT                                                        
*                                                                               
         GOTO1 CLUNPK,DMCB,(SVCPROF+6,COMKCLT),OCOCLT                           
***                                                                             
* DISPLAY PRODUCT                                                               
***                                                                             
DK05     OC    COMKPRD,COMKPRD     ANY PRODUCT TO SHOW?                         
         BZ    DK50                NO                                           
*                                                                               
         CLC   COMKPRD,=C'ALL'     ALL PRODUCTS?                                
         BE    *+10                YES                                          
         CLC   COMKPRD,=C'POL'     POL OF PRODUCTS?                             
         BNE   *+14                NO                                           
         MVC   OCOPRD(3),COMKPRD                                                
         B     DK10                                                             
*                                                                               
DK08     MVC   BYTE,WKPRD+2                                                     
         BAS   RE,GETPRD           GET PRODUCT NAME                             
         MVC   OCOPRD(3),WORK      PUT PRODUCT NAME TO SCREEN                   
***                                                                             
* DISPLAY ESTIMATE                                                              
***                                                                             
DK10     CLI   COMKEST,0           ANY ESTIMATE?                                
         BE    DK50                NO                                           
DK15     ZIC   R0,COMKEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  OCOEST,DUB                                                       
***                                                                             
* DISPLAY FLIGHT                                                                
***                                                                             
DK25     CLI   COMKFLT,0           ANY FLIGHT?                                  
         BE    DK50                NO                                           
         EDIT  COMKFLT,(2,OCOFLIT),ALIGN=LEFT                                   
*                                                                               
DK50     XC    KEY,KEY             REBUILT KEY BEFORE EXITING DK                
         LA    R6,KEY                                                           
         MVC   KEY(2),=X'0D0C'     NOTE: SAVED COMPNONENTS OF KEYS              
         MVC   COMKAGY,WKAGY             ARE USED.                              
         MVC   COMCTYPE,WKCTYPE                                                 
         MVC   COMKCLT,WKCLT                                                    
         MVC   COMKPRD,WKPRD                                                    
         MVC   COMKEST,WKEST                                                    
         MVC   COMKFLT,WKFLT                                                    
         DROP  R6                                                               
*                                                                               
         MVC   AIO,AIO1            RESET IO, (DOUBLE CHECK)                     
         GOTO1 HIGH                SHOULD NEVER BE AN ERROR ON HIGH             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(13),KEYSAVE     SHOULD HAVE READ SAME RECORD                 
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC              GET THE WHOLE RECORD                         
DKX      B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*                              LR                                     *         
***********************************************************************         
LR       LA    R4,KEY                                                           
         MVI   NLISTS,14           14 DISPLAYS PER SCREEN                       
         OC    KEY,KEY             FIRST TIME THROUGH?                          
         BNZ   LR05                NO                                           
*                                                                               
         USING COMHDRD,R4          COMMENT RECORD HEADER                        
         MVC   COMKTYPE,=X'0D0C'   RECORD TYPE                                  
         MVC   COMKAGY,BAGYMD      AGENCY/MEDIA                                 
         MVI   COMCTYPE,C'O'       "O" IS ASSIGNED FOR OCOM                     
         MVC   COMKCLT,MYCLT       CLIENT                                       
         MVC   COMKPRD,MYPRD       PRODUCT (1 BYTE PROD OR 'ALL' 'POL')         
         MVC   COMKEST,MYEST       ESTIMATE                                     
         MVC   COMKSTA(1),MYFLT    FLIGHT (X'01-X'10'),NULLS=NO FLIGHT          
         DROP  R4                                                               
         MVC   SAVEKEY,KEY                                                      
*                                                                               
LR05     GOTO1 HIGH                                                             
         CLI   DMCB+8,0            IS THERE AN ERROR?                           
         BE    LR20                NO                                           
         DC    H'0'                SHOULD NEVER BE AN ERROR ON HIGH             
*                                                                               
LR10     GOTO1 SEQ                                                              
         CLI   DMCB+8,0            IS THERE AN ERROR?                           
         BE    LR20                NO                                           
         DC    H'0'                SHOULD NEVER BE AN ERROR ON SEQ              
*                                                                               
LR20     LA    R4,KEY                                                           
         USING COMHDRD,R4                                                       
         CLC   KEY(COMKCLT-COMHDRD),SAVEKEY  COMP UP TO CLIENT                  
         BNE   LRX                                                              
         L     R6,AIO                                                           
         OC    MYCLT,MYCLT         IS THERE A CLIENT FILTER                     
         BZ    LR25                NO                                           
         CLC   COMKCLT,MYCLT       CLIENT MATCH?                                
         BNE   LR10                NO...READ NEXT REC                           
         B     LR30                YES, DISPLAY IT                              
*                                                                               
LR25     MVC   SVKEY,KEY                                                        
         BAS   RE,GETCLT                                                        
         MVC   KEY(13),SVKEY       RESTORE DIR FOR SEQ READING                  
         GOTO1 HIGH                                                             
*                                                                               
LR30     GOTO1 CLUNPK,DMCB,(SVCPROF+6,COMKCLT),LCLT                             
*                                                                               
         OC    MYPRD,MYPRD         PRODUCT FILTER?                              
         BZ    LR31                NO                                           
         OC    COMKPRD,COMKPRD     IS THERE A PRODUCT TO SHOW?                  
         BZ    LR10                NO...READ THE NEXT RECORD                    
         CLC   =C'ALL',MYPRD       ALL PRODUCTS?                                
         BE    LR31                YES                                          
         CLC   =C'POL',MYPRD       POL OF PRODUCTS?                             
         BE    LR31                YES                                          
         CLC   MYPRD,COMKPRD       PRODUCT MATCH?                               
         BNE   LR10                NO...DONE                                    
         B     LR32                                                             
*                                                                               
LR31     CLC   =C'ALL',COMKPRD     ALL PRODUCTS?                                
         BNE   *+14                NO                                           
         MVC   LPRD,COMKPRD        MOVE 'ALL' TO LIST LINE                      
         B     LR35                                                             
         CLC   =C'POL',COMKPRD     POL OF PRODUCTS?                             
         BNE   *+14                NO                                           
         MVC   LPRD,COMKPRD        MOVE 'POL' TO LIST LINE                      
         B     LR35                                                             
         OC    COMKPRD,COMKPRD     IS THERE A PRODUCT TO SHOW?                  
         BZ    LR40                NO,DISPLAY 1ST COMNT LINE (NO EST)           
*                                                                               
LR32     MVC   BYTE,COMKPRD+2                                                   
         BAS   RE,GETPRD           GET PRODUCT NAME                             
         MVC   LPRD,WORK           MOVE IT TO THE LIST LINE                     
*                                                                               
LR35     SR    R0,R0               ESTIMATE                                     
         ICM   R0,1,COMKEST                                                     
         BZ    LR40                                                             
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LEST,DUB                                                         
*                                                                               
LR38     CLI   COMKSTA,0           FLIGHT                                       
         BE    LR40                                                             
         EDIT  COMKFLT,(2,LFLT)                                                 
*                                                                               
LR40     DS    0H                                                               
         GOTO1 GETREC              GET OCOM RECORD                              
         L     R6,AIO              GET COMMENT ELEMENT X'05'                    
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BNE   LR55                NO COMMENT ELEMENTS FOUND                    
*                                                                               
LR50     MVC   LCOM,BLANKS                                                      
         ZIC   RE,1(R6)            GET ELEMENT LENGTH                           
         SHI   RE,3                                                             
         CHI   RE,44               <= 44? (45 = L'LCOM - 1 FOR EX)              
         BNH   *+8                 NOPE                                         
         LA    RE,44               YES, DONT CLOBBER FIELDS AFTER LCOM!         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   LCOM(0),2(R6)       ** EXECUTED **                               
LR55     GOTO1 LISTMON                                                          
         MVC   LISTAR,BLANKS       CLEAR LIST LINE                              
LR70     B     LR10                                                             
*                                                                               
LRX      B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*                          GETFLTRC                                   *         
***********************************************************************         
GETFLTRC NTR1                                                                   
*                                                                               
         XC    KEY,KEY             SEE IF WE HAVE THE FLIGHT REC                
         LA    R4,KEY                                                           
         USING DFLRECD,R4          FLIGHT RECORD                                
         MVI   DFLKTYP,DFLKTYPQ    X'0D'                                        
         MVI   DFLKSUB,DFLKSUBQ    X'38'                                        
         MVC   DFLKAGMD,BAGYMD     AGENCY/MEDIA                                 
         MVC   DFLKCLT,BCLT        CLIENT                                       
         CLC   MYPRD,=C'ALL'       ALL PRODUCTS?                                
         BNE   *+14                NO                                           
         MVC   DFLKPRD,MYPRD       YES, MOVE IN 'ALL' PRODUCTS                  
         B     *+10                                                             
         MVC   DFLKPRD,QPRD        PRODUCT NAME                                 
         MVC   DFLKEST,BEST        ESTIMATE                                     
         DROP  R4                                                               
*                                                                               
         MVI   RDUPDATE,C'N'       DO NOT READ FOR UPDATE                       
         GOTO1 HIGH                                                             
*                                                                               
         LA    R2,OCOFLITH                                                      
         CLC   KEY(L'DFLKEY),KEYSAVE   MATCH?                                   
         BE    GF10                YES                                          
*                                                                               
         CLC   =C'POL',MYPRD       DID WE CHECK PRODUCT 'POL' YET?              
         BE    ERRFLTNF            YES...FLIGHT NOT FOUND                       
*                                                                               
         XC    KEY,KEY             SEE IF WE HAVE THE FLIGHT REC                
         LA    R4,KEY                                                           
         USING DFLRECD,R4          FLIGHT RECORD                                
         MVI   DFLKTYP,DFLKTYPQ    X'0D'                                        
         MVI   DFLKSUB,DFLKSUBQ    X'38'                                        
         MVC   DFLKAGMD,BAGYMD     AGENCY/MEDIA                                 
         MVC   DFLKCLT,BCLT        CLIENT                                       
         MVC   DFLKPRD,=C'POL'     PRODUCT...THIS TIME CHECK POL                
         MVC   DFLKEST,BEST        ESTIMATE                                     
         DROP  R4                                                               
*                                                                               
         MVI   RDUPDATE,C'N'       DO NOT READ FOR UPDATE                       
         GOTO1 HIGH                                                             
*                                                                               
         LA    R2,OCOFLITH                                                      
         CLC   KEY(L'DFLKEY),KEYSAVE  MATCH?                                    
         BNE   ERRFLTNF            NO...FLIGHT NOT FOUND                        
*                                                                               
GF10     MVI   RDUPDATE,C'N'       PULL THE RECORD OUT SO WE CAN                
         GOTO1 GETREC              VALIDATE FLT # (FLIGHT REC IN AIO)           
*                                                                               
GFX      B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*                            COMMENTS                                 *         
***********************************************************************         
COMMENTS NTR1                                                                   
                                                                                
         LA    R2,OCOCMNTH         COMMENT FIELD HEADER                         
         XC    8(20,R2),8(R2)      CLEAR IT                                     
         OI    6(R2),X'80'         TRANSMIT                                     
***                                                                             
* CLIENT LEVEL                                                                  
***                                                                             
         MVC   SAVEKEY,KEY         SAVE OFF THE KEY                             
         OC    SAVEKEY+6(3),SAVEKEY+6                                           
         BZ    CMNT10              THIS IS CLIENT LEVEL                         
         XC    KEY,KEY                                                          
         MVC   KEY(6),SAVEKEY                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     CLIENT LEVEL FOUND?                          
         BNE   CMNT10              NO                                           
         MVC   8(4,R2),=C'CLT,'                                                 
         LA    R2,4(R2)                                                         
***                                                                             
* PRODUCT LEVEL                                                                 
***                                                                             
CMNT10   MVC   KEY,SAVEKEY                                                      
         OC    SAVEKEY+6(3),SAVEKEY+6                                           
         BZ    CMNT15              NOT PRODUCT LEVEL                            
         CLI   SAVEKEY+9,0         IS THIS A PRODUCT LEVEL                      
         BE    CMNT20              YES                                          
CMNT15   XC    KEY+9(4),KEY+9      CLEAR ALL AFTER PRODUCT                      
         GOTO1 HIGH                                                             
         B     CMNT17                                                           
*                                                                               
CMNT16   GOTO1 SEQ                                                              
CMNT17   CLC   KEY(6),SAVEKEY      MATCH UP TO CLIENT?                          
         BNE   CMNT20              NO...NO PRODUCT LEVEL FOUND                  
         CLI   SAVEKEY+8,0         SEARCH SPECIFIC PRODUCT?                     
         BE    CMNT18              NO...SEARCH ALL PRODUCTS FOR CLIENT          
         CLC   KEY(9),SAVEKEY      MATCH UP TO PRODUCT?                         
         BNE   CMNT20              NO PRD LEVEL FOR SPECIFIC PRD                
*                                                                               
CMNT18   OC    KEY+6(3),KEY+6      ANY PRODUCT?                                 
         BZ    CMNT16              NO                                           
*                                                                               
         CLI   KEY+9,0             ONLY PRODUCT LEVEL?                          
         BNE   CMNT16              NO                                           
         MVC   8(4,R2),=C'PRD,'                                                 
         LA    R2,4(R2)                                                         
***                                                                             
* ESTIMATE LEVEL                                                                
***                                                                             
CMNT20   MVC   KEY,SAVEKEY                                                      
         CLI   SAVEKEY+9,0         IS THIS ESTIMATE LEVEL                       
         BE    CMNT25              NO                                           
         CLI   SAVEKEY+10,0        IS THERE A FLIGHT?                           
         BE    CMNT30              NO...THIS IS AN ESTIMATE LEVEL               
CMNT25   XC    KEY+10(3),KEY+10    CLEAR ALL AFTER ESTIMATE                     
         GOTO1 HIGH                                                             
         B     CMNT27                                                           
*                                                                               
CMNT26   GOTO1 SEQ                                                              
CMNT27   CLC   KEY(6),SAVEKEY      MATCH UP TO CLIENT?                          
         BNE   CMNT30              NO...NO ESTIMATE LEVEL FOUND                 
         CLI   SAVEKEY+8,0         SEARCH SPECIFIC PRODUCT?                     
         BE    CMNT28              NO...SEARCH ALL PRODUCTS FOR CLIENT          
         CLC   KEY(9),SAVEKEY      MATCH UP TO PRODUCT?                         
         BNE   CMNT30              NO PRD LEVEL FOR SPECIFIC PRD                
         CLI   SAVEKEY+9,0         SEARCH SPECIFIC ESTIMATE?                    
         BE    CMNT28              NO                                           
         CLC   KEY(10),SAVEKEY     MATCH UP TO ESTIMATE?                        
         BNE   CMNT30              NO EST LEVEL FOR SPECIFIC EST                
*                                                                               
CMNT28   CLI   KEY+9,0             ANY ESTIMATE?                                
         BE    CMNT26              NO                                           
*                                                                               
         CLI   KEY+10,0            ONLY ESTIMATE LEVEL?                         
         BNE   CMNT26              NO                                           
         MVC   8(4,R2),=C'EST,'                                                 
         LA    R2,4(R2)                                                         
***                                                                             
* FLIGHT LEVEL                                                                  
***                                                                             
CMNT30   MVC   KEY,SAVEKEY                                                      
         CLI   SAVEKEY+10,0        IS THIS A FLIGHT LEVEL?                      
         BH    CMNT40              YES                                          
         GOTO1 HIGH                                                             
         B     CMNT36                                                           
*                                                                               
CMNT35   GOTO1 SEQ                                                              
CMNT36   CLC   KEY(6),SAVEKEY      MATCH UP TO CLIENT?                          
         BNE   CMNT40              NO...NO FLIGHT LEVEL FOUND                   
         CLI   SAVEKEY+8,0         SEARCH SPECIFIC PRODUCT?                     
         BE    CMNT38              NO...SEARCH ALL PRODUCTS FOR CLIENT          
         CLC   KEY(9),SAVEKEY      MATCH UP TO PRODUCT?                         
         BNE   CMNT40              NO PRD LEVEL FOR SPECIFIC PRD                
         CLI   SAVEKEY+9,0         SEARCH SPECIFIC ESTIMATE?                    
         BE    CMNT38              NO                                           
         CLC   KEY(10),SAVEKEY     MATCH UP TO ESTIMATE?                        
         BNE   CMNT40              NO EST LEVEL FOR SPECIFIC EST                
*                                                                               
CMNT38   CLI   KEY+10,0            ANY FLIGHT?                                  
         BE    CMNT35              NO                                           
*                                                                               
         MVC   8(4,R2),=C'FLT,'                                                 
         LA    R2,4(R2)                                                         
*                                                                               
CMNT40   LA    R3,OCOCMNTH         DID WE HAVE ANY COMMENTS?                    
         CR    R2,R3                                                            
         BE    CMNT50              NO                                           
         SHI   R2,1                YES WE DID...BUMP BACK 2 SPACES AND          
         MVI   8(R2),0             CLEAR ', ' FROM LAST INPUT                   
*                                                                               
CMNT50   MVC   KEY,SAVEKEY                                                      
         CLI   ACTNUM,ACTADD       IS THIS AN ADD                               
         BE    CMNTX               YES...RECORD NOT THERE YET                   
         GOTO1 HIGH                                                             
         CLC   KEY,SAVEKEY                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CMNTX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*                             CHKPF                                   *         
*        CHECK WHICH PF KEY WAS HIT AND RESPOND ACCORDINGLY           *         
***********************************************************************         
CHKPF    NTR1                                                                   
         CLI   PFKEY,5             PAGE DOWN?                                   
         BE    *+12                NO                                           
         CLI   PFKEY,6             PAGE UP?                                     
         BNE   *+12                NO                                           
         BAS   RE,NEWPAGE                                                       
         B     CPX                                                              
*                                                                               
         CLI   PFKEY,3             ERASE LINE?                                  
         BE    CP05                YES                                          
         CLI   PFKEY,4             ADD A LINE?                                  
         BNE   CPX                 NO...THATS ALL THIS PROGRAM SUPPORTS         
*                                                                               
CP01     LA    R3,20               MAX 20 COMMENTS PER OCOM RECORD              
         L     R6,AIO                                                           
         MVI   ELCODE,X'05'        ELEMENT X'05' FOR COMMENT LINE               
         BAS   RE,GETEL            FOUND?                                       
         B     *+8                                                              
         BAS   RE,NEXTEL           HAVE ANOTHER COMMENT ELEMENT?                
         BNE   CP05                NO                                           
         BCT   R3,*-8                                                           
         LA    R2,OCOL10H          POINT TO 10TH LINE                           
         B     ERRLINES                                                         
*                                                                               
CP05     L     R1,SYSPARMS                                                      
         L     R3,0(R1)            R3=A(TRANSLATOR I/O BLOCK)                   
         USING TIOBD,R3                                                         
         SR    R1,R1                                                            
         ICM   R1,3,TIOBCURS       ABSOLUTE CURSOR ADDRESS                      
         SR    R0,R0                                                            
         D     R0,=F'80'                                                        
         MHI   R1,80               ABSOLUTE ADDR OF BEGINNING OF LINE           
         DROP  R3                                                               
*                                                                               
         LA    R2,OCOLN1H          FIRST COMMENT INPUT LINE ON SCREEN           
         L     R6,AIO                                                           
         MVI   ELCODE,X'05'        ELEMENT X'05' FOR COMMENT LINE               
         BAS   RE,GETEL            FOUND?                                       
         BNE   CPX                 NO                                           
         CLI   OCOLHD+5,C'1'       HAVE LINE 11-20 ALREADY DISPLAYED?           
         BNE   CP10                NO                                           
*                                                                               
         LA    R3,10               POINT TO 11TH OCOM ELEMENT                   
         BAS   RE,NEXTEL           HAVE OCOM ELEMENT?                           
         BNE   CPX                 NO, DONE                                     
         BCT   R3,*-8                                                           
*                                                                               
CP10     SR    R5,R5                                                            
         ICM   R5,3,2(R2)          ABSOLUTE SCREEN ADDR OF THIS FIELD           
         SR    R4,R4                                                            
         D     R4,=F'80'                                                        
         MHI   R5,80               ABSOLUTE SCREEN ADDR OF LINE START           
         LA    R4,L'OCOLN1(R5)                                                  
         CR    R5,R1               WAS CURSOR ON THIS LINE?                     
         BH    CPX                 NO - IT'S ABOVE THIS FIELD - DONE            
         CR    R4,R1                                                            
         BNL   CP20                YES                                          
*                                                                               
         LR    R3,R6               SAVE LAST A(ELEMENT)                         
         BAS   RE,NEXTEL           BUMP TO NEXT ELEMENT                         
         BE    CP15                                                             
         LR    R6,R3                                                            
*                                                                               
CP15     BAS   RE,NXTUN            BUMP TO NEXT UNPROTECTED FIELD               
         LA    RF,OCOL10H          LAST FIELD ON SCREEN                         
         CR    R2,RF               END OF SCREEN?                               
         BH    CPX                 YES - IGNORE                                 
         B     CP10                                                             
*                                                                               
CP20     CLI   PFKEY,3             ERASE LINE?                                  
         BNE   CP50                NO                                           
         GOTO1 RECUP,DMCB,(0,AIO),(R6),(R6)                                     
         B     CP60                                                             
*                                                                               
CP50     ZIC   R1,1(R6)            GET START OF NEXT ELEMENT                    
         AR    R6,R1                                                            
         MVC   ELEM(3),=X'050300'                                               
         GOTO1 RECUP,DMCB,(0,AIO),ELEM,(R6)                                     
         BAS   RE,NXTUN            BUMP TO NEXT UNPROTECTED FIELD               
*                                                                               
CP60     ST    R2,ACURFORC         KEEP CURSOR IN PLACE                         
*                                                                               
CPX      B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*                             CNTCOMLN                                *         
*                  COUNT NUMBER OF LINES INPUT                        *         
***********************************************************************         
CNTCOMLN NTR1                                                                   
*                                                                               
         LA    R2,OCOLN1H                                                       
         MVI   NUMLNS,0            NUMBER OF LINES ON SCREEN                    
         XC    CMNTADD,CMNTADD                                                  
         LA    R1,10               MAX NUMBER OF LINES                          
         SR    R3,R3                                                            
*                                                                               
CL10     CLI   5(R2),0             ANY INPUT ON THIS LINE?                      
         BE    CL20                NO                                           
         ST    R2,CMNTADD                                                       
*                                                                               
CL20     BAS   RE,NXTUN            SKIP TO NEXT UNPROTECTED LINE                
         BCT   R1,CL10                                                          
*                                                                               
         OC    CMNTADD,CMNTADD                                                  
         BZ    CL50                                                             
*                                                                               
         LA    R2,OCOLN1H                                                       
         LA    R1,10               MAX NUMBER OF LINES                          
         LA    R3,1                                                             
                                                                                
CL30     C     R2,CMNTADD                                                       
         BE    CL50                NO                                           
         AHI   R3,1                                                             
CL40     BAS   RE,NXTUN            SKIP TO NEXT UNPROTECTED LINE                
         BCT   R1,CL30                                                          
                                                                                
CL50     STC   R3,NUMLNS                                                        
         CLI   OCOLHD+5,C'1'       UPDATING LINES 11-20?                        
         BE    EXIT                YES, EXIT                                    
         L     R3,AIO2                                                          
         CLI   0(R3),0             HAVE ANY LINES 11-20?                        
         BE    EXIT                NO, EXIT                                     
         MVI   NUMLNS,10           YES - MUST HAVE FIRST 10 LINES               
         LA    R2,OCOL10H                                                       
         ST    R2,CMNTADD                                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*                            PRNTOUT                                  *         
*                     PUT COMMENTS TO LINE                            *         
***********************************************************************         
PRNTOUT  NTR1                                                                   
*                                                                               
         LA    R4,10               ONLY DISPLAY 10 COMMENT LINES                
*                                                                               
PO10     ZIC   R1,1(R6)            ELEMENT LENGTH                               
         SHI   R1,3                SUB ELCODE/ELEM LENGTH/& 1 FOR EX            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),2(R6)       ** EXECUTED **                               
         BAS   RE,NXTUN            FIND NEXT UNPROTECTED FIELD                  
         ICM   R3,15,OCOL10H       OCOM ONLY HAVE 10 LINES                      
*                                                                               
         CR    R2,R3                                                            
         BNH   *+6                                                              
         DC    H'0'                                                             
         BAS   RE,NEXTEL           GET NEXT ELEMENT                             
         BNE   POX                                                              
         BCT   R4,PO10                                                          
*                                                                               
POX      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*                             NXTUN                                   *         
*                  FIND NEXT UNPROTECTED FIELD                        *         
***********************************************************************         
NXTUN    ZIC   RF,0(R2)                                                         
         LTR   RF,RF                                                            
         BNZ   *+6                 END OF SCREEN                                
         DC    H'0'                                                             
         AR    R2,RF                                                            
         TM    1(R2),X'20'         IS THIS PROTECTED                            
         BO    NXTUN               YES GET NEXT FIELD                           
*                                                                               
NXX      BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*                            NEWPAGE                                            
*                      DISPLAY PAGE UP/DOWN                                     
***********************************************************************         
NEWPAGE  NTR1                                                                   
*                                                                               
         CLI   PFKEY,5              PAGE DOWN?                                  
         BNE   NP10                 NO, MUST BE PAGE UP                         
         CLI   OCOLHD+5,C'1'        HAVE LINE 11-20 ALREADY DISPLAYED?          
         BE    NPX                  YES, NOTHING TO DO HERE                     
         LA    R3,11                START DISPLAYING AT LINE 1                  
         B     NP20                                                             
*                                                                               
NP10     CLI   OCOLHD+5,C'1'        HAVE LINE 1-10 ALREADY DISPLAYED?           
         BNE   NPX                  YES, NOTHING TO DO HERE                     
         LA    R3,1                 START DISPLAYING AT LINE 11                 
*                                                                               
NP20     LA    R2,OCOLHDH           FIRST LINE                                  
         XR    RF,RF                                                            
         XR    R5,R5                                                            
         LA    R4,OCOLN10H          LAST LINE                                   
*                                                                               
NP30     EDIT  (R3),(2,13(R2)),ZERO=BLANK                                       
         OI    6(R2),X'80'          TRANSMIT                                    
         IC    RF,0(R2)             BUMP TO NEXT FIELD                          
         AR    R2,RF                                                            
*                                                                               
         IC    R5,0(R2)             LENGTH OF FIELD + HEADERS                   
         SHI   R5,17                HDR + EXTENDED FIELD HDR + 1 FOR EX         
         EX    R5,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)        CLEAR THE FIELD                             
*                                                                               
         IC    RF,0(R2)             BUMP TO NEXT PROTECTED FIELD                
         AR    R2,RF                                                            
         AHI   R3,1                 BUMP LINE COUNT                             
         CR    R2,R4                <= LAST LINE?                               
         BNH   NP30                 YES                                         
*                                                                               
NPX      B     EXIT                                                             
***********************************************************************         
*                            GETCLT                                             
*                        GET THE CLIENT RECORD                                  
*                        SET SVCLIST                                            
***********************************************************************         
GETCLT   NTR1                                                                   
         XC    SVCPROF,SVCPROF                                                  
*                                                                               
         LA    R6,SVKEY                                                         
         USING COMHDRD,R6          USED TO BE R6                                
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),COMKAGY                                                 
         MVC   KEY+2(3),COMKCLT                                                 
         L     R4,AIO2                                                          
         ST    R4,AIO                                                           
         USING CLTHDRD,R4                                                       
         CLC   KEY(13),CKEY        TEST HAVE CLTHDR ALREADY                     
         BE    GCLTX                                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    GCLT05                                                           
* BETTER NOT TO DIE IF CLIENT HAS BEEN DELETED                                  
         LA    R1,CKEY-CLTHDRD(R4)                                              
         XC    0(256,R1),0(R1)                                                  
         MVC   0(13,R1),KEY        FORCE KEYS EQUAL                             
         LA    R1,CLIST-CLTHDRD(R4)                                             
         XC    0(256,R1),0(R1)     CLEAR THE LIST                               
         B     GCLTX                                                            
*                                                                               
GCLT05   GOTO1 GETREC                                                           
*                                                                               
         LA    R2,CLIST            SAVE CLIENT PRODUCT LIST                     
         LA    R3,880                                                           
         LA    RE,SVCLIST                                                       
         LA    RF,880                                                           
         MVCL  RE,R2                                                            
*                                                                               
         MVC   SVCPROF,CPROF       SAVE CLIENT PROFILES                         
*                                                                               
GCLTX    MVC   AIO,AIO1                                                         
         B     EXIT                                                             
         DROP  R4,R6                                                            
*                                                                               
***********************************************************************         
*                             GETPRD                                  *         
*                        GET PRODUCT CODE                             *         
***********************************************************************         
GETPRD   NTR1                                                                   
         LA    R1,SVCLIST                                                       
*                                                                               
GP20     CLC   BYTE,3(R1)                                                       
         BE    GP30                PRD NUMBER SAME, GET MNEMONIC                
         LA    R1,4(R1)                                                         
         CLI   0(R1),C' '                                                       
         BNL   GP20                                                             
         LA    R1,=C'***'                                                       
*                                                                               
GP30     MVC   WORK(3),0(R1)                                                    
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*                            INIT                                     *         
***********************************************************************         
INIT     NTR1                                                                   
         XC    SVMYPSWD,SVMYPSWD   AUTHORIZATION NUMBER                         
         GOTO1 GETFACT,DMCB,0      GET SOME DATA FROM GETFACT                   
         USING FACTSD,R3                                                        
         L     R3,DMCB                                                          
         MVC   SVSECAGY,FATAGYSC   SAVE SECURITY AGENCY                         
         TM    FATFLAG,X'08'                                                    
         BZ    *+10                                                             
         MVC   SVMYPSWD,FAPASSWD   SAVE AUTHORIZATION NUMBER                    
         DROP  R3                                                               
*                                                                               
         OI    GENSTAT4,NODELLST   CANNOT DELETE FROM A LIST                    
         OI    CONSERVH+6,X'81'    FORCE SCREEN CHANGE FOR PFKEYS               
         OI    OCOLN1H+6,X'81'     FORCE SCREEN CHANGE FOR PFKEYS               
*                                                                               
         MVI   ACTELOPT,C'Y'       ADD ACTIVITY ELEMENT                         
*                                                                               
         L     RF,ATIOB            A(TIOB)                                      
         USING TIOBD,RF                                                         
         ZIC   R0,TIOBAID          PICK UP PFKEY VALUE                          
         CHI   R0,12                                                            
         BNH   *+8                                                              
         SHI   R0,12                                                            
         STC   R0,PFKEY            SAVE ADJUSTED PFKEY VALUE                    
         DROP  RF                                                               
*                                                                               
         CLI   PFKEY,0                                                          
         BE    EXIT                                                             
         OI    GENSTAT2,RETEQSEL                                                
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        ADD A REQ TO REQUEST FILE                                    *         
***********************************************************************         
REQ      DS    0H                                                               
         L     R1,AIO2                                                          
         XC    0(250,R1),0(R1)                                                  
         MVI   10(R1),41                                                        
         MVI   14(R1),106                                                       
         LA    R1,26(R1)                                                        
         MVI   0(R1),X'40'                                                      
         MVC   1(79,R1),0(R1)                                                   
         MVC   0(2,R1),=C'L6'                                                   
         MVC   2(2,R1),AGENCY                                                   
         MVC   4(1,R1),QMED                                                     
         MVC   61(1,R1),SVKEY+3    COMMENT TYPE                                 
         MVC   5(3,R1),=C'ALL'     CLIENT                                       
         MVI   8(R1),C' '          PRODUCT GROUP                                
         MVC   11(3,R1),=C'ALL'    PRODUCT                                      
         MVC   23(3,R1),=C'ALL'    ESTIMATE                                     
         CLC   SVKEY+4(2),=X'0000' CLIENT SPECIFIED                             
         BE    REQ30                                                            
         MVC   5(3,R1),QCLT        YES                                          
         CLC   SVKEY+6,=X'000000'  PRODUCT OR GROUP SPECIFIED                   
         BE    REQ20                                                            
         CLI   SVKEY+6,X'00'       PRODUCT NUMBER USED                          
         BE    REQ10                                                            
         MVC   8(1,R1),SVKEY+6                                                  
*                                                                               
         ZIC   R3,SVBKLNS                                                       
         BCTR  R3,0                                                             
         MVC   HALF,SVKEY+7                                                     
         OI    HALF+1,X'0F'                                                     
         UNPK  WORK(3),HALF                                                     
         MVI   11(R1),C' '                                                      
         MVC   12(2,R1),11(R1)                                                  
         EX    R3,*+8              SHORTEN TO BREAK LEN                         
         B     REQ20                                                            
         MVC   11(0,R1),WORK       ** EXECUTED **                               
*                                                                               
REQ10    MVC   11(3,R1),QPRD       YES                                          
*                                                                               
REQ20    CLI   SVKEY+9,X'00'       ESTIMATE SPECIFIED                           
         BE    REQ30                                                            
         ZIC   R3,SVKEY+9          YES                                          
         CVD   R3,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  23(3,R1),DUB+6(2)                                                
*                                                                               
REQ30    MVI   65(R1),C'*'                                                      
         MVC   68(7,R1),=C'CONTROL'                                             
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'REQUEST',AIO2,AIO2                     
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*                        ERROR MESSAGES                               *         
***********************************************************************         
ERRMIS   MVC   ERRNUM,=AL2(1)       MISSING INPUT FIELD                         
         B     SPERREX                                                          
ERRFLTNU MVC   ERRNUM,=AL2(924)     FLIGHT NUMBER MUST BE NUMERIC               
         B     SPERREX                                                          
ERRFLT17 MVC   ERRNUM,=AL2(925)     FLIGHT NUMBER MUST BE <17                   
         B     SPERREX                                                          
ERRFLTEL MVC   ERRNUM,=AL2(926)     NO FLIGHT ELEMENTS EXIST                    
         B     SPERREX                                                          
ERRFLTNM MVC   ERRNUM,=AL2(927)     NO MATCHING FLIGHT NUMBERS FOUND            
         B     SPERREX                                                          
ERRFLTNF MVC   ERRNUM,=AL2(53)      FLIGHT RECORD NOT FOUND                     
         B     SPERREX                                                          
ERRMTPRD MVC   ERRNUM,=AL2(943)     PRODUCT CANNOT BE POPULATED                 
         B     SPERREX                                                          
ERRMTEST MVC   ERRNUM,=AL2(937)     ESTIMATE CANNOT BE POPULATED                
         B     SPERREX                                                          
ERRMTES2 MVC   ERRNUM,=AL2(949)     MUST HAVE ESTIMATE FOR ALL PRODUCTS         
         B     SPERREX                                                          
ERRMTFLT MVC   ERRNUM,=AL2(938)     FLIGHT CANNOT BE POPULATED                  
         B     SPERREX                                                          
ERRALLPR MVC   ERRNUM,=AL2(939)     ALL CANNOT CO-EXIST W/ OTHER PRD            
         B     SPERREX                                                          
ERRINV   MVC   ERRNUM,=AL2(002)     INVALID                                     
         B     SPERREX                                                          
ERRLINES MVC   ERRNUM,=AL2(944)     TOO MANY COMMENT LINES                      
         B     SPERREX                                                          
ERRBCLT  MVC   ERRNUM,=AL2(933)     CLIENT MUST BE BRAND                        
         B     SPERREX                                                          
*                                                                               
SPERREX  OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM                                                   
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,2                                                         
         MVC   AIO,AIO1                                                         
         GOTO1 ERREX                                                            
         DROP  RF                                                               
         SPACE 2                                                                
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
***********************************************************************         
*                          CONSTANTS                                  *         
***********************************************************************         
         DS    0F                                                               
MEDTAB   DC    C'TRNX...C'         FOR DK (DISPLAYING MEDIA)                    
BLANKS   DC  80C' '                80 SPACES                                    
         LTORG                                                                  
         SPACE 3                                                                
********************************************************                        
*                     DSECTS                           *                        
********************************************************                        
* SPSFMFFD                                             *                        
* DDGENTWA                                             *                        
* SCSFM47D          MAINTENANCE SCREEN                 *                        
* SCSFM48D          LIST SCREEN                        *                        
* SPGENDRFLT        FLIGHT RECORD                      *                        
*COMHDRD  DSECT     GENERAL COMMENT (OCOM RECORD)      *                        
* SPGENCOM                                             *                        
*CLTHDRD  DSECT     FOR PRODUCT MNEMONIC               *                        
* SPGENCLT                                             *                        
* FAGETTXTD         ERROR MSGS (SPERREX)               *                        
* DDACTIVD          ACTIVITY ELEMENT DSECT             *                        
* SPGENPRG                                             *                        
* DDSPLWORKD                                           *                        
* DDSPOOLD                                             *                        
* DDBIGBOX                                             *                        
* FAFACTS                                              *                        
* FATIOB                                               *                        
* SPSFMWORKD                                           *                        
********************************************************                        
       ++INCLUDE SPOMSDSCTS                                                     
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE FATIOB                                                         
       ++INCLUDE SPOMSFFD          (BASE SCREEN FOR SYSTEM)                     
         ORG   CONTAGH                                                          
       ++INCLUDE SPOMSC1D          (OUR MAINTENANCE SCREEN OVERLAY)             
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPOMSC2D          (OUR LIST SCREEN OVERLAY)                    
         EJECT                                                                  
       ++INCLUDE SPGENDRFLT        FLIGHT RECORD                                
         EJECT                                                                  
*                                                                               
COMHDRD  DSECT                     GENERAL COMMENT (OCOM RECORD)                
COMHDR   DS    0C                                                               
COMKEY2  DS    0CL13     V         KEY                                          
COMKTYPE DS    CL2       B         RECORD TYPE (X'0D0C')                        
COMKAGY  DS    CL1       B         AGENCY/MEDIA                                 
COMCTYPE DS    CL1       A         COMMENT TYPE (SEE BOTTOM)                    
COMKCLT  DS    CL2       B         CLIENT CODE OR AGENCY COMMENT NO.            
COMKPRD  DS    CL3       A/B       PRODUCT GROUP OR NUMBER                      
COMKEST  DS    CL1       B         ESTIMATE NUMBER                              
COMKSTA  DS    XL3       B         PACKED STATION CODE                          
         ORG   *-3                                                              
         DS    X'00'               *** OR ***                                   
COMKMKT  DS    XL2                 MARKET NUMBER                                
         ORG   *-3                                                              
COMKFLT  DS    XL1                 FLIGHT  *** FOR OCOM ***                     
         DS    XL2                 SPARE                                        
         ORG   COMKEY2             I2 KEY IS DIFFERENT                          
         ORG                                                                    
COMLEN   DS    CL2       B         RECORD LENGTH                                
COMCTL   DS    CL1       B         CONTROL BYTE                                 
COMLINK  DS    CL4       B         LINK FIELD                                   
         DS    CL4       B         SPARE                                        
COMEL    DS    CL1       B         PROFILE ELEMENT CODE (X'01')                 
COMELEN  DS    CL1       B         ELEMENT LENGTH (12)                          
COMCREAT DS    CL3       B         CREATION DATE (YYMMDD)                       
COMACTIV DS    CL3       B         LAST ACTIVITY DATE (YYMMDD)                  
COMPROF1 DS    CL1       B         PROFILE BYTE                                 
*                                  X'80'  PAGE=ALL                              
         DS    CL3       B         SPARE                                        
*                                                                               
CT0REC   DSECT                                                                  
CT0KEY   DS    0CL25     V         KEY                                          
CT0KTYP  DS    CL1       C         RECORD TYPE                                  
CT0KTEQU EQU   C'0'                                                             
CT0KAGY  DS    CL2       C         AGENCY ALPHA CODE                            
CT0KEYS  DS    0CL22                                                            
*                                  AUTHORIZATION NUMBER RECORDS                 
         DS    XL20      X         SPARE (BINARY ZEROES)                        
CT0KNUM  DS    XL2       X         PERSON AUTH NUMBER (X'FFFF'-X'0000')         
*                                  AUTHORIZATION CODE RECORDS                   
         ORG   CT0KEYS                                                          
CT0KPID  DS    CL8       C         PERSONAL ID IF REQUIRED ELSE NULLS           
CT0KDTM  DS    XL4       X         EFFECTIVE DATE/TIME 1'S COMPLEMENT           
CT0KCODE DS    CL10      C         SECRET LOG-ON CODE                           
*                                  PERSON RECORDS                               
         ORG   CT0KEYS                                                          
CT0KOFFC DS    CL2       C         OFFICE CODE                                  
CT0KLAST DS    CL18      C         LAST NAME                                    
CT0KFI   DS    CL1       C         FIRST INITIAL                                
CT0KMI   DS    CL1       C         MIDDLE INITIAL                               
*                                                                               
CT0LEN   DS    XL2       X         RECORD LENGTH                                
CT0STAT  DS    XL1       X         STATUS (X'20' LOCKED)                        
CT0DATA  DS    0C        V         DATE (X'01' ACTIVITY ELEMENT)                
*                                       (X'03' POINTER ELEMENTS)                
*                                       (X'21' SYSTEM ELEMENTS)                 
         EJECT                                                                  
CLTHDRD  DSECT                     FOR PRODUCT MNEMONIC                         
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
       ++INCLUDE FAGETTXTD         ERROR MSGS (SPERREX)                         
         EJECT                                                                  
       ++INCLUDE DDACTIVD          ACTIVITY ELEMENT DSECT                       
         EJECT                                                                  
*              DSECT TO COVER SECURITY INFO                                     
AUTHD    DSECT                                                                  
SECRAGY  DS    XL2                 SECURITY AGENCY                              
PASSWD   DS    XL2                 AUTHORIZATION NUMBER                         
PRSNLID  DS    CL8                 PERSONAL ID                                  
         EJECT                                                                  
       ++INCLUDE SPGENPRG                                                       
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE SPOMSWORKD                                                     
***********************************************************************         
*                     START OF SAVED STORAGE                          *         
***********************************************************************         
MYAREAD  DSECT                                                                  
RELO     DS    A                   RELOCATION FACTOR                            
CMNTADD  DS    F                   LAST COMMENT ADDRESS                         
MYFLT    DS    XL1                 HOLDS TEMP FLIGHT NUMBER                     
ERRNUM   DS    XL2                 FOR ERROR MESSAGES (SPERREX)                 
SAVEKEY  DS    CL(L'KEY)           USED TO SAVE THE KEY                         
SVBKLNS  DS    XL3                 SAVES NUMBER OF BLANK LINES                  
MYCLT    DS    XL2                 CLIENT FOR LR                                
MYPRD    DS    XL3                 PRODUCT FOR LR                               
MYEST    DS    XL1                 ESTIMATE FOR LR                              
NUMLNS   DS    XL1                 COUNTS NUMBER OF COMMENT LINES IN VR         
ELSADDED DS    XL1                 MAKES SURE WE DONT ADD EXTRA ELEMENT         
SVMYPSWD DS    XL2                 AUTHORIZATION NUMBER                         
SVSECAGY DS    XL2                 SECURITY AGENCY                              
*                                                                               
WKAGY    DS    XL1                 FOR DISPLAY KEY USE                          
WKCTYPE  DS    XL1                 CAN BE USED FOR OTHER PURPOSES TOO           
WKCLT    DS    XL2                                                              
WKPRD    DS    XL3                                                              
WKEST    DS    XL1                                                              
WKFLT    DS    XL3                                                              
***********************************************************************         
*        LIST LINE DSECT                                              *         
***********************************************************************         
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
         DS    CL1                                                              
LCLT     DS    CL3                 CLIENT                                       
         DS    CL3                                                              
LPRD     DS    CL3                 PRODUCT                                      
         DS    CL2                                                              
LEST     DS    CL3                 ESTIMATE                                     
         DS    CL3                                                              
LFLT     DS    CL2                 FLIGHT                                       
         DS    CL4                                                              
LCOM     DS    CL45                1ST LINE OF COMMENT                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'201SPOMS19   01/17/07'                                      
         END                                                                    
