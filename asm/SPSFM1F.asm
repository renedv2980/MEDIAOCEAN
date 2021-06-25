*          DATA SET SPSFM1F    AT LEVEL 067 AS OF 05/01/02                      
*PHASE T2171FA                                                                  
*INCLUDE KHDUMMY                                                                
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE        T2171F - SIR NSID MINI-CONTROLLER                     *         
*                                                                     *         
*  CALLED FROM  T21700 (SFM CONTROLLER)                               *         
*                                                                     *         
*  INPUTS                     PHASE     SCREEN                        *         
*               NSID          T2170E    T217BE                        *         
*               DETAIL        T2170F    T217BF                        *         
*               COMPETE       T21716    T217B6                        *         
*               ESTIMATE      T21717    T217B7                        *         
*               RANK          T21718    T217B8                        *         
*                                                                     *         
*  REGISTERS    R0 -- WORK                                            *         
*               R1 -- WORK                                            *         
*               R2 -- WORK                                            *         
*               R3 -- WORK                                            *         
*               R4 -- WORK                                            *         
*               R5 -- WORK                                            *         
*               R6 -- WORK                                            *         
*               R7 -- WORKD                                           *         
*               R8 -- WORK                                            *         
*               R9 -- SYSD                                            *         
*               RA -- TWA                                             *         
*               RB -- FIRST BASE                                      *         
*               RC -- GEND                                            *         
*               RD -- SYSTEM                                          *         
*               RE -- SYSTEM                                          *         
*               RF -- SYSTEM                                          *         
*                                                                     *         
*  COMMENTS                                                           *         
*                                                                     *         
*  THIS MINI-CONTROLLER IS LOADED BY T21700 WHEN SELECTS HAVE BEEN    *         
*  MADE FROM THE NSID MAINTENANCE SCREEN.  THE CONTROLLER LOADS THE   *         
*  APPROPRIATE PHASE TO PROCESS THE SELECT.  UPON RETURN, THE NSID    *         
*  PHASE AND SCREEN ARE RELOADED, AND THE NSID SCREEN IS MODIFIED TO  *         
*  REFLECT ANY CHANGES MADE DURING THE SELECT.                        *         
*                                                                     *         
***********************************************************************         
         TITLE 'T2171F - SIR NSID SELECT MINI-CONTROLLER'                       
T2171F   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,T2171F,RR=R3,CLEAR=YES                               
         LR    R7,RC                                                            
         USING WORKD,R7                                                         
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
*                                                                               
         TM    CONACTH+1,FATBPROT  ACTION FIELD MUST BE PROTECTED               
         BO    CHKDSKAD                                                         
         OI    CONACTH+6,FOUTCUR   USER TYPED 'SELECT' HIMSELF                  
         MVI   ERROR,INVACT        THAT'S A NO-NO                               
         GOTO1 ERREX                                                            
         B     XIT                                                              
*                                                                               
CHKDSKAD OC    SVDETADR,SVDETADR   THERE MUST BE A SAVED DISK ADDRESS           
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+14(4),SVDETADR  SAVED DETAIL RECORD DISK ADDRESS             
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC              PUT DETAIL RECORD IN IO2                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO1            RESTORE AIO                                  
*                                                                               
         L     RF,SYSPARMS                                                      
         L     RF,0(RF)            A(TIOB)                                      
         USING TIOBD,RF                                                         
         MVC   PFKNUM,TIOBAID      PF KEY NUMBER                                
         MVC   CURSDISP,TIOBCURD   DISP TO FIELD CONTAINING CURSOR              
         DROP  RF                                                               
*                                                                               
         ZIC   RE,PFKNUM                                                        
         CH    RE,=H'12'           TEST PF13-PF24                               
         BNH   *+8                 NO                                           
         SH    RE,=H'12'           YES - EQUATE TO PF1-PF12                     
         STC   RE,PFKNUM                                                        
*                                                                               
         CLC   =C'DET',CONREC      TEST DETAIL PROCESSING                       
         BNE   PF10                NO                                           
         CLI   SVUSECMP,C'Y'       TEST COMPETITION/ESTIMATE USER               
         BNE   PF5                 NO                                           
*                                                                               
         CLI   PFKNUM,1            TEST PF1 (COMPETE)                           
         BE    COMP10              YES                                          
         CLI   PFKNUM,2            TEST PF2 (ESTIMATE)                          
         BE    EST10               YES                                          
*                                                                               
PF5      CLI   PFKNUM,12           TEST PF12 (QUIT)                             
         BE    DETBYE              YES                                          
         MVI   PFKNUM,0                                                         
         B     DET10               TREAT LIKE AN ENTER                          
*                                                                               
PF10     CLI   SVUSECMP,C'Y'       TEST COMPETITION/ESTIMATE USER               
         BE    *+6                 YES                                          
         DC    H'0'                                                             
*                                                                               
         CLC   =C'COM',CONREC      TEST COMPETITION PROCESSING                  
         BNE   PF20                NO                                           
         CLI   PFKNUM,1            TEST PF1 (ESTIMATE)                          
         BE    ESTIMATE            YES                                          
         CLI   PFKNUM,2            TEST PF2 (DETAIL)                            
         BE    DETAIL              YES                                          
         CLI   PFKNUM,11           TEST PF11 (NEXT SELECTION)                   
         BE    COMPBYE             YES                                          
         CLI   PFKNUM,12           TEST PF12 (QUIT)                             
         BE    COMPBYE             YES                                          
         MVI   PFKNUM,0                                                         
         B     COMP20              TREAT LIKE AN ENTER                          
*                                                                               
PF20     CLC   =C'CES',CONREC      COMPETITIVE ESTIMATE PROCESSING?             
         BE    *+6                 YES                                          
         DC    H'0'                WE SHOULDN'T BE HERE                         
         CLI   PFKNUM,1            TEST PF1 (COMPETITION)                       
         BE    COMPETE             YES                                          
         CLI   PFKNUM,2            TEST PF2 (DETAIL)                            
         BE    DETAIL              YES                                          
         CLI   PFKNUM,11           TEST PF11 (NEXT SELECTION)                   
         BE    ESTBYE              YES                                          
         CLI   PFKNUM,12           TEST PF12 (QUIT)                             
         BE    ESTBYE              YES                                          
         MVI   PFKNUM,0                                                         
         B     EST15               TREAT LIKE AN ENTER                          
         EJECT                                                                  
DETAIL   LA    RE,SYSSPARE+1024    CLEAR APPLICATION STORAGE                    
         LH    RF,=Y(SYSDEND-SYSSPARE-1024)                                     
         XCEF                                                                   
*                                                                               
         XC    DMCB,DMCB                                                        
         LA    R3,CONTAGH                                                       
         ST    R3,DMCB                                                          
         MVC   DMCB+4(4),=X'D90217BF'                                           
         CLI   ALTPROG,0           ALTERNATE PHASE FOR SCREENS                  
         BE    *+10                                                             
         MVC   DMCB+6(1),ALTPROG                                                
         GOTO1 CALLOV,DMCB         LOAD IN DETAIL SCREEN                        
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   CONREC(8),=C'DETAIL  '                                           
         OI    CONRECH+6,FOUTTRN                                                
         MVC   CONACT(8),=C'SELECT  '                                           
         OI    CONACTH+6,FOUTTRN                                                
*                                                                               
DET10    XC    DMCB,DMCB                                                        
         L     R3,=V(DUMMY)                                                     
         A     R3,RELO                                                          
         ST    R3,DMCB                                                          
         MVC   DMCB+4(4),=X'D902170F'                                           
         GOTO1 CALLOV,DMCB         LOAD IN DETAIL VALIDATION PHASE              
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(RC)      PERFORM DETAIL SCREEN VALIDATION             
*                                                                               
         OC    CONHEAD,CONHEAD     TEST ANYTHING IN MESSAGE AREA                
         BNZ   XIT                 YES - AN ERROR WAS FOUND                     
         EJECT                                                                  
DETBYE   XC    DMCB,DMCB           RESTORE NSID SCREEN                          
         MVC   DMCB+10(2),2(RA)    TERMINAL NUMBER                              
         MVI   DMCB+8,4            PAGE NUMBER                                  
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'TEMPSTR',,(RA)                    
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   ACTNUM,ACTRANK                                                   
         CLC   =C'RAN',CONACT      TEST IF RANK WAS CALLER                      
         BE    RANKBYE             YES-GO RIGHT TO IT                           
*                                                                               
         LA    R2,SIRSELH          FIRST SELECT FIELD ON NSID SCREEN            
DET20    CLI   8(R2),C'*'          TEST ALREADY SELECTED                        
         BE    DET25               YES                                          
         CLI   8(R2),C' '          TEST THIS IS THE ONE                         
         BNH   DET25               NO, TRY NEXT                                 
         MVI   8(R2),C'*'          FLAG AS PROCESSED ON SCREEN                  
         B     DET40                                                            
*                                                                               
DET25    LA    R1,6                6 FIELDS PER ROW                             
DET30    ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   R1,DET30            BUMP TO NEXT SELECT                          
         B     DET20                                                            
*                                                                               
DET40    MVI   ACTNUM,ACTDIS       ASSUME WE CAME FROM DISPLAY                  
         CLC   =C'DIS',CONACT                                                   
         BE    DET50               RIGHT                                        
*                                                                               
         MVI   ACTNUM,ACTCHA       MAYBE IT'S THE CHANGE ACTION                 
         CLC   =C'CHA',CONACT                                                   
         BE    DET50               RIGHT                                        
*                                                                               
         MVI   ACTNUM,ACTSEL       BETTER BE A SELECT                           
         CLC   =C'SEL',CONACT                                                   
         BE    DET50               IT IS                                        
         DC    H'0'                WE'RE IN BIG TROUBLE, KIDS                   
*                                                                               
DET50    ZIC   R0,0(R2)            DAY                                          
         AR    R2,R0                                                            
         ZIC   R0,0(R2)            TIME                                         
         AR    R2,R0                                                            
*                                                                               
         ZIC   R0,0(R2)            PROGTYPE                                     
         AR    R2,R0                                                            
         CLI   SVPRGTYP,0                                                       
         BE    *+18                                                             
*                                                                               
         MVI   5(R2),1                                                          
         MVC   8(1,R2),SVPRGTYP                                                 
         B     *+12                                                             
         MVI   5(R2),0                                                          
         MVI   8(R2),0                                                          
*                                                                               
         ZIC   R0,0(R2)            COST                                         
         AR    R2,R0                                                            
         MVC   5(1,R2),SVCOST1L                                                 
         MVC   8(5,R2),SVCOST1                                                  
*                                                                               
         ZIC   R0,0(R2)            UPGRADE/OVERRIDE                             
         AR    R2,R0                                                            
         MVC   5(1,R2),SVUPGL                                                   
         MVC   8(40,R2),SVUPG                                                   
         B     BYE                                                              
         EJECT                                                                  
COMPETE  LR    RF,RA               A(TWA)                                       
         AH    RF,CURSDISP         A(SELECT FIELD)                              
         LA    R1,CONTAGH          START LOOKING HERE                           
         SR    R0,R0                                                            
*                                                                               
COMP5    LR    R2,R1               SAVE PREVIOUS FIELD ADDRESS                  
         IC    R0,0(R1)                                                         
         AR    R1,R0               BUMP TO NEXT FIELD                           
         CR    R1,RF               COMPARE WITH CURSOR POSITION                 
         BH    COMP7               CURSOR IS TOO HIGH UP ON SCREEN              
         BL    COMP5               CURSOR IS FURTHER DOWN                       
*                                                                               
         TM    1(R1),FATBXHDR      CURSOR IS HERE - TEST EXTENDED HDR           
         BZ    COMP7               NO - NOT A SELECT FIELD                      
         IC    R0,0(R1)                                                         
         AR    R1,R0               BUMP TO NEXT FIELD                           
         SH    R1,=H'8'            BACK UP TO BEGINNING OF EXTENSION            
         CLI   0(R1),66            TEST IT'S A SELECT FIELD                     
         BE    COMP9               YES                                          
*                                                                               
COMP7    LA    R1,CONTAGH          FIND THE FIRST SELECT FIELD                  
*                                                                               
COMP8    LR    R2,R1               SAVE PREVIOUS FIELD ADDRESS                  
         IC    R0,0(R1)                                                         
         AR    R1,R0               BUMP TO NEXT FIELD                           
         TM    1(R1),FATBXHDR      IS THE HEADER EXTENDED?                      
         BZ    COMP8               NO - NOT A SELECT FIELD                      
         IC    R0,0(R1)                                                         
         AR    R1,R0               BUMP TO NEXT FIELD                           
         SH    R1,=H'8'            BACK UP TO BEGINNING OF EXTENSION            
         CLI   0(R1),66            TEST IT'S A SELECT FIELD                     
         BE    COMP9               YES                                          
         LR    R1,R2               RESTORE R1                                   
         IC    R0,0(R1)                                                         
         AR    R1,R0               BUMP TO NEXT FIELD                           
         B     COMP8                                                            
*                                                                               
COMP9    MVC   FIELDLEN,7(R2)      OUTPUT LENGTH BECOMES INPUT LENGTH           
         CLI   FIELDLEN,0                                                       
         BE    COMP10              NOTHING TO MOVE IN                           
         ZIC   R1,FIELDLEN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FIELD(0),8(R2)      WE'LL PUT THIS ON THE COMPETE SCREEN         
*                                                                               
COMP10   LA    RE,SYSSPARE+1024    CLEAR APPLICATION STORAGE                    
         LH    RF,=Y(SYSDEND-SYSSPARE-1024)                                     
         XCEF                                                                   
*                                                                               
         XC    DMCB,DMCB                                                        
         LA    R3,CONTAGH                                                       
         ST    R3,DMCB                                                          
         MVC   DMCB+4(4),=X'D90217B6'                                           
         CLI   ALTPROG,0           ALTERNATE PHASE FOR SCREENS                  
         BE    *+10                                                             
         MVC   DMCB+6(1),ALTPROG                                                
         GOTO1 CALLOV,DMCB         LOAD IN COMPETITION SCREEN                   
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   CONREC(8),=C'COMPETE '                                           
         OI    CONRECH+6,FOUTTRN                                                
         MVC   CONACT(8),=C'SELECT  '                                           
         OI    CONACTH+6,FOUTTRN                                                
*                                                                               
         MVC   SRCDEM,FIELD        PUT DEMO IN COMPETITION KEY FIELD            
         OI    SRCDEMH+6,FOUTTRN                                                
         MVC   SRCDEMH+5(1),FIELDLEN   FUDGE INPUT LENGTH                       
*                                                                               
COMP20   XC    DMCB,DMCB                                                        
         L     R3,=V(DUMMY)                                                     
         A     R3,RELO                                                          
         ST    R3,DMCB                                                          
         MVC   DMCB+4(4),=X'D9021716'                                           
         GOTO1 CALLOV,DMCB         LOAD IN COMPETITION PHASE                    
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,DMCB             A(COMPETITION PHASE)                         
         MVI   MODE,VALKEY                                                      
         GOTO1 (RF),DMCB,(RC)      VALIDATE COMPETITION KEY                     
         MVI   MODE,DISPREC                                                     
         GOTO1 (RF),DMCB,(RC)      VALIDATE COMPETITION SCREEN                  
         B     XIT                                                              
         EJECT                                                                  
COMPBYE  XC    DMCB,DMCB           RESTORE NSID SCREEN                          
         MVC   DMCB+10(2),2(RA)    TERMINAL NUMBER                              
         MVI   DMCB+8,4            PAGE NUMBER                                  
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'TEMPSTR',,(RA)                    
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   ACTNUM,ACTRANK                                                   
         CLC   =C'RAN',CONACT      TEST IF RANK CALLED COMP                     
         BE    RANKBYE             YES                                          
*                                                                               
         LA    R2,SIRSELH          FIRST SELECT FIELD ON NSID SCREEN            
COMP30   CLI   8(R2),C'*'          TEST ALREADY SELECTED                        
         BE    COMP35              YES                                          
         CLI   8(R2),C' '          TEST THIS IS THE ONE                         
         BNH   COMP35              NO, TRY NEXT                                 
         MVI   8(R2),C'*'          FLAG AS PROCESSED ON SCREEN                  
         B     COMP50                                                           
*                                                                               
COMP35   LA    R1,6                6 FIELDS PER ROW                             
COMP40   ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   R1,COMP40           BUMP TO NEXT SELECT                          
         B     COMP30                                                           
*                                                                               
COMP50   MVI   ACTNUM,ACTDIS       ASSUME WE CAME FROM DISPLAY                  
         CLC   =C'DIS',CONACT                                                   
         BE    BYE                 RIGHT                                        
*                                                                               
         MVI   ACTNUM,ACTCHA       MAYBE IT'S THE CHANGE ACTION                 
         CLC   =C'CHA',CONACT                                                   
         BE    BYE                 RIGHT                                        
*                                                                               
         MVI   ACTNUM,ACTSEL       BETTER BE A SELECT                           
         CLC   =C'SEL',CONACT                                                   
         BE    BYE                 IT IS                                        
         DC    H'0'                WE'RE IN BIG TROUBLE, KIDS                   
         EJECT                                                                  
ESTIMATE XC    SVTRESTA,SVTRESTA   NO STATION SELECTED YET                      
*                                                                               
         LR    RF,RA               A(TWA)                                       
         AH    RF,CURSDISP         A(SELECT FIELD)                              
         LA    R1,CONTAGH          START LOOKING HERE                           
         SR    R0,R0                                                            
*                                                                               
EST3     LR    R2,R1               SAVE PREVIOUS FIELD ADDRESS                  
         IC    R0,0(R1)                                                         
         AR    R1,R0               BUMP TO NEXT FIELD                           
         CR    R1,RF               COMPARE WITH CURSOR POSITION                 
         BH    EST10               CURSOR IS TOO HIGH UP ON SCREEN              
         BL    EST3                CURSOR IS FURTHER DOWN                       
*                                                                               
         TM    1(R1),FATBXHDR      CURSOR IS HERE - TEST EXTENDED HDR           
         BZ    EST10               NO - NOT A SELECT FIELD                      
         IC    R0,0(R1)                                                         
         AR    R1,R0               BUMP TO NEXT FIELD                           
         SH    R1,=H'8'            BACK UP TO BEGINNING OF EXTENSION            
         CLI   0(R1),66            TEST IT'S A SELECT FIELD                     
         BNE   EST10               NO - USE DEFAULT                             
*                                                                               
         MVI   FIELDLEN,4          ASSUME 4-CHARACTER STATION                   
         CLI   11(R2),C'-'         TEST 3-CHARACTER STATION                     
         BH    *+12                NO - IT'S 4 CHARACTERS                       
         MVI   11(R2),C' '         REPLACE HYPHEN WITH SPACE                    
         MVI   FIELDLEN,3                                                       
         MVC   FIELD(4),8(R2)      WE'LL PUT THIS ON ESTIMATE SCREEN            
*                                                                               
         MVC   WORK(4),=C'F0F0F0F0' PUT ZEROES IN MARKET                        
         MVC   WORK+4(4),FIELD     STATION CALL LETTERS                         
         MVI   WORK+8,C'T'         ALWAYS TV FOR NOW                            
         GOTO1 MSPACK,DMCB,WORK,WORK+4,WORK+9                                   
         MVC   STATION,WORK+11     THE SELECTED STATION                         
*                                                                               
         L     R4,AIO2             A(DETAIL RECORD)                             
         USING SIRRECD,R4                                                       
         CLC   SIRKSTA,STATION     TEST THEY PICKED THE SAME STATION            
         BE    EST10               YES - DETAIL RECORD IS OK                    
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(13),SIRKEY      BUILD NEW NSID KEY                           
         LA    R4,KEY                                                           
         MVC   SIRKSTA,STATION     SELECTED STATION                             
         MVI   SIRKSEQ,0                                                        
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                LOOK FOR NSID OF SELECTED STATION            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   SIRKEY,KEYSAVE      TEST RECORD WAS FOUND                        
         BNE   EST7                NO                                           
*                                                                               
         L     R6,AIO2             DETAIL RECORD                                
         USING EDPELEM,R6                                                       
         MVI   ELCODE,EDPCODEQ     DAY/TIME ELEMENT                             
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DAYTIME,EDPDAY      SAVE DAY/TIME                                
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC              READ NEW NSID RECORD IN IO1                  
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,EDPCODEQ     DAY/TIME ELEMENT                             
         BAS   RE,GETEL            TEST ANY ELEMENTS                            
         BNE   EST7                NO                                           
*                                                                               
EST5     CLC   DAYTIME,EDPDAY      LOOK FOR THE GIVEN DAY/TIME                  
         BNE   *+18                                                             
         L     R4,AIO2                                                          
         MVC   SIRKSTA,STATION     PUT SELECTED STATION IN IO2                  
         B     EST10               WE HAVE A MATCH                              
*                                                                               
         BAS   RE,NEXTEL           KEEP LOOKING                                 
         BE    EST5                                                             
         DROP  R4,R6                                                            
*                                                                               
EST7     MVC   SVTRESTA,STATION    TELL ESTIMATE PROG THAT IO2 IS FAKE          
         EJECT                                                                  
EST10    LA    RE,SYSSPARE+1024    BEGINNING OF APPLICATION STORAGE             
         LH    RF,=Y(SYSDEND-SYSSPARE-1024)                                     
         XCEF                                                                   
*                                                                               
         XC    DMCB,DMCB                                                        
         LA    R3,CONTAGH                                                       
         ST    R3,DMCB                                                          
         MVC   DMCB+4(4),=X'D90217B7'                                           
         CLI   ALTPROG,0           ALTERNATE PHASE FOR SCREENS                  
         BE    *+10                                                             
         MVC   DMCB+6(1),ALTPROG                                                
         GOTO1 CALLOV,DMCB         LOAD IN ESTIMATE SCREEN                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   CONREC(8),=C'CEST    '                                           
         OI    CONRECH+6,FOUTTRN                                                
         MVC   CONACT(8),=C'SELECT  '                                           
         OI    CONACTH+6,FOUTTRN                                                
*                                                                               
         MVC   SRTSTA,FIELD        PUT STATION IN ESTIMATE KEY FIELD            
         OI    SRTSTAH+6,FOUTTRN                                                
         MVC   SRTSTAH+5(1),FIELDLEN    FUDGE INPUT LENGTH                      
*                                                                               
         OC    SVTRESTA,SVTRESTA   TEST IO2 IS FAKE                             
         BZ    EST15               NO                                           
         OI    SRTCSTH+1,FATBPROT  YES - PROTECT COST/UPGRADE FIELDS            
         NI    SRTCSTH+1,X'FF'-FATBMOD  UNMODIFY                                
         OI    SRTCSTH+6,FOUTTRN   XMIT                                         
         OI    SRTUPGH+1,FATBPROT                                               
         NI    SRTUPGH+1,X'FF'-FATBMOD                                          
         OI    SRTUPGH+6,FOUTTRN                                                
*                                                                               
EST15    XC    DMCB,DMCB                                                        
         L     R3,=V(DUMMY)                                                     
         A     R3,RELO                                                          
         ST    R3,DMCB                                                          
         MVC   DMCB+4(4),=X'D9021717'                                           
         GOTO1 CALLOV,DMCB         LOAD IN ESTIMATE PHASE                       
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,DMCB             A(ESTIMATE PHASE)                            
         MVI   MODE,VALKEY                                                      
         GOTO1 (RF),DMCB,(RC)      VALIDATE ESTIMATE KEY                        
         MVI   MODE,DISPREC                                                     
         GOTO1 (RF),DMCB,(RC)      VALIDATE ESTIMATE SCREEN                     
         B     XIT                                                              
         EJECT                                                                  
ESTBYE   XC    DMCB,DMCB           RESTORE NSID SCREEN                          
         MVC   DMCB+10(2),2(RA)    TERMINAL NUMBER                              
         MVI   DMCB+8,4            PAGE NUMBER                                  
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'TEMPSTR',,(RA)                    
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   ACTNUM,ACTRANK                                                   
         CLC   =C'RAN',CONACT                                                   
         BE    RANKBYE                                                          
*                                                                               
         LA    R2,SIRSELH          FIRST SELECT FIELD ON NSID SCREEN            
EST20    CLI   8(R2),C'*'          TEST ALREADY SELECTED                        
         BE    EST25               YES                                          
         CLI   8(R2),C' '          TEST THIS IS THE ONE                         
         BNH   EST25               NO, TRY NEXT                                 
         MVI   8(R2),C'*'          FLAG AS PROCESSED ON SCREEN                  
         B     EST40                                                            
*                                                                               
EST25    LA    R1,6                6 FIELDS PER ROW                             
EST30    ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   R1,EST30            BUMP TO NEXT SELECT                          
         B     EST20                                                            
*                                                                               
EST40    MVI   ACTNUM,ACTDIS       ASSUME WE CAME FROM DISPLAY                  
         CLC   =C'DIS',CONACT                                                   
         BE    EST50               RIGHT                                        
*                                                                               
         MVI   ACTNUM,ACTCHA       MAYBE IT'S THE CHANGE ACTION                 
         CLC   =C'CHA',CONACT                                                   
         BE    EST50               RIGHT                                        
*                                                                               
         MVI   ACTNUM,ACTSEL       BETTER BE A SELECT                           
         CLC   =C'SEL',CONACT                                                   
         BE    EST50               IT IS                                        
         DC    H'0'                WE'RE IN BIG TROUBLE, KIDS                   
*                                                                               
EST50    ZIC   R0,0(R2)            DAY                                          
         AR    R2,R0                                                            
         ZIC   R0,0(R2)            TIME                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)            PROGTYPE                                     
         AR    R2,R0                                                            
*                                                                               
         ZIC   R0,0(R2)            COST                                         
         AR    R2,R0                                                            
         MVC   5(1,R2),SVCOST1L                                                 
         MVC   8(5,R2),SVCOST1                                                  
*                                                                               
         ZIC   R0,0(R2)            UPGRADE                                      
         AR    R2,R0                                                            
         MVC   5(1,R2),SVUPGL                                                   
         MVC   8(40,R2),SVUPG                                                   
         EJECT                                                                  
BYE      MVC   SIRLAST+1(2),=X'0101' XMIT WHOLE SCREEN                          
         MVI   MODE,VALREC         SET UP GEND FOR NSID PHASE                   
*                                                                               
         L     R3,=V(DUMMY)                                                     
         A     R3,RELO                                                          
         ST    R3,DMCB                                                          
         MVC   DMCB+4(4),=X'D902170E'                                           
         GOTO1 CALLOV,DMCB         LOAD IN NSID PHASE                           
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   PFKNUM,12           TEST QUIT                                    
         BE    BYE10               YES                                          
         L     RF,DMCB             NO - SELECT NEXT                             
         GOTO1 (RF),DMCB,(RC)      CONTINUE NSID VALIDATION                     
         B     XIT                                                              
*                                                                               
BYE10    MVC   CONHEAD(32),=C'Hit enter to continue processing'                 
         OI    SIRSELH+6,FOUTCUR   POSITION CURSOR                              
         GOTO1 SAVEUWKA            SAVE SYSD                                    
*                                                                               
XIT      XIT1                                                                   
         SPACE 2                                                                
RANKBYE  MVC   RNKLAST+1(2),=X'0101' XMIT WHOLE SCREEN                          
         L     R3,=V(DUMMY)                                                     
         A     R3,RELO                                                          
         ST    R3,DMCB                                                          
         MVC   DMCB+4(4),=X'D9021718' LOAD IN RANK PHASE                        
         GOTO1 CALLOV,DMCB                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         MVI   MODE,VALREC         CONTINUE RANK EDIT                           
         GOTO1 (RF),DMCB,(RC)                                                   
         ICM   R2,15,ACURFORC      TEST CURSOR SET                              
         BZ    *+8                 NO-MORE CALLS ON SCREEN                      
         OI    6(R2),FOUTCUR       SET CURSOR BIT                               
         B     XIT                                                              
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         LTORG                                                                  
         SPACE 3                                                                
WORKD    DSECT                                                                  
RELO     DS    F                   RELOCATION FACTOR                            
CURSDISP DS    H                   DISPLACEMENT TO FIELD WITH CURSOR            
PFKNUM   DS    X                   ADJUSTED (1..12) PF KEY NUMBER               
FIELDLEN DS    X                   L'FIELD TO BE MOVED TO NEW SCREEN            
FIELD    DS    CL8                 FIELD TO BE MOVED TO NEW SCREEN              
STATION  DS    XL3                 CHOSEN STATION ON COMPETITION SCREEN         
DAYTIME  DS    XL5                 DAY AND TIME FROM DETAIL RECORD              
WORKX    EQU   *                                                                
         PRINT OFF                                                              
       ++INCLUDE FATIOB                                                         
       ++INCLUDE DDFLDHDR                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPSFMFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMBED                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMB6D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMB7D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMB8D                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE SPSFMWORKD                                                     
SIRRECD  DSECT                                                                  
       ++INCLUDE SPGENSIR                                                       
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'067SPSFM1F   05/01/02'                                      
         END                                                                    
