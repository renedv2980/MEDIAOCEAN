*          DATA SET DDDROOL    AT LEVEL 070 AS OF 05/01/02                      
*PHASE T00A8BA                                                                  
*INCLUDE RIGHT                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        DROOL -- ONLINE REPORT GENERATOR                     *         
*                                                                     *         
*  COMMENTS:     PROCESSES DPG PROGRAMS AND HOOKS TO APPLICATIONS     *         
*                AND SYSTEM DRIVER TO PRODUCE ONLINE REPORTS.         *         
*                                                                     *         
*  PARAMS:       P1:   A(GLOBALD)                                     *         
*                P2:   A(REPBLK)                                      *         
*                P3:   A(560-BYTE TSARD AREA)                         *         
*                P4:   A(6K TSARD BUFFER)                             *         
*                P5:   A(TWA FIELD HEADER) (OPTIONAL)                 *         
*                                                                     *         
*  OUTPUTS:      REPORT (IN SOME FORMAT)                              *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- WORK                                           *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- S P A R E  (CAN YOU BELIEVE IT?)               *         
*                R7 -- S P A R E  (CAN YOU BELIEVE IT?)               *         
*                R8 -- REPBLKD                                        *         
*                R9 -- SECOND BASE                                    *         
*                RA -- GLOBALD                                        *         
*                RB -- FIRST BASE                                     *         
*                RC -- LOCAL WORKING STORAGE                          *         
*                RD -- REGISTER SAVE AREA                             *         
*                RE -- WORK                                           *         
*                RF -- WORK                                           *         
*                                                                     *         
*  NOTE:         PROGRAM IS DIVIDED INTO SEGMENTS, EACH DOES AN NMOD. *         
*                **DRUL** -- OFFSET X'0000':  MAIN PROGRAM            *         
*                **DRL1** -- OFFSET X'0???':  MODE GLINIT             *         
*                **DRL2** -- OFFSET X'2000':  MODE GLINPUT            *         
*                **DRL3** -- OFFSET X'3000':  MODE GLOUTPUT           *         
*                **DRHK** -- OFFSET X'4???':  HOOK ROUTINE            *         
*                                                                     *         
***********************************************************************         
         TITLE 'DROOL - ONLINE REPORT GENERATOR - T00A8B'                       
DROOL    CSECT                                                                  
         PRINT NOGEN                                                            
         DS    (5*4096)X           SO WE DON'T KEEP FILLING UP THE HOLE         
         ORG   *-(5*4096)                                                       
*                                                                               
         NMOD1 0,**DRUL**,RR=R3                                                 
*                                                                               
         USING DROOLD,R1                                                        
         L     RA,DROGLOBL         A(DROOL GLOBALS)                             
         USING GLOBALD,RA                                                       
         ST    R1,ADRPARMS         A(PARAMETER LIST)                            
*                                                                               
         MVI   DROERROR,DROERROK   NO ERRORS (YET)                              
         SR    RF,RF                                                            
         ICM   RF,7,DROTSARD       A(TSAR DSECT AREA)                           
         ST    RF,ATSARD                                                        
         MVC   ATWAFLDH,DROTWAH    A(TWA FIELD HEADER)                          
         L     R8,DRORPBLK         A(REPBLK DSECT AREA)                         
         USING REPBLKD,R8                                                       
         DROP  R1                                                               
*                                                                               
         L     RF,=A(GOHOOK)                                                    
         AR    RF,R3                                                            
         ST    RF,AGOHOOK          A(USER HOOK ROUTINE)                         
         ST    R3,DRRELO                                                        
*                                                                               
         MVC   USERRD,4(RD)        SAVE USER'S RD FOR HOOKS                     
         CLI   GLMODE,GLINIT                                                    
         BE    INIT                INITIALIZE DROOL                             
         CLI   GLMODE,GLINPUT                                                   
         BE    INPUT               INPUT PHASE                                  
         CLI   GLMODE,GLOUTPUT                                                  
         BE    OUTPUT              OUTPUT PHASE                                 
         DC    H'0'                INVALID MODE SET BY APPLICATION              
         EJECT                                                                  
INIT     L     RF,=A(DOINIT)                                                    
         A     RF,DRRELO                                                        
         BASR  RE,RF               PERFORM INITIALIZATION PHASE                 
         B     GOODBYE                                                          
         SPACE 3                                                                
INPUT    CLI   GLANYERR,C'Y'       DON'T PROCESS IF PREVIOUS ERRORS             
         BE    XIT                                                              
         L     RF,=A(DOINPUT)                                                   
         A     RF,DRRELO                                                        
         BASR  RE,RF               PERFORM INPUT PHASE                          
         B     GOODBYE                                                          
         SPACE 3                                                                
OUTPUT   CLI   GLANYERR,C'Y'                                                    
         BE    XIT                                                              
         CLI   GLANYSRT,C'Y'       ANY RECORDS PUT TO TSAR?                     
         BE    *+12                                                             
         L     R2,GLAIO            THERE WERE NO RECORDS                        
         B     GOODBYE                                                          
         SPACE 2                                                                
         L     RF,=A(DOOUTPUT)                                                  
         A     RF,DRRELO                                                        
         BASR  RE,RF               GENERATE THE REPORT                          
GOODBYE  XIT1                                                                   
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
         DROP  RB                                                               
         SPACE 2                                                                
DOINIT   DS    0H                                                               
         SPACE 2                                                                
         NMOD1 WORKDX1-WORKD1,**DRL1**,R9,CLEAR=YES                             
         USING WORKD1,RC                                                        
*                                                                               
         BAS   RE,PRELIMS          INITIALIZE                                   
         BAS   RE,BLDDRIVE         BUILD DRIVE TABLE                            
         BAS   RE,DINTRECS         GET DETAILS OF RECORDS                       
         BAS   RE,FILLADDS         FILL IN ADDRESSES                            
         BAS   RE,OUTADDS          FILL IN OUTPUT POSITIONS                     
         BAS   RE,TSARINIT         INITIALIZE TSAR                              
*                                                                               
         OC    ATWAFLDH,ATWAFLDH   ON-SCREEN REPORT?                            
         BNZ   XIT                 YES                                          
         CLI   GLANYERR,C'Y'       ANY ERRORS FOUND?                            
         BNE   XIT                 NO                                           
*                                                                               
         MVI   REPACTN,REPACLOE    YES -- CLOSE REPORT WITH ERROR               
         GOTO1 REPORT,(R8)                                                      
         CLI   REPERRS,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ANSR                                                                   
         EJECT                                                                  
*              PRELIMINARY INITIALIZATION                                       
         SPACE 2                                                                
PRELIMS  NTR1                                                                   
*                                                                               
         L     R1,GLADTAB          A(DRIVE TABLE)                               
         LR    RF,R1                                                            
         A     RF,GLSIZE                                                        
         ST    RF,GLAEND           A(END OF DRIVE TABLE)                        
         MVC   0(8,R1),=C'*DRIVE**'                                             
         LA    R1,8(R1)                                                         
         ST    R1,GLADTAB          ACTUAL A(DRIVE TABLE)                        
*                                                                               
         MVI   GLANYERR,C'N'                                                    
         MVC   GLAPROG-8(8),=C'*GLOBAL*'                                        
         MVC   DRLLOCAL,=C'*LOCAL**'                                            
*                                                                               
         CLI   GLFHEADL,0          SET 1ST HEADLINE TO H9 IF NOT GIVEN          
         BNE   *+8                                                              
         MVI   GLFHEADL,9                                                       
         CLI   GLGAP,0             GAP TO 1                                     
         BNE   *+8                                                              
         MVI   GLGAP,1                                                          
         CLI   GLSPACE,0           DETAIL SPACING TO 1                          
         BNE   *+8                                                              
         MVI   GLSPACE,1                                                        
*                                                                               
         MVI   GLAROUT,2           GIVE FIRST HOOK TO SYSDRIVER                 
         MVI   GLHOOK,GLINIT                                                    
         L     RF,=A(GOHOOK)                                                    
         A     RF,DRRELO                                                        
         ST    RF,AGOHOOK                                                       
         GOTO1 AGOHOOK                                                          
*                                                                               
         LA    R1,GLOPTS           INTERNAL FILTER ADDRESSES                    
         ST    R1,GLAOPTS                                                       
         LA    R1,GLDATENO                                                      
         ST    R1,GLADTNO                                                       
         LA    R1,GLRECNO                                                       
         ST    R1,GLARECNO                                                      
         LA    R1,GLLEVEL                                                       
         ST    R1,GLALEVEL                                                      
         LA    R1,GLAGENCY                                                      
         ST    R1,GLAAGNCY                                                      
*                                                                               
         L     RF,GLCOMFAC         A(COMFACS)                                   
         USING COMFACSD,RF                                                      
         MVC   CALLOV,CCALLOV                                                   
         MVC   DATAMGR,CDATAMGR                                                 
         MVC   DATCON,CDATCON                                                   
         MVC   GETFACT,CGETFACT                                                 
         MVC   HEXIN,CHEXIN                                                     
         MVC   HEXOUT,CHEXOUT                                                   
         MVC   SWITCH,CSWITCH                                                   
         DROP  RF                                                               
*                                                                               
         XC    DMCB,DMCB           OBTAIN CORE-RESIDENT ADDRESSES               
         LA    R2,CORETAB                                                       
         LA    R0,CORES            COUNTER                                      
         LA    R4,COREADDS         POINT TO ADDRESS AREA                        
         L     RF,CALLOV                                                        
         LA    R1,DMCB                                                          
         MVC   DMCB+4(3),=X'D9000A'                                             
*                                                                               
PRE10    MVC   DMCB+7(1),0(R2)                                                  
         GOTO1 (RF),(R1),0                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   0(4,R4),DMCB        SAVE MODULE ADDRESS                          
         LA    R2,1(R2)            NEXT MODULE NUMBER                           
         LA    R4,4(R4)            NEXT ADDRESS                                 
         BCT   R0,PRE10                                                         
*                                                                               
         MVC   GLAEDITR,EDITOR                                                  
         L     RF,=V(RIGHT)                                                     
         A     RF,DRRELO                                                        
         ST    RF,RIGHT                                                         
*                                                                               
         MVI   REPACTN,REPAINI     INITIALIZE REPBLK                            
         GOTO1 REPORT,(R8)                                                      
         CLI   REPERRS,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OC    ATWAFLDH,ATWAFLDH   ON-SCREEN REPORT?                            
         BZ    PRE30               NO                                           
*                                                                               
         MVI   GLFHEADL,1          START HEADLINES AT TOP                       
         MVI   GLLFTOPT,C'Y'       ALWAYS LEFT-JUSTIFY THE REPORT               
*                                                                               
         L     RF,ATWAFLDH         A(FIRST REPORTING FIELD)                     
         ZIC   R1,0(RF)            FIELD LENGTH                                 
         SH    R1,=H'8'            MINUS 8 FOR HEADER                           
         TM    1(R1),X'02'         IS THERE AN EXTENDED HEADER?                 
         BZ    *+8                 NO                                           
         SH    R1,=H'8'                                                         
         ST    R1,GLWPAPER         LENGTH OF DATA FIELD IS MAX WIDTH            
*                                                                               
         BCTR  R1,0                FOR EX INSTRUCTION                           
         SR    R0,R0                                                            
PRE20    OI    6(RF),X'80'         XMIT EACH FIELD                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,RF),8(RF)       CLEAR EACH FIELD                             
         IC    R0,0(RF)                                                         
         AR    RF,R0               BUMP TO NEXT FIELD                           
         CLI   0(RF),1             ANY MORE FIELDS FOR REPORTING?               
         BH    PRE20               YES                                          
         B     PREX                                                             
*                                                                               
PRE30    MVI   REPACTN,REPAOPN     OPEN PRINT QUEUE REPORT                      
         GOTO1 REPORT,(R8)                                                      
         CLI   REPERRS,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PREX     B     XIT                                                              
         SPACE 2                                                                
CORETAB  DC    AL1(QTSAR)          SEQUENCE MUST MATCH LIST AT. . .             
         DC    AL1(QCENTER)        . . . LABEL COREADDS IN DROOLLOCAL           
         DC    AL1(QCHOPPER)                                                    
         DC    AL1(QUNDRLIN)                                                    
         DC    AL1(QREPORT)                                                     
         DC    AL1(QEDITOR)                                                     
CORES    EQU   (*-CORETAB)         NUMBER OF ENTRIES                            
         EJECT                                                                  
*              ROUTINES TO BUILD A DRIVE TABLE                                  
         SPACE 2                                                                
BLDDRIVE NTR1                                                                   
*                                                                               
         L     R2,GLAPROG          R2 = A(MAIN ELEMENTS)                        
         L     R3,GLADTAB          R3 = A(DRIVE TABLE)                          
         MVI   FILATOT,C'N'        INIT FIRST/LAST/TOTAL INDICATOR              
*                                                                               
BLDD2    CLI   0(R2),X'00'         CHECK END OF PROGRAM                         
         BE    XIT                                                              
         CLI   0(R2),X'FF'                                                      
         BE    XIT                                                              
         LA    R1,MAINLIST                                                      
*                                                                               
BLDD4    CLC   0(1,R1),0(R2)       LOOK UP ELEMENT CODE                         
         BE    BLDD6                                                            
         CLI   0(R1),X'FF'                                                      
         BE    BLDD8                                                            
         LA    R1,4(R1)                                                         
         B     BLDD4                                                            
*                                                                               
BLDD6    ST    R2,AMAINEL          MAIN ELEMENT FOUND - NOTE ADDRESS            
         XC    0(255,R3),0(R3)     PRECLEAR ELEMENT                             
         L     RF,0(R1)            PICK UP ADDRESS OF ROUTINE                   
         A     RF,DRRELO           . . .AND RELOCATE                            
         BASR  RE,RF               GO AND FILL IN DRIVE, ETC.                   
         CLI   1(R3),0             ANY LENGTH SET?                              
         BE    BLDD8                                                            
         MVC   0(1,R3),0(R2)       YES - SO COPY ELEMENT CODE                   
*                                                                               
BLDD7    ZIC   R1,1(R3)                                                         
         AR    R3,R1               R3 = A(CURRENT END OF DRIVE TABLE)           
         C     R3,GLAEND                                                        
         BNH   *+6                                                              
         DC    H'0'                NOT ENOUGH ROOM FOR DRIVE TABLE              
         CLI   0(R3),0             MAY HAVE ADDED OTHER ELEMENTS                
         BNE   BLDD7                                                            
*                                                                               
BLDD8    ZIC   R1,1(R2)            IGNORE UNKNOWN ELEMENTS                      
         LTR   R1,R1                (MIGHT BE FOR DRIVER, NOT DROOL)            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R1                                                            
         B     BLDD2                                                            
         EJECT                                                                  
*              MAIN ELEMENTS - REC AND DATA                                     
         SPACE 2                                                                
REC      NTR1                                                                   
*                                                                               
         USING DERECD,R2           RECORD START                                 
         USING DLRECD,R3                                                        
         MVI   RECYN,C'N'                                                       
         BAS   RE,CHKCOND          CHECK FOR CONDITIONS                         
         BNE   XIT                                                              
*                                                                               
         MVI   RECYN,C'Y'                                                       
         MVC   INDISP,=H'2'        CONTROL LENGTH                               
         MVI   DLRECLEN,DLRECLNQ                                                
         ZIC   R1,GLRECNO                                                       
         LA    R1,1(R1)                                                         
         STC   R1,GLRECNO                                                       
*                                                                               
         MVC   DLRECNUM,GLRECNO                                                 
         MVI   GLLEVEL,0                                                        
         XC    ALASTIN,ALASTIN                                                  
         XC    ALASTOUT,ALASTOUT                                                
         XC    ADICTENT,ADICTENT                                                
         XC    RANKCOL,RANKCOL                                                  
         MVI   KEYORDAT,C'K'       SET IN KEY                                   
         DROP  R2,R3                                                            
*                                                                               
         B     XIT                                                              
         SPACE 3                                                                
DATA     NTR1                                                                   
*                                                                               
         USING DEDATD,R2           DATA START                                   
         USING DLDATD,R3                                                        
*                                                                               
         CLI   RECYN,C'N'          IGNORE IF PREVIOUS REC FAILED                
         BE    XIT                                                              
*                                                                               
         MVI   DLDATLEN,DLDATLNQ                                                
         MVI   KEYORDAT,C'D'       SET IN DATA                                  
         LH    RF,INDISP                                                        
         LA    RF,2(RF)                                                         
         STH   RF,INDISP           TWO MORE CONTROL BYTES                       
         DROP  R2,R3                                                            
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*              MAIN ELEMENTS - SET (AND UNSET)                                  
         SPACE 2                                                                
SET      NTR1                                                                   
*                                                                               
         USING DESETD,R2           SET OR UNSET                                 
         MVI   ELCODE,0                                                         
         BAS   RE,GETSUB                                                        
         BE    SET2                                                             
         MVI   ERRNUM,1            MUST HAVE A SUB ELEMENT                      
         BAS   RE,ERROR                                                         
         B     SETX                                                             
*                                                                               
SET2     LA    R1,DICSAVE          DICTIONARY SAVE AREA                         
         L     R4,ASUBEL                                                        
*                                                                               
SET4     CLI   0(R1),0             FIND A SPOT IN DICSAVE                       
         BE    SET6                                                             
         CLC   0(1,R1),0(R4)       OR A PREVIOUS VERSION                        
         BE    SET6                                                             
         ZIC   R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     SET4                                                             
*                                                                               
SET6     CLI   DESETSW,0           SET                                          
         BE    SET8                                                             
         ZIC   RE,1(R4)            MOVE IN NEW ELEMENT                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(R4)                                                    
         B     SETX                                                             
*                                                                               
SET8     CLI   0(R1),0             UNSET                                        
         BNE   SET10                                                            
         MVI   ERRNUM,2            MUST BE PRECEDED BY SET                      
         BAS   RE,ERROR                                                         
         B     SETX                                                             
*                                                                               
SET10    ZIC   RE,1(R4)            MOVE IN NEW ELEMENT                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(R4)                                                    
         DROP  R2                                                               
*                                                                               
SETX     B     XIT                                                              
         EJECT                                                                  
*              MAIN ELEMENTS - IN                                               
         SPACE 2                                                                
IN       NTR1                                                                   
*                                                                               
         USING DEISD,R2            INPUT STATEMENT                              
         USING DLIND,R3                                                         
         MVI   INOROUT,C'I'                                                     
         MVI   INYN,C'N'                                                        
         CLI   RECYN,C'N'          DON'T NEED IN IF REC IS OFF                  
         BE    XIT                                                              
         BAS   RE,CHKCOND                                                       
         BNE   XIT                                                              
*                                                                               
         MVI   INYN,C'Y'                                                        
         CLI   DEISLEN,2           ANY LABEL ON THIS STATEMENT?                 
         BE    IN4                                                              
         MVC   DLINLABL,DEISLAB    YES -- SAVE IT                               
         CLC   DEISLAB,RANKCOL     IS THIS THE RANKED COLUMN?                   
         BNE   IN4                                                              
         OI    DLINFLAG,DLINRNKC   YES                                          
*                                                                               
IN4      ST    R3,ALASTIN                                                       
         XC    ALASTOUT,ALASTOUT                                                
         ZIC   R1,GLLEVEL          INCREMENT LEVEL NUMBER                       
         LA    R1,1(R1)                                                         
         STC   R1,GLLEVEL                                                       
*                                                                               
         MVC   DLINLEV,GLLEVEL     AND SAVE                                     
         XC    ADICTENT,ADICTENT                                                
         MVC   DICNAME,=CL8' '                                                  
         MVC   ENTNAME,=CL8' '                                                  
         BAS   RE,DICENT           CHECK FOR ANY ENTRY                          
*                                                                               
         MVI   ELCODE,X'22'        PICK UP INPUT TYPE                           
         BAS   RE,GETBEST                                                       
         BE    *+16                                                             
         MVI   ERRNUM,3                                                         
         BAS   RE,ERROR                                                         
         B     IN6                                                              
*                                                                               
         L     R4,ASUBEL                                                        
         USING DEITD,R4                                                         
         MVC   DLINTYPE,DEITYPE                                                 
         CLI   KEYORDAT,C'K'       IF WE ARE IN THE KEY                         
         BNE   *+16                                                             
         CLI   DLINTYPE+1,C'+'     PROTECT AGAINST ADDITIVE FIELDS              
         BNE   *+8                                                              
         MVI   DLINTYPE+1,C'-'                                                  
*                                                                               
         MVI   DLINREP,1           ASSUME NO REPETITION                         
         CLI   DEITLEN,4           IS IT THERE AT ALL?                          
         BE    IN6                 NO                                           
         MVC   DLINREP,DEITREP                                                  
         DROP  R4                                                               
*                                                                               
IN6      MVI   ELCODE,X'23'        INPUT LENGTH                                 
         BAS   RE,GETBEST                                                       
         BE    *+16                                                             
         MVI   ERRNUM,4                                                         
         BAS   RE,ERROR                                                         
         B     IN10                                                             
*                                                                               
         L     R4,ASUBEL                                                        
         USING DEILD,R4                                                         
         MVC   DLINFLEN,DEILEN                                                  
         ZIC   R1,DLINFLEN         FIELD LENGTH                                 
         ZIC   R0,DLINREP                                                       
         MR    R0,R0               TIMES REPITITION                             
         STC   R1,DLINLEN          =TOTAL INPUT LENGTH                          
         MVC   DLINDISP,INDISP                                                  
         ZIC   R1,DLINLEN                                                       
         AH    R1,INDISP           SET DISPLACEMENT FOR NEXT IN                 
         STH   R1,INDISP                                                        
         DROP  R4                                                               
*                                                                               
IN10     MVI   ELCODE,X'24'        ANY INPUT ROUTINE?                           
         MVI   INPUTRTN,C'N'                                                    
         BAS   RE,GETBEST                                                       
         BNE   IN12                                                             
         MVI   INPUTRTN,C'Y'                                                    
         L     R4,ASUBEL                                                        
         USING DEIRD,R4                                                         
         MVC   DLINROUT,DEIROUT                                                 
         MVC   GLLABEL,DEIROUT                                                  
         XC    GLARGS,GLARGS                                                    
         DROP  R4                                                               
*                                                                               
IN12     MVI   ELCODE,X'25'        ANY INPUT ARGUMENTS?                         
         BAS   RE,GETBEST                                                       
         BNE   IN13                                                             
         L     R4,ASUBEL                                                        
         USING DEIAD,R4                                                         
         ZIC   R1,DEIALEN                                                       
         SH    R1,=H'3'            2 FOR OVERHEAD, 1 FOR EX                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DLINARGS(0),DEIARGS                                              
         EX    R1,*+8              SET GLARGS FOR RESOLVE HOOK                  
         B     *+10                                                             
         MVC   GLARGS(0),DEIARGS                                                
         DROP  R4                                                               
*                                                                               
IN13     CLI   INPUTRTN,C'Y'                                                    
         BNE   *+14                                                             
         BAS   RE,RESOLVE                                                       
         MVC   DLINRADD,GLAROUT                                                 
         XC    GLARGS,GLARGS                                                    
*                                                                               
         MVI   ELCODE,X'27'        ANY RANKING COLUMN?                          
         BAS   RE,GETSUB                                                        
         BNE   IN30                                                             
         L     R4,ASUBEL                                                        
         USING DEICD,R4                                                         
         MVC   RANKCOL,DEICCOL                                                  
         DROP  R4                                                               
*                                                                               
IN30     MVI   ELCODE,X'87'        ANY LITERALS                                 
         SR    R1,R1                                                            
         BAS   RE,GETSUB                                                        
         BNE   INEND               NO                                           
*                                                                               
         L     R4,ASUBEL                                                        
         USING DELITD,R4                                                        
         ZIC   R1,DELITLEN         COMPUTE LENGTH OF LITERAL                    
         SH    R1,=H'4'            3 FOR OVERHEAD, 1 FOR EX                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DLINLIT(0),DELITRAL                                              
         LA    R1,1(R1)                                                         
         DROP  R4                                                               
*                                                                               
INEND    LA    R1,DLINLENQ(R1)     ADD ELEMENT LENGTH WITHOUT LITERAL           
         STC   R1,DLINELEN                                                      
         DROP  R2,R3                                                            
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*              MAIN ELEMENTS - OUT                                              
         SPACE 2                                                                
OUT      NTR1                                                                   
*                                                                               
         USING DEOSD,R2            OUTPUT STATEMENT                             
         USING DLOD,R3                                                          
         MVI   INOROUT,C'O'                                                     
         MVI   OUTYN,C'N'                                                       
         CLI   RECYN,C'N'          REJECT OUT IF PRECEDING REC FAILED           
         BE    XIT                                                              
         CLI   INYN,C'N'           REJECT OUT IF PRECEDING IN FAILED            
         MVI   INYN,C'Y'                                                        
         BE    XIT                                                              
         BAS   RE,CHKCOND                                                       
         BNE   XIT                                                              
*                                                                               
         ST    R3,ALASTOUT                                                      
         MVC   DLOIADD,ALASTIN     DEFAULT IS PREVIOUS IN                       
*                                                                               
         BAS   RE,DICENT           CHECK FOR ANY ENTRY                          
         MVC   ENTNAME,=CL8' '     (DON'T USE FOR NEXT OUT)                     
         MVI   OUTYN,C'Y'                                                       
         ICM   R1,15,DLOIADD                                                    
         BZ    *+10                                                             
         USING DLIND,R1                                                         
         MVC   DLOLEV,DLINLEV                                                   
         DROP  R1                                                               
*                                                                               
         XC    ALASTIN,ALASTIN                                                  
         MVI   ELCODE,X'32'        PICK UP OUTPUT TYPE                          
         BAS   RE,GETBEST                                                       
         BNE   *+14                                                             
         L     R4,ASUBEL                                                        
         USING DEOTD,R4                                                         
         MVC   DLOTYPE,DEOTYPE                                                  
         DROP  R4                                                               
*                                                                               
         MVI   ELCODE,X'33'        AND LENGTH                                   
         BAS   RE,GETBEST                                                       
         BNE   *+14                                                             
         L     R4,ASUBEL                                                        
         USING DEOLD,R4                                                         
         MVC   DLOLEN,DEOLEN                                                    
         DROP  R4                                                               
*                                                                               
         MVI   ELCODE,X'34'        ANY OUTPUT ROUTINE?                          
         BAS   RE,GETBEST                                                       
         BNE   OUT12                                                            
         L     R4,ASUBEL                                                        
         USING DEORD,R4                                                         
         MVC   DLOROUT,DEOROUT                                                  
         MVC   GLLABEL,DEOROUT                                                  
         BAS   RE,RESOLVE                                                       
         MVC   DLORADD,GLAROUT                                                  
         DROP  R4                                                               
*                                                                               
OUT12    MVI   ELCODE,X'35'        ANY OUTPUT ARGUMENTS?                        
         BAS   RE,GETBEST                                                       
         BNE   OUT14                                                            
         L     R4,ASUBEL                                                        
         USING DEOAD,R4                                                         
         ZIC   R1,DEOALEN                                                       
         SH    R1,=H'3'            2 FOR OVERHEAD, 1 FOR EX                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DLOARGS(0),DEOARGS                                               
         DROP  R4                                                               
*                                                                               
OUT14    MVI   ELCODE,X'36'        LOOK FOR OPTIONS                             
         BAS   RE,GETBEST                                                       
         B     OUT18                                                            
*                                                                               
OUT16    BAS   RE,NEXTSUB                                                       
*                                                                               
OUT18    BNE   OUT30                                                            
         L     R4,ASUBEL                                                        
         USING DEOOD,R4                                                         
         CLI   DEOONUM,1           DECIMAL PLACES                               
         BNE   *+10                                                             
         MVC   DLODEC,DEOOVAL                                                   
         CLI   DEOONUM,2           COMMAS=YES                                   
         BNE   *+8                                                              
         OI    DLOEDIT,X'80'                                                    
         CLI   DEOONUM,3           MINUS=YES                                    
         BNE   *+8                                                              
         OI    DLOEDIT,X'40'                                                    
         CLI   DEOONUM,4           ZERO=NOBLANK                                 
         BNE   *+8                                                              
         OI    DLOEDIT,X'20'                                                    
         CLI   DEOONUM,5           FILL=X                                       
         BNE   *+10                                                             
         MVC   DLOFILL,DEOOVAL                                                  
         CLI   DEOONUM,6           FLOAT=X                                      
         BNE   *+10                                                             
         MVC   DLOFLOAT,DEOOVAL                                                 
         CLI   DEOONUM,7           BRACKET(=M)                                  
         BNE   OUT20                                                            
         OI    DLOEDIT,X'10'                                                    
         CLI   DEOOVAL,C'M'                                                     
         BNE   OUT20                                                            
         OI    DLOEDIT,X'08'                                                    
*                                                                               
OUT20    CLI   DEOONUM,8           DIV=                                         
         BNE   *+10                                                             
         MVC   DLODIV,DEOOVAL                                                   
         CLI   DEOONUM,9           TRIM                                         
         BNE   *+8                                                              
         OI    DLOFORM,X'80'                                                    
         CLI   DEOONUM,10          BIG=BELOW                                    
         BNE   *+8                                                              
         OI    DLOFORM,X'40'                                                    
         CLI   DEOONUM,X'20'       ALIGN=                                       
         BNE   *+10                                                             
         MVC   DLOALIGN,DEOOVAL                                                 
         CLI   DEOONUM,X'21'       CENTER                                       
         BNE   *+8                                                              
         MVI   DLOALIGN,C'C'                                                    
         CLI   DEOONUM,X'22'       UNDER=X                                      
         BNE   OUT20B                                                           
         MVC   DLOUNDER,DEOOVAL                                                 
         CLI   DLOUNDER,0                                                       
         BNE   OUT20B                                                           
         MVI   DLOUNDER,X'BF'      DEFAULT IS HORIZONTAL                        
*                                                                               
OUT20B   CLI   DEOONUM,X'23'       NOBOX                                        
         BNE   *+8                                                              
         OI    DLOFORM,X'20'                                                    
         CLI   DEOONUM,X'24'       CHOP                                         
         BNE   *+8                                                              
         OI    DLOFORM,X'10'                                                    
         CLI   DEOONUM,X'25'       MARK                                         
         BNE   *+8                                                              
         OI    DLOFORM,X'04'                                                    
         CLI   DEOONUM,X'26'       FOLD                                         
         BNE   *+8                                                              
         OI    DLOFORM,X'02'                                                    
         CLI   DEOONUM,X'27'       SCALE                                        
         BNE   *+10                                                             
         MVC   DLOSCALE,DEOOVAL                                                 
         CLI   DEOONUM,X'28'       TRAIL                                        
         BNE   *+10                                                             
         MVC   DLOTRAIL,DEOOVAL                                                 
         B     OUT16                                                            
*                                                                               
OUT30    MVI   DLOLTYP,C'P'        PRESET OUTPUT                                
         MVI   DLOLINE,0           TO P*                                        
         MVI   DLOCOL,0                                                         
         DROP  R4                                                               
*                                                                               
         MVI   ELCODE,X'86'        LOOK FOR OUTPUT POSITION                     
         BAS   RE,GETSUB                                                        
         BNE   *+14                                                             
         L     R4,ASUBEL                                                        
         USING DEOPPD,R4                                                        
         MVC   DLOPOS,DEOPPLT                                                   
*                                                                               
         CLI   DLOLINE,1           SET RELATIVE LINE FOR HEADINGS               
         BH    *+12                                                             
         MVI   RELHEAD,1                                                        
         B     OUTLIT                                                           
*                                                                               
         CLI   DLOLINE,C'+'                                                     
         BE    *+14                                                             
         MVC   RELHEAD,DLOLINE                                                  
         B     OUTLIT                                                           
         DROP  R4                                                               
*                                                                               
         ZIC   R1,RELHEAD                                                       
         LA    R1,1(R1)                                                         
         STC   R1,RELHEAD                                                       
*                                                                               
OUTLIT   MVI   ELCODE,X'87'        ANY LITERALS                                 
         SR    R1,R1                                                            
         BAS   RE,GETSUB                                                        
         BNE   OUTEND                                                           
         L     R4,ASUBEL                                                        
         USING DELITD,R4                                                        
         ZIC   R1,DELITLEN                                                      
         SH    R1,=H'3'                                                         
         CLI   DLOLEN,0            IF LENGTH HAS NOT BEEN SPECIFIED             
         BNE   *+8                                                              
         STC   R1,DLOLEN           USE LITERAL LENGTH                           
         CLI   DLOTYPE,0           IF TYPE HAS NOT BEEN SPECIFIED               
         BNE   *+8                                                              
         MVI   DLOTYPE,C'C'        USE TYPE C                                   
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DLOLIT(0),DELITRAL                                               
         LA    R1,1(R1)                                                         
         DROP  R4                                                               
*                                                                               
OUTEND   LA    R1,DLODLENQ(R1)     ADD ELEMENT LENGTH WITHOUT LITERAL           
         STC   R1,DLOELEN                                                       
         BAS   RE,DICHEAD          CHECK FOR HEADINGS IN ENTRY                  
         CLI   DLOTYPE,0           TYPE SHOULD NOW BE ESTABLISHED               
         BNE   *+12                                                             
         MVI   ERRNUM,3                                                         
         BAS   RE,ERROR                                                         
*                                                                               
         CLI   DLOLEN,0            AND SO SHOULD LENGTH                         
         BNE   OUTX                                                             
         MVI   ERRNUM,4                                                         
         BAS   RE,ERROR                                                         
         DROP  R2                                                               
*                                                                               
OUTX     B     XIT                                                              
         EJECT                                                                  
*              IF HEADINGS ARE IN THE DICTIONARY ENTRY                          
*              PRESET HEAD ELEMENTS IN DRIVE TABLE                              
         SPACE 2                                                                
DICHEAD  NTR1                                                                   
*                                                                               
         ICM   R2,15,ADICTENT                                                   
         BZ    XIT                 NONE FOUND                                   
         CLI   DLOLTYP,C'P'        ONLY INTERESTED IF OUT IS IN P               
         BNE   XIT                                                              
*                                                                               
         LR    R4,R3                                                            
         ZIC   R1,DLOELEN                                                       
         AR    R4,R1               A(NEW HEAD ELEMENT IN DRIVE TABLE)           
         USING DLHDD,R4                                                         
*                                                                               
DICH10   CLI   0(R2),0             END OF RECORD?                               
         BE    XIT                                                              
*                                                                               
         CLI   0(R2),X'82'         ANY HEAD ROUTINE?                            
         BNE   DICH20                                                           
         USING DESRD,R2                                                         
         MVI   DLHDEL,X'40'        HEAD ELEMENT                                 
         MVI   DLHDELEN,DLHDLENQ   LENGTH WITHOUT LITERAL                       
         MVC   DLHDROUT,DESROUT                                                 
         MVC   GLLABEL,DESROUT                                                  
         BAS   RE,RESOLVE                                                       
         MVC   DLHDRADD,GLAROUT                                                 
         MVC   DLHDWDTH,DLOLEN     PICK UP WIDTH FROM OUT                       
         MVC   DLHDLINE,RELHEAD    LINE NUMBER FROM DISPLACED HEADING           
         B     DICH40                                                           
         DROP  R2                                                               
*                                                                               
DICH20   CLI   0(R2),X'83'         ANY HEAD ARGUMENTS?                          
         BNE   DICH30                                                           
         USING DESAD,R2                                                         
         ZIC   R1,DESALEN                                                       
         SH    R1,=H'3'            2 FOR OVERHEAD, 1 FOR EX                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DLHDARGS(0),DESARGS                                              
         B     DICH40                                                           
         DROP  R2                                                               
*                                                                               
DICH30   CLI   0(R2),X'87'         ANY HEAD LITERALS?                           
         BNE   DICH40                                                           
         USING DELITD,R2                                                        
         MVI   DLHDEL,X'40'                                                     
         MVC   DLHDWDTH,DLOLEN     PICK UP WIDTH FROM OUT                       
         ZIC   R1,DELITLIN                                                      
         ZIC   R0,RELHEAD                                                       
         AR    R1,R0               DISPLACED HEADING                            
         BCTR  R1,0                                                             
         STC   R1,DLHDLINE                                                      
         ZIC   R1,DELITLEN                                                      
         SH    R1,=H'4'            3 FOR OVERHEAD, 1 FOR EX                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DLHDLIT(0),DELITRAL                                              
         LA    R1,DLHDLENQ+1(R1)                                                
         STC   R1,DLHDELEN                                                      
         AR    R4,R1                                                            
         DROP  R2                                                               
*                                                                               
DICH40   ZIC   R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     DICH10                                                           
         DROP  R4                                                               
         EJECT                                                                  
*              MAIN ELEMENTS - HEAD                                             
         SPACE 2                                                                
HEAD     NTR1                                                                   
*                                                                               
         USING DEHDD,R2            HEAD STATEMENT                               
         USING DLHDD,R3                                                         
         CLI   OUTYN,C'N'                                                       
         BE    HEADEND             IGNORE IF PREVIOUS OUT FAILED                
         BAS   RE,CHKCOND                                                       
         BNE   XIT                                                              
*                                                                               
         MVI   ELCODE,X'82'        ANY HEAD ROUTINE?                            
         BAS   RE,GETSUB                                                        
         BNE   HED2                                                             
         L     R4,ASUBEL                                                        
         USING DESRD,R4                                                         
         MVC   DLHDROUT,DESROUT                                                 
         MVC   GLLABEL,DESROUT                                                  
         BAS   RE,RESOLVE                                                       
         MVC   DLHDRADD,GLAROUT                                                 
         DROP  R4                                                               
*                                                                               
HED2     MVI   ELCODE,X'83'        ANY HEAD ARGUMENTS?                          
         BAS   RE,GETSUB                                                        
         BNE   HED4                                                             
         L     R4,ASUBEL                                                        
         USING DESAD,R4                                                         
         ZIC   R1,DESALEN                                                       
         SH    R1,=H'3'            2 FOR OVERHEAD, 1 FOR EX                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DLHDARGS(0),DESARGS                                              
         DROP  R4                                                               
*                                                                               
HED4     MVC   DLHDLINE,DEHDLIN                                                 
         MVC   THISHLIN,DEHDLIN                                                 
         MVI   ELCODE,X'36'        LOOK FOR OPTIONS                             
         BAS   RE,GETBEST                                                       
         BNE   HED6                                                             
*                                                                               
HED5     L     R4,ASUBEL                                                        
         USING DEOOD,R4                                                         
         CLI   DEOONUM,X'20'       ALIGNMENT OPTION                             
         BNE   *+10                                                             
         MVC   DLHDALIN,DEOOVAL                                                 
         BAS   RE,NEXTSUB                                                       
         BE    HED5                                                             
         DROP  R4                                                               
*                                                                               
HED6     MVI   ELCODE,X'87'        ANY LITERALS                                 
         SR    R1,R1                                                            
         BAS   RE,GETSUB                                                        
         BNE   HED8                                                             
         L     R4,ASUBEL                                                        
         USING DELITD,R4                                                        
         ZIC   R1,DELITLEN                                                      
         SH    R1,=H'4'            3 FOR OVERHEAD, 1 FOR EX                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DLHDLIT(0),DELITRAL                                              
         LA    R1,1(R1)                                                         
         DROP  R4                                                               
*                                                                               
HED8     LA    R1,DLHDLENQ(R1)     ADD ELEMENT LENGTH WITHOUT LITERAL           
         STC   R1,DLHDELEN                                                      
         ICM   R4,15,ALASTOUT      FIND WHAT THIS RELATES TO                    
         BZ    HEADEND                                                          
         USING DLOD,R4                                                          
         MVC   DLHDWDTH,DLOLEN     PICK UP WIDTH FROM OUT                       
         DROP  R4                                                               
*                                                                               
*                                  R4=A(RELATED OUT ENTRY)                      
HED10    ZIC   R1,1(R4)            SEE IF THERE IS A PREVIOUS HEAD              
         AR    R4,R1               FOR THE SAME LINE                            
         CR    R4,R3                                                            
         BE    HEADEND                                                          
         CLI   0(R4),X'40'                                                      
         BNE   HED10                                                            
         DROP  R3                                                               
         USING DLHDD,R4                                                         
         CLC   DLHDLINE,THISHLIN                                                
         BNE   HED10                                                            
         OI    DLHDFLAG,DLHDFLDE   FOUND - SO DELETE                            
         DROP  R2,R4                                                            
*                                                                               
HEADEND  B     XIT                                                              
         EJECT                                                                  
*              MAIN ELEMENTS - FIRST LAST AND TOTAL                             
         SPACE 2                                                                
TOTAL    DS    0H                                                               
FIRST    NTR1                                                                   
*                                                                               
         USING DEFLD,R2                                                         
         USING DLFLD,R3                                                         
         CLI   RECYN,C'N'                                                       
         BE    XIT                 IGNORE IF PREVIOUS REC FAILED                
         CLI   OUTYN,C'N'                                                       
         BE    XIT                 IGNORE IF PREVIOUS OUT FAILED                
         BAS   RE,CHKCOND                                                       
         BNE   XIT                                                              
*                                                                               
         MVI   FILATOT,C'Y'        INDICATE FIRST/LAST/TOTAL                    
         CLI   DEFLEL,X'48'                                                     
         BE    *+10                                                             
         MVC   DLFLTYPE,DEFLIND    F OR L                                       
         MVI   ELCODE,X'82'        ANY SYSTEM ROUTINE?                          
         BAS   RE,GETBEST                                                       
         BNE   FIRST2                                                           
         L     R4,ASUBEL                                                        
         USING DESRD,R4                                                         
         MVC   DLFLROUT,DESROUT                                                 
         MVC   GLLABEL,DESROUT                                                  
         BAS   RE,RESOLVE                                                       
         MVC   DLFLRADD,GLAROUT                                                 
         DROP  R4                                                               
*                                                                               
FIRST2   MVI   ELCODE,X'83'        ANY ARGUMENTS?                               
         BAS   RE,GETSUB                                                        
         BNE   FIRST4                                                           
         L     R4,ASUBEL                                                        
         USING DESAD,R4                                                         
         ZIC   R1,DESALEN                                                       
         SH    R1,=H'3'            2 FOR OVERHEAD, 1 FOR EX                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DLFLARGS(0),DESARGS                                              
         DROP  R4                                                               
*                                                                               
FIRST4   MVI   ELCODE,X'86'        LOOK FOR  POSITION                           
         BAS   RE,GETSUB                                                        
         BNE   *+14                                                             
         L     R4,ASUBEL                                                        
         USING DEOPPD,R4                                                        
         MVC   DLFLPOS,DEOPPLT                                                  
         DROP  R4                                                               
*                                                                               
         MVI   ELCODE,X'49'        LOOK FOR TOTAL DETAIL ELEMENT                
         BAS   RE,GETSUB                                                        
         BNE   *+14                                                             
         L     R4,ASUBEL                                                        
         USING DEDETD,R4                                                        
         MVC   DLFLDET,DEDETAIL                                                 
         DROP  R4                                                               
*                                                                               
         MVI   ELCODE,X'87'        ANY LITERALS                                 
         SR    R1,R1                                                            
         BAS   RE,GETSUB                                                        
         BNE   FIRST8                                                           
         L     R4,ASUBEL                                                        
         USING DELITD,R4                                                        
         ZIC   R1,DELITLEN                                                      
         SH    R1,=H'4'            3 FOR OVERHEAD, 1 FOR EX                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DLFLLIT(0),DELITRAL                                              
         LA    R1,1(R1)                                                         
         DROP  R4                                                               
*                                                                               
FIRST8   LA    R1,DLFLLENQ(R1)     ADD ELEMENT LENGTH WITHOUT LITERAL           
         STC   R1,DLFLELEN                                                      
         MVI   ELCODE,X'46'        LOOK FOR INSTRUCTIONS                        
         BAS   RE,GETSUB                                                        
         B     FIRST12                                                          
*                                                                               
FIRST10  BAS   RE,NEXTSUB                                                       
*                                                                               
FIRST12  BNE   FIRST22                                                          
         L     R4,ASUBEL                                                        
         USING DECBD,R4                                                         
         CLI   DECBINS,2                                                        
         BL    FIRST14                                                          
         BE    FIRST16                                                          
         CLI   DECBINS,4                                                        
         BL    FIRST18                                                          
         BE    FIRST20                                                          
         DC    H'0'                                                             
*                                                                               
FIRST14  OI    DLFLOPT,DLFLOPSK    SKIP TO NEW PAGE                             
         B     FIRST10                                                          
*                                                                               
FIRST16  MVC   DLFLSPAC,DECBVAL    SPACE N LINES                                
         B     FIRST10                                                          
*                                                                               
FIRST18  OI    DLFLOPT,DLFLOPRE    RESET PAGE NUMBER                            
         B     FIRST10                                                          
*                                                                               
FIRST20  OI    DLFLOPT,DLFLOPRS    RESET SUB PAGE NUMBER                        
         B     FIRST10                                                          
         DROP  R4                                                               
*                                                                               
FIRST22  ICM   R4,15,ALASTOUT      FIND WHAT THIS RELATES TO                    
         BZ    FIRSTEND                                                         
         USING DLOD,R4                                                          
         CLI   DEFLEL,X'48'                                                     
         BE    FIRST24                                                          
         ZIC   R1,DLOFLFOL         INCREMENT CONNECTED OUT ENTRY                
         LA    R1,1(R1)                                                         
         STC   R1,DLOFLFOL                                                      
         B     FIRSTEND                                                         
*                                                                               
FIRST24  ZIC   R1,DLOTLFOL         INCREMENT CONNECTED OUT ENTRY                
         LA    R1,1(R1)                                                         
         STC   R1,DLOTLFOL                                                      
         DROP  R2,R3,R4                                                         
*                                                                               
FIRSTEND MVI   FILATOT,C'N'        RESET FIRST/LAST/TOTAL INDICATOR             
         B     XIT                                                              
         EJECT                                                                  
*              MAIN ELEMENTS - COMPUTATIONAL                                    
         SPACE 2                                                                
COMP     NTR1                                                                   
*                                                                               
         USING DLCMD,R3                                                         
         CLI   INOROUT,C'O'        CHECK WHETHER IN OR OUT CAME BEFORE          
         BNE   XIT                                                              
*                                                                               
         CLI   OUTYN,C'N'                                                       
         BE    COMPX               IGNORE IF PREVIOUS OUT FAILED                
*                                                                               
         USING DLOD,R1                                                          
         ICM   R1,15,ALASTOUT                                                   
         BZ    COMPERR                                                          
         ST    R3,DLOACOMP         SAVE A(COMPUTE DRIVE TABLE ENTRY)            
*????????XC    DLOIADD,DLOIADD     OUT DOESN'T NEED IN NOW                      
         DROP  R1                                                               
*                                                                               
         LA    R5,DLCMEXP          R5=A(EXPRESSION IN DRIVE TABLE)              
         MVI   ELCODE,X'52'        GO AND LOOK FOR FORMULA ELS.                 
         BAS   RE,GETSUB                                                        
         B     COMP4                                                            
*                                                                               
COMP2    BAS   RE,NEXTSUB                                                       
*                                                                               
COMP4    BNE   COMPEND                                                          
         ZIC   R1,DLCMNEXP         INCREMENT NUMBER OF EXPRESSIONS              
         LA    R1,1(R1)                                                         
         STC   R1,DLCMNEXP                                                      
*                                                                               
         L     R4,ASUBEL                                                        
         USING DEFRMD,R4                                                        
         DROP  R3                                                               
         USING DLCMEXP,R5                                                       
         MVC   DLCMOP,DEFRMOP                                                   
         MVC   DLCMSUB,DEFRMLEV                                                 
         MVC   DLCMTYPE,DEFRMON                                                 
         CLI   DEFRMON,1                                                        
         BNE   *+14                                                             
         MVC   DLCMLIT,DEFRMLIT                                                 
         B     *+10                                                             
         MVC   DLCMINLB,DEFRMLAB   OTHERWISE WE WILL NEED AN ADDRESS            
         DROP  R5                                                               
*                                                                               
         LA    R5,12(R5)                                                        
         B     COMP2                                                            
         DROP  R4                                                               
*                                                                               
         USING DLCMD,R3                                                         
COMPEND  ZIC   R1,DLCMNEXP         LENGTH = N'EXPRESSIONS                       
         MH    R1,=H'12'           *12                                          
         LA    R1,4(R1)            +4                                           
         STC   R1,DLCMELEN                                                      
         B     COMPX                                                            
         DROP  R3                                                               
*                                                                               
COMPERR  MVI   ERRNUM,7                                                         
         BAS   RE,ERROR                                                         
*                                                                               
COMPX    B     XIT                                                              
         EJECT                                                                  
*              MAIN ELEMENTS - CONDITIONS                                       
         SPACE 2                                                                
CON      NTR1                                                                   
*                                                                               
         XC    WORK,WORK                                                        
         USING DECOND,R2           CONDITION STATEMENT                          
         MVC   WORK(8),DECONLAB    CONDITION LABEL                              
         DROP  R2                                                               
*                                                                               
         SR    RF,RF               CONDITION COUNTER                            
         LA    R3,WORK+8           BUILD FAKE DRIVE TABLE ENTRIES               
         MVI   ELCODE,X'62'        LOOK FOR SUB-CONDITIONS                      
         BAS   RE,GETSUB                                                        
         B     CON20                                                            
*                                                                               
CON10    BAS   RE,NEXTSUB                                                       
*                                                                               
CON20    BNE   CON60               NO MORE SUB-CONDITIONS                       
         LA    RF,1(RF)            NOTE NUMBER OF CONDITIONS HERE               
         L     R4,ASUBEL                                                        
         USING DESCND,R4                                                        
         ZIC   R1,DESCNFLD         CHANGE FLD# TO ADDRESS                       
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         LA    R1,GLAOPTS(R1)                                                   
         L     R1,0(R1)                                                         
         ZIC   R0,DESCNSUB         (+DISPLACEMENT)                              
         LTR   R0,R0                                                            
         BZ    *+6                                                              
         BCTR  R0,0                                                             
         AR    R1,R0                                                            
         ST    R1,0(R3)            A(FIELD)                                     
         MVC   4(1,R3),DESCNCON    I=IF, A=AND, O=OR                            
         LA    R1,COPLIST                                                       
         SR    RE,RE                                                            
*                                                                               
CON30    CLC   DESCNOP,0(R1)       LOOK UP OP CODE                              
         BE    CON40                                                            
         LA    R1,2(R1)                                                         
         LA    RE,1(RE)                                                         
         CLI   0(R1),X'FF'                                                      
         BNE   CON30                                                            
         DC    H'0'                                                             
*                                                                               
CON40    STC   RE,5(R3)            OPERATOR NUMBER                              
         MVI   6(R3),1             LENGTH -- 1 BYTE                             
         MVC   7(1,R3),DESCNVAL+1  VALUE                                        
         CLI   DESCNVAL,0                                                       
         BE    CON50                                                            
         MVI   6(R3),2             LENGTH -- 2 BYTES                            
         MVC   7(2,R3),DESCNVAL    VALUE                                        
         CLI   DESCNVAL,C' '                                                    
         BH    CON50                                                            
         DC    H'0'                TRYING TO COMPARE TO ANOTHER FIELD           
*                                                                               
CON50    LA    R3,9(R3)            READY FOR NEXT SUB-CONDITION                 
         B     CON10                                                            
*                                                                               
CON60    LA    R3,WORK+8           BACK TO BEGINNING OF SUB-CONDITIONS          
*                                                                               
CON70    L     RE,0(R3)            RE=A(FIELD)                                  
         ZIC   R5,5(R3)            R5=OPERATOR NUMBER                           
         LA    R5,TRUTH(R5)                                                     
*                                                                               
         ZIC   R1,6(R3)            R1=L'COMP                                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),7(R3)       THE CONDITIONAL COMPARE                      
         BH    *+16                                                             
         LA    R5,6(R5)                                                         
         BE    *+8                                                              
         LA    R5,6(R5)                                                         
*                                                                               
         CLI   4(R3),C'I'          IF                                           
         BNE   *+14                                                             
         MVC   BYTE,0(R5)                                                       
         B     CON80                                                            
*                                                                               
         CLI   4(R3),C'O'          OR                                           
         BNE   *+20                                                             
         CLI   0(R5),C'Y'                                                       
         BNE   CON80                                                            
         MVI   BYTE,C'Y'                                                        
         B     CON90                                                            
*                                                                               
         CLI   0(R5),C'N'          AND                                          
         BNE   CON80                                                            
         MVI   BYTE,C'N'                                                        
         B     CON90                                                            
*                                                                               
CON80    LA    R3,9(R3)            EVALUATE NEXT SUB-CONDITION                  
         BCT   RF,CON70                                                         
*                                                                               
CON90    LA    R1,IFSAVE           TABLE OF SAVED CONDITIONS                    
         LA    R0,20               MAXIMUM NUMBER OF CONDITIONS                 
         CLI   0(R1),0             END OF TABLE?                                
         BE    *+14                YES -- WE'VE FOUND A SLOT                    
         LA    R1,9(R1)                                                         
         BCT   R0,*-12                                                          
         DC    H'0'                TOO MANY CONDITIONS                          
         MVC   0(8,R1),WORK        PUT LABEL INTO TABLE                         
         MVC   8(1,R1),BYTE        PUT CONDITIONAL RESULT INTO TABLE            
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
COPLIST  DC    C'EQGTLTNENLNH'                                                  
         DC    X'FF'                                                            
         SPACE 2                   VALUES FOR EQ GT LT NE NL NH                 
TRUTH    DC    C'NYNYYN'           IF TEST IS HIGH                              
         DC    C'YNNNYY'           IF TEST IS EQUAL                             
         DC    C'NNYYNY'           IF TEST IS LOW                               
         EJECT                                                                  
*              ROUTINE TO CHECK CONDITIONAL RESULTS                             
         SPACE 2                                                                
CHKCOND  NTR1                                                                   
*                                  R2 = A(MAIN ELEMENT)                         
         LA    R0,4                PRESET FOR UP TO 4 CONDITIONS                
         MVI   ELCODE,0                                                         
         BAS   RE,GETSUB                                                        
         B     CHKCOND4                                                         
*                                                                               
CHKCOND2 BAS   RE,NEXTSUB          FIND NEXT CONDITION                          
*                                                                               
CHKCOND4 BNE   YES                 NO MORE CONDITIONS                           
         L     R2,ASUBEL                                                        
         CLI   0(R2),X'84'         IS IT AN 'IF'?                               
         BE    *+12                                                             
         CLI   0(R2),X'85'         OR A 'NOT'?                                  
         BNE   CHKCOND2                                                         
*                                                                               
         USING DEIFD,R2                                                         
         LA    R1,IFSAVE           TABLE OF SAVED CONDITION RESULTS             
CHKCOND6 CLI   0(R1),0             END OF TABLE?                                
         BE    NO                  YES -- ITS A RUN-TIME (WE DON'T DO)          
         CLC   DEIFLABL,0(R1)                                                   
         BE    *+12                FOUND IT                                     
         LA    R1,9(R1)                                                         
         B     CHKCOND6                                                         
*                                                                               
         CLI   0(R2),X'84'         'IF' TEST?                                   
         BNE   *+16                NO                                           
         CLI   8(R1),C'Y'                                                       
         BNE   NO                                                               
         B     *+12                                                             
         CLI   8(R1),C'N'          IT'S A 'NOT' TEST                            
         BNE   NO                                                               
         DROP  R2                                                               
*                                                                               
         BCT   R0,CHKCOND2         NEXT CONDITION                               
         B     YES                 IF WE GET HERE, CONDITIONS PASSED            
         EJECT                                                                  
*              DEAL WITH POSSIBLE DICTIONARY AND ENTRY ELEMENTS                 
         SPACE 2                                                                
DICENT   NTR1                                                                   
*                                  R2=A(PRESENT MAIN ELEMENT)                   
*                                  RETURNING ADICTENT IF FOUND                  
*                                                                               
         MVI   ELCODE,X'80'        FIRST LOOK FOR A DICTIONARY NAME             
         BAS   RE,GETBEST                                                       
         BNE   *+14                                                             
         L     R4,ASUBEL                                                        
         USING DEDICD,R4                                                        
         MVC   DICNAME,DEDICNAM                                                 
*                                                                               
         MVI   ELCODE,X'81'        NOW TRY FOR ENTRY                            
         BAS   RE,GETBEST                                                       
         BNE   *+14                                                             
         L     R4,ASUBEL                                                        
         USING DEENTD,R4                                                        
         MVC   ENTNAME,DEENTNAM                                                 
*                                                                               
         CLC   ENTNAME,=CL8' '     GOT SOMETHING?                               
         BE    DICMISS2                                                         
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING DICKEY,R4                                                        
         MVI   DICSYS,DICSYSQ                                                   
         MVI   DICKTYP,DICKTYPQ                                                 
         MVC   DICCODE,DICNAME                                                  
         MVC   DICENTRY,ENTNAME                                                 
         BAS   RE,DICREAD                                                       
         BNE   DICMISS                                                          
         LA    R4,IO                                                            
         LA    R1,DICFIRST                                                      
         ST    R1,ADICTENT                                                      
         B     XIT                                                              
*                                                                               
DICMISS  L     R3,REPAPRNT         A(FIRST PRINT LINE)                          
         MVC   0(31,R3),=C'ERROR TRYING TO READ DICTIONARY'                     
         BAS   RE,PRINTERR                                                      
         MVC   0(8,R3),=C'DICNAME='                                             
         MVC   8(8,R3),DICNAME                                                  
         MVC   20(8,R3),=C'ENTNAME='                                            
         MVC   28(8,R3),ENTNAME                                                 
         BAS   RE,PRINTERR                                                      
*                                                                               
DICMISS2 XC    ADICTENT,ADICTENT                                                
         B     XIT                                                              
         EJECT                                                                  
*              I/O FOR DICTIONARY FILE                                          
         SPACE 2                                                                
DICREAD  NTR1                                                                   
*                                                                               
         GOTO1 GETFACT,DMCB,0      GET A(FACTSD)                                
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         IC    R0,FASYS            SAVE USER'S SYSTEM NUMBER                    
         DROP  R1                                                               
*                                                                               
         MVC   DUB,=X'0AFFFFFF00000000'                                         
         GOTO1 SWITCH,DUB                                                       
*                                                                               
         STC   R0,DUB              SET UP DUB TO RESTORE OLD SYSTEM             
         MVC   DUB+1(7),=X'FFFFFF00000000'                                      
         CLI   4(R1),0             WAS SWITCH TO CONTROL OK?                    
         BNE   DMNO                NO                                           
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'GENDIR',KEY,KEY                   
         CLI   DMCB+8,0                                                         
         BNE   DMNO                                                             
         GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),=C'GENFIL',DICDA,IO,DMWORK           
         CLI   DMCB+8,0                                                         
         BNE   DMNO                                                             
         DROP  R4                                                               
*                                                                               
         GOTO1 SWITCH,DUB                                                       
         CLI   4(R1),0             WAS SWITCH BACK OK?                          
         BE    YES                 YES                                          
         DC    H'0'                                                             
*                                                                               
DMNO     GOTO1 SWITCH,DUB                                                       
         CLI   4(R1),0             WAS SWITCH BACK OK?                          
         BE    NO                  YES                                          
         DC    H'0'                                                             
         EJECT                                                                  
*              RESOLVE ROUTINE LABELS INTO ROUTINE ADDRESSES                    
         SPACE 2                                                                
RESOLVE  NTR1                                                                   
*                                  GLLABEL HAS LABEL OF ROUTINE                 
*                                  RETURNING ADDRESS IN GLAROUT                 
         XC    GLAROUT,GLAROUT                                                  
         MVI   GLHOOK,GLRESOLV                                                  
*                                                                               
         MVI   GLAROUT,1           GO TO APPLICATION FIRST                      
         GOTO1 AGOHOOK                                                          
         MVI   GLAROUT,1                                                        
         OC    GLAROUT+1(3),GLAROUT+1     ANY HELP?                             
         BNZ   XIT                 YES                                          
*                                                                               
         MVI   GLAROUT,2           NOW TRY SYSTEM DRIVER                        
         GOTO1 AGOHOOK                                                          
         MVI   GLAROUT,2                                                        
         OC    GLAROUT+1(3),GLAROUT+1     ANY HELP?                             
         BNZ   XIT                 YES                                          
*                                                                               
         MVI   GLAROUT,3           NOW TRY DROOL ITSELF                         
         GOTO1 AGOHOOK                                                          
         MVI   GLAROUT,3                                                        
         OC    GLAROUT+1(3),GLAROUT+1     ANY HELP?                             
         BNZ   XIT                 YES                                          
*                                                                               
*                                  NO GOOD - WE HAVE A PROBLEM                  
*                                                                               
         L     RE,REPAPRNT         A(FIRST PRINT LINE)                          
         MVC   0(24,RE),=C'UNRESOLVED ROUTINE NAME '                            
         MVC   24(8,RE),GLLABEL                                                 
         BAS   RE,PRINTERR                                                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES TO FILL IN REC ELEMENTS WITH DETAILS                    
         SPACE 2                                                                
DINTRECS NTR1                                                                   
*                                                                               
         ZIC   R0,GLRECNO                                                       
         MVI   GLRECNO,1           THIS WILL HOLD CURRENT RECORD NUMBER         
         CH    R0,=H'4'                                                         
         BNH   DINT2                                                            
         LA    R0,4                                                             
         L     RE,REPAPRNT         A(FIRST PRINT LINE)                          
         MVC   0(29,RE),=C'MORE THAN 4 RECORDS SPECIFIED'                       
         BAS   RE,PRINTERR                                                      
*                                                                               
DINT2    L     R2,GLADTAB          A(DRIVE TABLE)                               
DINT4    CLI   0(R2),X'10'         LOOK FOR RECORD ELEMENT                      
         BNE   *+14                IN DRIVE TABLE                               
         USING DLRECD,R2                                                        
         CLC   GLRECNO,DLRECNUM    THAT MATCHES                                 
         BE    DINT6                                                            
*                                                                               
         CLI   0(R2),0                                                          
         BNE   *+6                                                              
         DC    H'0'                NO REC ELEMENT FOUND                         
         ZIC   R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     DINT4                                                            
*                                                                               
DINT6    BAS   RE,GETLENS          FIGURE OUT INPUT/OUTPUT LENGTHS              
*                                                                               
         ZIC   R1,GLRECNO          NEXT RECORD                                  
         LA    R1,1(R1)                                                         
         STC   R1,GLRECNO                                                       
         BCT   R0,DINT2                                                         
*                                                                               
         L     R2,GLADTAB          A(DRIVE TABLE)                               
DINT8    LH    R1,BGKEYLEN         LARGEST KEY LENGTH                           
         SH    R1,DLRKEYLN         LENGTH OF THIS RECORD KEY                    
         DROP  R2                                                               
*                                                                               
DINT10   IC    R0,1(R2)            R0 IS ZERO FROM BCT LOOP                     
         AR    R2,R0                                                            
         CLI   0(R2),X'12'         DATA ELEMENT?                                
         BNE   DINT10                                                           
*                                                                               
DINT12   IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0             END OF TABLE?                                
         BE    DINTX                                                            
         CLI   0(R2),X'10'         REC ELEMENT?                                 
         BE    DINT8                                                            
         CLI   0(R2),X'20'         IN ELEMENT?                                  
         BNE   DINT12                                                           
*                                                                               
         USING DLIND,R2                                                         
         LH    RF,DLINDISP         R1 = DIFFERENCE BETWEEN THIS RECORD          
         AR    RF,R1               . . . LENGTH AND LARGEST RECORD LEN          
         STH   RF,DLINDISP         ADJUST DISPLACEMENT OF COLUMN                
         B     DINT12                                                           
         DROP  R2                                                               
*                                                                               
DINTX    B     XIT                                                              
         EJECT                                                                  
*              FIGURE OUT LENGTHS OF KEY, DATA RECORDS AND PRINT                
         SPACE 2                                                                
GETLENS  NTR1                                                                   
*                                  R2=A(DRIVE TABLE ENTRY FOR RECORD)           
         MVI   LENTYPE,C'K'                                                     
         MVI   RANKFND,C'N'                                                     
         MVI   ANYFOLD,C'N'                                                     
         USING DLRECD,R3                                                        
         LR    R3,R2               A(REC ELEMENT)                               
*                                                                               
GTL2     ZIC   R1,1(R2)            GET AN ELEMENT                               
         AR    R2,R1                                                            
         CLI   0(R2),X'10'         HAVE WE GOT TO THE END                       
         BE    GTLEND                                                           
         CLI   0(R2),X'00'                                                      
         BE    GTLEND                                                           
         CLI   0(R2),X'FF'                                                      
         BE    GTLEND                                                           
         CLI   0(R2),X'12'                                                      
         BE    GTL12                                                            
         CLI   0(R2),X'20'                                                      
         BE    GTL20                                                            
         CLI   0(R2),X'30'                                                      
         BE    GTL30                                                            
         CLI   0(R2),X'48'                                                      
         BE    GTL48                                                            
         B     GTL2                                                             
*                                                                               
GTL12    MVI   LENTYPE,C'D'        DATA ELEMENT - RESET TYPE                    
         ZIC   R1,LASTILEV                                                      
         LA    R1,DLRTOTLV-1(R1)   SET LEVEL FOR DETAIL                         
         MVI   0(R1),2                                                          
         B     GTL2                                                             
*                                                                               
         USING DLIND,R2                                                         
GTL20    CLI   DLINTYPE+1,C'+'     CHECK FOR ADDITIVE FIELDS                    
         BNE   *+8                                                              
         OI    DLRECIND,X'80'      AND SET BIT ON                               
         ZIC   R1,DLINLEN          INPUT FIELD - PICK UP LENGTH                 
         LH    RE,DLRRECLN         ADD TO RECORD LENGTH                         
         AR    RE,R1                                                            
         STH   RE,DLRRECLN                                                      
         TM    DLINFLAG,DLINRNKC   IS THIS THE RANKED COLUMN?                   
         BZ    *+8                 NO                                           
         ST    R2,DLRARNKC         A(RANKED COLUMN)                             
         CLI   LENTYPE,C'K'        AND, IF WE'RE STILL IN THE KEY,              
         BNE   GTL2                                                             
*                                                                               
         MVC   DLRDETLV,DLINLEV    NOTE LEVEL NUMBER                            
         STC   R1,DLRDETLN         NOTE LEVEL LENGTH                            
         LH    RE,DLRKEYLN         ADD TO KEY LENGTH                            
         AR    RE,R1                                                            
         STH   RE,DLRKEYLN                                                      
         CLI   DLINLEV,1           AT LEVEL 1                                   
         BNE   *+8                                                              
         STC   R1,DLRLCBS          LENGTH=L'C/B 1                               
*                                                                               
         ZIC   RE,DLINLEV          PICK UP LEVEL NUMBER                         
         STC   RE,LASTILEV                                                      
         LA    RE,DLRLCBS-2(RE)                                                 
         ZIC   R0,0(RE)            PICK UP LENGTH OF PREVIOUS LEVEL             
         AR    R1,R0               ADD LENGTH OF THIS LEVEL                     
         STC   R1,1(RE)            AND SAVE                                     
         STC   R0,DLRDTDSP         NOTE DISPLACEMENT TO LEVEL                   
*                                                                               
         OC    DLRAFIN,DLRAFIN     NOTE ADDRESS OF FIRST IN                     
         BNZ   *+8                                                              
         ST    R2,DLRAFIN                                                       
         CLC   DLINROUT,=CL8'RANK' NOTE IN RELATED TO RANK                      
         BNE   GTL2                                                             
         ST    R2,DLRARNKR         A(RANKED ROW)                                
         MVC   DLRRNKLV,DLINLEV                                                 
         MVI   RANKFND,C'Y'                                                     
         B     GTL2                                                             
*                                                                               
         USING DLOD,R2                                                          
GTL30    CLI   DLOLTYP,C'P'        THIS STUFF IS ONLY FOR P LINES               
         BNE   GTL32                                                            
         TM    DLOFORM,X'02'       IS FOLD SET ON THIS                          
         BNO   *+8                                                              
         MVI   ANYFOLD,C'Y'                                                     
         CLI   ANYFOLD,C'Y'        IF PREVIOUS FOLD                             
         BE    GTL32               DON'T COUNT IN LENGTH                        
         CLI   DLOLINE,1           ONLY INTERESTED IN FIRST PRINT LINE          
         BH    GTL32               (ALSO CUTS OUT +)                            
         CLI   DLRCTWID,0          HAS CONTINUATION BEEN SET YET                
         BNE   GTL31                                                            
         CLI   DLOLEN,6            IS THIS WIDE ENOUGH                          
         BL    GTL31                                                            
         MVC   DLRCTDSP,DLRPWID    YES - SO SAVE DISPLACEMENT                   
         MVC   DLRCTWID,DLOLEN     AND WIDTH UP TO NOW                          
*                                                                               
GTL31    ZIC   R1,DLOLEN           OUTPUT ELEMENT - PICK UP LENGTH              
         ZIC   R0,DLRPWID                                                       
         AR    R1,R0                                                            
         ZIC   R0,GLGAP            ADD THIS +GAP TO PRINT WIDTH                 
         AR    R1,R0                                                            
         STC   R1,DLRPWID                                                       
         CLI   LENTYPE,C'K'        IF WE'RE STILL IN THE KEY                    
         BNE   GTL32                                                            
         CLI   RANKFND,C'Y'        UNLESS RANK IS ACTIVE                        
         BE    GTL32                                                            
         SR    R1,R1               NOTE PRINT DISP AT THIS LEVEL                
         ICM   R1,1,DLOLEV                                                      
         BZ    GTL32                                                            
         LA    R1,DLRPDCBS-1(R1)                                                
         MVC   0(1,R1),DLRPWID                                                  
*                                                                               
GTL32    CLI   DLOTLFOL,0          ANY TOTALS FOR THIS OUT?                     
         BE    GTL34                                                            
         ZIC   R1,DLOLEV           THEN NOTE LEVEL                              
         STC   R1,LASTOLEV                                                      
         LA    R1,DLRTOTLV-1(R1)   WHERE TOTALS ARE NEEDED                      
         MVI   0(R1),1                                                          
*                                                                               
GTL34    OC    DLRAFOUT,DLRAFOUT   NOTE ADDRESS OF FIRST OUT                    
         BNZ   *+8                                                              
         ST    R2,DLRAFOUT                                                      
         B     GTL2                                                             
*                                                                               
         USING DLFLD,R2                                                         
GTL48    CLI   DLFLDET,0           WAS DETAIL TOTAL LEVELS SPECIFIED            
         BE    GTL2                NO                                           
         ZIC   R1,LASTOLEV                                                      
         LA    R1,DLRTOTLV-1(R1)                                                
         ZIC   RE,DLFLDET                                                       
         LA    RE,16(RE)           SAVE N'LEVELS + 16                           
         STC   RE,0(R1)                                                         
         OI    DLRFLAGS,DLRTDETQ   TAKE A NOTE                                  
         B     GTL2                                                             
*                                                                               
GTLEND   ZIC   RE,DLRDETLV         CHECK THERE IS A TOTAL FOR THE LEVEL         
         BCTR  RE,0                ABOVE DETAIL LEVEL                           
         CLM   RE,1,LASTOLEV                                                    
         BNE   *+8                                                              
         OI    DLRFLAGS,DLRTTOTQ   YES - TAKE A NOTE                            
         LH    R1,DLRRECLN                                                      
         AH    R1,=H'2'            ADD IN CONTROL LENGTH                        
         STH   R1,DLRRECLN         TO TOTAL RECORD LENGTH                       
         SH    R1,DLRKEYLN                                                      
         STH   R1,DLRDATLN         RECLEN - KEYLEN = DATALEN                    
         CLC   DLRKEYLN,BGKEYLEN                                                
         BL    *+10                                                             
         MVC   BGKEYLEN,DLRKEYLN   UPDATE BIGGEST KEY FIELD                     
*                                                                               
         MVI   DLRPDISP,0                                                       
         LA    R1,132                                                           
         OC    GLWPAPER,GLWPAPER   OPTIONAL OVERRIDE PAPER WIDTH                
         BZ    *+8                                                              
         L     R1,GLWPAPER         (DROOL SETS THIS FOR ON-SCREEN RPTS)         
         ZIC   RE,DLRPWID                                                       
         SR    R1,RE                                                            
         BM    GTLEND2                                                          
         SRL   R1,1                CENTER REPORT ON PAGE                        
         CLI   GLLFTOPT,C'Y'       OPTION TO LEFT ALIGN                         
         BNE   *+6                                                              
         SR    R1,R1                                                            
         STC   R1,DLRPDISP                                                      
         DROP  R3                                                               
         B     XIT                                                              
*                                                                               
GTLEND2  L     RE,REPAPRNT         A(FIRST PRINT LINE)                          
         MVC   0(18,RE),=C'REPORT IS TOO WIDE'                                  
         BAS   RE,PRINTERR                                                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES TO FILL IN MISSING INTERNAL ADDRESSES                   
         SPACE 2                                                                
FILLADDS NTR1                                                                   
*                                                                               
         L     R2,GLADTAB          A(DRIVE TABLE)                               
         SR    R1,R1                                                            
*                                                                               
FILL2    CLI   0(R2),X'12'         LOOK FOR DATA                                
         BE    FILL4                                                            
         CLI   0(R2),0             END OF TABLE?                                
         BNE   *+6                                                              
         DC    H'0'                NO COLUMNS                                   
         IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     FILL2                                                            
*                                                                               
FILL4    ST    R2,FULL             SAVE A(DATA)                                 
*                                                                               
FILL6    CLI   0(R2),X'50'         LOOK FOR COMPUTES                            
         BE    FILL8                                                            
         CLI   0(R2),0             END OF TABLE?                                
         BE    FILLX                                                            
         CLI   0(R2),X'10'         NEW REC?                                     
         BE    FILL2                                                            
         IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     FILL6                                                            
*                                                                               
         USING DLCMD,R2                                                         
FILL8    ZIC   R0,DLCMNEXP                                                      
         LA    R2,DLCMEXP                                                       
         USING DLCMEXP,R2                                                       
*                                                                               
FILL10   CLI   DLCMTYPE,0          WE CAN IGNORE LITERALS                       
         BNE   FILL16                                                           
         L     R3,FULL             A(PREVIOUS DATA)                             
*                                                                               
FILL12   IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         CLI   0(R3),X'20'         IN?                                          
         BE    FILL14                                                           
         CLI   0(R3),X'10'         REC?                                         
         BE    *+12                                                             
         CLI   0(R3),0             END OF TABLE?                                
         BNE   FILL12                                                           
         L     RE,REPAPRNT         A(FIRST PRINT LINE)                          
         MVC   0(18,RE),=C'UNRESOLVED COMPUTE'                                  
         BAS   RE,PRINTERR                                                      
         B     XIT                                                              
*                                                                               
         USING DLIND,R3                                                         
FILL14   CLC   DLCMINLB,DLINLABL   MATCH ON LABEL?                              
         BNE   FILL12                                                           
         XC    DLCMINLB,DLCMINLB                                                
         ST    R3,DLCMINAD         SAVE RELATED IN ADDRESS                      
         DROP  R3                                                               
*                                                                               
FILL16   LA    R2,12(R2)           BUMP TO NEXT EXPRESSION                      
         BCT   R0,FILL10                                                        
         B     FILL6               LOOK FOR MORE COMPUTE ELEMENTS               
         DROP  R2                                                               
*                                                                               
FILLX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINES TO GENERATE OUTPUT ADDRESSES IN DRIVE TABLE             
         SPACE 2                                                                
OUTADDS  NTR1                                                                   
*                                                                               
         L     R3,GLADTAB          A(DRIVE TABLE)                               
*                                                                               
PAD2     CLI   0(R3),0                                                          
         BE    XIT                                                              
         CLI   0(R3),X'10'         LOOK FOR REC                                 
         BE    PAD10                                                            
         CLI   0(R3),X'30'         OUT                                          
         BE    PAD30                                                            
         CLI   0(R3),X'40'         HEAD                                         
         BE    PAD40                                                            
         CLI   0(R3),X'44'         FIRST/LAST                                   
         BE    PAD44                                                            
         CLI   0(R3),X'48'         TOTAL                                        
         BE    PAD44                                                            
         B     PADEND                                                           
*                                                                               
         USING DLRECD,R3                                                        
PAD10    ZIC   R1,DLRPDISP         SET UP SOFT COLUMN NUMBER FROM DISP.         
         LA    R1,2(R1)            . . . AND LEAVE DUMMY BOX COLUMN             
         STC   R1,SOFTCOL                                                       
         MVI   SOFTLINE,1                                                       
         MVI   WLASTOUT,0                                                       
         B     PADEND                                                           
         DROP  R3                                                               
*                                                                               
         USING DLOD,R3                                                          
PAD30    LA    R5,DLOAPOS                                                       
         MVI   SOFTHLIN,0          RESET HEADING NUMBER                         
         TM    DLOFORM,X'04'       WAS MARK SET FOR THIS OUT                    
         BNO   *+10                                                             
         MVC   MARKCOL,SOFTCOL                                                  
         TM    DLOFORM,X'02'       WAS FOLD SET FOR THIS OUT                    
         BNO   PAD30B                                                           
         MVC   SOFTCOL,MARKCOL     THEN BACK UP TO MARK COLUMN                  
         ZIC   R1,SOFTLINE                                                      
         LA    R1,1(R1)                                                         
         STC   R1,SOFTLINE                                                      
*                                                                               
PAD30B   GOTO1 FIGPADD,DMCB,DLOPOS,(R5)                                         
         CLI   DLOLTYP,C'P'        FOR PRINT OUTS...                            
         BNE   PADEND                                                           
         MVC   WLASTOUT,DLOLEN     SAVE WIDTH OF THIS FOR LATER                 
         B     PADEND                                                           
         DROP  R3                                                               
*                                                                               
         USING DLHDD,R3                                                         
PAD40    CLI   DLHDLINE,0          WAS SOFT HEADING INDICATED                   
         BNE   PAD40B                                                           
         ZIC   R1,SOFTHLIN                                                      
         LA    R1,1(R1)                                                         
         STC   R1,SOFTHLIN                                                      
         MVC   DLHDLINE,SOFTHLIN                                                
*                                                                               
PAD40B   ZIC   R1,GLFHEADL         NUMBER OF HEAD LINE                          
         ZIC   R0,DLHDLINE         PLUS RELATIVE HEADING NO                     
         AR    R1,R0                                                            
         ZIC   R0,SOFTLINE         PLUS ADJUSTMENT FOR FOLD, ETC.               
         AR    R1,R0                                                            
         SH    R1,=H'3'                                                         
         ZIC   RE,REPHEADN         NUMBER OF HEADLINES                          
         BCTR  RE,0                                                             
         CR    R1,RE                                                            
         BL    *+6                                                              
         LR    R1,RE                                                            
         M     R0,=F'132'                                                       
         L     RE,REPAHEAD                                                      
         AR    R1,RE                                                            
         ZIC   R0,SOFTCOL          PLUS SOFT COLUMN                             
         BCTR  R0,0                                                             
         AR    R1,R0                                                            
         ST    R1,DLHDAPOS                                                      
         B     PADEND                                                           
         DROP  R3                                                               
*                                                                               
         USING DLFLD,R3                                                         
PAD44    LA    R5,DLFLAPOS                                                      
         GOTO1 FIGPADD,DMCB,DLFLPOS,(R5)                                        
         DROP  R3                                                               
*                                                                               
PADEND   ZIC   R1,1(R3)            BUMP TO NEXT DRIVE TABLE ELEMENT             
         AR    R3,R1                                                            
         B     PAD2                                                             
         EJECT                                                                  
*              GENERATE OUTPUT ADDRESS FROM WHERE EXPRESSION                    
         SPACE 2                                                                
FIGPADD  NTR1                                                                   
*                                                                               
         L     R2,0(R1)                                                         
         L     R5,4(R1)                                                         
*                                  R2=A(PRINT EXPRESSION)                       
*                                  R3=A(DRIVE TABLE ENTRY)                      
*                                  R5=A(PRINT ADDRESS RETURNED)                 
*                                  SOFTLINE SOFTCOL SET TO PRESENT              
         L     R4,REPAHEAD                                                      
         CLI   0(R2),C'H'          POSITION R4 TO HEAD                          
         BE    FIGP2                                                            
         L     R4,REPAPRNT                                                      
         CLI   0(R2),C'P'          PRINT                                        
         BE    FIGP10                                                           
         CLI   0(R2),0                                                          
         BE    FIGP10                                                           
         SR    R4,R4                                                            
         CLI   0(R2),C'N'          OR NOT AT ALL                                
         BE    FIGPEND                                                          
         MVI   ERRNUM,5                                                         
         BAS   RE,ERROR                                                         
         B     XIT                                                              
*                                                                               
FIGP2    ZIC   R1,1(R2)            NOW PICK UP LINE NUMBER                      
         LTR   R1,R1                                                            
         BNZ   *+8                                                              
         LA    R1,1                                                             
         BCTR  R1,0                                                             
         M     R0,=F'132'                                                       
         AR    R4,R1                                                            
         ZIC   R1,2(R2)            AND COLUMN NUMBER                            
         AR    R4,R1                                                            
         BCTR  R4,0                                                             
         B     FIGPEND                                                          
*                                                                               
* HANDLE THE SPECIAL RULES FOR PRINT LINES                                      
*                                                                               
FIGP10   CLI   1(R2),C'+'          TEST LINE FOR +                              
         BE    FIGP12                                                           
         CLI   1(R2),0             TEST LINE SOFT                               
         BE    FIGP14                                                           
         B     FIGP16                                                           
*                                                                               
FIGP12   ZIC   R1,SOFTLINE         BUMP SOFTLINE 1 FOR +                        
         LA    R1,1(R1)                                                         
         STC   R1,SOFTLINE                                                      
         B     FIGP20                                                           
*                                                                               
FIGP14   ZIC   R1,SOFTCOL          FOR SOFT ADD WIDTH OF PREVIOUS               
         ZIC   R0,WLASTOUT                                                      
         LTR   R0,R0               DON'T ADD WIDTH IF FIRST OUT                 
         BZ    FIGP20                                                           
         CLI   0(R3),X'30'         AND ONLY IF IT IS AN OUT                     
         BNE   FIGP20                                                           
         AR    R1,R0                                                            
         ZIC   R0,GLGAP            +GAP                                         
         AR    R1,R0                                                            
         STC   R1,SOFTCOL                                                       
         B     FIGP20                                                           
*                                                                               
FIGP16   MVC   SOFTLINE,1(R2)      USE SPECIFIED LINE NUMBER                    
         CLI   2(R2),0             MAY USE SOFT COLUMN THOUGH                   
         BE    FIGP14                                                           
*                                                                               
FIGP20   CLI   2(R2),0             WAS SPECIFIC ROW SPECIFIED?                  
         BE    *+10                                                             
         MVC   SOFTCOL,2(R2)       THEN USE IT                                  
         ZIC   R1,SOFTLINE         NOW COMPUTE ADDRESS                          
         BCTR  R1,0                                                             
         MH    R1,=H'132'                                                       
         ZIC   R0,SOFTCOL                                                       
         BCTR  R0,0                                                             
         AR    R1,R0                                                            
         AR    R4,R1                                                            
*                                                                               
FIGPEND  ST    R4,0(R5)            RETURN COMPUTED ADDRESS                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO INITIALIZE TSAR                                       
         SPACE 2                                                                
TSARINIT NTR1                                                                   
*                                                                               
         MVI   GLANYSRT,C'N'                                                    
         L     RE,ATSARD           CLEAR TSAR BLOCK                             
         LA    RF,TSARDL                                                        
         XCEF                                                                   
*                                                                               
         L     R1,ATSARD           R1 = A(TSAR BLOCK)                           
         USING TSARD,R1                                                         
         L     R2,ADRPARMS                                                      
         USING DROOLD,R2                                                        
         MVC   TSABUF,DROTSBUF     A(6K CORE BUFFER)                            
         MVC   TSACOM,GLCOMFAC     A(COMFACS)                                   
         MVI   TSNBUF,1            NUMBER OF CORE BUFFERS                       
         MVI   TSPAGN,6            6 14K PAGES                                  
         OI    TSRECI,TSRVAR       RECORDS ARE VARIABLE LENGTH                  
         MVI   TSINDS,TSIALLOC+TSIRTNAF+TSINODSK+TSIXTTWA                       
         MVI   TSIND2,TSI2SCND     USE TSAR'S SECONDARY BUFFER                  
         MVC   TSKEYL,BGKEYLEN+1   MAXIMUM KEY LENGTH                           
         MVC   TSRECL,=H'256'      MAXIMUM RECORD LENGTH                        
*                                                                               
         OC    DROSAVE,DROSAVE     TEMPEST PREVIOUSLY RESERVED?                 
         BZ    TSARI10             NO                                           
*                                                                               
         MVC   TSPAGL,DROSAVE      LOW PAGE NUMBER                              
         MVC   TSPAGN,DROSAVE+1    NUMBER OF PAGES ALLOCATED                    
         MVC   TSINDS,DROSAVE+2    INDICATORS                                   
         OI    TSINDS,TSIREUSE+TSINODSK  RE-USE PREVIOUS ALLOCATION             
*                                                                               
TSARI10  MVI   TSACTN,TSAINI       INITIALIZE                                   
         GOTO1 TSAR                CALL TO INITIALIZE/RESTORE                   
         BE    TSARI20             SUCCESSFUL                                   
         TM    TSERRS,TSEALF       TEMPEST ALLOCATION FAILURE?                  
         BO    *+6                                                              
         DC    H'0'                NO -- FATAL ERROR                            
         L     RF,ADRPARMS                                                      
         MVI   DROERROR-DROOLD(RF),DROERRAF                                     
         MVI   GLANYERR,C'Y'                                                    
         B     TSARIX                                                           
*                                                                               
TSARI20  MVC   DROSAVE(1),TSPAGL   SAVE LOW TSAR PAGE NUMBER                    
         MVC   DROSAVE+1(1),TSPAGN SAVE NUMBER OF PAGES ALLOCATED               
         MVC   DROSAVE+2(1),TSINDS SAVE TEMPSTR/TEMPEST INDICATOR               
         NI    DROSAVE+2,TSIALLOC+TSIXTTWA                                      
*                                                                               
         DROP  R1,R2                                                            
TSARIX   B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO GET BEST SUBSIDIARY ELEMENT                           
         SPACE 2                                                                
GETBEST  NTR1                                                                   
*                                                                               
         BAS   RE,GETSUB           PROGRAM IS PREFERRED                         
         BE    YES                                                              
         CLI   FILATOT,C'Y'        FIRST/LAST/TOTAL?                            
         BE    NO                  YES-STOP HERE                                
*                                                                               
         L     R2,ADICTENT         MAYBE THERE WAS AN ENTRY                     
         BAS   RE,GB2                                                           
         BE    YES                                                              
*                                                                               
         LA    R2,DICSAVE          OR FROM SET DEFAULTS                         
         BAS   RE,GB2                                                           
         BNE   GB1                                                              
         ZIC   R1,1(R2)            WAS THERE ANY SIGNIFICANT DATA               
         SH    R1,=H'3'            2 FOR OVERHEAD, 1 FOR EX                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    2(0,R2),2(R2)                                                    
         BZ    NO                                                               
         B     YES                                                              
*                                                                               
GB1      LA    R2,DDEFAREA         FINALLY FROM DRIVER DEFAULTS                 
         BAS   RE,GB2                                                           
         BNE   NO                                                               
         B     YES                                                              
*                                                                               
GB2      NTR1                                                                   
*                                                                               
         LTR   R2,R2                                                            
         BZ    NO                                                               
*                                                                               
GB4      CLI   0(R2),0                                                          
         BE    GB8                                                              
         CLI   0(R2),X'FF'                                                      
         BE    GB8                                                              
         CLC   ELCODE,0(R2)                                                     
         BE    GB6                                                              
         ZIC   R1,1(R2)                                                         
         LTR   R1,R1                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R1                                                            
         B     GB4                                                              
*                                                                               
GB6      ST    R2,ASUBEL                                                        
         B     YES                                                              
*                                                                               
GB8      XC    ASUBEL,ASUBEL                                                    
         B     NO                                                               
         EJECT                                                                  
*              ROUTINE TO GET SUBSIDIARY ELEMENTS FROM PROGRAM                  
         SPACE 2                                                                
GETSUB   NTR1                                                                   
*                                                                               
         XC    ASUBEL,ASUBEL       R2 = A(MAIN ELEMENT)                         
         B     GSUB2                                                            
*                                                                               
NEXTSUB  NTR1                                                                   
*                                                                               
         L     R2,ASUBEL           R2 = A(MAIN ELEMENT)                         
         XC    ASUBEL,ASUBEL                                                    
*                                                                               
GSUB2    ZIC   R1,1(R2)                                                         
         LTR   R1,R1                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R1                                                            
         CLI   0(R2),0             CHECK FOR END OF PROGRAM                     
         BE    NO                                                               
         CLI   0(R2),X'FF'                                                      
         BE    NO                                                               
         LA    R1,MAINLIST                                                      
*                                                                               
GSUB4    CLC   0(1,R1),0(R2)       OR FOR A MAIN ELEMENT                        
         BE    NO                                                               
         LA    R1,4(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BNE   GSUB4                                                            
*                                                                               
         CLI   ELCODE,0            CHECK ELEMENT CODE IF NEEDED                 
         BE    *+14                                                             
         CLC   ELCODE,0(R2)                                                     
         BNE   GSUB2                                                            
         ST    R2,ASUBEL                                                        
         B     YES                                                              
         SPACE 2                                                                
MAINLIST DS    0F                                                               
         DC    X'10',AL3(REC)                                                   
         DC    X'12',AL3(DATA)                                                  
         DC    X'14',AL3(SET)                                                   
         DC    X'20',AL3(IN)                                                    
         DC    X'30',AL3(OUT)                                                   
         DC    X'40',AL3(HEAD)                                                  
         DC    X'44',AL3(FIRST)                                                 
         DC    X'48',AL3(TOTAL)                                                 
         DC    X'50',AL3(COMP)                                                  
         DC    X'60',AL3(CON)                                                   
         DC    X'FF'                                                            
         SPACE 2                                                                
DDEFAREA DC    X'2204',C'B+'       DEFAULT TO INPUT TYPE BINARY ADD             
         DC    X'230304'           DEFAULT TO INPUT LENGTH 4                    
         DC    X'FF'                                                            
         EJECT                                                                  
*              ERROR ROUTINES                                                   
         SPACE 2                                                                
ERROR    NTR1                                                                   
*                                                                               
         LA    R1,ERRLIST                                                       
*                                                                               
ERROR2   CLC   ERRNUM,0(R1)        LOOK UP FOR ERROR                            
         BE    ERROR4                                                           
         ZIC   R0,1(R1)                                                         
         AR    R1,R0                                                            
         CLI   0(R1),X'FF'                                                      
         BNE   ERROR2                                                           
         DC    H'0'                                                             
*                                                                               
ERROR4   L     RF,REPAPRNT         A(FIRST PRINT LINE)                          
         ZIC   RE,1(R1)            PICK IT OFF                                  
         SH    RE,=H'3'            2 FOR OVERHEAD, 1 FOR EX                     
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),2(R1)                                                    
         BAS   RE,PRINTERR                                                      
         B     XIT                                                              
*                                                                               
ERRLIST  DS    0H                                                               
         DC    AL1(1,22),CL20'SET NEEDS AN OPERAND'                             
         DC    AL1(2,18),CL16'UNSET BEFORE SET'                                 
         DC    AL1(3,14),CL12'MISSING TYPE'                                     
         DC    AL1(4,16),CL14'MISSING LENGTH'                                   
         DC    AL1(5,17),CL15'PRINT NOT H/P/T'                                  
         DC    AL1(6,24),CL22'MORE THAN 4 CONDITIONS'                           
         DC    AL1(7,17),CL15'NO OUT FOR COMP'                                  
         DC    X'FF'                                                            
         SPACE 3                                                                
PRINTERR NTR1                                                                   
*                                                                               
         OC    ATWAFLDH,ATWAFLDH   ON-SCREEN REPORT?                            
         BZ    *+6                 NO                                           
         DC    H'0'                                                             
*                                                                               
         MVI   GLANYERR,C'Y'                                                    
         MVI   REPACTN,REPAPUT     PRINT ERROR MESSAGE                          
         GOTO1 REPORT,(R8)                                                      
         CLI   REPERRS,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              WORKING STORAGE                                                  
         SPACE 2                                                                
WORKD1   DSECT                                                                  
*                                                                               
DMWORK   DS    12D                                                              
CALLOV   DS    V                                                                
DATAMGR  DS    V                                                                
GETFACT  DS    V                                                                
SWITCH   DS    V                                                                
ASUBEL   DS    A                                                                
AMAINEL  DS    A                                                                
ADICTENT DS    A                                                                
ALASTIN  DS    A                                                                
ALASTOUT DS    A                                                                
INDISP   DS    H                                                                
SOFTLINE DS    X                                                                
SOFTCOL  DS    X                                                                
WLASTOUT DS    X                                                                
MARKCOL  DS    X                                                                
SOFTHLIN DS    X                                                                
LENTYPE  DS    C                                                                
RELHEAD  DS    X                                                                
THISHLIN DS    X                                                                
KEYORDAT DS    C                                                                
LASTILEV DS    X                                                                
LASTOLEV DS    X                                                                
RECYN    DS    C                                                                
INYN     DS    C                                                                
OUTYN    DS    C                                                                
INOROUT  DS    C                                                                
FILATOT  DS    C                                                                
RANKFND  DS    C                                                                
ERRNUM   DS    X                                                                
DICNAME  DS    CL8                                                              
ENTNAME  DS    CL8                                                              
RANKCOL  DS    CL8                                                              
ANYFOLD  DS    C                                                                
INPUTRTN DS    C                                                                
KEY      DS    XL40                                                             
DICSAVE  DS    XL256               DICTIONARY SAVE AREA                         
IFSAVE   DS    XL181               ROOM FOR 20 CONDITION RESULTS                
*                                   8-BYTE LABEL AND 'Y' OR 'N'                 
*                                   FOLLOWED BY NULL FOR END OF TABLE           
IO       DS    2000X               FOR GENFIL DICTIONARY ENTRY RECORDS          
*                                                                               
WORKDX1  EQU   *                                                                
         EJECT                                                                  
         DROP  RC,RB,R9                                                         
         SPACE 2                                                                
DROOL    CSECT                                                                  
         SPACE 2                                                                
         DS    CL(8192-(*-DROOL))  FORCE MODULE TO OFFSET X'2000'               
         SPACE 2                                                                
DOINPUT  DS    0H                                                               
         SPACE 2                                                                
         NMOD1 WORKDX2-WORKD2,**DRL2**,CLEAR=YES                                
         USING WORKD2,RC                                                        
         ST    RD,SAVERD                                                        
*                                                                               
*              BROWSE DRIVE TABLE - REC STATEMENT                               
*                                                                               
         MVI   FIRSTSW,C'Y'                                                     
         MVI   GLRECNO,0                                                        
         ZAP   MAXREC,=P'0'                                                     
         ZAP   COUNT,=P'0'                                                      
         LA    R1,IO1                                                           
         ST    R1,GLAIO                                                         
*                                                                               
         L     R3,GLADTAB          R3=A(DRIVE TABLE)                            
*                                                                               
DIN2     CLI   0(R3),X'10'         LOOK FOR REC                                 
         BE    DIN10                                                            
         CLI   0(R3),X'12'         LOOK FOR DATA                                
         BE    DINEND                                                           
*                                                                               
         CLI   0(R3),X'20'         LOOK FOR IN                                  
         BE    DIN20               HOOK TO APPLICATION                          
         CLI   0(R3),0             END OF DRIVE TABLE?                          
         BNE   DINEND              NO                                           
*                                                                               
         CLI   RECYORN,C'N'                                                     
         BE    *+8                                                              
         BAS   RE,RECOUT           OUTPUT LAST RECORD                           
EXIT1    XIT1                                                                   
*                                                                               
DIN10    CLI   GLRECNO,0           UNLESS THIS IS THE FIRST                     
         BE    *+16                                                             
         CLI   RECYORN,C'N'        OR THIS REC REJECTED                         
         BE    *+8                                                              
         BAS   RE,RECOUT           OUTPUT RECORDS                               
*                                                                               
         ST    R3,GLATHID          SAVE A(CURRENT REC ELEMENT)                  
         ZIC   R1,GLRECNO                                                       
         LA    R1,1(R1)                                                         
         STC   R1,GLRECNO                                                       
*                                                                               
         MVI   RECYORN,C'Y'                                                     
         L     RE,GLAIO                                                         
         XC    0(256,RE),0(RE)                                                  
         L     R4,GLAIO                                                         
         MVC   0(1,R4),GLRECNO     EACH RECORD STARTS WITH NUMBER               
         B     DINEND                                                           
         EJECT                                                                  
*              DEAL WITH IN STATEMENTS IN DRIVE TABLE                           
         SPACE 2                                                                
DIN20    CLI   RECYORN,C'N'                                                     
         BE    DINEND                                                           
         USING DLIND,R3                                                         
         L     R4,GLAIO                                                         
         AH    R4,DLINDISP         A(INPUT FIELD)                               
*                                                                               
         CLI   DLINELEN,DLINLENQ   IS THERE A LITERAL?                          
         BE    DIN20B                                                           
         ZIC   R1,DLINELEN         YES                                          
         SH    R1,=Y(DLINLENQ)                                                  
         BCTR  R1,0                                                             
         ZIC   R0,DLINLEN          MAKE SURE FIELD IS LARGE ENOUGH              
         BCTR  R0,0                                                             
         CR    R1,R0                                                            
         BL    *+6                                                              
         LR    R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),DLINLIT                                                  
         B     DIN20X                                                           
*                                                                               
DIN20B   MVI   GLHOOK,GLROUT       BUILD HOOK FOR INPUT ROUTINE                 
         MVC   GLLABEL,DLINROUT                                                 
         MVC   GLAROUT,DLINRADD                                                 
         MVC   GLARGS,DLINARGS                                                  
         ST    R4,GLAIFLD                                                       
         ZIC   R1,DLINLEN          PRE CLEAR FIELD                              
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    0(0,R4),0(R4)                                                    
         ST    R3,GLADTENT                                                      
         GOTO1 AGOHOOK                                                          
*                                                                               
DIN20X   BAS   RE,NUMCHECK         CHECK FOR ACTIVE NUMERIC                     
*                                                                               
DINEND   ZIC   R1,1(R3)            BUMP TO NEXT DRIVE TABLE ENTRY               
         AR    R3,R1                                                            
         B     DIN2                                                             
         EJECT                                                                  
*              CHECK FOR ACTIVE NUMERIC FIELDS                                  
         SPACE 2                                                                
*                                  R3=A(DRIVE TABLE IN ELEMENT)                 
*                                  R4=A(DATA)                                   
*                                                                               
NUMCHECK NTR1                                                                   
*                                                                               
         CLI   DLINTYPE+1,C'+'                                                  
         BNE   EXIT1                                                            
         ZIC   R0,DLINREP                                                       
*                                                                               
NUCH2    ZIC   R1,DLINFLEN                                                      
         BCTR  R1,0                R1=L'DATA-1                                  
         LR    R5,R4               R5=A(THIS DATA)                              
         CLI   DLINTYPE,C'B'       ONLY BINARY ALLOWED                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    0(0,R5),0(R5)                                                    
         BZ    *+12                                                             
         MVI   ANYACTIV,2                                                       
         B     NUMCHX                                                           
*                                                                               
         CLI   ANYACTIV,0                                                       
         BNE   NUMCHX                                                           
         MVI   ANYACTIV,1                                                       
*                                                                               
NUMCHX   ZIC   R1,DLINFLEN                                                      
         AR    R4,R1                                                            
         BCT   R0,NUCH2                                                         
*                                                                               
         DROP  R3                                                               
         B     EXIT1                                                            
         EJECT                                                                  
*              ROUTINE TO WRITE OUT THE RECORDS                                 
         SPACE 2                                                                
RECOUT   NTR1                                                                   
*                                                                               
         TM    GLINDS,GLPALDET     OPTION TO GET INACTIVE RECORDS               
         BO    *+20                                                             
         CLI   ANYACTIV,1          IGNORE ZERO ACTIVITY RECORDS                 
         BNE   *+12                                                             
         MVI   ANYACTIV,0                                                       
         B     EXIT1                                                            
         MVI   ANYACTIV,0                                                       
*                                                                               
         L     R2,GLATHID                                                       
         USING DLRECD,R2                                                        
         MVI   GLHOOK,0                                                         
         L     RE,GLAIO                                                         
         MVC   IO2,0(RE)           SAVE THE RECORD                              
*                                                                               
         LA    R3,DLRTOTLV+11                                                   
         LA    R4,DLRLCBS+11                                                    
         LA    R0,12                                                            
*                                                                               
RECOUT6  L     R1,GLAIO            NOTE THE LEVEL NUMBER                        
         STC   R0,1(R1)                                                         
         STC   R0,TRIMLEV                                                       
         CLI   0(R3),X'10'         DETAILED TOTAL?                              
         BL    *+12                                                             
         BAS   RE,RECDET           YES                                          
         B     RECNEXT                                                          
         CLI   0(R3),1             SEE IF RECORD NEEDED AT THIS LEVEL           
         BL    RECNEXT             NO                                           
         BH    *+12                                                             
         BAS   RE,RECTOT           TOTAL RECORD AT THIS LEVEL                   
         B     RECNEXT                                                          
         TM    GLINDS,GLINODET     DETAIL REC - CHECK THAT IT'S NEEDED          
         BO    RECNEXT             NO                                           
         BAS   RE,RECPUT           YES - PUT DETAIL RECORD                      
*                                                                               
RECNEXT  LTR   R0,R0                                                            
         BZ    EXIT1                                                            
         BCTR  R3,0                                                             
         BCTR  R4,0                                                             
         BCTR  R0,0                                                             
         B     RECOUT6                                                          
         EJECT                                                                  
RECTOT   NTR1                                                                   
*                                                                               
         CLI   GLMAXTLV,0          OPTIONS TO LIMIT TOTAL RECORDS               
         BE    *+14                                                             
         CLC   TRIMLEV,GLMAXTLV                                                 
         BL    EXIT1                                                            
         CLI   GLMINTLV,0                                                       
         BE    *+14                                                             
         CLC   TRIMLEV,GLMINTLV                                                 
         BH    EXIT1                                                            
*                                                                               
         ZIC   R1,0(R4)            R1=L'CONTROL BREAK                           
         L     RE,GLAIO            RE=A(PIECE OF KEY TO CLEAR)                  
         LA    RE,2(RE,R1)         SKIP CONTROL BYTES                           
         LH    RF,DLRKEYLN         RF=L'PIECE TO BE CLEARED                     
         SR    RF,R1                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    0(0,RE),0(RE)                                                    
*                                                                               
         BAS   RE,RECPUT                                                        
*                                                                               
         B     EXIT1                                                            
         EJECT                                                                  
RECDET   NTR1                                                                   
*                                                                               
         CLI   GLHOOK,GLDONT       RECORD ALREADY REJECTED?                     
         BE    RDX                                                              
         CLI   GLMINTLV,0                                                       
         BE    *+14                                                             
         CLC   TRIMLEV,GLMINTLV                                                 
         BH    RDX                                                              
         ZIC   RF,DLRDETLV         CHECK CURRENT LEVEL LOW ENOUGH               
         BCTR  RF,0                FOR DETAILED TOTALS                          
         ZIC   RE,TRIMLEV                                                       
         SR    RF,RE                                                            
         BNP   RDX                                                              
         ZIC   R5,0(R3)            CHECK WE CAN SHOW THE DETAILS ASKED          
         SLL   R5,28               FOR                                          
         SRL   R5,28                                                            
         CR    RF,R5                                                            
         BL    RDX                                                              
         MVC   FULL,GLAIO          SAVE A(RECORD)                               
         B     RD04                                                             
*                                                                               
RD02     TM    DLRFLAGS,DLRTTOTQ   CHECK TOT AT LEVEL ABOVE DETAIL LEV          
         BZ    RD04                                                             
         ZIC   RE,1(R1)            YES - THEN AUTO GENERATE A REGULAR           
         BCTR  RE,0                      TOTAL RECORD                           
         CLI   GLMAXTLV,0                                                       
         BE    *+12                                                             
         CLM   RE,1,GLMAXTLV       OPTIONS TO LIMIT REGULAR TOTAL RECS          
         BL    RD04                                                             
         CLI   GLMINTLV,0                                                       
         BE    *+12                                                             
         CLM   RE,1,GLMINTLV                                                    
         BH    RD04                                                             
         STC   RE,1(R1)                                                         
         ZIC   RE,DLRDTDSP                                                      
         LA    RE,2(RE,R1)         SKIP OVER CONTROL BYTES                      
         ZIC   RF,DLRDETLN                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    0(0,RE),0(RE)                                                    
         ST    R1,GLAIO                                                         
         BAS   RE,RECPUT                                                        
*                                                                               
RD04     CLI   GLMAXTLV,0          OPTION TO LIMIT TOTALS?                      
         BE    RD05                                                             
         CLC   TRIMLEV,GLMAXTLV    YES- ARE DETAIL TOTS LIMITED?                
         BNL   RD05                                                             
         ZIC   RF,DLRDETLV         YES-                                         
         SR    RF,R5               RF=LAST LEVEL TO RECEIVE FF'S                
         CLM   RF,1,GLMAXTLV       WILL MAX TOTAL LEV RECEIVE FF'S?             
         BNL   RD08                YES-SO DO NOT GENERATE THIS TOTAL            
*                                                                               
RD05     MVC   IO3(0),IO2          BUILD RECORD IN IO3                          
         LA    R1,IO3              R1=A(RECORD)                                 
         MVC   1(1,R1),DLRDETLV                                                 
*                                                                               
         ZIC   RF,DLRDETLV         MOVE FF'S TO LEVELS BELOW THIS ONE           
         SR    RF,R5               WHICH ARE NOT DETAILED                       
         LA    RF,DLRLCBS-1(RF)                                                 
         ZIC   RE,0(RF)                                                         
         ZIC   RF,0(R4)                                                         
         SR    RE,RF                                                            
         BCTR  RE,0                                                             
         LA    RF,2(RF,R1)         SKIP OVER CONTROL BYTES                      
         MVI   0(RF),X'FF'                                                      
         LTR   RE,RE                                                            
         BNP   RD06                                                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   1(0,RF),0(RF)                                                    
*                                                                               
RD06     ST    R1,GLAIO            PUT DETAILED TOTAL RECORD                    
         BAS   RE,RECPUT                                                        
RD08     BCT   R5,RD02             AUTO GENERATE DETAILED TOTALS AT             
*                                  ALL LOWER LEVELS                             
         MVC   GLAIO,FULL          RESTORE A(RECORD)                            
         TM    DLRFLAGS,DLRTTOTQ                                                
         BZ    RDX                                                              
         BAS   RE,RECTOT           FINAL REGULAR TOTAL RECORD                   
*                                                                               
RDX      B     EXIT1                                                            
         EJECT                                                                  
*              ROUTINE TO PUT A RECORD TO TSAR                                  
         SPACE 2                                                                
RECPUT   NTR1                                                                   
*                                                                               
         L     R4,GLAIO                                                         
         LA    R3,2(R4)                                                         
         AH    R3,BGKEYLEN         COPY RECORD NUMBER AND LEVEL. . .            
         MVC   0(2,R3),0(R4)       . . . INTO DATA AREA                         
         LH    R1,DLRDATLN                                                      
         AH    R1,BGKEYLEN                                                      
         LA    R1,2(R1)            BUMP RECLEN BY 2 FOR VARIABLE LENGTH         
         STH   R1,0(R4)            TSAR VARIABLE RECORD LENGTH                  
*                                                                               
         MVI   GLHOOK,GLPUTSRT     TELL APPLICATION WE'RE WRITING               
         XC    GLAROUT,GLAROUT                                                  
         MVI   GLAROUT,1                                                        
         GOTO1 AGOHOOK                                                          
         CLI   GLHOOK,GLDONT       APPLICATION CAN REJECT                       
         BE    EXIT1                                                            
         MVI   GLHOOK,GLPUTSRT     TELL SYSTEM DRIVER AS WELL                   
         XC    GLAROUT,GLAROUT                                                  
         MVI   GLAROUT,2                                                        
         GOTO1 AGOHOOK                                                          
         CLI   GLHOOK,GLDONT       WHO CAN ALSO REJECT                          
         BE    EXIT1                                                            
*                                                                               
         CLI   FIRSTSW,C'Y'        FIRST TIME?                                  
         BNE   RP10                                                             
         MVI   FIRSTSW,C'N'                                                     
         SR    RE,RE                                                            
         ICM   RE,3,GLMAXREC       YES -- MAXIMUM RECORD COUNT?                 
         BZ    RP10                                                             
         CVD   RE,DUB              YES -- SET THE MAX COUNT                     
         ZAP   MAXREC,DUB                                                       
*                                                                               
RP10     CP    MAXREC,=P'0'        MAX INPUT RECORD COUNT?                      
         BE    *+14                                                             
         CP    COUNT,MAXREC        YES-HAVE WE GOT THERE YET ?                  
         BNL   EXIT1                                                            
         AP    COUNT,=P'1'                                                      
*                                                                               
         BAS   RE,TRACEIN          POSSIBLE INPUT TRACE                         
         MVI   GLANYSRT,C'Y'                                                    
         USING TSARD,R1                                                         
         L     R1,ATSARD                                                        
         MVC   TSAREC,GLAIO        A(RECORD)                                    
         MVI   TSACTN,TSAADD       ADD THE RECORD TO TSAR BUFFER                
         GOTO1 TSAR                                                             
         BE    RP40                RECORD WAS ADDED SUCCESSFULLY                
         TM    TSERRS,TSEDUP       DUPLICATE KEY?                               
         BO    RP30                YES -- MUST ACCUMULATE RECORD                
         TM    TSERRS,TSEEOF       NO MORE ROOM?                                
         BO    *+6                 RIGHT -- RETURN ERROR CODE                   
         DC    H'0'                DIE ON ANY OTHER ERROR                       
*                                                                               
         L     RF,ADRPARMS         SET ERROR FLAG IN DROOL PARAMETERS           
         MVI   DROERROR-DROOLD(RF),DROERREF                                     
         MVI   GLANYERR,C'Y'                                                    
         OC    ATWAFLDH,ATWAFLDH   ON-SCREEN REPORT?                            
         BNZ   RP20                YES                                          
         MVI   REPACTN,REPACLOE    NO -- CLOSE REPORT WITH ERROR                
         GOTO1 REPORT,(R8)                                                      
         CLI   REPERRS,0                                                        
         BE    RP20                                                             
         DC    H'0'                                                             
*                                                                               
RP20     L     RD,SAVERD           RETURN DIRECTLY TO APPLICATION               
         B     EXIT1                                                            
*                                                                               
RP30     MVC   IO4,0(R4)           SAVE RECORD                                  
         MVI   TSACTN,TSARDH       READ THE ACCUMULATED RECORD                  
         GOTO1 TSAR                                                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RF,IO4                                                           
         ST    RF,DUB              A(NEW RECORD)                                
         ST    R4,DUB+4            A(PRE-EXISTING RECORD)                       
         BAS   RE,ADDTWO           ADD THEM -- PUT RESULT IN GLAIO              
*                                                                               
         MVI   TSACTN,TSAWRT       WRITE THE RECORD TO TSAR BUFFER              
         GOTO1 TSAR                                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R1                                                               
         MVC   0(256,R4),IO4       RESTORE RECORD IN GLAIO                      
*                                                                               
RP40     MVC   0(2,R4),0(R3)       RESTORE RECNUM AND LEVEL IN GLAIO            
*                                                                               
         B     EXIT1                                                            
         EJECT                                                                  
*              ROUTINE TO ADD TWO TSAR RECORDS TOGETHER                         
         SPACE 2                                                                
ADDTWO   NTR1                                                                   
*                                                                               
         L     R5,DUB              A(NEW RECORD)                                
         L     R3,DUB+4            A(PRE-EXISTING RECORD)                       
         L     R2,DLRAFIN          A(FIRST IN ELEMENT)                          
         DROP  R2                                                               
*                                                                               
ADDTWO10 CLI   0(R2),X'20'         LOOK FOR IN FIELDS                           
         BNE   ADDTWO70                                                         
         USING DLIND,R2                                                         
         CLI   DLINTYPE+1,C'+'     ONLY INTERESTED IN ADDITIVE FIELDS           
         BNE   ADDTWO70                                                         
*                                                                               
         L     R5,DUB                                                           
         AH    R5,DLINDISP         DISPLACE TO THIS FIELD IN BOTH RECS          
         L     R3,DUB+4                                                         
         AH    R3,DLINDISP                                                      
         ZIC   R1,DLINFLEN         INDIVIDUAL FIELD LENGTH                      
         ZIC   R0,DLINREP          REPETITION FACTOR                            
*                                                                               
ADDTWO20 CLI   DLINFLEN,1          SINGLE-BYTE FIELD?                           
         BNE   ADDTWO30                                                         
         ZIC   RF,0(R5)                                                         
         ZIC   R4,0(R3)                                                         
         AR    RF,R4                                                            
         STC   RF,0(R3)                                                         
         B     ADDTWO60                                                         
*                                                                               
ADDTWO30 CLI   DLINFLEN,2          HALFWORD FIELD?                              
         BNE   ADDTWO40                                                         
         LH    RF,0(R5)                                                         
         AH    RF,0(R3)                                                         
         STH   RF,0(R3)                                                         
         B     ADDTWO60                                                         
*                                                                               
ADDTWO40 CLI   DLINFLEN,3          3-BYTE FIELD?                                
         BNE   ADDTWO50                                                         
         SR    RF,RF                                                            
         ICM   RF,7,0(R5)                                                       
         SR    R4,R4                                                            
         ICM   R4,7,0(R3)                                                       
         AR    RF,R4                                                            
         STCM  RF,7,0(R3)                                                       
         B     ADDTWO60                                                         
*                                                                               
ADDTWO50 CLI   DLINFLEN,4          FULLWORD FIELD?                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R5)                                                         
         A     RF,0(R3)                                                         
         ST    RF,0(R3)                                                         
*                                                                               
ADDTWO60 AR    R5,R1               BUMP TO NEXT REPEATED FIELD                  
         AR    R3,R1                                                            
         BCT   R0,ADDTWO20                                                      
*                                                                               
ADDTWO70 ZIC   R1,1(R2)            BUMP TO NEXT DRIVE TABLE ELEMENT             
         AR    R2,R1                                                            
         CLI   0(R2),X'00'                                                      
         BE    ADDTWOX             END OF DRIVE TABLE                           
         CLI   0(R2),X'10'                                                      
         BE    ADDTWOX             NEW RECORD                                   
         B     ADDTWO10            STILL IN SAME RECORD                         
         DROP  R2                                                               
*                                                                               
ADDTWOX  B     EXIT1                                                            
         EJECT                                                                  
*              ROUTINE TO TRACE INPUT RECORDS                                   
         SPACE 2                                                                
TRACEIN  NTR1                                                                   
*                                                                               
         CLI   GLTRACE,C'Y'                                                     
         BNE   EXIT1                                                            
         OC    ATWAFLDH,ATWAFLDH   ON-SCREEN REPORT?                            
         BNZ   EXIT1               YES -- NO TRACE POSSIBLE                     
*                                                                               
         L     R2,GLAIO                                                         
         L     R3,REPAPRNT                                                      
         MVC   0(132,R3),0(R2)                                                  
         LA    R3,132(R3)                                                       
         GOTO1 HEXOUT,DMCB,(R2),(R3),132,=C'SEP'                                
         OC    DMCB+16(4),DMCB+16                                               
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVI   REPACTN,REPAPUT     PRINT TRACE LINE                             
         GOTO1 REPORT,(R8)                                                      
         CLI   REPERRS,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 REPORT,(R8)         SKIP A LINE                                  
         CLI   REPERRS,0                                                        
         BE    EXIT1                                                            
         DC    H'0'                                                             
         EJECT                                                                  
         LTORG                                                                  
         SPACE 3                                                                
*              WORKING STORAGE                                                  
         SPACE 2                                                                
WORKD2   DSECT                                                                  
*                                                                               
SAVERD   DS    F                   SAVE CALLER'S RD                             
RECYORN  DS    C                                                                
ANYACTIV DS    X                                                                
FIRSTSW  DS    C                                                                
TRIMLEV  DS    X                                                                
MAXREC   DS    PL3                                                              
COUNT    DS    PL8                                                              
IO1      DS    XL256               BUILD TSAR RECORDS HERE                      
IO2      DS    XL256               GENERAL SAVE AREA                            
IO3      DS    XL256               BUILD TOTAL RECORDS HERE                     
IO4      DS    XL256               GENERAL SAVE AREA                            
*                                                                               
WORKDX2  EQU   *                                                                
         EJECT                                                                  
         DROP  RC,RB                                                            
         SPACE 2                                                                
DROOL    CSECT                                                                  
         SPACE 2                                                                
         DS    CL(12288-(*-DROOL)) FORCE MODULE TO OFFSET X'3000'               
         SPACE 2                                                                
DOOUTPUT DS    0H                                                               
         SPACE 2                                                                
         NMOD1 WORKDX3-WORKD3,**DRL3**,R9,CLEAR=YES                             
         USING WORKD3,RC                                                        
*                                                                               
         MVC   LASTPDET,SPACES     INITIALIZE                                   
         MVC   LASTP2,SPACES                                                    
         MVC   SAVEDET,SPACES                                                   
         MVC   SAVEP2,SPACES                                                    
         L     R1,REPAPRNT         A(FIRST PRINT LINE)                          
         MVC   0(132,R1),SPACES                                                 
         MVC   ASCRFLDH,ATWAFLDH   SCREEN POINTER (USED ON-SCREEN ONLY)         
*                                                                               
         LA    R1,HOOK             INTERCEPT SO WE GET THE HEAD HOOK            
         ST    R1,REPAUSR                                                       
         L     R1,GLASPECS                                                      
         ST    R1,REPAPHS                                                       
         MVI   REPHEADH,1                                                       
         MVI   REPMIDSH,1                                                       
         MVI   REPFOOTN,0                                                       
*                                                                               
         BAS   RE,COMPHOOK         HOOK FOR INTERNAL COMPUTES AND RANK          
*                                                                               
         LA    R5,IOA                                                           
         ST    R5,GLAIO            R5=A(RECORD)                                 
         L     R1,ATSARD                                                        
         USING TSARD,R1                                                         
         XC    IOA,IOA                                                          
         ST    R5,TSAREC                                                        
         MVI   TSACTN,TSARDH                                                    
         GOTO1 TSAR                GET FIRST RECORD                             
         TM    TSERRS,TSERNF       EXACT MATCH NOT FOUND?                       
         BO    OUT2                RIGHT                                        
         DC    H'0'                DIE ON OTHER ERROR                           
*                                                                               
OUT1     L     R1,ATSARD                                                        
         MVI   TSACTN,TSANXT                                                    
         GOTO1 TSAR                GET NEXT RECORD                              
         BE    OUT2                IT IS THERE                                  
         TM    TSERRS,TSEEOF       EOF?                                         
         BO    *+6                 YES                                          
         DC    H'0'                DIE ON OTHER ERROR                           
*                                                                               
         MVI   CBLEV,0             EOF HANDLING                                 
         BAS   RE,MULTLT                                                        
*                                                                               
         OC    ATWAFLDH,ATWAFLDH   ON-SCREEN REPORT?                            
         BNZ   EXIT                YES                                          
*                                                                               
         MVI   REPACTN,REPACLO     ASSUME NORMAL CLOSE                          
         TM    REPIND1,REPIPUT     ANY LINES IN REPORT?                         
         BO    *+8                 YES                                          
         MVI   REPACTN,REPACLOP    NO -- CLOSE/PURGE                            
         GOTO1 REPORT,(R8)                                                      
         CLI   REPERRS,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
OUT2     MVC   TSRECNUM,TSRNUM     SAVE RECORD NUMBER                           
         DROP  R1                                                               
*                                                                               
         LA    RF,IOA+2            BUMP PAST CONTROL BYTES                      
         AH    RF,BGKEYLEN         COPY RECORD NUMBER AND LEVEL. . .            
         MVC   IOA(2),0(RF)        . . . INTO FIRST TWO BYTES                   
*                                                                               
         L     R2,GLADTAB          A(DRIVE TABLE)                               
OUT3     CLI   0(R2),X'10'         LOOK FOR RECORD ELEMENT                      
         BNE   *+14                IN DRIVE TABLE                               
         USING DLRECD,R2                                                        
         CLC   DLRECNUM,0(R5)      THAT MATCHES                                 
         BE    OUT4                                                             
*                                                                               
         CLI   0(R2),0                                                          
         BNE   *+6                                                              
         DC    H'0'                NO REC ELEMENT FOUND                         
         ZIC   R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     OUT3                                                             
*                                                                               
OUT4     BAS   RE,TRACEOUT                                                      
         CLI   LASTTYPE,2          WAS LAST RECORD A DETAIL?                    
         BL    *+16                NO                                           
         BAS   RE,SETCB            YES -- SET CONTROL BREAK LEVEL               
         BAS   RE,TRACB            POSSIBLY TRACE CONTROL BREAK                 
         BAS   RE,MULTLT           DEAL WITH LAST TIMES                         
*                                                                               
         CLC   IOC(1),0(R5)        IF THIS IS A NEW 'REC'                       
         BE    *+18                                                             
         MVI   COMPSTAT,0          RESET COMPUTES STATUS                        
         OI    REPHEADI,REPHFRCE   FORCE HEADLINES AFTER LAST TIMES             
         XC    TOTCNTS,TOTCNTS     RESET TOTAL COUNTERS                         
*                                                                               
         ST    R2,GLATHID          A(THIS REC ELEMENT IN DRIVE TABLE)           
         MVC   GLRECNO,0(R5)       SET RECORD                                   
         MVC   GLLEVEL,1(R5)       AND LEVEL NUMBER                             
         ZIC   RF,GLLEVEL                                                       
         BCTR  RF,0                                                             
         SLL   RF,1                                                             
         LA    RF,DLRRECNM(RF)                                                  
         MVC   0(2,RF),TSRECNUM    PUT RECORD NUMBER AT CORRECT LEVEL           
*                                                                               
         BAS   RE,COMPREC          HANDLE DRIVER COMPUTES                       
         CLI   COMPSTAT,2          DID WE DO ANY COMPUTES?                      
         BNE   OUT6                                                             
*                                                                               
         LH    R1,DLRDATLN                                                      
         AH    R1,BGKEYLEN                                                      
         LA    R1,2(R1)            BUMP RECLEN BY 2 FOR VARIABLE LENGTH         
         STCM  R1,3,IOA            TSAR VARIABLE RECORD LENGTH                  
*                                                                               
         L     R1,ATSARD                                                        
         USING TSARD,R1                                                         
         LA    RF,IOB                                                           
         ST    RF,TSAREC                                                        
         MVC   TSRNUM,TSRECNO2                                                  
         MVI   TSACTN,TSAGET                                                    
         GOTO1 TSAR                RESTORE TSAR SEQUENCE                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RF,IOA                                                           
         ST    RF,TSAREC                                                        
         MVI   TSACTN,TSAPUT       YES -- PUT THE CHANGED RECORD                
         GOTO1 TSAR                                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R1                                                               
*                                                                               
         LA    R1,IOA+2            BUMP PAST CONTROL BYTES                      
         AH    R1,BGKEYLEN         COPY RECORD NUMBER AND LEVEL. . .            
         MVC   IOA(2),0(R1)        . . . INTO FIRST TWO BYTES                   
*                                                                               
OUT6     CLC   GLLEVEL,DLRDETLV    IS THIS A DETAIL                             
         BE    *+12                YES                                          
         MVI   LASTTYPE,1                                                       
         B     OUT1                                                             
*                                                                               
         MVI   LASTTYPE,2                                                       
         MVC   IOC,0(R5)           SAVE PRESENT KEY, ETC.                       
         BAS   RE,MULTFT           HANDLE ANY FIRST TIME ROUTINES               
         BAS   RE,DETAIL           AND THEN DO THE DETAILS                      
         B     OUT1                                                             
         DROP  R2                                                               
         EJECT                                                                  
*              HOOK TO USER FOR INTERNAL COMPUTES (INCLUDING RANK)              
         SPACE 2                                                                
COMPHOOK NTR1                                                                   
*                                                                               
         L     R1,ATSARD                                                        
         USING TSARD,R1                                                         
         XC    IOA,IOA                                                          
         LA    R5,IOA                                                           
         ST    R5,TSAREC                                                        
         MVI   TSACTN,TSARDH                                                    
         GOTO1 TSAR                GET FIRST RECORD                             
         TM    TSERRS,TSERNF       EXACT MATCH NOT FOUND?                       
         BO    CH20                RIGHT                                        
         DC    H'0'                DIE ON OTHER ERROR                           
*                                                                               
CH10     MVI   TSACTN,TSANXT                                                    
         GOTO1 TSAR                GET NEXT RECORD                              
         BE    CH20                IT IS THERE                                  
         TM    TSERRS,TSEEOF       EOF?                                         
         BO    *+6                 YES                                          
         DC    H'0'                DIE ON OTHER ERROR                           
*                                                                               
         XC    IOB,IOB                                                          
         XC    IOC,IOC                                                          
         B     EXIT                                                             
*                                                                               
CH20     LA    RF,IOA+2            BUMP PAST CONTROL BYTES                      
         AH    RF,BGKEYLEN                                                      
         MVC   GLRECNO,0(RF)       SET RECORD                                   
         MVC   GLLEVEL,1(RF)       AND LEVEL NUMBER                             
*                                                                               
         L     R2,GLADTAB          A(DRIVE TABLE)                               
CH30     CLI   0(R2),X'10'         LOOK FOR RECORD ELEMENT                      
         BNE   *+14                IN DRIVE TABLE                               
         USING DLRECD,R2                                                        
         CLC   DLRECNUM,GLRECNO    THAT MATCHES                                 
         BE    CH40                                                             
*                                                                               
         CLI   0(R2),0                                                          
         BNE   *+6                                                              
         DC    H'0'                NO REC ELEMENT FOUND                         
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     CH30                                                             
*                                                                               
CH40     OI    GLINDS,GLTOTLIN     ASSUME IT'S A TOTAL RECORD                   
         CLC   GLLEVEL,DLRDETLV    IS IT?                                       
         BL    CH50                YES -- WE HAVE NOT SEEN IT BEFORE            
         BE    *+6                 IT'S A DETAIL RECORD                         
         DC    H'0'                BAD LEVEL NUMBER ON RECORD                   
         NI    GLINDS,X'FF'-GLTOTLIN                                            
         OC    DLRARNKR,DLRARNKR   IS THIS RECORD RANKED?                       
         BZ    CH50                NO -- WE HAVE NOT SEEN IT BEFORE             
*                                                                               
         L     R3,DLRARNKR         A('IN' OF RANKED ROW)                        
         USING DLIND,R3                                                         
         LR    RF,R5                                                            
         AH    RF,DLINDISP         RF POINTS TO RANKED VALUE                    
         CLC   =X'FFFFFFFF',0(RF)  HAS THIS RECORD ALREADY BEEN RANKED?         
         BNE   CH10                YES -- WE'VE ALREADY USED IT                 
*                                                                               
CH50     L     R3,DLRAFIN          A(FIRST 'IN')                                
         XC    GLAIFLD,GLAIFLD                                                  
*                                                                               
CH60     CLI   DLINTYPE+1,C'+'     MUST BE ADDITIVE                             
         BNE   CH90                                                             
         LR    RF,R5               INPUT FIELD                                  
         AH    RF,DLINDISP                                                      
         ST    RF,GLAIFLD          A(INPUT FIELD)                               
         B     CH90                                                             
         DROP  R3                                                               
*                                                                               
         USING DLOD,R3                                                          
CH70     OC    DLORADD,DLORADD     IN ORDER TO QUALIFY, FIELD. . .              
         BZ    CH75                . . . MUST HAVE AN OUTPUT ROUTINE            
         OC    GLAIFLD,GLAIFLD     . . . AND ALSO A PREVIOUS 'IN'               
         BZ    CH75                                                             
*                                                                               
         MVI   GLHOOK,GLINCOMP                                                  
         MVC   GLLABEL,DLOROUT                                                  
         MVC   GLAROUT,DLORADD                                                  
         MVC   GLARGS,DLOARGS                                                   
         MVC   IOB,SPACES                                                       
         MVC   IOC,SPACES                                                       
         LA    RF,IOB                                                           
         ST    RF,GLAOFLD          DUMMY OUTPUT AREA                            
         ST    R3,GLADTENT                                                      
         GOTO1 AGOHOOK                                                          
         DROP  R3                                                               
*                                                                               
CH75     BAS   RE,COMPREC          HANDLE DRIVER COMPUTES                       
*                                                                               
         CLI   COMPSTAT,2          DID WE DO ANY COMPUTES?                      
         BE    *+12                                                             
         CLI   GLHOOK,GLIDID       DID USER COMPUTE FOR US?                     
         BNE   CH80                NO -- NEXT ELEMENT                           
*                                                                               
         MVI   TSACTN,TSAPUT       YES -- PUT THE CHANGED RECORD                
         GOTO1 TSAR                                                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CH80     XC    GLAIFLD,GLAIFLD                                                  
*                                                                               
CH90     ZIC   R0,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,R0                                                            
         CLI   0(R3),X'20'                                                      
         BE    CH60                IN                                           
         CLI   0(R3),X'30'                                                      
         BE    CH70                OUT                                          
         CLI   0(R3),X'10'                                                      
         BE    *+12                REC -- NOTHING ELSE FOR THIS RECORD          
         CLI   0(R3),0                                                          
         BNE   CH90                NEXT ELEMENT                                 
*                                                                               
         OC    DLRARNKR,DLRARNKR   IS THIS RECORD RANKED?                       
         BZ    CH10                NO -- NOTHING ELSE TO DO HERE                
         CLC   DLRDETLV,GLLEVEL    IS THIS A DETAIL RECORD?                     
         BNE   CH10                NO -- NOTHING ELSE TO DO HERE                
*                                                                               
         L     R4,DLRARNKC         A('IN' OF RANKED COLUMN)                     
         USING DLIND,R4                                                         
         LR    RF,R5                                                            
         AH    RF,DLINDISP                                                      
*                                                                               
         CLI   DLINFLEN,1          SINGLE-BYTE FIELD?                           
         BNE   *+14                                                             
         SR    R0,R0                                                            
         IC    R0,0(RF)                                                         
         B     CH95                                                             
*                                                                               
         CLI   DLINFLEN,2          HALFWORD FIELD?                              
         BNE   *+12                                                             
         LH    R0,0(RF)            YES                                          
         B     CH95                                                             
*                                                                               
         CLI   DLINFLEN,3          3-BYTE FIELD?                                
         BNE   *+14                                                             
         SR    R0,R0                                                            
         ICM   R0,7,0(RF)                                                       
         B     CH95                                                             
*                                                                               
         CLI   DLINFLEN,4          FULLWORD FIELD?                              
         BE    *+6                 YES                                          
         DC    H'0'                                                             
         L     R0,0(RF)                                                         
*                                                                               
CH95     C     R0,=F'-1000000000'  PROTECT AGAINST HUGE NEGATIVE. . .           
         BH    *+12                                                             
         LA    R0,1                                                             
         B     CH100                                                            
         C     R0,=F'1000000000'   . . . OR POSITIVE NUMBERS                    
         BNH   *+12                                                             
         L     R0,=X'FFFFFFFE'                                                  
         B     CH100                                                            
         A     R0,=F'1000000000'   SCALE ALL NUMBERS TO BE POSITIVE             
*                                                                               
CH100    TM    DLRFLAGS,DLRRNKCQ   SHOULD WE COMPLEMENT THE VALUE?              
         BO    *+8                 NO                                           
         X     R0,=X'FFFFFFFF'                                                  
*                                                                               
         MVI   TSACTN,TSADEL                                                    
         GOTO1 TSAR                                                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,DLRARNKR         A('IN' OF RANKED ROW)                        
         LR    RF,R5                                                            
         AH    RF,DLINDISP                                                      
         ST    R0,0(RF)            PUT VALUE INTO RANK FIELD                    
         DROP  R4,R2                                                            
*                                                                               
         MVI   TSACTN,TSAADD       ADD RECORD BACK WITH RANK VALUE              
         GOTO1 TSAR                                                             
         BE    CH10                                                             
         DC    H'0'                                                             
*                                                                               
         DROP  R1                                                               
         EJECT                                                                  
*              ROUTINE CONTROLS COMPUTES ON RECORD                              
         SPACE 2                                                                
*                                  R5=A(RECORD TO BE COMPUTED)                  
*                                  R2=A(RELATED REC ELEMENT)                    
*                                                                               
COMPREC  NTR1                                                                   
*                                                                               
         CLI   COMPSTAT,1                                                       
         BE    EXIT                FORGET IT - NO COMPUTES NEEDED               
*                                                                               
         L     RF,ATSARD                                                        
         MVC   TSRECNO2,TSRNUM-TSARD(RF) SAVE CURRENT RECORD NUMBER             
*                                                                               
         MVI   COMPSTAT,1                                                       
         BAS   RE,COMPREC2                                                      
         BAS   RE,COMPREC2         SECOND TIME TO ALLOW RIGHT TO LEFT           
*                                                                               
         B     EXIT                                                             
         SPACE 3                                                                
COMPREC2 NTR1                                                                   
*                                                                               
         USING DLRECD,R2                                                        
         L     R3,DLRAFIN          A(FIRST IN ELEMENT)                          
*                                                                               
COMPREC4 LR    R4,R3               NOTE AN INPUT FIELD                          
         B     COMPREC9                                                         
*                                                                               
         USING DLOD,R3                                                          
COMPREC6 OC    DLOACOMP,DLOACOMP   IN ORDER TO QUALIFY FIELD                    
         BZ    COMPREC8            MUST HAVE AN ATTACHED COMPUTE                
         LTR   R4,R4               AND ALSO A PREVIOUS 'IN'                     
         BZ    COMPREC8                                                         
         USING DLIND,R4                                                         
         MVI   COMPSTAT,2                                                       
         L     R0,DLOACOMP                                                      
         BAS   RE,DOCOMPUT                                                      
         LH    R1,DLINDISP         POSITION TO INPUT FIELD                      
         AR    R1,R5                                                            
*                                                                               
         CVB   R0,DUB                                                           
         CLI   DLINFLEN,2                                                       
         BL    COMPBIN1                                                         
         BE    COMPBIN2                                                         
         CLI   DLINFLEN,4                                                       
         BL    COMPBIN3                                                         
         BE    COMPBIN4                                                         
*                                                                               
COMPBIN1 STC   R0,0(R1)                                                         
         B     COMPREC8                                                         
*                                                                               
COMPBIN2 STH   R0,0(R1)                                                         
         B     COMPREC8                                                         
*                                                                               
COMPBIN3 ST    R0,DUB                                                           
         MVC   0(3,R1),DUB+1                                                    
         B     COMPREC8                                                         
*                                                                               
COMPBIN4 ST    R0,0(R1)                                                         
*                                                                               
COMPREC8 SR    R4,R4                                                            
*                                                                               
COMPREC9 ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),X'20'                                                      
         BE    COMPREC4                                                         
         CLI   0(R3),X'30'                                                      
         BE    COMPREC6                                                         
         CLI   0(R3),X'10'                                                      
         BE    COMPRECX                                                         
         CLI   0(R3),0                                                          
         BNE   COMPREC9                                                         
*                                                                               
COMPRECX B     EXIT                                                             
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
*              SET CONTROL BREAK                                                
         SPACE 2                                                                
*                                  R5=A(RECORD COMING FROM SORT)                
*                                  IOC=KEY OF PREVIOUS DETAIL                   
*                                                                               
SETCB    NTR1                                                                   
*                                                                               
         L     R2,GLATHID                                                       
         USING DLRECD,R2                                                        
         MVI   CBLEV,0                                                          
         CLC   IOC(1),0(R5)        CHANGE OF RECORD?                            
         BNE   SCBX                YES                                          
*                                                                               
         LA    R3,DLRLCBS          NOW SET UP TO TEST OTHER LEVELS              
         LA    R4,1                R4=LEVEL NUMBER                              
         AH    R5,=H'2'            NUMBER OF CONTROL BYTES                      
         LA    RF,IOC                                                           
         AH    RF,=H'2'            NUMBER OF CONTROL BYTES                      
*                                                                               
SCB4     STC   R4,CBLEV                                                         
         ZIC   R1,0(R3)            PICK UP CUMULATIVE LENGTH                    
         LTR   R1,R1                                                            
         BNZ   *+14                                                             
*                                                                               
         BCTR  R4,0                                                             
         STC   R4,CBLEV                                                         
         B     SCBX                                                             
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R5),0(RF)       LOOK FOR A CHANGE AT THIS LEVEL              
         BNE   SCBX                FOUND                                        
*                                                                               
         LA    R3,1(R3)            GO AND TRY NEXT LOWER LEVEL                  
         LA    R4,1(R4)                                                         
         B     SCB4                                                             
*                                                                               
SCBX     B     EXIT                                                             
         EJECT                                                                  
*              CONTROL MULTIPLE FIRST TIME ROUTINES                             
         SPACE 2                                                                
MULTFT   NTR1                                                                   
*                                  CBLEV IS SET TO BREAK LEVEL                  
         L     R2,GLATHID                                                       
         MVC   GLLEVEL,CBLEV       START FROM LOWEST UP TO DETAIL               
*                                                                               
MULTFT2  CLC   GLLEVEL,DLRDETLV    DON'T NEED FT AT DETAIL LEVEL                
         BE    EXIT                                                             
*                                                                               
         BAS   RE,SINGFT           HANDLE FT ROUTINES AT THIS LEVEL             
         ZIC   R1,GLLEVEL                                                       
         LA    R1,1(R1)                                                         
         STC   R1,GLLEVEL                                                       
         B     MULTFT2                                                          
         SPACE 5                                                                
*              CONTROL MULTIPLE LAST TIME ROUTINES                              
         SPACE 2                                                                
MULTLT   NTR1                                                                   
*                                  CBLEV IS SET TO BREAK LEVEL                  
         L     R2,GLATHID                                                       
         CLC   CBLEV,DLRDETLV      DON'T NEED LT ON DETAIL                      
         BE    EXIT                                                             
         ZIC   R1,DLRDETLV         SO PROCESS FROM DETAIL-1 LEVEL               
         BCTR  R1,0                TO CBLEV SETTING BACKWARDS                   
*                                                                               
MULTLT2  STC   R1,GLLEVEL                                                       
         BAS   RE,SINGLT           HANDLE LT ROUTINES AT THIS LEVEL             
         CLC   GLLEVEL,CBLEV                                                    
         BE    EXIT                                                             
         BCTR  R1,0                                                             
         B     MULTLT2                                                          
         EJECT                                                                  
*              CONTROL FIRST, LAST AND TOTAL ROUTINES                           
         SPACE 2                                                                
SINGFT   NTR1                                                                   
*                                                                               
         MVI   BREAK,C'F'                                                       
         MVI   GLHOOK,GLFIRST                                                   
         B     SING2                                                            
         SPACE 2                                                                
SINGLT   NTR1                                                                   
*                                                                               
         MVI   BREAK,C'L'                                                       
         MVI   GLHOOK,GLLAST                                                    
         CLI   GLLEVEL,0                                                        
         BE    EXIT                                                             
         CLC   GLLEVEL,DLRRNKLV    MAY NEED TO RELEASE RANK RECORDS             
         BNE   SING2                                                            
*********BAL   RE,RANKOUT          ** TEMPORARY -- NO RANK PRINTING **          
*                                                                               
SING2    XC    GLARGS,GLARGS                                                    
         MVC   GLARGS(1),GLLEVEL                                                
         BAS   RE,SETAOUT          PICK UP A(THIS OUT) IF ANY                   
         MVC   GLADTENT,ALEVOUT                                                 
         XC    GLAROUT,GLAROUT                                                  
         MVI   GLAROUT,1                                                        
         GOTO1 AGOHOOK             HOOK TO APPLICATION FOR FIRST/LAST           
         OC    ALEVOUT,ALEVOUT                                                  
         BZ    EXIT                (NONE AT THIS LEVEL)                         
*                                                                               
         USING DLOD,R3                                                          
         L     R3,ALEVOUT                                                       
         CLI   DLOFLFOL,0          ANY FIRST/LAST SPECIFIED?                    
         BE    SINGTOT                                                          
         MVI   ELCODE,X'44'        YES SO GO AND LOOK FOR THEM                  
         ZIC   R0,DLOFLFOL                                                      
         BAS   RE,EXFLT                                                         
*                                                                               
SINGTOT  CLI   BREAK,C'L'          IF LAST TIME ROUTINE                         
         BNE   EXIT                                                             
         CLI   DLOTLFOL,0          ARE THERE ANY TOTALS SPECIFIED?              
         BE    EXIT                                                             
*                                                                               
         MVI   ELCODE,X'48'                                                     
         ZIC   R0,DLOTLFOL                                                      
         BAS   RE,EXFLT            THEN EXECUTE ANY CONTROLS                    
         B     EXIT                                                             
         EJECT                                                                  
*              EXECUTE FIRST, LAST OR TOTAL ELEMENT                             
         SPACE 2                                                                
EXFLT    NTR1                                                                   
*                                                                               
         MVI   AUTALLSW,C'Y'                                                    
*                                  R0=NUMBER OF ELEMENTS TO COME                
*                                  ELCODE = X'44' OR X'48'                      
*                                  BREAK = C'F' OR 'L'                          
*                                  R3=A(OUT) OR A(RECORD) FOR LEVEL 0           
         L     R2,GLATHID                                                       
         XC    GLAIFLD,GLAIFLD                                                  
         XC    GLATOUT,GLATOUT                                                  
         CLI   GLLEVEL,0                                                        
         BE    EXFLT2                                                           
         USING DLOD,R3                                                          
         ST    R3,GLATOUT          SAVE A(OUT FOR TOTAL ROUTINES)               
         ICM   RF,15,DLOIADD       PICK UP A(IN) IF ANY                         
         BZ    EXFLT2                                                           
*                                                                               
         LA    R1,IOA              A(THIS RECORD)                               
         USING DLIND,RF                                                         
         AH    R1,DLINDISP                                                      
         DROP  RF                                                               
         ST    R1,GLAIFLD          RECORD A(INPUT FIELD IN RECORD)              
*                                                                               
EXFLT2   ZIC   R1,1(R3)                                                         
         LTR   R1,R1                                                            
         BZ    EXIT                                                             
         AR    R3,R1                                                            
         CLC   0(1,R3),ELCODE      LOOK FOR MATCH ON ELEMENT CODE               
         BNE   EXFLT2                                                           
         USING DLFLD,R3                                                         
         CLI   ELCODE,X'48'                                                     
         BE    *+14                                                             
         CLC   DLFLTYPE,BREAK      AND FIRST/LAST                               
         BNE   EXFLTEND                                                         
*                                                                               
EXFLT4   CLI   ELCODE,X'48'        ARE WE HANDLING TOTALS?                      
         BNE   EXFLT5                                                           
         CLI   DLFLDET,0           IS THIS DETAILED TOTALS?                     
         BE    *+12                                                             
         TM    DLRFLAGS,DLRTTOTQ   YES - ONLY GET REGULAR TOTALS IF             
         BZ    EXFLTEND                  TOTALS AT LEVEL ABOVE DETAIL           
*                                                                               
         L     RF,GLAIFLD          PROTECT GLAIFLD                              
         BAS   RE,TOTALS           YES - SO LOOK FOR NUMBERS                    
         ST    RF,GLAIFLD                                                       
         CLI   TOTPEND,C'Y'                                                     
         BNE   EXFLT9                                                           
*                                                                               
EXFLT5   OC    DLFLRADD,DLFLRADD   ANY ROUTINE TO HANDLE?                       
         BZ    EXFLT6                                                           
         MVC   GLAROUT,DLFLRADD                                                 
         MVC   GLLABEL,DLFLROUT                                                 
         MVC   GLARGS,DLFLARGS                                                  
*                                  (GLAIFLD SET ABOVE)                          
         MVC   GLAOFLD,DLFLAPOS                                                 
         ST    R3,GLADTENT                                                      
         MVI   GLHOOK,GLROUT                                                    
         OI    GLINDS,GLINTOTL     HOOK COMES FROM TOTAL ELEMENT                
         GOTO1 AGOHOOK                                                          
         NI    GLINDS,(X'FF'-GLINTOTL)                                          
         MVI   AUTALLSW,C'N'                                                    
*                                                                               
EXFLT6   CLI   DLFLELEN,DLFLLENQ   ANY LITERALS FOR US TO HANDLE                
         BE    EXFLT8                                                           
         ZIC   R1,DLFLELEN         YES                                          
         SH    R1,=Y(DLFLLENQ)                                                  
         STC   R1,LITLEN           SET LENGTH                                   
         LA    R1,DLFLLIT                                                       
         ST    R1,ALITIN           ADDRESS                                      
         MVC   ALITOUT,DLFLAPOS    AND WHERE IT SHOULD GO                       
         BAS   RE,DOLIT                                                         
         MVI   AUTALLSW,C'N'                                                    
*                                                                               
EXFLT8   CLI   LINETYPE,C'T'       ANY TOTALS WAITING FOR PRINT?                
         BNE   EXFLT9              LINETYPE SET IN TOTALS ROUTINE               
         BAS   RE,AUTOALL          CAN PUT 'ALL' AUTOMATICALLY                  
         MVC   NLINES,GLSPACE                                                   
         BAS   RE,SPLAT                                                         
         MVI   LINETYPE,0                                                       
*                                                                               
EXFLT9   CLI   DLFLSPAC,0          ANY PRINTING TO DO                           
         BE    EXFLT10                                                          
         CLI   BREAK,C'L'          ONLY LAST GETS HANDLED HERE                  
         BNE   EXFLT10                                                          
         TM    GLINDS,GLISDONT     SUPPRESS SPACING FOR REJECTED                
         BZ    *+12                                         PRNT LINES?         
         CLI   DONTSW,C'Y'         YES- PRINT LINE REJECTED?                    
         BE    EXFLT10                 YES-DO NOT PRINT BLANK LINE(S)           
         MVC   NLINES,DLFLSPAC     SET NUMBER OF LINES                          
         BAS   RE,SPLAT            AND DO IT                                    
*                                                                               
EXFLT10  TM    DLFLOPT,DLFLOPSK    OPTION TO SKIP NEXT TIME                     
         BNO   EXFLT12                                                          
         MVC   LASTPDET,SPACES     STOP 'CONTINUED' FEATURE                     
         MVC   LASTP2,SPACES                                                    
         OI    REPHEADI,REPHFRCE   FORCE HEADLINES AFTER LAST TIMES             
*                                                                               
EXFLT12  TM    DLFLOPT,DLFLOPRE    OPTION TO RESET PAGE NUMBER                  
         BNO   *+10                                                             
         MVC   REPPAGE,=H'1'                                                    
*                                                                               
         CLI   DLFLSPAC,0          ANY PRINTING TO DO                           
         BE    EXFLTEND                                                         
         CLI   BREAK,C'L'          DEALING WITH LAST HERE                       
         BE    EXFLTEND                                                         
         TM    GLINDS,GLISDONT     SUPPRESS SPACING FOR REJECTED                
         BZ    *+12                                         PRNT LINES?         
         CLI   DONTSW,C'Y'         YES- PRINT LINE REJECTED?                    
         BE    EXFLTEND                YES-DO NOT PRINT BLANK LINE(S)           
         MVC   NLINES,DLFLSPAC     SET NUMBER OF LINES                          
         BAS   RE,SPLAT            AND DO IT                                    
*                                                                               
EXFLTEND BCT   R0,EXFLT2                                                        
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*              ROUTINE TO PUT IN 'ALL' AUTOMATICALLY                            
         SPACE 2                                                                
AUTOALL  NTR1                                                                   
*                                                                               
         CLI   AUTALLSW,C'Y'       NOT IF USER HAD LITERAL OR ROUTINE           
         BNE   EXIT                                                             
*                                                                               
AUTALL2  ZIC   R1,1(R3)            LOOK FOR NEXT OUT ELEMENT                    
         AR    R3,R1                                                            
         CLI   0(R3),0                                                          
         BE    EXIT                                                             
         CLI   0(R3),X'30'                                                      
         BNE   AUTALL2                                                          
         USING DLOD,R3                                                          
         CLI   DLOLTYP,C'P'                                                     
         BNE   AUTALL2                                                          
         CLI   DLOLEN,3                                                         
         BL    EXIT                NOT ROOM FOR ANYTHING                        
*                                                                               
         ICM   R4,15,DLOAPOS                                                    
         BZ    AUTALL2                                                          
         MVC   0(3,R4),=C'ALL'     ROOM FOR 'ALL'                               
         CLI   DLOLEN,5                                                         
         BL    *+10                                                             
         MVC   0(5,R4),=C'*ALL*'   ROOM FOR '*ALL*'                             
         B     EXIT                                                             
         EJECT                                                                  
*              SET ADDRESS OF OUT AT SPECIFIED LEVEL                            
         SPACE 2                                                                
SETAOUT  NTR1                                                                   
*                                                                               
         L     R2,GLATHID                                                       
         L     R3,DLRAFOUT                                                      
         USING DLOD,R3                                                          
*                                                                               
SAO2     CLI   0(R3),0             NO GOOD IF 0 OR X'10'                        
         BE    SAOMISS                                                          
         CLI   0(R3),X'10'                                                      
         BE    SAOMISS                                                          
         CLI   0(R3),X'30'         WE NEED AN X'30' AT REQUIRED LEVEL           
         BNE   SAONEXT                                                          
         CLC   DLOLEV,GLLEVEL                                                   
         BNE   SAONEXT                                                          
         ST    R3,ALEVOUT          FOUND                                        
         B     EXIT                                                             
*                                                                               
SAONEXT  ZIC   R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     SAO2                                                             
*                                                                               
SAOMISS  XC    ALEVOUT,ALEVOUT                                                  
         B     EXIT                                                             
         EJECT                                                                  
*              OVERALL CONTROL OF DETAIL AND TOTALS                             
         SPACE 2                                                                
DETAIL   NTR1                                                                   
*                                                                               
         MVI   LINETYPE,C'D'                                                    
         NI    GLINDS,X'FF'-GLTOTLIN                                            
         B     DET2                                                             
         SPACE 2                                                                
TOTALS   NTR1                                                                   
*                                                                               
         MVI   LINETYPE,C'T'                                                    
         OI    GLINDS,GLTOTLIN                                                  
*                                                                               
DET2     NI    GLINDS,X'FF'-GLDETFTS   RESET FIRST TIME SWITCH                  
         L     R2,GLATHID                                                       
         L     R3,DLRAFOUT         PICK UP ADDRESS OF FIRST OUT                 
         USING DLOD,R3                                                          
*                                                                               
         MVI   TOTPEND,C'Y'        ASSUME WE WILL HAVE TOTALS                   
         ZIC   R1,GLLEVEL                                                       
         CLI   LINETYPE,C'D'                                                    
         BNE   *+8                                                              
         IC    R1,DLRDETLV                                                      
*                                                                               
         BAS   RE,TOTCON           CHECK FOR UNNECESSARY TOTALS                 
         BNE   DET3                                                             
         BCTR  R1,0                                                             
         SLL   R1,1                                                             
         LA    R1,DLRRECNM(R1)     R1=A(TSAR RECORD NUM AT THIS LEVEL)          
         OC    0(2,R1),0(R1)       RECORD EXISTS AT THIS LEVEL?                 
         BNZ   *+16                YES                                          
*                                                                               
DET3     MVI   TOTPEND,C'N'        NO-SUPPRESS TOTAL                            
         MVI   LINETYPE,0                                                       
         B     EXIT                                                             
*                                                                               
         MVC   FULL(2),0(R1)       SAVE RECORD NUMBER                           
         XC    0(2,R1),0(R1)       DON'T REUSE THIS RECORD LATER                
*                                                                               
         L     R1,ATSARD                                                        
         USING TSARD,R1                                                         
         LA    RF,IOB                                                           
         ST    RF,TSAREC                                                        
         MVC   TSRNUM,FULL                                                      
         MVI   TSACTN,TSAGET                                                    
         GOTO1 TSAR                GET THE RECORD BACK FROM TSAR                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RF,IOB+2            BUMP PAST CONTROL BYTES                      
         AH    RF,BGKEYLEN         COPY RECORD NUMBER AND LEVEL. . .            
         MVC   IOB(2),0(RF)        . . . INTO FIRST TWO BYTES                   
*                                                                               
         LA    RF,IOA                                                           
         ST    RF,TSAREC                                                        
         MVC   TSRNUM,TSRECNUM                                                  
         GOTO1 TSAR                RESTORE TSAR SEQUENCE                        
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R1                                                               
*                                                                               
         LA    RF,IOA+2            BUMP PAST CONTROL BYTES                      
         AH    RF,BGKEYLEN         COPY RECORD NUMBER AND LEVEL. . .            
         MVC   IOA(2),0(RF)        . . . INTO FIRST TWO BYTES                   
*                                                                               
DET4     CLI   GLDETHED,C'Y'       UNLESS USER ASKED SPECIALLY                  
         BE    *+12                                                             
         CLI   DLOLTYP,C'H'        DON'T BOTHER WITH HEADLINES HERE             
         BE    DETEND                                                           
*                                                                               
         ST    R3,GLADTENT                                                      
         L     R5,DLOAPOS          R5=A(OUTPUT)                                 
         CLI   DLOELEN,DLODLENQ    DO WE NEED TO HANDLE A LITERAL               
         BE    DET6                                                             
         ZIC   R1,DLOELEN          YES                                          
         SH    R1,=Y(DLODLENQ)                                                  
         STC   R1,LITLEN           LENGTH OF LITERAL                            
         CLC   LITLEN,DLOLEN       WILL IT FIT?                                 
         BNH   *+10                YES                                          
         MVC   LITLEN,DLOLEN       NO -- FUDGE THE LENGTH                       
         LA    R1,DLOLIT                                                        
         ST    R1,ALITIN           ITS ADDRESS                                  
         ST    R5,ALITOUT          AND WHERE IT GOES                            
         BAS   RE,DOLIT                                                         
         L     R5,ALITOUT          R5=A(NEXT SPACE)                             
         LA    R5,1(R5)            LEAVE A BLANK                                
*                                                                               
DET6     ST    R5,GLAOFLD                                                       
         XC    GLAIFLD,GLAIFLD                                                  
         ICM   R1,15,DLOIADD                                                    
         BZ    DET8                                                             
         USING DLIND,R1                                                         
         ZIC   RE,DLINLEN                                                       
         LA    RF,IOB                                                           
         AH    RF,DLINDISP                                                      
         ST    RF,GLAIFLD                                                       
         CLI   DLINTYPE+1,C'+'     SKIP SIGNIFICANCE TESTS                      
         BE    DET8                FOR ADDITIVE FIELDS                          
         DROP  R1                                                               
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         OC    0(0,RF),0(RF)       ANY SIGNIFICANT DATA?                        
         BZ    DETEND              NO SO NO NEED TO PROCESS                     
*                                                                               
         TM    DLRFLAGS,DLRTDETQ   ANY DETAILED TOTALS?                         
         BZ    DET8                NO                                           
         CLC   DLOLEV,DLRDETLV     X'FF' ALLOWED FOR DETAIL LEVEL               
         BNL   DET8                                                             
         CLI   0(RF),X'FF'         X'FF'?                                       
         BNE   DET8                                                             
         BCTR  RE,0                                                             
         LTR   RE,RE                                                            
         BM    DET7                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   1(0,RF),0(RF)                                                    
         BNE   DET8                                                             
*                                                                               
DET7     LTR   R5,R5               YES- PRINT=NO?                               
         BZ    DETEND                                                           
         ZIC   RE,DLOLEN                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8              CLEAR THE OUTPUT FIELD                       
         B     *+10                                                             
         MVC   0(0,R5),SPACES                                                   
*                                                                               
         CLI   DLOLEN,3            PUT 'ALL' IN OUTPUT FIELD                    
         BL    DETEND                                                           
         MVC   0(3,R5),=C'ALL'                                                  
         CLI   DLOLEN,5                                                         
         BL    DETEND                                                           
         MVC   0(5,R5),=C'*ALL*'                                                
         B     DETEND                                                           
*                                                                               
DET8     OC    DLORADD,DLORADD     IS THERE A USER ROUTINE FOR THIS             
         BZ    DET10                                                            
         MVI   GLHOOK,GLROUT       YES SO SET UP FOR A HOOK                     
         MVC   GLLABEL,DLOROUT                                                  
         MVC   GLAROUT,DLORADD                                                  
         MVC   GLARGS,DLOARGS                                                   
         GOTO1 AGOHOOK                                                          
         CLI   GLHOOK,GLEDIT       DID USER WANT US TO EDIT                     
         BE    DET10                                                            
         BAS   RE,ALIGN                                                         
         B     DETEND                                                           
*                                                                               
DET10    BAS   RE,FORM             OTHERWISE DRIVER WILL DEAL WITH IT           
*                                                                               
DETEND   ZIC   R1,1(R3)                                                         
         AR    R3,R1                                                            
         CLI   0(R3),X'30'                                                      
         BE    DET4                                                             
         CLI   0(R3),0                                                          
         BE    DETEND2                                                          
         CLI   0(R3),X'10'                                                      
         BNE   DETEND                                                           
*                                                                               
DETEND2  MVC   NLINES,GLSPACE                                                   
         TM    GLINDS2,GLPWHOLE    OPTION NOT TO SUPPRESS                       
         BNO   *+10                                                             
         MVC   LASTPDET,SPACES     FORCE NO SUPPRESSION OF LEADING DATA         
         BAS   RE,SUPPRESS         GO AND CHECK FOR SUPPRESSION                 
         CLI   LINETYPE,C'T'       TOTALS WILL PRINT AFTER WE HAVE              
         BE    EXIT                PROCESSED ANY TOTAL LITERALS, ETC.           
         BAS   RE,SPLAT                                                         
         MVI   LINETYPE,0                                                       
         B     EXIT                                                             
         EJECT                                                                  
*              CHECK FOR UNNECESSARY TOTALS                                     
         SPACE 2                                                                
TOTCON   NTR1                                                                   
*                                  R1=LEVEL NUMBER                              
         MVI   TOTPEND,C'Y'                                                     
         LTR   R1,R1               ALWAYS ALLOW LEVEL 0                         
         BZ    TOTCNTOK                                                         
*                                                                               
         L     R2,GLATHID                                                       
         ZIC   R0,DLRDETLV         R0=N'LEVELS TO CHECK                         
         SR    R0,R1                                                            
         SLL   R1,1                                                             
         LA    R1,TOTCNTS(R1)      ADDRESS TO LEVEL COUNTER                     
         LH    RE,0(R1)            AND BUMP BY 1                                
         LA    RE,1(RE)                                                         
         STH   RE,0(R1)                                                         
         CLI   LINETYPE,C'D'       ALWAYS DO DETAILS                            
         BE    TOTCNTOK                                                         
*                                                                               
TOTCNT2  LA    R1,2(R1)            FIND COUNT FOR NEXT LOWEST LEVEL             
         OC    0(2,R1),0(R1)                                                    
         BNZ   *+12                                                             
         BCT   R0,TOTCNT2                                                       
         B     TOTCNT6                                                          
*                                                                               
         LH    RE,0(R1)                                                         
         XC    0(2,R1),0(R1)                                                    
         CH    RE,=H'1'            DON'T HANDLE IF ONE OR LESS                  
         BH    TOTCNTOK                                                         
*                                                                               
TOTCNT6  TM    GLINDS,GLPALTOT     OPTION TO GET ALL TOTALS                     
         BO    TOTCNTOK                                                         
         CR    RB,R1               SET CC NOT EQUAL                             
         B     EXIT                                                             
*                                                                               
TOTCNTOK CR    RE,RE               SET CC EQUAL                                 
         B     EXIT                                                             
         EJECT                                                                  
*              POSSIBLE SUPPRESSION OF LEADING DETAIL DATA                      
         SPACE 2                                                                
SUPPRESS NTR1                                                                   
*                                                                               
         MVI   CONTSW,C'N'                                                      
         L     R1,REPAPRNT         A(FIRST PRINT LINE)                          
         MVC   SAVEDET,0(R1)                                                    
         MVC   SAVEP2,132(R1)                                                   
         LA    R4,DLRPDCBS                                                      
         LA    R0,12                                                            
*                                                                               
SUP2     CLI   0(R4),0                                                          
         BE    SUP4                                                             
         ZIC   R1,0(R4)            PICK UP LENGTH OF COMPOSITE DISP.            
         BCTR  R1,0                FOR THIS LEVEL                               
         ZIC   RF,DLRPDISP                                                      
         AR    R1,RF                                                            
         EX    R1,*+8              IS DATA THE SAME                             
         B     *+10                                                             
         CLC   SAVEDET(0),LASTPDET                                              
         BNE   SUPEND                                                           
         EX    R1,*+8              IS DATA THE SAME                             
         B     *+10                                                             
         CLC   SAVEP2(0),LASTP2                                                 
         BNE   SUPEND                                                           
         EX    R1,*+8              AND NOT SPACES                               
         B     *+10                                                             
         CLC   SAVEDET(0),SPACES                                                
         BE    SUPEND                                                           
         L     RF,REPAPRNT         A(FIRST PRINT LINE)                          
         EX    R1,*+8              THEN CLEAR ON THIS LINE                      
         B     *+10                                                             
         MVC   0(0,RF),SPACES                                                   
         EX    R1,*+8              AND MAY BE ON NEXT                           
         B     *+10                                                             
         MVC   132(0,RF),SPACES                                                 
         MVI   CONTSW,C'S'         AND SET CONTINUATION SWITCH                  
*                                  S=SUSPENSE UNTIL ACTUAL PRINT                
*                                                                               
SUP4     LA    R4,1(R4)                                                         
         BCT   R0,SUP2                                                          
*                                                                               
SUPEND   B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*              CONTROL DRIVER FORMATTING                                        
         SPACE 2                                                                
*                                  R2=A(ASSOCIATED IN)                          
*                                  R3=A(OUT)                                    
*                                  R4=A(INPUT FIELD)                            
*                                  R5=A(PRINT POSITION)                         
FORM     NTR1                                                                   
*                                                                               
         LTR   R5,R5               REJECT IF P=NO                               
         BZ    EXIT                                                             
         L     R2,DLOIADD                                                       
         USING DLIND,R2                                                         
         LTR   R2,R2                                                            
         BZ    EXIT                (NO INPUT FIELD TO HANDLE)                   
*                                                                               
         L     R4,GLAIFLD                                                       
         ZIC   R1,DLINLEN                                                       
         BCTR  R1,0                                                             
         CLI   DLINTYPE,C'C'       PICK INPUT TYPE                              
         BE    CHAR                                                             
         CLI   DLINTYPE,C'D'                                                    
         BE    DATE                                                             
         CLI   DLINTYPE,C'X'                                                    
         BE    HEX                                                              
         B     FORMNUM                                                          
         EJECT                                                                  
*              CHARACTER ROUTINES                                               
         SPACE 2                                                                
CHAR     CLI   DLOTYPE,C'C'        PICK OUTPUT TYPE                             
         BE    CTOC                                                             
         CLI   DLOTYPE,C'X'                                                     
         BE    CTOX                                                             
         B     EXIT                                                             
*                                                                               
*                                  CHARACTER TO CHARACTER ROUTINES              
CTOC     CLC   DLINLEN,DLOLEN      INPUT LENGTH NOT GREATER THAN                
         BNH   ODDOUT              OUTPUT LENGTH                                
*                                                                               
*                                  INPUT IS GREATER THAN OUTPUT                 
CTOC2    TM    DLOFORM,X'10'       IS CHOPPING ALLOWED                          
         BO    CTOC4                                                            
         ZIC   R1,DLOLEN           NO SO USE OUTPUT LENGTH                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),0(R4)       AND TRUNCATE                                 
         B     EXIT                                                             
*                                                                               
CTOC4    ST    R4,DMCB             CHOPPING HERE                                
         MVC   DMCB(1),DLINLEN                                                  
         ST    R5,DMCB+4                                                        
         MVC   DMCB+4(1),DLOLEN                                                 
         MVC   DMCB+8(4),=F'20'                                                 
         MVI   DMCB+8,132                                                       
         GOTO1 CHOPPER,DMCB                                                     
         B     EXIT                                                             
*                                                                               
CTOX     ZIC   R1,DLOLEN           CHARACTER TO HEX                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    0(0,R5),0(R5)       CLEAR OUTPUT FIRST                           
         ST    R4,DMCB                                                          
         ST    R5,DMCB+4                                                        
         LA    R1,1(R1)                                                         
         SLL   R1,1                                                             
         ZIC   R0,DLINLEN          ENSURE THAT INPUT LENGTH IS NOT              
         CR    R0,R1               MORE THAN TWICE THE OUTPUT LENGTH            
         BL    *+6                                                              
         LR    R0,R1                                                            
         ST    R0,DMCB+8                                                        
         GOTO1 HEXIN,DMCB                                                       
         B     EXIT                                                             
         EJECT                                                                  
*              DATE AND HEX ROUTINES                                            
         SPACE 2                                                                
DATE     STM   R4,R5,DMCB                                                       
         MVC   DMCB(1),DLINTYPE+1                                               
         MVC   DMCB+4(1),DLOTYPE+1                                              
         GOTO1 DATCON,DMCB                                                      
         B     EXIT                                                             
*                                                                               
HEX      CLI   DLOTYPE,C'C'                                                     
         BE    XTOC                                                             
         CLI   DLOTYPE,C'X'                                                     
         BE    XTOX                                                             
         B     EXIT                                                             
*                                                                               
*                                  HEX TO HEX ROUTINES                          
XTOX     CLC   DLINLEN,DLOLEN      CHECK INPUT NOT TOO LONG                     
         BH    ODDOUT                                                           
         ZIC   R1,DLOLEN                                                        
         BCTR  R1,0                                                             
         B     ODDOUT                                                           
*                                                                               
XTOC     ZIC   R1,DLOLEN           HEX TO CHARACTER                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),SPACES      CLEAR OUTPUT FIRST                           
         ST    R4,DMCB                                                          
         ST    R5,DMCB+4                                                        
         LA    R1,1(R1)                                                         
         SRL   R1,1                                                             
         ZIC   R0,DLINLEN          ENSURE THAT INPUT LENGTH IS NOT              
         CR    R0,R1               MORE THAN HALF THE OUTPUT LENGTH             
         BL    *+6                                                              
         LR    R0,R1                                                            
         ST    R0,DMCB+8                                                        
         GOTO1 HEXOUT,DMCB,,,,=C'TOG'                                           
         OC    DMCB+16(4),DMCB+16                                               
         BNZ   EXIT                                                             
         DC    H'0'                                                             
*                                                                               
ODDOUT   EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),0(R4)                                                    
         BAS   RE,ALIGN                                                         
         B     EXIT                                                             
         EJECT                                                                  
*              ALIGNMENT AND UNDERLINE ROUTINES                                 
         SPACE 2                                                                
ALIGN    NTR1                                                                   
*                                                                               
         CLI   DLOALIGN,C'R'       CHECK SPECIAL ALIGNMENT                      
         BE    RALIGN                                                           
         CLI   DLOALIGN,C'C'                                                    
         BE    CALIGN                                                           
         B     ANYUNDER                                                         
*                                                                               
RALIGN   L     RF,RIGHT                                                         
         B     ALIGNALL                                                         
*                                                                               
CALIGN   L     RF,CENTER                                                        
*                                                                               
ALIGNALL ST    R5,DMCB             A(OUTPUT)                                    
         ZIC   R1,DLOLEN           L'OUTPUT                                     
         ST    R1,DMCB+4                                                        
         GOTO1 (RF),DMCB           GO TO RIGHT OR CENTER                        
*                                                                               
ANYUNDER CLI   DLOUNDER,0                                                       
         BE    EXIT                                                             
         ST    R5,DMCB                                                          
         MVC   DMCB(1),DLOLEN                                                   
         LA    R1,132(R5)                                                       
         ST    R1,DMCB+4                                                        
         MVC   DMCB+4(1),DLOUNDER                                               
         GOTO1 UNDERLIN,DMCB                                                    
         B     EXIT                                                             
         EJECT                                                                  
*              OUTPUT NUMERIC                                                   
         SPACE 2                                                                
FORMNUM  LA    R1,DMCB             GENERAL NUMERIC                              
         USING EBLOCKD,R1                                                       
         XC    EBLOCK,EBLOCK                                                    
*                                                                               
         ST    R4,EBAIN            R4=A(INPUT)                                  
         MVC   EBTIN,DLINTYPE      PICK UP INPUT TYPE                           
         MVC   EBLIN,DLINFLEN      AND LENGTH                                   
         ST    R5,EBAOUT           R5=A(OUTPUT)                                 
         MVC   EBLOUT,DLOLEN       PASS OTHER OUTPUT VALUES                     
         MVC   EBDECS,DLODEC                                                    
         MVC   EBFILL,DLOFILL                                                   
         MVC   EBFLOAT,DLOFLOAT                                                 
         MVC   EBROUND,DLODIV                                                   
         MVC   EBOPT,DLOEDIT                                                    
         MVC   EBTRIM,DLOFORM                                                   
         MVC   EBALIGN,DLOALIGN                                                 
         MVC   EBSCOUT,DLOSCALE                                                 
         MVC   EBTRAIL,DLOTRAIL                                                 
*                                                                               
         TM    GLINDS,GLRNDOPT     ROUND OPTION                                 
         BNO   FORMNUM6                                                         
         TM    GLINDS,GLTOTLIN     ONLY APPLIES TO TOTALS                       
         BNO   FORMNUM6                                                         
*                                                                               
         ZIC   RF,EBLOUT           REDUCE OUTPUT LENGTH                         
         SH    RF,=H'3'            (NO POINT OR DECIMALS)                       
         STC   RF,EBLOUT                                                        
         MVI   EBROUND,2           FORCE ROUND BY 100                           
         MVI   EBDECS,0                  AND NO DECIMAL PLACES                  
*                                                                               
FORMNUM6 ST    R1,FULL                                                          
         GOTO1 EDITOR,FULL                                                      
         DROP  R1                                                               
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*              ROUTINE TO HANDLE A LITERAL                                      
         SPACE 2                                                                
DOLIT    NTR1                                                                   
*                                                                               
         ZIC   R0,LITLEN           LITLEN=LENGTH OF THIS ONE                    
         L     R2,ALITIN           ALITIN=ITS ADDRESS                           
         ICM   R3,15,ALITOUT       ALITOUT=WHERE IT GOES                        
         BZ    EXIT                                                             
*                                                                               
DOLIT2   CLI   0(R2),C'&&'         CHECK FOR START OF SOFT EXPRESSION           
         BE    DOLIT4                                                           
         MVC   0(1,R3),0(R2)       OTHERWISE MOVE OUT THE DATA                  
         LA    R2,1(R2)                                                         
         LA    R3,1(R3)                                                         
         BCT   R0,DOLIT2                                                        
*                                                                               
         ST    R3,ALITOUT          PASS BACK NEXT OUTPUT POSITION               
         B     EXIT                                                             
*                                                                               
DOLIT4   LR    RF,R0               LOOK FOR END OF SOFT EXPRESSION              
         LA    RE,1(R2)                                                         
*                                                                               
DOLIT6   CLI   0(RE),C'&&'                                                      
         BE    DOLIT8                                                           
         LA    RE,1(RE)                                                         
         BCT   RF,DOLIT6                                                        
         B     DOLITEND            NOT FOUND SO TREAT FIRST AS DATA             
*                                                                               
DOLIT8   LR    R5,RE               FIGURE OUT L'SOFT TO R5                      
         SR    R5,R2                                                            
         BCT   R5,*+8                                                           
         B     DOLITEND                                                         
*                                                                               
         MVC   WORK,SPACES                                                      
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),1(R2)       PICK OFF SOFT                                
         LA    RF,WORK                                                          
         ST    RF,GLAIFLD          PASS ITS ADDRESS                             
         ST    R3,GLAOFLD          PASS ADDRESS OF OUTPUT                       
         LA    R5,1(R5)                                                         
         XC    GLARGS,GLARGS                                                    
         STC   R5,GLARGS           PASS INPUT LENGTH IN ARG(1)                  
         MVI   GLHOOK,GLRESLIT     ASK FOR LITERAL RESOLVE                      
         XC    GLAROUT,GLAROUT                                                  
         MVI   GLAROUT,1                                                        
         GOTO1 AGOHOOK             FIRST FROM APPLICATION                       
         CLI   GLARGS+1,0                                                       
         BNE   DOLIT10                                                          
         MVI   GLAROUT,2           THEN FROM SYSDRIVER                          
         GOTO1 AGOHOOK                                                          
         CLI   GLARGS+1,0                                                       
         BE    DOLITEND            NO HELP SO TREAT BOTH AS DATA                
*                                                                               
DOLIT10  LA    R5,2(R5)            SKIP PAST WORD AND DELIMITERS                
         AR    R2,R5                                                            
         SR    R0,R5               REDUCE REMAINING LENGTH                      
         BZ    EXIT                (NONE LEFT)                                  
         ZIC   R5,GLARGS+1         L'RETURNED OUTPUT                            
         AR    R3,R5                                                            
*                                                                               
DOLITEND MVC   0(1,R3),0(R2)       MOVE OUT THE REST OF THE DATA                
         LA    R2,1(R2)                                                         
         LA    R3,1(R3)                                                         
         BCT   R0,DOLITEND                                                      
*                                                                               
         ST    R3,ALITOUT          PASS BACK NEXT OUTPUT POSITION               
         B     EXIT                                                             
         EJECT                                                                  
*              ROUTINE TO CONTROL THE PRINTING OF DETAIL LINES                  
         SPACE 2                                                                
SPLAT    NTR1                                                                   
*                                                                               
         MVI   GLAROUT,1           HOOK TO APPLICATION TO SAY                   
         MVI   GLHOOK,GLPRINT      WE'RE ABOUT TO PRINT                         
         GOTO1 AGOHOOK                                                          
         MVI   DONTSW,C'N'                                                      
         CLI   GLHOOK,GLDONT       APPLIC. CAN THEN REJECT                      
         BNE   *+12                                                             
         MVI   DONTSW,C'Y'                                                      
         B     SPLATX                                                           
*                                                                               
         OC    ATWAFLDH,ATWAFLDH   ON-SCREEN REPORT?                            
         BZ    SPLAT2              NO                                           
         CLI   TOPSW,0             HAVE WE ALREADY FILLED IN HEADINGS?          
         BNE   *+20                YES                                          
         MVI   TOPSW,C'N'                                                       
         BAS   RE,HOOK             FILL IN COLUMN HEADINGS                      
         L     R3,REPAHEAD         A(FIRST HEAD LINE)                           
         BAS   RE,SCRNFILL         DISPLAY COLUMN HEADINGS                      
         L     R3,REPAPRNT         A(FIRST PRINT LINE)                          
         BAS   RE,SCRNFILL         DISPLAY PRINT LINES                          
         B     SPLATX                                                           
*                                                                               
SPLAT2   MVC   LASTPDET,SAVEDET    ACTUALLY PRINTING SO SET                     
         MVC   LASTP2,SAVEP2                                                    
         CLI   CONTSW,C'S'         CONTINUATION DETAILS                         
         BNE   *+8                                                              
         MVI   CONTSW,C'Y'                                                      
*                                                                               
         L     R2,REPAPRNT                                                      
         SR    R1,R1               FIRST COUNT ACTIVE LINES IN R1               
         ZIC   R0,REPPRNTN                                                      
*                                                                               
SPLAT3   CLC   0(132,R2),SPACES                                                 
         BE    *+8                                                              
         LA    R1,1(R1)                                                         
         LA    R2,132(R2)                                                       
         BCT   R0,SPLAT3                                                        
*                                                                               
         CLI   NLINES,0                                                         
         BNE   *+8                                                              
         MVI   NLINES,1                                                         
         IC    R0,NLINES                                                        
         AR    R1,R0               THEN ADD IN SPACING LINES                    
         IC    R0,REPLINE          AND ADD IN PRESENT LINE NUMBER               
         AR    R1,R0                                                            
         IC    R0,REPMAXL          MAXIMUM LINES PER PAGE                       
         MVI   TOPSW,C'N'                                                       
         CR    R1,R0               WILL THEY FIT?                               
         BNH   *+12                                                             
         OI    REPHEADI,REPHFRCE   NO, FORCE HEADLINES ON FIRST PRINT           
         MVI   TOPSW,C'Y'                                                       
*                                                                               
         L     R2,REPAPRNT                                                      
         ZIC   R0,REPPRNTN                                                      
         CLC   0(132,R2),SPACES    IS FIRST LINE BLANK?                         
         BNE   SPLAT7                                                           
         CLI   TOPSW,C'Y'          YES- TOP OF PAGE?                            
         BE    SPLAT8                                                           
         B     SPLAT7              ALWAYS PRINT LINE 1                          
*                                                                               
SPLAT6   CLC   0(132,R2),SPACES    NOW RELEASE THE PRINT LINES                  
         BE    SPLAT8                                                           
*                                                                               
SPLAT7   CLI   TOPSW,C'Y'          IF WE ARE AT 'TOP' OF PAGE                   
         BNE   *+8                                                              
         BAS   RE,CONTIN           CHECK FOR CONTINUATION                       
         MVI   TOPSW,C'N'                                                       
         MVI   REPPRNSA,1          SINGLE SPACE                                 
         BAS   RE,PRINTOUT                                                      
*                                                                               
SPLAT8   LA    R2,132(R2)                                                       
         BCT   R0,SPLAT6                                                        
*                                                                               
         CLI   NLINES,1            ANY EXTRA LINES NEEDED?                      
         BE    SPLATX                                                           
         CLI   TOPSW,C'Y'          YES- STILL AT TOP OF PAGE?                   
         BE    SPLATX                  YES-THEN DON'T BOTHER                    
         IC    R0,NLINES                                                        
         BCTR  R0,0                                                             
         STC   R0,REPPRNSA                                                      
         BAS   RE,PRINTOUT                                                      
*                                                                               
SPLATX   MVI   REPACTN,REPACLR     CLEAR ALL PRINT LINES                        
         GOTO1 REPORT,(R8)                                                      
         CLI   REPERRS,0                                                        
         BE    EXIT                                                             
         DC    H'0'                                                             
         EJECT                                                                  
*              PUT LINES ON SCREEN FROM PRINT BUFFER                            
         SPACE 2                                                                
SCRNFILL NTR1                                                                   
*                                                                               
         L     R2,ASCRFLDH         CURRENT SCREEN FIELD HEADER                  
SCRN10   CLI   0(R2),0             END OF TWA?                                  
         BE    SCRNX               YES - CAN'T DISPLAY ANY MORE                 
         CLC   0(132,R3),SPACES    ANYTHING ELSE TO DISPLAY?                    
         BE    SCRNX               NO                                           
         L     R1,GLWPAPER         WIDTH OF DATA LINES ON SCREEN                
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),0(R3)       FILL IN HEADINGS                             
         OI    REPIND1,REPIPUT     PRETEND A LINE WAS PRINTED                   
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ST    R2,ASCRFLDH         BUMP SCREEN FIELD POINTER                    
         LA    R3,132(R3)                                                       
         B     SCRN10                                                           
*                                                                               
SCRNX    B     EXIT                                                             
         EJECT                                                                  
*              ROUTINES TO HANDLE CONTINUATION ON NEXT PAGE                     
         SPACE 2                                                                
CONTIN   NTR1                                                                   
*                                                                               
         L     R2,GLATHID                                                       
         USING DLRECD,R2                                                        
         CLI   CONTSW,C'Y'         IS CONTINUATION RELEVANT?                    
         BNE   EXIT                                                             
         MVI   CONTSW,C'N'                                                      
         CLI   LINETYPE,C'T'       NOT APPLICABLE FOR TOTAL LINES               
         BE    EXIT                                                             
*                                                                               
         L     RE,REPAPRNT         A(FIRST PRINT LINE)                          
         OC    0(132,RE),LASTPDET  MAKE SURE WE GET IT ALL                      
         MVC   132(132,RE),0(RE)   MOVE FIRST PRTLINE TO SECOND PRTLINE         
         MVC   0(132,RE),SPACES                                                 
         CLI   DLRCTWID,0                                                       
         BE    EXIT                                                             
*                                                                               
         L     RE,REPAPRNT                                                      
         ZIC   R0,DLRCTDSP         PICK UP CONTINUATION DISPLACEMENT            
         AR    RE,R0                                                            
         ZIC   RF,DLRPDISP                                                      
         LA    RE,1(RF,RE)                                                      
         MVC   0(4,RE),=C'CONT'    MOVE IN APPROPRIATE MESSAGE                  
         CLI   DLRCTWID,6                                                       
         BL    EXIT                                                             
         MVC   0(6,RE),=C'(CONT)'                                               
         CLI   DLRCTWID,10                                                      
         BL    EXIT                                                             
         MVC   0(9,RE),=C'CONTINUED'                                            
         CLI   DLRCTWID,12                                                      
         BL    EXIT                                                             
         MVC   0(11,RE),=C'(CONTINUED)'                                         
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*              HEAD HOOK ROUTINES                                               
         SPACE 2                                                                
*                                                                               
HOOK     NTR1                                                                   
*                                                                               
         LA    RF,IOC              PICK UP DLRECD FOR ACTIVE DETAIL             
         L     R2,GLADTAB          A(DRIVE TABLE)                               
HOOK5    CLI   0(R2),X'10'         LOOK FOR RECORD ELEMENT                      
         BNE   *+14                IN DRIVE TABLE                               
         USING DLRECD,R2                                                        
         CLC   DLRECNUM,0(RF)      THAT MATCHES                                 
         BE    HOOK8                                                            
*                                                                               
         CLI   0(R2),0                                                          
         BNE   *+6                                                              
         DC    H'0'                NO REC ELEMENT FOUND                         
         ZIC   R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     HOOK5                                                            
*                                                                               
HOOK8    L     R3,DLRAFOUT         SO WE CAN GET TO THE OUTS                    
*                                                                               
HOUT12   CLI   0(R3),X'12'         END OF ROWS?                                 
         BE    HOOKEND             YES                                          
         CLI   0(R3),X'40'         HEAD?                                        
         BE    HEAD0               YES                                          
         CLI   0(R3),X'30'         OUT?                                         
         BNE   HOOKEND             NO                                           
*                                                                               
         ST    R3,ALAST30                                                       
         USING DLOD,R3                                                          
         MVC   LASTLTYP,DLOLTYP    SAVE LINE TYPE FOR HEAD ROUTES               
*                                                                               
         MVI   OUTYORN,C'Y'                                                     
         CLI   DLOLTYP,C'H'        ONLY INTERESTED IN HEADS                     
         BNE   HOOKEND                                                          
         EJECT                                                                  
*              DEAL WITH OUT RELATED TEXTS                                      
         SPACE 2                                                                
         ST    R3,GLADTENT                                                      
         L     R5,DLOAPOS          R5=A(OUTPUT)                                 
         CLI   DLOELEN,DLODLENQ    DO WE NEED TO HANDLE A LITERAL               
         BE    HOUT36                                                           
         ZIC   R1,DLOELEN          YES                                          
         SH    R1,=Y(DLODLENQ)                                                  
         STC   R1,LITLEN           LENGTH OF LITERAL                            
         CLC   LITLEN,DLOLEN       WILL IT FIT?                                 
         BNH   *+10                YES                                          
         MVC   LITLEN,DLOLEN       NO -- FUDGE THE LENGTH                       
         LA    R1,DLOLIT                                                        
         ST    R1,ALITIN           ITS ADDRESS                                  
         ST    R5,ALITOUT          AND WHERE IT GOES                            
         BAS   RE,DOLIT                                                         
         L     R5,ALITOUT          R5=A(NEXT SPACE)                             
         LA    R5,1(R5)            LEAVE A BLANK                                
*                                                                               
HOUT36   ST    R5,GLAOFLD                                                       
         XC    GLAIFLD,GLAIFLD                                                  
         ICM   RF,15,DLOIADD                                                    
         BZ    HOUT38                                                           
*                                                                               
         LA    R0,IOA              A(THIS RECORD)                               
         LR    R1,R0                                                            
         USING DLIND,RF                                                         
         AH    R1,DLINDISP                                                      
         DROP  RF                                                               
         ST    R1,GLAIFLD                                                       
*                                                                               
HOUT38   OC    DLORADD,DLORADD     IS THERE A USER ROUTINE FOR THIS             
         BZ    HOUT40                                                           
         MVI   GLHOOK,GLROUT       YES SO SET UP FOR A HOOK                     
         MVC   GLLABEL,DLOROUT                                                  
         MVC   GLAROUT,DLORADD                                                  
         MVC   GLARGS,DLOARGS                                                   
         GOTO1 AGOHOOK                                                          
         B     HOOKEND                                                          
*                                                                               
HOUT40   BAS   RE,FORM                                                          
         B     HOOKEND                                                          
         EJECT                                                                  
*              DEAL WITH HEAD ELEMENTS                                          
         SPACE 2                                                                
HEAD0    CLI   OUTYORN,C'N'        IGNORE IF PREVIOUS OUT FAILED                
         BE    HOOKEND                                                          
         USING DLHDD,R3                                                         
         CLI   LASTLTYP,C'P'       ONLY INTERESTED IN P TYPE                    
         BNE   HOOKEND                                                          
         TM    DLHDFLAG,DLHDFLDE   IGNORE DELETED HEADS                         
         BO    HOOKEND                                                          
*                                                                               
         ST    R3,GLADTENT                                                      
         L     R5,DLHDAPOS         R5=A(OUTPUT)                                 
         SR    R1,R1                                                            
         ICM   R1,1,GLFHLOVR       FIRST HEAD LINE OVERRIDE?                    
         BZ    HEAD1A                                                           
         ZIC   RE,GLFHEADL         YES - ADJUST R5 TO NEW A(OUTPUT)             
         SR    R1,RE                                                            
         BNP   HEAD1A                                                           
         M     R0,=F'132'                                                       
         AR    R5,R1                                                            
*                                                                               
HEAD1A   ST    R5,AHEAD            SAVE A(OUTPUT)                               
         CLI   DLHDELEN,DLHDLENQ   DO WE NEED TO HANDLE A LITERAL               
         BE    HEAD2A                                                           
         ZIC   R1,DLHDELEN         YES                                          
         SH    R1,=Y(DLHDLENQ)                                                  
         STC   R1,LITLEN           LENGTH OF LITERAL                            
         CLC   LITLEN,DLHDWDTH     WILL IT FIT?                                 
         BNH   *+10                YES                                          
         MVC   LITLEN,DLHDWDTH     NO -- FUDGE THE LENGTH                       
         LA    R1,DLHDLIT                                                       
         ST    R1,ALITIN           ITS ADDRESS                                  
         ST    R5,ALITOUT          AND WHERE IT GOES                            
         BAS   RE,DOLIT                                                         
         L     R5,ALITOUT          R5=A(NEXT SPACE)                             
         LA    R5,1(R5)            LEAVE A BLANK                                
*                                                                               
HEAD2A   ST    R5,GLAOFLD                                                       
         XC    GLAIFLD,GLAIFLD                                                  
         L     R1,ALAST30                                                       
         USING DLOD,R1                                                          
         ICM   RF,15,DLOIADD       PICK UP A(IN) IF ANY                         
         BZ    HEAD3A                                                           
*                                                                               
         LA    R0,IOA              A(THIS RECORD)                               
         LR    R1,R0                                                            
         USING DLIND,RF                                                         
         AH    R1,DLINDISP                                                      
         DROP  RF                                                               
         ST    R1,GLAIFLD          RECORD A(INPUT FIELD IN RECORD)              
*                                                                               
HEAD3A   OC    DLHDRADD,DLHDRADD   IS THERE A USER ROUTINE FOR THIS             
         BZ    HEAD8A                                                           
         MVI   GLHOOK,GLROUT       YES SO SET UP FOR A HOOK                     
         MVC   GLLABEL,DLHDROUT                                                 
         MVC   GLAROUT,DLHDRADD                                                 
         MVC   GLARGS,DLHDARGS                                                  
         GOTO1 AGOHOOK                                                          
*                                                                               
HEAD8A   CLI   DLHDALIN,C'L'       IF LEFT ALIGN, WE'RE DONE                    
         BE    HOOKEND                                                          
         L     R1,AHEAD                                                         
         ZIC   R0,DLHDWDTH                                                      
         LTR   R0,R0                                                            
         BZ    HOOKEND                                                          
         LR    RE,R0                                                            
         BCTR  RE,0                                                             
*                                                                               
HOOK10   CLI   0(R1),C' '          LEFT ALIGN FIRST                             
         BNE   HOOK12                                                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),1(R1)                                                    
         BCT   R0,HOOK10                                                        
*                                                                               
HOOK12   L     RF,CENTER           DEFAULT IS TO CENTER                         
         CLI   DLHDALIN,C'R'       OPTION TO RIGHT ALIGN                        
         BNE   *+8                                                              
         L     RF,RIGHT                                                         
         MVC   DMCB(4),AHEAD       PASS PRINT POSTION                           
         XC    DMCB+4(4),DMCB+4                                                 
         MVC   DMCB+7(1),DLHDWDTH  AND WIDTH                                    
         GOTO1 (RF),DMCB                                                        
*                                                                               
HOOKEND  ZIC   R1,1(R3)                                                         
         AR    R3,R1                                                            
         CLI   0(R3),X'00'                                                      
         BE    *+12                                                             
         CLI   0(R3),X'10'                                                      
         BNE   HOUT12                                                           
*                                                                               
         MVI   GLHOOK,GLHEAD                                                    
         MVC   GLADTENT,DLRAFOUT                                                
         XC    GLAROUT,GLAROUT                                                  
         MVI   GLAROUT,1                                                        
         GOTO1 AGOHOOK             FINALLY HOOK TO APPLICATION                  
         MVI   GLAROUT,2                                                        
         GOTO1 AGOHOOK             AND SYSTEM CONTROLLER                        
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*              DEAL WITH COMPUTES                                               
         SPACE 2                                                                
*                                  R5=A(RECORD TO BE COMPUTED)                  
*                                  R2=A(RELATED REC ELEMENT)                    
*                                  R0=A(COMP ELEMENT)                           
*                                                                               
DOCOMPUT NTR1                                                                   
*                                                                               
         LR    R4,R0                                                            
         USING DLCMD,R4                                                         
*                                  RESULT WILL BE RETURNED IN DUB               
         ZAP   CDUB,=P'0'                                                       
         ZAP   DUB,=P'0'                                                        
         ZIC   R0,DLCMNEXP         R0=NUMBER OF EXPRESSIONS                     
         LA    R4,DLCMEXP          NOW R4 WILL ADDRESS AN EXPRESSION            
         USING DLCMEXP,R4                                                       
*                                                                               
DOCOMP20 CLI   DLCMTYPE,1          OPERAND IS IN RECORD                         
         BNE   *+16                                                             
         L     R1,DLCMLIT          SELF CONTAINED (BINARY) LITERAL              
         CVD   R1,DUB                                                           
         B     DOCOMP60                                                         
*                                                                               
         L     R3,DLCMINAD         PICK UP ASSOCIATED IN                        
         LTR   R3,R3                                                            
         BZ    DOCOMPX             NOT PRESENT                                  
*                                                                               
         USING DLIND,R3                                                         
         LH    RE,DLINDISP                                                      
         AR    RE,R5               AND DISPLACE TO INPUT FIELD                  
         CLI   DLCMOP,C'V'         FOR V WE NEED TO FIND SPECIFIC LEVEL         
         BNE   DOCOMP40                                                         
*                                                                               
         CLC   1(1,R5),DLCMSUB     IF THE LEVEL OF TOTALS IS HIGHER             
         BL    BADCOMP             (LOWER NUMBER) THAN V% LEVEL, IGNORE         
         ZIC   R1,DLCMSUB                                                       
         BCTR  R1,0                                                             
         SLL   R1,1                                                             
         USING DLRECD,R2                                                        
         LA    R1,DLRRECNM(R1)     R1=A(TSAR RECORD NUM AT THIS LEVEL)          
         DROP  R2                                                               
         OC    0(2,R1),0(R1)       RECORD EXISTS AT THIS LEVEL?                 
         BZ    BADCOMP             NO                                           
*                                                                               
         MVC   FULL(2),0(R1)                                                    
         L     R1,ATSARD                                                        
         USING TSARD,R1                                                         
         LA    RF,IOB                                                           
         ST    RF,TSAREC                                                        
         MVC   TSRNUM,FULL                                                      
         MVI   TSACTN,TSAGET                                                    
         GOTO1 TSAR                GET THE RECORD BACK FROM TSAR                
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R1                                                               
*                                                                               
         LA    RF,IOB+2            BUMP PAST CONTROL BYTES                      
         AH    RF,BGKEYLEN         COPY RECORD NUMBER AND LEVEL. . .            
         MVC   IOB(2),0(RF)        . . . INTO FIRST TWO BYTES                   
*                                                                               
         CLI   IOB,0                                                            
         BE    BADCOMP                                                          
         CLC   IOB(1),0(R5)        AND BOTH S/B FOR SAME REC                    
         BNE   BADCOMP                                                          
*                                                                               
         LH    RE,DLINDISP                                                      
         LA    RE,IOB(RE)          RE = A(INPUT FIELD IN TOTAL RECORD)          
*                                                                               
DOCOMP40 ZIC   R1,0(RE)            BINARY DATA -- CONVERT TO DUB                
         CLI   DLINFLEN,1                                                       
         BE    COMPBEND                                                         
         LH    R1,0(RE)                                                         
         CLI   DLINFLEN,2                                                       
         BE    COMPBEND                                                         
         L     R1,0(RE)                                                         
         CLI   DLINFLEN,4                                                       
         BE    COMPBEND                                                         
         CLI   DLINFLEN,3                                                       
         BNE   COMPBEND                                                         
         SRA   R1,8                                                             
         DROP  R3                                                               
*                                                                               
COMPBEND CVD   R1,DUB                                                           
*                                                                               
DOCOMP60 CLI   DLCMOP,C'='         CARRY OUT THE COMPUTE                        
         BE    COMPEQU                                                          
         CLI   DLCMOP,0                                                         
         BE    COMPEQU                                                          
         CLI   DLCMOP,C'+'                                                      
         BE    COMPADD                                                          
         CLI   DLCMOP,C'-'                                                      
         BE    COMPSUB                                                          
         CLI   DLCMOP,C'X'                                                      
         BE    COMPMULT                                                         
         CLI   DLCMOP,C'/'                                                      
         BE    COMPDIV                                                          
         CLI   DLCMOP,C'I'                                                      
         BE    COMPIX                                                           
         CLI   DLCMOP,C'%'                                                      
         BE    COMPPCT                                                          
         CLI   DLCMOP,C'V'                                                      
         BE    COMPPCT                                                          
         DC    H'0'                                                             
*                                                                               
COMPEQU  ZAP   CDUB,DUB            EQUALS                                       
         B     COMPNEXT                                                         
*                                                                               
COMPADD  AP    CDUB,DUB            ADD                                          
         B     COMPNEXT                                                         
*                                                                               
COMPSUB  SP    CDUB,DUB            SUBTRACT                                     
         B     COMPNEXT                                                         
*                                                                               
COMPMULT MP    CDUB,DUB            MULTIPLY                                     
         B     COMPNEXT                                                         
*                                                                               
COMPDIV  ZAP   FACTOR,=P'2'        DIVIDE                                       
         B     COMPDIV2                                                         
*                                                                               
COMPIX   ZAP   FACTOR,=P'200'      INDEX                                        
         B     COMPDIV2                                                         
*                                                                               
COMPPCT  ZAP   FACTOR,=P'20000'    PERCENT                                      
*                                                                               
COMPDIV2 CP    DUB,=P'0'                                                        
         BNE   *+14                                                             
         ZAP   CDUB,=P'0'                                                       
         B     COMPNEXT                                                         
*                                                                               
         MP    CDUB,FACTOR                                                      
         ZAP   BIG,CDUB                                                         
         DP    BIG,DUB                                                          
         ZAP   CDUB,BIG(8)         ROUND                                        
         BM    *+10                                                             
         AP    CDUB,=P'1'                                                       
         ZAP   BIG,CDUB                                                         
         DP    BIG,=PL8'2'                                                      
         ZAP   CDUB,BIG(8)                                                      
*                                                                               
COMPNEXT LA    R4,L'DLCMEXP(R4)                                                 
         BCT   R0,DOCOMP20                                                      
         ZAP   DUB,CDUB                                                         
         B     DOCOMPX                                                          
         DROP  R4                                                               
*                                                                               
BADCOMP  ZAP   DUB,=P'0'                                                        
*                                                                               
DOCOMPX  XC    IOB,IOB                                                          
         B     EXIT                                                             
         EJECT                                                                  
*              TRACE AND PRINTING FACILITIES                                    
         SPACE 2                                                                
TRACEOUT NTR1                                                                   
*                                                                               
         CLI   GLTRACE,C'Y'                                                     
         BNE   EXIT                                                             
         OC    ATWAFLDH,ATWAFLDH   ON-SCREEN REPORT?                            
         BNZ   EXIT                YES -- NO TRACE POSSIBLE                     
*                                                                               
         L     R2,GLAIO                                                         
         L     R3,REPAPRNT                                                      
         MVC   0(132,R3),0(R2)                                                  
         LA    R3,132(R3)                                                       
         GOTO1 HEXOUT,DMCB,(R2),(R3),132,=C'SEP'                                
         OC    DMCB+16(4),DMCB+16                                               
         BNZ   *+6                                                              
         DC    H'0'                                                             
         XC    REPAUSR,REPAUSR     NO HEAD HOOK                                 
         BAS   RE,PRINTOUT                                                      
         BAS   RE,PRINTOUT                                                      
         LA    RE,HOOK             RESTORE HEAD HOOK ADDRESS                    
         ST    RE,REPAUSR                                                       
         B     EXIT                                                             
         SPACE 3                                                                
TRACB    NTR1                                                                   
*                                                                               
         CLI   GLTRACE,C'Y'                                                     
         BNE   EXIT                                                             
         OC    ATWAFLDH,ATWAFLDH   ON-SCREEN REPORT?                            
         BNZ   EXIT                YES -- NO TRACE POSSIBLE                     
*                                                                               
         L     R3,REPAPRNT                                                      
         MVC   0(9,R3),=C'CB LEVEL='                                            
         LA    R3,9(R3)                                                         
         EDIT  CBLEV,(2,(R3)),ALIGN=LEFT,ZERO=NOBLANK                           
         XC    REPAUSR,REPAUSR     NO HEAD HOOK                                 
         BAS   RE,PRINTOUT                                                      
         LA    RE,HOOK             RESTORE HEAD HOOK ADDRESS                    
         ST    RE,REPAUSR                                                       
         B     EXIT                                                             
         SPACE 3                                                                
PRINTOUT NTR1                                                                   
*                                                                               
         MVI   REPACTN,REPAPUT     PRINT REPORT BUFFER                          
         GOTO1 REPORT,(R8)                                                      
         CLI   REPERRS,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
SPACES   DC    CL132' '                                                         
         EJECT                                                                  
         DROP  RC,RB,R9                                                         
         SPACE 2                                                                
*              HOOK TO APPLICATION OR SYSTEM DRIVER                             
         SPACE 2                                                                
GOHOOK   DS    0H                                                               
         SPACE 2                                                                
         NMOD1 0,**DRHK**                                                       
*                                                                               
         CLI   GLAROUT,0                                                        
         BE    GOHX                                                             
         CLI   GLAROUT,2           FIRST BYTE INDICATES WHERE                   
         BE    GOH2                1=APPLICATION 2=SYSDRIVER 3=DROOL            
         CLI   GLAROUT,3                                                        
         BE    GOH3                                                             
         CLI   GLAROUT,1                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ICM   RF,15,GLAHOOK       PICK UP APPLICATION HOOK                     
         BZ    GOHX                                                             
         L     RE,USERRD           USERS RD                                     
         LM    R0,RC,20(RE)        RESTORE USERS R0-RC                          
         BASR  RE,RF               RF CONTAINED A(HOOK ROUTINE)                 
GOHX     XIT1                                                                   
*                                                                               
GOH2     ICM   RF,15,GLASYSDR      A(SYSTEM DRIVER)                             
         BZ    GOHX                                                             
         GOTO1 (RF),DMCB,(RA)                                                   
         B     GOHX                                                             
*                                                                               
GOH3     CLC   GLLABEL,=CL8'RANK'  RANK ROUTINE?                                
         BNE   GOHX                NO -- OTHER ROUTINES NOT SUPPORTED           
         CLI   GLHOOK,GLRESOLV                                                  
         BNE   *+16                                                             
         LA    RF,RANK             RESOLVE RANK ROUTINE ADDRESS                 
         ST    RF,GLAROUT                                                       
         B     GOHX                                                             
*                                                                               
         CLI   GLHOOK,GLROUT                                                    
         BNE   GOHX                                                             
         L     RF,GLAROUT          EXECUTE RANK ROUTINE                         
         BASR  RE,RF                                                            
         B     GOHX                                                             
         EJECT                                                                  
*              USER ROUTINES CONTAINED WITHIN DROOL                             
         SPACE 2                                                                
RANK     NTR1                                                                   
*                                                                               
         CLI   GLMODE,GLOUTPUT     OUTPUT ROUTINE?                              
         BE    RANK10              YES                                          
*                                                                               
         L     R2,GLAIFLD          R2=A(INPUT)                                  
         MVC   0(4,R2),=X'FFFFFFFF' FORCE TO HIGH VALUES FOR TSAR               
*                                                                               
         CLI   GLARGS,C'C'         REVERSE RANKING ORDER?                       
         BNE   GOHX                                                             
         L     R2,GLATHID                                                       
         USING DLRECD,R2                                                        
         OI    DLRFLAGS,DLRRNKCQ   YES -- WE WON'T COMPLEMENT LATER             
         DROP  R2                                                               
         B     GOHX                                                             
*                                                                               
* OUTPUT OF RANK NUMBERS IS NOT YET SUPPORTED                                   
*                                                                               
RANK10   B     GOHX                *** TEMPORARY ***                            
         EJECT                                                                  
         LTORG                                                                  
         SPACE 3                                                                
*              WORKING STORAGE                                                  
         SPACE 2                                                                
WORKD3   DSECT                                                                  
*                                                                               
ALEVOUT  DS    A                                                                
AHEAD    DS    A                                                                
ALITIN   DS    A                                                                
ALITOUT  DS    A                                                                
ALAST30  DS    A                                                                
ASCRFLDH DS    A                                                                
TSRECNUM DS    H                                                                
TSRECNO2 DS    H                                                                
OUTYORN  DS    C                                                                
LASTTYPE DS    X                                                                
CBLEV    DS    X                                                                
TOPSW    DS    C                                                                
BREAK    DS    C                                                                
CONTSW   DS    C                                                                
LASTLTYP DS    C                                                                
TOTPEND  DS    C                                                                
DONTSW   DS    C                                                                
AUTALLSW DS    C                                                                
LITLEN   DS    C                                                                
NLINES   DS    X                                                                
LINETYPE DS    C                                                                
COMPSTAT DS    X                   1 IF NO COMPUTES, 2 IF COMPUTES OK           
CDUB     DS    CL16                                                             
BIG      DS    CL16                                                             
FACTOR   DS    PL6                                                              
TOTCNTS  DS    XL50                                                             
IOA      DS    XL256               MAIN TSAR RECORD BUILDING AREA               
IOB      DS    XL256               GENERAL SAVE AREA                            
IOC      DS    XL256               SAVE DETAIL RECORDS HERE                     
SAVEDET  DS    CL132                                                            
SAVEP2   DS    CL132                                                            
LASTPDET DS    CL132                                                            
LASTP2   DS    CL132                                                            
*                                                                               
WORKDX3  EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE DDDROOLD                                                       
         EJECT                                                                  
       ++INCLUDE DRGLOBAL                                                       
         SPACE 5                                                                
       ++INCLUDE DROOLLOCAL                                                     
         EJECT                                                                  
       ++INCLUDE DROOLTABLE                                                     
         EJECT                                                                  
       ++INCLUDE DDTSARD                                                        
         EJECT                                                                  
REPBLKD  DSECT                                                                  
       ++INCLUDE FAREPBLK                                                       
         EJECT                                                                  
* CTGENDIC                                                                      
* FAFACTS                                                                       
* DDCOMFACS                                                                     
* DDCOREQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENDIC                                                       
         EJECT                                                                  
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE DDCOREQUS                                                      
         EJECT                                                                  
EBLOCKD  DSECT                                                                  
       ++INCLUDE DDEBLOCK                                                       
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'070DDDROOL   05/01/02'                                      
         END                                                                    
