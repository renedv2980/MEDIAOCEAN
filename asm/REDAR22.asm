*          DATA SET REDAR22    AT LEVEL 032 AS OF 12/13/04                      
*PHASE T80F22C                                                                  
*&&      SET   DB=N,TK=Y                                                        
         TITLE 'T80F22 - REDAR22 - DARE REVISION PROCESSING'                    
***********************************************************************         
*                                                                     *         
*  REDAR22 (T80F22) --- DARE REVISION APPROVE/REJECT                  *         
*                                                                     *         
*  NOTE: R7 IS RESERVED AS BASE REGISTER FOR COMMON ROUTINES          *         
* ------------------------------------------------------------------- *         
* UPDATE HISTORY:                                                     *         
*                                                                     *         
* 13DEC04 SKU REMOVE B2/B3 ELEMENTS                                   *         
* 20AUG04 HQ  HANDLE PASSIVE KEYS                                     *         
* 19AUG04 SKU DAILY PACING                                            *         
* 28APR04 HQ  STOP ALL ACTIONS ON NON-UPDATEABLE SYSTEM               *         
* 15APR04 HQ  DELETE PARTIAL CONFIRM COMMENT ON OPEN                  *         
* 24MAR04 HQ  SKIP DEMO VALUE THAT HAS NULL CATEGORY                  *         
* 08JAN03 HQ  SKIP DELETED 41 RECORDS WHEN CONSTRUCTING 0B01 RECORD   *         
* 03OCT03 HQ  POPULATE COST OVERRIDE FLAG ON OPEN                     *         
* 14JUL03 HQ  CLEAR TSARREC BEFORE READING TSAR RECORD IN GETNDX      *         
*             CLEAR TBLOCK FOR TSAR INITIALIZATION                    *         
* 11JUL03 HQ  FIX ADD-TO-SCHEDUL BUY MOD #                            *         
* 09JUN03 SKU CLEAR OFFERS IF PENDING DEMO CATEGORY CHANGES           *         
* 24JAN03 SKU SUPPORT ALPHANUMERIC ESTIMATE                           *         
* 17JUL02 SKU DEMO RATING SUPPORT                                     *         
* 20MAY02 HQ  ADD SCROLL BACK AND *ONLY VIEW FOR BUYLINES             *         
* 12FEB02 HQ  BUGFIX: DIFF OPEN LOGIC READING DELETED SHADOW BUY REC  *         
* 04APR01 SKU CONCEPTION                                              *         
*                    ***  END TOMBSTONE  ***                          *         
***********************************************************************         
* EXPLANATION OF TERMINOLOGIES USED:                                  *         
*                                                                     *         
* A BUY SEGMENT IS DEFINED AS SPOT(S) BOUGHT FOR A PARTICULAR WEEK.   *         
* IT HAS THE FOLLOWING FORMAT:                                        *         
* DAY/TIMES/LENGTH/RATE/PROGRAM NAME/WEEK-OF                          *         
*                                                                     *         
* FOR EACH PARTICULAR BUY SEGMENT, THE SEGMENT CAN POINT (LINK) TO    *         
* A MAXIMUM OF TWO BUY-LINK RECORDS, ONE FOR THE REP AND ONE FOR THE  *         
* AGENCY. A BUY-LINK RECORD IS A LIST OF BUY LINES THAT CONTAINS      *         
* SPOTS AS DESCRIBED BY THE ATTACHED BUY SEGMENT. THE LIST ALSO       *         
* INCLUDES THE NUMBER OF SPOTS NEXT TO EACH BUY LINE. IF A BUY        *         
* SEGMENT IS ONLY MATCHED WITH ONE BUYLINE (REP OR AGY), THE ACTUAL   *         
* BUY NUMBER AND SPOTS ARE KEPT IN THE BUY SEGMENT ITSELF IN LIEU OF  *         
* A POINTER TO A BUY-LINK RECORD.                                     *         
*                                                                     *         
***********************************************************************         
T80F22   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKLQ,*T80F22*,RR=R3                                            
         LR    R5,RC                                                            
         USING MYAREAD,R5                                                       
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         LR    R7,RB                                                            
         A     R7,=A(COMMON-T80F22)                                             
         USING COMMON,R7           COMMON ROUTINES                              
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
*                                                                               
         CLI   TWAOFFC,C'*'        DDS TERMINAL - ALLOW ACTION                  
         BE    MAIN05                                                           
         GOTO1 (RFCHKSYS,REPFACS),DMCB,ACOMFACS                                 
         BE    MAIN05                                                           
         MVC   RERROR,=AL2(991)    SET ERROR TYPE = NOT UPDATEABLE              
         LA    R2,AORHDLNH         SET CURSOR HERE                              
         GOTO1 MYERROR                                                          
*                                                                               
MAIN05   DS    0H                                                               
         TM    FLAGS2,FIRSTQ       FIRST TIME, CLEAR SCREEN NUMBER              
         BO    *+14                                                             
         OI    FLAGS2,FIRSTQ                                                    
         XC    SCRNUM,SCRNUM                                                    
*                                                                               
TB       USING TSARD,TBLOCK        TSAR BLOCK                                   
TR       USING TLSTD,TSARREC       TSAR RECORD                                  
*                                                                               
         MVC   PRTSTAT,4(R1)       PRINT STATUS, IF ANY                         
         CLI   PRTSTAT,0           PRINT REQUESTED                              
         BE    MAIN10                                                           
         OI    GENSTAT1,RDUPAPPL   ALLOW UPDATE FOR ACTION LIST                 
         B     VALRECRD                                                         
*                                                                               
* INCASE OF RETURN FROM GLOBBER, NEEDS TO RETRANSMIT ENTIRE SCREEN              
*                                                                               
MAIN10   DS    0H                                                               
         GOTO1 =A(CKGLOB),RR=RELO  CHECK IF WE CAME BACK FROM CONTRACT          
*                                                                               
         MVC   DIFLAST+1(2),=X'0101'                                            
         CLC   =C'DIF',CONACT      IF APP OR REJ                                
         BE    MAIN20              RETRANSMIT DIFFERENT SCREEN                  
         MVC   AORLAST+1(2),=X'0101'                                            
*                                                                               
MAIN20   DS    0H                                                               
         BAS   RE,SETPFKYS         SETUP THE PFKEYS                             
                                                                                
         CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VK                  YES                                          
         CLI   MODE,VALREC         VALIDATE RECORD?                             
         BE    VALRECRD            YES                                          
*                                  NO OTHER ACTIONS RECOGNIZED                  
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY                                                                  
***********************************************************************         
VK       DS    0H                                                               
         CLI   CALLSP,0            MUST BE CALLED TO GET HERE                   
         BNE   VKX                                                              
         LA    R2,CONRECH                                                       
         B     INVLRCAC            INVALID REC/ACTION                           
*                                                                               
VKX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE RECORD                                                               
***********************************************************************         
VALRECRD DS    0H                                                               
         CLI   PRTSTAT,0                                                        
         BNE   VR10                                                             
         MVI   ACTCODE,C'R'        REJECTION?                                   
         MVI   MYSCRNUM,X'F8'      SET SCREEN NUMBER                            
         CLC   =C'REJ',CONACT                                                   
         BE    VR10                                                             
         MVI   ACTCODE,0                                                        
         MVI   MYSCRNUM,X'E8'                                                   
*                                                                               
VR10     DS    0H                                                               
         TM    PRTSTAT,PRTCLOSE    CLOSE THE PRINTQ?                            
         BO    VR50                NO                                           
*                                                                               
         MVC   KEY(L'SELECTKY),SELECTKY                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         MVC   AIO,AIO1                                                         
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
*                                                                               
         CLI   PFKEY,10            APPROVE?                                     
         BNE   VR20                                                             
         CLI   RDARBSTS,C'A'                                                    
         BE    WASAPR                                                           
         B     VR30                                                             
*                                                                               
VR20     DS    0H                                                               
         CLI   PFKEY,11            REJECT?                                      
         BNE   VR30                                                             
         CLI   RDARBSTS,C'R'                                                    
         BE    WASREJ                                                           
*                                                                               
* READ STATION RECORD FOR REVISION AUTO METHOD OVERRIDE, IF ANY                 
* DEFAULT IF ADD SPOT TO NEW LINES                                              
* NOTE THAT METHOD 1, CANCEL AND SUPERSEDE, IS NO LONGER SUPPORTED              
*                                                                               
VR30     DS    0H                                                               
         XC    ELTBUILD,ELTBUILD                                                
         MVC   ELTBUILD+8(4),RDARKSTA                                           
         MVI   ELTBUILD+5,4                                                     
         CLI   RDARKSTA+3,C' '                                                  
         BNE   *+8                                                              
         MVI   ELTBUILD+5,3        IF 3 LETTER CALL LETTERS                     
         MVI   ELTBUILD+2,X'40'    SET ALPHA                                    
         CLI   RDARKSTA+4,C'T'      NO NEED FOR TV BAND                         
         BE    VR35                                                             
         MVI   ELTBUILD+12,C'-'                                                 
         MVC   ELTBUILD+13(1),RDARKSTA+4                                        
         MVI   ELTBUILD+5,6                                                     
         CLI   RDARKSTA+3,C' '                                                  
         BNE   VR35                                                             
         MVC   ELTBUILD+11(2),ELTBUILD+12                                       
         MVI   ELTBUILD+13,0       IF 3 LETTER CALL LETTERS                     
         MVI   ELTBUILD+5,5                                                     
                                                                                
VR35     DS    0H                                                               
         LA    R2,ELTBUILD                                                      
         GOTO1 VALISTA                                                          
*                                                                               
         ZAP   WORK+20(5),=P'0'                                                 
         MVO   WORK+20(5),RDARREP#                                              
         XC    ELEM,ELEM                                                        
         LA    R2,ELEM             SETUP FAKE FIELD HEADER                      
         EDIT  (P5,WORK+20),(8,8(R2)),ALIGN=LEFT TO CALL VCON WITH              
         STC   R0,5(R2)            SET LENGTH OF FIELD                          
         MVI   0(R2),16            FIELD HEADER LENGTH                          
         MVI   4(R2),X'08'         SET VALID NUMERIC                            
         GOTO1 VALICON,DMCB,(R2)   GET CONTRACT INFO AND STORE IT               
         GOTO1 =A(GETCON),RR=RELO  RETRIEVE CONTRACT RECORD                     
         BZ    VR40                EXIT:  NO CONTRACT NUMBER                    
*                                  ERROR IN GETCON:  MESSAGE CODE               
*                                     ALREADY SET IN RERROR                     
         LA    R2,CONACTH          SET CURSOR TO 'ACTION' FIELD                 
         B     ERREND              EXIT WITH ERROR                              
         DROP  R6                                                               
*                                                                               
VR40     DS    0H                                                               
         CLI   ACTCODE,C'R'        REJECTION?                                   
         BNE   VR45                                                             
         CLI   PFKEY,2             PROCESS REJECTION UNLESS WE ARE              
         BE    VR200               ACTUALLY TRYING TO JUMP TO CONTRACT          
         B     VR1100                                                           
*                                                                               
VR45     DS    0H                                                               
         TM    FLAGS,FGINITQ                                                    
         BO    VR60                                                             
*                                                                               
         LR    RE,RA               CHECK TO SEE IF LOCAL SIGN ON                
         AHI   RE,DARPROFS-CONHEADH                                             
         USING SVDSECT,RE                                                       
         TM    SVPGPBIT+CNTRPEAB,CNTRPEAA                                       
         BZ    VR47                                                             
         DROP  RE                                                               
*                                                                               
         CLI   SIGNONID+4,C'L'                                                  
         BE    VR48                                                             
VR47     DS    0H                                                               
         GOTO1 =A(SVAGYDEM),RR=RELO SAVE AGENCY DEMO CATEGORIES                 
*                                                                               
VR48     DS    0H                                                               
*                                                                               
         GOTO1 =A(SHADOW),RR=RELO  CONVERT DARE AGENCY BUYS TO                  
*                                  REPPAK BUYS WITH KEY X'0B01'                 
         GOTO1 =A(INITTSAR),RR=RELO                                             
*                                                                               
         GOTO1 =A(BLDTSREC),RR=RELO                                             
*                                                                               
*                                                                               
*        GOTO1 =A(DUMPTSAR),RR=RELO                                             
*                                                                               
         OI    FLAGS,FGINITQ                                                    
*                                                                               
         CLI   PRTSTAT,0           PRINT WORKSHEET                              
         BE    VR70                                                             
VR50     DS    0H                                                               
         GOTO1 =A(PR),RR=RELO                                                   
         GOTO1 VTOUCHED                                                         
         NI    FLAGS,X'FF'-FGINITQ RESET INCASE OF MULTIPLE PRINTS              
         B     EXIT                                                             
*                                                                               
VR60     DS    0H                                                               
         GOTOX GOTSAR,TSARES                                                    
*                                                                               
VR70     DS    0H                                                               
         CLI   PFKEY,2             USER WANTS TO SWITCH TO CONTRACT             
         BE    VR200                                                            
         CLI   PFKEY,3             TOGGLE DIFFERENCE DETAIL/SUMMARY             
         BE    VR300                                                            
         CLI   PFKEY,4                                                          
         BE    VR400                                                            
         CLI   PFKEY,5             PRINT REPORT                                 
         BE    VR500                                                            
         CLI   PFKEY,6             SCROLL BACK                                  
         BE    VR600                                                            
         CLI   PFKEY,7             PREVIOUS PAGE                                
         BE    VR700                                                            
         CLI   PFKEY,8             COLLAPSE/EXPAND                              
         BE    VR800                                                            
         CLI   PFKEY,9             REFRESH                                      
         BE    VR900                                                            
         CLI   PFKEY,10            APPROVE REVISION                             
         BE    VR1000                                                           
         CLI   PFKEY,11            REJECT REVISION                              
         BE    VR1100                                                           
*                                                                               
VR90     DS    0H                                                               
         GOTO1 =A(DIFFLIST),RR=RELO                                             
         GOTO1 =A(CHAPFLN),RR=RELO CHANGE PFKEY MENUS                           
         MVI   PFKEY,0                                                          
         OI    CONRECH+6,X'40'     FORCE CURSOR HERE                            
         B     VREND                                                            
*                                                                               
         EJECT                                                                  
***********************************************************************         
* PF2:SWAP TO CONTRACT                                                          
***********************************************************************         
VR200    DS    0H                                                               
         NI    FLAGS,X'FF'-FGINITQ SET TO REFRESH WHEN WE COME BACK             
         GOTO1 =A(SWAP2CON),RR=RELO                                             
         CLI   ACTCODE,C'R'        SAVE TSAR BUFFER UNLESS WE ARE               
         BE    EXIT                IN REJECTION SCREEN                          
         B     VREND                                                            
         EJECT                                                                  
***********************************************************************         
* PF3:TOGGLE BETWEEN SUMMARY AND DETAIL DIFFERENCES DISPLAY                     
***********************************************************************         
VR300    DS    0H                  TOGGLE BETWEEN DIFFERENCE                    
         XI    FLAGS,FGDETLQ                                                    
         XC    LISTSTRT,LISTSTRT                                                
         XC    LISTLAST,LISTLAST                                                
         XC    SCRNUM,SCRNUM                                                    
         XC    SCRLLTAB(MAXSCRLL*L'SCRLLTAB),SCRLLTAB                           
*        MVI   PFKEY,0                                                          
         B     VR90                                                             
         EJECT                                                                  
***********************************************************************         
* PF4:                                                                          
***********************************************************************         
VR400    DS    0H                                                               
         OI    CONRECH+6,X'40'     FORCE CURSOR HERE                            
         TM    FLAGS,FGDETLQ                                                    
         BZ    EXIT                                                             
         XI    FLAGS2,FGALLQ       OR *ONLY BUYLINE DISPLAY                     
         XC    LISTLAST,LISTLAST   GO BACK TO PAGE ONE                          
         XC    SCRNUM,SCRNUM                                                    
         XC    SCRLLTAB(MAXSCRLL*L'SCRLLTAB),SCRLLTAB                           
*                                                                               
*        MVI   PFKEY,0                                                          
         B     VR90                                                             
         EJECT                                                                  
***********************************************************************         
* PF5:REVISION WORKSHEET PRINT                                                  
*     NEVER GETS HERE. CALL GETS INTERCEPTED ABOVE                              
***********************************************************************         
VR500    DS    0H                                                               
         GOTO1 =A(PR),RR=RELO                                                   
         GOTO1 VTOUCHED                                                         
         B     VREND                                                            
***********************************************************************         
* PF6:                                                                          
***********************************************************************         
VR600    DS    0H                                                               
         B     VREND                                                            
***********************************************************************         
* PF7: SCROLL BACK TO PREVIOUS PAGE                                             
***********************************************************************         
VR700    DS    0H                                                               
         LA    RE,SCRLLTAB                                                      
         LH    RF,SCRNUM                                                        
         OI    DIFLISTH+6,X'40'     FORCE CURSOR HERE                           
         CHI   RF,1                 DO NOT SCROLL BACK ON 1ST SCREEN            
         BNH   EXITOK                                                           
*                                                                               
         OI    FLAGS2,SCRLBACK      MARK SCRLBACK STATUS                        
         SHI   RF,2                 GET PREVIOUS FIRST REC ON SCREEN            
         MHI   RF,L'SCRLLTAB                                                    
         AR    RE,RF                                                            
         MVC   LISTLAST,0(RE)                                                   
         GOTO1 =A(DIFFLIST),RR=RELO                                             
         OI    DIFLISTH+6,X'40'     FORCE CURSOR HERE                           
         LH    RF,SCRNUM            DECREASE CURRENT SCREEN NUM BY 1            
         BCTR  RF,0                                                             
         STH   RF,SCRNUM                                                        
         NI    FLAGS2,X'FF'-SCRLBACK                                            
         GOTO1 =A(CHAPFLN),RR=RELO                                              
         MVI   PFKEY,0                                                          
         XIT1                                                                   
***********************************************************************         
* PF8:                                                                          
***********************************************************************         
VR800    DS    0H                  TOGGLE BETWEEN EXPLODED                      
         XI    FLAGS,FGEXPLDQ      OR COLLAPSED DISPLAY                         
         MVC   LISTLAST,LISTSTRT   REDISPLAY PAGE                               
*        XC    LISTSTRT,LISTSTRT                                                
*                                                                               
         B     VR90                                                             
*        GOTO1 =A(DIFFLIST),RR=RELO                                             
*        GOTO1 =A(CHAPFLN),RR=RELO CHANGE PFKEY MENUS                           
*        MVI   PFKEY,0                                                          
*        OI    CONRECH+6,X'40'     FORCE CURSOR HERE                            
*        B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PF9: REFRESH DISPLAY VIA REREADING RECORDS TO TSAR BUFFER                     
***********************************************************************         
VR900    DS    0H                                                               
         NI    FLAGS,X'FF'-FGINITQ SET TO REBUILD TSAR RECORDS                  
         MVI   PFKEY,0                                                          
         XC    DIFATOT,DIFATOT     REDISPLAY TOTAL                              
         B     VR40                                                             
***********************************************************************         
* PF10: REVISION APPROVAL PROCESSING                                            
***********************************************************************         
VR1000   DS    0H                                                               
*                                                                               
* REBUILD TSAR RECORDS INCASE THERE ARE LAST MINUTE CHANGES TO THE              
* CONTRACT BUYS THAT WERE NOT REFLECTED IN THE DISPLAY                          
*                                                                               
         GOTO1 =A(INITTSAR),RR=RELO                                             
         GOTO1 =A(BLDTSREC),RR=RELO                                             
*                                                                               
         GOTO1 =A(PREAPRV),RR=RELO                                              
*        GOTO1 =A(DUMPTSAR),RR=RELO                                             
         GOTO1 =A(APPROVE),RR=RELO                                              
*                                                                               
         GOTO1 =A(DEMOPROC),RR=RELO                                             
         GOTOR DELCFC                                                           
         GOTO1 =A(TRAP),RR=RELO                                                 
         GOTO1 =A(POSTAPRJ),RR=RELO                                             
         B     VREND                                                            
         EJECT                                                                  
***********************************************************************         
* PF11: REVISION REJECTION                                                      
***********************************************************************         
VR1100   DS    0H                                                               
         MVC   AIO,AIO1                                                         
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         GOTO1 HEXOUT,DMCB,RDARREP#,AORHDLN,4,=C'TOG'                           
         OI    AORHDLNH+6,X'80'    XMIT                                         
         GOTO1 HEXOUT,DMCB,RDARKORD,AORAORD,4,=C'TOG'                           
         OI    AORAORDH+6,X'80'    XMIT                                         
*                                                                               
         LA    R2,CONRECH                                                       
         CLI   RDARBSTS,C'R'       REJECTED ALREADY?                            
         BE    WASREJ                                                           
         DROP  R6                                                               
*                                                                               
         LA    R2,AORREASH         YES - MUST HAVE AT LEAST 1 LINE              
         CLI   5(R2),0             ANYTHING ON FIRST LINE?                      
         BE    REJERR                                                           
*                                                                               
         GOTO1 =A(DATETIME),RR=RELO                                             
*                                  SEND MESSAGE: ORDER REJECTED                 
         GOTO1 =A(POSTAPRJ),RR=RELO                                             
*                                                                               
         OI    PRTSTAT,PRTONE                                                   
         NI    FLAGS,X'FF'-FGINITQ                                              
         B     VR45                                                             
*                                                                               
* SAVE OFF FOR RETRIEVAL                                                        
*                                                                               
VREND    DS    0H                                                               
         TM    FLAGS,FGTBSVDQ      TSAR BUFFER WAS SAVED?                       
         BO    EXIT                                                             
         GOTO1 GOTSAR,TSASAV                                                    
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* SET THE PFKEY INFORMATION                                                     
***********************************************************************         
SETPFKYS NTR1                                                                   
         SR    R2,R2               NO PFKEY AT TABLE FIRST                      
*                                                                               
***************                                                                 
* FOR ACTION APPROVE/REJECT                                                     
***************                                                                 
         CLI   ACTNUM,ACTDIF       ACTION DIFFERENCE?                           
         BE    STPFK10                                                          
         CLI   ACTNUM,ACTREJ       ACTION REJECT?                               
         BNE   STPFINIT                                                         
*                                                                               
STPFK10  LA    R2,SPFTABLE         YES, USE LIST PFKEY TABLE                    
*                                                                               
STPFINIT GOTO1 INITIAL,DMCB,(R2)   INITIALIZE THE PFKEYS                        
*                                                                               
         CLI   PFKEY,11            REJECT                                       
         BNE   STPFX                                                            
*                                                                               
         LA    R2,SPFTABLE                                                      
         ZIC   R1,PFKEY                                                         
         AHI   R1,12                                                            
         STC   R1,PFKEY                                                         
*                                                                               
         GOTO1 INITIAL,DMCB,(R2)                                                
*                                                                               
STPFX    B     EXIT                                                             
*                                                                               
***********************************************************************         
* APPROVE/REJECT PFKEY TABLE DEFINITIONS                                        
***********************************************************************         
SPFTABLE  DS    0C                                                              
*                                                                               
* JUMP TO THE CONTRACT PROGRAM                                                  
         DC    AL1(SPF02X-*,02,0,0,0,PFTRETRN)                                  
         DC    CL3' ',CL8' ',CL8' '                                             
SPF02X   EQU   *                                                                
*                                                                               
* TOGGLE BETWEEN SUMMARY AND DETAIL DISPLAY                                     
         DC    AL1(SPF03X-*,03,0,0,0,PFTRETRN)                                  
         DC    CL3' ',CL8' ',CL8' '                                             
SPF03X   EQU   *                                                                
*                                                                               
* TOGGLE BETWEEN *ONLY BUYLINES AND ALL BUYLINES                                
         DC    AL1(SPF04X-*,04,0,0,0,PFTRETRN)                                  
         DC    CL3' ',CL8' ',CL8' '                                             
SPF04X   EQU   *                                                                
*                                                                               
* PRINT ORDER WORKSHEET                                                         
         DC    AL1(SPF05X-*,05,0,0,0,PFTRETRN)                                  
         DC    CL3' ',CL8' ',CL8' '                                             
SPF05X   EQU   *                                                                
*                                                                               
* TOGGLE BETWEEN EXPLODED AND COLLASPED VIEW                                    
*                                                                               
         DC    AL1(SPF07X-*,07,0,0,0,PFTRETRN)                                  
         DC    CL3' ',CL8' ',CL8' '                                             
SPF07X   EQU   *                                                                
*                                                                               
* SCROLL BACK AND FORTH                                                         
         DC    AL1(SPF08X-*,08,0,0,0,PFTRETRN)                                  
         DC    CL3' ',CL8' ',CL8' '                                             
SPF08X   EQU   *                                                                
*                                                                               
* REFRESH DISPLAY BY REFRESHING READ TO TSAR BUFFER                             
         DC    AL1(SPF09X-*,09,0,0,0,PFTRETRN)                                  
         DC    CL3' ',CL8' ',CL8' '                                             
SPF09X   EQU   *                                                                
*                                                                               
* APPROVE REVISION ORDER                                                        
         DC    AL1(SPF10X-*,10,0,0,0,PFTRETRN)                                  
         DC    CL3' ',CL8' ',CL8' '                                             
SPF10X   EQU   *                                                                
*                                                                               
* REJECT REVISION ORDER                                                         
         DC    AL1(SPF11X-*,11,0,0,0,PFTRETRN)                                  
         DC    CL3' ',CL8' ',CL8' '                                             
SPF11X   EQU   *                                                                
*                                                                               
* RETURN TO CALLER                                                              
         DC    AL1(SPF12X-*,12,PFTRPROG,0,0,0)                                  
         DC    CL3' ',CL8' ',CL8' '                                             
SPF12X   EQU   *                                                                
*                                                                               
* ACTUAL REJECT                                                                 
         DC    AL1(SPF23X-*,23,0,0,0,0)                                         
SPF22RRQ EQU   *                                                                
         DC    CL3' ',CL8'REVISION',CL8'REJECT'                                 
SPF23X   EQU   *                                                                
                                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* COMMON ROUTINES AND ERROR MESSAGES (ADDRESSABLE EXCLUSIVELY BY R7)            
***********************************************************************         
         DROP  RB                                                               
COMMON   DS    0H                                                               
***********************************************************************         
* ROUTINE TO INTERFACE WITH TSAR BUFFER                               *         
* NTRY: R1 = REQUESTED ACTION                                         *         
*                                                                     *         
* EXIT: CC LOW FOR END OF FILE ERROR                                  *         
*       CC HIGH FOR RECORD NOT FOUND                                  *         
***********************************************************************         
GOTSAR   NTR1                                                                   
         STCM  R1,1,TLACTN         REQUESTED ACTION                             
*                                                                               
         MVC   TB.TSACTN,TLACTN                                                 
*                                                                               
         LA    RF,TSARREC                                                       
         ST    RF,TB.TSAREC                                                     
*                                                                               
         CLI   TLACTN,TSARES       EXPLICIT RESTORE?                            
         BE    GOTSAR10                                                         
         CLI   TLACTN,TSASAV       EXPLICIT SAVE?                               
         BE    GOTSAR10                                                         
         CLI   TLACTN,TSAINI       EXPLICIT INITIALISE?                         
         BNE   GOTSAR30                                                         
*                                                                               
GOTSAR10 DS    0H                                                               
         MVI   TB.TSPAGL,1         USE TEMPSTR PAGE 1                           
         MVI   TB.TSPAGN,10                                                     
         MVC   TB.TSACOM,ACOMFACS  SET A(COMFACS)                               
         MVI   TB.TSKEYL,L'TLKEY   SET KEY LENGTH                               
         MVI   TB.TSRECI,TSRVAR    SET VARIABLE                                 
         MVC   TB.TSRECL,=Y(255)   SET MAXIMUM RECORD LENGTH                    
         OI    TB.TSINDS,TSIXTTWA  14K RECORDS                                  
*                                                                               
GOTSAR20 GOTOX VTSAR,TB.TSARD      CALL TO INITIALISE/RESTORE                   
         BE    GOTSARX                                                          
         DC    H'0'                ABEND                                        
*                                                                               
GOTSAR30 DS    0H                                                               
         MVC   TB.TSRNUM,TXNUM     SET TSAR NUMBER                              
*                                                                               
* SET MINIMUM LENGTH FOR VARIABLE LENGTH RECORDS                                
*                                                                               
         CLI   TLACTN,TSAADD       TEST ADDING                                  
         BNE   GOTSAR50                                                         
         CLC   TR.TLLEN,=Y(L'TLKEY+2+1)                                         
         BNL   GOTSAR50                                                         
         MVC   TR.TLLEN,=Y(L'TLKEY+2+1)                                         
*                                                                               
GOTSAR50 DS    0H                                                               
         GOTOX VTSAR,TB.TSARD                                                   
*&&DO                                                                           
* FOR GET NEXT, DON'T UPDATE RECORD NUMBER (TXNUM) IF                           
* EOF OR REC NOT FOUND WAS ENCOUNTERED                                          
*                                                                               
         CLI   TLACTN,TSANXT                                                    
         BNE   GOTSAR60                                                         
         TM    TB.TSERRS,TSEEOF+TSERNF                                          
         BNZ   GOTSAR70                                                         
*                                                                               
GOTSAR60 DS    0H                                                               
*&&                                                                             
         MVC   TXNUM,TB.TSRNUM     SET RECORD LIST NUMBER                       
*                                                                               
         CLI   TLACTN,TSAADD       CHECK IF END-OF-FILE ERROR                   
         BNE   GOTSAR70                                                         
         TM    TB.TSERRS,TSEEOF    DON'T DIE, DISPLAY ERROR INSTEAD             
         BZ    GOTSARX             ASK USER TO REQUEST REPORT OFFLINE           
         MVC   RERROR,=AL2(801)                                                 
         L     R2,ATWA                                                          
         LA    R2,CONRECH-CONHEADH+64(R2)                                       
         GOTO1 MYERROR                                                          
*                                                                               
GOTSAR70 DS    0H                                                               
         TM    TB.TSERRS,TSEEOF    RETURN CC=LOW FOR END-OF-FILE ERROR          
         BO    EXITL                                                            
         TM    TB.TSERRS,TSERNF    RETURN CC=HIGH IF RECORD NOT FOUND           
         BO    EXITH                                                            
*                                                                               
GOTSARX  DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
*   LOADELT : LOADS ELEMENT IN WORKSPACE (ELTBUILD) TO RECORD.                  
***********************************************************************         
LOADELT  NTR1                                                                   
         L     R2,0(R1)            RESET A(TARGET RECORD)                       
         L     R3,4(R1)            RESET A(ELEMENT BUILD AREA)                  
         L     R4,8(R1)            RESET A(COMMAND EXTENSION)                   
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),(R2),(R3),(R4)                     
*                                  ADD ELT TO RECORD                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*   DELELT :  DROPS X'03' ELEMENT FROM BUYRECS                                  
***********************************************************************         
DELELT   NTR1                                                                   
         L     R2,0(R1)            RESET A(TARGET RECORD)                       
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(3,(R2)),0,0                       
*                                  DROP X'03' ELTS FROM BUYRECORD               
         B      EXIT                                                            
         EJECT                                                                  
***********************************************************************         
*   DELELT02 :  DROPS X'02' ELEMENT(S) (DAY/TIME) FROM BUYRECS                  
*        WHEN THEY WERE GENERATED BY AN ORBIT RECORD, AND THE BUYS              
*        ARE 'DAILY'                                                            
***********************************************************************         
DELELT02 NTR1                                                                   
         L     R2,0(R1)            RESET A(TARGET RECORD)                       
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(2,(R2)),0,0                       
*                                  DROP X'02' ELTS FROM BUYRECORD               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PRINT A LINE                                                                  
***********************************************************************         
PRINT    NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* EXITS                                                                         
***********************************************************************         
EXITOK   CR    RB,RB                                                            
         B     EXIT                                                             
EXITL    CLI   *,X'FF'                                                          
         B     EXIT                                                             
EXITH    CLI   *,0                                                              
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
*                                                                               
* ERROR MESSAGES                                                                
*                                                                               
MISSFLD  MVC   RERROR,=AL2(1)                                                   
         B     ERREND                                                           
*                                                                               
INVLFLD  MVC   RERROR,=AL2(2)                                                   
         B     ERREND                                                           
*                                                                               
INVLACT  MVC   RERROR,=AL2(INVACT)                                              
         B     ERREND                                                           
*                                                                               
INVLCON  MVC   RERROR,=AL2(82)                                                  
         B     ERREND                                                           
*                                                                               
MAXBUYER MVC   RERROR,=AL2(93)                                                  
         B     ERREND                                                           
*                                                                               
INVMETH  MVC   RERROR,=AL2(728)                                                 
         B     ERREND                                                           
*                                                                               
MUSTMAN  MVC   RERROR,=AL2(737)                                                 
         B     ERREND                                                           
*                                                                               
WASAPR   MVC   RERROR,=AL2(424)                                                 
         B     ERREND                                                           
*                                                                               
WASREJ   MVC   RERROR,=AL2(425)                                                 
         B     ERREND                                                           
*                                                                               
REJERR   MVC   RERROR,=AL2(433)                                                 
         B     ERREND                                                           
*                                                                               
INVLRCAC MVC   RERROR,=AL2(INVRCACT)                                            
         B     ERREND                                                           
*                                                                               
RECNTFND MVI   GERROR1,53          RECORD NOT FOUND                             
         B     ERREND                                                           
TABOVER  MVC   RERROR,=AL2(TABOVERE)                                            
         B     ERREND                                                           
*                                                                               
*                                                                               
INFRTEXT DS    0H                  SUBSTITUTION TEXT                            
         XC    GETTXTCB,GETTXTCB                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         LA    RE,BLOCK                                                         
         STCM  RE,7,GTASUBST                                                    
         DROP  RF                                                               
         B     INFEND                                                           
*                                                                               
ERREND   DS    0H                                                               
         MVI   RMSGTYPE,C'E'                                                    
         B     *+8                                                              
INFEND   MVI   RMSGTYPE,C'I'                                                    
*        LA    R2,CONRECH          SET CURSOR HERE FOR ALL ERROR/INFO           
*                                                                               
         GOTO1 MYERROR                                                          
*                                                                               
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
*                                                                               
***********************************************************************         
* PRINT LINE HEADINGS - 132 CHARACTER LENGTH                                    
***********************************************************************         
SUMTITLE DS    0C                                                               
         DC    C'SUMMARY OF CHANGES TO ORDER - LINES REFLECT HOW THE'           
         DC    C' REP ORDER MUST CHANGE TO AGREE WITH AGENCY ORDER:'            
SUMTITLQ EQU   *-SUMTITLE                                                       
*                                                                               
*                         1         2         3         4         5             
*                12345678901234567890123456789012345678901234567890             
BUYTITL1 DC    C'                                                  '            
*                                                                               
*                                                                 1             
*                5        6         7         8         9         0             
*                12345678901234567890123456789012345678901234567890             
         DC    C'    +/-                                           '            
*                                                                               
*                1        1         1         1                                 
*                0        1         2         3                                 
*                12345678901234567890123456789012                               
         DC    C'TOTAL DIFFERENCES               '                              
*                                                                               
*                         1         2         3         4         5             
*                12345678901234567890123456789012345678901234567890             
BUYTITL2 DC    C'DAY/TIMES                LEN         RATE DATES   '            
* DATA AREAS     ------------------------ --- ------------ --------             
*                                                                               
*                                                                 1             
*                5        6         7         8         9         0             
*                12345678901234567890123456789012345678901234567890             
         DC    C'  SP/WK PROGRAM              ELIGIBLE BUY NUMBERS '            
* DATA AREAS     -- ---- -------------------- --------------------              
*                                                                               
*                1        1         1         1                                 
*                0        1         2         3                                 
*                12345678901234567890123456789012                               
         DC    C'SPOTS          DOLLARS          '                              
* DATA AREAS     ----- ----------------                                         
*                                                                               
*                         1         2         3         4         5             
*                12345678901234567890123456789012345678901234567890             
DBUYTIT1 DC    C'                                                 #'            
*                                                                               
*                                                                 1             
*                5        6         7         8         9         0             
*                12345678901234567890123456789012345678901234567890             
         DC    C'REP                     #AGY                      '            
*                                                                               
*                1        1         1         1                                 
*                0        1         2         3                                 
*                12345678901234567890123456789012                               
         DC    C'                                '                              
*                                                                               
*                         1         2         3         4         5             
*                12345678901234567890123456789012345678901234567890             
DBUYTIT2 DC    C'DIF DAY/TIMES              LEN         RATE WK/OF '            
* DATA AREAS     --- ---------------------- --- ------------ -----              
*                                                                               
*                                                                 1             
*                5        6         7         8         9         0             
*                12345678901234567890123456789012345678901234567890             
         DC    C'SPT REP BUY NUMBERS      SPT AGY BUY NUMBERS      '            
* DATA AREAS     --- -------------------- --- --------------------              
*                                                                               
*                1        1         1         1                                 
*                0        1         2         3                                 
*                12345678901234567890123456789012                               
         DC    C'PROGRAM NAME                    '                              
* DATA AREAS     --------------------                                           
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
***********************************************************************         
* END OF COMMON ROUTINES                                                        
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*                                                                               
***********************************************************************         
CKGLOB   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         NI    MISCFLG2,X'FF'-MF2DTOT                                           
*                                                                               
         L     R4,ACOMFACS                                                      
         USING COMFACSD,R4                                                      
*                                                                               
         GOTO1 CGLOBBER,DMCB,=C'GETD',BLOCK,14,GLVXCTL                          
         CLI   DMCB+8,0                                                         
         BNE   CKGLOBX                                                          
         DROP  R4                                                               
*                                                                               
         LA    R1,BLOCK                                                         
         USING GLVXFRSY,R1                                                      
         CLC   GLVXFRPR,=C'CON'    CON PROGRAM                                  
         BNE   CKGLOBX                                                          
         OI    MISCFLG2,MF2DTOT                                                 
         DROP  R1                                                               
*                                                                               
CKGLOBX  DS    0H                                                               
         B     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*                                  GET 9'S REVERSE FOR BUYS LATER               
***********************************************************************         
* READ CONTRACT TO IO AREA 3                                                    
* SET UP ALT CALENDER FLAGS, VERSION, ETC.                                      
***********************************************************************         
NOCONREC EQU   82                                                               
*                                                                               
GETCON   NTR1  BASE=*,LABEL=*                                                   
*                                  GET 9'S REVERSE FOR BUYS LATER               
         PACK  COMPCON+0(1),CCONNUM+3(1)                                        
         PACK  COMPCON+1(1),CCONNUM+2(1)                                        
         PACK  COMPCON+2(1),CCONNUM+1(1)                                        
         PACK  COMPCON+3(1),CCONNUM+0(1)                                        
*                                                                               
         LA    R6,KEY                                                           
         USING RCONKEY,R6                                                       
         XC    KEY,KEY                                                          
         MVI   RCONPTYP,X'8C'                                                   
         MVC   RCONPREP,TWAAGY                                                  
         MVC   RCONPCON,CCONNUM                                                 
         DROP  R6                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'RCONKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R4,AIO3                                                          
         USING RCONREC,R4                                                       
*                                  DELETE  ALT CAL CTL  ELT                     
         XC    WORK,WORK                                                        
         MVC   WORK(4),ACOMFACS                                                 
         MVC   WORK+4(2),TWAAGY                                                 
*                                                                               
         GOTOX (RFVALTCL,REPFACS),DMCB,(X'FF',RCONREC),GETBROAD,0,WORK          
         BE    GETC0140            ALT CALENDAR(S) TEST PASSED                  
         MVC   RERROR,=AL2(780)    SET ERROR MESSAGE                            
         B     ERREXIT             EXIT CC NOT ZERO                             
*                                                                               
GETC0140 EQU   *                                                                
         GOTOX (RFCHKALT,REPFACS),DMCB,(0,RCONREC),ACOMFACS                     
         MVC   BUCKFLGS(1),0(R1)                                                
*                                  SET ALT CAL FLAGS FOR STATION                
***>>>                                                                          
         CLI   ACTCODE,C'R'        'REJECT' ACTION?                             
         BE    GETC0360            YES - DON'T DUMP BUYS                        
         DROP  R4                                                               
*                                                                               
         NI    MISCFLAG,X'FF'-MFMANUAL                                          
*                                                                               
         L     R6,AIO3                                                          
         MVI   ELCODE,X'1D'        RETRIEVE                                     
         BRAS  RE,GETEL                                                         
         BNE   GETC0050                                                         
         USING RCONDREL,R6                                                      
         CLI   RCONDRLN,RCONDL2Q                                                
         BL    GETC0050                                                         
         TM    RCONDRF2,X'04'                                                   
         BZ    GETC0050                                                         
         OI    MISCFLAG,MFMANUAL   SET MANUAL CHANGES STARTED                   
         DROP  R6                                                               
*                                                                               
GETC0050 EQU   *                                                                
         BAS   RE,GETVERSN         RETRIEVE VERSION NUMBER                      
*                                                                               
* CALL TO REGENVER MIGHT HAVE FLAGGED CONTRACT UPDATED MANUALLY                 
* IF SO, RESET MANUAL FLAG BACK IF CONTRACT WAS NOT PREVIOUSLY                  
* UPDATE MANUALLY                                                               
*                                                                               
         L     R6,AIO3                                                          
         MVI   ELCODE,X'1D'        RETRIEVE                                     
         BRAS  RE,GETEL                                                         
         BNE   GETC0060                                                         
         USING RCONDREL,R6                                                      
         CLI   RCONDRLN,RCONDL2Q                                                
         BL    GETC0060                                                         
         TM    MISCFLAG,MFMANUAL                                                
         BNZ   GETC0060                                                         
         NI    RCONDRF2,X'FF'-X'04'                                             
         DROP  R6                                                               
*                                                                               
*   RETRIEVE REP RECORD IN ALL CASES TO GET DAILY PACING PROFILE                
*                                                                               
GETC0060 EQU   *                                                                
*                                                                               
         MVC   AIO,AIOAREA         SET ALTERNATE READ AREA                      
*                                     FOR SPOT CODES                            
         L     R2,AIO                                                           
         USING RREPRECD,R2                                                      
         XC    RREPKEY,RREPKEY                                                  
         MVI   RREPKEY,1           INSERT RECORD CODE                           
         MVC   RREPKREP,TWAAGY     INSERT POWER CODE                            
         MVC   KEY,RREPKEY         LOAD KEY                                     
         MVI   RDUPDATE,C'N'       SET UPDATE TO NO                             
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                 RECORD FOUND                                 
         DC    H'0'                ??                                           
         MVI   RDUPDATE,C'N'       SET UPDATE TO NO                             
         GOTO1 GETREC                                                           
         MVC   DAILYFLG,RREPPROF+27                                             
*                                     FOR SPOT CODES                            
         L     R4,AIO3                                                          
         USING RCONREC,R4                                                       
*                                                                               
         CLI   RCONTYPE,C'N'       REP-TO-SPOT TRANSFER NEEDED?                 
         BE    GETC0160            YES                                          
         CLI   RCONTYPE,C'X'       REP-TO-SPOT TRANSFER NEEDED?                 
         BNE   GETC0360            NO                                           
GETC0160 EQU   *                                                                
         MVC   IFELCODE,=X'0830'   INSERT CODE/LENGTH                           
*                                                                               
         LA    R3,RREPELEM         FIND X'05' ELEMENT                           
GETC0200 EQU   *                                                                
         CLI   0(R3),0             END OF RECORD?                               
         BNE   *+6                                                              
         DC    H'0'                NO SPOT INTERFACE CODES!!                    
         CLI   0(R3),X'05'         SPOT INTERFACE ELEMENT?                      
         BE    GETC0240            YES                                          
         ZIC   RF,1(R3)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R3,R4                                                            
         B     GETC0200            GO BACK FOR NEXT                             
GETC0240 EQU   *                                                                
         MVC   IFSPAG,RREPSPPC-RREPSPOT(R3)                                     
*                                  INSERT SPOT AGENCY CODE                      
         MVC   IFSPMD,RREPMED-RREPSPOT(R3)                                      
*                                  INSERT SPOT MEDIA                            
         DROP R2                                                                
         USING RPRDREC,R2                                                       
*        XC    RPRDKEY,RPRDKEY                                                  
         MVI   RPRDKEY,9           INSERT RECORD CODE                           
         MVC   RPRDKADV,RCONKADV   INSERT ADVERTISER CODE                       
         MVC   RPRDKPRD,RCONPRD    INSERT PRODUCT CODE                          
         MVC   RPRDKREP,TWAAGY     INSERT POWER CODE                            
         MVC   KEY,RPRDKEY         LOAD KEY                                     
         MVI   RDUPDATE,C'N'       SET UPDATE TO NO                             
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                 RECORD FOUND                                 
         DC    H'0'                ??                                           
         MVI   RDUPDATE,C'N'       SET UPDATE TO NO                             
         GOTO1 GETREC                                                           
         LA    R3,RPRDELEM         FIND X'03' ELEMENT                           
GETC0280 EQU   *                                                                
         CLI   0(R3),0             END OF RECORD?                               
         BNE   *+6                                                              
         DC    H'0'                NO SPOT INTERFACE CODES!!                    
         CLI   0(R3),X'03'         SPOT INTERFACE ELEMENT?                      
         BE    GETC0320            YES                                          
         ZIC   RF,1(R3)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R3,R4                                                            
         B     GETC0280            GO BACK FOR NEXT                             
GETC0320 EQU   *                                                                
         MVC   IFSPCL,RPRDSPCL-RPRDSPOT(R3)                                     
*                                  INSERT SPOT CLIENT CODE                      
         MVC   IFSPPRD,RPRDSPP1-RPRDSPOT(R3)                                    
*                                  INSERT SPOT PRODUCT CODE                     
         MVC   IFSPES,RPRDSPES-RPRDSPOT(R3)                                     
*                                  INSERT SPOT ESTIMATE NUMBER                  
         MVC   IFSPPP,RPRDSPP2-RPRDSPOT(R3)                                     
*                                  INSERT SPOT PRODUCT PIGGY                    
         MVC   IFSPP1,RPRDSPS1-RPRDSPOT(R3)                                     
*                                  INSERT SPOT PRODUCT PIGGY SPLIT 1            
         MVC   IFSPP2,RPRDSPS2-RPRDSPOT(R3)                                     
*                                  INSERT SPOT PRODUCT PIGGY SPLIT 2            
         MVC   IFSPST,RCONKSTA     INSERT STATION CALL LETTERS                  
         MVC   IFSPADV,RCONKADV    INSERT REP ADVERTISER                        
         MVC   IFSPRD,RCONPRD      INSERT REP PRODUCT CODE                      
GETC0360 EQU   *                                                                
         MVC   CONMOD#,RCONMOD     SAVE MOD #                                   
*                                                                               
GETC0370 EQU   *                                                                
         SR    RC,RC               SET CC ZERO - CLEAN END                      
ERREXIT  LTR   RC,RC                                                            
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*    GETVERSN:  RETRIEVE X'20' ELEMENT FROM CONTRACT, SAVE THE                  
*        REP VERSION NUMBER.  BUMP THE NUMBER IF CONTRACT IF                    
*        IT HAS NOT ALREADY                                                     
*        TO SUPPORT WIP IN CONTRACT, THE NEXT REP VERSION NUMBER MUST           
*        BE THE HIGHER OF CURRENT REP NUMBER PLUS 2 *OR* CURRENT STA            
*        NUMBER PLUS 1.                                                         
***********************************************************************         
GETVERSN NTR1                                                                   
         MVI   VERDFLT,1           SET REP VERSION DEFAULT                      
*                                                                               
         L     R6,AIO3             SET WORK AREA FOR BUYS                       
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL                                                         
         BNE   GETVX                                                            
*                                  SAVE THE REP VERSION NUMBER                  
*        CLI   RESENT,C'Y'         CONTRACT IS RESENT?                          
*        BE    GETV0010            YES - NEED VERSION BUMP                      
*                                                                               
*        L     R4,AIO1                                                          
*        USING RDARREC,R4                                                       
*        TM    RDARMISC,X'10'      IF VARIOUS, THIS MIGHT BE A                  
*        BZ    GETV0100            POOL RESENT AFTER ALL                        
*        DROP  R4                                                               
                                                                                
* BUMP REP VERSION IF REP VERSION WAS NOT ADVANCED, ELSE EXIT                   
GETV0010 EQU   *                                                                
         USING RCONSEND,R6                                                      
         TM    RCONSENF,X'20'                                                   
         BZ    GETV0100                                                         
         DROP  R6                                                               
*                                                                               
         MVC   WORK(4),HELLO                                                    
         MVC   WORK+4(4),DATCON                                                 
         GOTOX (RFGENVER,REPFACS),DMCB,(C'R',AIO3),WORK                         
         BNZ   INVLFLD                                                          
*                                                                               
GETV0100 EQU   *                                                                
         L     R6,AIO3             SET WORK AREA FOR BUYS                       
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL                                                         
         BNE   GETVX                                                            
         USING RCONSEND,R6                                                      
         MVC   VERDFLT,RCONSRV                                                  
         DROP  R6                                                               
*                                                                               
GETVX    EQU   *                                                                
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CONVERT AND BUILD DEMO CATEGORIES ELEMENT                                     
* CHECK TO SEE IF THE DEMO CATEGORIES HAVE BEEN CHANGED BETWEEN                 
*  THE AGENCY AND THE REP                                                       
***********************************************************************         
SVAGYDEM NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* SAVE OFF DEMO CATEGORIES TO LATER STORE IN CONTRACT RECORD                    
*                                                                               
         XC    WORK,WORK                                                        
         XC    SVDEMCAT,SVDEMCAT                                                
         NI    MISCFLG2,X'FF'-MF2NODEM-MF2NOADM-MF2USRDM  NO DEM                
         NI    FLAGS2,X'FF'-FG2DEMO                                             
*                                                                               
         L     R6,AIO1                                                          
         MVI   ELCODE,X'03'                                                     
         BRAS  RE,GETEL                                                         
         BE    SVDC05                                                           
         OI    MISCFLG2,MF2NOADM                                                
*                                                                               
SVDC03   DS    0H                                                               
         L     R6,AIO3             IF AGY DIDN'T SEND OVER DEMO CAT             
         MVI   ELCODE,X'DD'                                                     
         BRAS  RE,GETEL                                                         
         BE    SVDC04                                                           
SVDC03A  DS    0H                                                               
         OI    MISCFLG2,MF2NODEM   NO DEM ON LAST SEND AND THIS SEND            
         B     SVDCX                                                            
*                                                                               
SVDC04   DS    0H                                                               
         CLI   1(R6),18                                                         
         BE    SVDC03A                                                          
         ZIC   R1,1(R6)            TREAT IT LIKE IT SENDS OVER 0                
         SHI   R1,3                VALUE WITH OLD DEMO CAT                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   2(0,R6),SPACES                                                   
         BE    SVDC03A                                                          
*                                                                               
         AHI   R1,2                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SVDEMCAT(0),0(R6)                                                
         B     SVDCX                                                            
*                                                                               
SVDC05   DS    0H                                                               
         CLC   2(RDARDMLQ-2,R6),SPACES                                          
         BE    SVDC03              ALL SPACES?                                  
         CLI   2(R6),C'('          USER DEFINED?                                
         BNE   SVDC08                                                           
         OI    MISCFLG2,MF2USRDM                                                
         B     SVDCX                                                            
*                                                                               
SVDC08   DS    0H                                                               
         USING RDARDMEL,R6                                                      
         ZIC   R1,RDARDMLN                                                      
         SHI   R1,3                SUBTRACT OVERHEAD, AND 1 FOR EX              
         LTR   R1,R1                                                            
         BNZ   *+8                                                              
         LA    R1,L'RDARDEM1       MOVE AT LEAST ONE CATEGORY                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),RDARDEM1                                                 
         OC    WORK,SPACES                                                      
         DROP  R6                                                               
*                                                                               
ELEMD    USING RCONDDEL,SVDEMCAT                                                
         MVI   ELEMD.RCONDDCD,X'DD'                                             
         MVI   ELEMD.RCONDDLN,2                                                 
*                                                                               
         LA    R3,10                                                            
         LA    R6,WORK                                                          
         LA    R4,ELEMD.RCONDDCT                                                
*                                                                               
SVDC10   DS    0H                                                               
         CLC   0(L'RDARDEM1,R6),SPACES                                          
         BE    SVDC30                                                           
         CLI   0(R6),C'('          USER DEFINED DEMO?                           
         BNE   *+14                                                             
         MVC   0(3,R4),0(R6)                                                    
         B     SVDC20                                                           
*                                                                               
         MVC   1(1,R4),0(R6)                                                    
         PACK  DUB(8),1(3,R6)                                                   
         CVB   RF,DUB                                                           
         STC   RF,2(R4)                                                         
*                                                                               
SVDC20   DS    0H                                                               
         ZIC   RE,ELEMD.RCONDDLN                                                
         AHI   RE,L'RCONDDCT                                                    
         STC   RE,ELEMD.RCONDDLN                                                
*                                                                               
         AHI   R4,L'RCONDDCT                                                    
         AHI   R6,L'RDARDEM1                                                    
         BCT   R3,SVDC10                                                        
*                                                                               
* CHECK IF THE AGENCY HAS CHANGED THE DEMO CATEGORIES                           
*                                                                               
SVDC30   DS    0H                                                               
         CLC   ELEMD.RCONDDCT,SPACES                                            
         BE    SVDCX                                                            
*                                                                               
         L     R6,AIO3                                                          
         MVI   ELCODE,X'DD'                                                     
         BRAS  RE,GETEL                                                         
         BNE   SVDC0100                                                         
         USING RCONDDEL,R6                                                      
         CLC   ELEMD.RCONDDLN,RCONDDLN                                          
         BNE   SVDC0100                                                         
         ZIC   R1,RCONDDLN                                                      
         SHI   R1,3                2 FOR LENGTH, 1 FOR EX                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   ELEMD.RCONDDCT(0),RCONDDCT                                       
         BE    SVDCX                                                            
         DROP  R6                                                               
*                                                                               
SVDC0100 DS    0H                                                               
         L     R6,AIO3                                                          
         MVI   ELCODE,X'12'                                                     
         BRAS  RE,GETEL                                                         
         BNE   SVDC0200                                                         
         USING RSARCO,R6                                                        
         MVC   WORK(L'RCONDDCT),RSARDEM                                         
         NI    WORK,X'FF'-X'40'    TURN OFF USER DEFINE                         
         CLC   WORK(L'RCONDDCT),ELEMD.RCONDDCT                                  
         BE    SVDCX                                                            
SVDC0200 DS    0H                                                               
         OI    FLAGS2,FG2DEMO      DEFAULT DEMO CHANGED                         
*                                                                               
SVDCX    DS    0H                                                               
         XIT1                                                                   
         DROP  R6,ELEMD                                                         
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
* DELETE PARTIAL CONFIRM COMMENTS WHEN CONTRACT VERSION GO UP                   
*---------------------------------------------------------------------          
DELCFC   NTR1  BASE=*,WORK=(R4,IMWORKQ),LABEL=*                                 
*                                                                               
         USING IMWORKD,R4                                                       
         MVC   IMSVKEY,KEY                                                      
         MVC   IMSVIO,AIO                                                       
*                                                                               
         L     R2,AIO3                                                          
CON      USING RCONREC,R2                                                       
*                                                                               
         LA    RE,IMIO                                                          
         ST    RE,AIO                                                           
*                                                                               
         XC    KEY,KEY             READ CFC REC                                 
         LA    R6,KEY                                                           
         USING RCFCREC,R6                                                       
*                                                                               
         MVI   RCFCKTYP,RCFCKTYQ   47 RECORD                                    
         MVC   RCFCKREP,CON.RCONKREP                                            
         MVC   RCFCKCON,CON.RCONKCON                                            
         DROP  CON                                                              
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(L'RCFCKEY),KEYSAVE   HAVE CFC REC?                           
         BNE   DELCFCX                  NO - GET OUT                            
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         OI    RCFCCNTL,X'80'          DELETE RECORD                            
         GOTO1 PUTREC                                                           
*                                                                               
         LA    R6,KEY                                                           
         OI    RCFCKEY+27,X'80'        DELETE KEY                               
         GOTO1 WRITE                                                            
*                                                                               
DELCFCX  DS    0H                                                               
         MVC   AIO,IMSVIO                                                       
         MVC   KEY,IMSVKEY                                                      
         B     EXIT                                                             
         DROP  R4,R6                                                            
         LTORG                                                                  
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
***********************************************************************         
* AGENCY BUY PREPROCESSING:                                                     
* DARE AGENCY BUYS WILL BE CONVERTED TO REPPAK BUY FORMAT WITH KEY TYPE         
* X'0B01'                                                                       
***********************************************************************         
SHADOW   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BAS   RE,DELOLD           DEL EXISTING X'0B01' RECORDS FOR             
*                                  THIS CONTRACT                                
         MVC   KEY(L'SELECTKY),SELECTKY                                         
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BE    SHAD03              ORDER PROBABLY CONFIRMED                     
         LA    R2,CONACTH          SET CURSOR TO 'ACTION' FIELD                 
         MVC   RERROR,=AL2(53)     SET ERROR MESSAGE                            
         GOTO1 MYERROR             EXIT WITH ERROR                              
*                                                                               
SHAD03   DS    0H                                                               
         MVC   AIO,AIO1            SET IO AREA FOR X'41' RECS                   
         GOTO1 GETREC                                                           
                                                                                
         L     R4,AIO2             SET WORK AREA FOR BUYS                       
         USING RBUYREC,R4                                                       
*                                                                               
         L     R3,AIO                                                           
         USING RDARREC,R3                                                       
*                                                                               
         MVI   RDARKRT,X'40'       SET REC TYPE TO 'BUY'                        
         XC    RDARKSEQ(2),RDARKSEQ                                             
*                                  CLEAR SEQ # AND SUB TYPE                     
         MVC   KEY,RDARKEY         RETRIEVE FIRST BUY HEADER                    
         MVI   RDUPDATE,C'N'       SET UPDATE TO NO                             
         NI    DMINBTS,X'FF'-X'08' SKIP DELETED                                 
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE     SAME KEY? THROUGH REC TYPE                   
         BNE   SHADOWX             YES                                          
*                                                                               
         MVI   RDUPDATE,C'N'       SET UPDATE TO NO                             
         MVI   BUYUPDAT,C'Y'       SET 'WRITE OUTPUT RECORD'                    
         GOTO1 GETREC              RETRIEVE RECORD                              
         GOTO1 =A(CHKDAREC),DMCB,(R3),RR=RELO                                   
*                                  ROUTINE SETS 'BUYUPDAT' TO NO                
*                                     WHEN HEADER IS SOFT-DELETED.              
*                                     THIS WILL PREVENT OUTPUT.                 
*                                  ALSO SETS 'SKIP RECORD' FLAG FOR             
*                                     OTHER RECORD TYPES                        
         MVI   BUYLINE#,0          INIT BUY NUMBER                              
*                                                                               
SHAD05   EQU   *                                                                
         MVI   ORBSTDAY,0          CLEAR 'ORBIT START DAY'                      
         MVI   ORBFLAG,C'N'        SET   'ORBIT NOT PRESENT'                    
         MVI   DETLFLAG,C'N'       CLEAR COUNT OF DETAILS                       
         XCEFL RBUYREC,1024        CLEAR BUY BUILD AREA                         
         MVC   RBUYKTYP(2),=X'0B01' AGENCY BUY SHADOW RECORD TYPE               
         MVC   RBUYKREP,TWAAGY     INSERT REP CODE                              
         MVC   RBUYKCON,COMPCON    INSERT CONTRACT #, 9/COMP/REV                
         MVC   RBUYKPLN,=X'FFFFFF' INSERT PLAN CODE                             
         ZIC   RF,BUYLINE#         GET NEXT BUYLINE NUMBER                      
         LA    RF,1(RF)                                                         
         STC   RF,BUYLINE#         PUT IT BACK                                  
*                                                                               
* STORE AGENCY'S BUY NUMBER IN MASTER LINE NUMBER                               
*                                                                               
*        STC   RF,RBUYKMLN         INSERT MASTER LINE NUMBER                    
         MVC   RBUYKMLN,RDARKSEQ   INSERT AGENCY BUY LINE NUMBER                
*                                                                               
         STC   RF,RBUYKLIN         INSERT LINE NUMBER                           
         MVC   RBUYLEN,=X'004D'    INSERT RECORD LENGTH:                        
*                                     34+43 = 77 = X'4D'                        
         MVC   RBUYELEM(2),=X'012B'                                             
*                                  INSERT ELEMENT CODE/LENGTH                   
         LA    R6,RDARELEM                                                      
         USING RDARBYEL,R6                                                      
         MVC   PROGNAME,RDARBYPN   SAVE BUY HDR PROGRAM NAME                    
         MVC   RBUYCOS,RDARBYCO    INSERT COST                                  
         MVC   BUYCOST,RDARBYCO    SAVE COST FOR CALCULATIONS                   
         MVC   RBUYCLS(6),SPACES   SET CLASS/SECTION TO SPACES                  
         GOTO1 DATCON,DMCB,(5,WORK),(3,RBUYCREA)                                
*                                  INSERT CREATION DATE                         
         MVC   RBUYKMOD,CONMOD#    INSERT CONTRACT MOD #                        
         MVI   RBUYCHGI,C'A'       INSERT CHANGE INDICATOR                      
         MVC   RBUYAGBL,RDARKSEQ   INSERT AGENCY BUY LINE NUMBER                
         XC    STARTDAY(2),STARTDAY                                             
*                                  CLEAR START/END DAYS OF WEEK                 
         GOTO1 =A(STARTEND),DMCB,RDARBYRO,RBUYSTED,RR=RELO                      
*                                  CONVERT ONE-BYTE START/END DATE              
         GOTO1 =A(EFFDATEL),DMCB,RDARREC,RR=RELO                                
*                                                                               
         MVC   RBUYDUR,RDARBYSL    INSERT TOTAL SPOT LENGTH                     
         CLI   RDARBYSL,C'M'       LENGTH IN MINUTES?                           
         BNE   SHAD10              NO                                           
         OI    RBUYDUR,X'80'       YES - SET 'MINUTES' INDICATOR                
SHAD10   EQU   *                                                                
*                                                                               
         MVC   RBUYVER,VERDFLT     INSERT VERSION NUMBER                        
*                                     FROM REP VERSION NUMBER                   
         OC    IFELCODE(IFELLEN),IFELCODE SPOTPAK XFER ELEMENT?                 
         BZ    SHAD20              NO                                           
         GOTO1 LOADELT,DMCB,(R4),IFELCODE,=C'ADD=CODE'                          
         DROP  R6                                                               
*                                                                               
SHAD20   EQU   *                                                                
         GOTO1 =A(PROGRAM),RR=RELO  SAVE OFF PROGRAM NAME                       
         GOTO1 =A(SAVEDEMO),RR=RELO SAVE OFF DEMO VALUES, IF ANY                
*                                                                               
         L     R6,AIO              IF AGENCY BUY IS A MAKEGOOD                  
         MVI   AGBUYMTR,0                                                       
         MVI   ELCODE,X'10'        NEED TO SAVE OFF TARGET/MASTER               
         BRAS  RE,GETEL            AGENCY BUY FOR BETTER MATCHING               
         BNE   SHAD30                                                           
         USING RDARRVEL,R6                                                      
         MVC   AGBUYMTR,RDARRVBY                                                
         DROP  R6                                                               
*                                                                               
SHAD30   EQU   *                                                                
         MVI   RDUPDATE,C'N'       SET UPDATE TO NO                             
         NI    DMINBTS,X'FF'-X'08' SKIP DELETED                                 
         GOTO1 SEQ                 ACCESS NEXT X'41' RECORD                     
         MVC   SAVEKEY,KEY                                                      
         CLC   KEY(25),KEYSAVE     SAME KEY? THROUGH RECORD TYPE?               
         BE    SHAD40              YES - CONTINUE                               
         CLI   BUYUPDAT,C'Y'       WRITE THIS RECORD?                           
         BNE   SHADOWX             NO  - SKIP THE REWRITE                       
         TM    MISCFLAG,X'80'      'DAILY' BUY?                                 
         BO    SHADOWX             YES - ALREADY WRITTEN                        
         GOTO1 =A(GENREC),RR=RELO                                               
         B     SHADOWX                                                          
*                                                                               
SHAD40   EQU   *                                                                
         CLC   KEY+25(1),KEYSAVE+25                                             
*                                  SAME BUYLINE #?                              
         BE    SHAD60              YES - CONTINUE                               
         CLI   BUYUPDAT,C'Y'       WRITE THIS RECORD?                           
         BNE   SHAD60              NO  - SKIP THE REWRITE                       
         TM    MISCFLAG,X'80'      'DAILY' BUY?                                 
         BNO   SHAD50              NO  - GO PUT IT OUT                          
*                                                                               
*   NOTE:  'DAILY'S ARE GENERATED FROM WITHIN THE BUYDETL ROUTINE.              
*      AS SUCH, THE BUYLINE# IS BUMPED AFTER THE RECORD IS WRITTEN.             
*      WHEN THE AGENCY BUY IS COMPLETED, THE BUYLINE # IS SET TO                
*      THE NEXT EXPECTED BUYLINE.  TO ENSURE THAT IT IS NOT DOUBLE-             
*      INCREMENTED, IT IS BACKED OFF HERE.                                      
*                                                                               
         ZIC   RF,BUYLINE#         YES - DROP BUYLINE # BY 1                    
         BCTR  RF,0                                                             
         STC   RF,BUYLINE#                                                      
         B     SHAD60              DON'T PUT IT OUT AGAIN                       
SHAD50   EQU   *                                                                
         GOTO1 =A(GENREC),RR=RELO                                               
*                                  NO  - OUTPUT PREVIOUS RECORD                 
SHAD60   EQU   *                                                                
         MVC   AIO,AIO1            SET A(IO AREA 1)                             
         GOTO1 GETREC              RETRIEVE RECORD                              
         GOTO1 =A(CHKDAREC),DMCB,(R3),RR=RELO                                   
*                                  ROUTINE SETS 'BUYUPDAT' TO NO                
*                                     WHEN HEADER IS SOFT-DELETED.              
*                                     THIS WILL PREVENT OUTPUT.                 
*                                  ALSO SETS 'SKIP RECORD' FLAG FOR             
*                                     OTHER RECORD TYPES                        
*                                                                               
         CLI   KEY+26,X'00'        BUY HEADER?                                  
         BE    SHAD05              YES - START NEXT HEADER                      
         CLI   KEY+26,X'10'        BUY ORBIT?                                   
         BNE   SHAD70              NO                                           
         CLI   SKIPRECS,C'Y'       SKIP THIS RECORD?                            
         BE    SHAD30              YES - GO BACK FOR NEXT                       
         GOTO1 =A(BUYORBIT),DMCB,(R3),RR=RELO                                   
*                                  PROCESS BUY ORBIT RECORD                     
         B     SHAD30              GO BACK FOR NEXT RECORD                      
SHAD70   EQU   *                                                                
         CLI   KEY+26,X'20'        BUY COMMENT?                                 
         BNE   SHAD80              NO                                           
         CLI   SKIPRECS,C'Y'       SKIP THIS RECORD?                            
         BE    SHAD30              YES - GO BACK FOR NEXT                       
         GOTO1 =A(BUYCOMMT),DMCB,(R3),RR=RELO                                   
*                                  PROCESS BUY COMMENT RECORD                   
         B     SHAD30              GO BACK FOR NEXT RECORD                      
SHAD80   EQU   *                                                                
         CLI   KEY+26,X'30'        BUY DETAIL?                                  
         BE    *+6                 NO  - ONLY TRAILER LEFT                      
         DC    H'0'                UNKNOWN RECORD SUBTYPE                       
         CLI   SKIPRECS,C'Y'       SKIP THIS RECORD?                            
         BE    SHAD30              YES - GO BACK FOR NEXT                       
         CLC   PROGNAME,SPACES     ANY PROGRAM NAME?                            
         BE    SHAD110             NO  - DON'T NEED COMMENT                     
         CLI   KATZEDI,C'Y'        KATZ/EDI USES ONLY THE FIRST 32              
         BNE   SHAD90              WITH THE LAST 2 FOR DAYPART                  
         CLC   PROGNAME(32),SPACES ANY PROGRAM NAME?                            
         BE    SHAD110             NO  - DON'T NEED COMMENT                     
SHAD90   DS    0H                                                               
         GOTO1 =A(GENCOMMT),DMCB,RBUYREC,RR=RELO                                
SHAD110  DS    0H                                                               
         MVC   SAVEKEY,KEY                                                      
         GOTO1 =A(BUYDETL),DMCB,(R3),RR=RELO                                    
         BZ    SHAD120                                                          
         MVI   BUYUPDAT,C'N'                                                    
         B     SHAD30                                                           
*                                                                               
SHAD120  DS    0H                                                               
         MVC   KEY,SAVEKEY                                                      
         GOTO1 HIGH                                                             
         B     SHAD30              GO BACK FOR NEXT RECORD                      
         DROP  R3,R4                                                            
*                                                                               
SHADOWX  DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DELETE X'0B01' RECORDS FOR THIS DARE ORDER, IF ANY                            
***********************************************************************         
DELOLD   NTR1                                                                   
         LA    R6,KEY                                                           
         USING RBUYREC,R6                                                       
         XC    KEY,KEY                                                          
         MVC   RBUYKTYP(2),=X'0B01' AGENCY BUY SHADOW RECORD TYPE               
         MVC   RBUYKREP,TWAAGY     INSERT REP CODE                              
         MVC   RBUYKCON,COMPCON    INSERT CONTRACT #, 9/COMP/REV                
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
*                                                                               
DELOLD10 DS    0H                                                               
         CLC   KEY(RBUYKPLN-RBUYKEY),KEYSAVE                                    
         BNE   DELOLDX                                                          
*                                                                               
         MVC   AIO,AIO2                                                         
         L     R6,AIO                                                           
         USING RBUYREC,R6                                                       
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         OI    RBUYCNTL,X'80'      MARK DELETED                                 
         GOTO1 PUTREC                                                           
         DROP  R6                                                               
*                                                                               
         OI    KEY+27,X'80'                                                     
         GOTO1 WRITE                                                            
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 SEQ                                                              
         B     DELOLD10                                                         
*                                                                               
DELOLDX  DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*   CHKDAREC:  CHECKS FOR TYPE OF DARE RECORD.  IF SOFT DELETE                  
*        BIT OR HARD DELETE BIT IS SET, CC IS SET TO NON-ZERO,                  
*        INDICATING THAT RECORD IS TO BE SKIPPED/NOT PROCESSED.                 
*        SOFT DELETE BIT (X'80') WILL ALWAYS BE SET WHEN HARD BIT               
*        (X'40') IS SET.  THEREFORE, ONLY SOFT BIT IS TESTED.                   
***********************************************************************         
CHKDAREC NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            RESET A(DARE RECORD IN PROCESS)              
         USING RDARREC,R2                                                       
*                                                                               
         MVI   SKIPRECS,C'N'       SET 'SKIP RECORD' TO NO                      
*                                                                               
         CLI   RDARKRT,X'10'       AGENCY ORDER HEADER?                         
         BNE   CDAR0040            NO                                           
         LA    R3,RDARELEM         YES -                                        
         USING RDARELEM,R3                                                      
*                                                                               
         TM    RDARDELS,X'80'      SOFT DELETE SET?                             
         BNO   CDAR0900            NO  - EXIT WITH 'PROCESS RECORD'             
         ZIC   RF,BUYLINE#         YES - BUMP LINE NUMBER                       
         LA    RF,1(RF)                                                         
         STC   RF,BUYLINE#         PUT IT BACK                                  
         B     CDAR0800            EXIT WITH 'SKIP RECORD'                      
         DROP  R3                                                               
CDAR0040 EQU   *                                                                
         CLI   RDARKRT,X'40'       BUY?                                         
         BNE   CDAR0080            NO                                           
         CLI   RDARKSRT,X'00'      BUY HEADER?                                  
         BNE   CDAR0120            NO  - ORB/COMMT/DETAIL                       
*                                                                               
         LA    R3,RDARELEM         YES -                                        
         USING RDARBYEL,R3                                                      
*                                                                               
         MVI   BUYUPDAT,C'Y'       SET WRITE FLAG TO YES                        
         ZIC   RF,RDARBYLN         CHECK LENGTH                                 
         LA    RE,RDARBYOL         SET OLD ELEMENT LENGTH                       
         CR    RF,RE               COMPARE OLD VS NEW                           
         BE    CDAR0900            EQUAL: OLD LENGTH FOUND -                    
*                                     NOT KEY-DELETED:  MUST NOT                
*                                     BE SOFT DELETED.                          
CDAR0050 EQU   *                                                                
         TM    RDARBYDL,X'80'      SOFT DELETE SET?                             
         BNO   CDAR0900            NO  - EXIT WITH 'PROCESS RECORD'             
         MVI   BUYUPDAT,C'N'       YES - SET WRITE FLAG TO NO                   
         B     CDAR0800            EXIT WITH 'SKIP RECORD'                      
         DROP  R3                                                               
CDAR0080 EQU   *                                                                
         BH    CDAR0800            SKIP OVER RECORD TYPES                       
*                                     50 (TRAILER) + 60 (EQUIVS)                
*                                        THESE AREN'T SOFT-DELETED.             
CDAR0120 EQU   *                                                                
*                                                                               
*                                  ONLY BUYS WILL HAVE RDARKSRT                 
*                                     SET - STANDARD AND ORDER                  
*                                        COMMENTS WILL NOT                      
         CLI   RDARKSRT,X'30'      BUY DETAIL?                                  
         BL    CDAR0280            NO  - ORB/COMMT                              
         LA    R3,RDARELEM         YES - CHECK FOR MULTI RATES                  
         USING RDARBDEL,R3                                                      
*                                                                               
         TM    RDARBDDL,X'80'      SOFT DELETE SET?                             
         BNO   CDAR0900            NO  - EXIT WITH 'PROCESS RECORD'             
         MVI   SKIPRECS,C'Y'       YES - SET SKIP RECORD TO YES                 
         B     CDAR0800            FINISHED                                     
         DROP  R3                                                               
*                                                                               
CDAR0280 EQU   *                                                                
*                                                                               
*   REMAINING RECORD TYPES ARE 15, 20, 30, 40/10, 40/20, AND                    
*        SOFT/HARD DELETE BYTE IS IN SAME PLACE IN ALL ELEMENTS.                
*        STANDARD COMMENT ELEMENT FORMAT USED, ARBITRARILY.                     
*                                                                               
         LA    R3,RDARELEM                                                      
         USING RDARELE2,R3                                                      
*                                                                               
         TM    RDARELDL,X'80'      SOFT DELETE SET?                             
         BNO   CDAR0900            NO  - EXIT WITH 'PROCESS RECORD'             
         MVI   SKIPRECS,C'Y'       YES - SET SKIP RECORD TO YES                 
         B     CDAR0800            EXIT WITH 'SKIP RECORD'                      
         DROP  R3                                                               
CDAR0800 EQU   *                                                                
         LTR   RB,RB               SET CC NOT = ZERO                            
         B     CDAR1000                                                         
CDAR0900 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
CDAR1000 EQU   *                                                                
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* BUILDS PROGRAM NAME ELEMENT                                                   
**********************************************************************          
PROGRAM  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* BUILDS DEMO ELEMENT                                                           
**********************************************************************          
SAVEDEMO NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LR    RE,RA               CHECK TO SEE IF  LOCAL SIGN ON               
         AHI   RE,DARPROFS-CONHEADH                                             
         USING SVDSECT,RE                                                       
         TM    SVPGPBIT+CNTRPEAB,CNTRPEAA                                       
         BZ    SVDEM03                                                          
         DROP  RE                                                               
         CLI   SIGNONID+4,C'L'                                                  
         BE    SVDEMX              YES, DON'T CONSTRUCT DEMO ELE                
*                                                                               
SVDEM03  DS    0H                                                               
         TM    MISCFLG2,MF2NODEM                                                
         BO    SVDEMX                                                           
*                                                                               
         L     R6,AIO1                                                          
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL                                                         
         BE    SVDEM05                                                          
*                                                                               
         XC    WORK,WORK                                                        
         LA    R6,WORK             FAKE A 20 ELEMENT WITH NULLS                 
         MVI   WORK+1,RDARBMLQ                                                  
*                                                                               
SVDEM05  DS    0H                                                               
         USING RDARBMEL,R6                                                      
*                                                                               
         SR    R2,R2                                                            
         ZIC   R3,RDARBMLN                                                      
         SHI   R3,2                SUBTRACT OVERHEAD                            
         LA    RE,L'RDARBDM1                                                    
         DR    R2,RE               GET NUMBER OF DEMOS TO PROCESS               
*                                                                               
         LA    R6,RDARBDM1                                                      
         DROP  R6                                                               
*                                                                               
         XC    ELEM,ELEM                                                        
ELEMD    USING RBUYDMEL,ELEM                                                    
         MVI   ELEMD.RBUYDMCD,RBUYDMCQ                                          
         MVI   ELEMD.RBUYDMLN,2                                                 
         LA    R4,ELEMD.RBUYDMCT                                                
         LA    R2,SVDEMCAT+2                                                    
*                                                                               
SVDEM10  DS    0H                                                               
         MVC   0(L'RBUYDMCT,R4),0(R2)                                           
         AHI   R4,L'RBUYDMCT                                                    
         MVC   0(8,R4),=8X'FF'     -1 MEANS VALUE NOT PROVIDED                  
*                                                                               
         OC    0(L'RBUYDMCT,R2),0(R2)  NO CATEGORY - SKIP                       
         BZ    SVDEM20                                                          
*                                                                               
         CLC   0(L'RDARBDM1,R6),SPACES                                          
         BE    SVDEM20                                                          
*                                                                               
         PACK  DUB(8),0(7,R6)                                                   
         L     RF,DUB+4                                                         
         SRL   RF,4                DUMP THE SIGN                                
         STCM  RF,15,0(R4)                                                      
*                                                                               
SVDEM20  DS    0H                                                               
         AHI   R6,L'RDARBDM1                                                    
         AHI   R4,L'RBUYDMDM+L'RBUYDM2M                                         
         ZIC   RE,ELEMD.RBUYDMLN                                                
         AHI   RE,L'RBUYDMCT+L'RBUYDMDM+L'RBUYDM2M                              
         STC   RE,ELEMD.RBUYDMLN                                                
*                                                                               
         AHI   R2,L'RCONDDCT                                                    
*                                                                               
         BCT   R3,SVDEM10                                                       
*                                                                               
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),AIO2,ELEM,=C'ADD=CODE'             
*                                                                               
SVDEMX   DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*   BUYORBIT:  BUILDS DAY-TIME ELEMENT FROM THE ORBIT ELEMENTS                  
*        IN THE RECORD, INSERTS THEM INTO THE NEW BUY RECORD.                   
***********************************************************************         
BUYORBIT NTR1  BASE=*,LABEL=*                                                   
         L     R3,0(R1)            RESET A(X'41' RECORD)                        
         USING RDARREC,R3                                                       
         LA    R8,RDARELEM         SET A(X'01' ELEMENT)                         
         ZIC   RF,1(R8)            GET LENGTH                                   
         AR    R8,RF               BUMP TO 1ST ORBIT ELEMENT                    
         MVI   ORBFLAG,C'Y'        SET 'ORBIT PRESENT' INDICATOR                
*                                     ORBIT TAKES PRIORITY OVER                 
*                                     BUY DAY/TIME ELTS                         
BORB0040 EQU   *                                                                
*                                  CLEAR START/END DAYS                         
         CLI   0(R8),0             END OF RECORD?                               
         BE    BORB0400            YES - FINISHED                               
*                                                                               
         USING RDAROEEL,R8                                                      
*                                                                               
         XC    ELTBUILD,ELTBUILD   CLEAR ELEMENT BUILD AREA                     
         XC    STARTDAY(2),STARTDAY                                             
*                                  CLEAR START/END DAYS                         
         MVC   ELTBUILD(2),=X'0209'                                             
*                                  INSERT ELEMENT CODE/LENGTH                   
         GOTO1 =A(STARTEND),DMCB,RDAROERO,ELTBUILD+2,RR=RELO                    
*                                  INSERT START/END DAY                         
         CLI   ORBSTDAY,0          ANY ENTRY IN ORBIT START DAY?                
         BNE   BORB0060            YES - DON'T REPLACE IT                       
         MVC   ORBSTDAY,STARTDAY   NO  - SAVE FIRST ORBIT START DAY             
*                                                                               
* EARLIEST ORBIT START DAY MIGHT BE LATER THAN CURRENT STAY DAY AS              
* SPECIFIED IN THE BUY HEADER. WE'LL TAKE THE FIRST ORBIT START DAY             
* AS THE OVERALL BUY START DAY                                                  
*                                                                               
* NOTE THAT THIS ASSUMES DAILYS SHOULD NEVER BE ACCOMPANIED WITH                
* ORBITS!!                                                                      
*                                                                               
         L     R2,AIO2             A(BUY RECORD IN PROGRESS)                    
         USING RBUYREC,R2                                                       
         ZIC   RF,ORBSTDAY                                                      
         SLL   RF,4                SHIFT TO LOW ORDER, HI NIBBLE                
         ZIC   RE,RBUYSTED                                                      
         SLL   RE,8+8+8+4          SHIFT OFF HI NIBBLE OR CURRENT               
         SRL   RE,8+8+8+4          START DAY                                    
         AR    RE,RF                                                            
         STC   RE,RBUYSTED                                                      
         DROP  R2                                                               
*                                                                               
BORB0060 EQU   *                                                                
         SR    RE,RE               INITIALIZE FINAL OUTPUT                      
         LA    RF,7                SET LOOP CONTROL                             
         LA    R1,RDAROERO         SET A(ROTATION ARRAY)                        
BORB0080 EQU   *                                                                
         CLI   0(R1),C' '          DAY SET?                                     
         BE    BORB0120            NO                                           
         CLI   0(R1),0             DAY SET?                                     
         BE    BORB0120            NO                                           
         LA    RE,1(RE)            YES - TURN ON LOW-ORDER BIT                  
BORB0120 EQU   *                                                                
         SLL   RE,1                SHIFT UP 1 'DAY'                             
         LA    R1,1(R1)            BUMP TO NEXT ARRAY POSITION                  
         BCT   RF,BORB0080         GO BACK AND TEST NEXT                        
         SRL   RE,1                SHIFT BACK 1 'DAY'                           
         STC   RE,ELTBUILD+3       INSERT DAYS INTO ELEMENT                     
         MVC   ELTBUILD+4(4),RDAROEST                                           
*                                  INSERT START/END TIMES INTO ELEMENT          
         MVI   ELTBUILD+8,1        INSERT WEIGHT OF 1                           
*                                  THIS DOESN'T SEEM TO CHANGE                  
*                                     NO IDEA WHY.....                          
         L     R2,AIO2             A(BUY RECORD IN PROGRESS)                    
         GOTO1 LOADELT,DMCB,(R2),ELTBUILD,=C'ADD=CODE'                          
*                                                                               
*   NOTE:  DAY/TIME ELEMENTS ENTERED VIA ORBITS WILL BE DELETED IF              
*      BUY HAS BEEN ENTERED AS A 'DAILY' BUY.  THIS SHOULD NOT HAPPEN,          
*      BUT HAS BEEN HANDLED IF IT DOES.                                         
*                                                                               
         ZIC   RF,1(R8)                                                         
         AR    R8,RF                                                            
         B     BORB0040            GO BACK FOR NEXT ELEMENT                     
BORB0400 EQU   *                                                                
         B     EXIT                                                             
         DROP  R3,R8                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*   BUYCOMMT:  BUILDS BUY COMMENT ELEMENT FOR THE FIRST TWO COMMENTS            
*        IN THE RECORD, INSERTS THEM INTO THE NEW BUY RECORD.                   
***********************************************************************         
BUYCOMMT NTR1  BASE=*,LABEL=*                                                   
         L     R3,0(R1)            RESET A(X'41' RECORD)                        
         USING RDARREC,R3                                                       
*                                                                               
         L     R2,AIO2             A(BUY RECORD IN PROGRESS)                    
         LA    R4,2                LOOP CONTROL:                                
*                                     FIRST TWO ARE BUY COMMENTS                
*                                     NEXT  TWO ARE BUY ORDER COMMENTS          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         CLC   PROGNAME,SPACES     ANY PROGRAM NAME?                            
         BE    BCOM0020            NO  - PROCEED                                
         CLI   KATZEDI,C'Y'        KATZ/EDI USES ONLY THE FIRST 32              
         BNE   BCOM0005            WITH THE LAST 2 FOR DAYPART                  
*                                                                               
*                                  REMOVE OLD X'ED' DAYPART CODE                
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'ED',(R2)),0,0                   
         XC    ELEMENT,ELEMENT     FOR KATZ/EDI ADD THE DAYPART CODE            
         LA    R6,ELEMENT          ELEMENT                                      
         USING RBUYEDEL,R6                                                      
         MVI   RBUYEDCD,X'ED'                                                   
         MVI   RBUYEDLN,RBUYEDLQ                                                
         MVC   RBUYEDDP,PROGNAME+32                                             
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),(R2),ELEMENT,0                     
         DROP  R6                                                               
*                                                                               
         CLC   PROGNAME(32),SPACES ANY PROGRAM NAME?                            
         BE    BCOM0020            NO  - DON'T NEED COMMENT                     
         DROP  R8                                                               
*                                                                               
BCOM0005 EQU   *                                                                
         XC    ELTBUILD,ELTBUILD   CLEAR ELEMENT BUILD AREA                     
         MVI   ELTBUILD,X'04'      INSERT ELEMENT CODE                          
*                                  CLEAR WORKSPACE                              
         LA    R6,PROGNAME+33      SCAN PROGRAM NAME FOR BLANKS                 
         LA    RF,34               LOOP CONTROL                                 
         CLI   KATZEDI,C'Y'        KATZ/EDI USES ONLY THE FIRST 32              
         BNE   BCOM0010            WITH THE LAST 2 FOR DAYPART                  
         LA    R6,PROGNAME+31      SCAN PROGRAM NAME FOR BLANKS                 
         LA    RF,32               LOOP CONTROL                                 
BCOM0010 EQU   *                                                                
         CLI   0(R6),C' '          CHARACTER = SPACE?                           
         BNE   BCOM0015            NO  - LAST CHARACTER FOUND                   
         BCTR  R6,0                YES - BACK UP 1 SPACE                        
         BCT   RF,BCOM0010         LOOP THROUGH ALL                             
         DC    H'0'                SHOULDN'T HAPPEN:  SPACES CHECKED            
BCOM0015 EQU   *                                                                
         LA    RF,1(RF)            ADD 1 FOR KEYWORD (+2 -1 FOR EX)             
         MVC   ELTBUILD+2(2),=C'P='                                             
*                                  INSERT KEYWORD                               
         EX    RF,BCOM0505         MOVE PROGRAM NAME BY LENGTH                  
         LA    RF,3(RF)            RESTORE LENGTH + L(CONTROLS)                 
         STC   RF,ELTBUILD+1       INSERT LENGTH INTO ELEMENT                   
*                                                                               
* SKIP BUILDING P= COMMENT PROGRAM NAME ELEMENT                                 
*                                                                               
*        GOTO1 LOADELT,DMCB,(R2),ELTBUILD,=C'ADD=CODE'                          
*                                                                               
* BUILD DEDICATED PROGRAM NAME ELEMENT                                          
*                                                                               
         MVI   ELTBUILD,X'21'      PROGRAM NAME ELEMENT                         
         XC    ELTBUILD+2(L'ELTBUILD-2),ELTBUILD+2                              
         ZIC   RF,ELTBUILD+1       REUSE LENGTH FROM THE P= COMMENT             
         SHI   RF,2                ELEMENT TO BUILD LENGTH OF                   
         STC   RF,ELTBUILD+1       PROGRAM NAME ELEMENT                         
         SHI   RF,1                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ELTBUILD+2(0),PROGNAME                                           
         GOTO1 LOADELT,DMCB,(R2),ELTBUILD,=C'ADD=CODE'                          
*                                                                               
         MVI   PROGNAME,C' '       CLEAR THE PROGRAM NAME                       
         MVC   PROGNAME+1(L'PROGNAME-1),PROGNAME                                
         BCTR  R4,0                SUBTRACT 1 FROM COMMENT COUNT                
BCOM0020 EQU   *                                                                
         LA    R8,RDARELEM         SET A(X'01' ELEMENT)                         
         ZIC   RF,1(R8)            GET LENGTH                                   
         AR    R8,RF               BUMP TO 1ST COMMT ELEMENT                    
BCOM0040 EQU   *                                                                
****                                                                            
**** SKIP FOR REVISION                                                          
****                                                                            
         B     BCOM0400            SKIP REST OF COMMENTS                        
****                                                                            
         CLI   0(R8),0             END OF RECORD?                               
         BE    BCOM0400            YES - FINISHED                               
*                                                                               
         USING RDARCTEL,R8                                                      
*                                                                               
         XC    ELTBUILD,ELTBUILD   CLEAR ELEMENT BUILD AREA                     
         MVI   ELTBUILD,X'04'      INSERT ELEMENT CODE                          
         ZIC   R1,1(R8)            GET ELEMENT LENGTH                           
         LA    RF,3                DECREMENT FOR EX + CTRL                      
         SR    R1,RF                                                            
         LA    RE,59               MAX SIZE FOR REP COMMENTS = 60               
         CR    R1,RE               NEW INPUT VS REP MAX                         
         BNH   BCOM0060            ACCEPTABLE                                   
         LR    R1,RE               NOT ACCEPTABLE                               
BCOM0060 EQU   *                                                                
         EX    R1,BCOM0500         MOVE ELEMENT TO BUILT AREA                   
         LA    R1,3(R1)            SET ELEMENT LENGTH                           
         STC   R1,ELTBUILD+1       INSERT ELEMENT LENGTH                        
         GOTO1 LOADELT,DMCB,(R2),ELTBUILD,=C'ADD=CODE'                          
         ZIC   RF,1(R8)                                                         
         AR    R8,RF                                                            
         BCT   R4,BCOM0040         GO BACK FOR NEXT ELEMENT                     
*                                     IF COUNT < 2                              
         LA    R4,2                IF MORE COMMENTS, PUT TO                     
*                                     BUY ORDER COMMENT RECORDS                 
BCOM0080 EQU   *                                                                
         CLI   0(R8),0             END OF RECORD?                               
         BE    BCOM0400            YES - FINISHED                               
*                                                                               
         XC    ELTBUILD,ELTBUILD   CLEAR ELEMENT BUILD AREA                     
         MVI   ELTBUILD,X'84'      INSERT ELEMENT CODE                          
         ZIC   R1,1(R8)            GET ELEMENT LENGTH                           
         LA    RF,3                DECREMENT FOR EX + CTRL                      
         SR    R1,RF                                                            
*                                                                               
         CH    R1,=H'60'           TRUNCATE IF MORE THAN 60 CHARS.              
         BL    BCOM0090                                                         
         LA    R1,59               MAX - 1 FOR EX                               
*                                                                               
BCOM0090 EQU   *                                                                
         EX    R1,BCOM0510         MOVE ELEMENT TO BUILD AREA                   
         LA    R1,4(R1)            RESET ELEMENT LENGTH                         
         STC   R1,ELTBUILD+1       INSERT ELEMENT LENGTH                        
         MVI   ELTBUILD+2,X'80'    TURN ON 'SENT BY REP'                        
         GOTO1 LOADELT,DMCB,(R2),ELTBUILD,=C'ADD=CODE'                          
         ZIC   RF,1(R8)                                                         
         AR    R8,RF                                                            
         BCT   R4,BCOM0080         GO BACK FOR NEXT ELEMENT                     
*                                     IF COUNT < 2                              
BCOM0400 EQU   *                                                                
         B     EXIT                                                             
*                                                                               
BCOM0500 MVC   ELTBUILD+2(0),2(R8) SET UP BUY COMMENT                           
*                                                                               
BCOM0505 MVC   ELTBUILD+4(0),PROGNAME                                           
*                                  SET UP PROGRAM NAME COMMENT                  
BCOM0510 MVC   ELTBUILD+3(0),2(R8) SET UP BUY ORDER COMMENT                     
*                                                                               
         DROP  R3,R8                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*   EFFDATEL:  BUILD AN ALTERNATE X'02' ELEMENT IN EVENT THERE                  
*        ARE NO ORBIT RECORDS.                                                  
***********************************************************************         
EFFDATEL NTR1  BASE=*,LABEL=*                                                   
         L     R3,0(R1)            RESET A(X'41' RECORD)                        
         USING RDARREC,R3                                                       
         LA    R8,RDARELEM         SET A(X'01' ELEMENT)                         
         USING RDARBYEL,R8                                                      
*                                                                               
         XC    STARTDAY(2),STARTDAY                                             
*                                  CLEAR START/END DAY WORK AREA                
         XC    ELTBILD2,ELTBILD2   CLEAR ELEMENT BUILD AREA                     
         MVC   ELTBILD2(2),=X'0209'                                             
*                                  INSERT ELEMENT CODE/LENGTH                   
         GOTO1 =A(STARTEND),DMCB,RDARBYRO,ELTBILD2+2,RR=RELO                    
*                                  INSERT START/END DAY                         
                                                                                
         SR    RE,RE               INITIALIZE FINAL OUTPUT                      
         LA    RF,7                SET LOOP CONTROL                             
         LA    R1,RDARBYRO         SET A(ROTATION ARRAY)                        
EFFD0080 EQU   *                                                                
         CLI   0(R1),C' '          DAY SET?                                     
         BE    EFFD0120            NO                                           
         CLI   0(R1),0             DAY SET?                                     
         BE    EFFD0120            NO                                           
         LA    RE,1(RE)            YES - TURN ON LOW-ORDER BIT                  
EFFD0120 EQU   *                                                                
         SLL   RE,1                SHIFT UP 1 'DAY'                             
         LA    R1,1(R1)            BUMP TO NEXT ARRAY POSITION                  
         BCT   RF,EFFD0080         GO BACK AND TEST NEXT                        
         SRL   RE,1                SHIFT DOWN 1 'DAY' FOR                       
*                                     PROPER ALIGNMENT                          
         STC   RE,ELTBILD2+3       INSERT DAYS INTO ELEMENT                     
         ZICM  RF,RDARBYST,2       CHECK START TIME                             
         C     RF,=F'2400'         AFTER MIDNIGHT?                              
         BNH   EFFD0160            NO  - LEAVE AS IS.....                       
         S     RF,=F'2400'         YES - SUBTRACT 2400 FROM FIGURE              
EFFD0160 EQU   *                                                                
         STCM  RF,3,ELTBILD2+4     SAVE START TIME                              
*                                                                               
         CLC   RDARBYST,RDARBYET   IF SAME, SKIP END TIME                       
         BE    EFFD0210                                                         
*                                                                               
         ZICM  RF,RDARBYET,2       CHECK END   TIME                             
         C     RF,=F'2400'         AFTER MIDNIGHT?                              
         BNH   EFFD0200            NO  - LEAVE AS IS.....                       
         S     RF,=F'2400'         YES - SUBTRACT 2400 FROM FIGURE              
EFFD0200 EQU   *                                                                
         STCM  RF,3,ELTBILD2+6     SAVE END   TIME                              
*                                  INSERT START/END TIMES INTO ELEMENT          
EFFD0210 EQU   *                                                                
         MVI   ELTBILD2+8,1        INSERT WEIGHT OF 1                           
*                                  THIS DOESN'T SEEM TO CHANGE                  
*                                     NO IDEA WHY.....                          
         B     EXIT                                                             
*                                                                               
         DROP  R3,R8                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*   STARTEND:  CONVERTS ROTATION MATRIX AND START DAY TO ONE-BYTE               
*        START/END DAY, INSERTS INTO ADDRESS                                    
***********************************************************************         
STARTEND NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            A(INPUT: ROTATION+START DAY)                 
         L     R3,4(R1)            A(RECEIVING FIELD)                           
*                                                                               
         ZIC   RF,7(R2)            ROTATION START DAY                           
         SLL   RF,28               STRIP OFF ZONE BITS                          
         SRL   RF,28               SHIFT START DAY BACK                         
         STC   RF,ROTATDAY         SAVE ROTATION START DAY                      
*                                                                               
         LA    R8,0(R2)            A(ROTATION FIELD)                            
         LR    RE,R8               CALCULATE END OF ROTATION FIELD              
         LA    RE,7(RE)                                                         
         AR    R8,RF               GET A(1ST DAY+1)                             
         BCTR  R8,0                BACK OFF TO A(1ST ENTRY)                     
         LR    RF,R8               SAVE A(1ST ENTRY)                            
*                                     FOR WHEN 1ST ENTRY IS ONLY ENTRY          
         LA    R0,7                SET LOOP CONTROL TO SIX DAYS                 
STEX0040 EQU   *                                                                
         CR    R8,RE               END OF ROTATION FIELD REACHED?               
         BNE   STEX0080            NO                                           
         LA    R8,0(R2)            YES  - GO BACK TO FIRST LOCATION             
STEX0080 EQU   *                                                                
         CLI   0(R8),C' '          ARRAY POSITION USED?                         
         BE    STEX0200            NO  - SKIP IT                                
         CLI   0(R8),0             ARRAY POSITION USED?                         
         BE    STEX0200            NO  - SKIP IT                                
         CLI   STARTDAY,0          ANYTHING IN START DAY?                       
         BNE   STEX0120            YES - DON'T REPLACE                          
         LA    R4,7                NO  - CALCULATE STARTDAY                     
         SR    R4,R0               SUBTRACT REMAINING DAYS                      
         ZIC   R1,ROTATDAY         OFFSET BY ROTATION START DAY                 
         AR    R4,R1                                                            
         CH    R4,=H'7'            WRAPAROUND?                                  
         BNH   STEX0100            NO                                           
         SH    R4,=H'7'            YES - SUBTRACT 7                             
STEX0100 EQU   *                                                                
         STC   R4,STARTDAY         SAVE CALCULATED STARTDAY                     
STEX0120 EQU   *                                                                
         OC    0(1,R3),0(R3)       RECEIVING FIELD ENTRY MADE?                  
         BNZ   STEX0160            YES - START DAY ENTERED                      
         SLL   R4,4                NO  - MOVE START DAY TO HIGH NYBBLE          
         STC   R4,0(R3)            INSERT INTO RECORD                           
STEX0160 EQU   *                                                                
         LR    RF,R8               YES - SAVE NEW ARRAY POSITION                
STEX0200 EQU   *                                                                
         LA    R8,1(R8)            BUMP TO NEXT ARRAY LOCATION                  
         BCT   R0,STEX0040         GO BACK AND CHECK NEXT                       
         LA    R8,0(R2)            A(START OF ARRAY)                            
         BCTR  R8,0                BACK UP 1 POSITION                           
         SR    RF,R8               CALCULATED DISPLACEMENT                      
         STC   RF,ENDDAY           SAVE END DAY FOR EFF DATE SETTING            
         ZIC   RE,0(R3)            RETRIEVE START DAY                           
         AR    RF,RE               ADD START TO END                             
         STC   RF,0(R3)            PUT IT BACK IN RECORD                        
         LA    R8,0(R2)            COUNT NUMBER OF DAYS                         
         SR    RF,RF                                                            
         LA    R0,7                                                             
STEX0240 EQU   *                                                                
         CLI   0(R8),C' '          DAY ACTIVE?                                  
         BE    STEX0280            NO                                           
         CLI   0(R8),0             DAY ACTIVE?                                  
         BE    STEX0280            NO                                           
         LA    RF,1(RF)            YES - ADD 1                                  
STEX0280 EQU   *                                                                
         LA    R8,1(R8)            BUMP TO NEXT POSITION                        
         BCT   R0,STEX0240         GO BACK AND CHECK NEXT                       
         C     RF,=F'1'            COUNT = 1?                                   
         BH    STEX0320            NO  - HIGHER - EXIT                          
         BE    *+6                 YES - SET START=END IN RECORD                
         DC    H'0'                SHOULD NEVER HAPPEN                          
*                                     MEANS EMPTY ARRAY!!!                      
         ZIC   RF,0(R3)            RETRIEVE START/END DAY                       
         SLL   RF,28               DROP START DAY                               
         SRL   RF,24               MOVE END DAY BACK TO HI NYBBLE               
         NI    0(R3),X'0F'         CLEAR START DAY                              
         ZIC   RE,0(R3)            RETRIEVE 0/END DAY                           
         AR    RE,RF               ADD NEW START DAY                            
         STC   RE,0(R3)            MOVE IT BACK                                 
STEX0320 EQU   *                                                                
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*   BUYDETL:  BUILDS BUY EFFECTIVE DATE ELEMENTS, THEN INSERTS                  
*        THEM INTO THE NEW BUY RECORD.                                          
***********************************************************************         
BUYDETL  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R3,0(R1)            RESET A(X'41' RECORD)                        
         USING RDARREC,R3                                                       
*                                                                               
         GOTO1 DATCON,DMCB,(2,FLTDATES),(3,WORK)                                
         CLC   RTKODATE,WORK                                                    
         BNH   BDET0010                                                         
*                                                                               
         ST    R3,DMCB             SETUP CHOPPING CALL                          
         MVC   DMCB+4(3),RTKODATE                                               
         MVC   DMCB+8(1),STARTDAY                                               
         MVC   DMCB+9(1),ENDDAY                                                 
         MVC   DMCB+10(1),ROTATDAY                                              
         MVC   DMCB+11(1),ORBSTDAY                                              
         XC    WORK,WORK                                                        
         MVC   WORK(4),DATCON                                                   
         MVC   WORK+4(4),GETDAY                                                 
         MVC   WORK+8(4),ADDAY                                                  
         MVC   WORK+12(4),PERVERT                                               
         LA    RE,WORK                                                          
         ST    RE,DMCB+12                                                       
*                                                                               
         GOTO1 VREDARTK,DMCB                                                    
         BNE   BDETNO              BUY DELETED, EXIT                            
*                                                                               
BDET0010 EQU   *                                                                
         TM    FLAGS,FGTOTALQ      TOTAL ONLY, SKIP REST OF PROCESSING?         
         BO    BDETYES                                                          
*                                                                               
         LA    R8,RDARELEM         SET A(X'01' ELEMENT)                         
         DROP  R3                                                               
         USING RDARBDEL,R8         BUY DETAIL DESCRIP ELEMENT                   
*                                                                               
         L     R2,AIO2             A(BUY RECORD IN PROGRESS)                    
         USING RBUYREC,R2                                                       
*                                                                               
         NI    MISCFLAG,X'7F'      TURN OFF 'DAILY' BUY FLAG                    
         TM    RDARBDFL,X'80'      'DAILY' BUY?                                 
         BNO   BDET0020            NO                                           
         OI    MISCFLAG,X'80'      SET FLAG FOR 'DAILY' BUY                     
BDET0020 EQU   *                                                                
         TM    MISCFLAG,X'80'      'DAILY' BUY?                                 
         BNO   BDET0030            NO                                           
         CLI   ORBFLAG,C'Y'        ANY ORBIT RECORDS?                           
         BNE   BDET0040            NO                                           
         GOTO1 DELELT02,DMCB,RBUYREC                                            
*                                  CLEAR ORBIT X'02' DAY/TIME ELTS              
         MVI   ORBFLAG,C'N'        SET ORBIT FLAG TO NO                         
         OI    MISCFLAG,X'40'      SET 'ORBIT DROPPED' FLAG                     
         B     BDET0040                                                         
BDET0030 EQU   *                                                                
         CLI   ORBFLAG,C'Y'        ANY ORBIT RECORDS?                           
         BE    BDET0060            YES - DON'T NEED SECONDARY -                 
*                                  ORBIT HAS ALREADY ADDED THE X'02'            
*                                     ELT.  SHOULD NEVER ABE ORB W/             
*                                        'DAILY' RECORDS                        
BDET0040 EQU   *                                                                
         GOTO1 LOADELT,DMCB,(R2),ELTBILD2,=C'ADD=CODE'                          
         XC    ELTBILD2,ELTBILD2                                                
BDET0060 EQU   *                                                                
*                                  MOVE FROM DESCRIP ELT TO DETAIL              
         ZIC   RF,1(R8)            GET LENGTH                                   
         AR    R8,RF               BUMP TO 1ST DETAIL ELEMENT                   
BDET0080 EQU   *                                                                
         CLI   0(R8),0             END OF RECORD?                               
         BE    BDETYES             YES - FINISHED                               
*                                                                               
         USING RDARBUEL,R8                                                      
*                                                                               
         TM    MISCFLAG,X'80'      'DAILY' BUY?                                 
         BO    BDET0100            YES - SKIP CHECK FOR COST BREAK ->           
*                                     EACH ELEMENT BECOMES A BUY REC            
         GOTO1 DETLBRAK,DMCB,(RC),(R8),(R2)                                     
*                                  CHECK FOR COST BREAK                         
BDET0100 EQU   *                                                                
         MVI   DETLFLAG,C'Y'       SET DETAILS TO YES                           
         OC    RBUYNW,RBUYNW       NUMBER/WEEK FILLED IN?                       
         BNZ   BDET0120            YES                                          
*                                                                               
         MVC   RBUYNW,RDARBUSW     NO  - INSERT NUMBER PER WEEK                 
BDET0120 EQU   *                                                                
         XC    ELTBUILD,ELTBUILD   CLEAR ELEMENT BUILD AREA                     
         MVC   ELTBUILD(2),=X'030B'                                             
*                                  INSERT ELEMENT CODE/LENGTH                   
         GOTO1 DATCON,DMCB,(2,RDARBUSD),(0,WORK)                                
*                                  CONVERT START DATE TO EBCDIC                 
         TM    MISCFLAG,X'80'      'DAILY' BUY?                                 
         BNO   BDET0130            NO  -                                        
         MVC   WORK+12(6),WORK     YES - USE START AS IS, THEN                  
*                                     SET END TO START                          
         B     BDET0190                                                         
BDET0130 EQU   *                                                                
         ZIC   RF,RDARBUWK         CALCULATE NUMBER OF DAYS                     
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         BCTR  RF,0                MAKE WEEKS ZERO RELATIVE                     
         MH    RF,=H'7'            MULTIPLY WEEKS BY 7                          
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(RF)                                      
*                                                                               
         GOTO1 GETDAY,DMCB,WORK,WORK+12                                         
*                                  GET DAY OF START WEEK                        
         ZIC   RF,DMCB             GET DAY OF WEEK                              
         BCTR  RF,0                MAKE DAY ZERO RELATIVE                       
         LTR   RF,RF               MONDAY?                                      
         BZ    BDET0140            YES - LEAVE                                  
         LNR   RF,RF               NEGATE REGISTER                              
         GOTO1 ADDAY,DMCB,WORK,WORK+12,(RF)                                     
*                                  BACK UP TO MONDAY                            
*                                                                               
* IN CASE OF OUT OF WEEK ROTATORS, COMPARE BUY START DAY AND ROTATION           
* START DAY TO GET THE CORRECT MONDAY DATE                                      
*                                                                               
         CLC   STARTDAY,ROTATDAY                                                
         BNL   BDET0135                                                         
         MVC   WORK(6),WORK+12                                                  
         LA    RF,7                                                             
         GOTO1 ADDAY,DMCB,WORK,WORK+12,(RF)                                     
*                                                                               
BDET0135 DS    0H                                                               
         MVC   WORK(6),WORK+12                                                  
*                                                                               
BDET0140 EQU   *                                                                
         CLI   ORBSTDAY,0          ANY ORBIT START DAY?                         
         BZ    BDET0160            NO  - USE HEADER START DAY                   
         ZIC   RF,ORBSTDAY         YES - USE IT                                 
         B     BDET0180                                                         
BDET0160 EQU   *                                                                
         ZIC   RF,STARTDAY         ADD START DAY                                
BDET0180 EQU   *                                                                
         BCTR  RF,0                MAKE ZERO RELATIVE                           
         GOTO1 ADDAY,DMCB,WORK,WORK+12,(RF)                                     
*                                  BUMP TO START DAY IN WEEK                    
BDET0190 EQU   *                                                                
         GOTO1 DATCON,DMCB,(0,WORK+12),(3,ELTBUILD+2)                           
*                                  INSERT START DATE INTO ELEMENT               
         TM    MISCFLAG,X'80'      'DAILY' BUY?                                 
         BNO   BDET0200            NO                                           
         MVC   ELTBUILD+5(3),ELTBUILD+2                                         
*                                  YES - SET END DATE = START DATE              
         B     BDET0260                                                         
BDET0200 EQU   *                                                                
*                                                                               
         GOTO1 GETDAY,DMCB,WORK+6,WORK+12                                       
*                                  GET DAY OF END   WEEK                        
         ZIC   RF,DMCB             GET DAY OF WEEK                              
         BCTR  RF,0                MAKE DAY ZERO RELATIVE                       
         LTR   RF,RF               MONDAY?                                      
         BZ    BDET0240            YES - LEAVE                                  
         LNR   RF,RF               NEGATE REGISTER                              
         CLC   STARTDAY,ENDDAY     START/END ON SAME DAY?                       
*                                     (SINGLE-DAY BUY?)                         
         BE    BDET0220            YES - DON'T BUMP TO NEXT WEEK                
         BL    BDET0220            START < END DAY:  NOT AN                     
*                                     OOWR - DON'T BUMP                         
         LA    RF,7(RF)            BUMP IT INTO NEXT WEEK                       
BDET0220 EQU   *                                                                
         GOTO1 ADDAY,DMCB,WORK+6,WORK+12,(RF)                                   
*                                  BACK UP TO MONDAY                            
*                                                                               
* IN CASE OF OUT OF WEEK ROTATORS, COMPARE BUY START DAY AND ROTATION           
* START DAY TO GET THE CORRECT MONDAY DATE                                      
*                                                                               
         CLC   STARTDAY,ROTATDAY                                                
         BNL   BDET0230                                                         
         MVC   WORK+6(6),WORK+12                                                
         LA    RF,7                                                             
         GOTO1 ADDAY,DMCB,WORK+6,WORK+12,(RF)                                   
*                                                                               
BDET0230 DS    0H                                                               
         MVC   WORK+6(6),WORK+12                                                
*                                                                               
BDET0240 EQU   *                                                                
         ZIC   RF,ENDDAY           ADD END   DAY                                
         BCTR  RF,0                MAKE ZERO RELATIVE                           
         GOTO1 ADDAY,DMCB,WORK+6,WORK+12,(RF)                                   
*                                  BUMP TO END   DAY IN WEEK                    
         GOTO1 DATCON,DMCB,(0,WORK+12),(3,ELTBUILD+5)                           
*                                  INSERT END   DATE INTO ELEMENT               
*                                                                               
BDET0260 EQU   *                                                                
         MVI   ELTBUILD+8,X'80'    SET TO 'EVERY WEEK'                          
         CLC   RBUYNW,RDARBUSW     HEADER #/WK = DETAIL #/WK?                   
         BE    BDET0280            YES                                          
         OI    ELTBUILD+8,X'01'    NO  - PUT IN 'OVERRIDE' FLAG                 
         B     BDET0290                                                         
BDET0280 EQU   *                                                                
         CLI   RDARBUSW,0          SPOT PER WEEK ZERO??                         
         BNE   BDET0290            RBUYNW MIGHT GET SET TO SOMETHING            
         OI    ELTBUILD+8,X'01'    ELSE LATER, SET OVERRIDE ANYWAY              
*                                                                               
BDET0290 EQU   *                                                                
         MVC   ELTBUILD+9(1),RDARBUSW                                           
*                                  INSERT NUMBER PER WEEK                       
         MVC   ELTBUILD+10(1),RDARBUWK                                          
*                                  INSERT NUMBER OF WEEKS                       
         ZIC   RF,RDARBUWK         ACCUMULATE NUMBER OF WEEKS                   
         SR    R0,R0                  AND CALC # SPOTS                          
         ZIC   R1,RDARBUSW         NUMBER SPOTS PER WEEK                        
         MR    R0,RF               # SPOTS X # WEEKS                            
         L     RE,SPOTCTR                                                       
         AR    RE,R1               ACCUMULATE NUMBER OF SPOTS                   
         ST    RE,SPOTCTR          STORE IT BACK                                
         A     RF,WEEKCTR          ACCUMULATE NUMBER OF WEEKS                   
         ST    RF,WEEKCTR          STORE IT BACK                                
         L     R2,AIO2             A(BUY RECORD IN PROGRESS)                    
         GOTO1 LOADELT,DMCB,(R2),ELTBUILD,=C'ADD=CODE'                          
         TM    MISCFLAG,X'80'      'DAILY' BUY?                                 
         BNO   BDET0300            NO  - GO BACK FOR NEXT ELEMENT               
         BAS   RE,GENDAILY         YES - GENERATE A BUY RECORD                  
BDET0300 EQU   *                                                                
         ZIC   RF,1(R8)                                                         
         AR    R8,RF                                                            
         B     BDET0080            GO BACK FOR NEXT ELEMENT                     
BDETYES  SR    RC,RC                                                            
BDETNO   LTR   RC,RC                                                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*   GENDAILY:  FOR 'DAILY' BUYS, EACH DETAIL IS TO BE A SEPARATE BUY            
*        RECORD.  THE DETAIL'S DETAILS ARE INSERTED INTO THE HEADER,            
*        AND THE RECORD WRITTEN TO THE FILE.                                    
***********************************************************************         
GENDAILY NTR1                                                                   
         OI    RBUYFLG2,X'80'      SET 'DAILY ORDER' FLAG                       
         MVC   RBUYNW,RDARBUSW     INSERT NUMBER SPOTS/WEEK                     
         MVC   RBUYTSPT,RDARBUSW   INSERT TOTAL SPOT: SAME AS SPTS/WK           
         MVC   RBUYCOS,RDARBU$$    INSERT SPOT COST                             
         MVC   BUYCOST,RDARBU$$    SAVE   SPOT COST FOR CALCULATION             
*                                  INSERT ELEMENT CODE/LENGTH                   
         GOTO1 DATCON,DMCB,(2,RDARBUSD),(0,WORK)                                
*                                  CONVERT START DATE TO EBCDIC                 
         GOTO1 GETDAY,DMCB,WORK,WORK+12                                         
*                                  GET DAY OF WEEK OF START DAY                 
         ZIC   RF,DMCB             GET DAY OF WEEK:  START DAY                  
         SLL   RF,4                SHIFT TO LOW ORDER, HI NYBBLE                
         ZIC   RE,DMCB             GET DAY OF WEEK:  END   DAY                  
         AR    RF,RE               ADD END DAY TO START DAY                     
         STC   RF,RBUYSTED         INSERT START/END DAYS                        
         LA    RF,RBUYELEM         SET A(01 ELEMENT)                            
GDAI0020 EQU   *                                                                
         ZIC   RE,1(RF)            GET LENGTH                                   
         AR    RF,RE               BUMP TO NEXT                                 
         CLI   0(RF),0             END OF RECORD?                               
         BNE   *+6                 NO                                           
         DC    H'0'                YES - SHOULDN'T HAPPEN                       
         CLI   0(RF),2             DAY/TIME ELEMENT?                            
         BNE   GDAI0020            NO  - GO BACK FOR NEXT                       
         MVC   RBUYDYIN-RBUYDYEL(1,RF),RBUYSTED                                 
*                                  YES - INSERT START/END FROM HDR              
         ZIC   RE,RBUYSTED         GET ST/END DAYS                              
         SRL   RE,4                DROP THE END DAY                             
         LA    R3,DAYTABLE                                                      
         AR    R3,RE               ADD DAY TO TABLE ADDR                        
         MVC   RBUYDAYS-RBUYDYEL(1,RF),0(R3)                                    
*                                  INSERT DAYS INTO ELEMENT -                   
*                                     ALWAYS A SINGLE DAY                       
         GOTO1 =A(GENREC),RR=RELO                                               
*                                                                               
         ZIC   RF,BUYLINE#         BUMP BUYLINE #                               
         LA    RF,1(RF)                                                         
         STC   RF,BUYLINE#                                                      
*                                                                               
* KEEP CURRENT AGENCY BUY 'MASTER LINE NUMBER'                                  
*        STC   RF,RBUYKMLN         INSERT NEW NUMBER IN MASTER LINE#            
         STC   RF,RBUYKLIN         INSERT NEW NUMBER IN LINE #                  
         GOTO1 DELELT,DMCB,RBUYREC                                              
         MVI   DETLFLAG,C'N'       RESET DETAIL COUNT TO NONE                   
         B     EXIT                                                             
*                                                                               
DAYTABLE DC    X'8040201008040201'                                              
*                                                                               
         DROP  R2,R8                                                            
         EJECT                                                                  
***********************************************************************         
*   DETLBRAK:  IF DETAIL COST IS DIFFERENT THAN BUYHDR COST, AND                
*      PREVIOUS DETAILS HAVE BEEN ENCOUNTERED WITHIN THE BUYHDR                 
*      GROUP, THE FOLLOWING STEPS MUST BE TAKEN:                                
*         1.  THE BUY MUST BE OUTPUT                                            
*         2.  THE NEXT BUYLINE NUMBER MUST BE CALCULATED                        
*         3.  THE RECORD MUST BE CLEARED OF DETAIL INFORMATION                  
*         4.  PROCESSING OF THE NEW DETAIL WILL THEN CONTINUE                   
***********************************************************************         
DETLBRAK NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R3,4(R1)            RESET A(DETAIL ELEMENT)                      
         USING RDARBUEL,R3         BUY DETAIL ELEMENT                           
*                                                                               
         L     R4,8(R1)            RESET A(RBUYREC)                             
         USING RBUYREC,R4                                                       
*                                                                               
         CLI   DETLFLAG,C'N'       ANY DETAILS ENCOUNTERED?                     
         BNE   DBRA0040            YES - CHECK FOR $$ CHANGE                    
         MVC   RBUYCOS,RDARBU$$    NO  - INSERT BUY COST IN CASE                
*                                     DIFFERENT FROM HEADER COST                
         MVC   BUYCOST,RBUYCOS     SAVE COST FOR CALCULATION                    
         MVC   RBUYNW,RDARBUSW     INSERT SPOTS/WK IN CASE DIFFERENT            
*                                                                               
         B     DBRA0200            EXIT                                         
DBRA0040 EQU   *                                                                
         CLC   RBUYCOS,RDARBU$$    BUY COST = DETAIL COST?                      
         BE    DBRA0200            YES - FINISHED                               
*                                                                               
         LR    R6,R4                                                            
         MVI   ELCODE,X'10'        MISC FLAG ELEMENT                            
         BRAS  RE,GETEL                                                         
         BE    DBRA0050                                                         
*                                                                               
WKD      USING RBUYXXEL,ELEM                                                    
         XC    ELEM,ELEM                                                        
         MVI   WKD.RBUYXXCD,RBUYXXCQ                                            
         MVI   WKD.RBUYXXLN,RBUYXXLQ                                            
         OI    WKD.RBUYXXFG,X'40'  MARK COST OVERRIDE FLAG                      
         DROP  WKD                                                              
         GOTO1 LOADELT,DMCB,(R4),ELEM,=C'ADD=CODE'                              
         B     DBRA0055                                                         
*                                                                               
DBRA0050 EQU   *                                                                
         USING RBUYXXEL,R6                                                      
         OI    RBUYXXFG,X'40'      MARK COST OVERRIDE FLAG                      
         DROP  R6                                                               
*                                                                               
DBRA0055 EQU   *                                                                
         GOTO1 =A(GENREC),RR=RELO                                               
*                                                                               
         MVC   RBUYCOS,RDARBU$$    INSERT NEW COST                              
         MVC   BUYCOST,RBUYCOS     SAVE COST FOR CALCULATION                    
         MVC   RBUYNW,RDARBUSW     INSERT SPOTS/WK IN CASE DIFFERENT            
         ZIC   RF,BUYLINE#         BUMP BUYLINE #                               
         LA    RF,1(RF)                                                         
         STC   RF,BUYLINE#                                                      
*                                                                               
* KEEP CURRENT AGENCY BUY 'MASTER LINE NUMBER'                                  
*        STC   RF,RBUYKMLN         INSERT NEW NUMBER IN MASTER LINE#            
         STC   RF,RBUYKLIN         INSERT NEW NUMBER IN LINE #                  
         GOTO1 DELELT,DMCB,RBUYREC                                              
         MVI   DETLFLAG,C'N'       RESET DETAIL COUNT TO NONE                   
DBRA0200 EQU   *                                                                
         B     EXIT                                                             
         DROP  R3,R4                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*   GENCOMMT:  GENERATE A COMMENT RECORD IN THOSE INSTANCES WHEN                
*      A PROGRAM NAME IS PRESENT, AND NO DARE COMMENTS HAVE BEEN                
*      FOUND.  OTHERWISE, THE PROGRAM NAME AS A COMMENT WILL BE                 
*      SKIPPED.                                                                 
***********************************************************************         
GENCOMMT NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            RESET A(BUYREC)                              
         USING RBUYREC,R2                                                       
*                                                                               
         CLI   KATZEDI,C'Y'        KATZ/EDI USES ONLY THE FIRST 32              
         BNE   GCOM0005            WITH THE LAST 2 FOR DAYPART                  
*                                                                               
*                                  REMOVE OLD X'ED' DAYPART CODE                
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'ED',(R2)),0,0                   
         XC    ELEMENT,ELEMENT     FOR KATZ/EDI ADD THE DAYPART CODE            
         LA    R6,ELEMENT          ELEMENT                                      
         USING RBUYEDEL,R6                                                      
         MVI   RBUYEDCD,X'ED'                                                   
         MVI   RBUYEDLN,RBUYEDLQ                                                
         MVC   RBUYEDDP,PROGNAME+32                                             
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),(R2),ELEMENT,0                     
         DROP  R6                                                               
*                                                                               
GCOM0005 EQU   *                                                                
         XC    ELTBUILD,ELTBUILD   CLEAR ELEMENT BUILD AREA                     
         MVI   ELTBUILD,X'04'      INSERT ELEMENT CODE                          
*                                  CLEAR WORKSPACE                              
         LA    R6,PROGNAME+33      SCAN PROGRAM NAME FOR BLANKS                 
         LA    RF,34               LOOP CONTROL                                 
         CLI   KATZEDI,C'Y'        KATZ/EDI USES ONLY THE FIRST 32              
         BNE   GCOM0010            WITH THE LAST 2 FOR DAYPART                  
         LA    R6,PROGNAME+31      SCAN PROGRAM NAME FOR BLANKS                 
         LA    RF,32               LOOP CONTROL                                 
GCOM0010 EQU   *                                                                
         CLI   0(R6),C' '          CHARACTER = SPACE?                           
         BNE   GCOM0015            NO  - LAST CHARACTER FOUND                   
         BCTR  R6,0                YES - BACK UP 1 SPACE                        
         BCT   RF,GCOM0010         LOOP THROUGH ALL                             
         DC    H'0'                SHOULDN'T HAPPEN:  SPACES CHECKED            
GCOM0015 EQU   *                                                                
         LA    RF,1(RF)            ADD 1 FOR KEYWORD (+2 -1 FOR EX)             
         MVC   ELTBUILD+2(2),=C'P='                                             
*                                  INSERT KEYWORK                               
         EX    RF,GCOM0505         MOVE PROGRAM NAME BY LENGTH                  
         LA    RF,3(RF)            RESTORE LENGTH + L(CONTROLS)                 
         STC   RF,ELTBUILD+1       INSERT LENGTH INTO ELEMENT                   
*                                                                               
* SKIP BUILDING P= COMMENT PROGRAM NAME ELEMENT                                 
*                                                                               
*        GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),RBUYREC,ELTBUILD,         X        
               =C'ADD=CODE'                                                     
*                                                                               
* BUILD DEDICATED PROGRAM NAME ELEMENT                                          
*                                                                               
         MVI   ELTBUILD,X'21'      PROGRAM NAME ELEMENT                         
         XC    ELTBUILD+2(L'ELTBUILD-2),ELTBUILD+2                              
         ZIC   RF,ELTBUILD+1       REUSE LENGTH FROM THE P= COMMENT             
         SHI   RF,2                ELEMENT TO BUILD LENGTH OF                   
         STC   RF,ELTBUILD+1       PROGRAM NAME ELEMENT                         
         SHI   RF,1                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ELTBUILD+2(0),PROGNAME                                           
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),RBUYREC,ELTBUILD,         X        
               =C'ADD=CODE'                                                     
*                                                                               
         MVI   PROGNAME,C' '       CLEAR THE PROGRAM NAME                       
         MVC   PROGNAME+1(L'PROGNAME-1),PROGNAME                                
         B     EXIT                                                             
*                                                                               
GCOM0505 MVC   ELTBUILD+4(0),PROGNAME                                           
*                                                                               
         DROP  R2                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE RIPPED FROM REDAR20                                                   
* ADD OR UPDATE RECORD TO FILE                                                  
***********************************************************************         
GENREC   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   AIO,AIO2            SET IO AREA = IOAREA2                        
         L     R2,AIO                                                           
         USING RBUYREC,R2                                                       
*                                                                               
******** TEMP TRAP TO LOOK FOR BAD X'0B01' SHADOW BUYS                          
         CLC   =X'0B01',RBUYREC                                                 
         BNE   TEMP30                                                           
         CLI   RBUYELEM,X'01'                                                   
         BE    TEMP30                                                           
         DC    H'0'                                                             
TEMP30   DS    0H                                                               
******** TEMP TRAP TO LOOK FOR BAD X'0B01' SHADOW BUYS                          
*                                                                               
*   INSERT TOTAL SPOTS, TOTAL COST, AND TOTAL WEEKS FIGURES                     
*                                                                               
         MVC   RBUYTSPT,SPOTCTR+2  LAST TWO POSITIONS ONLY                      
         L     RF,SPOTCTR          ACCUMULATE TOTAL SPOTS                       
         A     RF,ORDTOTSP                                                      
         ST    RF,ORDTOTSP         SAVE ORDER TOTAL SPOTS                       
         MVC   RBUYTWKS,WEEKCTR+3  LAST POSITION ONLY                           
         ZICM  RF,RBUYTSPT,2       LOAD TOTAL SPOTS                             
         SR    R0,R0                                                            
         L     R1,BUYCOST          LOAD COST PER SPOT                           
         MR    R0,RF               # SPOTS X COST/SPOT                          
         STCM  R1,15,RBUYTCOS      INSERT TOTAL COST                            
         A     R1,ORDTOT$$         ACCUMULATE ORDER TOTAL DOLLARS               
         ST    R1,ORDTOT$$         SAVE ORDER TOTAL DOLLARS                     
         XC    SPOTCTR,SPOTCTR     CLEAR ACCUMULATORS                           
         XC    WEEKCTR,WEEKCTR                                                  
         XC    BUYCOST,BUYCOST                                                  
*                                                                               
         MVC   SAVEKEY,KEY         SAVE CURRENT KEY FOR RESTART                 
         MVC   KEY,RBUYKEY         GET KEY OF BUY                               
*                                  LOOK FOR OLD RECORD                          
*                                     GENCON DOESN'T LIKE 'ADDREC'              
         MVC   KEYSAVE,KEY                                                      
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         OI    DMINBTS,X'08'       RETURN DELETED KEYS ALSO                     
         GOTO1 HIGH                READ THE KEY                                 
         CLC   KEYSAVE(27),KEY     KEY ALREADY ON FILE?                         
         BE    GENR0040            YES - RECORD/KEY MUST BE REPLACED            
*                                                                               
         MVC   KEY(27),KEYSAVE     NO  - RESET NEW KEY                          
         CLI   RBUYVER,1           VERSION = 1?                                 
         BE    GENR0020            YES - ORIGINAL CREATION                      
         MVI   RBUYCHGI,C'A'       NO  - ADDED ON THIS PASS                     
         MVI   RBUYCHGI+1,0        CLEAR SECOND CHG BYTE                        
GENR0020 EQU   *                                                                
         GOTO1 ADDREC              ADD THE RECORD/KEY                           
*                                     GENCON TRAPS ERRORS                       
         B     GENR0080            RECORD ADDED SUCCESSFULLY                    
GENR0040 EQU   *                                                                
*                                                                               
*                                     OLD RECORD MAY HAVE BEEN DELETED:         
*                                     STILL MUST ACCESS KEY/RECORD              
*                                        FOR UPDATE                             
*                                                                               
*                                                                               
         NI    KEY+27,X'FF'-X'80'  RESTORE KEY                                  
         MVC   AIO,AIOAREA         SET ALTERNATE IO AREA                        
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         OI    DMINBTS,X'08'       RETURN DELETED KEYS ALSO                     
         GOTO1 GETREC              RETRIEVE ORIGINAL RECORD                     
*                                                                               
         MVC   AIO,AIO2            SET IO AREA FOR NEW BUY                      
         GOTO1 PUTREC              WRITE UPDATED RECORD TO FILE                 
         GOTO1 WRITE               REWRITE CLEARED KEY FOR RECORD               
GENR0080 EQU   *                                                                
GENR0100 EQU   *                                                                
         MVC   AIO,AIO1            RESET A(X'41' RECORD AREA)                   
         MVC   KEY,SAVEKEY         RESET KEY FOR X'41' RECS                     
         GOTO1 HIGH                                                             
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE TSAR                                                               
***********************************************************************         
INITTSAR NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ICM   R0,14,=X'D9000A'                                                 
         ICM   R0,1,=AL1(QTSAR)                                                 
         GOTOX CALLOV,DMCB,0,(R0)                                               
         MVC   VTSAR,0(R1)         VTSAR                                        
*                                                                               
         XC    TBLOCK,TBLOCK                                                    
         GOTOX GOTSAR,TSAINI                                                    
*                                                                               
         XC    LISTSTRT,LISTSTRT                                                
         XC    LISTLAST,LISTLAST                                                
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FOR ALL X'0B01' AND X'0B' BUY RECORDS:                                        
* BUILD BUY ATTRIBUTE TSAR RECORDS FOR DAYS, TIMES AND PROGRAM NAME             
* BUILD BUY SEGMENTS                                                            
* FIRST AND SECOND LOOP BUILD BUY ATTRIBUTE RECORDS FOR BOTH BUY TYPES          
* THIRD AND FORTH LOOP BUILD BUY SEGMENT RECORDS FOR BOTH BUY TYPES             
***********************************************************************         
BLDTSREC NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         NI    FLAGS,X'FF'-FGBSEGQ BUY SEGMENT NOT YET BUILD                    
*                                                                               
         XC    RPSPTTOT,RPSPTTOT   CLEAR TOTALS FOR PRINTING                    
         XC    RPORDTOT,RPORDTOT                                                
         XC    AGSPTTOT,AGSPTTOT                                                
         XC    AGORDTOT,AGORDTOT                                                
*                                                                               
*                                                                               
* PROCESS AGENCY SHADOW BUYS X'0B01'                                            
*                                                                               
BLDTR05  DS    0H                                                               
         MVC   AIO,AIO1            SET IO AREA                                  
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RBUYKEY,R6                                                       
         MVC   RBUYKTYP(2),=X'0B01' AGENCY BUY SHADOW RECORD TYPE               
         MVC   RBUYKREP,TWAAGY     INSERT REP CODE                              
         MVC   RBUYKCON,COMPCON    INSERT CONTRACT #, 9/COMP/REV                
         DROP  R6                                                               
*                                                                               
BLDTR10  DS    0H                                                               
         NI    DMINBTS,X'FF'-X'08' DON'T PASS DELETES                           
         GOTO1 HIGH                                                             
*                                                                               
BLDTR20  DS    0H                                                               
         CLC   KEY(22),KEYSAVE                                                  
         BNE   BLDTR200            AGENCY CANCELLED ALL BUYS                    
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING RBUYREC,R6          CHECK AND SKIP                               
         CLI   RBUYCHGI,C'C'       CANCELLED BUT NOT DELETED                    
         BE    BLDTR40                                                          
         DROP  R6                                                               
*                                                                               
         TM    FLAGS,FGBSEGQ       WHICH TSAR RECORDS ARE WE BUILDING?          
         BO    BLDTR30                                                          
*                                                                               
* BUILD DAYS, TIMES AND PROGRAM NAME TSAR RECORDS                               
*                                                                               
         GOTOX (X'01',=A(BLDDTP)),RR=RELO                                       
         GOTO1 GOTSAR,TSAADD                                                    
*                                                                               
         GOTOX (X'02',=A(BLDDTP)),RR=RELO                                       
         GOTO1 GOTSAR,TSAADD                                                    
*                                                                               
         GOTOX (X'03',=A(BLDDTP)),RR=RELO                                       
         BH    BLDTR40                                                          
         GOTO1 GOTSAR,TSAADD                                                    
         B     BLDTR40             TSAR RECORDS                                 
*                                                                               
BLDTR30  DS    0H                  BUILD BUY SEGMENTS                           
         GOTO1 =A(BLDSEGS),RR=RELO                                              
*                                                                               
BLDTR40  DS    0H                                                               
         NI    DMINBTS,X'FF'-X'08' DON'T PASS DELETES                           
         GOTO1 SEQ                                                              
         B     BLDTR20                                                          
*                                                                               
* PROCESS REPPAK BUYS                                                           
*                                                                               
BLDTR200 DS    0H                                                               
         CLC   =X'0B00',KEYSAVE    REPPAK BUYS PROCESSED ALREADY?               
         BE    BLDTR300                                                         
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RBUYKEY,R6                                                       
         MVI   RBUYKTYP,X'0B'      REPPAK BUY RECORD TYPE                       
         MVC   RBUYKREP,TWAAGY     INSERT REP CODE                              
         MVC   RBUYKCON,COMPCON    INSERT CONTRACT #, 9/COMP/REV                
         B     BLDTR10                                                          
         DROP  R6                                                               
*                                                                               
BLDTR300 DS    0H                                                               
         TM    FLAGS,FGBSEGQ                                                    
         BO    BLDTRX                                                           
         OI    FLAGS,FGBSEGQ                                                    
         B     BLDTR05                                                          
*                                                                               
BLDTRX   DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BUILD DAYS, TIMES AND PROGRAM NAMES TSAR RECORDS                              
* BUY RECORD ALREADY DELIVERED TO AIO                                           
* P1,HOB, X'01' = DAYS                                                          
*         X'02' = TIMES                                                         
*         X'03' = PROGRAM NAME                                                  
***********************************************************************         
BLDDTP   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    TSARREC,TSARREC                                                  
         STCM  RF,8,TR.TLKTYP                                                   
*                                                                               
         MVC   SVELCODE,ELCODE                                                  
*                                                                               
         CLI   TR.TLKTYP,X'01'     DAYS                                         
         BE    BLDDTP10                                                         
         CLI   TR.TLKTYP,X'02'     TIMES                                        
         BE    BLDDTP10                                                         
         CLI   TR.TLKTYP,X'03'     PROGRAM NAME                                 
         BE    BLDDTP50                                                         
         DC    H'0'                                                             
*                                                                               
BLDDTP10 DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RBUYDYEL,R6                                                      
*                                                                               
         LA    R3,TR.TLDATA                                                     
*                                                                               
BLDDTP20 DS    0H                                                               
         CLI   TR.TLKTYP,X'01'     DAYS                                         
         BNE   BLDDTP30                                                         
         MVC   0(2,R3),RBUYDYIN                                                 
*                                                                               
* IF SINGLE DAY, CHANGE LEFTMOST NIBBLE TO X'F' SO SINGLE DAY WILL SORT         
* AFTER DAY RANGES (IE: WED, X'33' WILL BECOME 'F3')                            
*                                                                               
         XC    HALF,HALF                                                        
         MVZ   HALF(1),RBUYDYIN                                                 
         ZIC   R1,HALF                                                          
         SRL   R1,4                                                             
         STC   R1,HALF             SHIFT START DAY TO LOWER HALF BYTE           
         MVN   HALF+1(1),RBUYDYIN                                               
         CLC   HALF(1),HALF+1      SINGLE DAY?                                  
         BNE   BLDDTP25                                                         
         OI    0(R3),X'F0'         START DAY NIBBLE IS SET TO X'F'              
*                                                                               
BLDDTP25 DS    0H                                                               
         AHI   R3,2                                                             
         B     BLDDTP40                                                         
*                                                                               
BLDDTP30 DS    0H                  TIMES                                        
         MVC   0(4,R3),RBUYDYT1                                                 
*                                                                               
         CLC   RBUYDYT1,=Y(0459)   00:01AM - 04:59AM                            
         BH    BLDDTP35            SORTS LATER SINCE BROADCAST DAY              
         ZICM  RE,RBUYDYT1,2       ENDS AT 4:59AM                               
         AHI   RE,2400                                                          
         STCM  RE,3,0(R3)                                                       
*                                                                               
BLDDTP35 DS    0H                                                               
         OC    RBUYDYT2,RBUYDYT2   SINGLE TIME?                                 
         BZ    BLDDTP38                                                         
         CLC   RBUYDYT2,=Y(0459)                                                
         BH    BLDDTP38                                                         
         ZICM  RE,RBUYDYT2,2                                                    
         AHI   RE,2400                                                          
         STCM  RE,3,2(R3)                                                       
*                                                                               
BLDDTP38 DS    0H                                                               
         AHI   R3,4                                                             
*                                                                               
BLDDTP40 DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         BE    BLDDTP20                                                         
         DROP  R6                                                               
*                                                                               
         LA    RE,TR.TLKTYP        INSERT VARIABLE LENGTH                       
         SR    R3,RE                                                            
         AHI   R3,2                OVERHEAD                                     
         STCM  R3,3,TR.TLLEN                                                    
         CHI   R3,255              CHECK IF ENOUGH ROOM                         
         BNH   BLDDTPX                                                          
         DC    H'0'                                                             
*                                                                               
* PROGRAM NAME                                                                  
*                                                                               
BLDDTP50 DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'21'                                                     
         BRAS  RE,GETEL                                                         
         BE    BLDDTP60                                                         
         MVC   ELCODE,SVELCODE                                                  
         B     EXITH                                                            
*                                                                               
BLDDTP60 DS    0H                                                               
         USING RBUYPGEL,R6                                                      
         ZIC   R1,RBUYPGLN                                                      
         SHI   R1,3                CODE+LENGTH+OVERHEAD                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TR.TLDATA(0),RBUYPGM                                             
*                                                                               
         AHI   R1,4                TYPE+OVERHEAD                                
         STCM  R1,3,TR.TLLEN       INSERT VARIABLE LENGTH                       
*                                                                               
BLDDTPX  DS    0H                                                               
         MVC   ELCODE,SVELCODE                                                  
         B     EXITOK                                                           
         DROP  R6                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOOP THROUGH ALL X'0B01'/X'0B' RECORDS. FOR EACH RECORD, LOOP THROUGH         
* X'03' EFFECTIVE DATE ELEMENTS AND BUILD BUY SEGMENTS BASE ON SINGLE           
* WEEK-OF DATE.                                                                 
***********************************************************************         
BLDSEGS  NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIO                                                           
BUYD     USING RBUYREC,R4                                                       
*                                                                               
* CALCULATE TOTAL SPOTS AND DOLLARS FOR BOTH REP AND AGENCY                     
*                                                                               
         CLC   =X'0B01',BUYD.RBUYKEY                                            
         BE    BLDSEG03                                                         
         ZICM  RE,RPSPTTOT,2                                                    
         ZICM  RF,BUYD.RBUYTSPT,2                                               
         AR    RE,RF                                                            
         STCM  RE,3,RPSPTTOT                                                    
*                                                                               
         ZICM  RE,RPORDTOT,4                                                    
         ZICM  RF,BUYD.RBUYTCOS,4                                               
         AR    RE,RF                                                            
         STCM  RE,15,RPORDTOT                                                   
         B     BLDSEG04                                                         
*                                                                               
BLDSEG03 DS    0H                                                               
         ZICM  RE,AGSPTTOT,2                                                    
         ZICM  RF,BUYD.RBUYTSPT,2                                               
         AR    RE,RF                                                            
         STCM  RE,3,AGSPTTOT                                                    
*                                                                               
         ZICM  RE,AGORDTOT,4                                                    
         ZICM  RF,BUYD.RBUYTCOS,4                                               
         AR    RE,RF                                                            
         STCM  RE,15,AGORDTOT                                                   
*                                                                               
BLDSEG04 DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'03'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RBUYDTEL,R6                                                      
*                                                                               
         XC    TSARREC,TSARREC                                                  
         XC    TSARREC2,TSARREC2                                                
SEG      USING TLSTD,TSARREC2                                                   
         MVC   SEG.TLLEN,=Y(TLBYLQ)                                             
         MVI   SEG.TLKTYP,X'0B'    TYPE BUY SEGMENT                             
*                                                                               
* FIND DAY(S) ATTRIBUTE RECORD INDEX                                            
*                                                                               
         GOTOX (X'01',=A(BLDDTP)),RR=RELO                                       
         GOTO1 GOTSAR,TSARDH                                                    
         BE    *+6                                                              
         DC    H'0'                MUST BE FOUND                                
*                                                                               
         MVC   SEG.TLBYDAY,TB.TSRNUM RECORD NUMBER IS THE INDEX                 
*                                                                               
* FIND TIME(S) ATTRIBUTE RECORD INDEX                                           
*                                                                               
         XC    TSARREC,TSARREC                                                  
         GOTOX (X'02',=A(BLDDTP)),RR=RELO                                       
         GOTO1 GOTSAR,TSARDH                                                    
         BE    *+6                                                              
         DC    H'0'                MUST BE FOUND                                
*                                                                               
         MVC   SEG.TLBYTIME,TB.TSRNUM  RECORD NUMBER IS THE INDEX               
*                                                                               
* FIND PROGRAM NAME ATTRIBUTE RECORD INDEX                                      
*                                                                               
         XC    TSARREC,TSARREC                                                  
         GOTOX (X'03',=A(BLDDTP)),RR=RELO                                       
         GOTO1 GOTSAR,TSARDH                                                    
         BNE   *+10                PROGRAM NAME IS OPTIONAL FOR NOW             
         MVC   SEG.TLBYPROG,TB.TSRNUM  RECORD NUMBER IS THE INDEX               
*                                                                               
         MVC   SEG.TLBYLEN,BUYD.RBUYDUR                                         
         MVC   SEG.TLBYRATE,BUYD.RBUYCOS                                        
*                                                                               
BLDSEG05 DS    0H                                                               
         GOTO1 DATCON,DMCB,(3,RBUYDTST),(0,WORK)                                
*                                                                               
BLDSEG10 DS    0H                                                               
         GOTO1 DATCON,DMCB,(0,WORK),(2,SEG.TLBYWKOF)                            
*                                                                               
* CLEAR FLAGS AND POINTER TO LIST OF BUY LINKS                                  
*                                                                               
         MVI   SEG.TLBYRFLG,0                                                   
         MVI   SEG.TLBYAFLG,0                                                   
         XC    SEG.TLBYRLNK,SEG.TLBYRLNK                                        
         XC    SEG.TLBYALNK,SEG.TLBYALNK                                        
*                                                                               
         CLC   =X'0B01',BUYD.RBUYKEY                                            
         BE    BLDSEG20                                                         
*                                                                               
* RECORD IS A REPPAK BUY                                                        
*                                                                               
         TM    BUYD.RBUYDUR,X'80'       MINUTES?                                
         BZ    *+8                                                              
         OI    SEG.TLBYRFLG,X'80'  SET LENGTH IN MINUTES                        
         B     BLDSEG30                                                         
*                                                                               
* RECORD IS AN AGENCY CONVERTED BUY                                             
*                                                                               
BLDSEG20 DS    0H                                                               
         TM    BUYD.RBUYDUR,X'80'       MINUTES?                                
         BZ    *+8                                                              
         OI    SEG.TLBYAFLG,X'80'  SET LENGTH IN MINUTES                        
*                                                                               
BLDSEG30 DS    0H                                                               
         MVC   TSARREC,TSARREC2                                                 
         GOTO1 GOTSAR,TSAADD                                                    
         TM    TB.TSERRS,TSEDUP                                                 
         BO    BLDSEG50                                                         
*                                                                               
* NEW BUY SEGMENT RECORD, NEED TO SPECIFY BUY NUMBER WHERE THIS SEGMENT         
* CAME FROM                                                                     
*                                                                               
         CLC   =X'0B01',BUYD.RBUYKEY                                            
         BE    BLDSEG40                                                         
         MVC   SEG.TLBYRLNK,BUYD.RBUYKLIN                                       
         MVC   SEG.TLBYRLNK+1(1),RBUYDTNW                                       
         B     BLDSEG45                                                         
*                                                                               
BLDSEG40 DS    0H                                                               
         MVC   SEG.TLBYALNK,BUYD.RBUYAGBL                                       
         MVC   SEG.TLBYALNK+1(1),RBUYDTNW                                       
*                                                                               
BLDSEG45 DS    0H                                                               
         MVC   TSARREC,TSARREC2                                                 
         GOTO1 GOTSAR,TSAWRT                                                    
         TM    TB.TSERRS,TSERNF                                                 
         BZ    BLDSEG80                                                         
         DC    H'0'                                                             
*                                                                               
BLDSEG50 DS    0H                                                               
         GOTO1 GOTSAR,TSAGET       RECORD EXISTS, GET IT                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* BUY SEGMENT RECORD EXISTS. SPECIFY REP OR AGENCY BUY#/#SPT IF NOT             
* ALREADY. OTHERWISE, WE WILL NEED TO ADD OR CREATE BUY LINK RECORD             
* SINCE TWO OR MORE BUYS REFER TO THIS SEGMENT                                  
*                                                                               
         MVC   TSARREC2,TSARREC                                                 
         CLC   =X'0B01',BUYD.RBUYKEY  REP OR AGENCY SEGMENT?                    
         BE    BLDSEG60                                                         
         OC    SEG.TLBYRLNK,SEG.TLBYRLNK                                        
         BNZ   BLDSEG70                                                         
         MVC   SEG.TLBYRLNK,BUYD.RBUYKLIN                                       
         MVC   SEG.TLBYRLNK+1(1),RBUYDTNW                                       
         B     BLDSEG65                                                         
*                                                                               
BLDSEG60 DS    0H                                                               
         OC    SEG.TLBYALNK,SEG.TLBYALNK                                        
         BNZ   BLDSEG70                                                         
         MVC   SEG.TLBYALNK,BUYD.RBUYAGBL                                       
         MVC   SEG.TLBYALNK+1(1),RBUYDTNW                                       
*                                                                               
BLDSEG65 DS    0H                                                               
         MVC   TSARREC,TSARREC2                                                 
         GOTO1 GOTSAR,TSAWRT                                                    
         TM    TB.TSERRS,TSERNF                                                 
         BZ    BLDSEG80                                                         
         DC    H'0'                                                             
*                                                                               
BLDSEG70 DS    0H                                                               
         GOTO1 =A(BLDBLINK),RR=RELO                                             
*                                                                               
* BUMP TO NEXT WEEK                                                             
*                                                                               
BLDSEG80 DS    0H                                                               
         LA    RF,7                                                             
         TM    RBUYDTIN,X'40'      ALTERNATING WEEKS?                           
         BZ    *+8                                                              
         LA    RF,14                                                            
         GOTO1 ADDAY,DMCB,WORK,WORK,(RF)                                        
*                                                                               
         GOTO1 DATCON,DMCB,(3,RBUYDTED),(0,WORK+6)                              
         CLC   WORK(6),WORK+6      HAVE WE PASSED THE END DATE?                 
         BNH   BLDSEG10                                                         
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BE    BLDSEG05                                                         
         DROP  R6                                                               
*                                                                               
BLDSEGX  DS    0H                                                               
*                                                                               
         B     EXIT                                                             
         DROP  SEG,BUYD                                                         
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CREATE NEW BUY LINK RECORD IF IT DOES NOT EXIST, ELSE APPEND EXISTING         
* BUY LINK RECORD WITH CURRENT BUY NUMBER AND NUMBER OF SPOTS                   
*                                                                               
* R6 IS POINTING TO CURRENT BUY EFFECTIVE ELEMENT                               
*                                                                               
***********************************************************************         
BLDBLINK NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   TSARREC2,TSARREC                                                 
*                                                                               
         L     R4,AIO                                                           
BUYD     USING RBUYREC,R4                                                       
         USING RBUYDTEL,R6                                                      
*                                                                               
         MVC   SVTSAR#,TB.TSRNUM   SAVE OFF BUY SEGMENT REC NUMBER              
*                                                                               
TKEY     USING TLKEY,TSARKEY                                                    
         XC    TSARKEY,TSARKEY                                                  
*                                                                               
         CLC   =X'0B00',BUYD.RBUYKEY                                            
         BE    BLDBL05                                                          
*                                                                               
         MVI   TKEY.TLKTYP,C'A'    AGENCY OR REP BUY?                           
         TM    TR.TLBYAFLG,X'40'   LINK OR ACTUAL BUY #?                        
         BZ    BLDBL50                                                          
         MVC   TKEY.TLBLINDX,TR.TLBYALNK                                        
         B     BLDBL10                                                          
*                                                                               
BLDBL05  DS    0H                                                               
         MVI   TKEY.TLKTYP,C'R'                                                 
         TM    TR.TLBYRFLG,X'40'   LINK OR ACTUAL BUY #?                        
         BZ    BLDBL50                                                          
         MVC   TKEY.TLBLINDX,TR.TLBYRLNK                                        
*                                                                               
BLDBL10  DS    0H                                                               
         OC    TKEY.TLBLINDX,TKEY.TLBLINDX                                      
         BZ    BLDBL50             NEED TO CREATE NEW BUY LINK RECORD           
*                                                                               
         XC    TSARREC,TSARREC                                                  
         MVC   TSARREC+2(L'TLKEY),TSARKEY                                       
         GOTO1 GOTSAR,TSARDH                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* INDEX POINTER MUST BE POINTING TO AN EXISTING BUY LINK RECORD                 
*                                                                               
         CLC   TR.TLKEY(TLBLTSPT-TLKEY),TKEY.TLKEY                              
         BE    *+6                                                              
         DC    H'0'                MUST BE IN TSAR BUFFER                       
*                                                                               
* BUY LINK RECORD FOUND, ADD THIS BUY TO THE LIST OF BUYS AND SPOTS             
*                                                                               
         LA    R3,TR.TLBLBUY#                                                   
*                                                                               
BLDBL20  DS    0H                                                               
         CLI   0(R3),0                                                          
         BE    BLDBL100                                                         
         AHI   R3,L'TLBLBUY#+L'TLBL#SPT                                         
*                                                                               
         LA    RE,TSARREC+L'TSARREC                                             
         CR    R3,RE               BOUNDARY CHECK                               
         BL    BLDBL20                                                          
         DC    H'0'                                                             
*                                                                               
* NEED TO CREATE NEW BUY LINK RECORD FOR THIS BUY SEGMENT                       
*                                                                               
BLDBL50  DS    0H                  FIND HIGHEST INDEX NUMBER                    
         XC    TBLINDEX,TBLINDEX                                                
         XC    TSARREC,TSARREC                                                  
         MVC   TSARREC+2(L'TLKEY),TSARKEY                                       
         GOTO1 GOTSAR,TSARDH                                                    
*                                                                               
BLDBL55  DS    0H                                                               
         CLC   TR.TLKTYP,TKEY.TLKTYP                                            
         BNE   BLDBL60                                                          
         MVC   TBLINDEX,TR.TLBLINDX                                             
         XC    TSARREC,TSARREC                                                  
         GOTO1 GOTSAR,TSANXT                                                    
         TM    TB.TSERRS,TSEEOF                                                 
         BZ    BLDBL55                                                          
*                                                                               
BLDBL60  DS    0H                                                               
         SR    R1,R1                                                            
         ICM   R1,3,TBLINDEX                                                    
         AHI   R1,1                                                             
         STCM  R1,3,TBLINDEX                                                    
*                                                                               
TR2      USING TLSTD,TSARREC2                                                   
*                                                                               
         XC    TSARREC,TSARREC                                                  
         MVC   TR.TLKTYP,TKEY.TLKTYP                                            
         MVC   TR.TLBLINDX,TBLINDEX                                             
*                                                                               
* SAVE OFF PREVIOUSLY SAVED LINE NUMBER AND NUMBER OF SPOTS                     
*                                                                               
         CLI   TR.TLKTYP,C'A'                                                   
         BE    BLDBL70                                                          
         MVC   TR.TLBLBUY#(2),TR2.TLBYRLNK                                      
         B     BLDBL80                                                          
*                                                                               
BLDBL70  DS    0H                                                               
         MVC   TR.TLBLBUY#(2),TR2.TLBYALNK                                      
*                                                                               
BLDBL80  DS    0H                                                               
         MVC   TR.TLBLTSPT,TR.TLBL#SPT                                          
*                                                                               
* APPEND CURRENT LINE NUMBER AND NUMBER OF SPOTS                                
*                                                                               
         LA    R3,TR.TLBLLIST                                                   
*                                                                               
BLDBL100 DS    0H                                                               
BL       USING TLBLBUY#,R3         REP OR AGENCY BUY NUMBER                     
         MVC   BL.TLBLBUY#,BUYD.RBUYKLIN                                        
         CLI   TR.TLKTYP,C'R'                                                   
         BE    *+10                                                             
         MVC   BL.TLBLBUY#,BUYD.RBUYAGBL                                        
*                                  NUMBER OF SPOTS COVERED                      
         MVC   BL.TLBL#SPT,RBUYDTNW                                             
*                                                                               
         ZIC   RF,BL.TLBL#SPT      CALCULATE TOTAL SPOTS FOR THIS WEEK          
         ZIC   RE,TR.TLBLTSPT                                                   
         AR    RE,RF                                                            
         STC   RE,TR.TLBLTSPT                                                   
         DROP  BL                                                               
*                                                                               
* CALCULATE NEW VARIABLE LENGTH AND ADD/PUT RECORD TO TSAR                      
*                                                                               
         AHI   R3,L'TLBLBUY#+L'TLBL#SPT                                         
*                                                                               
         LA    RE,TR.TLREC                                                      
         SR    R3,RE                                                            
         STCM  R3,3,TR.TLLEN                                                    
*                                                                               
* ADD OR PUT NEW BUY LINK RECORD                                                
*                                                                               
         CLI   TR.TLBLLIST+L'TLBLBUY#+L'TLBL#SPT,0                              
         BE    BLDBL105                                                         
         GOTO1 GOTSAR,TSAWRT                                                    
         TM    TB.TSERRS,TSERNF                                                 
         BZ    BLDBLX                                                           
         DC    H'0'                                                             
*                                                                               
BLDBL105 DS    0H                                                               
         GOTO1 GOTSAR,TSAADD                                                    
         TM    TB.TSERRS,TSEDUP+TSEEOF                                          
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*        GOTO1 =A(DUMPTSAR),RR=RELO                                             
*                                                                               
*                                                                               
* UPDATE BUY SEGMENT RECORD WITH INDEX OF NEW BUY LINK RECORD                   
*                                                                               
         MVC   TXNUM,SVTSAR#                                                    
         GOTO1 GOTSAR,TSAGET                                                    
         TM    TB.TSERRS,TSERNF                                                 
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   =X'0B01',BUYD.RBUYKEY                                            
         BE    BLDBL110                                                         
         MVC   TR.TLBYRLNK,TBLINDEX                                             
         OI    TR.TLBYRFLG,X'40'   FIELD IS A LINK POINTER                      
         B     BLDBL120                                                         
*                                                                               
BLDBL110 DS    0H                                                               
         MVC   TR.TLBYALNK,TBLINDEX                                             
         OI    TR.TLBYAFLG,X'40'   FIELD IS A LINK POINTER                      
*                                                                               
BLDBL120 DS    0H                                                               
         GOTO1 GOTSAR,TSAPUT                                                    
         TM    TB.TSERRS,TSERNF                                                 
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
BLDBLX   DS    0H                                                               
*                                                                               
*        GOTO1 =A(DUMPTSAR),RR=RELO                                             
*                                                                               
         B     EXIT                                                             
         DROP  R6,BUYD                                                          
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DUMP CONTENTS OF TSAR BUFFER TO DISPLAY                                       
***********************************************************************         
DIFFLIST NTR1  BASE=*,LABEL=*                                                   
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         OC    DIFATOT,DIFATOT     TOTALS ALREADY DISPLAYED?                    
         BZ    DL02                                                             
*                                                                               
         TM    MISCFLG2,MF2DTOT                                                 
         BZ    DL03                                                             
         NI    MISCFLG2,X'FF'-MF2DTOT                                           
*                                                                               
DL02     DS    0H                                                               
         NI    FLAGS2,X'FF'-TOTALPRC                                            
         GOTO1 =A(DISTOTAL),RR=RELO                                             
*                                                                               
DL03     DS    0H                                                               
         TWAXC DIFLISTH,DIFENDH,PROT=Y                                          
*                                                                               
         OC    LISTLAST,LISTLAST                                                
         BZ    DL05                                                             
         MVC   TXNUM,LISTLAST                                                   
         GOTO1 GOTSAR,TSAGET                                                    
         BE    DL08                                                             
         DC    H'0'                                                             
*                                                                               
DL05     DS    0H                                                               
         BAS   RE,DIFFHEAD                                                      
         XC    SCRNUM,SCRNUM                                                    
         XC    TSARREC,TSARREC                                                  
         MVI   TR.TLKTYP,X'0B'                                                  
         GOTO1 GOTSAR,TSARDH                                                    
         BL    DLX                                                              
*                                                                               
DL08     DS    0H                                                               
         XC    SEGSTRDT,SEGSTRDT                                                
         XC    SEGENDDT,SEGENDDT                                                
         XC    LISTSTRT,LISTSTRT                                                
         XC    DYTXNUM,DYTXNUM                                                  
         XC    TMTXNUM,TMTXNUM                                                  
         LA    R2,DIFLIST                                                       
*                                                                               
DL09     DS    0H                                                               
         MVC   SEGSTRDT,TR.TLBYWKOF                                             
*                                                                               
DL10     DS    0H                                                               
         CLI   TR.TLKTYP,X'0B'                                                  
         BNE   DLX                                                              
*                                                                               
DL13     DS    0H                                                               
         CLC   DYTXNUM,TR.TLBYDAY  CHECK IF WE NEED TO BREAK ON                 
         BNE   DL15                DIFFERENT DAY/TIME                           
         CLC   TMTXNUM,TR.TLBYTIME                                              
         BE    DL20                                                             
*                                                                               
DL15     DS    0H                                                               
         MVC   DYTXNUM,TR.TLBYDAY                                               
         MVC   TMTXNUM,TR.TLBYTIME                                              
         LA    RF,DIFLIST          DON'T DO IT IN THE FIRST LINE                
         CR    R2,RF                                                            
         BE    DL20                                                             
*                                                                               
         LR    RF,R2                                                            
         SHI   RF,8                  BACK UP TO HEADER                          
         SHI   RF,DIFLIS2H-DIFLISTH  BACK UP ONE DISPLAY LINE                   
         CLC   8(L'DIFLIST,RF),SPACES NO BUY SEGMENTS DISPLAYED                 
         BE    DL20                DON'T DUPLICATE VISUAL BREAKS                
         OC    8(L'DIFLIST,RF),8(RF)                                            
         BE    DL20                                                             
*                                                                               
*        MVI   0(R2),C'-'                                                       
*        MVC   1(L'DIFLIST-1,R2),0(R2)                                          
*                                                                               
         SHI   R2,8                MAKE SURE WE DIDN'T PASS SCREEN END          
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
*                                                                               
         LA    RF,DIFENDH                                                       
         CR    R2,RF                                                            
         BH    DLX                                                              
         AHI   R2,8                                                             
*                                                                               
DL20     DS    0H                                                               
         OC    LISTSTRT,LISTSTRT                                                
         BNZ   *+10                                                             
         MVC   LISTSTRT,TB.TSRNUM  SAVE OFF LIST START                          
         MVC   LISTLAST,TB.TSRNUM  SAVE OFF LIST END                            
*                                                                               
         TM    FLAGS,FGDETLQ                                                    
         BO    DL500                                                            
*                                                                               
***********************************************************************         
* DISPLAY SUMMARY LISTING                                                       
***********************************************************************         
         USING DIFFSUM,R2                                                       
*                                                                               
* CHECK IF SEGMENT IS FROM A SINGLE BUY. IF IT IS, THE SEGMENT                  
* CONTAINS THE ACTUAL BUY NUMBER AND SPOT COUNT                                 
* IF NOT, WE NEED TO FIND THE LINK TO THE LIST OF BUYS AGAINST THIS             
* SEGMENT                                                                       
*                                                                               
         GOTO1 =A(GETTSPTS),RR=RELO                                             
*                                                                               
         CLC   TOTREPSP,TOTAGYSP   SKIP IF ALL SPOTS MATCHED                    
         BNE   DL30                                                             
*                                                                               
* SKIP MATCHED SPOTS FOR SUMMARY LISTING                                        
*                                                                               
         XC    0(L'DIFLIST,R2),0(R2)                                            
         GOTO1 GOTSAR,TSANXT                                                    
         BL    DL25                                                             
         CLI   TR.TLKTYP,X'0B'                                                  
         BE    DL09                                                             
*                                                                               
DL25     DS    0H                                                               
         XC    LISTLAST,LISTLAST   ALL DONE                                     
         B     DLX                                                              
*                                                                               
DL30     DS    0H                                                               
         LA    RF,DIFLIST                                                       
         CR    R2,RF                                                            
         BNE   DL35                                                             
         GOTO1 =A(SCRLSAVE),RR=RELO  SAVE TXNUM TO TABLE FOR SCRL BACK          
*                                                                               
DL35     GOTO1 =A(GETNDX),RR=RELO  RETREIVE INDEXED ITEMS                       
*                                                                               
         EDIT  TR.TLBYLEN,DSLEN                                                 
         EDIT  TR.TLBYRATE,DSRATE,2,COMMAS=YES,ZERO=NOBLANK                     
*                                                                               
DL45     DS    0H                                                               
         MVC   DLSVREC,TSARREC     SAVE OFF CURRENT BUY SEGMENT                 
         MVC   PRVREPSP,TOTREPSP   FOR LATER COMPARISON                         
         MVC   PRVAGYSP,TOTAGYSP                                                
*                                                                               
* CHECK IF NEXT BUY SEGMENT IS A CONTINUATION FROM THIS BUY SEGMENT             
*                                                                               
DL70     DS    0H                                                               
         GOTO1 GOTSAR,TSANXT                                                    
         BL    DL80                                                             
         CLI   TR.TLKTYP,X'0B'                                                  
         BE    DL90                                                             
DL80     XC    LISTLAST,LISTLAST                                                
         B     DL300                                                            
*                                                                               
DL90     DS    0H                                                               
PREV     USING TLSTD,DLSVREC                                                    
         CLC   PREV.TLKEY(TLBYWKOF-TLKEY),TR.TLKEY                              
         BNE   DL300                                                            
*                                                                               
* NEXT BUY SEGMENT SHOULD HAVE WEEK-OF DATE EXACTLY ONE WEEK                    
* FROM THE WEEK-OF DATE OF CURRENT BUY SEGMENT                                  
*                                                                               
         GOTO1 DATCON,DMCB,(2,PREV.TLBYWKOF),(0,WORK)                           
         LA    RF,7                                                             
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(RF)                                      
         GOTO1 DATCON,DMCB,(2,TR.TLBYWKOF),(0,WORK)                             
         CLC   WORK(6),WORK+6                                                   
         BNE   DL300                                                            
*                                                                               
* CHECK IF SPOTS ALL SPOTS MATCHED ON BOTH REP AND AGENCY                       
* IF SO, SKIP BUY SEGMENT                                                       
*                                                                               
         GOTO1 =A(GETTSPTS),RR=RELO                                             
*                                                                               
         CLC   TOTREPSP,TOTAGYSP   SKIP IF ALL SPOTS MATCHED                    
         BE    DL70                                                             
*                                                                               
* CHECK IF SPOTS COVERED BY BOTH REP AND AGENCY ARE THE SAME AS                 
* THE PREVIOUS BUY SEGMENT                                                      
*                                                                               
DL92     DS    0H                                                               
         CLC   PRVREPSP,TOTREPSP                                                
         BNE   DL300                                                            
         CLC   PRVAGYSP,TOTAGYSP                                                
         BNE   DL300                                                            
*                                                                               
* CHECK IF THE BUY OR LIST OF BUYS ARE THE SAME AS THE PREVIOUS                 
* BUY SEGMENT                                                                   
*                                                                               
         TM    PREV.TLBYRFLG,X'40' ACTUAL BUY OR BUY LINK?                      
         BO    DL95                                                             
         TM    TR.TLBYRFLG,X'40'                                                
         BO    DL300                                                            
         CLC   PREV.TLBYRLNK(1),TR.TLBYRLNK                                     
         BNE   DL300                                                            
         B     DL100               CHECK AGENCY                                 
*                                                                               
DL95     DS    0H                  CHECK  BUY LINK RECORD                       
         TM    TR.TLBYRFLG,X'40'                                                
         BZ    DL300                                                            
***********************************************************************         
*                                                                               
* RETRIEVE BUY LINK RECORDS FOR BOTH THE CURRENT AND PREVIOUS SEGMENTS          
*                                                                               
***********************************************************************         
         MVC   DLSVTR#,TXNUM                                                    
*                                                                               
         XC    TSARKEY,TSARKEY                                                  
TKEY     USING TLKEY,TSARKEY                                                    
         MVI   TKEY.TLKTYP,C'R'                                                 
         MVC   TKEY.TLBLINDX,TR.TLBYRLNK                                        
         DROP  TKEY                                                             
*                                                                               
         MVC   TR.TLKEY,TSARKEY                                                 
         GOTO1 GOTSAR,TSARDH       RETRIEVE BUY LNK FOR CURRENT SEGMENT         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DLSVBLRC,TSARREC                                                 
*                                                                               
         XC    TSARKEY,TSARKEY                                                  
TKEY     USING TLKEY,TSARKEY                                                    
         MVI   TKEY.TLKTYP,C'R'                                                 
         MVC   TKEY.TLBLINDX,PREV.TLBYRLNK                                      
         DROP  TKEY                                                             
*                                                                               
         MVC   TR.TLKEY,TSARKEY                                                 
         GOTO1 GOTSAR,TSARDH       RETRIEVE BUY LNK FOR PREVIOUS SEGMNT         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   TR.TLLEN,DLSVBLRC   SAME LENGTH?                                 
         BNE   DL300                                                            
*                                                                               
         LA    R0,TSARREC          CHECK IF LIST OF BUYS ARE THE SAME           
         AHI   R0,TLBLTSPT-TLLEN                                                
         ZICM  R1,TR.TLLEN,2                                                    
         SHI   R1,TLBLTSPT-TLLEN                                                
*                                                                               
         LA    RE,DLSVBLRC                                                      
         AHI   RE,TLBLTSPT-TLLEN                                                
         LR    RF,R1                                                            
*                                                                               
         CLCL  R0,RE                                                            
         BE    DL98                                                             
*                                                                               
         MVC   TXNUM,DLSVTR#                                                    
         GOTO1 GOTSAR,TSAGET       RESTORE                                      
         BE    DL300                                                            
         DC    H'0'                                                             
*                                                                               
DL98     DS    0H                                                               
         MVC   TXNUM,DLSVTR#                                                    
         GOTO1 GOTSAR,TSAGET       RESTORE                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SEGENDDT,TR.TLBYWKOF                                             
         B     DL45                                                             
*                                                                               
* CHECK AGENCY BUYS                                                             
*                                                                               
DL100    DS    0H                                                               
*                                                                               
         TM    PREV.TLBYAFLG,X'40' ACTUAL BUY OR BUY LINK?                      
         BO    DL110                                                            
         TM    TR.TLBYAFLG,X'40'                                                
         BO    DL300                                                            
         CLC   PREV.TLBYALNK(1),TR.TLBYALNK                                     
         BNE   DL300                                                            
         MVC   SEGENDDT,TR.TLBYWKOF                                             
         B     DL45                                                             
*                                                                               
DL110    DS    0H                  CHECK  BUY LINK RECORD                       
         TM    TR.TLBYRFLG,X'40'                                                
         BZ    DL300                                                            
***********************************************************************         
*                                                                               
* RETRIEVE BUY LINK RECORDS FOR BOTH THE CURRENT AND PREVIOUS SEGMENTS          
*                                                                               
***********************************************************************         
         MVC   DLSVTR#,TXNUM                                                    
*                                                                               
         XC    TSARKEY,TSARKEY                                                  
TKEY     USING TLKEY,TSARKEY                                                    
         MVI   TKEY.TLKTYP,C'A'                                                 
         MVC   TKEY.TLBLINDX,TR.TLBYALNK                                        
         DROP  TKEY                                                             
*                                                                               
         MVC   TR.TLKEY,TSARKEY                                                 
         GOTO1 GOTSAR,TSARDH       RETRIEVE BUY LNK FOR CURRENT SEGMENT         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DLSVBLRC,TSARREC                                                 
*                                                                               
         XC    TSARKEY,TSARKEY                                                  
TKEY     USING TLKEY,TSARKEY                                                    
         MVI   TKEY.TLKTYP,C'A'                                                 
         MVC   TKEY.TLBLINDX,PREV.TLBYALNK                                      
         DROP  TKEY                                                             
*                                                                               
         MVC   TR.TLKEY,TSARKEY                                                 
         GOTO1 GOTSAR,TSARDH       RETRIEVE BUY LNK FOR PREVIOUS SEGMNT         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   TR.TLLEN,DLSVBLRC   SAME LENGTH?                                 
         BNE   DL300                                                            
*                                                                               
         LA    R0,TSARREC          CHECK IF LIST OF BUYS ARE THE SAME           
         AHI   R0,TLBLTSPT-TLLEN                                                
         ZICM  R1,TR.TLLEN,2                                                    
         SHI   R1,TLBLTSPT-TLLEN                                                
*                                                                               
         LA    RE,DLSVBLRC                                                      
         AHI   RE,TLBLTSPT-TLLEN                                                
         LR    RF,R1                                                            
         CLCL  R0,RE                                                            
         BE    DL120                                                            
*                                                                               
         MVC   TXNUM,DLSVTR#                                                    
         GOTO1 GOTSAR,TSAGET       RESTORE                                      
         BE    DL300                                                            
         DC    H'0'                                                             
*                                                                               
DL120    DS    0H                                                               
         MVC   TXNUM,DLSVTR#                                                    
         GOTO1 GOTSAR,TSAGET       RESTORE                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SEGENDDT,TR.TLBYWKOF                                             
         B     DL45                                                             
*                                                                               
* NEXT BUY SEGMENT IS DIFFERENT. DISPLAY DATES, SPOT DIFFERENCE                 
* AND ASSOCIATED REP BUYS FOR THIS SEGMENT                                      
*                                                                               
DL300    DS    0H                                                               
*        OI    MISCFLG1,MF1DIFF                                                 
         USING DIFFSUM,R2                                                       
         GOTO1 DATCON,DMCB,(2,SEGSTRDT),(3,FULL)                                
         LA    R3,DSDATES                                                       
         EDIT  (1,FULL+1),(2,(R3)),ALIGN=LEFT                                   
         AR    R3,R0                                                            
         MVI   0(R3),C'/'                                                       
         AHI   R3,1                                                             
         EDIT  (1,FULL+2),(2,(R3)),ALIGN=LEFT                                   
         AR    R3,R0                                                            
         OC    SEGENDDT,SEGENDDT                                                
         BZ    DL330                                                            
         MVI   0(R3),C'-'                                                       
         AHI   R3,1                                                             
*                                                                               
         GOTO1 DATCON,DMCB,(2,SEGENDDT),(0,WORK)                                
         ZIC   RF,STENDAY                                                       
         SLL   RF,28               SHIFT OFF START DAY TO GET CORRECT           
         SRL   RF,28               END DAY ADDED TO WEEK START                  
*                                                                               
         ZIC   RE,STENDAY          NO NEED TO BUMP IF SAME DAY                  
         SRL   RE,4                                                             
         CR    RE,RF                                                            
         BNE   DL310                                                            
         MVC   WORK+6(6),WORK                                                   
         B     DL320                                                            
*                                                                               
DL310    DS    0H                                                               
         SR    RF,RE                                                            
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(RF)                                      
*                                                                               
DL320    DS    0H                                                               
         GOTO1 DATCON,DMCB,(0,WORK+6),(3,FULL)                                  
         EDIT  (1,FULL+1),(2,(R3)),ALIGN=LEFT                                   
         AR    R3,R0                                                            
         MVI   0(R3),C'/'                                                       
         AHI   R3,1                                                             
         EDIT  (1,FULL+2),(2,(R3)),ALIGN=LEFT                                   
         XC    SEGENDDT,SEGENDDT                                                
*                                                                               
DL330    DS    0H                                                               
         ZIC   RE,PRVAGYSP                                                      
         ZIC   RF,PRVREPSP                                                      
         SR    RE,RF                                                            
         BP    DL340                                                            
         EDIT  (RE),DS#SPT,FLOAT=-                                              
         B     DL350                                                            
*                                                                               
DL340    DS    0H                                                               
         EDIT  (RE),DS#SPT,FLOAT=+                                              
         DROP  R2                                                               
*                                                                               
DL350    DS    0H                                                               
         MVC   PRVREPSP,TOTREPSP   RESET FOR NEXT SEGMENT COMPARISON            
         MVC   PRVAGYSP,TOTAGYSP                                                
*                                                                               
         OC    LISTLAST,LISTLAST   NULL IF ALL DONE DISPLAYING                  
         BZ    DLX                                                              
*                                                                               
         OC    NXOFFSET,NXOFFSET   EXPLODED VIEW USED                           
         BZ    DL360               BUMP TO CORRECT LINE FOR NEXT                
         A     R2,NXOFFSET         SEGMENT                                      
*                                                                               
DL360    DS    0H                                                               
         SHI   R2,8                                                             
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
*                                                                               
         LA    RF,DIFENDH                                                       
         CR    R2,RF                                                            
         BH    DLX                                                              
         AHI   R2,8                                                             
         B     DL09                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY DETAIL LISTING                                                        
***********************************************************************         
DL500    DS    0H                                                               
         USING DIFFDET,R2                                                       
*                                                                               
         EDIT  TR.TLBYLEN,DDLEN                                                 
*                                                                               
         EDIT  TR.TLBYRATE,DDRATE,2,COMMAS=YES,ZERO=NOBLANK                     
*                                                                               
* TEST   EDIT  TXNUM,DDLEN                                                      
*                                                                               
         GOTO1 DATCON,DMCB,(2,TR.TLBYWKOF),(3,FULL)                             
         LA    R3,DDDATE                                                        
         EDIT  (1,FULL+1),(2,(R3)),ALIGN=LEFT                                   
         AR    R3,R0                                                            
         MVI   0(R3),C'/'                                                       
         AHI   R3,1                                                             
         EDIT  (1,FULL+2),(2,(R3)),ALIGN=LEFT                                   
*                                                                               
         TM    TR.TLBYRFLG,X'40'   POINTER OR ACTUALLY BUY/SPT?                 
         BO    DL510                                                            
*                                                                               
         EDIT  (1,TR.TLBYRLNK),(3,DDRBUY#),ALIGN=LEFT                           
         CLI   TR.TLBYRLNK,0       NO BUY LINKED?                               
         BE    DL510                                                            
         EDIT  (1,TR.TLBYRLNK+1),(3,DDR#SPT),ALIGN=LEFT,ZERO=NOBLANK            
*                                                                               
DL510    DS    0H                                                               
         TM    TR.TLBYAFLG,X'40'   POINTER OR ACTUALLY BUY/SPT?                 
         BO    DL520                                                            
*                                                                               
         EDIT  (1,TR.TLBYALNK),(3,DDABUY#),ALIGN=LEFT                           
         CLI   TR.TLBYALNK,0       NO BUY LINKED?                               
         BE    DL520                                                            
         EDIT  (1,TR.TLBYALNK+1),(3,DDA#SPT),ALIGN=LEFT,ZERO=NOBLANK            
*                                                                               
DL520    DS    0H                                                               
         GOTO1 =A(GETNDX),RR=RELO  RETREIVE INDEXED ITEMS                       
*                                                                               
DL530    DS    0H                                                               
         CLC   DDR#SPT,DDA#SPT                                                  
         BE    DL560                                                            
*                                                                               
* CHECK IF BOTH REP AND AGENCY HAVE NO SPOTS FOR A PARTICULAR SEGMENT           
*                                                                               
         CLI   DDA#SPT,C'0'                                                     
         BE    DL540                                                            
         OC    DDA#SPT,DDA#SPT                                                  
         BNZ   DL550                                                            
*                                                                               
DL540    DS    0H                                                               
         CLI   DDR#SPT,C'0'                                                     
         BE    DL560                                                            
         OC    DDR#SPT,DDR#SPT                                                  
         BZ    DL560                                                            
*                                                                               
DL550    DS    0H                                                               
         MVI   DDIND,C'*'          INDICATE IF REP/AGY SPOTS DIFFER             
*                                                                               
DL560    DS    0H                                                               
         TM    FLAGS2,FGALLQ                                                    
         BO    DL565                                                            
         CLI   DDIND,C'*'          USERS ONLY WANT TO SEE BUYLINES              
         BE    DL565               THAT REP NEED TO TAKE ACTIONS ON             
*                                                                               
         XC    0(77,R2),0(R2)      CLEAR LINE                                   
         SHI   R2,8                                                             
         OI    6(R2),X'80'         RETRANSMIT LINE                              
         AHI   R2,8                                                             
*                                                                               
         GOTO1 GOTSAR,TSANXT                                                    
         BL    DL562                                                            
         CLI   TR.TLKTYP,X'0B'                                                  
         BE    DL10                                                             
DL562    XC    LISTLAST,LISTLAST                                                
         B     DLXDETL                                                          
*                                                                               
DL565    DS    0H                                                               
         LA    RF,DIFLIST                                                       
         CR    R2,RF                                                            
         BNE   DL565A                                                           
         GOTO1 =A(SCRLSAVE),RR=RELO  SAVE TXNUM TO TABLE FOR SCL BACK           
*                                                                               
DL565A   GOTO1 GOTSAR,TSANXT                                                    
         BL    DL570                                                            
         CLI   TR.TLKTYP,X'0B'                                                  
         BE    DL580                                                            
DL570    XC    LISTLAST,LISTLAST                                                
         B     DLXDETL                                                          
*                                                                               
DL580    DS    0H                                                               
         OC    NXOFFSET,NXOFFSET   EXPLODED VIEW USED                           
         BZ    DL590               BUMP TO CORRECT LINE FOR NEXT                
         A     R2,NXOFFSET         SEGMENT                                      
*                                                                               
DL590    DS    0H                                                               
         SHI   R2,8                ELSE JUST BUMP TO NEXT DISPLAY               
         ZIC   R1,0(R2)            LINE                                         
         AR    R2,R1                                                            
*                                                                               
         LA    RF,DIFENDH                                                       
         CR    R2,RF                                                            
         BH    DLX                                                              
         AHI   R2,8                                                             
         B     DL10                                                             
*                                                                               
DLXDETL  DS    0H                                                               
         TM    FLAGS2,FGALLQ       PREVENT LAST LINE FROM SHOWING UP            
         BO    EXIT                                                             
         CLI   DDIND,C'*'          USERS ONLY WANT TO SEE BUYLINES              
         BE    EXIT                THAT REP NEED TO TAKE ACTIONS ON             
         SHI   R2,8                CLEAR SCREEN AFTER                           
         TWAXC (R2),DIFENDH,PROT=Y                                              
DLX      DS    0H                                                               
         B     EXIT                                                             
         DROP  R8                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY HEADER INFO FOR DIFFERENCE SUMMARY/DETAIL LISTING                     
***********************************************************************         
DIFFHEAD NTR1                                                                   
*                                                                               
         MVC   DIFHDLN,ECONNUM     FILL IN CONTRACT/ORDER #S                    
         GOTO1 HEXOUT,DMCB,CDARNUM,DIFAORD,4,=C'TOG'                            
*                                                                               
         XC    DIFHLN1,DIFHLN1                                                  
         XC    DIFHLN2,DIFHLN2                                                  
*                                                                               
         TM    FLAGS,FGDETLQ                                                    
         BO    DH10                                                             
*                                  SUMMARY HEADER                               
         MVC   DIFHLN1+3(5),=C'Sp/Wk'                                           
         MVC   DIFHLN1+8(3),=C'Rep'                                             
*                                                                               
         LA    R2,DIFHLN2                                                       
         USING DIFFSUM,R2                                                       
         MVC   DSDYTM(9),=C'Day/Times'                                          
         MVC   DSLEN(3),=C'Len'                                                 
         MVC   DSRATE+8(4),=C'Rate'                                             
         MVC   DSDATES(5),=C'Dates'                                             
         MVC   DS#SPT(4),=C'Diff'                                               
         MVC   DSRBUY#(5),=C'Buy#s'                                             
         MVC   DSPGM(7),=C'Program'                                             
*                                                                               
         MVC   DIFTYPE(9),=C'*Summary*'                                         
         B     DHX                                                              
*                                                                               
DH10     DS    0H                  DETAIL HEADER                                
         MVC   DIFHLN1(3),=C'Rep'                                               
         MVC   DIFHLN1+4(3),=C'Rep'                                             
         MVC   DIFHLN1+9(3),=C'Agy'                                             
         MVC   DIFHLN1+13(3),=C'Agy'                                            
*                                                                               
         LA    R2,DIFHLN2                                                       
         USING DIFFDET,R2                                                       
         MVI   DDIND,C'D'                                                       
         MVC   DDDYTM(9),=C'Day/Times'                                          
         MVC   DDLEN(3),=C'Len'                                                 
         MVC   DDRATE+8(4),=C'Rate'                                             
         MVC   DDDATE(5),=C'Wk/Of'                                              
         MVC   DDR#SPT(3),=C'Spt'                                               
         MVC   DDRBUY#(4),=C'Ln#s'                                              
         MVC   DDA#SPT(3),=C'Spt'                                               
         MVC   DDABUY#(4),=C'Ln#s'                                              
         MVC   DDPGM(7),=C'Program'                                             
*                                                                               
         MVC   DIFTYPE(9),=C'*Details*'                                         
*                                                                               
DHX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* RETRIEVE INDEXED ITEMS: DAY/TIME, PROGRAM NAME AND BUY LINKS, IF ANY          
* R2 ALREADY POINTS TO LIST LINE                                                
***********************************************************************         
GETNDX   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         XC    NXOFFSET,NXOFFSET                                                
*                                                                               
* BUILD STRING OF DAY/TIMES IN ELEM                                             
*                                                                               
         MVC   GNXSVREC,TSARREC    SAVE OFF CURRENT TSAR RECORD                 
         MVC   SVTSAR#,TXNUM       AND TSAR RECORD NUMBER                       
BUYSEG   USING TLSTD,GNXSVREC                                                   
*                                                                               
* DAY(S)                                                                        
*                                                                               
         XC    TSARREC,TSARREC                                                  
         MVC   TXNUM,BUYSEG.TLBYDAY RETRIEVE DAY(S)                             
         GOTO1 GOTSAR,TSAGET                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   GNXSVDRC,TSARREC    SAVE OFF DAYS RECORD                         
*                                                                               
TRDAYS   USING TLSTD,GNXSVDRC                                                   
*                                                                               
* TIME(S)                                                                       
*                                                                               
         XC    TSARREC,TSARREC                                                  
         MVC   TXNUM,BUYSEG.TLBYTIME RETRIEVE TIME(S)                           
         GOTO1 GOTSAR,TSAGET                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
*                                                                               
         LA    R3,TRDAYS.TLDYSTEN                                               
         LA    R4,TR.TLTMSTRT                                                   
*                                                                               
GN10     DS    0H                  IF SINGLE DAY, WE NEED                       
         TM    0(R3),X'F0'         TO PUT THE START DAY                         
         BNO   GN15                BACK IN THE FIRST NIBBLE                     
         ZIC   R1,0(R3)                                                         
         SLL   R1,4                                                             
         STC   R1,HALF                                                          
         MVN   HALF(1),0(R3)                                                    
         MVC   0(1,R3),HALF                                                     
*                                                                               
GN15     DS    0H                                                               
         GOTO1 VOUTDAY,DMCB,1(R3),0(R3),0(R6)                                   
         MVC   STENDAY,0(R3)                                                    
*                                                                               
GN20     DS    0H                                                               
         CLI   0(R6),0                                                          
         BE    GN30                                                             
         CLI   0(R6),C' '                                                       
         BE    GN30                                                             
         AHI   R6,1                                                             
         LA    RF,ELEM+L'ELEM                                                   
         CR    R6,RF                                                            
         BH    GN80                                                             
         B     GN20                                                             
*                                                                               
GN30     DS    0H                                                               
         MVI   0(R6),C'/'                                                       
*        OC    2(L'TLTMENDT,R4),2(R4)                                           
*        BNZ   *+10                IF SINGLE TIME                               
*        MVC   2(L'TLTMENDT,R4),0(R4)                                           
         GOTO1 UNTIME,DMCB,0(R4),1(R6)                                          
*                                                                               
GN40     DS    0H                                                               
         CLI   0(R6),0                                                          
         BE    GN50                                                             
         CLI   0(R6),C' '                                                       
         BE    GN50                                                             
         AHI   R6,1                                                             
         LA    RF,ELEM+L'ELEM                                                   
         CR    R6,RF                                                            
         BH    GN80                                                             
         B     GN40                                                             
*                                                                               
GN50     DS    0H                                                               
         AHI   R3,L'TRDAYS.TLDYSTEN+L'TRDAYS.TLDYDAYS                           
         AHI   R4,L'TR.TLTMSTRT+L'TR.TLTMENDT                                   
         CLI   0(R3),0                                                          
         BE    GN80                                                             
         MVI   0(R6),C','                                                       
         AHI   R6,1                                                             
         B     GN10                                                             
*                                                                               
GN80     DS    0H                                                               
         TM    FLAGS,FGDETLQ       SUMMARY OR DETAIL?                           
         BO    GN190                                                            
*                                                                               
         USING DIFFSUM,R2                                                       
         MVC   DSDYTM,ELEM                                                      
         CLI   ELEM+L'DSDYTM,0     DID EVERYTHING FIT?                          
         BE    GN290                                                            
         TM    FLAGS,FGEXPLDQ      NO, DID USER REQUEST EXPLODED VIEW?          
         BO    GN100                                                            
         MVI   DSDYTM+L'DSDYTM-1,C'+'                                           
         B     GN290                                                            
*                                                                               
* USER TOGGLED EXPLODED VIEW, EXPAND ANY HIDDEN DATA                            
*                                                                               
GN100    DS    0H                                                               
         XC    DSDYTM,DSDYTM                                                    
         LA    R1,8                AT MOST 8 SCREEN LINES                       
         LR    R3,R2                                                            
         LA    RE,ELEM                                                          
*                                                                               
GN110    DS    0H                                                               
         LA    RF,L'DSDYTM(RE)                                                  
         CLI   0(RF),0                                                          
         BE    GN170                                                            
*                                                                               
LINED    USING DIFFSUM,R3                                                       
         CLI   0(RF),C','                                                       
         BE    GN140                                                            
*                                                                               
GN120    DS    0H                                                               
         SHI   RF,1                                                             
         CLI   0(RF),C','                                                       
         BE    GN130                                                            
         CR    RF,RE                                                            
         BH    GN120                                                            
         B     GN170                                                            
*                                                                               
GN130    DS    0H                                                               
         SR    RF,RE                                                            
         EX    RF,*+8                                                           
         B     GN150                                                            
         MVC   LINED.DSDYTM(0),0(RE)                                            
*                                                                               
GN140    DS    0H                                                               
         MVC   LINED.DSDYTM(L'DSDYTM),0(RE)                                     
*                                                                               
GN150    DS    0H                                                               
         LA    RE,1(RE,RF)                                                      
*                                                                               
         LA    RF,DIFENDH          DID WE GO PASS END OF SCREEN?                
         CR    R3,RF               YES, CONTINUE ON NEXT PAGE                   
         BL    GN160                                                            
         MVI   LINED.DSDYTM+L'DSDYTM-1,C'+'                                     
         B     GN180                                                            
*                                                                               
GN160    DS    0H                                                               
         SHI   R3,8                BACK TRACK TO THE START OF HEADER            
         ZIC   R0,0(R3)            NEXT SCREEN LINE                             
         AR    R3,R0                                                            
         AHI   R3,8                BUMP FORWARD TO DISPLAY AREA                 
*                                                                               
         BCT   R1,GN110                                                         
         B     GN180                                                            
*                                                                               
GN170    DS    0H                                                               
         MVC   LINED.DSDYTM(L'DSDYTM),0(RE)                                     
*                                                                               
GN180    DS    0H                                                               
         SR    R3,R2                                                            
         C     R3,NXOFFSET                                                      
         BNH   GN290                                                            
         ST    R3,NXOFFSET         NEXT DISPLAY LINE FOR NEXT SEGMENT           
         B     GN290                                                            
*                                                                               
GN190    DS    0H                  DETAIL DISPLAY                               
         USING DIFFDET,R2                                                       
         MVC   DDDYTM,ELEM                                                      
         CLI   ELEM+L'DDDYTM,0     DID EVERYTHING FIT?                          
         BE    GN290               NO, SHOW THAT THERE'S MORE                   
         TM    FLAGS,FGEXPLDQ      NO, DID USER REQUEST EXPLODED VIEW?          
         BO    GN200                                                            
         MVI   DDDYTM+L'DDDYTM-1,C'+'                                           
         B     GN290                                                            
*                                                                               
* USER TOGGLED EXPLODED VIEW, EXPAND ANY HIDDEN DATA                            
*                                                                               
GN200    DS    0H                                                               
         XC    DDDYTM,DDDYTM                                                    
         LA    R1,8                AT MOST 8 SCREEN LINES                       
         LR    R3,R2                                                            
         LA    RE,ELEM                                                          
*                                                                               
GN210    DS    0H                                                               
         LA    RF,L'DDDYTM(RE)                                                  
         CLI   0(RF),0                                                          
         BE    GN270                                                            
*                                                                               
LINED    USING DIFFDET,R3                                                       
         CLI   0(RF),C','                                                       
         BE    GN240                                                            
*                                                                               
GN220    DS    0H                                                               
         SHI   RF,1                                                             
         CLI   0(RF),C','                                                       
         BE    GN230                                                            
         CR    RF,RE                                                            
         BH    GN220                                                            
         B     GN270                                                            
*                                                                               
GN230    DS    0H                                                               
         SR    RF,RE                                                            
         EX    RF,*+8                                                           
         B     GN250                                                            
         MVC   LINED.DDDYTM(0),0(RE)                                            
*                                                                               
GN240    DS    0H                                                               
         MVC   LINED.DDDYTM(L'DDDYTM),0(RE)                                     
*                                                                               
GN250    DS    0H                                                               
         LA    RE,1(RE,RF)                                                      
*                                                                               
         LA    RF,DIFENDH          DID WE GO PASS END OF SCREEN?                
         CR    R3,RF               YES, CONTINUE ON NEXT PAGE                   
         BL    GN260                                                            
         MVI   LINED.DDDYTM+L'DDDYTM-1,C'+'                                     
         B     GN280                                                            
*                                                                               
GN260    DS    0H                                                               
         SHI   R3,8                BACK TRACK TO THE START OF HEADER            
         ZIC   R0,0(R3)            NEXT SCREEN LINE                             
         AR    R3,R0                                                            
         AHI   R3,8                BUMP FORWARD TO DISPLAY AREA                 
*                                                                               
         BCT   R1,GN210                                                         
         B     GN280                                                            
*                                                                               
GN270    DS    0H                                                               
         MVC   LINED.DDDYTM(L'DDDYTM),0(RE)                                     
*                                                                               
GN280    DS    0H                                                               
         SR    R3,R2                                                            
         C     R3,NXOFFSET                                                      
         BNH   GN290                                                            
         ST    R3,NXOFFSET         NEXT DISPLAY LINE FOR NEXT SEGMENT           
*                                                                               
* PROGRAM NAME                                                                  
*                                                                               
GN290    DS    0H                  DETAIL DISPLAY                               
         OC    BUYSEG.TLBYPROG,BUYSEG.TLBYPROG                                  
         BZ    GN370                                                            
         MVC   TXNUM,BUYSEG.TLBYPROG RETRIEVE PROGRAM NAME                      
         XC    TSARREC,TSARREC                                                  
         GOTO1 GOTSAR,TSAGET                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    FLAGS,FGDETLQ       SUMMARY OR DETAIL?                           
         BO    GN330                                                            
*                                  SUMMARY DISPLAY                              
         USING DIFFSUM,R2                                                       
         MVC   DSPGM,TR.TLPGNAME                                                
         CLI   TR.TLPGNAME+L'DSPGM,0                                            
         BE    GN370               SHOW NAME DIDN'T FIT                         
         MVI   DSPGM+L'DSPGM-1,C'+'                                             
*                                                                               
         TM    FLAGS,FGEXPLDQ      USER REQUESTED EXPLODED VIEW?                
         BZ    GN370                                                            
*                                                                               
         LR    R3,R2                                                            
LINED    USING DIFFSUM,R3                                                       
         LA    R1,8                AT MOST 8 SCREEN LINES                       
         LA    RE,TR.TLPGNAME                                                   
*                                                                               
GN300    DS    0H                                                               
         MVC   LINED.DSPGM(L'DSPGM),0(RE)                                       
         CLI   L'DSPGM(RE),0                                                    
         BE    GN320                                                            
*                                                                               
         AHI   RE,L'DSPGM                                                       
*                                                                               
         LA    RF,DIFENDH          DID WE GO PASS END OF SCREEN?                
         CR    R3,RF               YES, CONTINUE ON NEXT PAGE                   
         BL    GN310                                                            
         MVI   LINED.DSPGM+L'DSPGM-1,C'+'                                       
         B     GN370                                                            
*                                                                               
GN310    DS    0H                                                               
         SHI   R3,8                BACK TRACK TO THE START OF HEADER            
         ZIC   R0,0(R3)            NEXT SCREEN LINE                             
         AR    R3,R0                                                            
         AHI   R3,8                BUMP FORWARD TO DISPLAY AREA                 
         BCT   R1,GN300                                                         
*                                                                               
GN320    DS    0H                                                               
         SR    R3,R2                                                            
         C     R3,NXOFFSET                                                      
         BNH   GN370                                                            
         ST    R3,NXOFFSET         NEXT DISPLAY LINE FOR NEXT SEGMENT           
         B     GN370                                                            
*                                                                               
GN330    DS    0H                  DETAIL DISPLAY                               
         USING DIFFDET,R2                                                       
         MVC   DDPGM,TR.TLPGNAME                                                
         CLI   TR.TLPGNAME+L'DDPGM,0                                            
         BE    GN370               SHOW NAME DIDN'T FIT                         
         MVI   DDPGM+L'DDPGM-1,C'+'                                             
*                                                                               
         TM    FLAGS,FGEXPLDQ      USER REQUESTED EXPLODED VIEW?                
         BZ    GN370                                                            
*                                                                               
         LR    R3,R2                                                            
LINED    USING DIFFDET,R3                                                       
         LA    R1,8                AT MOST 8 SCREEN LINES                       
         LA    RE,TR.TLPGNAME                                                   
*                                                                               
GN340    DS    0H                                                               
         MVC   LINED.DDPGM(L'DDPGM),0(RE)                                       
         CLI   L'DDPGM(RE),0                                                    
         BE    GN360                                                            
*                                                                               
         AHI   RE,L'DDPGM                                                       
*                                                                               
         LA    RF,DIFENDH          DID WE GO PASS END OF SCREEN?                
         CR    R3,RF               YES, CONTINUE ON NEXT PAGE                   
         BL    GN350                                                            
         MVI   LINED.DDPGM+L'DDPGM-1,C'+'                                       
         B     GN370                                                            
*                                                                               
GN350    DS    0H                                                               
         SHI   R3,8                BACK TRACK TO THE START OF HEADER            
         ZIC   R0,0(R3)            NEXT SCREEN LINE                             
         AR    R3,R0                                                            
         AHI   R3,8                BUMP FORWARD TO DISPLAY AREA                 
         BCT   R1,GN340                                                         
*                                                                               
GN360    DS    0H                                                               
         SR    R3,R2                                                            
         C     R3,NXOFFSET                                                      
         BNH   GN370                                                            
         ST    R3,NXOFFSET         NEXT DISPLAY LINE FOR NEXT SEGMENT           
*                                                                               
* REP/AGY BUY LINKS IF MORE THAN ONE BUY WERE LINKED TO THIS SEGMENT            
*                                                                               
GN370    DS    0H                                                               
         TM    BUYSEG.TLBYRFLG,X'40' REP BUY LINK?                              
         BO    GN380                                                            
         TM    FLAGS,FGDETLQ       SUMMARY OR DETAIL?                           
         BO    GN540                                                            
***********************************************************************         
* DISPLAY FOR SUMMARY SINGLE REP BUY                                            
***********************************************************************         
         USING DIFFSUM,R2                                                       
         EDIT  (1,BUYSEG.TLBYRLNK),DSRBUY#,ALIGN=LEFT                           
         CLI   BUYSEG.TLBYRLNK,0                                                
         BNE   GN540                                                            
         MVC   DSRBUY#(3),=C'ADD'                                               
         B     GN540                                                            
         DROP  R2                                                               
***********************************************************************         
* DISPLAY FOR SUMMARY MULTIPLE REP BUY USING BUY LINK RECORD                    
***********************************************************************         
GN380    DS    0H                                                               
         XC    TSARREC,TSARREC                                                  
         MVI   TR.TLKTYP,C'R'                                                   
         MVC   TR.TLBLINDX,BUYSEG.TLBYRLNK                                      
         GOTO1 GOTSAR,TSARDH       RETRIEVE BUY LINK RECORD                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    FLAGS,FGDETLQ       SUMMARY OR DETAIL?                           
         BZ    GN440                                                            
*                                                                               
* DETAIL VIEW FOR # REP SPOTS AND REP BUY LINES                                 
*                                                                               
         USING DIFFDET,R2                                                       
         EDIT  TR.TLBLTSPT,DDR#SPT,ALIGN=LEFT,ZERO=NOBLANK                      
         EDIT  TR.TLBLBUY#,DDRBUY#,ALIGN=LEFT,TRAIL=C'+'                        
*                                                                               
         TM    FLAGS,FGEXPLDQ      DID USER REQUEST EXPLODED VIEW?              
         BZ    GN540                                                            
*                                                                               
         NI    DISPFLGS,X'FF'-DFCOMPQ                                           
         CLI   TR.TLBLBUY#+(2*8),0 MORE THAN 8 REP LINES LINKED?                
         BE    *+8                                                              
         OI    DISPFLGS,DFCOMPQ    TOO MANY LINES, COMPRESSED MODE              
*                                                                               
         LA    R3,TR.TLBLBUY#                                                   
         LR    R1,R2                                                            
LINED    USING DIFFDET,R1                                                       
         TM    DISPFLGS,DFCOMPQ                                                 
         BZ    GN390                                                            
         XC    LINED.DDRBUY#,LINED.DDRBUY#                                      
         MVI   LINED.DDRBUY#,C'+'                                               
         B     GN410                                                            
*                                                                               
* EXPLODE ALL LINKED REP BUYS                                                   
*                                                                               
GN390    DS    0H                                                               
         LA    R4,LINED.DDRBUY#                                                 
         TM    DISPFLGS,DFCOMPQ                                                 
         BZ    GN400                                                            
         LA    R4,LINED.DDDATE                                                  
         MVC   LINED.DDRATE(10),=C'REP BUY#S:'                                  
*                                                                               
GN400    DS    0H                                                               
         EDIT  (1,0(R3)),(3,0(R4)),ALIGN=LEFT                                   
         AHI   R3,L'TLBLBUY#+L'TLBL#SPT                                         
         CLI   0(R3),0                                                          
         BE    GN430                                                            
         TM    DISPFLGS,DFCOMPQ                                                 
         BZ    GN410                                                            
         AR    R4,R0                                                            
         MVI   0(R4),C','                                                       
         AHI   R4,1                                                             
         LA    RF,LINED.DDA#SPT-4                                               
         CR    R4,RF                                                            
         BL    GN400                                                            
*                                                                               
GN410    DS    0H                                                               
         LA    RF,DIFENDH          DID WE GO PASS END OF SCREEN?                
         CR    R1,RF               YES, CONTINUE ON NEXT PAGE                   
         BL    GN420                                                            
         MVI   LINED.DDRBUY#+L'DDRBUY#-1,C'+'                                   
         B     GN430                                                            
*                                                                               
GN420    DS    0H                                                               
         SHI   R1,8                BACK TRACK TO THE START OF HEADER            
         ZIC   R0,0(R1)            NEXT SCREEN LINE                             
         AR    R1,R0                                                            
         AHI   R1,8                BUMP FORWARD TO DISPLAY AREA                 
         B     GN390                                                            
*                                                                               
GN430    DS    0H                                                               
         SR    R1,R2                                                            
         C     R1,NXOFFSET                                                      
         BNH   GN540                                                            
         ST    R1,NXOFFSET         NEXT DISPLAY LINE FOR NEXT SEGMENT           
         B     GN540                                                            
*                                                                               
* SUMMARY VIEW                                                                  
*                                                                               
GN440    DS    0H                                                               
         TM    FLAGS,FGEXPLDQ      DID USER REQUEST EXPLODED VIEW?              
         BZ    GN500                                                            
*                                                                               
         NI    DISPFLGS,X'FF'-DFCOMPQ                                           
         CLI   TR.TLBLBUY#+(2*8),0 MORE THAN 8 REP LINES LINKED?                
         BE    *+8                                                              
         OI    DISPFLGS,DFCOMPQ    TOO MANY LINES, COMPRESSED MODE              
*                                                                               
         LA    R3,TR.TLBLBUY#                                                   
         LR    R1,R2                                                            
LINED    USING DIFFSUM,R1                                                       
         TM    DISPFLGS,DFCOMPQ                                                 
         BZ    GN450                                                            
         MVI   LINED.DSRBUY#,C'+'                                               
         B     GN470                                                            
*                                                                               
* EXPLODE ALL LINKED REP BUYS                                                   
*                                                                               
GN450    DS    0H                                                               
         LA    R4,LINED.DSRBUY#                                                 
         TM    DISPFLGS,DFCOMPQ                                                 
         BZ    GN460                                                            
         LA    R4,LINED.DSDATES                                                 
         MVC   LINED.DSRATE(10),=C'REP BUY#S:'                                  
*                                                                               
GN460    DS    0H                                                               
         EDIT  (1,0(R3)),(3,0(R4)),ALIGN=LEFT                                   
         AHI   R3,L'TLBLBUY#+L'TLBL#SPT                                         
         CLI   0(R3),0                                                          
         BE    GN490                                                            
         TM    DISPFLGS,DFCOMPQ                                                 
         BZ    GN470                                                            
         AR    R4,R0                                                            
         MVI   0(R4),C','                                                       
         AHI   R4,1                                                             
         LA    RF,LINED.DSPGM-4                                                 
         CR    R4,RF                                                            
         BL    GN460                                                            
*                                                                               
GN470    DS    0H                                                               
         LA    RF,DIFENDH          DID WE GO PASS END OF SCREEN?                
         CR    R1,RF               YES, CONTINUE ON NEXT PAGE                   
         BL    GN480                                                            
         MVI   LINED.DSRBUY#+L'DSRBUY#-1,C'+'                                   
         B     GN490                                                            
*                                                                               
GN480    DS    0H                                                               
         SHI   R1,8                BACK TRACK TO THE START OF HEADER            
         ZIC   R0,0(R1)            NEXT SCREEN LINE                             
         AR    R1,R0                                                            
         AHI   R1,8                BUMP FORWARD TO DISPLAY AREA                 
         B     GN450                                                            
*                                                                               
GN490    DS    0H                                                               
         SR    R1,R2                                                            
         C     R1,NXOFFSET                                                      
         BNH   GN540                                                            
         ST    R1,NXOFFSET         NEXT DISPLAY LINE FOR NEXT SEGMENT           
         B     GN540                                                            
*                                                                               
* COLLAPSED VIEW                                                                
*                                                                               
GN500    DS    0H                  TOO MANY BUYS, SIGNIFY THAT THERE IS         
         USING DIFFSUM,R2          MORE '+'                                     
         OC    DSRBUY#,SPACES                                                   
         CLC   DSRBUY#,SPACES                                                   
         BE    GN530                                                            
         CLC   =C'ADD',DSRBUY#                                                  
         BE    GN530                                                            
         LA    RE,DSRBUY#+1                                                     
         LA    RF,3                                                             
GN510    CLI   0(RE),C'+'          DONE THIS ALREADY                            
         BE    GN540                                                            
         CLI   0(RE),C' '                                                       
         BE    GN520                                                            
         AHI   RE,1                                                             
         BCT   RF,GN510                                                         
         B     GN540                                                            
*                                                                               
GN520    DS    0H                                                               
         MVI   0(RE),C'+'                                                       
         B     GN540                                                            
*                                                                               
GN530    DS    0H                                                               
         EDIT  TR.TLBLBUY#,DSRBUY#,ALIGN=LEFT,TRAIL=C'+'                        
         DROP  R2                                                               
*                                                                               
GN540    DS    0H                                                               
         TM    BUYSEG.TLBYAFLG,X'40' AGY BUY LINK?                              
         BZ    GNX                                                              
*                                                                               
* NOW SURE HOW THIS IS POSSIBLE TO HAVE BUY LINK FLAGGED BUT NO                 
* ACTUALLY LINK INFO, EXIT FOR NOW                                              
*                                                                               
         OC    BUYSEG.TLBYALNK,BUYSEG.TLBYALNK                                  
         BZ    GNX                                                              
*                                                                               
*                                                                               
         XC    TSARREC,TSARREC                                                  
         MVI   TR.TLKTYP,C'A'                                                   
         MVC   TR.TLBLINDX,BUYSEG.TLBYALNK                                      
         GOTO1 GOTSAR,TSARDH       RETRIEVE BUY LINK RECORD                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    FLAGS,FGDETLQ       SUMMARY OR DETAIL?                           
         BZ    GNX                                                              
*                                                                               
***********************************************************************         
* DISPLAY FOR DETAIL MULTIPLE AGY BUY USING BUY LINK RECORD                     
***********************************************************************         
         USING DIFFDET,R2                                                       
         EDIT  TR.TLBLTSPT,DDA#SPT,ALIGN=LEFT                                   
         EDIT  TR.TLBLBUY#,DDABUY#,ALIGN=LEFT,TRAIL=C'+'                        
***********                                                                     
         TM    FLAGS,FGEXPLDQ      DID USER REQUEST EXPLODED VIEW?              
         BZ    GNX                                                              
**********CHECK IF MULTI REP BUY ALREADY PUSHED LINES DOWN                      
**********CHECK IF MULTI REP BUY ALREADY PUSHED LINES DOWN                      
**********CHECK IF MULTI REP BUY ALREADY PUSHED LINES DOWN                      
**********CHECK IF MULTI REP BUY ALREADY PUSHED LINES DOWN                      
*                                                                               
         NI    DISPFLGS,X'FF'-DFCOMPQ                                           
         CLI   TR.TLBLBUY#+(2*8),0 MORE THAN 8 AGY LINES LINKED?                
         BE    *+8                                                              
         OI    DISPFLGS,DFCOMPQ    TOO MANY LINES, COMPRESSED MODE              
*                                                                               
         LA    R3,TR.TLBLBUY#                                                   
         LR    R1,R2                                                            
LINED    USING DIFFDET,R1                                                       
         TM    DISPFLGS,DFCOMPQ                                                 
         BZ    GN590                                                            
         XC    LINED.DDABUY#,LINED.DDABUY#                                      
         MVI   LINED.DDABUY#,C'+'                                               
         B     GN610                                                            
*                                                                               
* EXPLODE ALL LINKED AGY BUYS                                                   
*                                                                               
GN590    DS    0H                                                               
         LA    R4,LINED.DDABUY#                                                 
         TM    DISPFLGS,DFCOMPQ                                                 
         BZ    GN600                                                            
         LA    R4,LINED.DDDATE                                                  
         OC    LINED.DDRATE,LINED.DDRATE                                        
         BNZ   GN610               FIND FIRST BLANK AREA                        
         MVC   LINED.DDRATE(10),=C'AGY BUY#S:'                                  
*                                                                               
GN600    DS    0H                                                               
         EDIT  (1,0(R3)),(3,0(R4)),ALIGN=LEFT                                   
         AHI   R3,L'TLBLBUY#+L'TLBL#SPT                                         
         CLI   0(R3),0                                                          
         BE    GN630                                                            
         TM    DISPFLGS,DFCOMPQ                                                 
         BZ    GN610                                                            
         AR    R4,R0                                                            
         MVI   0(R4),C','                                                       
         AHI   R4,1                                                             
         LA    RF,LINED.DDA#SPT-4                                               
         CR    R4,RF                                                            
         BL    GN600                                                            
*                                                                               
GN610    DS    0H                                                               
         LA    RF,DIFENDH          DID WE GO PASS END OF SCREEN?                
         CR    R1,RF               YES, CONTINUE ON NEXT PAGE                   
         BL    GN620                                                            
         MVI   LINED.DDRBUY#+L'DDRBUY#-1,C'+'                                   
         B     GN630                                                            
*                                                                               
GN620    DS    0H                                                               
         SHI   R1,8                BACK TRACK TO THE START OF HEADER            
         ZIC   R0,0(R1)            NEXT SCREEN LINE                             
         AR    R1,R0                                                            
         AHI   R1,8                BUMP FORWARD TO DISPLAY AREA                 
         B     GN590                                                            
*                                                                               
GN630    DS    0H                                                               
         SR    R1,R2                                                            
         C     R1,NXOFFSET                                                      
         BNH   GNX                                                              
         ST    R1,NXOFFSET         NEXT DISPLAY LINE FOR NEXT SEGMENT           
         B     GNX                                                              
*                                                                               
GNX      DS    0H                                                               
         MVC   TXNUM,SVTSAR#       RESTORE SEQUENCE                             
         GOTO1 GOTSAR,TSAGET                                                    
         BE    EXIT                                                             
         DC    H'0'                                                             
         DROP  BUYSEG,TRDAYS,R2                                                 
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*****>>                                                                         
***********************************************************************         
* CHANGE THE PFKEY MENU ACCORDING TO WHAT SCREEN WE ARE IN                      
*                                                                               
***********************************************************************         
CHAPFLN  NTR1  BASE=*,LABEL=*                                                   
         LH    R5,SCRNUM                                                        
*        LA    R3,L'DIFPFLN+L'DIFPFLNH                                          
*        STC   R3,DIFPFLNH                                                      
         XC    DIFPFLN,DIFPFLN                                                  
         OI    DIFPFLNH+6,X'80'    RETRANSMIT PFKEY LINE                        
*                                                                               
         TM    FLAGS,FGDETLQ                                                    
         BZ    PFSUM                                                            
         TM    FLAGS2,FGALLQ                                                    
         BZ    DIFONLY                                                          
*                                                                               
DETLALL  DS    0H                 SEE ALL DETAILS MENUS                         
         MVC   DIFPFLN(72),=C'PF2=Con 3 Summary 4=*Only  5=Prt 8=+View X        
                9=Ref 10=Open 11=Rej 12=Return'                                 
         CHI   R5,1                                                             
         BNH   CHASIGN                                                          
*        XC    DIFPFLN,DIFPFLN                                                  
         MVC   DIFPFLN(72),=C'PF2=Con 3=Sum  4=*Only  5=Prt  7=Up  8=+VX        
               iew 9=Ref 10=Open 11=Rej 12=Ret'                                 
         B     CHASIGN                                                          
*                                                                               
DIFONLY  DS    0H                 SEE ONLY THE * BUYLINES                       
         MVC   DIFPFLN(72),=C'PF2=Con 3=Summary   4=All  5=Print  8=+ViX        
               ew  9=Ref 10=Open 11=Rej 12=Ret'                                 
         CHI   R5,1                                                             
         BNH   CHASIGN                                                          
*        XC    DIFPFLN,DIFPFLN                                                  
         MVC   DIFPFLN(72),=C'PF2=Con 3=Summary 4=All 5=Prt 7=Up 8=+VieX        
               w  9=Ref 10=Open 11=Rej 12=Ret '                                 
         B     CHASIGN                                                          
*                                                                               
PFSUM    DS    0H                                                               
         MVC   DIFPFLN(72),=C'PF2=Con 3=Details 5=Print  7=Up  8=+View X        
                9=Ref 10=Open 11=Rej 12=Return'                                 
         CHI   R5,1                                                             
         BH    CHASIGN                                                          
PFSUM20  DS    0H                                                               
*        XC    DIFPFLN,DIFPFLN                                                  
         MVC   DIFPFLN(72),=C'PF2=Con Dsm  3=Details  5=Print  8=+View X        
                9=Ref 10=Open 11=Rej 12=Return'                                 
                                                                                
         B     CHASIGN                                                          
CHASIGN  DS    0H                                                               
         LA    R3,DIFPFLN                                                       
         LA    R2,DIFPFLN+L'DIFPFLN                                             
         TM    FLAGS,FGEXPLDQ      IF EXPLOD VIEW, REPLACE + W/ -               
         BZ    MINUSV                                                           
         SR    R0,R0                                                            
         IC    R0,=C'+'                                                         
         SRST  R2,R3               SEARCHING FOR +                              
         BC    2,CHAPEXIT                                                       
         MVI   0(R2),C'-'          REPLACE WITH -                               
         B     CHAPEXIT                                                         
MINUSV   DS    0H                  OR VICE VERSA                                
         IC    R0,=C'-'                                                         
         SRST  R2,R3                                                            
         BC    2,CHAPEXIT                                                       
         MVI   0(R2),C'+'                                                       
CHAPEXIT DS    0H                                                               
         B     EXIT                                                             
*****>>                                                                         
***********************************************************************         
* SAVE THE THE TSAR REC # OF FIRST LINE ON THE SCREEN TO A TABLE                
*      FOR SCROLLING BACK PURPOSE                                               
***********************************************************************         
SCRLSAVE NTR1  BASE=*,LABEL=*                                                   
         TM    FLAGS2,SCRLBACK     ONLY SAVE WHEN GOING FORWARD                 
         BO    EXIT                                                             
         CLI   PFKEY,8             DON'T SAVE WHEN F8 IS PRESSED                
         BE    EXIT                                                             
         LH    RF,SCRNUM           SAVE TXNUM TO SCROLL TABLE                   
         CHI   RF,MAXSCRLL                                                      
         BNL   TABOVER                                                          
*                                                                               
         LA    RE,SCRLLTAB                                                      
         MHI   RF,L'SCRLLTAB                                                    
         AR    RE,RF                                                            
         MVC   0(L'SCRLLTAB,RE),TXNUM                                           
*                                                                               
         LH    RF,SCRNUM                                                        
         AHI   RF,1                                                             
         STH   RF,SCRNUM                                                        
*                                                                               
* TEST   USING DIFFDET,R2                                                       
* TEST   EDIT  SCRNUM,DDLEN                                                     
* TEST   EDIT  TXNUM,DDRATE                                                     
*                                                                               
         XIT1                                                                   
***********************************************************************         
* GET TOTALS REP AND AGY SPOTS FOR THIS BUY SEGMENT                             
***********************************************************************         
GETTSPTS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   GTSSVREC,TSARREC    SAVE OFF CURRENT TSAR RECORD                 
         MVC   GTSSVTR#,TXNUM       AND TSAR RECORD NUMBER                      
*                                                                               
BUYSEG   USING TLSTD,GTSSVREC                                                   
*                                                                               
         MVC   TOTREPSP,BUYSEG.TLBYRLNK+1                                       
         MVC   TOTAGYSP,BUYSEG.TLBYALNK+1                                       
         MVC   REPSPWK,BUYSEG.TLBYRLNK+1                                        
         MVC   AGYSPWK,BUYSEG.TLBYALNK+1                                        
*                                                                               
         TM    BUYSEG.TLBYRFLG,X'40' REP BUY LINK?                              
         BZ    GETSP10                                                          
         XC    TSARREC,TSARREC                                                  
         MVI   TR.TLKTYP,C'R'                                                   
         MVC   TR.TLBLINDX,BUYSEG.TLBYRLNK                                      
         GOTO1 GOTSAR,TSARDH       RETRIEVE BUY LINK RECORD                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   TOTREPSP,TR.TLBLTSPT                                             
         MVC   REPSPWK,TR.TLBL#SPT                                              
*                                                                               
GETSP10  DS    0H                                                               
         TM    BUYSEG.TLBYAFLG,X'40' AGY BUY LINK?                              
         BZ    GETSPX                                                           
*                                                                               
* NOW SURE HOW THIS IS POSSIBLE TO HAVE BUY LINK FLAGGED BUT NO                 
* ACTUALLY LINK INFO, EXIT FOR NOW                                              
*                                                                               
         OC    BUYSEG.TLBYALNK,BUYSEG.TLBYALNK                                  
         BZ    GETSPX                                                           
*                                                                               
         XC    TSARREC,TSARREC                                                  
         MVI   TR.TLKTYP,C'A'                                                   
         MVC   TR.TLBLINDX,BUYSEG.TLBYALNK                                      
         GOTO1 GOTSAR,TSARDH       RETRIEVE BUY LINK RECORD                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   TOTAGYSP,TR.TLBLTSPT                                             
         MVC   AGYSPWK,TR.TLBL#SPT                                              
*                                                                               
GETSPX   DS    0H                                                               
         MVC   TXNUM,GTSSVTR#      RESTORE SEQUENCE                             
         GOTO1 GOTSAR,TSAGET                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
         DROP  BUYSEG                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DUMP CONTENTS OF TSAR BUFFER TO REPORT                                        
***********************************************************************         
DUMPTSAR NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         OI    GENSTAT3,NOCLRSPK                                                
         MVC   REMUSER,=C'KUI'     DARE AGENCY ORDER                            
*                                                                               
         LA    RF,SPOOLKEY                                                      
         USING PQPLD,RF                                                         
         XC    SPOOLKEY,SPOOLKEY                                                
         MVC   PLDESC,=C'TSAR BUFFER'                                           
         MVI   PLCLASS,C' '                                                     
         OI    SPOOLIND,SPUINIT    PERMITS SETTING OF CLASS                     
         MVC   PLSUBID,=C'KUI'     DARE AGENCY ORDER                            
         DROP  RF                                                               
*                                                                               
PR05     DS    0H                                                               
         LA    RE,SPLKEYAD                                                      
         ST    RE,SPOOLQLK         SET EXTENDED KEY ADDRESS                     
         USING PQPLD,RE                                                         
         XC    0(133,RE),0(RE)                                                  
         MVI   QLEXTRA,X'FF'                                                    
         MVC   QLRETNL,=H'6'                                                    
         MVC   QLRETND,=H'2'                                                    
         MVC   QLRETNL,=H'36'                                                   
         MVI   QLSYS,C'R'          FORM/COPIES COLUMN                           
         MVC   QLPRG,=C'CO'                                                     
         DROP  RE                                                               
                                                                                
         GOTO1 OPENPQ                                                           
*                                                                               
         MVC   P(19),=C'*** TSAR BUFFER ***'                                    
         BAS   RE,PRINT                                                         
*                                                                               
*        GOTO1 HEXOUT,DMCB,TB.TSRNUM,P,2                                        
*        BAS   RE,PRINT                                                         
         XC    TSARREC,TSARREC                                                  
         GOTO1 GOTSAR,TSARDH                                                    
*                                                                               
DT10     DS    0H                                                               
         MVC   P(132),TSARREC                                                   
         BAS   RE,PRINT                                                         
         GOTO1 HEXOUT,DMCB,TB.TSRNUM,P,2                                        
         GOTO1 HEXOUT,DMCB,TR.TLLEN,P+6,2                                       
         GOTO1 HEXOUT,DMCB,TR.TLKEY,P+12,40                                     
         BAS   RE,PRINT                                                         
         XC    TSARREC,TSARREC                                                  
         GOTO1 GOTSAR,TSANXT                                                    
         BE    DT10                                                             
*                                                                               
DTX      DS    0H                                                               
         MVI   SPMODE,X'FF'                                                     
         BAS   RE,PRINT            CLOSE PRINT QUEUE                            
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CALCULATE GRAND TOTAL DOLLARS AND SPOTS. TAKE INTO ACCOUNT POSSIBLE           
* TAKEOVER ORDER                                                                
***********************************************************************         
DISTOTAL NTR1  BASE=*,LABEL=*                                                   
         XC    GTOTAL$,GTOTAL$                                                  
         XC    GSPT#,GSPT#                                                      
         OI    FLAGS,FGTOTALQ      SKIP BUY DETAIL BREAK OUT PROCESSING         
*                                                                               
         MVC   AIO,AIO1                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(RDARKRT-RDARKEY),SELECTKY                                    
*                                                                               
         MVI   KEY+RDARKRT-RDARKEY,X'40' BUY RECORD                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
DIST10   DS    0H                                                               
         CLC   KEY(RDARKSEQ-RDARKEY),KEYSAVE                                    
         BNE   DIST70                                                           
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         CLI   KEY+RDARKSRT-RDARKEY,X'00' BUY HEADER                            
         BE    DIST20                                                           
         CLI   KEY+RDARKSRT-RDARKEY,X'10' BUY ORBITS                            
         BE    DIST30                                                           
         CLI   KEY+RDARKSRT-RDARKEY,X'30' BUY DETAIL                            
         BE    DIST40                                                           
         B     DIST60                                                           
*                                                                               
* PROCESS HEADER RECORD                                                         
*                                                                               
DIST20   DS    0H                                                               
         L     R6,AIO                                                           
         USING RDARBYEL,R6                                                      
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    RDARBYDL,X'80'+X'40' HARD/SOFT DELETED??                         
         BNZ   DIST60              SKIP                                         
         MVC   BUYCOST,RDARBYCO                                                 
         MVI   STARTDAY,0          CLEAR START/END DAYS OF WEEK                 
         MVI   ENDDAY,0                                                         
         GOTO1 =A(STARTEND),DMCB,RDARBYRO,WORK,RR=RELO                          
         B     DIST60                                                           
         DROP  R6                                                               
*                                                                               
DIST30   DS    0H                                                               
         L     R6,AIO                                                           
         USING RDAROBEL,R6                                                      
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    RDAROBDL,X'80'+X'40' HARD/SOFT DELETED??                         
         BNZ   DIST60              SKIP                                         
         DROP  R6                                                               
*                                                                               
         GOTO1 =A(BUYORBIT),DMCB,AIO,RR=RELO                                    
         B     DIST60              READ NEXT                                    
*                                                                               
DIST40   DS    0H                                                               
         L     R6,AIO                                                           
         USING RDARBDEL,R6                                                      
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    RDARBDDL,X'80'+X'40' HARD/SOFT DELETED??                         
         BNZ   DIST60              SKIP                                         
         DROP  R6                                                               
*                                                                               
         GOTO1 =A(BUYDETL),DMCB,AIO,RR=RELO                                     
         BNE   DIST60              READ NEXT                                    
*                                                                               
         L     R6,AIO                                                           
         USING RDARBUEL,R6                                                      
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DIST50   DS    0H                                                               
         ZIC   R1,RDARBUWK                                                      
         XC    HALF,HALF                                                        
         MVC   HALF+1(1),RDARBUSW                                               
         MH    R1,HALF                                                          
         ZICM  R0,GSPT#,4                                                       
         AR    R0,R1                                                            
         STCM  R0,15,GSPT#                                                      
         MVC   FULL,RDARBU$$                                                    
         M     R0,FULL                                                          
         ZICM  R0,GTOTAL$,4                                                     
         AR    R0,R1                                                            
         STCM  R0,15,GTOTAL$                                                    
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BE    DIST50                                                           
         DROP  R6                                                               
*                                                                               
DIST60   DS    0H                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         B     DIST10                                                           
*                                                                               
DIST70   DS    0H                  DISPLAY GRAND TOTAL DOLLAR/SPOT              
         TM    FLAGS2,TOTALPRC     SKIP DISPLAY IF WE ARE PRINTING              
         BO    DIST120                                                          
*                                                                               
         EDIT  GTOTAL$,DIFATOT,2,COMMAS=YES,FLOAT=$,ZERO=NOBLANK                
         EDIT  GSPT#,DIFASPT,ZERO=NOBLANK                                       
*                                                                               
* DISPLAY CONTRACT TOTALS AS WELL                                               
*                                                                               
         EDIT  RPORDTOT,DIFCTOT,2,COMMAS=YES,FLOAT=$,ZERO=NOBLANK               
         EDIT  RPSPTTOT,DIFCSPT,ZERO=NOBLANK                                    
*                                                                               
* DISPLAY (AGENCY - CONTRACT) TOTALS                                            
*                                                                               
         ICM   R3,15,GTOTAL$                                                    
         ICM   R4,15,RPORDTOT                                                   
         SR    R3,R4                                                            
         EDIT  (R3),DIFDTOT,2,COMMAS=YES,FLOAT=$,ZERO=NOBLANK                   
         ST    R3,DIFTOT                                                        
*                                                                               
         CLC   GTOTAL$,RPORDTOT                                                 
         BE    DIST100             SHOW SIGN                                    
         LA    RE,DIFDTOT                                                       
         LA    RF,L'DIFDTOT                                                     
DIST80   CLI   0(RE),C'$'                                                       
         BE    DIST90                                                           
         AHI   RE,1                                                             
         BCT   RF,DIST80                                                        
         B     DIST100                                                          
*                                                                               
DIST90   DS    0H                                                               
         SHI   RE,1                                                             
         MVI   0(RE),C'-'                                                       
         CLC   GTOTAL$,RPORDTOT                                                 
         BL    DIST100             SHOW SIGN                                    
         MVI   0(RE),C'+'                                                       
*                                                                               
DIST100  DS    0H                                                               
         ICM   R3,15,GSPT#                                                      
         ZICM  R4,RPSPTTOT,2                                                    
         SR    R3,R4                                                            
         BNP   DIST110                                                          
         EDIT  (R3),DIFDSPT,ZERO=NOBLANK,FLOAT=+                                
         ST    R3,DIFSPT                                                        
         B     DIST120                                                          
*                                                                               
DIST110  DS    0H                                                               
         EDIT  (R3),DIFDSPT,ZERO=NOBLANK,FLOAT=-                                
*                                                                               
DIST120  DS    0H                                                               
         NI    FLAGS,X'FF'-FGTOTALQ                                             
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SWAP TO CONTRACT                                                              
***********************************************************************         
SWAP2CON NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   KEY(L'SELECTKY),SELECTKY                                         
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
*                                                                               
         XC    BLOCK(256),BLOCK                                                 
         LA    R1,BLOCK                                                         
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'REP'    FROM THE REP SYSTEM                          
         MVC   GLVXFRPR,=C'DAR'    DARE PROGRAM                                 
         MVC   GLVXTOSY,=C'REP'    TO THE REP SYSTEM                            
         MVC   GLVXTOPR,=C'CON'    CONTRACT PROGRAM                             
***>>>   OI    GLVXFLG1,GLV1GOTO+GLV1SEPS   CALL BASE ON TRANSFER               
         OI    GLVXFLG1,GLV1SEPS   CALL BASE ON TRANSFER                        
         DROP  R1                                                               
*                                  SET UP THE TRANSFER CONTROL BLOCK            
         L     R4,ACOMFACS                                                      
         USING COMFACSD,R4                                                      
*                                                                               
         GOTO1 CGLOBBER,DMCB,=C'PUTD',BLOCK,14,GLVXCTL                          
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         MVC   FULL,RDARREP#                                                    
         OC    RDARREP#,RDARREP#                                                
         BNZ   *+10                                                             
         MVC   FULL,CCONKNUM                                                    
         DROP  R6                                                               
*                                                                               
         LA    R2,DIFHDLNH         ANY CONTRACT NUMBER?                         
         GOTO1 CGLOBBER,DMCB,=C'PUTD',FULL,L'FULL,GLRCONNO                      
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTO1 CGLOBBER,DMCB,=C'PUTD',=CL1'D',1,GLRPFKEY                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R4                                                               
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*                                                                               
* APPROVAL LOGIC                                                                
*                                                                               
***********************************************************************         
APPROVE  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 =A(DATETIME),RR=RELO                                             
*                                                                               
         MVI   ACTCODE,C'A'                                                     
*                                                                               
         XC    TSARREC,TSARREC                                                  
         MVI   TR.TLKTYP,X'0B'                                                  
         GOTO1 GOTSAR,TSARDH                                                    
         BL    AP500                                                            
*                                                                               
         XC    SEGSTRDT,SEGSTRDT   SEGMENT START DATE                           
         XC    SEGENDDT,SEGENDDT   SEGMENT END DATE                             
*                                                                               
AP09     DS    0H                                                               
         XC    BUYGRID,BUYGRID                                                  
         XC    GSTRDATE,GSTRDATE                                                
         MVC   SEGSTRDT,TR.TLBYWKOF                                             
*                                                                               
         CLI   TR.TLKTYP,X'0B'                                                  
         BNE   AP500                                                            
*                                                                               
* CHECK IF SEGMENT IS FROM A SINGLE BUY. IF IT IS, THE SEGMENT                  
* CONTAINS THE ACTUAL BUY NUMBER AND SPOT COUNT                                 
* IF NOT, WE NEED TO FIND THE LINK TO THE LIST OF BUYS AGAINST THIS             
* SEGMENT                                                                       
*                                                                               
         GOTO1 =A(GETTSPTS),RR=RELO                                             
*                                                                               
         CLC   TOTREPSP,TOTAGYSP   SKIP IF ALL SPOTS MATCHED                    
         BNE   AP45                                                             
*                                                                               
* SKIP MATCHED SPOTS                                                            
*                                                                               
         GOTO1 GOTSAR,TSANXT                                                    
         BL    AP500                                                            
         CLI   TR.TLKTYP,X'0B'                                                  
         BE    AP09                                                             
         B     AP500                                                            
***********************************************************************         
* CHECK NEXT BUY SEGMENT TO SEE IF WE CAN COMBINE IT TO CURRENT SEGMENT         
* THE CURRENT SEGMENT IS SAVED OFF AS 'PREVIOUS' WHILE THE NEXT SEGMENT         
* WILL BE READ IN TO MEMORY AS THE CURRENT SEGMENT FOR COMPARISON               
***********************************************************************         
AP45     DS    0H                                                               
         MVC   APSVREC,TSARREC     SAVE OFF CURRENT BUY SEGMENT                 
         MVC   PRVREPSP,TOTREPSP   FOR LATER COMPARISON                         
         MVC   PRVAGYSP,TOTAGYSP                                                
PREV     USING TLSTD,APSVREC                                                    
*                                                                               
* CHECK IF NEXT BUY SEGMENT IS A CONTINUATION FROM THIS BUY SEGMENT             
*                                                                               
         MVC   PVSVTR#,TXNUM       SAVE TSAR# OF 'PREVIOUS' BUY SEGMENT         
*                                                                               
AP70     DS    0H                                                               
         GOTO1 GOTSAR,TSANXT                                                    
         BL    AP300                                                            
         CLI   TR.TLKTYP,X'0B'                                                  
         BNE   AP300                                                            
*                                                                               
         CLC   PREV.TLKEY(TLBYWKOF-TLKEY),TR.TLKEY                              
         BNE   AP300                                                            
*                                                                               
* NEXT BUY SEGMENT SHOULD HAVE WEEK-OF DATE EXACTLY ONE WEEK                    
* FROM THE WEEK-OF DATE OF CURRENT BUY SEGMENT                                  
*                                                                               
         GOTO1 DATCON,DMCB,(2,PREV.TLBYWKOF),(0,WORK)                           
         LA    RF,7                                                             
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(RF)                                      
         GOTO1 DATCON,DMCB,(2,TR.TLBYWKOF),(0,WORK)                             
         CLC   WORK(6),WORK+6                                                   
         BNE   AP300                                                            
*                                                                               
* CHECK IF ALL SPOTS MATCHED ON BOTH REP AND AGENCY                             
* IF SO, SKIP BUY SEGMENT                                                       
*                                                                               
         GOTO1 =A(GETTSPTS),RR=RELO                                             
*                                                                               
         CLC   TOTREPSP,TOTAGYSP   SKIP IF ALL SPOTS MATCHED                    
         BE    AP70                                                             
*                                                                               
* CHECK IF SPOTS COVERED BY BOTH REP AND AGENCY ARE THE SAME AS                 
* THE PREVIOUS BUY SEGMENT                                                      
*                                                                               
AP80     DS    0H                                                               
*                                                                               
* CHECK IF THE BUY OR LIST OF BUYS ARE THE SAME AS THE PREVIOUS                 
* BUY SEGMENT                                                                   
*                                                                               
* CHECK REP BUYS                                                                
*                                                                               
AP90     DS    0H                                                               
         TM    PREV.TLBYRFLG,X'40' ACTUAL BUY OR BUY LINK?                      
         BO    AP95                                                             
         TM    TR.TLBYRFLG,X'40'                                                
         BO    AP300                                                            
         CLC   PREV.TLBYRLNK,TR.TLBYRLNK                                        
         BNE   AP300                                                            
*        MVC   SEGENDDT,TR.TLBYWKOF                                             
*        B     AP45                                                             
         B     AP200                                                            
*                                                                               
AP95     DS    0H                  CHECK  BUY LINK RECORD                       
         TM    TR.TLBYRFLG,X'40'                                                
         BZ    AP300                                                            
***********************************************************************         
*                                                                               
* RETRIEVE BUY LINK RECORDS FOR BOTH THE CURRENT AND PREVIOUS SEGMENTS          
*                                                                               
***********************************************************************         
         MVC   CRSVTR#,TXNUM       SAVE 'CURRENT' TSAR RECORD #                 
*                                                                               
         XC    TSARKEY,TSARKEY                                                  
TKEY     USING TLKEY,TSARKEY                                                    
         MVI   TKEY.TLKTYP,C'R'                                                 
         MVC   TKEY.TLBLINDX,TR.TLBYRLNK                                        
         DROP  TKEY                                                             
*                                                                               
         MVC   TR.TLKEY,TSARKEY                                                 
         GOTO1 GOTSAR,TSARDH       RETRIEVE BUY LNK FOR CURRENT SEGMENT         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DLSVBLRC,TSARREC                                                 
*                                                                               
         XC    TSARKEY,TSARKEY                                                  
TKEY     USING TLKEY,TSARKEY                                                    
         MVI   TKEY.TLKTYP,C'R'                                                 
         MVC   TKEY.TLBLINDX,PREV.TLBYRLNK                                      
         DROP  TKEY                                                             
*                                                                               
         MVC   TR.TLKEY,TSARKEY                                                 
         GOTO1 GOTSAR,TSARDH       RETRIEVE BUY LNK FOR PREVIOUS SEGMNT         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   TR.TLLEN,DLSVBLRC   SAME LENGTH?                                 
         BNE   AP300                                                            
*                                                                               
         LA    R0,TSARREC          CHECK IF LIST OF BUYS ARE THE SAME           
         AHI   R0,TLBLTSPT-TLLEN                                                
         ZICM  R1,TR.TLLEN,2                                                    
         SHI   R1,TLBLTSPT-TLLEN                                                
*                                                                               
         LA    RE,DLSVBLRC                                                      
         AHI   RE,TLBLTSPT-TLLEN                                                
         LR    RF,R1                                                            
         CLCL  R0,RE                                                            
         BNE   AP300                                                            
*                                                                               
         MVC   TXNUM,CRSVTR#                                                    
         GOTO1 GOTSAR,TSAGET       RESTORE                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* CHECK AGENCY BUYS                                                             
*                                                                               
AP200    DS    0H                                                               
         TM    PREV.TLBYAFLG,X'40' ACTUAL BUY OR BUY LINK?                      
         BO    AP210                                                            
         TM    TR.TLBYAFLG,X'40'                                                
         BO    AP300                                                            
         CLC   PREV.TLBYALNK,TR.TLBYALNK                                        
         BNE   AP300                                                            
*        MVC   SEGENDDT,TR.TLBYWKOF                                             
*        B     AP45                                                             
         B     AP220                                                            
*                                                                               
AP210    DS    0H                  CHECK  BUY LINK RECORD                       
         TM    TR.TLBYAFLG,X'40'                                                
         BZ    AP300                                                            
***********************************************************************         
*                                                                               
* RETRIEVE BUY LINK RECORDS FOR BOTH THE CURRENT AND PREVIOUS SEGMENTS          
*                                                                               
***********************************************************************         
         MVC   CRSVTR#,TXNUM       SAVE 'CURRENT' TSAR REC NUMBER               
*                                                                               
         XC    TSARKEY,TSARKEY                                                  
TKEY     USING TLKEY,TSARKEY                                                    
         MVI   TKEY.TLKTYP,C'A'                                                 
         MVC   TKEY.TLBLINDX,TR.TLBYALNK                                        
         DROP  TKEY                                                             
*                                                                               
         MVC   TR.TLKEY,TSARKEY                                                 
         GOTO1 GOTSAR,TSARDH       RETRIEVE BUY LNK FOR CURRENT SEGMENT         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DLSVBLRC,TSARREC                                                 
*                                                                               
         XC    TSARKEY,TSARKEY                                                  
TKEY     USING TLKEY,TSARKEY                                                    
         MVI   TKEY.TLKTYP,C'A'                                                 
         MVC   TKEY.TLBLINDX,PREV.TLBYALNK                                      
         DROP  TKEY                                                             
*                                                                               
         MVC   TR.TLKEY,TSARKEY                                                 
         GOTO1 GOTSAR,TSARDH       RETRIEVE BUY LNK FOR PREVIOUS SEGMNT         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   TR.TLLEN,DLSVBLRC   SAME LENGTH?                                 
         BNE   AP300                                                            
*                                                                               
         LA    R0,TSARREC          CHECK IF LIST OF BUYS ARE THE SAME           
         AHI   R0,TLBLTSPT-TLLEN                                                
         ZICM  R1,TR.TLLEN,2                                                    
         SHI   R1,TLBLTSPT-TLLEN                                                
*                                                                               
         LA    RE,DLSVBLRC                                                      
         AHI   RE,TLBLTSPT-TLLEN                                                
         LR    RF,R1                                                            
*                                                                               
         CLCL  R0,RE                                                            
         BNE   AP300                                                            
*                                                                               
         MVC   TXNUM,CRSVTR#                                                    
         GOTO1 GOTSAR,TSAGET       RESTORE                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
AP220    DS    0H                                                               
         MVC   SEGENDDT,TR.TLBYWKOF                                             
*                                                                               
* FOR ADDITIONAL SPOTS ONLY:                                                    
* INCASE OF AN AGENCY BUY WITH DIFFERENT SETS OF EFFECTIVE DATES                
* PRE-BUILD THE BUY GRID TO BE ADDED                                            
*                                                                               
AP250    DS    0H                                                               
         ZIC   RE,PRVAGYSP         BUY WILL RECEIVE THE ADDITIONAL              
         ZIC   RF,PRVREPSP         SPOTS                                        
         SR    RE,RF                                                            
         BNP   AP45                                                             
         STC   RE,BMSS#SPT                                                      
*                                                                               
         OC    GSTRDATE,GSTRDATE                                                
         BNZ   AP260                                                            
         GOTO1 DATCON,DMCB,(2,SEGSTRDT),(3,GSTRDATE)                            
*                                                                               
AP260    DS    0H                                                               
         GOTO1 DATCON,DMCB,(2,PREV.TLBYWKOF),(3,BSTRDATE)                       
*                                                                               
*        GOTOX (C'+',ADJSPTS)                                                   
         GOTO1 =A(ADJSPTS),DMCB,(C'+',0),RR=RELO                                
         B     AP45                                                             
*                                                                               
* NEXT BUY SEGMENT IS DIFFERENT. PROCESS CHANGE FOR CURRENT SEGMENT(S)          
* AND CORRESPONDING BUY(S)                                                      
*                                                                               
AP300    DS    0H                                                               
         MVC   TXNUM,PVSVTR#                                                    
         GOTO1 GOTSAR,TSAGET       RESTORE                                      
         BE    AP310                                                            
         DC    H'0'                                                             
*                                                                               
* ADD/REMOVE SPOTS FROM REP BUYS                                                
*                                                                               
AP310    DS    0H                                                               
         GOTO1 =A(UPDTBUYS),RR=RELO                                             
*                                                                               
AP320    DS    0H                                                               
         XC    SEGENDDT,SEGENDDT                                                
         MVC   PRVREPSP,TOTREPSP   RESET FOR NEXT SEGMENT COMPARISON            
         MVC   PRVAGYSP,TOTAGYSP                                                
*                                                                               
         GOTO1 GOTSAR,TSANXT                                                    
         BL    AP500                                                            
         CLI   TR.TLKTYP,X'0B'                                                  
         BE    AP09                                                             
*                                                                               
* UPDATE CONTRACT RECORD                                                        
*                                                                               
AP500    DS    0H                                                               
         MVI   FLTKEYFG,C'N'       DEFAULT DON'T REWRITE 8D/8E POINTERS         
*                                  OVERRIDE REP CON'S FLIGHT DATES              
*                                     WITH AGENCY ORDER'S DATES                 
         GOTO1 DATCON,DMCB,(2,FLTDATES),(3,WORK)                                
         GOTO1 DATCON,(R1),(2,FLTDATES+2),(3,WORK+3)                            
*                                                                               
         L     R6,AIO3                                                          
         USING RCONREC,R6                                                       
*                                                                               
         GOTO1 DATCON,DMCB,(3,RCONDATE),(2,CONFLTDT)                            
         GOTO1 DATCON,(R1),(3,RCONDATE+3),(2,CONFLTDT+2)                        
*                                  SAVE ORIGINAL CONTRACT DATES                 
         CLC   RCONDATE,WORK       IF DIFFERENT, FLAG TO UPDATE                 
         BE    AP510               8E/8D KEYS TO CONTRACT                       
*                                                                               
         MVC   RCONDATE,WORK                                                    
         MVI   FLTKEYFG,C'Y'                                                    
*                                                                               
AP510    DS    0H                                                               
         CLC   RTKODATE,RCONDATE   DATE CHOPPING??                              
         BNH   AP520                                                            
*                                                                               
         MVC   RCONDATE(3),RTKODATE                                             
         MVI   FLTKEYFG,C'Y'                                                    
*                                                                               
AP520    DS    0H                  OVERRIDE BUYER NAME WITH A/O NAME            
         MVC   RCONBUYR(20),SAVEBUYR                                            
         DROP  R6                                                               
*                                                                               
         L     R6,AIO3                                                          
         MVI   ELCODE,X'1D'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONDREL,R6                                                      
         OI    RCONDRFG,X'40'      SET APPROVED AND NOT REJECT/RECALL           
         NI    RCONDRFG,X'FF'-X'20'-X'10'                                       
         MVC   RCONDRDA,ACTDATE                                                 
         MVC   RCONDRTA,ACTTIME                                                 
         DROP  R6                                                               
*                                                                               
         L     R6,AIO3                                                          
         MVI   ELCODE,X'1F'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONXEL,R6                                                       
         TM    RCONCONF,X'40'      CONFIRMED NOW                                
         BZ    AP530                                                            
*                                                                               
         NI    RCONCONF,X'FF'-X'40' TURN OFF CONFIRMED NOW                      
         OI    RCONCONF,X'20'      TURN ON CONFIRMED PREVIOUSLY                 
AP530    OI    RCONCONF,X'80'      NOT CONFIRMED                                
         DROP  R6                                                               
*                                                                               
         TM    MISCFLG2,MF2USRDM   USER DEFINED TARGET DEMO?                    
         BO    AP533               YES, BLOW AWAY OLD DEMO                      
         TM    FLAGS2,FG2DEMO      UPDATE DARE DEMO CATEGORIES ?                
         BZ    AP535                                                            
         TM    MISCFLG2,MF2NOADM                                                
         BO    AP535                                                            
         OC    SVDEMCAT,SVDEMCAT                                                
         BZ    AP535                                                            
         CLI   SVDEMCAT+1,2        NO DEMO, SKIP ELEMENT ADD                    
         BNH   AP535                                                            
AP533    DS    0H                                                               
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'DD',AIO3),0,0                   
         CLI   SVDEMCAT+1,2        NO DEMO, SKIP ELEMENT ADD                    
         BNH   AP535                                                            
         TM    MISCFLG2,MF2USRDM   DON'T ADD USER DEFINED TARGET DEMO           
         BO    AP535                                                            
*                                                                               
         XC    ELEM,ELEM                                                        
         MVC   ELEM(L'SVDEMCAT),SVDEMCAT                                        
         MVC   AIO,AIO3                                                         
         GOTO1 ADDELEM                                                          
*                                                                               
AP535    DS    0H                                                               
*                                                                               
* UPDATE EI FIELDS. ALSO SUPPORT FOR ALPHANUMERIC ESTIMATE #                    
*                                                                               
         L     R6,AIO3                                                          
         MVI   ELCODE,X'A2'        RETRIEVE                                     
         BRAS  RE,GETEL                                                         
         BE    AP540               FOUND: JUST REPLACE THE CODES                
         XC    ELTBUILD,ELTBUILD   NOT FOUND: BUILD IT                          
         MVC   ELTBUILD(2),=X'A220'                                             
*                                  INSERT ELEMENT CODE/LENGTH                   
         MVC   ELTBUILD+2(L'SAVEEASI),SAVEEASI                                  
*                                  INSERT EASI CODES FROM AGY HEADER            
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),AIO3,ELTBUILD,=C'ADD=CODE'         
*                                                                               
         L     R6,AIO3                                                          
         MVI   ELCODE,X'A2'        RETRIEVE                                     
         BRAS  RE,GETEL                                                         
         BE    AP550               MUST BE THERE!                               
         DC    H'0'                                                             
AP540    EQU   *                                                                
         MVC   RCONIADV-RCONIEL(L'SAVEEASI,R6),SAVEEASI                         
*                                  REPLACE EXISTING CODES WITH NEW              
*                                                                               
* MIGRATE TO NEW EXPANDED ESTIMATE FIELD                                        
*                                                                               
AP550    EQU   *                                                                
EASID    USING RCONIEL,R6                                                       
         XC    EASID.RCONXEST,EASID.RCONXEST                                    
         MVC   EASID.RCONXEST(L'RCONIEST),EASID.RCONIEST                        
         XC    EASID.RCONIEST,EASID.RCONIEST                                    
         OC    SAVEEST#,SAVEEST#   USE EBCDIC VALUE IF PRESENT                  
         BZ    *+10                INCASE OF ALPHA CHARACTERS                   
         MVC   EASID.RCONXEST(L'SAVEEST#),SAVEEST#                              
         DROP  EASID                                                            
*                                                                               
         TM    MISCFLAG,MFCONOK    IF NOT ALREADY                               
         BO    APX                                                              
         L     RF,AIO3                                                          
         MVC   KEY(L'RCONKEY),0(RF)                                             
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(L'RCONKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'Y'                                                    
         MVC   AIO,AIOAREA         WRITE OUT CONTRACT RECORD                    
         GOTO1 GETREC                                                           
         MVC   AIO,AIO3                                                         
         GOTO1 PUTREC                                                           
*                                                                               
         CLI   FLTKEYFG,C'Y'       NEED TO REFRESH 8D/E POINTER?                
         BNE   APX                                                              
*                                  YES! CONFLTDT HAS OLD DATES                  
         GOTO1 =A(GEN8DEKY),RR=RELO                                             
*                                                                               
APX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE REP BUYS EITHER BY REMOVING OR ADDING SPOTS                            
***********************************************************************         
UPDTBUYS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         NI    FLAGS,X'FF'-FGADDSPQ-FGNEWBYQ                                    
         CLC   PRVAGYSP,PRVREPSP   DETERMINE WHETHER WE ARE ADDING              
         BL    *+8                 OR REMOVING SPOTS                            
         OI    FLAGS,FGADDSPQ                                                   
*                                                                               
         MVC   APSVREC,TSARREC     SAVE OFF CURRENT BUY SEGMENT                 
         MVC   UPSVTR#,TXNUM                                                    
*                                                                               
BUYSEG   USING TLSTD,APSVREC                                                    
*                                                                               
         TM    BUYSEG.TLBYRFLG,X'40' ACTUAL BUY OR BUY LINK?                    
         BO    UPBUY10             FOR ACTUAL BUY                               
         XC    TSARREC,TSARREC     BUILD FAKE BUY LINK RECORD                   
         MVC   TR.TLBLBUY#(2),BUYSEG.TLBYRLNK                                   
         CLI   BUYSEG.TLBYRLNK,0   NO REP BUY ATTACHED TO THIS SEGMENT?         
         BNE   UPBUY20             THAT MEANS WE NEED TO ADD NEW BUYS           
         OI    FLAGS,FGNEWBYQ      USE AGENCY SHADOW BUY FOR TEMPLATE           
*                                                                               
         TM    BUYSEG.TLBYAFLG,X'40'                                            
         BO    UPBUY10                                                          
         MVC   TR.TLBLBUY#(2),BUYSEG.TLBYALNK                                   
         B     UPBUY20                                                          
*                                                                               
UPBUY10  DS    0H                  CHECK  BUY LINK RECORD                       
         XC    TSARKEY,TSARKEY                                                  
TKEY     USING TLKEY,TSARKEY                                                    
         MVI   TKEY.TLKTYP,C'R'                                                 
         MVC   TKEY.TLBLINDX,BUYSEG.TLBYRLNK                                    
*                                                                               
         TM    FLAGS,FGNEWBYQ      USE AGENCY SHADOW BUY FOR TEMPLATE           
         BZ    UPBUY15                                                          
         MVI   TKEY.TLKTYP,C'A'                                                 
         MVC   TKEY.TLBLINDX,BUYSEG.TLBYALNK                                    
         DROP  TKEY                                                             
*                                                                               
UPBUY15  DS    0H                                                               
         XC    TSARREC,TSARREC                                                  
         MVC   TR.TLKEY,TSARKEY                                                 
         GOTO1 GOTSAR,TSARDH       RETRIEVE BUY LNK FOR CURRENT SEGMENT         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
UPBUY20  DS    0H                                                               
         LA    R3,TR.TLBLBUY#                                                   
*                                                                               
         XC    KEY,KEY                                                          
REPBUY   USING RBUYKEY,KEY                                                      
         MVI   REPBUY.RBUYKTYP,X'0B'                                            
*                                                                               
         TM    FLAGS,FGNEWBYQ      USE AGENCY SHADOW BUY FOR TEMPLATE           
         BZ    *+8                                                              
         MVI   REPBUY.RBUYKTYP+1,X'01'                                          
*                                                                               
         MVC   REPBUY.RBUYKREP,TWAAGY                                           
         MVC   REPBUY.RBUYKCON,COMPCON                                          
         MVC   REPBUY.RBUYKPLN,=X'FFFFFF'                                       
*                                                                               
         TM    FLAGS,FGNEWBYQ                                                   
         BZ    *+8                                                              
         NI    DMINBTS,X'FF'-X'08'                                              
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
UPBUY25  DS    0H                                                               
         CLC   KEY(RBUYKMLN-RBUYKEY),KEYSAVE                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    FLAGS,FGNEWBYQ      USE AGENCY SHADOW BUY FOR TEMPLATE           
         BZ    UPBUY27             MASTER LINE HAS ACTUAL AGENCY BUY #          
         CLC   REPBUY.RBUYKMLN,0(R3)                                            
         BE    UPBUY30                                                          
         B     UPBUY28                                                          
*                                                                               
UPBUY27  DS    0H                                                               
         CLC   REPBUY.RBUYKLIN,0(R3)                                            
         BE    UPBUY30                                                          
*                                                                               
UPBUY28  DS    0H                                                               
         TM    FLAGS,FGNEWBYQ                                                   
         BZ    *+8                                                              
         NI    DMINBTS,X'FF'-X'08'                                              
         GOTO1 SEQ                                                              
         B     UPBUY25                                                          
*                                                                               
UPBUY30  DS    0H                                                               
         MVC   SVBUYKEY,KEY                                                     
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         GOTO1 GETREC                                                           
*                                                                               
         XC    MYELEM,MYELEM       INIT FOR SPOT CHANGE COMMENT                 
         XC    CMTSTRDT,CMTSTRDT                                                
         XC    CMTENDDT,CMTENDDT                                                
         XC    CMTNUMWK,CMTNUMWK                                                
         XC    CMTNUMSP,CMTNUMSP                                                
*                                                                               
         TM    FLAGS,FGNEWBYQ      USE AGENCY SHADOW BUY FOR TEMPLATE           
         BZ    UPBUY32                                                          
         L     R6,AIO2                                                          
         USING RBUYREC,R6                                                       
*                                                                               
* INCASE OF ADDING DAILY BUYS, THERE MIGHT BE MULTIPLE BUYS WITH                
* THE SAME RBUYKMLN(AGENCY) BUY NUMBER. WE NEED TO CHECK EACH OF THESE          
* BUYS TO FIND THE ONE THAT MATCH THE CURRENT BUY SEGMENT TO BE ADDED           
*                                                                               
         TM    RBUYFLG2,X'80'      DAILY?                                       
         BZ    UPBUY31                                                          
         GOTO1 =A(CHKDAILY),RR=RELO                                             
         BNZ   UPBUY28                                                          
*                                                                               
UPBUY31  DS    0H                                                               
*                                                                               
         NI    RBUYCNTL,X'FF'-X'80' INCASE DELETE WAS SET                       
         MVI   RBUYKTYP+1,0        CLEAR BUY SHADOW SUBTYPE                     
*                                                                               
* UPDATE RATE INCASE OF COST OVERRIDE FOR THIS SEGMENT                          
*                                                                               
         MVC   RBUYCOS,BUYSEG.TLBYRATE                                          
         DROP  R6                                                               
*                                                                               
UPBUY32  DS    0H                                                               
         GOTO1 DATCON,DMCB,(2,SEGSTRDT),(3,BSTRDATE)                            
*                                                                               
         TM    FLAGS,FGADDSPQ                                                   
         BZ    UPBUY33                                                          
*                                                                               
*        XC    BUYGRID,BUYGRID     INIT INCASE WE ARE ADDING NEW BUY            
*        MVC   GSTRDATE,BSTRDATE                                                
         CLI   METHOD,ADDBUYQ                                                   
         BE    UPBUY40                                                          
*                                                                               
UPBUY33  DS    0H                                                               
         MVI   BUCKFLAG,X'FF'      SET TO BACK OUT FIGURES                      
         OI    BUCKFLGS,X'10'      DON'T IGNORE CANCELLED BUYS                  
*                                  BACK OUT BUCKETS                             
         GOTO1 =A(BUCKUPDT),RR=RELO                                             
         NI    BUCKFLGS,X'FF'-X'10' RESET                                       
*                                                                               
         BAS   RE,BLDBGRID                                                      
*                                  START DATE                                   
         TM    FLAGS,FGADDSPQ                                                   
         BO    UPBUY40                                                          
         ZIC   RE,PRVREPSP         BUY WILL BE SUBTRACTED THE SPOTS             
         ZIC   RF,PRVAGYSP                                                      
         SR    RE,RF                                                            
         STC   RE,BMSS#SPT                                                      
*                                                                               
* CHECK IF THERE IS ENOUGH SPOTS IN THIS BUY TO BE REMOVED. IF NOT,             
* REMOVE WHAT'S AVAILABLE AND REMOVE THE REMAINING SPOTS FROM THE REST          
* OF THE BUYS LINKED AFTER THIS BUY                                             
*                                                                               
         CLC   BMSS#SPT,1(R3)                                                   
         BNH   UPBUY38                                                          
         MVC   BMSS#SPT,1(R3)      MAX NUMBER OF SPOTS TO PROCESS               
*                                                                               
UPBUY38  DS    0H                                                               
         ZIC   RE,PRVREPSP         REDUCE TOTAL NUMBER OF SPOTS                 
         ZIC   RF,BMSS#SPT                                                      
         SR    RE,RF                                                            
         STC   RE,PRVREPSP                                                      
*                                                                               
UPBUY39  DS    0H                                                               
*        GOTOX (C'-',ADJSPTS)                                                   
         GOTO1 =A(ADJSPTS),DMCB,(C'-',0),RR=RELO                                
         GOTO1 =A(SPCHGCMT),DMCB,,RR=RELO                                       
         B     UPBUY50                                                          
*                                                                               
* FOR ADD TO SCHEDULE, THE BUY GRID WAS BUILD EARLIER, BUT THE SPOTS            
* FOR THE LAST WEEK HAS NOT BEEN ADD TO THE BUY GRID YET, SO DO IT HERE         
*                                                                               
* HOWEVER, IF THE ADD TO SCHEDULE IS ONLY A SINGLE WEEK THEN WE'LL NEED         
* TO INITIALIZE GRID START AND SEGMENT END HERE                                 
*                                                                               
UPBUY40  DS    0H                  PROCESS IF SINGLE WEEK                       
         OC    GSTRDATE,GSTRDATE                                                
         BNZ   UPBUY45                                                          
         GOTO1 DATCON,DMCB,(2,SEGSTRDT),(3,BSTRDATE)                            
         MVC   GSTRDATE,BSTRDATE                                                
         B     UPBUY48                                                          
*                                                                               
UPBUY45  DS    0H                  ADD FOR THE LAST WEEK IF MULTI WKS           
         GOTO1 DATCON,DMCB,(2,SEGENDDT),(3,BSTRDATE)                            
*                                                                               
UPBUY48  DS    0H                                                               
         ZIC   RE,PRVAGYSP                                                      
         ZIC   RF,PRVREPSP                                                      
         SR    RE,RF                                                            
         STC   RE,BMSS#SPT                                                      
         GOTO1 =A(ADJSPTS),DMCB,(C'+',0),RR=RELO                                
         B     UPBUY60                                                          
*                                                                               
UPBUY50  DS    0H                  LOOP THRU DATE RANGE TO BE PROCESSED         
         OC    SEGENDDT,SEGENDDT                                                
         BZ    UPBUY60                                                          
         GOTO1 DATCON,DMCB,(3,BSTRDATE),(0,WORK)                                
         LA    RF,7                                                             
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(RF)                                      
         GOTO1 DATCON,DMCB,(2,SEGENDDT),(0,WORK+12)                             
         CLC   WORK+6(6),WORK+12                                                
         BH    UPBUY60                                                          
         GOTO1 DATCON,DMCB,(0,WORK+6),(3,BSTRDATE)                              
         TM    FLAGS,FGADDSPQ                                                   
         BO    UPBUY40                                                          
         B     UPBUY39                                                          
*                                                                               
UPBUY60  DS    0H                                                               
         BAS   RE,REBLDBUY                                                      
*                                                                               
         TM    FLAGS,FGADDSPQ                                                   
         BZ    UPBUY70                                                          
         CLI   METHOD,CHGBUYQ                                                   
         BE    UPBUY70                                                          
*                                                                               
         GOTO1 =A(ADDBUY),RR=RELO                                               
         B     UPBUYX                                                           
*                                                                               
UPBUY70  DS    0H                                                               
         GOTO1 =A(ADORDCMT),RR=RELO                                             
         GOTO1 PUTREC                                                           
*                                                                               
UPBUY80  DS    0H                                                               
         MVI   BUCKFLAG,X'00'      SET TO ADD NEW FIGURES                       
*                                  ADD NEW BUCKETS                              
         GOTO1 =A(BUCKUPDT),RR=RELO                                             
*                                                                               
         TM    FLAGS,FGADDSPQ      IF WE ARE ADDING SPOTS, ONLY                 
         BO    UPBUYX              THE FIRST BUY GETS ALL THE ADDITION          
*                                                                               
         CLC   PRVAGYSP,PRVREPSP   DETERMINE WHETHER WE ARE DONE                
         BE    UPBUYX              REMOVING SPOTS                               
         BL    *+6                                                              
         DC    H'0'                REMOVED TOO MUCH, WHAT HAPPENED??            
*                                                                               
         AHI   R3,2                BUMP TO NEXT BUY AND SPOTS                   
         CLI   0(R3),0                                                          
         BE    UPBUYX                                                           
*                                                                               
                                                                                
* REESTABLISH KEY SEQUENCE BEFORE PROCEEDING                                    
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(27),SVBUYKEY                                                 
         GOTO1 HIGH                                                             
         B     UPBUY28                                                          
*                                                                               
UPBUYX   DS    0H                                                               
         MVC   TXNUM,UPSVTR#                                                    
         GOTO1 GOTSAR,TSAGET       RESTORE                                      
         BE    EXIT                                                             
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
*                                                                               
* BUILD GRID OF AVALIABLE SPOTS TO BE REMOVED/ADDED                             
* POPULATE GRID BY SEEDING WITH X'03' ELEMENT                                   
* REMOVE RESERVED SPOTS IN X'06', X'07' AND X'66' ELEMENTS                      
*                                                                               
***********************************************************************         
BLDBGRID NTR1                                                                   
*                                                                               
         L     R6,AIO2                                                          
         MVI   ELCODE,X'03'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RBUYDTEL,R6                                                      
*                                                                               
         MVC   GSTRDATE,RBUYDTST   GET BUY START DATE                           
         XC    BUYGRID,BUYGRID                                                  
         LA    R4,BUYGRID          POINT TO START OF GRID                       
         B     BBGRID20            SKIP OFFSET FOR FIRST DATES                  
*                                                                               
BBGRID10 DS    0H                                                               
         LA    R4,BUYGRID          RESET TO START OF GRID                       
         XC    ELEM,ELEM                                                        
         XC    WORK2,WORK2                                                      
         GOTO1 DATCON,DMCB,(3,GSTRDATE),(5,ELEM)                                
         GOTO1 DATCON,DMCB,(3,RBUYDTST),(5,ELEM+9)                              
         MVI   ELEM+8,C'-'                                                      
         GOTO1 PERVAL,DMCB,(17,ELEM),WORK2                                      
         CLI   DMCB+4,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
WKD      USING PERVALD,WORK2                                                    
         ZICM  R1,WKD.PVALNWKS,2   # WEEKS ROUNDED UP                           
         BCTR  R1,0                ADJUST FOR ROUNDING                          
         DROP  WKD                                                              
*                                                                               
         SLL   R1,1                MULTIPLY BY 2                                
         AR    R4,R1               BUMP TO STARTING CELL FOR THIS DATE          
*                                                                               
BBGRID20 DS    0H                                                               
         ZIC   R1,RBUYDTWK                                                      
*                                                                               
BBGRID30 DS    0H                                                               
         MVC   0(1,R4),RBUYDTNW                                                 
*                                                                               
         CLI   RBUYDTNW,0          IF ZERO SPOT, FORCE OVERRIDE                 
         BE    BBGRID35                                                         
         TM    RBUYDTIN,X'01'      NPW OVERRIDE??                               
         BZ    BBGRID38                                                         
*                                                                               
BBGRID35 DS    0H                                                               
         OI    1(R4),X'01'                                                      
*                                                                               
BBGRID38 DS    0H                                                               
         AHI   R4,2                BUMP TO NEXT WEEK                            
         TM    RBUYDTIN,X'40'      ALTERNATE WEEKS??                            
         BZ    *+8                                                              
         AHI   R4,2                YES, BUMP ONE MORE                           
         LTR   R1,R1                                                            
         BZ    BBGRID40                                                         
         BCT   R1,BBGRID30         PROCESS FOR SPECIFIED NUMBER OF WKS          
*                                                                               
BBGRID40 DS    0H                  INCASE OF MULTIPLE EFFECTIVE DATES           
         BRAS  RE,NEXTEL                                                        
         BE    BBGRID10                                                         
         DROP  R6                                                               
*                                                                               
* REMOVE RESERVED SPOTS FROM GRID                                               
*                                                                               
         L     R6,AIO2             CHECK OLD STYLE MAKEGOOD                     
         MVI   ELCODE,X'06'                                                     
         BRAS  RE,GETEL                                                         
         BNE   BBGRID60                                                         
         USING RBUYMSEL,R6                                                      
BBGRID50 MVC   BSTRDATE,RBUYMSDT                                                
         MVC   BMSS#SPT,RBUYMSSP                                                
         DROP  R6                                                               
*                                                                               
*        GOTOX (C'-',ADJSPTS)                                                   
         GOTO1 =A(ADJSPTS),DMCB,(C'-',0),RR=RELO                                
         BZ    *+6                                                              
         DC    H'0'                SHOULD NOT HAPPEN!                           
         BRAS  RE,NEXTEL                                                        
         BE    BBGRID50                                                         
*                                                                               
BBGRID60 DS    0H                  CHECK CREDIT                                 
         L     R6,AIO2                                                          
         MVI   ELCODE,X'07'                                                     
         BRAS  RE,GETEL                                                         
         BNE   BBGRID80                                                         
         USING RBUYCREL,R6                                                      
BBGRID70 MVC   BSTRDATE,RBUYCRDT                                                
         MVC   BMSS#SPT,RBUYCRSP                                                
         DROP  R6                                                               
*                                                                               
*        GOTOX (C'-',ADJSPTS)                                                   
         GOTO1 =A(ADJSPTS),DMCB,(C'-',0),RR=RELO                                
         BZ    *+6                                                              
         DC    H'0'                SHOULD NOT HAPPEN!                           
         BRAS  RE,NEXTEL                                                        
         BE    BBGRID70                                                         
*                                                                               
BBGRID80 DS    0H                  CHECK MAKEGOOD OFFERS                        
*&&DO                                                                           
         L     R6,AIO2                                                          
         MVI   ELCODE,X'66'                                                     
         BRAS  RE,GETEL                                                         
         BNE   BBGRIDX                                                          
         USING RBMGMSEL,R6                                                      
BBGRID90 MVC   BSTRDATE,RBMGMSDT                                                
         MVC   BMSS#SPT,RBMGMSSP                                                
         DROP  R6                                                               
*                                                                               
*        GOTOX (C'-',ADJSPTS)                                                   
         GOTO1 =A(ADJSPTS),DMCB,(C'-',0),RR=RELO                                
         BZ    *+6                                                              
         DC    H'0'                SHOULD NOT HAPPEN!                           
         BRAS  RE,NEXTEL                                                        
         BE    BBGRID90                                                         
*&&                                                                             
*                                                                               
BBGRIDX  DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* REBUILD X'03' ELEMENT FROM BUY GRID                                           
* BEWARE: R8 IS USED HERE, SO NO SPOOLING FUNCTIONALITIES ARE ALLOWED           
***********************************************************************         
REBLDBUY NTR1                                                                   
         LA    R8,BUYGRID          ADDRESS OF BUY GRID TO BE REBUILD            
*                                                                               
         L     R6,AIO2                                                          
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RBLD10   DS    0H                  POINT TO LAST X'02' ELEMENT TO GET           
         LR    R2,R6               END DAY                                      
         BRAS  RE,NEXTEL                                                        
         BE    RBLD10                                                           
         LR    R6,R2                                                            
         USING RBUYDYEL,R6                                                      
         MVI   MSENDDAY,0     GET END DAY                                       
         MVN   MSENDDAY,RBUYDYIN                                                
         DROP  R6                                                               
*                                                                               
         L     R6,AIO2                                                          
         MVI   ELCODE,X'03'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RBUYDTEL,R6         SAVE OFF BUY START DATE                      
         GOTO1 DATCON,DMCB,(3,RBUYDTST),(0,MSSTDT)                              
*                                                                               
* IF WE ARE DOING ADDITION TO SCHEDULE, THE START DATE IS ACTUALLY THE          
* GRID'S START DATE AND NOT THE BUY'S ORIGINAL START DATE                       
*                                                                               
         TM    FLAGS,FGADDSPQ                                                   
         BZ    RBLD15                                                           
         CLI   METHOD,CHGBUYQ                                                   
         BE    RBLD15                                                           
         GOTO1 DATCON,DMCB,(3,GSTRDATE),(0,MSSTDT)                              
         DROP  R6                                                               
*                                                                               
* DELETE OLD X'03'S                                                             
*                                                                               
RBLD15   DS    0H                                                               
         MVI   ELCODE,X'03'                                                     
         GOTO1 REMELEM                                                          
*                                                                               
         SR    R3,R3                                                            
*                                                                               
         L     R6,AIO2                                                          
         USING RBUYREC,R6                                                       
         CLC   RBUYVER,VERDFLT                                                  
         BL    RBLD30                                                           
         CLI   RBUYCHGI,C'*'                                                    
         BE    RBLD40                                                           
         OC    RBUYCHGI,SPACES                                                  
         CLI   RBUYCHGI,C'S'                                                    
         BE    RBLD35                                                           
         CLI   RBUYCHGI,C' '                                                    
         BE    RBLD30                                                           
         CLI   RBUYCHGI+1,C' '                                                  
         BNE   RBLD20                                                           
         MVI   RBUYCHGI+1,C'S'                                                  
         B     RBLD35                                                           
*                                                                               
RBLD20   DS    0H                                                               
         MVC   RBUYCHGI,=C'* '                                                  
         B     RBLD40                                                           
*                                                                               
RBLD30   DS    0H                                                               
         MVC   RBUYVER,VERDFLT     STORE REP VERSION NO. IN BUY                 
         MVC   RBUYCHGI,=C'S '                                                  
*                                                                               
RBLD35   DS    0H                  NEW BUY ORDER COMMENTS ADDED?                
         OC    CMTSTRDT,CMTSTRDT                                                
         BZ    RBLD40                                                           
         CLI   RBUYCHGI+1,C' '                                                  
         BNE   RBLD38                                                           
         MVI   RBUYCHGI+1,C'O'                                                  
         B     RBLD40                                                           
*                                                                               
RBLD38   DS    0H                                                               
         MVC   RBUYCHGI(2),=C'* '                                               
*                                                                               
RBLD40   DS    0H                                                               
         XC    RBUYTSPT,RBUYTSPT   CLEAR AND RECALCULATE TOTALS                 
         XC    RBUYTCOS,RBUYTCOS                                                
         XC    RBUYTWKS,RBUYTWKS                                                
*                                                                               
RBLD50   DS    0H                                                               
         LA    R4,1                                                             
         XC    ELEM,ELEM                                                        
WKD      USING RBUYDTEL,ELEM                                                    
*                                                                               
         MVI   WKD.RBUYDTCD,X'03'                                               
         MVI   WKD.RBUYDTLN,11                                                  
*                                                                               
RBLD60   DS    0H                                                               
         CLI   0(R8),0                                                          
         BNE   RBLD70                                                           
         TM    1(R8),X'01'         OVERRIDE??                                   
         BZ    RBLD160                                                          
*                                                                               
RBLD70   DS    0H                                                               
         OC    WKD.RBUYDTST,WKD.RBUYDTST                                        
         BNZ   RBLD90              NEED TO FIND START DATE                      
         LTR   R3,R3                                                            
         BNZ   RBLD80                                                           
         MVC   WKD.RBUYDTST,MSSTDT                                              
         MVC   MSSTDT2,MSSTDT                                                   
         GOTO1 DATCON,DMCB,(0,MSSTDT),(3,WKD.RBUYDTST)                          
         B     RBLD90                                                           
*                                                                               
RBLD80   DS    0H                  SUBSEQUENT X'03'S NEED TO COMPUTE            
         LR    R2,R3               START DATES RELATIVE TO INITIAL              
         MHI   R2,7                FLIGHT START FOR THIS BUY                    
         GOTO1 ADDAY,DMCB,MSSTDT,MSSTDT2,(R2)                                   
         GOTO1 DATCON,DMCB,(0,MSSTDT2),(3,WKD.RBUYDTST)                         
*                                                                               
RBLD90   DS    0H                                                               
         CLC   0(1,R8),2(R8)                                                    
         BNE   RBLD110                                                          
         CLI   0(R8),0             INCASE ALL SPOTS CREDITED OUT FOR            
         BNE   RBLD100             THIS WEEK, CHECK IF ZERO SPOT                
         TM    3(R8),X'01'         OVERRIDE FLAG IS SET                         
         BZ    RBLD110                                                          
*                                                                               
RBLD100  DS    0H                                                               
         AHI   R8,2                                                             
         AHI   R4,1                                                             
         AHI   R3,1                                                             
         CHI   R3,53                                                            
         BL    RBLD60              BUMP TO NEXT CELL                            
*                                                                               
RBLD110  DS    0H                                                               
         MVC   WKD.RBUYDTNW,0(R8)                                               
         STC   R4,WKD.RBUYDTWK                                                  
*** CHECK IF OVERRIDE OR ALTERNATE WEEKS                                        
         OI    WKD.RBUYDTIN,X'80'  DEFAULT IS WEEKLY                            
*        TM    1(R8),X'01'         OVERRIDE??                                   
*        BZ    *+8                                                              
*        OI    WKD.RBUYDTIN,X'01'                                               
*** CHECK IF OVERRIDE OR ALTERNATE WEEKS                                        
*** CHECK IF OVERRIDE OR ALTERNATE WEEKS                                        
         L     R2,AIO2                                                          
TBUYD    USING RBUYREC,R2                                                       
         CLC   TBUYD.RBUYNW,0(R8)                                               
         BE    RBLD120                                                          
         OI    WKD.RBUYDTIN,X'01'                                               
         DROP  TBUYD                                                            
***********                                                                     
***********                                                                     
*                                                                               
* CALCULATE END DATE                                                            
*                                                                               
RBLD120  DS    0H                                                               
         ZIC   R2,WKD.RBUYDTWK                                                  
         MHI   R2,7                                                             
         GOTO1 ADDAY,DMCB,MSSTDT2,MSSTDT2,(R2)                                  
*                                                                               
         GOTO1 GETDAY,DMCB,MSSTDT2,FULL                                         
         ZIC   RE,MSENDDAY                                                      
         ZIC   RF,DMCB                                                          
         SR    RE,RF                                                            
         BZ    RBLD140                                                          
         BP    RBLD130                                                          
*                                                                               
* HANDLE OUT OF WEEK ROTATIONS                                                  
*                                                                               
         LR    R2,RE                                                            
         B     RBLD150                                                          
*                                                                               
RBLD130  DS    0H                                                               
         LA    R2,7                                                             
         LNR   R2,R2                                                            
         AR    R2,RE                                                            
         B     RBLD150                                                          
*                                                                               
RBLD140  DS    0H                  SAME START END DAY, BACK UP A WEEK           
         LA    R2,7                                                             
         LNR   R2,R2                                                            
*                                                                               
RBLD150  DS    0H                                                               
         GOTO1 ADDAY,DMCB,MSSTDT2,MSSTDT2,(R2)                                  
         GOTO1 DATCON,DMCB,(0,MSSTDT2),(3,WKD.RBUYDTED)                         
*                                                                               
         SR    RE,RE               CALCULATE TOTAL SPOTS                        
         ZIC   RF,WKD.RBUYDTNW                                                  
         ZIC   R1,WKD.RBUYDTWK                                                  
         MR    RE,R1                                                            
         ZICM  R1,RBUYTSPT,2                                                    
         AR    R1,RF                                                            
         STCM  R1,3,RBUYTSPT                                                    
*                                                                               
         ZIC   RE,WKD.RBUYDTWK     CALCULATE TOTAL WEEKS                        
         ZIC   RF,RBUYTWKS                                                      
         AR    RE,RF                                                            
         STC   RE,RBUYTWKS                                                      
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
RBLD160  DS    0H                                                               
         AHI   R8,2                                                             
         LA    R3,1(R3)                                                         
         CH    R3,=H'53'                                                        
         BL    RBLD50              BUMP TO NEXT CELL                            
*                                                                               
         SR    RE,RE               CALCULATE TOTAL COST                         
         ZICM  RF,RBUYCOS,4                                                     
         ZICM  R1,RBUYTSPT,2                                                    
         MR    RE,R1                                                            
         STCM  RF,15,RBUYTCOS                                                   
*                                                                               
         B     EXIT                                                             
         DROP  WKD,R6                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INCASE OF ADDING DAILY BUYS, THERE MIGHT BE MULTIPLE BUYS WITH                
* THE SAME RBUYKMLN(AGENCY) BUY NUMBER. WE NEED TO CHECK EACH OF THESE          
* BUYS TO FIND THE ONE THAT MATCH THE CURRENT BUY SEGMENT TO BE ADDED           
***********************************************************************         
CHKDAILY NTR1  BASE=*,LABEL=*                                                   
*                                                                               
BUYSEG   USING TLSTD,APSVREC                                                    
*                                                                               
         L     R6,AIO2                                                          
         MVI   ELCODE,X'03'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RBUYDTEL,R6                                                      
*                                                                               
         GOTO1 DATCON,DMCB,(2,BUYSEG.TLBYWKOF),(3,WORK)                         
         CLC   RBUYDTST,WORK       MUST BE DAILY!!                              
         BNE   CHKDYNO                                                          
         CLC   RBUYDTED,WORK                                                    
         BNE   CHKDYNO                                                          
*                                                                               
CHKDYYES SR    RC,RC                                                            
CHKDYNO  LTR   RC,RC                                                            
         XIT1                                                                   
         DROP  BUYSEG,R6                                                        
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ADD/REMOVE SPOTS FROM GRID                                                    
* RETURNS # OF SPOTS BEFORE CHANGE                                              
***********************************************************************         
ADJSPTS  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   MYWRK(1),0(R1)                                                   
         LA    R2,BUYGRID          RESET TO START OF GRID                       
         XC    ELEM,ELEM                                                        
         XC    WORK2,WORK2                                                      
         GOTO1 DATCON,DMCB,(3,GSTRDATE),(5,ELEM)                                
         GOTO1 DATCON,DMCB,(3,BSTRDATE),(5,ELEM+9)                              
         MVI   ELEM+8,C'-'                                                      
         GOTO1 PERVAL,DMCB,(17,ELEM),WORK2                                      
         CLI   DMCB+4,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
WKD      USING PERVALD,WORK2                                                    
         ZICM  R1,WKD.PVALNWKS,2   # WEEKS ROUNDED UP                           
         BCTR  R1,0                ADJUST FOR ROUNDING                          
         DROP  WKD                                                              
*                                                                               
         SLL   R1,1                MULTIPLY BY 2                                
         AR    R2,R1               BUMP TO STARTING CELL FOR THIS DATE          
*                                                                               
         MVC   DMCB(1),0(R2)       RETURN # SPOT BEFORE CHANGE                  
*                                                                               
         CLI   MYWRK,C'-'          ADDING TO GRID?                              
         BE    ASPT10                                                           
         ZIC   RF,BMSS#SPT                                                      
         ZIC   RE,0(R2)                                                         
         AR    RE,RF                                                            
         STC   RE,0(R2)                                                         
         B     ASPTSYES                                                         
*                                                                               
ASPT10   DS    0H                  REMOVING FROM GRID                           
         CLC   BMSS#SPT,0(R2)      SPTS MUST BE AVAILABLE TO BE REMOVED         
         BH    ASPTSNO                                                          
         ZIC   RF,BMSS#SPT                                                      
         ZIC   RE,0(R2)                                                         
         SR    RE,RF                                                            
         STC   RE,0(R2)                                                         
         OI    1(R2),X'01'         SET NPW OVERRIDE INCASE NO MORE SPOT         
*                                                                               
ASPTSYES SR    RC,RC                                                            
ASPTSNO  LTR   RC,RC                                                            
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BUILD SPOT CHANGED COMMENT                                                    
***********************************************************************         
SPCHGCMT NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ZIC   R3,DMCB                                                          
         OC    CMTSTRDT,CMTSTRDT                                                
         BNZ   SPCMT05                                                          
         MVC   CMTSTRDT,BSTRDATE                                                
         MVC   CMTENDDT,CMTSTRDT                                                
         MVI   CMTNUMWK,1                                                       
         STC   R3,CMTNUMSP                                                      
         B     SPCMTX                                                           
*                                                                               
SPCMT05  DS    0H                                                               
         ZIC   RE,CMTNUMSP                                                      
         CR    R3,RE                                                            
         BNE   SPCMT08                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(3,CMTENDDT),(0,WORK)                                
         LA    RF,7                                                             
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(RF)                                      
         GOTO1 DATCON,DMCB,(3,BSTRDATE),(0,WORK)                                
         CLC   WORK(6),WORK+6                                                   
         BNE   SPCMT08                                                          
         MVC   CMTENDDT,BSTRDATE                                                
         ZIC   RE,CMTNUMWK                                                      
         AHI   RE,1                                                             
         STC   RE,CMTNUMWK                                                      
         B     SPCMTX                                                           
*                                                                               
SPCMT08  DS    0H                                                               
         LA    R2,MYELEM                                                        
         LA    R1,120              MAX 2 ORD COMMENTS                           
*                                                                               
SPCMT10  DS    0H                                                               
         CLI   0(R2),0                                                          
         BE    SPCMT20                                                          
         AHI   R2,1                                                             
         BCT   R1,SPCMT10                                                       
         B     SPCMTX                                                           
*                                                                               
SPCMT20  DS    0H                                                               
         GOTO1 DATCON,DMCB,(3,CMTSTRDT),(4,(R2))                                
         AHI   R2,5                                                             
         CLI   CMTNUMWK,1                                                       
         BNH   SPCMT30                                                          
         MVI   0(R2),C'-'                                                       
         AHI   R2,1                                                             
         EDIT  CMTNUMWK,(3,0(R2)),ALIGN=LEFT                                    
         AR    R2,R0                                                            
         MVC   0(3,R2),=C'WKS'                                                  
         AHI   R2,3                                                             
*                                                                               
SPCMT30  DS    0H                                                               
         MVC   0(11,R2),=C' #SPTS WAS '                                         
         AHI   R2,11                                                            
*                                                                               
         EDIT  CMTNUMSP,(3,0(R2)),ALIGN=LEFT,ZERO=NOBLANK                       
         AR    R2,R0                                                            
         MVI   0(R2),C','                                                       
         AHI   R2,1                                                             
*                                                                               
         MVC   CMTSTRDT,BSTRDATE                                                
         MVC   CMTENDDT,CMTSTRDT                                                
         MVI   CMTNUMWK,1                                                       
         STC   R3,CMTNUMSP                                                      
*                                                                               
SPCMTX   DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ADD SPOT CHANGED COMMENT                                                      
***********************************************************************         
ADORDCMT NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*                                                                               
         OC    CMTSTRDT,CMTSTRDT                                                
         BZ    ORDCMTX                                                          
*                                                                               
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'B2',AIO2),0,0                   
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'B3',AIO2),0,0                   
*                                                                               
         XC    MYELEM,MYELEM                                                    
*                                                                               
         L     R6,AIO2             CHAIN AND SAVE OFF CURRENT COMMENTS          
         MVI   ELCODE,X'84'                                                     
         BRAS  RE,GETEL                                                         
         BNE   ADOC20                                                           
         ZIC   RF,1(R6)                                                         
         SHI   RF,4                OVERHEAD + EXECUTED MOVE                     
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   MYELEM(0),3(R6)                                                  
         LA    R4,MYELEM                                                        
         LA    R4,1(R4,RF)                                                      
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   ADOC05                                                           
         ZIC   RF,1(R6)                                                         
         SHI   RF,4                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R6)                                                    
         LA    R4,1(R4,RF)                                                      
*                                                                               
ADOC05   DS    0H                                                               
         MVI   0(R4),C','                                                       
         AHI   R4,1                                                             
*                                                                               
* REPLACE EXISTING BUY ORDER COMMENT                                            
ADOC10   DS    0H                                                               
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'84',AIO2),0,0                   
*                                                                               
ADOC20   DS    0H                                                               
         LA    R2,MYELEM                                                        
         LA    R1,120              MAX 2 ORD COMMENTS                           
*                                                                               
ADOC30   DS    0H                                                               
         CLI   0(R2),0                                                          
         BE    ADOC40                                                           
         AHI   R2,1                                                             
         BCT   R1,ADOC30                                                        
         B     ORDCMTX                                                          
*                                                                               
ADOC40   DS    0H                                                               
         GOTO1 DATCON,DMCB,(3,CMTSTRDT),(4,(R2))                                
         AHI   R2,5                                                             
         CLI   CMTNUMWK,1                                                       
         BNH   ADOC50                                                           
         MVI   0(R2),C'-'                                                       
         AHI   R2,1                                                             
         EDIT  CMTNUMWK,(3,0(R2)),ALIGN=LEFT                                    
         AR    R2,R0                                                            
         MVC   0(3,R2),=C'WKS'                                                  
         AHI   R2,3                                                             
*                                                                               
ADOC50   DS    0H                                                               
         MVC   0(11,R2),=C' #SPTS WAS '                                         
         AHI   R2,11                                                            
*                                                                               
         EDIT  CMTNUMSP,(3,0(R2)),ALIGN=LEFT,ZERO=NOBLANK                       
*                                  BUILD AND ADD CHANGE BUY ORD CMTS            
         XC    ELTBUILD,ELTBUILD                                                
         MVC   ELTBUILD+3(60),MYELEM                                            
ADOC60   LA    R1,60                                                            
         LA    R2,ELTBUILD+63                                                   
ADOC70   CLI   0(R2),0                                                          
         BE    ADOC80                                                           
         CLI   0(R2),C' '                                                       
         BNE   ADOC90                                                           
ADOC80   BCTR  R2,0                                                             
         BCT   R1,ADOC70                                                        
         B     ORDCMTX                                                          
*                                                                               
ADOC90   DS    0H                                                               
         CLI   0(R2),C','          LINE SHOULDN'T END IN A COMMA                
         BNE   *+6                                                              
         BCTR  R1,0                                                             
         LA    R1,4(R1)                                                         
         STC   R1,ELTBUILD+1                                                    
         MVI   ELTBUILD,X'84'                                                   
         MVI   ELTBUILD+2,X'80'    FLAG AS REP ORDER COMMENT                    
*                                  USE HELLO FOR FIRST LINE                     
         L     R6,AIO2                                                          
         MVI   ELCODE,X'84'                                                     
         BRAS  RE,GETEL                                                         
         BE    ADOC100                                                          
*                                                                               
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),AIO2,ELTBUILD,0                    
*                                                                               
         OC    MYELEM+60(60),SPACES                                             
         CLC   MYELEM+60(60),SPACES                                             
         BE    ORDCMTX                                                          
         L     R6,AIO2                                                          
         MVI   ELCODE,X'84'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    ELTBUILD,ELTBUILD                                                
         MVC   ELTBUILD+3(60),MYELEM+60                                         
         B     ADOC60              GO ADD SECOND ELEMENT                        
*                                                                               
ADOC100  DS    0H                  SECOND LINE MUST USE RECUP FOR               
         ZIC   R1,1(R6)            PROPER ORDERING                              
         AR    R6,R1                                                            
         GOTO1 VRECUP,DMCB,(2,AIO2),ELTBUILD,(R6),0                             
*                                                                               
ORDCMTX  DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*   BUCKUPDT: EITHER DELETE OR ADD BUCKET DOLLARS TO CONTRACT RECORD,           
*        BASED ON BUCKFLAG VALUE.                                               
***********************************************************************         
BUCKUPDT NTR1  BASE=*,WORK=(R2,500),LABEL=*                                     
         L     R4,AIO3             SET A(CONTRACT RECORD)                       
         USING RCONREC,R4                                                       
*                                                                               
         CLI   DAILYFLG,C'Y'       DAILY PACING?                                
         BNE   BUUP0010            NO                                           
         OI    BUCKFLGS,X'08'      YES - SET DAILY PACING CALL                  
*                                                                               
BUUP0010 EQU   *                                                                
*                                  SET ADDRESSES FOR REGENBUC                   
         MVC   DMCB,AIO            A(BUYREC)                                    
         MVC   DMCB(1),BUCKFLAG                                                 
         L     R0,VRECUP                                                        
         GOTOX (RFBUCKUP,REPFACS),DMCB,,(BUCKFLGS,RCONREC),            +        
               ACOMFACS,GETBROAD,(R0),(R2)                                      
         BNE   BUUP0100                                                         
         GOTO1 =A(TRUDATE),RR=RELO UPDATE TRUE ACTIVITY DATE                    
*                                                                               
         B     ABUCEXIT                                                         
*                                                                               
BUUP0100 EQU   *                                                                
         LA    R2,CONACTH                                                       
         L     R3,0(R1)                                                         
         GOTO1 GETTXT,DMCB+12,(R3),0,(C'E',DMCB),0,0,0                          
         DC    H'0',C'$ABEND'                                                   
*                                                                               
ABUCEXIT DS    0H                                                               
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* IO2 HAS NEW BUY TO ADD                                                        
***********************************************************************         
ADDBUY   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R6,AIO2                                                          
AGYBUY   USING RBUYREC,R6                                                       
*** TEST                                                                        
         TM    AGYBUY.RBUYCNTL,X'80'                                            
         BZ    *+6                                                              
         DC    H'0'                SHOULDN'T BE SET                             
*** TEST                                                                        
         XC    KEY,KEY                                                          
         MVI   BUYNUM,0                                                         
         MVC   KEY(RBUYKPLN-RBUYKEY),AGYBUY.RBUYKEY                             
         OI    DMINBTS,X'08'       RETURN DELETED KEYS                          
         GOTO1 HIGH                                                             
ADDB10   CLC   KEY(RBUYKPLN-RBUYKEY),KEYSAVE                                    
         BNE   ADDB20                                                           
         CLI   KEY+26,255          PLANREC?                                     
         BE    ADDB15                                                           
         CLC   BUYNUM,KEY+26                                                    
         BNL   *+10                                                             
         MVC   BUYNUM,KEY+26       HIGHEST LINE NUMBER SO FAR                   
ADDB15   OI    DMINBTS,X'08'       RETURN DELETED KEYS                          
         GOTO1 SEQ                                                              
         B     ADDB10                                                           
*                                                                               
ADDB20   DS    0H                  ASSIGN A NEW BUYLINE #                       
         CLI   BUYNUM,X'FF'                                                     
         BE    MAXBUYER                                                         
         ZIC   RF,BUYNUM                                                        
         LA    RF,1(RF)                                                         
         STC   RF,AGYBUY.RBUYKMLN                                               
         STC   RF,AGYBUY.RBUYKLIN                                               
         DROP  AGYBUY                                                           
*                                                                               
         L     R6,AIO3                                                          
         USING RCONSEND,R6                                                      
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    RCONSENF,X'20'      ON MEANS VERSION NOT ADVANCED                
         BZ    ADDB30                                                           
*                                                                               
         TM    MISCFLAG,MFCONOK                                                 
         BO    ADDB30                                                           
*                                                                               
* ADVANCE REP VERSION AND UPDATE VERSION DATES                                  
*                                                                               
         MVC   WORK(4),HELLO                                                    
         MVC   WORK+4(4),DATCON                                                 
*        GOTO1 VREGENVR,DMCB,(C'R',AIO3),WORK                                   
         GOTOX (RFGENVER,REPFACS),DMCB,(C'R',AIO3),WORK                         
         BNZ   INVLFLD                                                          
         GOTOR DELCFC              DELETE PARTIAL CONFIRM COMMENT               
         L     R6,AIO3                                                          
         USING RCONSEND,R6                                                      
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
ADDB30   DS    0H                                                               
         L     R4,AIO2                                                          
         USING RBUYREC,R4                                                       
         MVC   RBUYVER,RCONSRV     STORE REP VERSION NO. IN BUY                 
         MVI   RBUYCHGI,C'A'                                                    
         MVI   RBUYCHGI+1,0                                                     
         MVC   RBUYKMOD,CONMOD#                                                 
         TM    MISCFLG2,MF2TRADE   TRADE ORDER?                                 
         BZ    *+8                                                              
         OI    RBUYFLG2,X'02'      FLAG TRADE                                   
         DROP  R4,R6                                                            
*                                                                               
         MVC   AIO,AIO2                                                         
         L     R6,AIO                                                           
         AHI   R6,RBUYELEM-RBUYREC                                              
*                                                                               
* REMOVE ELEMENTS NOT PERTAINING TO THIS NEW BUY                                
* (EXAMPLE, X'56', X'66' ,ETC)                                                  
*                                                                               
ADDB40   DS    0H                                                               
         LA    R3,ELMLIST                                                       
         LA    R2,ELMLISTQ                                                      
*                                                                               
ADDB50   DS    0H                                                               
         CLC   0(1,R6),0(R3)                                                    
         BE    ADDB70                                                           
         AHI   R3,1                                                             
         CLI   0(R3),0                                                          
         BE    ADDB60                                                           
         BCT   R2,ADDB50                                                        
*                                                                               
ADDB60   DS    0H                                                               
         LR    R3,R6                                                            
         MVC   ELCODE,0(R6)                                                     
         GOTO1 REMELEM                                                          
         LR    R6,R3               RESTORE POINTER TO RECORD                    
         B     ADDB73                                                           
*                                                                               
ADDB70   DS    0H                                                               
         ZIC   RF,1(R6)                                                         
         AR    R6,RF                                                            
*                                                                               
ADDB73   DS    0H                                                               
         CLI   0(R6),0                                                          
         BNE   ADDB40                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    ADDB74              ELEMENT ALREADY EXIST                        
*                                                                               
         L     R6,AIO3                                                          
         USING RCONREC,R6                                                       
         XC    ELEM,ELEM                                                        
WKD      USING RBUYXXEL,ELEM                                                    
         MVI   WKD.RBUYXXCD,RBUYXXCQ                                            
         MVI   WKD.RBUYXXLN,RBUYXXLQ                                            
         MVC   WKD.RBUYXXMD,RCONMOD    CONTRACT MOD# AT BUY CREATION            
         MVC   WKD.RBUYXXVR,VERDFLT    CONTRACT VER# AT BUY CREATION            
         DROP  R6,WKD                                                           
*                                                                               
         GOTO1 ADDELEM                                                          
         B     ADDB80                                                           
*                                                                               
ADDB74   DS    0H                                                               
WKD      USING RBUYXXEL,R6                                                      
         L     RF,AIO3                                                          
         USING RCONREC,RF                                                       
         MVC   WKD.RBUYXXMD,RCONMOD                                             
         MVC   WKD.RBUYXXVR,VERDFLT                                             
         DROP  WKD                                                              
         DROP  RF                                                               
*                                                                               
ADDB80   DS    0H                                                               
*                                                                               
* REGENVER MARKS CONTRACT GOING TO MANUAL PROCESSING. WE NEED TO RESET          
* IT BACK TO AUTOMATIC                                                          
*                                                                               
         L     R6,AIO3                                                          
         MVI   ELCODE,X'1D'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONDREL,R6                                                      
         NI    RCONDRF2,X'FF'-X'04'                                             
         DROP  R6                                                               
*                                                                               
         XC    ELTBUILD,ELTBUILD                                                
ELM      USING RBUYOCEL,ELTBUILD                                                
         MVI   ELM.RBUYOCCD,X'84'                                               
         MVI   ELM.RBUYOCLN,23                                                  
         MVI   ELM.RBUYOCID,X'80'                                               
         MVC   ELM.RBUYOCNT(20),=C'ADDITION TO SCHEDULE'                        
         DROP  ELM                                                              
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'84',AIO2),0,0                   
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),AIO2,ELTBUILD,0                    
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 ADDREC                                                           
         MVI   BUCKFLAG,X'00'      SET TO ADD NEW FIGURES                       
*                                  ADD NEW BUCKETS                              
         GOTO1 =A(BUCKUPDT),RR=RELO                                             
*                                                                               
ADDBX    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* LIST OF ELEMENTS TO RETAIN FOR NEW BUY                                        
*                                                                               
ELMLIST  DC    X'01',X'02',X'03',X'04',X'08',X'0D',X'10',X'21',X'84'            
         DC    X'00'                                                            
ELMLISTQ EQU   *-ELMLIST                                                        
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*  TRUDATE:  FOR CONTRACTS WHERE THE BUYLINES HAVE RESULTED IN BUCKET           
*     CHANGES, FIND (ADD IF NOT PRESENT) THE X'08' ELEMENT, AND                 
*     UPDATE THE TRUE ACTIVITY DATE FOR SAR REPORTING                           
***********************************************************************         
TRUDATE  NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIO3                                                          
         USING RCONREC,R4                                                       
*                                                                               
         LA    R2,RCONELEM         A(1ST ELEMENT)                               
TDAT0010 EQU   *                                                                
         ZIC   RF,1(R2)            BUMP TO NEXT ELEMENT                         
         AR    R2,RF                                                            
         CLI   0(R2),0             END OF RECORD?                               
         BE    TDAT0030            YES - NO X'08' FOUND - ADD IT                
         CLI   0(R2),X'08'         X'08'?                                       
         BNE   TDAT0010            NO  - GO BACK FOR NEXT                       
TDAT0020 EQU   *                   YES - ADD TODAYS DATE                        
         USING RCONACEL,R2                                                      
         GOTO1 DATCON,DMCB,(5,RCONACTA),(3,RCONACTA)                            
*                                  TODAY'S DATE INTO TRUE ACT DATE              
         DROP  R2                                                               
*                                                                               
         B     TDAT0040            FINISHED                                     
TDAT0030 EQU   *                                                                
         XC    ELEM,ELEM                                                        
         MVC   ELEM(2),=X'080C'                                                 
         GOTO1 DATCON,DMCB,(5,RCONDATE),(3,ELEM+5)                              
*                                  TODAY'S DATE INTO TRUE ACT DATE              
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),RCONREC,ELEM,=C'ADD=CODE'          
TDAT0040 EQU   *                                                                
         MVI   TRUFLAG,C'Y'        SET 'NEED EC KEY' FLAG                       
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*   GEN8DEKY:  DELETE OLD 8D/8E KEY AND ADD NEW ONES                            
*                                                                               
* CONFLTDT = OLD/ORIGINAL CONTRACT FLIGHT DATES                                 
* FLTDATES = NEW/DARE ORDER FLIGHT DATES                                        
***********************************************************************         
GEN8DEKY NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R6,AIO              SAVE OFF STATION AND CON# BEFORE             
         USING RCONREC,R6          AIO GETS USED BY DATAMGR                     
*                                                                               
         GOTO1 DATCON,DMCB,(3,RCONDATE),(2,NEWFLTDT)                            
         GOTO1 DATCON,(R1),(3,RCONDATE+3),(2,NEWFLTDT+2)                        
         CLC   CONFLTDT,NEWFLTDT   NO CHANGES, SHOULDN'T BE IN HERE             
         BE    GENKEYX                                                          
*                                                                               
         MVC   WORK(5),RCONKSTA                                                 
         MVC   WORK+5(4),RCONKCON                                               
         DROP  R6                                                               
*                                                                               
* DELETE OLD SET OF 8D/8E RIS KEYS FIRST                                        
*                                                                               
         XC    KEY,KEY                                                          
KYD      USING RCONKEY,KEY                                                      
         MVI   KYD.RCON8TYP,X'8D'  INSERT KEY ID                                
*                                                                               
GENKEY10 DS    0H                                                               
         XC    BLOCK,BLOCK                                                      
         LA    R4,BLOCK                                                         
*                                                                               
         MVC   KYD.RCON8REP,AGENCY                                              
         MVC   KYD.RCON8FST(4),CONFLTDT                                         
         MVC   KYD.RCON8CON,WORK+5                                              
         LA    R3,1                                                             
         STC   R3,KYD.RCON8RID                                                  
         NI    DMINBTS,X'FF'-X'08' TURN OFF 'RETURN DELETES'                    
         GOTO1 HIGH                READ HIGH AND GET X'01' KEY                  
*                                  READ ACTIVE KEYS ONLY!                       
GENKEY20 DS    0H                                                               
         MVC   0(32,R4),KEY        SAVE OFF KEY                                 
*                                                                               
         CLC   KEY(16),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                DIE FOR NOW                                  
*                                                                               
         ZIC   R1,KYD.RCON8RID     SEQUENCE MUST MATCH                          
         CR    R3,R1                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    KEY+27,X'80'                                                     
         GOTO1 WRITE                                                            
*                                                                               
         GOTO1 SEQ                                                              
         AHI   R3,1                                                             
         LA    R4,32(R4)                                                        
         CHI   R3,4                                                             
         BL    GENKEY20                                                         
*                                                                               
* ADD NEW SET OF 8D/8E RIS KEYS WITH NEW FLIGHT DATES                           
*                                                                               
GENKEY30 DS    0H                                                               
*                                                                               
         LA    R3,1                                                             
         LA    R4,BLOCK                                                         
*                                                                               
GENKEY40 DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(32),0(R4)                                                    
*                                  ADD NEW KEYS WITH NEW FLIGHT DATES           
         MVC   KYD.RCON8FST(4),NEWFLTDT                                         
         STC   R3,KYD.RCON8RID                                                  
         OI    DMINBTS,X'08'       RETURN DELETED KEY                           
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     CHECK IF NEW KEY EXISTS                      
         BNE   GENKEY50                                                         
         MVI   KEY+27,0            YES, RESTORE IT                              
         GOTO1 WRITE                                                            
         B     GENKEY60                                                         
*                                                                               
GENKEY50 DS    0H                                                               
         MVC   KEY,KEYSAVE                                                      
         MVI   KEY+27,0                                                         
         GOTO1 ADD                                                              
*                                                                               
GENKEY60 DS    0H                                                               
         AHI   R3,1                                                             
         LA    R4,32(R4)                                                        
         CHI   R3,4                                                             
         BL    GENKEY40                                                         
*                                                                               
         CLI   KEYSAVE,X'8E'       HAVE DONE PROCESSING 8D AND 8E KEYS?         
         BE    GENKEYX             YES, EXIT                                    
         XC    KEY,KEY             NO, SET TO PROCESS 8E KEYS                   
         MVI   KYD.RCON8TYP,X'8E'  INSERT KEY ID                                
         MVC   KYD.RCON8EST,WORK                                                
         B     GENKEY10                                                         
*                                                                               
GENKEYX  EQU   *                                                                
         B     EXIT                                                             
         DROP  KYD                                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PRE APPROVAL PROCESSING:                                                      
* - SAVE OFF HEADER INFORMATION                                                 
***********************************************************************         
PREAPRV  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   KEY(L'SELECTKY),SELECTKY                                         
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                SHOULD NEVER HAPPEN!                         
*                                                                               
         MVC   AIO,AIO1                                                         
*                                                                               
         GOTO1 GETREC                                                           
                                                                                
         L     R4,AIO                                                           
         USING RDARREC,R4                                                       
*                                                                               
         NI    MISCFLG2,X'FF'-MF2TRADE                                          
         CLI   RDARCORT,C'T'                                                    
         BNE   *+8                                                              
         OI    MISCFLG2,MF2TRADE                                                
*                                                                               
         MVI   KATZEDI,C'N'        SET KATZ EDI ORDER TO 'NO'                   
         CLC   =C'$EDI$',RDARAGAD  SCAN FOR KATZ EDI SPECIAL                    
         BNE   *+8                                                              
         MVI   KATZEDI,C'Y'                                                     
*                                                                               
         MVI   RESENT,C'N'         SET RESENT FLAG TO 'NO'                      
         TM    RDARMISC,X'80'      ORDER RESENT?                                
         BNO   PREA10              NO                                           
         MVI   RESENT,C'Y'         YES - NEED VERSION BUMP                      
PREA10   DS    0H                                                               
         TM    RDARMISC,X'20'      IS ORDER 'NOTDARE'?                          
         BNO   PREA20              NO                                           
         LA    R2,CONACTH          SET CURSOR TO 'ACTION' FIELD                 
         MVC   RERROR,=AL2(451)    SET ERROR MESSAGE                            
         GOTO1 MYERROR             EXIT WITH ERROR                              
PREA20   DS    0H                                                               
         LA    R2,DIFHDLNH         ANY CONTRACT NUMBER?                         
         CLI   5(R2),0                                                          
         BNE   PREA60              YES - CONTINUE                               
         OC    RDARREP#,RDARREP#   IS ORDER LINKED?                             
         BNZ   PREA30              YES                                          
         MVC   RERROR,=AL2(435)    NO  - UNLINKED: ILLEGAL APPROVAL             
         B     PREAERR                                                          
PREA30   DS    0H                                                               
         GOTO1 HEXOUT,DMCB,RDARREP#,WORK,4,=C'TOG'                              
*                                  GET REP CONTRACT NUMBER                      
         LA    R3,WORK                                                          
         LA    RF,8                LOOP CONTROL                                 
PREA40   DS    0H                                                               
         CLI   0(R3),C'0'          LEADING ZERO?                                
         BNE   PREA50              NO  - MOVE IT                                
         LA    R3,1(R3)            YES - BUMP TO NEXT POSITION                  
         BCT   RF,PREA40           GO BACK FOR NEXT                             
         DC    H'0'                SHOULDN'T HAPPEN                             
PREA50   DS    0H                                                               
         BCTR  RF,0                DECREMENT FOR EX                             
         EX    RF,*+8              MOVE BY LENGTH TO SCREEN                     
         B     *+10                                                             
         MVC   8(0,R2),0(R3)                                                    
PREA60   DS    0H                                                               
         TM    CCONFLAG,CCONFSWP   CANNOT APPROVE IF CONTRACT IS IN             
         BZ    PREA70              STATION'S WORK-IN-PROGRESS                   
         MVC   RERROR,=AL2(656)    SET ERROR TYPE = STATION IN WIP              
         B     PREAERR                                                          
*                                                                               
PREA70   DS    0H                                                               
         TM    RDARMISC,X'20'      ORDER NOTDARED FROM AGENCY SIDE?             
         BZ    PREA80                                                           
         MVI   RMSGTYPE,C'E'                                                    
         LA    R2,CONSERVH         SET A(SERVICE REQUEST)                       
         MVC   RERROR,=AL2(450)    SET ERROR TYPE = ONLY ACTION REJECT          
*                                     IS ALLOWED FOR NOTDARE ORDER              
         B     PREAERR                                                          
                                                                                
PREA80   DS    0H                                                               
*                                                                               
         CLI   RDARBSTS,0          AGY ORDER PRIOR STATUS?                      
         BE    PREA100             NO  - DO APPROVE OR REJECT                   
         CLI   RDARBSTS,C'A'       PRIOR STATUS = APPROVED?                     
         BE    PREAERR                                                          
PREA90   DS    0H                                                               
         MVI   RMSGTYPE,C'E'                                                    
         LA    R2,CONSERVH         SET A(SERVICE REQUEST)                       
         MVC   RERROR,=AL2(425)    SET ERROR TYPE = REJECTED:                   
*                                     THIS ACTION IGNORED                       
         B     PREAERR                                                          
*                                                                               
PREA100  DS    0H                                                               
*                                                                               
*   SAVE EASI CODES BEFORE ANYTHING ELSE.  AGENCY HEADER RECORD                 
*     CONTAINS X'01' AND X'02' ELEMENT, SO ALL LABELS APPLY AT                  
*     THIS TIME.                                                                
*                                                                               
         MVC   EASIADV(4),RDARCLI  INSERT CLIENT/ADV CODE                       
*                                     CHOPPED TO 4 CHARS                        
         MVC   EASIPROD,RDARPRD1   INSERT PRODUCT CODE                          
         MVC   EASIPRD2,RDARPRD2   INSERT PRODUCT CODE 2                        
*                                                                               
         MVC   SAVEPROD,RDARPRN1   SAVE PRODUCT NAME(S)                         
         CLC   =C'$EDI$',RDARAGAD  KATZ EDI ONLY HAS ONE PRODUCT                
         BE    PREA120                                                          
         CLC   RDARPRN2,SPACES                                                  
         BE    PREA120                                                          
         MVI   SAVEPROD+10,C'/'                                                 
         MVC   SAVEPROD+11(9),RDARPRN2                                          
                                                                                
PREA120  DS    0H                                                               
         EDIT  RDAREST#,(4,EASIEST#),FILL=0,ZERO=NOBLANK                        
*                                  INSERT ESTIMATE NUMBER, 3 CHARS              
         MVC   FLTDATES,RDARESST   SAVE FLIGHT START/END                        
         MVC   SAVEBUYR,RDARBUYR   SAVE BUYER NAME                              
*                                                                               
* SAVE OFF ESTIMATE NUMBER IN EBCDIC INCASE IT IS ALPHA-NUMERIC                 
*                                                                               
         XC    SAVEEST#,SAVEEST#                                                
         L     R6,AIO                                                           
         MVI   ELCODE,X'11'                                                     
         BRAS  RE,GETEL                                                         
         BNE   PREA125                                                          
         USING RDAREL2M,R6                                                      
         MVC   SAVEEST#,RDAR2EST                                                
         DROP  R6                                                               
*                                                                               
*   INSERT INFORMATION FROM KEY SECTION INTO RETURN MESSAGE                     
*      SAVE AREA                                                                
PREA125  DS    0H                                                               
*                                                                               
         GOTO1 HEXOUT,DMCB,RDARKORD,RETORD#,4,=C'TOG'                           
*                                  INSERT ORDER #                               
         MVC   RETSTAT,RDARKSTA    INSERT STATION                               
         CLI   RETSTAT+4,C'L'      IS IT A TV STATION?                          
         BE    PREA130             YES - LEAVE AS IS                            
         MVI   RETSTAT+5,C'V'      INSERT LAST CHAR OF MEDIA                    
         CLI   RETSTAT+4,C'T'      IS IT A TV STATION?                          
         BE    PREA130             YES - LEAVE AS IS                            
         MVI   RETSTAT+5,C'M'      NO  - INSERT RADIO MEDIA                     
*                                                                               
PREA130  DS    0H                                                               
         MVI   METHOD,ADDBUYQ                                                   
         CLI   STAMETH,0                                                        
         BE    PREA150                                                          
         CLI   STAMETH,3                                                        
         BNE   PREAX                                                            
         MVI   METHOD,CHGBUYQ                                                   
         B     PREAX                                                            
*                                                                               
PREA150  DS    0H                                                               
         LR    R4,RA               USE R2 TO COVER THE ENTRY                    
         AH    R4,=AL2(DARPROFS-CONHEADH)                                       
         USING SVDSECT,R4                                                       
*                                                                               
         TM    SVPGPBIT+CNTDRV3B,CNTDRV3A                                       
         BZ    PREAX                                                            
         MVI   METHOD,CHGBUYQ                                                   
*                                                                               
PREAX    DS    0H                                                               
***** FORCE ADD TO SCHED FOR ALL TEMPORARILY                                    
*****                                                                           
*FORCE ADD TO SCHED FOR ALL TEMPORARILY                                         
***** FORCE ADD TO SCHED FOR ALL TEMPORARILY                                    
         MVI   METHOD,ADDBUYQ                                                   
***** FORCE ADD TO SCHED FOR ALL TEMPORARILY                                    
***** FORCE ADD TO SCHED FOR ALL TEMPORARILY                                    
***** FORCE ADD TO SCHED FOR ALL TEMPORARILY                                    
         B     EXIT                                                             
*                                                                               
PREAERR  DS    0H                                                               
         LA    R2,CONRECH          SET CURSOR HERE                              
         GOTO1 MYERROR                                                          
         DROP  R4                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*                                                                               
* PROPAGATE AGENCY DEMO CHANGES                                                 
* USING THE REP BUY'S LINK TO ITS AGENCY COUNTERPART. WE'LL COMPARE THE         
* TWO TO SEE IF WE NEED TO UPDATE ANY DEMO CATEGORY/VALUE CHANGES               
*                                                                               
***********************************************************************         
DEMOPROC NTR1  BASE=*,LABEL=*                                                   
         TM    MISCFLG2,MF2NODEM                                                
         BO    DEMOPX                                                           
*                                                                               
         LR    RE,RA               CHECK TO SEE IF LOCAL SIGN ON                
         AHI   RE,DARPROFS-CONHEADH                                             
         USING SVDSECT,RE                                                       
         TM    SVPGPBIT+CNTRPEAB,CNTRPEAA                                       
         BZ    DEMOP05                                                          
         DROP  RE                                                               
*                                                                               
         CLI   SIGNONID+4,C'L'                                                  
         BE    DEMOPX                                                           
*                                                                               
DEMOP05  DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RBUYREC,R6                                                       
         XC    KEY,KEY                                                          
         MVI   RBUYKTYP,X'0B'                                                   
         MVC   RBUYKREP,TWAAGY     INSERT REP CODE                              
         MVC   RBUYKCON,COMPCON    INSERT CONTRACT #, 9/COMP/REV                
         DROP  R6                                                               
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
*                                                                               
DEMOP10  DS    0H                                                               
         CLC   KEY(RBUYKPLN-RBUYKEY),KEYSAVE                                    
         BNE   DEMOP100                                                         
*                                                                               
         MVC   SVBUYKEY,KEY        SAVE FOR RESTARTS                            
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         GOTO1 GETREC                                                           
*                                                                               
DEMOP15  DS    0H                                                               
*                                                                               
         TM    MISCFLG2,MF2USRDM   USER DEFINED DEMO?                           
         BZ    DEMOP18                                                          
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'0D',AIO2),0,0                   
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'0E',AIO2),0,0                   
         B     DEMOP75             DELETE 0D,0E AND UPDATE RECORD               
*                                                                               
DEMOP18  DS    0H                                                               
         L     R6,AIO                                                           
         USING RBUYREC,R6                                                       
         CLI   RBUYAGBL,0          HAVE LINK?                                   
         BNE   DEMOP20             YES                                          
*                                                                               
         GOTOR MKGDMPR             PROCESS MAKEGOOD DEMO                        
         B     DEMOP80                                                          
*                                                                               
DEMOP20  DS    0H                                                               
*                                                                               
* FIND MATCHING AGENCY SHADOW BUY                                               
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
SHADOWD  USING RBUYREC,R4                                                       
         XC    KEY,KEY                                                          
         MVC   SHADOWD.RBUYKTYP(2),=X'0B01'                                     
         MVC   SHADOWD.RBUYKREP,TWAAGY    INSERT REP CODE                       
         MVC   SHADOWD.RBUYKCON,COMPCON   INSERT CONTRACT #, 9/COMP/REV         
         MVC   SHADOWD.RBUYKPLN,=X'FFFFFF' INSERT PLAN CODE                     
         MVC   SHADOWD.RBUYKMLN,RBUYAGBL                                        
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(RBUYKLIN-RBUYKEY),KEYSAVE                                    
         BNE   DEMOP80             COULDN'T FIND LINK, SKIP                     
         DROP  R6,SHADOWD                                                       
*                                                                               
DEMOP30  DS    0H                                                               
         MVC   AIO,AIO1                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO1             AGENCY SHADOW BUY                            
         MVI   ELCODE,X'0D'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DEMOP80                                                          
         LR    R4,R6                                                            
SHADOWD  USING RBUYDMEL,R4                                                      
*                                                                               
         XC    ELEM,ELEM                                                        
         ZIC   R1,SHADOWD.RBUYDMLN                                              
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),SHADOWD.RBUYDMEL                                         
         DROP  SHADOWD                                                          
*                                                                               
         LA    R4,ELEM                                                          
SHADOWD  USING RBUYDMEL,ELEM                                                    
*                                                                               
         L     R6,AIO2             REP BUY                                      
         MVI   ELCODE,X'0D'                                                     
         BRAS  RE,GETEL                                                         
         BE    DEMOP35                                                          
*                                                                               
         L     R6,AIO2                                                          
         MVI   ELCODE,X'0E'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DEMOP70                                                          
REPBUYD  USING RBUYRDEL,R6                                                      
         CLC   REPBUYD.RBUYRDCT,SHADOWD.RBUYDMCT                                
         BE    DEMOP70                                                          
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'0E',AIO2),0,0                   
         B     DEMOP70                                                          
         DROP  REPBUYD                                                          
*                                                                               
DEMOP35  DS    0H                                                               
         USING RBUYDMEL,R6                                                      
*                                                                               
         CLC   SHADOWD.RBUYDMLN,RBUYDMLN                                        
         BNE   DEMOP60                                                          
         TM    MISCFLG2,MF2USRDM   AGY SEND OVER USR DEFINED TARGET DEM         
         BO    DEMOP60             BLOW AWAY DEMOS                              
*                                                                               
         SR    R2,R2                                                            
         ZIC   R3,RBUYDMLN                                                      
         SHI   R3,2                                                             
         LA    RE,L'RBUYDMCV                                                    
         DR    R2,RE               R3 HAS NUMBER OF DEMOS TO CHECK              
*                                                                               
DEMOP40  DS    0H                  IF AGY CHANGE FIRST CATEGORY                 
         CLC   SHADOWD.RBUYDMCT,RBUYDMCT                                        
         BE    DEMOP45             ON REVISION, THEN DELETE 0E ELE              
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'0E',AIO2),0,0                   
         B     DEMOP60                                                          
*                                                                               
DEMOP45  DS    0H                                                               
         CLC   SHADOWD.RBUYDMCT,RBUYDMCT                                        
         BNE   DEMOP60             SAME CATEGORY?                               
         CLC   SHADOWD.RBUYDMDM,RBUYDMDM                                        
         BE    DEMOP50             SAME VALUE?                                  
         MVC   SHADOWD.RBUYDM2M,RBUYDMDM                                        
*                                                                               
DEMOP50  DS    0H                                                               
         AHI   R4,L'RBUYDMCV                                                    
         AHI   R6,L'RBUYDMCV                                                    
         BCT   R3,DEMOP45                                                       
*                                                                               
DEMOP60  DS    0H                  REMOVE EXISTING AGENCY DEMO VALUES           
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'0D',AIO2),0,0                   
DEMOP70  DS    0H                                                               
         L     RE,AIO2             REREAD FOR PUTREC                            
         MVC   KEY(L'RBUYKEY),0(RE)                                             
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(L'RBUYKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'Y'                                                    
         MVC   AIO,AIOAREA         WRITE OUT BUY RECORD                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO2                                                         
         GOTO1 ADDELEM                                                          
DEMOP75  DS    0H                                                               
         GOTO1 PUTREC                                                           
*                                                                               
DEMOP80  DS    0H                                                               
         MVC   KEY(27),SVBUYKEY    RESTART                                      
         GOTO1 HIGH                                                             
*                                                                               
DEMOP90  DS    0H                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 SEQ                                                              
         B     DEMOP10                                                          
*                                                                               
* PROCESS MAKEGOOD OFFERS                                                       
*                                                                               
DEMOP100 DS    0H                                                               
         XC    KEY,KEY                                                          
         L     R2,AIO3                                                          
         USING RCONREC,R2                                                       
MGK      USING RMKGREC,KEY                                                      
         MVI   MGK.RMKGKTYP,X'11'                                               
         MVC   MGK.RMKGKREP,TWAAGY     INSERT REP CODE                          
         MVC   MGK.RMKGKOFF,RCONKOFF   INSERT OFFICE CODE                       
         MVC   MGK.RMKGKSTA,RCONKSTA   INSERT STATION CALL LETTERS              
         MVC   MGK.RMKGKCON,COMPCON    INSERT CONTRACT #, 9/COMP/REV            
         DROP  R2,MGK                                                           
*                                                                               
DEMOP105 DS    0H                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
*                                                                               
DEMOP110 DS    0H                                                               
         CLC   KEY(RMKGKGRP-RMKGKEY),KEYSAVE                                    
         BNE   DEMOPX                                                           
*                                                                               
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING RMKGREC,R6                                                       
         OC    RMKGKPLN,RMKGKPLN   HEADER RECORD?                               
         BNZ   DEMOP130            YES, CHECK IF APPLIED OR CANCELLED           
         TM    RMKGSCST,RMKGSAPQ+RMKGSCNQ                                       
         BNZ   DEMOP115            IF SO, SKIP TO NEXT GROUP                    
         GOTO1 SEQ                                                              
         B     DEMOP110                                                         
         DROP  R6                                                               
*                                                                               
DEMOP115 DS    0H                                                               
         LA    R6,KEY                                                           
         USING RMKGREC,R6                                                       
         CLI   RMKGKGR2,C'Z'                                                    
         BL    DEMOP120                                                         
         MVI   RMKGKGR2,C'A'                                                    
         ZIC   RF,RMKGKGR1                                                      
         AHI   RF,1                                                             
         STC   RF,RMKGKGR1                                                      
         B     DEMOP105                                                         
*                                                                               
DEMOP120 DS    0H                                                               
         ZIC   RF,RMKGKGR2                                                      
         AHI   RF,1                                                             
         STC   RF,RMKGKGR2                                                      
         B     DEMOP105                                                         
*                                                                               
DEMOP130 DS    0H                                                               
         MVI   ELCODE,X'0E'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DEMOP140                                                         
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'0E',AIO2),0,0                   
         GOTO1 PUTREC                                                           
*                                                                               
DEMOP140 DS    0H                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 SEQ                                                              
         B     DEMOP110                                                         
*                                                                               
DEMOPX   DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*                                                                               
* PROCESS MAKEGOOD LINES DEMO CATEGORY UPDATE                                   
*                                                                               
***********************************************************************         
MKGDMPR NTR1   BASE=*,LABEL=*                                                   
*                                  THIS IS EITHER A MKGOOD                      
*                                  OR A BUY CREATED BY REPS AFTER CF            
         TM    MISCFLG2,MF2USRDM                                                
         BO    MKGDM10                                                          
         TM    FLAGS2,FG2DEMO      IF DEMO CAT CHANGED                          
         BZ    MKGDMX                                                           
MKGDM10  DS    0H                                                               
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'0E',AIO2),0,0                   
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'0D',AIO2),0,0                   
*                                  WIPE OUT EXISTING DEMO CAT                   
         TM    MISCFLG2,MF2USRDM                                                
         BO    MKGDM30                                                          
*                                                                               
         XC    ELEM,ELEM           RESET DEMO TO 0                              
D        USING RBUYDMEL,ELEM                                                    
         MVI   D.RBUYDMCD,RBUYDMCQ                                              
         LA    R2,2                                                             
         LA    R3,SVDEMCAT+2                                                    
         LA    R4,ELEM+2                                                        
         USING RBUYDMCV,R4                                                      
*                                                                               
MKGDM13  DS    0H                                                               
         OC    0(3,R3),0(R3)                                                    
         BZ    MKGDM15                                                          
*                                                                               
         MVC   RBUYDMCT,0(R3)                                                   
         XC    RBUYDMDM,RBUYDMDM                                                
         MVC   RBUYDM2M,=F'-1'                                                  
*                                                                               
         LA    R3,L'RBUYDMCT(R3)                                                
         LA    R4,L'RBUYDMCV(R4)                                                
         LA    R2,L'RBUYDMCV(R2)                                                
         B     MKGDM13                                                          
*                                                                               
MKGDM15  DS    0H                                                               
         CLI   R2,2                                                             
         BE    MKGDMX                                                           
         STC   R2,D.RBUYDMLN                                                    
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),AIO2,ELEM,=C'ADD=CODE'             
*                                                                               
         L     RE,AIO2             REREAD FOR PUTREC                            
         MVC   KEY(L'RBUYKEY),0(RE)                                             
*                                                                               
MKGDM30  DS    0H                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(L'RBUYKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'Y'                                                    
         MVC   AIO,AIOAREA         WRITE OUT BUY RECORD                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO2                                                         
         GOTO1 PUTREC                                                           
*                                                                               
MKGDMX   DS    0H                                                               
         B     EXIT                                                             
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*                                                                               
* SPECIAL TRAP FOR AUTO-APPROVAL TOTALS NOT MATCHING                            
*                                                                               
***********************************************************************         
TRAP     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XR    R1,R1                                                            
         L     R6,AIO3                                                          
         MVI   ELCODE,X'03'                                                     
         TM    MISCFLG2,MF2TRADE   TRADE ORDER?                                 
         BZ    *+8                                                              
         MVI   ELCODE,X'63'                                                     
*                                                                               
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
TRAP10   BRAS  RE,NEXTEL                                                        
         BNE   TRAP20                                                           
*                                                                               
         ZICM  R0,6(R6),4                                                       
         AR    R1,R0                                                            
         B     TRAP10                                                           
*                                                                               
TRAP20   DS    0H                                                               
         ZICM  R0,GTOTAL$,4                                                     
         CR    R0,R1                                                            
         BNE   TRAP30                                                           
*                                                                               
         B     TRAPX                                                            
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BNE   TRAPX                                                            
*                                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(APPRVOKQ),APPRVOK                                        
         OI    CONHEADH+6,X'80'    XMIT                                         
         OI    CONACTH+6,X'40'     FORCE CURSOR HERE                            
         L     RF,ATIOB                                                         
         USING TIOBD,RF                                                         
         OI    TIOBINDS,TIOBALRM   SOUND A BEEP                                 
         DROP  RF                                                               
         DC    H'0',C'$ABEND'                                                   
*                                                                               
*                                                                               
* NOTIFY USER TO CONTRACT DDS                                                   
*                                                                               
TRAP30   DS    0H                                                               
*        GOTO1 =A(DUMPTSAR),RR=RELO                                             
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(ERRMTCHQ),ERRMTCH                                        
         OI    CONHEADH+6,X'80'    XMIT                                         
         OI    CONACTH+6,X'40'     FORCE CURSOR HERE                            
         L     RF,ATIOB                                                         
         USING TIOBD,RF                                                         
         OI    TIOBINDS,TIOBALRM   SOUND A BEEP                                 
         DROP  RF                                                               
         DC    H'0',C'$ABEND'                                                   
*                                                                               
TRAPX    DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
ERRMTCH  DC    C'Error: Contract and order total mismatch. Call DDS.'           
ERRMTCHQ EQU   *-ERRMTCH                                                        
APPRVOK  DC    C'Approval went to completion. All spots matched.'               
APPRVOKQ EQU   *-APPRVOK                                                        
         EJECT                                                                  
***********************************************************************         
* POST APPROVAL/REJECTION PROCESSING:                                           
* - UPDATE DARE AGENCY RECORD                                                   
* - WRITE OUT DARE TRANSACTION MESSAGES TO PRINT QUEUE                          
* - UPDATE PASSIVE KEYS                                                         
***********************************************************************         
POSTAPRJ NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   KEY(L'SELECTKY),SELECTKY                                         
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                SHOULD NEVER HAPPEN!                         
*                                                                               
         MVC   AIO,AIO1                                                         
*                                                                               
         GOTO1 GETREC                                                           
                                                                                
         L     R4,AIO                                                           
         USING RDARREC,R4                                                       
*                                                                               
         L     R6,AIO                                                           
         USING RDARELEM,R6                                                      
*                                                                               
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
*   SAVE HEADER INFORMATION FOR RETURN MESSAGE                                  
*                                                                               
         MVC   RETFROM(20),RDARSNDR                                             
*                                                                               
         CLC   =C'H7',RDARSNDR                                                  
         BNE   *+10                                                             
         MVC   RETFROM(2),=C'MS'                                                
*                                                                               
*   INSERT INFORMATION FROM KEY SECTION INTO RETURN MESSAGE                     
*      SAVE AREA                                                                
*                                                                               
         GOTO1 HEXOUT,DMCB,RDARKORD,RETORD#,4,=C'TOG'                           
*                                  INSERT ORDER #                               
         MVC   RETSTAT,RDARKSTA    INSERT STATION                               
         CLI   RETSTAT+4,C'L'      IS IT A TV STATION?                          
         BE    PAR400              YES - LEAVE AS IS                            
         MVI   RETSTAT+5,C'V'      INSERT LAST CHAR OF MEDIA                    
         CLI   RETSTAT+4,C'T'      IS IT A TV STATION?                          
         BE    PAR400              YES - LEAVE AS IS                            
         MVI   RETSTAT+5,C'M'      NO  - INSERT RADIO MEDIA                     
*                                                                               
*                                  INSERT SENDER/RECEIVER                       
PAR400   DS    0H                                                               
         GOTO1 HEXOUT,DMCB,RDARREP#,RETCON#,4,=C'TOG'                           
*                                  INSERT CONTRACT # (REP)                      
*                                                                               
         MVC   RETSENDR,RDARRTS    INSERT 'RETURN TO SENDER' INFO               
*                                                                               
*   SET REPORT ID/CLASS FOR SPOOLING                                            
*                                                                               
         CLI   KATZEDI,C'Y'        SKIP SENDING APPROVAL NOTICE BACK TO         
         BE    PAR690              THE AGENCY FOR KATZ EDI ORDERS               
*                                                                               
         MVC   REMUSER,=C'DAR'                                                  
         LA    RF,SPOOLKEY                                                      
         USING PQPLD,RF                                                         
*                                                                               
         XC    SPOOLKEY,SPOOLKEY                                                
*                                                                               
         MVC   PLDESC,=CL11'DARE REJECT'                                        
         MVC   EDICTACT,=C'REJ'    SET EDICT 'ACTION'                           
         CLI   ACTCODE,C'R'        APPROVAL/REJECTION?                          
         BE    PAR510              NO                                           
         MVC   PLDESC,=CL11'DARE APPRVL'                                        
         MVC   EDICTACT,=C'APP'    SET EDICT 'ACTION'                           
*                                                                               
PAR510   EQU   *                                                                
         OI    GENSTAT3,NOCLRSPK                                                
         MVI   PLCLASS,C'G'        CLASS G                                      
         OI    SPOOLIND,SPUINIT    PERMITS SETTING OF CLASS                     
*                                                                               
         DROP  R6,RF                                                            
*                                                                               
         LA    RE,SPLKEYAD         SET EXTENDED KEY ADDRESS                     
*                                                                               
*                                                                               
         ST    RE,SPOOLQLK         SAVE EXTENDED KEY ADDRESS                    
         USING PQPLD,RE                                                         
         XC    0(133,RE),0(RE)                                                  
         MVC   QLRETNL,=H'6'                                                    
         MVC   QLRETND,=H'2'                                                    
         DROP  RE                                                               
*                                                                               
PAR690   DS    0H                                                               
         CLI   KATZEDI,C'Y'        SKIP MESSAGE FOR KATZ EDI ORDERS             
         BE    PAR0880                                                          
*                                  SEND MESSAGE: ORDER APPROVED                 
         GOTO1 OPENPQ                                                           
         GOTO1 =A(EDICT),RR=RELO                                                
*                                  PUT OUT EDICT HEADER                         
         GOTO1 =A(APPRREJC),RR=RELO                                             
*                                                                               
         CLI   ACTCODE,C'R'                                                     
         BNE   PAR0880                                                          
         GOTO1 =A(REJCMSGS),RR=RELO                                             
*                                  RETRIEVE HEADER OF AGY ORD                   
PAR0880  DS    0H                                                               
         MVC   KEY(L'SELECTKY),SELECTKY                                         
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
                                                                                
         L     R4,AIO                                                           
         USING RDARREC,R4                                                       
*                                                                               
         MVC   RDARBSTS,ACTCODE    SET STATUS TO ACTCODE                        
         CLI   ACTCODE,C'A'        ORDER APPROVED?                              
         BNE   PAR0900             NO  -                                        
         OI    RDARMISC,X'40'      YES - SET 'APPROVED AT LEAST ONCE'           
*                                                                               
PAR0900  EQU    *                                                               
         CLI   ACTCODE,C'R'        ORDER REJECTED?                              
         BNE   PAR0910             NO  -                                        
*                                                                               
         GOTO1 =A(ADDREJS),DMCB,(R4),RR=RELO                                    
PAR0910  EQU   *                                                                
**       GOTO1 PUTREC              REWRITE RECORD WITH NEW STATUS               
**                                                                              
*                                                                               
         GOTO1 =A(WRITEREC),RR=RELO                                             
*                                                                               
         GOTO1 =A(DOAUDIT),DMCB,(R4),RR=RELO                                    
*                                                                               
         CLI   KATZEDI,C'Y'        SKIP MESSAGE FOR KATZ EDI ORDERS             
         BE    PAR0960                                                          
*                                  SEND MESSAGE: ORDER APPROVED                 
*                       1.3.5.7.9.1.3.5.7.9.1.3.5.                              
*        MVC   P(26),=C'*** END OF DDS MESSAGE ***'                             
*                                  SEND SPECIAL PRINT LINE                      
*        GOTO1 SPOOL,DMCB,(R8)                                                  
*        MVI   LINE,1              FORCE TO SINGLE PAGE                         
         DROP  R4                                                               
*                                                                               
PAR0920 DS     0H                                                               
         MVI   SPMODE,X'FF'        CLOSE THE PRINT QUEUE                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   LINE,1              FORCE TO SINGLE PAGE                         
*                                                                               
*                                  INSTEAD OF REJECT MESSAGE                    
PAR0960 DS     0H                                                               
*                                                                               
* SPECIAL FOR KATZ EDI ORDERS. IF ACTION IS APPROVE, ADD A PASSIVE KEY          
* X'E1' WITH CONTRACT # AND DATE/TIME STAMP INFO TO BE PICKED UP LATER          
* AT NIGHT BY A REPORT THAT WILL WRITE THESE ORDERS OUT TO TAPE                 
*                                                                               
         CLI   ACTCODE,C'A'        FOR ACTION APPROVE                           
         BNE   PARX                                                             
         CLI   KATZEDI,C'Y'        AND KATZ EDI ORDERS                          
         BNE   PAR0970                                                          
         GOTO1 =A(GENEDIKY),RR=RELO                                             
*                                                                               
PAR0970 DS     0H                                                               
         MVI   RMSGTYPE,C'I'                                                    
         LA    R2,CONSERVH         SET A(SERVICE REQUEST)                       
         MVC   RERROR,=AL2(113)    SET ERROR TYPE = APPROVED                    
         CLI   ACTCODE,C'A'        APPROVED?                                    
         BE    PAR1000             YES                                          
         MVC   RERROR,=AL2(114)    SET ERROR TYPE = REJECTED                    
*                                                                               
PAR1000 EQU    *                                                                
         LA    R2,CONRECH                                                       
         GOTO1 MYERROR                                                          
*                                                                               
PARX     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CREATES EDICT HEADER CARDS FOR EDICT PROCESSING                               
***********************************************************************         
         DS    0F                                                               
EDICT    NTR1  BASE=*,LABEL=*                                                   
         MVI   FORCEHED,C'N'       PREVENT HEADLINES                            
         MVI   LINE,0                                                           
         MVC   P+4(5),=C'*HDR*'                                                 
                                                                                
         MVC   P+9(14),=C'EDICT=*DDSDARR'                                       
                                                                                
*                                                                               
         MVI   P+34,C'W'           WIDE REPORT - 132 CHARS                      
*                                  SEND SPECIAL PRINT LINE                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
* PRINT A ++DDS CARD                                                            
*                                                                               
         LA    R3,P                                                             
         USING EDICTD,R3                                                        
         MVC   EDIDDSID,=C'++DDS'                                               
         MVI   EDISYST,C'D'        UPPER CASE 'D'                               
         MVC   EDIPROG,EDICTACT    INSERT EDICT 'ACTION'                        
         MVC   EDIIDEN,=C'TRN'     TRANSACTION DATA                             
                                                                                
*                                                                               
* INFORMATION CHUNK FOR ETI REPORTING                                           
*                                                                               
         MVC   KEY(L'SELECTKY),SELECTKY                                         
         GOTO1 HIGH                                                             
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
                                                                                
         L     R6,AIO              DARE RECORD                                  
         USING RDARREC,R6                                                       
         MVC   EDIRDRRP,RDARKREP   REP CODE                                     
         MVC   EDIRDRAG,RDARKAGY   AGENCY CODE                                  
         MVC   EDIRDRST,RDARKSTA   STATION CODE                                 
         MVC   EDIRDRMD,RDARMEDI   MEDIA CODE                                   
                                                                                
         EDIT  RDAREST#,(3,EDIRDRES),ALIGN=LEFT                                 
                                                                                
* AGENCY ORDER #                                                                
         ZAP   WORK2(5),=P'0'                                                   
         MVO   WORK2(5),RDARKORD                                                
         EDIT  (P5,WORK2),(8,EDIRDRAN),ALIGN=LEFT                               
                                                                                
* CONTRACT #                                                                    
         OC    RDARREP#,RDARREP#                                                
         BZ    EDICT10                                                          
         ZAP   WORK2(5),=P'0'                                                   
         MVO   WORK2(5),RDARREP#                                                
         EDIT  (P5,WORK2),(8,EDIRDRCN),ALIGN=LEFT                               
                                                                                
         L     R4,AIO3             RESET A(CONTRACT RECORD)                     
         USING RCONREC,R4                                                       
         MVC   EDIRDRSP,RCONSAL    SALESMAN CODE                                
         DROP  R4                                                               
                                                                                
EDICT10  DS    0H                                                               
         MVC   EDIRDRBY,RDARBUYC   BUYER CODE                                   
                                                                                
         MVI   ELCODE,X'02'        DESCRIPTIVE ELEMENT #2                       
         BRAS  RE,GETEL                                                         
         BNE   EDICT50                                                          
         USING RDARCLEM,R6                                                      
         MVC   EDIRDRCL,RDARCLI    CLIENT CODE                                  
         MVC   EDIRDRP1,RDARPRD1   PRODUCT CODE 1                               
         MVC   EDIRDRP2,RDARPRD2   PRODUCT CODE 2                               
         DROP  R3,R6                                                            
*                                  SEND SPECIAL PRINT LINE                      
EDICT50  DS    0H                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
                                                                                
         MVI   LINE,1              FORCE TO SINGLE PAGE                         
                                                                                
EDICTX   DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*   APPRREJC:  APPROVAL/REJECTION HEADER OUTPUT                                 
***********************************************************************         
         DS    0F                                                               
APPRREJC NTR1  BASE=*,LABEL=*                                                   
         LA    R4,P                SET PRINT OUTPUT                             
         USING ORDAPREJ,R4                                                      
*                                                                               
         CLI   ACTCODE,C'A'        APPROVAL?                                    
         BNE   APRJ0020            NO  -  REJECT                                
         MVC   ARTRANID,=C'ORDAPP'                                              
         B     APRJ0040                                                         
APRJ0020 EQU   *                                                                
         MVC   ARTRANID,=C'ORDREJ'                                              
APRJ0040 EQU   *                                                                
*                                  INSERT IDENTIFIER                            
         MVC   ARORDNUM,RETORD#    INSERT ORDER NUMBER                          
         MVC   ARFROM,RETTO        INSERT FROM CODE                             
         MVC   ARTO,RETFROM        INSERT TO CODE                               
*                                     NOTE:  CODES ARE REVERSED                 
         GOTO1 DATCON,DMCB,(5,WORK),(X'20',ARDATE)                              
*                                  INSERT TODAY'S DATE                          
         THMS  DDSTIME=YES                                                      
         ST    R0,DUB              ACTUAL TIME ADJUSTMENT                       
         ST    R1,DUB+4            DDS TIME                                     
         AP    DUB(4),DUB+4(4)                                                  
         ICM   R1,15,DUB                                                        
         SRL   R1,12               SHIFT OFF SECONDS AND SIGN                   
         STCM  R1,3,WORK                                                        
         GOTO1 HEXOUT,DMCB,WORK,ARTIME,2,0                                      
*                                                                               
*                                                                               
         MVC   ARSTAT(6),RETSTAT   INSERT STATION                               
         MVC   ARCON#,RETCON#      INSERT CONTRACT #                            
         MVC   ARRETSND,RETSENDR   INSERT 'RET TO SENDER' INFO                  
****>>>> MVC   ARDDS,=C'DDS'       INSERT LINE DELIMITER                        
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)     SEND THE MESSAGE                             
         MVI   LINE,1              FORCE TO SINGLE PAGE                         
*                                                                               
         CLI   ACTCODE,C'A'        APPROVAL?                                    
         BE    APRJ0320            APPROVED:  DON'T UPDATE CONTRACT             
*                                     STATUS FLAG HERE...                       
*                                                                               
*                                  SET 'ORDER REJECTED' FLAG                    
*                                     IN CONTRACT RECORD AND REWRITE            
*&&DO                                                                           
         MVC   AIO,AIO3            SET A(IO AREA)                               
         L     R2,AIO3                                                          
         USING RCONREC,R2                                                       
*                                                                               
         OC    RCONREC(L'RCONKEY),RCONREC                                       
         BZ    APRJ0320            ORDER IS UNLINKED                            
*                                                                               
         MVC   KEY(27),RCONREC     GET KEY FROM CONTRACT RECORD                 
         MVC   KEYSAVE,KEY         DON'T RETURN DELETED RECORDS                 
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY     REDUNDANT CHECK: KEY FOUND?                  
         BNE   APRJ0320                       NO  - EXIT                        
         GOTO1 GETREC              READ CONTRACT                                
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'        RETRIEVE                                     
         BRAS  RE,GETEL                                                         
         BE    APRJ0160            FOUND: UPDATE IT                             
         XC    ELTBUILD,ELTBUILD   NOT FOUND: BUILD IT                          
         MVI   ELTBUILD,X'1D'      INSERT ELEMENT CODE                          
         LA    RF,RCONDL2Q                                                      
         STC   RF,ELTBUILD+1       INSERT ELEMENT LENGTH                        
         OI    ELTBUILD+2,X'20'    SET 'REJECTED' FLAG                          
*                                                                               
         CLI   DIFHDLNH+5,0        ANY CONTRACT NUMBER ON SCREEN?               
         BE    *+8                                                              
         OI    ELTBUILD+2,X'80'    YES, SET 'LINKED' FLAG                       
*                                                                               
         GOTO1 HEXIN,DMCB,RETORD#,ELTBUILD+3,8                                  
*                                  INSERT HEX VALUE OF AGY ORD #                
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),RCONREC,ELTBUILD,         X        
               =C'ADD=CODE'                                                     
         B     APRJ0200                                                         
APRJ0160 EQU   *                                                                
         MVI   2(R6),X'A0'         SET 'REJECTED+LINKED' FLAGS, CLEAR           
*                                     ANY PREVIOUS VALUE                        
APRJ0200 EQU   *                                                                
         GOTO1 PUTREC              WRITE UPDATED RECORD TO FILE                 
         DROP  R2,R4                                                            
*                                                                               
*&&                                                                             
APRJ0320 EQU   *                                                                
         B     EXIT                                                             
*                                                                               
         DROP  R4                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   REJCMSGS:  REJECTION MESSAGES                                               
*                                                                               
         DS    0F                                                               
REJCMSGS NTR1  BASE=*,LABEL=*                                                   
         LA    RF,REJMESS          SET A(REJECTION MESSAGES)                    
         ST    RF,AREJMESS                                                      
*                                                                               
*   LINES ON SCREEN ARE COUNTED, TO DETERMINE WHERE '*' CONTINUATION            
*     MARKS ARE TO BE PLACED.  BLANK LINES WITHIN THE BODY OF THE               
*     REJECTION COMMENTS ARE SKIPPED.  THESE ARE THEN SKIPPED OVER              
*     WHEN THE SCREEN IS FORMATTED INTO THE OUTPUT MESSAGES.                    
*                                                                               
         LA    R2,AORREASH         MESSAGE HEADER                               
         LA    R3,AORLAST          LAST                                         
         SR    R4,R4                                                            
REMS0020 EQU   *                                                                
         CLI   5(R2),0             ANYTHING ON LINE?                            
         BE    REMS0040            NO  - DON'T COUNT LINE                       
         LA    R4,1(R4)            YES - COUNT LINE                             
REMS0040 EQU   *                                                                
         LA    R2,AORREA2H-AORREASH(R2)                                         
*                                  BUMP TO NEXT LINE                            
         CR    R2,R3               END OF SCREEN REACHED?                       
         BL    REMS0020            NO  - GO BACK FOR NEXT                       
REMS0060 EQU   *                                                                
         XC    P,P                 CLEAR PRINT LINE                             
         LA    R6,P                                                             
         USING ORDCOM,R6                                                        
*                                                                               
         LR    R3,R4               SET UP LOOP                                  
         LTR   R3,R3               ANY REJECT LINES?                            
         BZ    REMS0100            NO                                           
         LA    R2,AORREASH                                                      
REMS0080 EQU   *                                                                
         BAS   RE,MSGCHECK         OUTPUT PREVIOUS LINE?                        
         MVC   OCTRANID,=C'ORDCOM'                                              
         MVC   OCORDNUM,RETORD#                                                 
REMS0090 EQU   *                                                                
         ZIC   RF,5(R2)            GET LENGTH OF LINE                           
         LTR   RF,RF               ANYTHING ON LINE?                            
         BNZ   REMS0095            YES                                          
         ST    RF,DMCB+8           INSERT LENGTH (ZERO) INTO P3                 
         GOTO1 REJMSGS,DMCB,(RC),0                                              
*                                  NO  - BLANK LINE IN MESS AREA                
         LA    R4,1(R4)            COUNT BLANK LINE IN                          
*                                     TOTAL LINES                               
         LA    R2,AORREA2H-AORREASH(R2)                                         
*                                  BUMP TO NEXT LINE                            
         B     REMS0080            GO BACK FOR NEXT                             
*                                     WITHOUT CHANGING COUNTER                  
REMS0095 EQU   *                                                                
         BCTR  RF,0                DECREMENT FOR MOVE                           
         EX    RF,REMS0950         MOVE BY LENGTH                               
****>>>  MVC   OCDDS,=C'DDS'       INSERT LINE DELIMITER                        
         ST    RF,DMCB+8           INSERT LENGTH INTO P3                        
         GOTO1 REJMSGS,DMCB,(RC),(R2)                                           
         LA    R2,AORREA2H-AORREASH(R2)                                         
*                                  BUMP TO NEXT LINE                            
         BCT   R3,REMS0080         GO BACK FOR NEXT                             
*                                                                               
* SUPPRESS STATION ORDER COMMENTS PER PETRY AND ELLEN WEINSTEIN                 
*                                                                               
REMS0100 EQU   *                                                                
         B     REMS1000                                                         
REMS0950 MVC   OCCOMMNT(0),8(R2)   MOVE BY LENGTH                               
*                                                                               
REMS1000 EQU   *                                                                
*                                                                               
*   DETERMINE IF LAST BUYLINE COMMENT ENTRY IS STILL WAITING TO                 
*      SPOOL.                                                                   
*                                                                               
         CLC   P,SPACES            ANYTHING ON PRINT LINE?                      
         BE    REMS1100                                                         
         OC    P,P                 ANYTHING ON PRINT LINE?                      
         BZ    REMS1100            NO  - NO OUTPUT                              
         GOTO1 SPOOL,DMCB,(R8)     SPOOL THE LINE                               
         MVI   LINE,1              FORCE TO SINGLE PAGE                         
         OC    OCBUYLIN,OCBUYLIN   BUYLINE NUMBER IN LINE?                      
         BZ    REMS1100            NO                                           
         CLC   OCBUYLIN,SPACES     BUYLINE NUMBER IN LINE?                      
         BE    REMS1100                                                         
*                                                                               
*   ONLY BUMP THE LINECOUNT FOR COMMENTS FROM BUYLINE RECORDS.                  
*      THE SCREEN MESSAGE LINES ARE ALREADY COUNTED.                            
*                                                                               
*        LA    R4,1(R4)            BUMP THE LINECOUNT                           
*                                                                               
*        DROP  R2,R3,R5                                                         
*                                                                               
REMS1100 EQU   *                                                                
         USING ORDTRLR,R6                                                       
         MVC   OTTRANID,=C'ORDTLR'                                              
         MVC   OTORDNUM,RETORD#                                                 
         LA    R4,2(R4)            ADD 1 EACH FOR HDR, TRLR                     
         EDIT  (R4),(6,OTCOUNT),FILL=0,ZERO=NOBLANK                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   LINE,1              FORCE TO SINGLE PAGE                         
*                                                                               
*   INSERT REJECT DATE/TIME STAMP                                               
*                                                                               
         OC    AORHDLN,AORHDLN     ANY CONTRACT NUMBER ON SCREEN?               
         BZ    REMS1300            NO  - DON'T GET CONTRACT NUMBER              
         L     RF,AIOAREA          PREPARE TO REWRITE CONTRACT RECORD           
         ST    RF,AIO              SET IO AREA TO READ OLD RECORD               
         L     R2,AIO3                                                          
         USING RCONREC,R2                                                       
*                                                                               
         MVC   KEY(27),RCONREC     GET KEY FROM NEW RECORD                      
         MVC   KEYSAVE,KEY                                                      
         OI    DMINBTS,X'08'       RETURN DELETED KEY ALSO                      
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY     REDUNDANT CHECK: KEY FOUND?                  
         BNE   REMS1300            YES                                          
         NI    KEY+27,X'FF'-X'80'  RESTORE KEY                                  
         OI    DMINBTS,X'08'       RETURN DELETED RECORDS ALSO                  
         GOTO1 GETREC              READ INTO AIOAREA                            
         MVC   AIO,AIO3            RESET TO UPDATED CON RECORD                  
*                                                                               
*                                  UPDATE CONTRACT STATUS ELEMENT               
*                                                                               
*                                                                               
         LA    R2,RCONELEM         FIND X'1D' ELEMENT                           
REMS1120 EQU   *                                                                
         CLI   0(R2),0             END OF RECORD?                               
         BE    REMS1140            YES - NOT FOUND - BUILD IT                   
         CLI   0(R2),X'1D'         DARE ELEMENT?                                
         BE    REMS1200            FOUND: UPDATE IT                             
         ZIC   RF,1(R2)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R2,RF                                                            
         B     REMS1120            GO BACK FOR NEXT                             
REMS1140 EQU   *                                                                
         XC    ELTBUILD,ELTBUILD   NOT FOUND: BUILD IT                          
         MVI   ELTBUILD,X'1D'      INSERT ELEMENT CODE                          
         LA    RF,RCONDL2Q                                                      
         STC   RF,ELTBUILD+1       INSERT ELEMENT LENGTH                        
         OI    ELTBUILD+2,X'20'    SET 'REJECTED' FLAG                          
*                                                                               
         CLI   AORHDLNH+5,0        ANY CONTRACT NUMBER ON SCREEN?               
         BE    *+8                                                              
         OI    ELTBUILD+2,X'80'    YES, SET 'LINKED' FLAG                       
*                                                                               
         TM    MISCFLAG,X'80'      DAILY ORDER?                                 
         BZ    *+8                                                              
         OI    ELTBUILD+2,X'08'    SET DAILY FLAG                               
                                                                                
         GOTO1 HEXIN,DMCB,RETORD#,ELTBUILD+3,8                                  
*                                  INSERT HEX VALUE OF AGY ORD #                
         MVC   ELTBUILD+RCONDRDR-RCONDREL(4),ACTDATE                            
*                                  YES - MOVE DATE+TIME TO REJECTED             
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),RCONREC,ELTBUILD,         X        
               =C'ADD=CODE'                                                     
         B     REMS1250                                                         
REMS1200 EQU   *                                                                
                                                                                
         MVI   2(R2),X'A0'         SET 'REJECTED+LINKED' FLAGS, CLEAR           
*                                     ANY PREVIOUS VALUE                        
         TM    MISCFLAG,X'80'      DAILY ORDER?                                 
         BZ    *+8                                                              
         OI    2(R2),X'08'         SET DAILY FLAG                               
                                                                                
         MVC   RCONDRDR-RCONDREL(4,R2),ACTDATE                                  
*                                  YES - MOVE DATE+TIME TO REJECTED             
REMS1250 EQU   *                                                                
         GOTO1 PUTREC              WRITE UPDATED RECORD TO FILE                 
         GOTO1 WRITE               REWRITE CLEARED KEY FOR RECORD               
REMS1300 EQU   *                                                                
         B     EXIT                                                             
*                                                                               
         DROP  R2,R6                                                            
         EJECT                                                                  
*                                                                               
*   MSGCHECK:  DETERMINE IF A LINE HAS TO BE SPOOLED.  THIS IS TO               
*      ENABLE THE '*' TO BE INSERTED FOR ANOTHER LINE FOLLOWING                 
*      INDICATOR.  THIS ROUTINE WILL ONLY BE CALLED WHEN A NEW LINE             
*      IS TO BE CONSTRUCTED.  IF OLD LINE IS WAITING TO SPOOL, IT               
*      MUST BE FLAGGED.                                                         
*   NOTE:  CONDITION CODE ON EXIT DETERMINES WHETHER LINE COUNT IS              
*      TO BE INCREMENTED.                                                       
*                                                                               
MSGCHECK NTR1                                                                   
         CLC   P,SPACES            ANYTHING ON PRINT LINE?                      
         BE    MCHE0100            NO  - NO OUTPUT                              
         OC    P,P                 ANYTHING ON PRINT LINE?                      
         BZ    MCHE0100            NO  - NO OUTPUT                              
         USING ORDCOM,R6                                                        
         MVI   OCCONTIN,C'*'       YES - INSERT INDICATOR                       
         GOTO1 SPOOL,DMCB,(R8)     SPOOL THE LINE                               
         MVI   LINE,1              FORCE TO SINGLE PAGE                         
MCHE0100 EQU   *                                                                
         B     EXIT                                                             
         DROP  R6                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
*   REJMSGS:  INSERTS MESSAGE INTO HOLD AREA, FOR LATER INCLUSION               
*        IN THE AGENCY ORDER HEADER, SO THAT MESSAGES CAN BE RE-                
*        CALLED DURING WORKSHEET PRINTING.                                      
*                                                                               
REJMSGS  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R2,4(R1)            RESET A(ERROR MESSAGE LINE)                  
         L     RF,8(R1)            RESET L(ERROR MESSAGE LINE)                  
         L     R3,AREJMESS         SET A(NEXT MESSAGE SLOT)                     
         LTR   RF,RF               ANY LENGTH IN LINE?                          
         BNZ   RMSG0040            YES - PROCESS                                
         MVC   0(2,R3),=X'1002'    NO  - PUT IN EMPTY ELEMENT                   
         LA    R3,2(R3)                                                         
         B     RMSG0200            EXIT                                         
RMSG0040 EQU   *                                                                
         LR    RE,RF               SET NEW ELEMENT LENGTH                       
         LA    RE,3(RE)            ADD FOR ELTID+CTRL+EX DEC                    
         MVI   0(R3),X'10'         INSERT ELEMENT CODE                          
         STC   RE,1(R3)            INSERT ELEMENT LENGTH                        
         EX    RF,RMSG0080         MOVE BY LENGTH                               
         AR    R3,RE               ADD LENGTH TO SLOT ADDR                      
         B     RMSG0200                                                         
*                                                                               
RMSG0080 MVC   2(0,R3),8(R2)       INSERT MESSAGE BY LENGTH                     
RMSG0200 EQU   *                                                                
         ST    R3,AREJMESS         REPLACE A(NEXT MESSAGE SLOT)                 
         XC    0(4,R3),0(R3)       CLEAR NEXT SLOT                              
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   ADDREJS :  REJECTION MESSAGES                                               
*                                                                               
         DS    0F                                                               
ADDREJS  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R4,0(R1)            RESET A(IO AREA)                             
         USING RDARREC,R4                                                       
*                                                                               
         LA    R3,REJMESS          SET A(REJECT MSG AREA)                       
*                                                                               
ARJS0020 EQU   *                                                                
         CLI   0(R3),0             ANY ENTRY?                                   
         BE    ARJS0080            NO  - FINISHED                               
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),(R4),(R3),=C'ADD=CODE'             
*                                  YES - ADD ELEMENT TO AGY HDR REC             
         ZIC   RF,1(R3)            BUMP TO NEXT ENTRY IN TABLE                  
         AR    R3,RF                                                            
         B     ARJS0020            GO BACK FOR NEXT                             
ARJS0080 EQU   *                                                                
         XC    ELTBUILD,ELTBUILD   INSERT DATE/TIME ELEMENT                     
         MVC   ELTBUILD(2),=X'2006'                                             
         MVC   ELTBUILD+2(4),ACTDATE                                            
*                                  MOVE DATE+TIME TO APPROVED                   
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),(R4),ELTBUILD,            X        
               =C'ADD=CODE'                                                     
         B     EXIT                                                             
*                                                                               
         DROP  R4                                                               
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   WRITE OUT DARE RECORD                                                       
*   UPDATE ALL APPROPRIATE PASSIVE KEYS IF RADIO EDI ORDER                      
*   AIO/AIO1 HAS DARE RECORD TO PUT TO FILE                                     
*                                                                               
WRITEREC NTR1  BASE=*,WORK=(R3,500),LABEL=*                                     
*                                                                               
*&&DO                                                                           
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         CLI   RDARMEDI,C'R'       DO ONLY FOR RADIO                            
         BE    WR10                                                             
         GOTO1 PUTREC                                                           
         B     WRX                                                              
         DROP  R6                                                               
*&&                                                                             
*                                                                               
WR10     DS    0H                                                               
         XCEFL (R3),1600           CLEAR KEY BUILD AREA                         
         L     R6,AIO                                                           
         MVC   KEY(27),0(R6)                                                    
*                                                                               
         L     R2,AIO                                                           
         MVC   AIO,AIOAREA         SET ALTERNATE IO AREA                        
         MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                READ THE KEY                                 
         CLC   KEYSAVE(27),KEY     KEY MUST BE ON FILE?                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   HDRDA,KEY+28                                                     
*                                                                               
         GOTO1 GETREC              READ RECD INTO AIOAREA                       
         ST    R2,AIO              RESTORE AIO                                  
*                                                                               
*   PULL OLD KEYS PRE-P/P-S/P CHANGE:                                           
*          R3:  KEY BUILD AREA                                                  
*     AIOAREA:  CURRENT LOCATION OF OLD AGENCY ORDER RECORD                     
*        AIO2:  IO AREA                                                         
*                                                                               
         GOTO1 (RFGENDTR,REPFACS),DMCB,(X'41',ACOMFACS),(R3),AIOAREA,  X        
               AIO2                                                             
*                                                                               
         GOTO1 PUTREC              WRITE OUT CHANGED DARE RECORD                
*                                                                               
*   PULL NEW KEYS PRE-P/P-S/P CHANGE:                                           
*          R3:  KEY BUILD AREA                                                  
*         AIO:  CURRENT LOCATION OF NEW AGENCY ORDER RECORD                     
*        AIO2:  IO AREA                                                         
*                                                                               
         LA    R6,800(R3)          ADD 800 TO KEY BUILD AREA                    
         GOTO1 (RFGENDTR,REPFACS),DMCB,(X'41',ACOMFACS),(R6),AIO,AIO2           
*                                                                               
*   PROCESS OLD VS NEW PASSIVE POINTERS                                         
*                                                                               
         LA    R6,800(R3)          R6->NEW PASSIVE POINTERS                     
         GOTO1 (RFGENDTR,REPFACS),DMCB,(X'02',ACOMFACS),(R3),(R6),HDRDA         
*                                                                               
WRX      DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   DOAUDIT :  ADD AUDIT TRAIL                                                  
*                                                                               
         DS    0F                                                               
DOAUDIT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R4,0(R1)            RESET A(IO AREA)                             
         USING RDARREC,R4                                                       
*                                                                               
         MVC   REVNUM,RDARRNUM                                                  
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(RDARKRT-RDARKEY),RDARKEY                                     
         MVI   KEY+RDARKRT-RDARKEY,X'70'                                        
         GOTO1 HIGH                                                             
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
         BNE   DOAUDX                                                           
         GOTO1 GETREC                                                           
*                                                                               
         MVC   WORK(4),HELLO       RECORD DARE HISTORY                          
         MVC   WORK+4(4),DATCON                                                 
         XC    DMCB+4(4),DMCB+4                                                 
         MVI   DMCB+4,X'FF'        VALID ACTION                                 
         MVI   DMCB+5,DHAPPROQ     ACTION APPROVE                               
         CLI   ACTCODE,C'A'        APPROVAL?                                    
         BE    *+8                                                              
         MVI   DMCB+5,DHREJECQ     ACTION REJECT                                
         MVC   DMCB+6(1),REVNUM    REVISION NUMBER                              
         GOTO1 VREGENDH,DMCB,AIO,,WORK                                          
*                                                                               
         GOTO1 PUTREC                                                           
*                                                                               
DOAUDX   DS    0H                                                               
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   DATETIME:  DEVELOPS THE DATE AND TIME FOR STAMPING                          
*                                                                               
         DS    0F                                                               
DATETIME NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 DATCON,DMCB,(5,WORK),(2,ACTDATE)                                 
*                                  FETCH TODAY'S DATE                           
         THMS  DDSTIME=YES                                                      
         ST    R0,DUB              ACTUAL TIME ADJUSTMENT                       
         ST    R1,DUB+4            DDS TIME                                     
         AP    DUB(4),DUB+4(4)                                                  
         ICM   R1,15,DUB                                                        
         SRL   R1,12               SHIFT OFF SECONDS AND SIGN                   
         STCM  R1,3,ACTTIME                                                     
                                                                                
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* SPECIAL FOR KATZ EDI ORDERS. IF ACTION IS APPROVE, ADD A PASSIVE KEY          
* X'E1' WITH CONTRACT # AND DATE/TIME STAMP INFO TO BE PICKED UP LATER          
* AT NIGHT BY A REPORT THAT WILL WRITE THESE ORDERS OUT TO TAPE                 
*                                                                               
GENEDIKY NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING REDIKEY,R6                                                       
         MVI   REDIKTYP,REDIKTYQ                                                
         MVC   REDIKREP,AGENCY                                                  
         MVI   REDIKACT,C'A'       ACTION IF APPROVE                            
         MVC   REDIKCON,CCONKNUM   CONTRACT NUMBER IN PWOS                      
*                                  DATE IN YYMMDD                               
         GOTO1 DATCON,DMCB,(5,WORK),(3,REDIKDTE)                                
*                                  FETCH TODAY'S DATE                           
         THMS  DDSTIME=YES                                                      
         ST    R0,DUB              ACTUAL TIME ADJUSTMENT                       
         ST    R1,DUB+4            DDS TIME                                     
         AP    DUB(4),DUB+4(4)                                                  
         ICM   R1,15,DUB                                                        
         SRL   R1,4                SHIFT OFF SIGN                               
         STCM  R1,7,REDIKTIM       TIME IN HHMMSS                               
         DROP  R6                                                               
*                                                                               
         L     RE,AIOAREA                                                       
         LA    RF,LIOS                                                          
         XCEF                                                                   
         L     R6,AIOAREA                                                       
         MVC   0(27,R6),KEY                                                     
*                                                                               
         MVC   KEY+28(4),RECADDR   CONTRACT RECORD ADDRESS                      
*                                                                               
         GOTO1 ADD                 ADD THE KEY                                  
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIOAREA                                                       
         USING REDBKEY,R6                                                       
*                                                                               
         MVI   REDBKTYP,REDBKTYQ   ADD X'0E' RECORD AS A BACKUP                 
         MVI   REDBLEN+1,34+REDBELLQ                                            
         MVI   REDBCODE,1          TO THE X'E1' KEYS                            
         MVI   REDBELLN,REDBELLQ                                                
         DROP  R6                                                               
*                                                                               
         MVC   MYSVAIO,AIO                                                      
         MVC   AIO,AIOAREA         SET ALTERNATE READ AREA                      
         GOTO1 ADDREC                                                           
*                                                                               
         MVC   AIO,MYSVAIO         RESTORE AIO BEFORE EXIT                      
*                                                                               
GENEDIX  DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
********************************************************************            
* GENERATE WORKSHEET                                                            
********************************************************************            
PR       NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*        GOTO1 =A(DUMPTSAR),RR=RELO                                             
*                                                                               
         MVC   KEY(L'SELECTKY),SELECTKY                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         MVC   AIO,AIO1                                                         
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         MVC   REVNUM,RDARRNUM                                                  
         MVC   REVDATE,RDARDATE                                                 
         MVC   REVTIME,RDARTIME                                                 
         DROP  R6                                                               
*                                                                               
         TM    PRTSTAT,PRTCLOSE    CLOSE THE PRINTQ?                            
         BNO   *+12                NO                                           
         BAS   RE,PQCLOSE                                                       
         B     PRX                 CLEAN UP AFTER PRINTING                      
*                                                                               
         TM    PRTSTAT,PRTNEWPG    PQ OPEN?                                     
         BO    *+12                                                             
         BAS   RE,PQOPEN           NO                                           
         B     *+8                                                              
         MVI   FORCEHED,C'Y'       YES - FORCEHEAD ON NEXT REPORT               
*                                                                               
         L     R1,=A(HEDSPECS)                                                  
         A     R1,RELO                                                          
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
         MVI   METHOD,ADDBUYQ                                                   
         CLI   STAMETH,0                                                        
         BE    PR08A                                                            
         CLI   STAMETH,3                                                        
         BNE   PR08B                                                            
         MVI   METHOD,CHGBUYQ                                                   
         B     PR08B                                                            
*                                                                               
PR08A    DS    0H                                                               
         LR    R4,RA               USE R2 TO COVER THE ENTRY                    
         AH    R4,=AL2(DARPROFS-CONHEADH)                                       
         USING SVDSECT,R4                                                       
*                                                                               
         TM    SVPGPBIT+CNTDRV3B,CNTDRV3A                                       
         BZ    PR08B                                                            
         MVI   METHOD,CHGBUYQ                                                   
*                                                                               
PR08B    DS    0H                                                               
         NI    PRTSTAT,X'FF'-PRTPAGE1                                           
         GOTO1 =A(PRPAGE1),RR=RELO PRINT PAGE HEADINGS                          
         OI    PRTSTAT,PRTPAGE1                                                 
*                                                                               
         XC    TSARREC,TSARREC                                                  
         MVI   TR.TLKTYP,X'0B'                                                  
         GOTO1 GOTSAR,TSARDH                                                    
         BL    PRX                                                              
*                                                                               
PR08     DS    0H                                                               
         XC    SEGSTRDT,SEGSTRDT                                                
         XC    SEGENDDT,SEGENDDT                                                
         XC    PDYTXNUM,PDYTXNUM                                                
         XC    PTMTXNUM,PTMTXNUM                                                
         XC    PBRKLINE,PBRKLINE                                                
         XC    PBRKPAGE,PBRKPAGE                                                
         XC    NUMWKS,NUMWKS       # OF WKS FOR CONTINUOUS SEGMENTS             
*                                                                               
         LA    R2,P                                                             
*                                                                               
PR09     DS    0H                                                               
         MVC   SEGSTRDT,TR.TLBYWKOF                                             
*                                                                               
PR10     DS    0H                                                               
         CLI   TR.TLKTYP,X'0B'                                                  
         BNE   PRX                                                              
*                                                                               
         CLC   PDYTXNUM,TR.TLBYDAY CHECK IF WE NEED TO BREAK ON                 
         BNE   PR15                DIFFERENT DAY/TIME                           
         CLC   PTMTXNUM,TR.TLBYTIME                                             
         BE    PR20                                                             
*                                                                               
PR15     DS    0H                                                               
         OC    PDYTXNUM,PDYTXNUM   SKIP BREAK ON FIRST PRINT LINE               
         BZ    PR18                                                             
*                                                                               
         ZIC   RE,PBRKLINE                                                      
         ZIC   RF,LINE                                                          
         SHI   RF,1                                                             
         CR    RE,RF               ALSO SKIP IF PREVIOUS LINE IS A              
         BNE   PR16                VISUAL BREAK                                 
*                                                                               
         CLC   PBRKPAGE,PAGE       MUST BE ON THE SAME PAGE                     
         BE    PR18                                                             
*                                                                               
PR16     DS    0H                                                               
         MVC   PBRKLINE,LINE                                                    
         MVC   PBRKPAGE,PAGE                                                    
         MVI   P,C'-'                                                           
         MVC   P+1(L'P-10),P                                                    
         BAS   RE,PRINT                                                         
*                                                                               
PR18     DS    0H                                                               
         MVC   PDYTXNUM,TR.TLBYDAY                                              
         MVC   PTMTXNUM,TR.TLBYTIME                                             
*                                                                               
PR20     DS    0H                                                               
         MVC   PRTXNUM,TB.TSRNUM   SAVE OFF CURRENT REC #                       
*                                                                               
         TM    FLAGS,FGDETLQ                                                    
         BO    PR400                                                            
*                                                                               
***********************************************************************         
* DISPLAY SUMMARY LISTING                                                       
***********************************************************************         
         USING DIFFSUM,R2                                                       
*                                                                               
* CHECK IF SEGMENT IS FROM A SINGLE BUY. IF IT IS, THE SEGMENT                  
* CONTAINS THE ACTUAL BUY NUMBER AND SPOT COUNT                                 
* IF NOT, WE NEED TO FIND THE LINK TO THE LIST OF BUYS AGAINST THIS             
* SEGMENT                                                                       
*                                                                               
         GOTO1 =A(GETTSPTS),RR=RELO                                             
*                                                                               
         CLC   TOTREPSP,TOTAGYSP   SKIP IF ALL SPOTS MATCHED                    
         BNE   PR30                                                             
*                                                                               
* SKIP MATCHED SPOTS FOR SUMMARY LISTING                                        
*                                                                               
         XC    0(L'P,R2),0(R2)                                                  
         GOTO1 GOTSAR,TSANXT                                                    
         BL    PRX                                                              
         CLI   TR.TLKTYP,X'0B'                                                  
         BNE   PRX                                                              
         B     PR09                                                             
*                                                                               
PR30     DS    0H                                                               
         GOTO1 =A(PGETNDX),RR=RELO  RETREIVE INDEXED ITEMS                      
*                                                                               
         EDIT  TR.TLBYLEN,PDSLEN                                                
         EDIT  TR.TLBYRATE,PDSRATE,2,COMMAS=YES,ZERO=NOBLANK                    
         MVC   SEGRATE,TR.TLBYRATE                                              
*                                                                               
PR45     DS    0H                                                               
         MVC   DLSVREC,TSARREC     SAVE OFF CURRENT BUY SEGMENT                 
         MVC   PRVREPSP,TOTREPSP   FOR LATER COMPARISON                         
         MVC   PRVAGYSP,TOTAGYSP                                                
*                                                                               
         LH    RF,NUMWKS                                                        
         AHI   RF,1                                                             
         STH   RF,NUMWKS                                                        
*                                                                               
* CHECK IF NEXT BUY SEGMENT IS A CONTINUATION FROM THIS BUY SEGMENT             
*                                                                               
PR70     DS    0H                                                               
         GOTO1 GOTSAR,TSANXT                                                    
         BL    PR75                                                             
         CLI   TR.TLKTYP,X'0B'                                                  
         BE    PR80                                                             
*                                                                               
PR75     DS    0H                                                               
         XC    PRTXNUM,PRTXNUM                                                  
         B     PR300                                                            
*                                                                               
PR80     DS    0H                                                               
PREV     USING TLSTD,DLSVREC                                                    
         CLC   PREV.TLKEY(TLBYWKOF-TLKEY),TR.TLKEY                              
         BNE   PR300                                                            
*                                                                               
* NEXT BUY SEGMENT SHOULD HAVE WEEK-OF DATE EXACTLY ONE WEEK                    
* FROM THE WEEK-OF DATE OF CURRENT BUY SEGMENT                                  
*                                                                               
         GOTO1 DATCON,DMCB,(2,PREV.TLBYWKOF),(0,WORK)                           
         LA    RF,7                                                             
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(RF)                                      
         GOTO1 DATCON,DMCB,(2,TR.TLBYWKOF),(0,WORK)                             
         CLC   WORK(6),WORK+6                                                   
         BNE   PR300                                                            
*                                                                               
* CHECK IF SPOTS ALL SPOTS MATCHED ON BOTH REP AND AGENCY                       
* IF SO, SKIP BUY SEGMENT                                                       
*                                                                               
         GOTO1 =A(GETTSPTS),RR=RELO                                             
*                                                                               
         CLC   TOTREPSP,TOTAGYSP   SKIP IF ALL SPOTS MATCHED                    
         BE    PR70                                                             
*                                                                               
* CHECK IF SPOTS COVERED BY BOTH REP AND AGENCY ARE THE SAME AS                 
* THE PREVIOUS BUY SEGMENT                                                      
*                                                                               
         CLC   PRVREPSP,TOTREPSP                                                
         BNE   PR300                                                            
         CLC   PRVAGYSP,TOTAGYSP                                                
         BNE   PR300                                                            
*                                                                               
* CHECK IF THE BUY OR LIST OF BUYS ARE THE SAME AS THE PREVIOUS                 
* BUY SEGMENT                                                                   
*                                                                               
         TM    PREV.TLBYRFLG,X'40' ACTUAL BUY OR BUY LINK?                      
         BO    PR95                                                             
         TM    TR.TLBYRFLG,X'40'                                                
         BO    PR300                                                            
         CLC   PREV.TLBYRLNK(1),TR.TLBYRLNK                                     
         BNE   PR300                                                            
*        MVC   SEGENDDT,TR.TLBYWKOF                                             
         B     PR100                                                            
*                                                                               
PR95     DS    0H                  CHECK  BUY LINK RECORD                       
         TM    TR.TLBYRFLG,X'40'                                                
         BZ    PR300                                                            
***********************************************************************         
*                                                                               
* RETRIEVE BUY LINK RECORDS FOR BOTH THE CURRENT AND PREVIOUS SEGMENTS          
*                                                                               
***********************************************************************         
         MVC   PRSVTR#,TXNUM                                                    
*                                                                               
         XC    TSARKEY,TSARKEY                                                  
TKEY     USING TLKEY,TSARKEY                                                    
         MVI   TKEY.TLKTYP,C'R'                                                 
         MVC   TKEY.TLBLINDX,TR.TLBYRLNK                                        
         DROP  TKEY                                                             
*                                                                               
         MVC   TR.TLKEY,TSARKEY                                                 
         GOTO1 GOTSAR,TSARDH       RETRIEVE BUY LNK FOR CURRENT SEGMENT         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DLSVBLRC,TSARREC                                                 
*                                                                               
         XC    TSARKEY,TSARKEY                                                  
TKEY     USING TLKEY,TSARKEY                                                    
         MVI   TKEY.TLKTYP,C'R'                                                 
         MVC   TKEY.TLBLINDX,PREV.TLBYRLNK                                      
         DROP  TKEY                                                             
*                                                                               
         MVC   TR.TLKEY,TSARKEY                                                 
         GOTO1 GOTSAR,TSARDH       RETRIEVE BUY LNK FOR PREVIOUS SEGMNT         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   TR.TLLEN,DLSVBLRC   SAME LENGTH?                                 
         BNE   PR300                                                            
*                                                                               
         LA    R0,TSARREC          CHECK IF LIST OF BUYS ARE THE SAME           
         AHI   R0,TLBLTSPT-TLLEN                                                
         ZICM  R1,TR.TLLEN,2                                                    
         SHI   R1,TLBLTSPT-TLLEN                                                
*                                                                               
         LA    RE,DLSVBLRC                                                      
         AHI   RE,TLBLTSPT-TLLEN                                                
         LR    RF,R1                                                            
         CLCL  R0,RE                                                            
         BE    PR98                                                             
*                                                                               
         MVC   TXNUM,PRSVTR#                                                    
         GOTO1 GOTSAR,TSAGET       RESTORE                                      
         BE    PR300                                                            
         DC    H'0'                                                             
*                                                                               
PR98     DS    0H                                                               
         MVC   TXNUM,PRSVTR#                                                    
         GOTO1 GOTSAR,TSAGET       RESTORE                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SEGENDDT,TR.TLBYWKOF                                             
         B     PR45                                                             
*                                                                               
* CHECK AGENCY BUYS                                                             
*                                                                               
PR100    DS    0H                                                               
         TM    PREV.TLBYAFLG,X'40' ACTUAL BUY OR BUY LINK?                      
         BO    PR110                                                            
         TM    TR.TLBYAFLG,X'40'                                                
         BO    PR300                                                            
         CLC   PREV.TLBYALNK(1),TR.TLBYALNK                                     
         BNE   PR300                                                            
         MVC   SEGENDDT,TR.TLBYWKOF                                             
         B     PR45                                                             
*                                                                               
PR110    DS    0H                  CHECK  BUY LINK RECORD                       
         TM    TR.TLBYRFLG,X'40'                                                
         BZ    PR300                                                            
***********************************************************************         
*                                                                               
* RETRIEVE BUY LINK RECORDS FOR BOTH THE CURRENT AND PREVIOUS SEGMENTS          
*                                                                               
***********************************************************************         
         MVC   PRSVTR#,TXNUM                                                    
*                                                                               
         XC    TSARKEY,TSARKEY                                                  
TKEY     USING TLKEY,TSARKEY                                                    
         MVI   TKEY.TLKTYP,C'A'                                                 
         MVC   TKEY.TLBLINDX,TR.TLBYALNK                                        
         DROP  TKEY                                                             
*                                                                               
         MVC   TR.TLKEY,TSARKEY                                                 
         GOTO1 GOTSAR,TSARDH       RETRIEVE BUY LNK FOR CURRENT SEGMENT         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DLSVBLRC,TSARREC                                                 
*                                                                               
         XC    TSARKEY,TSARKEY                                                  
TKEY     USING TLKEY,TSARKEY                                                    
         MVI   TKEY.TLKTYP,C'A'                                                 
         MVC   TKEY.TLBLINDX,PREV.TLBYALNK                                      
         DROP  TKEY                                                             
*                                                                               
         MVC   TR.TLKEY,TSARKEY                                                 
         GOTO1 GOTSAR,TSARDH       RETRIEVE BUY LNK FOR PREVIOUS SEGMNT         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   TR.TLLEN,DLSVBLRC   SAME LENGTH?                                 
         BNE   PR300                                                            
         LA    R0,TSARREC          CHECK IF LIST OF BUYS ARE THE SAME           
         AHI   R0,TLBLTSPT-TLLEN                                                
         ZICM  R1,TR.TLLEN,2                                                    
         SHI   R1,TLBLTSPT-TLLEN                                                
*                                                                               
         LA    RE,DLSVBLRC                                                      
         AHI   RE,TLBLTSPT-TLLEN                                                
         LR    RF,R1                                                            
*                                                                               
         CLCL  R0,RE                                                            
         BE    PR120                                                            
*                                                                               
         MVC   TXNUM,PRSVTR#                                                    
         GOTO1 GOTSAR,TSAGET       RESTORE                                      
         BE    PR300                                                            
         DC    H'0'                                                             
*                                                                               
PR120    DS    0H                                                               
         MVC   TXNUM,PRSVTR#                                                    
         GOTO1 GOTSAR,TSAGET       RESTORE                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SEGENDDT,TR.TLBYWKOF                                             
         B     PR45                                                             
*                                                                               
* NEXT BUY SEGMENT IS DIFFERENT. DISPLAY DATES, SPOT DIFFERENCE                 
* AND ASSOCIATED REP BUYS FOR THIS SEGMENT                                      
*                                                                               
PR300    DS    0H                                                               
         USING DIFFSUM,R2                                                       
         GOTO1 DATCON,DMCB,(2,SEGSTRDT),(3,FULL)                                
         LA    R3,PDSDATES                                                      
         EDIT  (1,FULL+1),(2,(R3)),ALIGN=LEFT                                   
         AR    R3,R0                                                            
         MVI   0(R3),C'/'                                                       
         AHI   R3,1                                                             
         EDIT  (1,FULL+2),(2,(R3)),ALIGN=LEFT                                   
         AR    R3,R0                                                            
         OC    SEGENDDT,SEGENDDT                                                
         BZ    PR330                                                            
         MVI   0(R3),C'-'                                                       
         AHI   R3,1                                                             
*                                                                               
         GOTO1 DATCON,DMCB,(2,SEGENDDT),(0,WORK)                                
         ZIC   RF,STENDAY                                                       
         SLL   RF,28               SHIFT OFF START DAY TO GET CORRECT           
         SRL   RF,28               END DAY ADDED TO WEEK START                  
*                                                                               
         ZIC   RE,STENDAY          NO NEED TO BUMP IF SAME DAY                  
         SRL   RE,4                                                             
         CR    RE,RF                                                            
         BNE   PR310                                                            
         MVC   WORK+6(6),WORK                                                   
         B     PR320                                                            
*                                                                               
PR310    DS    0H                                                               
         SR    RF,RE                                                            
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(RF)                                      
*                                                                               
PR320    DS    0H                                                               
         GOTO1 DATCON,DMCB,(0,WORK+6),(3,FULL)                                  
         EDIT  (1,FULL+1),(2,(R3)),ALIGN=LEFT                                   
         AR    R3,R0                                                            
         MVI   0(R3),C'/'                                                       
         AHI   R3,1                                                             
         EDIT  (1,FULL+2),(2,(R3)),ALIGN=LEFT                                   
         XC    SEGENDDT,SEGENDDT                                                
*                                                                               
PR330    DS    0H                                                               
         ZIC   RE,PRVAGYSP                                                      
         ZIC   RF,PRVREPSP                                                      
         SR    RE,RF                                                            
         BP    PR340                                                            
         MH    RE,NUMWKS                                                        
         EDIT  (RE),PDST#SPT,FLOAT=-                                            
         LR    RF,RE                                                            
         SR    RE,RE                                                            
         M     RE,SEGRATE                                                       
         EDIT  (RF),PDST$,2,COMMAS=YES,FLOAT=-                                  
         B     PR350                                                            
*                                                                               
PR340    DS    0H                                                               
         MH    RE,NUMWKS                                                        
         EDIT  (RE),PDST#SPT,FLOAT=+                                            
         LR    RF,RE                                                            
         SR    RE,RE                                                            
         M     RE,SEGRATE                                                       
         EDIT  (RF),PDST$,2,COMMAS=YES,FLOAT=+                                  
         DROP  R2                                                               
*                                                                               
PR350    DS    0H                                                               
         MVC   PRVREPSP,TOTREPSP   RESET FOR NEXT SEGMENT COMPARISON            
         MVC   PRVAGYSP,TOTAGYSP                                                
         XC    NUMWKS,NUMWKS       # OF WKS FOR CONTINUOUS SEGMENTS             
*                                                                               
         BAS   RE,PRINT                                                         
*                                                                               
         OC    PRTXNUM,PRTXNUM     NULL IF ALL DONE PRINTING                    
         BZ    PRX                                                              
         B     PR09                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY DETAIL LISTING                                                        
***********************************************************************         
PR400    DS    0H                                                               
         USING DIFFDET,R2                                                       
*                                                                               
         EDIT  TR.TLBYLEN,PDDLEN                                                
*                                                                               
         EDIT  TR.TLBYRATE,PDDRATE,2,COMMAS=YES,ZERO=NOBLANK                    
*                                                                               
         GOTO1 DATCON,DMCB,(2,TR.TLBYWKOF),(3,FULL)                             
         LA    R3,PDDDATE                                                       
         EDIT  (1,FULL+1),(2,(R3)),ALIGN=LEFT                                   
         AR    R3,R0                                                            
         MVI   0(R3),C'/'                                                       
         AHI   R3,1                                                             
         EDIT  (1,FULL+2),(2,(R3)),ALIGN=LEFT                                   
*                                                                               
         TM    TR.TLBYRFLG,X'40'   POINTER OR ACTUALLY BUY/SPT?                 
         BO    PR410                                                            
*                                                                               
         EDIT  (1,TR.TLBYRLNK),(3,PDDRBUY#),ALIGN=LEFT                          
         CLI   TR.TLBYRLNK,0       NO BUY LINKED?                               
         BE    PR410                                                            
         EDIT  (1,TR.TLBYRLNK+1),(3,PDDR#SPT),ALIGN=LEFT,ZERO=NOBLANK           
*                                                                               
PR410    DS    0H                                                               
         TM    TR.TLBYAFLG,X'40'   POINTER OR ACTUALLY BUY/SPT?                 
         BO    PR420                                                            
*                                                                               
         EDIT  (1,TR.TLBYALNK),(3,PDDABUY#),ALIGN=LEFT                          
         CLI   TR.TLBYALNK,0       NO BUY LINKED?                               
         BE    PR420                                                            
         EDIT  (1,TR.TLBYALNK+1),(3,PDDA#SPT),ALIGN=LEFT,ZERO=NOBLANK           
*                                                                               
PR420    DS    0H                                                               
         GOTO1 =A(PGETNDX),RR=RELO RETREIVE INDEXED ITEMS                       
*                                                                               
         CLC   PDDR#SPT,PDDA#SPT                                                
         BE    PR450                                                            
*                                                                               
* CHECK IF BOTH REP AND AGENCY HAVE NO SPOTS FOR A PARTICULAR SEGMENT           
*                                                                               
         CLI   PDDA#SPT,C'0'                                                    
         BE    PR430                                                            
         CLC   PDDA#SPT,SPACES                                                  
         BNE   PR440                                                            
*                                                                               
PR430    DS    0H                                                               
         CLI   PDDR#SPT,C'0'                                                    
         BE    PR450                                                            
         CLC   PDDR#SPT,SPACES                                                  
         BE    PR450                                                            
*                                                                               
PR440    DS    0H                                                               
         MVI   PDDIND,C'*'         INDICATE IF REP/AGY SPOTS DIFFER             
*                                                                               
PR450    DS    0H                                                               
         BAS   RE,PRINT                                                         
*                                                                               
         GOTO1 GOTSAR,TSANXT                                                    
         BNE   PRX                                                              
         CLI   TR.TLKTYP,X'0B'                                                  
         BE    PR10                                                             
*                                                                               
PRX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* REPORT CONTRACT SPECS                                                         
***********************************************************************         
HEDSPECS DS    0H                                                               
         SPROG 0                                                                
         PSPEC H1,1,AGYNAME                                                     
         PSPEC H1,56,REQUESTOR                                                  
         PSPEC H1,120,PAGE                                                      
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
* CLOSE PRINTQ                                                                  
***********************************************************************         
PQCLOSE  NTR1                                                                   
         MVI   SPMODE,X'FF'                                                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
***********************************************************************         
* OPEN PRINTQ AND SET HEADSPECS/HOOK                                            
***********************************************************************         
PQOPEN   NTR1                                                                   
         OI    GENSTAT3,NOCLRSPK                                                
         MVC   REMUSER,=C'DAO'     DARE AGENCY ORDER                            
*                                                                               
         OC    REQINIT,REQINIT     CHECK IF OVERRIDE REPORT NAME                
         BZ    *+10                                                             
         MVC   REMUSER,REQINIT                                                  
*&&DB                                                                           
         MVC   REMUSER,=C'DBR'     DEBUG REPORT                                 
*&&                                                                             
         LA    RF,SPOOLKEY                                                      
         USING PQPLD,RF                                                         
         XC    SPOOLKEY,SPOOLKEY                                                
         MVC   PLDESC,=C'WORKSHEET  '                                           
         MVI   PLCLASS,C' '                                                     
         OI    SPOOLIND,SPUINIT    PERMITS SETTING OF CLASS                     
         MVC   PLSUBID,=C'DAO'     DARE AGENCY ORDER                            
*                                                                               
         OC    REQINIT,REQINIT     CHECK IF OVERRIDE REPORT NAME                
         BZ    *+10                                                             
         MVC   REMUSER,REQINIT                                                  
*&&DB                                                                           
         MVC   PLSUBID,=C'DBR'     DEBUG REPORT                                 
*&&                                                                             
         DROP  RF                                                               
*                                                                               
         LA    RE,SPLKEYAD                                                      
         ST    RE,SPOOLQLK         SET EXTENDED KEY ADDRESS                     
         USING PQPLD,RE                                                         
         XC    0(133,RE),0(RE)                                                  
         MVI   QLEXTRA,X'FF'                                                    
         MVC   QLRETNL,=H'6'                                                    
         MVC   QLRETND,=H'2'                                                    
         MVC   QLRETNL,=H'36'                                                   
         MVI   QLSYS,C'R'          FORM/COPIES COLUMN                           
         MVC   QLPRG,=C'CO'                                                     
         DROP  RE                                                               
*                                                                               
         GOTO1 OPENPQ                                                           
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* REPORT HEADHOOK ROUTINE                                                       
***********************************************************************         
HOOK     NTR1  BASE=*,LABEL=*                                                   
         GOTO1 DATCON,DMCB,(5,0),(8,H1+90)                                      
*                                                  GET TODAY'S DATE             
         THMS  DDSTIME=YES                         GET CURRENT TIME             
         ST    R1,DUB                                                           
         ZAP   PRTTIME,DUB(4)                                                   
         ST    R0,DUB              ADDJUST FOR DDS TIME                         
         AP    PRTTIME,DUB(4)                                                   
*                                                                               
         MVC   H1+99(2),=C'AT'                                                  
         UNPK  DUB,PRTTIME                                                      
         MVC   H1+102(2),DUB+2     TIME                                         
         MVI   H1+104,C'.'                                                      
         MVC   H1+105(2),DUB+4                                                  
*                                                                               
         MVC   H2+42(26),=C'****** DARE AGENCY SUMMARY'                         
*                                                                               
         MVC   H2+69(22),=C'REVISION REPORT ******'                             
*                                                                               
         TM    FLAGS,FGDETLQ                                                    
         BZ    HOOK10                                                           
         MVC   H2+61(7),=C'DETAIL '                                             
         MVC   H2+68(22),=C'REVISION REPORT ******'                             
*                                                                               
HOOK10   DS    0H                                                               
         MVC   H3+32(27),=C'REVISION PROCESSING METHOD:'                        
         CLI   METHOD,ADDBUYQ                                                   
         BNE   *+10         1234567890123456789012345678901234567890            
         MVC   H3+60(37),=C'ADD LINE FOR INCREASE IN SPOTS/FLIGHT'              
         CLI   METHOD,CHGBUYQ                                                   
         BNE   *+10         12345678901234567890                                
         MVC   H3+60(20),=C'CHANGE EXISTING LINE'                               
*                                                                               
         MVC   H4(010),=C'CONTRACT #'                                           
         ZAP   WORK+20(5),=P'0'                                                 
         MVO   WORK+20(5),CCONKNUM                                              
         EDIT  (P5,WORK+20),(8,H4+11),ALIGN=LEFT                                
*                                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         MVC   H4+56(012),=C'DARE ORDER #'                                      
         ZAP   WORK+20(5),=P'0'                                                 
         MVO   WORK+20(5),RDARKORD                                              
         EDIT  (P5,WORK+20),(8,H4+70),ALIGN=LEFT,ZERO=NOBLANK,FILL=0            
         DROP  R6                                                               
*                                                                               
         MVC   H4+118(10),=C'REVISION #'                                        
         EDIT  REVNUM,(3,H4+129),ALIGN=LEFT,ZERO=NOBLANK,FILL=0                 
*                                                                               
         TM    PRTSTAT,PRTPAGE1                                                 
         BNO   HOOKX                                                            
*                                                                               
*----------------*                                                              
* REP AGENCY                                                                    
*----------------*                                                              
         MVC   H5+56(L'CCONKAGY),CCONKAGY                                       
*                                                                               
         LA    RE,H5+56                                                         
         CLI   0(RE),C' '                                                       
         BNH   *+12                                                             
         LA    RE,1(RE)                                                         
         B     *-12                                                             
*                                                                               
         CLC   CCONKAOF,SPACES     OFFICE?                                      
         BE    HOOK20              NO                                           
         MVI   0(RE),C'-'                                                       
         MVC   1(2,RE),CCONKAOF    AGENCY OFFICE                                
*                                                                               
HOOK20   DS    0H                                                               
         MVC   H5+70(L'EAGYNAM1),EAGYNAM1   AGENCY EXPANSION                    
*----------------*                                                              
* REP ADVERTISER                                                                
*----------------*                                                              
         MVC   H6+56(4),CCONKADV                                                
         MVC   H6+70(L'EADVNAME),EADVNAME                                       
*-------------*                                                                 
* REP PRODUCT                                                                   
*-------------*                                                                 
         MVC   H7+56(3),CCONPRD                                                 
         MVC   H7+70(20),EPRDNAME                                               
*                                                                               
         MVI   H6,C'*'             DIVIDER                                      
         MVC   H6+1(L'H6-1),H6                                                  
*                                  COLUMN HEADINGS                              
         MVC   H7(132),BUYTITL1                                                 
         MVC   H8(132),BUYTITL2                                                 
*                                                                               
         TM    FLAGS,FGDETLQ                                                    
         BZ    HOOK30                                                           
*                                                                               
         MVC   H7(132),DBUYTIT1                                                 
         MVC   H8(132),DBUYTIT2                                                 
*                                                                               
HOOK30   DS    0H                                                               
         MVI   H9,C'-'                                                          
         MVC   H9+1(L'H9-1),H9                                                  
*                                                                               
HOOKX    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PRINT PAGE1 "HEADINGS"                                                        
*   CONTRACT DETAILS ARE SET                                                    
*   DARE RECORD IS IN AIO                                                       
***********************************************************************         
PRPAGE1  NTR1  BASE=*,LABEL=*                                                   
*----------------------*                                                        
* REP STATION + MARKET                                                          
*----------------------*                                                        
         XC    WORK,WORK                                                        
         MVC   WORK(4),CCONKSTA                                                 
         MVC   WORK+4(3),=C'- M'                                                
         CLI   CCONKSTA+4,C' '                                                  
         BNE   *+14                                                             
         MVC   WORK+5(2),=C'TV'                                                 
         B     PRSTA02                                                          
*                                                                               
         MVC   WORK+5(1),CCONKSTA+4                                             
         CLI   CCONKSTA+4,C'L'                                                  
         BNE   *+8                                                              
         MVI   WORK+6,C' '                                                      
*                                                                               
PRSTA02  DS    0H                                                               
         MVC   P1(7),WORK                                                       
         CLI   P1+4,C' '                                                        
         BNE   *+10                                                             
         MVC   P1+4(3),WORK+5                                                   
*                                                                               
         MVC   P1+10(20),EMKTNAME                                               
*------------*                                                                  
* REP AGENCY                                                                    
*------------*                                                                  
         MVC   P1+56(L'CCONKAGY),CCONKAGY                                       
         CLC   CCONKAOF,SPACES     OFFICE?                                      
         BE    PRAGY02             NO                                           
*                                                                               
         LA    RE,P1+56                                                         
         CLI   0(RE),C' '                                                       
         BNH   *+12                                                             
         LA    RE,1(RE)                                                         
         B     *-12                                                             
         MVI   0(RE),C'-'                                                       
         MVC   1(2,RE),CCONKAOF    AGENCY OFFICE                                
*                                                                               
PRAGY02  DS    0H                                                               
         MVC   P1+70(20),EAGYNAM1  AGENCY EXPANSION                             
*------------------------*                                                      
* STATION TRAFFIC NUMBER                                                        
*------------------------*                                                      
         MVC   P2(13),=C'STA TRAFFIC #'                                         
         L     R6,AIO3                                                          
         MVI   ELCODE,X'1F'                                                     
         BRAS  RE,GETEL                                                         
         BNE   PRADV10                                                          
         USING RCONXEL,R6                                                       
         MVC   P2+14(10),RCONTRF                                                
         DROP  R6                                                               
*                                                                               
*----------------*                                                              
* REP ADVERTISER                                                                
*----------------*                                                              
PRADV10  DS    0H                                                               
         MVC   P2+56(4),CCONKADV                                                
         MVC   P2+70(20),EADVNAME                                               
*-----------*                                                                   
* AGY BUYER                                                                     
*-----------*                                                                   
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         MVC   WORK(8),=C'BUYER - '                                             
         MVC   WORK+8(20),RDARBUYR                                              
         LA    R1,20                                                            
         LA    RE,WORK+27                                                       
PRBUY10  CLI   0(RE),C' '                                                       
         BNE   PRBUY20                                                          
         BCTR  R1,0                                                             
         BCT   RE,PRBUY10                                                       
PRBUY20  LA    RF,P1+131                                                        
         AHI   R1,7                OVERHEAD                                     
         SR    RF,R1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),WORK                                                     
*--------------*                                                                
* REP ESTIMATE                                                                  
*--------------*                                                                
         MVC   P2+122(5),=C'EST#:'                                              
         MVC   P2+128(4),CCONIEST                                               
         CLC   CCONIEST,SPACES                                                  
         BNE   *+10                                                             
         MVC   P2+128(10),CCONXEST                                              
*----------------------------*                                                  
* REP OFFICE AND SALESPERSON                                                    
*------------*---------------*                                                  
         MVC   P3(2),CCONKOFF                                                   
         MVC   P3+3(3),CCONSAL                                                  
         MVC   P3+7(20),ESALNAME                                                
*                                                                               
*-------------*                                                                 
* REP PRODUCT                                                                   
*-------------*                                                                 
         MVC   P3+56(3),CCONPRD                                                 
         MVC   P3+70(20),EPRDNAME                                               
*------------------*                                                            
* REP FLIGHT DATES                                                              
*------------------*                                                            
         MVC   P3+115(17),ECONDATE                                              
*                                                                               
         BAS   RE,PRINT                                                         
*                                                                               
         BAS   RE,PRINT                                                         
*                                                                               
*-----------------------------*                                                 
* REP AND AGENCY ORDER TOTALS                                                   
*-----------------------------*                                                 
*                                                                               
         OI    FLAGS2,TOTALPRC                                                  
         GOTO1 =A(DISTOTAL),RR=RELO                                             
*                                                                               
         MVC   P1(25),=C'REP CONTRACT TOTAL: SPOTS'                             
         EDIT  RPSPTTOT,(4,P1+26),ZERO=NOBLANK                                  
         EDIT  RPORDTOT,(16,P1+34),2,ZERO=NOBLANK,COMMAS=YES,FLOAT=$            
*                                                                               
         MVC   P2(25),=C'AGENCY ORDER TOTAL: SPOTS'                             
         EDIT  GSPT#,(4,P2+26),ZERO=NOBLANK                                     
*        EDIT  AGSPTTOT,(4,P2+26),ZERO=NOBLANK                                  
         EDIT  GTOTAL$,(16,P2+34),2,ZERO=NOBLANK,COMMAS=YES,FLOAT=$             
*        EDIT  AGORDTOT,(16,P2+34),2,ZERO=NOBLANK,COMMAS=YES,FLOAT=$            
*                                                                               
         MVC   P2+70(17),=C'DIFFERENCE: SPOTS'                                  
         ZICM  RE,GSPT#,4                                                       
*        ZICM  RE,AGSPTTOT,2                                                    
         ZICM  RF,RPSPTTOT,2                                                    
         SR    RE,RF                                                            
         BP    PRTOT10                                                          
         EDIT  (RE),(5,P2+88),FLOAT=-,ZERO=NOBLANK                              
         B     PRTOT20                                                          
*                                                                               
PRTOT10  DS    0H                                                               
         EDIT  (RE),(5,P2+88),FLOAT=+                                           
*                                                                               
PRTOT20  DS    0H                                                               
         MVI   P2+96,C'+'                                                       
         ZICM  RE,GTOTAL$,4                                                     
*        ZICM  RE,AGORDTOT,4                                                    
         ZICM  RF,RPORDTOT,4                                                    
         SR    RE,RF                                                            
         BP    PRTOT30                                                          
         MVI   P2+96,C'-'                                                       
         LTR   RE,RE                                                            
         BNZ   PRTOT30                                                          
         MVI   P2+96,C' '                                                       
*                                                                               
PRTOT30  DS    0H                                                               
         EDIT  (RE),(16,P2+97),2,ALIGN=LEFT,ZERO=NOBLANK,COMMAS=YES,   X        
               FLOAT=$                                                          
*                                                                               
         BAS   RE,PRINT                                                         
         BAS   RE,PRINT                                                         
*----------------*                                                              
* AGENCY COMMENTS                                                               
*----------------*                                                              
         MVC   MYSVAIO,AIO                                                      
         MVC   AIO,AIO2                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(RDARKRT-RDARKEY),SELECTKY                                    
         LA    R6,KEY                                                           
         USING RDARKEY,R6                                                       
         MVI   RDARKRT,X'20'       TYPE STANDARD COMMENT                        
         DROP  R6                                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
         BNE   PRAGCM20            IF STANDARD COMMENT                          
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         BAS   RE,PRTSTCMT                                                      
*                                                                               
PRAGCM20 DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(RDARKRT-RDARKEY),SELECTKY                                    
         LA    R6,KEY                                                           
         USING RDARKEY,R6                                                       
         MVI   RDARKRT,X'30'       TYPE ORDER COMMENT                           
         DROP  R6                                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
         BNE   PRRJCM10            IF ORDER COMMENT                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         BAS   RE,PRTORCMT                                                      
                                                                                
PRRJCM10 DS    0H                  PRINT REJECTION CMTS IF REQUESTED            
         XC    KEY,KEY                                                          
         MVC   KEY(RDARKRT-RDARKEY),SELECTKY                                    
         LA    R6,KEY                                                           
         USING RDARKEY,R6                                                       
         MVI   RDARKRT,X'10'       REJECT CMT IS IN THE HEADER                  
         DROP  R6                                                               
                                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
         BNE   PRRJCM20                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         BAS   RE,PRTREJCT                                                      
                                                                                
PRRJCM20 DS    0H                                                               
         MVC   AIO,MYSVAIO                                                      
*                                                                               
*----------*                                                                    
* HEADINGS                                                                      
*----------*                                                                    
         MVC   P1(SUMTITLQ),SUMTITLE                                            
         MVI   P2,C'-'                                                          
         MVC   P2+1(L'P2-2),P2                                                  
*                                                                               
         TM    FLAGS,FGDETLQ                                                    
         BZ    PRHEAD10                                                         
         MVC   P1(7),=C'DETAILS'                                                
*                                                                               
PRHEAD10 DS    0H                                                               
         MVC   P3(132),BUYTITL1                                                 
         MVC   P4(132),BUYTITL2                                                 
*                                                                               
         TM    FLAGS,FGDETLQ                                                    
         BZ    PRHEAD20                                                         
*                                                                               
         MVC   P3(132),DBUYTIT1                                                 
         MVC   P4(132),DBUYTIT2                                                 
*                                                                               
PRHEAD20 DS    0H                                                               
         BAS   RE,PRINT                                                         
*                                                                               
         MVI   P1,C'-'                                                          
         MVC   P1+1(L'P1-2),P1                                                  
*                                                                               
         BAS   RE,PRINT                                                         
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PRINT STANDARD COMMENTS                                                       
***********************************************************************         
PRTSTCMT NTR1                                                                   
         BAS   RE,PRINT                                                         
         MVC   P(24),=C'AGENCY STANDARD COMMENT:'                               
         BAS   RE,PRINT                                                         
                                                                                
PRTST10  DS    0H                                                               
         L     R6,AIO                                                           
         USING RDARSCEL,R6                                                      
         MVI   ELCODE,2                                                         
         BRAS  RE,GETEL                                                         
         BNE   PRTSTX                                                           
                                                                                
PRTST20  DS    0H                                                               
         ZIC   R1,RDARSCLN                                                      
         CLI   RDARSCLN,3                                                       
         BL    *+8                                                              
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+7(0),RDARSCCM                                                  
         BAS   RE,PRINT                                                         
         BRAS  RE,NEXTEL                                                        
         BE    PRTST20                                                          
         DROP  R6                                                               
                                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
                                                                                
         CLC   KEY(RDARKSEQ-RDARKEY),KEYSAVE                                    
         BNE   PRTSTX                                                           
                                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         B     PRTST10                                                          
                                                                                
PRTSTX   DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PRINT ORDER COMMENTS                                                          
***********************************************************************         
PRTORCMT NTR1                                                                   
         BAS   RE,PRINT                                                         
         MVC   P(21),=C'AGENCY ORDER COMMENT:'                                  
         BAS   RE,PRINT                                                         
                                                                                
PRTOR10  DS    0H                                                               
         L     R6,AIO                                                           
         USING RDAROREL,R6                                                      
         MVI   ELCODE,2                                                         
         BRAS  RE,GETEL                                                         
         BNE   PRTORX                                                           
                                                                                
PRTOR20  DS    0H                                                               
         ZIC   R1,RDARORLN                                                      
         CLI   RDARORLN,3                                                       
         BL    *+8                                                              
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+7(0),RDARORCM                                                  
         BAS   RE,PRINT                                                         
         BRAS  RE,NEXTEL                                                        
         BE    PRTOR20                                                          
                                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
                                                                                
         CLC   KEY(RDARKSEQ-RDARKEY),KEYSAVE                                    
         BNE   PRTORX                                                           
                                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         B     PRTOR10                                                          
                                                                                
PRTORX   DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT REJECTION COMMENTS                                                      
***********************************************************************         
PRTREJCT NTR1                                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BNE   PRTREJX                                                          
         USING RDARRCEM,R6                                                      
                                                                                
         BAS   RE,PRINT                                                         
         MVC   P(22),=C'REP REJECTION COMMENT:'                                 
         BAS   RE,PRINT                                                         
                                                                                
PRTREJ10 DS    0H                                                               
         CLI   RDARRCEN,2                                                       
         BNH   PRTREJ20                                                         
         ZIC   R1,RDARRCEN                                                      
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+7(0),RDARRCCM                                                  
                                                                                
PRTREJ20 DS    0H                                                               
         BAS   RE,PRINT                                                         
         BRAS  RE,NEXTEL                                                        
         BE    PRTREJ10                                                         
                                                                                
PRTREJX  DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* RETRIEVE INDEXED ITEMS: DAY/TIME, PROGRAM NAME AND BUY LINKS, IF ANY          
* R2 ALREADY POINTS TO LIST LINE                                                
***********************************************************************         
PGETNDX  NTR1  BASE=*,LABEL=*                                                   
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         MVC   GNXSVREC,TSARREC    SAVE OFF CURRENT TSAR RECORD                 
         MVC   SVTSAR#,TXNUM       AND TSAR RECORD NUMBER                       
BUYSEG   USING TLSTD,GNXSVREC                                                   
*                                                                               
* DAY(S)                                                                        
*                                                                               
         XC    TSARREC,TSARREC                                                  
         MVC   TXNUM,BUYSEG.TLBYDAY RETRIEVE DAY(S)                             
         GOTO1 GOTSAR,TSAGET                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   GNXSVDRC,TSARREC    SAVE OFF DAYS RECORD                         
TRDAYS   USING TLSTD,GNXSVDRC                                                   
*                                                                               
* TIME(S)                                                                       
*                                                                               
         XC    TSARREC,TSARREC                                                  
         MVC   TXNUM,BUYSEG.TLBYTIME RETRIEVE TIME(S)                           
         GOTO1 GOTSAR,TSAGET                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
*                                                                               
         LA    R3,TRDAYS.TLDYSTEN                                               
         LA    R4,TR.TLTMSTRT                                                   
*                                                                               
PGN10    DS    0H                  IF SINGLE DAY, WE NEED                       
         TM    0(R3),X'F0'         TO PUT THE START DAY                         
         BNO   PGN15               BACK IN THE FIRST NIBBLE                     
         ZIC   R1,0(R3)                                                         
         SLL   R1,4                                                             
         STC   R1,HALF                                                          
         MVN   HALF(1),0(R3)                                                    
         MVC   0(1,R3),HALF                                                     
*                                                                               
PGN15    DS    0H                                                               
         GOTO1 VOUTDAY,DMCB,1(R3),0(R3),0(R6)                                   
         MVC   STENDAY,0(R3)                                                    
*                                                                               
PGN20    DS    0H                                                               
         CLI   0(R6),0                                                          
         BE    PGN30                                                            
         CLI   0(R6),C' '                                                       
         BE    PGN30                                                            
         AHI   R6,1                                                             
         LA    RF,ELEM+L'ELEM                                                   
         CR    R6,RF                                                            
         BH    PGN100                                                           
         B     PGN20                                                            
*                                                                               
PGN30    DS    0H                                                               
         MVI   0(R6),C'/'                                                       
*        OC    2(L'TLTMENDT,R4),2(R4)                                           
*        BNZ   *+10                IF SINGLE TIME                               
*        MVC   2(L'TLTMENDT,R4),0(R4)                                           
         GOTO1 UNTIME,DMCB,0(R4),1(R6)                                          
*                                                                               
PGN40    DS    0H                                                               
         CLI   0(R6),0                                                          
         BE    PGN50                                                            
         CLI   0(R6),C' '                                                       
         BE    PGN50                                                            
         AHI   R6,1                                                             
         LA    RF,ELEM+L'ELEM                                                   
         CR    R6,RF                                                            
         BH    PGN100                                                           
         B     PGN40                                                            
*                                                                               
PGN50    DS    0H                                                               
         AHI   R3,L'TRDAYS.TLDYSTEN+L'TRDAYS.TLDYDAYS                           
         AHI   R4,L'TR.TLTMSTRT+L'TR.TLTMENDT                                   
         CLI   0(R3),0                                                          
         BE    PGN100                                                           
         MVI   0(R6),C','                                                       
         AHI   R6,1                                                             
         B     PGN10                                                            
*                                                                               
PGN100   DS    0H                                                               
         TM    FLAGS,FGDETLQ       SUMMARY OR DETAIL?                           
         BO    PGN120                                                           
*                                                                               
* DAY/TIMES DISPLAY                                                             
*                                                                               
         USING DIFFSUM,R2                                                       
*                                                                               
         CLI   ELEM+L'PDSDYTM,0    DOES EVERYTHING FIT?                         
         BNE   PGN112                                                           
         MVC   PDSDYTM(L'PDSDYTM),ELEM                                          
         B     PGN200                                                           
*                                                                               
PGN112   DS    0H                                                               
         LA    R1,4                AT MOST 4 PRINT LINES                        
         LR    R3,R2                                                            
         LA    RE,ELEM                                                          
*                                                                               
PGN112A  DS    0H                                                               
         LA    RF,L'PDSDYTM(RE)                                                 
         CLI   0(RF),0                                                          
         BE    PGN118                                                           
*                                                                               
LINED    USING DIFFSUM,R3                                                       
         CLI   0(RF),C','                                                       
         BE    PGN115                                                           
*                                                                               
PGN113   DS    0H                                                               
         SHI   RF,1                                                             
         CLI   0(RF),C','                                                       
         BE    PGN114                                                           
         CR    RF,RE                                                            
         BH    PGN113                                                           
         B     PGN118                                                           
*                                                                               
PGN114   DS    0H                                                               
         SR    RF,RE                                                            
         EX    RF,*+8                                                           
         B     PGN117                                                           
         MVC   LINED.PDSDYTM(0),0(RE)                                           
*                                                                               
PGN115   DS    0H                                                               
         MVC   LINED.PDSDYTM(L'PDSDYTM),0(RE)                                   
*                                                                               
PGN117   DS    0H                                                               
         LA    RE,1(RE,RF)                                                      
         AHI   R3,132              NEXT PRINT LINE                              
         BCT   R1,PGN112A                                                       
         B     PGN200                                                           
*                                                                               
PGN118   DS    0H                                                               
         MVC   LINED.PDSDYTM(L'PDSDYTM),0(RE)                                   
         B     PGN200                                                           
*                                                                               
PGN120   DS    0H                  DETAIL DISPLAY                               
         USING DIFFDET,R2                                                       
*                                                                               
         CLI   ELEM+L'PDDDYTM,0    DOES EVERYTHING FIT?                         
         BNE   PGN122                                                           
         MVC   PDDDYTM(L'PDDDYTM),ELEM                                          
         B     PGN200                                                           
*                                                                               
PGN122   DS    0H                                                               
         LA    R1,4                AT MOST 4 PRINT LINES                        
         LR    R3,R2                                                            
         LA    RE,ELEM                                                          
*                                                                               
PGN122A  DS    0H                                                               
         LA    RF,L'PDDDYTM(RE)                                                 
         CLI   0(RF),0                                                          
         BE    PGN128                                                           
*                                                                               
LINED    USING DIFFDET,R3                                                       
         CLI   0(RF),C','                                                       
         BE    PGN125                                                           
*                                                                               
PGN123   DS    0H                                                               
         SHI   RF,1                                                             
         CLI   0(RF),C','                                                       
         BE    PGN124                                                           
         CR    RF,RE                                                            
         BH    PGN123                                                           
         B     PGN128                                                           
*                                                                               
PGN124   DS    0H                                                               
         SR    RF,RE                                                            
         EX    RF,*+8                                                           
         B     PGN127                                                           
         MVC   LINED.PDDDYTM(0),0(RE)                                           
*                                                                               
PGN125   DS    0H                                                               
         MVC   LINED.PDDDYTM(L'PDDDYTM),0(RE)                                   
*                                                                               
PGN127   DS    0H                                                               
         LA    RE,1(RE,RF)                                                      
         AHI   R3,132              NEXT PRINT LINE                              
         BCT   R1,PGN122A                                                       
         B     PGN200                                                           
*                                                                               
PGN128   DS    0H                                                               
         MVC   LINED.PDDDYTM(L'PDDDYTM),0(RE)                                   
*                                                                               
*                                                                               
* PROGRAM NAME                                                                  
*                                                                               
PGN200   DS    0H                  DETAIL DISPLAY                               
         OC    BUYSEG.TLBYPROG,BUYSEG.TLBYPROG                                  
         BZ    PGN300                                                           
         MVC   TXNUM,BUYSEG.TLBYPROG RETRIEVE PROGRAM NAME                      
         GOTO1 GOTSAR,TSAGET                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    FLAGS,FGDETLQ       SUMMARY OR DETAIL?                           
         BO    PGN220                                                           
*                                  SUMMARY DISPLAY                              
         USING DIFFSUM,R2                                                       
         LR    R3,R2                                                            
         LA    RE,TR.TLPGNAME                                                   
         ZICM  R1,TR.TLLEN,2                                                    
         SHI   R1,3                                                             
         CHI   R1,L'PDSPGM         CHECK IF PROGRAM NAME FITS ON 1 LINE         
         BNH   PGN210                                                           
*                                                                               
         MVC   PDSPGM,TR.TLPGNAME                                               
         AHI   R3,132                                                           
         AHI   RE,L'PDSPGM                                                      
         SHI   R1,L'PDSPGM                                                      
*                                                                               
PGN210   DS    0H                  SPLIT TO SECOND LINE, IF NEED BE             
LINED    USING DIFFSUM,R3                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     PGN300                                                           
         MVC   LINED.PDSPGM(0),0(RE)                                            
         DROP  LINED                                                            
*                                                                               
PGN220   DS    0H                  DETAIL DISPLAY                               
         USING DIFFDET,R2                                                       
         LR    R3,R2                                                            
         LA    RE,TR.TLPGNAME                                                   
         ZICM  R1,TR.TLLEN,2                                                    
         SHI   R1,3                                                             
         CHI   R1,L'PDDPGM         CHECK IF PROGRAM NAME FITS ON 1 LINE         
         BNH   PGN230                                                           
*                                                                               
         MVC   PDDPGM,TR.TLPGNAME                                               
         AHI   R3,132                                                           
         AHI   RE,L'PDDPGM                                                      
         SHI   R1,L'PDDPGM                                                      
*                                                                               
PGN230   DS    0H                  SPLIT TO SECOND LINE, IF NEED BE             
LINED    USING DIFFDET,R3                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     PGN300                                                           
         MVC   LINED.PDDPGM(0),0(RE)                                            
         DROP  LINED                                                            
*                                                                               
*                                                                               
* REP/AGY BUY LINKS IF MORE THAN ONE BUY WERE LINKED TO THIS SEGMENT            
*                                                                               
PGN300   DS    0H                                                               
         TM    BUYSEG.TLBYRFLG,X'40' REP BUY LINK?                              
         BO    PGN310                                                           
         TM    FLAGS,FGDETLQ       SUMMARY OR DETAIL?                           
         BO    PGN350                                                           
***********************************************************************         
* DISPLAY FOR SUMMARY SINGLE REP BUY                                            
***********************************************************************         
         USING DIFFSUM,R2                                                       
         EDIT  (1,BUYSEG.TLBYRLNK),(3,PDSRBUY#),ALIGN=LEFT                      
         CLI   BUYSEG.TLBYRLNK,0                                                
         BNE   PGN340                                                           
         MVC   PDSRBUY#(3),=C'ADD'                                              
         B     PGN340                                                           
         DROP  R2                                                               
***********************************************************************         
* DISPLAY FOR SUMMARY MULTIPLE REP BUY USING BUY LINK RECORD                    
***********************************************************************         
PGN310   DS    0H                                                               
         XC    TSARREC,TSARREC                                                  
         MVI   TR.TLKTYP,C'R'                                                   
         MVC   TR.TLBLINDX,BUYSEG.TLBYRLNK                                      
         GOTO1 GOTSAR,TSARDH       RETRIEVE BUY LINK RECORD                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    FLAGS,FGDETLQ       SUMMARY OR DETAIL?                           
         BZ    PGN330                                                           
*                                                                               
         USING DIFFDET,R2                                                       
         EDIT  TR.TLBLTSPT,PDDR#SPT,ALIGN=LEFT,ZERO=NOBLANK                     
         LA    R3,TR.TLBLBUY#                                                   
         LA    R4,PDDRBUY#                                                      
         LA    R1,PDDRBUY#                                                      
         B     PGN335                                                           
*                                                                               
PGN330   DS    0H                                                               
         USING DIFFSUM,R2                                                       
         LA    R3,TR.TLBLBUY#                                                   
         LA    R4,PDSRBUY#                                                      
         LA    R1,PDSRBUY#                                                      
*                                                                               
* EXPLODE ALL LINKED REP BUYS TO MAX OF 4 PRINT LINES                           
*                                                                               
PGN335   DS    0H                                                               
         EDIT  (1,0(R3)),(3,0(R4)),ALIGN=LEFT                                   
         AHI   R3,L'TLBLBUY#+L'TLBL#SPT                                         
         CLI   0(R3),0                                                          
         BE    PGN340                                                           
         AR    R4,R0                                                            
         MVI   0(R4),C','                                                       
         AHI   R4,1                                                             
         LA    RE,L'PDSRBUY#-3(R1)                                              
         CR    R4,RE                                                            
         BL    PGN335                                                           
         AHI   R1,132              LIST OF BUYS DO NOT FIT ON ONE LINE          
         LR    R4,R1                                                            
         B     PGN335                                                           
*                                                                               
* SUMMARY: NUMBER OF SPOTS PER WEEK DIFFERENCE                                  
*                                                                               
PGN340   DS    0H                                                               
         TM    FLAGS,FGDETLQ       SUMMARY OR DETAIL?                           
         BO    PGN350                                                           
*                                                                               
         ZIC   RE,TOTAGYSP                                                      
         ZIC   RF,REPSPWK                                                       
         SR    RE,RF                                                            
         BP    PGN345                                                           
         EDIT  (RE),PDS#SPT,FLOAT=-,ZERO=NOBLANK                                
         B     PGN350                                                           
*                                                                               
PGN345   DS    0H                                                               
         EDIT  (RE),PDS#SPT,FLOAT=+                                             
         DROP  R2                                                               
*                                                                               
PGN350   DS    0H                                                               
         TM    BUYSEG.TLBYAFLG,X'40' AGY BUY LINK?                              
         BZ    PGNX                                                             
         TM    FLAGS,FGDETLQ       SUMMARY OR DETAIL?                           
         BZ    PGNX                                                             
*                                                                               
         XC    TSARREC,TSARREC                                                  
         MVI   TR.TLKTYP,C'A'                                                   
         MVC   TR.TLBLINDX,BUYSEG.TLBYALNK                                      
         GOTO1 GOTSAR,TSARDH       RETRIEVE BUY LINK RECORD                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
***********************************************************************         
* DISPLAY FOR DETAIL MULTIPLE AGY BUY USING BUY LINK RECORD                     
***********************************************************************         
         USING DIFFDET,R2                                                       
         EDIT  TR.TLBLTSPT,PDDA#SPT,ALIGN=LEFT                                  
*                                                                               
         LA    R3,TR.TLBLBUY#                                                   
         LA    R4,PDDABUY#                                                      
         LA    R1,PDDABUY#                                                      
*                                                                               
* EXPLODE ALL LINKED REP BUYS TO MAX OF 4 PRINT LINES                           
*                                                                               
PGN435   DS    0H                                                               
         EDIT  (1,0(R3)),(3,0(R4)),ALIGN=LEFT                                   
         AHI   R3,L'TLBLBUY#+L'TLBL#SPT                                         
         CLI   0(R3),0                                                          
         BE    PGNX                                                             
         AR    R4,R0                                                            
         MVI   0(R4),C','                                                       
         AHI   R4,1                                                             
         LA    RE,L'PDDABUY#-3(R1)                                              
         CR    R4,RE                                                            
         BL    PGN435                                                           
         AHI   R1,132              LIST OF BUYS DO NOT FIT ON ONE LINE          
         LR    R4,R1                                                            
         B     PGN435                                                           
*                                                                               
PGNX     DS    0H                                                               
         MVC   TXNUM,SVTSAR#       RESTORE SEQUENCE                             
         GOTO1 GOTSAR,TSAGET                                                    
         BE    EXIT                                                             
         DC    H'0'                                                             
         DROP  BUYSEG,TRDAYS,R2                                                 
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE REPFACSQ                                                       
       ++INCLUDE RECNTPROF                                                      
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE FATIOB                                                         
       ++INCLUDE REDARFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE REDARF8D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE REDARE8D                                                       
         EJECT                                                                  
       ++INCLUDE REDARTWA                                                       
         EJECT                                                                  
       ++INCLUDE REDARWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REDARDSECT                                                     
         EJECT                                                                  
RSTARECD DSECT                                                                  
       ++INCLUDE REGENSTA                                                       
         EJECT                                                                  
RREPRECD DSECT                                                                  
       ++INCLUDE REGENREPA                                                      
         EJECT                                                                  
REDIRECD DSECT                                                                  
       ++INCLUDE REGENEDI                                                       
         EJECT                                                                  
RECFCD   DSECT                                                                  
       ++INCLUDE REGENCFC                                                       
         EJECT                                                                  
       ++INCLUDE DMPRTQL                                                        
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
EDICTD   DSECT                                                                  
       ++INCLUDE EDIDDSHD                                                       
         EJECT                                                                  
       ++INCLUDE EDILINKD                                                       
         EJECT                                                                  
         PRINT ON                                                               
*                                                                               
* SYSSPARE SAVED VARIABLES                                                      
*                                                                               
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
VTSAR    DS    A                                                                
ATSARBUF DS    A                   ADDRESS OF TSAR BUFFER TO USE                
*                                                                               
FLAGS    DS    X                                                                
FGINITQ EQU    X'80'                                                            
* X'80' = BUY PREPROCESSING AND TSAR INITIALIZATION PERFORMED                   
FGBSEGQ  EQU   X'40'                                                            
* X'40' = BUILD BUY SEGMENT                                                     
FGDETLQ  EQU   X'20'                                                            
* X'20' = DIFFERENCE DETAIL LIST IN PROCESS (DEFAULT = SUMMARY)                 
FGTBSVDQ EQU   X'10'                                                            
* X'10' = TSAR BUFFER HAS BEEN SAVED                                            
FGTOTALQ EQU   X'08'                                                            
* X'08' = TOTAL CALCULATION, SKIP BUY DETAIL PROCESSING                         
FGADDSPQ EQU   X'04'                                                            
* X'04' = ADD SPOTS TO REP BUYS, ELSE REMOVE SPOTS IF OFF                       
FGNEWBYQ EQU   X'02'                                                            
* X'02' = ADDING NEW REP BUY LINE                                               
FGEXPLDQ EQU   X'01'                                                            
* X'01' = EXPLODED VIEW REQUESTED                                               
*                                                                               
METHOD   DS    X                   ADD SPOTS TO NEW LINE OR CHG CURRENT         
ADDBUYQ  EQU   X'80'                                                            
CHGBUYQ  EQU   X'40'                                                            
*                                                                               
DISPFLGS DS    X                   DISPLAY FLAGS                                
DFCOMPQ  EQU   X'80'               TOO MANY REP LINES, COMPRESSED MODE          
*                                                                               
* KEEP TRACK OF TSAR RECORD NUMBERS FOR BUY SEGMENTS IN                         
* DIFFERENCES LIST DISPLAY                                                      
*                                                                               
LISTSTRT DS    H                   TSAR NUMBER OF RECORD AT LIST START          
LISTLAST DS    H                   TSAR NUMBER OF RECORD AT LIST END            
*                                                                               
NXOFFSET DS    F                   OFFSET TO NEXT DISPLAY LINE                  
*                                  SET WHEN IN EXPLODED MODE                    
*                                                                               
RPORDTOT DS    XL4                 REP CONTRACT ORDER TOTAL                     
AGORDTOT DS    XL4                 AGENCY ORDER TOTAL                           
RPSPTTOT DS    XL2                 REP CONTRACT SPOT TOTAL                      
AGSPTTOT DS    XL2                 AGENCY SPOT TOTAL                            
*                                                                               
PRVREPSP DS    X                   TOTAL SPTS FROM REP BUYS FOR THIS WK         
PRVAGYSP DS    X                   TOTAL SPTS FROM AGY BUYS FOR THIS WK         
TOTREPSP DS    X                   TOTAL SPTS FROM REP BUYS FOR THIS WK         
TOTAGYSP DS    X                   TOTAL SPTS FROM AGY BUYS FOR THIS WK         
REPSPWK  DS    X                   SPTS FROM REP BUY FOR THIS WK                
AGYSPWK  DS    X                   SPTS FROM AGY BUY FOR THIS WK                
NUMWKS   DS    H                   # OF WKS OF CONTINUOUS BUY SEGMENTS          
SEGRATE  DS    F                   RATE OF CURRENT SEGMENTS                     
SEGSTRDT DS    XL2                 BUY SEGMENT START DATE                       
SEGENDDT DS    XL2                 BUY SEGMENT END DATE                         
STENDAY  DS    X                   LAST START END DAY INDICATOR                 
*                                                                               
SAVEROTE DS    A                   SAVE A(ROTATION FIELD)                       
COMPCON  DS    CL4                 CONTRACT # COMP/REVERSED                     
ELTBUILD DS    CL96                ELEMENT BUILD AREA                           
ELTBILD2 DS    CL64                SECOND BUILD AREA                            
SVDEMCAT DS    XL42                SAVED DEMO CATEGORIES                        
PROGNAME DS    CL34                SAVE BUY PROGRAM NAME                        
DETLFLAG DS    XL2                 TOTAL SPOT LENGTH                            
BUYUPDAT DS    CL1                 BUY WRITE FLAG                               
SKIPRECS DS    CL1                 GENERAL RECORD SKIP FLAG                     
TOTSPTUN DS    XL1                 TOTAL SPOT UNITS                             
ACTCODE  DS    CL1                 ACTION CODE:                                 
*                                     A  =  APPROVE                             
*                                     R  =  REJECT                              
PRTSTAT  DS    X                   STATUS FLAG FOR PRINTING                     
PRTONE   EQU   X'80'               ONLY ONE REPORT WILL BE PRINTED              
PRTNEWPG EQU   X'40'               EJECT PAGE FOR NEXT REPORT                   
PRTCLOSE EQU   X'20'               WE'RE ALL DONE, CLOSE PQ AND EXIT            
PRTDAILY EQU   X'10'               BUY IS DAILY                                 
PRTPAGE1 EQU   X'08'               PAGE 1 IS PRINTED                            
PRTCHEAD EQU   X'04'               COMMENT HEADER PRINTED                       
PRTKBUY  EQU   X'01'               CONTRACT BUYS FOUND, PRINT CONTRACT          
*                                   BUYS SIDE-BY-SIDE WITH AGENCY'S             
*                                                                               
MISCFLAG DS    XL1                 MESSAGE FLAGS                                
*                                     X'80'  =  DAILY ORDER                     
*                                     X'40'  =  ORBIT DROPPED                   
MFMANUAL EQU   X'20'                  X'20'  =  MANUAL CHANGES STARTED          
MFEXACT  EQU   X'10'                  X'10'  =  EXACT #SPOTS MATCH              
MFCREDIT EQU   X'08'                  X'08'  =  K BUY HAS CREDIT BUYS           
MFMGLOOP EQU   X'04'                  X'04'  =  MATCH MAKEGOOD ONLY             
MFCONOK  EQU   X'01'                  X'01'  =  CONTRACT UPDATED                
*                                                                               
MISCFLG2 DS    XL1                 MORE MESSAGE FLAGS                           
MF2PTCAN EQU   X'80'                  X'80'  =  PARTIAL CANCEL                  
MF20SBUY EQU   X'40'                  X'40'  =  AGENCY SENT 0 SPT BUY           
MF2NOXIT EQU   X'20'                  X'20'  =  DON'T ERROR EXIT IN             
*                                               MANUAL MATCHING                 
MF2TRADE EQU   X'10'                  X'10'  =  TRADE ORDER                     
MF2DTOT  EQU   X'08'                  X'08'  =  REDISPLAY TOTALS                
MF2NODEM EQU   X'04'                  X'04'  =  NO DEMO CATEGORIES              
MF2NOADM EQU   X'02'                  X'02'  =  NO AGENCY DEMO                  
MF2USRDM EQU   X'01'                  X'01'  =  USER DEFINED DEMO               
APPRBUY# DS    X                   LAST BUY DISPLAYED IN APPROVAL SCRN          
AGBUYMTR DS    X                   AGENCY MAKEGOOD BUY MASTER LINE              
ORBFLAG  DS    CL1                 ORBIT PRESENCE INDICATOR                     
*                                     Y  =  ORBIT PRESENT                       
BUCKFLGS DS    CL1                 INDICATOR FOR BUCKUP ROUTINE                 
EDICTACT DS    CL3                 EDICT 'ACTION'                               
RESENT   DS    CL1                 RESENT FLAG                                  
ACTDATE  DS    XL2                 DATE FOR STAMPING                            
ACTTIME  DS    XL2                 TIME FOR STAMPING                            
VERDFLT  DS    CL1                 VERSION NUMBER (REP)                         
CONMOD#  DS    CL1                 SAVE AREA FOR MOD NUMBER                     
BUYLINE# DS    X                   BUYLINE NUMBER BEING GENERATED               
NEWVER#  DS    CL1                 NEW RECORD VERSION NUMBER                    
NEEDSCRN DS    CL1                                                              
HDRDA    DS    XL4                 DISK ADDRESS OF DARE RECORD                  
SAVEKEY  DS    CL27                KEY SAVE AREA FOR RESTARTS                   
SVBUYKEY DS    CL27                KEY SAVE AREA FOR RESTARTS                   
SAVEPROD DS    CL20                PROD NAME(S) FROM AGENCY ORDER               
SAVEEASI DS    0CL16               SAVE AREA FOR EASI CODES                     
EASIADV  DS    CL4                 CLIENT CODE: 6 CHARS ON AGENCY HDR           
EASIPROD DS    CL4                                                              
EASIEST# DS    CL4                 EST#: 3 CHARS ON AGENCY HDR                  
EASIPRD2 DS    CL4                 PIGGY/PARTNER PRODUCT                        
*                                                                               
SAVEEST# DS    CL6                 EBCDIC ESTIMATE NUMBER AGENCY                
*                                  MAY CONTAIN ALPHA LETTERS                    
*                                                                               
FLTDATES DS    CL4                 AGENCY ORDER FLIGHT DATES                    
FLTSTART DS    XL3                                                              
FLTEND   DS    XL3                                                              
SAVEBUYR DS    CL24                BUYER NAME FROM AGENCY ORDER                 
STARTDAY DS    XL1                 START DAY FOR BUY                            
ENDDAY   DS    XL1                 END DAY FOR BUY                              
ORBSTDAY DS    XL1                 ORBIT START DAY                              
ROTATDAY DS    XL1                 ROTATION START DAY                           
BUCKFLAG DS    CL1                 ADD/SUBTRACT FLAG FOR BUCKETS                
TRUFLAG  DS    CL1                 GENERATE EC/SAR KEY IF 'Y'                   
FLTKEYFG DS    CL1                 GENERATE 8E AND 8D KEY IF 'Y'                
CONFLTDT DS    CL4                 ORIGINAL CONTRACT FLIGHT DATES               
NEWFLTDT DS    CL4                 NEW CONTRACT FLIGHT DATES                    
RECADDR  DS    CL4                 DISK ADDRESS OF CONTRACT RECORD              
TKOVDATE DS    XL3                 TAKEOVER DATE                                
SAVEBU$$ DS    CL4                 BUY BREAK SAVE AREA                          
SENDID   DS    CL2                                                              
INTTYPE  DS    CL1                 INPUT TYPE                                   
SPLKEYAD DS    133C                EXTENDED SPOOL AREA                          
PRTTIME  DS    XL4                 PRINT TIME                                   
REVNUM   DS    XL(L'RDARRNUM)                                                   
REVDATE  DS    XL(L'RDARDATE)                                                   
REVTIME  DS    XL(L'RDARTIME)                                                   
ORDRETRN EQU   *                   INFO FOR RETURN MESSAGES                     
RETORD#  DS    CL8                                                              
RETFROM  DS    CL10                                                             
RETTO    DS    CL10                                                             
RETSTAT  DS    CL6                                                              
RETCON#  DS    CL8                                                              
RETSENDR DS    CL16                                                             
*                                                                               
*   SPOTPAK TRANSFER ELEMENT                                                    
*                                                                               
IFELCODE DS    CL2                 CODE/LENGTH = X'0830'                        
IFSPAG   DS    CL2                 SPOT AGENCY POWER CODE                       
IFSPMD   DS    CL1                 SPOT MEDIA CODE                              
IFSPCL   DS    CL3                 SPOT CLIENT CODE                             
IFSPPRD  DS    CL3                 SPOT PRODUCT CODE                            
IFSPES   DS    CL1                 SPOT ESTIMATE NUMBER                         
IFSPPP   DS    CL3                 SPOT PRODUCT PIGGY                           
IFSPP1   DS    CL1                 SPOT PRODUCT 1 SPLIT                         
IFSPP2   DS    CL1                 SPOT PRODUCT 2 SPLIT                         
IFSPL#   DS    CL1                 SPOT BUYLINE NUMBER                          
IFSPST   DS    CL5                 STATION CALL LETTERS                         
IFSPADV  DS    CL4                 REP ADVERTISER CODE                          
IFSPRD   DS    CL3                 REP PRODUCT CODE                             
IFSPDT   DS    CL3                 SPOT TRANSFER DATE                           
IFSPTM   DS    CL4                 SPOT TRANSFER TIME                           
         DS    CL12                SPARE                                        
IFELLEN  EQU   *-IFELCODE          ELEMENT LENGTH                               
*                                                                               
WORK2    DS    300C                WORK SPACE FOR BUCKET BUILD                  
*                                     AND GENERAL COMPARISONS                   
         DS    0F                                                               
BUYCOST  DS    F                   COST OF SPOTS IN BUY                         
SPOTCTR  DS    F                   NUMBER SPOTS IN BUY                          
WEEKCTR  DS    F                   NUMBER WEEKS IN BUY                          
ORDTOT$$ DS    F                   ORDER TOTAL DOLLARS                          
ORDTOTSP DS    F                   ORDER TOTAL SPOTS                            
AREJMESS DS    A                   A(NEXT REJ MESS SPACE)                       
KATZEDI  DS    C                   Y/N KATZ EDI ORDER?                          
DAILYFLG DS    C                   DAILY PACING FLAG                            
TARGETBY DS    X                                                                
TKODATE  DS    XL3                 TAKEOVER DATE                                
MYSVAIO  DS    F                                                                
*                                                                               
BUYNUM   DS    X                                                                
*                                                                               
RSTRDATE DS    XL3                 (YMD) REP BUY START DATE                     
RBUYGRID DS    XL53                BUYGRID FOR THE REP BUYLINE                  
*                                                                               
ASTRDATE DS    XL3                 (YMD) AGENCY BUY START DATE                  
ABUYGRID DS    XL53                BUYGRID FOR THE AGY BUYLINE                  
ABUYGRD2 DS    XL53                A COPY OF ABUYGRID                           
*                                                                               
FLAGS2   DS    XL1                 MORE FLAGS                                   
SCRLBACK EQU   X'80'               SCROLL BACK                                  
PF8DOWN  EQU   X'40'               PF8 WAS PRESSED                              
FGALLQ   EQU   X'20'               VIEW ALL BUYLINES                            
TOTALPRC EQU   X'10'               TOTAL PROCESSING FOR PRINT                   
* X'20' = VIEW ALL BUYLINES                                                     
FG2DEMO EQU    X'08'                                                            
* X'08' = AGENCY DEMO CATEGORIES CHANGED                                        
FIRSTQ   EQU   X'01'               VIEW ALL BUYLINES                            
*                                                                               
SCRNUM   DS    H      SCREEN NUM THAT WE ARE IN, FIRST IS 1                     
         DS    0H                                                               
SCRLLTAB DS    (MAXSCRLL)H         TABLE CONTAINING THE TXNUM                   
*        DS    X'FF'               OF THE FIRST LINE ON EACH SCREEN             
MAXSCRLL EQU   100                 MAX SCROLL PAGE                              
TABOVERE EQU   918                 ERROR MESSAGE FOR TABLE OVERFLOW             
*                                                                               
         EJECT                                                                  
*                                                                               
* APPLICATION STORAGE AREA                                                      
*                                                                               
MYAREAD  DSECT                                                                  
ACTREJ   EQU   16                                                               
ACTDIF   EQU   17                                                               
RELO     DS    A                                                                
*                                                                               
TBLINDEX DS    XL2                 BUY LINK INDEX                               
SVTSAR#  DS    XL2                 SAVED TSAR RECORD NUMBER                     
TLACTN   DS    XL1                 REQUESTED ACTION                             
TLST     DS    XL(L'TLREC+2)       TSAR2 RECORD BUFFER                          
         ORG   TLST                                                             
TXNUM    DS    XL2                                                              
TXREC    DS    0X                                                               
TXLEN    DS    XL2                                                              
TXKEY    DS    0X                                                               
         ORG                                                                    
TBLOCK   DS    XL(TSARDL)          TSAR BUFFER                                  
*                                                                               
GSTRDATE DS    XL3                 (YMD) MISSED BUY START DATE FOR GRID         
BUYGRID  DS    XL(53*2)                                                         
BSTRDATE DS    XL3                 START DATE USED IN GRID CALCULATIONS         
BMSS#SPT DS    X                                                                
MYWRK    DS    XL27                                                             
MSSTDT   DS    CL8                 BUY GRID START DATE                          
         DS    CL1                 SEPARATOR                                    
MSSTDT2  DS    CL8                 USER INPUT START DATE                        
         DS    CL1                 SEPARATOR                                    
MSSTDT3  DS    CL8                 USER INPUT END DATE                          
MSBUYEL  DS    XL11                BUY ELEMENT BUILD AREA                       
MSENDDAY DS    X                                                                
*                                                                               
TSARREC  DS    XL(L'TLREC)                                                      
TSARREC2 DS    XL(L'TLREC)                                                      
TSARKEY  DS    XL(L'TLKEY)                                                      
*                                                                               
DIFTOT   DS    F                                                                
DIFSPT   DS    F                                                                
*                                                                               
GTSSVREC DS    XL(L'TLREC)         SAVED TSARREC AREA                           
GTSSVTR# DS    XL2                 SAVED TSAR RECORD NUMBER                     
DYTXNUM  DS    XL2                 SAVED TSAR RECORD NUMBER FOR DAY             
TMTXNUM  DS    XL2                 SAVED TSAR RECORD NUMBER FOR TIME            
*                                                                               
PRTXNUM  DS    XL2                 SAVED TSAR RECORD NUMBER FOR PRINT           
PDYTXNUM DS    XL2                 SAVED TSAR RECORD NUMBER FOR DAY             
PTMTXNUM DS    XL2                 SAVED TSAR RECORD NUMBER FOR TIME            
*                                                                               
PBRKLINE DS    X                   LINE NUMBER OF LAST BREAK PRINTED            
PBRKPAGE DS    XL2                 PAGE NUMBER OF LAST BREAK PRINTED            
*                                                                               
GNXSVREC DS    XL(L'TLREC)         SAVED TSARREC AREA                           
GNXSVDRC DS    XL(L'TLREC)         SAVED DAYS TSARREC AREA                      
GNXSVTR# DS    XL2                 SAVED TSAR RECORD NUMBER                     
*                                                                               
DLSVREC  DS    XL(L'TLREC)         SAVED TSARREC AREA FOR LIST DISPLAY          
         ORG   DLSVREC             RECYCLE                                      
APSVREC  DS    XL(L'TLREC)          SAVED TSARREC AREA FOR APPROVAL             
DLSVBLRC DS    XL(L'TLREC)         SAVED PREVIOUS BUY LINK AREA                 
DLSVTR#  DS    XL2                 SAVED TSAR RECORD NUMBER                     
PRSVTR#  DS    XL2                 SAVED TSAR# FOR PRINT ROUTINE                
PVSVTR#  DS    XL2                 SAVED 'PREVIOUS' TSAR RECORD NUMBER          
CRSVTR#  DS    XL2                 SAVED 'CURRENT' TSAR RECORD NUMBER           
UPSVTR#  DS    XL2                 SAVED TSAR# FOR UPDTBUYS ROUTINE             
*                                                                               
MYELEM   DS    CL256               REUSE REJMESS AREA                           
CMTSTRDT DS    XL3                                                              
CMTENDDT DS    XL3                                                              
CMTNUMWK DS    X                                                                
CMTNUMSP DS    X                                                                
         ORG   MYELEM                                                           
REJMESS  DS    600C                REJECTION MESSAGE BUILD AREA                 
*                                                                               
ASBUYDA  DS    XL(4*255)           AGENCY SHADOW BUY D/A LIST                   
RBUYDA   DS    XL(4*255)           REPPAK BUY D/A LIST                          
*                                                                               
*                                                                               
WORKLQ   EQU   *-MYAREAD                                                        
*                                                                               
* ONLINE LIST LINE                                                              
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTRHDLN DS    CL8                                                              
         DS    CL1                                                              
LSTRAORN DS    CL8                                                              
         DS    CL1                                                              
LSTRSTA  DS    CL6                                                              
         DS    CL1                                                              
LSTRAGY  DS    CL5                                                              
         EJECT                                                                  
ORDAPREJ DSECT                                                                  
ARTRANID DS    CL6                                                              
ARORDNUM DS    CL8                                                              
ARFROM   DS    CL10                                                             
ARTO     DS    CL10                                                             
ARDATE   DS    CL6                                                              
ARTIME   DS    CL4                                                              
ARSTAT   DS    CL6                                                              
ARCON#   DS    CL8                                                              
ARRETSND DS    CL16                                                             
ARDDS    DS    CL3                                                              
         SPACE 4                                                                
ORDCOM   DSECT                                                                  
OCTRANID DS    CL6                                                              
OCORDNUM DS    CL8                                                              
OCCONTIN DS    CL1                                                              
OCBUYLIN DS    CL4                                                              
OCCOMMNT DS    CL70                                                             
OCDDS    DS    CL3                                                              
         SPACE 4                                                                
ORDTRLR  DSECT                                                                  
OTTRANID DS    CL6                                                              
OTORDNUM DS    CL8                                                              
OTCOUNT  DS    CL6                                                              
*                                                                               
* IF ANY OF THE BELOW FIELDS CHANGE, MAKE SURE TO CHANGE THE PARAMETERS         
* IN THE XSORT CALLS ABOVE                                                      
*                                                                               
SORTD    DSECT                                                                  
SRTAGYBY DS    X                   AGENCY BUY THAT K BUY IS MAPPED TO           
*                                  X'FF' MEANS K BUY ADDED MANUALLY             
SRTTKBUY DS    X                   TARGET K BUY (FOR MAKEGOOD/CREDIT)           
SRTCONBY DS    X                   K BUY                                        
SRTBUYDA DS    XL4                 CONTRACT BUY DISK ADDRESS                    
SRTFLAGS DS    X                   MISC. FLAGS                                  
SFMATCHD EQU   X'80'               K BUY WAS MATCHED                            
SFMATING EQU   X'40'               K BUY IS IN PROCESS OF MATCHING              
SFMKGOOD EQU   X'20'               K BUY IS A MAKEGOOD                          
SFCREDIT EQU   X'10'               K BUY IS A CREDIT                            
SFTMKGD  EQU   X'08'               K BUY IS TARGET MAKEGOOD                     
SFTCRDT  EQU   X'04'               K BUY IS TARGET CREDIT                       
SFADDED  EQU   X'02'               K BUY WAS ADDED AT THIS APPROVAL             
SFTKMKGD EQU   X'01'               K BUY IS A TAKEOVER MAKEGOOD                 
SRTFLG2  DS    X                                                                
SF2METH1 EQU   X'80'               AUTO METHOD 1                                
SF2METH2 EQU   X'40'               AUTO METHOD 2                                
SF2METH3 EQU   X'20'               AUTO METHOD 3                                
SF20SPTS EQU   X'10'               CONTRACT BUY HAS ZERO TOTAL SPOTS            
SF20CSTS EQU   X'08'               CONTRACT BUY HAS ZERO TOTAL DOLLARS          
SF2MGOF  EQU   X'04'               CONTRACT BUY HAS PENDING MKGD OFFERS         
SF2CAN   EQU   X'02'               CANCELLED BY 0 SPOT OVERRIDE                 
SORTDLQ  EQU   *-SORTD                                                          
*                                                                               
SLINKD   DSECT SHOW LINK ON SCREEN                                              
SLAGYBUY DS    CL3                                                              
         DS    C                                                                
SLKBUY   DS    CL3                                                              
         DS    C                                                                
SLTKBUY  DS    CL3                                                              
         DS    C                                                                
SLFLAGS  DS    CL23                                                             
*----------------------------------------------------------------------         
TLSTD    DSECT                     TSAR RECORD DSECT                            
TLREC    DS    0XL255              *** TSAR RECORD ***                          
TLLEN    DS    XL2                 RECORD LENGTH                                
TLKEY    DS    0XL13               *** TSAR KEY ***                             
TLKTYP   DS    XL1                 TSAR RECORD TYPE                             
*                                    - X'01' DAYS                               
*                                    - X'02' TIMES                              
*                                    - X'03' PROGRAM NAME                       
*                                    - X'0B' BUY SEGMENT                        
*                                    - C'A'  AGENCY BUY LINKS                   
*                                    - C'R'  REP BUY LINKS                      
*                                                                               
TLDATA   DS    0X                                                               
         EJECT                                                                  
*---------------------------------------*                                       
* KEY FOR 'DAYS' RECORD                                                         
*---------------------------------------*                                       
         ORG   TLDATA                                                           
TLDYSTEN DS    XL1                 START-END DAY INDICATOR                      
TLDYDAYS DS    XL1                 DAY(S)                                       
*                                                                               
*---------------------------------------*                                       
* KEY FOR 'TIMES' RECORD                                                        
*---------------------------------------*                                       
         ORG   TLDATA                                                           
TLTMSTRT DS    XL2                 START TIME                                   
TLTMENDT DS    XL2                 END TIME                                     
*---------------------------------------*                                       
* KEY FOR 'PROGRAM NAME' RECORD                                                 
*---------------------------------------*                                       
         ORG   TLDATA                                                           
TLPGNAME DS    0C                  PROGRAM NAME                                 
*---------------------------------------*                                       
* KEY FOR 'BUY SEGMENT' RECORD                                                  
*---------------------------------------*                                       
         ORG   TLDATA                                                           
TLBYDAY  DS    XL2                 DAY INDEX                                    
TLBYTIME DS    XL2                 TIME INDEX                                   
TLBYLEN  DS    XL2                 LENGTH IN SECONDS                            
TLBYRATE DS    XL4                 RATE                                         
TLBYWKOF DS    XL2                 WEEK-OF                                      
TLBYPROG DS    XL2                 PROGRAM NAME INDEX                           
TLBYRFLG DS    X                   REP FLAGS                                    
*                                  X'80' = LENGTH IS IN MINUTES                 
*                                  X'40' = TLBYRLNK IS A LINK POINTER           
*                                          ELSE IT IS THE ACTUAL LINE#          
*                                  X'01' = SPECIAL FOR DISPLAY:                 
*                                          FLAG THIS RECORD IF THIS IS          
*                                          THE FIRST RECORD ON THE TOP          
*                                          OF THE SCREEN. THIS WILL             
*                                          ALLOW US TO SCROLL BACK TO           
*                                          THE CORRECT RECORD                   
TLBYAFLG DS    X                   AGENCY FLAGS                                 
*                                  X'80' = LENGTH IS IN MINUTES                 
*                                  X'40' = TLBYALNK IS A LINK POINTER           
*                                          ELSE IT IS THE ACTUAL LINE#          
*                                                                               
TLBYRLNK DS    XL2                 REP BUY LINK POINTER IF TLBYRFLG             
*                                  HAS X'40' ON. OTHERWISE THIS IS              
*                                  THE ACTUAL BUY LINE NUMBER                   
*                                  AND NUMBER OF SPOTS FOR THIS SEGMEMT         
TLBYALNK DS    XL2                 AGENCY BUY LINK POINTER IF TLBYAFLG          
*                                  HAS X'40' ON. OTHERWISE THIS IS              
*                                  THE ACTUAL BUY LINE NUMBER                   
*                                  AND NUMBER OF SPOTS FOR THIS SEGMEMT         
TLBYLQ   EQU   *-TLSTD                                                          
*---------------------------------------*                                       
* KEY FOR AGENCY/REP 'BUY LINK' RECORD                                          
*---------------------------------------*                                       
         ORG   TLDATA                                                           
TLBLINDX DS    XL2                 LINK INDEX POINTED FROM BUY SEGMENT          
*                                  FOR THIS WEEK                                
         DS    XL(L'TLKEY-L'TLKTYP-L'TLBLINDX)  KEY SPARE                       
*                                                                               
TLBLTSPT DS    X                   TOTAL NUMBER OF REP OR AGENY SPOTS           
TLBLBUY# DS    X                   LIST OF AGENCY OR REP BUY NUMBERS            
TLBL#SPT DS    X                   AND CORRESPONDING NUMBER OF SPOTS            
TLBLLIST DS    0X                                                               
*                                                                               
DIFFSUM  DSECT                     DIFFERENCE SUMMARY LIST DSECT                
DSDYTM   DS    CL22                DAY/TIME                                     
         DS    CL1                                                              
DSLEN    DS    CL3                 LENGTH                                       
         DS    CL1                                                              
DSRATE   DS    CL12                RATE                                         
         DS    CL1                                                              
DSDATES  DS    CL10                DATES                                        
         DS    CL1                                                              
DS#SPT   DS    CL4                 +/- NUMBER OF SPOTS                          
         DS    CL1                                                              
DSRBUY#  DS    CL5                 REP BUY NUMBERS                              
         DS    CL1                                                              
DSPGM    DS    CL15                PROGRAM NAME                                 
         ORG   DSDYTM                                                           
PDSDYTM  DS    CL24                DAY/TIME                                     
         DS    CL1                                                              
PDSLEN   DS    CL3                 LENGTH                                       
         DS    CL1                                                              
PDSRATE  DS    CL12                RATE                                         
         DS    CL1                                                              
PDSDATES DS    CL10                DATES                                        
         DS    CL1                                                              
PDS#SPT  DS    CL4                 +/- NUMBER OF SPOTS                          
         DS    CL1                                                              
PDSPGM   DS    CL20                PROGRAM NAME                                 
         DS    CL1                                                              
PDSRBUY# DS    CL20                REP BUY NUMBERS                              
         DS    CL1                                                              
PDST#SPT DS    CL5                 TOTAL SPOT DIFFERENCE                        
         DS    CL1                                                              
PDST$    DS    CL16                TOTAL DOLLAR DIFFERENCE                      
*                                                                               
DIFFDET  DSECT                     DIFFERENCE SUMMARY LIST DSECT                
DDIND    DS    CL1                 DIFFERENCE INDICATOR                         
         DS    CL1                                                              
DDDYTM   DS    CL22                DAY/TIME                                     
         DS    CL1                                                              
DDLEN    DS    CL3                 LENGTH                                       
         DS    CL1                                                              
DDRATE   DS    CL12                RATE                                         
         DS    CL1                                                              
DDDATE   DS    CL5                 DATES                                        
         DS    CL1                                                              
DDR#SPT  DS    CL3                 REP NUMBER OF SPOTS                          
         DS    CL1                                                              
DDRBUY#  DS    CL4                 REP BUY NUMBER                               
         DS    CL1                                                              
DDA#SPT  DS    CL3                 AGY NUMBER OF SPOTS                          
         DS    CL1                                                              
DDABUY#  DS    CL4                 AGY BUY NUMBER                               
         DS    CL1                                                              
DDPGM    DS    CL11                PROGRAM NAME                                 
         ORG   DDIND                                                            
PDDIND   DS    CL3                 DIFFERENCE INDICATOR                         
         DS    CL1                                                              
PDDDYTM  DS    CL22                DAY/TIME                                     
         DS    CL1                                                              
PDDLEN   DS    CL3                 LENGTH                                       
         DS    CL1                                                              
PDDRATE  DS    CL12                RATE                                         
         DS    CL1                                                              
PDDDATE  DS    CL5                 DATES                                        
         DS    CL1                                                              
PDDR#SPT DS    CL3                 REP NUMBER OF SPOTS                          
         DS    CL1                                                              
PDDRBUY# DS    CL20                REP BUY NUMBER                               
         DS    CL1                                                              
PDDA#SPT DS    CL3                 AGY NUMBER OF SPOTS                          
         DS    CL1                                                              
PDDABUY# DS    CL20                AGY BUY NUMBER                               
         DS    CL1                                                              
PDDPGM   DS    CL20                PROGRAM NAME                                 
*                                                                               
IMWORKD  DSECT                     DIFFERENCE SUMMARY LIST DSECT                
IMSVIO   DS    A                   TOTAL DOLLAR DIFFERENCE                      
IMSVKEY  DS    XL27                                                             
IMIO     DS    XL2000                                                           
IMWORKQ  EQU   *-IMWORKD                                                        
*                                                                               
*                                                                               
T80F22   CSECT                                                                  
         ORG   T80F22+(((*-T80F22)/2048)+1)*2048                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032REDAR22   12/13/04'                                      
         END                                                                    
