*          DATA SET RESTR03    AT LEVEL 026 AS OF 05/01/02                      
*PHASE T80E03A                                                                  
         TITLE 'T80E03 - RESTR03 - GOALS, KEY INVENTORY AND COMMENTS'           
***********************************************************************         
*                                                                     *         
*  RESTR03 (T80E03) --- GOALS, KEY INVENTORY AND COMMENT MAINTENANCE  *         
*                                                                     *         
* ------------------------------------------------------------------- *         
* UPDATE HISTORY:                                                     *         
*                                                                     *         
* 01FEB94 (SKU) INITIAL RELEASE                                       *         
*                                                                     *         
* 08MAR94 (SKU) ADD GROUP/SUBGROUP TO KEY                             *         
*                                                                     *         
* 19MAY94 (SKU) FIX LISTING BUG                                       *         
*                                                                     *         
* 24NOV97 (JRD) Y2000 JULIAN DATES                                    *         
***********************************************************************         
T80E03   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T80E03*,R7,RR=R3                                              
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
*                                                                               
         GOTO1 INITIAL             INIT SYSSPARE                                
         MVI   ACTELOPT,C'N'       DON'T WANT ACTIVITIES ELEMENT                
         OI    CONSERVH+6,X'81'    SCREEN IS ALWAYS MODIFIED                    
         MVC   MYSCRNUM,TWASCR     SET SCREEN NUMBER                            
         MVI   IOOPT,C'Y'          DO MY OWN I/O 'S                             
*                                                                               
         TM    STRFLAGS,PFKEYHIT   IF PFKEY 2/3 WAS PRESSED IN                  
         BZ    MAIN10              LIST CHANGE, GOTO VALREC SINCE               
         NI    STRFLAGS,X'FF'-PFKEYHIT  CONTROLLER RESTORES RECORD              
         B     VR                                                               
*                                                                               
MAIN10   DS    0H                                                               
         CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD?                             
         BE    VR                                                               
         CLI   MODE,XRECPUT        RECORD UPDATE??                              
         BE    XRP                                                              
         CLI   MODE,XRECADD        RECORD ADD??                                 
         BE    XRP                                                              
         CLI   MODE,DISPKEY        DISPLAY KEY?                                 
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD?                              
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS?                                
         BE    LR                                                               
         CLI   MODE,RECDEL         DEL RECORD                                   
         BE    DELR                                                             
         CLI   MODE,RECREST        RESTORE RECORD                               
         BE    RESR                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
K        USING RSTRKEY,KEY         REMOVE HARD KEY LENGTHS                      
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE KEY                                                              
***********************************************************************         
VK       DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RSTRKEY,R6                                                       
         MVI   RSTRKTYP,RSTRTYPQ                                                
         MVI   RSTRKSUB,RSTRGKCQ   TYPE STRATEGY AND TACTICS                    
         MVC   RSTRKREP,AGENCY                                                  
*                                                                               
         CLI   ACTNUM,ACTLIST                                                   
         BNE   VK30                                                             
***********************************************************************         
* FOR LIST, VALIDATE FILTERS                                                    
***********************************************************************         
         LA    R2,GKLGRUPH         CAN ONLY FILTER ON GROUP OR STATION          
         CLI   GKLGRUPH+5,0        FILTER ON GROUP?                             
         BE    VK10                                                             
*                                                                               
         CLI   GKLSTATH+5,0                                                     
         BNE   INVLFILT                                                         
                                                                                
         CLI   TWAACCS,C'$'        STA SIGN-ON MUST FILTER ON STA               
         BE    INVLSIGN                                                         
                                                                                
         OC    GKLGRUP,SPACES                                                   
         MVC   SVKEY,KEY           SAVE OFF KEY                                 
         GOTO1 VALIGRP                                                          
         BNZ   INVLGRP                                                          
         MVC   GRPSGRP,GKLGRUP                                                  
         MVC   KEY,SVKEY                                                        
         B     VK20                                                             
*                                                                               
VK10     DS    0H                  FILTER ON STATION ?                          
         CLI   GKLSTATH+5,0                                                     
         BE    INVLFILT                                                         
         MVC   SVKEY,KEY                                                        
         LA    R2,GKLSTATH                                                      
         GOTO1 VALISTA                                                          
         MVC   STATION,WORK                                                     
         MVC   GRPSGRP,WORK+41                                                  
         MVC   KEY,SVKEY                                                        
                                                                                
         GOTO1 CKACCESS                                                         
         BNZ   SLOCKOUT                                                         
                                                                                
VK20     DS    0H                  FILTER ON PERIOD?                            
         CLI   GKLPERIH+5,0                                                     
         BE    VKX                                                              
         MVC   SVKEY,KEY                                                        
         LA    R2,GKLPERIH                                                      
         GOTO1 VALIPERI                                                         
         BNZ   INVLPER                                                          
                                                                                
         MVC   STARTDT,WORK                                                     
         MVC   ENDDT,WORK+3                                                     
         MVC   KEY,SVKEY                                                        
                                                                                
         CLC   ENDDT,STARTDT       END V START DATE                             
         BH    INVLDAT             ERR - END DATE BEFORE START DATE             
                                                                                
         B     VKX                                                              
*                                                                               
* VALIDATE STATION CALL LETTERS                                                 
*                                                                               
VK30     DS    0H                                                               
         MVC   SVKEY,KEY           VALISTA USES KEY                             
         LA    R2,GKCSTATH         VALIDATE STATION CALL LETTER FORMAT          
         GOTO1 VALISTA                                                          
         MVC   KEY,SVKEY                                                        
         MVC   RSTRKGRP,WORK+41                                                 
         MVC   RSTRKSTA,WORK       SAVE TO KEY                                  
         MVC   STRGROUP,WORK+41                                                 
         MVC   STRSTAT,WORK        SAVE IN CASE WE PF TO ANOTHER SCRN           
         CLI   STRSTAT+4,C'L'                                                   
         BE    VK31                                                             
         CLI   STRSTAT+4,C'T'                                                   
         BE    VK31                                                             
         B     VK32                                                             
VK31     EQU   *                                                                
         MVC   STRSTAT+5(1),STRSTAT+4                                           
         MVI   STRSTAT+4,C'-'                                                   
VK32     EQU   *                                                                
         MVC   GKCLOCA,WORK+10     MARKET NAME                                  
                                                                                
         GOTO1 CKACCESS                                                         
         BNZ   SLOCKOUT                                                         
                                                                                
         OI    GKCLOCAH+6,X'80'    XMIT                                         
*                                                                               
* VALIDATE PERIOD                                                               
*                                                                               
         LA    R2,GKCPERIH                                                      
         GOTO1 VALIPERI                                                         
         BNZ   INVLPER                                                          
         MVC   STARTDT,WORK                                                     
         MVC   ENDDT,WORK+3                                                     
                                                                                
         CLC   ENDDT,STARTDT       END V START DATE                             
         BH    INVLDAT             ERR - END DATE BEFORE START DATE             
                                                                                
         MVC   SVKEY,KEY                                                        
         CLI   ACTNUM,ACTADD                                                    
         BNE   VK100                                                            
*                                                                               
* IF ACTION ADD, CHECK IF THIS RECORD OVERLAPS ANY EXISTING RECORDS             
*   WITH THE SAME STATION.  IF YES, ERROR EXIT                                  
*                                                                               
VK40     DS    0H                                                               
         OI    DMINBTS,X'08'       PASS DELETED RECS                            
         GOTO1 HIGH                                                             
                                                                                
VK50     CLC   KEY(RSTRKEND-RSTRKEY),KEYSAVE                                    
         BNE   VK100                                                            
         CLC   STARTDT,K.RSTRKSTD  CAN'T OVERLAP                                
         BE    INVLDOVL                                                         
         CLC   ENDDT,K.RSTRKEND                                                 
         BE    INVLDOVL                                                         
                                                                                
         CLC   ENDDT,K.RSTRKEND    IS NEW END DATE EARLIER THAN                 
         BH    VK60                EXISTING END DATE?                           
                                                                                
         CLC   STARTDT,K.RSTRKEND  NO, NEW START DATE HAS TO BE                 
         BL    VK70                LATER THAN EXISTING END DATE                 
         B     INVLDOVL                                                         
                                                                                
VK60     CLC   ENDDT,K.RSTRKSTD    YES, NEW END DATE HAS TO BE                  
         BNH   INVLDOVL            EARLIER THAN EXISTING START DATE             
                                                                                
VK70     DS    0H                                                               
         OI    DMINBTS,X'08'       PASS DELETED RECS                            
         GOTO1 SEQ                                                              
         B     VK50                                                             
                                                                                
VK100    DS    0H                                                               
         MVC   KEY,SVKEY                                                        
         MVC   RSTRKSTD,WORK                                                    
         MVC   RSTRKEND,WORK+3                                                  
         MVC   STRPERI,GKCPERI     SAVE IN CASE WE PF TO OTHER SCRN             
                                                                                
         OI    GKCPERIH+4,X'20'                                                 
                                                                                
         CLI   ACTNUM,ACTLIST                                                   
         BE    VK110                                                            
                                                                                
* DISPLAY SHARE GOAL, LAST UPD, DESC                                            
         MVC   SVKEY,KEY                                                        
         GOTO1 DISINFO,DMCB,GKCSGOLH,GKCLDATH,GKCDESCH                          
         MVC   KEY,SVKEY                                                        
                                                                                
VK110    DS    0H                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    VK120                                                            
         CLI   ACTNUM,ACTREST                                                   
         BE    VKX                                                              
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'RSTRKEY),KEYSAVE     RECORD NOT FOUND                      
         BE    VK120                                                            
         LA    R2,CONACTH                                                       
         B     RECNOTF                                                          
*                                                                               
* FOR ACTION ADD, CHECK IF SITUATION ANALYSIS RECORD EXISTS                     
*                                                                               
VK120    DS    0H                                                               
         MVC   SVKEY,KEY           SAVE KEY                                     
         MVI   RSTRKSUB,RSTRSITQ                                                
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'RSTRKEY),KEYSAVE     SIT RECORD MUST BE THERE              
         BNE   NOSITREC                                                         
                                                                                
         MVC   KEY,SVKEY           RESTORE KEY/AIO                              
                                                                                
VKX      DS    0H                                                               
         MVC   GKCKEY,KEY                                                       
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE THE MASTER RECORD                                                      
***********************************************************************         
XRP      DS    0H                                                               
         MVC   KEY,GKCKEY                                                       
         GOTO1 UPDTLCHG            GO UPDATE LAST CHANGED DATE                  
         BNZ   NOSITREC            SIT ANA REC NOT FOUND                        
         MVC   KEY,GKCKEY                                                       
XRPX     B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE RECORD ON SCREEN                                                 
***********************************************************************         
VR       DS    0H                                                               
         CLI   PFKEY,ADDLINE       ADD/DELETE LINE?                             
         BE    VR30                                                             
         CLI   PFKEY,DELLINE                                                    
         BNE   VR200                                                            
         EJECT                                                                  
***********************************************************************         
* ADD/DELETE A LINE                                                             
***********************************************************************         
VR30     DS    0H                                                               
         CLI   ACTNUM,ACTADD       ACTION ADD, ADD FIRST CHANGE LATER           
         BE    VR200                                                            
*                                                                               
         L     RF,ATIOB            A(TIOB)                                      
         USING TIOBD,RF                                                         
         SR    R1,R1                                                            
         ICM   R1,3,TIOBCURS       ABSOLUTE CURSOR ADDRESS                      
         DROP  RF                                                               
         SR    R0,R0                                                            
         D     R0,=F'80'                                                        
         MH    R1,=H'80'           ABSOLUTE ADDR OF BEGINNING OF LINE           
*                                                                               
* FIND POSITION OF CURSOR AND WHICH SECTION IT'S IN                             
*                                                                               
         NI    GKCFLAG,X'FF'-CURSGOL-CURSKIN-CURSCMT                            
         OI    GKCFLAG,CURSGOL     CURSOR IS IN GOAL SECTION                    
         LA    R2,GKCHGOLH         1ST FIELD WHICH COULD CONTAIN CURSOR         
VR50     SR    RF,RF                                                            
         ICM   RF,3,2(R2)          ABSOLUTE SCREEN ADDR OF THIS FIELD           
         SR    RE,RE                                                            
         D     RE,=F'80'                                                        
         MH    RF,=H'80'           ABSOLUTE SCREEN ADDR OF LINE START           
         LA    RE,79(RF)           ABSOLUTE SCREEN ADDR OF LINE END             
*                                                                               
         CR    RF,R1               WAS CURSOR ON THIS LINE?                     
         BH    VR200               NO - IT'S ABOVE THIS FIELD                   
         CR    RE,R1                                                            
         BNL   VR100               YES                                          
*                                                                               
         ZIC   RF,0(R2)            BUMP TO FIRST FIELD OF NEXT LINE             
         AR    R2,RF                                                            
*                                                                               
         LA    RF,GKCHKINH                                                      
         CR    R2,RF               KEY INVENTORY TITLE FIELD?                   
         BNE   VR60                YES                                          
         NI    GKCFLAG,X'FF'-CURSGOL                                            
         OI    GKCFLAG,CURSKIN     CURSOR IS IN KEY & INV SECTION               
         B     VR50                                                             
*                                                                               
VR60     DS    0H                                                               
         LA    RF,GKCHCMTH                                                      
         CR    R2,RF               COMMENT FIELD?                               
         BNE   VR70                YES                                          
         NI    GKCFLAG,X'FF'-CURSKIN                                            
         OI    GKCFLAG,CURSCMT     CURSOR IS IN CMT SECTION                     
         B     VR50                                                             
*                                                                               
VR70     DS    0H                                                               
         LA    RF,GKCLCMTH                                                      
         CR    R2,RF               END OF SCREEN?                               
         BH    VR200               YES                                          
         B     VR50                                                             
*                                                                               
VR100    CLI   PFKEY,ADDLINE       JUMP TO ROUTINE                              
         BE    VR150                                                            
         EJECT                                                                  
***********************************************************************         
* DELETE LINE                                                                   
***********************************************************************         
         LA    R0,GKCHGOLH                                                      
         CR    R2,R0               IS CURSOR ON PROTECTED HEADER?               
         BE    VR200               YES -- ONLY ALLOWED FOR ADD                  
         LA    R0,GKCHKINH                                                      
         CR    R2,R0               IS CURSOR ON PROTECTED HEADER?               
         BE    VR200               YES -- ONLY ALLOWED FOR ADD                  
         LA    R0,GKCHCMTH                                                      
         CR    R2,R0               IS CURSOR ON PROTECTED HEADER?               
         BE    VR200               YES -- ONLY ALLOWED FOR ADD                  
*                                                                               
         ST    R2,ACURSOR          KEEP CURSOR IN PLACE                         
         LR    R3,R2                                                            
*                                                                               
VR110    DS    0H                                                               
         TM    GKCFLAG,CURSGOL                                                  
         BZ    VR115                                                            
         LA    RF,GKCLGOLH                                                      
         B     VR130                                                            
*                                                                               
VR115    TM    GKCFLAG,CURSKIN                                                  
         BZ    VR120                                                            
         LA    RF,GKCLKINH                                                      
         B     VR130                                                            
*                                                                               
VR120    TM    GKCFLAG,CURSCMT                                                  
         BNZ   *+6                                                              
         DC    H'0'                CAN'T FIND CURSOR, DUMP ME!                  
         LA    RF,GKCLCMTH                                                      
*                                                                               
VR130    DS    0H                                                               
         CR    R2,RF               ARE WE UP TO LAST LINE OF TEXT?              
         BE    VR140               YES                                          
         ZIC   R0,0(R2)                                                         
         AR    R3,R0               R3 POINTS TO FOLLOWING LINE                  
         LA    R1,L'GKCGOAL                                                     
         BCTR  R1,0                FOR EX INSTRUCTIONS:  R1 = L'TEXT            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),8(R3)       MOVE LINE OF TEXT UP                         
         MVC   4(2,R2),4(R3)       MOVE INPUT INDICATORS AND LENGTH             
         LR    R2,R3                                                            
         B     VR110                                                            
*                                                                               
VR140    XC    4(2,R2),4(R2)       CLEAR INPUT INDICATORS AND LENGTH            
         LA    R1,L'GKCGOAL                                                     
         BCTR  R1,0                FOR EX INSTRUCTIONS:  R1 = L'TEXT            
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)       CLEAR LAST TEXT FIELD                        
         B     VR200                                                            
         EJECT                                                                  
***********************************************************************         
* INSERT LINE                                                                   
***********************************************************************         
VR150    DS    0H                                                               
         LA    RF,GKCLGOLH         ARE THEY TRYING TO INSERT AFTER END          
         CR    R2,RF               OF STR & TACT?                               
         BE    VR200               YES                                          
         LA    RF,GKCLKINH                                                      
         CR    R2,RF               OF KEY INV?                                  
         BE    VR200               YES                                          
         LA    RF,GKCLCMTH                                                      
         CR    R2,RF               OF COMMENT?                                  
         BE    VR200               YES                                          
*                                                                               
         ST    R2,ACURSOR          KEEP CURSOR IN PLACE                         
         LR    RF,R2               SAVE A(INSERTION)                            
         TM    GKCFLAG,CURSGOL                                                  
         BZ    VR153                                                            
         LA    R3,GKCLGOLH                                                      
         B     VR158                                                            
*                                                                               
VR153    TM    GKCFLAG,CURSKIN                                                  
         BZ    VR155                                                            
         LA    R3,GKCLKINH                                                      
         B     VR158                                                            
*                                                                               
VR155    TM    GKCFLAG,CURSCMT                                                  
         BNZ   *+6                                                              
         DC    H'0'                CAN'T FIND CURSOR, DUMP ME!                  
         LA    R3,GKCLCMTH         LAST LINE OF TEXT                            
*                                                                               
VR158    DS    0H                                                               
         LR    R2,R3                                                            
*                                                                               
VR160    ZIC   R0,0(R2)                                                         
         SR    R2,R0               R3 POINTS TO PREVIOUS LINE                   
         CR    R2,RF               ARE WE UP TO LAST LINE OF TEXT?              
         BE    VR170               YES                                          
         LA    R1,L'GKCGOAL                                                     
         BCTR  R1,0                FOR EX INSTRUCTIONS:  R1 = L'TEXT            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R3),8(R2)       MOVE LINE OF TEXT DOWN                       
         MVC   4(2,R3),4(R2)       MOVE INPUT INDICATORS AND LENGTH             
         LR    R3,R2                                                            
         B     VR160                                                            
*                                                                               
VR170    XC    4(2,R3),4(R3)       CLEAR INPUT INDICATORS AND LENGTH            
         LA    R1,L'GKCGOAL                                                     
         BCTR  R1,0                FOR EX INSTRUCTIONS:  R1 = L'TEXT            
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R3),8(R3)       CLEAR TEXT FIELD (INSERT BLANK LINE)         
         EJECT                                                                  
***********************************************************************         
* ADD/UPDATE NOTE ELEMENT                                                       
***********************************************************************         
VR200    DS    0H                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    VR210                                                            
                                                                                
         L     R6,AIO                                                           
         GOTO1 GETREC                                                           
         MVI   ELCODE,RSTRGCDQ     GOAL ELEMENT CODE                            
         GOTO1 REMELEM             GET RID OF EXISTING ELEMENTS                 
         MVI   ELCODE,RSTRKCDQ     KEY INVENTORY ELEMENT CODE                   
         GOTO1 REMELEM             GET RID OF EXISTING ELEMENTS                 
         MVI   ELCODE,RSTRCCDQ     COMMENTS ELEMENT CODE                        
         GOTO1 REMELEM             GET RID OF EXISTING ELEMENTS                 
                                                                                
VR210    DS    0H                                                               
         LA    R2,GKCGOALH         FIRST TEXT FIELD                             
         LA    R5,PROCTAB          PROCESS TABLE                                
*                                                                               
VR215    DS    0H                                                               
         MVI   SEQNUM,0                                                         
         ZIC   R4,1(R5)            NUMBER OF LINES TO PROCESS                   
*                                                                               
VR220    DS    0H                                                               
         XC    ELEM,ELEM                                                        
         ZIC   RF,SEQNUM                                                        
         LA    RF,1(RF)                                                         
         STC   RF,SEQNUM                                                        
         MVC   ELEM(1),0(R5)                                                    
         MVC   ELEM+2(1),SEQNUM                                                 
*                                                                               
VR225    DS    0H                                                               
         CLC   =C'C=',8(R2)                                                     
         BNE   VR228                                                            
         MVC   SVKEY,KEY           VALICMT USES KEY                             
         GOTO1 VALICMT             VALIDATE FILE COMMENT RECORD                 
         MVC   KEY,SVKEY                                                        
*                                                                               
VR228    DS    0H                                                               
         ZIC   R1,5(R2)            LENGTH OF INPUT                              
         LTR   R1,R1               NO INPUT ON THIS LINE?                       
         BZ    VR240                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEM+3(0),8(R2)     NOTE TEXT LINE                               
         LA    R1,4(R1)            TOTAL LENGTH OF ELEMENT                      
         STC   R1,ELEM+1                                                        
*                                                                               
VR230    DS    0H                                                               
         L     R6,AIO                                                           
         GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VR240    DS    0H                                                               
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
*                                                                               
         BCT   R4,VR220                                                         
*                                                                               
         LA    R5,2(R5)                                                         
         CLI   0(R5),X'FF'                                                      
         BE    VR305                                                            
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         B     VR215                                                            
*                                                                               
VR305    DS    0H                                                               
         CLI   ACTNUM,ACTADD                                                    
         BNE   VR310                                                            
         GOTO1 ADDREC              ADD THE RECORD/KEY                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   KEY,SVKEY           KEY HAS DISK ADDRESS, RESTORE IT             
         B     VRX                                                              
*                                                                               
VR310    DS    0H                                                               
         GOTO1 PUTREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VRX      DS    0H                                                               
         B     DR                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE KEY                                                               
***********************************************************************         
DK       DS    0H                                                               
         MVC   SELCKEY,KEY                                                      
         MVC   GKCKEY,KEY                                                       
         LA    R6,KEY                                                           
         USING RSTRKEY,R6                                                       
*                                                                               
* DISPLAY STATION CALL LETTERS                                                  
*                                                                               
         MVC   GKCSTAT(4),RSTRKSTA                                              
         MVC   GKCSTAT+4(2),=C'-L'                                              
         CLI   RSTRKSTA+4,C'L'                                                  
         BE    DK05                                                             
         MVC   GKCSTAT+4(2),=C'-T'                                              
         CLI   RSTRKSTA+4,C' '                                                  
         BZ    *+10                                                             
         MVC   GKCSTAT+5(1),RSTRKSTA+4                                          
DK05     EQU   *                                                                
         MVI   GKCSTATH+5,6        OUTPUT LENGTH                                
         CLI   GKCSTAT+3,C' '                                                   
         BNE   DK10                                                             
         MVC   GKCSTAT+3(3),GKCSTAT+4                                           
         MVI   GKCSTATH+5,5        OUTPUT LENGTH                                
                                                                                
DK10     OI    GKCSTATH+6,X'80'    XMIT                                         
         MVC   STRGROUP,RSTRKGRP   SAVE IN CASE WE PF TO OTHER SCRNS            
         MVC   STRSTAT,GKCSTAT     SAVE IN CASE WE PF TO OTHER SCRNS            
*                                                                               
* DISPLAY PERIOD                                                                
*                                                                               
         XC    GKCPERI,GKCPERI     CLEAR THE FIELD                              
         ZAP   WORK+8(4),=P'0'                                                  
         MVO   WORK+8(4),RSTRKSTD  CHANGE TO PACK WITH SIGN                     
         ZAP   WORK+4(4),=P'999999'                                             
         SP    WORK+4(4),WORK+8(4) GET 9'S COMPLEMENT                           
         MVO   WORK(4),WORK+4(4)   CHANGE TO PWOS                               
         GOTO1 DATCON,DMCB,(8,WORK),(6,GKCPERI)                                 
                                                                                
         CLC   RSTRKSTD,RSTRKEND                                                
         BE    DK20                                                             
         MVI   GKCPERI+6,C'-'                                                   
                                                                                
         ZAP   WORK+8(4),=P'0'                                                  
         MVO   WORK+8(4),RSTRKEND  CHANGE TO PACK WITH SIGN                     
         ZAP   WORK+4(4),=P'999999'                                             
         SP    WORK+4(4),WORK+8(4) GET 9'S COMPLEMENT                           
         MVO   WORK(4),WORK+4(4)   CHANGE TO PWOS                               
         GOTO1 DATCON,DMCB,(8,WORK),(6,GKCPERI+7)                               
                                                                                
DK20     DS    0H                                                               
         OI    GKCPERIH+6,X'80'    XMIT                                         
         MVC   STRPERI,GKCPERI     SAVE IN CASE WE PF TO NOTE SCREEN            
         DROP  R6                                                               
*                                                                               
* READ STATION RECORD FOR MARKET NAME                                           
*                                                                               
         MVC   SVKEY,KEY           VALISTA USES KEY                             
         LA    R2,GKCSTATH         VALIDATE STATION CALL LETTER FORMAT          
         GOTO1 VALISTA                                                          
         MVC   KEY,SVKEY                                                        
                                                                                
         MVC   GKCLOCA,WORK+10                                                  
         OI    GKCLOCAH+6,X'80'    XMIT                                         
                                                                                
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE RECORD                                                            
***********************************************************************         
DR       DS    0H                                                               
         TWAXC GKCSGOLH            CLEAR SCREEN                                 
                                                                                
* DISPLAY SHARE GOAL, LAST UPD, DESC                                            
         MVC   KEY,GKCKEY                                                       
         GOTO1 DISINFO,DMCB,GKCSGOLH,GKCLDATH,GKCDESCH                          
         MVC   KEY,GKCKEY                                                       
                                                                                
DR10     DS    0H                                                               
         LA    R2,GKCGOALH         FIRST TEXT FIELD                             
         LA    R5,PROCTAB          PROCESS TABLE                                
*                                                                               
DR20     DS    0H                                                               
         ZIC   R4,1(R5)            NUMBER OF LINES TO PROCESS                   
         L     R6,AIO                                                           
         MVC   ELCODE,0(R5)        CURRENT ELEMENT CODE                         
         BAS   RE,GETEL                                                         
         BE    DR30                                                             
*                                                                               
DR25     ZIC   R0,0(R2)            ELEMENT NOT FOUND,                           
         AR    R2,R0                 BUMP TO NEXT SECTION                       
         BCT   R4,DR25                                                          
         B     DR110                                                            
*                                                                               
DR30     DS    0H                                                               
         MVI   SEQNUM,0                                                         
         B     DR50                                                             
*                                                                               
DR40     DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BE    DR50                                                             
*                                                                               
DR45     ZIC   R0,0(R2)            IF NOT FOUND, BUMP TO NEXT FIELD             
         AR    R2,R0                                                            
         BCT   R4,DR45                                                          
         B     DR110                                                            
*                                                                               
DR50     DS    0H                                                               
         ZIC   RF,SEQNUM           INCREMENT SEQUENCE NUMBER UNTIL              
         LA    RF,1(RF)            WE FIND A MATCH WITH A LINE OF               
         STC   RF,SEQNUM           TEXT THAT BELONGS ON THIS LINE               
         CLC   SEQNUM,2(R6)                                                     
         BE    DR60                                                             
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   R4,DR50                                                          
*                                                                               
DR60     DS    0H                                                               
         ZIC   R1,1(R6)            LENGTH OF ELEMENT                            
         LA    RF,3                OVERHEAD LENGTH=EL CODE+LEN+SEQNUM           
         SR    R1,RF                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),3(R6)       LINE OF TEXT                                 
*                                                                               
         CLC   =C'C=',8(R2)        DISPLAY FILE COMMENT, IF ANY                 
         BNE   DR100                                                            
         CLI   MODE,VALREC         SKIP FOR VALREC                              
         BE    DR100               WILL ENCOUNTER PUTREC DRAMA                  
         MVC   SVKEY,KEY                                                        
         GOTO1 DISPCMT                                                          
         MVC   KEY,SVKEY                                                        
*                                                                               
DR100    DS    0H                                                               
         OI    6(R2),X'80'         XMIT                                         
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   R4,DR40                                                          
*                                                                               
DR110    DS    0H                                                               
         LA    R5,2(R5)            NEXT FIELD TO DISPLAY                        
         CLI   0(R5),X'FF'                                                      
         BE    DRX                                                              
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     DR20                                                             
*                                                                               
DRX      DS    0H                                                               
         MVC   ACURFORC,ACURSOR    PFKEY WAS HIT -- PLACE CURSOR                
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DELETE THE RECORD                                                             
***********************************************************************         
DELR     DS    0H                                                               
         L     R6,AIO                                                           
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         USING RSTRREC,R6                                                       
         OI    RSTRCNTL,X'80'      MARK FOR DELETION                            
         DROP  R6                                                               
*                                                                               
         GOTO1 PUTREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    KEY+27,X'80'        MARK FOR DELETION                            
*                                                                               
         GOTO1 WRITE                                                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DELX     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* RESTORE THE RECORD                                                            
***********************************************************************         
RESR     DS    0H                                                               
         OI    DMINBTS,X'08'       PASS BACK DELETES                            
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(L'RSTRKEY),KEYSAVE                                           
         BNE   INVLFLD             NOTHING TO RESTORE                           
*                                                                               
         L     R6,AIO                                                           
*                                                                               
         OI    DMINBTS,X'08'       READ FOR DELETE                              
         GOTO1 GETREC                                                           
*                                                                               
         USING RSTRREC,R6                                                       
         NI    RSTRCNTL,X'FF'-X'80'  RESTORE                                    
         DROP  R6                                                               
*                                                                               
         GOTO1 PUTREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         NI    KEY+27,X'FF'-X'80'  RESTORE                                      
*                                                                               
         GOTO1 WRITE                                                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RESX     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* LIST THE RECORD                                                               
***********************************************************************         
LR       DS    0H                                                               
         OC    SELCKEY,SELCKEY     DISPLAY FROM SELECT?                         
         BZ    LR05                OVERRIDE GENCON KEY                          
         MVC   KEY,SELCKEY         USE SELECTED KEY                             
         XC    SELCKEY,SELCKEY                                                  
         B     LR10                                                             
                                                                                
LR05     OC    KEY(L'RSTRKEY),KEY  FIRST TIME THRU?                             
         BNZ   LR10                                                             
                                                                                
         LA    R6,KEY                                                           
         USING RSTRKEY,R6                                                       
                                                                                
         XC    KEY,KEY                                                          
         MVI   RSTRKTYP,RSTRTYPQ                                                
         MVI   RSTRKSUB,RSTRGKCQ                                                
         MVC   RSTRKREP,AGENCY                                                  
                                                                                
         CLI   GKLGRUPH+5,0        FILTER ON GRP/SUBGRP                         
         BE    LR08                                                             
         MVC   RSTRKGRP,GRPSGRP    YES, START WITH THIS STATION                 
         B     LR10                                                             
                                                                                
LR08     DS    0H                                                               
         CLI   GKLSTATH+5,0        FILTER ON STATION?                           
         BE    LR10                                                             
         MVC   RSTRKGRP,GRPSGRP    YES, START WITH THIS GRP/SUBGRP              
         MVC   RSTRKSTA,STATION    AND THIS STATION                             
         DROP  R6                                                               
                                                                                
LR10     DS    0H                                                               
         GOTO1 HIGH                                                             
                                                                                
LR20     DS    0H                                                               
         LA    R6,KEY                                                           
         USING RSTRKEY,R6                                                       
                                                                                
         CLC   RSTRKEY(RSTRKGRP-RSTRKEY),GKCKEY                                 
         BNE   LRX                                                              
                                                                                
         CLI   GKLGRUPH+5,0        FILTER ON GRP/SUBGRP                         
         BE    LR25                                                             
         CLI   GKLGRUPH+5,1        FILTER ON GRP/SUBGRP                         
         BH    LR23                                                             
         CLC   GRPSGRP(1),RSTRKGRP                                              
         BNE   LRX                 WANT ONLY RECORDS WITH THIS GRP/SUB          
         B     LR25                                                             
                                                                                
LR23     DS    0H                                                               
         CLC   GRPSGRP,RSTRKGRP                                                 
         BNE   LRX                 WANT ONLY RECORDS WITH THIS GRP/SUB          
                                                                                
LR25     DS    0H                                                               
         CLI   GKLSTATH+5,0        FILTER ON STATION                            
         BE    LR30                                                             
         CLC   STATION,RSTRKSTA                                                 
         BNE   LRX                 WANT ONLY RECORDS WITH THIS STATION          
                                                                                
LR30     DS    0H                                                               
         CLI   GKLPERIH+5,0        FILTER ON PERIOD?                            
         BE    LR60                                                             
*                                                                               
* PERIOD FILTER MUST AT LEAST OVERLAP                                           
*                                                                               
         CLC   STARTDT,RSTRKEND    THESE DATES ARE IN 9'S COMP!                 
         BL    LRSEQ                                                            
         CLC   ENDDT,RSTRKSTD                                                   
         BH    LRSEQ                                                            
         DROP  R6                                                               
                                                                                
LR60     DS    0H                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RSTRREC,R6                                                       
*                                                                               
* CONSTRUCT ONE LIST LINE                                                       
*                                                                               
         MVC   LISTAR,SPACES                                                    
                                                                                
         MVC   LGKCGRUP,RSTRKGRP   GROUP                                        
                                                                                
         MVC   LGKCSTAT(4),RSTRKSTA STATION CALL LETTER                         
         MVC   LGKCSTAT+4(2),=C'-L'                                             
         CLI   RSTRKSTA+4,C'L'                                                  
         BE    LR70                                                             
         MVC   LGKCSTAT+4(2),=C'-T'                                             
         CLI   RSTRKSTA+4,C' '                                                  
         BZ    LR70                                                             
         MVC   LGKCSTAT+5(1),RSTRKSTA+4                                         
*                                  PERIOD                                       
LR70     DS    0H                                                               
         ZAP   WORK+8(4),=P'0'                                                  
         MVO   WORK+8(4),RSTRKSTD  CHANGE TO PACK WITH SIGN                     
         ZAP   WORK+4(4),=P'999999'                                             
         SP    WORK+4(4),WORK+8(4) GET 9'S COMPLEMENT                           
         MVO   WORK(4),WORK+4(4)   CHANGE TO PWOS                               
         GOTO1 DATCON,DMCB,(8,WORK),(6,LGKCPERI)                                
                                                                                
         CLC   RSTRKSTD,RSTRKEND                                                
         BE    LR75                                                             
         MVI   LGKCPERI+6,C'-'                                                  
                                                                                
         ZAP   WORK+8(4),=P'0'                                                  
         MVO   WORK+8(4),RSTRKEND  CHANGE TO PACK WITH SIGN                     
         ZAP   WORK+4(4),=P'999999'                                             
         SP    WORK+4(4),WORK+8(4) GET 9'S COMPLEMENT                           
         MVO   WORK(4),WORK+4(4)   CHANGE TO PWOS                               
         GOTO1 DATCON,DMCB,(8,WORK),(6,LGKCPERI+7)                              
         DROP  R6                                                               
                                                                                
LR75     DS    0H                                                               
         L     R6,AIO              TEXT                                         
         USING RSTRGOLD,R6                                                      
         MVI   ELCODE,RSTRGCDQ                                                  
         BAS   RE,GETEL                                                         
         BNE   LR90                NO GOAL TEXT FOUND                           
                                                                                
         CLI   RSTRGELN,RSTRGOOV                                                
         BNH   LR90                                                             
                                                                                
         ZIC   R1,RSTRGELN         TEXT HAS VARIABLE LENGTH                     
         LA    RF,RSTRGOOV         OVERHEAD LENGTH                              
         SR    R1,RF               TEXT LEN = TOTAL LEN - OVERHEAD LEN          
         BCTR  R1,0                                                             
         CH    R1,=H'43'           MAX LEN IS 44                                
         BNH   LR80                                                             
         LA    R1,43                                                            
                                                                                
LR80     DS    0H                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LGKCCODE(0),RSTRGOLE                                             
         DROP  R6                                                               
                                                                                
LR90     DS    0H                                                               
         GOTO1 LISTMON             SEND LINE TO SCREEN                          
                                                                                
LRSEQ    GOTO1 SEQ                 GET NEXT RECORD                              
         B     LR20                                                             
                                                                                
LRX      B     EXIT                                                             
         EJECT                                                                  
RELO     DS    A                                                                
*                                                                               
* ERROR MESSAGES                                                                
*                                                                               
INVLFILT MVC   RERROR,=AL2(363)    MUST SELECT GRP OR STATION FILTER            
         B     ERREND                                                           
*                                                                               
INVLGRP  MVC   RERROR,=AL2(358)    INVALID GROUP/SUBGROUP FILTER                
         B     ERREND                                                           
*                                                                               
INVLDAT  MVC   RERROR,=AL2(362)    START DATE MUST BE BEFORE END DATE           
         B     ERREND                                                           
*                                                                               
NOSITREC MVC   RERROR,=AL2(414)    SIT REC NOT FOUND                            
         B     ERREND                                                           
*                                                                               
INVLPER  MVC   RERROR,=AL2(417)    FORMAT IS MMM/YY-MMM/YY                      
         B     ERREND                                                           
*                                                                               
INVLDOVL MVC   RERROR,=AL2(366)    DATES OVERLAP EXISTING RECORDS               
         B     ERREND                                                           
*                                                                               
RECNOTF  MVC   RERROR,=AL2(413)    RECORD NOT FOUND. PLS ADD                    
         B     ERREND                                                           
*                                                                               
MISSFLD  MVC   RERROR,=AL2(MISSING)                                             
         B     ERREND                                                           
*                                                                               
INVLFLD  MVC   RERROR,=AL2(INVALID)                                             
         B     ERREND                                                           
*                                                                               
SLOCKOUT MVC   RERROR,=AL2(55)                                                  
         B     ERREND                                                           
*                                                                               
INVLSIGN MVC   RERROR,=AL2(418)                                                 
         B     ERREND                                                           
*                                                                               
ERREND   GOTO1 MYERROR                                                          
         EJECT                                                                  
**********************************************************************          
* LOCAL STORAGE AREA                                                            
**********************************************************************          
* USE TO VALIDATE GOAL, KI AND CMTS                                             
* CL1 ELEMENT CODE                                                              
* CL1 NUMBER OF LINES TO PROCESS                                                
PROCTAB  DC    AL1(RSTRGCDQ),AL1(6)                                             
         DC    AL1(RSTRKCDQ),AL1(3)                                             
         DC    AL1(RSTRCCDQ),AL1(3)                                             
         DC    X'FF'                                                            
*                                                                               
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE FATIOB                                                         
       ++INCLUDE RESTRFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE RESTRF5D          (OUR MAINTENANCE SCREEN OVERLAY)             
         ORG   CONTAGH                                                          
       ++INCLUDE RESTRF6D          (OUR LIST SCREEN OVERLAY)                    
       ++INCLUDE RESTRWORKD                                                     
       ++INCLUDE RESTRDSECT                                                     
         PRINT ON                                                               
*                                                                               
* APPLICATION STORAGE AREA                                                      
*                                                                               
         ORG   SYSSPARE                                                         
ADDLINE  EQU   2                                                                
DELLINE  EQU   3                                                                
STARTDT  DS    XL3                                                              
ENDDT    DS    XL3                                                              
SVKEY    DS    CL(L'KEY)                                                        
SELCKEY  DS    CL(L'KEY)                                                        
GKCKEY   DS    CL(L'KEY)                                                        
SAVER2   DS    F                                                                
ACURSOR  DS    A                   FORCE CURSOR HERE                            
SEQNUM   DS    X                                                                
GRPSGRP  DS    CL2                                                              
STATION  DS    CL5                                                              
GKCFLAG  DS    X                                                                
NOBLNKLN EQU   X'02'               RECORD HAS NO BLANK DAY/TIME LINES           
PFKEYUSE EQU   X'04'               PFKEY WAS USED                               
CURSGOL  EQU   X'08'               CURSOR IN GOALS SECTION                      
CURSKIN  EQU   X'10'               CURSOR IN KEY INV SECTION                    
CURSCMT  EQU   X'20'               CURSOR IN COMMENT SECTION                    
*                                                                               
* ONLINE LIST LINE                                                              
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LGKCGRUP DS    CL2                                                              
         DS    CL2                                                              
LGKCSTAT DS    CL7                                                              
         DS    CL2                                                              
LGKCPERI DS    CL13                                                             
         DS    CL2                                                              
LGKCCODE DS    CL44                                                             
*                                                                               
* ONLINE LIST LINE                                                              
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'026RESTR03   05/01/02'                                      
         END                                                                    
