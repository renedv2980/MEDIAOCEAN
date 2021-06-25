*          DATA SET RESTR01    AT LEVEL 035 AS OF 11/25/97                      
*PHASE T80E01A                                                                  
         TITLE 'T80E01 - RESTR01 - SITUATION ANALYSIS'                          
***********************************************************************         
*                                                                     *         
*  RESTR01 (T80E01) --- SITUATION ANALYSIS MAINTENANCE                *         
*                                                                     *         
* ------------------------------------------------------------------- *         
* UPDATE HISTORY:                                                     *         
*                                                                     *         
* 19JAN94 (SKU) INITIAL RELEASE                                       *         
*                                                                     *         
* 07MAR94 (SKU) ADD GROUP/SUBGROUP IN KEY                             *         
*                                                                     *         
* 20SEP94 (SKU) CANNOT DELETE SIT RECORD UNTIL ALL OTHER (IE SAT, GKC *         
*               ETC) CORRESPONDING RECORDS ARE DELETED FIRST          *         
*                                                                     *         
* 24NOV97 (JRD) Y2000 JULIAN DATES                                    *         
***********************************************************************         
T80E01   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T80E01*,R7,RR=R3                                              
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
                                                                                
         GOTO1 INITIAL                                                          
         MVI   ACTELOPT,C'N'       DON'T WANT ACTIVITIES ELEMENT                
         OI    CONSERVH+6,X'81'    SCREEN IS ALWAYS MODIFIED                    
         MVC   MYSCRNUM,TWASCR     SET SCREEN NUMBER                            
         MVI   IOOPT,C'Y'          DO MY OWN I/O 'S                             
                                                                                
         TM    STRFLAGS,PFKEYHIT   IF PFKEY 2/3 WAS PRESSED IN                  
         BZ    MAIN10              LIST CHANGE, GOTO VALREC SINCE               
         NI    STRFLAGS,X'FF'-PFKEYHIT  CONTROLLER RESTORES RECORD              
         B     VR                                                               
                                                                                
MAIN10   DS    0H                                                               
         CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD?                             
         BE    VR                                                               
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
         MVI   RSTRKSUB,RSTRSITQ                                                
         MVC   RSTRKREP,AGENCY                                                  
                                                                                
         CLI   ACTNUM,ACTLIST                                                   
         BNE   VK30                                                             
***********************************************************************         
* FOR LIST, VALIDATE FILTERS                                                    
***********************************************************************         
         LA    R2,SALGRUPH         CAN ONLY FILTER ON GROUP OR STATION          
         CLI   SALGRUPH+5,0        FILTER ON GROUP?                             
         BE    VK10                                                             
                                                                                
         CLI   SALSTATH+5,0                                                     
         BNE   INVLFILT                                                         
                                                                                
         CLI   TWAACCS,C'$'        STA SIGN-ON MUST FILTER ON STA               
         BE    INVLSIGN                                                         
                                                                                
         OC    SALGRUP,SPACES                                                   
         MVC   SVKEY,KEY           SAVE OFF KEY                                 
         GOTO1 VALIGRP                                                          
         BNZ   INVLGRP                                                          
         MVC   GRPSGRP,SALGRUP                                                  
         MVC   KEY,SVKEY                                                        
         B     VK20                                                             
                                                                                
VK10     DS    0H                  FILTER ON STATION ?                          
         CLI   SALSTATH+5,0                                                     
         BE    INVLFILT                                                         
         MVC   SVKEY,KEY                                                        
         LA    R2,SALSTATH                                                      
         GOTO1 VALISTA                                                          
         MVC   STATION,WORK                                                     
         MVC   GRPSGRP,WORK+41                                                  
         MVC   KEY,SVKEY                                                        
                                                                                
         GOTO1 CKACCESS                                                         
         BNZ   SLOCKOUT                                                         
                                                                                
VK20     DS    0H                  FILTER ON PERIOD?                            
         CLI   SALPERIH+5,0                                                     
         BE    VKX                                                              
         MVC   SVKEY,KEY                                                        
         LA    R2,SALPERIH                                                      
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
         LA    R2,SITSTATH         VALIDATE STATION CALL LETTER FORMAT          
         GOTO1 VALISTA                                                          
         MVC   KEY,SVKEY                                                        
         MVC   RSTRKGRP,WORK+41    SAVE TO KEY                                  
         MVC   RSTRKSTA,WORK       SAVE TO KEY                                  
         MVC   STRGROUP,WORK+41    SAVE IN CASE WE PF TO ANOTHER SCRN           
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
         MVC   SITLOCA,WORK+10     MARKET NAME                                  
                                                                                
         GOTO1 CKACCESS                                                         
         BNZ   SLOCKOUT                                                         
                                                                                
         OI    SITLOCAH+6,X'80'    XMIT                                         
*                                                                               
* VALIDATE PERIOD                                                               
*                                                                               
         LA    R2,SITPERIH                                                      
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
         CLC   ENDDT,RSTRKEND                                                   
         BE    INVLDOVL                                                         
                                                                                
         CLC   ENDDT,RSTRKEND      IS NEW END DATE EARLIER THAN                 
         BH    VK60                EXISTING END DATE?                           
                                                                                
         CLC   STARTDT,RSTRKEND    NO, NEW START DATE HAS TO BE                 
         BL    VK70                LATER THAN EXISTING END DATE                 
         B     INVLDOVL                                                         
                                                                                
VK60     CLC   ENDDT,RSTRKSTD      YES, NEW END DATE HAS TO BE                  
         BNH   INVLDOVL            EARLIER THAN EXISTING START DATE             
                                                                                
VK70     DS    0H                                                               
         OI    DMINBTS,X'08'       PASS DELETED RECS                            
         GOTO1 SEQ                                                              
         B     VK50                                                             
                                                                                
VK100    DS    0H                                                               
         MVC   KEY,SVKEY                                                        
         MVC   RSTRKSTD,WORK                                                    
         MVC   RSTRKEND,WORK+3                                                  
         MVC   STRPERI,SITPERI     SAVE IN CASE WE PF TO NOTE SCREEN            
                                                                                
         OI    SITPERIH+4,X'20'                                                 
                                                                                
         CLI   ACTNUM,ACTADD                                                    
         BE    VKX                                                              
         CLI   ACTNUM,ACTREST                                                   
         BE    VKX                                                              
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'RSTRKEY),KEYSAVE     RECORD NOT FOUND                      
         BE    VKX                                                              
         LA    R2,CONACTH                                                       
         B     RECNOTF                                                          
                                                                                
VKX      DS    0H                                                               
         MVC   SITKEY,KEY                                                       
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE RECORD ON SCREEN                                                 
***********************************************************************         
VR       DS    0H                                                               
         LA    R2,SITSGOLH                                                      
         CLI   5(R2),0                                                          
         BE    MISSFLD             MUST ENTER A SHARE GOAL                      
                                                                                
         CLI   ACTNUM,ACTADD                                                    
         BE    VR10                                                             
*                                                                               
         L     R6,AIO                                                           
         GOTO1 GETREC                                                           
         MVI   ELCODE,RSTRDCDQ     DESCRIPTIVE ELEMENT                          
         GOTO1 REMELEM             GET RID OF EXISTING ELEMENTS                 
         MVI   ELCODE,RSTRTCDQ     TEXT ELEMENT CODE                            
         GOTO1 REMELEM             GET RID OF EXISTING ELEMENTS                 
*                                                                               
VR10     DS    0H                                                               
         LA    R6,ELEM                                                          
         USING RSTRDESD,R6                                                      
         XC    ELEM,ELEM                                                        
         MVI   RSTRDCDE,RSTRDCDQ   ELEMENT CODE                                 
         MVI   RSTRDELN,RSTRDOV    OVERHEAD LENGTH                              
                                                                                
         LA    R2,SITSGOLH                                                      
         GOTO1 VPACK                                                            
         LTR   R0,R0                                                            
         BZ    INVLFLD                                                          
         STCM  R0,3,RSTRDSHR       SHARE GOAL                                   
                                                                                
         GOTO1 DATCON,DMCB,(5,0),(2,RSTRDUPT)  LAST UPDATE DATE                 
                                                                                
         LA    R2,SITDESCH         DESCRIPTION                                  
         CLI   5(R2),0                                                          
         BE    VR15                                                             
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RSTRDESC(0),SITDESC                                              
*                                                                               
         ZIC   RF,RSTRDELN         ADD VARIABLE LENGTH FROM                     
         LA    RF,1(R1,RF)         DESCRIPTION FIELD                            
         STC   RF,RSTRDELN                                                      
*                                                                               
VR15     DS    0H                                                               
         L     R6,AIO                                                           
         GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R6                                                               
*                                                                               
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
         LA    R2,SITHEADH         1ST FIELD WHICH COULD CONTAIN CURSOR         
VR40     SR    RF,RF                                                            
         ICM   RF,3,2(R2)          ABSOLUTE SCREEN ADDR OF THIS FIELD           
         SR    RE,RE                                                            
         D     RE,=F'80'                                                        
         MH    RF,=H'80'           ABSOLUTE SCREEN ADDR OF LINE START           
         LA    RE,79(RF)           ABSOLUTE SCREEN ADDR OF LINE END             
*                                                                               
         CR    RF,R1               WAS CURSOR ON THIS LINE?                     
         BH    VR200               NO - IT'S ABOVE THIS FIELD                   
         CR    RE,R1                                                            
         BNL   VR60                YES                                          
*                                                                               
         ZIC   RF,0(R2)            BUMP TO FIRST FIELD OF NEXT LINE             
         AR    R2,RF                                                            
*                                                                               
         LA    RF,SITLTXTH                                                      
         CR    R2,RF               END OF SCREEN?                               
         BH    VR200               YES                                          
         B     VR40                                                             
*                                                                               
VR60     CLI   PFKEY,ADDLINE       JUMP TO ROUTINE                              
         BE    VR150                                                            
         EJECT                                                                  
***********************************************************************         
* DELETE LINE                                                                   
***********************************************************************         
         LA    R0,SITHEADH                                                      
         CR    R2,R0               IS CURSOR ABOVE 1ST LINE?                    
         BE    VR200               YES -- ONLY ALLOWED FOR ADD                  
*                                                                               
         ST    R2,ACURSOR          KEEP CURSOR IN PLACE                         
         LR    R3,R2                                                            
VR130    DS    0H                                                               
         LA    RF,SITLTXTH                                                      
         CR    R2,RF               ARE WE UP TO LAST LINE OF TEXT?              
         BE    VR140               YES                                          
         ZIC   R0,0(R2)                                                         
         AR    R3,R0               R3 POINTS TO FOLLOWING LINE                  
         LA    R1,L'SITTXT                                                      
         BCTR  R1,0                FOR EX INSTRUCTIONS:  R1 = L'TEXT            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),8(R3)       MOVE LINE OF TEXT UP                         
         MVC   4(2,R2),4(R3)       MOVE INPUT INDICATORS AND LENGTH             
         LR    R2,R3                                                            
         B     VR130                                                            
*                                                                               
VR140    XC    4(2,R2),4(R2)       CLEAR INPUT INDICATORS AND LENGTH            
         LA    R1,L'SITTXT                                                      
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
         LA    RF,SITLTXTH                                                      
         CR    R2,RF               ARE THEY TRYING TO INSERT AFTER END?         
         BE    VR200               YES                                          
*                                                                               
         ST    R2,ACURSOR          KEEP CURSOR IN PLACE                         
         LR    RF,R2               SAVE A(INSERTION)                            
         LA    R3,SITLTXTH         LAST LINE OF TEXT                            
         LR    R2,R3                                                            
*                                                                               
VR160    ZIC   R0,0(R2)                                                         
         SR    R2,R0               R3 POINTS TO PREVIOUS LINE                   
         CR    R2,RF               ARE WE UP TO LAST LINE OF TEXT?              
         BE    VR170               YES                                          
         LA    R1,L'SITTXT                                                      
         BCTR  R1,0                FOR EX INSTRUCTIONS:  R1 = L'TEXT            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R3),8(R2)       MOVE LINE OF TEXT DOWN                       
         MVC   4(2,R3),4(R2)       MOVE INPUT INDICATORS AND LENGTH             
         LR    R3,R2                                                            
         B     VR160                                                            
*                                                                               
VR170    XC    4(2,R3),4(R3)       CLEAR INPUT INDICATORS AND LENGTH            
         LA    R1,L'SITTXT                                                      
         BCTR  R1,0                FOR EX INSTRUCTIONS:  R1 = L'TEXT            
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R3),8(R3)       CLEAR TEXT FIELD (INSERT BLANK LINE)         
         EJECT                                                                  
***********************************************************************         
* ADD/UPDATE NOTE ELEMENT                                                       
***********************************************************************         
VR200    DS    0H                                                               
         NI    SITFLAG,X'FF'-NOBLNKLN  FLAG FOR INPUT                           
         LA    R2,SITTXTH          FIRST TEXT FIELD                             
         MVI   SEQNUM,0                                                         
*                                                                               
VR220    LA    R6,ELEM                                                          
         USING RSTRTXTD,R6                                                      
*                                                                               
         LA    RF,SITLTXTH                                                      
         CR    R2,RF               END OF SCREEN?                               
         BH    VR300               YES                                          
                                                                                
         XC    ELEM,ELEM                                                        
         ZIC   RF,SEQNUM           INCREMENT SEQUENCE NUMBER                    
         LA    RF,1(RF)                                                         
         STC   RF,SEQNUM                                                        
         MVI   RSTRTXTE,RSTRTCDQ   NOTE TEXT LINE ELEMENT CODE                  
         MVC   RSTRTXSQ,SEQNUM     SEQUENCE NUMBER                              
                                                                                
         CLC   =C'C=',8(R2)                                                     
         BNE   VR225                                                            
         MVC   SVKEY,KEY           VALICMT USES KEY                             
         GOTO1 VALICMT             VALIDATE FILE COMMENT RECORD                 
         MVC   KEY,SVKEY                                                        
                                                                                
VR225    DS    0H                                                               
         ZIC   R1,5(R2)            LENGTH OF INPUT                              
         LTR   R1,R1               NO INPUT ON THIS LINE?                       
         BZ    VR230                                                            
         OI    SITFLAG,NOBLNKLN    SOME TEXT WAS FOUND                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RSTRTEXT(0),8(R2)   NOTE TEXT LINE                               
         LA    R3,RSTRTXOV         LENGTH OF ELEMENT OVERHEAD                   
         LA    R1,1(R3,R1)         TOTAL LENGTH OF ELEMENT                      
         B     VR250                                                            
*                                                                               
VR230    ST    R2,SAVER2           HANG ON TO CURRENT TWA POINTER               
*                                                                               
VR240    ZIC   R0,0(R2)            BUMP TO NEXT TEXT FIELD                      
         AR    R2,R0                                                            
         LA    RF,SITLTXTH                                                      
         CR    R2,RF               END OF SCREEN?                               
         BNH   VR245               NO                                           
         L     R2,SAVER2                                                        
         B     VR300                                                            
*                                                                               
VR245    CLI   5(R2),0             ANY INPUT THIS FIELD?                        
         BE    VR240               TRY NEXT FIELD                               
         L     R2,SAVER2                                                        
         B     VR260                                                            
*                                                                               
VR250    STC   R1,RSTRTELN                                                      
*                                                                               
VR260    CLI   5(R2),0             DON'T ADD BLANK LINE                         
         BE    VR270                                                            
*                                                                               
         L     R6,AIO                                                           
         GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R6                                                               
*                                                                               
VR270    ZIC   R0,0(R2)            BUMP TO NEXT TEXT FIELD                      
         AR    R2,R0                                                            
         B     VR220                                                            
*                                                                               
VR300    DS    0H                                                               
         TM    SITFLAG,NOBLNKLN                                                 
         BNZ   VR305                                                            
         LA    R2,SITTXTH          FIRST TEXT FIELD                             
         B     MISSFLD             MUST HAVE AT LEAST ONE LINE OF NOTE          
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
         MVC   SITKEY,KEY                                                       
         MVC   SELCKEY,KEY                                                      
         LA    R6,KEY                                                           
         USING RSTRKEY,R6                                                       
*                                                                               
* DISPLAY STATION CALL LETTERS                                                  
*                                                                               
         MVC   SITSTAT(4),RSTRKSTA                                              
         MVC   SITSTAT+4(2),=C'-L' LOW POWER TV                                 
         CLI   RSTRKSTA+4,C'L'     LP-TV                                        
         BE    DK05                                                             
         MVC   SITSTAT+4(2),=C'-T'                                              
         CLI   RSTRKSTA+4,C' '                                                  
         BZ    *+10                                                             
         MVC   SITSTAT+5(1),RSTRKSTA+4                                          
DK05     EQU   *                                                                
         MVI   SITSTATH+5,6        OUTPUT LENGTH                                
         CLI   SITSTAT+3,C' '                                                   
         BNE   DK10                                                             
         MVC   SITSTAT+3(3),SITSTAT+4                                           
         MVI   SITSTATH+5,5        OUTPUT LENGTH                                
                                                                                
DK10     DS    0H                                                               
         OI    SITSTATH+6,X'80'    XMIT                                         
         MVC   STRGROUP,RSTRKGRP   SAVE IN CASE WE PF TO OTHER SCRNS            
         MVC   STRSTAT,SITSTAT     SAVE IN CASE WE PF TO OTHER SCRNS            
*                                                                               
* DISPLAY PERIOD                                                                
*                                                                               
         XC    SITPERI,SITPERI     CLEAR THE FIELD                              
         ZAP   WORK+8(4),=P'0'                                                  
         MVO   WORK+8(4),RSTRKSTD  CHANGE TO PACK WITH SIGN                     
         ZAP   WORK+4(4),=P'999999'                                             
         SP    WORK+4(4),WORK+8(4) GET 9'S COMPLEMENT                           
         MVO   WORK(4),WORK+4(4)   CHANGE TO PWOS                               
         GOTO1 DATCON,DMCB,(8,WORK),(6,SITPERI)                                 
                                                                                
         CLC   RSTRKSTD,RSTRKEND                                                
         BE    DK20                                                             
         MVI   SITPERI+6,C'-'                                                   
                                                                                
         ZAP   WORK+8(4),=P'0'                                                  
         MVO   WORK+8(4),RSTRKEND  CHANGE TO PACK WITH SIGN                     
         ZAP   WORK+4(4),=P'999999'                                             
         SP    WORK+4(4),WORK+8(4) GET 9'S COMPLEMENT                           
         MVO   WORK(4),WORK+4(4)   CHANGE TO PWOS                               
         GOTO1 DATCON,DMCB,(8,WORK),(6,SITPERI+7)                               
                                                                                
DK20     DS    0H                                                               
         OI    SITPERIH+6,X'80'    XMIT                                         
         MVC   STRPERI,SITPERI     SAVE IN CASE WE PF TO NOTE SCREEN            
         DROP  R6                                                               
*                                                                               
* READ STATION RECORD FOR MARKET NAME                                           
*                                                                               
         MVC   SVKEY,KEY           VALISTA USES KEY                             
         LA    R2,SITSTATH         VALIDATE STATION CALL LETTER FORMAT          
         GOTO1 VALISTA                                                          
         MVC   KEY,SVKEY                                                        
                                                                                
         MVC   SITLOCA,WORK+10                                                  
         OI    SITLOCAH+6,X'80'    XMIT                                         
                                                                                
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE RECORD                                                            
***********************************************************************         
DR       DS    0H                                                               
         TWAXC SITSGOLH            CLEAR SCREEN                                 
                                                                                
         L     R6,AIO                                                           
         USING RSTRDESD,R6                                                      
         MVI   ELCODE,RSTRDCDQ     DESCRIPTIVE ELEMENT                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST HAVE DESCRIPTIVE ELEMENT                
                                                                                
         EDIT  RSTRDSHR,(3,SITSGOL),ALIGN=LEFT                                  
         OI    SITSGOLH+6,X'80'    XMIT                                         
                                                                                
         GOTO1 DATCON,DMCB,(2,RSTRDUPT),(5,SITLDAT)                             
         OI    SITLDATH+6,X'80'    XMIT                                         
*                                                                               
         CLI   RSTRDELN,RSTRDOV    ANY DESCRIPTION?                             
         BNH   DR05                                                             
*                                                                               
         ZIC   R1,RSTRDELN         DESCRIPTION HAS VARIABLE LENGTH              
         LA    RF,RSTRDOV          OVERHEAD LENGTH                              
         SR    R1,RF               DESC LEN = TOTAL LEN - OVERHEAD LEN          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SITDESC(0),RSTRDESC                                              
         OI    SITDESCH+6,X'80'    XMIT                                         
         DROP  R6                                                               
*                                                                               
DR05     DS    0H                                                               
         L     R6,AIO                                                           
         USING RSTRTXTD,R6                                                      
         MVI   ELCODE,RSTRTCDQ     NOTES ELEMENT                                
         BAS   RE,GETEL                                                         
         BNE   DRX                                                              
*                                                                               
         SR    R5,R5               LINE COUNTER                                 
         LA    R2,SITTXTH          FIRST NOTE FIELD                             
*                                                                               
DR10     MVC   8(L'SITTXT,R2),SPACES                                            
         LA    R5,1(R5)            INCREMENT LINE COUNTER                       
         ZIC   R1,RSTRTXSQ         SEQUENCE NUMBER                              
         CR    R1,R5               IF NOT EQUAL, PRINT BLANK LINE               
         BNE   DR15                                                             
*                                                                               
         ZIC   R1,RSTRTELN         LENGTH OF ELEMENT                            
         LA    R3,RSTRTXOV         OVERHEAD LENGTH                              
         SR    R1,R3               LENGTH OF TEXT                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),RSTRTEXT    LINE OF TEXT                                 
*                                                                               
         CLC   =C'C=',8(R2)        DISPLAY FILE COMMENT, IF ANY                 
         BNE   DR15                                                             
         MVC   SVKEY,KEY                                                        
         GOTO1 DISPCMT                                                          
         MVC   KEY,SVKEY                                                        
*                                                                               
DR15     OI    6(R2),X'80'                                                      
         ZIC   R0,0(R2)            NEXT TEXT FIELD                              
         AR    R2,R0                                                            
*                                                                               
         LA    RF,SITLTXTH                                                      
         CR    R2,RF               END OF SCREEN?                               
         BH    DRX                 YES                                          
*                                                                               
         ZIC   R1,RSTRTXSQ         SEQUENCE NUMBER                              
         CR    R1,R5               IF NOT EQUAL, PRINT BLANK LINE               
         BNE   DR10                                                             
*                                                                               
         BAS   RE,NEXTEL           NEXT LINE OF TEXT                            
         BE    DR10                                                             
*                                                                               
DR20     ZIC   R1,0(R2)                                                         
         SH    R1,=H'9'            LENGTH OF DATA + 1 (FOR EX)                  
         TM    1(R2),X'02'         TEXT EXTENDED HEADER                         
         BZ    *+8                                                              
         SH    R1,=H'8'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SPACES      BLANK OUT REMAINING FIELDS                   
         OI    6(R2),X'80'                                                      
         ZIC   R0,0(R2)            NEXT FIELD                                   
         AR    R2,R0                                                            
*                                                                               
         LA    RF,SITLTXTH                                                      
         CR    R2,RF               END OF SCREEN?                               
         BNH   DR20                NO                                           
*                                                                               
DRX      MVC   ACURFORC,ACURSOR    PFKEY WAS HIT -- PLACE CURSOR                
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* DELETE THE RECORD                                                             
***********************************************************************         
DELR     DS    0H                                                               
         MVC   SVKEY,KEY                                                        
         LA    R6,KEY              MAKE SURE ALL CORRESPONDING RECORDS:         
         USING RSTRKEY,R6          SAT, GKC, SAC AND ACC RECS ARE               
         MVI   RSTRKSUB,RSTRSATQ   DELETED FIRST                                
                                                                                
DELR10   DS    0H                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(RSTRKPG-RSTRKEY),KEYSAVE                                     
         BE    ERDELSUB            DON'T COMPARE PAGE NUMBERS                   
         MVC   KEY,KEYSAVE                                                      
         ZIC   RF,RSTRKSUB                                                      
         LA    RF,1(RF)                                                         
         STC   RF,RSTRKSUB                                                      
         CLI   RSTRKSUB,RSTRACCQ                                                
         BNH   DELR10                                                           
         DROP  R6                                                               
                                                                                
         MVC   KEY,SVKEY           RESTORE                                      
         GOTO1 HIGH                                                             
                                                                                
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
*                                                                               
LR05     OC    KEY(L'RSTRKEY),KEY  FIRST TIME THRU?                             
         BNZ   LR10                                                             
                                                                                
         LA    R6,KEY                                                           
         USING RSTRKEY,R6                                                       
                                                                                
         XC    KEY,KEY                                                          
         MVI   RSTRKTYP,RSTRTYPQ                                                
         MVI   RSTRKSUB,RSTRSITQ                                                
         MVC   RSTRKREP,AGENCY                                                  
                                                                                
         CLI   SALGRUPH+5,0        FILTER ON GROUP/SUBGROUP?                    
         BE    LR08                                                             
         MVC   RSTRKGRP,GRPSGRP    YES, START WITH THIS GROUP/SUBGROUP          
         B     LR10                                                             
                                                                                
LR08     DS    0H                                                               
         CLI   SALSTATH+5,0        FILTER ON STATION?                           
         BE    LR10                                                             
         MVC   RSTRKGRP,GRPSGRP    YES, START WITH THIS GROUP/SUBGROUP          
         MVC   RSTRKSTA,STATION    YES, START WITH THIS STATION                 
         DROP  R6                                                               
                                                                                
LR10     DS    0H                                                               
         GOTO1 HIGH                                                             
                                                                                
LR20     DS    0H                                                               
         LA    R6,KEY                                                           
         USING RSTRKEY,R6                                                       
                                                                                
         CLC   RSTRKEY(RSTRKGRP-RSTRKEY),SITKEY                                 
         BNE   LRX                                                              
                                                                                
         CLI   SALGRUPH+5,0        FILTER ON GROUP/SUBGROUP                     
         BE    LR25                                                             
         CLI   SALGRUPH+5,1                                                     
         BH    LR23                                                             
         CLC   GRPSGRP(1),RSTRKGRP ONLY GROUP SPECIFIED                         
         BNE   LRX                 WANT ONLY RECORDS WITH THIS STATION          
         B     LR25                                                             
                                                                                
LR23     DS    0H                                                               
         CLC   GRPSGRP,RSTRKGRP    BOTH GROUP AND SUBGROUP SPECIFIED            
         BNE   LRX                 WANT ONLY RECORDS WITH THIS STATION          
                                                                                
LR25     DS    0H                                                               
         CLI   SALSTATH+5,0        FILTER ON STATION                            
         BE    LR30                                                             
         CLC   STATION,RSTRKSTA                                                 
         BNE   LRX                 WANT ONLY RECORDS WITH THIS STATION          
                                                                                
LR30     DS    0H                                                               
         CLI   SALPERIH+5,0        FILTER ON PERIOD?                            
         BE    LR60                                                             
*                                                                               
* PERIOD FILTER MUST AT LEAST OVERLAP                                           
*                                                                               
         LA    R6,KEY                                                           
         USING RSTRKEY,R6                                                       
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
                                                                                
         MVC   LSTRGRUP,RSTRKGRP   GROUP                                        
                                                                                
         MVC   LSTRSTAT(4),RSTRKSTA STATION CALL LETTER                         
         MVC   LSTRSTAT+4(2),=C'-L'                                             
         CLI   RSTRKSTA+4,C'L'                                                  
         BE    LR70                                                             
         MVC   LSTRSTAT+4(2),=C'-T'                                             
         CLI   RSTRKSTA+4,C' '                                                  
         BZ    LR70                                                             
         MVC   LSTRSTAT+5(1),RSTRKSTA+4                                         
*                                  PERIOD                                       
LR70     DS    0H                                                               
         ZAP   WORK+8(4),=P'0'                                                  
         MVO   WORK+8(4),RSTRKSTD  CHANGE TO PACK WITH SIGN                     
         ZAP   WORK+4(4),=P'999999'                                             
         SP    WORK+4(4),WORK+8(4) GET 9'S COMPLEMENT                           
         MVO   WORK(4),WORK+4(4)   CHANGE TO PWOS                               
         GOTO1 DATCON,DMCB,(8,WORK),(6,LSTRPERI)                                
                                                                                
         CLC   RSTRKSTD,RSTRKEND                                                
         BE    LR75                                                             
         MVI   LSTRPERI+6,C'-'                                                  
                                                                                
         ZAP   WORK+8(4),=P'0'                                                  
         MVO   WORK+8(4),RSTRKEND  CHANGE TO PACK WITH SIGN                     
         ZAP   WORK+4(4),=P'999999'                                             
         SP    WORK+4(4),WORK+8(4) GET 9'S COMPLEMENT                           
         MVO   WORK(4),WORK+4(4)   CHANGE TO PWOS                               
         GOTO1 DATCON,DMCB,(8,WORK),(6,LSTRPERI+7)                              
         DROP  R6                                                               
                                                                                
LR75     DS    0H                                                               
         L     R6,AIO              NOTES                                        
         USING RSTRDESD,R6                                                      
         MVI   ELCODE,RSTRDCDQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLI   RSTRDELN,RSTRDOV                                                 
         BNH   LR90                                                             
                                                                                
         ZIC   R1,RSTRDELN         NOTE HAS VARIABLE LENGTH                     
         LA    RF,RSTRDOV          OVERHEAD LENGTH                              
         SR    R1,RF               NOTE LEN = TOTAL LEN - OVERHEAD LEN          
         BCTR  R1,0                                                             
         CH    R1,=H'24'           MAX LEN IS 25                                
         BNH   LR80                                                             
         LA    R1,24                                                            
                                                                                
LR80     DS    0H                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LSTRDESC(0),RSTRDESC                                             
                                                                                
LR90     DS    0H                                                               
         GOTO1 DATCON,DMCB,(2,RSTRDUPT),(5,LSTRUPDT)                            
         DROP  R6                                                               
                                                                                
         GOTO1 LISTMON             SEND LINE TO SCREEN                          
                                                                                
LRSEQ    GOTO1 SEQ                 GET NEXT RECORD                              
         B     LR20                                                             
                                                                                
LRX      DS    0H                                                               
         B     EXIT                                                             
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
RECNOTF  MVC   RERROR,=AL2(413)    SIT REC NOT FOUND. PLS ADD                   
         B     ERREND                                                           
*                                                                               
INVLPER  MVC   RERROR,=AL2(417)    FORMAT IS MMM/YY-MMM/YY                      
         B     ERREND                                                           
*                                                                               
MISSFLD  MVC   RERROR,=AL2(1)                                                   
         B     ERREND                                                           
*                                                                               
INVLFLD  MVC   RERROR,=AL2(2)                                                   
         B     ERREND                                                           
*                                                                               
SLOCKOUT MVC   RERROR,=AL2(55)                                                  
         B     ERREND                                                           
*                                                                               
INVLSIGN MVC   RERROR,=AL2(418)                                                 
         B     ERREND                                                           
*                                                                               
INVLDOVL MVC   RERROR,=AL2(366)    DATES OVERLAP EXISTING REC DATES             
         B     ERREND                                                           
*                                                                               
ERDELSUB MVC   RERROR,=AL2(444)    DELETE CORRESPONDING SUBRECS                 
         B     ERREND                                                           
*                                                                               
ERREND   GOTO1 MYERROR                                                          
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
       ++INCLUDE RESTRF1D          (OUR MAINTENANCE SCREEN OVERLAY)             
         ORG   CONTAGH                                                          
       ++INCLUDE RESTRF2D          (OUR LIST SCREEN OVERLAY)                    
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
SITKEY   DS    CL(L'KEY)                                                        
SELCKEY  DS    CL(L'KEY)                                                        
SAVER2   DS    F                                                                
ACURSOR  DS    A                   FORCE CURSOR HERE                            
SEQNUM   DS    X                                                                
GRPSGRP  DS    CL2                                                              
STATION  DS    CL5                                                              
SITFLAG  DS    X                                                                
NOBLNKLN EQU   X'02'               RECORD HAS NO BLANK DAY/TIME LINES           
*                                                                               
* ONLINE LIST LINE                                                              
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTRGRUP DS    CL2                                                              
         DS    CL2                                                              
LSTRSTAT DS    CL7                                                              
         DS    CL2                                                              
LSTRPERI DS    CL13                                                             
         DS    CL2                                                              
LSTRDESC DS    CL25                                                             
         DS    CL2                                                              
LSTRUPDT DS    CL8                                                              
*                                                                               
* ONLINE LIST LINE                                                              
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'035RESTR01   11/25/97'                                      
         END                                                                    
